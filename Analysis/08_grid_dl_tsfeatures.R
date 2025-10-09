# Analysis/08_grid_dl_tsfeatures.R
# DeepLearning grid for tsfeature variants (no MOJOs). Wider search, grouped CV.
rm(list = ls()); invisible(gc())

suppressPackageStartupMessages({
  library(tidyverse); library(here); library(glue); library(lubridate)
  library(jsonlite); library(h2o)
})

source("Analysis/utils_thresholds.R")   # acc_threshold(), thr_max_acc(), clip_thr(), prob_col()

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

ts_tag <- format(with_tz(Sys.time(), "Australia/Melbourne"), "%Y%m%d_%H%M%S")
seed   <- 73
clamp_thr <- function(t, lo = 0.40, hi = 0.70) pmin(pmax(as.numeric(t), lo), hi)

# ---------- Ensure tsfeature inputs exist ----------
paths_needed <- c(
  qa = here("outputs","tables","fish_quintiles_allfreq_tsfeat.rds"),
  qf = here("outputs","tables","fish_quintiles_tsfeat_only.rds"),
  ma = here("outputs","tables","fish_median_allfreq_tsfeat.rds"),
  mf = here("outputs","tables","fish_median_tsfeat_only.rds")
)
if (!all(file.exists(paths_needed))) {
  message("TSfeature files missing — running Analysis/04_tsfeatures_build.R")
  source(here("Analysis","04_tsfeatures_build.R"), local = TRUE)
  if (!all(file.exists(paths_needed))) stop("Expected tsfeature files not created by 04_tsfeatures_build.R")
}

quintiles_allfreq <- readRDS(paths_needed["qa"])
quintiles_feats   <- readRDS(paths_needed["qf"])
median_allfreq    <- readRDS(paths_needed["ma"])
median_feats      <- readRDS(paths_needed["mf"])

variants <- list(
  list(name = "quintiles_allfreq", data = quintiles_allfreq),
  list(name = "quintiles_feats",   data = quintiles_feats),
  list(name = "median_allfreq",    data = median_allfreq),
  list(name = "median_feats",      data = median_feats)
)

# ---------- H2O helpers ----------
h2o_up <- function() !is.null(tryCatch(h2o.getConnection(), error = function(e) NULL))
ensure_h2o <- function(heap = "6G", random_port = FALSE) {
  if (!h2o_up()) {
    if (random_port) {
      # use a random available port to avoid race conditions with stuck JVMs
      h2o.init(nthreads = -1, max_mem_size = heap, port = 0, startH2O = TRUE)
    } else {
      h2o.init(nthreads = -1, max_mem_size = heap)
    }
  }
  invisible(TRUE)
}
ensure_h2o("6G")

# ---------- Split + grouped CV folds by fish ----------
split_60_20_20 <- function(df, seed = 73) {
  stopifnot(all(c("fishNum","species") %in% names(df)))
  set.seed(seed)
  per_fish <- df |> distinct(fishNum, species)
  per_fish <- per_fish |>
    group_by(species) |>
    mutate(
      cv_fold = sample(0:4, n(), replace = TRUE),
      r = runif(n())
    ) |>
    mutate(split = case_when(
      r <= 0.60 ~ "train",
      r <= 0.80 ~ "valid",
      TRUE      ~ "test"
    )) |>
    ungroup() |> select(-r)
  df |> left_join(per_fish, by = c("fishNum","species"))
}

# ---------- One variant runner ----------
run_one <- function(v, positive = "SMB") {
  message("\n=== DL GRID — ", v$name, " ===")
  # be gentle with removeAll: if it times out, restart H2O on a new port
  ok <- tryCatch({ h2o.removeAll(); TRUE }, error = function(e) FALSE)
  if (!ok) {
    try(h2o.shutdown(prompt = FALSE), silent = TRUE)
    Sys.sleep(1)
    ensure_h2o("6G", random_port = TRUE)
  }
  
  set.seed(seed)
  df <- split_60_20_20(v$data, seed = seed)
  x  <- setdiff(names(df), intersect(names(df), c("species","fishNum","quantile")))
  y  <- "species"
  
  # H2O frames
  hex_tr <- as.h2o(df |> filter(split=="train") |> select(all_of(c(y, "cv_fold", x))))
  hex_va <- as.h2o(df |> filter(split=="valid") |> select(all_of(c(y, "cv_fold", x))))
  hex_te <- as.h2o(df |> filter(split=="test")  |> select(all_of(c(y, "cv_fold", x))))
  hex_tr[,y] <- h2o.asfactor(hex_tr[,y])
  hex_va[,y] <- h2o.asfactor(hex_va[,y])
  hex_te[,y] <- h2o.asfactor(hex_te[,y])
  
  # ---------------- Wider DL grid (DL-safe params only) ----------------
  hyper_params <- list(
    # depth/width
    hidden = list(c(64,64), c(128,64), c(128,128), c(256,128), c(256,256),
                  c(256,128,64), c(128,128,64)),
    epochs = c(30, 60, 120, 240),
    
    # regularization
    l1 = c(0, 1e-6, 1e-5, 1e-4),
    l2 = c(0, 1e-6, 1e-5, 1e-4),
    max_w2 = c(10, 100, 1000),
    
    # dropout
    input_dropout_ratio   = c(0, 0.1, 0.2, 0.4),
    hidden_dropout_ratios = list(c(0,0), c(0.1,0.1), c(0.2,0.2)),
    
    # optimizers
    adaptive_rate = c(TRUE, FALSE),
    # adaptive params (used when adaptive_rate = TRUE)
    rho     = c(0.90, 0.95, 0.99),
    epsilon = c(1e-8, 1e-7),
    # SGD params (used when adaptive_rate = FALSE)
    rate            = c(0.005, 0.01, 0.02),
    rate_annealing  = c(1e-6, 1e-7),
    momentum_start  = c(0, 0.5),
    momentum_stable = c(0.5, 0.9),
    
    # activations
    activation = c("Rectifier", "Tanh", "Maxout",
                   "RectifierWithDropout", "TanhWithDropout", "MaxoutWithDropout"),
    
    # class balance (let grid decide)
    balance_classes = c(FALSE, TRUE)
  )
  
  search_criteria <- list(
    strategy = "RandomDiscrete",
    max_models = 50,
    seed = seed,
    stopping_rounds = 8,
    stopping_metric = "AUC",
    stopping_tolerance = 1e-4
  )
  
  grid_id <- paste0("dlgrid_", v$name, "_", ts_tag)
  
  build_grid <- function() {
    h2o.grid(
      algorithm = "deeplearning",
      grid_id   = grid_id,
      x = x, y = y,
      training_frame   = hex_tr,
      validation_frame = hex_va,
      # NOTE: Grid API has no leaderboard_frame — removed for compatibility
      fold_column      = "cv_fold",
      distribution     = "bernoulli",
      standardize      = TRUE,
      reproducible     = FALSE,
      stopping_rounds  = 8,
      stopping_metric  = "AUC",
      seed             = seed,
      hyper_params     = hyper_params,
      search_criteria  = search_criteria
    )
  }
  
  dl_grid <- tryCatch(build_grid(), error = function(e) {
    message("[grid] Caught error: ", conditionMessage(e), " — restarting H2O on a new port and retrying once.")
    try(h2o.shutdown(prompt = FALSE), silent = TRUE)
    Sys.sleep(1)
    ensure_h2o("6G", random_port = TRUE)
    # Recreate frames after restart
    hex_tr <<- as.h2o(df |> filter(split=="train") |> select(all_of(c(y, "cv_fold", x))))
    hex_va <<- as.h2o(df |> filter(split=="valid") |> select(all_of(c(y, "cv_fold", x))))
    hex_te <<- as.h2o(df |> filter(split=="test")  |> select(all_of(c(y, "cv_fold", x))))
    hex_tr[,y] <<- h2o.asfactor(hex_tr[,y]); hex_va[,y] <<- h2o.asfactor(hex_va[,y]); hex_te[,y] <<- h2o.asfactor(hex_te[,y])
    build_grid()
  })
  
  # best by AUC
  dlg <- h2o.getGrid(grid_id, sort_by = "auc", decreasing = TRUE)
  if (length(dlg@model_ids) == 0) stop("DL grid produced no models.")
  best_id <- as.character(dlg@model_ids[[1]])
  message("Best DL model: ", best_id)
  best <- h2o.getModel(best_id)
  
  # ---- Evaluate @0.50 and @policy threshold (VALID max-ACC, clipped 0.40–0.70) ----
  pv <- as.data.frame(h2o.predict(best, hex_va)) |>
    dplyr::bind_cols(species = as.character(as.data.frame(hex_va)$species))
  pt <- as.data.frame(h2o.predict(best, hex_te)) |>
    dplyr::bind_cols(species = as.character(as.data.frame(hex_te)$species))
  
  pc  <- prob_col(pt, positive = positive)
  neg <- setdiff(unique(pt$species), positive)[1]
  
  thr_pol_raw  <- thr_max_acc(pv$species, pv[[pc]], positive = positive)
  thr_pol_clip <- clamp_thr(thr_pol_raw, 0.40, 0.70)
  
  acc_test_050 <- mean(ifelse(pt[[pc]] >= 0.50,        positive, neg) == pt$species)
  acc_test_pol <- mean(ifelse(pt[[pc]] >= thr_pol_clip, positive, neg) == pt$species)
  
  # ---- Save artifacts ----
  readr::write_rds(
    as_tibble(h2o.getGrid(grid_id)@summary_table),
    here("outputs","tables", glue("dlgrid_leaderboard_{v$name}_{ts_tag}.rds"))
  )
  readr::write_rds(
    as_tibble(pt),
    here("outputs","tables", glue("dlgrid_preds_test_{v$name}_{ts_tag}.rds"))
  )
  mets <- list(
    variant             = v$name,
    model_id            = best_id,
    algorithm           = "deeplearning",
    policy_thr_clip     = as.numeric(thr_pol_clip),
    acc_test_at_050     = as.numeric(acc_test_050),
    acc_test_at_policy  = as.numeric(acc_test_pol),
    positive_class      = positive
  )
  readr::write_file(
    jsonlite::toJSON(mets, pretty = TRUE, auto_unbox = TRUE),
    here("outputs","tables", glue("dlgrid_metrics_{v$name}_{ts_tag}.json"))
  )
  
  message("=== DL GRID — ", toupper(v$name), " ===")
  message("Best model: ", best_id)
  message("Policy thr (VALID max-ACC, clipped): ", signif(thr_pol_clip, 6))
  message("TEST acc @ 0.50:  ", sprintf("%0.3f", acc_test_050))
  message("TEST acc @ policy:", sprintf("%0.3f", acc_test_pol))
  invisible(TRUE)
}

safe_run <- function(v) {
  tryCatch(run_one(v), error = function(e) {
    message("…caught error: ", conditionMessage(e), " — skipping ", v$name)
  })
}

invisible(lapply(variants, safe_run))
message("DeepLearning grid complete. Artifacts saved under outputs/tables/.")
