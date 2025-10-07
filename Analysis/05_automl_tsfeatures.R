# TSfeatures AutoML with policy clamp [0.40, 0.70] — self-contained

rm(list = ls()); invisible(gc())

suppressPackageStartupMessages({
  library(tidyverse); library(here); library(glue); library(ggplot2)
  library(lubridate); library(jsonlite); library(h2o)
})

source("Analysis/utils_thresholds.R")   # acc_threshold(), thr_max_acc(), clip_thr(), prob_col()
source("Analysis/utils_models.R")

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("figures",        recursive = TRUE, showWarnings = FALSE)
ts_tag <- format(with_tz(Sys.time(), "Australia/Melbourne"), "%Y%m%d_%H%M%S")
seed   <- 73
clamp_thr <- function(t, lo = 0.40, hi = 0.70) pmin(pmax(as.numeric(t), lo), hi)

# ---- Ensure the four tsfeature datasets exist (build if missing) ------------
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

# ---- Variants list -----------------------------------------------------------
variants <- list(
  list(name = "quintiles_allfreq", data = quintiles_allfreq, tag = "tsf_quint_all"),
  list(name = "quintiles_feats",   data = quintiles_feats,   tag = "tsf_quint_feats"),
  list(name = "median_allfreq",    data = median_allfreq,    tag = "tsf_median_all"),
  list(name = "median_feats",      data = median_feats,      tag = "tsf_median_feats")
)

# ---- H2O up ------------------------------------------------------------------
h2o_up     <- function() !is.null(tryCatch(h2o.getConnection(), error = function(e) NULL))
ensure_h2o <- function(heap = "6G") { if (!h2o_up()) h2o.init(nthreads = -1, max_mem_size = heap); invisible(TRUE) }
ensure_h2o("6G")

# ---- Stratified split by fish (60/20/20) ------------------------------------
split_by_fish_strat <- function(df, p_train = 0.6, p_valid = 0.2, seed = 73) {
  stopifnot(all(c("fishNum","species") %in% names(df)))
  set.seed(seed)
  ids <- df |> distinct(fishNum, species)
  split_one <- function(di) {
    n <- nrow(di); idx <- sample.int(n)
    n_tr <- max(1, floor(p_train*n)); n_va <- max(1, floor(p_valid*n))
    n_te <- max(1, n - n_tr - n_va)
    # adjust if sums overflow
    while (n_tr + n_va + n_te > n) { if (n_tr > 1) n_tr <- n_tr - 1 else if (n_va > 1) n_va <- n_va - 1 else n_te <- n_te - 1 }
    tibble(
      fishNum = di$fishNum[idx],
      split = c(rep("train", n_tr), rep("valid", n_va), rep("test", n - n_tr - n_va))
    )
  }
  map_dfr(group_split(ids, species), split_one) |>
    right_join(df, by = "fishNum") |>
    relocate(split)
}

run_one <- function(v, seed = 73, budget = 600, positive = "SMB") {
  message("\n=== AutoML variant: ", v$name, " ===")
  ensure_h2o(); h2o.removeAll()
  
  sp <- split_by_fish_strat(v$data, seed = seed)
  x_cols <- setdiff(names(v$data), intersect(names(v$data), c("species","fishNum","quantile")))
  hex_tr <- as.h2o(dplyr::select(filter(sp, split=="train"), all_of(c("species", x_cols))))
  hex_va <- as.h2o(dplyr::select(filter(sp, split=="valid"), all_of(c("species", x_cols))))
  hex_te <- as.h2o(dplyr::select(filter(sp, split=="test"),  all_of(c("species", x_cols))))
  hex_tr[,"species"] <- h2o.asfactor(hex_tr[,"species"])
  hex_va[,"species"] <- h2o.asfactor(hex_va[,"species"])
  hex_te[,"species"] <- h2o.asfactor(hex_te[,"species"])
  
  aml <- h2o.automl(
    x = x_cols, y = "species",
    training_frame    = hex_tr,
    validation_frame  = hex_va,
    leaderboard_frame = hex_te,
    max_runtime_secs  = budget,
    sort_metric       = "AUC",
    nfolds            = 5,
    seed              = seed,
    balance_classes   = FALSE
  )
  
  best   <- aml@leader
  perfcv <- h2o.performance(best, xval = TRUE)
  cv_auc <- h2o.auc(perfcv)
  
  # CV F1 diagnostics — clipped to policy window
  thr_cv_raw  <- as.numeric(h2o.find_threshold_by_max_metric(perfcv, "f1"))
  thr_cv_clip <- clamp_thr(thr_cv_raw, 0.40, 0.70)
  
  # Predictions (bind species for thresholding helpers)
  tr_df <- as.data.frame(hex_tr); va_df <- as.data.frame(hex_va); te_df <- as.data.frame(hex_te)
  pr <- as.data.frame(h2o.predict(best, hex_tr)) |> dplyr::bind_cols(species = as.character(tr_df$species), .before = 1)
  pv <- as.data.frame(h2o.predict(best, hex_va)) |> dplyr::bind_cols(species = as.character(va_df$species), .before = 1)
  pt <- as.data.frame(h2o.predict(best, hex_te)) |> dplyr::bind_cols(species = as.character(te_df$species), .before = 1)
  
  pc <- prob_col(pt, positive)
  
  # Policy threshold: VALID max-ACC, clipped [0.40, 0.70]
  thr_valid_raw   <- thr_max_acc(truth = pv$species, prob = pv[[pc]], positive = positive)
  thr_policy_clip <- clamp_thr(thr_valid_raw, 0.40, 0.70)
  
  # Baselines + @policy
  perf_test        <- h2o.performance(best, hex_te)
  acc_argmax       <- if ("predict" %in% names(pt)) mean(pt$species == pt$predict) else NA_real_
  acc_test_050     <- as.numeric(h2o.accuracy(perf_test, thresholds = 0.50))
  acc_test_clip    <- as.numeric(h2o.accuracy(perf_test, thresholds = thr_cv_clip))
  acc_train_policy <- acc_threshold(pr$species, pr[[pc]], thr_policy_clip, positive)
  acc_valid_policy <- acc_threshold(pv$species, pv[[pc]], thr_policy_clip, positive)
  acc_test_policy  <- acc_threshold(pt$species, pt[[pc]], thr_policy_clip, positive)
  
  # Write out leaderboards + preds
  readr::write_rds(as_tibble(aml@leaderboard), here("outputs","tables", glue("leaderboard_{v$name}_{ts_tag}.rds")))
  readr::write_rds(as_tibble(pt), here("outputs","tables", glue("preds_{v$name}_test_{ts_tag}.rds")))
  readr::write_rds(as_tibble(pv), here("outputs","tables", glue("preds_{v$name}_valid_{ts_tag}.rds")))
  readr::write_rds(as_tibble(pr), here("outputs","tables", glue("preds_{v$name}_train_{ts_tag}.rds")))
  
  # Metrics JSON
  metrics <- list(
    variant            = v$name,
    cv_auc             = cv_auc,
    thr_cv_f1_raw      = thr_cv_raw,
    thr_cv_f1_clip     = thr_cv_clip,
    acc_test_at_050    = acc_test_050,
    acc_test_at_clip   = acc_test_clip,
    acc_test_argmax    = acc_argmax,
    thr_policy_raw     = as.numeric(thr_valid_raw),
    thr_policy_clip    = as.numeric(thr_policy_clip),
    acc_train_at_policy= acc_train_policy,
    acc_valid_at_policy= acc_valid_policy,
    acc_test_at_policy = acc_test_policy,
    positive_class     = positive
  )
  readr::write_file(jsonlite::toJSON(metrics, pretty = TRUE, auto_unbox = TRUE),
                    here("outputs","tables", glue("automl_metrics_{v$name}_{ts_tag}.json")))
  
  # Save MOJO + binary with extras
  lb_full <- h2o.get_leaderboard(aml, extra_columns = "ALL")
  save_h2o_artifacts(
    model       = best,
    tag         = v$tag,
    leaderboard = lb_full,
    train       = hex_tr,
    save_binary = TRUE,
    extras      = list(policy_thr = thr_policy_clip, clamp = c(0.40, 0.70), positive_class = positive)
  )
  invisible(TRUE)
}

safe_run <- function(v) {
  tryCatch(run_one(v), error = function(e) {
    message("…caught error: ", conditionMessage(e), " — retry once with fresh H2O.")
    try(h2o.shutdown(prompt = FALSE), silent = TRUE)
    ensure_h2o("6G"); Sys.sleep(1); h2o.removeAll()
    run_one(v)
  })
}

invisible(lapply(variants, safe_run))
message("\nAutoML (tsfeatures) complete. Tables/metrics saved; MOJOs under outputs/models/tsf_*.\n")
