# ==============================================================================
# 05_automl_tsfeatures.R
# PURPOSE
#   Train H2O AutoML on fish-level summary datasets created by 04_tsfeatures_build.R
#   Variants:
#     - quintiles_allfreq  : F45–F170 + feasts features (5 rows per fish; by-quantile)
#     - quintiles_feats    : feasts features only (5 rows per fish; by-quantile)
#     - median_allfreq     : F45–F170 + feasts features (1 row per fish; medians)
#     - median_feats       : feasts features only (1 row per fish; medians)
# WHAT IT DOES
#   - Ensures inputs exist; loads 4 datasets
#   - Stratified 60/40 split by fish within species (so TEST contains both classes)
#   - H2O AutoML with 5-fold CV; TEST held out for leaderboard & ROC
#   - Saves leaderboard, TEST predictions, metrics JSON, ROC PNG
# HOW TO VIEW
#   source("Analysis/view_results_tsfeatures.R")
# ==============================================================================

rm(list = ls()); invisible(gc())

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(glue)
  library(ggplot2)
  library(lubridate)
  library(h2o)
})

# ---------- folders / timestamp ------------------------------------------------
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("figures",        recursive = TRUE, showWarnings = FALSE)
ts_tag <- format(with_tz(Sys.time(), "Australia/Melbourne"), "%Y%m%d_%H%M%S")

# ---------- ensure inputs ------------------------------------------------------
paths_needed <- c(
  here("outputs","tables","fish_quintiles_allfreq_tsfeat.rds"),
  here("outputs","tables","fish_quintiles_tsfeat_only.rds"),
  here("outputs","tables","fish_median_allfreq_tsfeat.rds"),
  here("outputs","tables","fish_median_tsfeat_only.rds")
)
builder <- here("Analysis","04_tsfeatures_build.R")
if (!all(file.exists(paths_needed))) {
  message("tsfeatures datasets missing — running: ", builder)
  source(builder, local = TRUE)
  stopifnot(all(file.exists(paths_needed)))
}
Q_ALL  <- readRDS(paths_needed[1])
Q_ONLY <- readRDS(paths_needed[2])
M_ALL  <- readRDS(paths_needed[3])
M_ONLY <- readRDS(paths_needed[4])

# ---------- helpers (define BEFORE anything uses them) -------------------------
# Simple AUC by trapezoid on ROC curve
auc_from_probs <- function(truth, score, positive = "SMB") {
  y <- factor(truth, levels = c(setdiff(unique(truth), positive)[1], positive))
  pos <- as.integer(y == positive)
  neg <- 1 - pos
  if (sum(pos) == 0 || sum(neg) == 0) return(NA_real_)
  ord <- order(score, decreasing = TRUE)
  tp <- cumsum(pos[ord]); fp <- cumsum(neg[ord])
  tpr <- tp / sum(pos);    fpr <- fp / sum(neg)
  # prepend origin for trapezoid integration
  tpr <- c(0, tpr); fpr <- c(0, fpr)
  sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2)
}


feat_cols <- function(df) setdiff(names(df), intersect(names(df), c("species","fishNum","quantile")))
prob_col  <- function(df, positive = "SMB") {
  if (positive %in% names(df)) return(positive)
  pc <- names(df)[grepl("^p", names(df))]
  if (length(pc)) return(pc[1])
  stop("No probability column found.")
}

# Stratified 60/40 split BY FISH within species
split_by_fish_strat <- function(df, seed = 73) {
  stopifnot(all(c("species","fishNum") %in% names(df)))
  set.seed(seed)
  
  parts <- df |>
    distinct(species, fishNum) |>
    group_by(species) |>
    summarise(fish = list(sample(fishNum)), .groups = "drop") |>
    mutate(
      n    = purrr::map_int(fish, length),
      n_tr = pmax(1, floor(0.6 * n)),
      train_ids = purrr::map2(fish, n_tr, ~ .x[seq_len(.y)]),
      test_ids  = purrr::map2(fish, train_ids, ~ setdiff(.x, .y))
    )
  
  tr_ids <- parts |>
    select(species, train_ids) |>
    tidyr::unnest(train_ids, names_repair = "minimal") |>
    rename(fishNum = train_ids)
  
  te_ids <- parts |>
    select(species, test_ids) |>
    tidyr::unnest(test_ids, names_repair = "minimal") |>
    rename(fishNum = test_ids)
  
  tr <- df |> semi_join(tr_ids, by = c("species","fishNum"))
  te <- df |> semi_join(te_ids, by = c("species","fishNum"))
  
  # Safety: ensure each species appears in TEST
  for (sp in unique(df$species)) {
    if (!any(te$species == sp)) {
      spare <- tr_ids |> filter(species == sp) |> slice_tail(n = 1)
      te <- bind_rows(te, df |> semi_join(spare, by = c("species","fishNum")))
      tr <- anti_join(tr, spare, by = c("species","fishNum"))
    }
  }
  list(train = tr, test = te)
}

# AUC via ROC trapezoid (used only if we ever need to compute test AUC ad-hoc)
auc_from_probs <- function(truth, score, positive = "SMB") {
  y <- factor(truth, levels = c(setdiff(unique(truth), positive)[1], positive))
  pos <- as.integer(y == positive); neg <- 1 - pos
  if (sum(pos) == 0 || sum(neg) == 0) return(NA_real_)
  ord <- order(score, decreasing = TRUE)
  tp <- cumsum(pos[ord]); fp <- cumsum(neg[ord])
  tpr <- tp / sum(pos); fpr <- fp / sum(neg)
  tpr <- c(0, tpr); fpr <- c(0, fpr)
  sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2)
}

# H2O connection helpers (compatible with 3.46)
h2o_up     <- function() !is.null(tryCatch(h2o.getConnection(), error = function(e) NULL))
ensure_h2o <- function(heap = "6G") { if (!h2o_up()) h2o.init(nthreads = -1, max_mem_size = heap); invisible(TRUE) }

# ---------- variants -----------------------------------------------------------
variants <- list(
  list(name = "quintiles_allfreq", data = Q_ALL),
  list(name = "quintiles_feats",   data = Q_ONLY),
  list(name = "median_allfreq",    data = M_ALL),
  list(name = "median_feats",      data = M_ONLY)
)

# ---------- init once ----------------------------------------------------------
ensure_h2o("6G")

run_one <- function(v, seed = 73, budget = 600, positive = "SMB") {
  message("\n=== AutoML variant: ", v$name, " ===")
  ensure_h2o(); h2o.removeAll()
  
  sp <- split_by_fish_strat(v$data, seed = seed)
  
  x_cols <- feat_cols(v$data)
  stopifnot("species" %in% names(v$data), length(x_cols) > 0)
  
  hex_tr <- as.h2o(select(sp$train, all_of(c("species", x_cols))))
  hex_te <- as.h2o(select(sp$test,  all_of(c("species", x_cols))))
  hex_tr[,"species"] <- h2o.asfactor(hex_tr[,"species"])
  hex_te[,"species"] <- h2o.asfactor(hex_te[,"species"])
  
  aml <- h2o.automl(
    x = x_cols, y = "species",
    training_frame    = hex_tr,
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
  
  pt <- as.data.frame(h2o.predict(best, hex_te)) |>
    bind_cols(species = as.character(as.data.frame(hex_te)$species), .before = 1)
  
  pc <- prob_col(pt, positive)
  truth <- as.integer(pt$species == positive)
  score <- pt[[pc]]
  
  # ROC points (guard if a class is missing)
  if (sum(truth) > 0 && sum(1 - truth) > 0) {
    ord <- order(score, decreasing = TRUE)
    tp <- cumsum(truth[ord]); fp <- cumsum(1 - truth[ord])
    tpr <- tp / sum(truth); fpr <- fp / sum(1 - truth)
    roc_df <- tibble(fpr = c(0, fpr), tpr = c(0, tpr))
    test_auc <- auc_from_probs(pt$species, pt[[pc]], positive)
  } else {
    roc_df <- tibble(fpr = c(0, 0), tpr = c(0, 1e-6))
    test_auc <- NA_real_
  }
  
  fig_path <- here("figures", glue("roc_{v$name}_{ts_tag}.png"))
  ggsave(
    fig_path,
    ggplot(roc_df, aes(fpr, tpr)) +
      geom_abline(slope = 1, intercept = 0, colour = "grey60") +
      { if (nrow(roc_df) >= 2) geom_path() else geom_point() } +
      coord_equal() +
      labs(title = glue("AutoML ROC ({v$name}) — {positive} positive"),
           x = "False positive rate", y = "True positive rate") +
      theme_classic(),
    width = 6.5, height = 5, dpi = 160
  )
  
  lb_path <- here("outputs","tables", glue("leaderboard_{v$name}_{ts_tag}.rds"))
  pt_path <- here("outputs","tables", glue("preds_{v$name}_test_{ts_tag}.rds"))
  readr::write_rds(as_tibble(aml@leaderboard), lb_path)
  readr::write_rds(as_tibble(pt), pt_path)
  
  # ---- collect metrics --------------------------------------------------------
  # CV AUC
  cv_auc <- h2o.auc(perfcv)
  
  # CV F1-optimal threshold (grab threshold at max F1 from CV curve)
  cv_thr <- tryCatch({
    mf <- as.data.frame(h2o.metric(perfcv)) # has F1 & threshold
    mf$threshold[which.max(mf$F1)]
  }, error = function(e) NULL)
  
  # TEST AUC (using saved preds + helper function)
  pc <- prob_col(pt, positive)
  test_auc <- auc_from_probs(pt$species, pt[[pc]], positive = positive)
  
  # Bundle up
  metrics <- list(
    variant   = v$name,
    n_train   = nrow(sp$train),
    n_test    = nrow(sp$test),
    cv_auc    = cv_auc,
    cv_f1_thr = cv_thr,
    test_auc  = test_auc,
    roc_png   = fig_path
  )
  
  readr::write_file(jsonlite::toJSON(metrics, pretty = TRUE, auto_unbox = TRUE),
                    here("outputs","tables", glue("automl_metrics_{v$name}_{ts_tag}.json")))
  
  invisible(TRUE)
}



# Retry wrapper: handle brief reconnect on Windows
safe_run <- function(v) {
  tryCatch(
    run_one(v),
    error = function(e) {
      message("…caught error: ", conditionMessage(e), " — retry once with fresh H2O.")
      try(h2o.shutdown(prompt = FALSE), silent = TRUE)
      ensure_h2o("6G"); Sys.sleep(1); h2o.removeAll()
      run_one(v)
    }
  )
}

invisible(lapply(variants, safe_run))
message("\nAutoML (tsfeatures) complete. Artifacts in outputs/tables and figures/.\n")
