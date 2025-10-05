# ==============================================================================
# 05_automl_tsfeatures.R
# PURPOSE
#   AutoML on fish-level summary datasets (feasts features).
#   Adds robust thresholding:
#     - CV F1 raw & clipped + near-opt F1 band (±1%)  [diagnostic]
#     - Single operating threshold (“policy”): VALID max-ACC, clipped to [0.2,0.8]
#     - Accuracy @ policy for TRAIN / VALID / TEST (all 3 reported)
# ==============================================================================

rm(list = ls()); invisible(gc())

suppressPackageStartupMessages({
  library(tidyverse); library(here); library(glue); library(ggplot2)
  library(lubridate); library(jsonlite); library(h2o)
})

# Helpers for thresholds/accuracy (you already have these in this file + utils)
source("Analysis/utils_thresholds.R")   # expects acc_threshold(), thr_max_acc(), print_acc_summary()

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("figures",        recursive = TRUE, showWarnings = FALSE)
ts_tag <- format(with_tz(Sys.time(), "Australia/Melbourne"), "%Y%m%d_%H%M%S")
seed   <- 73

# ---- local helpers -----------------------------------------------------------
clamp_thr <- function(t, lo = 0.20, hi = 0.80) pmin(pmax(as.numeric(t), lo), hi)

f1_band_from_perf <- function(perf, within = 0.01) {
  df <- try(as.data.frame(h2o.metric(perf)), silent = TRUE)
  if (inherits(df, "try-error") || !"threshold" %in% names(df) || !"f1" %in% names(df))
    return(list(lo = NA_real_, hi = NA_real_, f1_max = NA_real_))
  df <- df[is.finite(df$f1) & is.finite(df$threshold), , drop = FALSE]
  if (!nrow(df)) return(list(lo = NA_real_, hi = NA_real_, f1_max = NA_real_))
  fmax <- max(df$f1, na.rm = TRUE)
  band <- df$threshold[df$f1 >= (1 - within) * fmax]
  list(lo = min(band, na.rm = TRUE), hi = max(band, na.rm = TRUE), f1_max = fmax)
}

# choose probability column (SMB or first p#)
prob_col <- function(df, positive = "SMB") {
  if (positive %in% names(df)) return(positive)
  pcols <- names(df)[grepl("^p\\d+$", names(df))]
  if (length(pcols)) return(pcols[1])
  stop("No probability column found in predictions.")
}

# trapezoid AUC from scores
auc_from_probs <- function(truth_chr, score, positive = "SMB") {
  y <- as.integer(truth_chr == positive)
  if (sum(y) == 0 || sum(1 - y) == 0) return(NA_real_)
  o <- order(score, decreasing = TRUE)
  tp <- cumsum(y[o]); fp <- cumsum(1 - y[o])
  tpr <- tp / sum(y); fpr <- fp / sum(1 - y)
  sum(diff(c(0, fpr)) * (head(c(0, tpr), -1) + tail(c(0, tpr), -1)) / 2)
}

# ---- ensure inputs -----------------------------------------------------------
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

feat_cols <- function(df) setdiff(names(df), intersect(names(df), c("species","fishNum","quantile")))

source("Analysis/utils_models.R")

variants <- list(
  list(name = "quintiles_allfreq", data = Q_ALL,  tag = "tsf_q_all"),
  list(name = "quintiles_feats",   data = Q_ONLY, tag = "tsf_q_feat"),
  list(name = "median_allfreq",    data = M_ALL,  tag = "tsf_m_all"),
  list(name = "median_feats",      data = M_ONLY, tag = "tsf_m_feat")
)

h2o_up     <- function() !is.null(tryCatch(h2o.getConnection(), error = function(e) NULL))
ensure_h2o <- function(heap = "6G") { if (!h2o_up()) h2o.init(nthreads = -1, max_mem_size = heap); invisible(TRUE) }
ensure_h2o("6G")

# stratified 60/20/20 split by fish within species
split_by_fish_strat <- function(df, seed = 73, p_train = 0.6, p_valid = 0.2) {
  stopifnot(all(c("species","fishNum") %in% names(df)))
  set.seed(seed)
  parts <- df |>
    dplyr::distinct(species, fishNum) |>
    dplyr::group_by(species) |>
    dplyr::summarise(fish = list(sample(fishNum)), .groups = "drop") |>
    dplyr::mutate(
      n    = purrr::map_int(fish, length),
      n_tr = pmax(1, floor(p_train * n)),
      n_va = pmax(1, floor(p_valid * n)),
      train_ids = purrr::map2(fish, n_tr, ~ .x[seq_len(.y)]),
      
      # REPLACE THESE TWO:
      valid_ids = purrr::pmap(
        list(fish, train_ids, n_va),
        function(f, tr, nva) head(setdiff(f, tr), nva)
      ),
      test_ids  = purrr::pmap(
        list(fish, train_ids, valid_ids),
        function(f, tr, va) setdiff(f, c(tr, va))
      )
    )
  
  tr_ids <- parts |> select(species, train_ids) |> tidyr::unnest(train_ids) |> rename(fishNum = train_ids)
  va_ids <- parts |> select(species, valid_ids) |> tidyr::unnest(valid_ids) |> rename(fishNum = valid_ids)
  te_ids <- parts |> select(species, test_ids)  |> tidyr::unnest(test_ids)  |> rename(fishNum = test_ids)
  
  tr <- df |> semi_join(tr_ids, by = c("species","fishNum"))
  va <- df |> semi_join(va_ids, by = c("species","fishNum"))
  te <- df |> semi_join(te_ids, by = c("species","fishNum"))
  list(train = tr, valid = va, test = te)
}

run_one <- function(v, seed = 73, budget = 600, positive = "SMB") {
  message("\n=== AutoML variant: ", v$name, " ===")
  ensure_h2o(); h2o.removeAll()
  
  sp <- split_by_fish_strat(v$data, seed = seed)
  x_cols <- feat_cols(v$data)
  hex_tr <- as.h2o(select(sp$train, all_of(c("species", x_cols))))
  hex_va <- as.h2o(select(sp$valid, all_of(c("species", x_cols))))
  hex_te <- as.h2o(select(sp$test,  all_of(c("species", x_cols))))
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
  
  # ---- CV F1 diagnostics (for auditing) --------------------------------------
  thr_cv_raw  <- as.numeric(h2o.find_threshold_by_max_metric(perfcv, "f1"))
  thr_cv_clip <- clamp_thr(thr_cv_raw, 0.20, 0.80)
  band_cv     <- f1_band_from_perf(perfcv, within = 0.01)
  
  # ---- Predictions on TRAIN / VALID / TEST -----------------------------------
  pr <- as.data.frame(h2o.predict(best, hex_tr)) |>
    bind_cols(species = as.character(as.data.frame(hex_tr)$species), .before = 1)
  pv <- as.data.frame(h2o.predict(best, hex_va)) |>
    bind_cols(species = as.character(as.data.frame(hex_va)$species), .before = 1)
  pt <- as.data.frame(h2o.predict(best, hex_te)) |>
    bind_cols(species = as.character(as.data.frame(hex_te)$species), .before = 1)
  
  # probability column used everywhere below
  pc <- prob_col(pt, positive)
  
  # ---- Single "policy" threshold: VALID max-ACC, clipped to [0.2, 0.8] -------
  thr_valid_raw  <- thr_max_acc(truth = pv$species, prob = pv[[pc]], positive = positive)
  thr_policy_clip <- clamp_thr(thr_valid_raw, 0.20, 0.80)
  
  # Optional console summary (also returns the chosen thr); harmless if you keep it
  invisible(print_acc_summary(
    name       = paste0("TSFEATURES_", toupper(v$name)),
    pred_train = pr, pred_valid = pv, pred_test  = pt,
    positive   = positive, clip = c(0.20, 0.80)
  ))
  
  # ---- Accuracies @ baseline and @ policy ------------------------------------
  perf_test     <- h2o.performance(best, hex_te)
  acc_argmax    <- if ("predict" %in% names(pt)) mean(pt$species == pt$predict) else NA_real_
  acc_test_050  <- as.numeric(h2o.accuracy(perf_test, thresholds = 0.50))
  acc_test_clip <- as.numeric(h2o.accuracy(perf_test, thresholds = thr_cv_clip))
  
  acc_train_policy <- acc_threshold(pr$species, pr[[pc]], thr_policy_clip, positive)
  acc_valid_policy <- acc_threshold(pv$species, pv[[pc]], thr_policy_clip, positive)
  acc_test_policy  <- acc_threshold(pt$species, pt[[pc]], thr_policy_clip, positive)
  
  # ---- ROC + AUC (TEST) ------------------------------------------------------
  test_auc <- auc_from_probs(pt$species, pt[[pc]], positive)
  ord <- order(pt[[pc]], decreasing = TRUE)
  tp <- cumsum(as.integer(pt$species == positive)[ord])
  fp <- cumsum(as.integer(pt$species != positive)[ord])
  tpr <- tp / pmax(sum(pt$species == positive), 1)
  fpr <- fp / pmax(sum(pt$species != positive), 1)
  roc_df <- tibble(fpr = c(0, fpr), tpr = c(0, tpr))
  fig_path <- here("figures", glue("roc_{v$name}_{ts_tag}.png"))
  ggsave(
    fig_path,
    ggplot(roc_df, aes(fpr, tpr)) +
      geom_abline(slope = 1, intercept = 0, colour = "grey60") +
      geom_path() + coord_equal() +
      labs(title = glue("AutoML ROC ({v$name}) — {positive} positive"),
           x = "False positive rate", y = "True positive rate") +
      theme_classic(),
    width = 6.5, height = 5, dpi = 160
  )
  
  # ---- Save tables (LB + preds for TEST, VALID, TRAIN) -----------------------
  lb_path  <- here("outputs","tables", glue("leaderboard_{v$name}_{ts_tag}.rds"))
  pt_path  <- here("outputs","tables", glue("preds_{v$name}_test_{ts_tag}.rds"))
  pv_path  <- here("outputs","tables", glue("preds_{v$name}_valid_{ts_tag}.rds"))
  pr_path  <- here("outputs","tables", glue("preds_{v$name}_train_{ts_tag}.rds"))
  readr::write_rds(as_tibble(aml@leaderboard), lb_path)
  readr::write_rds(as_tibble(pt), pt_path)
  readr::write_rds(as_tibble(pv), pv_path)
  readr::write_rds(as_tibble(pr), pr_path)
  
  # ---- Metrics JSON ----------------------------------------------------------
  metrics <- list(
    variant            = v$name,
    n_train            = nrow(sp$train),
    n_valid            = nrow(sp$valid),
    n_test             = nrow(sp$test),
    # AUCs
    cv_auc             = cv_auc,
    test_auc           = test_auc,
    # CV-F1 diagnostics (auditing only)
    thr_cv_f1_raw      = thr_cv_raw,
    thr_cv_f1_clip     = thr_cv_clip,
    f1_band_cv_lo      = as.numeric(band_cv$lo),
    f1_band_cv_hi      = as.numeric(band_cv$hi),
    f1_band_within     = 0.01,
    # Baselines on TEST
    acc_test_at_050    = acc_test_050,
    acc_test_at_clip   = acc_test_clip,
    acc_test_argmax    = acc_argmax,
    # Single operating threshold (policy) + accuracies at that threshold
    thr_policy_raw     = as.numeric(thr_valid_raw),
    thr_policy_clip    = as.numeric(thr_policy_clip),
    acc_train_at_policy= acc_train_policy,
    acc_valid_at_policy= acc_valid_policy,
    acc_test_at_policy = acc_test_policy,
    # misc
    roc_png            = fig_path,
    positive_class     = positive
  )
  
  readr::write_file(
    jsonlite::toJSON(metrics, pretty = TRUE, auto_unbox = TRUE),
    here("outputs","tables", glue("automl_metrics_{v$name}_{ts_tag}.json"))
  )
  
  # ---- Save MOJO -------------------------------------------------------------
  lb_full <- h2o.get_leaderboard(aml, extra_columns = "ALL")
  save_h2o_artifacts(model = best, tag = v$tag, leaderboard = lb_full, train = hex_tr, save_binary = TRUE)
  
  message("Leader: ", best@algorithm, " | CV AUC: ", round(cv_auc, 3))
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
message("\nAutoML (tsfeatures) complete. Tables/figures + metrics saved; MOJOs under outputs/models/tsf_*.\n")
