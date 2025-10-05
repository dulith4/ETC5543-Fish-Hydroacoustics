# ==============================================================================
# 03b_automl_backscatter.R
# PURPOSE
#   AutoML on size-standardised (450 mm) acoustic backscatter features.
#   Robust thresholding + richer metrics JSON per variant:
#     • VALID F1 raw/clip + near-opt F1 band
#     • Single operating threshold (“policy”) = VALID max-ACC, clipped [0.20, 0.80]
#     • TRAIN/VALID/TEST accuracy at policy + baselines (0.50 and VALID-F1-clipped)
# ==============================================================================

rm(list = ls()); invisible(gc())

suppressPackageStartupMessages({
  library(tidyverse); library(here); library(lubridate); library(glue)
  library(ggplot2); library(jsonlite); library(h2o)
})

source("Analysis/utils_models.R")
source("Analysis/utils_thresholds.R")  # acc_threshold(), thr_max_acc(), print_acc_summary()

# ---- helpers (new) -----------------------------------------------------------
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
pick_prob_col <- function(df, positive = "SMB") {
  if (positive %in% names(df)) return(positive)
  pcols <- names(df)[grepl("^p\\d+$", names(df))]
  if (length(pcols)) return(pcols[1])
  # fallback: any class-prob or p* column except 'predict' and 'species'
  cand <- setdiff(names(df)[grepl("^(p\\d+|LT|SMB)$", names(df))], c("predict","species"))
  if (length(cand)) return(cand[1])
  stop("No probability column found in predictions.")
}

# -------- folders --------------------------------------------------------------
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE, recursive = TRUE)

ts_tag <- format(with_tz(Sys.time(), "Australia/Melbourne"), "%Y%m%d_%H%M%S")
seed   <- 73

# -------- ensure transformed inputs exist -------------------------------------
builder <- here("Analysis","02a_check_transformations.R")
paths <- list(
  train    = here("outputs","tables","train_backscatter_450.rds"),
  validate = here("outputs","tables","validate_backscatter_450.rds"),
  test     = here("outputs","tables","test_backscatter_450.rds")
)
if (!all(file.exists(unlist(paths)))) {
  message("Backscatter files missing — running: ", builder)
  source(builder, local = TRUE)
  stopifnot(all(file.exists(unlist(paths))))
}

train_bs    <- readRDS(paths$train)
validate_bs <- readRDS(paths$validate)
test_bs     <- readRDS(paths$test)

freq_cols <- names(train_bs)[stringr::str_detect(names(train_bs), "^F\\d+(?:\\.\\d+)?$")]
stopifnot(length(freq_cols) > 0, all(c("Region","species") %in% names(train_bs)))

# -------- helper: 5-ping blocks + mean across time ----------------------------
mk_blocks5 <- function(df) {
  df |>
    dplyr::group_by(Region) |>
    dplyr::mutate(.grp = rep(seq_len(ceiling(dplyr::n()/5)), each = 5, length.out = dplyr::n())) |>
    dplyr::ungroup() |>
    dplyr::group_by(Region, .grp, species) |>
    dplyr::filter(dplyr::n() == 5) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(freq_cols), mean), .groups = "drop")
}

train_blk <- mk_blocks5(train_bs)
valid_blk <- mk_blocks5(validate_bs)
test_blk  <- mk_blocks5(test_bs)

# -------- variants ------------------------------------------------------------
variant_perping <- list(
  name  = "original",
  train = train_bs    |> dplyr::select(dplyr::all_of(freq_cols), species),
  valid = validate_bs |> dplyr::select(dplyr::all_of(freq_cols), species),
  test  = test_bs     |> dplyr::select(dplyr::all_of(freq_cols), species)
)
variant_blockmean <- list(
  name  = "original_blocks",
  train = train_blk |> dplyr::select(dplyr::all_of(freq_cols), species),
  valid = valid_blk |> dplyr::select(dplyr::all_of(freq_cols), species),
  test  = test_blk  |> dplyr::select(dplyr::all_of(freq_cols), species)
)
variants <- list(variant_perping, variant_blockmean)

# -------- H2O setup -----------------------------------------------------------
if (!requireNamespace("h2o", quietly = TRUE)) install.packages("h2o")
library(h2o)
h2o.init(nthreads = -1)

run_automl <- function(v) {
  message("\n=== AutoML variant: ", v$name, " ===")
  
  hex_train <- as.h2o(v$train)
  hex_valid <- as.h2o(v$valid)
  hex_test  <- as.h2o(v$test)
  y <- "species"; x <- setdiff(names(v$train), y)
  hex_train[[y]] <- as.factor(hex_train[[y]])
  hex_valid[[y]] <- as.factor(hex_valid[[y]])
  hex_test[[y]]  <- as.factor(hex_test[[y]])
  
  aml <- h2o.automl(
    x = x, y = y,
    training_frame    = hex_train,
    validation_frame  = hex_valid,
    leaderboard_frame = hex_test,
    max_runtime_secs  = 600,
    nfolds            = 0,
    balance_classes   = TRUE,
    sort_metric       = "AUC",
    seed              = seed
  )
  
  lb   <- aml@leaderboard
  best <- aml@leader
  aucv <- suppressWarnings(tryCatch(h2o.auc(h2o.performance(best, hex_valid)), error = function(e) NA_real_))
  message("Leader: ", best@algorithm, " | AUC(valid): ", round(aucv, 3))
  
  # predictions on TRAIN / VALID / TEST
  pv <- h2o.predict(best, hex_valid) |> as.data.frame() |> dplyr::bind_cols(species = as.character(v$valid$species))
  pt <- h2o.predict(best, hex_test)  |> as.data.frame() |> dplyr::bind_cols(species = as.character(v$test$species))
  pr <- h2o.predict(best, hex_train) |> as.data.frame() |> dplyr::bind_cols(species = as.character(v$train$species))
  
  # Accuracy summary table (also prints nicely). Returns policy thr numerically.
  thr_policy <- print_acc_summary(
    name = paste0("AUTOML_", toupper(v$name)),
    pred_train = pr,
    pred_valid = pv,
    pred_test  = pt,
    positive   = "SMB",
    clip       = c(0.20, 0.80)
  )
  
  # perf objects
  perf_valid <- h2o.performance(best, hex_valid)
  perf_test  <- h2o.performance(best, hex_test)
  
  # VALID F1 diagnostics + TEST baselines
  thr_v_raw  <- as.numeric(h2o.find_threshold_by_max_metric(perf_valid, "f1"))
  thr_v_clip <- clamp_thr(thr_v_raw, 0.20, 0.80)
  band_v     <- f1_band_from_perf(perf_valid, within = 0.01)
  thr_t_raw  <- as.numeric(h2o.find_threshold_by_max_metric(perf_test,  "f1"))
  acc_test_050  <- as.numeric(h2o.accuracy(perf_test, thresholds = 0.50))
  acc_test_clip <- as.numeric(h2o.accuracy(perf_test, thresholds = thr_v_clip))
  acc_argmax    <- if ("predict" %in% names(pt)) mean(pt$predict == pt$species) else NA_real_
  
  # --- Policy threshold = VALID max-ACC (clipped) and accuracies on all splits
  pc <- pick_prob_col(pv, positive = "SMB")  # probability column name
  # If you’d prefer not to rely on print_acc_summary’s return, uncomment:
  # thr_policy_raw  <- thr_max_acc(truth = pv$species, prob = pv[[pc]], positive = "SMB")
  # thr_policy_clip <- clamp_thr(thr_policy_raw, 0.20, 0.80)
  thr_policy_clip <- as.numeric(thr_policy)     # already clipped in print_acc_summary()
  
  acc_train_policy <- acc_threshold(pr$species, pr[[pc]], thr_policy_clip, positive = "SMB")
  acc_valid_policy <- acc_threshold(pv$species, pv[[pc]], thr_policy_clip, positive = "SMB")
  acc_test_policy  <- acc_threshold(pt$species, pt[[pc]], thr_policy_clip, positive = "SMB")
  
  cat(glue("\nPolicy thr (VALID max-ACC, clipped): {round(thr_policy_clip, 6)}\n",
           "Acc @ policy — TRAIN: {sprintf('%.3f', acc_train_policy)}, ",
           "VALID: {sprintf('%.3f', acc_valid_policy)}, ",
           "TEST: {sprintf('%.3f', acc_test_policy)}\n"))
  cat("Confusion @ policy (TEST)\n"); print(as.data.frame(h2o.confusionMatrix(perf_test, thresholds = thr_policy_clip)))
  
  # simple ROC (TEST)
  pc_test <- pick_prob_col(pt, positive = "SMB")
  truth <- as.integer(pt$species == "SMB")
  score <- pt[[pc_test]]
  o <- order(score, decreasing = TRUE)
  tp <- cumsum(truth[o]); fp <- cumsum(1 - truth[o])
  tpr <- tp / pmax(sum(truth), 1); fpr <- fp / pmax(sum(1 - truth), 1)
  roc_df <- tibble(fpr = c(0, fpr), tpr = c(0, tpr))
  
  fig_path <- here("figures", glue("roc_{v$name}_{ts_tag}.png"))
  p <- ggplot(roc_df, aes(fpr, tpr)) +
    geom_abline(slope = 1, intercept = 0, colour = "grey60") +
    geom_path() + coord_equal() +
    labs(title = glue("AutoML ROC ({v$name}) — SMB positive"),
         x = "False positive rate", y = "True positive rate") +
    theme_classic()
  ggsave(fig_path, p, width = 6.5, height = 5, dpi = 160)
  
  # save artifacts (tables)
  lb_df   <- as.data.frame(lb)
  lb_path <- here("outputs","tables", glue("leaderboard_{v$name}_{ts_tag}.rds"))
  pv_path <- here("outputs","tables", glue("preds_{v$name}_valid_{ts_tag}.rds"))
  pt_path <- here("outputs","tables", glue("preds_{v$name}_test_{ts_tag}.rds"))
  pr_path <- here("outputs","tables", glue("preds_{v$name}_train_{ts_tag}.rds"))
  readr::write_rds(as_tibble(lb_df), lb_path)
  readr::write_rds(as_tibble(pv),    pv_path)
  readr::write_rds(as_tibble(pt),    pt_path)
  readr::write_rds(as_tibble(pr),    pr_path)
  
  # metrics JSON (now includes policy threshold + accuracies)
  metrics <- list(
    variant            = v$name,
    n_train            = nrow(v$train),
    n_valid            = nrow(v$valid),
    n_test             = nrow(v$test),
    roc_png            = fig_path,
    auc_valid          = unname(aucv),
    # VALID F1 diagnostics
    thr_valid_f1_raw   = thr_v_raw,
    thr_valid_f1_clip  = thr_v_clip,
    f1_band_valid_lo   = as.numeric(band_v$lo),
    f1_band_valid_hi   = as.numeric(band_v$hi),
    f1_band_within     = 0.01,
    # TEST F1 raw + baselines
    thr_test_f1_raw    = thr_t_raw,
    acc_test_at_050    = acc_test_050,
    acc_test_at_clip   = acc_test_clip,
    acc_test_argmax    = acc_argmax,
    # Unified policy threshold + accuracies on all splits
    thr_policy_clip    = as.numeric(thr_policy_clip),
    acc_train_at_policy= as.numeric(acc_train_policy),
    acc_valid_at_policy= as.numeric(acc_valid_policy),
    acc_test_at_policy = as.numeric(acc_test_policy),
    positive_class     = "SMB"
  )
  readr::write_file(jsonlite::toJSON(metrics, pretty = TRUE, auto_unbox = TRUE),
                    here("outputs","tables", glue("automl_metrics_{v$name}_{ts_tag}.json")))
  
  # save MOJO
  lb_full <- h2o.get_leaderboard(aml, extra_columns = "ALL")
  vtag <- switch(v$name,
                 "original"        = "bs_orig",
                 "original_blocks" = "bs_blk",
                 paste0("bs_", v$name))
  save_h2o_artifacts(
    model       = best,
    tag         = vtag,
    leaderboard = lb_full,
    train       = hex_train,
    save_binary = TRUE
  )
  invisible(TRUE)
}

# -------- run both variants ---------------------------------------------------
invisible(lapply(variants, run_automl))
message("\nAutoML runs complete. Tables/figures + metrics saved; MOJOs under outputs/models/bs_*.\n")
