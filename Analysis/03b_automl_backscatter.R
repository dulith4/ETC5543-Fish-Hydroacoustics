# ==============================================================================
# 03b_automl_backscatter.R
# AutoML on size-standardised (450 mm) acoustic backscatter features.
# Self-contained: ensures per-ping backscatter splits exist & loads them.
# Policy clamp: [0.40, 0.70]
# ==============================================================================

rm(list = ls()); invisible(gc())

suppressPackageStartupMessages({
  library(tidyverse); library(here); library(lubridate); library(glue)
  library(ggplot2); library(jsonlite); library(h2o)
})

source("Analysis/utils_models.R")
source("Analysis/utils_thresholds.R")   # acc_threshold(), thr_max_acc(), clip_thr(), prob_col()

clamp_thr <- function(t, lo = 0.40, hi = 0.70) pmin(pmax(as.numeric(t), lo), hi)

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE, recursive = TRUE)

ts_tag <- format(with_tz(Sys.time(), "Australia/Melbourne"), "%Y%m%d_%H%M%S")
seed   <- 73

# ---- Ensure backscatter splits exist (build if missing) ----------------------
builder <- here("Analysis","02a_check_transformations.R")
train_rds    <- here("outputs","tables","train_backscatter_450.rds")
validate_rds <- here("outputs","tables","validate_backscatter_450.rds")
test_rds     <- here("outputs","tables","test_backscatter_450.rds")

if (!file.exists(train_rds) || !file.exists(validate_rds) || !file.exists(test_rds)) {
  message("Backscatter files missing — running: ", builder)
  source(builder, local = TRUE)
  if (!file.exists(train_rds) || !file.exists(validate_rds) || !file.exists(test_rds)) {
    stop("Expected backscatter files not created by ", basename(builder))
  }
}

train_bs    <- readRDS(train_rds)
validate_bs <- readRDS(validate_rds)
test_bs     <- readRDS(test_rds)

freq_cols <- names(train_bs)[stringr::str_detect(names(train_bs), "^F\\d+(?:\\.\\d+)?$")]
stopifnot(length(freq_cols) > 0, all(c("Region","species") %in% names(train_bs)))

# ---- Block means (5 pings in order within Region) ----------------------------
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

# ---- Variants: per-ping & block-mean ----------------------------------------
variants <- list(
  list(
    name  = "original",
    train = dplyr::select(train_bs,    dplyr::all_of(freq_cols), species),
    valid = dplyr::select(validate_bs, dplyr::all_of(freq_cols), species),
    test  = dplyr::select(test_bs,     dplyr::all_of(freq_cols), species),
    tag   = "bs_orig"
  ),
  list(
    name  = "original_blocks",
    train = dplyr::select(train_blk, dplyr::all_of(freq_cols), species),
    valid = dplyr::select(valid_blk, dplyr::all_of(freq_cols), species),
    test  = dplyr::select(test_blk,  dplyr::all_of(freq_cols), species),
    tag   = "bs_blk"
  )
)

# ---- H2O ---------------------------------------------------------------------
if (!requireNamespace("h2o", quietly = TRUE)) install.packages("h2o")
h2o.init(nthreads = -1)

pick_prob_col <- function(df, positive = "SMB") {
  if (positive %in% names(df)) return(positive)
  pcols <- names(df)[grepl("^p\\d+$", names(df))]
  if (length(pcols)) return(pcols[1])
  cand <- setdiff(names(df)[grepl("^(p\\d+|LT|SMB|prob_.*)$", names(df))], c("predict","species"))
  if (length(cand)) return(cand[1])
  stop("No probability column found in predictions.")
}

run_automl <- function(v) {
  message("\n=== AutoML variant: ", v$name, " ===")
  
  hex_train <- as.h2o(v$train); hex_valid <- as.h2o(v$valid); hex_test  <- as.h2o(v$test)
  y <- "species"; x <- setdiff(names(v$train), y)
  hex_train[[y]] <- h2o.asfactor(hex_train[[y]]); hex_valid[[y]] <- h2o.asfactor(hex_valid[[y]]); hex_test[[y]] <- h2o.asfactor(hex_test[[y]])
  
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
  
  pv <- h2o.predict(best, hex_valid) |> as.data.frame() |> dplyr::bind_cols(species = as.character(v$valid$species))
  pt <- h2o.predict(best, hex_test)  |> as.data.frame() |> dplyr::bind_cols(species = as.character(v$test$species))
  pr <- h2o.predict(best, hex_train) |> as.data.frame() |> dplyr::bind_cols(species = as.character(v$train$species))
  
  # Policy threshold: VALID max-ACC (clipped)
  pc <- pick_prob_col(pv, positive = "SMB")
  thr_policy_raw  <- thr_max_acc(truth = pv$species, prob = pv[[pc]], positive = "SMB")
  thr_policy_clip <- clamp_thr(thr_policy_raw, 0.40, 0.70)
  
  # VALID-F1 diagnostic (clamped)
  perf_valid <- h2o.performance(best, hex_valid)
  perf_test  <- h2o.performance(best, hex_test)
  thr_v_raw  <- as.numeric(h2o.find_threshold_by_max_metric(perf_valid, "f1"))
  thr_v_clip <- clamp_thr(thr_v_raw, 0.40, 0.70)
  thr_t_raw  <- as.numeric(h2o.find_threshold_by_max_metric(perf_test,  "f1"))
  acc_test_050  <- as.numeric(h2o.accuracy(perf_test, thresholds = 0.50))
  acc_test_clip <- as.numeric(h2o.accuracy(perf_test, thresholds = thr_v_clip))
  acc_argmax    <- if ("predict" %in% names(pt)) mean(pt$predict == pt$species) else NA_real_
  
  acc_train_policy <- acc_threshold(pr$species, pr[[pc]], thr_policy_clip, positive = "SMB")
  acc_valid_policy <- acc_threshold(pv$species, pv[[pc]], thr_policy_clip, positive = "SMB")
  acc_test_policy  <- acc_threshold(pt$species, pt[[pc]], thr_policy_clip, positive = "SMB")
  
  cat(glue("\nPolicy thr (VALID max-ACC, clipped): {round(thr_policy_clip, 6)}\n",
           "Acc @ policy — TRAIN: {sprintf('%.3f', acc_train_policy)}, ",
           "VALID: {sprintf('%.3f', acc_valid_policy)}, ",
           "TEST: {sprintf('%.3f', acc_test_policy)}\n"))
  
  # Save artifacts (tables)
  readr::write_rds(as_tibble(as.data.frame(lb)), here("outputs","tables", glue("leaderboard_{v$name}_{ts_tag}.rds")))
  readr::write_rds(as_tibble(pv), here("outputs","tables", glue("preds_{v$name}_valid_{ts_tag}.rds")))
  readr::write_rds(as_tibble(pt), here("outputs","tables", glue("preds_{v$name}_test_{ts_tag}.rds")))
  readr::write_rds(as_tibble(pr), here("outputs","tables", glue("preds_{v$name}_train_{ts_tag}.rds")))
  
  # Metrics JSON
  metrics <- list(
    variant             = v$name,
    auc_valid           = unname(aucv),
    thr_valid_f1_raw    = thr_v_raw,
    thr_valid_f1_clip   = thr_v_clip,
    thr_test_f1_raw     = thr_t_raw,
    acc_test_at_050     = acc_test_050,
    acc_test_at_clip    = acc_test_clip,
    acc_test_argmax     = acc_argmax,
    thr_policy_raw      = as.numeric(thr_policy_raw),
    thr_policy_clip     = as.numeric(thr_policy_clip),
    acc_train_at_policy = as.numeric(acc_train_policy),
    acc_valid_at_policy = as.numeric(acc_valid_policy),
    acc_test_at_policy  = as.numeric(acc_test_policy),
    positive_class      = "SMB"
  )
  readr::write_file(jsonlite::toJSON(metrics, pretty = TRUE, auto_unbox = TRUE),
                    here("outputs","tables", glue("automl_metrics_{v$name}_{ts_tag}.json")))
  
  # Save MOJO + binary (with extras)
  lb_full <- h2o.get_leaderboard(aml, extra_columns = "ALL")
  save_h2o_artifacts(
    model       = best,
    tag         = v$tag,
    leaderboard = lb_full,
    train       = hex_train,
    save_binary = TRUE,
    extras      = list(policy_thr = thr_policy_clip, clamp = c(0.40, 0.70), positive_class = "SMB")
  )
  invisible(TRUE)
}

invisible(lapply(variants, run_automl))
message("\nAutoML runs complete. Tables/metrics saved; MOJOs under outputs/models/bs_*.\n")
