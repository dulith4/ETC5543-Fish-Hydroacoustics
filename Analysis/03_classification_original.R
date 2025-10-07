# ORIGINAL per-ping AutoML with policy clamp [0.40, 0.70] — self-contained

rm(list = ls()); invisible(gc())

suppressPackageStartupMessages({
  library(tidyverse); library(h2o); library(glue); library(readr)
  library(here); library(jsonlite); library(lubridate)
})

# shared helpers
source("Analysis/utils_thresholds.R")   # acc_threshold(), thr_max_acc(), clip_thr(), prob_col()
source("Analysis/utils_models.R")      # save_h2o_artifacts()

clamp_thr <- function(t, lo = 0.40, hi = 0.70) pmin(pmax(as.numeric(t), lo), hi)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
dir.create("figures",        recursive = TRUE, showWarnings = FALSE)

# ---- Ensure backscatter per-ping splits exist (build if missing) ------------
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

train_df    <- readRDS(train_rds)
valid_df    <- readRDS(validate_rds)
test_df     <- readRDS(test_rds)

freq_cols <- names(train_df)[stringr::str_detect(names(train_df), "^F\\d+(?:\\.\\d+)?$")]
stopifnot(length(freq_cols) > 0)

# ---- H2O frames --------------------------------------------------------------
h2o.init()

train_h2o <- as.h2o(dplyr::select(train_df, all_of(c("species", freq_cols))))
valid_h2o <- as.h2o(dplyr::select(valid_df, all_of(c("species", freq_cols))))
test_h2o  <- as.h2o(dplyr::select(test_df,  all_of(c("species", freq_cols))))
y <- "species"; x <- freq_cols
train_h2o[[y]] <- h2o.asfactor(train_h2o[[y]])
valid_h2o[[y]] <- h2o.asfactor(valid_h2o[[y]])
test_h2o[[y]]  <- h2o.asfactor(test_h2o[[y]])

# ---- AutoML ------------------------------------------------------------------
aml <- h2o.automl(
  x = x, y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 600,
  nfolds            = 0,
  sort_metric       = "AUC",
  seed              = 73
)
leader    <- aml@leader
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

lb <- as.data.frame(aml@leaderboard)
saveRDS(lb,  glue("outputs/tables/leaderboard_original_{timestamp}.rds"))

# ---- VALID / TEST performance + F1 (clipped to policy window) ----
perf_valid <- h2o.performance(leader, newdata = valid_h2o)
perf_test  <- h2o.performance(leader, newdata = test_h2o)
thr_v_raw  <- as.numeric(h2o.find_threshold_by_max_metric(perf_valid, "f1"))
thr_v_clip <- clamp_thr(thr_v_raw, 0.40, 0.70)
acc_test_050  <- as.numeric(h2o.accuracy(perf_test, thresholds = 0.50))
acc_test_clip <- as.numeric(h2o.accuracy(perf_test, thresholds = thr_v_clip))

# ---- Predictions (TRAIN/VALID/TEST) -----------------------------------------
pred_valid <- as.data.frame(h2o.predict(leader, valid_h2o)) |>
  dplyr::bind_cols(species = as.character(as.data.frame(valid_h2o)$species))
pred_test <- as.data.frame(h2o.predict(leader, test_h2o)) |>
  dplyr::bind_cols(species = as.character(as.data.frame(test_h2o)$species))
pred_train <- as.data.frame(h2o.predict(leader, train_h2o)) |>
  dplyr::bind_cols(species = as.character(as.data.frame(train_h2o)$species))

# Save with names expected by viewer
readr::write_rds(as_tibble(pred_train), glue("outputs/tables/preds_original_train_{timestamp}.rds"))
readr::write_rds(as_tibble(pred_valid), glue("outputs/tables/preds_original_valid_{timestamp}.rds"))
readr::write_rds(as_tibble(pred_test),  glue("outputs/tables/preds_original_{timestamp}.rds"))  # test

# ---- Policy threshold = VALID max-ACC, clamped [0.40, 0.70] -----------------
pc <- prob_col(pred_valid, positive = "SMB")
thr_policy_raw  <- thr_max_acc(truth = pred_valid$species, prob = pred_valid[[pc]], positive = "SMB")
thr_policy_clip <- clamp_thr(thr_policy_raw, 0.40, 0.70)

acc_train_policy <- acc_threshold(pred_train$species, pred_train[[pc]], thr_policy_clip, positive = "SMB")
acc_valid_policy <- acc_threshold(pred_valid$species, pred_valid[[pc]], thr_policy_clip, positive = "SMB")
acc_test_policy  <- acc_threshold(pred_test$species,  pred_test[[pc]],  thr_policy_clip, positive = "SMB")

# ---- Save best model artifacts (MOJO + binary) with metadata -----------------
lb_full <- h2o.get_leaderboard(aml, extra_columns = "ALL")
save_h2o_artifacts(
  model       = leader,
  tag         = "bs_orig",   # per-ping
  leaderboard = lb_full,
  train       = train_h2o,
  save_binary = TRUE,
  extras      = list(policy_thr = thr_policy_clip, clamp = c(0.40, 0.70), positive_class = "SMB")
)

# ---- Metrics JSON (incl. policy) --------------------------------------------
metrics_path <- glue("outputs/tables/automl_metrics_original_{timestamp}.json")
readr::write_file(
  jsonlite::toJSON(list(
    auc_valid            = as.numeric(h2o.auc(perf_valid)),
    auc_test             = as.numeric(h2o.auc(perf_test)),
    thr_valid_f1_raw     = as.numeric(thr_v_raw),
    thr_valid_f1_clip    = as.numeric(thr_v_clip),
    thr_policy_raw       = as.numeric(thr_policy_raw),
    thr_policy_clip      = as.numeric(thr_policy_clip),
    acc_train_at_policy  = as.numeric(acc_train_policy),
    acc_valid_at_policy  = as.numeric(acc_valid_policy),
    acc_test_at_policy   = as.numeric(acc_test_policy),
    acc_test_at_050      = as.numeric(acc_test_050),
    acc_test_at_clip     = as.numeric(acc_test_clip),
    positive_class       = "SMB"
  ), pretty = TRUE, auto_unbox = TRUE),
  metrics_path
)
cat("\nSaved metrics to: ", metrics_path, "\n")
