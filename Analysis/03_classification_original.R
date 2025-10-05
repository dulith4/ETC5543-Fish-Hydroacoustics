# ==============================================================================
# 03_classification_original.R
# PURPOSE
#   Train H2O AutoML on the ORIGINAL (per-ping, wide F45–F170) structure.
#   - 60/20/20 split by fishNum (grouped), stratified by species
#   - 5-fold CV (folds grouped by fishNum via fold_column)
#   - Algorithms: GBM, DRF, GLM, DeepLearning
#   - Save leaderboard, test predictions, best model, ROC, and a metrics JSON
#   - Robust thresholding:
#       * CV F1 raw + clipped [0.20,0.80] and near-optimal F1 band
#       * Single operating threshold (“policy”) = VALID max-ACC, clipped [0.20,0.80]
#       * Accuracies at policy for TRAIN / VALID / TEST
# ==============================================================================

# ---- 0) Clean & setup ----
rm(list = ls())
invisible(gc())
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(glue)
  library(lubridate)
  library(readr)
  library(rsample)
  library(h2o)
  library(jsonlite)
  library(ggplot2)
})

# --- helpers ------------------------------------------------------------------
clamp_thr <- function(t, lo = 0.20, hi = 0.80) pmin(pmax(as.numeric(t), lo), hi)

f1_band_from_perf <- function(perf, within = 0.01) {
  df <- try(as.data.frame(h2o.metric(perf)), silent = TRUE)
  if (inherits(df, "try-error") || !"threshold" %in% names(df) || !"f1" %in% names(df)) {
    return(list(lo = NA_real_, hi = NA_real_, f1_max = NA_real_))
  }
  df <- df[is.finite(df$f1) & is.finite(df$threshold), , drop = FALSE]
  if (!nrow(df)) return(list(lo = NA_real_, hi = NA_real_, f1_max = NA_real_))
  fmax <- max(df$f1, na.rm = TRUE)
  band <- df$threshold[df$f1 >= (1 - within) * fmax]
  list(lo = min(band, na.rm = TRUE), hi = max(band, na.rm = TRUE), f1_max = fmax)
}


# Project utils (data + threshold helpers)
source("Analysis/utils_data.R")
source("Analysis/utils_thresholds.R")  # defines: acc_threshold(), thr_max_acc(), print_acc_summary()

# Ensure output dirs exist
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/models/best_original", recursive = TRUE, showWarnings = FALSE)

# Timestamp for artifacts
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")

# ---- 1) Load ORIGINAL-long data (F45:F170 present here) ----
fish_raw <- load_fish_transformed(
  raw_path   = "data/TSresponse_clean.RDS",
  use_cache  = TRUE,
  cache_path = "outputs/cache/TS_clean_transformed.rds"
)
stopifnot(all(c("species","fishNum") %in% names(fish_raw)))

# Identify frequency columns (F45–F170 incl. decimals like F45.5)
freq_tbl <- tibble(name = names(fish_raw)) |>
  filter(str_detect(name, "^F\\d+(\\.\\d+)?$")) |>
  mutate(freq = as.numeric(str_remove(name, "^F"))) |>
  filter(!is.na(freq), freq >= 45, freq <= 170) |>
  arrange(freq)

f_cols <- freq_tbl$name
if (length(f_cols) == 0) stop("No F45–F170 columns found.")

# Keep only needed columns; drop rows missing species/fishNum
df <- fish_raw |>
  select(all_of(c("species","fishNum", f_cols))) |>
  filter(!is.na(species), !is.na(fishNum))

x_cols <- f_cols
stopifnot(all(c("species","fishNum") %in% names(df)))
stopifnot(length(x_cols) > 0, all(x_cols %in% names(df)))

# ---- 2) Grouped 60/20/20 split at fishNum, stratified by species ----
set.seed(1234)
split_ids <- function(data, id_col = "fishNum", target = "species",
                      prop = c(train = 0.6, valid = 0.2, test = 0.2), seed = 1234) {
  stopifnot(abs(sum(prop) - 1) < 1e-8)
  set.seed(seed)
  ids <- data |>
    distinct(.data[[id_col]], .data[[target]]) |>
    rename(id = {{ id_col }}, y = {{ target }})
  split_list <- ids |>
    group_by(y) |>
    group_map(~{
      n <- nrow(.x)
      n_train <- floor(prop["train"] * n)
      n_valid <- floor(prop["valid"] * n)
      idx <- sample(seq_len(n))
      id_train <- .x$id[ idx[1:n_train] ]
      id_valid <- .x$id[ idx[(n_train+1):(n_train+n_valid)] ]
      id_test  <- .x$id[ idx[(n_train+n_valid+1):n] ]
      tibble(
        split = c(rep("train", length(id_train)),
                  rep("valid", length(id_valid)),
                  rep("test",  length(id_test))),
        id = c(id_train, id_valid, id_test)
      )
    }) |>
    list_rbind()
  out <- split_list |>
    left_join(ids, by = c("id" = "id")) |>
    select(id, y, split)
  list(
    train = out |> filter(split == "train") |> pull(id),
    valid = out |> filter(split == "valid") |> pull(id),
    test  = out |> filter(split == "test")  |> pull(id)
  )
}
id_splits <- split_ids(df, id_col = "fishNum", target = "species",
                       prop = c(train = 0.6, valid = 0.2, test = 0.2), seed = 1234)

train_df <- df |> filter(fishNum %in% id_splits$train)
valid_df <- df |> filter(fishNum %in% id_splits$valid)
test_df  <- df |> filter(fishNum %in% id_splits$test)

# ---- 3) 5-fold CV folds grouped by fishNum (training only) ----
nfolds <- 5
fold_map <- train_df |>
  distinct(fishNum) |>
  mutate(cv_fold = sample(seq(0, nfolds - 1), size = n(), replace = TRUE))
train_df <- train_df |> left_join(fold_map, by = "fishNum")

# ---- 4) H2O init & frames ----
h2o.no_progress()
h2o.init(nthreads = -1, max_mem_size = "12G")

train_h2o <- as.h2o(train_df)
valid_h2o <- as.h2o(valid_df)
test_h2o  <- as.h2o(test_df)
for (frm in list(train_h2o, valid_h2o, test_h2o)) frm[,"species"] <- h2o.asfactor(frm[,"species"])

# ---- 5) AutoML (AUC), 5-fold CV with fold_column on training data ----
aml <- h2o.automl(
  x = x_cols, y = "species",
  training_frame  = train_h2o,
  include_algos   = c("GBM","DRF","GLM","DeepLearning"),
  sort_metric     = "AUC",
  stopping_metric = "AUC",
  stopping_rounds = 5,
  stopping_tolerance = 1e-3,
  nfolds          = nfolds,
  fold_column     = "cv_fold",
  max_runtime_secs= 1200,
  keep_cross_validation_models          = FALSE,
  keep_cross_validation_predictions     = FALSE,
  keep_cross_validation_fold_assignment = FALSE,
  project_name    = paste0("fish_original_", ts),
  seed            = 1234
)
leader <- aml@leader

# ---- 6) CV perf + F1 thresholds/band ----------------------------------------
perf_xval  <- h2o.performance(leader, xval = TRUE)
thr_v_raw  <- as.numeric(h2o.find_threshold_by_max_metric(perf_xval, "f1"))
thr_v_clip <- clamp_thr(thr_v_raw, 0.20, 0.80)
band_cv    <- f1_band_from_perf(perf_xval, within = 0.01)

# ---- 7) TEST perf (AUC, baseline accs) --------------------------------------
perf_test <- h2o.performance(leader, newdata = test_h2o)
auc_test  <- as.numeric(h2o.auc(perf_test))
thr_t_raw <- as.numeric(h2o.find_threshold_by_max_metric(perf_test, "f1"))
acc_test_050  <- as.numeric(h2o.accuracy(perf_test, thresholds = 0.50))
acc_test_clip <- as.numeric(h2o.accuracy(perf_test, thresholds = thr_v_clip))

# ---- 8) Predictions (TEST + VALID + TRAIN) ----------------------------------
pred_h2o <- h2o.predict(leader, test_h2o)
preds <- as_tibble(pred_h2o) |> mutate(row_id = row_number())
test_bind <- test_df |> mutate(row_id = row_number()) |> select(row_id, fishNum, species)
preds_out <- test_bind |> left_join(preds, by = "row_id") |> rename(pred_label = predict) |> select(-row_id)
write_rds(preds_out, glue("outputs/tables/preds_original_{ts}.rds"))

pred_valid_h2o <- h2o.predict(leader, valid_h2o) |> as.data.frame()
pred_train_h2o <- h2o.predict(leader, train_h2o) |> as.data.frame()
pred_valid <- pred_valid_h2o |>
  mutate(row_id = row_number()) |>
  left_join(valid_df |> mutate(row_id = row_number()) |> select(row_id, fishNum, species), by = "row_id") |>
  rename(pred_label = predict) |>
  select(-row_id)
pred_train <- pred_train_h2o |>
  mutate(row_id = row_number()) |>
  left_join(train_df |> mutate(row_id = row_number()) |> select(row_id, fishNum, species), by = "row_id") |>
  rename(pred_label = predict) |>
  select(-row_id)
write_rds(pred_valid, glue("outputs/tables/preds_original_valid_{ts}.rds"))
write_rds(pred_train, glue("outputs/tables/preds_original_train_{ts}.rds"))

# ---- 9) Single operating threshold (“policy”) from VALID ---------------------
pc <- prob_col(pred_valid, positive = "SMB")
thr_policy_raw  <- thr_max_acc(truth = pred_valid$species, prob = pred_valid[[pc]], positive = "SMB")
thr_policy_clip <- clamp_thr(thr_policy_raw, 0.20, 0.80)

# Accuracies at the policy threshold
acc_train_policy <- acc_threshold(pred_train$species, pred_train[[pc]], thr_policy_clip, positive = "SMB")
acc_valid_policy <- acc_threshold(pred_valid$species, pred_valid[[pc]], thr_policy_clip, positive = "SMB")
acc_test_policy  <- acc_threshold(preds_out$species, preds_out[[pc]],  thr_policy_clip, positive = "SMB")

# Optional console summary (and returns the policy threshold)
invisible(print_acc_summary(
  name       = "ORIGINAL",
  pred_train = pred_train,
  pred_valid = pred_valid,
  pred_test  = preds_out,
  positive   = "SMB",
  clip       = c(0.20, 0.80)
))

# ---- 10) ROC curve (TEST) ----------------------------------------------------
m <- h2o.metric(perf_test)
roc_df <- as_tibble(m) |> select(fpr, tpr)
p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_path(linewidth = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  coord_equal() +
  labs(
    title = glue("ROC — ORIGINAL (test AUC = {round(auc_test, 3)})"),
    x = "False Positive Rate", y = "True Positive Rate"
  ) + theme_minimal(base_size = 12)
ggsave(filename = glue("figures/roc_original_{ts}.png"),
       plot = p_roc, width = 6, height = 6, dpi = 300)

# ---- 11) Console snapshot ----------------------------------------------------
cat(glue("\n==== ORIGINAL structure (TEST) ====\n",
         "AUC: {round(auc_test, 3)}\n",
         "F1-opt (CV) raw: {round(thr_v_raw, 6)} | clipped[0.2,0.8]: {round(thr_v_clip, 6)}\n",
         "Near-opt F1 band (CV, ±1%): [{signif(band_cv$lo,6)}, {signif(band_cv$hi,6)}]\n",
         "Acc @ 0.50: {sprintf('%.3f', acc_test_050)} | Acc @ clipped: {sprintf('%.3f', acc_test_clip)}\n",
         "Policy thr (VALID max-ACC) raw: {round(thr_policy_raw,6)} | clipped: {round(thr_policy_clip,6)}\n",
         "Acc @ policy — TRAIN: {sprintf('%.3f', acc_train_policy)}, VALID: {sprintf('%.3f', acc_valid_policy)}, TEST: {sprintf('%.3f', acc_test_policy)}\n"))
cm_050    <- h2o.confusionMatrix(perf_test, thresholds = 0.50)
cm_clip   <- h2o.confusionMatrix(perf_test, thresholds = thr_v_clip)
cm_policy <- h2o.confusionMatrix(perf_test, thresholds = thr_policy_clip)
cat("\nConfusion @ 0.50\n");              print(cm_050)
cat("\nConfusion @ clipped CV-F1\n");    print(cm_clip)
cat("\nConfusion @ policy (VALID max-ACC)\n"); print(cm_policy)

# ---- 12) Metrics JSON (AFTER policy is computed!) ----------------------------
metrics <- list(
  auc_test            = auc_test,
  # CV-F1 diagnostics
  thr_cv_f1_raw       = thr_v_raw,
  thr_cv_f1_clip      = thr_v_clip,
  f1_band_cv_lo       = as.numeric(band_cv$lo),
  f1_band_cv_hi       = as.numeric(band_cv$hi),
  f1_band_within      = 0.01,
  # TEST F1 raw (for reference)
  thr_test_f1_raw     = thr_t_raw,
  # Single operating threshold + accuracies
  thr_policy_raw      = thr_policy_raw,
  thr_policy_clip     = thr_policy_clip,
  acc_train_at_policy = acc_train_policy,
  acc_valid_at_policy = acc_valid_policy,
  acc_test_at_policy  = acc_test_policy,
  # Baseline accuracies on TEST
  acc_test_at_050     = acc_test_050,
  acc_test_at_clip    = acc_test_clip,
  # Back-compat for viewers that expect 'thr_cv_f1'
  thr_cv_f1           = thr_v_clip
)
readr::write_file(
  jsonlite::toJSON(metrics, pretty = TRUE, auto_unbox = TRUE),
  glue("outputs/tables/automl_metrics_original_{ts}.json")
)

# ---- 13) Save leaderboard, best model ----------------------------------------
lb <- as_tibble(aml@leaderboard)
write_rds(lb, glue("outputs/tables/leaderboard_original_{ts}.rds"))
model_dir <- file.path("outputs","models","best_original", ts)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
h2o.saveModel(leader, path = model_dir, force = TRUE)

# ---- 14) Save model bundle (MOJO etc.) ---------------------------------------
source("Analysis/utils_models.R")
lb_full <- h2o.get_leaderboard(aml, extra_columns = "ALL")
best    <- best_from_aml(aml)
save_h2o_artifacts(best, tag = "original", leaderboard = lb_full,
                   train = train_h2o, save_binary = TRUE)

# h2o.shutdown(prompt = FALSE)  # optional
