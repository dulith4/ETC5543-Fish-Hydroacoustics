# ==============================================================================
# 03_classification.R
# PURPOSE
#   AutoML on fish acoustic data summarised by quintiles.
#   Robust thresholding:
#     * CV/VALID F1 raw + clipped [0.20, 0.80] and near-opt F1 band
#     * Single operating threshold (“policy”) = VALID max-ACC, clipped [0.20, 0.80]
#     * Accuracies at policy for TRAIN / VALID / TEST
# ==============================================================================

# ---- 0. Setup ----
rm(list = ls())
seed <- 20250904

suppressPackageStartupMessages({
  library(tidyverse); library(h2o); library(glue); library(readr)
  library(forcats); library(here); library(jsonlite)
})

# ---- helpers -----------------------------------------------------------
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

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/models", showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE)

utils_path <- "Analysis/utils_data.R"
if (file.exists(utils_path)) source(utils_path)
source("Analysis/utils_thresholds.R")   # acc_threshold(), thr_max_acc(), print_acc_summary()

# Ensure required quintile file exists (build if missing)
quintile_rds <- here("outputs", "tables", "fish_freq_quintiles_long.rds")
builder_r    <- here("Analysis", "02b_fish_quantiles.R")
if (!file.exists(quintile_rds)) {
  message("Quintile file not found — running: ", builder_r)
  source(builder_r, local = TRUE)
  if (!file.exists(quintile_rds)) stop("Expected file not created: ", quintile_rds)
}

# ---- 1. Load transformed features ----
path_features <- quintile_rds
dat <- readRDS(path_features) |>
  mutate(
    species  = fct_drop(as.factor(species)),
    fishNum  = as.character(fishNum),
    quantile = as.factor(quantile)
  )
feature_cols <- names(dat)[grepl("^F\\d", names(dat))]
stopifnot(length(feature_cols) > 0)

# ---- 2. Fish-level, species-stratified split (60/20/20) ----
set.seed(seed)
fish_ids <- dat |> distinct(fishNum, species)

split_species_ids <- function(df_ids, p_train = 0.6, p_valid = 0.2, seed = 1) {
  set.seed(seed)
  vec_ids <- df_ids$fishNum
  n <- length(vec_ids)
  idx <- sample(n)
  
  # base allocations
  n_train <- floor(p_train * n)
  n_valid <- floor(p_valid * n)
  n_test  <- n - n_train - n_valid
  
  # ensure at least 1 per split when possible
  if (n >= 3) {
    if (n_train < 1) n_train <- 1
    if (n_valid < 1) n_valid <- 1
    n_test <- n - n_train - n_valid
    if (n_test < 1) { n_test <- 1; n_train <- max(1, n_train - 1) }
  } else {
    # tiny species groups: shove remainder into test
    n_train <- max(1, min(1, n))
    n_valid <- if (n >= 2) 1 else 0
    n_test  <- max(0, n - n_train - n_valid)
  }
  
  id_train <- vec_ids[idx[seq_len(n_train)]]
  id_valid <- if (n_valid > 0) vec_ids[idx[seq(n_train + 1, length.out = n_valid)]] else character(0)
  id_test  <- if (n_test  > 0) vec_ids[idx[seq(n_train + n_valid + 1, length.out = n_test)]] else character(0)
  
  tibble(
    fishNum = c(id_train, id_valid, id_test),
    species = df_ids$species[1],
    split   = c(
      rep("train", length(id_train)),
      rep("valid", length(id_valid)),
      rep("test",  length(id_test))
    )
  )
}

split_map <- fish_ids |>
  group_split(species) |>
  purrr::map_dfr(~ split_species_ids(.x, p_train = 0.6, p_valid = 0.2, seed = seed))

# IMPORTANT: join by BOTH keys to avoid many-to-many problems
dat_s <- dat |> left_join(split_map, by = c("fishNum", "species"))
stopifnot(!any(is.na(dat_s$split)))

# ---- 3. Prep H2O frames ----
h2o.init()
cols_keep <- c("species", "fishNum", "quantile", feature_cols)
train_df <- dat_s |> filter(split == "train") |> select(all_of(cols_keep))
valid_df <- dat_s |> filter(split == "valid") |> select(all_of(cols_keep))
test_df  <- dat_s |> filter(split == "test")  |> select(all_of(cols_keep))

train_h2o <- as.h2o(train_df)
valid_h2o <- as.h2o(valid_df)
test_h2o  <- as.h2o(test_df)
y <- "species"; x <- feature_cols
train_h2o[[y]] <- as.factor(train_h2o[[y]])
valid_h2o[[y]] <- as.factor(valid_h2o[[y]])
test_h2o[[y]]  <- as.factor(test_h2o[[y]])

# ---- 4. AutoML ----
runtime_secs <- 300
aml <- h2o.automl(
  x = x, y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = runtime_secs,
  nfolds            = 0,
  sort_metric       = "AUC",
  seed              = seed
)
leader <- aml@leader
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Save leaderboard
lb <- as.data.frame(aml@leaderboard)
write_csv(lb, glue("outputs/tables/automl_leaderboard_{timestamp}.csv"))
saveRDS(lb,  glue("outputs/tables/automl_leaderboard_{timestamp}.rds"))

# ---- 5. Key metrics (VALID & TEST) with robust thresholding ----
perf_valid <- h2o.performance(leader, newdata = valid_h2o)
perf_test  <- h2o.performance(leader, newdata = test_h2o)

thr_v_raw  <- as.numeric(h2o.find_threshold_by_max_metric(perf_valid, "f1"))
thr_v_clip <- clamp_thr(thr_v_raw, 0.20, 0.80)
band_v     <- f1_band_from_perf(perf_valid, within = 0.01)

thr_t_raw  <- as.numeric(h2o.find_threshold_by_max_metric(perf_test,  "f1"))
acc_test_050  <- as.numeric(h2o.accuracy(perf_test, thresholds = 0.50))
acc_test_clip <- as.numeric(h2o.accuracy(perf_test, thresholds = thr_v_clip))

cat("\n==== VALIDATION ====\n")
cat("AUC: ", h2o.auc(perf_valid), "\n", sep = "")
cat("F1-opt raw: ", thr_v_raw, " | clipped[0.2,0.8]: ", thr_v_clip, "\n", sep = "")
cat("Near-opt F1 band (±1%): [", signif(band_v$lo,6), ", ", signif(band_v$hi,6), "]\n", sep = "")

cat("\n==== TEST ====\n")
cat("AUC: ", h2o.auc(perf_test), "\n", sep = "")
cat("Acc @ 0.50: ", sprintf('%.4f', acc_test_050), " | Acc @ clipped: ", sprintf('%.4f', acc_test_clip), "\n", sep = "")
cat("Confusion @ clipped VALID-F1\n"); print(as.data.frame(h2o.confusionMatrix(perf_test, thresholds = thr_v_clip)))

# ---- 6. Predictions (saved, not printed) ----
pred_valid <- as.data.frame(h2o.predict(leader, valid_h2o)) |>
  bind_cols(as.data.frame(valid_h2o)[, c("fishNum", "species", "quantile")])
pred_test <- as.data.frame(h2o.predict(leader, test_h2o)) |>
  bind_cols(as.data.frame(test_h2o)[, c("fishNum", "species", "quantile")])
pred_train <- as.data.frame(h2o.predict(leader, train_h2o)) |>
  bind_cols(as.data.frame(train_h2o)[, c("fishNum", "species", "quantile")])

# ---- 7. Single operating threshold (“policy”) from VALID max-ACC ------------
pc <- prob_col(pred_valid, positive = "SMB")
thr_policy_raw  <- thr_max_acc(truth = pred_valid$species, prob = pred_valid[[pc]], positive = "SMB")
thr_policy_clip <- clamp_thr(thr_policy_raw, 0.20, 0.80)

acc_train_policy <- acc_threshold(pred_train$species, pred_train[[pc]], thr_policy_clip, positive = "SMB")
acc_valid_policy <- acc_threshold(pred_valid$species, pred_valid[[pc]], thr_policy_clip, positive = "SMB")
acc_test_policy  <- acc_threshold(pred_test$species,  pred_test[[pc]],  thr_policy_clip, positive = "SMB")

acc_train_argmax <- mean(pred_train$predict == pred_train$species)
acc_valid_argmax <- mean(pred_valid$predict == pred_valid$species)
acc_test_argmax  <- mean(pred_test$predict  == pred_test$species)

# Also print the summary table (and keep the returned policy thr)
invisible(print_acc_summary(
  name       = "QUINTILES",
  pred_train = pred_train,
  pred_valid = pred_valid,
  pred_test  = pred_test,
  positive   = "SMB",
  clip       = c(0.20, 0.80)   # <- unified clip range
))

cat(glue("\nPolicy thr (VALID max-ACC) raw: {round(thr_policy_raw,6)} | clipped: {round(thr_policy_clip,6)}\n",
         "Acc @ policy — TRAIN: {sprintf('%.3f', acc_train_policy)}, VALID: {sprintf('%.3f', acc_valid_policy)}, TEST: {sprintf('%.3f', acc_test_policy)}\n",
         "Argmax accuracy — TRAIN: {sprintf('%.3f', acc_train_argmax)}, VALID: {sprintf('%.3f', acc_valid_argmax)}, TEST: {sprintf('%.3f', acc_test_argmax)}\n"))
cat("Confusion @ policy (TEST)\n"); print(as.data.frame(h2o.confusionMatrix(perf_test, thresholds = thr_policy_clip)))

# ---- 8. Save predictions ----
write_csv(pred_valid, glue("outputs/tables/predictions_valid_{timestamp}.csv"))
saveRDS(pred_valid, glue("outputs/tables/predictions_valid_{timestamp}.rds"))
write_csv(pred_test,  glue("outputs/tables/predictions_test_{timestamp}.csv"))
saveRDS(pred_test,   glue("outputs/tables/predictions_test_{timestamp}.rds"))
write_rds(pred_train, glue("outputs/tables/predictions_train_{timestamp}.rds"))  

# ---- 9. Save best model ----
saved_path <- h2o.saveModel(leader, path = "outputs/models", force = TRUE)
invisible(h2o.download_mojo(leader, path = "outputs/models", get_genmodel_jar = TRUE))
cat(glue("\nSaved model: {saved_path}\n"))

# ---- 10. Save artifacts via utility + metrics JSON (includes policy) ---------
source("Analysis/utils_models.R")
lb_full <- h2o.get_leaderboard(aml, extra_columns = "ALL")
best    <- best_from_aml(aml)
save_h2o_artifacts(best, tag = "fish_ping", leaderboard = lb_full, train = train_h2o, save_binary = TRUE)

metrics_path <- glue("outputs/tables/automl_metrics_quintiles_{timestamp}.json")
readr::write_file(
  jsonlite::toJSON(list(
    auc_valid            = as.numeric(h2o.auc(perf_valid)),
    auc_test             = as.numeric(h2o.auc(perf_test)),
    # VALID F1 diagnostics
    thr_valid_f1_raw     = as.numeric(thr_v_raw),
    thr_valid_f1_clip    = as.numeric(thr_v_clip),
    f1_band_valid_lo     = as.numeric(band_v$lo),
    f1_band_valid_hi     = as.numeric(band_v$hi),
    f1_band_within       = 0.01,
    # TEST F1 raw (for reference)
    thr_test_f1_raw      = as.numeric(thr_t_raw),
    # Unified policy threshold + accuracies
    thr_policy_raw       = as.numeric(thr_policy_raw),
    thr_policy_clip      = as.numeric(thr_policy_clip),
    acc_train_at_policy  = as.numeric(acc_train_policy),
    acc_valid_at_policy  = as.numeric(acc_valid_policy),
    acc_test_at_policy   = as.numeric(acc_test_policy),
    # Baselines
    acc_test_at_050      = as.numeric(acc_test_050),
    acc_test_at_clip     = as.numeric(acc_test_clip),
    # Argmax (for reference)
    acc_train_argmax     = as.numeric(acc_train_argmax),
    acc_valid_argmax     = as.numeric(acc_valid_argmax),
    acc_test_argmax      = as.numeric(acc_test_argmax),
    positive_class       = "SMB"
  ), pretty = TRUE, auto_unbox = TRUE),
  metrics_path
)
