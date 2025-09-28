# ==============================================================================
# 03_classification_original.R
# PURPOSE
#   Train H2O AutoML on the ORIGINAL (per-ping, wide F45–F170) structure.
#   - 60/20/20 split by fishNum (grouped), stratified by species
#   - 5-fold CV (folds grouped by fishNum via fold_column)
#   - Algorithms: GBM, DRF, GLM, DeepLearning
#   - Save leaderboard, test predictions, best model, and ROC
#
# RUN THIS AT LEAST ONCE
#   Generates artifacts in outputs/ that the viewer script will read later:
#     source("Analysis/view_results_automl.R")
#
# INPUTS
#   - data/TSresponse_clean.RDS  (loaded via Analysis/utils_data.R)
#     Expect columns: species, fishNum, and F45…F170 (incl. decimals like F45.5)
#
# OUTPUTS (timestamped)
#   - outputs/tables/leaderboard_original_YYYYMMDD_HHMMSS.rds
#   - outputs/tables/preds_original_YYYYMMDD_HHMMSS.rds
#   - figures/roc_original_YYYYMMDD_HHMMSS.png
#   - outputs/models/best_original/YYYYMMDD_HHMMSS/  (H2O model files)
#
# NOTES
#   - Target = species (SMB vs LT); predictors = all F45–F170 columns only.
#   - Leaderboard is sorted by AUC; test AUC + ROC saved.
#   - Large artifacts are .gitignored; each collaborator should run this locally.
# ==============================================================================

# ---- 0) Clean & setup ----
rm(list = ls())                         # clean workspace
invisible(gc())                         # free memory
options(stringsAsFactors = FALSE)       # keep strings as characters

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(glue)
  library(lubridate)
  library(readr)
  library(rsample)     # only for helpers; splits are custom below
  library(h2o)
})

# Project utils (loads + cached transform)
source("Analysis/utils_data.R")

# Ensure output dirs exist
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/models/best_original", recursive = TRUE, showWarnings = FALSE)

# Timestamp for artifacts
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")

# ---- 1) Load ORIGINAL-long data (F45:F170 present here) ----
fish_raw <- load_fish_transformed(
  raw_path   = "data/TSresponse_clean.RDS",
  use_cache  = TRUE,
  cache_path = "outputs/cache/TS_clean_transformed.rds"
)

# Expect columns at least: species, fishNum, and F45:F170
stopifnot(all(c("species","fishNum") %in% names(fish_raw)))

# ---- Identify frequency columns (F45–F170 incl. decimals like F45.5) ----
# Build from fish_raw (exists now); then build df using those columns.
freq_tbl <- tibble(name = names(fish_raw)) |>
  filter(str_detect(name, "^F\\d+(\\.\\d+)?$")) |>
  mutate(freq = as.numeric(str_remove(name, "^F"))) |>
  filter(!is.na(freq), freq >= 45, freq <= 170) |>
  arrange(freq)

f_cols <- freq_tbl$name
if (length(f_cols) == 0) stop("No F45–F170 columns found.")

# Keep only what we need; drop any rows missing species/fishNum
df <- fish_raw |>
  select(all_of(c("species","fishNum", f_cols))) |>
  filter(!is.na(species), !is.na(fishNum))

# Predictors are the frequency columns
x_cols <- f_cols

# Sanity checks
stopifnot(all(c("species","fishNum") %in% names(df)))
stopifnot(length(x_cols) > 0, all(x_cols %in% names(df)))

# ---- 2) Grouped 60/20/20 split at fishNum, stratified by species ----
set.seed(1234)

# helper: stratified ID split (by species), proportions ~ 60/20/20
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

# ---- 3) 5-fold CV folds grouped by fishNum (for training only) ----
nfolds <- 5
# stable fold assignment per fish (training only) — numeric 0..K-1 is fine for H2O
fold_map <- train_df |>
  distinct(fishNum) |>
  mutate(cv_fold = sample(seq(0, nfolds - 1), size = n(), replace = TRUE))

train_df <- train_df |>
  left_join(fold_map, by = "fishNum")

# ---- 4) H2O init & frames ----
# If you previously had connection drops, ensure enough memory and do NOT pass leaderboard_frame.
# ---- 4) H2O init & frames ----
h2o.no_progress()                                # keep console minimal
h2o.init(nthreads = -1, max_mem_size = "12G")    # adjust to your machine

train_h2o <- as.h2o(train_df)
valid_h2o <- as.h2o(valid_df)
test_h2o  <- as.h2o(test_df)

# response factor
for (frm in list(train_h2o, valid_h2o, test_h2o)) {
  frm[,"species"] <- h2o.asfactor(frm[,"species"])
}

# ---- 5) AutoML (>=1200s), leaderboard by AUC, 5-fold CV with fold_column ----
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
  keep_cross_validation_models        = FALSE,
  keep_cross_validation_predictions   = FALSE,
  keep_cross_validation_fold_assignment = FALSE,
  project_name    = paste0("fish_original_", ts),
  seed            = 1234
)

leader    <- aml@leader
# Threshold from cross-validated metrics (since we didn't pass a validation_frame)
perf_xval <- h2o.performance(leader, xval = TRUE)
thr_v     <- h2o.find_threshold_by_max_metric(perf_xval, "f1")

# Test evaluation
perf_test <- h2o.performance(leader, newdata = test_h2o)
auc_test  <- h2o.auc(perf_test); auc_test



# ---- 7) Save leaderboard, best model ----
lb <- as_tibble(aml@leaderboard)
write_rds(lb, glue("outputs/tables/leaderboard_original_{ts}.rds"))

model_dir <- file.path("outputs","models","best_original", ts)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
h2o.saveModel(leader, path = model_dir, force = TRUE)

# ---- 8) Predictions on TEST (with class label) ----
pred_h2o <- h2o.predict(leader, test_h2o)
preds <- as_tibble(pred_h2o) |>
  mutate(row_id = row_number())

test_bind <- test_df |>
  mutate(row_id = row_number()) |>
  select(row_id, fishNum, species)

preds_out <- test_bind |>
  left_join(preds, by = "row_id") |>
  rename(pred_label = predict) |>
  select(-row_id)

write_rds(preds_out, glue("outputs/tables/preds_original_{ts}.rds"))

# ---- 9) ROC curve (TEST) ----
m <- h2o.metric(perf_test)   # includes fpr, tpr, thresholds, etc.
roc_df <- as_tibble(m) |> select(fpr, tpr)

p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_path(linewidth = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  coord_equal() +
  labs(
    title = glue("ROC — ORIGINAL (test AUC = {round(auc_test, 3)})"),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal(base_size = 12)

ggsave(filename = glue("figures/roc_original_{ts}.png"),
       plot = p_roc, width = 6, height = 6, dpi = 300)

# ---- 10) Minimal console snapshot ----
cm_test <- h2o.confusionMatrix(perf_test, thresholds = thr_v)
cat(glue("\n==== ORIGINAL structure (TEST) ====\n",
         "AUC: {round(auc_test, 3)}\n",
         "Threshold (max F1 from VALID): {round(thr_v, 6)}\n"))
print(cm_test)

# Done
# h2o.shutdown(prompt = FALSE)  # uncomment if you want to stop H2O after run
