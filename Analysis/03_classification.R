# AutoML on fish acoustic quintiles with policy clamp [0.40, 0.70]
rm(list = ls()); invisible(gc())

suppressPackageStartupMessages({
  library(tidyverse); library(h2o); library(glue); library(readr)
  library(forcats);  library(here); library(jsonlite)
})

# helpers & savers
source("Analysis/utils_thresholds.R")   # acc_threshold(), thr_max_acc(), clip_thr(), prob_col(), print_acc_summary()
source("Analysis/utils_models.R")       # save_h2o_artifacts()

clamp_thr <- function(t, lo = 0.40, hi = 0.70) pmin(pmax(as.numeric(t), lo), hi)

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/models", showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE)

seed <- 20250904
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# ---------------- Ensure quintile features exist ----------------
quintile_rds <- here("outputs", "tables", "fish_freq_quintiles_long.rds")
builder_r    <- here("Analysis", "02b_fish_quantiles.R")
if (!file.exists(quintile_rds)) {
  message("Quintile file not found — running: ", builder_r)
  source(builder_r, local = TRUE)
  if (!file.exists(quintile_rds)) stop("Expected file not created: ", quintile_rds)
}

# ---------------- Load & split ----------------
dat <- readRDS(quintile_rds) |>
  mutate(
    species  = fct_drop(as.factor(species)),
    fishNum  = as.character(fishNum),
    quantile = as.factor(quantile)
  )
feature_cols <- names(dat)[grepl("^F\\d", names(dat))]
stopifnot(length(feature_cols) > 0)

set.seed(seed)
fish_ids <- dat |> distinct(fishNum, species)

split_species_ids <- function(df_ids, p_train = 0.6, p_valid = 0.2, seed = 1) {
  set.seed(seed)
  vec_ids <- df_ids$fishNum; n <- length(vec_ids); idx <- sample(n)
  n_train <- floor(p_train * n); n_valid <- floor(p_valid * n); n_test  <- n - n_train - n_valid
  if (n >= 3) {
    if (n_train < 1) n_train <- 1
    if (n_valid < 1) n_valid <- 1
    n_test <- n - n_train - n_valid
    if (n_test < 1) { n_test <- 1; n_train <- max(1, n_train - 1) }
  } else {
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
    split   = c(rep("train", length(id_train)), rep("valid", length(id_valid)), rep("test",  length(id_test)))
  )
}
split_map <- fish_ids |> group_split(species) |> purrr::map_dfr(~ split_species_ids(.x, p_train = 0.6, p_valid = 0.2, seed = seed))
dat_s <- dat |> left_join(split_map, by = c("fishNum", "species"))
stopifnot(!any(is.na(dat_s$split)))

cols_keep <- c("species", "fishNum", "quantile", feature_cols)
train_df <- dat_s |> filter(split == "train") |> select(all_of(cols_keep))
valid_df <- dat_s |> filter(split == "valid") |> select(all_of(cols_keep))
test_df  <- dat_s |> filter(split == "test")  |> select(all_of(cols_keep))

# ---------------- H2O lifecycle helpers ----------------
h2o_up <- function() !is.null(tryCatch(h2o.getConnection(), error = function(e) NULL))
start_or_reuse_h2o <- function(heap = "6G") { if (!h2o_up()) h2o.init(nthreads = -1, max_mem_size = heap); invisible(TRUE) }
restart_h2o <- function(heap = "6G") { try(h2o.shutdown(prompt = FALSE), silent = TRUE); Sys.sleep(2); h2o.init(nthreads = -1, max_mem_size = heap); invisible(TRUE) }

# ---------------- One run (pure R data.frames -> H2O) ----------------
run_once <- function(train_df, valid_df, test_df, runtime_secs = 300, seed = 20250904, positive = "SMB") {
  start_or_reuse_h2o("6G"); h2o.removeAll()
  
  # Convert to H2O
  hex_tr <- as.h2o(select(train_df, all_of(c("species", feature_cols))))
  hex_va <- as.h2o(select(valid_df, all_of(c("species", feature_cols))))
  hex_te <- as.h2o(select(test_df,  all_of(c("species", feature_cols))))
  hex_tr[,"species"] <- h2o.asfactor(hex_tr[,"species"])
  hex_va[,"species"] <- h2o.asfactor(hex_va[,"species"])
  hex_te[,"species"] <- h2o.asfactor(hex_te[,"species"])
  
  x <- feature_cols; y <- "species"
  n_train <- nrow(train_df)
  exclude_algos <- c("XGBoost")
  if (n_train < 200) exclude_algos <- unique(c(exclude_algos, "StackedEnsemble"))
  
  message(glue("Training rows: {n_train}. Excluding: {toString(exclude_algos)}"))
  
  aml <- h2o.automl(
    x = x, y = y,
    training_frame    = hex_tr,
    validation_frame  = hex_va,
    leaderboard_frame = hex_te,
    max_runtime_secs  = runtime_secs,
    nfolds            = 0,
    sort_metric       = "AUC",
    seed              = seed,
    exclude_algos     = exclude_algos
  )
  
  leader <- aml@leader
  lb <- as.data.frame(aml@leaderboard)
  saveRDS(lb, glue("outputs/tables/automl_leaderboard_{timestamp}.rds"))
  
  # Metrics (VALID & TEST)
  perf_valid <- h2o.performance(leader, newdata = hex_va)
  perf_test  <- h2o.performance(leader, newdata = hex_te)
  thr_v_raw  <- as.numeric(h2o.find_threshold_by_max_metric(perf_valid, "f1"))
  thr_v_clip <- clamp_thr(thr_v_raw, 0.40, 0.70)
  thr_t_raw  <- as.numeric(h2o.find_threshold_by_max_metric(perf_test,  "f1"))
  acc_test_050  <- as.numeric(h2o.accuracy(perf_test, thresholds = 0.50))
  acc_test_clip <- as.numeric(h2o.accuracy(perf_test, thresholds = thr_v_clip))
  
  # Predictions (+ ids for traceability)
  pred_valid <- as.data.frame(h2o.predict(leader, hex_va))  |> bind_cols(as.data.frame(valid_df)[, c("fishNum","species","quantile")])
  pred_test  <- as.data.frame(h2o.predict(leader, hex_te))  |> bind_cols(as.data.frame(test_df)[,  c("fishNum","species","quantile")])
  pred_train <- as.data.frame(h2o.predict(leader, hex_tr))  |> bind_cols(as.data.frame(train_df)[, c("fishNum","species","quantile")])
  
  # Policy threshold from VALID max-ACC (clipped)
  pc <- prob_col(pred_valid, positive = positive)
  thr_policy_raw  <- thr_max_acc(truth = pred_valid$species, prob = pred_valid[[pc]], positive = positive)
  thr_policy_clip <- clamp_thr(thr_policy_raw, 0.40, 0.70)
  
  acc_train_policy <- acc_threshold(pred_train$species, pred_train[[pc]], thr_policy_clip, positive)
  acc_valid_policy <- acc_threshold(pred_valid$species, pred_valid[[pc]], thr_policy_clip, positive)
  acc_test_policy  <- acc_threshold(pred_test$species,  pred_test[[pc]],  thr_policy_clip, positive)
  
  # Save predictions
  readr::write_rds(as_tibble(pred_valid), glue("outputs/tables/predictions_valid_{timestamp}.rds"))
  readr::write_rds(as_tibble(pred_test),  glue("outputs/tables/predictions_test_{timestamp}.rds"))
  readr::write_rds(as_tibble(pred_train), glue("outputs/tables/predictions_train_{timestamp}.rds"))
  
  # Metrics JSON
  metrics_path <- glue("outputs/tables/automl_metrics_quintiles_{timestamp}.json")
  readr::write_file(
    jsonlite::toJSON(list(
      auc_valid            = as.numeric(h2o.auc(perf_valid)),
      auc_test             = as.numeric(h2o.auc(perf_test)),
      thr_valid_f1_raw     = as.numeric(thr_v_raw),
      thr_valid_f1_clip    = as.numeric(thr_v_clip),
      thr_test_f1_raw      = as.numeric(thr_t_raw),
      thr_policy_raw       = as.numeric(thr_policy_raw),
      thr_policy_clip      = as.numeric(thr_policy_clip),
      acc_train_at_policy  = as.numeric(acc_train_policy),
      acc_valid_at_policy  = as.numeric(acc_valid_policy),
      acc_test_at_policy   = as.numeric(acc_test_policy),
      acc_test_at_050      = as.numeric(acc_test_050),
      acc_test_at_clip     = as.numeric(acc_test_clip),
      positive_class       = positive
    ), pretty = TRUE, auto_unbox = TRUE),
    metrics_path
  )
  cat("\nSaved metrics to: ", metrics_path, "\n")
  
  # Save best model + metadata (MOJO + binary)
  lb_full <- h2o.get_leaderboard(aml, extra_columns = "ALL")
  save_h2o_artifacts(
    model       = leader,
    tag         = "fish_ping",
    leaderboard = lb_full,
    train       = hex_tr,
    save_binary = TRUE,
    extras      = list(policy_thr = thr_policy_clip, clamp = c(0.40, 0.70), positive_class = positive)
  )
  invisible(TRUE)
}

# ---------------- Retry wrapper (handles CURL timeouts cleanly) ---------------
safe_run <- function(...) {
  tryCatch(
    run_once(...),
    error = function(e) {
      msg <- conditionMessage(e)
      message("…caught: ", msg, " — restarting H2O and retrying once.")
      restart_h2o("6G"); h2o.removeAll()
      run_once(...)
    }
  )
}

# ---------------- Go ----------------------------------------------------------
safe_run(train_df, valid_df, test_df, runtime_secs = 300, seed = seed, positive = "SMB")
message("\nQuintiles AutoML complete. Tables/metrics saved; MOJO under outputs/models/fish_ping/.\n")
