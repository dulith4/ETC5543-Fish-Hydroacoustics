# ==============================================================================
# 07_oof_threshold_tuning.R
# Purpose: Out-of-fold (OOF) threshold tuning for the four tsfeatures variants.
# - Reads the same data sources used by 05_automl_tsfeatures.R
# - Builds a grouped 5-fold CV by fishNum (seed=73) on TRAIN only
# - For each variant:
#     * Infers a single-model algorithm from the latest AutoML leaderboard
#       (skips StackedEnsemble; falls back to GBM if needed)
#     * Trains per-fold models, collects OOF probabilities (SMB)
#     * Finds OOF max-accuracy threshold, clamps to [0.40, 0.70]
#     * Trains a final single model on full TRAIN, evaluates TEST @ OOF thr
#     * Saves JSON + TEST confusion CSV
# - Skips variants whose .rds is missing
#
# Requirements:
#   - Positive class is "SMB"
#   - Policy clamp window: [0.40, 0.70]
#   - H2O 3.46.0.7 (Windows, R 4.5.1)
# ==============================================================================

rm(list = ls()); invisible(gc())

suppressPackageStartupMessages({
  library(tidyverse); library(here); library(glue); library(jsonlite)
  library(h2o); library(lubridate)
})

# ---- Shared helpers ----------------------------------------------------------
source("Analysis/utils_thresholds.R")   # thr_max_acc(), acc_threshold(), clip_thr(), prob_col()

CLAMP <- c(0.40, 0.70)
POS   <- "SMB"
SEED  <- 73
K     <- 5

ts_tag <- function() format(with_tz(Sys.time(), "Australia/Melbourne"), "%Y%m%d_%H%M%S")

dir.create(here("outputs","tables"), recursive = TRUE, showWarnings = FALSE)

# Short Windows-safe path helper (noop on non-Windows)
.short_path <- function(p) {
  if (.Platform$OS.type == "windows") {
    sp <- try(utils::shortPathName(p), silent = TRUE)
    if (!inherits(sp, "try-error") && nzchar(sp)) return(sp)
  }
  p
}

# H2O lifecycle ---------------------------------------------------------------
h2o_up <- function() !is.null(tryCatch(h2o.getConnection(), error = function(e) NULL))
ensure_h2o <- function(heap = "6G") { if (!h2o_up()) h2o.init(nthreads = -1, max_mem_size = heap); invisible(TRUE) }
fresh_h2o  <- function(heap = "6G") { try(h2o.shutdown(prompt = FALSE), silent = TRUE); Sys.sleep(1); h2o.init(nthreads = -1, max_mem_size = heap); invisible(TRUE) }

# Data paths ------------------------------------------------------------------
paths_needed <- c(
  qa = here("outputs","tables","fish_quintiles_allfreq_tsfeat.rds"),
  qf = here("outputs","tables","fish_quintiles_tsfeat_only.rds"),
  ma = here("outputs","tables","fish_median_allfreq_tsfeat.rds"),
  mf = here("outputs","tables","fish_median_tsfeat_only.rds")
)

variant_map <- tribble(
  ~name,               ~path_key,
  "quintiles_allfreq", "qa",
  "quintiles_feats",   "qf",
  "median_allfreq",    "ma",
  "median_feats",      "mf"
)

# Load (skip missing) ---------------------------------------------------------
load_variant_data <- function(vname) {
  key <- variant_map$path_key[match(vname, variant_map$name)]
  p   <- paths_needed[[key]]
  if (!file.exists(p)) return(NULL)
  readRDS(p)
}

# Split 60/20/20 grouped by fishNum, stratified by species (same spirit as 05)
split_by_fish_strat <- function(df, p_train = 0.6, p_valid = 0.2, seed = SEED) {
  stopifnot(all(c("fishNum","species") %in% names(df)))
  set.seed(seed)
  ids <- df |> distinct(fishNum, species)
  split_one <- function(di) {
    n <- nrow(di); idx <- sample.int(n)
    n_tr <- max(1, floor(p_train*n))
    n_va <- max(1, floor(p_valid*n))
    n_te <- max(1, n - n_tr - n_va)
    # adjust if overflow
    while (n_tr + n_va + n_te > n) {
      if (n_tr > 1) n_tr <- n_tr - 1 else if (n_va > 1) n_va <- n_va - 1 else n_te <- n_te - 1
    }
    tibble(
      fishNum = di$fishNum[idx],
      split   = c(rep("train", n_tr), rep("valid", n_va), rep("test", n - n_tr - n_va))
    )
  }
  ids |> group_split(species) |> purrr::map_dfr(split_one) |>
    right_join(df, by = "fishNum") |>
    relocate(split)
}

# Build grouped K-folds at fish level inside TRAIN ----------------------------
make_group_folds <- function(train_df, k = K, seed = SEED) {
  set.seed(seed)
  ids <- train_df |> distinct(fishNum, species)
  # stratify by species -> spread fish into k folds
  ids <- ids |> group_by(species) |> mutate(.fold = sample(rep(1:k, length.out = n()))) |> ungroup()
  train_df |> left_join(ids, by = c("fishNum","species")) |> rename(fold = .fold)
}

# Pick single-model algorithm from latest AutoML leaderboard ------------------
# - reads outputs/tables/leaderboard_<variant>_*.rds (created by 05_automl_tsfeatures.R)
# - skips StackedEnsemble; returns first strong base learner family
# - fallback = "GBM" if inference fails
latest_file <- function(dir, pattern) {
  paths <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(paths)) return(NA_character_)
  get_ts <- function(p) {
    m <- regexpr("\\d{8}_\\d{6}", basename(p))
    if (m[1] > 0) as.POSIXct(regmatches(basename(p), m), format = "%Y%m%d_%H%M%S", tz = "UTC") else file.info(p)$mtime
  }
  paths[order(sapply(paths, get_ts), decreasing = TRUE)][1]
}

infer_algo_from_leaderboard <- function(variant) {
  pat <- glue("^leaderboard_{variant}_\\d{{6,8}}_\\d{{6}}\\.rds$")
  fp  <- latest_file(here("outputs","tables"), pat)
  if (is.na(fp)) {
    message("No leaderboard found for ", variant, " — fallback to GBM.")
    return("GBM")
  }
  lb <- readr::read_rds(fp)
  # First non-ensemble
  mids <- as.character(lb$model_id)
  pick <- mids[!grepl("^StackedEnsemble", mids)][1]
  if (is.na(pick)) pick <- mids[1]
  # Map by prefix in model_id
  if (grepl("^GBM", pick))           return("GBM")
  if (grepl("^DRF", pick))           return("DRF")
  if (grepl("^XRT", pick))           return("DRF")       # XRT treated like DRF
  if (grepl("^DeepLearning", pick))  return("DeepLearning")
  if (grepl("^GLM", pick))           return("GLM")
  if (grepl("^XGBoost", pick))       return("GBM")       # avoid xgboost-internal dependency issues
  # default
  "GBM"
}

# Train a single model for one fold -------------------------------------------
fit_h2o_single <- function(algo, hex_train, y, x, seed = SEED) {
  switch(algo,
         "GBM" = h2o.gbm(x = x, y = y, training_frame = hex_train,
                         seed = seed, ntrees = 200, learn_rate = 0.05, max_depth = 5,
                         stopping_rounds = 5, stopping_metric = "AUC"),
         "DRF" = h2o.randomForest(x = x, y = y, training_frame = hex_train,
                                  seed = seed, ntrees = 300, max_depth = 20, histogram_type = "AUTO"),
         "DeepLearning" = h2o.deeplearning(x = x, y = y, training_frame = hex_train,
                                           seed = seed, hidden = c(128,64), epochs = 20, rate = 0.01,
                                           reproducible = FALSE, stopping_rounds = 5, stopping_metric = "AUC"),
         "GLM" = h2o.glm(x = x, y = y, training_frame = hex_train,
                         family = "binomial", alpha = 0.5, lambda_search = TRUE, seed = seed),
         # fallback
         h2o.gbm(x = x, y = y, training_frame = hex_train,
                 seed = seed, ntrees = 200, learn_rate = 0.05, max_depth = 5,
                 stopping_rounds = 5, stopping_metric = "AUC")
  )
}

# Confusion matrix helper ------------------------------------------------------
confusion_df <- function(truth, pred, pos = POS) {
  neg <- setdiff(unique(truth), pos)[1]
  tab <- table(actual = factor(truth, levels = c(neg, pos)),
               pred   = factor(pred,  levels = c(neg, pos)))
  as_tibble(as.data.frame.matrix(tab), rownames = "actual")
}

# Core runner for one variant --------------------------------------------------
run_variant_oof <- function(variant) {
  df <- load_variant_data(variant)
  if (is.null(df)) {
    message("Skipping '", variant, "': data file not found.")
    return(invisible(NULL))
  }
  
  # Identify X columns (drop explicit IDs/labels)
  drop_cols <- intersect(names(df), c("species","fishNum","quantile"))
  x_cols    <- setdiff(names(df), drop_cols)
  
  # Split & fold
  sp <- split_by_fish_strat(df, seed = SEED)
  train_df <- filter(sp, split == "train")
  valid_df <- filter(sp, split == "valid")
  test_df  <- filter(sp, split == "test")
  
  train_folds <- make_group_folds(train_df, k = K, seed = SEED)
  
  # H2O frames (we will rebuild per fold to isolate train data)
  ensure_h2o("6G"); h2o.removeAll()
  
  algo <- infer_algo_from_leaderboard(variant)
  message(glue("\n=== {variant}: OOF threshold tuning — using algorithm: {algo} ==="))
  
  # Collect OOF preds
  oof_prob <- numeric(0)
  oof_true <- character(0)
  
  for (f in sort(unique(train_folds$fold))) {
    tr_part <- filter(train_folds, fold != f)
    va_part <- filter(train_folds, fold == f)
    
    hex_tr <- as.h2o(select(tr_part, all_of(c("species", x_cols))))
    hex_va <- as.h2o(select(va_part, all_of(c("species", x_cols))))
    hex_tr[,"species"] <- h2o.asfactor(hex_tr[,"species"])
    hex_va[,"species"] <- h2o.asfactor(hex_va[,"species"])
    
    mdl <- fit_h2o_single(algo, hex_tr, y = "species", x = x_cols, seed = SEED)
    
    pr_va <- as.data.frame(h2o.predict(mdl, hex_va)) |>
      dplyr::bind_cols(species = as.character(as.data.frame(hex_va)$species), .before = 1)
    
    pc <- prob_col(pr_va, positive = POS)
    oof_prob <- c(oof_prob, pr_va[[pc]])
    oof_true <- c(oof_true, pr_va$species)
  }
  
  # Compute OOF threshold (raw + clipped)
  thr_oof_raw  <- thr_max_acc(truth = oof_true, prob = oof_prob, positive = POS)
  thr_oof_clip <- clip_thr(thr_oof_raw, lo = CLAMP[1], hi = CLAMP[2])
  
  # OOF accuracy at clipped threshold
  neg_lab      <- setdiff(unique(oof_true), POS)[1]
  oof_predlab  <- ifelse(oof_prob >= thr_oof_clip, POS, neg_lab)
  acc_oof_thr  <- mean(oof_predlab == oof_true)
  
  # Train final single model on full TRAIN (optionally include VALID inside TRAIN)
  train_full <- bind_rows(train_df, valid_df)  # consistent with "train+valid if appropriate"
  hex_trf <- as.h2o(select(train_full, all_of(c("species", x_cols))))
  hex_te  <- as.h2o(select(test_df,  all_of(c("species", x_cols))))
  hex_trf[,"species"] <- h2o.asfactor(hex_trf[,"species"])
  hex_te[,"species"]  <- h2o.asfactor(hex_te[,"species"])
  
  mdl_full <- fit_h2o_single(algo, hex_trf, y = "species", x = x_cols, seed = SEED)
  
  pr_te <- as.data.frame(h2o.predict(mdl_full, hex_te)) |>
    dplyr::bind_cols(species = as.character(as.data.frame(hex_te)$species), .before = 1)
  
  pc_te <- prob_col(pr_te, positive = POS)
  # Baseline @ 0.50
  pred_050 <- ifelse(pr_te[[pc_te]] >= 0.50, POS, neg_lab)
  acc_050  <- mean(pred_050 == pr_te$species)
  
  # TEST @ OOF threshold
  pred_oof <- ifelse(pr_te[[pc_te]] >= thr_oof_clip, POS, neg_lab)
  acc_oof  <- mean(pred_oof == pr_te$species)
  cm_oof   <- confusion_df(truth = pr_te$species, pred = pred_oof, pos = POS)
  
  # Save artifacts -------------------------------------------------------------
  ts  <- ts_tag()
  jfp <- here("outputs","tables", glue("oof_threshold_{variant}_{ts}.json"))
  cfp <- here("outputs","tables", glue("oof_confusion_{variant}_{ts}.csv"))
  
  meta <- list(
    variant        = variant,
    k              = K,
    algorithm      = algo,
    thr_oof_raw    = as.numeric(thr_oof_raw),
    thr_oof_clip   = as.numeric(thr_oof_clip),
    acc_oof_at_thr = as.numeric(acc_oof_thr),
    acc_test_at_thr= as.numeric(acc_oof),
    acc_test_at_050= as.numeric(acc_050),
    seed           = SEED,
    clamp          = CLAMP,
    positive_class = POS
  )
  readr::write_file(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), jfp)
  readr::write_csv(cm_oof, cfp)
  
  # Console summary ------------------------------------------------------------
  cat(glue(
    "\n=== {toupper(variant)} — OOF threshold tuning ===\n",
    "Leader-algo (inferred): {algo}\n",
    "OOF threshold (raw):  {sprintf('%.6f', thr_oof_raw)}\n",
    "OOF threshold (clip): {sprintf('%.6f', thr_oof_clip)}  [policy clamp {CLAMP[1]}–{CLAMP[2]}]\n",
    "OOF accuracy @ clip:  {sprintf('%.3f', acc_oof_thr)}\n",
    "TEST acc @ 0.50  :    {sprintf('%.3f', acc_050)}\n",
    "TEST acc @ OOF   :    {sprintf('%.3f', acc_oof)}\n",
    "Saved: {basename(jfp)} | {basename(cfp)}\n"
  ))
  
  invisible(list(json = jfp, confusion_csv = cfp))
}

# ------------------------------- RUN ALL --------------------------------------
main <- function() {
  ensure_h2o("6G"); h2o.removeAll()
  variants <- variant_map$name
  for (v in variants) {
    tryCatch(run_variant_oof(v),
             error = function(e) message("OOF tuning failed for ", v, ": ", conditionMessage(e)))
  }
  invisible(TRUE)
}

main()
message("\nOOF threshold tuning complete.")
