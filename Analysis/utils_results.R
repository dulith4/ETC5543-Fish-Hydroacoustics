# Analysis/utils_results.R
suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
})

# ------------------------------------------------------------------------------
# Core helpers
# ------------------------------------------------------------------------------

# Find latest file by timestamp in name (YYYYMMDD_HHMMSS) or fall back to mtime
.latest <- function(dirs, pattern) {
  paths <- unlist(lapply(dirs, function(d) {
    if (dir.exists(d)) list.files(d, pattern = pattern, full.names = TRUE) else character(0)
  }))
  if (length(paths) == 0) stop(glue("No files found for pattern '{pattern}' in {toString(dirs)}"))
  get_ts <- function(p) {
    m <- regexpr("\\d{8}_\\d{6}", basename(p))
    if (m[1] > 0) {
      ts <- regmatches(basename(p), m)
      as.POSIXct(ts, format = "%Y%m%d_%H%M%S", tz = "UTC")
    } else {
      file.info(p)$mtime
    }
  }
  ts <- sapply(paths, get_ts)
  paths[order(ts, decreasing = TRUE)][1]
}

# Load leaderboard/preds saved by our scripts
load_leaderboard <- function(run = c("original","fishlevel")) {
  run <- match.arg(run)
  path <- .latest("outputs/tables", glue("^leaderboard_{run}_\\d{{8}}_\\d{{6}}\\.rds$"))
  lb <- readr::read_rds(path)
  attr(lb, "path") <- path
  lb
}

load_preds <- function(run = c("original","fishlevel")) {
  run <- match.arg(run)
  path <- .latest("outputs/tables", glue("^preds_{run}_\\d{{8}}_\\d{{6}}\\.rds$"))
  preds <- readr::read_rds(path)
  attr(preds, "path") <- path
  preds
}

# Open ROC image if it exists (searches both outputs/figures and root figures/)
show_roc <- function(run = c("original","fishlevel"), open = TRUE) {
  run <- match.arg(run)
  path <- .latest(c("outputs/figures","figures"), glue("^roc_{run}_\\d{{8}}_\\d{{6}}\\.png$"))
  message("ROC image: ", normalizePath(path))
  if (open && .Platform$OS.type == "windows") shell.exec(normalizePath(path))
  invisible(path)
}

# Choose probability column for the positive class (works for H2O outputs)
.prob_col <- function(df, positive = "SMB") {
  n <- names(df)
  if (positive %in% n) return(positive)
  if ("p1" %in% n) return("p1")
  if ("TRUE" %in% n) return("TRUE")
  stop("No probability column for class '", positive, "'.")
}

# AUC from probabilities (simple trapezoid ROC; no extra packages)
auc_from_probs <- function(truth, prob, positive = "SMB") {
  y <- as.integer(truth == positive)
  o <- order(prob, decreasing = TRUE)
  tp <- cumsum(y[o]); fp <- cumsum(1 - y[o])
  tpr <- tp / sum(y)
  fpr <- fp / sum(1 - y)
  sum(diff(c(0, fpr)) * (head(c(0, tpr), -1) + tail(c(0, tpr), -1)) / 2)
}

# Summarize predictions using saved argmax labels (column 'pred_label')
summarize_preds <- function(preds) {
  stopifnot(all(c("species","pred_label") %in% names(preds)))
  tab <- table(actual = preds$species, pred = preds$pred_label)
  acc <- sum(diag(tab)) / sum(tab)
  classes <- union(rownames(tab), colnames(tab))
  byc <- purrr::map_dfr(classes, function(cl) {
    tp <- sum(preds$species == cl & preds$pred_label == cl)
    fp <- sum(preds$species != cl & preds$pred_label == cl)
    fn <- sum(preds$species == cl & preds$pred_label != cl)
    tibble(class = cl,
           precision = ifelse(tp + fp > 0, tp / (tp + fp), NA_real_),
           recall    = ifelse(tp + fn > 0, tp / (tp + fn), NA_real_),
           support   = sum(preds$species == cl))
  })
  list(accuracy = acc, confusion = tab, by_class = byc)
}

# One-call viewer (now also prints TEST AUC + optional thresholded metrics)
view_results <- function(run = c("original","fishlevel"),
                         open_roc = TRUE, n_top = 10,
                         positive = "SMB", thr = NULL) {
  run <- match.arg(run)
  lb <- load_leaderboard(run)
  preds <- load_preds(run)
  
  cat("\n==== ", toupper(run), " — Leaderboard (top ", n_top, ") ====\n", sep = "")
  print(dplyr::slice_head(lb, n = n_top))
  cat("\nLoaded leaderboard from: ", attr(lb, "path"), "\n", sep = "")
  
  # Test AUC from saved probabilities
  pc <- .prob_col(preds, positive)
  auc_test <- auc_from_probs(preds$species, preds[[pc]], positive)
  cat(sprintf("\nTest AUC (from saved probabilities): %.3f\n", auc_test))
  
  # Argmax summary
  s <- summarize_preds(preds)
  cat("\n==== Predictions summary (argmax label) ====\n")
  cat("Test Accuracy (argmax): ", sprintf("%.3f", s$accuracy), "\n")
  print(s$confusion)
  print(s$by_class)
  
  # Optional: thresholded confusion/accuracy at supplied threshold (e.g., 0.300689)
  if (!is.null(thr)) {
    neg <- setdiff(unique(preds$species), positive)[1]
    pred_lab_thr <- ifelse(preds[[pc]] >= thr, positive, neg)
    acc_thr <- mean(pred_lab_thr == preds$species)
    cm_thr <- table(
      actual = factor(preds$species, levels = c(neg, positive)),
      pred   = factor(pred_lab_thr,     levels = c(neg, positive))
    )
    cat(sprintf("\n==== Thresholded summary (thr = %.6f, positive = %s) ====\n", thr, positive))
    cat("Test Accuracy @ thr: ", sprintf("%.3f", acc_thr), "\n")
    print(cm_thr)
  }
  
  roc_path <- try(show_roc(run, open = open_roc), silent = TRUE)
  if (inherits(roc_path, "try-error")) message("ROC image not found; skipped.")
  
  invisible(list(
    leaderboard = lb,
    preds = preds,
    auc_test = auc_test,
    argmax_summary = s,
    thresholded = if (!is.null(thr)) list(thr = thr, acc = acc_thr, cm = cm_thr) else NULL,
    roc = if (!inherits(roc_path, "try-error")) roc_path else NULL
  ))
}

# ------------------------------------------------------------------------------
# QUINTILES viewer (from 03_classification.R artifacts)
# ------------------------------------------------------------------------------

load_leaderboard_quintiles <- function() {
  path <- .latest("outputs/tables", "^automl_leaderboard_\\d{8}_\\d{6}\\.rds$")
  lb <- readr::read_rds(path); attr(lb, "path") <- path; lb
}

load_preds_quintiles <- function(split = c("valid","test")) {
  split <- match.arg(split)
  path <- .latest("outputs/tables", glue("^predictions_{split}_\\d{{8}}_\\d{{6}}\\.rds$"))
  pr <- readr::read_rds(path); attr(pr, "path") <- path; pr
}

# Compute F1-optimal threshold on VALID preds
f1_threshold <- function(truth, prob, positive = "SMB") {
  stopifnot(length(truth) == length(prob))
  truth_bin <- as.integer(truth == positive)
  thrs <- sort(unique(prob))
  best_t <- 0.5; best_f1 <- -Inf
  for (t in thrs) {
    pred <- as.integer(prob >= t)
    tp <- sum(pred == 1 & truth_bin == 1)
    fp <- sum(pred == 1 & truth_bin == 0)
    fn <- sum(pred == 0 & truth_bin == 1)
    prec <- ifelse(tp+fp>0, tp/(tp+fp), 0)
    rec  <- ifelse(tp+fn>0, tp/(tp+fn), 0)
    f1 <- ifelse(prec+rec>0, 2*prec*rec/(prec+rec), 0)
    if (f1 > best_f1) { best_f1 <- f1; best_t <- t }
  }
  best_t
}

view_results_quintiles <- function(positive = "SMB", open_roc = FALSE) {
  lb <- load_leaderboard_quintiles()
  pv <- load_preds_quintiles("valid")
  pt <- load_preds_quintiles("test")
  
  cat("\n==== QUINTILES — Leaderboard (top 10) ====\n")
  print(dplyr::slice_head(as_tibble(lb), n = 10))
  cat("\nLoaded leaderboard from: ", attr(lb, "path"), "\n", sep = "")
  
  prob_col <- .prob_col(pt, positive)
  thr <- f1_threshold(truth = pv$species, prob = pv[[prob_col]], positive = positive)
  
  # Confusion at F1-opt threshold (TEST) — use real class labels
  neg <- setdiff(unique(pt$species), positive)[1]
  pred_lab <- ifelse(pt[[prob_col]] >= thr, positive, neg)
  cm <- table(
    actual = factor(pt$species, levels = c(neg, positive)),
    pred   = factor(pred_lab,     levels = c(neg, positive))
  )
  acc <- mean(pred_lab == pt$species)
  
  auc <- auc_from_probs(pt$species, pt[[prob_col]], positive)
  
  cat(glue("\nAUC (from test probs): {round(auc,3)}\n",
           "F1-opt threshold (from VALID): {round(thr,6)}\n",
           "Accuracy at threshold: {round(acc,3)}\n"))
  print(cm)
  
  # Argmax accuracy (matches H2O predict label)
  acc_argmax <- mean(pt$species == pt$predict)
  cat(glue("\nArgmax accuracy (predict column): {round(acc_argmax,3)}\n"))
  
  invisible(list(leaderboard = lb, preds_valid = pv, preds_test = pt,
                 threshold = thr, auc = auc, cm = cm, acc = acc))
}


# ------------------------------------------------------------------------------
# RNN viewer helpers (for 03a_rnn_reproduction.R)
# ------------------------------------------------------------------------------

# Load latest metrics JSON
load_rnn_metrics <- function() {
  path <- .latest("outputs/tables", "^rnn_metrics_(\\d{6}|\\d{8})_\\d{6}\\.json$")
  m <- jsonlite::fromJSON(path)
  attr(m, "path") <- path
  m
}

# Load latest confusion CSV
load_rnn_confusion <- function() {
  path <- .latest("outputs/tables", "^rnn_confusion_(\\d{6}|\\d{8})_\\d{6}\\.csv$")
  cm <- readr::read_csv(path, show_col_types = FALSE)
  attr(cm, "path") <- path
  cm
}

# Load latest predictions (if they were saved)
load_rnn_preds <- function(optional = TRUE) {
  path_rds <- try(.latest("outputs/tables", "^rnn_preds_(\\d{6}|\\d{8})_\\d{6}\\.rds$"), silent = TRUE)
  path_csv <- try(.latest("outputs/tables", "^rnn_preds_(\\d{6}|\\d{8})_\\d{6}\\.csv$"), silent = TRUE)
  if (!inherits(path_rds, "try-error") && file.exists(path_rds)) {
    pr <- readr::read_rds(path_rds); attr(pr, "path") <- path_rds; return(pr)
  }
  if (!inherits(path_csv, "try-error") && file.exists(path_csv)) {
    pr <- readr::read_csv(path_csv, show_col_types = FALSE); attr(pr, "path") <- path_csv; return(pr)
  }
  if (optional) return(NULL)
  stop("No RNN predictions found. Re-run 03a_rnn_reproduction.R with preds saving enabled.")
}

# Summarize predictions
summarize_rnn_preds <- function(preds) {
  stopifnot(all(c("species","pred_label") %in% names(preds)))
  tab <- table(actual = preds$species, pred = preds$pred_label)
  acc <- sum(diag(tab)) / sum(tab)
  classes <- union(rownames(tab), colnames(tab))
  byc <- purrr::map_dfr(classes, function(cl) {
    tp <- sum(preds$species == cl & preds$pred_label == cl)
    fp <- sum(preds$species != cl & preds$pred_label == cl)
    fn <- sum(preds$species == cl & preds$pred_label != cl)
    tibble(class = cl,
           precision = ifelse(tp + fp > 0, tp/(tp+fp), NA_real_),
           recall    = ifelse(tp + fn > 0, tp/(tp+fn), NA_real_),
           support   = sum(preds$species == cl))
  })
  list(accuracy = acc, confusion = tab, by_class = byc)
}

# Open latest ROC PNG
show_roc_rnn <- function(open = TRUE) {
  path <- .latest(c("figures","outputs/figures"), "^roc_rnn_(\\d{6}|\\d{8})_\\d{6}\\.png$")
  message("ROC image: ", normalizePath(path))
  if (open && .Platform$OS.type == "windows") shell.exec(normalizePath(path))
  invisible(path)
}

# One-call viewer
view_results_rnn <- function(open_roc = TRUE, positive = "SMB", thr = NULL) {
  m  <- load_rnn_metrics()
  cm <- try(load_rnn_confusion(), silent = TRUE)
  pr <- load_rnn_preds(optional = TRUE)
  
  cat("\n==== RNN — Metrics ====\n")
  cat("Test accuracy:", sprintf("%.3f", m$test_accuracy), "\n")
  cat("Test loss    :", sprintf("%.4f", m$test_loss), "\n")
  cat("n seq (train/val/test):", m$n_train_seq, m$n_valid_seq, m$n_test_seq, "\n")
  if (!is.null(m$roc_png)) cat("ROC image (saved path):", m$roc_png, "\n")
  cat("Loaded metrics from: ", attr(m, "path"), "\n", sep = "")
  
  if (!inherits(cm, "try-error")) {
    cat("\n==== Confusion matrix (latest CSV) ====\n")
    print(cm)
    cat("Loaded confusion from: ", attr(cm, "path"), "\n", sep = "")
  } else {
    message("No confusion CSV found (skipping).")
  }
  
  if (!is.null(pr)) {
    cat("\n==== Predictions summary (argmax) ====\n")
    s <- summarize_rnn_preds(pr)
    cat("Accuracy (argmax): ", sprintf("%.3f", s$accuracy), "\n")
    print(s$confusion); print(s$by_class)
    cat("Loaded predictions from: ", attr(pr, "path"), "\n", sep = "")
    
    # AUC from probabilities if available
    if ("prob_SMB" %in% names(pr)) {
      auc <- auc_from_probs(pr$species, pr$prob_SMB, positive = positive)
      cat(sprintf("\nTest AUC (from probs, positive=%s): %.3f\n", positive, auc))
      
      # Thresholded summary
      if (!is.null(thr)) {
        neg <- setdiff(unique(pr$species), positive)[1]
        pred_thr <- ifelse(pr$prob_SMB >= thr, positive, neg)
        acc_thr  <- mean(pred_thr == pr$species)
        cm_thr <- table(
          actual = factor(pr$species, levels = c(neg, positive)),
          pred   = factor(pred_thr,     levels = c(neg, positive))
        )
        cat(sprintf("\n==== Thresholded summary (thr = %.6f) ====\n", thr))
        cat("Accuracy @ thr:", sprintf("%.3f", acc_thr), "\n")
        print(cm_thr)
      }
    }
  } else {
    message("No predictions saved for RNN; AUC/threshold summaries skipped.")
  }
  
  # Show ROC image
  roc_path <- try(show_roc_rnn(open = open_roc), silent = TRUE)
  if (inherits(roc_path, "try-error")) message("ROC image not found; skipped.")
  
  invisible(list(metrics = m, cm = if (!inherits(cm,"try-error")) cm else NULL,
                 preds = pr, roc = if (!inherits(roc_path,"try-error")) roc_path else NULL))
}


# ------------------------------------------------------------------------------
# AutoML viewer helpers (for 03b_automl_backscatter.R)
# ------------------------------------------------------------------------------

# Load leaderboard by variant
load_lb_automl <- function(name = c("original","original_blocks")) {
  name <- match.arg(name)
  path <- .latest("outputs/tables", glue("^leaderboard_{name}_\\d{{6,8}}_\\d{{6}}\\.rds$"))
  lb <- readr::read_rds(path)
  attr(lb, "path") <- path
  lb
}

# Load predictions (VALID or TEST) by variant
load_preds_automl <- function(name = c("original","original_blocks"),
                              split = c("valid","test")) {
  name  <- match.arg(name)
  split <- match.arg(split)
  path <- .latest("outputs/tables", glue("^preds_{name}_{split}_\\d{{6,8}}_\\d{{6}}\\.rds$"))
  pr <- readr::read_rds(path)
  attr(pr, "path") <- path
  pr
}

# Show ROC for a given variant
show_roc_automl <- function(name = c("original","original_blocks"), open = TRUE) {
  name <- match.arg(name)
  path <- .latest(c("outputs/figures","figures"),
                  glue("^roc_{name}_\\d{{6,8}}_\\d{{6}}\\.png$"))
  message("ROC image: ", normalizePath(path))
  if (open && .Platform$OS.type == "windows") shell.exec(normalizePath(path))
  invisible(path)
}

# One-call AutoML viewer
view_results_automl <- function(name = c("original","original_blocks"),
                                open_roc = TRUE, n_top = 10,
                                positive = "SMB") {
  name <- match.arg(name)
  
  lb <- load_lb_automl(name)
  pv <- load_preds_automl(name, "valid")
  pt <- load_preds_automl(name, "test")
  
  cat("\n==== AutoML — ", toupper(name), " ====\n", sep = "")
  cat("\nLeaderboard (top ", n_top, ")\n", sep = "")
  print(dplyr::slice_head(as_tibble(lb), n = n_top))
  cat("\nLoaded leaderboard from: ", attr(lb, "path"), "\n", sep = "")
  
  # Choose probability column
  pc <- .prob_col(pt, positive)
  
  # Threshold from VALID (F1-opt)
  thr <- f1_threshold(truth = pv$species, prob = pv[[.prob_col(pv, positive)]], positive = positive)
  
  # TEST summaries
  neg <- setdiff(unique(pt$species), positive)[1]
  pred_thr <- ifelse(pt[[pc]] >= thr, positive, neg)
  acc_thr  <- mean(pred_thr == pt$species)
  cm_thr <- table(
    actual = factor(pt$species, levels = c(neg, positive)),
    pred   = factor(pred_thr,     levels = c(neg, positive))
  )
  
  acc_argmax <- mean(pt$species == pt$predict)
  auc_test   <- auc_from_probs(pt$species, pt[[pc]], positive)
  
  cat(glue("\nAUC (TEST): {round(auc_test,3)}",
           "\nF1-opt threshold (from VALID): {round(thr,6)}",
           "\nAccuracy @ threshold (TEST): {round(acc_thr,3)}",
           "\nArgmax accuracy (TEST): {round(acc_argmax,3)}\n\n"))
  print(cm_thr)
  
  show_roc_automl(name, open = open_roc)
  
  invisible(list(leaderboard = lb, preds_valid = pv, preds_test = pt,
                 auc = auc_test, threshold = thr, acc_thr = acc_thr,
                 acc_argmax = acc_argmax, cm = cm_thr))
}
