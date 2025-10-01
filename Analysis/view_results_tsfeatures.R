# ==============================================================================
# view_results_tsfeatures.R  (VIEWER ONLY — TSfeatures AutoML)
# PURPOSE
#   Display results from 05_automl_tsfeatures.R without re-running models:
#   - Shows top of AutoML leaderboard for each variant
#   - Prints TEST AUC, a thresholded accuracy (CV F1 if saved; else TEST F1),
#     Argmax Accuracy, and a confusion matrix at the chosen threshold
#   - Opens latest ROC image
#
# REQUIREMENT
#   Run at least once before viewing:
#     source("Analysis/05_automl_tsfeatures.R")
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(glue)
  library(jsonlite)
})

# ---- helpers -----------------------------------------------------------------

.latest <- function(dir, pattern) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(files)) stop("No files found that match: ", pattern)
  files[order(file.info(files)$mtime, decreasing = TRUE)][1]
}

.valid_variants <- c("quintiles_allfreq","quintiles_feats","median_allfreq","median_feats")

prob_col <- function(df, positive = "SMB") {
  if (positive %in% names(df)) return(positive)
  pc <- names(df)[grepl("^p", names(df))]
  if (length(pc)) return(pc[1])
  stop("No probability column found in predictions.")
}

# Simple AUC by trapezoid on ROC curve
auc_from_probs <- function(truth, score, positive = "SMB") {
  y <- factor(truth, levels = c(setdiff(unique(truth), positive)[1], positive))
  pos <- as.integer(y == positive)
  neg <- 1 - pos
  if (sum(pos) == 0 || sum(neg) == 0) return(NA_real_)
  ord <- order(score, decreasing = TRUE)
  tp <- cumsum(pos[ord]); fp <- cumsum(neg[ord])
  tpr <- tp / sum(pos);    fpr <- fp / sum(neg)
  # prepend origin for trapezoid integration
  tpr <- c(0, tpr); fpr <- c(0, fpr)
  sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2)
}

# Safe round: returns NA if x isn't a single finite number
safe_round <- function(x, digits = 3) {
  if (is.numeric(x) && length(x) == 1 && is.finite(x)) round(x, digits) else NA_real_
}

# F1-optimum threshold (used as TEST fallback when CV threshold absent)
f1_threshold <- function(truth, prob, positive = "SMB") {
  y <- factor(truth, levels = c(setdiff(unique(truth), positive)[1], positive))
  pos <- as.integer(y == positive)
  if (sum(pos) == 0 || sum(1 - pos) == 0) return(NA_real_)
  ord <- order(prob, decreasing = TRUE)
  tp <- cumsum(pos[ord]); fp <- cumsum(1 - pos[ord])
  fn <- sum(pos) - tp
  precision <- tp / pmax(tp + fp, 1)
  recall    <- tp / pmax(tp + fn, 1)
  f1 <- 2 * precision * recall / pmax(precision + recall, 1e-12)
  thr <- sort(prob, decreasing = TRUE)
  thr[which.max(f1)]
}

# ---- loaders -----------------------------------------------------------------

load_lb_ts  <- function(name) {
  stopifnot(name %in% .valid_variants)
  p <- .latest("outputs/tables", glue("^leaderboard_{name}_\\d{{6,8}}_\\d{{6}}\\.rds$"))
  out <- readr::read_rds(p); attr(out, "path") <- p; out
}

load_preds_ts <- function(name) {
  stopifnot(name %in% .valid_variants)
  p <- .latest("outputs/tables", glue("^preds_{name}_test_\\d{{6,8}}_\\d{{6}}\\.rds$"))
  out <- readr::read_rds(p); attr(out, "path") <- p; out
}

load_metrics_ts <- function(name) {
  stopifnot(name %in% .valid_variants)
  p <- .latest("outputs/tables", glue("^automl_metrics_{name}_\\d{{6,8}}_\\d{{6}}\\.json$"))
  out <- jsonlite::fromJSON(p)
  attr(out, "path") <- p
  out
}

show_roc_ts <- function(name, open = TRUE) {
  stopifnot(name %in% .valid_variants)
  p <- .latest("figures", glue("^roc_{name}_\\d{{6,8}}_\\d{{6}}\\.png$"))
  message("ROC image: ", normalizePath(p))
  if (open && .Platform$OS.type == "windows") shell.exec(normalizePath(p))
  invisible(p)
}

# ---- main printer ------------------------------------------------------------

view_results_tsfeatures <- function(
    name = c("quintiles_allfreq","quintiles_feats","median_allfreq","median_feats"),
    open_roc = TRUE, n_top = 10, positive = "SMB"
) {
  name <- match.arg(name)
  
  # Load artifacts
  lb <- load_lb_ts(name)
  pt <- load_preds_ts(name)
  mt <- load_metrics_ts(name)
  
  cat("\n==== AutoML — ", toupper(name), " ====\n", sep = "")
  cat("\nLeaderboard (top ", n_top, ")\n", sep = "")
  print(dplyr::slice_head(as_tibble(lb), n = n_top))
  cat("\nLoaded leaderboard from: ", attr(lb, "path"), "\n", sep = "")
  
  # Compute / fallback metrics from artifacts
  pc <- prob_col(pt, positive)
  test_auc   <- if (!is.null(mt$test_auc)) mt$test_auc else auc_from_probs(pt$species, pt[[pc]], positive)
  acc_argmax <- if (!is.null(mt$acc_argmax)) mt$acc_argmax else mean(pt$predict == pt$species)
  
  # Try to use saved CV F1 threshold if numeric; else fall back to TEST F1 threshold
  cv_thr <- mt$cv_f1_thr
  use_cv_thr <- is.numeric(cv_thr) && length(cv_thr) == 1 && is.finite(cv_thr)
  if (!use_cv_thr) cv_thr <- f1_threshold(pt$species, pt[[pc]], positive)
  neg <- setdiff(unique(pt$species), positive)[1]
  
  if (is.numeric(cv_thr) && is.finite(cv_thr)) {
    pred_thr <- ifelse(pt[[pc]] >= cv_thr, positive, neg)
    acc_thr  <- mean(pred_thr == pt$species)
    cat(glue(
      "\nAUC (TEST): {safe_round(test_auc, 3)}",
      "\nThreshold source: {if (use_cv_thr) 'CV (saved)' else 'TEST (fallback)'}",
      "\nF1-opt threshold: {safe_round(cv_thr, 6)}",
      "\nAccuracy @ threshold (TEST): {safe_round(acc_thr, 3)}",
      "\nArgmax accuracy (TEST): {safe_round(acc_argmax, 3)}\n\n"
    ))
    cm <- table(
      actual = factor(pt$species, levels = c(neg, positive)),
      pred   = factor(pred_thr,     levels = c(neg, positive))
    )
    print(cm)
  } else {
    cat(glue(
      "\nAUC (TEST): {safe_round(test_auc, 3)}",
      "\nF1-opt threshold: (not available)",
      "\nArgmax accuracy (TEST): {safe_round(acc_argmax, 3)}\n\n"
    ))
  }
  
  show_roc_ts(name, open = open_roc)
  
  invisible(list(leaderboard = lb, preds_test = pt, metrics = mt,
                 test_auc = test_auc, acc_argmax = acc_argmax))
}

# ---- convenience: view all four ---------------------------------------------

view_all_tsfeatures <- function(open_roc = TRUE, n_top = 10, positive = "SMB") {
  for (nm in .valid_variants) {
    try(view_results_tsfeatures(nm, open_roc = open_roc, n_top = n_top, positive = positive),
        silent = FALSE)
    cat("\n", strrep("-", 78), "\n")
  }
  invisible(TRUE)
}

# ---- auto-run when sourced (optional) ---------------------------------------
# ---- auto-run when sourced or run interactively --------------------------------
if (interactive() || sys.nframe() <= 1) {
  # open_roc=TRUE ensures ROC PNGs open; tweak n_top if you want fewer rows
  view_all_tsfeatures(open_roc = TRUE, n_top = 10, positive = "SMB")
}
