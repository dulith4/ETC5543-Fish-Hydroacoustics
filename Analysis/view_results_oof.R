# ==============================================================================
# view_results_oof.R — Viewer for OOF threshold tuning artifacts
#
# PURPOSE
#   - Read & display the latest OOF results produced by 07_oof_threshold_tuning.R
#   - Show OOF threshold (raw + clipped), OOF accuracy, TEST accuracy @ OOF thr
#   - Print the TEST confusion matrix at the OOF threshold
#   - If available, compare with baseline TEST @ 0.50 from 05_automl_tsfeatures.R
#
# WHAT THIS SCRIPT READS (latest by timestamp)
#   - outputs/tables/oof_threshold_<variant>_<timestamp>.json
#   - outputs/tables/oof_confusion_<variant>_<timestamp>.csv
#   - (optional) outputs/tables/automl_metrics_<variant>_<timestamp>.json
#
# VARIANTS (names must match 05_automl_tsfeatures.R)
#   1) "quintiles_allfreq"
#   2) "quintiles_feats"
#   3) "median_allfreq"
#   4) "median_feats"
#
# USAGE
#   source("Analysis/view_results_oof.R")
#   view_results_oof("quintiles_allfreq", positive = "SMB")
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(jsonlite)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

# --- tiny helper to fetch the latest file by timestamp in the filename --------
.latest_file <- function(dir, pattern) {
  paths <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(paths)) return(NA_character_)
  get_ts <- function(p) {
    m <- regexpr("\\d{8}_\\d{6}", basename(p))
    if (m[1] > 0) as.POSIXct(regmatches(basename(p), m), format = "%Y%m%d_%H%M%S", tz = "UTC") else file.info(p)$mtime
  }
  paths[order(sapply(paths, get_ts), decreasing = TRUE)][1]
}

# --- (optional) try to load baseline metrics from utils_results.R -------------
.load_metrics_automl_safe <- function(variant) {
  # Try to source utils_results.R (if not already sourced)
  if (!exists("load_metrics_automl", mode = "function")) {
    suppressWarnings(try(source("Analysis/utils_results.R"), silent = TRUE))
  }
  if (exists("load_metrics_automl", mode = "function")) {
    return(try(load_metrics_automl(variant), silent = TRUE))
  } else {
    # fallback: read metrics JSON directly if present
    pat <- glue("^automl_metrics_{variant}_\\d{{6,8}}_\\d{{6}}\\.json$")
    fp  <- .latest_file("outputs/tables", pat)
    if (is.na(fp)) return(structure(list(), class = "try-error"))
    return(jsonlite::fromJSON(fp))
  }
}

# --- viewer function ----------------------------------------------------------
view_results_oof <- function(variant = c("quintiles_allfreq","quintiles_feats","median_allfreq","median_feats"),
                             positive = "SMB") {
  variant <- match.arg(variant)
  
  jfp <- .latest_file("outputs/tables", glue("^oof_threshold_{variant}_\\d{{8}}_\\d{{6}}\\.json$"))
  cfp <- .latest_file("outputs/tables", glue("^oof_confusion_{variant}_\\d{{8}}_\\d{{6}}\\.csv$"))
  
  if (is.na(jfp) || is.na(cfp)) {
    cat(glue("No OOF artifacts found for '{variant}'. Run Analysis/07_oof_threshold_tuning.R first.\n"))
    return(invisible(NULL))
  }
  
  res <- jsonlite::fromJSON(jfp)
  cm  <- try(readr::read_csv(cfp, show_col_types = FALSE), silent = TRUE)
  
  # Try to fetch baseline TEST @ 0.50 (if 05_automl_tsfeatures.R has been run)
  acc_050 <- NA_real_
  m <- .load_metrics_automl_safe(variant)
  if (!inherits(m, "try-error")) {
    acc_050 <- as.numeric(m$acc_test_at_050 %||% NA_real_)
  }
  
  cat(glue("\n==== OOF RESULTS — {toupper(variant)} ====\n"))
  cat("Positive class: ", positive, "\n", sep = "")
  cat("OOF thr (raw):   ", sprintf('%.6f', res$thr_oof_raw),  "\n", sep = "")
  cat("OOF thr (clip):  ", sprintf('%.6f', res$thr_oof_clip), "  [policy clamp 0.40–0.70]\n", sep = "")
  cat("OOF accuracy @ thr: ", sprintf('%.3f', res$acc_oof_at_thr), "\n", sep = "")
  
  if (is.finite(acc_050)) {
    cat("TEST acc @ 0.50:     ", sprintf('%.3f', acc_050), "\n", sep = "")
  } else {
    cat("TEST acc @ 0.50:      (not available — run 05_automl_tsfeatures.R if you need this)\n")
  }
  
  cat("TEST acc @ OOF thr:  ", sprintf('%.3f', res$acc_test_at_thr), "\n", sep = "")
  
  if (!inherits(cm, "try-error")) {
    cat("\nConfusion matrix (TEST @ OOF thr):\n")
    print(cm)
  } else {
    cat("\nConfusion matrix CSV not found/readable.\n")
  }
  cat("\nFiles:\n - ", normalizePath(jfp, mustWork = FALSE), "\n - ", normalizePath(cfp, mustWork = FALSE), "\n", sep = "")
  invisible(res)
}

# --- optional quick-run when sourced interactively ----------------------------
if (interactive() && sys.nframe() <= 1) {
  # Change the default here if you usually check a specific variant
  try(view_results_oof("quintiles_allfreq", positive = "SMB"), silent = TRUE)
}
