# Analysis/utils_results.R  — unified viewers (policy-first)
suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(jsonlite)
})

# null-coalescing helper (fixes earlier %||% use)
`%||%` <- function(a, b) if (!is.null(a)) a else b

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

# Choose probability column for the positive class (works for H2O outputs)
.prob_col <- function(df, positive = "SMB") {
  n <- names(df)
  if (positive %in% n) return(positive)
  cand <- c("p1", paste0("prob_", positive), "TRUE", "1")
  hit  <- cand[cand %in% n]
  if (length(hit)) return(hit[1])
  pcols <- grep("^(p\\d+|prob_.*|LT|SMB)$", n, value = TRUE)
  pcols <- setdiff(pcols, c("predict","species"))
  if (length(pcols)) return(pcols[1])
  stop("No probability column for class '", positive, "'.")
}

# AUC from probabilities (simple trapezoid ROC; no extra packages)
auc_from_probs <- function(truth, prob, positive = "SMB") {
  y <- as.integer(truth == positive)
  if (sum(y) == 0L || sum(1 - y) == 0L) return(NA_real_)
  o <- order(prob, decreasing = TRUE)
  tp <- cumsum(y[o]); fp <- cumsum(1 - y[o])
  tpr <- tp / sum(y)
  fpr <- fp / sum(1 - y)
  sum(diff(c(0, fpr)) * (head(c(0, tpr), -1) + tail(c(0, tpr), -1)) / 2)
}

# Clamp helper (shared policy)
clamp_thr <- function(t, lo = 0.20, hi = 0.80) pmin(pmax(as.numeric(t), lo), hi)

# Summarize predictions using saved argmax labels
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

# pick policy threshold value from a metrics list
.pick_policy_thr <- function(m) {
  as.numeric(m$thr_policy_clip %||% m$thr_policy %||% NA_real_)
}

# ------------------------------------------------------------------------------
# ORIGINAL structure loaders/viewer
# ------------------------------------------------------------------------------

load_leaderboard <- function(run = c("original","fishlevel")) {
  run <- match.arg(run)
  path <- .latest("outputs/tables", glue("^leaderboard_{run}_\\d{{8}}_\\d{{6}}\\.rds$"))
  lb <- readr::read_rds(path); attr(lb, "path") <- path; lb
}
load_preds <- function(run = c("original","fishlevel")) {
  run <- match.arg(run)
  path <- .latest("outputs/tables", glue("^preds_{run}_\\d{{8}}_\\d{{6}}\\.rds$"))
  preds <- readr::read_rds(path); attr(preds, "path") <- path; preds
}
# NEW: load split preds for ORIGINAL (train/valid/test)
load_preds_original_split <- function(split = c("train","valid","test")) {
  split <- match.arg(split)
  pat <- switch(split,
                "train" = "^preds_original_train_\\d{8}_\\d{6}\\.rds$",
                "valid" = "^preds_original_valid_\\d{8}_\\d{6}\\.rds$",
                "test"  = "^preds_original_\\d{8}_\\d{6}\\.rds$"
  )
  path <- .latest("outputs/tables", pat)
  out <- readr::read_rds(path); attr(out, "path") <- path; out
}

# Find latest metrics JSON for "original"
load_metrics_original <- function() {
  path <- .latest("outputs/tables", "^automl_metrics_original_\\d{8}_\\d{6}\\.json$")
  m <- jsonlite::fromJSON(path); attr(m, "path") <- path; m
}

# Open ROC image if it exists
show_roc <- function(run = c("original","fishlevel"), open = TRUE) {
  run <- match.arg(run)
  path <- .latest(c("outputs/figures","figures"), glue("^roc_{run}_\\d{{8}}_\\d{{6}}\\.png$"))
  message("ROC image: ", normalizePath(path))
  if (open && .Platform$OS.type == "windows") shell.exec(normalizePath(path))
  invisible(path)
}

# One-call viewer (ORIGINAL) — baselines + policy (TRAIN/VALID/TEST)
view_results <- function(run = c("original","fishlevel"),
                         open_roc = TRUE, n_top = 10,
                         positive = "SMB", thr = NULL) {
  run <- match.arg(run)
  lb <- load_leaderboard(run)
  pt <- if (run == "original") load_preds_original_split("test") else load_preds(run)
  
  cat("\n==== ", toupper(run), " — Leaderboard (top ", n_top, ") ====\n", sep = "")
  print(dplyr::slice_head(lb, n = n_top))
  cat("\nLoaded leaderboard from: ", attr(lb, "path"), "\n", sep = "")
  
  pc <- .prob_col(pt, positive)
  auc_test <- auc_from_probs(pt$species, pt[[pc]], positive)
  cat(sprintf("\nTest AUC (from saved probabilities): %.3f\n", auc_test))
  
  # Argmax summary (informative)
  if ("pred_label" %in% names(pt)) {
    s <- summarize_preds(pt)
    cat("\n==== Predictions summary (argmax label, TEST) ====\n")
    cat("Test Accuracy (argmax): ", sprintf("%.3f", s$accuracy), "\n")
    print(s$confusion)
  }
  
  # Load metrics JSON (preferred)
  m <- try(load_metrics_original(), silent = TRUE)
  have_m <- !inherits(m, "try-error")
  
  thr_policy <- if (have_m) .pick_policy_thr(m) else NA_real_
  thr_clip   <- if (have_m) as.numeric(m$thr_cv_f1_clip %||% NA_real_) else NA_real_
  if (!is.null(thr)) thr_clip <- thr  # manual override only for the diagnostic
  
  neg <- setdiff(unique(pt$species), positive)[1]
  
  # Baseline @0.50 (TEST)
  pred_050 <- ifelse(pt[[pc]] >= 0.50, positive, neg)
  acc_050  <- mean(pred_050 == pt$species)
  cm_050   <- table(
    actual = factor(pt$species, levels = c(neg, positive)),
    pred   = factor(pred_050,     levels = c(neg, positive))
  )
  cat(sprintf("\n==== Baseline (TEST @ 0.50) ====\nAccuracy: %.3f\n", acc_050))
  print(cm_050)
  
  # Diagnostic: CV-F1 clipped (TEST) if available
  if (is.finite(thr_clip)) {
    pred_clip <- ifelse(pt[[pc]] >= thr_clip, positive, neg)
    acc_clip  <- mean(pred_clip == pt$species)
    cm_clip   <- table(
      actual = factor(pt$species, levels = c(neg, positive)),
      pred   = factor(pred_clip,     levels = c(neg, positive))
    )
    cat(sprintf("\n==== Diagnostic (TEST @ clipped CV-F1 = %.6f) ====\nAccuracy: %.3f\n",
                thr_clip, acc_clip))
    print(cm_clip)
  }
  
  # Policy threshold section (PRIMARY)
  if (is.finite(thr_policy)) {
    cat(sprintf("\n==== POLICY threshold (VALID max-ACC, clipped) = %.6f ====\n", thr_policy))
    # TEST computed from probs (always)
    pred_pol_t <- ifelse(pt[[pc]] >= thr_policy, positive, neg)
    acc_pol_t  <- mean(pred_pol_t == pt$species)
    cat("Acc @ policy (TEST): ", sprintf("%.3f", acc_pol_t), "\n", sep = "")
    
    # TRAIN / VALID: use metrics if present; else compute if preds exist
    acc_pol_tr <- m$acc_train_at_policy %||% NA_real_
    acc_pol_va <- m$acc_valid_at_policy %||% NA_real_
    
    if (is.na(acc_pol_tr)) {
      pr <- try(load_preds_original_split("train"), silent = TRUE)
      if (!inherits(pr, "try-error")) {
        pc_tr <- .prob_col(pr, positive)
        acc_pol_tr <- mean(ifelse(pr[[pc_tr]] >= thr_policy, positive, neg) == pr$species)
      }
    }
    if (is.na(acc_pol_va)) {
      pv <- try(load_preds_original_split("valid"), silent = TRUE)
      if (!inherits(pv, "try-error")) {
        pc_va <- .prob_col(pv, positive)
        acc_pol_va <- mean(ifelse(pv[[pc_va]] >= thr_policy, positive, neg) == pv$species)
      }
    }
    if (!is.na(acc_pol_tr)) cat("Acc @ policy (TRAIN): ", sprintf("%.3f", acc_pol_tr), "\n", sep = "")
    if (!is.na(acc_pol_va)) cat("Acc @ policy (VALID): ", sprintf("%.3f", acc_pol_va), "\n", sep = "")
  } else {
    cat("\nNo policy threshold found in metrics; showing baselines only.\n")
  }
  
  roc_path <- try(show_roc(run, open = open_roc), silent = TRUE)
  if (inherits(roc_path, "try-error")) message("ROC image not found; skipped.")
  
  invisible(TRUE)
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
load_metrics_quintiles <- function() {
  path <- .latest("outputs/tables", "^automl_metrics_quintiles_\\d{8}_\\d{6}\\.json$")
  m <- jsonlite::fromJSON(path); attr(m, "path") <- path; m
}

view_results_quintiles <- function(positive = "SMB", open_roc = FALSE) {
  lb <- load_leaderboard_quintiles()
  pv <- load_preds_quintiles("valid")
  pt <- load_preds_quintiles("test")
  m  <- try(load_metrics_quintiles(), silent = TRUE)
  have_m <- !inherits(m, "try-error")
  
  cat("\n==== QUINTILES — Leaderboard (top 10) ====\n")
  print(dplyr::slice_head(as_tibble(lb), n = 10))
  cat("\nLoaded leaderboard from: ", attr(lb, "path"), "\n", sep = "")
  
  pc <- .prob_col(pt, positive)
  auc <- auc_from_probs(pt$species, pt[[pc]], positive)
  cat(glue("\nAUC (TEST): {round(auc,3)}\n"))
  
  # Baseline @0.50 (TEST)
  neg <- setdiff(unique(pt$species), positive)[1]
  pred050 <- ifelse(pt[[pc]] >= 0.50, positive, neg)
  acc050  <- mean(pred050 == pt$species)
  cat(glue("Accuracy @ 0.50 (TEST): {round(acc050,3)}\n"))
  
  # Policy threshold (preferred), else show clipped VALID-F1 diagnostic
  thr_policy <- if (have_m) .pick_policy_thr(m) else NA_real_
  if (is.finite(thr_policy)) {
    predp <- ifelse(pt[[pc]] >= thr_policy, positive, neg)
    accp  <- mean(predp == pt$species)
    cat(glue("\nPolicy thr (VALID max-ACC, clipped): {signif(thr_policy,6)}"))
    cat(glue("\nAccuracy @ policy (TEST): {round(accp,3)}\n"))
    if (!is.null(m$acc_train_at_policy)) cat(glue("Accuracy @ policy (TRAIN): {round(m$acc_train_at_policy,3)}\n"))
    if (!is.null(m$acc_valid_at_policy)) cat(glue("Accuracy @ policy (VALID): {round(m$acc_valid_at_policy,3)}\n"))
  } else {
    thr_clip <- if (have_m) as.numeric(m$thr_valid_f1_clip %||% NA_real_) else NA_real_
    if (is.finite(thr_clip)) {
      predc <- ifelse(pt[[pc]] >= thr_clip, positive, neg)
      accc  <- mean(predc == pt$species)
      cat(glue("\nClipped VALID-F1: {signif(thr_clip,6)}"))
      cat(glue("\nAccuracy @ clipped (TEST): {round(accc,3)}\n"))
    }
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# AutoML (03b backscatter & 05 tsfeatures) — loaders and viewer
# ------------------------------------------------------------------------------

load_lb_automl <- function(name = c(
  "original","original_blocks","quintiles_allfreq","quintiles_feats",
  "median_allfreq","median_feats"
)) {
  name <- match.arg(name)
  path <- .latest("outputs/tables", glue::glue("^leaderboard_{name}_\\d{{6,8}}_\\d{{6}}\\.rds$"))
  lb <- readr::read_rds(path); attr(lb, "path") <- path; lb
}
load_preds_automl <- function(
    name = c("original","original_blocks","quintiles_allfreq","quintiles_feats",
             "median_allfreq","median_feats"),
    split = c("train","valid","test")
) {
  name  <- match.arg(name); split <- match.arg(split)
  path <- .latest("outputs/tables", glue::glue("^preds_{name}_{split}_\\d{{6,8}}_\\d{{6}}\\.rds$"))
  pr <- readr::read_rds(path); attr(pr, "path") <- path; pr
}
load_metrics_automl <- function(
    name = c("original","original_blocks","quintiles_allfreq","quintiles_feats",
             "median_allfreq","median_feats")) {
  name <- match.arg(name)
  path <- .latest("outputs/tables", glue::glue("^automl_metrics_{name}_\\d{{6,8}}_\\d{{6}}\\.json$"))
  m <- jsonlite::fromJSON(path); attr(m, "path") <- path; m
}
show_roc_automl <- function(name = c(
  "original","original_blocks","quintiles_allfreq","quintiles_feats",
  "median_allfreq","median_feats"), open = TRUE) {
  name <- match.arg(name)
  path <- .latest(c("outputs/figures","figures"), glue::glue("^roc_{name}_\\d{{6,8}}_\\d{{6}}\\.png$"))
  message("ROC image: ", normalizePath(path))
  if (open && .Platform$OS.type == "windows") shell.exec(normalizePath(path))
  invisible(path)
}

view_results_automl <- function(
    name = c("original","original_blocks","quintiles_allfreq","quintiles_feats",
             "median_allfreq","median_feats"),
    open_roc = TRUE, n_top = 10, positive = "SMB"
) {
  name <- match.arg(name)
  lb <- load_lb_automl(name)
  pt <- load_preds_automl(name, "test")
  m  <- load_metrics_automl(name)
  
  cat("\n==== AutoML — ", toupper(name), " ====\n", sep = "")
  cat("\nLeaderboard (top ", n_top, ")\n", sep = ""); print(dplyr::slice_head(as_tibble(lb), n = n_top))
  cat("\nLoaded leaderboard from: ", attr(lb, "path"), "\n", sep = "")
  
  pc <- .prob_col(pt, positive)
  auc_test <- auc_from_probs(truth = pt$species, prob = pt[[pc]], positive = positive)
  acc_argmax <- if ("predict" %in% names(pt)) mean(pt$species == pt$predict) else NA_real_
  cat("AUC (TEST): ", round(auc_test, 3), "\n", sep = "")
  
  # Baseline @0.50 (TEST)
  neg <- setdiff(unique(pt$species), positive)[1]
  pred050 <- ifelse(pt[[pc]] >= 0.50, positive, neg)
  acc050  <- mean(pred050 == pt$species)
  cat("Accuracy @ 0.50 (TEST): ", sprintf("%.3f", acc050), "\n", sep = "")
  
  # POLICY (preferred)
  thr_policy <- .pick_policy_thr(m)
  if (is.finite(thr_policy)) {
    predp <- ifelse(pt[[pc]] >= thr_policy, positive, neg)
    accp  <- mean(predp == pt$species)
    cat("Policy thr (VALID max-ACC, clipped): ", signif(thr_policy, 6), "\n", sep = "")
    cat("Accuracy @ policy (TEST): ", sprintf("%.3f", accp), "\n", sep = "")
    if (!is.null(m$acc_train_at_policy)) cat("Accuracy @ policy (TRAIN): ", sprintf("%.3f", m$acc_train_at_policy), "\n", sep = "")
    if (!is.null(m$acc_valid_at_policy)) cat("Accuracy @ policy (VALID): ", sprintf("%.3f", m$acc_valid_at_policy), "\n", sep = "")
  } else {
    # Diagnostic: clipped F1 threshold from metrics (VALID or CV depending on script)
    thr_clip <- suppressWarnings(as.numeric(m$thr_valid_f1_clip %||% m$thr_cv_f1_clip))
    if (is.finite(thr_clip)) {
      predc <- ifelse(pt[[pc]] >= thr_clip, positive, neg)
      accc  <- mean(predc == pt$species)
      cat("Accuracy @ clipped F1 (TEST): ", sprintf("%.3f", accc), "\n", sep = "")
      cat("Clipped F1 threshold: ", signif(thr_clip, 6), "\n", sep = "")
      if (!is.null(m$f1_band_cv_lo) && !is.null(m$f1_band_cv_hi)) {
        cat(sprintf("Near-optimal F1 band (±1%%): [%.6f, %.6f]\n",
                    as.numeric(m$f1_band_cv_lo), as.numeric(m$f1_band_cv_hi)))
      } else if (!is.null(m$f1_band_valid_lo) && !is.null(m$f1_band_valid_hi)) {
        cat(sprintf("Near-optimal F1 band (±1%%): [%.6f, %.6f]\n",
                    as.numeric(m$f1_band_valid_lo), as.numeric(m$f1_band_valid_hi)))
      }
    } else {
      cat("No policy/clipped threshold found; showing baseline only.\n")
    }
  }

  
  # ------------------------------------------------------------------------------
  # RNN viewer helpers (for 03a_rnn_reproduction.R artifacts)
  # ------------------------------------------------------------------------------
  
  # latest-by-timestamp loader (already defined above as .latest)
  
  load_rnn_metrics <- function() {
    path <- .latest("outputs/tables", "^rnn_metrics_(\\d{6}|\\d{8})_\\d{6}\\.json$")
    m <- jsonlite::fromJSON(path); attr(m, "path") <- path; m
  }
  
  load_rnn_confusion <- function(optional = TRUE) {
    path <- try(.latest("outputs/tables", "^rnn_confusion_(\\d{6}|\\d{8})_\\d{6}\\.csv$"), silent = TRUE)
    if (inherits(path, "try-error")) {
      if (optional) return(NULL) else stop("No RNN confusion CSV found.")
    }
    cm <- readr::read_csv(path, show_col_types = FALSE); attr(cm, "path") <- path; cm
  }
  
  load_rnn_preds <- function(optional = TRUE) {
    p_rds <- try(.latest("outputs/tables", "^rnn_preds_(\\d{6}|\\d{8})_\\d{6}\\.rds$"), silent = TRUE)
    p_csv <- try(.latest("outputs/tables", "^rnn_preds_(\\d{6}|\\d{8})_\\d{6}\\.csv$"), silent = TRUE)
    if (!inherits(p_rds, "try-error") && file.exists(p_rds)) {
      pr <- readr::read_rds(p_rds); attr(pr, "path") <- p_rds; return(pr)
    }
    if (!inherits(p_csv, "try-error") && file.exists(p_csv)) {
      pr <- readr::read_csv(p_csv, show_col_types = FALSE); attr(pr, "path") <- p_csv; return(pr)
    }
    if (optional) return(NULL)
    stop("No RNN predictions found.")
  }
  
  show_roc_rnn <- function(open = TRUE) {
    path <- .latest(c("figures","outputs/figures"), "^roc_rnn_(\\d{6}|\\d{8})_\\d{6}\\.png$")
    message("ROC image: ", normalizePath(path))
    if (open && .Platform$OS.type == "windows") shell.exec(normalizePath(path))
    invisible(path)
  }
  
  # Probability column for RNN preds (prefers prob_SMB then SMB then p1…)
  .rnn_prob_col <- function(df, positive = "SMB") {
    n <- names(df)
    cand <- c(paste0("prob_", positive), positive, "p1", "TRUE", "1")
    hit <- cand[cand %in% n]
    if (length(hit)) return(hit[1])
    pc <- setdiff(grep("^(prob_.*|p\\d+|LT|SMB)$", n, value = TRUE), c("predict","pred_label","species"))
    if (length(pc)) return(pc[1])
    stop("No probability column found for RNN predictions.")
  }
  
  view_results_rnn <- function(open_roc = TRUE, positive = "SMB", thr = NULL) {
    m  <- load_rnn_metrics()
    cm <- load_rnn_confusion(optional = TRUE)
    pr <- load_rnn_preds(optional = TRUE)
    
    cat("\n==== RNN — Metrics ====\n")
    if (!is.null(m$test_accuracy)) cat("Test accuracy:", sprintf("%.3f", m$test_accuracy), "\n")
    if (!is.null(m$test_loss))     cat("Test loss    :", sprintf("%.4f", m$test_loss), "\n")
    if (!is.null(m$n_train_seq) && !is.null(m$n_valid_seq) && !is.null(m$n_test_seq)) {
      cat("n seq (train/val/test):", m$n_train_seq, m$n_valid_seq, m$n_test_seq, "\n")
    }
    if (!is.null(m$roc_png)) cat("ROC image (saved path):", m$roc_png, "\n")
    cat("Loaded metrics from: ", attr(m, "path"), "\n", sep = "")
    
    if (!is.null(cm)) {
      cat("\n==== Confusion matrix (latest CSV) ====\n"); print(cm)
      cat("Loaded confusion from: ", attr(cm, "path"), "\n", sep = "")
    }
    
    if (!is.null(pr)) {
      cat("\n==== Predictions summary (argmax) ====\n")
      if (all(c("species","pred_label") %in% names(pr))) {
        tab <- table(actual = pr$species, pred = pr$pred_label)
        acc <- sum(diag(tab)) / sum(tab)
        cat("Accuracy (argmax): ", sprintf("%.3f", acc), "\n")
        print(tab)
      }
      # AUC from probs if available
      pc <- try(.rnn_prob_col(pr, positive), silent = TRUE)
      if (!inherits(pc, "try-error")) {
        # reuse AUC util from this file
        auc <- auc_from_probs(pr$species, pr[[pc]], positive = positive)
        cat(sprintf("\nTest AUC (from probs, positive=%s): %.3f\n", positive, auc))
        # Optional thresholded confusion if user supplies thr
        if (is.numeric(thr) && is.finite(thr)) {
          neg <- setdiff(unique(pr$species), positive)[1]
          pred_thr <- ifelse(pr[[pc]] >= thr, positive, neg)
          acc_thr  <- mean(pred_thr == pr$species)
          cat(sprintf("\nThresholded @ %.3f — Accuracy: %.3f\n", thr, acc_thr))
          print(table(
            actual = factor(pr$species, levels = c(neg, positive)),
            pred   = factor(pred_thr,     levels = c(neg, positive))
          ))
        }
      }
    }
    
    roc_path <- try(show_roc_rnn(open = open_roc), silent = TRUE)
    if (inherits(roc_path, "try-error")) message("ROC image not found; skipped.")
    invisible(TRUE)
  }
  
  cat("Argmax accuracy (TEST): ", sprintf("%.3f", acc_argmax), "\n\n", sep = "")
  roc_path <- try(show_roc_automl(name, open = open_roc), silent = TRUE)
  invisible(list(leaderboard = lb, preds_test = pt, metrics = m,
                 auc = auc_test, acc_argmax = acc_argmax))
}
