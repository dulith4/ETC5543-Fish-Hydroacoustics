# ==============================================================================
# Analysis/view_results_rnn.R  (SELF-CONTAINED VIEWER)
# ==============================================================================

suppressPackageStartupMessages({
  library(jsonlite)  # for fromJSON
})

# ---- helpers -----------------------------------------------------------------

.latest_any <- function(dirs, pattern) {
  files <- unlist(lapply(dirs, function(d) {
    if (dir.exists(d)) list.files(d, pattern = pattern, full.names = TRUE) else character(0)
  }))
  if (!length(files)) stop("No files found that match: ", pattern)
  files[order(file.info(files)$mtime, decreasing = TRUE)][1]
}

auc_from_probs <- function(truth, prob, positive = "SMB") {
  y <- as.integer(truth == positive)
  if (sum(y) == 0L || sum(1 - y) == 0L) return(NA_real_)
  ord <- order(prob, decreasing = TRUE)
  tp <- cumsum(y[ord]); fp <- cumsum(1 - y[ord])
  tpr <- tp / sum(y);   fpr <- fp / sum(1 - y)
  # prepend origin for trapezoid rule
  tpr <- c(0, tpr); fpr <- c(0, fpr)
  sum(diff(fpr) * (head(tpr, -1) + tail(tpr, -1)) / 2)
}

rnn_prob_col <- function(df, positive = "SMB") {
  n <- names(df)
  cand <- c(paste0("prob_", positive), positive, "p1", "TRUE", "1")
  hit <- cand[cand %in% n]
  if (length(hit)) return(hit[1])
  pc <- setdiff(grep("^(prob_.*|p\\d+|LT|SMB)$", n, value = TRUE),
                c("predict","pred_label","species"))
  if (length(pc)) return(pc[1])
  stop("No probability column found in RNN predictions.")
}

show_roc_rnn <- function(open = TRUE) {
  p <- .latest_any(c("figures","outputs/figures"),
                   "^roc_rnn_(\\d{6}|\\d{8})_\\d{6}\\.png$")
  message("ROC image: ", normalizePath(p))
  if (open && .Platform$OS.type == "windows") shell.exec(normalizePath(p))
  invisible(p)
}

# ---- loaders -----------------------------------------------------------------

load_rnn_metrics <- function() {
  p <- .latest_any("outputs/tables", "^rnn_metrics_(\\d{6}|\\d{8})_\\d{6}\\.json$")
  m <- jsonlite::fromJSON(p); attr(m, "path") <- p; m
}

load_rnn_confusion <- function(optional = TRUE) {
  p <- try(.latest_any("outputs/tables", "^rnn_confusion_(\\d{6}|\\d{8})_\\d{6}\\.csv$"), silent = TRUE)
  if (inherits(p, "try-error")) return(if (optional) NULL else stop("No RNN confusion CSV found."))
  cm <- utils::read.csv(p, check.names = FALSE)
  attr(cm, "path") <- p
  cm
}

load_rnn_preds <- function(optional = TRUE) {
  p_rds <- try(.latest_any("outputs/tables", "^rnn_preds_(\\d{6}|\\d{8})_\\d{6}\\.rds$"), silent = TRUE)
  if (!inherits(p_rds, "try-error") && file.exists(p_rds)) {
    pr <- readRDS(p_rds); attr(pr, "path") <- p_rds; return(pr)
  }
  p_csv <- try(.latest_any("outputs/tables", "^rnn_preds_(\\d{6}|\\d{8})_\\d{6}\\.csv$"), silent = TRUE)
  if (!inherits(p_csv, "try-error") && file.exists(p_csv)) {
    pr <- utils::read.csv(p_csv, check.names = FALSE)
    attr(pr, "path") <- p_csv; return(pr)
  }
  if (optional) return(NULL)
  stop("No RNN predictions found.")
}

# ---- main viewer -------------------------------------------------------------

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
    pc_ok <- TRUE
    pc <- try(rnn_prob_col(pr, positive), silent = TRUE)
    if (inherits(pc, "try-error")) pc_ok <- FALSE
    if (pc_ok) {
      auc <- auc_from_probs(pr$species, pr[[pc]], positive = positive)
      cat(sprintf("\nTest AUC (from probs, positive=%s): %.3f\n", positive, auc))
      if (is.numeric(thr) && length(thr) == 1 && is.finite(thr)) {
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

# ---- auto-run when sourced ---------------------------------------------------
if (interactive() || sys.nframe() <= 1) {
  view_results_rnn(open_roc = TRUE, positive = "SMB", thr = NULL)
}
