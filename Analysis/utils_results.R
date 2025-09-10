# Analysis/utils_results.R
suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
})

# find latest file by timestamp in name (YYYYMMDD_HHMMSS) or fall back to mtime
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

# open ROC image if it exists (searches both outputs/figures and root figures/)
show_roc <- function(run = c("original","fishlevel"), open = TRUE) {
  run <- match.arg(run)
  path <- .latest(c("outputs/figures","figures"), glue("^roc_{run}_\\d{{8}}_\\d{{6}}\\.png$"))
  message("ROC image: ", normalizePath(path))
  if (open && .Platform$OS.type == "windows") shell.exec(normalizePath(path))
  invisible(path)
}

# summarize predictions using the saved argmax labels (not the CV-F1 threshold)
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

# one-call viewer
view_results <- function(run = c("original","fishlevel"), open_roc = TRUE, n_top = 10) {
  run <- match.arg(run)
  lb <- load_leaderboard(run)
  preds <- load_preds(run)
  
  cat("\n==== ", toupper(run), " — Leaderboard (top ", n_top, ") ====\n", sep = "")
  print(dplyr::slice_head(lb, n = n_top))
  cat("\nLoaded leaderboard from: ", attr(lb, "path"), "\n", sep = "")
  
  s <- summarize_preds(preds)
  cat("\n==== Predictions summary (argmax label) ====\n")
  cat("Test Accuracy: ", sprintf("%.3f", s$accuracy), "\n")
  print(s$confusion)
  print(s$by_class)
  
  roc_path <- try(show_roc(run, open = open_roc), silent = TRUE)
  if (inherits(roc_path, "try-error")) message("ROC image not found; skipped.")
  
  invisible(list(leaderboard = lb, preds = preds, summary = s,
                 roc = if (!inherits(roc_path, "try-error")) roc_path else NULL))
}


# ---- QUINTILES (03_classification.R) ----------------------------------------

load_leaderboard_quintiles <- function() {
  path <- .latest("outputs/tables", "^automl_leaderboard_\\d{8}_\\d{6}\\.rds$")
  lb <- readr::read_rds(path); attr(lb, "path") <- path; lb
}

load_preds_quintiles <- function(split = c("valid","test")) {
  split <- match.arg(split)
  path <- .latest("outputs/tables", glue("^predictions_{split}_\\d{{8}}_\\d{{6}}\\.rds$"))
  pr <- readr::read_rds(path); attr(pr, "path") <- path; pr
}

# pick the probability column for the positive class (default = 'SMB')
.prob_col <- function(preds, positive = "SMB") {
  # typical H2O binomial preds have probs named by class labels
  nms <- names(preds)
  if (positive %in% nms) return(positive)
  # fallback: common names 'p1' or 'TRUE'
  if ("p1" %in% nms) return("p1")
  if ("TRUE" %in% nms) return("TRUE")
  stop("Could not find a probability column for class '", positive, "'.")
}

# compute F1-optimal threshold on VALID preds
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

# AUC from probs (no extra packages)
auc_from_probs <- function(truth, prob, positive = "SMB") {
  truth_bin <- as.integer(truth == positive)
  ord <- order(prob, decreasing = TRUE)
  tp <- cumsum(truth_bin[ord])
  fp <- cumsum(1 - truth_bin[ord])
  TPR <- tp / sum(truth_bin)
  FPR <- fp / sum(1 - truth_bin)
  # trapezoid rule on ROC curve
  sum(diff(c(0, FPR)) * (head(c(0, TPR), -1) + tail(c(0, TPR), -1)) / 2)
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
  
  # confusion at F1-opt threshold (test) — use real class labels
  neg <- setdiff(unique(pt$species), positive)[1]         # e.g. "LT" when positive = "SMB"
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
  
  # quick argmax summary too (matches H2O predict label)
  acc_argmax <- mean(pt$species == pt$predict)
  cat(glue("\nArgmax accuracy (predict column): {round(acc_argmax,3)}\n"))
  invisible(list(leaderboard = lb, preds_valid = pv, preds_test = pt,
                 threshold = thr, auc = auc, cm = cm, acc = acc))
}
