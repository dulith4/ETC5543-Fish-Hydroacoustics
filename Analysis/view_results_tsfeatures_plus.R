# ======================================================================
# view_results_tsfeatures_plus.R — streamlined viewer for *_plus variants
# Shows:
#   - AUC (TEST)
#   - Acc @ 0.50 (TEST)
#   - Acc @ VALID-max threshold (thr_policy_raw) on TEST
#   - If thr_policy_raw outside [0.40, 0.70], also show CLIPPED acc (TEST)
# Saves a ROC PNG per variant to outputs/figures/.
# ======================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(jsonlite)
  library(here)
  library(ggplot2)
})

`%||%` <- function(a,b) if (!is.null(a)) a else b

dir.create(here("outputs","figures"), recursive = TRUE, showWarnings = FALSE)

.latest <- function(dir, pattern){
  paths <- list.files(dir, pattern = pattern, full.names = TRUE)
  stopifnot(length(paths) > 0)
  get_ts <- function(p){
    m <- regexpr("\\d{8}_\\d{6}", basename(p))
    if (m[1] > 0) as.POSIXct(regmatches(basename(p), m), format = "%Y%m%d_%H%M%S", tz = "UTC")
    else file.info(p)$mtime
  }
  paths[order(sapply(paths, get_ts), decreasing = TRUE)][1]
}

prob_col <- function(df, positive = "SMB"){
  n <- names(df)
  if (positive %in% n) return(positive)
  cand <- c("p1", paste0("prob_", positive), "TRUE", "1")
  hit  <- cand[cand %in% n]; if (length(hit)) return(hit[1])
  pcols <- grep("^(p\\d+|prob_.*|LT|SMB)$", n, value = TRUE)
  pcols <- setdiff(pcols, c("predict","species"))
  if (length(pcols)) return(pcols[1])
  stop("No probability column for class '", positive, "'.")
}

auc_from_probs <- function(truth, prob, positive = "SMB"){
  y <- as.integer(truth == positive)
  if (sum(y) == 0L || sum(1 - y) == 0L) return(NA_real_)
  o <- order(prob, decreasing = TRUE)
  tp <- cumsum(y[o]); fp <- cumsum(1 - y[o])
  tpr <- tp / sum(y); fpr <- fp / sum(1 - y)
  sum(diff(c(0, fpr)) * (head(c(0, tpr), -1) + tail(c(0, tpr), -1)) / 2)
}

roc_points <- function(truth, prob, positive = "SMB"){
  y <- as.integer(truth == positive)
  # guard: need both classes present
  if (sum(y) == 0L || sum(1 - y) == 0L) {
    return(tibble(fpr = c(0,1), tpr = c(0,1)))
  }
  o <- order(prob, decreasing = TRUE)
  tp <- cumsum(y[o]); fp <- cumsum(1 - y[o])
  core <- tibble(
    fpr = fp / sum(1 - y),
    tpr = tp / sum(y)
  )
  # Add (0,0) at start and (1,1) at end without using n()
  dplyr::bind_rows(tibble(fpr = 0, tpr = 0), core, tibble(fpr = 1, tpr = 1))
}


load_lb  <- function(name) readr::read_rds(.latest(here("outputs","tables"), glue("^leaderboard_{name}_\\d{{8}}_\\d{{6}}\\.rds$")))
load_pr  <- function(name, split=c("train","valid","test")) {
  split <- match.arg(split); readr::read_rds(.latest(here("outputs","tables"), glue("^preds_{name}_{split}_\\d{{8}}_\\d{{6}}\\.rds$")))
}
load_mx  <- function(name) jsonlite::fromJSON(.latest(here("outputs","tables"), glue("^automl_metrics_{name}_\\d{{8}}_\\d{{6}}\\.json$")))

print_summary <- function(name, positive = "SMB", n_top = 5, clip_lo = 0.40, clip_hi = 0.70){
  cat("\n==== ", toupper(name), " ====\n", sep = "")
  lb <- load_lb(name)
  if (nrow(lb)) {
    top <- slice_head(as_tibble(lb), n = 1) %>% select(model_id, auc, algo)
    cat("Top model: ", top$model_id, "  (algo: ", top$algo, ", leaderboard AUC: ", round(top$auc,3), ")\n", sep = "")
  }
  
  pt <- load_pr(name, "test")
  m  <- load_mx(name)
  pc <- prob_col(pt, positive)
  
  # AUC
  auc <- auc_from_probs(pt$species, pt[[pc]], positive)
  cat("AUC (TEST): ", round(auc,3), "\n", sep = "")
  
  # Acc @ 0.50
  neg <- setdiff(unique(pt$species), positive)[1]
  pred050 <- ifelse(pt[[pc]] >= 0.50, positive, neg)
  acc050  <- mean(pred050 == pt$species)
  cat("Acc @ 0.50 (TEST): ", sprintf("%.3f", acc050), "\n", sep = "")
  
  # VALID-max threshold (unclipped)
  thr_raw <- m$thr_policy_raw %||% NA_real_
  if (is.finite(thr_raw)) {
    predr <- ifelse(pt[[pc]] >= thr_raw, positive, neg)
    accr  <- mean(predr == pt$species)
    cat("Acc @ VALID-max thr (", signif(thr_raw,6), ") (TEST): ", sprintf("%.3f", accr), "\n", sep = "")
    
    # Only also show clipped if raw is outside window
    if (thr_raw < clip_lo || thr_raw > clip_hi) {
      thr_clip <- max(min(thr_raw, clip_hi), clip_lo)
      predc <- ifelse(pt[[pc]] >= thr_clip, positive, neg)
      accc  <- mean(predc == pt$species)
      cat("Acc @ CLIPPED thr [", clip_lo, ",", clip_hi, "] -> ", signif(thr_clip,6),
          " (TEST): ", sprintf("%.3f", accc), "\n", sep = "")
    }
  } else {
    cat("VALID-max threshold not available; showing only baseline.\n")
  }
  
  # ROC plot -> outputs/figures/roc_{name}_{ts}.png
  rp <- roc_points(pt$species, pt[[pc]], positive)
  fig_path <- here("outputs","figures", glue("roc_{name}_{format(Sys.time(), '%Y%m%d_%H%M%S')}.png"))
  gg <- ggplot(rp, aes(x = fpr, y = tpr)) +
    geom_line(linewidth = 0.9) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    labs(title = paste0("ROC — ", toupper(name)),
         subtitle = paste0("AUC (TEST) = ", round(auc,3)),
         x = "False Positive Rate", y = "True Positive Rate") +
    theme_minimal(base_size = 12)
  ggsave(fig_path, gg, width = 6, height = 5, dpi = 120)
  cat("ROC saved: ", normalizePath(fig_path, winslash = "/"), "\n", sep = "")
}

view_all_tsfeatures_plus <- function(){
  for (nm in c("quintiles_allfreq_plus","quintiles_feats_plus","median_allfreq_plus","median_feats_plus")) {
    print_summary(nm)
  }
}

# auto-run
if (interactive() || sys.nframe() <= 1) view_all_tsfeatures_plus()
