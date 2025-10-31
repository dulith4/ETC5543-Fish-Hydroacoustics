# reports/build_rocs.R
# Build ROC curves for representative AutoML models and save PNGs
# Run this manually (not during knitting)

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(fs)
  library(glue)
  library(readr)
})

# --- helper functions --------------------------------------------------------
auc_from_probs <- function(truth, prob, positive = "SMB") {
  y <- as.integer(truth == positive)
  if (sum(y) == 0L || sum(1 - y) == 0L) return(NA_real_)
  o <- order(prob, decreasing = TRUE)
  tp <- cumsum(y[o]); fp <- cumsum(1 - y[o])
  tpr <- tp / sum(y); fpr <- fp / sum(1 - y)
  sum(diff(c(0, fpr)) * (head(c(0, tpr), -1) + tail(c(0, tpr), -1)) / 2)
}

prob_col <- function(df, positive = "SMB") {
  n <- names(df)
  cand <- c(positive, paste0("prob_", positive), "p1", "TRUE", "1")
  hit <- cand[cand %in% n]
  if (length(hit)) return(hit[1])
  pcols <- grep("^(p\\d+|prob_.*|LT|SMB)$", n, value = TRUE)
  pcols <- setdiff(pcols, c("predict", "pred_label", "species"))
  if (length(pcols)) return(pcols[1])
  stop("No probability column for class '", positive, "'.")
}

# --- locate your TEST prediction files ---------------------------------------
source(here("Analysis", "utils_latest_artifacts.R"))
variants <- c("original_blocks","quintiles_allfreq","quintiles_feats","median_allfreq")
mf <- list_latest_artifacts(variants, write_manifest = FALSE)

# --- build and save ROC plots -------------------------------------------------
dir_create(here("reports","figures_roc"))

for (i in seq_len(nrow(mf))) {
  v  <- mf$variant[i]
  pt <- if (grepl("\\.rds$", mf$preds_test[i], ignore.case = TRUE))
    read_rds(mf$preds_test[i]) else read_csv(mf$preds_test[i], show_col_types = FALSE)
  pc <- prob_col(pt, positive = "SMB")
  a  <- auc_from_probs(pt$species, pt[[pc]], positive = "SMB")
  
  o <- order(pt[[pc]], decreasing = TRUE)
  y <- as.integer(pt$species == "SMB")[o]
  tpr <- cumsum(y) / sum(y); fpr <- cumsum(1 - y) / sum(1 - y)
  p <- ggplot(tibble(fpr, tpr), aes(fpr, tpr)) +
    geom_line(linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = 0.5, alpha = 0.6) +
    coord_equal(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
    labs(
      title = glue("ROC — {toupper(v)}"),
      subtitle = glue("AUC (TEST) = {format(round(a, 3), nsmall = 3)}"),
      x = "False Positive Rate", y = "True Positive Rate"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(here("reports","figures_roc", glue("ROC_{v}.png")), p, width = 6, height = 5, dpi = 150)
}

cat("✅ ROC PNGs saved to reports/figures_roc/\n")
