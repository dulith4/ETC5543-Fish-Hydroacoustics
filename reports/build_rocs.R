# reports/build_rocs.R
suppressPackageStartupMessages({
  library(tidyverse); library(here); library(glue); library(pROC)
})

source(here("Analysis","utils_latest_artifacts.R"))
source(here("Analysis","make_results_from_manifest.R"))  # prob_col(), auc_from_probs()

variants <- c("original_blocks","quintiles_allfreq","quintiles_feats","median_allfreq")

# NEW isolated folder
out_dir <- here("reports","figures_roc2")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Names EXACTLY as used in the QMD (mixed case)
name_map <- c(
  original_blocks   = "ROC_original_blocks.png",
  quintiles_allfreq = "ROC_quintiles_allfreq.png",
  quintiles_feats   = "ROC_quintiles_feats.png",
  median_allfreq    = "ROC_median_allfreq.png"
)

mf <- list_latest_artifacts(variants, write_manifest = FALSE)

read_preds <- function(path){
  x <- try(readr::read_rds(path), silent = TRUE)
  if (inherits(x, "try-error")) readr::read_csv(path, show_col_types = FALSE) else x
}

plot_one <- function(variant, path, positive = "SMB"){
  stopifnot(!is.na(path), file.exists(path))
  dat  <- read_preds(path)
  pcol <- prob_col(dat, positive = positive)
  stopifnot(pcol %in% names(dat), "species" %in% names(dat))
  
  y <- factor(dat$species, levels = c("LT","SMB"))
  p <- as.numeric(dat[[pcol]])
  
  auc_tbl  <- auc_from_probs(y, p, positive = positive)
  roc_obj  <- pROC::roc(y, p, levels = c("LT","SMB"), direction = "<", quiet = TRUE)
  auc_pROC <- as.numeric(pROC::auc(roc_obj))
  
  if (is.na(auc_tbl) || abs(auc_tbl - auc_pROC) > 0.002)
    stop(glue("AUC mismatch for {variant}: table={round(auc_tbl,3)} vs pROC={round(auc_pROC,3)} (col={pcol})"))
  
  outfile <- file.path(out_dir, name_map[[variant]])
  png(outfile, width = 720, height = 600, res = 96)
  plot(roc_obj,
       main = glue("ROC — {toupper(variant)}\nAUC (TEST) = {round(auc_pROC, 3)}"),
       legacy.axes = TRUE, xlab = "False Positive Rate", ylab = "True Positive Rate", lwd = 3)
  abline(0, 1, lty = 2)
  dev.off()
  
  tibble(Variant = variant, OutFile = outfile, AUC = round(auc_pROC, 3))
}

# Clean target dir then build
unlink(file.path(out_dir, "ROC_*.png"))
out <- purrr::map_dfr(variants, function(v){
  path <- mf %>% dplyr::filter(variant == v) %>% dplyr::pull(preds_test) %>% .[[1]]
  if (is.na(path) || !file.exists(path))
    stop(glue("Missing TEST predictions for {v}. Expected in outputs/tables/preds_{v}_test_*.{rds,csv}"))
  message("== ", v, " ==> ", path)
  plot_one(v, path)
})

readr::write_csv(out, file.path(out_dir, "roc_auc_check.csv"))
message("Wrote: ", normalizePath(out_dir, winslash = "/"))
print(out, n = nrow(out))
