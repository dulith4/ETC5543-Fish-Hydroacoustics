# view_results_tsfeatures.R — TSfeatures viewer (policy-first)
# Reads artifacts produced by 05_automl_tsfeatures.R
# Usage:
#   source("Analysis/utils_results.R")
#   source("Analysis/view_results_tsfeatures.R")

source("Analysis/utils_results.R")

view_all_tsfeatures <- function(open_roc = TRUE, n_top = 10, positive = "SMB") {
  for (nm in c("quintiles_allfreq","quintiles_feats","median_allfreq","median_feats")) {
    view_results_automl(nm, open_roc = open_roc, n_top = n_top, positive = positive)
    cat("\n", strrep("-", 78), "\n")
  }
}

# auto-run (open ROC images)
if (interactive() || sys.nframe() <= 1) {
  view_all_tsfeatures(open_roc = TRUE, n_top = 10, positive = "SMB")
}
