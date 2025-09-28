# Analysis/view_results_automl.R
# Results of Run 03b_automl_backscatter.R
source("Analysis/utils_results.R")

# View AutoML results for both variants
view_results_automl("original", open_roc = TRUE, n_top = 10, positive = "SMB")
view_results_automl("original_blocks", open_roc = TRUE, n_top = 10, positive = "SMB")
