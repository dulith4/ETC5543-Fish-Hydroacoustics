# ==============================================================================
# view_results_automl.R  (VIEWER ONLY — Backscatter AutoML)
# PURPOSE
#   Display results from 03b_automl_backscatter.R without re-running models:
#   - Shows top of AutoML leaderboard for each variant
#   - Prints TEST AUC and thresholded metrics
#   - Opens latest ROC image
#
# REQUIREMENT
#   Run once before viewing:
#     source("Analysis/03b_automl_backscatter.R")
#   If you see “No files found …”, artifacts haven’t been created yet.
#
# WHAT THIS SCRIPT READS (latest by timestamp)
#   - outputs/tables/leaderboard_original*.rds
#   - outputs/tables/preds_original_valid*.rds / preds_original_test*.rds
#   - outputs/tables/leaderboard_original_blocks*.rds
#   - outputs/tables/preds_original_blocks_valid*.rds / preds_original_blocks_test*.rds
#   - figures/roc_original*.png, figures/roc_original_blocks*.png
#
# USAGE
#   source("Analysis/utils_results.R")
#   view_results_automl("original", open_roc = TRUE, n_top = 10, positive = "SMB")
#   view_results_automl("original_blocks", open_roc = TRUE, n_top = 10, positive = "SMB")
#
# NOTE
#   This viewer does NOT reload model binaries from outputs/models/.
#   It only reads saved result tables and ROC images.
# ==============================================================================



# view_results_automl.R — Backscatter AutoML viewer
source("Analysis/utils_results.R")
view_results_automl("original",        open_roc = TRUE, n_top = 10, positive = "SMB")
view_results_automl("original_blocks", open_roc = TRUE, n_top = 10, positive = "SMB")
