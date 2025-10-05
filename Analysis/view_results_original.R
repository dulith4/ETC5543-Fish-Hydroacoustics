# ==============================================================================
# view_results_original.R  (VIEWER ONLY — ORIGINAL structure)
# PURPOSE
#   Display the most recent AutoML results produced by 03_classification_original.R:
#   - Shows top of the leaderboard
#   - Prints TEST AUC
#   - Uses the supplied threshold (e.g., CV-F1 ≈ 0.300689) to show a confusion matrix
#   - Opens the latest ROC image
#
# REQUIREMENT
#   Run once before viewing:
#     source("Analysis/03_classification_original.R")
#   If you see “No files found …”, the artifacts haven’t been created yet.
#
# WHAT THIS SCRIPT READS (latest by timestamp)
#   - outputs/tables/leaderboard_original_*.rds
#   - outputs/tables/preds_original_*.rds
#   - figures/roc_original_*.png
#
# USAGE
#   source("Analysis/utils_results.R")
#   view_results("original", open_roc = TRUE, n_top = 10, positive = "SMB", thr = 0.300689)
#
# NOTE
#   This viewer does NOT reload the H2O model binaries in outputs/models/.
#   It only reads saved result tables + ROC images.
# ==============================================================================



# view_results_original.R — ORIGINAL structure viewer
source("Analysis/utils_results.R")
# Shows leaderboard, AUC, baseline (0.50) summary, and clipped CV-F1 summary + band
view_results("original", open_roc = TRUE, n_top = 10, positive = "SMB", thr = NULL)

