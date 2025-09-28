# ==============================================================================
# view_results_quintiles.R  (VIEWER ONLY)
# PURPOSE
#   Reload and display the most recent AutoML results from 03_classification.R:
#   - Prints leaderboard (top models)
#   - Computes test AUC
#   - Finds F1-optimal threshold from VALID set
#   - Shows confusion matrix & accuracy at threshold
#   - Prints argmax accuracy (H2O’s predicted label)
#
# REQUIREMENT
#   Expects artifacts already created by running:
#     source("Analysis/03_classification.R")
#   If you see “No files found …” errors, run the AutoML script once.
#
# WHAT IT READS (latest by timestamp)
#   - outputs/tables/automl_leaderboard_*.rds
#   - outputs/tables/predictions_valid_*.rds
#   - outputs/tables/predictions_test_*.rds
#
# NOTE
#   This viewer does NOT reload model binaries from `outputs/models/`.
#   It only reads the saved results (tables + ROC images). 
#   Run the model script once, then use this viewer as often as you like.
# ==============================================================================


source("Analysis/utils_results.R")
view_results_quintiles(positive = "SMB")
