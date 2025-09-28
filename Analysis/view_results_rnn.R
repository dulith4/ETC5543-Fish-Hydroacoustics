# ==============================================================================
# Analysis/view_results_rnn.R  (VIEWER ONLY)
# PURPOSE
#   Display the latest RNN run results without retraining:
#   - prints test accuracy/loss, confusion, (optional) AUC/thresholds
#   - opens the latest ROC image
#
# REQUIREMENT
#   Expects artifacts already created by:
#     source("Analysis/03a_rnn_reproduction.R")
#   If you see “No files found …” errors, run the model script once.
#
# WHAT IT READS (latest by timestamp)
#   outputs/tables/rnn_metrics_*.json
#   outputs/tables/rnn_confusion_*.csv
#   figures/roc_rnn_*.png
#   (optional) outputs/tables/rnn_preds_*.rds|.csv if you enabled saving preds
# ==============================================================================



# Analysis/view_results_rnn.R
source("Analysis/utils_results.R")

# Open ROC, print metrics, confusion, AUC (if saved), and by-class stats.
# You can pass a threshold to see thresholded confusion/accuracy, e.g. thr = 0.30
view_results_rnn(open_roc = TRUE, positive = "SMB", thr = NULL)
