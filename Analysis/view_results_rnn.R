# Analysis/view_results_rnn.R
source("Analysis/utils_results.R")

# Open ROC, print metrics, confusion, AUC (if saved), and by-class stats.
# You can pass a threshold to see thresholded confusion/accuracy, e.g. thr = 0.30
view_results_rnn(open_roc = TRUE, positive = "SMB", thr = NULL)
