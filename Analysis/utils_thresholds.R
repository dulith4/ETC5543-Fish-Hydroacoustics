# ----- Threshold & accuracy helpers (shared) -----

acc_threshold <- function(truth, prob, thr, positive = "SMB") {
  neg <- setdiff(unique(truth), positive)[1]
  mean(ifelse(prob >= thr, positive, neg) == truth)
}

thr_max_acc <- function(truth, prob, positive = "SMB") {
  y <- factor(truth, levels = c(setdiff(unique(truth), positive)[1], positive))
  grid <- unique(c(0, sort(unique(prob)), 1))
  pred_pos <- outer(prob, grid, ">=")
  neglab <- setdiff(levels(y), positive)[1]
  accs <- colMeans(ifelse(pred_pos, positive, neglab) == y)
  grid[which.max(accs)]
}

# New policy clamp
clip_thr <- function(t, lo = 0.40, hi = 0.70) max(lo, min(hi, t))

# Robust probability-column picker (H2O-style outputs)
prob_col <- function(df, positive = "SMB") {
  n <- names(df)
  if (positive %in% n) return(positive)
  cand <- c("p1", "TRUE", "1", paste0("prob_", positive))
  hit <- cand[cand %in% n]
  if (length(hit)) return(hit[1])
  pcols <- grep("^(p\\d+|prob_.*|LT|SMB)$", n, value = TRUE)
  setdiff(pcols, c("predict","species"))[1]
}

# One-line printer; returns the chosen policy threshold
print_acc_summary <- function(name, pred_train = NULL, pred_valid = NULL, pred_test,
                              positive = "SMB", clip = c(0.40, 0.70)) {
  pc_t <- prob_col(pred_test, positive)
  thr_pol <- if (!is.null(pred_valid)) {
    clip_thr(thr_max_acc(pred_valid$species, pred_valid[[prob_col(pred_valid, positive)]], positive),
             lo = clip[1], hi = clip[2])
  } else NA_real_
  
  cat("\n=== ACCURACY SUMMARY —", name, "===\n")
  a05 <- c(
    if (!is.null(pred_train)) TRAIN = acc_threshold(pred_train$species, pred_train[[prob_col(pred_train, positive)]], 0.50, positive),
    if (!is.null(pred_valid)) VALID = acc_threshold(pred_valid$species, pred_valid[[prob_col(pred_valid, positive)]], 0.50, positive),
    TEST = acc_threshold(pred_test$species,  pred_test[[pc_t]], 0.50, positive)
  )
  cat("Acc@0.50 -> ", paste(sprintf("%s=%.3f", names(a05), a05), collapse = " | "), "\n", sep = "")
  
  if (is.finite(thr_pol)) {
    ap <- c(
      if (!is.null(pred_train)) TRAIN = acc_threshold(pred_train$species, pred_train[[prob_col(pred_train, positive)]], thr_pol, positive),
      if (!is.null(pred_valid)) VALID = acc_threshold(pred_valid$species, pred_valid[[prob_col(pred_valid, positive)]], thr_pol, positive),
      TEST = acc_threshold(pred_test$species,  pred_test[[pc_t]], thr_pol, positive)
    )
    cat(sprintf("Acc@policy (thr=%.3f) -> %s\n", thr_pol,
                paste(sprintf("%s=%.3f", names(ap), ap), collapse = " | ")))
  } else {
    cat("No VALID set -> policy threshold not computed.\n")
  }
  invisible(thr_pol)
}
