# ======================================================================
# 04c_freqselectors.R — select top-K discriminative F* (train-only, no leakage)
# Score = ROC AUC per frequency (SMB as positive), fallback to |Cohen's d|
# ======================================================================

suppressPackageStartupMessages({ library(tidyverse); library(pROC) })

freq_cols_from <- function(df) grep("^F\\d+(?:\\.\\d+)?$", names(df), value = TRUE)

roc_auc_one <- function(x, y, positive = "SMB") {
  yb <- as.integer(y == positive)
  if (length(unique(yb)) < 2L) return(NA_real_)
  suppressWarnings(pROC::auc(yb, x, quiet = TRUE)) |> as.numeric()
}

cohen_d <- function(x, g) {
  if (length(unique(g)) != 2L) return(NA_real_)
  g1 <- x[g == unique(g)[1]]; g2 <- x[g == unique(g)[2]]
  m1 <- mean(g1, na.rm=TRUE); m2 <- mean(g2, na.rm=TRUE)
  s1 <- stats::var(g1, na.rm=TRUE); s2 <- stats::var(g2, na.rm=TRUE)
  sp <- sqrt(((length(g1)-1)*s1 + (length(g2)-1)*s2)/(length(g1)+length(g2)-2))
  (m2 - m1)/sp
}

# df_tr: TRAIN rows only of the variant you’ll run; must have species + F*
select_discriminative_freqs <- function(df_tr, k = 20, positive = "SMB") {
  fcols <- freq_cols_from(df_tr)
  stopifnot(length(fcols) > 0, "species" %in% names(df_tr))
  sc <- purrr::map_dfr(fcols, function(f) {
    xv <- df_tr[[f]]; y <- df_tr$species
    a <- suppressWarnings(roc_auc_one(xv, y, positive))
    if (is.na(a)) a <- abs(cohen_d(xv, y))
    tibble(freq = f, score = a)
  }) |> mutate(score = ifelse(is.infinite(score), NA_real_, score)) |>
    arrange(desc(score))
  head(sc$freq[is.finite(sc$score)], k)
}
