# ==============================================================================
# 04_tsfeatures_build.R
# Build fish-level summary features using feasts (tsibble-based).
# Produces four datasets:
#   1) fish_quintiles_allfreq_tsfeat.rds  (5 rows/fish: F45–F170 + tsfeatures)
#   2) fish_quintiles_tsfeat_only.rds     (5 rows/fish: tsfeatures only)
#   3) fish_median_allfreq_tsfeat.rds     (1 row/fish : median F* + tsfeatures)
#   4) fish_median_tsfeat_only.rds        (1 row/fish : tsfeatures only)
#
# First-time-friendly: if quintiles RDS is missing, it will run
# Analysis/02b_fish_quantiles.R to generate it.
# ==============================================================================

rm(list = ls())
invisible(gc())

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(tsibble)
  library(feasts)       
  library(stringr)
  library(fabletools) 
})

# -------- Ensure quintiles file exists ----------------------------------------
quintile_rds <- here("outputs","tables","fish_freq_quintiles_long.rds")
builder_q    <- here("Analysis","02b_fish_quantiles.R")

if (!file.exists(quintile_rds)) {
  message("Quintile file missing — running: ", builder_q)
  source(builder_q, local = TRUE)
  if (!file.exists(quintile_rds)) stop("Expected file not created: ", quintile_rds)
}

quintiles <- readRDS(quintile_rds)
stopifnot(all(c("fishNum","species","quantile") %in% names(quintiles)))

# Frequency columns (F45..F170 incl decimals)
freq_cols <- names(quintiles)[str_detect(names(quintiles), "^F\\d+(?:\\.\\d+)?$")]
stopifnot(length(freq_cols) > 0)

# -------- Helper: compute feasts features from wide -> long -> tsibble --------
# id_cols: character vector of key columns, e.g. c("fishNum","quantile") or "fishNum"
compute_feats <- function(df, id_cols, value_cols) {
  # turn character names into symbols
  id_syms  <- rlang::syms(id_cols)
  key_expr <- rlang::quo(c(!!!id_syms))   # build a tidyselect expression for key=
  
  df_long <- df |>
    pivot_longer(cols = all_of(value_cols), names_to = "freq", values_to = "val") |>
    mutate(freq = as.numeric(stringr::str_remove(freq, "^F"))) |>
    arrange(freq)
  
  tsib <- tsibble::as_tsibble(df_long, index = freq, key = !!key_expr)
  
  tsib |>
    fabletools::features(val, list(
      feasts::feat_acf,
      feasts::feat_pacf,
      feasts::feat_stl
    ))
}

# ============================== (A) QUINTILES =================================
# 5 rows per fish: (fishNum, species, n, quantile) + F* columns
message("Computing tsfeatures for quintiles …")
quintiles_feats <- compute_feats(
  df         = quintiles,
  id_cols    = c("fishNum","quantile"),   
  value_cols = freq_cols
)

# Keep species alongside (left join from original table)
quintiles_feats <- quintiles_feats |>
  left_join(quintiles |> distinct(fishNum, quantile, species),
            by = c("fishNum","quantile"))

# Two variants:
quintiles_allfreq <- quintiles |>
  left_join(quintiles_feats, by = c("fishNum","quantile","species"))

quintiles_only <- quintiles_feats

# Save
dir.create(here("outputs","tables"), recursive = TRUE, showWarnings = FALSE)
saveRDS(quintiles_allfreq, here("outputs","tables","fish_quintiles_allfreq_tsfeat.rds"))
saveRDS(quintiles_only,    here("outputs","tables","fish_quintiles_tsfeat_only.rds"))

# ========================== (B) MEDIAN PER FISH ===============================
# Collapse quintiles to one row per fish via median across quintiles for each F*
message("Computing tsfeatures for per-fish medians …")
median_df <- quintiles |>
  group_by(fishNum, species) |>
  summarise(across(all_of(freq_cols), \(x) median, na.rm = TRUE), .groups = "drop")

median_feats <- compute_feats(
  df         = median_df,
  id_cols    = c("fishNum"),             
  value_cols = freq_cols
)

# Keep species
median_feats <- median_feats |>
  left_join(median_df |> distinct(fishNum, species), by = "fishNum")

# Two variants:
median_allfreq <- median_df |>
  left_join(median_feats, by = c("fishNum","species"))

median_only <- median_feats

# Save
saveRDS(median_allfreq, here("outputs","tables","fish_median_allfreq_tsfeat.rds"))
saveRDS(median_only,    here("outputs","tables","fish_median_tsfeat_only.rds"))

message("Saved fish-level tsfeatures datasets to outputs/tables/")

