# Analysis/03b_features_fish_level.R
# Goal: From LONG-QUANTILES data (stored as WIDE F* cols), build fish-level features:
#       ACF-based descriptors per fish × quantile curve (q20,q40,q60,q80,q100),
#       then pivot to one row per fishNum and save outputs.

rm(list = ls())
invisible(gc())

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(tsibble)
  library(fabletools)  # features()
  library(feasts)      # feat_acf()
  library(lubridate)
})


# ---- Preflight: ensure required quintile file exists (build if missing) ----
quintile_rds <- here("outputs", "tables", "fish_freq_quintiles_long.rds")
builder_r    <- here("Analysis", "02b_fish_quantiles.R")  # generates the RDS

if (!file.exists(quintile_rds)) {
  message("Quintile file not found — running: ", builder_r)
  source(builder_r, local = TRUE)
  if (!file.exists(quintile_rds)) {
    stop("Expected file not created: ", quintile_rds)
  }
}

# ------------------------------- Paths ----------------------------------------
in_path    <- here("outputs", "tables", "fish_freq_quintiles_long.rds")
tables_dir <- here("outputs", "tables")
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)

timestamp_str <- function() format(with_tz(Sys.time(), tzone = "Australia/Melbourne"),
                                   "%Y%m%d_%H%M%S")
ts_now <- timestamp_str()

out_features <- file.path(tables_dir, paste0("fish_features_tsfeat_", ts_now, ".rds"))
out_diag     <- file.path(tables_dir, paste0("fish_features_diagnostics_", ts_now, ".rds"))

# ------------------------------ Load (WIDE) -----------------------------------
dat_long <- readRDS(in_path)
# Expect at least: fishNum, species, n, quantile, and F45...F170 columns.

# -------------------- Wide F* -> Long (freq_khz, value) -----------------------
freq_cols <- names(dat_long) |>
  keep(~ grepl("^F\\d+(\\.\\d+)?$", .x)) |>
  (\(v){
    v_num <- as.numeric(sub("^F", "", v))
    v[v_num >= 45 & v_num <= 170]
  })()

dat_long <- dat_long |>
  # Standardise quantiles to numeric levels: 20,40,60,80,100
  mutate(
    quantile_num = case_when(
      grepl("^q?20$",  quantile, ignore.case = TRUE) ~ 20,
      grepl("^q?40$",  quantile, ignore.case = TRUE) ~ 40,
      grepl("^q?60$",  quantile, ignore.case = TRUE) ~ 60,
      grepl("^q?80$",  quantile, ignore.case = TRUE) ~ 80,
      grepl("^q?100$", quantile, ignore.case = TRUE) ~ 100,
      TRUE ~ NA_real_
    ),
    species = as.factor(species)
  ) |>
  filter(!is.na(quantile_num)) |>
  pivot_longer(
    cols = all_of(freq_cols),
    names_to = "freq_label",
    values_to = "value"
  ) |>
  mutate(
    freq_khz = as.numeric(sub("^F", "", freq_label)),
    quantile = factor(quantile_num, levels = c(20, 40, 60, 80, 100), ordered = TRUE)
  ) |>
  select(fishNum, species, freq_khz, quantile, value)

# ------------------------- Tsibble (fish×quantile) ----------------------------
curve_ts <- dat_long |>
  arrange(fishNum, quantile, freq_khz) |>
  as_tsibble(index = freq_khz, key = c(fishNum, quantile))

# ----------------------------- ACF features -----------------------------------
acf_by_quant <- fabletools::features(
  .tbl     = curve_ts,
  .var     = value,
  features = list(feasts::feat_acf)
)
# Columns: fishNum, quantile, acf1, acf10, diff1_acf1, diff1_acf10, diff2_acf1, diff2_acf10

# ----------------------- Pivot to one row per fish ----------------------------
acf_wide <- acf_by_quant |>
  mutate(q_suffix = paste0("q", as.integer(as.character(quantile)))) |>
  select(fishNum, q_suffix, starts_with("acf"), starts_with("diff")) |>
  pivot_longer(-c(fishNum, q_suffix), names_to = "feat", values_to = "val") |>
  mutate(feat = paste0(feat, "_", q_suffix)) |>
  select(-q_suffix) |>
  pivot_wider(names_from = feat, values_from = val)

features_tbl <- dat_long |>
  distinct(fishNum, species) |>
  right_join(acf_wide, by = "fishNum") |>
  relocate(fishNum, species)

# ------------------------------ Diagnostics -----------------------------------
na_summary <- features_tbl |>
  summarise(across(everything(), ~sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "n_na") |>
  arrange(desc(n_na))

nzv <- features_tbl |>
  select(-fishNum, -species) |>
  summarise(across(everything(), ~ var(.x, na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "column", values_to = "variance") |>
  filter(is.na(variance) | variance < 1e-8)

diag <- list(
  timestamp      = ts_now,
  input_path     = in_path,
  n_fish         = nrow(features_tbl),
  species_counts = dplyr::count(features_tbl, species),
  quantiles_used = c(20, 40, 60, 80, 100),
  n_features     = ncol(features_tbl) - 2L,
  freq_range     = range(dat_long$freq_khz, na.rm = TRUE),
  na_summary     = na_summary,
  near_zero_var  = nzv
)

# -------------------------------- Save ----------------------------------------
saveRDS(features_tbl, out_features)
saveRDS(diag,         out_diag)

cat("Saved features:   ", out_features, "\n", sep = "")
cat("Saved diagnostics:", out_diag,     "\n", sep = "")

