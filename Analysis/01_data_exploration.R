# ==============================================================================
# ETC5543 Fish Hydroacoustics - Exploratory Data Analysis (Raw vs Transformed)
# File: Analysis/01_data_exploration.R
# Purpose: Sanity checks of raw data vs quintile-transformed frequency table,
#          and produce summary tables/figures for the report.
# Author: Dulitha Perera
# ==============================================================================

# ---- 0) Setup ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)     
  library(patchwork)   
  library(scales)      
  library(stringr)
})

# Ensure output folders exist
dir.create("figures",        showWarnings = FALSE, recursive = TRUE)
dir.create("outputs",        showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

# Source utils 
source("Analysis/utils_data.R")

# Reproducibility
set.seed(20250904)


# ---- 1) Load data -----------------------------------------------------------
# Raw (via utils with optional caching)
fish_raw <- load_fish_transformed(
  raw_path  = "data/TSresponse_clean.RDS",
  use_cache = TRUE,
  cache_path = "outputs/cache/TS_clean_transformed.rds"
)

# Transformed quintile table (fish-level × 5 quintiles × F45–F170)
fish_quint <- read_rds("outputs/tables/fish_freq_quintiles_long.rds")

# ---- 2) Quick sanity checks -------------------------------------------------
message("Rows/cols (raw):    ", nrow(fish_raw),  " / ", ncol(fish_raw))
message("Rows/cols (quints): ", nrow(fish_quint), " / ", ncol(fish_quint))

# Count species in both tables
species_counts_raw   <- fish_raw  |> tabyl(species) |> arrange(desc(n))
species_counts_quint <- fish_quint |> tabyl(species) |> arrange(desc(n))

write_csv(species_counts_raw,   "outputs/tables/species_counts_raw.csv")
write_csv(species_counts_quint, "outputs/tables/species_counts_quint.csv")


# Distinct fish ids in raw (if fishNum exists)
if ("fishNum" %in% names(fish_raw)) {
  fish_ids <- fish_raw |> distinct(species, fishNum)
  write_csv(fish_ids, "outputs/tables/distinct_fish_ids_raw.csv")
}

# ---- 3) Identify frequency columns in RAW ----------------------------------
freq_cols_raw <- grep("^F\\d+", names(fish_raw), value = TRUE)
stopifnot(length(freq_cols_raw) > 0)


# Helper: parse frequency label to numeric kHz safely
to_khz <- function(x) {
  # Works whether x is like "F145" or already numeric
  out <- suppressWarnings(as.numeric(str_remove(as.character(x), "^F")))
  return(out)
}

# ---- 4) Summaries: RAW frequency response ----------------------------------
raw_freq_summary <- fish_raw |>
  select(species, all_of(freq_cols_raw)) |>
  pivot_longer(-species, names_to = "frequency", values_to = "TS") |>
  mutate(freq_khz = to_khz(frequency)) |>
  filter(!is.na(TS), !is.na(freq_khz)) |>
  group_by(species, freq_khz) |>
  summarise(
    mean_TS = mean(TS, na.rm = TRUE),
    se_TS   = sd(TS,  na.rm = TRUE) / sqrt(n()),  #standard error of the mean (SE)
    .groups = "drop"
  )

write_csv(raw_freq_summary, "outputs/tables/raw_freq_summary.csv")


p_raw <- ggplot(raw_freq_summary,
                aes(x = freq_khz, y = mean_TS, colour = species, fill = species)) +
  geom_line(linewidth = 0.6) +
  geom_ribbon(aes(ymin = mean_TS - se_TS, ymax = mean_TS + se_TS),
              alpha = 0.18, colour = NA) +
  scale_x_continuous("Frequency (kHz)", breaks = pretty_breaks()) +
  labs(y = "Mean Target Strength (dB)",
       title = "Raw Frequency Response Curves (mean ± SE)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

ggsave("figures/raw_FRC.png", p_raw, width = 8, height = 4.8, dpi = 300)



# ---- 5) Summaries: TRANSFORMED quintile curves ---------------
nm <- names(fish_quint)

# detect the quintile column name and standardise to "quintile"
qcol <- dplyr::case_when(
  "quintile" %in% nm ~ "quintile",
  "quantile" %in% nm ~ "quantile",
  TRUE               ~ NA_character_
)
stopifnot(!is.na(qcol))

# all frequency columns (F45, F45.5, ..., F170)
freq_cols_trans <- grep("^F\\d+(\\.5)?$", nm, value = TRUE)
stopifnot(length(freq_cols_trans) > 0)

# pivot to long
fish_quint_long <- fish_quint |>
  rename(quintile = all_of(qcol)) |>           # unify name
  pivot_longer(
    cols = all_of(freq_cols_trans),
    names_to = "freq_label",
    values_to = "val"
  ) |>
  mutate(
    freq_khz = as.numeric(stringr::str_remove(freq_label, "^F"))
  ) |>
  filter(!is.na(freq_khz), !is.na(val))

# summarise mean ± SE by species × quintile × frequency
trans_summary <- fish_quint_long |>
  group_by(species, quintile, freq_khz) |>
  summarise(
    mean_TS = mean(val, na.rm = TRUE),
    se_TS   = sd(val,  na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

readr::write_csv(trans_summary, "outputs/tables/trans_freq_summary.csv")


# ---- 5b) Clean quintile labels and plot -------------------------------------

# Robustly convert whatever 'quintile/quantile' was (e.g., 1..5, "Q1", 0.2, 20) to 1..5
trans_summary <- trans_summary |>
  mutate(
    qnum = readr::parse_number(as.character(quintile)),   # "Q3" -> 3, "0.4" -> 0.4
    qnum = dplyr::case_when(
      qnum %in% 1:5        ~ qnum,               # already 1..5
      qnum > 0 & qnum <= 1 ~ round(qnum * 5),    # proportions -> 1..5
      qnum > 1 & qnum <= 100 ~ pmax(1, pmin(5, round(qnum / 20))), # percentages -> 1..5
      TRUE ~ NA_real_
    ),
    quintile = factor(
      qnum, levels = 1:5,
      labels = c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)")
    )
  ) |>
  select(-qnum)


# Plot: facet by species, colour = quintile 
p_trans <- ggplot(trans_summary,
                  aes(freq_khz, mean_TS, colour = quintile, group = quintile)) +
  geom_line(linewidth = 0.6, na.rm = TRUE) +
  facet_wrap(~ species, nrow = 1) +
  scale_x_continuous("Frequency (kHz)", breaks = scales::pretty_breaks()) +
  labs(y = "Mean Target Strength (dB)",
       title = "Quintile-Summarised Frequency Response Curves",
       subtitle = "Each line = species’ mean curve for a quintile (Q1 shallowest … Q5 deepest)",
       colour = "Quintile") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

ggsave("figures/transformed_FRC.png", p_trans, width = 8, height = 4.8, dpi = 300)
