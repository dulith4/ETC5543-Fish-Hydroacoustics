# ==============================================================================
# ETC5543 Fish Hydroacoustics - 02 Feature Engineering (Quintiles)
# Goal: per-Region frequency summaries using only F45:F170 columns
# Output: outputs/tables/reg_freq_quintiles_wide.{rds,csv}
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

source("Analysis/utils_data.R")

# 1) load (from cache by default)
fish_raw <- load_fish_transformed(
  raw_path  = "data/TSresponse_clean.RDS",
  use_cache = TRUE,
  cache_path = "outputs/cache/TS_clean_transformed.rds"
)

# 2) detect frequency columns F45 ... F170 (including decimals e.g., F45.5)
freq_cols <- names(fish_raw)[stringr::str_detect(names(fish_raw), "^F\\d+(?:\\.\\d+)?$")]
stopifnot(length(freq_cols) > 0)

# helper to build quantile functions
qfun <- function(p) function(x) as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE))

stat_funs <- list(
  q20 = qfun(.20),
  q40 = qfun(.40),
  q50 = qfun(.50),   # median
  q60 = qfun(.60),
  q80 = qfun(.80)
)

# 3) per-Region (and species) summaries, wide format
reg_freq_quintiles_wide <- fish_raw %>%
  group_by(Region, species) %>%
  summarise(
    n = dplyr::n(),
    across(all_of(freq_cols), stat_funs, .names = "{.col}_{.fn}"),
    .groups = "drop"
  )

# 4) save outputs (tracked in Git; cache is not)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
saveRDS(reg_freq_quintiles_wide, "outputs/tables/reg_freq_quintiles_wide.rds", compress = "xz")
readr::write_csv(reg_freq_quintiles_wide, "outputs/tables/reg_freq_quintiles_wide.csv")

cat("Wrote:\n  - outputs/tables/reg_freq_quintiles_wide.rds\n  - outputs/tables/reg_freq_quintiles_wide.csv\n")
