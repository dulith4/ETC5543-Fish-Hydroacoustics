# ==============================================================================
# ETC5543 Fish Hydroacoustics - 02b Fish-level Quintiles (Wide-by-frequency)
# Output: outputs/tables/fish_freq_quintiles_long.rds
# - 5 rows per fish (q20, q40, q60, q80, q100)
# - columns are F45..F170 (same names as input)
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})

source("Analysis/utils_data.R")

# 1) load transformed data (from cache if present)
fish_raw <- load_fish_transformed(
  raw_path  = "data/TSresponse_clean.RDS",
  use_cache = TRUE,
  cache_path = "outputs/cache/TS_clean_transformed.rds"
)

# 2) detect all frequency columns like F45, F45.5, ..., F170
freq_cols <- names(fish_raw)[str_detect(names(fish_raw), "^F\\d+(?:\\.\\d+)?$")]
stopifnot(length(freq_cols) > 0)

# 3) define the quintile functions (20,40,60,80,100)
qfun <- function(p) function(x) as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE))
stat_funs <- list(
  q20  = qfun(.20),
  q40  = qfun(.40),
  q60  = qfun(.60),
  q80  = qfun(.80),
  q100 = qfun(1.00)   
)

# 4) summarise per fish; then reshape to "5 rows per fish, columns = frequencies"
fish_freq_quintiles <- fish_raw |>
  group_by(fishNum, species) |>
  summarise(
    n = dplyr::n(),                                  # number of pings for this fish (all regions)
    across(all_of(freq_cols), stat_funs, .names = "{.col}_{.fn}"),
    .groups = "drop"
  ) |>
  # columns are like F60_q20, F60_q40,... -> split to freq_label + quantile
  pivot_longer(
    cols = -c(fishNum, species, n),
    names_to = c("freq_label", "quantile"),
    names_sep = "_",
    values_to = "value"
  ) |>
  # make quantile a nice ordered factor
  mutate(quantile = factor(quantile, levels = c("q20","q40","q60","q80","q100"))) %>%
  # pivot so that columns are frequencies (F45..F170), 5 rows per fish
  pivot_wider(
    id_cols = c(fishNum, species, n, quantile),
    names_from = freq_label,
    values_from = value
  ) |>
  arrange(fishNum, quantile)

# 5) save RDS
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
saveRDS(fish_freq_quintiles, "outputs/tables/fish_freq_quintiles_long.rds", compress = "xz")


