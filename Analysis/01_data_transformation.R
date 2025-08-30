# ==============================================================================
# ETC5543 Fish Hydroacoustics - 01 Data Transformation
# Purpose: Clean and prepare raw acoustic dataset 
# Author: Dulitha Perera
# ==============================================================================

rm(list = ls())
source("Analysis/utils_data.R")

fish_raw <- load_fish_transformed(
  raw_path  = "data/TSresponse_clean.RDS",
  use_cache = TRUE,
  cache_path = "outputs/cache/TS_clean_transformed.rds"
)


cat("Rows:", nrow(fish_raw), " Cols:", ncol(fish_raw), "\n")
cat("Cached to outputs/cache/TS_clean_transformed.rds (ignored by Git)\n")
