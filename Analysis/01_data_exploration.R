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

