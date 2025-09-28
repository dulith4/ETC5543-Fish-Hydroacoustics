# ==============================================================================
# 02a_check_transformations.R
# Verify size-standardisation (to 450 mm) + backscatter transform used by RNN
# ==============================================================================

rm(list = ls())
invisible(gc())

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)   
  library(here)
})

set.seed(73)

# ------------------------------- Paths ----------------------------------------
raw_path <- here("data", "TSresponse_clean.RDS")
out_dir  <- here("outputs", "tables")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------ Load data -------------------------------------
raw_data <- readRDS(raw_path)

# Identify frequency columns robustly
freq_cols <- names(raw_data)[which(names(raw_data) == "F45"):which(names(raw_data) == "F170")]

# --------------------------- Grouped splits -----------------------------------
# 80% train ; remaining 20% split 50/50 into validate/test
split1   <- group_initial_split(raw_data, group = fishNum, strata = species, prop = 0.8)
train    <- training(split1)
val_test <- testing(split1)

split2   <- group_initial_split(val_test, group = fishNum, strata = species, prop = 0.5)
validate <- training(split2)
test     <- testing(split2)

# ------------------ Keep needed cols, then standardise to 450 mm --------------
prep_standardise <- function(df, target_len = 450) {
  stopifnot(all(c("Region", "species", "totalLength") %in% names(df)))
  df2 <- df |>
    select(all_of(freq_cols), Region, species, totalLength)
  
  # Row-wise offset in dB to scale each fish to 450 mm
  # offset_dB = 10 * log10(450 / totalLength)
  df2 <- df2 |>
    mutate(.offset_dB = 10 * log10(target_len / totalLength)) %>%
    mutate(across(all_of(freq_cols), ~ exp((.x + .offset_dB) / 10))) %>%
    # We are now in linear "acoustic backscatter" units
    select(-.offset_dB)
  
  df2
}

train_bs    <- prep_standardise(train,    target_len = 450)
validate_bs <- prep_standardise(validate, target_len = 450)
test_bs     <- prep_standardise(test,     target_len = 450)

# ------------------------------- Checks ---------------------------------------
# 1) No NAs created in freq columns
stopifnot(!any(is.na(select(train_bs, all_of(freq_cols)))),
          !any(is.na(select(validate_bs, all_of(freq_cols)))),
          !any(is.na(select(test_bs, all_of(freq_cols)))))

# 2) Spot-check a few rows against manual formula
check_rows <- sample(nrow(train), size = min(5, nrow(train)))
manual_ok <- purrr::map_lgl(check_rows, function(i) {
  len_i   <- train$totalLength[i]
  off_dB  <- 10 * log10(450 / len_i)
  orig    <- as.numeric(train[i, freq_cols, drop = TRUE])
  expect  <- exp((orig + off_dB) / 10)
  got     <- as.numeric(train_bs[i, freq_cols, drop = TRUE])
  all.equal(expect, got, tolerance = 1e-12) == TRUE
})
stopifnot(all(manual_ok))

# 3) Units sanity: backscatter must be positive
stopifnot(all(select(train_bs, all_of(freq_cols)) > 0))

message("✓ Size-standardisation to 450 mm + backscatter conversion verified.")

# ------------------------------- Save -----------------------------------------
saveRDS(train_bs,    file = file.path(out_dir, "train_backscatter_450.rds"))
saveRDS(validate_bs, file = file.path(out_dir, "validate_backscatter_450.rds"))
saveRDS(test_bs,     file = file.path(out_dir, "test_backscatter_450.rds"))

# Small footprints for inspection
vroom::vroom_write(
  train_bs |> select(all_of(freq_cols), species) |> slice(1:100),
  file.path(out_dir, "train_backscatter_450_head.csv"),
  delim = ","
)

message("Artifacts saved in outputs/tables/.")
