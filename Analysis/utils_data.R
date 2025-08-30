# ==============================================================================
# Utils: load & lightly transform the Fish Hydroacoustics dataset
# - Leaves dateTimeSample untouched
# - Parses Ping_time to hms + numeric seconds
# - Standardises a couple of types/labels
# - No feature engineering here (that comes later)
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(hms)        # for time-of-day
  library(stringr)
})

load_fish_transformed <- function(
    raw_path   = "data/TSresponse_clean.RDS",
    use_cache  = TRUE,
    cache_path = "outputs/cache/TS_clean_transformed.rds"
) {
  # Create cache folder if needed (stays out of Git)
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  
  # Return cache if it's newer than the raw file
  if (use_cache && file.exists(cache_path)) {
    if (file.info(cache_path)$mtime >= file.info(raw_path)$mtime) {
      return(readRDS(cache_path))
    }
  }
  
  # ---- 1) Read raw ----
  df <- readr::read_rds(raw_path)
  
  # ---- 2) Light fixes (NO touch to dateTimeSample) ----
  
  # (a) Ping_time -> parse time-of-day with fractional seconds
  #     Examples look like: " 15:01:14.3180" (note the leading space)
  if ("Ping_time" %in% names(df)) {
    df <- df |>
      mutate(
        Ping_time_chr = str_trim(Ping_time),
        Ping_time_hms = as_hms(strptime(Ping_time_chr, format = "%H:%M:%OS")),
        Ping_time_sec = as.numeric(Ping_time_hms)  # seconds since midnight (fractional OK)
      )
  }
  
  # (b) forkLength came in as character in glimpse — parse to numeric
  if ("forkLength" %in% names(df) && !is.numeric(df$forkLength)) {
    df <- df |>
      mutate(
        forkLength = {
          x <- as.character(forkLength)
          x <- stringr::str_trim(x)
          # common non-numeric tokens → NA
          x[x %in% c("", "-", "--", ".", "NA", "N/A", "na", "NaN", "null", "NULL")] <- NA_character_
          suppressWarnings(readr::parse_number(
            x,
            locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
          ))
        }
      )
  }
  
  # (c) Factor labels for simple codes 
  if ("clipTag" %in% names(df)) {
    df <- df |> mutate(clipTag = factor(clipTag, levels = c("N","Y"), labels = c("no","yes")))
  }
  if ("sex" %in% names(df)) {
    df <- df |> mutate(sex = factor(sex, levels = c(1,2), labels = c("male","female")))
  }
  if ("mat" %in% names(df)) {
    df <- df |> mutate(mat = factor(mat, levels = c(1,2), labels = c("immature","mature")))
  }
  if ("species" %in% names(df)) {
    df <- df |> mutate(species = factor(species))  # "LT", "SMB"
  }
  
  # (d) Some datasets use deltaMajAng vs deltaMaxAng — standardise if needed
  if ("deltaMajAng" %in% names(df) && !"deltaMaxAng" %in% names(df)) {
    df <- df |> dplyr::rename(deltaMaxAng = deltaMajAng)
  }
  
  # (e) Ensure frequency columns (F45 ... F170, including halves) are numeric
  freq_cols <- names(df)[str_detect(names(df), "^F\\d+(?:\\.\\d+)?$")]
  if (length(freq_cols) > 0) {
    df <- df |> mutate(across(all_of(freq_cols), ~ suppressWarnings(as.numeric(.x))))
  }
  
  # (f) Sort inside-region by time-of-day if present (non-destructive)
  if (all(c("Region_name", "Ping_time_sec") %in% names(df))) {
    df <- df |> arrange(Region_name, Ping_time_sec)
  }
  
  # ---- 3) Cache (optional, stays out of Git) ----
  if (use_cache) saveRDS(df, cache_path)
  
  df
}
