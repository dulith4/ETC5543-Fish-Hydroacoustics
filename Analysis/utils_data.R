# ==============================================================================
# Utils: load & lightly transform the Fish Hydroacoustics dataset
# - Leaves dateTimeSample untouched
# - Parses Ping_time to hms + numeric seconds
# - Standardises a couple of types/labels
# - No feature engineering here (that comes later)
# ==============================================================================

# ---- packages ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(hms)
  library(stringr)
})

# ---- loader -----------------------------------------------------------------
load_fish_transformed <- function(
    raw_path   = "data/TSresponse_clean.RDS",
    use_cache  = TRUE,
    cache_path = "outputs/cache/TS_clean_transformed.rds"
) {
  # make sure cache folder exists (stays out of Git)
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  
  # reuse cache if fresh
  if (use_cache && file.exists(cache_path)) {
    if (file.info(cache_path)$mtime >= file.info(raw_path)$mtime) {
      return(readRDS(cache_path))
    }
  }
  
  # 1) read raw
  df <- readr::read_rds(raw_path)
  
  # 2) light fixes (do NOT touch dateTimeSample)
  
  # Ping_time: trim + parse (one IF only)
  if ("Ping_time" %in% names(df)) {
    df <- df |>
      mutate(
        Ping_time     = stringr::str_trim(Ping_time),                      # remove leading space
        Ping_time_hms = hms::as_hms(strptime(Ping_time, "%H:%M:%OS")),    # time-of-day
        Ping_time_sec = as.numeric(Ping_time_hms)                          # seconds since midnight
      )
  }
  
  # forkLength: robust numeric parse (quiet)
  if ("forkLength" %in% names(df) && !is.numeric(df$forkLength)) {
    df <- df |>
      mutate(forkLength = {
        x <- as.character(forkLength)
        x <- stringr::str_trim(x)
        x[x %in% c("", "-", "--", ".", "NA", "N/A", "na", "NaN", "null", "NULL")] <- NA_character_
        suppressWarnings(readr::parse_number(
          x, locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
        ))
      })
  }
  
  # simple factor labels
  if ("clipTag" %in% names(df)) df <- df |> mutate(clipTag = factor(clipTag, levels = c("N","Y"), labels = c("no","yes")))
  if ("sex" %in% names(df))     df <- df |> mutate(sex     = factor(sex, levels = c(1,2), labels = c("male","female")))
  if ("mat" %in% names(df))     df <- df |> mutate(mat     = factor(mat, levels = c(1,2), labels = c("immature","mature")))
  if ("species" %in% names(df)) df <- df |> mutate(species = factor(species))
  
  # deltaMajAng -> deltaMaxAng if needed
  if ("deltaMajAng" %in% names(df) && !"deltaMaxAng" %in% names(df)) {
    df <- df |> dplyr::rename(deltaMaxAng = deltaMajAng)
  }
  
  # frequency columns numeric
  freq_cols <- names(df)[stringr::str_detect(names(df), "^F\\d+(?:\\.\\d+)?$")]
  if (length(freq_cols) > 0) {
    df <- df |> mutate(across(all_of(freq_cols), ~ suppressWarnings(as.numeric(.x))))
  }
  
  # optional ordering within region
  if (all(c("Region_name","Ping_time_sec") %in% names(df))) {
    df <- df |> arrange(Region_name, Ping_time_sec)
  }
  
  # 3) cache (smaller, compressed)
  if (use_cache) saveRDS(df, cache_path, compress = "xz")
  
  df
}