# ======================================================================
# 04e_tsfeatures_plus.R — richer features (feasts + tsfeatures)
# Produces *_plus.rds for the same four variants:
#   fish_quintiles_allfreq_tsfeat_plus.rds
#   fish_quintiles_tsfeat_only_plus.rds
#   fish_median_allfreq_tsfeat_plus.rds
#   fish_median_tsfeat_only_plus.rds
# Safe to source standalone; reuses outputs/tables/fish_freq_quintiles_long.rds
# ======================================================================

rm(list = ls()); invisible(gc())
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(tsibble)
  library(feasts)
  library(fabletools)
  library(stringr)
})

# optional package; we guard every call
has_tsfeatures <- requireNamespace("tsfeatures", quietly = TRUE)

quintile_rds <- here("outputs","tables","fish_freq_quintiles_long.rds")
if (!file.exists(quintile_rds)) {
  message("Quintile file missing — building via Analysis/02b_fish_quantiles.R")
  source(here("Analysis","02b_fish_quantiles.R"), local = TRUE)
  if (!file.exists(quintile_rds)) stop("Expected file not created: ", quintile_rds)
}

quintiles <- readRDS(quintile_rds)
stopifnot(all(c("fishNum","species","quantile") %in% names(quintiles)))

# Frequency columns (F45..F170 incl decimals)
freq_cols <- names(quintiles)[str_detect(names(quintiles), "^F\\d+(?:\\.\\d+)?$")]
stopifnot(length(freq_cols) > 0)

# -------- feasts features (explicit args for tsibble method) ------------------
feasts_feats <- function(tsib, vcol = "value") {
  v <- rlang::sym(vcol)
  fabletools::features(
    .tbl     = tsib,
    .var     = !!v,
    features = list(
      feasts::feat_acf,
      feasts::feat_pacf,
      feasts::feat_stl,
      feasts::feat_spectral
    )
  )
}

# -------- tsfeatures (guarded; returns empty columns if package missing) ------
tf_safe <- function(vec) {
  if (!has_tsfeatures) return(tibble())
  x <- as.numeric(vec)
  out <- list()
  add <- function(nm, fn) {
    val <- suppressWarnings(try(fn(x), silent = TRUE))
    if (inherits(val, "try-error")) return()
    if (is.list(val)) val <- val[[1]]
    out[[nm]] <- as.numeric(val)
  }
  add("entropy",            tsfeatures::entropy)
  add("lumpiness",          tsfeatures::lumpiness)
  add("stability",          tsfeatures::stability)
  add("hurst",              tsfeatures::hurst)
  add("nonlinearity",       tsfeatures::nonlinearity)
  add("flat_spots",         tsfeatures::flat_spots)
  add("crossing_points",    tsfeatures::crossing_points)
  add("arch_stat",          tsfeatures::arch_stat)
  add("unitroot_kpss",      tsfeatures::unitroot_kpss)
  add("unitroot_pp",        tsfeatures::unitroot_pp)
  add("diff1_acf1",         function(z) tsfeatures::acf_features(diff(z,1))$acf1)
  add("diff1_acf10_sum",    function(z) sum(tsfeatures::acf_features(diff(z,1))$acf10))
  add("diff2_acf1",         function(z) tsfeatures::acf_features(diff(z,2))$acf1)
  add("max_level_shift",    tsfeatures::max_level_shift)
  add("max_var_shift",      tsfeatures::max_var_shift)
  add("stat_arch_lm",       tsfeatures::arch_acf)
  tibble(!!!out)
}

# -------- generic builder: wide(F*) -> long(freq,value) -> tsibble -> feats ---
compute_feats_plus <- function(df, id_cols, value_cols) {
  id_syms  <- rlang::syms(id_cols)
  key_expr <- rlang::quo(c(!!!id_syms))
  
  df_long <- df |>
    pivot_longer(cols = all_of(value_cols), names_to = "freq", values_to = "value") |>
    mutate(freq = as.numeric(str_remove(freq, "^F"))) |>
    arrange(freq)
  
  tsib <- tsibble::as_tsibble(df_long, index = freq, key = !!key_expr)
  
  # feasts path
  F1 <- feasts_feats(tsib, vcol = "value")
  
  # tsfeatures path (row-wise per key); use pick() to avoid cur_data() deprecation
  base_ids <- df_long |>
    distinct(!!!id_syms) |>
    arrange(!!!id_syms)
  
  add_tf <- base_ids |>
    rowwise() |>
    mutate(.tf = list({
      key_vals <- pick(everything())
      this <- df_long %>%
        semi_join(key_vals, by = setNames(names(key_vals), names(key_vals))) %>%
        arrange(freq)
      tf_safe(this$value)
    })) |>
    ungroup() |>
    tidyr::unnest_wider(.tf)
  
  F1 %>% left_join(add_tf, by = setNames(names(base_ids), names(base_ids)))
}

# ============================== (A) QUINTILES =================================
message("Computing tsfeatures+ for quintiles …")
q_feats <- compute_feats_plus(quintiles, id_cols = c("fishNum","quantile"), value_cols = freq_cols) |>
  left_join(quintiles |> distinct(fishNum, quantile, species),
            by = c("fishNum","quantile"))

quintiles_allfreq_plus <- quintiles |> left_join(q_feats, by = c("fishNum","quantile","species"))
quintiles_feats_plus   <- q_feats

# ============================ (B) MEDIAN PER FISH =============================
message("Computing tsfeatures+ for per-fish medians …")
median_df <- quintiles |>
  group_by(fishNum, species) |>
  summarise(across(all_of(freq_cols), \(x) median(x, na.rm = TRUE)), .groups = "drop")

m_feats <- compute_feats_plus(median_df, id_cols = "fishNum", value_cols = freq_cols) |>
  left_join(median_df |> distinct(fishNum, species), by = "fishNum")

median_allfreq_plus <- median_df |> left_join(m_feats, by = c("fishNum","species"))
median_feats_plus   <- m_feats

# Save
dir.create(here("outputs","tables"), recursive = TRUE, showWarnings = FALSE)
saveRDS(quintiles_allfreq_plus, here("outputs","tables","fish_quintiles_allfreq_tsfeat_plus.rds"))
saveRDS(quintiles_feats_plus,   here("outputs","tables","fish_quintiles_tsfeat_only_plus.rds"))
saveRDS(median_allfreq_plus,    here("outputs","tables","fish_median_allfreq_tsfeat_plus.rds"))
saveRDS(median_feats_plus,      here("outputs","tables","fish_median_tsfeat_only_plus.rds"))

message("Saved *_plus feature tables to outputs/tables/")
