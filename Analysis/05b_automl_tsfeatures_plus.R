# ======================================================================
# 05b_automl_tsfeatures_plus.R — AutoML using *_plus features
# - Extended ts-features (+ optional top-K F* for *FEATS variants)
# - Stratified split by fish
# - Variant-aware threshold clamp
# - Chooses FINAL policy threshold on VALID among {0.50, raw, clipped}
# - Robust to no-CV runs (median_* use nfolds=0), safer retries
# ======================================================================

rm(list = ls()); invisible(gc())

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(glue)
  library(h2o)
  library(lubridate)
  library(jsonlite)
})

source("Analysis/utils_thresholds.R")   # acc_threshold(), thr_max_acc()
source("Analysis/utils_models.R")       # save_h2o_artifacts()
source("Analysis/04c_freqselectors.R")  # select_discriminative_freqs()

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
dir.create("figures",        recursive = TRUE, showWarnings = FALSE)

ts_tag <- format(with_tz(Sys.time(), "Australia/Melbourne"), "%Y%m%d_%H%M%S")

# ---- Ensure *_plus datasets exist --------------------------------------------
paths_needed <- c(
  qa = here("outputs","tables","fish_quintiles_allfreq_tsfeat_plus.rds"),
  qf = here("outputs","tables","fish_quintiles_tsfeat_only_plus.rds"),
  ma = here("outputs","tables","fish_median_allfreq_tsfeat_plus.rds"),
  mf = here("outputs","tables","fish_median_tsfeat_only_plus.rds")
)
if (!all(file.exists(paths_needed))) {
  message("Building *_plus feature tables …")
  source(here("Analysis","04e_tsfeatures_plus.R"), local = TRUE)
  if (!all(file.exists(paths_needed))) stop("Expected *_plus tables not created by 04e_tsfeatures_plus.R")
}

quintiles_allfreq <- readRDS(paths_needed["qa"])
quintiles_feats   <- readRDS(paths_needed["qf"])
median_allfreq    <- readRDS(paths_needed["ma"])
median_feats      <- readRDS(paths_needed["mf"])

# ---- Splitter: stratified by species, grouped by fishNum ----------------------
split_by_fish_strat <- function(df, p_train = 0.6, p_valid = 0.2, seed = 73) {
  stopifnot(all(c("fishNum","species") %in% names(df)))
  set.seed(seed)
  ids <- df |> distinct(fishNum, species)
  
  split_one <- function(di) {
    n <- nrow(di)
    idx <- sample.int(n)
    n_tr <- max(1, floor(p_train * n))
    n_va <- max(1, floor(p_valid * n))
    n_te <- max(1, n - n_tr - n_va)
    while (n_tr + n_va + n_te > n) {
      if (n_tr > 1) n_tr <- n_tr - 1
      else if (n_va > 1) n_va <- n_va - 1
      else n_te <- n_te - 1
    }
    tibble(
      fishNum = di$fishNum[idx],
      split   = c(rep("train", n_tr), rep("valid", n_va), rep("test", n - n_tr - n_va))
    )
  }
  
  ids_split <- purrr::map_dfr(group_split(ids, species), split_one)
  ids_split |>
    right_join(df, by = "fishNum") |>
    relocate(split)
}

# ---- Top-K discriminative frequencies (train-only) ---------------------------
K <- 20

augment_feats_with_topF <- function(df_feats, df_allfreq, split_tbl, keys, k = K, positive = "SMB") {
  tr_ids <- split_tbl |>
    filter(split == "train") |>
    distinct(!!!rlang::syms(keys), species)
  
  df_tr_all <- df_allfreq |>
    semi_join(tr_ids, by = c(setNames(keys, keys), "species"))
  
  topF <- select_discriminative_freqs(df_tr_all, k = k, positive = positive)
  if (length(topF) == 0L) {
    warning("No discriminative frequencies found; returning feats un-augmented.")
    return(df_feats)
  }
  message("Selected top-", length(topF), " freqs: ", paste0(topF, collapse = ", "))
  
  df_feats |>
    left_join(df_allfreq |> select(all_of(c(keys, "species", topF))),
              by = c(setNames(keys, keys), "species"))
}

# ---- Variant-aware threshold clipping ----------------------------------------
clip_thr_variant <- function(t, name) {
  # default window for ALLFREQ variants
  lo <- 0.50; hi <- 0.80
  # FEATS variants usually need higher cuts
  if (grepl("_feats_", name)) { lo <- 0.55; hi <- 0.85 }
  pmin(pmax(as.numeric(t), lo), hi)
}

# ---- H2O init / health --------------------------------------------------------
h2o_up     <- function() !is.null(tryCatch(h2o.getConnection(), error = function(e) NULL))
ensure_h2o <- function(heap = "6G") { if (!h2o_up()) h2o.init(nthreads = -1, max_mem_size = heap); invisible(TRUE) }

wait_cluster <- function(retries = 20, sleep_sec = 0.5) {
  ok <- FALSE
  for (i in seq_len(retries)) {
    ok <- tryCatch({ h2o.clusterInfo(); TRUE }, error = function(e) FALSE)
    if (ok) break
    Sys.sleep(sleep_sec)
  }
  if (!ok) stop("H2O cluster not responding after init.")
}

ensure_h2o("6G"); wait_cluster()

# ---- Variants list ------------------------------------------------------------
variants <- list(
  list(
    name        = "quintiles_allfreq_plus",
    data        = quintiles_allfreq,
    tag         = "tsf_quint_all_plus",
    keys        = c("fishNum","quantile"),
    needs_topF  = FALSE,
    allfreq_ref = quintiles_allfreq
  ),
  list(
    name        = "quintiles_feats_plus",
    data        = quintiles_feats,
    tag         = "tsf_quint_feats_plus",
    keys        = c("fishNum","quantile"),
    needs_topF  = TRUE,
    allfreq_ref = quintiles_allfreq
  ),
  list(
    name        = "median_allfreq_plus",
    data        = median_allfreq,
    tag         = "tsf_median_all_plus",
    keys        = c("fishNum"),
    needs_topF  = FALSE,
    allfreq_ref = median_allfreq
  ),
  list(
    name        = "median_feats_plus",
    data        = median_feats,
    tag         = "tsf_median_feats_plus",
    keys        = c("fishNum"),
    needs_topF  = TRUE,
    allfreq_ref = median_allfreq
  )
)

# ---- Runner (robust) ----------------------------------------------------------
run_one <- function(v, seed = 73, budget = 600, positive = "SMB") {
  message("\n=== AutoML variant: ", v$name, " ===")
  ensure_h2o(); h2o.removeAll()
  
  # Split table (adds `split` column)
  sp <- split_by_fish_strat(v$data, seed = seed)
  
  # Augment FEATS variants with top-K F* chosen from TRAIN only
  d_use <- v$data
  if (v$needs_topF) {
    d_use <- augment_feats_with_topF(
      df_feats   = v$data,
      df_allfreq = v$allfreq_ref,
      split_tbl  = sp,
      keys       = v$keys,
      k          = K,
      positive   = positive
    )
  }
  
  # --- Build modeling table once (avoid dup/suffix names) ---------------------
  base_idx <- sp %>%
    dplyr::select(dplyr::all_of(c(v$keys, "species", "split"))) %>%
    dplyr::distinct()
  
  mod_df <- base_idx %>%
    dplyr::left_join(d_use, by = c(setNames(v$keys, v$keys), "species"))
  
  x_cols <- setdiff(names(mod_df), c("species", v$keys, "split"))
  stopifnot("species" %in% names(mod_df), length(x_cols) > 0)
  
  tr_df <- mod_df %>% filter(split == "train") %>% select(all_of(c("species", x_cols)))
  va_df <- mod_df %>% filter(split == "valid") %>% select(all_of(c("species", x_cols)))
  te_df <- mod_df %>% filter(split == "test")  %>% select(all_of(c("species", x_cols)))
  
  hex_tr <- as.h2o(tr_df); hex_va <- as.h2o(va_df); hex_te <- as.h2o(te_df)
  hex_tr[,"species"] <- h2o.asfactor(hex_tr[,"species"])
  hex_va[,"species"] <- h2o.asfactor(hex_va[,"species"])
  hex_te[,"species"] <- h2o.asfactor(hex_te[,"species"])
  
  # ---- AutoML -----------------------------------------------------------------
  nf <- if (grepl("^median_", v$name)) 0 else 5  # tiny sets: disable CV
  aml <- h2o.automl(
    x = x_cols, y = "species",
    training_frame    = hex_tr,
    validation_frame  = hex_va,
    leaderboard_frame = hex_te,
    max_runtime_secs  = budget,
    sort_metric       = "AUC",
    nfolds            = nf,
    seed              = seed,
    balance_classes   = FALSE
  )
  
  best    <- aml@leader
  lb_full <- h2o.get_leaderboard(aml, extra_columns = "ALL")
  
  # ---- Metrics & thresholds (robust to nf = 0) --------------------------------
  cv_auc        <- NA_real_
  thr_cv_raw    <- NA_real_
  thr_cv_clip   <- NA_real_
  if (nf > 1) {
    perfcv <- h2o.performance(best, xval = TRUE)
    if (!is.null(perfcv@metrics) || length(perfcv@metrics) > 0) {
      cv_auc      <- suppressWarnings(h2o.auc(perfcv))
      thr_cv_raw  <- suppressWarnings(as.numeric(h2o.find_threshold_by_max_metric(perfcv, "f1")))
      if (is.finite(thr_cv_raw)) thr_cv_clip <- clip_thr_variant(thr_cv_raw, v$name)
    }
  }
  
  # Predictions (for thresholds + saving)
  pr <- as.data.frame(h2o.predict(best, hex_tr)) |> dplyr::bind_cols(species = as.character(tr_df$species), .before = 1)
  pv <- as.data.frame(h2o.predict(best, hex_va)) |> dplyr::bind_cols(species = as.character(va_df$species), .before = 1)
  pt <- as.data.frame(h2o.predict(best, hex_te)) |> dplyr::bind_cols(species = as.character(te_df$species), .before = 1)
  
  # probability column chooser
  prob_col <- function(df, positive = "SMB") {
    n <- names(df)
    if (positive %in% n) return(positive)
    cand <- c("p1", paste0("prob_", positive), "TRUE", "1")
    hit  <- cand[cand %in% n]; if (length(hit)) return(hit[1])
    pcols <- grep("^(p\\d+|prob_.*|LT|SMB)$", n, value = TRUE)
    pcols <- setdiff(pcols, c("predict","species"))
    if (length(pcols)) return(pcols[1])
    stop("No probability column for class '", positive, "'.")
  }
  pc <- prob_col(pt, positive)
  
  # VALID max-ACC policy (variant-aware clamp)
  thr_valid_raw   <- thr_max_acc(truth = pv$species, prob = pv[[pc]], positive = positive)
  thr_policy_clip <- clip_thr_variant(thr_valid_raw, v$name)
  
  # --- Pick FINAL policy threshold using VALID accuracy among {0.50, raw, clipped}
  neg <- setdiff(unique(pv$species), positive)[1]
  acc_at <- function(th) mean(ifelse(pv[[pc]] >= th, positive, neg) == pv$species)
  
  acc_valid_050   <- acc_at(0.50)
  acc_valid_raw   <- if (is.finite(thr_valid_raw))   acc_at(thr_valid_raw)   else NA_real_
  acc_valid_clip  <- if (is.finite(thr_policy_clip)) acc_at(thr_policy_clip) else NA_real_
  
  cand <- tibble(
    name = c("baseline_050","policy_raw","policy_clipped"),
    thr  = c(0.50,          thr_valid_raw, thr_policy_clip),
    acc  = c(acc_valid_050, acc_valid_raw, acc_valid_clip)
  ) %>% filter(is.finite(thr), is.finite(acc))
  
  pick_idx <- if (nrow(cand)) which.max(cand$acc) else 1L
  thr_policy_final  <- cand$thr[pick_idx]
  thr_policy_choice <- cand$name[pick_idx]
  
  # ---- Baselines + @policy ----------------------------------------------------
  perf_test        <- h2o.performance(best, hex_te)
  acc_argmax       <- if ("predict" %in% names(pt)) mean(pt$species == pt$predict) else NA_real_
  acc_test_050     <- suppressWarnings(as.numeric(h2o.accuracy(perf_test, thresholds = 0.50)))
  
  acc_test_clip <- NA_real_
  if (is.finite(thr_policy_clip)) {
    acc_test_clip <- suppressWarnings(as.numeric(h2o.accuracy(perf_test, thresholds = thr_policy_clip)))
  }
  
  # Test accuracies computed from saved probabilities (independent of h2o grid)
  acc_test_raw   <- if (is.finite(thr_valid_raw))    mean(ifelse(pt[[pc]] >= thr_valid_raw,    positive, neg) == pt$species) else NA_real_
  acc_test_final <- if (is.finite(thr_policy_final)) mean(ifelse(pt[[pc]] >= thr_policy_final, positive, neg) == pt$species) else NA_real_
  
  # Train/Valid accuracies @ FINAL threshold
  acc_train_policy <- if (is.finite(thr_policy_final)) mean(ifelse(pr[[prob_col(pr, positive)]] >= thr_policy_final, positive, neg) == pr$species) else NA_real_
  acc_valid_policy <- if (is.finite(thr_policy_final)) mean(ifelse(pv[[pc]] >= thr_policy_final,                   positive, neg) == pv$species) else NA_real_
  
  # ---- Persist artifacts ------------------------------------------------------
  readr::write_rds(as_tibble(lb_full), here("outputs","tables", glue("leaderboard_{v$name}_{ts_tag}.rds")))
  readr::write_rds(as_tibble(pr),      here("outputs","tables", glue("preds_{v$name}_train_{ts_tag}.rds")))
  readr::write_rds(as_tibble(pv),      here("outputs","tables", glue("preds_{v$name}_valid_{ts_tag}.rds")))
  readr::write_rds(as_tibble(pt),      here("outputs","tables", glue("preds_{v$name}_test_{ts_tag}.rds")))
  
  metrics <- list(
    variant               = v$name,
    cv_auc                = cv_auc,
    thr_cv_f1_raw         = thr_cv_raw,
    thr_cv_f1_clip        = thr_cv_clip,
    acc_test_at_050       = acc_test_050,
    acc_test_at_clip      = acc_test_clip,
    acc_test_argmax       = acc_argmax,
    thr_policy_raw        = as.numeric(thr_valid_raw),
    thr_policy_clip       = as.numeric(thr_policy_clip),
    thr_policy_final      = as.numeric(thr_policy_final),   # FINAL
    thr_policy_choice     = thr_policy_choice,              # FINAL choice name
    acc_train_at_policy   = acc_train_policy,               # @FINAL
    acc_valid_at_policy   = acc_valid_policy,               # @FINAL
    acc_test_at_policy    = acc_test_final,                 # @FINAL
    acc_test_at_policyRaw = acc_test_raw,                   # reference
    positive_class        = positive
  )
  readr::write_file(jsonlite::toJSON(metrics, pretty = TRUE, auto_unbox = TRUE),
                    here("outputs","tables", glue("automl_metrics_{v$name}_{ts_tag}.json")))
  
  # Save MOJO/binary with extras
  save_h2o_artifacts(
    model       = best,
    tag         = v$tag,
    leaderboard = lb_full,
    train       = hex_tr,
    save_binary = TRUE,
    extras      = list(
      positive_class = positive,
      policy_thr     = thr_policy_final,                        # FINAL
      clamp          = if (grepl("_feats_", v$name)) c(0.55, 0.85) else c(0.50, 0.80)
    )
  )
  
  invisible(TRUE)
}

safe_run <- function(v, tries = 2) {
  attempt <- 1
  repeat {
    ok <- tryCatch({ run_one(v); TRUE },
                   error = function(e) {
                     message("…caught error (", attempt, "): ", conditionMessage(e))
                     FALSE
                   })
    if (ok) break
    if (attempt >= tries) stop("Failed after ", tries, " attempts for variant: ", v$name)
    # full reset
    message("Retrying with fresh H2O …")
    try(h2o.shutdown(prompt = FALSE), silent = TRUE)
    Sys.sleep(3)
    ensure_h2o("6G")
    wait_cluster()
    h2o.removeAll()
    attempt <- attempt + 1
  }
}

invisible(lapply(variants, safe_run))
message("\nAutoML (+extended features, +topK for FEATS) complete.\n")
