# Analysis/view_results_grid_dl.R
suppressPackageStartupMessages({
  library(tidyverse); library(glue); library(jsonlite); library(here)
})

# Helper: list files for a variant and return newest timestamp
.latest_ts <- function(dir, prefix, variant) {
  fs <- list.files(dir, pattern = paste0("^", prefix, "_", variant, "_\\d{8}_\\d{6}\\."), full.names = FALSE)
  if (!length(fs)) return(NA_character_)
  # pull ..._YYYYMMDD_HHMMSS.ext -> timestamp
  ts <- sub(sprintf("^%s_%s_(\\d{8}_\\d{6})\\..*$", prefix, variant), "\\1", fs)
  fs[order(ts, decreasing = TRUE)][1] |> 
    sub(sprintf("^%s_%s_(\\d{8}_\\d{6})\\..*$", prefix, variant), "\\1", x = _)
}

# Pretty print a small tibble
.pp <- function(x, n = 8) {
  if (is.null(x) || nrow(x) == 0) return(invisible(NULL))
  print(dplyr::slice_head(x, n = n))
}

# Main viewer
# variant ∈ { "quintiles_allfreq","quintiles_feats","median_allfreq","median_feats" }
# ts: optional timestamp "YYYYMMDD_HHMMSS". If NULL, uses newest artifacts.
view_results_grid_dl <- function(variant, positive = "SMB", ts = NULL, root = here("outputs","tables")) {
  stopifnot(dir.exists(root))
  if (is.null(ts) || is.na(ts)) {
    ts <- .latest_ts(root, "dlgrid_metrics", variant)
    if (is.na(ts)) {
      message("No metrics JSON found for variant '", variant, "'. Looking for leaderboard to at least show the grid summary…")
      ts <- .latest_ts(root, "dlgrid_leaderboard", variant)
      if (is.na(ts)) stop("No artifacts found for '", variant, "'.")
      metrics <- NULL
    } else {
      metrics_path <- file.path(root, glue("dlgrid_metrics_{variant}_{ts}.json"))
      metrics <- tryCatch(jsonlite::fromJSON(metrics_path), error = function(e) NULL)
    }
  } else {
    metrics <- tryCatch(
      jsonlite::fromJSON(file.path(root, glue("dlgrid_metrics_{variant}_{ts}.json"))),
      error = function(e) NULL
    )
  }
  
  # Best-effort load leaderboard for grid summary
  lb_path <- file.path(root, glue("dlgrid_leaderboard_{variant}_{ts}.rds"))
  lb <- if (file.exists(lb_path)) readr::read_rds(lb_path) else NULL
  
  cat("\n==== DL GRID — ", toupper(variant), " ====\n", sep = "")
  
  # Header line with best model + threshold/acc if metrics exist
  if (!is.null(metrics)) {
    # expected fields written by 08_grid_dl_tsfeatures.R:
    # model_id, algorithm, policy_thr_clip, acc_test_at_050, acc_test_at_policy
    mdl <- metrics$model_id %||% ""
    alg <- metrics$algorithm %||% "deeplearning"
    thr <- metrics$policy_thr_clip %||% NA
    acc50 <- metrics$acc_test_at_050 %||% NA
    accpol <- metrics$acc_test_at_policy %||% NA
    
    cat("Best model id: ", mdl, " (", alg, ")\n", sep = "")
    if (!is.na(thr)) {
      cat("Policy thr (VALID max-ACC, clipped): ", sprintf("%.6f", as.numeric(thr)), "\n", sep = "")
    } else {
      cat("Policy thr (VALID max-ACC, clipped): <not recorded>\n")
    }
    if (!is.na(acc50))  cat("TEST acc @ 0.50:  ", sprintf("%.3f", as.numeric(acc50)), "\n", sep = "")
    if (!is.na(accpol)) cat("TEST acc @ policy:", sprintf("%.3f", as.numeric(accpol)), "\n", sep = "")
  } else {
    cat("Best model id:  (deeplearning)\n")
    cat("Policy thr (VALID max-ACC, clipped): \n")
  }
  cat("\n")
  
  # Grid summary table (top by AUC if present)
  if (!is.null(lb) && nrow(lb)) {
    # normalise typical columns we saved
    # choose a compact set of columns if available
    keep <- intersect(c("activation","adaptive_rate","balance_classes","epochs","epsilon",
                        "hidden","hidden_dropout_ratios","input_dropout_ratio",
                        "l1","l2","max_w2","rate","rate_annealing","rho",
                        "momentum_start","momentum_stable","model_ids","auc","logloss"),
                      names(lb))
    cat("Grid summary (top 8 by AUC):\n")
    lb_show <- lb
    if ("auc" %in% names(lb_show)) lb_show <- dplyr::arrange(lb_show, dplyr::desc(auc))
    .pp(dplyr::select(lb_show, all_of(keep)), n = 8)
  } else {
    cat("No leaderboard table found for timestamp ", ts, ".\n", sep = "")
  }
  
  # Help if empty fields
  if (is.null(metrics)) {
    cat("\nHint: I couldn’t find dlgrid_metrics for this variant/timestamp.\n",
        "Make sure 08_grid_dl_tsfeatures.R completed and wrote:\n  ",
        file.path(root, glue("dlgrid_metrics_{variant}_{ts}.json")), "\n", sep = "")
  }
  
  invisible(TRUE)
}

`%||%` <- function(x,y) if (is.null(x)) y else x
