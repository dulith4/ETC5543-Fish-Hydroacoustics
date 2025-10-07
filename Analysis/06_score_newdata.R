# Analysis/06_score_newdata.R
# Score new rows with an H2O MOJO and apply the policy threshold to output labels.

suppressPackageStartupMessages({
  library(h2o); library(jsonlite); library(readr); library(dplyr)
})

prob_col <- function(df, positive = "SMB") {
  n <- names(df)
  if (positive %in% n) return(positive)
  cand <- c("p1", paste0("prob_", positive), "TRUE", "1")
  hit <- cand[cand %in% n]
  if (length(hit)) return(hit[1])
  pcols <- grep("^(p\\d+|prob_.*|LT|SMB)$", n, value = TRUE)
  pcols <- setdiff(pcols, c("predict"))
  if (length(pcols)) return(pcols[1])
  stop("No probability column for class '", positive, "'.")
}

# ---- function API -------------------------------------------------------------
score_newdata <- function(mojo_dir, input_csv, threshold, out_csv = NULL, positive = "SMB") {
  if (is.null(out_csv)) {
    dir.create("outputs/preds", recursive = TRUE, showWarnings = FALSE)
    out_csv <- file.path("outputs/preds", paste0("preds_", basename(mojo_dir), ".csv"))
  }
  
  # find mojo + meta
  meta_path <- list.files(mojo_dir, pattern = "meta\\.json$", full.names = TRUE, recursive = TRUE)
  if (!length(meta_path)) stop("meta.json not found in: ", mojo_dir)
  meta <- jsonlite::fromJSON(meta_path[1])
  
  mojo_zip <- list.files(mojo_dir, pattern = "model\\.mojo\\.zip$", full.names = TRUE, recursive = TRUE)
  if (!length(mojo_zip)) stop("model.mojo.zip not found under: ", mojo_dir)
  mojo_zip <- mojo_zip[1]
  
  features <- unlist(meta$features)
  if (!length(features)) stop("Feature list empty in meta.json")
  
  # load new data
  new <- suppressMessages(readr::read_csv(input_csv, show_col_types = FALSE))
  
  # check & reorder columns
  missing <- setdiff(features, names(new))
  if (length(missing)) stop("Input CSV missing required columns: ", paste(missing, collapse = ", "))
  new <- dplyr::select(new, dplyr::all_of(features))
  
  # init / import mojo / predict
  if (is.null(tryCatch(h2o.getConnection(), error = function(e) NULL))) {
    h2o.init(nthreads = -1)
  }
  m <- h2o.import_mojo(mojo_zip)
  ph <- as.h2o(new)
  pr <- as.data.frame(h2o.predict(m, ph))
  
  # apply threshold
  pc <- prob_col(pr, positive = positive)
  pr$label_policy <- ifelse(pr[[pc]] >= threshold, positive, setdiff(c("LT","SMB"), positive)[1])
  
  # merge back row ids if present
  out <- cbind(new, pr)
  readr::write_csv(out, out_csv)
  message("Wrote predictions -> ", normalizePath(out_csv))
  invisible(out_csv)
}

# ---- auto-run example (only when sourced interactively) ----------------------
if (interactive()) {
  # change these three lines for ad-hoc testing
  latest_dir <- list.dirs("outputs/models/tsf_quint_all", recursive = FALSE, full.names = TRUE)
  latest_dir <- latest_dir[which.max(file.info(latest_dir)$mtime)]
  score_newdata(
    mojo_dir  = latest_dir,
    input_csv = "templates/newdata_template_quint_allfreq.csv", # put real data here
    threshold = 0.4489,                                         # replace with your final value
    out_csv   = "outputs/preds/example_preds_quint_allfreq.csv"
  )
}
