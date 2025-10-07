# ==============================================================================
# Utils: H2O model saving (MOJO + Binary) with metadata & leaderboard
# ==============================================================================

suppressPackageStartupMessages({
  library(glue); library(lubridate); library(jsonlite)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b
.ts <- function() format(with_tz(Sys.time(), "Australia/Melbourne"), "%Y%m%d_%H%M%S")

# Helper: on Windows, try to shorten a path (8.3) to dodge MAX_PATH issues
.short_path <- function(p) {
  if (.Platform$OS.type == "windows") {
    sp <- try(utils::shortPathName(p), silent = TRUE)
    if (!inherits(sp, "try-error") && nzchar(sp)) return(sp)
  }
  p
}

# Optionally pass extra metadata (e.g. thresholds) via `extras = list(policy_thr = 0.62, clamp = c(0.40, 0.70))`
save_h2o_artifacts <- function(model, tag, leaderboard = NULL, train = NULL, save_binary = TRUE, extras = NULL) {
  stopifnot(!is.null(model), nzchar(tag))
  
  # Keep the on-disk path short (Windows/OneDrive safe)
  id_short <- substr(gsub("[^A-Za-z0-9]+", "_", model@model_id), 1, 40)
  dir_root <- file.path("outputs", "models", tag, glue("{.ts()}__{id_short}"))
  dir.create(dir_root, recursive = TRUE, showWarnings = FALSE)
  
  # Use a short path for writing (but keep metadata paths pretty)
  dir_write <- .short_path(dir_root)
  
  # --- MOJO (portable scoring) -----------------------------------------------
  # Write directly as 'model.mojo.zip' to avoid rename shenanigans
  mojo_zip <- file.path(dir_root, "model.mojo.zip")
  # try direct write
  mojo_ok <- TRUE
  mojo_ret <- try(
    h2o.download_mojo(model, path = dir_write, filename = "model.mojo.zip", get_genmodel_jar = TRUE),
    silent = TRUE
  )
  if (inherits(mojo_ret, "try-error")) {
    mojo_ok <- FALSE
  } else {
    # Some H2O versions return just a basename; ensure the file is really there
    mojo_found <- file.path(dir_write, basename("model.mojo.zip"))
    if (!file.exists(mojo_found)) {
      # fallback: maybe H2O ignored filename; look for any *.zip just created
      zips <- list.files(dir_write, pattern = "\\.zip$", full.names = TRUE)
      if (length(zips)) {
        file.copy(zips[1], file.path(dir_write, "model.mojo.zip"), overwrite = TRUE)
        mojo_found <- file.path(dir_write, "model.mojo.zip")
      }
    }
    if (file.exists(mojo_found) && !identical(normalizePath(mojo_found), normalizePath(mojo_zip))) {
      # copy to the pretty path (dir_root)
      file.copy(mojo_found, mojo_zip, overwrite = TRUE)
    }
    mojo_ok <- file.exists(mojo_zip)
  }
  
  if (!mojo_ok) {
    warning("MOJO download failed; continuing without MOJO zip.")
    mojo_zip <- NULL
  }
  
  # --- genmodel JAR (put it next to the MOJO) --------------------------------
  genmodel_jar <- file.path(dir_root, "h2o-genmodel.jar")
  if (!file.exists(genmodel_jar)) {
    candidates <- c(
      file.path(dir_write, "h2o-genmodel.jar"),
      file.path(getwd(),   "h2o-genmodel.jar")
    )
    cand <- candidates[file.exists(candidates)]
    if (length(cand)) file.copy(cand[1], genmodel_jar, overwrite = TRUE)
    if (!file.exists(genmodel_jar)) genmodel_jar <- NULL
  }
  
  # --- Optional native binary (same-version reload only) ----------------------
  binary_dir <- NULL
  if (isTRUE(save_binary)) {
    binary_dir <- file.path(dir_root, "h2o-binary")
    dir.create(binary_dir, showWarnings = FALSE)
    # use short path for write
    h2o.saveModel(model, path = .short_path(binary_dir), force = TRUE)
  }
  
  # --- Leaderboard CSV (optional) --------------------------------------------
  leaderboard_csv <- NULL
  if (!is.null(leaderboard)) {
    leaderboard_csv <- file.path(dir_root, "leaderboard.csv")
    utils::write.csv(as.data.frame(leaderboard), leaderboard_csv, row.names = FALSE)
  }
  
  # --- Metadata ---------------------------------------------------------------
  meta <- list(
    tag = tag,
    saved_at = .ts(),
    model_id = model@model_id,
    algorithm = model@algorithm,
    h2o_version = as.character(utils::packageVersion("h2o")),
    r_version = R.version.string,
    target = tryCatch(model@allparameters$y, error = function(e) NA_character_),
    features = tryCatch(model@allparameters$x, error = function(e) NA_character_),
    artifacts = list(
      mojo_zip = if (!is.null(mojo_zip) && file.exists(mojo_zip)) mojo_zip else NULL,
      genmodel_jar = genmodel_jar,
      binary_dir = if (!is.null(binary_dir) && dir.exists(binary_dir)) binary_dir else NULL,
      leaderboard_csv = leaderboard_csv
    ),
    extras = extras %||% NULL
  )
  writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE),
             file.path(dir_root, "meta.json"))
  
  # --- README ----------------------------------------------------------------
  readme <- c(
    "# Model Artifacts",
    glue("- Tag: {tag}"),
    glue("- Model ID: {model@model_id}"),
    "",
    "Files:",
    "- model.mojo.zip (portable scoring)",
    if (isTRUE(save_binary)) "- h2o-binary/ (same-version reload)" else NULL,
    if (!is.null(leaderboard)) "- leaderboard.csv" else NULL,
    if (!is.null(genmodel_jar)) "- h2o-genmodel.jar (Java CLI scoring)" else NULL,
    "",
    "How to score in R (cluster MOJO scoring):",
    "  library(h2o); h2o.init()",
    "  m <- h2o.import_mojo('model.mojo.zip')",
    "  pred <- h2o.predict(m, as.h2o(newdata))",
    "",
    "How to score via Java CLI:",
    "  java -cp h2o-genmodel.jar hex.genmodel.tools.PredictCsv \\",
    "    --mojo model.mojo.zip --input input.csv --output preds.csv --decimal"
  )
  writeLines(readme, file.path(dir_root, "README.txt"))
  
  message(glue("Saved artifacts to: {normalizePath(dir_root)}"))
  invisible(dir_root)
}

best_from_aml <- function(aml) aml@leader
