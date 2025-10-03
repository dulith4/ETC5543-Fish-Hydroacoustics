# ==============================================================================
# Utils: H2O model saving (MOJO + Binary) with metadata & leaderboard
# ==============================================================================


suppressPackageStartupMessages({
  library(glue); library(lubridate); library(jsonlite)
})

.ts <- function() format(with_tz(Sys.time(), "Australia/Melbourne"), "%Y%m%d_%H%M%S")

save_h2o_artifacts <- function(model, tag, leaderboard = NULL, train = NULL, save_binary = TRUE) {
  stopifnot(!is.null(model), nzchar(tag))
  dir_root <- file.path("outputs", "models", tag, glue("{.ts()}__{model@model_id}"))
  dir.create(dir_root, recursive = TRUE, showWarnings = FALSE)
  
  # MOJO
  mojo_file <- "model.mojo.zip"
  h2o.save_mojo(model, path = dir_root, filename = mojo_file, force = TRUE)
  
  # Optional native binary
  if (isTRUE(save_binary)) {
    dir.create(file.path(dir_root, "h2o-binary"), showWarnings = FALSE)
    h2o.saveModel(model, path = file.path(dir_root, "h2o-binary"), force = TRUE)
  }
  
  # Leaderboard CSV (optional)
  if (!is.null(leaderboard)) {
    utils::write.csv(as.data.frame(leaderboard), file.path(dir_root, "leaderboard.csv"), row.names = FALSE)
  }
  
  # Metadata
  meta <- list(
    tag = tag,
    saved_at = .ts(),
    model_id = model@model_id,
    algorithm = model@algorithm,
    h2o_version = as.character(utils::packageVersion("h2o")),
    r_version = R.version.string,
    target = tryCatch(model@allparameters$y, error = function(e) NA_character_),
    features = tryCatch(model@allparameters$x, error = function(e) NA_character_),
    artifacts = list(mojo = file.path(dir_root, mojo_file),
                     binary_dir = if (save_binary) file.path(dir_root, "h2o-binary") else NULL,
                     leaderboard_csv = if (!is.null(leaderboard)) file.path(dir_root, "leaderboard.csv") else NULL)
  )
  writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), file.path(dir_root, "meta.json"))
  
  # README
  writeLines(c(
    "# Model Artifacts",
    glue("- Tag: {tag}"),
    glue("- Model ID: {model@model_id}"),
    "",
    "Files:",
    "- model.mojo.zip",
    if (isTRUE(save_binary)) "- h2o-binary/ (same-version reload)" else NULL,
    if (!is.null(leaderboard)) "- leaderboard.csv" else NULL
  ), file.path(dir_root, "README.txt"))
  
  message(glue("Saved artifacts to: {normalizePath(dir_root)}"))
  invisible(dir_root)
}

best_from_aml <- function(aml) aml@leader