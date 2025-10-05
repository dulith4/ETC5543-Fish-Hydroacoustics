# ---- Project bootstrap ----------------------------------------------------
cat("R version:", R.version.string, "\n")

# Ensure we’re at project root (don’t run from subfolders)
if (!dir.exists("Analysis") || !file.exists("README.md")) {
  stop("Please run run_all.R from the repository root (Analysis/ and README.md must exist).")
}

# Required base R packages (fast fail if missing)
req <- c("ggplot2", "reticulate", "renv")
missing <- req[!req %in% rownames(installed.packages())]
if (length(missing)) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nRun: install.packages(c(", paste(sprintf('\"%s\"', missing), collapse = ", "), "))")
}

library(ggplot2);    cat("ggplot2 OK\n")
library(reticulate); cat("reticulate OK\n")

# ---- Locate Python virtualenv: r-keras ------------------------------------
# Standard locations across Windows (with/without OneDrive), macOS, and Linux.
userprofile <- Sys.getenv("USERPROFILE")
onedrive    <- Sys.getenv("OneDrive")
candidates <- unique(na.omit(c(
  if (nzchar(userprofile)) file.path(userprofile, "Documents", ".virtualenvs", "r-keras"),
  if (nzchar(onedrive))    file.path(onedrive,    "Documents", ".virtualenvs", "r-keras"),
  normalizePath("~/Documents/.virtualenvs/r-keras", mustWork = FALSE),
  normalizePath("~/.virtualenvs/r-keras",          mustWork = FALSE)
)))
existing <- Filter(dir.exists, candidates)

if (length(existing)) {
  venv_path <- existing[[1]]
  reticulate::use_virtualenv(venv_path, required = TRUE)
  cat("Using Python virtualenv at:", venv_path, "\n")
} else {
  stop(
    "Couldn't find the 'r-keras' virtualenv.\n",
    "I looked in:\n  - ", paste(candidates, collapse = "\n  - "),
    "\n\nCreate it and install TF/Keras as per README, then rerun."
  )
}

# ---- Log console output to file (non-intrusive) -----------------------------
dir.create("outputs/run_logs", recursive = TRUE, showWarnings = FALSE)
.run_tag  <- format(Sys.time(), "%Y%m%d_%H%M%S")
.log_path <- file.path("outputs/run_logs", paste0("run_", .run_tag, ".txt"))
zz <- file(.log_path, open = "wt"); sink(zz, split = TRUE)
on.exit({ sink(); close(zz) }, add = TRUE)
message("Logging console output to: ", normalizePath(.log_path, winslash = "/"))

# ---- Quick TF/Keras sanity check (non-fatal warnings suppressed) ----------
reticulate::py_run_string("
import tensorflow as tf, keras
print('TF version:', getattr(tf,'__version__','<none>'))
print('Keras version:', getattr(keras,'__version__','<none>'))
")
cat("Keras available (R):", tryCatch(keras::is_keras_available(), error = function(e) FALSE), "\n")

# ---- Optional: ensure renv library matches lockfile -----------------------
# Comment out if you prefer to run this manually.
if (interactive()) {
  message("Checking renv status…")
  s <- renv::status()
  if (!isTRUE(s$synchronized)) {
    message("Restoring packages listed in renv.lock…")
    renv::restore(prompt = FALSE)
  } else {
    message("renv library is synchronized.")
  }
}

# ---- Helper: detect whether viewer artifacts exist ------------------------
have_artifacts <- function(path) {
  # For viewer scripts: only run if their expected outputs exist.
  # Non-viewers always run.
  list_out <- function(dir) if (dir.exists(dir)) list.files(dir) else character(0)
  tbls <- list_out("outputs/tables")
  figs <- list_out("figures")
  
  if (grepl("view_results_original\\.R$", path)) {
    any(grepl("^leaderboard_original_\\d{8}_\\d{6}\\.rds$", tbls))
  } else if (grepl("view_results_automl\\.R$", path)) {
    any(grepl("^leaderboard_original(_blocks)?_\\d{6,8}_\\d{6}\\.rds$", tbls))
  } else if (grepl("view_results_quintiles\\.R$", path)) {
    any(grepl("^automl_leaderboard_\\d{8}_\\d{6}\\.rds$", tbls))
  } else if (grepl("view_results_tsfeatures\\.R$", path)) {
    any(grepl("^leaderboard_(quintiles|median)_(allfreq|feats)_\\d{6,8}_\\d{6}\\.rds$", tbls))
  } else if (grepl("view_results_rnn\\.R$", path)) {
    any(grepl("^rnn_metrics_\\d{6,8}_\\d{6}\\.json$", tbls))
  } else {
    TRUE  # not a viewer -> run
  }
}

# ---- Runner ----------------------------------------------------------------
run_script <- function(path) {
  if (!have_artifacts(path)) {
    message(">>> Skipping (artifacts not found yet): ", path)
    return(invisible(NULL))
  }
  message(">>> Running: ", path)
  t0 <- Sys.time()
  source(path, echo = TRUE, max.deparse.length = Inf)
  dt <- round(difftime(Sys.time(), t0, units = "mins"), 2)
  message("<<< Done: ", path, " [", dt, " min]")
}

# ---- Choose what to run ----------------------------------------------------
# Heads-up: some scripts can be long-running.
# Use message() calls so progress shows in logs/CI.
scripts <- c(
  # "00_dependencies.R",                 # optional; re-snapshot packages
  # "00_smoke_test.R",                   # optional; quick env check
  
  # Data prep / transforms
  # "Analysis/02a_check_transformations.R",   # size-standardisation + backscatter
  # "Analysis/02b_fish_quantiles.R",          # quintile summary (5 per fish)
  # "Analysis/04_tsfeatures_build.R",         # build fish-level tsfeatures
  
  # Modelling (original structures)
  # "Analysis/03_classification_original.R",  # AutoML on original per-ping wide table
  # "Analysis/03a_rnn_reproduction.R",        # RNN reproduction (TF/Keras)
  
  # Modelling (quintiles)
  # "Analysis/03_classification.R",           # AutoML on quintile-transformed data
  
  # Modelling (tsfeatures variants)
  # "Analysis/05_automl_tsfeatures.R",        # AutoML on tsfeature data 
  
  # Modelling (backscatter variants)
  # "Analysis/03b_automl_backscatter.R",      # AutoML per-ping + 5-ping block mean
  
  # Viewers (no training; expect artifacts to exist — will auto-skip if not)
  # "Analysis/view_results_original.R",
  # "Analysis/view_results_quintiles.R",
  # "Analysis/view_results_rnn.R",
  # "Analysis/view_results_automl.R",
  # "Analysis/view_results_tsfeatures.R"
)

invisible(lapply(scripts, run_script))
cat("\nAll selected scripts finished. ✅\n")
