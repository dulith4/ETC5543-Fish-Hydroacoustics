# ---- Project bootstrap ----------------------------------------------------
cat("R version:", R.version.string, "\n")

# Required base R packages (fast fail if missing)
req <- c("ggplot2", "reticulate", "renv")
missing <- req[!req %in% rownames(installed.packages())]
if (length(missing)) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nRun: install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))")
}

library(ggplot2);   cat("ggplot2 OK\n")
library(reticulate);cat("reticulate OK\n")

# ---- Locate Python virtualenv: r-keras ------------------------------------
# We standardize on a folder named 'r-keras'. These are the most common locations
# across Windows (with/without OneDrive), macOS, and Linux.
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

# ---- Run project analyses (edit to suit your repo) ------------------------
# Heads-up: some scripts can be long-running.
# Use message() calls so progress shows in logs/CI.
run_script <- function(path) {
  message(">>> Running: ", path)
  t0 <- Sys.time()
  source(path, echo = TRUE, max.deparse.length = Inf)
  message("<<< Done: ", path, " [", round(difftime(Sys.time(), t0, units = "mins"), 2), " min]")
}

# Example pipeline — adjust to your files: (un)comment as needed. and if you only running one script,
#remove the comma after the last entry.
scripts <- c(
  # "00_dependencies.R",              # (optional; re-snapshot packages)
  # "00_smoke_test.R",                # (optional; quick env check)
  
  # Data prep / transforms
  # "Analysis/02a_check_transformations.R",   # size-standardisation + backscatter
  # "Analysis/02b_fish_quantiles.R",          # quintile summary (5 per fish)
  # "Analysis/04_tsfeatures_build.R",        # build fish-level tsfeatures
  
  # Modelling (original structures)
  # "Analysis/03_classification_original.R",  # AutoML on original per-ping wide table
  # "Analysis/03a_rnn_reproduction.R",        # RNN reproduction (TF/Keras)
  
  # Modelling (quintiles)
  # "Analysis/03_classification.R",           # AutoML on quintile-transformed data
  
  # Modelling (tsfeatures variants)
  # "Analysis/05_automl_tsfeatures.R",        # AutoML on tsfeature data 
  
  # Modelling (backscatter variants)
  # "Analysis/03b_automl_backscatter.R",      # AutoML per-ping + 5-ping block mean
  
  
  # Viewers (no training; expect artifacts to exist)
  # "Analysis/view_results_original.R",       # viewer for 03_classification_original.R
  # "Analysis/view_results_quintiles.R",      # viewer for 03_classification.R
  # "Analysis/view_results_rnn.R",            # viewer for 03a_rnn_reproduction.R
  # "Analysis/view_results_automl.R",         # viewer for 03b_automl_backscatter.R
  # "Analysis/view_results_tsfeatures.R"      # viewer for 05_automl_tsfeatures.R 
)
invisible(lapply(scripts, run_script))

cat("\nAll selected scripts finished. ✅\n")



# === Reminder for adding new scripts ===
# 1. Save the new script.
# 2. Add the path inside the 'scripts <- c(...)' vector above.
# 3. Keep the order logical (data prep -> feature eng -> modelling).
# 4. To skip a script without deleting it, just comment it out with #.
