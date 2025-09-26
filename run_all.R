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

# Example pipeline — adjust to your files:
scripts <- c(
  # "Analysis/01_data_prep.R",
  # "Analysis/02_feature_engineering.R",
  "Analysis/03_classification_original.R"
  # "Analysis/03_rnn_classification.R"  # (long-running; uses Keras/TF)
)

invisible(lapply(scripts, run_script))

cat("\nAll selected scripts finished. ✅\n")
