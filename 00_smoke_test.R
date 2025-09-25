cat("R version:", R.version.string, "\n")
library(ggplot2);    cat("ggplot2 OK\n")
library(reticulate); cat("reticulate OK\n")

userprofile <- Sys.getenv("USERPROFILE")
onedrive    <- Sys.getenv("OneDrive")  # empty if not using OneDrive

candidates <- unique(na.omit(c(
  if (nzchar(userprofile)) file.path(userprofile, "Documents", ".virtualenvs", "r-keras"),
  if (nzchar(onedrive))    file.path(onedrive,    "Documents", ".virtualenvs", "r-keras"),
  normalizePath("~/Documents/.virtualenvs/r-keras", mustWork = FALSE)
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

# Optional: quick TF/Keras check
reticulate::py_run_string("
import tensorflow as tf, keras
print('TF version:', getattr(tf,'__version__','<none>'))
print('Keras version:', getattr(keras,'__version__','<none>'))
")

