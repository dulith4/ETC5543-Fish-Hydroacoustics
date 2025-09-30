cat("R version:", R.version.string, "\n")

suppressPackageStartupMessages({
  library(ggplot2);    cat("ggplot2 OK\n")
  library(reticulate); cat("reticulate OK\n")
})

userprofile <- Sys.getenv("USERPROFILE")
onedrive    <- Sys.getenv("OneDrive")

# Prefer explicit option if set (same key used in 00_dependencies.R)
opt_venv <- getOption("fishhydro.rkeras.path", NA_character_)

candidates <- unique(na.omit(c(
  opt_venv,
  if (nzchar(userprofile)) file.path(userprofile, "Documents", ".virtualenvs", "r-keras"),
  if (nzchar(onedrive))    file.path(onedrive,    "Documents", ".virtualenvs", "r-keras"),
  normalizePath("~/Documents/.virtualenvs/r-keras", mustWork = FALSE)
)))

cat("Searching for r-keras virtualenv in:\n", paste(" -", candidates), "\n")

existing <- Filter(dir.exists, candidates)
if (!length(existing)) {
  stop(
    "Couldn't find the 'r-keras' virtualenv.\n",
    "I looked in:\n  - ", paste(candidates, collapse = "\n  - "),
    "\n\nCreate it and install TF/Keras (2.15.0) as per README, then rerun."
  )
}

venv_path <- existing[[1]]
reticulate::use_virtualenv(venv_path, required = TRUE)
cat("Using Python virtualenv at:", venv_path, "\n")

# Check TF/Keras import & versions
ok <- try(reticulate::py_run_string("
import tensorflow as tf, keras
print('TF version:', getattr(tf,'__version__','<none>'))
print('Keras version:', getattr(keras,'__version__','<none>'))
"), silent = TRUE)

if (inherits(ok, "try-error")) {
  stop("Python packages not ready in venv: ", venv_path,
       "\nTry:\n  ",
       file.path(venv_path, "Scripts", "python.exe"),
       " -m pip install tensorflow==2.15.0 keras==2.15.0")
}

invisible(TRUE)
