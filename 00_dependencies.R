# Purpose: Central place to declare all R packages used in this project.
# This helps renv::snapshot() capture them reliably.

# -------------------------------------------------------------------------
# Dependency snapshot reminder:
# After editing this file (e.g., adding/removing libraries),
# run the following in the R console to update renv.lock:
#
#   source("00_dependencies.R")          # load all listed packages
#   renv::snapshot(type = "explicit")    # record them in renv.lock
# -------------------------------------------------------------------------

## --- Environment guard rails (runs on every session) ---

# Put renv project libs in a stable place (works for all collaborators too)
Sys.setenv(RENV_PATHS_LIBRARY_ROOT = "C:/R/renv-lib")

# Activate renv for this project
source("renv/activate.R")

# Ensure reticulate uses the pinned Python venv
suppressPackageStartupMessages(library(reticulate))

# Allow an override via option if someone’s venv lives elsewhere:
venv_path <- getOption(
  "fishhydro.rkeras.path",
  "C:/Users/hansa/OneDrive/Documents/.virtualenvs/r-keras"
)

use_virtualenv(venv_path, required = TRUE)

# --- Soft checks: verify TF/Keras are importable (no false alarms) ----
has_tf    <- reticulate::py_module_available("tensorflow")
has_keras <- reticulate::py_module_available("keras")

if (!(has_tf && has_keras)) {
  message(">> Python deps missing in venv '", venv_path, "'. Run in R *or* a shell:\n",
          "   ", file.path(venv_path, "Scripts", "python.exe"),
          " -m pip install tensorflow==2.15.0 keras==2.15.0")
} else {
  # Optional: confirm versions once
  try(reticulate::py_run_string("
import tensorflow as tf, keras
print('TF:', getattr(tf,'__version__','<none>'))
print('Keras:', getattr(keras,'__version__','<none>'))
"), silent = TRUE)
}

# Optional: warn if renv not in sync
st <- capture.output(renv::status())
if (!any(grepl("No issues found", st, fixed = TRUE))) {
  message(">> renv not in sync. Run: renv::restore(prompt = FALSE)")
}

## --- R packages (this is what snapshot() will record) ---

suppressPackageStartupMessages({
  # Core tidyverse
  library(tidyverse)
  library(lubridate)
  library(forcats)
  library(glue)
  
  # Modelling frameworks
  library(h2o)          # v3.46.0.7 (pinned in renv.lock)
  library(tidymodels)
  
  # Evaluation
  library(yardstick)
  library(ROCR)
  
  # Time series + feature extraction
  library(tsibble)
  library(feasts)
  library(tsfeatures)
  library(fabletools)
  library(forecast)
  library(stringr) # regex for frequency columns
  
  # Deep learning
  library(keras)
  library(tensorflow)
  library(pROC)
  
  # Utilities
  library(here)
  library(jsonlite)
  library(purrr)
  library(patchwork)
  library(janitor)
  library(zip)
  library(knitr)  
  library(kableExtra)
  library(htmltools)
  library(gt)
  
  

  
  # Optional but useful for EDA / plots
  library(GGally)
  library(ggstats)
  library(DiagrammeR)
  library(plotly)
  
})

if (requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
  requireNamespace("rsvg", quietly = TRUE)
}

# --- H2O guard: avoid noisy error when the package is loaded but not initialized
if ("package:h2o" %in% search()) {
  ok_h2o <- try(h2o.getConnection(), silent = TRUE)
  if (inherits(ok_h2o, "try-error")) {
    message("H2O is loaded but not running. Call h2o.init() when you need it.")
  }
}
