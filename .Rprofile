Sys.setenv(RENV_PATHS_LIBRARY_ROOT = "C:/R/renv-lib")

source("renv/activate.R")
# --- Project-local renv paths (avoid OneDrive locks on Windows) ---
if (interactive() && .Platform$OS.type == "windows") {
  Sys.setenv(RENV_PATHS_LIBRARY_ROOT = "C:/R/renv-lib")
  Sys.setenv(RENV_PATHS_CACHE        = "C:/R/renv-cache")
}

# Activate renv for this project
source("renv/activate.R", local = TRUE)

