# Purpose: Central place to declare all R packages used in this project.
# This helps renv::snapshot() capture them reliably.

# -------------------------------------------------------------------------
# Dependency snapshot reminder:
# After editing this file (e.g., adding/removing libraries),
# run the following in the R console to update renv.lock:
#
# source("00_dependencies.R")          # load all listed packages
# renv::snapshot(type = "explicit")    # record them in renv.lock
# -------------------------------------------------------------------------



# Core tidyverse
library(tidyverse)
library(lubridate)
library(forcats)
library(glue)

# Modelling frameworks
library(h2o)
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

# Deep learning
library(keras)
library(tensorflow)

# Utilities
library(here)
library(jsonlite)
library(purrr)
library(patchwork)
library(janitor)

# Optional but useful for EDA / plots
library(GGally)
library(ggstats)