##########################################################
# Title: Package Installer and Loader
# Description: This script ensures that all required R packages are installed
# and loaded into the environment. If a package is not installed, it will be
# automatically downloaded and installed.
#
# Date: 12/05/2025
##########################################################


# Helper function to install and load required packages
install_and_load <- function(packages) {
  install_if_missing <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
  sapply(packages, install_if_missing)
}

# Define required packages

required_packages <- c("tidyverse", "data.table", "ggplot2",
                       "dplyr","ggcorrplot", "readr", "lubridate",
                       "stringr", "rvest", "rio", "skimr",
                       "visdat","stargazer","purrr", "caret",
                       "boot", "glmnet", "MLmetrics", "Metrics",
                       "ranger", "summarytools", "GGally",
                       "gridExtra", "plotly", "leaflet",
                       "tmaptools", "sf", "osmdata", "tidymodels",
                       "stringr", "stringi", "FNN", "spatialsample",
                       "htmlwidgets", "textrecipes","rsample","nnet",
                       "stacks", "SuperLearner")


# Install and load all required packages
install_and_load(required_packages)
rm(required_packages, install_and_load)

cat("All required packages are installed and loaded.\n")
