library(testthat)

# Set working directory to project root for consistent path handling
setwd(dirname(normalizePath(".")))

# Source existing helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_check("ocular-melanoma-analysis")
