# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("minimal package structure is working", {
  # Test that helper functions are loaded
  expect_true(exists("use"))
  expect_true(is.function(use))
  
  # Test that we can read the test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  expect_s3_class(test_data, "data.frame")
  expect_gt(nrow(test_data), 0)
  
  # Test that we can create output directories
  test_output_dir <- "test_output/minimal_package_test"
  dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
  expect_true(dir.exists(test_output_dir))
}) 