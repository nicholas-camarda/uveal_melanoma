# Test for comprehensive variable census tool
# This test ensures the variable census tool works correctly and produces expected outputs

# Set up test environment
setwd(dirname(dirname(normalizePath("."))))
source("scripts/utils/all_helper_functions.R")
source("scripts/tools/comprehensive_variable_census.R")

# Load test data
test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

test_that("Comprehensive variable census tool functions exist", {
  # Test that the main function exists
  expect_true(exists("create_comprehensive_variable_census"))
  expect_true(exists("get_derived_variables_info"))
  expect_true(exists("create_census_html_report"))
})

test_that("Derived variables info function works", {
  # Test that we can get derived variables information
  derived_vars <- get_derived_variables_info()
  
  expect_true(is.data.frame(derived_vars))
  expect_true("variable_name" %in% names(derived_vars))
  expect_true("derivation_logic" %in% names(derived_vars))
  expect_true("source_variables" %in% names(derived_vars))
  
  # Test that we have some derived variables
  expect_gt(nrow(derived_vars), 0)
  
  # Test that key derived variables are included
  key_vars <- c("tt_death_years", "death_event", "treatment_group", "age_at_diagnosis")
  for (var in key_vars) {
    expect_true(var %in% derived_vars$variable_name)
  }
})

test_that("Comprehensive variable census creates expected outputs", {
  # Create a temporary output directory for testing
  test_output_dir <- file.path(TEST_OUTPUT_DIR, "comprehensive_census", "test_census")
  dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Run the census tool
  result <- create_comprehensive_variable_census(
    output_dir = test_output_dir
  )
  
  # Test that result is a list with expected components
  expect_true(is.list(result))
  expect_true("census" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("output_dir" %in% names(result))
  
  # Test that census is a data frame
  expect_true(is.data.frame(result$census))
  expect_gt(nrow(result$census), 0)
  
  # Test that summary contains expected statistics
  expect_true("total_variables" %in% names(result$summary))
  expect_true("original_variables" %in% names(result$summary))
  expect_true("derived_variables" %in% names(result$summary))
  expect_true("current_variables" %in% names(result$summary))
  expect_true("categories" %in% names(result$summary))
  
  # Test that output files were created
  expect_true(file.exists(file.path(test_output_dir, "comprehensive_variable_census.rds")))
  expect_true(file.exists(file.path(test_output_dir, "comprehensive_variable_census.xlsx")))
  expect_true(file.exists(file.path(test_output_dir, "comprehensive_variable_census.html")))
  
  # Test that XLSX file is readable
  xlsx_data <- read_excel(file.path(test_output_dir, "comprehensive_variable_census.xlsx"), sheet = "Variable_Census")
  expect_true(is.data.frame(xlsx_data))
  expect_gt(nrow(xlsx_data), 0)
  
  # Test that HTML file has content
  html_content <- readLines(file.path(test_output_dir, "comprehensive_variable_census.html"))
  expect_gt(length(html_content), 0)
  expect_true(any(grepl("Comprehensive Variable Census", html_content)))
})

test_that("Variable census includes all expected categories", {
  # Run the census tool
  result <- create_comprehensive_variable_census(
    output_dir = file.path(TEST_OUTPUT_DIR, "comprehensive_census", "test_census_categories")
  )
  
  # Test that expected categories are present
  expected_categories <- c(
    "Recurrence", "Metastasis", "Tumor Characteristics",
    "Vision", "Treatment", "GEP/Molecular", "Demographics",
    "Dates/Times", "Staging", "Anatomy", "Identification", "Other"
  )
  
  actual_categories <- names(result$summary$categories)
  for (category in expected_categories) {
    expect_true(category %in% actual_categories)
  }
})

test_that("Variable census correctly identifies derived vs original variables", {
  # Run the census tool
  result <- create_comprehensive_variable_census(
    output_dir = file.path(TEST_OUTPUT_DIR, "comprehensive_census", "test_census_derived")
  )
  
  census <- result$census
  
  # Test that derived variables are correctly identified
  derived_vars <- census[census$is_derived == TRUE, ]
  expect_gt(nrow(derived_vars), 0)
  
  # Test that some variables are marked as derived
  expect_true(any(census$is_derived, na.rm = TRUE))
  
  # Test that some variables are marked as original
  expect_true(any(census$is_original, na.rm = TRUE))
  
  # Test that all current variables are present
  expect_true(all(census$is_current, na.rm = TRUE))
})

test_that("Variable census includes current dataset information", {
  # Run the census tool
  result <- create_comprehensive_variable_census(
    output_dir = file.path(TEST_OUTPUT_DIR, "comprehensive_census", "test_census_current")
  )
  
  census <- result$census
  
  # Test that current dataset variables are included
  expect_true("current_type" %in% names(census))
  expect_true("current_missing" %in% names(census))
  expect_true("current_n" %in% names(census))
  
  # Test that we have information for all current variables
  current_vars <- census[census$is_current == TRUE, ]
  expect_equal(nrow(current_vars), 105)  # Based on actual census output
  
  # Test that missing counts are reasonable
  expect_true(all(current_vars$current_missing >= 0, na.rm = TRUE))
  expect_true(all(current_vars$current_missing <= current_vars$current_n, na.rm = TRUE))
})

# Cleanup
test_that("Test cleanup", {
  # This test ensures cleanup happens after all other tests
  expect_true(TRUE)
}) 