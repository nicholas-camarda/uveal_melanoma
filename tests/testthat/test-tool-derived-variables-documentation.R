# tests/testthat/test-tool-derived-variables-documentation.R
# Test file for standalone derived variables documentation tool script

# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source core functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

# Source the specific tool script being tested
source("scripts/tools/derived_variables_documentation.R")

test_that("derived variables documentation tool loads correctly", {
  # Test that tool script objects and functions are available
  expect_true(exists("DERIVED_VARIABLE_DOCUMENTATION"))
  expect_true(exists("get_derived_variable_docs"))
  expect_true(exists("generate_derived_variables_documentation"))
  expect_true(is.list(DERIVED_VARIABLE_DOCUMENTATION))
  expect_true(is.function(get_derived_variable_docs))
  expect_true(is.function(generate_derived_variables_documentation))
})

test_that("tool script can access core functions and constants", {
  # Test that tool can access core functions and constants
  expect_true(exists("use"))
  expect_true(exists("STANDARD_TABLE_LABELS"))
  expect_true(is.function(use))
  expect_true(is.list(STANDARD_TABLE_LABELS))
})

test_that("derived variables documentation contains expected variables", {
  # Test that key derived variables are documented
  expected_vars <- c("age_at_diagnosis", "follow_up_years", "tt_recurrence_months", 
                     "tt_mets_months", "tt_death_months", "gep_class_simple")
  
  for (var in expected_vars) {
    expect_true(var %in% names(DERIVED_VARIABLE_DOCUMENTATION))
  }
})

test_that("get_derived_variable_docs function works correctly", {
  # Test the helper function for getting documentation
  result <- get_derived_variable_docs("age_at_diagnosis")
  expect_true(is.list(result))
  expect_true("description" %in% names(result))
  expect_true("calculation" %in% names(result))
  expect_true("purpose" %in% names(result))
})

test_that("generate_derived_variables_documentation function works", {
  # Test the main documentation generation function
  dir.create("test_output", recursive = TRUE, showWarnings = FALSE)
  
  result_file <- generate_derived_variables_documentation(include_timestamp = FALSE)
  expect_true(file.exists(result_file))
  
  # Clean up
  unlink(result_file)
})

test_that("tool script works independently when sourced explicitly", {
  # Test that the tool script doesn't interfere with core functionality
  # and can be used independently
  expect_true(exists("categorize_derived_variable"))
  expect_true(is.function(categorize_derived_variable))
  
  # Test categorization function
  result <- categorize_derived_variable("age_at_diagnosis")
  expect_true(is.character(result))
  expect_gt(nchar(result), 0)
}) 