# tests/testthat/test-missing-functions.R
# Test file for the newly created coefficient name functions

# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("get_treatment_coefficient_name function exists and works", {
  # Test that function exists
  expect_true(exists("get_treatment_coefficient_name"))
  expect_true(is.function(get_treatment_coefficient_name))
  
  # Test with NULL model
  result <- get_treatment_coefficient_name(NULL, "treatment_group", data.frame())
  expect_null(result)
  
  # Test with try-error model
  error_model <- try(stop("test error"), silent = TRUE)
  result <- get_treatment_coefficient_name(error_model, "treatment_group", data.frame())
  expect_null(result)
})

test_that("get_interaction_coefficient_name function exists and works", {
  # Test that function exists
  expect_true(exists("get_interaction_coefficient_name"))
  expect_true(is.function(get_interaction_coefficient_name))
  
  # Test with NULL model
  result <- get_interaction_coefficient_name(NULL, "treatment_group", "age_at_diagnosis", "≥65", data.frame())
  expect_null(result)
  
  # Test with try-error model
  error_model <- try(stop("test error"), silent = TRUE)
  result <- get_interaction_coefficient_name(error_model, "treatment_group", "age_at_diagnosis", "≥65", data.frame())
  expect_null(result)
})

test_that("functions can handle edge cases", {
  # Test with empty coefficient names
  mock_model <- list()
  mock_model$coefficients <- numeric(0)
  names(mock_model$coefficients) <- character(0)
  
  result1 <- get_treatment_coefficient_name(mock_model, "treatment_group", data.frame())
  expect_null(result1)
  
  result2 <- get_interaction_coefficient_name(mock_model, "treatment_group", "age_at_diagnosis", "≥65", data.frame())
  expect_null(result2)
}) 