# tests/testthat/test-error-handling.R
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("subgroup analysis handles invalid inputs gracefully", {
  # Setup test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Setup output directories
  cohort_outputs <- setup_cohort_outputs("uveal_melanoma_full_cohort")
  output_dirs <- cohort_outputs$output_dirs
  prefix <- "test_"
  
  # Test with invalid outcome variable
  expect_error({
    analyze_treatment_effect_subgroups_binary(
      data = test_data,
      outcome_var = "nonexistent_variable",
      subgroup_vars = c("age_at_diagnosis", "sex"),
      confounders = c("age_at_diagnosis", "sex"),
      outcome_name = "Test Outcome",
      dataset_name = "uveal_melanoma_full_cohort"
    )
  }, regexp = "outcome_var.*not found")
  
  # Test with invalid subgroup variable
  expect_error({
    analyze_treatment_effect_subgroups_binary(
      data = test_data,
      outcome_var = "recurrence1",
      subgroup_vars = c("nonexistent_variable"),
      confounders = c("age_at_diagnosis", "sex"),
      outcome_name = "Test Outcome",
      dataset_name = "uveal_melanoma_full_cohort"
    )
  }, regexp = "subgroup_var.*not found")
  
  # Test with invalid confounders
  expect_error({
    analyze_treatment_effect_subgroups_binary(
      data = test_data,
      outcome_var = "recurrence1",
      subgroup_vars = c("age_at_diagnosis", "sex"),
      confounders = c("nonexistent_variable"),
      outcome_name = "Test Outcome",
      dataset_name = "uveal_melanoma_full_cohort"
    )
  }, regexp = "confounder.*not found")
})

test_that("subgroup analysis handles empty data gracefully", {
  # Test with empty data frame
  empty_data <- data.frame()
  
  expect_error({
    analyze_treatment_effect_subgroups_binary(
      data = empty_data,
      outcome_var = "recurrence1",
      subgroup_vars = c("age_at_diagnosis", "sex"),
      confounders = c("age_at_diagnosis", "sex"),
      outcome_name = "Test Outcome",
      dataset_name = "test_dataset"
    )
  }, regexp = "data.*empty")
})

test_that("subgroup analysis handles missing treatment group variable", {
  # Setup test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Remove treatment_group variable
  test_data_no_treatment <- test_data[, !names(test_data) %in% "treatment_group"]
  
  expect_error({
    analyze_treatment_effect_subgroups_binary(
      data = test_data_no_treatment,
      outcome_var = "recurrence1",
      subgroup_vars = c("age_at_diagnosis", "sex"),
      confounders = c("age_at_diagnosis", "sex"),
      outcome_name = "Test Outcome",
      dataset_name = "uveal_melanoma_full_cohort"
    )
  }, regexp = "treatment_group.*not found")
})

test_that("subgroup analysis handles insufficient sample sizes gracefully", {
  # Setup test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Create a very small subset that might cause issues
  small_data <- test_data[1:5, ]
  
  # This should handle the small sample size gracefully
  expect_no_error({
    result <- analyze_treatment_effect_subgroups_binary(
      data = small_data,
      outcome_var = "recurrence1",
      subgroup_vars = c("sex"),  # Only 2 levels
      confounders = c("age_at_diagnosis"),
      outcome_name = "Test Outcome",
      dataset_name = "uveal_melanoma_full_cohort"
    )
  })
  
  # Verify that the result structure is maintained even with small sample
  expect_true(is.list(result))
  expect_true("subgroup_results" %in% names(result))
})

test_that("subgroup analysis handles all missing values gracefully", {
  # Setup test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Create data with all missing values for a variable
  test_data_all_missing <- test_data
  test_data_all_missing$age_at_diagnosis <- NA
  
  expect_error({
    analyze_treatment_effect_subgroups_binary(
      data = test_data_all_missing,
      outcome_var = "recurrence1",
      subgroup_vars = c("age_at_diagnosis"),
      confounders = c("sex"),
      outcome_name = "Test Outcome",
      dataset_name = "uveal_melanoma_full_cohort"
    )
  }, regexp = "insufficient.*data")
})

test_that("subgroup analysis handles invalid output directories gracefully", {
  # Setup test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Test with NULL output_dirs
  expect_error({
    run_objective_1(
      data = test_data,
      dataset_name = "uveal_melanoma_full_cohort",
      output_dirs = NULL,
      prefix = "test_",
      other_map = list()
    )
  }, regexp = "output_dirs.*required")
  
  # Test with invalid output_dirs structure
  invalid_output_dirs <- list(invalid_key = "/tmp/invalid")
  
  expect_error({
    run_objective_1(
      data = test_data,
      dataset_name = "uveal_melanoma_full_cohort",
      output_dirs = invalid_output_dirs,
      prefix = "test_",
      other_map = list()
    )
  }, regexp = "output_dirs.*invalid")
})

test_that("subgroup analysis handles edge cases with factor levels", {
  # Setup test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Test with a variable that has only one level
  test_data_single_level <- test_data
  test_data_single_level$single_level_var <- factor("A", levels = "A")
  
  expect_error({
    analyze_treatment_effect_subgroups_binary(
      data = test_data_single_level,
      outcome_var = "recurrence1",
      subgroup_vars = c("single_level_var"),
      confounders = c("sex"),
      outcome_name = "Test Outcome",
      dataset_name = "uveal_melanoma_full_cohort"
    )
  }, regexp = "insufficient.*levels")
})

test_that("subgroup analysis handles time-to-event variables correctly", {
  # Setup test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Test with invalid time variable
  expect_error({
    analyze_treatment_effect_subgroups_survival(
      data = test_data,
      time_var = "nonexistent_time",
      event_var = "recurrence1",
      subgroup_vars = c("age_at_diagnosis", "sex"),
      confounders = c("age_at_diagnosis", "sex"),
      outcome_name = "Test Outcome",
      dataset_name = "uveal_melanoma_full_cohort"
    )
  }, regexp = "time_var.*not found")
  
  # Test with invalid event variable
  expect_error({
    analyze_treatment_effect_subgroups_survival(
      data = test_data,
      time_var = "tt_recurrence1_months",
      event_var = "nonexistent_event",
      subgroup_vars = c("age_at_diagnosis", "sex"),
      confounders = c("age_at_diagnosis", "sex"),
      outcome_name = "Test Outcome",
      dataset_name = "uveal_melanoma_full_cohort"
    )
  }, regexp = "event_var.*not found")
}) 