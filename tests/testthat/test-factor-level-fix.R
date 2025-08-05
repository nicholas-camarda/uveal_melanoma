# tests/testthat/test-factor-level-fix.R
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("factor level processing works correctly without 'Other' warnings", {
  # Setup test data (use project root paths)
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Test that subgroup analysis functions handle factor levels correctly
  expect_no_error({
    # Test binary outcome subgroup analysis
    binary_result <- analyze_treatment_effect_subgroups_binary(
      data = test_data,
      outcome_var = "recurrence1",
      subgroup_vars = c("age_at_diagnosis", "sex", "location"),
      confounders = c("age_at_diagnosis", "sex", "location"),
      outcome_name = "Local Recurrence",
      dataset_name = "uveal_melanoma_full_cohort"
    )
  })
  
  # Verify that the result structure is correct
  expect_true(is.list(binary_result))
  expect_true("subgroup_results" %in% names(binary_result))
  expect_true("other_map" %in% names(binary_result))
  
  # Test survival outcome subgroup analysis
  expect_no_error({
    survival_result <- analyze_treatment_effect_subgroups_survival(
      data = test_data,
      time_var = "tt_recurrence1_months",
      event_var = "recurrence1",
      subgroup_vars = c("age_at_diagnosis", "sex", "location"),
      confounders = c("age_at_diagnosis", "sex", "location"),
      outcome_name = "Local Recurrence",
      dataset_name = "uveal_melanoma_full_cohort"
    )
  })
  
  # Verify that the result structure is correct
  expect_true(is.list(survival_result))
  expect_true("subgroup_results" %in% names(survival_result))
  expect_true("other_map" %in% names(survival_result))
  
  # Test tumor height subgroup analysis
  expect_no_error({
    height_result <- analyze_treatment_effect_subgroups_height(
      data = test_data,
      subgroup_var = "age_at_diagnosis",
      confounders = c("sex", "location"),
      include_baseline_height = FALSE,
      dataset_name = "uveal_melanoma_full_cohort"
    )
  })
  
  # Verify that the result structure is correct
  expect_true(is.list(height_result))
  expect_true("subgroup_effects" %in% names(height_result))
  expect_true("interaction_p" %in% names(height_result))
})

test_that("factor level validation prevents 'Other' level warnings", {
  # Setup test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Test that factor levels are properly handled without generating 'Other' warnings
  expect_no_warning({
    # Test with categorical variables that might have 'Other' levels
    result <- analyze_treatment_effect_subgroups_binary(
      data = test_data,
      outcome_var = "recurrence1",
      subgroup_vars = c("location", "initial_t_stage", "biopsy1_gep"),
      confounders = c("age_at_diagnosis", "sex"),
      outcome_name = "Local Recurrence",
      dataset_name = "uveal_melanoma_full_cohort"
    )
  })
  
  # Verify that the analysis completed successfully
  expect_true(is.list(result))
  expect_true("subgroup_results" %in% names(result))
})

test_that("factor level processing handles edge cases correctly", {
  # Setup test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Test with variables that might have insufficient levels
  expect_no_error({
    # Test with a variable that might have very few levels
    result <- analyze_treatment_effect_subgroups_binary(
      data = test_data,
      outcome_var = "recurrence1",
      subgroup_vars = c("sex"),  # Only 2 levels
      confounders = c("age_at_diagnosis"),
      outcome_name = "Local Recurrence",
      dataset_name = "uveal_melanoma_full_cohort"
    )
  })
  
  # Verify that the analysis completed successfully
  expect_true(is.list(result))
  expect_true("subgroup_results" %in% names(result))
})

test_that("factor level processing works with clinical binning", {
  # Setup test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Test that clinical binning variables are handled correctly
  expect_no_error({
    # Test with variables that use clinical binning
    result <- analyze_treatment_effect_subgroups_binary(
      data = test_data,
      outcome_var = "recurrence1",
      subgroup_vars = c("initial_tumor_height", "initial_tumor_diameter"),
      confounders = c("age_at_diagnosis", "sex"),
      outcome_name = "Local Recurrence",
      dataset_name = "uveal_melanoma_full_cohort"
    )
  })
  
  # Verify that the analysis completed successfully
  expect_true(is.list(result))
  expect_true("subgroup_results" %in% names(result))
}) 