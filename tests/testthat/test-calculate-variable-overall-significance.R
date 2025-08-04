# tests/testthat/test-calculate-variable-overall-significance.R
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("calculate_variable_overall_significance works for survival case - treatment_group", {
  # Load real data
  data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  pval <- calculate_variable_overall_significance(
    data, 'treatment_group', 'death_event',
    treatment_var = 'treatment_group',
    confounders = c('age_at_diagnosis', 'sex'),
    outcome_type = 'survival',
    time_var = 'tt_death_years',
    event_var = 'death_event'
  )
  
  expect_true(!is.na(pval))
  expect_true(pval >= 0 && pval <= 1)
  expect_type(pval, "double")
})

test_that("calculate_variable_overall_significance works for survival case - age_at_diagnosis", {
  # Load real data
  data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  pval <- calculate_variable_overall_significance(
    data, 'age_at_diagnosis', 'death_event',
    treatment_var = 'treatment_group',
    confounders = c('sex'),
    outcome_type = 'survival',
    time_var = 'tt_death_years',
    event_var = 'death_event'
  )
  
  expect_true(!is.na(pval))
  expect_true(pval >= 0 && pval <= 1)
  expect_type(pval, "double")
})

test_that("calculate_variable_overall_significance works for binary case", {
  # Load real data
  data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  pval <- calculate_variable_overall_significance(
    data, 'treatment_group', 'death_event',
    treatment_var = 'treatment_group',
    confounders = c('age_at_diagnosis', 'sex'),
    outcome_type = 'binary'
  )
  
  expect_true(!is.na(pval))
  expect_true(pval >= 0 && pval <= 1)
  expect_type(pval, "double")
})

test_that("calculate_variable_overall_significance debug output shows survival case", {
  # Load real data
  data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Capture debug output
  debug_output <- capture.output({
    pval <- calculate_variable_overall_significance(
      data, 'treatment_group', 'death_event',
      treatment_var = 'treatment_group',
      confounders = c('age_at_diagnosis', 'sex'),
      outcome_type = 'survival',
      time_var = 'tt_death_years',
      event_var = 'death_event'
    )
  })
  
  # Check that survival case debug messages appear
  expect_true(any(grepl("Survival case in calculate_variable_overall_significance", debug_output)))
  expect_true(any(grepl("Variable: treatment_group", debug_output)))
  expect_true(any(grepl("Outcome type: survival", debug_output)))
}) 