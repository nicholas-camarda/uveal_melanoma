# Test file for fixing the critical other_map.rds structure bug
# This test documents the current broken state and tests the fix

# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("other_map.rds has correct structure after fix", {
  # Test that the other_map.rds exists and has correct structure
  other_map_file <- file.path("final_data", "Analytic Dataset", "other_map.rds")
  expect_true(file.exists(other_map_file), "other_map.rds file should exist")

  # Load the fixed other_map.rds
  other_map <- readRDS(other_map_file)

  # Document the correct structure
  expect_true(is.list(other_map), "other_map should be a list")
  expect_true(length(other_map) > 0, "other_map should have content")

  # The structure should be a nested list with cohort names
  expect_equal(class(other_map), "list")
  expect_equal(length(other_map), 3)
  expect_equal(names(other_map), c("uveal_melanoma_full_cohort", "uveal_melanoma_restricted_cohort", "uveal_melanoma_gksrs_only_cohort"))

  # Each cohort should contain a list of variables
  for (cohort_name in names(other_map)) {
    expect_true(is.list(other_map[[cohort_name]]), sprintf("Cohort %s should be a list", cohort_name))
  }
})

test_that("expected other_map structure should be nested list", {
  # Define what the correct structure should be based on actual pipeline output
  expected_structure <- list(
    uveal_melanoma_full_cohort = list(
      location = c("Ciliary Body", "Conjunctival", "Irido-Ciliary", "Iris")
      # Other variables would be here with their collapsed categories
    ),
    uveal_melanoma_restricted_cohort = list(
      location = c("Ciliary Body", "Conjunctival", "Irido-Ciliary", "Iris"),
      initial_tumor_height_binned = c("12.1-15 mm", "> 15 mm")
    ),
    uveal_melanoma_gksrs_only_cohort = list(
      location = c("Ciliary Body", "Conjunctival", "Irido-Ciliary", "Iris")
    )
  )

  # Test that expected structure is correct
  expect_true(is.list(expected_structure), "Expected structure should be a list")
  expect_true(all(sapply(expected_structure, is.list)), "Expected structure should be nested list")

  # Test that expected structure has cohort names
  expected_cohorts <- c("uveal_melanoma_full_cohort", "uveal_melanoma_restricted_cohort", "uveal_melanoma_gksrs_only_cohort")
  expect_true(all(expected_cohorts %in% names(expected_structure)), "Expected structure should have cohort names")
})

test_that("get_cohort_specific_other_map works with fixed structure", {
  # Test that the loading function works correctly with fixed structure

  # Load the fixed other_map
  other_map <- readRDS(file.path("final_data", "Analytic Dataset", "other_map.rds"))

  # The function expects a nested list and now gets a nested list
  expect_true(is.list(other_map), "Structure should be a list")

  # Test that trying to access it as nested list works correctly
  cohort_data <- other_map[["uveal_melanoma_full_cohort"]]
  expect_true(is.list(cohort_data), "Cohort data should be a list")
  expect_true("location" %in% names(cohort_data), "Cohort should contain location")

  # Test that get_cohort_specific_other_map function works
  cohort_other_map <- get_cohort_specific_other_map("uveal_melanoma_full_cohort")
  expect_true(is.list(cohort_other_map), "get_cohort_specific_other_map should return a list")
  expect_true("location" %in% names(cohort_other_map), "Should contain location variable")
})

test_that("other_map contains original categories that were collapsed", {
  # Test that the content contains the original categories that were collapsed
  # This validates that the data is correct and accessible in the new structure

  other_map <- readRDS(file.path("final_data", "Analytic Dataset", "other_map.rds"))

  # These should be the original location categories that were collapsed into "Other"
  expected_location_categories <- c("Ciliary Body", "Conjunctival", "Irido-Ciliary", "Iris")
  
  # These should be the original tumor height categories that were collapsed into "Other"
  expected_height_categories <- c("12.1-15 mm", "> 15 mm")

  # Check that the categories are accessible through the nested structure
  for (cohort_name in names(other_map)) {
    cohort_data <- other_map[[cohort_name]]
    expect_true("location" %in% names(cohort_data), sprintf("Cohort %s should contain location", cohort_name))

    location_categories <- cohort_data[["location"]]
    expect_true(all(expected_location_categories %in% location_categories),
                sprintf("Cohort %s should contain all expected location categories", cohort_name))

    # Verify the exact content matches expected categories
    expect_equal(sort(location_categories), sort(expected_location_categories))
    
    # Check for initial_tumor_height_binned if it exists in this cohort
    if ("initial_tumor_height_binned" %in% names(cohort_data)) {
      height_categories <- cohort_data[["initial_tumor_height_binned"]]
      expect_true(all(expected_height_categories %in% height_categories),
                  sprintf("Cohort %s should contain all expected height categories", cohort_name))
      
      # Verify the exact content matches expected categories
      expect_equal(sort(height_categories), sort(expected_height_categories))
    }
  }
})

test_that("all factor variables with 'Other' level in processed data are present in other_map.rds for each cohort", {
  cohorts <- c("uveal_melanoma_full_cohort", "uveal_melanoma_restricted_cohort", "uveal_melanoma_gksrs_only_cohort")
  other_map <- readRDS(file.path("final_data", "Analytic Dataset", "other_map.rds"))
  for (cohort in cohorts) {
    data <- readRDS(file.path("final_data", "Analytic Dataset", paste0(cohort, ".rds")))
    factor_vars <- names(data)[sapply(data, is.factor)]
    for (var in factor_vars) {
      if ("Other" %in% levels(data[[var]])) {
        expect_true(var %in% names(other_map[[cohort]]),
                    sprintf("%s: variable with 'Other' level should be in other_map.rds for cohort %s", var, cohort))
        # Optionally check that the collapsed categories are non-empty
        if (var %in% names(other_map[[cohort]])) {
          expect_true(length(other_map[[cohort]][[var]]) > 0,
                      sprintf("%s: collapsed categories should be non-empty for cohort %s", var, cohort))
        }
      }
    }
  }
})
