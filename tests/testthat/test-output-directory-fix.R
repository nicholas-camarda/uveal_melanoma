# tests/testthat/test-output-directory-fix.R
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("output directory configuration works correctly", {
  # Setup test data (use project root paths)
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Test that setup_cohort_outputs function works without errors
  expect_no_error({
    cohort_outputs <- setup_cohort_outputs("uveal_melanoma_full_cohort")
    output_dirs <- cohort_outputs$output_dirs
  })
  
  # Verify that obj1_subgroup_clinical is not present (since it was removed)
  expect_false("obj1_subgroup_clinical" %in% names(output_dirs))
  
  # Verify that obj1_forest_plots is present and properly configured
  expect_true("obj1_forest_plots" %in% names(output_dirs))
  expect_true(dir.exists(output_dirs$obj1_forest_plots))
  
  # Verify that other essential directories are present
  essential_dirs <- c("obj1_recurrence", "obj1_mets", "obj1_os", "obj1_pfs", 
                     "obj1_height_primary", "obj1_height_sensitivity",
                     "obj1_subgroup_primary", "obj1_subgroup_sensitivity")
  
  for (dir_name in essential_dirs) {
    expect_true(dir_name %in% names(output_dirs), 
                info = paste("Missing directory:", dir_name))
    expect_true(dir.exists(output_dirs[[dir_name]]), 
                info = paste("Directory does not exist:", dir_name))
  }
})

test_that("primary outcomes subgroup analysis handles missing directory gracefully", {
  # Setup test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Test that the function doesn't crash when obj1_subgroup_clinical is missing
  expect_no_error({
    cohort_outputs <- setup_cohort_outputs("uveal_melanoma_full_cohort")
    output_dirs <- cohort_outputs$output_dirs
    # This should not cause an error even though obj1_subgroup_clinical is missing
    expect_false("obj1_subgroup_clinical" %in% names(output_dirs))
  })
})

test_that("directory structure is correct after clinical_outcomes removal", {
  # Test that the directory structure is correct after removing clinical_outcomes subfolder
  cohort_outputs <- setup_cohort_outputs("uveal_melanoma_full_cohort")
  output_dirs <- cohort_outputs$output_dirs
  
  # Verify that obj1_subgroup_clinical is not present (since it was removed)
  expect_false("obj1_subgroup_clinical" %in% names(output_dirs))
  
  # Verify that essential directories are still present
  essential_dirs <- c("obj1_recurrence", "obj1_mets", "obj1_os", "obj1_pfs", 
                     "obj1_height_primary", "obj1_height_sensitivity",
                     "obj1_subgroup_primary", "obj1_subgroup_sensitivity", "obj1_forest_plots")
  
  for (dir_name in essential_dirs) {
    expect_true(dir_name %in% names(output_dirs), 
                info = paste("Missing directory:", dir_name))
  }
})

test_that("run_objective_1 completes successfully without clinical_outcomes directory", {
  # Setup test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Test that the entire function runs without errors
  expect_no_error({
    result <- run_objective_1(
      data = test_data,
      dataset_name = "uveal_melanoma_full_cohort",
      output_dirs = setup_cohort_outputs("uveal_melanoma_full_cohort")$output_dirs,
      prefix = "test_",
      other_map = list()
    )
  })
  
  # Verify that the function returns the expected structure
  expect_true(is.list(result))
  expect_true("recurrence_rates" %in% names(result))
  expect_true("mets_rates" %in% names(result))
  expect_true("os_analysis" %in% names(result))
  expect_true("pfs_analysis" %in% names(result))
  expect_true("height_changes" %in% names(result))
  expect_true("primary_subgroup_results" %in% names(result))
  expect_true("sensitivity_subgroup_results" %in% names(result))
}) 