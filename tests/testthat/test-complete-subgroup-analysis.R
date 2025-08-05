# tests/testthat/test-complete-subgroup-analysis.R
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("complete subgroup analysis workflow executes successfully", {
  # Setup test data (use project root paths)
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Setup output directories
  cohort_outputs <- setup_cohort_outputs("uveal_melanoma_full_cohort")
  output_dirs <- cohort_outputs$output_dirs
  prefix <- "test_"
  
  # Test that the complete subgroup analysis workflow runs without errors
  expect_no_error({
    # Execute the complete subgroup analysis workflow
    result <- run_objective_1(
      data = test_data,
      dataset_name = "uveal_melanoma_full_cohort",
      output_dirs = output_dirs,
      prefix = prefix,
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

test_that("subgroup analysis output files are created correctly", {
  # Setup test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Setup output directories
  cohort_outputs <- setup_cohort_outputs("uveal_melanoma_full_cohort")
  output_dirs <- cohort_outputs$output_dirs
  prefix <- "test_"
  
  # Execute the subgroup analysis
  result <- run_objective_1(
    data = test_data,
    dataset_name = "uveal_melanoma_full_cohort",
    output_dirs = output_dirs,
    prefix = prefix,
    other_map = list()
  )
  
  # Verify that forest plot files are created
  forest_plot_files <- list.files(output_dirs$obj1_forest_plots, pattern = paste0(prefix, ".*_subgroup_forest_plot.png"))
  expect_true(length(forest_plot_files) >= 4)  # Should have at least 4 forest plots (recurrence, mets, os, pfs)
  
  # Verify that diagnostic files are created
  primary_diagnostic_file <- file.path(output_dirs$obj1_subgroup_primary, paste0(prefix, "primary_tumor_height_diagnostics.xlsx"))
  expect_true(file.exists(primary_diagnostic_file))
  
  sensitivity_diagnostic_file <- file.path(output_dirs$obj1_subgroup_sensitivity, paste0(prefix, "sensitivity_tumor_height_diagnostics.xlsx"))
  expect_true(file.exists(sensitivity_diagnostic_file))
  
  # Verify that RDS files are created
  primary_rds_file <- file.path(output_dirs$obj1_subgroup_primary, paste0(prefix, "primary_subgroup_interactions.rds"))
  expect_true(file.exists(primary_rds_file))
  
  sensitivity_rds_file <- file.path(output_dirs$obj1_subgroup_sensitivity, paste0(prefix, "sensitivity_subgroup_interactions.rds"))
  expect_true(file.exists(sensitivity_rds_file))
  
  # Verify that forest plot diagnostics are created
  forest_diagnostic_file <- file.path(output_dirs$obj1_forest_plots, paste0(prefix, "forest_plot_diagnostics.xlsx"))
  expect_true(file.exists(forest_diagnostic_file))
})

test_that("subgroup analysis handles all outcome types correctly", {
  # Setup test data
  test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Setup output directories
  cohort_outputs <- setup_cohort_outputs("uveal_melanoma_full_cohort")
  output_dirs <- cohort_outputs$output_dirs
  prefix <- "test_"
  
  # Execute the subgroup analysis
  result <- run_objective_1(
    data = test_data,
    dataset_name = "uveal_melanoma_full_cohort",
    output_dirs = output_dirs,
    prefix = prefix,
    other_map = list()
  )
  
  # Verify that all outcome types are analyzed
  expect_true("recurrence_rates" %in% names(result))
  expect_true("mets_rates" %in% names(result))
  expect_true("os_analysis" %in% names(result))
  expect_true("pfs_analysis" %in% names(result))
  expect_true("height_changes" %in% names(result))
  
  # Verify that subgroup results are generated
  expect_true("primary_subgroup_results" %in% names(result))
  expect_true("sensitivity_subgroup_results" %in% names(result))
  
  # Verify that the results contain the expected structure
  expect_true(is.list(result$primary_subgroup_results))
  expect_true(is.list(result$sensitivity_subgroup_results))
})

test_that("subgroup analysis works with different cohort configurations", {
  # Test with a different cohort if available
  available_datasets <- list_available_datasets()
  
  if (length(available_datasets) > 1) {
    # Use the second available dataset
    test_dataset <- available_datasets[2]
    test_data <- readRDS(file.path("final_data/Analytic Dataset", paste0(test_dataset, ".rds")))
    
    # Setup output directories
    cohort_outputs <- setup_cohort_outputs(test_dataset)
    output_dirs <- cohort_outputs$output_dirs
    prefix <- "test_"
    
    # Test that the subgroup analysis works with different cohorts
    expect_no_error({
      result <- run_objective_1(
        data = test_data,
        dataset_name = test_dataset,
        output_dirs = output_dirs,
        prefix = prefix,
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
  }
}) 