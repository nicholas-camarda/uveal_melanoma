# tests/testthat/test-derived-variables-documentation.R
# Comprehensive tests for derived variables documentation system

# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("derived variables documentation system loads correctly", {
  # Test that the documentation system is available
  expect_true(exists("DERIVED_VARIABLE_DOCUMENTATION"))
  expect_true(exists("get_derived_variable_docs"))
  expect_true(exists("generate_derived_variables_documentation"))
  
  # Test that documentation contains expected variables
  expect_gt(length(DERIVED_VARIABLE_DOCUMENTATION), 0)
  expect_true("age_at_diagnosis" %in% names(DERIVED_VARIABLE_DOCUMENTATION))
  expect_true("tt_recurrence_months" %in% names(DERIVED_VARIABLE_DOCUMENTATION))
  expect_true("gep_class_simple" %in% names(DERIVED_VARIABLE_DOCUMENTATION))
})

test_that("get_derived_variable_docs function works correctly", {
  # Test getting all documentation
  all_docs <- get_derived_variable_docs()
  expect_type(all_docs, "list")
  expect_gt(length(all_docs), 0)
  
  # Test getting specific variable documentation
  age_docs <- get_derived_variable_docs("age_at_diagnosis")
  expect_type(age_docs, "list")
  expect_equal(age_docs$description, "Patient age at time of diagnosis")
  expect_equal(age_docs$data_type, "numeric")
  expect_equal(age_docs$units, "years")
  
  # Test error handling for non-existent variable
  expect_error(get_derived_variable_docs("non_existent_variable"))
})

test_that("categorize_derived_variable function works correctly", {
  # Test categorization of different variable types
  expect_equal(categorize_derived_variable("age_at_diagnosis"), "Demographic")
  expect_equal(categorize_derived_variable("follow_up_years"), "Follow-up Time")
  expect_equal(categorize_derived_variable("tt_recurrence_months"), "Time-to-Event")
  expect_equal(categorize_derived_variable("recurrence_event"), "Event Indicators")
  expect_equal(categorize_derived_variable("gep_class_simple"), "GEP Variables")
  expect_equal(categorize_derived_variable("treatment_date"), "Treatment")
  expect_equal(categorize_derived_variable("mets_free_at_baseline"), "Metastasis")
  expect_equal(categorize_derived_variable("height_change"), "Tumor Characteristics")
  expect_equal(categorize_derived_variable("unknown_variable"), "Other")
})

test_that("print_derived_variables_summary function works without errors", {
  # Test that the summary function runs without errors
  expect_no_error(print_derived_variables_summary())
})

test_that("export_derived_variables_to_excel function works correctly", {
  # Create a temporary test file
  test_output_file <- "test_output/derived_vars_test.xlsx"
  dir.create("test_output", recursive = TRUE, showWarnings = FALSE)
  
  # Test Excel export
  result_file <- export_derived_variables_to_excel(test_output_file, include_timestamp = FALSE)
  
  # Verify file was created
  expect_true(file.exists(result_file))
  expect_gt(file.size(result_file), 0)
  
  # Clean up
  unlink(test_output_file)
})

test_that("generate_derived_variables_documentation function works correctly", {
  # Test the main function that generates documentation in the correct location
  result_file <- generate_derived_variables_documentation(include_timestamp = FALSE)
  
  # Verify file was created in the correct location
  expect_true(file.exists(result_file))
  expect_true(grepl("final_data/Analytic Dataset", result_file))
  expect_gt(file.size(result_file), 0)
  
  # Verify the file is a valid Excel file
  expect_true(grepl("\\.xlsx$", result_file))
})

test_that("all documented variables have required fields", {
  # Test that all documented variables have the required fields
  for (var_name in names(DERIVED_VARIABLE_DOCUMENTATION)) {
    var_info <- DERIVED_VARIABLE_DOCUMENTATION[[var_name]]
    
    # Check required fields exist
    expect_true("description" %in% names(var_info))
    expect_true("calculation" %in% names(var_info))
    expect_true("purpose" %in% names(var_info))
    expect_true("data_type" %in% names(var_info))
    expect_true("units" %in% names(var_info))
    
    # Check fields are not empty
    expect_gt(nchar(var_info$description), 0)
    expect_gt(nchar(var_info$calculation), 0)
    expect_gt(nchar(var_info$purpose), 0)
    expect_gt(nchar(var_info$data_type), 0)
    expect_gt(nchar(var_info$units), 0)
  }
})

test_that("documentation covers all major variable categories", {
  # Test that we have documentation for variables from all major categories
  all_vars <- names(DERIVED_VARIABLE_DOCUMENTATION)
  
  # Check for demographic variables
  expect_true(any(grepl("^age_", all_vars)))
  
  # Check for time-to-event variables
  expect_true(any(grepl("^tt_", all_vars)))
  
  # Check for event indicators
  expect_true(any(grepl("_event$", all_vars)))
  
  # Check for GEP variables
  expect_true(any(grepl("^gep_", all_vars) | grepl("expected_", all_vars)))
  
  # Check for follow-up time variables
  expect_true(any(grepl("^follow_up_", all_vars)))
})

test_that("validation function works with test data", {
  # Create mock processed data for testing validation
  mock_data <- list(
    test_cohort = data.frame(
      age_at_diagnosis = c(50, 60, 70),
      tt_recurrence_months = c(12, 24, 36),
      recurrence_event = c(1, 0, 1),
      gep_class_simple = c("Class 1A", "Class 1B", "Class 2"),
      stringsAsFactors = FALSE
    )
  )
  
  # Test validation function
  validation_results <- validate_derived_variables_documentation(mock_data)
  
  # Check validation results structure
  expect_type(validation_results, "list")
  expect_true("documented_variables" %in% names(validation_results))
  expect_true("actual_variables" %in% names(validation_results))
  expect_true("documentation_complete" %in% names(validation_results))
  expect_true("documentation_accurate" %in% names(validation_results))
  
  # Check that validation ran without errors
  expect_true(is.numeric(validation_results$documented_variables))
  expect_true(is.numeric(validation_results$actual_variables))
  expect_true(is.logical(validation_results$documentation_complete))
  expect_true(is.logical(validation_results$documentation_accurate))
}) 