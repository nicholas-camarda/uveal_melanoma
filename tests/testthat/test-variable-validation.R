# tests/testthat/test-variable-validation.R
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("Variable validation framework works correctly", {
    # Clean up test output directory before running tests
    test_output_dir <- "test_output/variable_validation_test"
    if (dir.exists(test_output_dir)) {
        unlink(test_output_dir, recursive = TRUE)
    }
    dir.create(test_output_dir)

    # Setup test data
    test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

    # Test 1: Verify that analysis functions validate variable existence
    test_that("analysis functions validate variable existence", {
        # Test with valid variables
        expect_no_error({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = "sex",
                confounders = c("location"),
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        })
        
        # Test with invalid outcome variable - should handle gracefully
        expect_no_error({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "nonexistent_variable",
                predictor_vars = "sex",
                confounders = c("location"),
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        })
        
        # Verify that the function handles invalid variables gracefully
        expect_true(is.list(result), info = "Should return a list even with invalid variables")
    })

    # Test 2: Verify that analysis functions validate predictor variables
    test_that("analysis functions validate predictor variables", {
        # Test with valid predictor variables
        expect_no_error({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = c("sex", "location"),
                confounders = NULL,
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        })
        
        # Test with invalid predictor variable - should handle gracefully
        expect_no_error({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = c("sex", "nonexistent_predictor"),
                confounders = NULL,
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        })
        
        # Verify that the function handles invalid predictor variables gracefully
        expect_true(is.list(result), info = "Should return a list even with invalid predictor variables")
    })

    # Test 3: Verify that analysis functions validate confounders
    test_that("analysis functions validate confounders", {
        # Test with valid confounders
        expect_no_error({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = "sex",
                confounders = c("location", "initial_t_stage"),
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        })
        
        # Test with invalid confounder - should handle gracefully
        expect_no_error({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = "sex",
                confounders = c("location", "nonexistent_confounder"),
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        })
        
        # Verify that the function handles invalid confounders gracefully
        expect_true(is.list(result), info = "Should return a list even with invalid confounders")
    })

    # Test 4: Verify that analysis functions validate treatment variables
    test_that("analysis functions validate treatment variables", {
        # Test with valid treatment variable
        expect_no_error({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = "treatment_group",
                confounders = c("sex", "location"),
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        })
        
        # Test with invalid treatment variable - should handle gracefully
        expect_no_error({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = "treatment_group",
                confounders = c("sex", "location"),
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "nonexistent_treatment"
            )
        })
        
        # Verify that the function handles invalid treatment variables gracefully
        expect_true(is.list(result), info = "Should return a list even with invalid treatment variables")
    })

    # Test 5: Verify that analysis functions validate model types
    test_that("analysis functions validate model types", {
        # Test with valid model type
        expect_no_error({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = "sex",
                confounders = c("location"),
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        })
        
        # Test with invalid model type
        expect_error({
            generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = "sex",
                confounders = c("location"),
                model_type = "invalid_model_type",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        }, info = "Should error with invalid model type")
    })

    # Test 6: Verify that analysis functions validate effect measures
    test_that("analysis functions validate effect measures", {
        # Test with valid effect measure for linear model
        expect_no_error({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = "sex",
                confounders = c("location"),
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        })
        
        # Test with invalid effect measure
        expect_error({
            generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = "sex",
                confounders = c("location"),
                model_type = "linear",
                effect_measure = "invalid_effect_measure",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        }, info = "Should error with invalid effect measure")
    })

    # Test 7: Verify that analysis functions validate data types
    test_that("analysis functions validate data types", {
        # Test with appropriate data types
        expect_no_error({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",  # Numeric
                predictor_vars = "sex",  # Factor
                confounders = c("location"),  # Factor
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        })
    })

    # Test 8: Verify that analysis functions handle missing data appropriately
    test_that("analysis functions handle missing data appropriately", {
        # Test with data containing missing values
        test_data_with_missing <- test_data
        test_data_with_missing$age_at_diagnosis[1:5] <- NA
        
        expect_no_error({
            result <- generate_regression_table(
                data = test_data_with_missing,
                outcome_var = "age_at_diagnosis",
                predictor_vars = "sex",
                confounders = c("location"),
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        })
    })

    # Test 9: Verify that analysis functions validate output directories
    test_that("analysis functions validate output directories", {
        # Test with valid output directory
        expect_no_error({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = "sex",
                confounders = c("location"),
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        })
        
        # Test with invalid output directory (should create it)
        invalid_dir <- file.path(test_output_dir, "nonexistent_subdir")
        expect_no_error({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = "sex",
                confounders = c("location"),
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = invalid_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        })
    })

    # Test 10: Verify that analysis functions validate required parameters
    test_that("analysis functions validate required parameters", {
        # Test with all required parameters
        expect_no_error({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = "sex",
                confounders = c("location"),
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        })
        
        # Test with missing required parameter (data)
        expect_error({
            generate_regression_table(
                outcome_var = "age_at_diagnosis",
                predictor_vars = "sex",
                confounders = c("location"),
                model_type = "linear",
                effect_measure = "beta",
                analysis_name = "test_validation",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                treatment_var = "treatment_group"
            )
        }, info = "Should error with missing required parameter")
    })
}) 
 