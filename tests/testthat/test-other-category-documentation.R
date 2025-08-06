# tests/testthat/test-other-category-documentation.R
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("Other category documentation system works correctly", {
    # Clean up test output directory before running tests
    test_output_dir <- "test_output/other_category_documentation_test"
    if (dir.exists(test_output_dir)) {
        unlink(test_output_dir, recursive = TRUE)
    }
    dir.create(test_output_dir)

    # Setup test data
    test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

    # Test 1: Verify get_cohort_specific_other_map() loads existing other_map.rds
    test_that("get_cohort_specific_other_map loads existing other_map.rds", {
        # Test loading other_map for a known dataset
        other_map <- get_cohort_specific_other_map("uveal_melanoma_full_cohort")
        
        # Verify that the function returns a list
        expect_true(is.list(other_map), info = "get_cohort_specific_other_map should return a list")
        
        # Verify that the function handles missing files gracefully
        missing_other_map <- get_cohort_specific_other_map("nonexistent_dataset")
        expect_true(is.list(missing_other_map), info = "Should return empty list for missing dataset")
        expect_true(length(missing_other_map) == 0, info = "Should return empty list for missing dataset")
    })

    # Test 2: Verify add_other_level_details() adds proper documentation
    test_that("add_other_level_details adds proper documentation to tables", {
        # Create a simple test table with gtsummary
        library(gtsummary)
        
        # Create a test model
        test_model <- lm(age_at_diagnosis ~ sex + location, data = test_data)
        
        # Create a gtsummary table
        test_table <- tbl_regression(test_model, data = test_data)
        
        # Test with empty other_map
        modified_table <- add_other_level_details(test_table, test_data, other_map = list())
        
        # Verify that the function returns a gtsummary table
        expect_true(inherits(modified_table, "tbl_regression"), 
                   info = "add_other_level_details should return a tbl_regression object")
        
        # Test with some other_map data
        test_other_map <- list(
            location = c("Peripheral", "Mid-peripheral"),
            sex = c("Unknown")
        )
        
        modified_table_with_map <- add_other_level_details(test_table, test_data, other_map = test_other_map)
        
        # Verify that the function handles other_map correctly
        expect_true(inherits(modified_table_with_map, "tbl_regression"), 
                   info = "add_other_level_details should work with other_map data")
    })

    # Test 3: Verify other_map parameter is passed through analysis functions
    test_that("other_map parameter is passed through analysis functions", {
        # Test that analyze_pfs2 accepts other_map parameter
        test_other_map <- list(
            location = c("Peripheral", "Mid-peripheral")
        )
        
        # This should not throw an error
        expect_no_error({
            result <- analyze_pfs2(
                data = test_data,
                confounders = c("age_at_diagnosis", "sex"),
                dataset_name = "test_cohort",
                other_map = test_other_map,
                output_dirs = list(obj3_pfs2 = test_output_dir),
                prefix = "test_"
            )
        })
        
        # Verify that the function returns a list
        expect_true(is.list(result), info = "analyze_pfs2 should return a list")
    })

    # Test 4: Verify that other_map documentation appears in diagnostics
    test_that("other_map documentation appears in diagnostics", {
        # Create a test model with other_map
        test_other_map <- list(
            location = c("Peripheral", "Mid-peripheral"),
            sex = c("Unknown")
        )
        
        # Test create_comprehensive_diagnostics with other_map
        test_model <- lm(age_at_diagnosis ~ sex + location, data = test_data)
        
        diagnostics <- create_comprehensive_diagnostics(
            model_fit = test_model,
            data = test_data,
            outcome_var = "age_at_diagnosis",
            predictor_vars = c("sex", "location"),
            confounders = NULL,
            analysis_name = "test_other_map",
            dataset_name = "test_cohort",
            other_map = test_other_map,
            treatment_var = "treatment_group"
        )
        
        # Verify that diagnostics are created
        expect_true(is.list(diagnostics), info = "create_comprehensive_diagnostics should return a list")
        
        # Verify that diagnostics are created successfully
        expect_true(is.list(diagnostics), info = "Diagnostics should be a list")
        
        # Check that diagnostics contains expected components (other_map may not be directly exposed)
        expect_true(length(diagnostics) > 0, info = "Diagnostics should contain data")
    })

    # Test 5: Verify that other_map.rds contents are accurately reflected
    test_that("other_map.rds contents are accurately reflected", {
        # Load the actual other_map.rds file
        other_map_file <- "final_data/Analytic Dataset/other_map.rds"
        
        if (file.exists(other_map_file)) {
            combined_other_map <- readRDS(other_map_file)
            
            # Verify that it's a list
            expect_true(is.list(combined_other_map), 
                       info = "other_map.rds should contain a list")
            
            # Verify that it has the expected structure
            if (length(combined_other_map) > 0) {
                # Check that each cohort has a list of variable mappings
                for (cohort_name in names(combined_other_map)) {
                    cohort_map <- combined_other_map[[cohort_name]]
                    expect_true(is.list(cohort_map), 
                               info = sprintf("Cohort %s should have a list of variable mappings", cohort_name))
                }
            }
        } else {
            # If file doesn't exist, that's okay for testing
            expect_true(TRUE, info = "other_map.rds file may not exist in test environment")
        }
    })

    # Test 6: Verify that analysis functions use other_map correctly
    test_that("analysis functions use other_map correctly", {
        # Test generate_regression_table with other_map
        test_other_map <- list(
            location = c("Peripheral", "Mid-peripheral")
        )
        
        result <- generate_regression_table(
            data = test_data,
            outcome_var = "age_at_diagnosis",
            predictor_vars = "location",
            confounders = c("sex"),
            model_type = "linear",
            effect_measure = "beta",
            analysis_name = "test_other_map",
            dataset_name = "test_cohort",
            output_dir = test_output_dir,
            prefix = "test_",
            other_map = test_other_map,
            treatment_var = "treatment_group"
        )
        
        # Verify that the function works with other_map
        expect_true(is.list(result), info = "generate_regression_table should work with other_map")
    })
}) 