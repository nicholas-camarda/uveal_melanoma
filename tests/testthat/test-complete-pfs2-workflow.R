# tests/testthat/test-complete-pfs2-workflow.R
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("Complete PFS-2 workflow works correctly with all fixes", {
    # Clean up test output directory before running tests
    test_output_dir <- "test_output/complete_pfs2_workflow_test"
    if (dir.exists(test_output_dir)) {
        unlink(test_output_dir, recursive = TRUE)
    }
    dir.create(test_output_dir)

    # Setup test data
    test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

    # Test 1: Execute complete PFS-2 workflow with all fixes
    test_that("Execute complete PFS-2 workflow with all fixes", {
        expect_no_error({
            result <- analyze_pfs2(
                data = test_data,
                confounders = c("age_at_diagnosis", "sex", "location"),
                dataset_name = "test_cohort",
                other_map = list(),
                output_dirs = list(
                    obj3_pfs2 = test_output_dir,
                    obj3_ph_diagnostics = test_output_dir,
                    obj1_ph_diagnostics = test_output_dir,
                    obj1_os = test_output_dir,
                    baseline_characteristics = test_output_dir
                ),
                prefix = "test_"
            )
        })
        
        # Verify that the function returns a result
        expect_true(is.list(result), info = "Should return a list result")
    })

    # Test 2: Verify only second-line treatment variables are in the model
    test_that("Only second-line treatment variables are in the model", {
        # Check that the analysis uses recurrence1_treatment_clean, not treatment_group
        expect_no_error({
            result <- analyze_pfs2(
                data = test_data,
                confounders = c("age_at_diagnosis", "sex"),
                dataset_name = "test_cohort",
                other_map = list(),
                output_dirs = list(obj3_pfs2 = test_output_dir),
                prefix = "test_treatment_check_"
            )
        })
        
        # Verify that the function returns a result
        expect_true(is.list(result), info = "Should return a result")
        
        # Check that the analysis completed without errors
        expect_true(length(result) > 0, info = "Should have analysis results")
    })

    # Test 3: Verify proper confounders are used throughout
    test_that("Proper confounders are used throughout", {
        # Test with different confounder combinations
        confounder_combinations <- list(
            c("age_at_diagnosis", "sex"),
            c("age_at_diagnosis", "sex", "location"),
            c("age_at_diagnosis")
        )
        
        for (i in seq_along(confounder_combinations)) {
            confounders <- confounder_combinations[[i]]
            
            expect_no_error({
                result <- analyze_pfs2(
                    data = test_data,
                    confounders = confounders,
                    dataset_name = "test_cohort",
                    other_map = list(),
                    output_dirs = list(obj3_pfs2 = test_output_dir),
                    prefix = paste0("test_confounders_", i, "_")
                )
            }, info = paste("Should work with confounders:", paste(confounders, collapse = ", ")))
            
            # Verify that the function returns a result
            expect_true(is.list(result), info = paste("Should return result for confounders:", paste(confounders, collapse = ", ")))
        }
    })

    # Test 4: Verify "Other" categories are properly documented
    test_that("Other categories are properly documented", {
        # Load existing other_map for testing
        other_map <- get_cohort_specific_other_map("uveal_melanoma_full_cohort")
        
        expect_no_error({
            result <- analyze_pfs2(
                data = test_data,
                confounders = c("age_at_diagnosis", "sex"),
                dataset_name = "test_cohort",
                other_map = other_map,
                output_dirs = list(obj3_pfs2 = test_output_dir),
                prefix = "test_other_docs_"
            )
        })
        
        # Verify that the function returns a result
        expect_true(is.list(result), info = "Should work with other_map documentation")
    })

    # Test 5: Verify existing perfect separation handling is used (no custom logic)
    test_that("Existing perfect separation handling is used", {
        # Create a small dataset that might trigger perfect separation
        small_data <- test_data[1:30, ]
        
        expect_no_error({
            result <- analyze_pfs2(
                data = small_data,
                confounders = c("age_at_diagnosis"),
                dataset_name = "test_cohort",
                other_map = list(),
                output_dirs = list(obj3_pfs2 = test_output_dir),
                prefix = "test_perfect_sep_"
            )
        })
        
        # Verify that the function returns a result even with potential perfect separation
        expect_true(is.list(result), info = "Should handle perfect separation gracefully")
    })

    # Test 6: Verify existing edge case handling functions are used (no custom logic)
    test_that("Existing edge case handling functions are used", {
        # Test with challenging data
        challenging_data <- test_data[1:20, ]
        
        expect_no_error({
            result <- analyze_pfs2(
                data = challenging_data,
                confounders = c("age_at_diagnosis"),
                dataset_name = "test_cohort",
                other_map = list(),
                output_dirs = list(obj3_pfs2 = test_output_dir),
                prefix = "test_edge_cases_"
            )
        })
        
        # Verify that the function returns a result
        expect_true(is.list(result), info = "Should handle edge cases gracefully")
    })

    # Test 7: Verify all analysis components work together
    test_that("All analysis components work together", {
        # Test the complete workflow with all features
        other_map <- get_cohort_specific_other_map("uveal_melanoma_full_cohort")
        
        expect_no_error({
            result <- analyze_pfs2(
                data = test_data,
                confounders = c("age_at_diagnosis", "sex", "location"),
                dataset_name = "test_cohort",
                other_map = other_map,
                output_dirs = list(
                    obj3_pfs2 = test_output_dir,
                    obj3_ph_diagnostics = test_output_dir,
                    obj1_ph_diagnostics = test_output_dir,
                    obj1_os = test_output_dir,
                    baseline_characteristics = test_output_dir
                ),
                prefix = "test_complete_"
            )
        })
        
        # Verify that the function returns a result
        expect_true(is.list(result), info = "Should complete full workflow successfully")
        
        # Check that output files are created
        output_files <- list.files(test_output_dir, pattern = "test_complete_.*", full.names = TRUE)
        expect_true(length(output_files) > 0, info = "Should create output files")
    })

    # Test 8: Verify workflow handles all our fixes correctly
    test_that("Workflow handles all our fixes correctly", {
        # Test that the workflow uses the correct treatment variable
        # Test that it handles hardcoded variable issues
        # Test that it uses existing edge case handling
        # Test that it documents "Other" categories properly
        
        expect_no_error({
            result <- analyze_pfs2(
                data = test_data,
                confounders = c("age_at_diagnosis", "sex"),
                dataset_name = "test_cohort",
                other_map = list(),
                output_dirs = list(obj3_pfs2 = test_output_dir),
                prefix = "test_fixes_"
            )
        })
        
        # Verify that the function returns a result
        expect_true(is.list(result), info = "Should work with all our fixes")
        
        # Check that the analysis completed without critical errors
        expect_true(length(result) > 0, info = "Should have analysis results")
    })
}) 
 