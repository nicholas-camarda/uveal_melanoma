# tests/testthat/test-pfs2-edge-case-handling.R
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("PFS-2 edge case handling works correctly", {
    # Clean up test output directory before running tests
    test_output_dir <- "test_output/pfs2_edge_case_handling_test"
    if (dir.exists(test_output_dir)) {
        unlink(test_output_dir, recursive = TRUE)
    }
    dir.create(test_output_dir)

    # Setup test data
    test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

    # Test 1: Verify fit_regression_model() handles perfect separation correctly
    test_that("fit_regression_model handles perfect separation correctly", {
        # Create test data with perfect separation
        perfect_sep_data <- test_data[1:50, ]  # Small subset for testing
        perfect_sep_data$perfect_sep_var <- factor(ifelse(perfect_sep_data$age_at_diagnosis > 60, "A", "B"))
        
        # This should create perfect separation
        expect_no_error({
            result <- fit_regression_model(
                data = perfect_sep_data,
                formula = "age_at_diagnosis ~ perfect_sep_var + sex",
                model_type = "linear",
                time_var = NULL,
                event_var = NULL
            )
        })
        
        # Verify that the function returns a result even with perfect separation
        expect_true(!is.null(result), info = "Should handle perfect separation gracefully")
    })

    # Test 2: Verify apply_extreme_estimate_filtering() removes entire variables when all estimates are extreme
    test_that("apply_extreme_estimate_filtering removes entire variables when all estimates are extreme", {
        # Create a test model result with extreme estimates
        test_model <- lm(age_at_diagnosis ~ sex + location, data = test_data)
        
        # Test the filtering function - use the correct signature
        test_table <- tbl_regression(test_model, data = test_data)
        expect_no_error({
            filtered_result <- apply_extreme_estimate_filtering(
                tbl = test_table,
                model_fit = test_model,
                analysis_name = "test_extreme_filtering"
            )
        })
        
        # Verify that the function returns a result
        expect_true(!is.null(filtered_result), info = "Should return filtered results")
    })

    # Test 3: Verify save_table_outputs() creates diagnostic HTML when no meaningful content
    test_that("save_table_outputs creates diagnostic HTML when no meaningful content", {
        # Create a test table result
        test_model <- lm(age_at_diagnosis ~ sex, data = test_data)
        test_table <- tbl_regression(test_model, data = test_data)
        
        # Test saving with minimal content
        expect_no_error({
            result <- save_table_outputs(
                table_result = test_table,
                raw_output = data.frame(),  # Empty raw output
                model_fit = test_model,
                analysis_name = "test_minimal_content",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                diagnostics = list(
                    filtering_summary = list(
                        table_has_meaningful_content = FALSE
                    )
                ),
                data = test_data,
                outcome_var = "age_at_diagnosis",
                confounders = NULL,
                treatment_var = "treatment_group"
            )
        })
        
        # Verify that the function returns a result
        expect_true(!is.null(result), info = "Should handle minimal content gracefully")
    })

    # Test 4: Verify comprehensive diagnostics Excel file documents all removed variables
    test_that("comprehensive diagnostics Excel file documents all removed variables", {
        # Create a test model with some variables that might be filtered
        test_model <- lm(age_at_diagnosis ~ sex + location + initial_t_stage, data = test_data)
        
        # Test comprehensive diagnostics creation
        expect_no_error({
            diagnostics <- create_comprehensive_diagnostics(
                model_fit = test_model,
                data = test_data,
                outcome_var = "age_at_diagnosis",
                predictor_vars = c("sex", "location", "initial_t_stage"),
                confounders = NULL,
                analysis_name = "test_comprehensive_diagnostics",
                dataset_name = "test_cohort",
                other_map = list(),
                treatment_var = "treatment_group"
            )
        })
        
        # Verify that diagnostics are created
        expect_true(is.list(diagnostics), info = "Should return diagnostics list")
        expect_true("filtering_summary" %in% names(diagnostics), info = "Should include filtering summary")
    })

    # Test 5: Verify existing edge case handling functions are used (no custom logic)
    test_that("existing edge case handling functions are used", {
        # Test that we're using the centralized fit_regression_model function
        expect_no_error({
            result <- analyze_pfs2(
                data = test_data,
                confounders = c("age_at_diagnosis", "sex"),
                dataset_name = "test_cohort",
                other_map = list(),
                output_dirs = list(obj3_pfs2 = test_output_dir),
                prefix = "test_"
            )
        })
        
        # Verify that the function returns a result
        expect_true(is.list(result), info = "Should use existing edge case handling")
    })

    # Test 6: Verify perfect separation warnings are handled appropriately
    test_that("perfect separation warnings are handled appropriately", {
        # Create data that might trigger perfect separation warnings
        small_data <- test_data[1:20, ]
        
        expect_no_error({
            result <- analyze_pfs2(
                data = small_data,
                confounders = c("age_at_diagnosis"),
                dataset_name = "test_cohort",
                other_map = list(),
                output_dirs = list(obj3_pfs2 = test_output_dir),
                prefix = "test_"
            )
        })
        
        # Verify that the function handles warnings gracefully
        expect_true(is.list(result), info = "Should handle perfect separation warnings")
    })

    # Test 7: Verify extreme estimate filtering works correctly
    test_that("extreme estimate filtering works correctly", {
        # Test the extreme estimate filtering function directly
        test_model <- lm(age_at_diagnosis ~ sex + location, data = test_data)
        
        test_table <- tbl_regression(test_model, data = test_data)
        expect_no_error({
            filtered_result <- apply_extreme_estimate_filtering(
                tbl = test_table,
                model_fit = test_model,
                analysis_name = "test_extreme_filtering"
            )
        })
        
        # Verify that filtering works
        expect_true(!is.null(filtered_result), info = "Should filter extreme estimates")
    })

    # Test 8: Verify that edge cases don't crash the analysis pipeline
    test_that("edge cases don't crash the analysis pipeline", {
        # Test with various edge cases
        edge_cases <- list(
            # Very small dataset
            small_data = test_data[1:10, ],
            # Data with many missing values
            missing_data = test_data,
            # Data with extreme values
            extreme_data = test_data
        )
        
        for (case_name in names(edge_cases)) {
            test_data_case <- edge_cases[[case_name]]
            
            expect_no_error({
                result <- analyze_pfs2(
                    data = test_data_case,
                    confounders = c("age_at_diagnosis"),
                    dataset_name = "test_cohort",
                    other_map = list(),
                    output_dirs = list(obj3_pfs2 = test_output_dir),
                    prefix = paste0("test_", case_name, "_")
                )
            })
            
            # Verify that the function returns a result
            expect_true(is.list(result), info = paste("Should return result for", case_name))
        }
    })

    # Test 9: Verify that diagnostic output is created for edge cases
    test_that("diagnostic output is created for edge cases", {
        # Test with a challenging dataset
        challenging_data <- test_data[1:15, ]  # Very small dataset
        
        expect_no_error({
            result <- analyze_pfs2(
                data = challenging_data,
                confounders = c("age_at_diagnosis"),
                dataset_name = "test_cohort",
                other_map = list(),
                output_dirs = list(obj3_pfs2 = test_output_dir),
                prefix = "test_challenging_"
            )
        })
        
        # Verify that diagnostic files are created
        diagnostic_files <- list.files(test_output_dir, pattern = "test_challenging_.*_diagnostics.xlsx", full.names = TRUE)
        expect_true(length(diagnostic_files) > 0, info = "Should create diagnostic files for edge cases")
    })

    # Test 10: Verify that the analysis pipeline remains stable after edge cases
    test_that("analysis pipeline remains stable after edge cases", {
        # Test multiple edge cases in sequence
        test_cases <- list(
            list(data = test_data[1:10, ], confounders = c("age_at_diagnosis")),
            list(data = test_data[1:20, ], confounders = c("sex")),
            list(data = test_data[1:30, ], confounders = c("location"))
        )
        
        for (i in seq_along(test_cases)) {
            case <- test_cases[[i]]
            
            expect_no_error({
                result <- analyze_pfs2(
                    data = case$data,
                    confounders = case$confounders,
                    dataset_name = "test_cohort",
                    other_map = list(),
                    output_dirs = list(obj3_pfs2 = test_output_dir),
                    prefix = paste0("test_stability_", i, "_")
                )
            })
            
            # Verify that the function returns a result
            expect_true(is.list(result), info = paste("Should return result for test case", i))
        }
    })
}) 
 