# tests/testthat/test-hardcoded-treatment-variable-fix.R
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("Functions handle different treatment variables without hardcoded references", {
    # Clean up test output directory before running tests
    main_test_output_dir <- "test_output/hardcoded_treatment_variable_test"
    if (dir.exists(main_test_output_dir)) {
        unlink(main_test_output_dir, recursive = TRUE)
    }
    dir.create(main_test_output_dir)

    # Setup test data
    test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

    # Test 1: Verify that analyze_time_to_event_outcomes() doesn't create hardcoded treatment_group
    test_that("analyze_time_to_event_outcomes doesn't create hardcoded treatment_group", {
        # Create a copy of data with recurrence1_treatment_clean as the treatment variable
        test_data_pfs2 <- test_data %>%
            filter(!is.na(tt_pfs2_months), tt_pfs2_months >= 0, !is.null(recurrence1_treatment_clean))
        
        # Run the function with recurrence1_treatment_clean as group_var
        result <- analyze_time_to_event_outcomes(
            data = test_data_pfs2,
            time_var = "tt_pfs2_months",
            event_var = "pfs2_event",
            group_var = "recurrence1_treatment_clean",  # Use recurrence1_treatment_clean instead of treatment_group
            confounders = c("age_at_diagnosis", "sex"),
            ylab = "PFS-2 Probability",
            analysis_type = "all_patients",
            dataset_name = "test_cohort",
            output_dirs = list(
                obj3_pfs2 = main_test_output_dir,
                obj3_ph_diagnostics = main_test_output_dir,
                obj1_ph_diagnostics = main_test_output_dir,
                obj1_os = main_test_output_dir,
                baseline_characteristics = main_test_output_dir
            ),
            prefix = "test_"
        )

        # Verify that the function doesn't create a hardcoded treatment_group variable
        # The function should use the group_var parameter directly
        expect_true(is.list(result), info = "Function should return a list")
        
        if (!is.null(result$cox_model)) {
            # Check that the model uses the correct treatment variable
            model_terms <- attr(terms(result$cox_model), "term.labels")
            expect_true("recurrence1_treatment_clean" %in% model_terms, 
                       info = "Model should include recurrence1_treatment_clean")
            expect_false("treatment_group" %in% model_terms, 
                        info = "Model should NOT include treatment_group")
        }
    })

    # Test 2: Verify that generate_regression_table() handles different treatment variables
    test_that("generate_regression_table handles different treatment variables", {
        # Test with recurrence1_treatment_clean
        result_pfs2 <- generate_regression_table(
            data = test_data,
            outcome_var = "pfs2_event",
            predictor_vars = "recurrence1_treatment_clean",
            confounders = c("age_at_diagnosis", "sex"),
            model_type = "cox",
            effect_measure = "HR",
            analysis_name = "test_pfs2",
            dataset_name = "test_cohort",
            output_dir = main_test_output_dir,
            prefix = "test_",
            time_var = "tt_pfs2_months",
            event_var = "pfs2_event",
            treatment_var = "recurrence1_treatment_clean"  # Pass explicit treatment variable
        )

        # Test with treatment_group
        result_os <- generate_regression_table(
            data = test_data,
            outcome_var = "pfs_event",
            predictor_vars = "treatment_group",
            confounders = c("age_at_diagnosis", "sex"),
            model_type = "cox",
            effect_measure = "HR",
            analysis_name = "test_pfs",
            dataset_name = "test_cohort",
            output_dir = main_test_output_dir,
            prefix = "test_",
            time_var = "tt_pfs_months",
            event_var = "pfs_event",
            treatment_var = "treatment_group"  # Pass explicit treatment variable
        )

        # Both should work without errors
        expect_true(is.list(result_pfs2), info = "PFS-2 analysis should return a list")
        expect_true(is.list(result_os), info = "OS analysis should return a list")
    })

    # Test 3: Verify that create_comprehensive_diagnostics() handles different treatment variables
    test_that("create_comprehensive_diagnostics handles different treatment variables", {
        # Create a simple test model
        test_data_subset <- test_data %>%
            filter(!is.na(tt_pfs2_months), tt_pfs2_months >= 0, !is.null(recurrence1_treatment_clean)) %>%
            select(tt_pfs2_months, pfs2_event, recurrence1_treatment_clean, age_at_diagnosis, sex) %>%
            filter(!is.na(recurrence1_treatment_clean))

        if (nrow(test_data_subset) > 0) {
            # Fit a simple model
            model <- tryCatch({
                survival::coxph(Surv(tt_pfs2_months, pfs2_event) ~ recurrence1_treatment_clean + age_at_diagnosis + sex, 
                               data = test_data_subset)
            }, error = function(e) NULL)

            if (!is.null(model)) {
                # Test diagnostics with recurrence1_treatment_clean
                diagnostics <- create_comprehensive_diagnostics(
                    model_fit = model,
                    data = test_data_subset,
                    outcome_var = "pfs2_event",
                    predictor_vars = "recurrence1_treatment_clean",
                    confounders = c("age_at_diagnosis", "sex"),
                    analysis_name = "test_pfs2",
                    dataset_name = "test_cohort",
                    treatment_var = "recurrence1_treatment_clean"  # Pass explicit treatment variable
                )

                expect_true(is.list(diagnostics), info = "Diagnostics should return a list")
            }
        }
    })

    # Test 4: Verify that modify_gt_table_pvalues() handles different treatment variables
    test_that("modify_gt_table_pvalues handles different treatment variables", {
        # Create a simple test table
        test_data_subset <- test_data %>%
            filter(!is.na(tt_pfs2_months), tt_pfs2_months >= 0, !is.null(recurrence1_treatment_clean)) %>%
            select(tt_pfs2_months, pfs2_event, recurrence1_treatment_clean, age_at_diagnosis, sex) %>%
            filter(!is.na(recurrence1_treatment_clean))

        if (nrow(test_data_subset) > 0) {
            # Fit a simple model
            model <- tryCatch({
                survival::coxph(Surv(tt_pfs2_months, pfs2_event) ~ recurrence1_treatment_clean + age_at_diagnosis + sex, 
                               data = test_data_subset)
            }, error = function(e) NULL)

            if (!is.null(model)) {
                # Create a gtsummary table
                table <- gtsummary::tbl_regression(model, exponentiate = TRUE)
                
                # Test table modification with recurrence1_treatment_clean
                modified_table <- modify_gt_table_pvalues(
                    gt_table = table %>% as_gt(),
                    table_result = table,
                    data = test_data_subset,
                    outcome_var = "pfs2_event",
                    confounders = c("age_at_diagnosis", "sex"),
                    model_fit = model,
                    treatment_var = "recurrence1_treatment_clean"  # Pass explicit treatment variable
                )

                expect_true(inherits(modified_table, "tbl_regression") || inherits(modified_table, "gtsummary"), 
                           info = "Modified table should be a tbl_regression or gtsummary object")
            }
        }
    })

    # Test 5: Verify that calculate_factor_label_pvalue() handles different treatment variables
    test_that("calculate_factor_label_pvalue handles different treatment variables", {
        test_data_subset <- test_data %>%
            filter(!is.na(tt_pfs2_months), tt_pfs2_months >= 0, !is.null(recurrence1_treatment_clean)) %>%
            select(tt_pfs2_months, pfs2_event, recurrence1_treatment_clean, age_at_diagnosis, sex) %>%
            filter(!is.na(recurrence1_treatment_clean))

        if (nrow(test_data_subset) > 0) {
            # Fit a simple model
            model <- tryCatch({
                survival::coxph(Surv(tt_pfs2_months, pfs2_event) ~ recurrence1_treatment_clean + age_at_diagnosis + sex, 
                               data = test_data_subset)
            }, error = function(e) NULL)

            if (!is.null(model)) {
                # Test p-value calculation with recurrence1_treatment_clean
                p_value <- calculate_factor_label_pvalue(
                    model_fit = model,
                    variable_name = "recurrence1_treatment_clean",
                    data = test_data_subset,
                    outcome_var = "pfs2_event",
                    confounders = c("age_at_diagnosis", "sex"),
                    treatment_var = "recurrence1_treatment_clean"  # Pass explicit treatment variable
                )

                expect_true(is.numeric(p_value), info = "P-value should be numeric")
                expect_true(p_value >= 0 && p_value <= 1, info = "P-value should be between 0 and 1")
            }
        }
    })
})

test_that("Backward compatibility is maintained for existing treatment_group usage", {
    # Clean up test output directory before running tests
    test_output_dir <- "test_output/backward_compatibility_test"
    if (dir.exists(test_output_dir)) {
        unlink(test_output_dir, recursive = TRUE)
    }
    dir.create(test_output_dir)

    # Setup test data
    test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

    # Test 1: Verify that functions still work with treatment_group (backward compatibility)
    test_that("Functions work with treatment_group for backward compatibility", {
        # Test analyze_time_to_event_outcomes with treatment_group
        result <- analyze_time_to_event_outcomes(
            data = test_data,
            time_var = "tt_pfs_months",
            event_var = "pfs_event",
            group_var = "treatment_group",  # Use treatment_group (default)
            confounders = c("age_at_diagnosis", "sex"),
            ylab = "Progression-Free Survival",
            analysis_type = "post_treatment_only",
            dataset_name = "test_cohort",
            output_dirs = list(
                obj1_pfs = test_output_dir,
                obj1_ph_diagnostics = test_output_dir,
                obj1_os = test_output_dir,
                baseline_characteristics = test_output_dir
            ),
            prefix = "test_"
        )

        expect_true(is.list(result), info = "Function should return a list with treatment_group")
        
        if (!is.null(result$cox_model)) {
            model_terms <- attr(terms(result$cox_model), "term.labels")
            expect_true("treatment_group" %in% model_terms, 
                       info = "Model should include treatment_group when specified")
        }
    })

    # Test 2: Verify that generate_regression_table works with treatment_group
    test_that("generate_regression_table works with treatment_group", {
        result <- generate_regression_table(
            data = test_data,
            outcome_var = "pfs_event",
            predictor_vars = "treatment_group",
            confounders = c("age_at_diagnosis", "sex"),
            model_type = "cox",
            effect_measure = "HR",
            analysis_name = "test_pfs_backward",
            dataset_name = "test_cohort",
            output_dir = test_output_dir,
            prefix = "test_",
            time_var = "tt_pfs_months",
            event_var = "pfs_event",
            treatment_var = "treatment_group"  # Use treatment_group
        )

        expect_true(is.list(result), info = "Function should work with treatment_group")
    })
})

test_that("No warnings are generated when using recurrence1_treatment_clean", {
    # Clean up test output directory before running tests
    test_output_dir <- "test_output/no_warnings_test"
    if (dir.exists(test_output_dir)) {
        unlink(test_output_dir, recursive = TRUE)
    }
    dir.create(test_output_dir)

    # Setup test data
    test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

    # Test 1: Verify that no warnings are generated when using recurrence1_treatment_clean
    test_that("No warnings with recurrence1_treatment_clean", {
        # Capture warnings
        warnings_captured <- capture_warnings({
            result <- analyze_time_to_event_outcomes(
                data = test_data,
                time_var = "tt_pfs2_months",
                event_var = "pfs2_event",
                group_var = "recurrence1_treatment_clean",
                confounders = c("age_at_diagnosis", "sex"),
                ylab = "PFS-2 Probability",
                analysis_type = "all_patients",
                dataset_name = "test_cohort",
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

        # Check that no warnings are related to treatment variable issues
        treatment_warnings <- warnings_captured[grepl("treatment", tolower(warnings_captured))]
        expect_true(length(treatment_warnings) == 0, 
                   info = "No warnings should be generated related to treatment variable issues")
    })

    # Test 2: Verify that generate_regression_table works without warnings
    test_that("generate_regression_table works without warnings", {
        warnings_captured <- capture_warnings({
            result <- generate_regression_table(
                data = test_data,
                outcome_var = "pfs2_event",
                predictor_vars = "recurrence1_treatment_clean",
                confounders = c("age_at_diagnosis", "sex"),
                model_type = "cox",
                effect_measure = "HR",
                analysis_name = "test_pfs2_no_warnings",
                dataset_name = "test_cohort",
                output_dir = test_output_dir,
                prefix = "test_",
                time_var = "tt_pfs2_months",
                event_var = "pfs2_event",
                treatment_var = "recurrence1_treatment_clean"
            )
        })

        # Check that no warnings are related to treatment variable issues
        treatment_warnings <- warnings_captured[grepl("treatment", tolower(warnings_captured))]
        expect_true(length(treatment_warnings) == 0, 
                   info = "No warnings should be generated related to treatment variable issues")
    })
}) 
 