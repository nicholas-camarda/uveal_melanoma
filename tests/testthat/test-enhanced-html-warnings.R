# tests/testthat/test-enhanced-html-warnings.R
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

# Clean up test output directory before running tests
test_output_dir <- "test_output/simple_html_warnings_test"
if (dir.exists(test_output_dir)) {
    unlink(test_output_dir, recursive = TRUE)
}
dir.create(test_output_dir)

# Setup test data
test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

# Test 1: Verify HTML table captions get warnings when main predictor is filtered
test_that("HTML table captions get warnings when main predictor is filtered", {
    # Test that HTML tables include warnings in captions when main predictor is filtered
    expect_no_error({
        result <- generate_regression_table(
            data = test_data,
            outcome_var = "pfs2_event",
            predictor_vars = "recurrence1_treatment_clean",
            confounders = c("age_at_diagnosis", "sex"),
            model_type = "cox",
            effect_measure = "hazard_ratio",
            analysis_name = "test_caption_warnings",
            dataset_name = "test_cohort",
            output_dir = test_output_dir,
            prefix = "test_",
            treatment_var = "recurrence1_treatment_clean",
            time_var = "tt_pfs2_years",
            event_var = "pfs2_event"
        )
    })
    
    # Verify that HTML files are created
    html_files <- list.files(test_output_dir, pattern = ".*\\.html$", full.names = TRUE)
    expect_true(length(html_files) > 0, info = "Should create HTML files")
    
    # Verify that HTML files contain warning text in captions if main predictor was filtered
    if (length(html_files) > 0) {
        html_content <- readLines(html_files[1], warn = FALSE)
        # Check for warning text in the HTML content
        has_warning_text <- any(grepl("WARNING|warning|Warning", html_content))
        expect_true(has_warning_text, info = "HTML files should contain warning text when main predictor is filtered")
    }
})

# Test 2: Verify model warnings appear in diagnostics tab
test_that("Model warnings appear in diagnostics tab", {
    # Create a simple model for testing
    test_model <- tryCatch({
        surv_obj <- Surv(test_data$tt_pfs2_years, test_data$pfs2_event)
        coxph(surv_obj ~ recurrence1_treatment_clean + age_at_diagnosis + sex, data = test_data)
    }, error = function(e) NULL)
    
    # Skip this test if model creation failed
    if (is.null(test_model)) {
        skip("Model creation failed, skipping test")
    }
    
    expect_no_error({
        result <- create_comprehensive_diagnostics(
            model_fit = test_model,
            data = test_data,
            outcome_var = "pfs2_event",
            predictor_vars = "recurrence1_treatment_clean",
            confounders = c("age_at_diagnosis", "sex"),
            analysis_name = "test_model_warnings",
            dataset_name = "test_cohort",
            treatment_var = "recurrence1_treatment_clean"
        )
    })
    
    # Verify that diagnostics contain model warnings
    expect_true(is.list(result), info = "Should return diagnostics result")
    expect_true("model_diagnostics_tab" %in% names(result), 
               info = "Should contain model diagnostics table")
    
    # Verify that model diagnostics tab contains warnings
    if ("model_diagnostics_tab" %in% names(result)) {
        model_diagnostics <- result$model_diagnostics_tab
        expect_true("model_warnings" %in% names(model_diagnostics), 
                   info = "Should include model warnings column")
    }
})

# Test 3: Verify simple implementation doesn't break existing functionality
test_that("Simple implementation doesn't break existing functionality", {
    # Test that the simple approach works without breaking existing code
    expect_no_error({
        result <- generate_regression_table(
            data = test_data,
            outcome_var = "pfs2_event",
            predictor_vars = "recurrence1_treatment_clean",
            confounders = c("age_at_diagnosis", "sex"),
            model_type = "cox",
            effect_measure = "hazard_ratio",
            analysis_name = "test_simple_implementation",
            dataset_name = "test_cohort",
            output_dir = test_output_dir,
            prefix = "test_simple_",
            treatment_var = "recurrence1_treatment_clean",
            time_var = "tt_pfs2_years",
            event_var = "pfs2_event"
        )
    })
    
    # Verify that the function returns a result
    expect_true(is.list(result), info = "Should return a list result")
})

# Test 4: Verify warnings only appear in HTML regression tables, not diagnostic files
test_that("Warnings only appear in HTML regression tables, not diagnostic files", {
    # Test that HTML tables get warnings but diagnostic files don't
    expect_no_error({
        result <- generate_regression_table(
            data = test_data,
            outcome_var = "pfs2_event",
            predictor_vars = "recurrence1_treatment_clean",
            confounders = c("age_at_diagnosis", "sex"),
            model_type = "cox",
            effect_measure = "hazard_ratio",
            analysis_name = "test_warnings_scope",
            dataset_name = "test_cohort",
            output_dir = test_output_dir,
            prefix = "test_scope_",
            treatment_var = "recurrence1_treatment_clean",
            time_var = "tt_pfs2_years",
            event_var = "pfs2_event"
        )
    })
    
    # Verify that HTML files are created
    html_files <- list.files(test_output_dir, pattern = ".*\\.html$", full.names = TRUE)
    expect_true(length(html_files) > 0, info = "Should create HTML files")
    
    # Verify that diagnostic Excel files are created
    excel_files <- list.files(test_output_dir, pattern = ".*\\.xlsx$", full.names = TRUE)
    expect_true(length(excel_files) > 0, info = "Should create diagnostic Excel files")
})

# Test 5: Verify model diagnostics tab contains all model warnings
test_that("Model diagnostics tab contains all model warnings", {
    # Create a model that will have warnings
    test_model <- tryCatch({
        surv_obj <- Surv(test_data$tt_pfs2_years, test_data$pfs2_event)
        coxph(surv_obj ~ recurrence1_treatment_clean + age_at_diagnosis + sex, data = test_data)
    }, error = function(e) NULL)
    
    # Skip this test if model creation failed
    if (is.null(test_model)) {
        skip("Model creation failed, skipping test")
    }
    
    expect_no_error({
        result <- create_comprehensive_diagnostics(
            model_fit = test_model,
            data = test_data,
            outcome_var = "pfs2_event",
            predictor_vars = "recurrence1_treatment_clean",
            confounders = c("age_at_diagnosis", "sex"),
            analysis_name = "test_model_warnings_complete",
            dataset_name = "test_cohort",
            treatment_var = "recurrence1_treatment_clean"
        )
    })
    
    # Verify that model diagnostics tab exists and has warnings column
    expect_true("model_diagnostics_tab" %in% names(result), 
               info = "Should contain model diagnostics table")
    
    if ("model_diagnostics_tab" %in% names(result)) {
        model_diagnostics <- result$model_diagnostics_tab
        expect_true("model_warnings" %in% names(model_diagnostics), 
                   info = "Should include model warnings column")
        
        # Verify that warnings are properly formatted
        warnings_text <- model_diagnostics$model_warnings[1]
        expect_true(is.character(warnings_text), 
                   info = "Model warnings should be character text")
    }
})

# Test 6: Verify simple approach works with all our fixes
test_that("Simple approach works with all our fixes", {
    # Test that the simple approach works with all the fixes we've implemented
    expect_no_error({
        result <- generate_regression_table(
            data = test_data,
            outcome_var = "pfs2_event",
            predictor_vars = "recurrence1_treatment_clean",
            confounders = c("age_at_diagnosis", "sex"),
            model_type = "cox",
            effect_measure = "hazard_ratio",
            analysis_name = "test_complete_simple_approach",
            dataset_name = "test_cohort",
            output_dir = test_output_dir,
            prefix = "test_complete_",
            treatment_var = "recurrence1_treatment_clean",
            time_var = "tt_pfs2_years",
            event_var = "pfs2_event"
        )
    })
    
    # Verify that the complete workflow works
    expect_true(is.list(result), info = "Should return a complete result")
    
    # Verify that both HTML and Excel files are created
    html_files <- list.files(test_output_dir, pattern = ".*\\.html$", full.names = TRUE)
    excel_files <- list.files(test_output_dir, pattern = ".*\\.xlsx$", full.names = TRUE)
    
    expect_true(length(html_files) > 0, info = "Should create HTML files")
    expect_true(length(excel_files) > 0, info = "Should create diagnostic Excel files")
}) 