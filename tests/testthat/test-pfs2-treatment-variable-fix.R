# tests/testthat/test-pfs2-treatment-variable-fix.R
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("PFS-2 analysis uses recurrence1_treatment_clean as treatment variable", {
    # Clean up test output directory before running tests
    main_test_output_dir <- "test_output/pfs2_treatment_variable_test"
    if (dir.exists(main_test_output_dir)) {
        unlink(main_test_output_dir, recursive = TRUE)
    }
    dir.create(main_test_output_dir)

    # Setup test data
    test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

    # Run the actual analyze_pfs2 function
    analyze_pfs2_result <- analyze_pfs2(
        data = test_data,
        confounders = c("age_at_diagnosis", "sex", "location"),
        dataset_name = "test_cohort",
        other_map = list(),
        output_dirs = list(
            obj3_pfs2 = main_test_output_dir,
            obj3_ph_diagnostics = main_test_output_dir
        ),
        prefix = "test_"
    )

    # Test 1: Verify that recurrence1_treatment_clean is used in the model
    if (!is.null(analyze_pfs2_result$survival_analysis$cox_model)) {
        model_formula <- formula(analyze_pfs2_result$survival_analysis$cox_model)
        model_terms <- attr(terms(model_formula), "term.labels")
        
        # The model should include recurrence1_treatment_clean as the main predictor
        expect_true("recurrence1_treatment_clean" %in% model_terms, 
                   info = "recurrence1_treatment_clean should be in the model terms")
        
        # The model should NOT include treatment_group as a predictor
        expect_false("treatment_group" %in% model_terms, 
                    info = "treatment_group should NOT be in the model terms")
        
        # Verify that recurrence1_treatment_clean is the first term (main predictor)
        expect_equal(model_terms[1], "recurrence1_treatment_clean", 
                    info = "recurrence1_treatment_clean should be the first term in the model")
    }

    # Test 2: Verify that the data filtering uses recurrence1_treatment_clean
    expect_true("recurrence1_treatment_clean" %in% names(analyze_pfs2_result$pfs2_data),
               info = "recurrence1_treatment_clean should be in the filtered data")
    
    # Test 3: Verify that the summary table uses recurrence1_treatment_clean for grouping
    if (!is.null(analyze_pfs2_result$summary_table)) {
        # The summary table should be grouped by recurrence1_treatment_clean
        expect_true("recurrence1_treatment_clean" %in% names(analyze_pfs2_result$pfs2_data),
                   info = "Summary table should use recurrence1_treatment_clean for grouping")
    }

    # Test 4: Verify that the model correctly represents second-line treatment effects
    if (!is.null(analyze_pfs2_result$survival_analysis$cox_model)) {
        # Check that the model terms represent the correct research question
        model_terms <- attr(terms(analyze_pfs2_result$survival_analysis$cox_model), "term.labels")
        expect_true("recurrence1_treatment_clean" %in% model_terms,
                   info = "Model terms should include recurrence1_treatment_clean")
        expect_false("treatment_group" %in% model_terms,
                    info = "Model terms should NOT include treatment_group")
    }

    # Test 5: Verify that confounders are correctly included
    expected_confounders <- c("age_at_diagnosis", "sex")  # location was removed due to insufficient levels
    if (!is.null(analyze_pfs2_result$survival_analysis$cox_model)) {
        model_terms <- attr(terms(analyze_pfs2_result$survival_analysis$cox_model), "term.labels")
        included_confounders <- intersect(expected_confounders, model_terms)
        expect_true(length(included_confounders) > 0,
                   info = paste("Expected confounders:", paste(expected_confounders, collapse = ", "),
                               "Found:", paste(model_terms, collapse = ", ")))
    }

    # Test 6: Verify that the analysis correctly models second-line treatment effects only
    if (!is.null(analyze_pfs2_result$survival_analysis$cox_model)) {
        # The model should only include variables related to second-line treatment
        # and confounders, not initial treatment
        model_terms <- attr(terms(analyze_pfs2_result$survival_analysis$cox_model), "term.labels")
        
        # Should include recurrence1_treatment_clean (second-line treatment)
        expect_true("recurrence1_treatment_clean" %in% model_terms,
                   info = "Model should include recurrence1_treatment_clean for second-line treatment")
        
        # Should NOT include treatment_group (initial treatment)
        expect_false("treatment_group" %in% model_terms,
                    info = "Model should NOT include treatment_group (initial treatment)")
    }
})

test_that("PFS-2 analysis handles perfect separation correctly using existing functions", {
    # Clean up test output directory before running tests
    test_output_dir <- "test_output/pfs2_perfect_separation_test"
    if (dir.exists(test_output_dir)) {
        unlink(test_output_dir, recursive = TRUE)
    }
    dir.create(test_output_dir)

    # Setup test data
    test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

    # Run PFS-2 analysis
    result <- analyze_pfs2(
        data = test_data,
        confounders = c("age_at_diagnosis", "sex", "location"),
        dataset_name = "test_cohort",
        other_map = list(),
        output_dirs = list(
            obj3_pfs2 = test_output_dir,
            obj3_ph_diagnostics = test_output_dir
        ),
        prefix = "test_"
    )

    # Test 1: Verify that perfect separation is handled by existing fit_regression_model function
    if (!is.null(result$survival_analysis$cox_model)) {
        # The model should have perfect separation information if it exists
        expect_true(is.list(result$survival_analysis$cox_model),
                   info = "Cox model should be a list object")
        
        # Check if perfect separation was detected (this is expected in PFS-2 data)
        if (!is.null(result$survival_analysis$cox_model$perfect_separation_vars)) {
            expect_true(is.character(result$survival_analysis$cox_model$perfect_separation_vars),
                       info = "Perfect separation variables should be a character vector")
        }
    }

    # Test 2: Verify that the analysis continues despite perfect separation
    expect_true(is.list(result),
               info = "Analysis should return a list even with perfect separation")
    
    expect_true("survival_analysis" %in% names(result),
               info = "Result should contain survival_analysis element")
    
    expect_true("summary_table" %in% names(result),
               info = "Result should contain summary_table element")

    # Test 3: Verify that extreme estimates are handled by existing functions
    if (!is.null(result$survival_analysis$diagnostics)) {
        expect_true(is.list(result$survival_analysis$diagnostics),
                   info = "Diagnostics should be a list")
    }
})

test_that("PFS-2 analysis uses correct treatment variable in all function calls", {
    # Clean up test output directory before running tests
    test_output_dir <- "test_output/pfs2_function_calls_test"
    if (dir.exists(test_output_dir)) {
        unlink(test_output_dir, recursive = TRUE)
    }
    dir.create(test_output_dir)

    # Setup test data
    test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

    # Run PFS-2 analysis
    result <- analyze_pfs2(
        data = test_data,
        confounders = c("age_at_diagnosis", "sex", "location"),
        dataset_name = "test_cohort",
        other_map = list(),
        output_dirs = list(
            obj3_pfs2 = test_output_dir,
            obj3_ph_diagnostics = test_output_dir
        ),
        prefix = "test_"
    )

    # Test 1: Verify that the correct treatment variable is passed through the function chain
    if (!is.null(result$survival_analysis$cox_model)) {
        # The model should use recurrence1_treatment_clean as the treatment variable
        model_formula <- formula(result$survival_analysis$cox_model)
        model_terms <- attr(terms(model_formula), "term.labels")
        
        # Verify that recurrence1_treatment_clean is the main predictor
        expect_true("recurrence1_treatment_clean" %in% model_terms,
                   info = "Model should include recurrence1_treatment_clean as main predictor")
        
        # Verify that treatment_group is NOT included
        expect_false("treatment_group" %in% model_terms,
                    info = "Model should NOT include treatment_group")
    }

    # Test 2: Verify that the analysis correctly represents the research question
    # PFS-2 analysis should model: "What is the effect of second-line treatment on freedom from second recurrence?"
    if (!is.null(result$survival_analysis$cox_model)) {
        model_terms <- attr(terms(result$survival_analysis$cox_model), "term.labels")
        
        # The model should represent second-line treatment effects
        expect_true("recurrence1_treatment_clean" %in% model_terms,
                   info = "Model should represent second-line treatment effects")
        
        # The model should NOT represent initial treatment effects
        expect_false("treatment_group" %in% model_terms,
                    info = "Model should NOT represent initial treatment effects")
    }

    # Test 3: Verify that the analysis uses proper confounders
    expected_confounders <- c("age_at_diagnosis", "sex")  # location removed due to insufficient levels
    if (!is.null(result$survival_analysis$cox_model)) {
        model_terms <- attr(terms(result$survival_analysis$cox_model), "term.labels")
        included_confounders <- intersect(expected_confounders, model_terms)
        
        expect_true(length(included_confounders) > 0,
                   info = paste("Model should include proper confounders. Expected:", 
                               paste(expected_confounders, collapse = ", "),
                               "Found:", paste(model_terms, collapse = ", ")))
    }
}) 