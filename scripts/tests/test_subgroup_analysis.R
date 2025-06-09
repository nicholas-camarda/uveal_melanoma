# Test Subgroup Analysis Components
# Independent test of subgroup analysis and interaction testing

# Define VERBOSE for data utilities functions
VERBOSE <- TRUE

# Load required libraries
suppressMessages({
    library(tidyverse)
    library(survival)
    library(gtsummary)
    library(gt)
})

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/data_helper/data_utilities.R") 
source("scripts/analysis/subgroup_analysis.R")
# Primary outcomes subgroup analysis functions now in subgroup_analysis.R
source("scripts/visualization/forest_plot.R")

# Create test output directory
test_output_dir <- file.path("test_output", format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== TESTING SUBGROUP ANALYSIS COMPONENTS ===\n")
cat(sprintf("Test output directory: %s\n", test_output_dir))

# Load test data
cat("\n1. Loading test data...\n")
tryCatch({
    test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
    cat(sprintf("✓ Loaded %d patients for testing\n", nrow(test_data)))
    
    # Check subgroup variables
    cat("Available subgroup variables:\n")
    for (var in subgroup_vars) {
        if (var %in% names(test_data)) {
            n_levels <- ifelse(is.factor(test_data[[var]]), 
                             length(levels(test_data[[var]])),
                             length(unique(test_data[[var]])))
            cat(sprintf("  %s: %d levels\n", var, n_levels))
        } else {
            cat(sprintf("  %s: MISSING\n", var))
        }
    }
    
}, error = function(e) {
    cat("✗ Error loading test data:", e$message, "\n")
    stop("Cannot proceed without test data")
})

# Test 2: Binary outcome subgroup analysis
cat("\n2. Testing binary outcome subgroup analysis...\n")
tryCatch({
    # Test with a subset of subgroup variables for speed
    test_subgroup_vars <- subgroup_vars[1:3]
    
    recurrence_subgroup_results <- analyze_treatment_effect_subgroups_binary(
        data = test_data,
        outcome_var = "recurrence1",
        subgroup_vars = test_subgroup_vars,
        confounders = confounders,
        outcome_name = "Local Recurrence (Test)"
    )
    
    cat(sprintf("✓ Binary subgroup analysis completed for %d variables\n", length(test_subgroup_vars)))
    
    # Show interaction p-values
    for (var in names(recurrence_subgroup_results)) {
        p_val <- recurrence_subgroup_results[[var]]$interaction_p
        sig_status <- ifelse(!is.na(p_val) && p_val < 0.05, "SIGNIFICANT", "non-significant")
        cat(sprintf("  %s: p = %.4f (%s)\n", var, 
                    ifelse(is.na(p_val), 999, p_val), sig_status))
    }
    
    # Save test results
    saveRDS(recurrence_subgroup_results, 
            file.path(test_output_dir, "test_binary_subgroup_results.rds"))
    
}, error = function(e) {
    cat("✗ Binary subgroup analysis test failed:", e$message, "\n")
})

# Test 3: Survival outcome subgroup analysis  
cat("\n3. Testing survival outcome subgroup analysis...\n")
tryCatch({
    test_subgroup_vars <- subgroup_vars[1:3]
    
    os_subgroup_results <- analyze_treatment_effect_subgroups_survival(
        data = test_data,
        time_var = "tt_death_months",
        event_var = "death_event",
        subgroup_vars = test_subgroup_vars,
        confounders = confounders,
        outcome_name = "Overall Survival (Test)"
    )
    
    cat(sprintf("✓ Survival subgroup analysis completed for %d variables\n", length(test_subgroup_vars)))
    
    # Show interaction p-values
    for (var in names(os_subgroup_results)) {
        p_val <- os_subgroup_results[[var]]$interaction_p
        sig_status <- ifelse(!is.na(p_val) && p_val < 0.05, "SIGNIFICANT", "non-significant") 
        cat(sprintf("  %s: p = %.4f (%s)\n", var,
                    ifelse(is.na(p_val), 999, p_val), sig_status))
    }
    
    # Save test results
    saveRDS(os_subgroup_results,
            file.path(test_output_dir, "test_survival_subgroup_results.rds"))
    
}, error = function(e) {
    cat("✗ Survival subgroup analysis test failed:", e$message, "\n")
})

# Test 4: Tumor height subgroup analysis
cat("\n4. Testing tumor height subgroup analysis...\n")
tryCatch({
    test_subgroup_var <- subgroup_vars[1]  # Test one variable for speed
    
    height_subgroup_result <- analyze_treatment_effect_subgroups_height(
        data = test_data,
        subgroup_var = test_subgroup_var,
        percentile_cut = 0.5,
        confounders = confounders,
        include_baseline_height = FALSE
    )
    
    cat("✓ Tumor height subgroup analysis completed\n")
    
    # Show interaction p-value
    p_val <- height_subgroup_result$interaction_p
    sig_status <- ifelse(!is.na(p_val) && p_val < 0.05, "SIGNIFICANT", "non-significant")
    cat(sprintf("  %s: p = %.4f (%s)\n", test_subgroup_var,
                ifelse(is.na(p_val), 999, p_val), sig_status))
    
    # Save test results
    saveRDS(height_subgroup_result,
            file.path(test_output_dir, "test_height_subgroup_result.rds"))
    
}, error = function(e) {
    cat("✗ Tumor height subgroup analysis test failed:", e$message, "\n")
})

# Test 5: Subgroup data processing functions
cat("\n5. Testing subgroup data processing...\n")
tryCatch({
    # Test continuous variable binning
    if ("age_at_treatment" %in% names(test_data)) {
        processed_result <- process_subgroup_data(
            test_data, 
            "age_at_treatment", 
            confounders
        )
        
        cat("✓ Continuous variable binning tested\n")
        cat(sprintf("  Original variable: age_at_treatment\n"))
        cat(sprintf("  Binned variable: %s\n", processed_result$subgroup_var_to_use))
        
        # Show levels
        if (is.factor(processed_result$data[[processed_result$subgroup_var_to_use]])) {
            levels_str <- paste(levels(processed_result$data[[processed_result$subgroup_var_to_use]]), 
                               collapse = ", ")
            cat(sprintf("  Levels: %s\n", levels_str))
        }
    }
    
    # Test categorical variable processing
    if ("sex" %in% names(test_data)) {
        processed_result <- process_subgroup_data(
            test_data,
            "sex",
            confounders
        )
        cat("✓ Categorical variable processing tested\n")
    }
    
}, error = function(e) {
    cat("✗ Subgroup data processing test failed:", e$message, "\n")
})

# Test 6: Create test forest plot
cat("\n6. Testing forest plot creation from subgroup results...\n")
tryCatch({
    # Use results from earlier tests if available
    if (exists("recurrence_subgroup_results") && !is.null(recurrence_subgroup_results)) {
        test_forest_plot <- create_forest_plot(
            subgroup_results = recurrence_subgroup_results,
            outcome_name = "Local Recurrence (Test)",
            effect_measure = "OR",
            dataset_name = "Test Cohort",
            output_path = file.path(test_output_dir, "test_subgroup_forest_plot.png")
        )
        
        if (!is.null(test_forest_plot)) {
            cat("✓ Forest plot created successfully\n")
        } else {
            cat("✗ Forest plot creation returned NULL\n")
        }
    } else {
        cat("⚠ Skipping forest plot test - no subgroup results available\n")
    }
    
}, error = function(e) {
    cat("✗ Forest plot creation test failed:", e$message, "\n")
})

# Test 7: Interaction term detection
cat("\n7. Testing interaction term coefficient detection...\n")
tryCatch({
    # Create a simple test model with interaction
    test_model_data <- test_data %>%
        filter(!is.na(recurrence1), !is.na(treatment_group), !is.na(sex)) %>%
        ensure_consistent_contrasts()
    
    # Fit model with interaction
    test_model <- glm(recurrence1 ~ treatment_group * sex, 
                     data = test_model_data, 
                     family = binomial())
    
    # Test coefficient name detection
    interaction_coef <- get_interaction_coefficient_name(
        test_model, "treatment_group", "sex", "Male", test_model_data
    )
    
    if (!is.null(interaction_coef)) {
        cat(sprintf("✓ Interaction coefficient detected: %s\n", interaction_coef))
        cat(sprintf("  Coefficient value: %.3f\n", coef(test_model)[interaction_coef]))
    } else {
        cat("✗ Interaction coefficient not detected\n")
    }
    
}, error = function(e) {
    cat("✗ Interaction term detection test failed:", e$message, "\n")
})

# Summary
cat("\n=== SUBGROUP ANALYSIS TEST SUMMARY ===\n")
cat(sprintf("All test outputs saved to: %s\n", test_output_dir))
cat("\nTest components checked:\n")
cat("  - Binary outcome subgroup analysis\n")
cat("  - Survival outcome subgroup analysis\n")
cat("  - Tumor height subgroup analysis\n")
cat("  - Subgroup data processing (binning, rare categories)\n")
cat("  - Forest plot creation from subgroup results\n")
cat("  - Interaction term coefficient detection\n")

cat("\nTo test individual functions:\n")
cat("  analyze_treatment_effect_subgroups_binary(data, ...)\n")
cat("  analyze_treatment_effect_subgroups_survival(data, ...)\n")
cat("  analyze_treatment_effect_subgroups_height(data, ...)\n")
cat("  process_subgroup_data(data, subgroup_var, confounders)\n") 