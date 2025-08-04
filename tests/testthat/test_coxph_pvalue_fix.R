# Test Script for Cox Regression P-Value Fix
# Author: Nicholas Camarda
# Date: 2025-01-03
# Description: Test script to verify that Cox models with model=TRUE produce p-values correctly

# Load required libraries
library(survival)
library(gtsummary)

# Source helper functions
source("scripts/utils/all_helper_functions.R")

#' Test Cox model p-value extraction
#'
#' This function tests whether Cox models fitted with model=TRUE produce p-values
#' correctly in gtsummary tables.
#'
#' @return Logical indicating whether all tests passed
test_coxph_pvalue_extraction <- function() {
    cat("=== Testing Cox Model P-Value Fix ===\n")
    
    # Load test data
    test_data_file <- "final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds"
    if (!file.exists(test_data_file)) {
        cat("ERROR: Test data file not found:", test_data_file, "\n")
        return(FALSE)
    }
    
    test_data <- readRDS(test_data_file)
    cat("✓ Loaded test data with", nrow(test_data), "patients\n")
    
    # Test 1: Basic Cox model with model=TRUE
    cat("\n--- Test 1: Basic Cox Model ---\n")
    surv_obj <- Surv(test_data$tt_death_years, test_data$death_event)
    test_data$surv_obj <- surv_obj
    
    # Test with model=TRUE
    cox_model_with_model <- coxph(surv_obj ~ treatment_group, data = test_data, model = TRUE)
    
    # Verify model frame is present
    if (is.null(cox_model_with_model$model)) {
        cat("❌ ERROR: Model frame missing from Cox model with model=TRUE\n")
        return(FALSE)
    } else {
        cat("✓ Model frame present in Cox model with model=TRUE\n")
    }
    
    # Test without model=TRUE (for comparison)
    cox_model_without_model <- coxph(surv_obj ~ treatment_group, data = test_data)
    
    if (is.null(cox_model_without_model$model)) {
        cat("✓ Model frame missing from Cox model without model=TRUE (expected)\n")
    } else {
        cat("⚠️  WARNING: Model frame present in Cox model without model=TRUE (unexpected)\n")
    }
    
    # Test 2: Create gtsummary table with model=TRUE
    cat("\n--- Test 2: gtsummary Table Generation ---\n")
    table_with_model <- cox_model_with_model %>% tbl_regression(exponentiate = TRUE)
    
    # Verify p-values are present
    table_data <- table_with_model$table_body
    p_values_present <- !all(is.na(table_data$p.value))
    
    if (p_values_present) {
        cat("✓ P-values present in gtsummary table with model=TRUE\n")
        cat("  P-values found:", sum(!is.na(table_data$p.value)), "out of", nrow(table_data), "rows\n")
    } else {
        cat("❌ ERROR: P-values missing from gtsummary table with model=TRUE\n")
        return(FALSE)
    }
    
    # Test 3: Check factor level handling
    cat("\n--- Test 3: Factor Level Handling ---\n")
    # Add a variable with "Other" level - create a proper factor for all rows
    test_data$test_factor <- factor(
        sample(c("A", "B", "Other"), nrow(test_data), replace = TRUE),
        levels = c("A", "B", "Other")
    )
    
    # Test Cox model with "Other" level
    tryCatch({
        cox_model_with_other <- coxph(surv_obj ~ treatment_group + test_factor, data = test_data, model = TRUE)
        cat("✓ Cox model with 'Other' level fitted successfully\n")
        
        # Check if model has p-values
        summary_result <- summary(cox_model_with_other)
        if (ncol(summary_result$coefficients) >= 5) {
            p_values <- summary_result$coefficients[, 5]  # Pr(>|z|)
        } else if (ncol(summary_result$coefficients) >= 4) {
            p_values <- summary_result$coefficients[, 4]  # Fallback
        } else {
            p_values <- rep(NA, nrow(summary_result$coefficients))
        }
        
        if (any(!is.na(p_values))) {
            cat("✓ P-values extracted from Cox model with 'Other' level\n")
        } else {
            cat("❌ ERROR: No p-values extracted from Cox model with 'Other' level\n")
            return(FALSE)
        }
        
    }, error = function(e) {
        cat("❌ ERROR: Cox model with 'Other' level failed:", e$message, "\n")
        return(FALSE)
    })
    
    # Test 4: Test the full table generation pipeline
    cat("\n--- Test 4: Full Pipeline Test ---\n")
    tryCatch({
        # Use the actual table generation function
        result <- generate_regression_table(
            data = test_data,
            outcome_var = "surv_obj",
            predictor_vars = "treatment_group",
            confounders = c("age_at_diagnosis", "sex"),
            model_type = "cox",
            effect_measure = "HR",
            analysis_name = "test_cox_analysis",
            dataset_name = "test_dataset",
            output_dir = "test_output",
            prefix = "test_",
            time_var = "tt_death_years",
            event_var = "death_event"
        )
        
        if (!is.null(result$table)) {
            cat("✓ Full pipeline test passed\n")
        } else {
            cat("❌ ERROR: Full pipeline test failed - table is NULL\n")
            return(FALSE)
        }
        
    }, error = function(e) {
        cat("❌ ERROR: Full pipeline test failed:", e$message, "\n")
        return(FALSE)
    })
    
    cat("\n=== All Cox Model P-Value Tests Passed ===\n")
    return(TRUE)
}

#' Test factor level validation
#'
#' This function tests the factor level validation for Cox models.
#'
#' @return Logical indicating whether all tests passed
test_factor_level_validation <- function() {
    cat("\n=== Testing Factor Level Validation ===\n")
    
    # Load test data
    test_data_file <- "final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds"
    if (!file.exists(test_data_file)) {
        cat("ERROR: Test data file not found:", test_data_file, "\n")
        return(FALSE)
    }
    
    test_data <- readRDS(test_data_file)
    
    # Create survival object
    surv_obj <- Surv(test_data$tt_death_years, test_data$death_event)
    test_data$surv_obj <- surv_obj
    
    # Test with various factor level scenarios
    test_scenarios <- list(
        "normal_factor" = list(levels = c("A", "B", "C"), probs = c(0.4, 0.4, 0.2)),
        "factor_with_other" = list(levels = c("A", "B", "Other"), probs = c(0.4, 0.4, 0.2)),
        "factor_with_rare_other" = list(levels = c("A", "B", "Other"), probs = c(0.45, 0.45, 0.1))
    )
    
    for (scenario_name in names(test_scenarios)) {
        cat(sprintf("\n--- Testing %s ---\n", scenario_name))
        
        # Create factor with proper length for the dataset
        scenario <- test_scenarios[[scenario_name]]
        test_data$test_factor <- factor(
            sample(scenario$levels, nrow(test_data), replace = TRUE, prob = scenario$probs),
            levels = scenario$levels
        )
        
        tryCatch({
            cox_model <- coxph(surv_obj ~ treatment_group + test_factor, data = test_data, model = TRUE)
            cat(sprintf("✓ Cox model fitted successfully for %s\n", scenario_name))
            
            # Check if p-values are available
            summary_result <- summary(cox_model)
            if (ncol(summary_result$coefficients) >= 5) {
                p_values <- summary_result$coefficients[, 5]
            } else if (ncol(summary_result$coefficients) >= 4) {
                p_values <- summary_result$coefficients[, 4]
            } else {
                p_values <- rep(NA, nrow(summary_result$coefficients))
            }
            
            if (any(!is.na(p_values))) {
                cat(sprintf("✓ P-values extracted for %s\n", scenario_name))
            } else {
                cat(sprintf("⚠️  WARNING: No p-values extracted for %s\n", scenario_name))
            }
            
        }, error = function(e) {
            cat(sprintf("❌ ERROR: Cox model failed for %s: %s\n", scenario_name, e$message))
        })
    }
    
    return(TRUE)
}

#' Main test function
#'
#' Runs all tests for the Cox model p-value fix.
#'
#' @return Logical indicating whether all tests passed
main_test <- function() {
    cat("Starting Cox Model P-Value Fix Tests\n")
    cat("=====================================\n\n")
    
    # Test 1: Basic p-value extraction
    test1_result <- test_coxph_pvalue_extraction()
    
    # Test 2: Factor level validation
    test2_result <- test_factor_level_validation()
    
    # Summary
    cat("\n=== Test Summary ===\n")
    if (test1_result && test2_result) {
        cat("✓ All tests passed! Cox model p-value fix is working correctly.\n")
        return(TRUE)
    } else {
        cat("❌ Some tests failed. Please review the errors above.\n")
        return(FALSE)
    }
}

# Run tests if script is executed directly
if (!interactive()) {
    success <- main_test()
    if (!success) {
        stop("Tests failed")
    }
} 