# Test Script for Proportional Hazards Assumption Testing
# Author: Nicholas Camarda
# Description: Demonstrates and validates the new PH assumption testing functionality

# Load required libraries
library(tidyverse)
library(survival)

# Source required functions
source("scripts/data_helper/data_processing.R")
source("scripts/utils/analysis_config.R")
source("scripts/data_helper/data_utilities.R")
source("scripts/utils/output_utilities.R")
source("scripts/analysis/statistical_analysis.R")

# Create test output directory
test_output_dir <- file.path("test_output", "ph_diagnostics")
if (!dir.exists(test_output_dir)) {
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
}

cat("=== TESTING PROPORTIONAL HAZARDS ASSUMPTION FUNCTION ===\n")
cat("Test Date:", format(Sys.time()), "\n\n")

# Test 1: Load a sample dataset and run PH diagnostics
cat("Test 1: Testing PH assumption function with sample data\n")
cat("-------------------------------------------------------\n")

tryCatch({
    # Load any available dataset for testing
    available_datasets <- list_available_datasets()
    if (length(available_datasets) == 0) {
        cat("No datasets available for testing. Please run main analysis first.\n")
        stop("No datasets available")
    }
    
    # Use the first available dataset
    test_dataset <- available_datasets[1]
    cat(sprintf("Using dataset: %s\n", test_dataset))
    
    # Load data
    data <- readRDS(file.path(PROCESSED_DATA_DIR, paste0(test_dataset, ".rds")))
    cat(sprintf("Loaded %d patients\n", nrow(data)))
    
    # Create a simple Cox model for testing
    cat("Fitting test Cox model...\n")
    
    # Filter valid data
    test_data <- data %>%
        filter(
            !is.na(tt_death_months),
            !is.na(death_event),
            tt_death_months >= 0
        )
    
    cat(sprintf("Test data: %d patients, %d events\n", 
                nrow(test_data), sum(test_data$death_event)))
    
    # Fit a simple Cox model
    test_cox_model <- coxph(Surv(tt_death_months, death_event) ~ treatment_group + age_at_diagnosis, 
                           data = test_data)
    
    cat("Cox model fitted successfully\n")
    print(summary(test_cox_model))
    
    # Test the PH assumption function
    cat("\nTesting proportional hazards assumption...\n")
    
    ph_results <- test_proportional_hazards_assumption(
        cox_model = test_cox_model,
        outcome_name = "Overall Survival (Test)",
        output_dir = test_output_dir,
        file_prefix = "test_",
        dataset_name = paste("Test Dataset:", test_dataset)
    )
    
    if (!is.null(ph_results)) {
        cat("SUCCESS: PH assumption testing completed\n")
        cat("Files created in:", test_output_dir, "\n")
        
        # Show summary of results
        cat("\nPH Test Results Summary:\n")
        print(ph_results$ph_summary)
        
        # Check for violations
        violations <- ph_results$ph_summary[ph_results$ph_summary$PH_Assumption == "VIOLATED", ]
        if (nrow(violations) > 0) {
            cat(sprintf("\nVIOLATIONS DETECTED: %d variable(s) violate PH assumption\n", nrow(violations)))
            print(violations[, c("Variable", "P_Value", "Interpretation")])
        } else {
            cat("\nNO VIOLATIONS: All variables satisfy PH assumption\n")
        }
        
    } else {
        cat("ERROR: PH assumption testing failed\n")
    }
    
}, error = function(e) {
    cat(sprintf("Test 1 failed: %s\n", e$message))
})

cat("\n")

# Test 2: Test with invalid inputs
cat("Test 2: Testing error handling with invalid inputs\n")
cat("---------------------------------------------------\n")

tryCatch({
    # Test with NULL model
    result1 <- test_proportional_hazards_assumption(
        cox_model = NULL,
        outcome_name = "Test",
        output_dir = test_output_dir
    )
    cat("NULL model test:", ifelse(is.null(result1), "PASSED", "FAILED"), "\n")
    
    # Test with wrong model type
    wrong_model <- lm(mpg ~ cyl, data = mtcars)
    result2 <- test_proportional_hazards_assumption(
        cox_model = wrong_model,
        outcome_name = "Test",
        output_dir = test_output_dir
    )
    cat("Wrong model type test:", ifelse(is.null(result2), "PASSED", "FAILED"), "\n")
    
}, error = function(e) {
    cat(sprintf("Test 2 error: %s\n", e$message))
})

cat("\n=== PH ASSUMPTION TESTING VALIDATION COMPLETE ===\n")
cat("Check the test_output/ph_diagnostics/ directory for generated files\n")

# List generated files
if (dir.exists(test_output_dir)) {
    files <- list.files(test_output_dir, full.names = FALSE)
    if (length(files) > 0) {
        cat("\nFiles generated:\n")
        for (file in files) {
            cat(sprintf("- %s\n", file))
        }
    }
} 