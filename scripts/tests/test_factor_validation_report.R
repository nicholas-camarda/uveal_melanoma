#!/usr/bin/env Rscript

#' Test Script for Factor Validation Report Integration
#' 
#' This script tests that factor validation results are properly included
#' in the validation report output.

# Setup logging
cat("=== Testing Factor Validation Report Integration ===\n")
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Load required libraries
suppressPackageStartupMessages({
    library(dplyr)
})

# Source required files
tryCatch({
    source("scripts/utils/analysis_config.R")
    source("scripts/data_helper/data_processing.R")
    cat("✓ Required scripts loaded successfully\n")
}, error = function(e) {
    cat("✗ Error loading scripts:", e$message, "\n")
    quit(status = 1)
})

# Test the validation report with factor validation
tryCatch({
    cat("\n--- Testing Factor Validation Report Generation ---\n")
    
    # Load and process data (this should trigger factor validation)
    cat("Loading raw data first...\n")
    raw_data <- load_raw_data()
    if (is.null(raw_data)) {
        stop("Failed to load raw data")
    }
    
    cat("Applying criteria to generate cohorts...\n")
    processed_data <- apply_criteria(raw_data)
    if (is.null(processed_data)) {
        stop("Failed to process main data")
    }
    
    cat("✓ Data processing completed successfully\n")
    
    # Generate a test validation report
    test_output_path <- file.path("test_output", paste0("test_validation_report_", timestamp, ".txt"))
    dir.create("test_output", recursive = TRUE, showWarnings = FALSE)
    
    validation_report_path <- generate_validation_report(processed_data, output_path = test_output_path)
    
    cat(sprintf("✓ Validation report generated at: %s\n", validation_report_path))
    
    # Read and check the validation report content
    if (file.exists(validation_report_path)) {
        report_content <- readLines(validation_report_path)
        
        # Check for factor validation section
        factor_section_found <- any(grepl("FACTOR LEVEL VALIDATION", report_content))
        expected_config_found <- any(grepl("EXPECTED FACTOR CONFIGURATIONS", report_content))
        cross_cohort_found <- any(grepl("CROSS-COHORT FACTOR CONSISTENCY", report_content))
        factor_result_found <- any(grepl("FACTOR VALIDATION RESULT", report_content))
        
        cat("\n--- Validation Report Content Check ---\n")
        cat(sprintf("✓ Factor validation section found: %s\n", factor_section_found))
        cat(sprintf("✓ Expected configurations found: %s\n", expected_config_found))
        cat(sprintf("✓ Cross-cohort consistency found: %s\n", cross_cohort_found))
        cat(sprintf("✓ Factor validation result found: %s\n", factor_result_found))
        
        # Display a few key lines from the factor validation section
        cat("\n--- Key Factor Validation Lines ---\n")
        factor_lines <- report_content[grepl("FACTOR VALIDATION|Treatment Group:|PASS:|FAIL:", report_content)]
        for (line in head(factor_lines, 10)) {
            cat(line, "\n")
        }
        
        # Check if all expected factors are mentioned
        expected_factors <- c("treatment_group", "recurrence1", "sex", "optic_nerve")
        factors_mentioned <- sapply(expected_factors, function(f) any(grepl(f, report_content)))
        
        cat("\n--- Expected Factors Coverage ---\n")
        for (i in 1:length(expected_factors)) {
            cat(sprintf("✓ %s mentioned: %s\n", expected_factors[i], factors_mentioned[i]))
        }
        
        all_checks_passed <- factor_section_found && expected_config_found && 
                           cross_cohort_found && factor_result_found && 
                           all(factors_mentioned)
        
        if (all_checks_passed) {
            cat("\n🎯 SUCCESS: Factor validation properly integrated into validation report!\n")
        } else {
            cat("\n❌ FAILURE: Factor validation not properly integrated\n")
            quit(status = 1)
        }
        
    } else {
        cat("❌ FAILURE: Validation report file not found\n")
        quit(status = 1)
    }
    
}, error = function(e) {
    cat("✗ Error during factor validation report test:", e$message, "\n")
    traceback()
    quit(status = 1)
})

cat("\n=== Factor Validation Report Test Completed Successfully ===\n") 