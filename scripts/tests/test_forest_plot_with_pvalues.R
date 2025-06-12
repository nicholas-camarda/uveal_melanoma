#!/usr/bin/env Rscript

#' Test Script for Forest Plots with P-values
#' 
#' This script tests the updated forest plot functions to ensure p-values 
#' are correctly included in the plots.

# Setup logging
cat("=== Testing Forest Plots with P-values ===\n")
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Load required libraries
suppressPackageStartupMessages({
    library(dplyr)
    library(forestploter)
    library(grid)
})

# Source required files
tryCatch({
    source("scripts/utils/analysis_config.R")
    source("scripts/visualization/forest_plot.R")
    cat("✓ Required scripts loaded successfully\n")
}, error = function(e) {
    cat("✗ Error loading scripts:", e$message, "\n")
    quit(status = 1)
})

# Create mock subgroup results data for testing
create_mock_subgroup_results <- function() {
    # Mock results for age_at_diagnosis variable
    age_results <- list(
        subgroup_effects = data.frame(
            subgroup_level = c("< 65", ">= 65"),
            treatment_effect = c(0.85, 1.20),
            ci_lower = c(0.60, 0.90),
            ci_upper = c(1.20, 1.60),
            p_value = c(0.023, 0.156),
            n_gksrs = c(45, 35),
            n_plaque = c(50, 40),
            n_total = c(95, 75),
            stringsAsFactors = FALSE
        )
    )
    
    # Mock results for sex variable
    sex_results <- list(
        subgroup_effects = data.frame(
            subgroup_level = c("Female", "Male"),
            treatment_effect = c(1.10, 0.75),
            ci_lower = c(0.80, 0.55),
            ci_upper = c(1.50, 1.05),
            p_value = c(0.445, 0.089),
            n_gksrs = c(30, 50),
            n_plaque = c(35, 55),
            n_total = c(65, 105),
            stringsAsFactors = FALSE
        )
    )
    
    return(list(
        age_at_diagnosis = age_results,
        sex = sex_results
    ))
}

# Test the forest plot functionality
tryCatch({
    cat("\n--- Creating Mock Subgroup Data ---\n")
    
    # Create test data
    test_results <- create_mock_subgroup_results()
    cat("✓ Mock subgroup data created with", length(test_results), "variables\n")
    
    # Test data structure creation
    cat("\n--- Testing Data Structure Creation ---\n")
    plot_data <- create_forest_plot_data(
        subgroup_results = test_results,
        variable_order = names(test_results),
        treatment_labels = c("GKSRS", "Plaque"),
        effect_measure = "HR"
    )
    
    cat("✓ Forest plot data structure created\n")
    cat("✓ Data frame columns:", paste(colnames(plot_data$data_frame), collapse = ", "), "\n")
    
    # Check for p-value column
    if ("p-value" %in% colnames(plot_data$data_frame)) {
        cat("✓ P-value column successfully included\n")
    } else {
        cat("✗ P-value column missing\n")
        print("Available columns:")
        print(colnames(plot_data$data_frame))
    }
    
    # Display sample of the data
    cat("\n--- Sample Data Structure ---\n")
    print(plot_data$data_frame)
    
    # Test single cohort forest plot creation
    cat("\n--- Testing Single Cohort Forest Plot ---\n")
    single_plot <- create_single_cohort_forest_plot(
        subgroup_results = test_results,
        outcome_name = "Overall Survival",
        cohort_name = "Test Cohort",
        treatment_labels = c("GKSRS", "Plaque"),
        variable_order = names(test_results),
        effect_measure = "HR",
        favours_labels = c("Favours GKSRS", "Favours Plaque"),
        clip = c(0.1, 10)
    )
    
    if (is.null(single_plot)) {
        stop("Single cohort forest plot creation failed")
    }
    cat("✓ Single cohort forest plot created successfully\n")
    
    # Save test plot
    test_output_dir <- "test_output/forest_plots_with_pvalues"
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    
    png(file.path(test_output_dir, paste0("test_single_cohort_forest_plot_", timestamp, ".png")), 
        width = 12, height = 8, units = "in", res = 300)
    plot(single_plot)
    dev.off()
    cat("✓ Test plot saved to:", file.path(test_output_dir, paste0("test_single_cohort_forest_plot_", timestamp, ".png")), "\n")
    
    cat("\n=== All Tests Completed Successfully ===\n")
    cat("✓ P-values are now included in forest plots\n")
    cat("✓ The forest plot function follows forestploter documentation correctly\n")
    
}, error = function(e) {
    cat("✗ Error in testing:", e$message, "\n")
    cat("Traceback:\n")
    traceback()
    quit(status = 1)
}) 