# Test script to verify our forest plot functions work with forestploter
# Location: scripts/tests/test_forest_plot_functions.R

cat("=== TESTING FOREST PLOT FUNCTIONS ===\n")

# Load libraries that should be in main.R
library(forestploter)
library(grid)
library(tidyverse)

# Source the forest plot functions
source("scripts/visualization/forest_plot.R")
source("scripts/utils/analysis_config.R")

# Test 1: Create mock subgroup results
cat("Test 1: Creating mock subgroup analysis results...\n")
tryCatch({
    # Create mock subgroup results similar to what the analysis functions would produce
    mock_subgroup_results <- list(
        age_at_diagnosis = list(
            subgroup_effects = data.frame(
                subgroup_level = c("< 65 years", ">= 65 years"),
                treatment_effect = c(1.2, 0.8),
                ci_lower = c(0.9, 0.6),
                ci_upper = c(1.6, 1.1),
                p_value = c(0.15, 0.25),
                n_gksrs = c(45, 35),
                n_plaque = c(50, 40),
                n_total = c(95, 75),
                stringsAsFactors = FALSE
            )
        ),
        sex = list(
            subgroup_effects = data.frame(
                subgroup_level = c("Male", "Female"),
                treatment_effect = c(1.1, 1.3),
                ci_lower = c(0.8, 1.0),
                ci_upper = c(1.5, 1.7),
                p_value = c(0.40, 0.05),
                n_gksrs = c(40, 40),
                n_plaque = c(45, 45),
                n_total = c(85, 85),
                stringsAsFactors = FALSE
            )
        )
    )
    
    cat("✓ Mock subgroup results created successfully\n")
}, error = function(e) {
    cat("✗ Mock data creation failed:", conditionMessage(e), "\n")
})

# Test 2: Test create_forest_plot_data function
cat("Test 2: Testing create_forest_plot_data function...\n")
tryCatch({
    plot_data <- create_forest_plot_data(
        subgroup_results = mock_subgroup_results,
        variable_order = c("age_at_diagnosis", "sex"),
        treatment_labels = c("GKSRS", "Plaque"),
        effect_measure = "HR"
    )
    
    cat("✓ Forest plot data created successfully\n")
    cat("Data frame dimensions:", nrow(plot_data$data_frame), "x", ncol(plot_data$data_frame), "\n")
    print(head(plot_data$data_frame))
}, error = function(e) {
    cat("✗ Forest plot data creation failed:", conditionMessage(e), "\n")
})

# Test 3: Test create_single_cohort_forest_plot function
cat("Test 3: Testing create_single_cohort_forest_plot function...\n")
tryCatch({
    forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = mock_subgroup_results,
        outcome_name = "Test Outcome",
        cohort_name = "Test Cohort",
        treatment_labels = c("GKSRS", "Plaque"),
        variable_order = c("age_at_diagnosis", "sex"),
        effect_measure = "HR",
        favours_labels = c("Favours GKSRS", "Favours Plaque"),
        title = "Test Forest Plot"
    )
    
    cat("✓ Forest plot object created successfully\n")
    
    # Save the plot to test output
    test_output_dir <- "test_output"
    if (!dir.exists(test_output_dir)) {
        dir.create(test_output_dir, recursive = TRUE)
    }
    
    png(file.path(test_output_dir, "test_forest_plot_functions.png"), 
        width = 1000, height = 800, res = 150)
    plot(forest_plot)
    dev.off()
    
    cat("✓ Forest plot saved to test_output/test_forest_plot_functions.png\n")
    
}, error = function(e) {
    cat("✗ Forest plot creation failed:", conditionMessage(e), "\n")
    cat("Error details:", e$message, "\n")
})

cat("=== FOREST PLOT FUNCTIONS TESTING COMPLETE ===\n") 