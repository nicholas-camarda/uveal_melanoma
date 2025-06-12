# Test Fixed Forest Plot Implementation
# This script tests the corrected forest plot following forestploter documentation

# Load required libraries
library(tidyverse)
library(forestploter)
library(grid)

# Source the updated forest plot functions
source("scripts/utils/analysis_config.R")
source("scripts/visualization/forest_plot.R")

cat("=== TESTING FIXED FOREST PLOT IMPLEMENTATION ===\n")

# Create mock subgroup results that match the expected structure
create_mock_subgroup_results <- function() {
    # Create realistic mock data for testing
    variables <- c("age_at_diagnosis", "sex", "location")
    
    results <- list()
    
    for (var in variables) {
        if (var == "age_at_diagnosis") {
            subgroup_effects <- data.frame(
                subgroup_level = c("< 65 years", "≥ 65 years"),
                treatment_effect = c(0.85, 1.20),
                ci_lower = c(0.60, 0.90),
                ci_upper = c(1.20, 1.60),
                p_value = c(0.35, 0.19),
                n_gksrs = c(45, 38),
                n_plaque = c(52, 41),
                n_total = c(97, 79),
                stringsAsFactors = FALSE
            )
        } else if (var == "sex") {
            subgroup_effects <- data.frame(
                subgroup_level = c("Male", "Female"),
                treatment_effect = c(1.10, 0.95),
                ci_lower = c(0.75, 0.70),
                ci_upper = c(1.61, 1.29),
                p_value = c(0.61, 0.75),
                n_gksrs = c(40, 43),
                n_plaque = c(42, 51),
                n_total = c(82, 94),
                stringsAsFactors = FALSE
            )
        } else if (var == "location") {
            subgroup_effects <- data.frame(
                subgroup_level = c("Anterior", "Posterior"),
                treatment_effect = c(0.90, 1.15),
                ci_lower = c(0.65, 0.85),
                ci_upper = c(1.25, 1.55),
                p_value = c(0.54, 0.37),
                n_gksrs = c(35, 48),
                n_plaque = c(38, 55),
                n_total = c(73, 103),
                stringsAsFactors = FALSE
            )
        }
        
        results[[var]] <- list(
            subgroup_effects = subgroup_effects,
            interaction_p = runif(1, 0.1, 0.8)
        )
    }
    
    return(results)
}

# Helper functions that format_variable_name and format_sample_size might call
format_variable_name <- function(var_name) {
    switch(var_name,
           "age_at_diagnosis" = "Age at Diagnosis",
           "sex" = "Sex", 
           "location" = "Tumor Location",
           tools::toTitleCase(gsub("_", " ", var_name))
    )
}

format_sample_size <- function(n_treatment, n_total) {
    if (is.na(n_treatment) || is.na(n_total)) return("")
    sprintf("%d", n_treatment)
}

format_p_value <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("< 0.001")
    return(sprintf("%.3f", p))
}

# Create mock data
cat("✓ Creating mock subgroup results...\n")
mock_results <- create_mock_subgroup_results()

# Test the fixed forest plot
cat("✓ Creating forest plot with FIXED implementation...\n")

# Create forest plot using the corrected functions
fp <- create_single_cohort_forest_plot(
    subgroup_results = mock_results,
    outcome_name = "Overall Survival",
    cohort_name = "Test Cohort",
    treatment_labels = c("GKSRS", "Plaque"),
    variable_order = c("age_at_diagnosis", "sex", "location"),
    effect_measure = "HR",
    favours_labels = c("Favours GKSRS", "Favours Plaque"),
    clip = c(0.5, 2.0),
    title = "Fixed Forest Plot Test"
)

# Save the plot
output_file <- "test_output/fixed_forest_plot.png"
dir.create("test_output", showWarnings = FALSE, recursive = TRUE)

png(output_file, width = 12, height = 8, units = "in", res = 300)
print(fp)
dev.off()

cat("✓ Fixed forest plot saved to:", output_file, "\n")
cat("=== FIXED IMPLEMENTATION TEST COMPLETE ===\n") 