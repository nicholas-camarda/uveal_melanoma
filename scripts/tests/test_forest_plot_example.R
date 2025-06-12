# Test script for improved forest plot formatting
# This script demonstrates the new forest plot styling with better headers

# Load required libraries
library(forestploter)
library(grid)

# Source the forest plot functions
source("scripts/visualization/forest_plot.R")

# Create example subgroup results data for testing
create_example_subgroup_data <- function() {
    
    # Example data for Age at Diagnosis
    age_effects <- data.frame(
        subgroup_level = c("< 65 years", ">= 65 years"),
        treatment_effect = c(1.20, 0.80),
        ci_lower = c(0.90, 0.60),
        ci_upper = c(1.60, 1.10),
        p_value = c(0.15, 0.25),
        n_gksrs = c(45, 35),
        n_plaque = c(50, 40),
        n_total = c(95, 75),
        stringsAsFactors = FALSE
    )
    
    # Example data for Sex
    sex_effects <- data.frame(
        subgroup_level = c("Male", "Female"),
        treatment_effect = c(1.10, 1.30),
        ci_lower = c(0.80, 1.00),
        ci_upper = c(1.50, 1.70),
        p_value = c(0.40, 0.05),
        n_gksrs = c(40, 40),
        n_plaque = c(45, 45),
        n_total = c(85, 85),
        stringsAsFactors = FALSE
    )
    
    # Create subgroup results structure
    subgroup_results <- list(
        age_at_diagnosis = list(subgroup_effects = age_effects),
        sex = list(subgroup_effects = sex_effects)
    )
    
    return(subgroup_results)
}

# Test the improved forest plot
cat("Testing improved forest plot formatting...\n")

# Create example data
test_data <- create_example_subgroup_data()
variable_order <- c("age_at_diagnosis", "sex")
treatment_labels <- c("GKSRS", "Plaque")

# Test single cohort forest plot
cat("Creating single cohort forest plot...\n")

tryCatch({
    # Create the forest plot
    fp <- create_single_cohort_forest_plot(
        subgroup_results = test_data,
        outcome_name = "Test Forest Plot",
        cohort_name = "Test Cohort",
        treatment_labels = treatment_labels,
        variable_order = variable_order,
        effect_measure = "HR",
        favours_labels = c("Favours GKSRS", "Favours Plaque"),
        title = "Test Forest Plot with Improved Formatting"
    )
    
    # Save the plot
    output_file <- file.path("test_output", "improved_forest_plot_example.png")
    dir.create("test_output", showWarnings = FALSE, recursive = TRUE)
    
    png(output_file, width = 12, height = 8, units = "in", res = 300)
    print(fp)
    dev.off()
    
    cat("SUCCESS: Forest plot created and saved to:", output_file, "\n")
    cat("The plot should show:\n")
    cat("- Clean, professional headers\n")
    cat("- Bold variable names (Age at Diagnosis, Sex)\n")
    cat("- Properly indented subgroup levels\n")
    cat("- Clear treatment group headers\n")
    cat("- Improved spacing and formatting\n")
    
}, error = function(e) {
    cat("ERROR in forest plot creation:", e$message, "\n")
    traceback()
})

cat("Test completed.\n") 