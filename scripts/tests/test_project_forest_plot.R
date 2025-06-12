# Test the actual project forest plot functions with corrected headers

cat("=== TESTING PROJECT FOREST PLOT FUNCTIONS ===\n")

# Load required libraries
library(forestploter)
library(grid)

# Source the corrected forest plot functions
source("scripts/utils/analysis_config.R")
source("scripts/visualization/forest_plot.R")

# Create test output directory
if (!dir.exists("test_output")) {
    dir.create("test_output", recursive = TRUE)
}

# Create mock subgroup results to test the function
mock_subgroup_results <- list(
    age_at_diagnosis = list(
        subgroup_effects = data.frame(
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
    ),
    sex = list(
        subgroup_effects = data.frame(
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
    )
)

# Test the single cohort forest plot function
cat("Testing create_single_cohort_forest_plot...\n")

tryCatch({
    # Test the create_single_cohort_forest_plot function
    forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = mock_subgroup_results,
        outcome_name = "Overall Survival",
        cohort_name = "Full Cohort",
        treatment_labels = c("GKSRS", "Plaque"),
        variable_order = c("age_at_diagnosis", "sex"),
        effect_measure = "HR",
        favours_labels = c("Favours GKSRS", "Favours Plaque"),
        title = "Test Subgroup Analysis: Overall Survival (Full Cohort)"
    )
    
    cat("✓ Forest plot created successfully\n")
    
    # Save the plot
    png("test_output/test_project_forest_plot.png", 
        width = 1000, height = 400, res = 150)
    print(forest_plot)
    dev.off()
    
    cat("✓ Forest plot saved to test_output/test_project_forest_plot.png\n")
    
}, error = function(e) {
    cat("✗ Error creating project forest plot:", conditionMessage(e), "\n")
    cat("Error details:", e$message, "\n")
})

cat("=== PROJECT FOREST PLOT TEST COMPLETE ===\n") 