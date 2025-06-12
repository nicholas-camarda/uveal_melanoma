# Test the corrected forest plot implementation
# Following forestploter documentation pattern

cat("=== TESTING CORRECTED FOREST PLOT IMPLEMENTATION ===\n")

library(forestploter)
library(grid)

# Load required functions
source("scripts/utils/analysis_config.R")
source("scripts/visualization/forest_plot.R")
source("scripts/data_helper/data_utilities.R")

# Create test output directory
if (!dir.exists("test_output")) {
    dir.create("test_output", recursive = TRUE)
}

# Create mock subgroup results data
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
            treatment_effect = c(0.95, 1.15),
            ci_lower = c(0.70, 0.85),
            ci_upper = c(1.30, 1.55),
            p_value = c(0.75, 0.35),
            n_gksrs = c(40, 40),
            n_plaque = c(45, 45),
            n_total = c(85, 85),
            stringsAsFactors = FALSE
        )
    )
)

# Test the corrected forest plot function
cat("✓ Creating forest plot with corrected implementation...\n")

forest_plot <- create_single_cohort_forest_plot(
    subgroup_results = mock_subgroup_results,
    outcome_name = "Overall Survival",
    cohort_name = "Test Cohort",
    treatment_labels = TREATMENT_LABELS,
    variable_order = c("age_at_diagnosis", "sex"),
    effect_measure = "HR",
    favours_labels = FAVOURS_LABELS,
    title = "Test Forest Plot - Corrected Implementation"
)

# Save the plot
png("test_output/test_corrected_forest_plot.png", 
    width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, 
    units = PLOT_UNITS, res = PLOT_DPI)
print(forest_plot)
dev.off()

cat("✓ Corrected forest plot saved successfully\n")
cat("=== CORRECTED IMPLEMENTATION TEST COMPLETE ===\n") 