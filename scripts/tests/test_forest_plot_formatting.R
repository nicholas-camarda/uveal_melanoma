# Test script for forest plot formatting improvements
# Tests the new "Favors" labels and selective bold formatting

library(forestploter)
library(grid)

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/visualization/forest_plot.R")

# Create sample test data
test_subgroup_results <- list(
    age_at_diagnosis = list(
        subgroup_effects = data.frame(
            subgroup_level = c("< 60 years", ">= 60 years"),
            treatment_effect = c(0.75, 1.25),
            ci_lower = c(0.45, 0.85),
            ci_upper = c(1.25, 1.85),
            p_value = c(0.032, 0.156),
            n_gksrs = c(25, 40),
            n_plaque = c(30, 35),
            n_total = c(55, 75),
            stringsAsFactors = FALSE
        ),
        interaction_p = 0.045
    ),
    sex = list(
        subgroup_effects = data.frame(
            subgroup_level = c("Male", "Female"),
            treatment_effect = c(0.85, 1.15),
            ci_lower = c(0.55, 0.75),
            ci_upper = c(1.35, 1.75),
            p_value = c(0.089, 0.234),
            n_gksrs = c(35, 30),
            n_plaque = c(40, 25),
            n_total = c(75, 55),
            stringsAsFactors = FALSE
        ),
        interaction_p = 0.178
    )
)

# Test the single cohort forest plot with new formatting
test_variable_order <- c("age_at_diagnosis", "sex")
test_treatment_labels <- c("GKSRS", "Plaque")
test_favours_labels <- c("Favours GKSRS", "Favours Plaque")

cat("Creating test forest plot with improved formatting...\n")

# Create the test plot
test_plot <- create_single_cohort_forest_plot(
    subgroup_results = test_subgroup_results,
    outcome_name = "Overall Survival",
    cohort_name = "Test Cohort",
    treatment_labels = test_treatment_labels,
    variable_order = test_variable_order,
    effect_measure = "HR",
    favours_labels = test_favours_labels,
    clip = c(0.1, 3),
    title = "Test Forest Plot - Formatting Verification"
)

# Save the test plot
output_dir <- "test_output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

output_file <- file.path(output_dir, "test_forest_plot_formatting.png")
png(output_file, width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, 
    units = PLOT_UNITS, res = PLOT_DPI)
plot(test_plot)
dev.off()

cat("Test forest plot saved to:", output_file, "\n")
cat("Please visually inspect the plot to verify:\n")
cat("1. 'Favours GKSRS' and 'Favours Plaque' labels appear below the x-axis\n")
cat("2. Variable headers (Age at Diagnosis, Sex) are bold\n")
cat("3. Subgroup levels (< 60 years, >= 60 years, Male, Female) are plain text\n")
cat("4. Column headers (Subgroup, GKSRS n/N, etc.) are bold\n") 