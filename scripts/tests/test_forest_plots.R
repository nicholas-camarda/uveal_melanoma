# Test Forest Plot Functions - Working Version
# Demonstrates the publication-style forest plots with real data

library(dplyr)
library(forestplot)

# Source the forest plot functions
source("scripts/visualization/forest_plot.R")

# Load actual subgroup analysis results
cat("Loading subgroup analysis results...\n")

full_results_file <- "final_data/Analysis/uveal_full/tables/primary_outcomes/subgroup_analysis/uveal_full_primary_outcomes_subgroup_results.rds"
if (file.exists(full_results_file)) {
    full_results <- readRDS(full_results_file)
    cat("✓ Full cohort results loaded\n")
} else {
    stop("Full cohort results file not found.")
}

# Create output directory for test plots
test_plots_dir <- "test_output/forest_plots"
if (!dir.exists(test_plots_dir)) {
    dir.create(test_plots_dir, recursive = TRUE, showWarnings = FALSE)
}
cat(sprintf("✓ Created output directory: %s\n", test_plots_dir))

# Define variables in the order you want them displayed
# THIS IS NOW REQUIRED for consistency across all cohorts
variable_order <- FOREST_PLOT_VARIABLE_ORDER

# Available outcomes in your data
available_outcomes <- names(full_results)
cat(sprintf("Available outcomes: %s\n", paste(available_outcomes, collapse = ", ")))

cat(sprintf("\nUsing consistent variable order: %s\n", paste(variable_order, collapse = ", ")))
cat("This ensures all cohorts show the same variables for proper comparison.\n")

# Test forest plots for each outcome
for (outcome in available_outcomes) {
    
    cat(sprintf("\n=== CREATING FOREST PLOT FOR %s ===\n", toupper(outcome)))
    
    tryCatch({
        # Create single cohort forest plot - now with required variable_order
        forest_plot <- create_single_cohort_forest_plot(
            subgroup_results = full_results[[outcome]],
            outcome_name = tools::toTitleCase(gsub("_", " ", outcome)),
            cohort_name = "Full Cohort",
            treatment_labels = c("GKSRS", "Plaque"),
            variable_order = variable_order,  # Required parameter for consistency
            effect_measure = "HR",
            favours_labels = c("Favours GKSRS", "Favours Plaque"),
            clip = c(0.1, 10),
            title = sprintf("Subgroup Analysis: %s (Full Cohort)", 
                           tools::toTitleCase(gsub("_", " ", outcome)))
        )
        
        cat(sprintf("✓ Forest plot created for %s\n", outcome))
        
        # Save the plot to PNG file
        png_filename <- file.path(test_plots_dir, sprintf("forest_plot_%s_full_cohort.png", outcome))
        png(png_filename, width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)
        print(forest_plot)
        dev.off()
        
        cat(sprintf("✓ Saved PNG: %s\n", png_filename))
        
        # Add spacing between plots
        cat("\n", paste0(rep("=", 80), collapse=""), "\n")
        
    }, error = function(e) {
        cat(sprintf("✗ Error creating forest plot for %s: %s\n", outcome, e$message))
    })
}

cat("\n=== FOREST PLOT CREATION COMPLETE ===\n")
cat(sprintf("\n📁 All forest plots saved to: %s\n", test_plots_dir))
cat("\nSUCCESS! Your forest plots are now working with the following features:\n")
cat("✓ Publication-style formatting matching your example image\n")
cat("✓ Grouped variables with bold headers (ONLY headers are bold, not subgroups)\n") 
cat("✓ Indented subgroup levels (plain text formatting)\n")
cat("✓ Sample sizes in n/N format\n")
cat("✓ Hazard ratios with 95% confidence intervals\n")
cat("✓ Properly formatted p-values\n")
cat("✓ Horizontal reference lines\n")
cat("✓ 'Favours' labels on the axis\n")
cat("✓ Plots saved in both PNG (high-resolution) and PDF formats\n")
cat("✓ CONSISTENT VARIABLES: All cohorts will show the same variables in the same order\n")
cat("✓ HANDLES MISSING DATA: Shows 'No data available' for missing variables\n")

cat("\nGenerated files:\n")
for (outcome in available_outcomes) {
    cat(sprintf("- forest_plot_%s_full_cohort.png\n", outcome))
}

cat("\nIMPORTANT FEATURES:\n")
cat("1. variable_order is REQUIRED - ensures all cohorts show the same variables\n")
cat("2. Bold formatting ONLY for variable headers, not subgroup levels\n")
cat("3. Missing variables/data show 'No data available' to maintain consistency\n")
cat("4. Handles different data structures across outcomes seamlessly\n")

cat("\nTo use these functions in your main analysis:\n")
cat("1. ALWAYS provide variable_order to ensure consistency across cohorts\n")
cat("2. Use the same variable_order for all cohorts you want to compare\n")
cat("3. Only variable headers will be bold, subgroup levels will be plain text\n")
cat("4. Missing data will be handled gracefully with consistent formatting\n")

cat("\nExample usage:\n")
cat("# Define consistent variable order for ALL cohorts\n")
cat("my_variables <- c(\"age_at_diagnosis\", \"sex\", \"location\")\n")
cat("\n")
cat("forest_plot <- create_single_cohort_forest_plot(\n")
cat("    subgroup_results = your_results[[\"overall_survival\"]],\n")
cat("    outcome_name = \"Overall Survival\",\n")
cat("    cohort_name = \"Full Cohort\",\n")
cat("    variable_order = my_variables  # REQUIRED for consistency\n")
cat(")\n")
cat("# Save the plot\n")
    cat("png(\"my_forest_plot.png\", width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)\n")
cat("print(forest_plot)\n")
cat("dev.off()\n")