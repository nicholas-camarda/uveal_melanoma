# Test Updated Subgroup Table Styling
# Tests the updated format_subgroup_analysis_results function to match the image styling

# Load required libraries
library(tidyverse)
library(gt)
library(gtsummary)
library(writexl)

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/utils/output_utilities.R")
source("scripts/analysis/subgroup_analysis.R")
source("scripts/visualization/forest_plot.R")

# Create test output directory
test_output_dir <- "test_output/updated_subgroup_styling"
if (!dir.exists(test_output_dir)) {
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
}

cat("=== UPDATED SUBGROUP TABLE STYLING TEST ===\n")
cat("Testing the new styling to match the image format\n")
cat("Test output directory:", test_output_dir, "\n\n")

# Create mock subgroup results that match the image structure
create_optic_nerve_subgroup_results <- function() {
    
    # Mock data for optic nerve subgroup (matching the image)
    optic_nerve_effects <- data.frame(
        subgroup_level = c("Y", "N"),
        n_total = c(70, 23),
        n_plaque = c(16, 1),
        n_gksrs = c(54, 22),
        treatment_effect = c(-0.49, 1.13),
        ci_lower = c(-1.57, -3.17),
        ci_upper = c(0.59, 5.42),
        p_value = c(0.3724, 0.6091),
        stringsAsFactors = FALSE
    )
    
    # Create the subgroup results structure with confounders info
    subgroup_results <- list(
        optic_nerve = list(
            subgroup_effects = optic_nerve_effects,
            interaction_p = 0.4752,
            confounders_used = c("age_at_diagnosis", "sex", "location", "initial_tumor_height")
        )
    )
    
    return(subgroup_results)
}

cat("1. Creating mock optic nerve subgroup results to match the image...\n")
optic_nerve_results <- create_optic_nerve_subgroup_results()
cat("✓ Mock data created\n")

cat("\n2. Testing the updated table formatting...\n")
output_path <- file.path(test_output_dir, "optic_nerve_subgroup_styled.xlsx")

tryCatch({
    formatted_table <- format_subgroup_analysis_results(
        subgroup_results = optic_nerve_results,
        outcome_name = "Tumor Height Change | SENSITIVITY - Restricted",
        effect_measure = "MD",
        output_path = output_path
    )
    
    cat("✓ Table formatting completed successfully\n")
    cat(sprintf("  - Excel file: %s\n", output_path))
    cat(sprintf("  - HTML file: %s\n", gsub("\\.xlsx$", ".html", output_path)))
    
    if (!is.null(formatted_table)) {
        cat(sprintf("  - Table dimensions: %d rows x %d columns\n", nrow(formatted_table), ncol(formatted_table)))
        cat("  - Column names:", paste(names(formatted_table), collapse = ", "), "\n")
    }
    
}, error = function(e) {
    cat("✗ Table formatting failed:", e$message, "\n")
    cat("Error details:", conditionMessage(e), "\n")
})

cat("\n3. Testing with different effect measures...\n")

# Test HR formatting
cat("  Testing HR formatting...\n")
hr_results <- optic_nerve_results
hr_results$optic_nerve$subgroup_effects$treatment_effect <- c(0.75, 1.15)
hr_results$optic_nerve$subgroup_effects$ci_lower <- c(0.45, 0.68)
hr_results$optic_nerve$subgroup_effects$ci_upper <- c(1.25, 1.95)

hr_output_path <- file.path(test_output_dir, "hr_example_styled.xlsx")
tryCatch({
    hr_formatted <- format_subgroup_analysis_results(
        subgroup_results = hr_results,
        outcome_name = "Overall Survival - Full Cohort",
        effect_measure = "HR",
        output_path = hr_output_path
    )
    cat("  ✓ HR table created\n")
}, error = function(e) {
    cat("  ✗ HR table failed:", e$message, "\n")
})

# Test OR formatting
cat("  Testing OR formatting...\n")
or_results <- optic_nerve_results
or_results$optic_nerve$subgroup_effects$treatment_effect <- c(1.25, 0.85)
or_results$optic_nerve$subgroup_effects$ci_lower <- c(0.75, 0.45)
or_results$optic_nerve$subgroup_effects$ci_upper <- c(2.10, 1.60)

or_output_path <- file.path(test_output_dir, "or_example_styled.xlsx")
tryCatch({
    or_formatted <- format_subgroup_analysis_results(
        subgroup_results = or_results,
        outcome_name = "Local Recurrence - Restricted Cohort",
        effect_measure = "OR",
        output_path = or_output_path
    )
    cat("  ✓ OR table created\n")
}, error = function(e) {
    cat("  ✗ OR table failed:", e$message, "\n")
})

cat("\n4. Checking generated files...\n")
all_files <- list.files(test_output_dir, recursive = TRUE)
for (file in all_files) {
    file_path <- file.path(test_output_dir, file)
    if (file.exists(file_path)) {
        file_size <- file.size(file_path)
        cat(sprintf("  ✓ %s (%d bytes)\n", file, file_size))
    } else {
        cat(sprintf("  ✗ %s (missing)\n", file))
    }
}

cat("\n=== STYLING VERIFICATION ===\n")
cat("The new styling should include:\n")
cat("  ✓ Title: 'Subgroup Analysis: [Variable Name]'\n")
cat("  ✓ Subtitle: 'Treatment Effect on [Outcome] | Interaction P-value: [value]'\n")
cat("  ✓ Column headers: 'Subgroup Level', 'Sample Size', 'Treatment Effect (95% CI)', 'P-value'\n")
cat("  ✓ Caption with:\n")
cat("    - Treatment comparison (GKSRS vs Plaque reference)\n")
cat("    - Effect interpretation\n")
cat("    - Model formula\n")
cat("    - Confounders list\n")
cat("    - Dataset information\n")
cat("  ✓ Bold labels and italicized levels\n")

cat("\n✓ UPDATED SUBGROUP TABLE STYLING TEST COMPLETED!\n")
cat("\nOpen the HTML files to verify the styling matches the image format.\n") 