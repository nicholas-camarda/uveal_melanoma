# Re-run Primary Outcomes Subgroup Analysis
# Specifically to ensure location data is included for local recurrence and metastatic progression

# Define VERBOSE for data utilities functions
VERBOSE <- TRUE

# Load required libraries
suppressMessages({
    library(tidyverse)
    library(survival)
    library(gtsummary)
    library(gt)
})

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/data_helper/data_processing.R")
source("scripts/data_helper/data_utilities.R") 
source("scripts/analysis/subgroup_analysis.R")
source("scripts/visualization/forest_plot.R")

cat("=== RE-RUNNING PRIMARY OUTCOMES SUBGROUP ANALYSIS ===\n")
cat("Goal: Ensure location data is included for local recurrence and metastatic progression\n\n")

# Load test data
cat("1. Loading full cohort data...\n")
test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
cat(sprintf("✓ Loaded %d patients\n", nrow(test_data)))

# Show current subgroup variables
cat("\n2. Current subgroup variables:\n")
for (var in subgroup_vars) {
    if (var %in% names(test_data)) {
        cat(sprintf("  ✓ %s\n", var))
    } else {
        cat(sprintf("  ✗ %s (missing)\n", var))
    }
}

# Specifically test location for the problematic outcomes
cat("\n3. Testing location analysis for problematic outcomes...\n")

# 3a. Local Recurrence with location
cat("\n3a. Local recurrence × location interaction...\n")
recurrence_location <- analyze_treatment_effect_subgroups_binary(
    data = test_data,
    outcome_var = "recurrence1",
    subgroup_vars = "location",
    confounders = setdiff(confounders, "location"),
    outcome_name = "Local Recurrence"
)

if ("location" %in% names(recurrence_location)) {
    result <- recurrence_location[["location"]]
    if (!is.null(result$interaction_p) && !is.na(result$interaction_p)) {
        cat(sprintf("✓ Interaction p-value: %.4f\n", result$interaction_p))
        cat(sprintf("✓ Subgroup effects: %d rows\n", nrow(result$subgroup_effects)))
    } else {
        cat("✗ Failed to get interaction p-value\n")
    }
} else {
    cat("✗ Location data not found in results\n")
}

# 3b. Metastatic Progression with location
cat("\n3b. Metastatic progression × location interaction...\n")
mets_location <- analyze_treatment_effect_subgroups_binary(
    data = test_data,
    outcome_var = "mets_progression", 
    subgroup_vars = "location",
    confounders = setdiff(confounders, "location"),
    outcome_name = "Metastatic Progression"
)

if ("location" %in% names(mets_location)) {
    result <- mets_location[["location"]]
    if (!is.null(result$interaction_p) && !is.na(result$interaction_p)) {
        cat(sprintf("✓ Interaction p-value: %.4f\n", result$interaction_p))
        cat(sprintf("✓ Subgroup effects: %d rows\n", nrow(result$subgroup_effects)))
    } else {
        cat("✗ Failed to get interaction p-value\n")
    }
} else {
    cat("✗ Location data not found in results\n")
}

# 4. Now run full primary outcomes subgroup analysis with ALL variables
cat("\n4. Running full primary outcomes subgroup analysis...\n")

# Local Recurrence - all subgroup variables
cat("\n4a. Local recurrence subgroup analysis (all variables)...\n")
recurrence_all_results <- analyze_treatment_effect_subgroups_binary(
    data = test_data,
    outcome_var = "recurrence1",
    subgroup_vars = subgroup_vars,
    confounders = confounders,
    outcome_name = "Local Recurrence"
)

cat(sprintf("✓ Local recurrence results for %d variables\n", length(recurrence_all_results)))
cat("Variables included:", paste(names(recurrence_all_results), collapse = ", "), "\n")

# Metastatic Progression - all subgroup variables  
cat("\n4b. Metastatic progression subgroup analysis (all variables)...\n")
mets_all_results <- analyze_treatment_effect_subgroups_binary(
    data = test_data,
    outcome_var = "mets_progression",
    subgroup_vars = subgroup_vars,
    confounders = confounders,
    outcome_name = "Metastatic Progression"
)

cat(sprintf("✓ Metastatic progression results for %d variables\n", length(mets_all_results)))
cat("Variables included:", paste(names(mets_all_results), collapse = ", "), "\n")

# Overall Survival - all subgroup variables
cat("\n4c. Overall survival subgroup analysis (all variables)...\n")
os_all_results <- analyze_treatment_effect_subgroups_survival(
    data = test_data,
    time_var = "tt_death_months",
    event_var = "death_event",
    subgroup_vars = subgroup_vars,
    confounders = confounders,
    outcome_name = "Overall Survival"
)

cat(sprintf("✓ Overall survival results for %d variables\n", length(os_all_results)))
cat("Variables included:", paste(names(os_all_results), collapse = ", "), "\n")

# Progression-Free Survival - all subgroup variables
cat("\n4d. Progression-free survival subgroup analysis (all variables)...\n")
pfs_all_results <- analyze_treatment_effect_subgroups_survival(
    data = test_data,
    time_var = "tt_pfs_months",
    event_var = "pfs_event",
    subgroup_vars = subgroup_vars,
    confounders = confounders,
    outcome_name = "Progression-Free Survival"
)

cat(sprintf("✓ Progression-free survival results for %d variables\n", length(pfs_all_results)))
cat("Variables included:", paste(names(pfs_all_results), collapse = ", "), "\n")

# 5. Create the combined results object
cat("\n5. Creating combined primary outcomes subgroup results...\n")
primary_outcomes_subgroup_results <- list(
    local_recurrence = recurrence_all_results,
    metastatic_progression = mets_all_results,
    overall_survival = os_all_results,
    progression_free_survival = pfs_all_results
)

# Check location specifically in each outcome
cat("\nLocation data check:\n")
for (outcome in names(primary_outcomes_subgroup_results)) {
    results <- primary_outcomes_subgroup_results[[outcome]]
    if ("location" %in% names(results)) {
        location_result <- results[["location"]]
        if (!is.null(location_result$interaction_p)) {
            cat(sprintf("  %s: ✓ p = %.4f\n", outcome, location_result$interaction_p))
        } else {
            cat(sprintf("  %s: ✗ p = NULL\n", outcome))
        }
    } else {
        cat(sprintf("  %s: ✗ NO location data\n", outcome))
    }
}

# 6. Save the updated results
cat("\n6. Saving updated primary outcomes subgroup results...\n")
output_file <- "final_data/Analysis/uveal_full/tables/primary_outcomes/subgroup_analysis/uveal_full_primary_outcomes_subgroup_results.rds"

# Create directory if it doesn't exist
output_dir <- dirname(output_file)
if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    cat(sprintf("✓ Created directory: %s\n", output_dir))
}

# Save the results
saveRDS(primary_outcomes_subgroup_results, output_file)
cat(sprintf("✓ Saved updated results to: %s\n", output_file))

cat("\n=== RERUN COMPLETE ===\n")
cat("Primary outcomes subgroup analysis has been updated with location data.\n")
cat("You can now run the forest plot test again to see location data.\n") 