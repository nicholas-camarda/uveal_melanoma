# Test Missing Interaction P-values Fix
# Focused test for initial_t_stage and initial_tumor_height

cat("=== TESTING MISSING INTERACTION P-VALUES FIX ===\n")

# Load only required libraries and functions
library(dplyr)
library(survival)

# Source only the required scripts without running main.R
source("scripts/utils/analysis_config.R")
source("scripts/data_helper/data_utilities.R")
source("scripts/analysis/subgroup_analysis.R")

cat("✓ Required functions loaded\n")

# Load data
data_file <- "final_data/processed_data/uveal_full_analytic_dataset.rds"
if (file.exists(data_file)) {
    data <- readRDS(data_file)
    cat("✓ Data loaded:", nrow(data), "rows\n")
} else {
    stop("Data file not found")
}

# Test the problematic variables
test_vars <- c("initial_t_stage", "initial_tumor_height")
results <- list()

for (var in test_vars) {
    cat(sprintf("\n=== TESTING %s ===\n", var))
    
    # Test the tumor height subgroup function directly
    result <- analyze_treatment_effect_subgroups_height(
        data = data,
        subgroup_var = var,
        confounders = c("age_at_diagnosis", "sex", "location"),
        include_baseline_height = FALSE
    )
    
    results[[var]] <- result
    
    # Check results
    if (!is.null(result$interaction_p) && !is.na(result$interaction_p)) {
        cat(sprintf("✓ %s: Interaction p-value = %.4f\n", var, result$interaction_p))
    } else {
        cat(sprintf("✗ %s: Interaction p-value = NA\n", var))
        
        # Check diagnostic information
        if (!is.null(result$interaction_diagnostics)) {
            cat("Diagnostics:\n")
            diag <- result$interaction_diagnostics
            if (!is.null(diag$failure_reason)) {
                cat(sprintf("  Failure reason: %s\n", diag$failure_reason))
            }
            if (!is.null(diag$anova_error_details)) {
                cat(sprintf("  ANOVA error: %s\n", diag$anova_error_details))
            }
        }
    }
    
    # Check subgroup effects
    if (!is.null(result$subgroup_effects) && nrow(result$subgroup_effects) > 0) {
        cat(sprintf("✓ %s: %d subgroup effects calculated\n", var, nrow(result$subgroup_effects)))
    } else {
        cat(sprintf("✗ %s: No subgroup effects\n", var))
    }
}

cat("\n=== SUMMARY ===\n")
for (var in test_vars) {
    result <- results[[var]]
    status <- if (!is.null(result$interaction_p) && !is.na(result$interaction_p)) "FIXED" else "STILL BROKEN"
    cat(sprintf("%s: %s\n", var, status))
}

if (all(sapply(results, function(x) !is.null(x$interaction_p) && !is.na(x$interaction_p)))) {
    cat("\n🎉 ALL INTERACTION P-VALUES NOW CALCULATED! BUG FIXED! 🎉\n")
} else {
    cat("\n❌ Some interaction p-values still missing. Need more investigation.\n")
} 