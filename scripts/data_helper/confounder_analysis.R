# Confounder Analysis and Update Utility
# This script identifies variables that differ across treatment groups and updates the confounders list

library(tidyverse)

# Source configuration
# No need to source all_helper_functions.R - it will be sourced by the calling script

cat("=== CONFOUNDER ANALYSIS AND UPDATE ===\n\n")

# Load the full cohort data
data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
cat("Loaded full cohort with", nrow(data), "patients\n\n")

# Check current confounders
current_confounders <- confounders
cat("CURRENT CONFOUNDERS:\n")
for (i in seq_along(current_confounders)) {
    cat(sprintf("  %d. %s\n", i, current_confounders[i]))
}
cat("\n")

# Variables to test for differences (baseline characteristics)
test_variables <- BASELINE_VARIABLES_TO_SUMMARIZE[BASELINE_VARIABLES_TO_SUMMARIZE != "treatment_group"]

cat("TESTING", length(test_variables), "VARIABLES FOR TREATMENT GROUP DIFFERENCES:\n\n")

# Test each variable for differences between treatment groups
significant_vars <- list()
results_summary <- data.frame(
    variable = character(),
    p_value = numeric(),
    test_type = character(),
    significant = logical(),
    stringsAsFactors = FALSE
)

for (var in test_variables) {
    if (!var %in% names(data)) {
        cat(sprintf("SKIP: %s (not in data)\n", var))
        next
    }
    
    # Skip if already in confounders
    if (var %in% current_confounders) {
        cat(sprintf("SKIP: %s (already in confounders)\n", var))
        next
    }
    
    # Check if variable has sufficient variation
    if (is.numeric(data[[var]])) {
        # Continuous variable - t-test
        tryCatch({
            test_result <- t.test(data[[var]] ~ data$treatment_group)
            p_val <- test_result$p.value
            test_type <- "t-test"
        }, error = function(e) {
            p_val <- NA
            test_type <- "t-test (failed)"
        })
    } else {
        # Categorical variable - chi-square or fisher
        tryCatch({
            # Check if chi-square is appropriate
            expected_counts <- chisq.test(table(data[[var]], data$treatment_group))$expected
            if (any(expected_counts < 5)) {
                # Use Fisher's exact test for small expected counts
                test_result <- fisher.test(table(data[[var]], data$treatment_group), simulate.p.value = TRUE)
                test_type <- "Fisher's exact"
            } else {
                test_result <- chisq.test(table(data[[var]], data$treatment_group))
                test_type <- "Chi-square"
            }
            p_val <- test_result$p.value
        }, error = function(e) {
            p_val <- NA
            test_type <- "categorical test (failed)"
        })
    }
    
    significant <- !is.na(p_val) && p_val < 0.05
    
    # Store results
    results_summary <- rbind(results_summary, data.frame(
        variable = var,
        p_value = p_val,
        test_type = test_type,
        significant = significant,
        stringsAsFactors = FALSE
    ))
    
    # Print results
    if (significant) {
        cat(sprintf("SIGNIFICANT: %s (p=%.4f, %s)\n", var, p_val, test_type))
        significant_vars[[var]] <- list(p_value = p_val, test_type = test_type)
    } else {
        cat(sprintf("NS: %s (p=%.4f, %s)\n", var, p_val, test_type))
    }
}

cat("\n=== SUMMARY ===\n")
cat(sprintf("Found %d variables with significant differences (p<0.05):\n", length(significant_vars)))

if (length(significant_vars) > 0) {
    cat("\nSIGNIFICANT VARIABLES:\n")
    for (i in seq_along(significant_vars)) {
        var_name <- names(significant_vars)[i]
        p_val <- significant_vars[[var_name]]$p_value
        test_type <- significant_vars[[var_name]]$test_type
        cat(sprintf("  %d. %s (p=%.4f, %s)\n", i, var_name, p_val, test_type))
    }
    
    # Filter out variables that might be problematic
    problematic_vars <- c(
        "initial_mets",  # Too rare
        "initial_m_stage",  # Too rare
        "initial_n_stage",   # Too rare
        "initial_tumor_height"  # May cause overadjustment in height change analysis
    )
    
    recommended_vars <- names(significant_vars)[!names(significant_vars) %in% problematic_vars]
    
    cat("\nRECOMMENDED CONFOUNDERS TO ADD:\n")
    for (i in seq_along(recommended_vars)) {
        cat(sprintf("  %d. %s\n", i, recommended_vars[i]))
    }
    
    # Special handling for overall stage
    cat("\n=== STAGE ANALYSIS ===\n")
    if ("initial_overall_stage" %in% names(significant_vars)) {
        stage_table <- table(data$initial_overall_stage, data$treatment_group, useNA = "no")
        cat("Overall stage distribution:\n")
        print(stage_table)
        
        stage4_count <- sum(data$initial_overall_stage == "4", na.rm = TRUE)
        cat(sprintf("\nStage IV patients: %d (%.1f%% of cohort)\n", 
                    stage4_count, 100 * stage4_count / nrow(data)))
        
        if (stage4_count < 10) {
            cat("→ Stage IV has low numbers - consider binary stage variable or exclude\n")
        }
    }
    
    # Create updated confounders list
    updated_confounders <- c(current_confounders, recommended_vars)
    updated_confounders <- unique(updated_confounders)  # Remove duplicates
    
    cat("\n=== UPDATED CONFOUNDERS LIST ===\n")
    for (i in seq_along(updated_confounders)) {
        cat(sprintf("  %d. %s\n", i, updated_confounders[i]))
    }
    
    # Generate R code for updating config_constants.R
cat("\n=== R CODE TO UPDATE config_constants.R ===\n")
cat("# Replace the confounders line in config_constants.R with:\n")
    cat("confounders <- c(\n")
    for (i in seq_along(updated_confounders)) {
        comma <- if (i < length(updated_confounders)) "," else ""
        cat(sprintf('    "%s"%s\n', updated_confounders[i], comma))
    }
    cat(")\n\n")
    
    # Save detailed results to analytic dataset folder
    write.csv(results_summary, "final_data/Analytic Dataset/confounder_analysis_results.csv", row.names = FALSE)
    cat("Detailed results saved to: final_data/Analytic Dataset/confounder_analysis_results.csv\n")
    
} else {
    cat("No additional variables found with significant differences.\n")
}

cat("\n=== RECOMMENDATIONS ===\n")
cat("1. Review the significant variables above\n")
cat("2. Consider clinical relevance when adding confounders\n")
cat("3. Test models with new confounders to ensure convergence\n")
cat("4. Consider analysis-specific confounder lists for different outcomes\n\n")

cat("=== ANALYSIS COMPLETE ===\n") 