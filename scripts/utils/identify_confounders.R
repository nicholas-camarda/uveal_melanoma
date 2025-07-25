# Systematic Analysis: Identifying Additional Confounders
# This script identifies variables that differ significantly across treatment groups
# and proposes an updated confounders list for model adjustment

library(tidyverse)
library(gtsummary)

# Source configuration
source("scripts/utils/analysis_config.R")

cat("=== SYSTEMATIC CONFOUNDER IDENTIFICATION ===\n\n")

# Load the full cohort data
data <- readRDS("final_data/analytic dataset/uveal_melanoma_full_cohort.rds")
cat("Loaded full cohort with", nrow(data), "patients\n\n")

# Current confounders from analysis_config.R
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
        cat(sprintf("⚠ Variable '%s' not found in data, skipping\n", var))
        next
    }
    
    var_data <- data[[var]]
    treatment <- data$treatment_group
    
    # Skip if too much missing data
    if (sum(!is.na(var_data)) < 20) {
        cat(sprintf("⚠ Variable '%s' has insufficient data (n=%d), skipping\n", var, sum(!is.na(var_data))))
        next
    }
    
    # Determine test type and run test
    p_val <- NA
    test_type <- NA
    
    tryCatch({
        if (is.numeric(var_data)) {
            # Wilcoxon test for continuous variables
            test_result <- wilcox.test(var_data ~ treatment)
            p_val <- test_result$p.value
            test_type <- "Wilcoxon"
        } else {
            # Fisher's exact test for categorical variables
            table_data <- table(var_data, treatment, useNA = "no")
            if (min(table_data) >= 5) {
                test_result <- chisq.test(table_data)
                p_val <- test_result$p.value
                test_type <- "Chi-square"
            } else {
                test_result <- fisher.test(table_data, simulate.p.value = TRUE)
                p_val <- test_result$p.value
                test_type <- "Fisher"
            }
        }
    }, error = function(e) {
        cat(sprintf("⚠ Error testing '%s': %s\n", var, e$message))
    })
    
    # Store results
    is_significant <- !is.na(p_val) && p_val < 0.05
    results_summary <- rbind(results_summary, data.frame(
        variable = var,
        p_value = p_val,
        test_type = test_type,
        significant = is_significant
    ))
    
    if (is_significant) {
        significant_vars[[var]] <- list(p_value = p_val, test_type = test_type)
        cat(sprintf("✓ %s: p = %.4f (%s) - SIGNIFICANT\n", var, p_val, test_type))
    } else {
        cat(sprintf("  %s: p = %.4f (%s)\n", var, ifelse(is.na(p_val), NA, p_val), test_type))
    }
}

cat("\n=== SUMMARY OF SIGNIFICANT DIFFERENCES ===\n")
sig_results <- results_summary[results_summary$significant == TRUE, ]
if (nrow(sig_results) > 0) {
    cat(sprintf("Found %d variables with significant differences (p < 0.05):\n\n", nrow(sig_results)))
    for (i in 1:nrow(sig_results)) {
        var <- sig_results$variable[i]
        p_val <- sig_results$p_value[i]
        test <- sig_results$test_type[i]
        cat(sprintf("%d. %s (p = %.4f, %s test)\n", i, var, p_val, test))
    }
} else {
    cat("No variables showed significant differences between treatment groups.\n")
}

cat("\n=== CLINICAL CONSIDERATIONS ===\n")

# Analyze specific variables of clinical importance
clinical_vars <- c("initial_overall_stage", "initial_t_stage", "initial_tumor_height", 
                  "initial_tumor_diameter", "optic_nerve", "initial_vision", "biopsy1_gep")

cat("Clinical variables analysis:\n")
for (var in clinical_vars) {
    if (var %in% sig_results$variable) {
        p_val <- sig_results$p_value[sig_results$variable == var]
        cat(sprintf("• %s: DIFFERS between groups (p = %.4f) - CONSIDER FOR ADJUSTMENT\n", var, p_val))
    } else if (var %in% current_confounders) {
        cat(sprintf("• %s: Already in confounders list ✓\n", var))
    } else {
        p_val <- results_summary$p_value[results_summary$variable == var]
        if (!is.na(p_val)) {
            cat(sprintf("• %s: No significant difference (p = %.3f)\n", var, p_val))
        } else {
            cat(sprintf("• %s: Not tested or insufficient data\n", var))
        }
    }
}

cat("\n=== PROPOSED CONFOUNDER UPDATES ===\n")

# Handle overall stage specially (exclude stage 4 due to small numbers)
if ("initial_overall_stage" %in% sig_results$variable) {
    stage_table <- table(data$initial_overall_stage, data$treatment_group, useNA = "no")
    cat("Overall stage distribution:\n")
    print(stage_table)
    
    stage4_count <- sum(data$initial_overall_stage == "Stage IV", na.rm = TRUE)
    cat(sprintf("\nStage IV patients: %d (%.1f%% of cohort)\n", 
                stage4_count, 100 * stage4_count / nrow(data)))
    
    if (stage4_count < 10) {
        cat("→ RECOMMENDATION: Create binary stage variable (Stage I-III vs Stage IV) or exclude Stage IV\n")
    }
}

# Proposed additional confounders
proposed_additions <- character()
for (var in sig_results$variable) {
    if (!var %in% current_confounders) {
        # Skip variables that might cause overadjustment or collinearity
        if (var %in% c("initial_tumor_height", "initial_tumor_diameter")) {
            cat(sprintf("• %s: Significant but may cause overadjustment in tumor height analysis - USE CAREFULLY\n", var))
        } else if (var %in% c("initial_overall_stage", "initial_t_stage")) {
            cat(sprintf("• %s: Significant staging variable - RECOMMEND ADDING\n", var))
            proposed_additions <- c(proposed_additions, var)
        } else if (var == "initial_vision") {
            cat(sprintf("• %s: Significant baseline difference - RECOMMEND ADDING\n", var))
            proposed_additions <- c(proposed_additions, var)
        } else {
            cat(sprintf("• %s: Significant difference - CONSIDER ADDING\n", var))
            proposed_additions <- c(proposed_additions, var)
        }
    }
}

cat("\n=== FINAL RECOMMENDATIONS ===\n")

# Create modified overall stage variable
cat("1. CREATE MODIFIED STAGE VARIABLE:\n")
cat("   initial_stage_binary = ifelse(initial_overall_stage == 'Stage IV', 'Stage IV', 'Stage I-III')\n")
cat("   OR exclude Stage IV patients if n < 10\n\n")

# Updated confounders list
cat("2. UPDATED CONFOUNDERS LIST:\n")
updated_confounders <- c(current_confounders, proposed_additions)
updated_confounders <- unique(updated_confounders)  # Remove duplicates

cat("confounders <- c(\n")
for (i in seq_along(updated_confounders)) {
    comma <- if (i < length(updated_confounders)) "," else ""
    cat(sprintf('    "%s"%s\n', updated_confounders[i], comma))
}
cat(")\n\n")

cat("3. VARIABLES REQUIRING SPECIAL HANDLING:\n")
cat("   • initial_tumor_height/diameter: Use carefully in height change analysis\n")
cat("   • initial_overall_stage: Consider binary version or exclude Stage IV\n")
cat("   • biopsy1_gep: May have missing data patterns\n\n")

cat("=== ANALYSIS COMPLETE ===\n")
cat("Review the recommendations above and update analysis_config.R accordingly.\n") 