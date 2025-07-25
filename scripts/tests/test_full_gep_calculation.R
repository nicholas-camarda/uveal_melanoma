# Test Full GEP Calculation with Improved Coefficient Matching
# Verify that each GEP subgroup gets unique treatment effects

cat("=== TESTING FULL GEP SUBGROUP CALCULATION ===\n")

# Load required libraries and functions
library(dplyr)
source("scripts/utils/all_helper_functions.R")  # CRITICAL: Source all helper functions first
source("scripts/utils/analysis_config.R")
source("scripts/analysis/subgroup_analysis.R")

# Load real data to check factor levels
cat("=== CHECKING REAL DATA FACTOR LEVELS ===\n")

# Try to find and load the real dataset
data_paths <- c(
    "final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds"
)

data_loaded <- FALSE
for (path in data_paths) {
    if (file.exists(path)) {
        real_data <- readRDS(path)
        cat("✓ Real data loaded from:", path, "- rows:", nrow(real_data), "\n")
        data_loaded <- TRUE
        break
    }
}

if (!data_loaded) {
    # Find any RDS files that might contain the data
    rds_files <- system("find . -name '*.rds' -type f | head -5", intern = TRUE)
    cat("Available RDS files:\n")
    for (file in rds_files) {
        cat(" ", file, "\n")
    }
    stop("Could not find analytic dataset")
}

# Check GEP factor levels in real data
if ("biopsy1_gep" %in% names(real_data)) {
    cat("\n📊 REAL DATA GEP ANALYSIS:\n")
    gep_levels <- levels(real_data$biopsy1_gep)
    cat("GEP factor levels:", paste(gep_levels, collapse = ", "), "\n")
    cat("Reference level:", gep_levels[1], "\n")
    
    # Check actual values in data
    gep_values <- table(real_data$biopsy1_gep, useNA = "ifany")
    cat("\nGEP value counts:\n")
    print(gep_values)
    
    # Test with a small subset of real data
    gep_subset <- real_data %>% 
        filter(!is.na(biopsy1_gep)) %>%
        head(100)
    
    cat(sprintf("\nTesting with %d rows of real GEP data\n", nrow(gep_subset)))
    
    # Test the actual subgroup analysis function
    cat("\n=== TESTING REAL GEP SUBGROUP ANALYSIS ===\n")
    result <- analyze_treatment_effect_subgroups_height(
        data = gep_subset,
        subgroup_var = "biopsy1_gep",
        confounders = c("age_at_diagnosis", "sex", "location"),
        include_baseline_height = FALSE
    )
    
    # Check results
    if (!is.null(result$interaction_p) && !is.na(result$interaction_p)) {
        cat(sprintf("✅ Interaction p-value calculated: %.4f\n", result$interaction_p))
    } else {
        cat("❌ Interaction p-value missing\n")
        if (!is.null(result$interaction_diagnostics)) {
            cat("Failure reason:", result$interaction_diagnostics$failure_reason, "\n")
        }
    }
    
    if (!is.null(result$subgroup_effects) && nrow(result$subgroup_effects) > 0) {
        cat(sprintf("✅ Subgroup effects calculated for %d levels\n", nrow(result$subgroup_effects)))
        
        effects_df <- result$subgroup_effects
        cat("\nSubgroup effects:\n")
        for (i in 1:nrow(effects_df)) {
            row <- effects_df[i,]
            cat(sprintf("  %s: Effect=%.4f, CI=(%.4f, %.4f), p=%.4f\n", 
                       row$subgroup_level, row$treatment_effect, 
                       row$ci_lower, row$ci_upper, row$p_value))
        }
        
        # Check for uniqueness  
        unique_effects <- length(unique(effects_df$treatment_effect[!is.na(effects_df$treatment_effect)]))
        total_effects <- sum(!is.na(effects_df$treatment_effect))
        
        cat(sprintf("\n📊 UNIQUENESS CHECK:\n"))
        cat(sprintf("  Unique effect estimates: %d out of %d non-NA values\n", unique_effects, total_effects))
        
        if (unique_effects >= (total_effects - 1)) {
            cat("🎉 SUCCESS: GEP coefficient matching bug FIXED!\n")
        } else {
            cat("🚨 PROBLEM: Some subgroups still have identical effect estimates\n")
            
            # Show which ones are identical
            effect_counts <- table(round(effects_df$treatment_effect, 6))
            duplicates <- effect_counts[effect_counts > 1]
            if (length(duplicates) > 0) {
                cat("Identical effect values:\n")
                for (val in names(duplicates)) {
                    matching_rows <- which(round(effects_df$treatment_effect, 6) == as.numeric(val))
                    levels_with_val <- effects_df$subgroup_level[matching_rows]
                    cat(sprintf("  %.6f: %s\n", as.numeric(val), paste(levels_with_val, collapse = ", ")))
                }
            }
        }
        
    } else {
        cat("❌ No subgroup effects calculated\n")
    }
    
} else {
    cat("❌ biopsy1_gep variable not found in real data\n")
    cat("Available variables:", paste(names(real_data)[1:10], collapse = ", "), "...\n")
}

cat("\n=== TEST COMPLETE ===\n") 