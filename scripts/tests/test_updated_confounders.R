# Test Updated Confounders Implementation
# This script validates that the new confounders list works properly

library(tidyverse)
library(survival)

# Set required variables
VERBOSE <- TRUE

# Source required files
source("scripts/utils/analysis_config.R")
source("scripts/data_helper/data_processing.R")
source("scripts/data_helper/data_utilities.R")

cat("=== TESTING UPDATED CONFOUNDERS IMPLEMENTATION ===\n\n")

# Load data
data <- readRDS("final_data/analytic dataset/uveal_melanoma_full_cohort.rds")
cat("Loaded", nrow(data), "patients\n\n")

# Test 1: Check that new variables exist
cat("1. CHECKING NEW VARIABLES EXIST:\n")
new_vars <- c("initial_stage_binary", "internal_reflectivity", "srf", "initial_vision")
for (var in new_vars) {
    if (var %in% names(data)) {
        cat(sprintf("✓ %s: exists\n", var))
        if (is.factor(data[[var]])) {
            cat(sprintf("  Levels: %s\n", paste(levels(data[[var]]), collapse = ", ")))
        }
        cat(sprintf("  Missing: %d/%d (%.1f%%)\n", sum(is.na(data[[var]])), nrow(data), 
                   100 * sum(is.na(data[[var]])) / nrow(data)))
    } else {
        cat(sprintf("✗ %s: MISSING\n", var))
    }
}

# Test 2: Check stage distribution
cat("\n2. STAGE DISTRIBUTION ANALYSIS:\n")
if ("initial_stage_binary" %in% names(data)) {
    stage_table <- table(data$initial_stage_binary, data$treatment_group, useNA = "ifany")
    cat("Binary stage by treatment:\n")
    print(stage_table)
    
    original_stage_table <- table(data$initial_overall_stage, data$treatment_group, useNA = "ifany")
    cat("\nOriginal stage distribution:\n")
    print(original_stage_table)
} else {
    cat("initial_stage_binary not found in data\n")
}

# Test 3: Test confounders validation
cat("\n3. TESTING CONFOUNDERS VALIDATION:\n")
cat("Standard confounders:\n")
for (i in seq_along(confounders)) {
    var <- confounders[i]
    if (var %in% names(data)) {
        cat(sprintf("✓ %d. %s\n", i, var))
    } else {
        cat(sprintf("✗ %d. %s - NOT FOUND\n", i, var))
    }
}

# Test 4: Test generate_valid_confounders function
cat("\n4. TESTING CONFOUNDER VALIDATION FUNCTION:\n")
tryCatch({
    valid_confounders <- generate_valid_confounders(data, confounders, threshold = 5)
    cat(sprintf("Valid confounders (%d/%d):\n", length(valid_confounders), length(confounders)))
    for (i in seq_along(valid_confounders)) {
        cat(sprintf("  %d. %s\n", i, valid_confounders[i]))
    }
    
    removed_confounders <- setdiff(confounders, valid_confounders)
    if (length(removed_confounders) > 0) {
        cat(sprintf("\nRemoved confounders (%d):\n", length(removed_confounders)))
        for (var in removed_confounders) {
            cat(sprintf("  - %s\n", var))
        }
    }
}, error = function(e) {
    cat(sprintf("Error in confounder validation: %s\n", e$message))
})

# Test 5: Test specialized confounders
cat("\n5. TESTING SPECIALIZED CONFOUNDERS:\n")
cat("Height analysis confounders:\n")
for (i in seq_along(confounders_height_analysis)) {
    var <- confounders_height_analysis[i]
    if (var %in% names(data)) {
        cat(sprintf("✓ %d. %s\n", i, var))
    } else {
        cat(sprintf("✗ %d. %s - NOT FOUND\n", i, var))
    }
}

cat("\nVision analysis confounders:\n")
for (i in seq_along(confounders_vision_analysis)) {
    var <- confounders_vision_analysis[i]
    if (var %in% names(data)) {
        cat(sprintf("✓ %d. %s\n", i, var))
    } else {
        cat(sprintf("✗ %d. %s - NOT FOUND\n", i, var))
    }
}

# Test 6: Test model fitting with new confounders
cat("\n6. TESTING MODEL FITTING:\n")
tryCatch({
    # Test logistic regression
    cat("Testing logistic regression with new confounders...\n")
    valid_confounders <- generate_valid_confounders(data, confounders, threshold = 5)
    
    if (length(valid_confounders) > 0) {
        formula_str <- paste("recurrence1 ~ treatment_group +", paste(valid_confounders, collapse = " + "))
        model <- glm(as.formula(formula_str), data = data, family = binomial())
        cat(sprintf("✓ Model fitted successfully with %d confounders\n", length(valid_confounders)))
        cat(sprintf("  Formula: %s\n", formula_str))
        cat(sprintf("  Coefficients: %d\n", length(coef(model))))
    } else {
        cat("✗ No valid confounders for model fitting\n")
    }
}, error = function(e) {
    cat(sprintf("✗ Model fitting failed: %s\n", e$message))
})

cat("\n=== TESTING COMPLETE ===\n")
cat("Review results above to ensure all new confounders are working properly.\n") 