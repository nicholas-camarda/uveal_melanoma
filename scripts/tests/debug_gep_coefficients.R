# Debug GEP Coefficient Names
# Investigate why GEP subgroups have identical CI values

cat("=== DEBUGGING GEP COEFFICIENT NAMES ===\n")

# Load required libraries
library(dplyr)

# Source required functions
source("scripts/utils/analysis_config.R")
source("scripts/data_helper/data_utilities.R")
source("scripts/analysis/subgroup_analysis.R")

# Load a small sample of data for testing
data_file <- "final_data/processed_data/uveal_full_analytic_dataset.rds"
if (file.exists(data_file)) {
    data <- readRDS(data_file)
    cat("✓ Data loaded:", nrow(data), "rows\n")
} else {
    # Try alternative path
    data_files <- list.files("data", pattern = "*.rds", full.names = TRUE, recursive = TRUE)
    if (length(data_files) > 0) {
        data <- readRDS(data_files[1])
        cat("✓ Data loaded from:", data_files[1], "- rows:", nrow(data), "\n")
    } else {
        stop("No data file found")
    }
}

# Check GEP variable
if (!"biopsy1_gep" %in% names(data)) {
    stop("biopsy1_gep variable not found in data")
}

# Calculate height change
if (!("height_change" %in% names(data))) {
    data <- data %>%
        mutate(
            height_change = case_when(
                recurrence1 == "Y" ~ initial_tumor_height - recurrence1_pretreatment_height,
                TRUE ~ initial_tumor_height - last_height
            )
        )
}

# Filter to only cases with GEP data
gep_data <- data %>% filter(!is.na(biopsy1_gep))
cat("GEP data available for", nrow(gep_data), "patients\n")

# Check GEP levels
gep_levels <- levels(gep_data$biopsy1_gep)
cat("GEP levels:", paste(gep_levels, collapse = ", "), "\n")

# Fit a manual model to see coefficient names
cat("\n=== FITTING MANUAL MODEL ===\n")
formula_str <- "height_change ~ treatment_group * biopsy1_gep + age_at_diagnosis + sex + location"
model <- lm(as.formula(formula_str), data = gep_data)

cat("Model coefficients:\n")
coef_names <- names(coef(model))
for (i in seq_along(coef_names)) {
    cat(sprintf("  %d: %s = %.6f\n", i, coef_names[i], coef(model)[i]))
}

cat("\nInteraction coefficients (containing 'biopsy1_gep'):\n")
interaction_coefs <- coef_names[grepl("biopsy1_gep", coef_names)]
for (coef_name in interaction_coefs) {
    cat(sprintf("  %s = %.6f\n", coef_name, coef(model)[coef_name]))
}

# Test the get_interaction_coefficient_name function for each GEP level
cat("\n=== TESTING get_interaction_coefficient_name ===\n")
for (level in gep_levels) {
    result <- get_interaction_coefficient_name(
        model = model,
        treatment_var = "treatment_group", 
        subgroup_var = "biopsy1_gep",
        subgroup_level = level,
        data = gep_data
    )
    cat(sprintf("Level '%s': %s\n", level, ifelse(is.null(result), "NULL", result)))
}

# Now test the full subgroup effects calculation
cat("\n=== TESTING SUBGROUP EFFECTS CALCULATION ===\n")
subgroup_effects <- calculate_subgroup_effects(
    model = model,
    data = gep_data,
    subgroup_var_to_use = "biopsy1_gep",
    outcome_type = "continuous",
    original_var_name = "biopsy1_gep"
)

if (nrow(subgroup_effects) > 0) {
    cat("Subgroup effects calculated:\n")
    for (i in 1:nrow(subgroup_effects)) {
        row <- subgroup_effects[i,]
        cat(sprintf("  %s: Effect=%.6f, CI=(%.6f, %.6f), p=%.6f\n", 
                   row$subgroup_level, row$treatment_effect, 
                   row$ci_lower, row$ci_upper, row$p_value))
    }
    
    # Check for identical values
    unique_effects <- unique(subgroup_effects$treatment_effect)
    unique_ci_lower <- unique(subgroup_effects$ci_lower)
    unique_ci_upper <- unique(subgroup_effects$ci_upper)
    
    cat(sprintf("\nUnique effect estimates: %d (out of %d)\n", length(unique_effects), nrow(subgroup_effects)))
    cat(sprintf("Unique CI lower bounds: %d (out of %d)\n", length(unique_ci_lower), nrow(subgroup_effects)))
    cat(sprintf("Unique CI upper bounds: %d (out of %d)\n", length(unique_ci_upper), nrow(subgroup_effects)))
    
    if (length(unique_effects) < nrow(subgroup_effects)) {
        cat("🚨 IDENTICAL EFFECT ESTIMATES DETECTED!\n")
    }
    
    if (length(unique_ci_lower) < nrow(subgroup_effects)) {
        cat("🚨 IDENTICAL CI LOWER BOUNDS DETECTED!\n")
    }
    
    if (length(unique_ci_upper) < nrow(subgroup_effects)) {
        cat("🚨 IDENTICAL CI UPPER BOUNDS DETECTED!\n")
    }
} else {
    cat("No subgroup effects calculated\n")
}

cat("\n=== DEBUG COMPLETE ===\n") 