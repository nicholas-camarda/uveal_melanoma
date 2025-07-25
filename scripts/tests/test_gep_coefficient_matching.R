# Test GEP Coefficient Matching
# Create a simple test to see what coefficient names are generated

cat("=== TESTING GEP COEFFICIENT MATCHING ===\n")

# Load required libraries
library(dplyr)

# Create synthetic test data that matches the structure
set.seed(42)
n <- 100

# Create test data with GEP levels matching the real data
test_data <- data.frame(
    treatment_group = factor(rep(c("Plaque", "GKSRS"), n/2)),
    biopsy1_gep = factor(sample(c(
        "Class_1A_PRAME_negative",
        "Class_1A_PRAME_positive", 
        "Class_1B_PRAME_negative",
        "Class_1B_PRAME_positive",
        "Class_2_PRAME_negative", 
        "Class_2_PRAME_positive",
        "Failed",
        "Other"
    ), n, replace = TRUE)),
    height_change = rnorm(n, mean = 0, sd = 2),
    age_at_diagnosis = rnorm(n, mean = 60, sd = 10),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    location = factor(sample(c("Anterior", "Posterior", "Equatorial"), n, replace = TRUE))
)

cat("Test data created with", nrow(test_data), "rows\n")
cat("GEP levels:", paste(levels(test_data$biopsy1_gep), collapse = ", "), "\n")

# Fit interaction model
cat("\n=== FITTING INTERACTION MODEL ===\n")
formula_str <- "height_change ~ treatment_group * biopsy1_gep + age_at_diagnosis + sex + location"
model <- lm(as.formula(formula_str), data = test_data)

# Show all coefficient names
cat("All model coefficients:\n")
coef_names <- names(coef(model))
for (i in seq_along(coef_names)) {
    cat(sprintf("  %2d: %s\n", i, coef_names[i]))
}

# Show interaction coefficients specifically
cat("\nInteraction coefficients (containing both 'treatment_group' and 'biopsy1_gep'):\n")
interaction_coefs <- coef_names[grepl("treatment_group.*biopsy1_gep|biopsy1_gep.*treatment_group", coef_names)]
for (i in seq_along(interaction_coefs)) {
    cat(sprintf("  %d: %s\n", i, interaction_coefs[i]))
}

# Source the current function
source("scripts/utils/analysis_config.R")

# Test the current function for each GEP level
cat("\n=== TESTING CURRENT get_interaction_coefficient_name FUNCTION ===\n")
gep_levels <- levels(test_data$biopsy1_gep)
for (level in gep_levels) {
    result <- get_interaction_coefficient_name(
        model = model,
        treatment_var = "treatment_group", 
        subgroup_var = "biopsy1_gep",
        subgroup_level = level,
        data = test_data
    )
    expected_name <- paste0("treatment_groupGKSRS:biopsy1_gep", level)
    found_match <- expected_name %in% coef_names
    
    cat(sprintf("Level '%s':\n", level))
    cat(sprintf("  Function result: %s\n", ifelse(is.null(result), "NULL", result)))
    cat(sprintf("  Expected name: %s\n", expected_name))
    cat(sprintf("  Expected exists: %s\n", found_match))
    
    if (is.null(result) && found_match) {
        cat("  🚨 BUG: Function returned NULL but expected coefficient exists!\n")
    } else if (!is.null(result) && result == expected_name) {
        cat("  ✅ WORKING: Function found correct coefficient\n")
    } else if (!is.null(result)) {
        cat("  ⚠️  MISMATCH: Function found different coefficient\n")
    }
    cat("\n")
}

cat("=== ANALYSIS COMPLETE ===\n") 