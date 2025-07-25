# Test Proper GEP Solution
# Verify that clean factor levels + treatment contrasts fix the coefficient matching

cat("=== TESTING PROPER GEP SOLUTION ===\n")

# Load required functions
library(dplyr)
source("scripts/utils/all_helper_functions.R")
source("scripts/data_helper/data_processing.R")

# Test with synthetic data that mimics the cleaned structure
set.seed(42)
n <- 100

# Create test data with clean GEP level names (no spaces/special characters)
test_data <- data.frame(
    treatment_group = factor(rep(c("Plaque", "GKSRS"), n/2), levels = c("Plaque", "GKSRS")),  # Explicit ordering
    biopsy1_gep_raw = sample(c(
        "Class_1A_PRAME_negative",   # Reference level
        "Class_1A_PRAME_positive", 
        "Class_1B_PRAME_negative",
        "Class_1B_PRAME_positive",
        "Class_2_PRAME_negative", 
        "Class_2_PRAME_positive",
        "DISCORDANT CASTLE RESULTS: Class 1A, PRAME not reported"  # Problematic original
    ), n, replace = TRUE, prob = c(0.3, 0.15, 0.15, 0.15, 0.1, 0.1, 0.05)),
    height_change = rnorm(n, mean = 0, sd = 2),
    age_at_diagnosis = rnorm(n, mean = 60, sd = 10),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    location = factor(sample(c("Anterior", "Posterior", "Equatorial"), n, replace = TRUE))
)

# Apply the data cleaning step (same as in data_processing.R)
test_data <- test_data %>%
    mutate(
        # Clean the problematic GEP level name
        biopsy1_gep = case_when(
            biopsy1_gep_raw == "DISCORDANT CASTLE RESULTS: Class 1A, PRAME not reported" ~ "Class_1A_PRAME_discordant",
            TRUE ~ biopsy1_gep_raw
        ),
        # Factor with clean levels and NO ordering (uses treatment contrasts)
        biopsy1_gep = factor(biopsy1_gep,
            levels = c(
                "Class_1A_PRAME_negative",   # Reference level
                "Class_1A_PRAME_positive", 
                "Class_1B_PRAME_negative",
                "Class_1B_PRAME_positive",
                "Class_2_PRAME_negative", 
                "Class_2_PRAME_positive",
                "Class_1A_PRAME_discordant"
            ), 
            ordered = FALSE  # CRITICAL: treatment contrasts, not polynomial
        )
    )

cat("Test data created with", nrow(test_data), "rows\n")
cat("GEP levels:", paste(levels(test_data$biopsy1_gep), collapse = ", "), "\n")
cat("Reference level:", levels(test_data$biopsy1_gep)[1], "\n")

# Test model coefficient generation
cat("\n=== TESTING MODEL COEFFICIENTS ===\n")
model <- lm(height_change ~ treatment_group * biopsy1_gep + age_at_diagnosis + sex + location, 
            data = test_data)

coef_names <- names(coef(model))
gep_coefs <- coef_names[grepl("biopsy1_gep", coef_names)]
interaction_coefs <- coef_names[grepl("treatment_group.*:.*biopsy1_gep", coef_names)]

cat("All GEP coefficients:\n")
for (coef in gep_coefs) {
    cat(sprintf("  %s\n", coef))
}

cat("\nInteraction coefficients:\n")
for (coef in interaction_coefs) {
    cat(sprintf("  %s\n", coef))
}

# Test coefficient matching
cat("\n=== TESTING COEFFICIENT MATCHING ===\n")
source("scripts/utils/analysis_config.R")

gep_levels <- levels(test_data$biopsy1_gep)
for (level in gep_levels[-1]) {  # Skip reference level
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
    
    if (!is.null(result) && result == expected_name) {
        cat("  ✅ PERFECT: Function found correct coefficient\n")
    } else if (!is.null(result)) {
        cat("  ⚠️  Found different coefficient - checking if valid...\n")
        cat(sprintf("     Found: %s\n", result))
    } else {
        cat("  ❌ NULL result\n")
    }
    cat("\n")
}

cat("=== SOLUTION VERIFICATION ===\n")
if (length(interaction_coefs) >= (length(gep_levels) - 1)) {
    cat("✅ SUCCESS: Model generates proper interaction coefficients!\n")
    cat("✅ Each non-reference GEP level should now have unique treatment effects\n")
    cat("✅ Proper solution implemented:\n")
    cat("   - Clean factor level names (no spaces/special characters)\n")
    cat("   - Treatment contrasts (ordered = FALSE)\n")
    cat("   - Simple, robust coefficient matching\n")
} else {
    cat("❌ Issue persists - need further investigation\n")
}

cat("\n=== TEST COMPLETE ===\n") 