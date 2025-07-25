# Examine GEP Factor Levels in Analytic Dataset
# Check if factor levels are properly set and identify coefficient naming issues

cat("=== EXAMINING GEP FACTOR LEVELS ===\n")

# Load data
data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

cat("GEP variable structure:\n")
cat("Is factor:", is.factor(data$biopsy1_gep), "\n")
cat("Number of levels:", nlevels(data$biopsy1_gep), "\n")
cat("Reference level:", levels(data$biopsy1_gep)[1], "\n")

cat("\nAll GEP levels:\n")
gep_levels <- levels(data$biopsy1_gep)
for (i in seq_along(gep_levels)) {
    cat(sprintf("  %d: \"%s\"\n", i, gep_levels[i]))
}

cat("\nProblematic level names (contain spaces/special characters):\n")
problematic <- gep_levels[grepl("[^A-Za-z0-9_]", gep_levels)]
if (length(problematic) > 0) {
    for (level in problematic) {
        cat(sprintf("  \"%s\"\n", level))
    }
    cat(sprintf("\n🚨 PROBLEM: %d out of %d factor levels have problematic names!\n", length(problematic), length(gep_levels)))
} else {
    cat("✅ All factor level names are clean\n")
}

# Test how R transforms these names in model coefficients
cat("\n=== TESTING MODEL COEFFICIENT TRANSFORMATION ===\n")
gep_data <- data[!is.na(data$biopsy1_gep), ]
if (nrow(gep_data) > 20) {
    # Create a simple model to see coefficient names
    test_model <- lm(age_at_diagnosis ~ biopsy1_gep, data = gep_data[1:min(50, nrow(gep_data)), ])
    coef_names <- names(coef(test_model))
    gep_coefs <- coef_names[grepl("biopsy1_gep", coef_names)]
    
    cat("R transforms factor levels into these coefficient names:\n")
    for (coef in gep_coefs) {
        cat(sprintf("  \"%s\"\n", coef))
    }
    
    cat("\nOriginal levels vs R coefficient names:\n")
    for (i in 2:length(gep_levels)) {  # Skip reference level
        original <- gep_levels[i]
        expected_coef <- paste0("biopsy1_gep", original)
        actual_coef <- gep_coefs[grepl(gsub("[^A-Za-z0-9_]", ".", original), gep_coefs)]
        
        cat(sprintf("  Original: \"%s\"\n", original))
        cat(sprintf("  Expected: \"%s\"\n", expected_coef))
        cat(sprintf("  Actual:   \"%s\"\n", if(length(actual_coef) > 0) actual_coef[1] else "NOT FOUND"))
        cat("\n")
    }
}

cat("=== RECOMMENDATION ===\n")
if (length(problematic) > 0) {
    cat("🚨 ROOT CAUSE: Factor levels should be cleaned during analytic dataset creation!\n")
    cat("   Current complex matching logic is a workaround for improper data processing.\n")
    cat("   SOLUTION: Fix factor levels in the data processing step, not coefficient matching.\n")
} else {
    cat("✅ Factor levels are properly formatted - investigate other causes.\n")
} 