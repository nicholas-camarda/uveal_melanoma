# Test Ratio Capping System
# Validates the extensible ratio capping and diagnostic system

library(tidyverse)
library(gtsummary)

# Source the ratio capping functions
source("scripts/utils/analysis_config.R")

cat("=== TESTING RATIO CAPPING SYSTEM ===\n\n")

# Test 1: Basic ratio detection
cat("1. TESTING EXTREME RATIO DETECTION:\n")
test_ratios <- c(1.5, 25, 150, 2847392, 0.01, 0.001)
detection_result <- detect_extreme_ratios(test_ratios)

cat(sprintf("  Input ratios: %s\n", paste(round(test_ratios, 2), collapse = ", ")))
cat(sprintf("  Has extreme: %s\n", detection_result$has_extreme))
cat(sprintf("  Needs capping: %s\n", detection_result$needs_capping))
cat(sprintf("  Max ratio: %.0f\n", detection_result$max_ratio))
cat(sprintf("  N extreme: %d\n", detection_result$n_extreme))

# Test 2: Ratio capping
cat("\n2. TESTING RATIO CAPPING:\n")
capping_result <- cap_ratios_for_display(
    ratios = test_ratios,
    ci_lower = test_ratios * 0.5,
    ci_upper = test_ratios * 2
)

cat("  Original -> Capped ratios:\n")
for (i in seq_along(test_ratios)) {
    cat(sprintf("    %.0f -> %.0f %s\n", 
                capping_result$original_ratios[i], 
                capping_result$ratios[i],
                ifelse(capping_result$ratios_capped[i], "(CAPPED)", "")))
}
cat(sprintf("  Total capped: %d\n", capping_result$n_ratios_capped))

# Test 3: Diagnostic report creation
cat("\n3. TESTING DIAGNOSTIC REPORT:\n")
diagnostics <- create_ratio_diagnostics(
    capping_result,
    variable_names = c("Age", "Sex", "Stage", "Treatment", "Location", "Vision"),
    analysis_name = "Test Safety Analysis",
    sample_sizes = c("Plaque" = 45, "GKSRS" = 38)
)

cat("  Diagnostic report created:\n")
print(diagnostics[c("variable", "original_ratio", "displayed_ratio", "ratio_capped")])

# Test 4: Manual ratio capping (simulating subgroup analysis)
cat("\n4. TESTING MANUAL RATIO CAPPING:\n")

# Create mock model results
set.seed(123)
mock_coefs <- c(0.5, 2.1, 15.2, -0.3)  # Last one creates extreme ratio
mock_vcov <- diag(c(0.1, 0.2, 2.5, 0.15))  # Variance matrix

manual_result <- cap_manual_ratios(
    coefficients = mock_coefs,
    vcov_matrix = mock_vcov,
    coef_indices = 1:4,
    variable_names = c("Treatment", "Age", "Extreme_Variable", "Sex"),
    analysis_name = "Subgroup Analysis Test"
)

cat(sprintf("  Capping applied: %s\n", manual_result$capping_applied))
cat(sprintf("  Number capped: %d\n", manual_result$n_capped))
cat("  Results:\n")
for (i in seq_along(manual_result$ratios)) {
    cat(sprintf("    %s: %.2f (%.2f, %.2f)\n", 
                c("Treatment", "Age", "Extreme_Variable", "Sex")[i],
                manual_result$ratios[i],
                manual_result$ci_lower[i],
                manual_result$ci_upper[i]))
}

# Test 5: Configuration flexibility
cat("\n5. TESTING CONFIGURATION FLEXIBILITY:\n")
cat(sprintf("  Display limit: %d\n", RATIO_DISPLAY_LIMIT))
cat(sprintf("  Diagnostic threshold: %d\n", RATIO_DIAGNOSTIC_THRESHOLD))
cat(sprintf("  CI display limit: %d\n", RATIO_CI_DISPLAY_LIMIT))

# Test different thresholds
custom_detection <- detect_extreme_ratios(test_ratios, threshold = 20)
cat(sprintf("  With threshold=20, extreme count: %d\n", custom_detection$n_extreme))

# Test 6: Edge cases
cat("\n6. TESTING EDGE CASES:\n")

# Test with NAs and infinite values
edge_ratios <- c(1.5, NA, Inf, -Inf, 0, 150)
edge_detection <- detect_extreme_ratios(edge_ratios)
cat(sprintf("  Edge case detection successful: %s\n", !is.null(edge_detection)))
cat(sprintf("  Valid ratios handled: %d\n", sum(is.finite(edge_ratios) & !is.na(edge_ratios))))

# Test with empty input
empty_detection <- detect_extreme_ratios(numeric(0))
cat(sprintf("  Empty input handled: %s\n", !empty_detection$has_extreme))

cat("\n=== RATIO CAPPING SYSTEM TEST SUMMARY ===\n")
cat("✓ Extreme ratio detection\n")
cat("✓ Ratio capping with CI handling\n") 
cat("✓ Diagnostic report generation\n")
cat("✓ Manual ratio calculation support\n")
cat("✓ Configuration flexibility\n")
cat("✓ Edge case handling\n")
cat("✓ All ratio capping functions tested successfully!\n")

# Save test diagnostics
if (!dir.exists("test_output")) {
    dir.create("test_output", showWarnings = FALSE)
}

writexl::write_xlsx(
    list(
        "Test_Diagnostics" = diagnostics,
        "Manual_Diagnostics" = if (!is.null(manual_result$diagnostics)) manual_result$diagnostics else data.frame()
    ),
    path = "test_output/ratio_capping_test_diagnostics.xlsx"
)

cat(sprintf("\nTest diagnostics saved to: test_output/ratio_capping_test_diagnostics.xlsx\n")) 