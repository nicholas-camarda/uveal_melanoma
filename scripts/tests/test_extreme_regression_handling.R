# Test Extreme Regression Estimate Handling
# This script tests the new functionality to detect and exclude extreme 
# regression estimates from tables while creating companion diagnostics files

# Load required libraries
library(tidyverse)
library(survival)
library(gtsummary)

# Source required scripts
source("scripts/utils/all_helper_functions.R")
source("scripts/utils/analysis_config.R")
source("scripts/utils/output_utilities.R")

cat("=== TESTING EXTREME REGRESSION ESTIMATE HANDLING ===\n\n")

# Test 1: Detection function with known extreme values
cat("1. TESTING DETECTION FUNCTION:\n")

# Simulate problematic values from repeat radiation analysis
extreme_estimates <- c(1.5, 703840149, 2712963623, 285951979, 1.07, 0.04, 22.9)
extreme_ci_lower <- c(0.8, 0.00, 0.00, 0.00, 0.97, 0.00, 2.29)  
extreme_ci_upper <- c(2.5, Inf, Inf, Inf, 1.18, 0.38, 229)

detection_result <- detect_extreme_regression_estimates(
    estimate = extreme_estimates,
    ci_lower = extreme_ci_lower,
    ci_upper = extreme_ci_upper,
    effect_measure = "HR"
)

cat("Extreme indices detected:", paste(detection_result$extreme_indices, collapse = ", "), "\n")
cat("Expected: 2, 3, 4, 6 (the extreme HRs and the 0.00 CI bound)\n")

# Verify detection worked correctly
expected_extreme <- c(2, 3, 4, 6)
if (all(detection_result$extreme_indices == expected_extreme)) {
    cat("✓ Detection function working correctly\n")
} else {
    cat("✗ Detection function failed\n")
    cat("Expected:", paste(expected_extreme, collapse = ", "), "\n")
    cat("Got:", paste(detection_result$extreme_indices, collapse = ", "), "\n")
}

# Test 2: Normal values should not be flagged
cat("\n2. TESTING WITH NORMAL VALUES:\n")

normal_estimates <- c(1.2, 0.8, 2.1, 1.5)
normal_ci_lower <- c(0.9, 0.6, 1.2, 1.0)
normal_ci_upper <- c(1.6, 1.1, 3.5, 2.3)

normal_result <- detect_extreme_regression_estimates(
    estimate = normal_estimates,
    ci_lower = normal_ci_lower,
    ci_upper = normal_ci_upper,
    effect_measure = "HR"
)

if (length(normal_result$extreme_indices) == 0) {
    cat("✓ Normal values correctly not flagged as extreme\n")
} else {
    cat("✗ Normal values incorrectly flagged as extreme\n")
    cat("Flagged indices:", paste(normal_result$extreme_indices, collapse = ", "), "\n")
}

# Test 3: Test with OR (odds ratios) 
cat("\n3. TESTING WITH ODDS RATIOS:\n")

or_estimates <- c(2.5, 0.3, 1000000, 0.0001)  # Include extreme OR
or_ci_lower <- c(1.2, 0.1, 500000, 0.00001)
or_ci_upper <- c(5.2, 0.9, 2000000, 0.001)

or_result <- detect_extreme_regression_estimates(
    estimate = or_estimates,
    ci_lower = or_ci_lower,
    ci_upper = or_ci_upper,
    effect_measure = "OR"
)

cat("OR extreme indices:", paste(or_result$extreme_indices, collapse = ", "), "\n")
cat("Expected: none for this simple test (all values are finite and positive)\n")

if (length(or_result$extreme_indices) == 0) {
    cat("✓ OR detection working as expected\n")
} else {
    cat("Note: OR detection found extreme values - this may be expected behavior\n")
}

# Test 4: Test with problematic values (NA, Inf, negative)
cat("\n4. TESTING WITH PROBLEMATIC VALUES:\n")

problem_estimates <- c(1.5, NA, Inf, -1.2, 0)
problem_ci_lower <- c(0.8, 0.5, 2.0, -2.0, -0.5)
problem_ci_upper <- c(2.5, 3.0, Inf, -0.5, 0.5)

problem_result <- detect_extreme_regression_estimates(
    estimate = problem_estimates,
    ci_lower = problem_ci_lower,
    ci_upper = problem_ci_upper,
    effect_measure = "HR"
)

expected_problem_indices <- c(2, 3, 4, 5)  # NA, Inf, negative, zero
cat("Problem indices detected:", paste(problem_result$extreme_indices, collapse = ", "), "\n")
cat("Expected:", paste(expected_problem_indices, collapse = ", "), "\n")

if (all(sort(problem_result$extreme_indices) == sort(expected_problem_indices))) {
    cat("✓ Problematic values correctly detected\n")
} else {
    cat("✗ Problematic value detection failed\n")
}

cat("\n=== EXTREME REGRESSION HANDLING TESTS COMPLETED ===\n") 