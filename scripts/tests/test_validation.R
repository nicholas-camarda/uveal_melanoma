# Test Validation Functions
# Author: Nicholas Camarda
# Description: Test the new data validation functions to ensure they catch integrity issues

# Load required libraries
library(tidyverse)

# Source required functions
source("scripts/utils/analysis_config.R")

#' Test validation functions with synthetic data
test_validation_functions <- function() {
    log_enhanced("=== TESTING VALIDATION FUNCTIONS ===", level = "SECTION")
    
    # Create synthetic test data
    log_enhanced("Creating synthetic test data", level = "INFO")
    
    # Create valid cohort data
    full_data <- data.frame(
        id = 1:100,
        consort_group = c(rep("eligible_both", 60), rep("gksrs_only", 40)),
        treatment_group = sample(c("GKSRS", "Plaque"), 100, replace = TRUE),
        initial_tumor_diameter = c(runif(60, 10, 20), runif(40, 20, 30)),
        initial_tumor_height = c(runif(60, 5, 10), runif(40, 10, 15)),
        optic_nerve = c(rep("No", 60), rep("Yes", 40))
    )
    
    restricted_data <- full_data[full_data$consort_group == "eligible_both", ]
    gksrs_only_data <- full_data[full_data$consort_group == "gksrs_only", ]
    
    # Test 1: Valid cohort list (should pass)
    log_enhanced("TEST 1: Valid cohort structure", level = "INFO")
    valid_cohorts <- list(
        uveal_melanoma_full_cohort = full_data,
        uveal_melanoma_restricted_cohort = restricted_data,
        uveal_melanoma_gksrs_only_cohort = gksrs_only_data
    )
    
    result1 <- validate_cohort_integrity(valid_cohorts)
    log_enhanced(sprintf("Test 1 result: %s", if(result1) "PASSED" else "FAILED"), level = "INFO")
    
    # Test 2: Invalid cohort names (should fail)
    log_enhanced("TEST 2: Invalid cohort names", level = "INFO")
    invalid_names <- list(
        wrong_name_1 = full_data,
        wrong_name_2 = restricted_data,
        wrong_name_3 = gksrs_only_data
    )
    
    result2 <- validate_cohort_integrity(invalid_names)
    log_enhanced(sprintf("Test 2 result: %s (should be FAILED)", if(result2) "PASSED" else "FAILED"), level = "INFO")
    
    # Test 3: Wrong eligibility criteria (should fail)
    log_enhanced("TEST 3: Wrong eligibility criteria", level = "INFO")
    
    # Put some restricted patients in GKSRS-only cohort
    wrong_restricted <- full_data[1:30, ]  # Mix of both types
    wrong_gksrs <- full_data[31:100, ]     # Mix of both types
    
    invalid_eligibility <- list(
        uveal_melanoma_full_cohort = full_data,
        uveal_melanoma_restricted_cohort = wrong_restricted,
        uveal_melanoma_gksrs_only_cohort = wrong_gksrs
    )
    
    result3 <- validate_cohort_integrity(invalid_eligibility)
    log_enhanced(sprintf("Test 3 result: %s (should be FAILED)", if(result3) "PASSED" else "FAILED"), level = "INFO")
    
    # Test 4: Naming consistency validation
    log_enhanced("TEST 4: Naming consistency", level = "INFO")
    
    # Valid naming
    result4a <- validate_naming_consistency("uveal_melanoma_full_cohort", "full_cohort_", "uveal_full")
    log_enhanced(sprintf("Test 4a (valid naming): %s", if(result4a) "PASSED" else "FAILED"), level = "INFO")
    
    # Invalid naming
    result4b <- validate_naming_consistency("uveal_melanoma_full_cohort", "wrong_prefix_", "wrong_dir")
    log_enhanced(sprintf("Test 4b (invalid naming): %s (should be FAILED)", if(result4b) "PASSED" else "FAILED"), level = "INFO")
    
    # Summary
    log_enhanced("=== VALIDATION TEST SUMMARY ===", level = "SECTION")
    tests_passed <- sum(c(result1, !result2, !result3, result4a, !result4b))
    log_enhanced(sprintf("Tests passed: %d/5", tests_passed), level = "INFO")
    
    if (tests_passed == 5) {
        log_enhanced("ALL VALIDATION TESTS PASSED", level = "PROGRESS")
    } else {
        log_enhanced("SOME VALIDATION TESTS FAILED", level = "WARN")
    }
    
    return(tests_passed == 5)
}

# Run the test if this script is sourced directly
if (interactive() || !exists("test_mode")) {
    test_validation_functions()
} 