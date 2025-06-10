# Test Workflow Integrity with Real Data
# Author: Nicholas Camarda
# Description: Test that validation checkpoints work correctly in the actual data processing pipeline

# Load libraries
library(tidyverse)
library(readxl)
library(lubridate)

# Source required functions
source("scripts/utils/analysis_config.R")
source("scripts/data_helper/data_processing.R")

# Set required global variables
VERBOSE <- TRUE

# Test workflow integrity
test_workflow_integrity <- function() {
    log_enhanced("=== TESTING WORKFLOW INTEGRITY ON REAL DATA ===", level = "SECTION")
    
    # Step 1: Load and process data
    log_enhanced("Loading and processing data...", level = "INFO")
    cleaned_data <- load_and_clean_data(filename = INPUT_FILENAME)
    derived_data <- create_derived_variables(cleaned_data)  
    factored_data <- prepare_factor_levels(derived_data)
    
    # Step 2: Apply criteria (this should trigger validation)
    log_enhanced("Applying criteria and validating cohort integrity...", level = "INFO")
    final_analytic_datasets_lst <- apply_criteria(factored_data)
    
    # Step 3: Test that validation checkpoints work during analysis setup
    log_enhanced("Testing analysis setup validation...", level = "INFO")
    
    # Test each dataset naming validation
    for (dataset_name in names(final_analytic_datasets_lst)) {
        # Set up cohort information (same logic as main.R)
        cohort_info <- case_when(
            grepl("full", dataset_name) ~ list(prefix = "full_cohort_", dir_name = "uveal_full"),
            grepl("restricted", dataset_name) ~ list(prefix = "restricted_cohort_", dir_name = "uveal_restricted"), 
            grepl("gksrs", dataset_name) ~ list(prefix = "gksrs_only_cohort_", dir_name = "gksrs"),
            TRUE ~ list(prefix = paste0(dataset_name, "_"), dir_name = dataset_name)
        )
        
        # Test naming validation
        naming_valid <- validate_naming_consistency(dataset_name, cohort_info$prefix, cohort_info$dir_name)
        
        if (naming_valid) {
            log_enhanced(sprintf("✓ Naming validation passed for %s", dataset_name), level = "INFO")
        } else {
            log_enhanced(sprintf("✗ Naming validation FAILED for %s", dataset_name), level = "ERROR")
            return(FALSE)
        }
    }
    
    # Step 4: Generate validation report  
    log_enhanced("Generating validation report...", level = "INFO")
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    report_path <- file.path("logs", paste0("workflow_integrity_test_", timestamp, ".txt"))
    generate_validation_report(final_analytic_datasets_lst, report_path)
    
    log_enhanced("=== WORKFLOW INTEGRITY TEST COMPLETED SUCCESSFULLY ===", level = "SECTION")
    log_enhanced(sprintf("Validation report saved to: %s", report_path), level = "INFO")
    
    return(TRUE)
}

# Run the test
test_result <- test_workflow_integrity()

if (test_result) {
    cat("\n🎉 WORKFLOW INTEGRITY TEST PASSED! All validation checkpoints working correctly.\n")
} else {
    cat("\n❌ WORKFLOW INTEGRITY TEST FAILED! Check validation errors above.\n")
} 