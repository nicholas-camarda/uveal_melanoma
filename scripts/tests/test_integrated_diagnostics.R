#!/usr/bin/env Rscript
#' Test Integrated Diagnostics System
#' 
#' This script tests that the modified analysis functions return diagnostics
#' instead of writing individual files, and that they can be consolidated.

# Source all required functions
source("scripts/utils/all_helper_functions.R")

# Set required global variables
VERBOSE <- TRUE
SHOW_ALL_PVALUES <- FALSE

# Set up output directories (required by analysis functions)
dir.create("test_output", showWarnings = FALSE)
output_dirs <- list(
    efficacy = "test_output",
    safety = "test_output", 
    repeat_radiation = "test_output",
    obj1_recurrence = "test_output",
    obj1_mets = "test_output",
    obj1_os = "test_output", 
    obj1_pfs = "test_output",
    obj3_pfs2 = "test_output",
    baseline_characteristics = "test_output"
)
prefix <- "test_"

cat("=== TESTING INTEGRATED DIAGNOSTICS SYSTEM ===\n")

# Load test data
data <- readxl::read_excel("final_data/Analytic Dataset/uveal_melanoma_full_cohort.xlsx")
cat("Loaded data:", nrow(data), "rows\n")

# Test binary outcome analysis
cat("\n1. Testing binary outcome analysis (recurrence)...\n")
recurrence_result <- analyze_binary_outcome_rates(
    data, 
    "recurrence1", 
    "tt_recurrence_months", 
    "recurrence_event"
)

cat("   - Analysis completed\n")
cat("   - Diagnostics available:", !is.null(recurrence_result$diagnostics), "\n")
if (!is.null(recurrence_result$diagnostics)) {
    cat("   - Diagnostics rows:", nrow(recurrence_result$diagnostics), "\n")
    cat("   - Diagnostics columns:", paste(colnames(recurrence_result$diagnostics), collapse = ", "), "\n")
}

# Test Cox regression analysis
cat("\n2. Testing Cox regression analysis (overall survival)...\n")
os_result <- analyze_time_to_event_outcomes(
    data,
    time_var = "tt_death_months",
    event_var = "death_event", 
    group_var = "treatment_group",
    ylab = "Overall Survival Probability"
)

cat("   - Analysis completed\n")
cat("   - Diagnostics available:", !is.null(os_result$diagnostics), "\n")
if (!is.null(os_result$diagnostics)) {
    cat("   - Diagnostics rows:", nrow(os_result$diagnostics), "\n")
    cat("   - Diagnostics columns:", paste(colnames(os_result$diagnostics), collapse = ", "), "\n")
}

# Test consolidated diagnostics writing
cat("\n3. Testing consolidated diagnostics writing...\n")
diagnostics_list <- list()
if (!is.null(recurrence_result$diagnostics)) {
    diagnostics_list[["recurrence_logistic"]] <- recurrence_result$diagnostics
}
if (!is.null(os_result$diagnostics)) {
    diagnostics_list[["overall_survival_cox"]] <- os_result$diagnostics
}

if (length(diagnostics_list) > 0) {
    test_file <- "test_output/test_consolidated_diagnostics.xlsx"
    dir.create("test_output", showWarnings = FALSE)
    
    write_diagnostics_excel(diagnostics_list, test_file)
    cat("   - Consolidated diagnostics written to:", test_file, "\n")
    cat("   - Number of tabs:", length(diagnostics_list), "\n")
    
    # Verify the file was created and has correct structure
    if (file.exists(test_file)) {
        sheets <- readxl::excel_sheets(test_file)
        cat("   - Excel sheets created:", paste(sheets, collapse = ", "), "\n")
        
        # Check first sheet content
        first_sheet <- readxl::read_excel(test_file, sheet = 1)
        cat("   - First sheet rows:", nrow(first_sheet), "\n")
        
        cat("✅ Consolidated diagnostics test PASSED\n")
    } else {
        cat("❌ Consolidated diagnostics test FAILED - file not created\n")
    }
} else {
    cat("❌ No diagnostics available for consolidation\n")
}

cat("\n=== INTEGRATED DIAGNOSTICS TEST COMPLETE ===\n") 