#!/usr/bin/env Rscript
#' Simple Test for Diagnostics Return
#' 
#' This script tests that analysis functions return diagnostics objects
#' without running full analyses.

# Source required functions  
source("scripts/utils/all_helper_functions.R")

cat("=== TESTING DIAGNOSTICS RETURN ===\n")

# Test 1: Check that analyze_binary_outcome_rates function signature includes diagnostics
cat("1. Checking analyze_binary_outcome_rates function...\n")
func_body <- deparse(body(analyze_binary_outcome_rates))
has_diagnostics_return <- any(grepl("diagnostics.*=.*logit_diagnostics", func_body))
cat("   - Function returns diagnostics:", has_diagnostics_return, "\n")

# Test 2: Check that analyze_time_to_event_outcomes function signature includes diagnostics
cat("2. Checking analyze_time_to_event_outcomes function...\n")
func_body <- deparse(body(analyze_time_to_event_outcomes))
has_diagnostics_return <- any(grepl("diagnostics.*=.*cox_diagnostics", func_body))
cat("   - Function returns diagnostics:", has_diagnostics_return, "\n")

# Test 3: Check that analyze_radiation_complications function signature includes diagnostics
cat("3. Checking analyze_radiation_complications function...\n")
func_body <- deparse(body(analyze_radiation_complications))
has_diagnostics_return <- any(grepl("diagnostics.*=", func_body))
cat("   - Function returns diagnostics:", has_diagnostics_return, "\n")

# Test 4: Check that write_diagnostics_excel function exists
cat("4. Checking write_diagnostics_excel function...\n")
has_function <- exists("write_diagnostics_excel")
cat("   - Function exists:", has_function, "\n")

if (has_function) {
    # Test the function with dummy data
    dummy_diagnostics <- list(
        "test_analysis" = data.frame(
            analysis_type = "test",
            outcome = "test_outcome", 
            estimate = 1.5,
            ci_lower = 0.8,
            ci_upper = 2.2,
            p_value = 0.05,
            status = "INCLUDED",
            exclusion_reason = ""
        )
    )
    
    test_file <- "test_output/test_write_diagnostics.xlsx"
    dir.create("test_output", showWarnings = FALSE)
    
    tryCatch({
        write_diagnostics_excel(dummy_diagnostics, test_file)
        if (file.exists(test_file)) {
            sheets <- readxl::excel_sheets(test_file)
            cat("   - Successfully wrote diagnostics file with sheets:", paste(sheets, collapse = ", "), "\n")
            file.remove(test_file)  # cleanup
            cat("✅ write_diagnostics_excel test PASSED\n")
        } else {
            cat("❌ write_diagnostics_excel test FAILED - file not created\n")
        }
    }, error = function(e) {
        cat("❌ write_diagnostics_excel test FAILED:", e$message, "\n")
    })
}

cat("\n=== DIAGNOSTICS RETURN TEST COMPLETE ===\n") 