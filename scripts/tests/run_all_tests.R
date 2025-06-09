# Run All Tests - SYSTEMATIC DEBUGGING VERSION
# Master script to run numbered tests systematically to identify function name mismatches

cat("=== SYSTEMATIC FUNCTION NAME AND LOCATION DATA DEBUGGING ===\n")
cat(sprintf("Started at: %s\n", Sys.time()))
cat("\nThis systematic test runner helps identify function name mismatches\n")
cat("and missing location data issues across the codebase.\n")

# Create master test output directory
master_test_dir <- file.path("test_output", paste0("systematic_debug_", format(Sys.time(), "%Y%m%d_%H%M%S")))
dir.create(master_test_dir, recursive = TRUE, showWarnings = FALSE)

cat(sprintf("\nMaster test output directory: %s\n", master_test_dir))

# NUMBERED TEST EXECUTION ORDER (from basic to complex)
# Run these tests ONE BY ONE to systematically identify issues
numbered_tests <- list(
    "01" = list(
        script = "test_data_availability.R",
        description = "Data Availability Check",
        purpose = "Verify all required data files exist and are accessible"
    ),
    "02" = list(
        script = "test_data_processing.R", 
        description = "Data Processing Functions",
        purpose = "Test data loading, cleaning, and cohort creation functions"
    ),
    "03" = list(
        script = "test_statistical_analysis.R",
        description = "Statistical Analysis Functions", 
        purpose = "Test primary statistical analysis functions work correctly"
    ),
    "04" = list(
        script = "test_tumor_height_analysis.R",
        description = "Tumor Height Analysis",
        purpose = "Test tumor height-specific analysis functions"
    ),
    "05" = list(
        script = "test_subgroup_analysis.R",
        description = "Subgroup Analysis Functions",
        purpose = "Test subgroup analysis and interaction testing functions"
    ),
    "06" = list(
        script = "test_forest_plots.R", 
        description = "Forest Plot Visualization",
        purpose = "Test forest plot creation functions (KNOWN ISSUE: function name mismatch)"
    )
)

# Display numbered test plan
cat("\n=== NUMBERED TEST EXECUTION PLAN ===\n")
for (test_num in names(numbered_tests)) {
    test_info <- numbered_tests[[test_num]]
    cat(sprintf("TEST %s: %s\n", test_num, test_info$description))
    cat(sprintf("        Script: %s\n", test_info$script))
    cat(sprintf("        Purpose: %s\n", test_info$purpose))
    cat("\n")
}

cat("=== SYSTEMATIC TESTING INSTRUCTIONS ===\n")
cat("TO RUN TESTS SYSTEMATICALLY:\n")
cat("\n1. Run tests one by one in order using:\n")
for (test_num in names(numbered_tests)) {
    test_info <- numbered_tests[[test_num]]
    cat(sprintf("   source('scripts/tests/%s')  # Test %s: %s\n", 
                test_info$script, test_num, test_info$description))
}

cat("\n2. When a test fails:\n")
cat("   - Note the test number and error message\n")
cat("   - Check for function name mismatches in the error\n")
cat("   - Look for 'missing location data' or 'object not found' errors\n")
cat("   - Report the specific test number and error to debug systematically\n")

cat("\n3. Known function name mismatches to check:\n")
cat("   - main.R calls create_forest_plot() but forest_plot.R has create_single_cohort_forest_plot()\n")
cat("   - Check if all subgroup analysis function names match between files\n")
cat("   - Verify all analysis function names are consistent across scripts\n")

# Track test results
test_results <- data.frame(
    test_number = character(0),
    test_name = character(0),
    status = character(0),
    duration_seconds = numeric(0),
    error_message = character(0),
    stringsAsFactors = FALSE
)

total_start_time <- Sys.time()

# Run each numbered test
for (test_num in names(numbered_tests)) {
    test_info <- numbered_tests[[test_num]]
    test_script <- test_info$script
    test_name <- gsub("\\.R$", "", test_script)
    
    cat(sprintf("\n=== RUNNING TEST %s: %s ===\n", test_num, test_info$description))
    cat(sprintf("Script: %s\n", test_script))
    cat(sprintf("Purpose: %s\n", test_info$purpose))
    
    test_start_time <- Sys.time()
    
    tryCatch({
        # Capture output to log file
        log_file <- file.path(master_test_dir, paste0("test_", test_num, "_", test_name, "_log.txt"))
        
        # Source the test script and capture output
        capture.output({
            source(file.path("scripts/tests", test_script))
        }, file = log_file, type = "output")
        
        test_duration <- as.numeric(difftime(Sys.time(), test_start_time, units = "secs"))
        
        test_results <- rbind(test_results, data.frame(
            test_number = test_num,
            test_name = test_name,
            status = "SUCCESS",
            duration_seconds = test_duration,
            error_message = "",
            stringsAsFactors = FALSE
        ))
        
        cat(sprintf("✓ TEST %s completed successfully (%.1f seconds)\n", test_num, test_duration))
        
    }, error = function(e) {
        test_duration <- as.numeric(difftime(Sys.time(), test_start_time, units = "secs"))
        
        test_results <<- rbind(test_results, data.frame(
            test_number = test_num,
            test_name = test_name,
            status = "FAILED",
            duration_seconds = test_duration,
            error_message = as.character(e$message),
            stringsAsFactors = FALSE
        ))
        
        cat(sprintf("✗ TEST %s FAILED (%.1f seconds)\n", test_num, test_duration))
        cat(sprintf("   ERROR: %s\n", e$message))
        
        # Write error to log file with more detail
        error_log <- file.path(master_test_dir, paste0("test_", test_num, "_", test_name, "_ERROR.txt"))
        error_details <- c(
            paste("TEST", test_num, "FAILED:", test_info$description),
            paste("Script:", test_script), 
            paste("Error:", e$message),
            "",
            "CHECK FOR:",
            "- Function name mismatches",
            "- Missing location data issues", 
            "- Object not found errors",
            "- Incorrect function calls in main.R"
        )
        writeLines(error_details, error_log)
        
        cat(sprintf("   Detailed error saved to: test_%s_%s_ERROR.txt\n", test_num, test_name))
    })
}

total_duration <- as.numeric(difftime(Sys.time(), total_start_time, units = "mins"))

# Generate systematic debugging summary
cat("\n=== SYSTEMATIC DEBUGGING SUMMARY ===\n")
cat(sprintf("Total execution time: %.1f minutes\n", total_duration))

successful_tests <- sum(test_results$status == "SUCCESS")
failed_tests <- sum(test_results$status == "FAILED")

cat(sprintf("Tests passed: %d/%d\n", successful_tests, nrow(test_results)))
cat(sprintf("Tests failed: %d/%d\n", failed_tests, nrow(test_results)))

if (failed_tests > 0) {
    cat("\n=== FAILED TESTS REQUIRING ATTENTION ===\n")
    failed_results <- test_results[test_results$status == "FAILED", ]
    for (i in 1:nrow(failed_results)) {
        cat(sprintf("TEST %s (%s): %s\n", 
                    failed_results$test_number[i], 
                    failed_results$test_name[i], 
                    failed_results$error_message[i]))
    }
    
    cat("\n=== SYSTEMATIC DEBUGGING STEPS ===\n")
    cat("1. Focus on the first failed test number\n")
    cat("2. Check the detailed error log for that test\n")
    cat("3. Look for function name mismatches in the error message\n")
    cat("4. Fix the function name issues before proceeding to next test\n")
    cat("5. Re-run the test to verify the fix\n")
    cat("6. Move to the next failed test number\n")
}

# Save detailed results
results_file <- file.path(master_test_dir, "systematic_test_results.csv")
write.csv(test_results, results_file, row.names = FALSE)
cat(sprintf("\nDetailed results saved to: %s\n", results_file))

# Create summary table
suppressMessages({
    library(gt)
    
    summary_table <- test_results %>%
        mutate(
            duration_formatted = sprintf("%.1f sec", duration_seconds),
            status_icon = ifelse(status == "SUCCESS", "✓", "✗"),
            status_display = paste(status_icon, status),
            test_display = paste("Test", test_number)
        ) %>%
        select(
            `Test #` = test_display,
            `Test Name` = test_name,
            `Status` = status_display,
            `Duration` = duration_formatted,
            `Error Message` = error_message
        ) %>%
        gt() %>%
        tab_header(
            title = "Systematic Function Name Debugging Results",
            subtitle = sprintf("Executed on %s", Sys.time())
        ) %>%
        tab_style(
            style = cell_text(color = "green", weight = "bold"),
            locations = cells_body(columns = `Status`, rows = grepl("✓", `Status`))
        ) %>%
        tab_style(
            style = cell_text(color = "red", weight = "bold"),
            locations = cells_body(columns = `Status`, rows = grepl("✗", `Status`))
        ) %>%
        cols_width(
            `Test #` ~ px(80),
            `Test Name` ~ px(200),
            `Status` ~ px(100),
            `Duration` ~ px(100),
            `Error Message` ~ px(350)
        )
    
    # Save HTML summary
    save_gt_html(
        summary_table,
        filename = file.path(master_test_dir, "systematic_debug_summary.html")
    )
})

cat(sprintf("\nHTML summary saved to: %s\n", file.path(master_test_dir, "systematic_debug_summary.html")))

# Specific guidance for known issues
cat("\n=== KNOWN FUNCTION NAME ISSUES TO CHECK ===\n")
cat("1. FOREST PLOT FUNCTIONS:\n")
cat("   - main.R line 339+ calls: create_forest_plot()\n")
cat("   - forest_plot.R only has: create_single_cohort_forest_plot()\n")
cat("   - Fix: Either rename function or update main.R calls\n")
cat("\n2. SUBGROUP ANALYSIS FUNCTIONS:\n") 
cat("   - Check if all function names match between:\n")
cat("     * scripts/analysis/subgroup_analysis.R\n")
cat("     * scripts/main.R calls\n")
cat("     * scripts/tests/test_subgroup_analysis.R\n")
cat("\n3. LOCATION DATA ISSUES:\n")
cat("   - Check if data processing creates 'location' variable\n")
cat("   - Verify forest plot functions can handle missing location data\n")
cat("   - Look for hardcoded assumptions about data structure\n")

cat("\n=== INDIVIDUAL TEST EXECUTION COMMANDS ===\n")
cat("To run individual numbered tests for debugging:\n")
for (test_num in names(numbered_tests)) {
    test_info <- numbered_tests[[test_num]]
    cat(sprintf("source('scripts/tests/%s')  # Test %s: %s\n", 
                test_info$script, test_num, test_info$description))
}

cat(sprintf("\nSystematic debugging session completed at: %s\n", Sys.time()))
cat(sprintf("Results directory: %s\n", master_test_dir)) 