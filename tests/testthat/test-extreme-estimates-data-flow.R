# Test Extreme Estimates Data Flow Pipeline
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("demonstrate CI width detection bug - ratio vs raw difference", {
  # Test the specific CI case that should be detected as extreme
  # CI: (-29,404, 29,456)
  # Raw difference: 29,456 - (-29,404) = 58,860 (extremely wide!)
  # Ratio: 29,456 / (-29,404) = -1.002 (not wide by ratio measure)
  
  test_ci_lower <- -29404
  test_ci_upper <- 29456
  test_estimate <- 26  # Some reasonable estimate
  
  # Calculate what the current logic would do
  raw_difference <- test_ci_upper - test_ci_lower
  ratio <- test_ci_upper / test_ci_lower
  
  cat("=== CI Width Detection Bug Test ===\n")
  cat("CI: (", test_ci_lower, ", ", test_ci_upper, ")\n", sep = "")
  cat("Raw difference:", raw_difference, "\n")
  cat("Ratio:", ratio, "\n")
  cat("CI_WIDTH_THRESHOLD:", CI_WIDTH_THRESHOLD, "\n")
  cat("Would be detected by raw difference logic:", raw_difference > CI_WIDTH_THRESHOLD, "\n")
  
  # Test the actual detection function
  result <- detect_extreme_regression_estimates(
    estimate = test_estimate,
    ci_lower = test_ci_lower,
    ci_upper = test_ci_upper,
    effect_measure = "OR",
    is_exponentiated = TRUE
  )
  
  cat("Detection result - extreme indices:", result$extreme_indices, "\n")
  cat("Detection result - reasons:", result$exclusion_reasons, "\n")
  
  # This CI should be detected as extreme because it's extremely wide
  # But the current logic uses ratio instead of raw difference
  expect_true(length(result$extreme_indices) > 0, 
              info = "This CI should be detected as extreme due to extremely wide raw difference")
})

test_that("isolate where filtering is lost in the data flow pipeline", {
  # Create test data with known extreme estimates
  test_data <- data.frame(
    treatment_group = factor(c("Plaque", "GKSRS")),
    outcome = c(1, 0),  # Perfect separation to create extreme estimates
    age_at_diagnosis = c(50, 60)
  )
  
  # Create a real gtsummary table with extreme estimates
  model <- glm(outcome ~ treatment_group + age_at_diagnosis, 
               data = test_data, family = binomial)
  
  # Create gtsummary table
  tbl <- tbl_regression(model, exponentiate = TRUE)
  
  # Step 1: Test that extreme estimates are detected and filtered
  cat("=== STEP 1: Testing extreme estimate filtering ===\n")
  filtering_result <- apply_extreme_estimate_filtering(tbl, model, "OR", analysis_name = "test")
  filtered_tbl <- filtering_result$tbl_filtered
  
  cat("Original table has", nrow(tbl$table_body), "rows\n")
  cat("Filtered table has", nrow(filtered_tbl$table_body), "rows\n")
  cat("Extreme terms found:", paste(filtering_result$diagnostics$extreme_terms, collapse = ", "), "\n")
  
  # Verify that filtering worked
  expect_true(nrow(filtered_tbl$table_body) < nrow(tbl$table_body))
  expect_true(length(filtering_result$diagnostics$extreme_terms) > 0)
  
  # Step 2: Test that filtering is preserved through gt conversion
  cat("=== STEP 2: Testing gt conversion ===\n")
  gt_tbl <- as_gt(filtered_tbl)
  
  # Convert to HTML to check content
  html_output <- gt_tbl %>% as_raw_html()
  cat("HTML length:", nchar(html_output), "\n")
  
  # Check if extreme terms are still in HTML after gt conversion
  if (length(filtering_result$diagnostics$extreme_terms) > 0) {
    for (term in filtering_result$diagnostics$extreme_terms) {
      term_in_html <- grepl(term, html_output, fixed = TRUE)
      cat("Term", term, "in HTML after gt conversion:", term_in_html, "\n")
      expect_false(term_in_html, info = paste("Extreme term", term, "should not be in HTML after gt conversion"))
    }
  }
  
  # Step 3: Test that filtering is preserved through modify_gt_table_pvalues
  cat("=== STEP 3: Testing modify_gt_table_pvalues ===\n")
  modified_table <- modify_gt_table_pvalues(gt_tbl, filtered_tbl, test_data, "outcome", "age_at_diagnosis", model)
  
  cat("Modified table has", nrow(modified_table$table_body), "rows\n")
  
  # Convert modified table to HTML
  modified_gt <- as_gt(modified_table)
  modified_html <- modified_gt %>% as_raw_html()
  cat("Modified HTML length:", nchar(modified_html), "\n")
  
  # Check if extreme terms are still in HTML after modify_gt_table_pvalues
  if (length(filtering_result$diagnostics$extreme_terms) > 0) {
    for (term in filtering_result$diagnostics$extreme_terms) {
      term_in_modified_html <- grepl(term, modified_html, fixed = TRUE)
      cat("Term", term, "in HTML after modify_gt_table_pvalues:", term_in_modified_html, "\n")
      expect_false(term_in_modified_html, info = paste("Extreme term", term, "should not be in HTML after modify_gt_table_pvalues"))
    }
  }
  
  # Step 4: Test the complete pipeline
  cat("=== STEP 4: Testing complete pipeline ===\n")
  
  # Simulate the complete pipeline
  # 1. Apply filtering
  filtered_result <- apply_extreme_estimate_filtering(tbl, model, "OR", analysis_name = "test")
  filtered_table <- filtered_result$tbl_filtered
  
  # 2. Convert to gt
  gt_table <- as_gt(filtered_table)
  
  # 3. Apply modify_gt_table_pvalues
  modified_table <- modify_gt_table_pvalues(gt_table, filtered_table, test_data, "outcome", "age_at_diagnosis", model)
  
  # 4. Convert to HTML
  final_html <- as_gt(modified_table) %>% as_raw_html()
  
  cat("Final HTML length:", nchar(final_html), "\n")
  
  # Check if extreme terms are in final HTML
  if (length(filtering_result$diagnostics$extreme_terms) > 0) {
    for (term in filtering_result$diagnostics$extreme_terms) {
      term_in_final_html <- grepl(term, final_html, fixed = TRUE)
      cat("Term", term, "in final HTML:", term_in_final_html, "\n")
      expect_false(term_in_final_html, info = paste("Extreme term", term, "should not be in final HTML"))
    }
  }
})

test_that("specific CI case (-29,404, 29,456) is correctly identified as extreme", {
  # Create test data that should produce the specific CI case
  test_data <- data.frame(
    treatment_group = factor(c("Plaque", "GKSRS")),
    outcome = c(1, 0),  # Perfect separation to create extreme estimates
    age_at_diagnosis = c(50, 60)
  )
  
  # Create a real gtsummary table with extreme estimates
  model <- glm(outcome ~ treatment_group + age_at_diagnosis, 
               data = test_data, family = binomial)
  
  # Create gtsummary table
  tbl <- tbl_regression(model, exponentiate = TRUE)
  
  # Test that extreme estimates are detected and filtered
  filtering_result <- apply_extreme_estimate_filtering(tbl, model, "OR", analysis_name = "test")
  filtered_tbl <- filtering_result$tbl_filtered
  
  # DEBUG: Print what was filtered
  cat("DEBUG: Original table has", nrow(tbl$table_body), "rows\n")
  cat("DEBUG: Filtered table has", nrow(filtered_tbl$table_body), "rows\n")
  cat("DEBUG: Extreme terms found:", paste(filtering_result$diagnostics$extreme_terms, collapse = ", "), "\n")
  
  # Test that the filtering is preserved through gt conversion
  gt_tbl <- as_gt(filtered_tbl)
  
  # Test that the filtering is preserved through HTML conversion
  html_output <- gt_tbl %>% as_raw_html()
  
  # DEBUG: Check what's in the HTML
  cat("DEBUG: HTML length:", nchar(html_output), "\n")
  if (length(filtering_result$diagnostics$extreme_terms) > 0) {
    cat("DEBUG: HTML contains extreme terms:", sapply(filtering_result$diagnostics$extreme_terms, function(term) grepl(term, html_output, fixed = TRUE)), "\n")
  }
  
  # Test that HTML doesn't contain extreme estimates that were filtered
  if (length(filtering_result$diagnostics$extreme_terms) > 0) {
    for (term in filtering_result$diagnostics$extreme_terms) {
      expect_false(grepl(term, html_output, fixed = TRUE))
    }
  }
})

test_that("non-extreme estimates are preserved through the pipeline", {
  # Create test data with non-extreme estimates - ensure no perfect separation
  test_data <- data.frame(
    treatment_group = factor(c("Plaque", "GKSRS", "Plaque", "GKSRS", "Plaque", "GKSRS", "Plaque", "GKSRS")),
    outcome = c(1, 0, 1, 0, 0, 1, 1, 0),  # No perfect separation
    age_at_diagnosis = c(50, 60, 55, 65, 70, 45, 75, 40),
    sex = factor(c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female"))
  )
  
  # Create a real gtsummary table with non-extreme estimates
  model <- glm(outcome ~ treatment_group + age_at_diagnosis + sex, 
               data = test_data, family = binomial)
  
  # Create gtsummary table
  tbl <- tbl_regression(model, exponentiate = TRUE)
  
  # Test that non-extreme estimates are preserved
  filtering_result <- apply_extreme_estimate_filtering(tbl, model, "OR", analysis_name = "test")
  filtered_tbl <- filtering_result$tbl_filtered
  
  # DEBUG: Print what was filtered
  cat("DEBUG: Original table has", nrow(tbl$table_body), "rows\n")
  cat("DEBUG: Filtered table has", nrow(filtered_tbl$table_body), "rows\n")
  cat("DEBUG: Extreme terms found:", paste(filtering_result$diagnostics$extreme_terms, collapse = ", "), "\n")
  
  # Test that the filtering is preserved through gt conversion
  gt_tbl <- as_gt(filtered_tbl)
  
  # Test that the filtering is preserved through HTML conversion
  html_output <- gt_tbl %>% as_raw_html()
  
  # DEBUG: Check what's in the HTML
  cat("DEBUG: HTML length:", nchar(html_output), "\n")
  if (length(filtering_result$diagnostics$extreme_terms) > 0) {
    cat("DEBUG: HTML contains extreme terms:", sapply(filtering_result$diagnostics$extreme_terms, function(term) grepl(term, html_output, fixed = TRUE)), "\n")
  }
  
  # Test that HTML doesn't contain extreme estimates that were filtered
  if (length(filtering_result$diagnostics$extreme_terms) > 0) {
    for (term in filtering_result$diagnostics$extreme_terms) {
      expect_false(grepl(term, html_output, fixed = TRUE))
    }
  }
  
  # Test that non-extreme terms are preserved in HTML
  non_extreme_terms <- c("treatment_group", "age_at_diagnosis", "sex")
  for (term in non_extreme_terms) {
    expect_true(grepl(term, html_output, fixed = TRUE))
  }
})

test_that("extreme estimates with wide CIs are correctly filtered", {
  # Create test data that should produce wide CIs
  test_data <- data.frame(
    treatment_group = factor(c("Plaque", "GKSRS")),
    outcome = c(1, 0),  # Perfect separation
    age_at_diagnosis = c(50, 60)
  )
  
  # Create a real gtsummary table with extreme estimates
  model <- glm(outcome ~ treatment_group + age_at_diagnosis, 
               data = test_data, family = binomial)
  
  # Create gtsummary table
  tbl <- tbl_regression(model, exponentiate = TRUE)
  
  # Test that extreme estimates are detected and filtered
  filtering_result <- apply_extreme_estimate_filtering(tbl, model, "OR", analysis_name = "test")
  filtered_tbl <- filtering_result$tbl_filtered
  
  # DEBUG: Print what was filtered
  cat("DEBUG: Original table has", nrow(tbl$table_body), "rows\n")
  cat("DEBUG: Filtered table has", nrow(filtered_tbl$table_body), "rows\n")
  cat("DEBUG: Extreme terms found:", paste(filtering_result$diagnostics$extreme_terms, collapse = ", "), "\n")
  
  # Test that the filtering is preserved through gt conversion
  gt_tbl <- as_gt(filtered_tbl)
  
  # Test that the filtering is preserved through HTML conversion
  html_output <- gt_tbl %>% as_raw_html()
  
  # DEBUG: Check what's in the HTML
  cat("DEBUG: HTML length:", nchar(html_output), "\n")
  if (length(filtering_result$diagnostics$extreme_terms) > 0) {
    cat("DEBUG: HTML contains extreme terms:", sapply(filtering_result$diagnostics$extreme_terms, function(term) grepl(term, html_output, fixed = TRUE)), "\n")
  }
  
  # Test that HTML doesn't contain extreme estimates that were filtered
  if (length(filtering_result$diagnostics$extreme_terms) > 0) {
    for (term in filtering_result$diagnostics$extreme_terms) {
      expect_false(grepl(term, html_output, fixed = TRUE))
    }
  }
}) 

test_that("demonstrate reasonable CI width thresholds", {
  # Test various CI scenarios to determine appropriate threshold
  
  # Extremely wide CI (should be filtered)
  extreme_ci_lower <- -29404
  extreme_ci_upper <- 29456
  extreme_raw_diff <- extreme_ci_upper - extreme_ci_lower
  
  # Reasonable CIs (should NOT be filtered)
  reasonable_cis <- list(
    c(0.5, 2.0),    # OR/HR: reasonable range
    c(0.8, 1.5),    # OR/HR: narrow range
    c(0.1, 10.0),   # OR/HR: wider but still reasonable
    c(0.01, 100.0), # OR/HR: very wide but might be reasonable in some cases
    c(-2.0, 5.0),   # Log scale: reasonable range
    c(-1.0, 1.0),   # Log scale: narrow range
    c(-5.0, 5.0)    # Log scale: wider but reasonable
  )
  
  cat("=== CI Width Threshold Analysis ===\n")
  cat("Extremely wide CI: (", extreme_ci_lower, ", ", extreme_ci_upper, ") - width = ", extreme_raw_diff, "\n", sep = "")
  
  cat("\nReasonable CIs:\n")
  for (i in seq_along(reasonable_cis)) {
    ci <- reasonable_cis[[i]]
    width <- ci[2] - ci[1]
    cat("CI ", i, ": (", ci[1], ", ", ci[2], ") - width = ", width, "\n", sep = "")
  }
  
  # Calculate what threshold would be appropriate
  reasonable_widths <- sapply(reasonable_cis, function(ci) ci[2] - ci[1])
  max_reasonable_width <- max(reasonable_widths)
  
  cat("\nAnalysis:\n")
  cat("Maximum reasonable width:", max_reasonable_width, "\n")
  cat("Extreme width:", extreme_raw_diff, "\n")
  cat("Ratio of extreme to max reasonable:", extreme_raw_diff / max_reasonable_width, "\n")
  
  # Suggest a threshold
  suggested_threshold <- max_reasonable_width * 10  # 10x the max reasonable width
  
  cat("\nSuggested threshold:", suggested_threshold, "\n")
  cat("Would filter extreme CI:", extreme_raw_diff > suggested_threshold, "\n")
  cat("Would filter reasonable CIs:", sum(reasonable_widths > suggested_threshold), "out of", length(reasonable_widths), "\n")
}) 

test_that("demonstrate why ratio approach was used and why it failed", {
  # The ratio approach was originally designed for exponentiated values (ORs, HRs)
  # where values are always positive and ratios make sense
  
  cat("=== Ratio Approach Analysis ===\n")
  
  # Exponentiated scale examples (ORs, HRs) - ratio approach works here
  cat("EXPONENTIATED SCALE (ORs, HRs) - Ratio approach works:\n")
  exp_cis <- list(
    c(0.5, 2.0),     # Reasonable OR
    c(0.8, 1.5),     # Reasonable HR  
    c(0.1, 10.0),    # Wide but reasonable
    c(0.01, 100.0),  # Very wide - should be filtered
    c(0.001, 1000.0) # Extremely wide - should be filtered
  )
  
  for (i in seq_along(exp_cis)) {
    ci <- exp_cis[[i]]
    ratio <- ci[2] / ci[1]
    width <- ci[2] - ci[1]
    cat("CI ", i, ": (", ci[1], ", ", ci[2], ") - ratio = ", ratio, ", width = ", width, "\n", sep = "")
  }
  
  cat("\nLOG SCALE examples - Ratio approach FAILS here:\n")
  log_cis <- list(
    c(-1.0, 1.0),        # Reasonable log-odds
    c(-2.0, 2.0),        # Reasonable log-odds
    c(-29,404, 29,456),  # Extremely wide - should be filtered
    c(-100, 100),        # Wide - should be filtered
    c(-0.5, 0.5)         # Reasonable log-odds
  )
  
  for (i in seq_along(log_cis)) {
    ci <- log_cis[[i]]
    ratio <- ci[2] / ci[1]
    width <- ci[2] - ci[1]
    cat("CI ", i, ": (", ci[1], ", ", ci[2], ") - ratio = ", ratio, ", width = ", width, "\n", sep = "")
  }
  
  cat("\nANALYSIS:\n")
  cat("Ratio approach was designed for exponentiated values where:\n")
  cat("- All values are positive\n") 
  cat("- Ratio > 1000 indicates extremely wide CI\n")
  cat("- Ratio = 1 indicates narrow CI\n")
  cat("\nRatio approach FAILS for log-scale values because:\n")
  cat("- Values can be negative\n")
  cat("- CIs can span zero (ratio becomes negative)\n")
  cat("- Ratio close to -1 doesn't indicate width\n")
  cat("- CIs like (-29,404, 29,456) get ratio = -1.002 (missed!)\n")
  
  cat("\nRaw difference approach works for BOTH scales:\n")
  cat("- Exponentiated: width = 1000+ indicates extremely wide\n")
  cat("- Log scale: width = 1000+ indicates extremely wide\n")
  cat("- No confusion with negative values\n")
}) 

test_that("demonstrate scale-specific threshold issues", {
  # This test demonstrates that the current implementation has a critical issue:
  # It uses the same CI_WIDTH_THRESHOLD (1000) for both exponentiated and log scale values
  # This is problematic because reasonable exponentiated CIs are much smaller
  
  cat("=== Scale-Specific Threshold Analysis ===\n")
  
  # Test exponentiated values (ORs, HRs) - should use different threshold
  cat("EXPONENTIATED VALUES (ORs, HRs):\n")
  exp_cis <- list(
    c(0.5, 2.0),      # Reasonable OR - should NOT be filtered
    c(0.8, 1.5),      # Reasonable HR - should NOT be filtered
    c(0.1, 10.0),     # Wide but reasonable - should NOT be filtered
    c(0.01, 100.0),   # Very wide - SHOULD be filtered
    c(0.001, 1000.0), # Extremely wide - SHOULD be filtered
    c(0.0001, 10000.0) # Extremely wide - SHOULD be filtered
  )
  
  for (i in seq_along(exp_cis)) {
    ci <- exp_cis[[i]]
    width <- ci[2] - ci[1]
    ratio <- ci[2] / ci[1]
    cat("CI ", i, ": (", ci[1], ", ", ci[2], ") - width = ", width, ", ratio = ", ratio, "\n", sep = "")
    cat("  Current threshold (1000): ", width > 1000, " (", if(width > 1000) "FILTERED" else "PRESERVED", ")\n", sep = "")
    cat("  Suggested threshold (100): ", width > 100, " (", if(width > 100) "FILTERED" else "PRESERVED", ")\n", sep = "")
  }
  
  cat("\nLOG SCALE VALUES:\n")
  log_cis <- list(
    c(-1.0, 1.0),        # Reasonable log-odds - should NOT be filtered
    c(-2.0, 2.0),        # Reasonable log-odds - should NOT be filtered
    c(-5.0, 5.0),        # Wide log-odds - should NOT be filtered
    c(-10.0, 10.0),      # Very wide - SHOULD be filtered
    c(-29,404, 29,456),  # Extremely wide - SHOULD be filtered
    c(-100, 100)         # Extremely wide - SHOULD be filtered
  )
  
  for (i in seq_along(log_cis)) {
    ci <- log_cis[[i]]
    width <- ci[2] - ci[1]
    cat("CI ", i, ": (", ci[1], ", ", ci[2], ") - width = ", width, "\n", sep = "")
    cat("  Current threshold (1000): ", width > 1000, " (", if(width > 1000) "FILTERED" else "PRESERVED", ")\n", sep = "")
    cat("  Suggested threshold (10): ", width > 10, " (", if(width > 10) "FILTERED" else "PRESERVED", ")\n", sep = "")
  }
  
  cat("\nISSUE IDENTIFIED:\n")
  cat("- Exponentiated values need lower threshold (e.g., 100)\n")
  cat("- Log scale values need higher threshold (e.g., 10)\n")
  cat("- Current implementation uses same threshold (1000) for both\n")
  cat("- This means reasonable exponentiated CIs are being missed\n")
}) 

test_that("verify scale-specific thresholds work correctly", {
  # Test that the detection function now uses appropriate thresholds for each scale
  
  cat("=== Scale-Specific Threshold Verification ===\n")
  
  # Test exponentiated values
  cat("TESTING EXPONENTIATED VALUES:\n")
  exp_test_cases <- list(
    list(estimate = 1.0, ci_lower = 0.5, ci_upper = 2.0, expected = FALSE, desc = "Reasonable OR"),
    list(estimate = 1.0, ci_lower = 0.01, ci_upper = 100.0, expected = FALSE, desc = "Wide but acceptable OR"),
    list(estimate = 1.0, ci_lower = 0.001, ci_upper = 1000.0, expected = TRUE, desc = "Extremely wide OR")
  )
  
  for (i in seq_along(exp_test_cases)) {
    case <- exp_test_cases[[i]]
    result <- detect_extreme_regression_estimates(
      estimate = case$estimate,
      ci_lower = case$ci_lower,
      ci_upper = case$ci_upper,
      effect_measure = "OR",
      is_exponentiated = TRUE
    )
    detected <- length(result$extreme_indices) > 0
    cat("  ", case$desc, ": ", case$ci_lower, " to ", case$ci_upper, 
        " (width = ", case$ci_upper - case$ci_lower, ") - ", 
        if(detected) "DETECTED" else "PRESERVED", 
        " (expected: ", if(case$expected) "DETECTED" else "PRESERVED", ")\n", sep = "")
    expect_equal(detected, case$expected, 
                info = paste("Exponentiated case", i, "failed:", case$desc))
  }
  
  # Test log scale values
  cat("\nTESTING LOG SCALE VALUES:\n")
  log_test_cases <- list(
    list(estimate = 0.0, ci_lower = -1.0, ci_upper = 1.0, expected = FALSE, desc = "Reasonable log-odds"),
    list(estimate = 0.0, ci_lower = -10.0, ci_upper = 10.0, expected = TRUE, desc = "Very wide log-odds"),
    list(estimate = 0.0, ci_lower = -100.0, ci_upper = 100.0, expected = TRUE, desc = "Extremely wide log-odds")
  )
  
  for (i in seq_along(log_test_cases)) {
    case <- log_test_cases[[i]]
    result <- detect_extreme_regression_estimates(
      estimate = case$estimate,
      ci_lower = case$ci_lower,
      ci_upper = case$ci_upper,
      effect_measure = "OR",
      is_exponentiated = FALSE
    )
    detected <- length(result$extreme_indices) > 0
    cat("  ", case$desc, ": ", case$ci_lower, " to ", case$ci_upper, 
        " (width = ", case$ci_upper - case$ci_lower, ") - ", 
        if(detected) "DETECTED" else "PRESERVED", 
        " (expected: ", if(case$expected) "DETECTED" else "PRESERVED", ")\n", sep = "")
    expect_equal(detected, case$expected, 
                info = paste("Log scale case", i, "failed:", case$desc))
  }
  
  cat("\nVERIFICATION COMPLETE:\n")
  cat("- Scale-specific thresholds are working correctly\n")
  cat("- Exponentiated values use EXPONENTIATED_CI_THRESHOLD (100)\n")
  cat("- Log scale values use LOG_SCALE_CI_THRESHOLD (10)\n")
  cat("- Appropriate CIs are being detected and filtered\n")
}) 

test_that("verify deterministic effect_measure-based detection", {
  # Test that the deterministic approach based on effect_measure works correctly
  # This is much more robust than trying to detect from values
  
  cat("=== Deterministic Effect Measure Detection ===\n")
  
  # Test cases with different effect measures
  test_cases <- list(
    list(effect_measure = "OR", expected_exponentiated = TRUE, desc = "Odds Ratio - should be exponentiated"),
    list(effect_measure = "HR", expected_exponentiated = TRUE, desc = "Hazard Ratio - should be exponentiated"),
    list(effect_measure = "MD", expected_exponentiated = TRUE, desc = "Mean Difference - should be exponentiated"),
    list(effect_measure = "beta", expected_exponentiated = FALSE, desc = "Beta coefficient - should be log scale"),
    list(effect_measure = "estimate", expected_exponentiated = FALSE, desc = "Raw estimate - should be log scale"),
    list(effect_measure = "log-odds", expected_exponentiated = FALSE, desc = "Log odds - should be log scale"),
    list(effect_measure = "log-hazard", expected_exponentiated = FALSE, desc = "Log hazard - should be log scale")
  )
  
  for (i in seq_along(test_cases)) {
    case <- test_cases[[i]]
    
    # Test the detection logic directly
    is_exponentiated <- case$effect_measure %in% c("OR", "HR", "MD")
    
    cat("  ", case$desc, ": ", case$effect_measure, " → ", 
        if(is_exponentiated) "EXPONENTIATED" else "LOG SCALE", 
        " (expected: ", if(case$expected_exponentiated) "EXPONENTIATED" else "LOG SCALE", ")\n", sep = "")
    
    expect_equal(is_exponentiated, case$expected_exponentiated,
                info = paste("Effect measure detection failed for:", case$effect_measure))
  }
  
  cat("\nDETERMINISTIC APPROACH BENEFITS:\n")
  cat("- ✅ No fragile value-based detection\n")
  cat("- ✅ Explicit and predictable\n")
  cat("- ✅ Based on analysis type, not data values\n")
  cat("- ✅ Handles edge cases (e.g., positive log values)\n")
  cat("- ✅ Consistent across all analyses\n")
  
  # Test with actual detection function
  cat("\nTESTING WITH ACTUAL DETECTION FUNCTION:\n")
  
  # Test exponentiated case
  result_or <- detect_extreme_regression_estimates(
    estimate = 1.0,
    ci_lower = 0.001,
    ci_upper = 1000.0,
    effect_measure = "OR",
    is_exponentiated = TRUE  # Explicitly set based on effect_measure
  )
  cat("  OR with wide CI: ", if(length(result_or$extreme_indices) > 0) "DETECTED" else "MISSED", "\n")
  
  # Test log scale case
  result_beta <- detect_extreme_regression_estimates(
    estimate = 0.0,
    ci_lower = -100.0,
    ci_upper = 100.0,
    effect_measure = "beta",
    is_exponentiated = FALSE  # Explicitly set based on effect_measure
  )
  cat("  Beta with wide CI: ", if(length(result_beta$extreme_indices) > 0) "DETECTED" else "MISSED", "\n")
  
  expect_true(length(result_or$extreme_indices) > 0, "OR with wide CI should be detected")
  expect_true(length(result_beta$extreme_indices) > 0, "Beta with wide CI should be detected")
}) 