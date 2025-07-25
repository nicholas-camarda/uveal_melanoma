# Test PFS-2 Extreme Value Filtering
# This script tests if our extreme value filtering works on the actual analysis
# that was producing the 703 million HR values

# Source required scripts
source("scripts/utils/all_helper_functions.R")

# Set required global variables
VERBOSE <- TRUE
SHOW_ALL_PVALUES <- FALSE

cat("=== TESTING PFS-2 EXTREME VALUE FILTERING ===\n")

library(readxl)
library(survival)
library(gtsummary)

# Load the full cohort data
full_data <- read_excel("final_data/Analytic Dataset/uveal_melanoma_full_cohort.xlsx")
cat("Loaded full cohort data:", nrow(full_data), "rows\n")

# Create PFS-2 subset (patients who had recurrence and got second treatment)
pfs2_data <- full_data %>%
  filter(!is.na(recurrence1_treatment_clean)) %>%
  filter(!is.na(tt_pfs2_months)) %>%
  filter(!is.na(pfs2_event))

cat("PFS-2 subset:", nrow(pfs2_data), "rows\n")

if(nrow(pfs2_data) > 0) {
  cat("Treatment groups in PFS-2 data:\n")
  print(table(pfs2_data$recurrence1_treatment_clean, useNA = "ifany"))
  
  cat("Events by treatment group:\n")
  print(table(pfs2_data$recurrence1_treatment_clean, pfs2_data$pfs2_event, useNA = "ifany"))
  
  # This should trigger the extreme value filtering
  cat("\nRunning Cox regression analysis...\n")
  
  # Set output directory for test (create simple directory structure)
  if (!exists("output_dirs")) {
    output_dirs <- list(obj3_pfs2 = "test_output")
  } else {
    output_dirs$obj3_pfs2 <- "test_output"
  }
  prefix <- "test_"
  
  # Create output directory
  dir.create("test_output", showWarnings = FALSE)
  
  tryCatch({
    results <- analyze_time_to_event_outcomes(
      data = pfs2_data,
      time_var = "tt_pfs2_months",
      event_var = "pfs2_event",
      group_var = "recurrence1_treatment_clean",
      confounders = c("age_at_diagnosis", "sex", "optic_nerve"),
      ylab = "PFS-2 Test",
      dataset_name = "test_pfs2"
    )
    
    cat("✓ Analysis completed successfully\n")
    cat("Check test_output/ for:\n")
    cat("  - test_PFS-2_Test_cox.html (should show filtered results)\n")
    cat("  - test_PFS-2_Test_cox_diagnostics.xlsx (should show excluded extreme values)\n")
    
  }, error = function(e) {
    cat("✗ Analysis failed:", e$message, "\n")
  })
  
} else {
  cat("No PFS-2 data available for testing\n")
} 