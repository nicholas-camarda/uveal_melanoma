# Test GEP Validation Analysis Implementation
# Author: Nicholas Camarda
# Description: Comprehensive testing of GEP validation functions to ensure they work correctly

# Clear environment and set up
rm(list = ls())
cat("=== Testing GEP Validation Implementation ===\n")

# Source the main configuration first
source("scripts/utils/analysis_config.R")

# Source required scripts in order
cat("Loading required scripts...\n")
source("scripts/data_helper/data_processing.R")
source("scripts/data_helper/data_utilities.R")
source("scripts/utils/output_utilities.R")
source("scripts/analysis/statistical_analysis.R")
source("scripts/analysis/gep_validation_analysis.R")

# Load required libraries (should already be loaded in main.R but just in case)
suppressMessages({
    library(tidyverse)
    library(survival)
    library(readxl)
    library(writexl)
    library(pROC)
    library(survcomp)
    library(rms)
    library(pec)
})

cat("Required scripts and libraries loaded successfully.\n\n")

# Create test output directory
test_output_dir <- "test_output/gep_validation_test"
dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

# Test 1: Load analytic dataset
cat("=== TEST 1: Loading analytic dataset ===\n")
tryCatch({
    if (file.exists("final_data/analytic dataset/uveal_melanoma_full_cohort.rds")) {
        test_data <- readRDS("final_data/analytic dataset/uveal_melanoma_full_cohort.rds")
        cat("✓ Successfully loaded full cohort data (n =", nrow(test_data), ")\n")
    } else {
        cat("✗ Analytic dataset not found. Running data processing...\n")
        
        # Quick data processing for testing
        cleaned_data <- load_and_clean_data(filename = INPUT_FILENAME)
        derived_data <- create_derived_variables(cleaned_data)
        factored_data <- prepare_factor_levels(derived_data)
        final_datasets <- apply_criteria(factored_data)
        save_cohorts(final_datasets)
        
        test_data <- final_datasets$uveal_melanoma_full_cohort
        cat("✓ Created and loaded full cohort data (n =", nrow(test_data), ")\n")
    }
}, error = function(e) {
    cat("✗ Error loading data:", e$message, "\n")
    stop("Cannot proceed without data")
})

# Set up global variables for testing
prefix <<- "test_"
output_dirs <<- create_output_structure(test_output_dir)

# Test 2: Check required GEP variables exist
cat("\n=== TEST 2: Checking GEP variable availability ===\n")
gep_vars_required <- c("biopsy1_gep", "biopsy1_gep_mfs", "biopsy1_gep_mss", 
                       "gep_class_simple", "expected_mfs_5yr", "expected_mfs_7yr", 
                       "expected_mfs_10yr", "expected_mss_5yr", "expected_mss_7yr", 
                       "expected_mss_10yr", "prame_status")

missing_gep_vars <- setdiff(gep_vars_required, names(test_data))
if (length(missing_gep_vars) > 0) {
    cat("✗ Missing GEP variables:", paste(missing_gep_vars, collapse = ", "), "\n")
    cat("Available GEP-related variables:\n")
    gep_available <- names(test_data)[grepl("gep|prame", names(test_data), ignore.case = TRUE)]
    cat("  ", paste(gep_available, collapse = ", "), "\n")
} else {
    cat("✓ All required GEP variables present\n")
}

# Check data availability for GEP analysis
gep_available_count <- test_data %>%
    filter(!is.na(biopsy1_gep_mfs), 
           !is.na(biopsy1_gep),
           biopsy1_gep != "Failed",
           biopsy1_gep != "Unknown") %>%
    nrow()

cat("GEP data available for", gep_available_count, "patients\n")

if (gep_available_count < 50) {
    cat("⚠ Warning: Limited GEP data for validation (n =", gep_available_count, ")\n")
} else {
    cat("✓ Sufficient GEP data for validation analysis\n")
}

# Test 3: Test individual GEP validation functions (if they exist)
cat("\n=== TEST 3: Testing individual GEP validation functions ===\n")

# Test function existence first
function_tests <- list(
    "assess_gep_missing_data" = exists("assess_gep_missing_data"),
    "calculate_observed_expected_mfs" = exists("calculate_observed_expected_mfs"),
    "perform_calibration_mfs" = exists("perform_calibration_mfs"),
    "perform_discrimination_mfs" = exists("perform_discrimination_mfs"),
    "perform_decision_curve_analysis_mfs" = exists("perform_decision_curve_analysis_mfs"),
    "perform_prame_augmented_analysis_mfs" = exists("perform_prame_augmented_analysis_mfs"),
    "create_mfs_validation_report" = exists("create_mfs_validation_report"),
    "save_mfs_validation_results" = exists("save_mfs_validation_results")
)

for (func_name in names(function_tests)) {
    if (function_tests[[func_name]]) {
        cat("✓", func_name, "exists\n")
    } else {
        cat("✗", func_name, "missing\n")
    }
}

# Test 4: Test main MFS validation function
cat("\n=== TEST 4: Testing MFS validation function ===\n")
tryCatch({
    # Test with minimal bootstrap iterations for speed
    cat("Testing MFS validation with reduced bootstrap iterations...\n")
    
    # Test with ALL configured timepoints and fewer iterations for speed
    test_mfs_result <- analyze_gep_mfs_validation(
        data = test_data,
        dataset_name = "Test Cohort",
        timepoints = GEP_VALIDATION_TIMEPOINTS,  # Test all configured timepoints (5, 7, 10)
        bootstrap_iterations = 10  # Minimal for testing
    )
    
    if (!is.null(test_mfs_result)) {
        cat("✓ MFS validation function executed successfully\n")
        cat("Result structure:\n")
        str(test_mfs_result, max.level = 2)
    } else {
        cat("✗ MFS validation returned NULL\n")
    }
    
}, error = function(e) {
    cat("✗ Error in MFS validation:", e$message, "\n")
    cat("This suggests missing helper functions or data issues\n")
})

# Test 5: Test MSS validation function
cat("\n=== TEST 5: Testing MSS validation function ===\n")

mss_validation_result <- analyze_gep_mss_validation(
  data = test_data,
  dataset_name = "Test Cohort",
  timepoints = GEP_VALIDATION_TIMEPOINTS,
  bootstrap_iterations = 10  # keep small for test speed
)

if (!is.null(mss_validation_result$standard_validation)) {
  cat("✓ MSS validation returned standard results\n")
} else {
  cat("✗ MSS standard validation missing\n")
}

# Test 6: Check output files were created
cat("\n=== TEST 6: Checking output file creation ===\n")
output_files <- list.files(test_output_dir, recursive = TRUE, pattern = "\\.(xlsx|rds|txt|png)$")
if (length(output_files) > 0) {
    cat("✓ Output files created:\n")
    for (file in output_files) {
        cat("  ", file, "\n")
    }
} else {
    cat("✗ No output files created\n")
}

# Summary
cat("\n=== TEST SUMMARY ===\n")
cat("Test completed at:", format(Sys.time()), "\n")
cat("Test output directory:", test_output_dir, "\n")
cat("Check above for specific issues that need to be addressed.\n")
cat("=== TEST 7: Direct FGR coefficient extraction test ===\n")
cat("Testing FGR coefficient extraction directly...\n")

# Test the competing risk part for 5-year MSS
mss_data <- test_data %>%
  filter(!is.na(expected_mss_5yr), !is.na(tt_death_months), !is.na(death_event)) %>%
  mutate(
    time_years = tt_death_months / 12,
    predicted_risk = 1 - expected_mss_5yr,
    status = case_when(
      death_event == 1 & !is.na(cod) & str_detect(tolower(cod), "melanoma|mets|liver|cancer") ~ 1,
      death_event == 1 ~ 2,
      TRUE ~ 0
    )
  ) %>%
  filter(time_years > 0, !is.na(predicted_risk), predicted_risk > 0, predicted_risk < 1)

cat("FGR test data: n =", nrow(mss_data), "\n")
cat("Events: melanoma =", sum(mss_data$status == 1), ", other =", sum(mss_data$status == 2), ", censored =", sum(mss_data$status == 0), "\n")

if (nrow(mss_data) > 10 && sum(mss_data$status == 1) >= 5) {
  tryCatch({
    # Fit FGR model directly
    fgr_model <- riskRegression::FGR(Hist(time_years, status) ~ predicted_risk, data = mss_data, cause = 1)
    cat("✓ FGR model fitted successfully\n")
    
    # Test coefficient extraction using our implementation
    fgr_coef <- NA
    fgr_se <- NA
    
    if ("crrFit" %in% names(fgr_model)) {
      crr_model <- fgr_model$crrFit
      if (!is.null(crr_model$coef) && length(crr_model$coef) > 0) {
        fgr_coef <- as.numeric(crr_model$coef[1])
        if (!is.null(crr_model$var) && is.matrix(crr_model$var) && nrow(crr_model$var) > 0) {
          fgr_se <- sqrt(crr_model$var[1, 1])
        }
      }
    } else {
      # Fallback to summary method
      fgr_summary <- summary(fgr_model)
      if (!is.null(fgr_summary$coefficients) && nrow(fgr_summary$coefficients) > 0) {
        fgr_coef <- fgr_summary$coefficients[1, "coef"]
        fgr_se <- fgr_summary$coefficients[1, "se(coef)"]
      }
    }
    
    if (!is.na(fgr_coef) && !is.na(fgr_se)) {
      fgr_shr <- exp(fgr_coef)
      fgr_ci_lower <- exp(fgr_coef - 1.96 * fgr_se)
      fgr_ci_upper <- exp(fgr_coef + 1.96 * fgr_se)
      fgr_p_value <- 2 * (1 - pnorm(abs(fgr_coef / fgr_se)))
      
      cat("✓ FGR coefficient extraction successful:\n")
      cat("  Coefficient:", round(fgr_coef, 4), "\n")
      cat("  SE:", round(fgr_se, 4), "\n")
      cat("  SHR:", round(fgr_shr, 3), "\n")
      cat("  95% CI:", round(fgr_ci_lower, 3), "-", round(fgr_ci_upper, 3), "\n")
      cat("  P-value:", round(fgr_p_value, 4), "\n")
    } else {
      cat("✗ FGR coefficient extraction failed\n")
      cat("  Coefficient extracted:", !is.na(fgr_coef), "\n")
      cat("  SE extracted:", !is.na(fgr_se), "\n")
    }
    
  }, error = function(e) {
    cat("✗ FGR test failed:", e$message, "\n")
  })
} else {
  cat("Insufficient data for FGR test\n")
}

cat("=== End of Testing ===\n")

 