# Validation Utilities
# Author: Nicholas Camarda
# Description: Data validation and integrity checking functions

# =============================================================================
# DATA VALIDATION FUNCTIONS
# =============================================================================

#' Comprehensive validation of cohort assignments and data integrity
#'
#' This function performs critical validation checks to prevent bugs like the 
#' dataset naming issue that was discovered. It should be called after cohort
#' creation to ensure data integrity.
#'
#' @param cohort_list List of cohort datasets from apply_criteria()
#' @return TRUE if all validations pass, FALSE otherwise with detailed error messages
validate_cohort_integrity <- function(cohort_list) {
    log_enhanced("=== STARTING COMPREHENSIVE COHORT VALIDATION ===", level = "SECTION")
    validation_passed <- TRUE
    
    # Check 1: Verify expected cohort names exist
    expected_names <- c("uveal_melanoma_full_cohort", "uveal_melanoma_restricted_cohort", "uveal_melanoma_gksrs_only_cohort")
    actual_names <- names(cohort_list)
    
    if (!all(expected_names %in% actual_names)) {
        missing_names <- setdiff(expected_names, actual_names)
        log_enhanced(sprintf("VALIDATION FAILED: Missing expected cohort names: %s", paste(missing_names, collapse = ", ")), level = "ERROR")
        validation_passed <- FALSE
    } else {
        log_enhanced("✓ All expected cohort names present", level = "INFO")
    }
    
    # Only proceed with detailed validation if all expected cohorts exist
    if (!validation_passed) {
        return(validation_passed)
    }
    
    # Check 2: Verify sample size relationships
    n_full <- nrow(cohort_list$uveal_melanoma_full_cohort)
    n_restricted <- nrow(cohort_list$uveal_melanoma_restricted_cohort)
    n_gksrs_only <- nrow(cohort_list$uveal_melanoma_gksrs_only_cohort)
    
    log_enhanced(sprintf("Sample sizes - Full: %d, Restricted: %d, GKSRS-only: %d", n_full, n_restricted, n_gksrs_only), level = "INFO")
    
    # Full cohort should be largest
    if (n_full < n_restricted || n_full < n_gksrs_only) {
        log_enhanced("VALIDATION FAILED: Full cohort should be largest", level = "ERROR")
        validation_passed <- FALSE
    } else {
        log_enhanced("✓ Full cohort is largest as expected", level = "INFO")
    }
    
    # Restricted + GKSRS-only should approximately equal full (allowing for exclusions)
    total_subsets <- n_restricted + n_gksrs_only
    if (abs(n_full - total_subsets) > 10) { # Allow some tolerance for exclusions
        log_enhanced(sprintf("VALIDATION WARNING: Full cohort (%d) vs sum of subsets (%d) differs by %d patients", 
                            n_full, total_subsets, abs(n_full - total_subsets)), level = "WARN")
    } else {
        log_enhanced("✓ Cohort size relationships are reasonable", level = "INFO")
    }
    
    # Check 3: Validate GEP variables were created correctly (Objective 4)
    log_enhanced("Validating GEP-related variables for Objective 4", level = "INFO")
    gep_validation_result <- validate_gep_variables_with_report(cohort_list$uveal_melanoma_full_cohort)
    gep_validation_passed <- gep_validation_result$validation_passed
    if (!gep_validation_passed) {
        log_enhanced("VALIDATION FAILED: GEP variables not created correctly", level = "ERROR")
        validation_passed <- FALSE
    } else {
        log_enhanced("✓ GEP variables validated successfully", level = "INFO")
    }
    
    # Check 4: Verify cohort definitions match consort_group assignments
    restricted_data <- cohort_list$uveal_melanoma_restricted_cohort
    gksrs_only_data <- cohort_list$uveal_melanoma_gksrs_only_cohort
    
    # All restricted cohort patients should have consort_group == "eligible_both"
    if (any(restricted_data$consort_group != "eligible_both")) {
        wrong_consort <- table(restricted_data$consort_group)
        log_enhanced(sprintf("VALIDATION FAILED: Restricted cohort contains wrong consort_group: %s", 
                            paste(names(wrong_consort), "=", wrong_consort, collapse = ", ")), level = "ERROR")
        validation_passed <- FALSE
    } else {
        log_enhanced("✓ Restricted cohort contains only eligible_both patients", level = "INFO")
    }
    
    # All GKSRS-only cohort patients should have consort_group == "gksrs_only"
    if (any(gksrs_only_data$consort_group != "gksrs_only")) {
        wrong_consort <- table(gksrs_only_data$consort_group)
        log_enhanced(sprintf("VALIDATION FAILED: GKSRS-only cohort contains wrong consort_group: %s", 
                            paste(names(wrong_consort), "=", wrong_consort, collapse = ", ")), level = "ERROR")
        validation_passed <- FALSE
    } else {
        log_enhanced("✓ GKSRS-only cohort contains only gksrs_only patients", level = "INFO")
    }
    
    # Check 4: Verify eligibility criteria are correctly applied
    # Restricted cohort: diameter ≤ 20, height ≤ 10, no optic nerve
    restricted_violations <- restricted_data %>%
        filter(
            initial_tumor_diameter > TUMOR_DIAMETER_THRESHOLD |
            initial_tumor_height > TUMOR_HEIGHT_THRESHOLD |
            optic_nerve == "Yes"
        )
    
    if (nrow(restricted_violations) > 0) {
        log_enhanced(sprintf("VALIDATION FAILED: %d patients in restricted cohort violate eligibility criteria", nrow(restricted_violations)), level = "ERROR")
        print(restricted_violations %>% select(id, initial_tumor_diameter, initial_tumor_height, optic_nerve))
        validation_passed <- FALSE
    } else {
        log_enhanced("✓ Restricted cohort eligibility criteria correctly applied", level = "INFO")
    }
    
    # GKSRS-only cohort: diameter > 20 OR height > 10 OR optic nerve involvement
    gksrs_only_should_qualify <- gksrs_only_data %>%
        filter(
            initial_tumor_diameter > TUMOR_DIAMETER_THRESHOLD |
            initial_tumor_height > TUMOR_HEIGHT_THRESHOLD |
            optic_nerve == "Yes"
        )
    
    if (nrow(gksrs_only_should_qualify) != nrow(gksrs_only_data)) {
        log_enhanced(sprintf("VALIDATION FAILED: %d/%d patients in GKSRS-only cohort don't meet ineligibility criteria", 
                            nrow(gksrs_only_data) - nrow(gksrs_only_should_qualify), nrow(gksrs_only_data)), level = "ERROR")
        validation_passed <- FALSE
    } else {
        log_enhanced("✓ GKSRS-only cohort ineligibility criteria correctly applied", level = "INFO")
    }
    
    # Check 5: Verify no patient overlap between restricted and GKSRS-only
    overlap_patients <- intersect(restricted_data$id, gksrs_only_data$id)
    if (length(overlap_patients) > 0) {
        log_enhanced(sprintf("VALIDATION FAILED: %d patients appear in both restricted and GKSRS-only cohorts: %s", 
                            length(overlap_patients), paste(overlap_patients, collapse = ", ")), level = "ERROR")
        validation_passed <- FALSE
    } else {
        log_enhanced("✓ No patient overlap between cohorts", level = "INFO")
    }
    
    # Check 6: Verify treatment assignments make sense
    # Check treatment distribution in each cohort
    for (cohort_name in names(cohort_list)) {
        cohort_data <- cohort_list[[cohort_name]]
        treatment_dist <- table(cohort_data$treatment_group, useNA = "ifany")
        log_enhanced(sprintf("Treatment distribution in %s: %s", 
                            gsub("uveal_melanoma_", "", cohort_name),
                            paste(names(treatment_dist), "=", treatment_dist, collapse = ", ")), level = "INFO")
        
        # All patients should have a treatment assignment
        if (any(is.na(cohort_data$treatment_group))) {
            log_enhanced(sprintf("VALIDATION WARNING: %d patients in %s have missing treatment_group", 
                                sum(is.na(cohort_data$treatment_group)), cohort_name), level = "WARN")
        }
    }
    
    # Final validation summary
    if (validation_passed) {
        log_enhanced("=== COHORT VALIDATION PASSED: All checks successful ===", level = "SECTION")
    } else {
        log_enhanced("=== COHORT VALIDATION FAILED: See errors above ===", level = "SECTION")
    }
    
    return(validation_passed)
}

#' Validate factor level consistency throughout the analysis pipeline
#'
#' Ensures that factor levels remain consistent from data processing through analysis
#' phases. This is critical for maintaining consistent reference groups and interpretation.
#'
#' @param cohort_list List of cohort datasets from apply_criteria()
#' @param phase Character string indicating analysis phase ("data_processing", "analysis", etc.)
#' @return TRUE if all factor level validations pass, FALSE otherwise with detailed error messages
validate_factor_level_consistency <- function(cohort_list, phase = "data_processing") {
    log_enhanced("=== STARTING FACTOR LEVEL CONSISTENCY VALIDATION ===", level = "SECTION")
    validation_passed <- TRUE
    
    # Define expected factor configurations
    expected_factors <- list(
        treatment_group = list(
            levels = TREATMENT_FACTOR_LEVELS,
            reference = TREATMENT_REFERENCE_LEVEL,
            comparison = TREATMENT_COMPARISON_LEVEL,
            critical = TRUE
        ),
        recurrence1 = list(
            levels = YN_DISPLAY_LABELS,
            reference = YN_DISPLAY_LABELS[1],  # "No" 
            critical = TRUE
        ),
        sex = list(
            levels = SEX_FACTOR_LEVELS,
            reference = SEX_FACTOR_LEVELS[1],
            critical = FALSE
        ),
        optic_nerve = list(
            levels = YN_DISPLAY_LABELS,  # c("No", "Yes")
            reference = YN_DISPLAY_LABELS[1],  # "No" 
            critical = TRUE
        )
    )
    
    # Check each cohort for factor level consistency
    for (cohort_name in names(cohort_list)) {
        cohort_data <- cohort_list[[cohort_name]]
        cohort_display_name <- gsub("uveal_melanoma_", "", cohort_name)
        
        log_enhanced(sprintf("Validating factor levels for %s", cohort_display_name), level = "INFO")
        
        for (factor_name in names(expected_factors)) {
            expected_config <- expected_factors[[factor_name]]
            
            # Check if factor exists in data
            if (!factor_name %in% names(cohort_data)) {
                if (expected_config$critical) {
                    log_enhanced(sprintf("VALIDATION FAILED: Critical factor '%s' missing from %s", 
                                        factor_name, cohort_display_name), level = "ERROR")
                    validation_passed <- FALSE
                } else {
                    log_enhanced(sprintf("VALIDATION WARNING: Optional factor '%s' missing from %s", 
                                        factor_name, cohort_display_name), level = "WARN")
                }
                next
            }
            
            factor_col <- cohort_data[[factor_name]]
            
            # Check if variable is actually a factor
            if (!is.factor(factor_col)) {
                log_enhanced(sprintf("VALIDATION FAILED: '%s' is not a factor in %s (class: %s)", 
                                    factor_name, cohort_display_name, class(factor_col)[1]), level = "ERROR")
                validation_passed <- FALSE
                next
            }
            
            # Check factor levels
            actual_levels <- levels(factor_col)
            expected_levels <- expected_config$levels
            
            if (!identical(actual_levels, expected_levels)) {
                log_enhanced(sprintf("VALIDATION FAILED: Factor levels mismatch for '%s' in %s", 
                                    factor_name, cohort_display_name), level = "ERROR")
                log_enhanced(sprintf("  Expected: %s", paste(expected_levels, collapse = ", ")), level = "ERROR")
                log_enhanced(sprintf("  Actual:   %s", paste(actual_levels, collapse = ", ")), level = "ERROR")
                validation_passed <- FALSE
            } else {
                log_enhanced(sprintf("✓ Factor levels correct for '%s' in %s", factor_name, cohort_display_name), level = "INFO")
            }
            
            # Check reference level (first level)
            if (length(actual_levels) > 0 && actual_levels[1] != expected_config$reference) {
                log_enhanced(sprintf("VALIDATION FAILED: Reference level mismatch for '%s' in %s", 
                                    factor_name, cohort_display_name), level = "ERROR")
                log_enhanced(sprintf("  Expected reference: %s", expected_config$reference), level = "ERROR")
                log_enhanced(sprintf("  Actual reference:   %s", actual_levels[1]), level = "ERROR")
                validation_passed <- FALSE
            }
            
            # Special validation for treatment_group (most critical)
            if (factor_name == "treatment_group") {
                # Check that both treatment groups are present
                unique_values <- unique(as.character(factor_col[!is.na(factor_col)]))
                if (length(unique_values) < 2) {
                    log_enhanced(sprintf("VALIDATION WARNING: Only %d treatment group(s) present in %s: %s", 
                                        length(unique_values), cohort_display_name, 
                                        paste(unique_values, collapse = ", ")), level = "WARN")
                }
                
                # Check sample sizes per treatment group
                treatment_dist <- table(factor_col, useNA = "ifany")
                log_enhanced(sprintf("Treatment distribution in %s: %s", 
                                    cohort_display_name,
                                    paste(names(treatment_dist), "=", treatment_dist, collapse = ", ")), level = "INFO")
                
                # Validate that reference group is Plaque (expected for our analysis)
                if (actual_levels[1] != "Plaque") {
                    log_enhanced(sprintf("VALIDATION FAILED: Treatment reference group should be 'Plaque', got '%s' in %s", 
                                        actual_levels[1], cohort_display_name), level = "ERROR")
                    validation_passed <- FALSE
                }
            }
        }
    }
    
    # Cross-cohort consistency check
    log_enhanced("Checking factor level consistency across cohorts", level = "INFO")
    for (factor_name in names(expected_factors)) {
        if (!expected_factors[[factor_name]]$critical) next
        
        cohort_levels <- list()
        for (cohort_name in names(cohort_list)) {
            if (factor_name %in% names(cohort_list[[cohort_name]])) {
                cohort_levels[[cohort_name]] <- levels(cohort_list[[cohort_name]][[factor_name]])
            }
        }
        
        # Check that all cohorts have identical factor levels
        if (length(cohort_levels) > 1) {
            first_levels <- cohort_levels[[1]]
            for (i in 2:length(cohort_levels)) {
                if (!identical(first_levels, cohort_levels[[i]])) {
                    log_enhanced(sprintf("VALIDATION FAILED: Factor levels for '%s' differ between cohorts", factor_name), level = "ERROR")
                    log_enhanced(sprintf("  %s: %s", names(cohort_levels)[1], paste(first_levels, collapse = ", ")), level = "ERROR")
                    log_enhanced(sprintf("  %s: %s", names(cohort_levels)[i], paste(cohort_levels[[i]], collapse = ", ")), level = "ERROR")
                    validation_passed <- FALSE
                }
            }
        }
    }
    
    # Validation summary
    if (validation_passed) {
        log_enhanced("=== FACTOR LEVEL VALIDATION PASSED: All factor levels consistent ===", level = "SECTION")
        log_enhanced(sprintf("✓ Treatment reference group: %s", TREATMENT_REFERENCE_LEVEL), level = "INFO")
        log_enhanced(sprintf("✓ Treatment comparison group: %s", TREATMENT_COMPARISON_LEVEL), level = "INFO")
        log_enhanced("✓ All critical factor levels match expected configuration", level = "INFO")
    } else {
        log_enhanced("=== FACTOR LEVEL VALIDATION FAILED: See errors above ===", level = "SECTION")
        log_enhanced("⚠️  CRITICAL: Factor level inconsistencies detected.", level = "ERROR")
        log_enhanced("⚠️  This will cause incorrect model results and interpretation.", level = "ERROR")
    }
    
    return(validation_passed)
}

#' Comprehensive Data Processing Validation
#'
#' Performs systematic validation of the entire data processing pipeline
#' to ensure all steps completed successfully and data integrity is maintained.
#' Based on the data processing pipeline: load → derive → factor → cohort → collapse → save.
#'
#' @param data Data frame or list of data frames (cohorts) to validate
#' @return Logical indicating if all validations passed
generate_validation_report <- function(data) {
    log_enhanced("=== COMPREHENSIVE DATA PROCESSING VALIDATION ===", level = "SECTION")
    
    # If data is a list (multiple cohorts), validate all cohorts systematically
    if (is.list(data) && !is.data.frame(data)) {
        validation_passed <- TRUE
        
        # Phase 1: Individual cohort validation
        log_enhanced("Phase 1: Individual Cohort Validation", level = "INFO")
        for (cohort_name in names(data)) {
            log_enhanced(sprintf("Validating cohort: %s", cohort_name), level = "INFO", indent = 1)
            cohort_validation <- validate_single_cohort_comprehensive(data[[cohort_name]], cohort_name)
            if (!cohort_validation) {
                validation_passed <- FALSE
            }
        }
        
        # Phase 2: Cross-cohort validation
        log_enhanced("Phase 2: Cross-Cohort Validation", level = "INFO")
        cross_cohort_validation <- validate_cross_cohort_consistency(data)
        if (!cross_cohort_validation) {
            validation_passed <- FALSE
        }
        
        # Phase 3: File system validation
        log_enhanced("Phase 3: File System Validation", level = "INFO")
        file_validation <- validate_processed_files_exist(data)
        if (!file_validation) {
            validation_passed <- FALSE
        }
        
        # Final validation summary
        if (validation_passed) {
            log_enhanced("=== ALL DATA PROCESSING VALIDATIONS PASSED ===", level = "SUCCESS")
        } else {
            log_enhanced("=== DATA PROCESSING VALIDATION FAILED - SEE ERRORS ABOVE ===", level = "ERROR")
        }
        
        return(validation_passed)
    } else {
        # Single dataset validation
        return(validate_single_cohort_comprehensive(data, "single_dataset"))
    }
}

#' Comprehensive Single Cohort Validation
#'
#' Validates all aspects of a single cohort based on the data processing pipeline
#'
#' @param data Data frame to validate
#' @param cohort_name Name of the cohort for logging
#' @return Logical indicating if validation passed
validate_single_cohort_comprehensive <- function(data, cohort_name) {
    validation_passed <- TRUE
    
    log_enhanced(sprintf("Comprehensive validation for %s (%d rows, %d columns)", 
                        cohort_name, nrow(data), ncol(data)), level = "INFO", indent = 1)
    
    # 1. DATA STRUCTURE VALIDATION
    log_enhanced("1. Data Structure Validation", level = "INFO", indent = 2)
    
    # Basic structure checks
    if (nrow(data) == 0) {
        log_enhanced("VALIDATION FAILED: Empty dataset", level = "ERROR", indent = 3)
        validation_passed <- FALSE
    }
    
    if (ncol(data) < MINIMUM_COLUMNS_AFTER_PROCESSING) {  # Should have many variables after processing
        log_enhanced(sprintf("VALIDATION WARNING: Few variables (%d columns) - expected more after processing", ncol(data)), level = "WARN", indent = 3)
    }
    
    # Duplicate check
    if (sum(duplicated(data)) > 0) {
        log_enhanced(sprintf("VALIDATION WARNING: %d duplicate rows found", sum(duplicated(data))), level = "WARN", indent = 3)
    }
    
    # 2. CRITICAL VARIABLE VALIDATION
    log_enhanced("2. Critical Variable Validation", level = "INFO", indent = 2)
    
    # Essential variables that must exist
    missing_critical <- setdiff(CRITICAL_VARIABLES, names(data))
    if (length(missing_critical) > 0) {
        log_enhanced(sprintf("VALIDATION FAILED: Missing critical variables: %s", 
                            paste(missing_critical, collapse = ", ")), level = "ERROR", indent = 3)
        validation_passed <- FALSE
    }
    
    # 3. DERIVED VARIABLE VALIDATION
    log_enhanced("3. Derived Variable Validation", level = "INFO", indent = 2)
    
    # Check that key derived variables were created
    missing_derived <- setdiff(DERIVED_VARIABLES, names(data))
    if (length(missing_derived) > 0) {
        log_enhanced(sprintf("VALIDATION FAILED: Missing derived variables: %s", 
                            paste(missing_derived, collapse = ", ")), level = "ERROR", indent = 3)
        validation_passed <- FALSE
    }
    
    # Validate derived variable types and ranges
    if ("age_at_diagnosis" %in% names(data)) {
        if (!is.numeric(data$age_at_diagnosis)) {
            log_enhanced("VALIDATION FAILED: age_at_diagnosis is not numeric", level = "ERROR", indent = 3)
            validation_passed <- FALSE
        }
        if (any(data$age_at_diagnosis < 0, na.rm = TRUE)) {
            log_enhanced("VALIDATION FAILED: Negative ages found", level = "ERROR", indent = 3)
            validation_passed <- FALSE
        }
    }
    
    # 4. FACTOR LEVEL VALIDATION
    log_enhanced("4. Factor Level Validation", level = "INFO", indent = 2)
    
    # Critical factors that should be factors
    for (factor_name in CRITICAL_FACTORS) {
        if (factor_name %in% names(data)) {
            if (!is.factor(data[[factor_name]])) {
                log_enhanced(sprintf("VALIDATION FAILED: %s is not a factor", factor_name), level = "ERROR", indent = 3)
                validation_passed <- FALSE
            } else if (length(levels(data[[factor_name]])) == 0) {
                log_enhanced(sprintf("VALIDATION FAILED: %s has no levels", factor_name), level = "ERROR", indent = 3)
                validation_passed <- FALSE
            }
        }
    }
    
    # Treatment group specific validation
    if ("treatment_group" %in% names(data)) {
        treatment_dist <- table(data$treatment_group, useNA = "ifany")
        if (length(treatment_dist) < 2) {
            log_enhanced("VALIDATION FAILED: Insufficient treatment groups", level = "ERROR", indent = 3)
            validation_passed <- FALSE
        }
        
        # Check for expected treatment levels
        actual_levels <- levels(data$treatment_group)
        if (!all(EXPECTED_TREATMENT_LEVELS %in% actual_levels)) {
            log_enhanced(sprintf("VALIDATION FAILED: Unexpected treatment levels. Expected: %s, Got: %s", 
                                paste(EXPECTED_TREATMENT_LEVELS, collapse = ", "), 
                                paste(actual_levels, collapse = ", ")), level = "ERROR", indent = 3)
            validation_passed <- FALSE
        }
    }
    
    # 5. GEP VARIABLE VALIDATION
    log_enhanced("5. GEP Variable Validation", level = "INFO", indent = 2)
    
    if ("biopsy1_gep" %in% names(data)) {
        gep_validation <- validate_gep_variables_with_report(data)
        if (!gep_validation$validation_passed) {
            log_enhanced("VALIDATION FAILED: GEP variables failed validation", level = "ERROR", indent = 3)
            validation_passed <- FALSE
        }
        
        # Check for derived GEP variables
        missing_gep_derived <- setdiff(GEP_DERIVED_VARIABLES, names(data))
        if (length(missing_gep_derived) > 0) {
            log_enhanced(sprintf("VALIDATION FAILED: Missing derived GEP variables: %s", 
                                paste(missing_gep_derived, collapse = ", ")), level = "ERROR", indent = 3)
            validation_passed <- FALSE
        }
    }
    
    # 6. DATA QUALITY VALIDATION
    log_enhanced("6. Data Quality Validation", level = "INFO", indent = 2)
    
    # Check for excessive missing data in critical variables
    for (var_name in MISSING_DATA_CHECK_VARIABLES) {
        if (var_name %in% names(data)) {
            missing_pct <- mean(is.na(data[[var_name]])) * 100
            if (missing_pct > MAXIMUM_MISSING_DATA_PERCENTAGE) {
                log_enhanced(sprintf("VALIDATION WARNING: High missing data in %s (%.1f%%)", var_name, missing_pct), level = "WARN", indent = 3)
            }
        }
    }
    
    # Check for logical inconsistencies
    if (all(c("recurrence1", "tt_recurrence_months") %in% names(data))) {
        # Patients with recurrence should have positive time to recurrence
        inconsistent_recurrence <- data$recurrence1 == "Y" & data$tt_recurrence_months <= 0
        if (any(inconsistent_recurrence, na.rm = TRUE)) {
            log_enhanced(sprintf("VALIDATION WARNING: %d patients with recurrence have non-positive time to recurrence", 
                                sum(inconsistent_recurrence, na.rm = TRUE)), level = "WARN", indent = 3)
        }
    }
    
    # 7. COHORT-SPECIFIC VALIDATION
    log_enhanced("7. Cohort-Specific Validation", level = "INFO", indent = 2)
    
    # Validate cohort size expectations
    if (cohort_name %in% names(EXPECTED_COHORT_SIZES)) {
        expected_size <- EXPECTED_COHORT_SIZES[[cohort_name]]
        if (nrow(data) < expected_size) {
            log_enhanced(sprintf("VALIDATION WARNING: %s smaller than expected (%d < %d)", cohort_name, nrow(data), expected_size), level = "WARN", indent = 3)
        }
    }
    
    # Final validation result
    if (validation_passed) {
        log_enhanced(sprintf("✓ All validations passed for %s", cohort_name), level = "INFO", indent = 1)
    } else {
        log_enhanced(sprintf("✗ Validation failed for %s", cohort_name), level = "ERROR", indent = 1)
    }
    
    return(validation_passed)
}

#' Validate Cross-Cohort Consistency
#'
#' @param cohort_list List of cohorts to validate
#' @return Logical indicating if validation passed
validate_cross_cohort_consistency <- function(cohort_list) {
    log_enhanced("Validating cross-cohort consistency", level = "INFO", indent = 1)
    
    # Use existing validation functions
    cohort_integrity <- validate_cohort_integrity(cohort_list)
    factor_consistency <- validate_factor_level_consistency(cohort_list, phase = "data_processing")
    
    return(cohort_integrity && factor_consistency)
}

#' Validate Processed Files Exist
#'
#' @param cohort_list List of cohorts to validate
#' @return Logical indicating if validation passed
validate_processed_files_exist <- function(cohort_list) {
    log_enhanced("Validating processed files exist", level = "INFO", indent = 1)
    
    validation_passed <- TRUE
    
    # Check that RDS files exist for each cohort
    for (cohort_name in names(cohort_list)) {
        rds_file <- file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".rds"))
        if (!file.exists(rds_file)) {
            log_enhanced(sprintf("VALIDATION FAILED: RDS file missing for %s: %s", cohort_name, rds_file), level = "ERROR", indent = 2)
            validation_passed <- FALSE
        }
        
        excel_file <- file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".xlsx"))
        if (!file.exists(excel_file)) {
            log_enhanced(sprintf("VALIDATION FAILED: Excel file missing for %s: %s", cohort_name, excel_file), level = "ERROR", indent = 2)
            validation_passed <- FALSE
        }
    }
    
    # Check that other_map.rds exists
    other_map_file <- file.path(PROCESSED_DATA_DIR, "other_map.rds")
    if (!file.exists(other_map_file)) {
        log_enhanced("VALIDATION FAILED: other_map.rds file missing", level = "ERROR", indent = 2)
        validation_passed <- FALSE
    }
    
    if (validation_passed) {
        log_enhanced("✓ All processed files exist", level = "INFO", indent = 2)
    }
    
    return(validation_passed)
} 