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
    logger::log_info("=== STARTING COMPREHENSIVE COHORT VALIDATION ===")
    validation_passed <- TRUE

    # Check 1: Verify expected cohort names exist
    expected_names <- c("uveal_melanoma_full_cohort", "uveal_melanoma_restricted_cohort", "uveal_melanoma_gksrs_only_cohort")
    actual_names <- names(cohort_list)

    if (!all(expected_names %in% actual_names)) {
        missing_names <- setdiff(expected_names, actual_names)
        logger::log_error(sprintf("VALIDATION FAILED: Missing expected cohort names: %s", paste(missing_names, collapse = ", ")))
        validation_passed <- FALSE
    } else {
        logger::log_info("✓ All expected cohort names present")
    }

    # Only proceed with detailed validation if all expected cohorts exist
    if (!validation_passed) {
        return(validation_passed)
    }

    # Check 2: Verify sample size relationships
    n_full <- nrow(cohort_list$uveal_melanoma_full_cohort)
    n_restricted <- nrow(cohort_list$uveal_melanoma_restricted_cohort)
    n_gksrs_only <- nrow(cohort_list$uveal_melanoma_gksrs_only_cohort)

    logger::log_info(sprintf("Sample sizes - Full: %d, Restricted: %d, GKSRS-only: %d", n_full, n_restricted, n_gksrs_only))

    # Full cohort should be largest
    if (n_full < n_restricted || n_full < n_gksrs_only) {
        logger::log_error("VALIDATION FAILED: Full cohort should be largest")
        validation_passed <- FALSE
    } else {
        logger::log_info("✓ Full cohort is largest as expected")
    }

    # Restricted + GKSRS-only should approximately equal full (allowing for exclusions)
    total_subsets <- n_restricted + n_gksrs_only
    if (abs(n_full - total_subsets) > 10) { # Allow some tolerance for exclusions
        logger::log_warn(sprintf(
            "VALIDATION WARNING: Full cohort (%d) vs sum of subsets (%d) differs by %d patients",
            n_full, total_subsets, abs(n_full - total_subsets)
        ))
    } else {
        logger::log_info("✓ Cohort size relationships are reasonable")
    }

    # Check 3: GEP variables will be validated in the comprehensive validation pipeline
    logger::log_info("✓ GEP variables will be validated in comprehensive validation pipeline")

    # Check 4: Verify cohort definitions match consort_group assignments
    restricted_data <- cohort_list$uveal_melanoma_restricted_cohort
    gksrs_only_data <- cohort_list$uveal_melanoma_gksrs_only_cohort

    # All restricted cohort patients should have consort_group == "eligible_both"
    if (any(restricted_data$consort_group != "eligible_both")) {
        wrong_consort <- table(restricted_data$consort_group)
        logger::log_error(sprintf(
            "VALIDATION FAILED: Restricted cohort contains wrong consort_group: %s",
            paste(names(wrong_consort), "=", wrong_consort, collapse = ", ")
        ))
        validation_passed <- FALSE
    } else {
        logger::log_info("✓ Restricted cohort contains only eligible_both patients")
    }

    # All GKSRS-only cohort patients should have consort_group == "gksrs_only"
    if (any(gksrs_only_data$consort_group != "gksrs_only")) {
        wrong_consort <- table(gksrs_only_data$consort_group)
        logger::log_error(sprintf(
            "VALIDATION FAILED: GKSRS-only cohort contains wrong consort_group: %s",
            paste(names(wrong_consort), "=", wrong_consort, collapse = ", ")
        ))
        validation_passed <- FALSE
    } else {
        logger::log_info("✓ GKSRS-only cohort contains only gksrs_only patients")
    }

    # Check 4: Verify eligibility criteria are correctly applied
    # Restricted cohort: diameter <= 20, height <= 10, no optic nerve
    restricted_violations <- restricted_data %>%
        filter(
            initial_tumor_diameter > TUMOR_DIAMETER_THRESHOLD |
                initial_tumor_height > TUMOR_HEIGHT_THRESHOLD |
                optic_nerve == "Yes"
        )

    if (nrow(restricted_violations) > 0) {
        logger::log_error(sprintf("VALIDATION FAILED: %d patients in restricted cohort violate eligibility criteria", nrow(restricted_violations)))
        print(restricted_violations %>% select(id, initial_tumor_diameter, initial_tumor_height, optic_nerve))
        validation_passed <- FALSE
    } else {
        logger::log_info("✓ Restricted cohort eligibility criteria correctly applied")
    }

    # GKSRS-only cohort: diameter > 20 OR height > 10 OR optic nerve involvement
    gksrs_only_should_qualify <- gksrs_only_data %>%
        filter(
            initial_tumor_diameter > TUMOR_DIAMETER_THRESHOLD |
                initial_tumor_height > TUMOR_HEIGHT_THRESHOLD |
                optic_nerve == "Yes"
        )

    if (nrow(gksrs_only_should_qualify) != nrow(gksrs_only_data)) {
        logger::log_error(sprintf(
            "VALIDATION FAILED: %d/%d patients in GKSRS-only cohort don't meet ineligibility criteria",
            nrow(gksrs_only_data) - nrow(gksrs_only_should_qualify), nrow(gksrs_only_data)
        ))
        validation_passed <- FALSE
    } else {
        logger::log_info("✓ GKSRS-only cohort ineligibility criteria correctly applied")
    }

    # Check 5: Verify no patient overlap between restricted and GKSRS-only
    overlap_patients <- intersect(restricted_data$id, gksrs_only_data$id)
    if (length(overlap_patients) > 0) {
        logger::log_error(sprintf(
            "VALIDATION FAILED: %d patients appear in both restricted and GKSRS-only cohorts: %s",
            length(overlap_patients), paste(overlap_patients, collapse = ", ")
        ))
        validation_passed <- FALSE
    } else {
        logger::log_info("✓ No patient overlap between cohorts")
    }

    # Check 6: Verify treatment assignments make sense
    # Check treatment distribution in each cohort
    for (cohort_name in names(cohort_list)) {
        cohort_data <- cohort_list[[cohort_name]]
        treatment_dist <- table(cohort_data$treatment_group, useNA = "ifany")
        logger::log_info(sprintf(
            "Treatment distribution in %s: %s",
            gsub("uveal_melanoma_", "", cohort_name),
            paste(names(treatment_dist), "=", treatment_dist, collapse = ", ")
        ))

        # All patients should have a treatment assignment
        if (any(is.na(cohort_data$treatment_group))) {
            logger::log_warn(sprintf(
                "VALIDATION WARNING: %d patients in %s have missing treatment_group",
                sum(is.na(cohort_data$treatment_group)), cohort_name
            ))
        }
    }

    # Final validation summary
    if (validation_passed) {
        logger::log_info("=== COHORT VALIDATION PASSED: All checks successful ===")
    } else {
        logger::log_info("=== COHORT VALIDATION FAILED: See errors above ===")
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
    logger::log_info("=== STARTING FACTOR LEVEL CONSISTENCY VALIDATION ===")
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
            reference = YN_DISPLAY_LABELS[1], # "No"
            critical = TRUE
        ),
        sex = list(
            levels = SEX_FACTOR_LEVELS,
            reference = SEX_FACTOR_LEVELS[1],
            critical = FALSE
        ),
        optic_nerve = list(
            levels = YN_DISPLAY_LABELS, # c("No", "Yes")
            reference = YN_DISPLAY_LABELS[1], # "No"
            critical = TRUE
        )
    )

    # Check each cohort for factor level consistency
    for (cohort_name in names(cohort_list)) {
        cohort_data <- cohort_list[[cohort_name]]
        cohort_display_name <- gsub("uveal_melanoma_", "", cohort_name)

        logger::log_info(sprintf("Validating factor levels for %s", cohort_display_name))

        for (factor_name in names(expected_factors)) {
            expected_config <- expected_factors[[factor_name]]

            # Check if factor exists in data
            if (!factor_name %in% names(cohort_data)) {
                if (expected_config$critical) {
                    logger::log_error(sprintf(
                        "VALIDATION FAILED: Critical factor '%s' missing from %s",
                        factor_name, cohort_display_name
                    ))
                    validation_passed <- FALSE
                } else {
                    logger::log_warn(sprintf(
                        "VALIDATION WARNING: Optional factor '%s' missing from %s",
                        factor_name, cohort_display_name
                    ))
                }
                next
            }

            factor_col <- cohort_data[[factor_name]]

            # Check if variable is actually a factor
            if (!is.factor(factor_col)) {
                logger::log_error(sprintf(
                    "VALIDATION FAILED: '%s' is not a factor in %s (class: %s)",
                    factor_name, cohort_display_name, class(factor_col)[1]
                ))
                validation_passed <- FALSE
                next
            }

            # Check factor levels
            actual_levels <- levels(factor_col)
            expected_levels <- expected_config$levels

            if (identical(actual_levels, expected_levels)) {
                logger::log_info(sprintf("✓ Factor levels correct for '%s' in %s", factor_name, cohort_display_name))
            } else if (length(actual_levels) == 1 && all(actual_levels %in% expected_levels)) {
                # Relaxed rule: if only a single level is present, warn but do not fail
                logger::log_warn(sprintf(
                    "VALIDATION WARNING: Only single level '%s' present for '%s' in %s; expected levels: %s",
                    paste(actual_levels, collapse = ", "), factor_name, cohort_display_name, paste(expected_levels, collapse = ", ")
                ))
            } else {
                logger::log_error(sprintf(
                    "VALIDATION FAILED: Factor levels mismatch for '%s' in %s",
                    factor_name, cohort_display_name
                ))
                logger::log_error(sprintf("  Expected: %s", paste(expected_levels, collapse = ", ")))
                logger::log_error(sprintf("  Actual:   %s", paste(actual_levels, collapse = ", ")))
                validation_passed <- FALSE
            }

            # Check reference level (first level) when at least one expected level present
            if (length(actual_levels) > 0 && length(expected_levels) > 0) {
                if (length(actual_levels) == length(expected_levels)) {
                    if (actual_levels[1] != expected_config$reference) {
                        logger::log_error(sprintf(
                            "VALIDATION FAILED: Reference level mismatch for '%s' in %s",
                            factor_name, cohort_display_name
                        ))
                        logger::log_error(sprintf("  Expected reference: %s", expected_config$reference))
                        logger::log_error(sprintf("  Actual reference:   %s", actual_levels[1]))
                        validation_passed <- FALSE
                    }
                } else if (length(actual_levels) == 1) {
                    # Single-level present: only warn about potential reference mismatch
                    if (actual_levels[1] != expected_config$reference) {
                        logger::log_warn(sprintf(
                            "VALIDATION WARNING: Single-level '%s' for '%s' in %s differs from expected reference '%s'",
                            actual_levels[1], factor_name, cohort_display_name, expected_config$reference
                        ))
                    }
                }
            }

            # Special validation for treatment_group (most critical)
            if (factor_name == "treatment_group") {
                # Check that both treatment groups are present
                unique_values <- unique(as.character(factor_col[!is.na(factor_col)]))
                if (length(unique_values) < 2) {
                    logger::log_warn(sprintf(
                        "VALIDATION WARNING: Only %d treatment group(s) present in %s: %s",
                        length(unique_values), cohort_display_name,
                        paste(unique_values, collapse = ", ")
                    ))
                }

                # Check sample sizes per treatment group
                treatment_dist <- table(factor_col, useNA = "ifany")
                logger::log_info(sprintf(
                    "Treatment distribution in %s: %s",
                    cohort_display_name,
                    paste(names(treatment_dist), "=", treatment_dist, collapse = ", ")
                ))

                # Validate that reference group is PBT (expected for our analysis)
                if (actual_levels[1] != "PBT") {
                    logger::log_error(sprintf(
                        "VALIDATION FAILED: Treatment reference group should be 'PBT', got '%s' in %s",
                        actual_levels[1], cohort_display_name
                    ))
                    validation_passed <- FALSE
                }
            }
        }
    }

    # Cross-cohort consistency check
    logger::log_info("Checking factor level consistency across cohorts")
    for (factor_name in names(expected_factors)) {
        if (!expected_factors[[factor_name]]$critical) next

        cohort_levels <- list()
        for (cohort_name in names(cohort_list)) {
            if (factor_name %in% names(cohort_list[[cohort_name]])) {
                lvls <- levels(cohort_list[[cohort_name]][[factor_name]])
                cohort_levels[[cohort_name]] <- lvls
                if (length(lvls) < 2) {
                    logger::log_warn(sprintf(
                        "Cross-cohort check: %s has single level for '%s' (levels: %s) - skipping this cohort for strict comparison",
                        cohort_name, factor_name, paste(lvls, collapse = ", ")
                    ))
                }
            }
        }

        # Compare only cohorts where at least two levels exist
        multi_level <- Filter(function(x) length(x) >= 2, cohort_levels)
        if (length(multi_level) > 1) {
            ref_levels <- multi_level[[1]]
            for (i in 2:length(multi_level)) {
                if (!identical(ref_levels, multi_level[[i]])) {
                    logger::log_error(sprintf("VALIDATION FAILED: Factor levels for '%s' differ between multi-level cohorts", factor_name))
                    logger::log_error(sprintf("  %s: %s", names(multi_level)[1], paste(ref_levels, collapse = ", ")))
                    logger::log_error(sprintf("  %s: %s", names(multi_level)[i], paste(multi_level[[i]], collapse = ", ")))
                    validation_passed <- FALSE
                }
            }
        }
    }

    # Validation summary
    if (validation_passed) {
        logger::log_info("=== FACTOR LEVEL VALIDATION PASSED: All factor levels consistent ===")
        logger::log_info(sprintf("✓ Treatment reference group: %s", TREATMENT_REFERENCE_LEVEL))
        logger::log_info(sprintf("✓ Treatment comparison group: %s", TREATMENT_COMPARISON_LEVEL))
        logger::log_info("✓ All critical factor levels match expected configuration")
    } else {
        logger::log_info("=== FACTOR LEVEL VALIDATION FAILED: See errors above ===")
        logger::log_error("⚠️  CRITICAL: Factor level inconsistencies detected.")
        logger::log_error("⚠️  This will cause incorrect model results and interpretation.")
    }

    return(validation_passed)
}

#' Comprehensive Data Processing Validation
#'
#' Return the canonical set of analytic cohort object names expected from
#' Objective 0 processing.
#'
#' @return Character vector of expected analytic cohort names.
get_expected_analytic_cohort_names <- function() {
    c(
        "uveal_melanoma_full_cohort",
        "uveal_melanoma_restricted_cohort",
        "uveal_melanoma_gksrs_only_cohort"
    )
}

#' Collect structured validation results for the processed-data pipeline
#'
#' @param data Data frame or list of data frames (cohorts) to validate
#' @param stop_on_failure Logical. If TRUE, throw an error when validation fails.
#' @return List with `success`, `validated_cohorts`, and `validation_errors`
validate_processing_pipeline <- function(data, stop_on_failure = TRUE) {
    logger::log_info("=== COMPREHENSIVE DATA PROCESSING VALIDATION ===")

    validation_errors <- character()
    validated_cohorts <- character()

    if (is.list(data) && !is.data.frame(data)) {
        logger::log_info("Phase 1: Individual Cohort Validation")
        for (cohort_name in names(data)) {
            logger::log_info(formatted(sprintf("Validating cohort: %s", cohort_name), indent = 1))
            cohort_validation <- validate_single_cohort_comprehensive(data[[cohort_name]], cohort_name)
            if (cohort_validation) {
                validated_cohorts <- c(validated_cohorts, cohort_name)
            } else {
                validation_errors <- c(validation_errors, sprintf("cohort_validation:%s", cohort_name))
            }
        }

        logger::log_info("Phase 2: Cross-Cohort Validation")
        cross_cohort_validation <- validate_cross_cohort_consistency(data)
        if (!cross_cohort_validation) {
            validation_errors <- c(validation_errors, "cross_cohort_consistency")
        }

        logger::log_info("Phase 3: File System Validation")
        file_validation <- validate_processed_files_exist(data)
        if (!file_validation) {
            validation_errors <- c(validation_errors, "processed_files_missing")
        }
    } else {
        validation_result <- validate_single_cohort_comprehensive(data, "single_dataset")
        if (validation_result) {
            validated_cohorts <- "single_dataset"
        } else {
            validation_errors <- c(validation_errors, "cohort_validation:single_dataset")
        }
    }

    validation_errors <- unique(validation_errors)
    validation_result <- list(
        success = length(validation_errors) == 0,
        validated_cohorts = validated_cohorts,
        validation_errors = validation_errors
    )

    if (validation_result$success) {
        logger::log_info("=== ALL DATA PROCESSING VALIDATIONS PASSED ===")
    } else {
        logger::log_error("=== DATA PROCESSING VALIDATION FAILED - SEE ERRORS ABOVE ===")
        if (isTRUE(stop_on_failure)) {
            stop("Data processing validation failed. Please fix the errors above before proceeding.")
        }
    }

    validation_result
}

#' Performs systematic validation of the entire data processing pipeline
#' to ensure all steps completed successfully and data integrity is maintained.
#' Based on the data processing pipeline: load → derive → factor → cohort → collapse → save.
#'
#' @param data Data frame or list of data frames (cohorts) to validate
#' @return Logical indicating if all validations passed
generate_validation_report <- function(data) {
    validation_result <- validate_processing_pipeline(data, stop_on_failure = TRUE)
    validation_result$success
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

    logger::log_info(formatted(sprintf(
        "Comprehensive validation for %s (%d rows, %d columns)",
        cohort_name, nrow(data), ncol(data)
    ), indent = 1))

    # 1. DATA STRUCTURE VALIDATION
    logger::log_info(formatted("1. Data Structure Validation", indent = 2))

    # Basic structure checks
    if (nrow(data) == 0) {
        logger::log_error(formatted("VALIDATION FAILED: Empty dataset", indent = 3))
        validation_passed <- FALSE
    }

    if (ncol(data) < MINIMUM_COLUMNS_AFTER_PROCESSING) { # Should have many variables after processing
        logger::log_warn(formatted(sprintf("VALIDATION WARNING: Few variables (%d columns) - expected more after processing", ncol(data)), indent = 3))
    }

    # Duplicate check
    if (sum(duplicated(data)) > 0) {
        logger::log_warn(formatted(sprintf("VALIDATION WARNING: %d duplicate rows found", sum(duplicated(data))), indent = 3))
    }

    # 2. CRITICAL VARIABLE VALIDATION
    logger::log_info(formatted("2. Critical Variable Validation", indent = 2))

    # Essential variables that must exist
    missing_critical <- setdiff(CRITICAL_VARIABLES, names(data))
    if (length(missing_critical) > 0) {
        logger::log_error(formatted(sprintf(
            "VALIDATION FAILED: Missing critical variables: %s",
            paste(missing_critical, collapse = ", ")
        ), indent = 3))
        validation_passed <- FALSE
    }

    # 3. DERIVED VARIABLE VALIDATION
    logger::log_info(formatted("3. Derived Variable Validation", indent = 2))

    # Check that key derived variables were created
    missing_derived <- setdiff(DERIVED_VARIABLES, names(data))
    if (length(missing_derived) > 0) {
        logger::log_error(formatted(sprintf(
            "VALIDATION FAILED: Missing derived variables: %s",
            paste(missing_derived, collapse = ", ")
        ), indent = 3))
        validation_passed <- FALSE
    }

    # Validate derived variable types and ranges
    if ("age_at_diagnosis" %in% names(data)) {
        if (!is.numeric(data$age_at_diagnosis)) {
            logger::log_error(formatted("VALIDATION FAILED: age_at_diagnosis is not numeric", indent = 3))
            validation_passed <- FALSE
        }
        if (any(data$age_at_diagnosis < 0, na.rm = TRUE)) {
            logger::log_error(formatted("VALIDATION FAILED: Negative ages found", indent = 3))
            validation_passed <- FALSE
        }
    }

    # 4. FACTOR LEVEL VALIDATION
    logger::log_info(formatted("4. Factor Level Validation", indent = 2))

    # Critical factors that should be factors
    for (factor_name in CRITICAL_FACTORS) {
        if (factor_name %in% names(data)) {
            if (!is.factor(data[[factor_name]])) {
                logger::log_error(formatted(sprintf("VALIDATION FAILED: %s is not a factor", factor_name), indent = 3))
                validation_passed <- FALSE
            } else if (length(levels(data[[factor_name]])) == 0) {
                logger::log_error(formatted(sprintf("VALIDATION FAILED: %s has no levels", factor_name), indent = 3))
                validation_passed <- FALSE
            }
        }
    }

    # Treatment group specific validation
    if ("treatment_group" %in% names(data)) {
        treatment_dist <- table(data$treatment_group, useNA = "ifany")
        if (length(treatment_dist) < 2) {
            # Relaxed: warn rather than fail when only a single treatment level is present
            logger::log_warn(formatted("VALIDATION WARNING: Only one treatment group present; models may be skipped", indent = 3))
        }

        # Check for expected treatment levels
        actual_levels <- levels(data$treatment_group)
        if (!all(TREATMENT_FACTOR_LEVELS %in% actual_levels)) {
            logger::log_error(formatted(sprintf(
                "VALIDATION FAILED: Unexpected treatment levels. Expected: %s, Got: %s",
                paste(TREATMENT_FACTOR_LEVELS, collapse = ", "),
                paste(actual_levels, collapse = ", ")
            ), indent = 3))
            validation_passed <- FALSE
        }
    }

    # 5. GEP VARIABLE VALIDATION
    logger::log_info(formatted("5. GEP Variable Validation", indent = 2))

    # Always validate GEP variables if they are present in the dataset
    # This ensures data quality issues are caught during data processing, not just during analysis
    if ("biopsy1_gep" %in% names(data)) {
        # Required GEP variables
                required_gep_vars <- c(
            "biopsy1_gep", "biopsy1_gep_mss", "mss_analysis_eligible",
            "biopsy1_gep_mfs", "mfs_analysis_eligible",
            "tt_death_years", "melanoma_death_event", "competing_death_event",
            "mss_event_5yr", "mss_event_7yr", "mss_event_10yr",
            "expected_mss_5yr", "expected_mss_7yr", "expected_mss_10yr",
            "prame_status", "gep_validation_set", "gep_class_simple",
            "missing_gep_group", "has_gep", "has_gep_mfs", "has_gep_mss", "has_prame"
        )
        
        # Check for missing required GEP variables
        missing_gep_vars <- setdiff(required_gep_vars, names(data))
        if (length(missing_gep_vars) > 0) {
            logger::log_error(formatted(sprintf(
                "VALIDATION FAILED: Missing required GEP variables: %s",
                paste(missing_gep_vars, collapse = ", ")
            ), indent = 3))
            validation_passed <- FALSE
        }
        
        # Enhanced GEP validation set checks
        if ("gep_validation_set" %in% names(data)) {
            validation_set_counts <- table(data$gep_validation_set, useNA = "ifany")
            training_count <- ifelse("Training" %in% names(validation_set_counts), validation_set_counts["Training"], 0)
            testing_count <- ifelse("Testing" %in% names(validation_set_counts), validation_set_counts["Testing"], 0)
            no_gep_count <- ifelse("No GEP Data" %in% names(validation_set_counts), validation_set_counts["No GEP Data"], 0)
            enforce_global_split_shape <- identical(cohort_name, "uveal_melanoma_full_cohort") ||
                identical(cohort_name, "single_dataset")
            
            # Calculate expected counts for patients with valid GEP data
            patients_with_valid_gep <- sum(!is.na(data$biopsy1_gep_mfs) & !is.na(data$biopsy1_gep_mss) & 
                                          data$gep_class_simple %in% c("Class 1", "Class 2"), na.rm = TRUE)
            
            # Validation checks for gep_validation_set
            # Split-shape checks are only meaningful on the global/full GEP population.
            if (enforce_global_split_shape) {
                if (training_count == 0 && patients_with_valid_gep > 0) {
                    logger::log_error(formatted("VALIDATION FAILED: No patients assigned to Training set despite having valid GEP data", indent = 3))
                    validation_passed <- FALSE
                }
                
                if (testing_count == 0 && patients_with_valid_gep > 0) {
                    logger::log_error(formatted("VALIDATION FAILED: No patients assigned to Testing set despite having valid GEP data", indent = 3))
                    validation_passed <- FALSE
                }
                
                if (training_count > 0 && testing_count > 0) {
                    actual_training_rate <- training_count / (training_count + testing_count)
                    if (actual_training_rate < 0.5 || actual_training_rate > 0.9) {
                        logger::log_error(formatted(sprintf(
                            "VALIDATION FAILED: Training/testing split is unreasonable: %.1f%% training (expected ~70%%)", 
                            actual_training_rate * 100
                        ), indent = 3))
                        validation_passed <- FALSE
                    }
                }
            } else if (patients_with_valid_gep > 0) {
                logger::log_info(formatted(sprintf(
                    "Subset cohort split summary: %d Training, %d Testing, %d No GEP Data",
                    training_count, testing_count, no_gep_count
                ), indent = 3))
            }
            
            # Check 3: Ensure total Training + Testing equals patients with valid GEP data
            total_assigned <- training_count + testing_count
            if (abs(total_assigned - patients_with_valid_gep) > 1) { # Allow small rounding differences
                logger::log_error(formatted(sprintf(
                    "VALIDATION FAILED: Training + Testing count (%d) doesn't match patients with valid GEP data (%d)", 
                    total_assigned, patients_with_valid_gep
                ), indent = 3))
                validation_passed <- FALSE
            }
            
            # Check 4: Ensure "No GEP Data" count is reasonable
            expected_no_gep <- nrow(data) - patients_with_valid_gep
            if (abs(no_gep_count - expected_no_gep) > 1) {
                logger::log_error(formatted(sprintf(
                    "VALIDATION FAILED: 'No GEP Data' count (%d) doesn't match expected (%d)", 
                    no_gep_count, expected_no_gep
                ), indent = 3))
                validation_passed <- FALSE
            }
            
            # Log validation results
            if (validation_passed) {
                logger::log_info(formatted(sprintf("✓ GEP validation set properly configured: %d Training, %d Testing, %d No GEP Data", 
                                                 training_count, testing_count, no_gep_count), indent = 3))
            }
        } else {
            logger::log_error(formatted("VALIDATION FAILED: gep_validation_set variable not found in dataset", indent = 3))
            validation_passed <- FALSE
        }
        
        # Check for derived GEP variables
        if (exists("GEP_DERIVED_VARIABLES")) {
            missing_gep_derived <- setdiff(GEP_DERIVED_VARIABLES, names(data))
            if (length(missing_gep_derived) > 0) {
                logger::log_error(formatted(sprintf(
                    "VALIDATION FAILED: Missing derived GEP variables: %s",
                    paste(missing_gep_derived, collapse = ", ")
                ), indent = 3))
                validation_passed <- FALSE
            }
        }
        
        # Log GEP data availability summary
        patients_with_gep <- sum(!is.na(data$biopsy1_gep) & data$biopsy1_gep != "Failed" & data$biopsy1_gep != "Unknown")
        patients_with_mfs <- sum(!is.na(data$biopsy1_gep_mfs))
        patients_with_mss <- sum(!is.na(data$biopsy1_gep_mss))
        
        logger::log_info(formatted(sprintf("GEP data availability: %d patients with GEP, %d with MFS, %d with MSS", 
                                         patients_with_gep, patients_with_mfs, patients_with_mss), indent = 3))
        
    } else {
        logger::log_info(formatted("Skipping GEP validation (no GEP variables present)", indent = 3))
    }

    # 6. DATA QUALITY VALIDATION
    logger::log_info(formatted("6. Data Quality Validation", indent = 2))

    # Check for excessive missing data in critical variables
    for (var_name in MISSING_DATA_CHECK_VARIABLES) {
        if (var_name %in% names(data)) {
            missing_pct <- mean(is.na(data[[var_name]])) * 100
            if (missing_pct > MAXIMUM_MISSING_DATA_PERCENTAGE) {
                logger::log_warn(formatted(sprintf("VALIDATION WARNING: High missing data in %s (%.1f%%)", var_name, missing_pct), indent = 3))
            }
        }
    }

    # Check for logical inconsistencies
    if (all(c("recurrence1", "tt_recurrence_months") %in% names(data))) {
        # Patients with recurrence should have positive time to recurrence
        inconsistent_recurrence <- data$recurrence1 == "Y" & data$tt_recurrence_months <= 0
        if (any(inconsistent_recurrence, na.rm = TRUE)) {
            logger::log_warn(formatted(sprintf(
                "VALIDATION WARNING: %d patients with recurrence have non-positive time to recurrence",
                sum(inconsistent_recurrence, na.rm = TRUE)
            ), indent = 3))
        }
    }

    # 7. COHORT-SPECIFIC VALIDATION
    logger::log_info(formatted("7. Cohort-Specific Validation", indent = 2))

    # Validate cohort size expectations
    if (cohort_name %in% names(EXPECTED_COHORT_SIZES)) {
        expected_range <- EXPECTED_COHORT_SIZES[[cohort_name]]
        if (nrow(data) < expected_range[1] || nrow(data) > expected_range[2]) {
            logger::log_warn(formatted(sprintf("VALIDATION WARNING: %s size outside expected range (%d not in [%d, %d])", cohort_name, nrow(data), expected_range[1], expected_range[2]), indent = 3))
        }
    }

    # Final validation result
    if (validation_passed) {
        logger::log_info(formatted(sprintf("✓ All validations passed for %s", cohort_name), indent = 1))
    } else {
        logger::log_error(formatted(sprintf("✗ Validation failed for %s", cohort_name), indent = 1))
    }

    return(validation_passed)
}

#' Validate Cross-Cohort Consistency
#'
#' @param cohort_list List of cohorts to validate
#' @return Logical indicating if validation passed
validate_cross_cohort_consistency <- function(cohort_list) {
    logger::log_info(formatted("Validating cross-cohort consistency", indent = 1))

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
    logger::log_info(formatted("Validating processed files exist", indent = 1))

    validation_passed <- TRUE

    # Check that RDS files exist for each cohort
    for (cohort_name in names(cohort_list)) {
        rds_file <- file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".rds"))
        if (!file.exists(rds_file)) {
            logger::log_error(formatted(sprintf("VALIDATION FAILED: RDS file missing for %s: %s", cohort_name, rds_file), indent = 2))
            validation_passed <- FALSE
        }

        excel_file <- file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".xlsx"))
        if (!file.exists(excel_file)) {
            logger::log_error(formatted(sprintf("VALIDATION FAILED: Excel file missing for %s: %s", cohort_name, excel_file), indent = 2))
            validation_passed <- FALSE
        }
    }

    if (validation_passed) {
        logger::log_info(formatted("✓ All processed files exist", indent = 2))
    }

    return(validation_passed)
}
