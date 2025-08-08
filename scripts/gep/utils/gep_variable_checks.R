# GEP Variable Checks (renamed from gep_validation_utilities.R)
# Author: Nicholas Camarda
# Description: Validation of GEP-related variables for Objective 4

#' Validate GEP-related variables with detailed results for inclusion in the validation report.
#'
#' @param data Data frame to validate (typically the full cohort)
#' @return List with validation_passed (TRUE/FALSE) and detailed_results (list)
validate_gep_variables_with_report <- function(data) {
    log_enhanced("Starting GEP variable validation checks", level = "INFO", indent = 1)
    validation_passed <- TRUE
    detailed_results <- list()
    required_gep_vars <- c(
        "biopsy1_gep", "biopsy1_gep_mfs", "biopsy1_gep_mss",
        "gep_class_simple", "prame_status",
        "expected_mfs_5yr", "expected_mfs_7yr", "expected_mfs_10yr",
        "expected_mss_5yr", "expected_mss_7yr", "expected_mss_10yr"
    )
    missing_vars <- setdiff(required_gep_vars, names(data))
    if (length(missing_vars) > 0) {
        detailed_results$missing_variables <- missing_vars
        validation_passed <- FALSE
    } else {
        detailed_results$missing_variables <- NULL
    }
    patients_with_gep <- sum(!is.na(data$biopsy1_gep) & data$biopsy1_gep != "Failed" & data$biopsy1_gep != "Unknown")
    patients_with_mfs <- sum(!is.na(data$biopsy1_gep_mfs))
    patients_with_mss <- sum(!is.na(data$biopsy1_gep_mss))
    training_testing_stats <- NULL
    if ("gep_validation_set" %in% names(data)) {
        validation_set_counts <- table(data$gep_validation_set, useNA = "ifany")
        training_count <- ifelse("Training" %in% names(validation_set_counts), validation_set_counts["Training"], 0)
        testing_count <- ifelse("Testing" %in% names(validation_set_counts), validation_set_counts["Testing"], 0)
        no_gep_count <- ifelse("No GEP Data" %in% names(validation_set_counts), validation_set_counts["No GEP Data"], 0)
        training_testing_stats <- list(
            training_patients = training_count,
            testing_patients = testing_count,
            no_gep_patients = no_gep_count,
            training_rate = round(100 * training_count / nrow(data), 1),
            testing_rate = round(100 * testing_count / nrow(data), 1)
        )
    }
    detailed_results$data_availability <- list(
        total_patients = nrow(data),
        patients_with_gep = patients_with_gep,
        patients_with_mfs = patients_with_mfs,
        patients_with_mss = patients_with_mss,
        gep_availability_rate = round(100 * patients_with_gep / nrow(data), 1),
        mfs_availability_rate = round(100 * patients_with_mfs / nrow(data), 1),
        mss_availability_rate = round(100 * patients_with_mss / nrow(data), 1),
        training_testing_split = training_testing_stats
    )
    if (length(missing_vars) == 0) {
        if ("gep_class_simple" %in% names(data)) {
            gep_distribution <- table(data$gep_class_simple, useNA = "ifany")
            detailed_results$gep_distribution <- gep_distribution
        }
        if ("prame_status" %in% names(data)) {
            prame_distribution <- table(data$prame_status, useNA = "ifany")
            detailed_results$prame_distribution <- prame_distribution
        }
        detailed_results$extrapolation_valid <- TRUE
        if (all(c("expected_mfs_5yr", "expected_mfs_7yr") %in% names(data))) {
            valid_data <- data[!is.na(data$expected_mfs_5yr) & data$expected_mfs_5yr > 0, ]
            if (nrow(valid_data) > 0) {
                expected_7yr <- valid_data$expected_mfs_5yr^(7/5)
                actual_7yr <- valid_data$expected_mfs_7yr
                extrapolation_errors <- sum(abs(expected_7yr - actual_7yr) > 1e-10, na.rm = TRUE)
                if (extrapolation_errors > 0) {
                    detailed_results$extrapolation_valid <- FALSE
                    validation_passed <- FALSE
                }
            }
        }
    }
    detailed_results$validation_passed <- validation_passed
    return(list(
        validation_passed = validation_passed,
        detailed_results = detailed_results
    ))
}

#' Validate GEP-related variables were created correctly (Objective 4)
#'
#' Performs comprehensive validation of all GEP-related variables created in data_processing.R
#' to ensure they are properly formatted and contain expected values for the validation analysis.
#'
#' @param data Data frame to validate (typically the full cohort)
#' @return TRUE if all validations pass, FALSE otherwise
validate_gep_variables <- function(data) {
    result <- validate_gep_variables_with_report(data)
    return(result$validation_passed)
}
