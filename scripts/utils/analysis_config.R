# Analysis Configuration and Setup Functions
# Author: Nicholas Camarda
# Description: Configuration, contrast setup, and helper functions for analysis

# Note: All required libraries are loaded in main.R

# Set consistent contrast options for all modeling functions
# This ensures factor variables use consistent naming across all models
options(contrasts = c("contr.treatment", "contr.poly"))

# Define data paths
DATA_DIR <- "final_data"
RAW_DATA_DIR <- file.path(DATA_DIR, "Original Files")
PROCESSED_DATA_DIR <- file.path(DATA_DIR, "Analytic Dataset")
OUTPUT_DIR <- file.path(DATA_DIR, "Analysis")
ANALYSIS_DIR <- OUTPUT_DIR  # Alias for consistency with legacy code

# Minimum number of observations required to keep a category
THRESHOLD_RARITY <- 5

# Define confounders for adjustment
confounders <- c(
    "age_at_diagnosis", "sex", "location",
    # "initial_overall_stage", "initial_tumor_height", "initial_tumor_diameter", "biopsy1_gep",
    "optic_nerve"
)

# Define time conversion constants
DAYS_IN_YEAR <- 365.25
DAYS_IN_MONTH <- 30.44

# Define subgroup variables for analysis
subgroup_vars <- c(
    "age_at_diagnosis", "sex", "location", "initial_t_stage",
    "initial_tumor_height", "initial_tumor_diameter", "biopsy1_gep", "optic_nerve"
)

# =============================================================================
# GLOBAL CONFIGURATION VARIABLES
# =============================================================================
# This section contains all hardcoded variables used throughout the analysis
# To modify any settings, change them here rather than in individual files

# Treatment labels and names
TREATMENT_LABELS <- c("GKSRS", "Plaque")
FAVOURS_LABELS <- c("Favours GKSRS", "Favours Plaque")

# Plot dimensions and settings
FOREST_PLOT_WIDTH <- 12    # inches
FOREST_PLOT_HEIGHT <- 8    # inches
SURVIVAL_PLOT_WIDTH <- 10  # inches  
SURVIVAL_PLOT_HEIGHT <- 8  # inches
RMST_PLOT_WIDTH <- 10      # inches
RMST_PLOT_HEIGHT <- 6      # inches
PLOT_DPI <- 300           # resolution
PLOT_UNITS <- "in"        # units

# Data processing constants
INPUT_FILENAME <- "Ocular Melanoma Master Spreadsheet REVISED FOR STATS (5-10-25, TJM).xlsx"
SPECIFIC_PATIENTS_TO_EXCLUDE <- c(271) # Patient 271: all supporting documentation was lost
TUMOR_HEIGHT_THRESHOLD <- 10           # mm
TUMOR_DIAMETER_THRESHOLD <- 20         # mm
FOLLOW_UP_YEARS <- 5                   # For 5-year outcomes
UNITS_OF_TIME <- "months" # "days" or "months" or "years"

# TOGGLE: Switch between standardized vs median cutoffs
USE_STANDARDIZED_CUTOFFS <- TRUE

# Standardized cutoffs (when USE_STANDARDIZED_CUTOFFS = TRUE)
STANDARDIZED_CUTOFFS <- list(
    age_at_diagnosis = 65.0,
    initial_tumor_height = 4.2,
    initial_tumor_diameter = 11.0
)

# Define consistent variable order for forest plots and subgroup analysis
# This ensures all plots and tables show variables in the same order across cohorts
# Used by main.R, forest plot functions, and subgroup analysis to maintain consistency
# To change the order of variables in all outputs, modify this single variable
FOREST_PLOT_VARIABLE_ORDER <- c(
    "age_at_diagnosis", "sex", "location", "initial_t_stage", 
    "initial_tumor_height", "initial_tumor_diameter", "biopsy1_gep", "optic_nerve"
)

# Define variables for baseline characteristics summary tables
# Used by create_summary_tables() and merge_cohort_tables() to ensure consistency
BASELINE_VARIABLES_TO_SUMMARIZE <- c(
    "age_at_diagnosis", "race", "sex", "eye",
    "initial_vision", "location", "optic_nerve",
    "initial_tumor_height", "initial_tumor_diameter",
    "internal_reflectivity", "srf", "op", "symptoms",
    "vision_loss_blurred_vision", "visual_field_defect",
    "flashes_photopsia", "floaters", "pain",
    "initial_overall_stage", "initial_t_stage",
    "initial_n_stage", "initial_m_stage",
    "initial_mets", "biopsy1_gep"
)

########################################################
############### TABLE LABELS ##########################
########################################################

# Centralized table labels to ensure consistency across all gtsummary tables
# These should match the labels used in data_processing.R baseline tables
STANDARD_TABLE_LABELS <- list(
    # Demographics
    age_at_diagnosis = "Age at Diagnosis (years)",
    race = "Race",
    sex = "Sex", 
    eye = "Eye",
    
    # Vision and measurements
    initial_vision = "Initial Visual Acuity (logMAR)",
    
    # Tumor characteristics
    location = "Tumor Location",
    optic_nerve = "Optic Nerve Involvement",
    initial_tumor_height = "Initial Tumor Height (mm)",
    initial_tumor_diameter = "Initial Tumor Diameter (mm)",
    internal_reflectivity = "Internal Reflectivity",
    srf = "Subretinal Fluid (SRF)",
    op = "Orange Pigment",
    
    # Symptoms
    symptoms = "Any Symptoms",
    vision_loss_blurred_vision = "Vision Loss/Blurred Vision",
    visual_field_defect = "Visual Field Defect",
    flashes_photopsia = "Flashes/Photopsia",
    floaters = "Floaters",
    pain = "Pain",
    
    # Staging
    initial_overall_stage = "Overall Stage",
    initial_t_stage = "T Stage",
    initial_n_stage = "N Stage", 
    initial_m_stage = "M Stage",
    initial_mets = "Initial Metastases",
    biopsy1_gep = "Gene Expression Profile",
    
    # Treatment
    treatment_group = "Treatment Group",
    recurrence1_treatment_clean = "Recurrence Treatment",
    
    # Outcomes
    recurrence1 = "Local Recurrence",
    recurrence2 = "Second Recurrence",
    mets_progression = "Metastatic Progression",
    enucleation = "Enucleation",
    retinopathy = "Radiation Retinopathy",
    nvg = "Neovascular Glaucoma",
    srd = "Serous Retinal Detachment",
    
    # Changes/follow-up
    height_change = "Tumor Height Change (mm)",
    vision_change = "Visual Acuity Change (logMAR)",
    follow_up_years = "Follow-up Time (years)",
    follow_up_months = "Follow-up Time (months)",
    
    # PFS-2 specific
    tt_pfs2_months = "PFS-2 Time (months)", 
    pfs2_event = "Second Recurrence Events"
)

#' Enhanced logging utility functions
#'
#' Provides structured logging with timestamps, progress indicators, and visual formatting
#' 

#' Log a message with timestamp and optional formatting
#'
#' @param msg Message to log
#' @param level Log level ("INFO", "WARN", "ERROR", "PROGRESS", "SECTION")
#' @param indent Number of spaces to indent (default: 0)
log_enhanced <- function(msg, level = "INFO", indent = 0) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    indent_str <- paste(rep("  ", indent), collapse = "")
    
    # Format based on level
    formatted_msg <- switch(level,
        "SECTION" = sprintf("\n%s[%s] === %s ===\n", indent_str, timestamp, msg),
        "PROGRESS" = sprintf("%s[%s] >>> %s", indent_str, timestamp, msg),
        "INFO" = sprintf("%s[%s] %s", indent_str, timestamp, msg),
        "WARN" = sprintf("%s[%s] WARNING: %s", indent_str, timestamp, msg),
        "ERROR" = sprintf("%s[%s] ERROR: %s", indent_str, timestamp, msg),
        sprintf("%s[%s] %s", indent_str, timestamp, msg)  # default
    )
    
    message(formatted_msg)
}

#' Log progress through a list of items
#'
#' @param current Current item number
#' @param total Total number of items
#' @param item_name Name of current item
#' @param action Action being performed
log_progress <- function(current, total, item_name = NULL, action = "Processing") {
    progress_pct <- round(100 * current / total, 1)
    base_msg <- sprintf("%s (%d/%d - %.1f%%)", action, current, total, progress_pct)
    
    if (!is.null(item_name)) {
        full_msg <- sprintf("%s: %s", base_msg, item_name)
    } else {
        full_msg <- base_msg
    }
    
    log_enhanced(full_msg, level = "PROGRESS")
}

#' Log start of a major analysis section
#'
#' @param section_name Name of the analysis section
#' @param detail_name Optional detail for the section
log_section_start <- function(section_name, detail_name = NULL) {
    if (!is.null(detail_name)) {
        full_name <- sprintf("%s - %s", section_name, detail_name)
    } else {
        full_name <- section_name
    }
    log_enhanced(full_name, level = "SECTION")
}

#' Log completion of a major analysis section with timing
#'
#' @param section_name Name of the analysis section
#' @param start_time Start time from Sys.time()
log_section_complete <- function(section_name, start_time) {
    duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    log_enhanced(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)", section_name, duration), level = "PROGRESS")
}

#' Log a function call with its purpose
#'
#' @param func_name Name of the function being called
#' @param purpose Description of what the function does
log_function <- function(func_name, purpose) {
    log_enhanced(sprintf("Executing %s: %s", func_name, purpose), level = "INFO", indent = 1)
}

########################################################
############### DATA VALIDATION FUNCTIONS #############
########################################################

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
    
    # Check 3: Verify cohort definitions match consort_group assignments
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

#' Validate dataset naming and directory structure consistency
#'
#' Ensures that dataset names, directory names, and prefixes are all consistent
#' throughout the analysis pipeline.
#'
#' @param dataset_name Name of the dataset being processed
#' @param prefix File prefix being used
#' @param cohort_dir_name Directory name being used
#' @return TRUE if naming is consistent, FALSE otherwise
validate_naming_consistency <- function(dataset_name, prefix, cohort_dir_name) {
    log_enhanced(sprintf("Validating naming consistency for dataset: %s", dataset_name), level = "INFO")
    
    # Define expected mappings
    expected_mappings <- list(
        "uveal_melanoma_full_cohort" = list(prefix = "full_cohort_", dir_name = "uveal_full"),
        "uveal_melanoma_restricted_cohort" = list(prefix = "restricted_cohort_", dir_name = "uveal_restricted"),
        "uveal_melanoma_gksrs_only_cohort" = list(prefix = "gksrs_only_cohort_", dir_name = "gksrs")
    )
    
    # Check if dataset name is recognized
    if (!dataset_name %in% names(expected_mappings)) {
        log_enhanced(sprintf("VALIDATION WARNING: Unrecognized dataset name: %s", dataset_name), level = "WARN")
        return(FALSE)
    }
    
    expected <- expected_mappings[[dataset_name]]
    
    # Validate prefix
    if (prefix != expected$prefix) {
        log_enhanced(sprintf("VALIDATION FAILED: Prefix mismatch for %s. Expected: %s, Got: %s", 
                            dataset_name, expected$prefix, prefix), level = "ERROR")
        return(FALSE)
    }
    
    # Validate directory name
    if (cohort_dir_name != expected$dir_name) {
        log_enhanced(sprintf("VALIDATION FAILED: Directory name mismatch for %s. Expected: %s, Got: %s", 
                            dataset_name, expected$dir_name, cohort_dir_name), level = "ERROR")
        return(FALSE)
    }
    
    log_enhanced("✓ Naming consistency validated", level = "INFO")
    return(TRUE)
}

#' Generate a comprehensive validation report
#'
#' Creates a detailed report of all validation checks performed during the analysis
#'
#' @param cohort_list List of cohort datasets
#' @param output_path Path to save the validation report
generate_validation_report <- function(cohort_list, output_path = NULL) {
    if (is.null(output_path)) {
        output_path <- file.path("final_data", "Analysis", "validation_report.txt")
    }
    
    # Ensure directory exists
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    
    # Capture validation output
    validation_output <- capture.output({
        validation_result <- validate_cohort_integrity(cohort_list)
    })
    
    # Add timestamp and system info
    report_content <- c(
        sprintf("COHORT VALIDATION REPORT"),
        sprintf("Generated: %s", Sys.time()),
        sprintf("System: %s", Sys.info()["sysname"]),
        sprintf("R Version: %s", R.version.string),
        "",
        "=== VALIDATION OUTPUT ===",
        validation_output,
        "",
        "=== SUMMARY STATISTICS ===",
        sprintf("Full cohort: %d patients", nrow(cohort_list$uveal_melanoma_full_cohort)),
        sprintf("Restricted cohort: %d patients", nrow(cohort_list$uveal_melanoma_restricted_cohort)),
        sprintf("GKSRS-only cohort: %d patients", nrow(cohort_list$uveal_melanoma_gksrs_only_cohort)),
        ""
    )
    
    # Write report
    writeLines(report_content, output_path)
    log_enhanced(sprintf("Validation report saved to: %s", output_path), level = "INFO")
    
    return(output_path)
}

#' Function to ensure consistent factor contrasts with human-readable names
#'
#' Sets up consistent factor contrasts across the dataset for modeling
#'
#' @param data Data frame containing factor variables
#' @return Data frame with consistent contrasts applied
ensure_consistent_contrasts <- function(data) {
    factor_vars <- names(data)[sapply(data, is.factor)]
    for (var in factor_vars) {
        if (nlevels(data[[var]]) > 1) {
            # Set contrasts with meaningful names based on factor levels
            factor_levels <- levels(data[[var]])
            if (length(factor_levels) > 1) {
                contrast_matrix <- contr.treatment(length(factor_levels))
                # Use factor level names for contrast names (excluding reference level)
                colnames(contrast_matrix) <- factor_levels[-1]
                contrasts(data[[var]]) <- contrast_matrix
            }
        }
    }
    return(data)
}

#' Function to extract coefficient names more robustly
#'
#' Extracts treatment coefficient names from model objects with fallback options
#'
#' @param model Model object (lm, glm, coxph)
#' @param treatment_var Name of treatment variable
#' @param data Optional data frame for factor level extraction
#' @return Character string of coefficient name or NULL if not found
get_treatment_coefficient_name <- function(model, treatment_var = "treatment_group", data = NULL) {
    coef_names <- names(coef(model))
    
    # Get the actual factor levels from the data if available
    if (!is.null(data) && treatment_var %in% names(data)) {
        treatment_levels <- levels(data[[treatment_var]])
        if (length(treatment_levels) > 1) {
            treatment_nonref <- treatment_levels[2]  # Second level (non-reference)
        } else {
            treatment_nonref <- "GKSRS"  # fallback
        }
    } else {
        treatment_nonref <- "GKSRS"  # fallback
    }
    
    # Try multiple patterns in order of preference
    possible_patterns <- c(
        paste0(treatment_var, treatment_nonref),     # treatment_groupGKSRS
        paste0(treatment_var, "2"),                  # treatment_group2
        paste0(treatment_var, "GKSRS"),              # treatment_groupGKSRS (exact)
        treatment_nonref                             # GKSRS (just the level)
    )
    
    for (pattern in possible_patterns) {
        if (pattern %in% coef_names) {
            return(pattern)
        }
    }
    
    # If no exact match, try partial matching
    for (pattern in possible_patterns) {
        matches <- grep(pattern, coef_names, value = TRUE)
        if (length(matches) > 0) {
            return(matches[1])
        }
    }
    
    return(NULL)
}

#' Function to extract interaction coefficient names more robustly  
#'
#' Extracts interaction coefficient names from model objects
#'
#' @param model Model object
#' @param treatment_var Name of treatment variable
#' @param subgroup_var Name of subgroup variable
#' @param subgroup_level Specific level of subgroup variable
#' @param data Optional data frame for factor level extraction
#' @return Character string of interaction coefficient name or NULL if not found
get_interaction_coefficient_name <- function(model, treatment_var = "treatment_group", 
                                           subgroup_var, subgroup_level, data = NULL) {
    coef_names <- names(coef(model))
    
    # Get the actual factor levels from the data if available
    if (!is.null(data) && treatment_var %in% names(data)) {
        treatment_levels <- levels(data[[treatment_var]])
        if (length(treatment_levels) > 1) {
            treatment_nonref <- treatment_levels[2]  # Second level (non-reference)
        } else {
            treatment_nonref <- "GKSRS"  # fallback
        }
    } else {
        treatment_nonref <- "GKSRS"  # fallback
    }
    
    # Try multiple interaction patterns
    possible_patterns <- c(
        paste0(treatment_var, treatment_nonref, ":", subgroup_var, subgroup_level),
        paste0(treatment_var, "2:", subgroup_var, subgroup_level),
        paste0(treatment_var, treatment_nonref, ":", subgroup_var, "2"),
        paste0(treatment_var, "2:", subgroup_var, "2"),
        paste0(treatment_var, "GKSRS:", subgroup_var, subgroup_level),
        paste0(treatment_var, "GKSRS:", subgroup_var, "2"),
        paste0(treatment_var, treatment_nonref, ":", subgroup_var, "Yes"),
        paste0(treatment_var, treatment_nonref, ":", subgroup_var, "No"),
        paste0(treatment_var, "2:", subgroup_var, "Yes"),
        paste0(treatment_var, "2:", subgroup_var, "No")
    )
    
    # Try exact matches first
    for (pattern in possible_patterns) {
        if (pattern %in% coef_names) {
            return(pattern)
        }
    }
    
    # Try pattern matching with regex
    interaction_pattern <- paste0(treatment_var, ".*:", subgroup_var, ".*")
    matches <- grep(interaction_pattern, coef_names, value = TRUE)
    if (length(matches) > 0) {
        return(matches[1])
    }
    
    return(NULL)
}

#' Function to get human-readable variable labels for tables
#'
#' Returns a named list of human-readable labels for variables
#'
#' @return Named list of variable labels
get_variable_labels <- function() {
    return(STANDARD_TABLE_LABELS)
} 