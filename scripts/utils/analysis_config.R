# Analysis Configuration and Setup Functions
# Author: Nicholas Camarda
# Description: Configuration, contrast setup, and helper functions for analysis

# Note: All required libraries are loaded in main.R

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
#' @param dataset_name Name of the current dataset
log_section_start <- function(section_name, dataset_name = NULL) {
    if (!is.null(dataset_name)) {
        msg <- sprintf("%s - Dataset: %s", section_name, dataset_name)
    } else {
        msg <- section_name
    }
    log_enhanced(msg, level = "SECTION")
}

#' Log completion of a major analysis section with timing
#'
#' @param section_name Name of the analysis section
#' @param start_time Start time from Sys.time()
log_section_complete <- function(section_name, start_time) {
    duration <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)
    log_enhanced(sprintf("COMPLETED %s (Duration: %.1f seconds)", section_name, duration), level = "PROGRESS")
}

#' Log function execution
#'
#' @param func_name Name of the function being executed
#' @param details Additional details about the function call
log_function <- function(func_name, details = NULL) {
    if (!is.null(details)) {
        msg <- sprintf("Executing %s: %s", func_name, details)
    } else {
        msg <- sprintf("Executing %s", func_name)
    }
    log_enhanced(msg, level = "INFO", indent = 1)
}

# Set consistent contrast options for all modeling functions
# This ensures factor variables use consistent naming across all models
options(contrasts = c("contr.treatment", "contr.poly"))

# Define data paths
DATA_DIR <- "final_data"
RAW_DATA_DIR <- file.path(DATA_DIR, "Original Files")
PROCESSED_DATA_DIR <- file.path(DATA_DIR, "Analytic Dataset")
OUTPUT_DIR <- file.path(DATA_DIR, "Analysis")

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
    "age_at_diagnosis", "sex", "location", "initial_overall_stage", "initial_t_stage",
    "initial_tumor_height", "initial_tumor_diameter", "biopsy1_gep", "optic_nerve"
)

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
    list(
        # Treatment and demographics
        treatment_group = "Treatment Group",
        age_at_diagnosis = "Age at Diagnosis (years)",
        sex = "Sex",
        
        # Tumor characteristics
        location = "Tumor Location",
        optic_nerve = "Optic Nerve Involvement",
        initial_tumor_height = "Initial Tumor Height (mm)",
        initial_tumor_diameter = "Initial Tumor Diameter (mm)",
        initial_overall_stage = "Initial Overall Stage",
        initial_t_stage = "Initial T Stage",
        biopsy1_gep = "Gene Expression Profile",
        internal_reflectivity = "Internal Reflectivity",
        srf = "Subretinal Fluid",
        op = "Orange Pigment",
        
        # Symptoms
        symptoms = "Any Symptoms",
        vision_loss_blurred_vision = "Vision Loss/Blurred Vision",
        visual_field_defect = "Visual Field Defect",
        flashes_photopsia = "Flashes/Photopsia",
        floaters = "Floaters",
        pain = "Pain",
        
        # Outcomes
        recurrence1 = "Local Recurrence",
        recurrence2 = "Second Recurrence",
        mets_progression = "Metastatic Progression",
        enucleation = "Enucleation",
        retinopathy = "Radiation Retinopathy",
        nvg = "Neovascular Glaucoma",
        srd = "Serous Retinal Detachment",
        
        # Recurrence treatment
        recurrence1_treatment_clean = "Recurrence Treatment",
        
        # Follow-up variables
        follow_up_years = "Follow-up Time (years)",
        follow_up_months = "Follow-up Time (months)"
    )
} 