# Model Utilities
# Author: Nicholas Camarda
# Description: Model-related utility functions for the analysis pipeline

# =============================================================================
# MODEL UTILITY FUNCTIONS
# =============================================================================

#' Enforce unordered factors for modeling
#'
#' Converts any ordered factors to regular factors while preserving their level order.
#' This ensures that all factors use treatment contrasts (not polynomial contrasts) in models.
#' The function preserves the display order of levels but removes the 'ordered' attribute.
#'
#' @param data Data frame containing factor variables
#' @param verbose Whether to log which factors were converted (default: FALSE)
#' @return Data frame with all factors converted to unordered
enforce_unordered_factors <- function(data, verbose = FALSE) {
    if (verbose) {
        log_enhanced("Enforcing unordered factors for modeling", level = "INFO")
    }
    
    factor_vars <- names(data)[sapply(data, is.factor)]
    converted_count <- 0
    
    for (var in factor_vars) {
        if (is.ordered(data[[var]])) {
            # Get current levels to preserve order
            current_levels <- levels(data[[var]])
            
            # Convert to unordered factor while preserving level order
            data[[var]] <- factor(data[[var]], 
                                 levels = current_levels, 
                                 ordered = FALSE)
            
            converted_count <- converted_count + 1
            if (verbose) {
                log_enhanced(sprintf("  Converted ordered factor '%s' to unordered (levels: %s)", 
                                   var, paste(current_levels, collapse = ", ")), level = "INFO")
            }
        }
    }
    
    if (verbose && converted_count > 0) {
        log_enhanced(sprintf("✓ Converted %d ordered factors to unordered for modeling", converted_count), level = "INFO")
    } else if (verbose) {
        log_enhanced("✓ No ordered factors found - all factors already unordered", level = "INFO")
    }
    
    return(data)
}

#' Ensure Consistent Contrasts for Modeling
#'
#' Enforces consistent contrast options for all modeling functions.
#' This ensures factor variables use consistent naming across all models.
#'
#' @param data Data frame containing factor variables
#' @return Data frame with consistent contrasts applied
ensure_consistent_contrasts <- function(data) {
    # Set consistent contrast options for all modeling functions
    # This ensures factor variables use consistent naming across all models
    options(contrasts = c("contr.treatment", "contr.poly"))
    
    # Convert any ordered factors to unordered factors for modeling
    data <- enforce_unordered_factors(data)
    
    return(data)
}

#' Get Variable Labels for Display
#'
#' Returns a named vector of human-readable labels for variables.
#' Used for consistent labeling across tables and plots.
#'
#' @return Named character vector of variable labels
get_variable_labels <- function() {
    labels <- c(
        # Demographics
        "age_at_diagnosis" = "Age at Diagnosis",
        "sex" = "Sex",
        "race" = "Race",
        
        # Tumor characteristics
        "location" = "Tumor Location",
        "initial_t_stage" = "Initial T Stage",
        "initial_tumor_height" = "Initial Tumor Height (mm)",
        "initial_tumor_diameter" = "Initial Tumor Diameter (mm)",
        "initial_overall_stage" = "Initial Overall Stage",
        "initial_stage_binary" = "Initial Stage (Binary)",
        "optic_nerve" = "Optic Nerve Involvement",
        "internal_reflectivity" = "Internal Reflectivity",
        "srf" = "Subretinal Fluid",
        "op" = "Orange Pigment",
        
        # Symptoms
        "symptoms" = "Symptoms Present",
        "vision_loss_blurred_vision" = "Vision Loss/Blurred Vision",
        "visual_field_defect" = "Visual Field Defect",
        "flashes_photopsia" = "Flashes/Photopsia",
        "floaters" = "Floaters",
        "pain" = "Pain",
        
        # Treatment
        "treatment_group" = "Treatment Group",
        "recurrence1_treatment_clean" = "Recurrence Treatment",
        
        # Outcomes
        "recurrence1" = "Local Recurrence",
        "mets_progression" = "Metastatic Progression",
        "mss_event" = "Melanoma-Specific Death",
        "mets_event" = "Metastasis",
        "pfs_event" = "Progression-Free Survival",
        "pfs2_event" = "PFS-2",
        
        # GEP variables
        "biopsy1_gep" = "GEP Class",
        "gep_class_simple" = "GEP Class (Simplified)",
        "prame_status" = "PRAME Status",
        "gep_validation_set" = "GEP Validation Set",
        
        # Subgroup variables
        "age_at_diagnosis_binned" = "Age Group",
        "initial_tumor_height_binned" = "Tumor Height Group",
        "initial_tumor_diameter_binned" = "Tumor Diameter Group"
    )
    
    return(labels)
} 