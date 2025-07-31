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