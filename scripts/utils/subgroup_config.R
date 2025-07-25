# Subgroup Analysis Configuration
# Single source of truth for cutoffs and variable ordering

# Source the main analysis config for consistent variable ordering
source("scripts/utils/analysis_config.R")

# Variable order for consistent display across all plots and tables
# Uses the global FOREST_PLOT_VARIABLE_ORDER for consistency
SUBGROUP_VARIABLE_ORDER <- FOREST_PLOT_VARIABLE_ORDER

#' Get cutoff value for a variable
#' @param var_name Variable name
#' @param data Data frame (for median calculation)
#' @param percentile_cut Percentile to use if not standardized (default 0.5)
#' @return Cutoff value or vector of cutoffs
get_cutoff_value <- function(var_name, data, percentile_cut = 0.5) {
  if (USE_STANDARDIZED_CUTOFFS && var_name %in% names(STANDARDIZED_CUTOFFS)) {
    return(STANDARDIZED_CUTOFFS[[var_name]])
  } else {
    return(quantile(data[[var_name]], probs = percentile_cut, na.rm = TRUE))
  }
}

#' Create T-stage clinical bins for tumor height or diameter
#' @param values Numeric vector of height or diameter values
#' @param cutoffs Vector of cutoff values
#' @param var_name Variable name for labeling
#' @return Factor with T-stage clinical bin labels
create_clinical_bins <- function(values, cutoffs, var_name) {
  if (length(cutoffs) == 1) {
    # Single cutoff - create binary split (for backwards compatibility)
    bin_labels <- c(paste0("< ", cutoffs), paste0("\u2265 ", cutoffs))
    bins <- ifelse(values < cutoffs, bin_labels[1], bin_labels[2])
  } else {
    # Multiple cutoffs - create T-stage clinical bins
    bin_labels <- character(length(cutoffs) + 1)
    
    # First bin: ≤ first cutoff
    bin_labels[1] <- paste0("\u2264 ", cutoffs[1])
    
    # Middle bins: previous cutoff + 0.1 to current cutoff
    for (i in 2:length(cutoffs)) {
      bin_labels[i] <- paste0(cutoffs[i-1] + 0.1, "-", cutoffs[i])
    }
    
    # Last bin: > last cutoff
    bin_labels[length(cutoffs) + 1] <- paste0("> ", cutoffs[length(cutoffs)])
    
    # Create bins using cut function
    bins <- cut(values, 
                breaks = c(-Inf, cutoffs, Inf), 
                labels = bin_labels, 
                include.lowest = TRUE)
  }
  
  return(factor(bins, levels = bin_labels))
}

#' Get fixed, formatted subgroup levels for a variable (for plotting/alignment)
#' @param var_name Variable name
#' @return Character vector of levels, or NULL if not a continuous variable
get_subgroup_levels <- function(var_name) {
  if (!var_name %in% names(STANDARDIZED_CUTOFFS)) {
    return(NULL)
  }
  
  cutoffs <- STANDARDIZED_CUTOFFS[[var_name]]
  
  if (var_name == "age_at_diagnosis") {
    # Age uses simple binary split
    return(c(paste0("< ", cutoffs), paste0("\u2265 ", cutoffs)))
  } else if (var_name %in% c("initial_tumor_height", "initial_tumor_diameter")) {
    # Height and diameter use T-stage clinical bins
    if (length(cutoffs) == 1) {
      return(c(paste0("< ", cutoffs), paste0("\u2265 ", cutoffs)))
    } else {
      bin_labels <- character(length(cutoffs) + 1)
      bin_labels[1] <- paste0("\u2264 ", cutoffs[1])
      for (i in 2:length(cutoffs)) {
        bin_labels[i] <- paste0(cutoffs[i-1] + 0.1, "-", cutoffs[i])
      }
      bin_labels[length(cutoffs) + 1] <- paste0("> ", cutoffs[length(cutoffs)])
      return(bin_labels)
    }
  } else {
    return(NULL)
  }
} 