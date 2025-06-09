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
#' @return Cutoff value
get_cutoff_value <- function(var_name, data, percentile_cut = 0.5) {
  if (USE_STANDARDIZED_CUTOFFS && var_name %in% names(STANDARDIZED_CUTOFFS)) {
    return(STANDARDIZED_CUTOFFS[[var_name]])
  } else {
    return(quantile(data[[var_name]], probs = percentile_cut, na.rm = TRUE))
  }
}

#' Get fixed, formatted subgroup levels for a variable (for plotting/alignment)
#' @param var_name Variable name
#' @return Character vector of levels, or NULL if not a continuous variable
get_subgroup_levels <- function(var_name) {
  if (var_name == "age_at_diagnosis") {
    cutoff <- STANDARDIZED_CUTOFFS$age_at_diagnosis
    return(c(paste0("< ", cutoff), paste0("\u2265 ", cutoff)))
  } else if (var_name == "initial_tumor_height") {
    cutoff <- STANDARDIZED_CUTOFFS$initial_tumor_height
    return(c(paste0("< ", cutoff), paste0("\u2265 ", cutoff)))
  } else if (var_name == "initial_tumor_diameter") {
    cutoff <- STANDARDIZED_CUTOFFS$initial_tumor_diameter
    return(c(paste0("< ", cutoff), paste0("\u2265 ", cutoff)))
  } else {
    return(NULL)
  }
} 