# Comprehensive Subgroup Analysis Functions
# Author: Nicholas Camarda
# Description: Unified functions for testing treatment interactions across all outcome types
# Consolidates functionality from subgroup_analysis.R and primary_outcomes_subgroup_analysis.R

# =============================================================================
# SUBGROUP CONFIGURATION FUNCTIONS
# =============================================================================

#' Get cutoff value for a variable (simplified for T-stage cutoffs)
#' @param var_name Variable name
#' @param data Data frame (for median calculation)
#' @param percentile_cut Percentile to use if not standardized (default 0.5)
#' @return Cutoff value or vector of cutoffs
get_cutoff_value <- function(var_name, data, percentile_cut = 0.5) {
  if (USE_T_STAGE_CUTOFFS && var_name %in% c("initial_tumor_height", "initial_tumor_diameter")) {
    # T-stage evidence-based cutoffs
    if (var_name == "initial_tumor_height") {
      return(T_STAGE_HEIGHT_CUTOFFS)
    } else if (var_name == "initial_tumor_diameter") {
      return(T_STAGE_DIAMETER_CUTOFFS)
    }
  } else if (var_name %in% names(LEGACY_CUTOFFS)) {
    # Legacy median-based cutoffs (default)
    return(LEGACY_CUTOFFS[[var_name]])
  } else {
    # Fallback to data-driven cutoffs
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

#' Get fixed, formatted subgroup levels for a variable (simplified for T-stage cutoffs)
#' @param var_name Variable name
#' @return Character vector of levels, or NULL if not a continuous variable
get_subgroup_levels <- function(var_name) {
  if (USE_T_STAGE_CUTOFFS && var_name %in% c("initial_tumor_height", "initial_tumor_diameter")) {
    # T-stage evidence-based levels
    if (var_name == "initial_tumor_height") {
      return(c(
        paste0("≤ ", T_STAGE_HEIGHT_CUTOFFS[1], " mm"),
        paste0(T_STAGE_HEIGHT_CUTOFFS[1] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[2], " mm"),
        paste0(T_STAGE_HEIGHT_CUTOFFS[2] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[3], " mm"),
        paste0(T_STAGE_HEIGHT_CUTOFFS[3] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[4], " mm"),
        paste0(T_STAGE_HEIGHT_CUTOFFS[4] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[5], " mm"),
        paste0("> ", T_STAGE_HEIGHT_CUTOFFS[5], " mm")
      ))
    } else if (var_name == "initial_tumor_diameter") {
      return(c(
        paste0("≤ ", T_STAGE_DIAMETER_CUTOFFS[1], " mm"),
        paste0(T_STAGE_DIAMETER_CUTOFFS[1] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[2], " mm"),
        paste0(T_STAGE_DIAMETER_CUTOFFS[2] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[3], " mm"),
        paste0(T_STAGE_DIAMETER_CUTOFFS[3] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[4], " mm"),
        paste0(T_STAGE_DIAMETER_CUTOFFS[4] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[5], " mm"),
        paste0(T_STAGE_DIAMETER_CUTOFFS[5] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[6], " mm"),
        paste0("> ", T_STAGE_DIAMETER_CUTOFFS[6], " mm")
      ))
    }
  } else if (var_name == "age_at_diagnosis") {
    # Age uses simple binary split (legacy)
    return(c(paste0("< ", LEGACY_CUTOFFS$age_at_diagnosis), 
             paste0("≥ ", LEGACY_CUTOFFS$age_at_diagnosis)))
  } else {
    return(NULL)
  }
}

# =============================================================================
# SUBGROUP ANALYSIS FUNCTIONS
# =============================================================================



#' Analyze treatment effects across subgroups for survival outcomes
#'
#' Performs subgroup analysis for time-to-event outcomes (OS, PFS, recurrence, metastatic progression)
#' using Cox regression with interaction terms
#'
#' @param data Data frame containing the analysis variables
#' @param time_var Name of the time variable
#' @param event_var Name of the event indicator variable
#' @param subgroup_vars Character vector of subgroup variables to test
#' @param confounders Character vector of confounders
#' @param outcome_name Name of the outcome for labeling
#' @return List of subgroup analysis results
analyze_treatment_effect_subgroups_survival <- function(data, time_var, event_var, subgroup_vars, confounders = NULL, outcome_name = "Survival") {
    log_enhanced(sprintf("Performing subgroup analysis for %s", outcome_name), level = "INFO")
    
    subgroup_results <- list()

    for (subgroup_var in subgroup_vars) {
        log_enhanced(sprintf("Testing interaction for: %s", subgroup_var), level = "INFO")

        tryCatch({
            # Common data processing steps
            processed_results <- process_subgroup_data(data, subgroup_var, confounders, FALSE)
            
            # Check if processing failed due to insufficient levels
            if (!is.null(processed_results$error) && processed_results$error == "insufficient_levels") {
                log_enhanced(sprintf("Skipping %s: insufficient valid levels after rare category handling", subgroup_var), level = "WARN")
                subgroup_results[[subgroup_var]] <- list(
                    interaction_p = NA,
                    subgroup_effects = data.frame(),
                    error = "insufficient_levels"
                )
                next
            }
            
            # Build and fit model for survival outcome
            outcome_config <- list(type = "survival", time_var = time_var, event_var = event_var)
            model_results <- fit_subgroup_model(
                processed_results$data, 
                outcome_config, 
                processed_results$subgroup_var_to_use,
                processed_results$confounders_to_use
            )
            
            # Calculate effects for each subgroup level using the same filtered data as the interaction test
            if (!is.null(model_results$filtered_data)) {
                log_enhanced(sprintf("  SURVIVAL: Using filtered data with %d rows for %s (original had %d)", nrow(model_results$filtered_data), subgroup_var, nrow(processed_results$data)), level = "DEBUG")
                data_for_effects <- model_results$filtered_data
            } else {
                log_enhanced(sprintf("  SURVIVAL WARNING: No filtered data for %s, using original %d rows", subgroup_var, nrow(processed_results$data)), level = "WARN")
                data_for_effects <- processed_results$data
            }
            
            subgroup_effects <- calculate_subgroup_effects(
                model_results$model,
                data_for_effects,
                processed_results$subgroup_var_to_use,
                outcome_config$type,
                subgroup_var
            )
            
            subgroup_results[[subgroup_var]] <- list(
                interaction_p = model_results$interaction_p,
                subgroup_effects = subgroup_effects,
                model = model_results$model,
                subgroup_var_used = processed_results$subgroup_var_to_use,
                formula_used = model_results$formula_used,
                confounders_used = processed_results$confounders_to_use,
                was_continuous = processed_results$was_continuous,
                cutoff_value = processed_results$cutoff_value,
                interaction_diagnostics = model_results$interaction_diagnostics,
                other_map = processed_results$other_map # Collect other_map from process_subgroup_data
            )

            log_enhanced(sprintf("  Interaction p-value: %.4f", ifelse(is.na(model_results$interaction_p), 999, model_results$interaction_p)), level = "INFO")
            
        }, error = function(e) {
            log_enhanced(sprintf("Error in subgroup analysis for %s: %s", subgroup_var, e$message), level = "ERROR")
            subgroup_results[[subgroup_var]] <- list(
                interaction_p = NA,
                subgroup_effects = data.frame(),
                error = e$message
            )
        })
    }

    # Collect other_map from all variables
    other_map <- list()
    for (var_name in names(subgroup_results)) {
        if (!is.null(subgroup_results[[var_name]]) && !is.null(subgroup_results[[var_name]]$other_map)) {
            other_map[[var_name]] <- subgroup_results[[var_name]]$other_map
        }
    }

    return(list(
        subgroup_results = subgroup_results,
        other_map = other_map
    ))
}

#' Analyze treatment effects across subgroups for binary outcomes
#'
#' Performs subgroup analysis for binary outcomes (local recurrence, metastatic progression)
#' using logistic regression with interaction terms
#'
#' @param data Data frame containing the analysis variables
#' @param outcome_var Name of the binary outcome variable
#' @param subgroup_vars Character vector of subgroup variables to test
#' @param confounders Character vector of confounders
#' @param outcome_name Name of the outcome for labeling
#' @return List of subgroup analysis results
analyze_treatment_effect_subgroups_binary <- function(data, outcome_var, subgroup_vars, confounders = NULL, outcome_name = "Binary Outcome") {
    log_enhanced(sprintf("Performing subgroup analysis for %s", outcome_name), level = "INFO")
    
    subgroup_results <- list()

    for (subgroup_var in subgroup_vars) {
        log_enhanced(sprintf("Testing interaction for: %s", subgroup_var), level = "INFO")

        tryCatch({
            # Common data processing steps
            processed_results <- process_subgroup_data(data, subgroup_var, confounders, FALSE)
            
            # Check if processing failed due to insufficient levels
            if (!is.null(processed_results$error) && processed_results$error == "insufficient_levels") {
                log_enhanced(sprintf("Skipping %s: insufficient valid levels after rare category handling", subgroup_var), level = "WARN")
                subgroup_results[[subgroup_var]] <- list(
                    interaction_p = NA,
                    subgroup_effects = data.frame(),
                    error = "insufficient_levels"
                )
                next
            }
            
            # Build and fit model for binary outcome
            outcome_config <- list(type = "binary", outcome_var = outcome_var)
            model_results <- fit_subgroup_model(
                processed_results$data, 
                outcome_config, 
                processed_results$subgroup_var_to_use,
                processed_results$confounders_to_use
            )
            
            # Calculate effects for each subgroup level using the same filtered data as the interaction test
            data_for_effects <- if (!is.null(model_results$filtered_data)) {
                model_results$filtered_data
            } else {
                processed_results$data  # Fallback to original data if filtering failed
            }
            
            subgroup_effects <- calculate_subgroup_effects(
                model_results$model,
                data_for_effects,
                processed_results$subgroup_var_to_use,
                outcome_config$type,
                subgroup_var
            )
            
            subgroup_results[[subgroup_var]] <- list(
                interaction_p = model_results$interaction_p,
                subgroup_effects = subgroup_effects,
                model = model_results$model,
                subgroup_var_used = processed_results$subgroup_var_to_use,
                formula_used = model_results$formula_used,
                confounders_used = processed_results$confounders_to_use,
                was_continuous = processed_results$was_continuous,
                cutoff_value = processed_results$cutoff_value,
                interaction_diagnostics = model_results$interaction_diagnostics,
                other_map = processed_results$other_map # Collect other_map from process_subgroup_data
            )

            log_enhanced(sprintf("  Interaction p-value: %.4f", ifelse(is.na(model_results$interaction_p), 999, model_results$interaction_p)), level = "INFO")
            
        }, error = function(e) {
            log_enhanced(sprintf("Error in subgroup analysis for %s: %s", subgroup_var, e$message), level = "ERROR")
            subgroup_results[[subgroup_var]] <- list(
                interaction_p = NA,
                subgroup_effects = data.frame(),
                error = e$message
            )
        })
    }

    # Collect other_map from all variables
    other_map <- list()
    for (var_name in names(subgroup_results)) {
        if (!is.null(subgroup_results[[var_name]]) && !is.null(subgroup_results[[var_name]]$other_map)) {
            other_map[[var_name]] <- subgroup_results[[var_name]]$other_map
        }
    }

    return(list(
        subgroup_results = subgroup_results,
        other_map = other_map
    ))
}

#' Analyze treatment effects across subgroups for tumor height change
#'
#' Tests whether treatment effect on tumor height change differs across subgroups
#' using interaction terms. Bins continuous variables at specified percentile.
#'
#' @param data Data frame containing the analysis variables
#' @param subgroup_var Name of the subgroup variable (character)
#' @param percentile_cut Percentile for binning continuous variables (default: 0.5 for median split)
#' @param confounders Character vector of confounders to adjust for (subgroup variable will be automatically excluded)
#' @param include_baseline_height Logical, whether to include initial_tumor_height as a confounder (default: FALSE for primary analysis)
#'
#' @return List containing:
#'   - interaction_p: P-value for the interaction term
#'   - subgroup_effects: Data frame with treatment effects in each subgroup
#'   - model: The fitted linear model object
#'   - subgroup_var_used: Name of the binned variable created
#'   - formula_used: The formula used for the model
#'   - confounders_used: Character vector of confounders actually used in the model
#'
#' @examples
#' analyze_treatment_effect_subgroups_height(data, "age_at_diagnosis", confounders = c("sex", "location"))
analyze_treatment_effect_subgroups_height <- function(data, subgroup_var, percentile_cut = 0.5, confounders = NULL, include_baseline_height = FALSE) {
    # height_change variable should already be calculated in data_processing.R
    
    # Check if subgroup variable exists and has variation
    if (!subgroup_var %in% names(data)) {
        warning(sprintf("Variable '%s' not found in data", subgroup_var))
        return(list(interaction_p = NA, subgroup_effects = data.frame(), 
                   model = NULL, subgroup_var_used = NA, formula_used = NA, confounders_used = NA))
    }
    
    # Remove rows with missing subgroup variable
    data <- data %>% filter(!is.na(.data[[subgroup_var]]))
    
    if (nrow(data) == 0) {
        warning(sprintf("No data remaining after removing missing values for '%s'", subgroup_var))
        return(list(interaction_p = NA, subgroup_effects = data.frame(), 
                   model = NULL, subgroup_var_used = NA, formula_used = NA, confounders_used = NA))
    }
    
    # Process subgroup data
    processed_results <- process_subgroup_data(data, subgroup_var, confounders, include_baseline_height)
    
    # Check if processing failed due to insufficient levels
    if (!is.null(processed_results$error) && processed_results$error == "insufficient_levels") {
        return(list(
            interaction_p = NA, 
            subgroup_effects = data.frame(), 
            model = NULL, 
            subgroup_var_used = NA, 
            formula_used = NA, 
            confounders_used = NA,
            error = "insufficient_levels"
        ))
    }
    
    # Build and fit model for tumor height (continuous outcome)
    outcome_config <- list(type = "continuous", outcome_var = "height_change")
    model_results <- fit_subgroup_model(
        processed_results$data, 
        outcome_config, 
        processed_results$subgroup_var_to_use,
        processed_results$confounders_to_use
    )
    
    # Calculate effects for each subgroup level using the same filtered data as the interaction test
    if (!is.null(model_results$filtered_data)) {
        log_enhanced(sprintf("  CONTINUOUS: Using filtered data with %d rows for %s (original had %d)", nrow(model_results$filtered_data), subgroup_var, nrow(processed_results$data)), level = "DEBUG")
        data_for_effects <- model_results$filtered_data
    } else {
        log_enhanced(sprintf("  CONTINUOUS WARNING: No filtered data for %s, using original %d rows", subgroup_var, nrow(processed_results$data)), level = "WARN")
        data_for_effects <- processed_results$data
    }
    
    subgroup_effects <- calculate_subgroup_effects(
        model_results$model,
        data_for_effects,
        processed_results$subgroup_var_to_use,
        outcome_config$type,
        subgroup_var
    )
    
    return(list(
        interaction_p = model_results$interaction_p,
        subgroup_effects = subgroup_effects,
        model = model_results$model,
        subgroup_var_used = processed_results$subgroup_var_to_use,
        formula_used = model_results$formula_used,
        confounders_used = processed_results$confounders_to_use,
        interaction_diagnostics = model_results$interaction_diagnostics,
        other_map = if (!is.null(processed_results$other_map)) processed_results$other_map else list()
    ))
}

#' Process subgroup data (common steps for all outcome types)
#' @param data Input data
#' @param subgroup_var Subgroup variable name
#' @param confounders Vector of confounder names
#' @param include_baseline_height For tumor height analysis - include initial height
#' @return List with processed data and variable names
process_subgroup_data <- function(data, subgroup_var, confounders, include_baseline_height = FALSE) {
    # Initialize other_map
    other_map <- list()
    
    # Check if subgroup variable exists
    if (!subgroup_var %in% names(data)) {
        stop(sprintf("Variable '%s' not found in data", subgroup_var))
    }
    
    # Remove rows with missing subgroup variable
    data <- data %>% filter(!is.na(.data[[subgroup_var]]))
    
    if (nrow(data) == 0) {
        stop(sprintf("No data remaining after removing missing values for '%s'", subgroup_var))
    }
    
    # Filter confounders to exclude the subgroup variable
    confounders_to_use <- if (!is.null(confounders)) {
        confounders[confounders != subgroup_var]
    } else {
        NULL
    }
    
    # Add initial_tumor_height if requested for tumor height analysis
    if (include_baseline_height && !"initial_tumor_height" %in% confounders_to_use) {
        confounders_to_use <- c(confounders_to_use, "initial_tumor_height")
    }

    # Validate confounders
    if (length(confounders_to_use) > 0) {
        confounders_to_use <- generate_valid_confounders(data, confounders_to_use, threshold = THRESHOLD_RARITY)
    }

    # Process subgroup variable (bin if continuous)
    processed_data <- data
    was_continuous <- is.numeric(data[[subgroup_var]]) || is.integer(data[[subgroup_var]])
    cutoff_value <- NULL

    if (was_continuous) {
        # Use centralized cutoff configuration
        cutoff_val <- get_cutoff_value(subgroup_var, data, 0.5)
        cutoff_type <- if (USE_T_STAGE_CUTOFFS && subgroup_var %in% c("initial_tumor_height", "initial_tumor_diameter")) "standardized" else "legacy"
        
        # Check if this variable uses T-stage clinical bins
        if (subgroup_var %in% c("initial_tumor_height", "initial_tumor_diameter") && 
            USE_T_STAGE_CUTOFFS && length(cutoff_val) > 1) {
            # Use T-stage clinical bins
            log_enhanced(sprintf("Using T-stage clinical bins for %s: %s", subgroup_var, paste(cutoff_val, collapse=", ")), level = "INFO")
            
            subgroup_var_binned <- paste0(subgroup_var, "_binned")
            processed_data[[subgroup_var_binned]] <- create_clinical_bins(data[[subgroup_var]], cutoff_val, subgroup_var)
            subgroup_var_to_use <- subgroup_var_binned
            cutoff_value <- cutoff_val
            
                    # Rare category handling already done in data processing
        other_map <- list()
            
        } else {
            # Use simple binary split (original logic)
            log_enhanced(sprintf("Using %s cutoff for %s: %.1f", cutoff_type, subgroup_var, cutoff_val), level = "INFO")
            
            subgroup_var_binned <- paste0(subgroup_var, "_binned")
            processed_data[[subgroup_var_binned]] <- factor(
                ifelse(data[[subgroup_var]] < cutoff_val,
                    paste0("< ", round(cutoff_val, 1)),
                    paste0("≥ ", round(cutoff_val, 1))
                ),
                levels = c(
                    paste0("< ", round(cutoff_val, 1)),
                    paste0("≥ ", round(cutoff_val, 1))
                )
            )
            subgroup_var_to_use <- subgroup_var_binned
            cutoff_value <- cutoff_val
        }
    } else {
        if (!is.factor(processed_data[[subgroup_var]])) {
            processed_data[[subgroup_var]] <- as.factor(processed_data[[subgroup_var]])
        }
        # Rare category handling already done in data processing
        other_map <- list()
        subgroup_var_to_use <- subgroup_var
    }

    # Check if subgroup variable has sufficient levels after processing
    if (is.factor(processed_data[[subgroup_var_to_use]])) {
        # Check both the number of levels AND that each level has at least some observations
        level_counts <- table(processed_data[[subgroup_var_to_use]])
        valid_levels <- sum(level_counts > 0)
        
        if (valid_levels < 2) {
            warning(sprintf("Variable %s has insufficient valid levels (%d) after rare category handling. Level counts: %s", 
                           subgroup_var, valid_levels, paste(names(level_counts), "=", level_counts, collapse=", ")))
            return(list(
                data = NULL,
                subgroup_var_to_use = NULL,
                confounders_to_use = NULL,
                was_continuous = FALSE,
                cutoff_value = NA,
                error = "insufficient_levels"
            ))
        }
    }

    # Ensure consistent contrasts
    processed_data <- enforce_unordered_factors(processed_data)
    
    return(list(
        data = processed_data,
        subgroup_var_to_use = subgroup_var_to_use,
        confounders_to_use = confounders_to_use,
        was_continuous = was_continuous,
        cutoff_value = cutoff_value,
        other_map = other_map
    ))
}

#' Fit model with interaction based on outcome type
#' @param data Processed data
#' @param outcome_config Outcome configuration
#' @param subgroup_var_to_use Processed subgroup variable name
#' @param confounders_to_use Valid confounders
#' @return List with fitted model and interaction p-value
fit_subgroup_model <- function(data, outcome_config, subgroup_var_to_use, confounders_to_use) {
    
    # Initialize interaction diagnostics
    interaction_diagnostics <- list()
    
    # Initialize interaction_p variable at function scope
    interaction_p <- NA
    
    # CRITICAL FIX: Filter data to only include subgroup levels that will produce valid effects
    # This ensures the interaction test runs on the same data that gets displayed
    valid_levels <- c()
    subgroup_levels <- levels(data[[subgroup_var_to_use]])
    
    for (level in subgroup_levels) {
        level_data <- data[data[[subgroup_var_to_use]] == level, ]
        
        # Check if this level has sufficient data for both treatment groups
        n_plaque <- sum(level_data$treatment_group == "Plaque", na.rm = TRUE)
        n_gksrs <- sum(level_data$treatment_group == "GKSRS", na.rm = TRUE)
        
        # CRITICAL DEBUG: Log exact counts for problematic levels
        if (level %in% c("T4b", "12.1-15", "Other")) {
            log_enhanced(sprintf("  DEBUG PROBLEMATIC LEVEL %s: n_plaque=%d, n_gksrs=%d, total_rows=%d", 
                               level, n_plaque, n_gksrs, nrow(level_data)), level = "ERROR")
        }
        
        # For survival outcomes, also check event counts
        if (outcome_config$type == "survival") {
            # Find the event variable
            event_vars <- c("death_event", "mets_event", "pfs_event", "event")
            event_var <- NULL
            for (ev in event_vars) {
                if (ev %in% names(level_data)) {
                    event_var <- ev
                    break
                }
            }
            
            if (!is.null(event_var)) {
                # Count events by treatment group
                plaque_events <- sum(level_data$treatment_group == "Plaque" & level_data[[event_var]] == 1, na.rm = TRUE)
                gksrs_events <- sum(level_data$treatment_group == "GKSRS" & level_data[[event_var]] == 1, na.rm = TRUE)
                
                # Require at least 2 patients AND at least 1 event in each treatment group
                if (n_plaque >= 2 && n_gksrs >= 2 && plaque_events >= 1 && gksrs_events >= 1) {
                    valid_levels <- c(valid_levels, level)
                    log_enhanced(sprintf("  SURVIVAL: INCLUDED level %s (n_plaque=%d, n_gksrs=%d, events_plaque=%d, events_gksrs=%d)", 
                                       level, n_plaque, n_gksrs, plaque_events, gksrs_events), level = "DEBUG")
                }
                # Store diagnostics for excluded levels
                if (!(n_plaque >= 2 && n_gksrs >= 2 && plaque_events >= 1 && gksrs_events >= 1)) {
                    interaction_diagnostics[[paste0("excluded_", level)]] <- list(
                        n_plaque = n_plaque, n_gksrs = n_gksrs,
                        events_plaque = plaque_events, events_gksrs = gksrs_events,
                        reason = sprintf("Insufficient data: n_plaque=%d, n_gksrs=%d, events_plaque=%d, events_gksrs=%d", 
                                       n_plaque, n_gksrs, plaque_events, gksrs_events)
                    )
                }
            } else {
                # No event variable found, fall back to patient count only
                interaction_diagnostics[[paste0("no_event_var_", level)]] <- list(
                    n_plaque = n_plaque, n_gksrs = n_gksrs,
                    reason = sprintf("No event variable found, using patient counts: n_plaque=%d, n_gksrs=%d", n_plaque, n_gksrs)
                )
                if (n_plaque >= 2 && n_gksrs >= 2) {
                    valid_levels <- c(valid_levels, level)
                    interaction_diagnostics[[paste0("included_", level)]] <- "Included based on patient counts"
                    log_enhanced(sprintf("  NO_EVENT_VAR: INCLUDED level %s (n_plaque=%d, n_gksrs=%d)", 
                                       level, n_plaque, n_gksrs), level = "DEBUG")
                } else {
                    interaction_diagnostics[[paste0("excluded_", level)]] <- sprintf(
                        "Excluded: insufficient patients n_plaque=%d, n_gksrs=%d (need >=2 each)", n_plaque, n_gksrs
                    )
                }
            }
        } else {
            # For binary outcomes, just require sufficient patients
            # TODO: Could also check for outcome event counts here
            if (n_plaque >= 2 && n_gksrs >= 2) {
                valid_levels <- c(valid_levels, level)
                log_enhanced(sprintf("  BINARY/CONTINUOUS: INCLUDED level %s (n_plaque=%d, n_gksrs=%d)", 
                                   level, n_plaque, n_gksrs), level = "DEBUG")
            } else {
                log_enhanced(sprintf("  BINARY/CONTINUOUS: EXCLUDED level %s (n_plaque=%d, n_gksrs=%d)", 
                                   level, n_plaque, n_gksrs), level = "DEBUG")
            }
        }
    }
    
    # Filter data to only include valid levels
    if (length(valid_levels) == 0) {
        if (outcome_config$type == "survival") {
            interaction_diagnostics$failure_reason <- "No subgroup levels with sufficient patients (>=2 each) AND events (>=1 each) in both treatment groups"
        } else {
            interaction_diagnostics$failure_reason <- "No subgroup levels with sufficient patients (>=2 each) in both treatment groups"
        }
        return(list(
            model = NULL,
            interaction_p = NA,
            formula_used = NA,
            interaction_diagnostics = interaction_diagnostics,
            filtered_data = NULL  # Return NULL filtered_data when no valid levels
        ))
    }
    
    # Filter the data to only valid levels
    if (length(valid_levels) > 0) {
        filtered_data <- data[data[[subgroup_var_to_use]] %in% valid_levels, ]
        
        # CRITICAL: Force factor to only have valid levels
        filtered_data[[subgroup_var_to_use]] <- factor(
            filtered_data[[subgroup_var_to_use]], 
            levels = valid_levels
        )
        
        # Log the filtering results
        log_enhanced(sprintf("  FILTERING: %s reduced from %d to %d rows, levels from %d to %d", 
                           subgroup_var_to_use, nrow(data), nrow(filtered_data), 
                           length(subgroup_levels), length(valid_levels)), level = "INFO")
        log_enhanced(sprintf("  EXCLUDED LEVELS: %s", paste(setdiff(subgroup_levels, valid_levels), collapse = ", ")), level = "INFO")
    } else {
        filtered_data <- data[FALSE, ]  # Empty data frame with same structure
    }
    
    interaction_diagnostics$original_levels <- length(subgroup_levels)
    interaction_diagnostics$original_level_names <- paste(subgroup_levels, collapse = ", ")
    interaction_diagnostics$valid_levels <- length(valid_levels)
    interaction_diagnostics$valid_level_names <- paste(valid_levels, collapse = ", ")
    interaction_diagnostics$excluded_level_names <- paste(setdiff(subgroup_levels, valid_levels), collapse = ", ")
    
    # Initialize interaction_p variable at function scope
    interaction_p <- NA
    
    # Build base formula components
    confounders_str <- if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
        ""
    } else {
        paste(" + ", paste(confounders_to_use, collapse = " + "))
    }
    
    interaction_term <- paste0("treatment_group * ", subgroup_var_to_use)
    
    # Simple check: if only one level remains, skip model creation
    if (length(unique(filtered_data[[subgroup_var_to_use]])) < 2) {
        interaction_diagnostics$failure_reason <- "Only one factor level remains after filtering"
        return(list(
            model = NULL,
            interaction_p = NA,
            formula_used = NA,
            interaction_diagnostics = interaction_diagnostics,
            filtered_data = filtered_data
        ))
    }
    
    # Check all factor variables in the model for sufficient levels
    all_vars <- c("treatment_group", subgroup_var_to_use, confounders_to_use)
    
    # CRITICAL FIX: Check that all factor variables have at least 2 levels
    for (var in all_vars) {
        if (var %in% names(filtered_data) && is.factor(filtered_data[[var]])) {
            unique_levels <- unique(filtered_data[[var]])
            if (length(unique_levels) < 2) {
                interaction_diagnostics$failure_reason <- sprintf("Variable %s has only %d level(s) after filtering: %s", 
                                                                var, length(unique_levels), paste(unique_levels, collapse = ", "))
                return(list(
                    model = NULL,
                    interaction_p = NA,
                    formula_used = NA,
                    interaction_diagnostics = interaction_diagnostics,
                    filtered_data = filtered_data
                ))
            }
        }
    }
    for (var in all_vars) {
        if (var %in% names(filtered_data) && is.factor(filtered_data[[var]])) {
            var_levels <- unique(filtered_data[[var]][!is.na(filtered_data[[var]])])
            if (length(var_levels) < 2) {
                interaction_diagnostics$failure_reason <- sprintf("Variable '%s' has only %d level(s), need >=2", var, length(var_levels))
                return(list(
                    model = NULL,
                    interaction_p = NA,
                    formula_used = NA,
                    interaction_diagnostics = interaction_diagnostics,
                    filtered_data = filtered_data
                ))
            }
        }
    }
    
    # Additional check: validate the final model formula variables
    model_vars <- c("treatment_group", subgroup_var_to_use)
    if (!is.null(confounders_to_use) && length(confounders_to_use) > 0) {
        model_vars <- c(model_vars, confounders_to_use)
    }
    
    for (var in model_vars) {
        if (var %in% names(filtered_data)) {
            if (is.factor(filtered_data[[var]])) {
                var_levels <- unique(filtered_data[[var]][!is.na(filtered_data[[var]])])
                if (length(var_levels) < 2) {
                    interaction_diagnostics$failure_reason <- sprintf("Model variable '%s' has only %d level(s) after filtering, need >=2", var, length(var_levels))
                    return(list(
                        model = NULL,
                        interaction_p = NA,
                        formula_used = NA,
                        interaction_diagnostics = interaction_diagnostics,
                        filtered_data = filtered_data
                    ))
                }
            }
        }
    }
    
    # CRITICAL FIX: Final check before model fitting - ensure all variables in the model have sufficient levels
    model_vars <- c("treatment_group", subgroup_var_to_use)
    if (!is.null(confounders_to_use) && length(confounders_to_use) > 0) {
        model_vars <- c(model_vars, confounders_to_use)
    }
    
    # Check each variable that will be in the model
    for (var in model_vars) {
        if (var %in% names(filtered_data)) {
            if (is.factor(filtered_data[[var]])) {
                var_levels <- unique(filtered_data[[var]][!is.na(filtered_data[[var]])])
                if (length(var_levels) < 2) {
                    interaction_diagnostics$failure_reason <- sprintf("Model variable '%s' has only %d level(s): %s", 
                                                                    var, length(var_levels), paste(var_levels, collapse = ", "))
                    return(list(
                        model = NULL,
                        interaction_p = NA,
                        formula_used = NA,
                        interaction_diagnostics = interaction_diagnostics,
                        filtered_data = filtered_data
                    ))
                }
            }
        }
    }
    
    # Build formula based on outcome type - using filtered_data
    if (outcome_config$type == "survival") {
        formula_str <- paste0("Surv(", outcome_config$time_var, ", ", outcome_config$event_var, ") ~ ", 
                             interaction_term, confounders_str)
        model <- tryCatch({
            coxph(as.formula(formula_str), data = filtered_data, model = TRUE)
        }, error = function(e) {
            interaction_diagnostics$model_error <- e$message
            NULL
        })
        no_interaction_formula <- paste0("Surv(", outcome_config$time_var, ", ", outcome_config$event_var, ") ~ ", 
                                        "treatment_group + ", subgroup_var_to_use, confounders_str)
        no_interaction_model <- coxph(as.formula(no_interaction_formula), data = filtered_data, model = TRUE)
        
    } else if (outcome_config$type == "binary") {
        formula_str <- paste0(outcome_config$outcome_var, " ~ ", interaction_term, confounders_str)
        model <- tryCatch({
            glm(as.formula(formula_str), data = filtered_data, family = binomial())
        }, error = function(e) {
            interaction_diagnostics$model_error <- e$message
            NULL
        })
        no_interaction_formula <- paste0(outcome_config$outcome_var, " ~ ", 
                                        "treatment_group + ", subgroup_var_to_use, confounders_str)
        no_interaction_model <- tryCatch({
            glm(as.formula(no_interaction_formula), data = filtered_data, family = binomial())
        }, error = function(e) {
            NULL
        })
        
    } else if (outcome_config$type == "continuous") {
        formula_str <- paste0(outcome_config$outcome_var, " ~ ", interaction_term, confounders_str)
        model <- tryCatch({
            lm(as.formula(formula_str), data = filtered_data)
        }, error = function(e) {
            interaction_diagnostics$model_error <- e$message
            NULL
        })
        no_interaction_formula <- paste0(outcome_config$outcome_var, " ~ ", 
                                        "treatment_group + ", subgroup_var_to_use, confounders_str)
        no_interaction_model <- tryCatch({
            lm(as.formula(no_interaction_formula), data = filtered_data)
        }, error = function(e) {
            NULL
        })
    }
    
    # Calculate interaction p-value with detailed diagnostics
    subgroup_levels <- levels(filtered_data[[subgroup_var_to_use]])
    interaction_diagnostics$subgroup_levels <- subgroup_levels
    interaction_diagnostics$n_levels <- length(subgroup_levels)
    
    # Check if model fitting succeeded
    if (is.null(model) || inherits(model, "try-error")) {
        interaction_p <- NA
        interaction_diagnostics$failure_reason <- "Model fitting failed"
        interaction_diagnostics$model_error <- if (inherits(model, "try-error")) as.character(model) else "Model is NULL"
    } else if (length(subgroup_levels) < 2) {
        interaction_p <- NA
        interaction_diagnostics$failure_reason <- "Insufficient subgroup levels (<2)"
    } else if (length(subgroup_levels) == 2) {
        # Simple interaction test for binary subgroup
        interaction_coef_name <- get_interaction_coefficient_name(
            model, "treatment_group", subgroup_var_to_use, subgroup_levels[2], filtered_data
        )
        interaction_diagnostics$coefficient_name <- interaction_coef_name
        
        if (is.null(interaction_coef_name)) {
            interaction_p <- NA
            interaction_diagnostics$failure_reason <- "Interaction coefficient not found in model"
        } else if (!interaction_coef_name %in% rownames(summary(model)$coefficients)) {
            interaction_p <- NA
            interaction_diagnostics$failure_reason <- "Interaction coefficient missing from summary"
        } else {
            tryCatch({
                if (outcome_config$type == "survival") {
                    interaction_p <- summary(model)$coefficients[interaction_coef_name, "Pr(>|z|)"]
                } else if (outcome_config$type == "binary") {
                    interaction_p <- summary(model)$coefficients[interaction_coef_name, "Pr(>|z|)"]
                } else {
                    interaction_p <- summary(model)$coefficients[interaction_coef_name, "Pr(>|t|)"]
                }
                interaction_diagnostics$failure_reason <- "None"
            }, error = function(e) {
                interaction_p <<- NA
                interaction_diagnostics$failure_reason <<- paste("Error extracting p-value:", e$message)
            })
        }
            } else {
            # Multiple levels - use likelihood ratio test
            interaction_diagnostics$anova_attempt_reason <- sprintf("Attempting ANOVA with %d levels: %s", 
                                                                   length(subgroup_levels), paste(subgroup_levels, collapse=", "))
            tryCatch({
                if (outcome_config$type == "survival") {
                interaction_test <- anova(no_interaction_model, model)
                # Add detailed diagnostics
                interaction_diagnostics$anova_nrows <- nrow(interaction_test)
                interaction_diagnostics$anova_colnames <- names(interaction_test)
                interaction_diagnostics$anova_has_pr_chi <- "Pr(>Chi)" %in% names(interaction_test)
                interaction_diagnostics$anova_pr_chi_values <- if ("Pr(>Chi)" %in% names(interaction_test)) interaction_test$`Pr(>Chi)` else "MISSING"
                interaction_diagnostics$anova_result_summary <- capture.output(print(interaction_test))
                
                if (nrow(interaction_test) >= 2) {
                    # Try different ways to extract the p-value
                    pr_chi_col <- NULL
                    for (col_name in names(interaction_test)) {
                        if (grepl("Pr.*Chi", col_name, ignore.case = TRUE)) {
                            pr_chi_col <- col_name
                            break
                        }
                    }
                    
                    if (!is.null(pr_chi_col) && length(interaction_test[[pr_chi_col]]) >= 2) {
                        interaction_p <- interaction_test[[pr_chi_col]][2]
                        interaction_diagnostics$failure_reason <- "None"
                        interaction_diagnostics$pr_chi_column_used <- pr_chi_col
                    } else {
                        interaction_p <- NA
                        interaction_diagnostics$failure_reason <- sprintf(
                            "ANOVA p-value extraction failed: nrows=%d, pr_chi_col=%s, colnames=%s", 
                            nrow(interaction_test),
                            ifelse(is.null(pr_chi_col), "NULL", pr_chi_col),
                            paste(names(interaction_test), collapse=", ")
                        )
                    }
                } else {
                    interaction_p <- NA
                    interaction_diagnostics$failure_reason <- sprintf("ANOVA insufficient rows: %d", nrow(interaction_test))
                }
            } else if (outcome_config$type == "binary") {
                interaction_test <- anova(no_interaction_model, model, test = "Chisq")
                # Add detailed diagnostics
                interaction_diagnostics$anova_nrows <- nrow(interaction_test)
                interaction_diagnostics$anova_colnames <- names(interaction_test)
                interaction_diagnostics$anova_result <- interaction_test
                
                if (nrow(interaction_test) >= 2 && "Pr(>Chi)" %in% names(interaction_test)) {
                    interaction_p <- interaction_test$`Pr(>Chi)`[2]
                    interaction_diagnostics$failure_reason <- "None"
                } else {
                    interaction_p <- NA
                    interaction_diagnostics$failure_reason <- sprintf(
                        "Chi-square test failed: nrows=%d, has_pr_chi=%s, colnames=%s", 
                        nrow(interaction_test),
                        "Pr(>Chi)" %in% names(interaction_test),
                        paste(names(interaction_test), collapse=", ")
                    )
                }
            } else {
                # For continuous outcomes (like tumor height change)
                interaction_test <- anova(no_interaction_model, model)
                interaction_diagnostics$anova_nrows_continuous <- nrow(interaction_test)
                interaction_diagnostics$anova_colnames_continuous <- names(interaction_test)
                interaction_diagnostics$anova_result_continuous <- capture.output(print(interaction_test))
                
                if (nrow(interaction_test) >= 2 && "Pr(>F)" %in% names(interaction_test)) {
                    interaction_p <- interaction_test$`Pr(>F)`[2]
                    interaction_diagnostics$failure_reason <- "None"
                    interaction_diagnostics$anova_p_value_extracted <- interaction_p
                } else {
                    interaction_p <- NA
                    interaction_diagnostics$failure_reason <- sprintf(
                        "F-test failed: nrows=%d, has_pr_f=%s, colnames=%s", 
                        nrow(interaction_test),
                        "Pr(>F)" %in% names(interaction_test),
                        paste(names(interaction_test), collapse=", ")
                    )
                }
            }
        }, error = function(e) {
            interaction_p <- NA
            interaction_diagnostics$failure_reason <- paste("Likelihood ratio test error:", e$message)
            interaction_diagnostics$anova_error_details <- e$message
            interaction_diagnostics$model_summary <- if (!is.null(model)) capture.output(summary(model)) else "Model is NULL"
            interaction_diagnostics$no_interaction_model_summary <- if (!is.null(no_interaction_model)) capture.output(summary(no_interaction_model)) else "No-interaction model is NULL"
        })
    }
    
    return(list(
        model = model,
        interaction_p = interaction_p,
        formula_used = formula_str,
        interaction_diagnostics = interaction_diagnostics,
        filtered_data = filtered_data  # Return the filtered data so subgroup effects use the same data
    ))
}

#' Calculate subgroup effects for each level
#' @param model Fitted model
#' @param data Data used for fitting
#' @param subgroup_var_to_use Subgroup variable name
#' @param outcome_type Type of outcome
#' @param original_var_name Original variable name
#' @return Data frame of subgroup effects
calculate_subgroup_effects <- function(model, data, subgroup_var_to_use, outcome_type, original_var_name) {
    # Only process levels that actually exist in the filtered data
    subgroup_levels <- levels(data[[subgroup_var_to_use]])
    actual_levels <- unique(data[[subgroup_var_to_use]][!is.na(data[[subgroup_var_to_use]])])
    
    # CRITICAL DEBUG: Log what we're processing
    log_enhanced(sprintf("    CALCULATE_EFFECTS DEBUG for %s:", original_var_name), level = "DEBUG")
    log_enhanced(sprintf("      Data rows: %d", nrow(data)), level = "DEBUG")
    log_enhanced(sprintf("      Factor levels: %s", paste(subgroup_levels, collapse=", ")), level = "DEBUG")
    log_enhanced(sprintf("      Actual levels in data: %s", paste(actual_levels, collapse=", ")), level = "DEBUG")
    
    # Use only levels that have data (intersection of factor levels and actual data)
    levels_to_process <- intersect(subgroup_levels, actual_levels)
    log_enhanced(sprintf("      Levels to process: %s", paste(levels_to_process, collapse=", ")), level = "DEBUG")
    
    subgroup_effects <- data.frame()

    for (i in seq_along(levels_to_process)) {
        level <- levels_to_process[i]
        level_data <- data[data[[subgroup_var_to_use]] == level, ]

        n_total <- nrow(level_data)
        n_plaque <- sum(level_data$treatment_group == "Plaque", na.rm = TRUE)
        n_gksrs <- sum(level_data$treatment_group == "GKSRS", na.rm = TRUE)
        
        # Skip levels with no data (should not happen with proper filtering, but safety check)
        if (n_total == 0) {
            next
        }

        # Calculate events by treatment group based on outcome type
        events_plaque <- NA
        events_gksrs <- NA
        
        if (outcome_type == "survival") {
            # For survival outcomes, count events from the model data
            plaque_data <- level_data %>% filter(treatment_group == "Plaque")
            gksrs_data <- level_data %>% filter(treatment_group == "GKSRS")
            
            # Try to find event variable from common survival variable names
            event_vars <- c("death_event", "mets_event", "pfs_event", "event")
            found_event_var <- NULL
            for (ev in event_vars) {
                if (ev %in% names(level_data)) {
                    found_event_var <- ev
                    break
                }
            }
            
            if (!is.null(found_event_var)) {
                events_plaque <- sum(plaque_data[[found_event_var]] == 1, na.rm = TRUE)
                events_gksrs <- sum(gksrs_data[[found_event_var]] == 1, na.rm = TRUE)
            }
        } else if (outcome_type == "binary") {
            # For binary outcomes, count positive outcomes
            plaque_data <- level_data %>% filter(treatment_group == "Plaque")
            gksrs_data <- level_data %>% filter(treatment_group == "GKSRS")
            
            # Try to find outcome variable from common binary outcome names
            outcome_vars <- c("recurrence1", "mets_progression", "outcome")
            found_outcome_var <- NULL
            for (ov in outcome_vars) {
                if (ov %in% names(level_data)) {
                    found_outcome_var <- ov
                    break
                }
            }
            
            if (!is.null(found_outcome_var)) {
                outcome_var <- level_data[[found_outcome_var]]
                if (is.factor(outcome_var)) {
                    # Factor variable - count non-reference level (assuming first level is reference)
                    events_plaque <- sum(plaque_data[[found_outcome_var]] != levels(outcome_var)[1], na.rm = TRUE)
                    events_gksrs <- sum(gksrs_data[[found_outcome_var]] != levels(outcome_var)[1], na.rm = TRUE)
                } else {
                    # Numeric/logical - count 1s or TRUEs
                    events_plaque <- sum(plaque_data[[found_outcome_var]] == 1 | plaque_data[[found_outcome_var]] == TRUE, na.rm = TRUE)
                    events_gksrs <- sum(gksrs_data[[found_outcome_var]] == 1 | gksrs_data[[found_outcome_var]] == TRUE, na.rm = TRUE)
                }
            }
        }

        if (i == 1) {
            # Reference subgroup: main treatment effect
            coef_idx <- get_treatment_coefficient_name(model, "treatment_group", data)
            if (!is.null(coef_idx)) {
                if (outcome_type == "continuous") {
                    effect_est <- coef(model)[coef_idx]  # Don't exponentiate for continuous
                    se_effect <- sqrt(vcov(model)[coef_idx, coef_idx])
                    ci_lower <- effect_est - 1.96 * se_effect
                    ci_upper <- effect_est + 1.96 * se_effect
                    p_val <- summary(model)$coefficients[coef_idx, "Pr(>|t|)"]
                } else {
                    # For survival and binary: exponentiate to get HR/OR
                    effect_est <- exp(coef(model)[coef_idx])
                    se_effect <- sqrt(vcov(model)[coef_idx, coef_idx])
                    ci_lower <- exp(coef(model)[coef_idx] - 1.96 * se_effect)
                    ci_upper <- exp(coef(model)[coef_idx] + 1.96 * se_effect)
                    if (outcome_type == "survival") {
                        p_val <- summary(model)$coefficients[coef_idx, "Pr(>|z|)"]
                    } else {
                        p_val <- summary(model)$coefficients[coef_idx, "Pr(>|z|)"]
                    }
                }
            } else {
                effect_est <- NA
                ci_lower <- NA
                ci_upper <- NA
                p_val <- NA
            }
        } else {
            # Non-reference subgroup: combined effect
            main_coef_idx <- get_treatment_coefficient_name(model, "treatment_group", data)
            interaction_coef_idx <- get_interaction_coefficient_name(
                model, "treatment_group", subgroup_var_to_use, level, data
            )

            if (!is.null(main_coef_idx) && !is.null(interaction_coef_idx)) {
                combined_coef <- coef(model)[main_coef_idx] + coef(model)[interaction_coef_idx]
                
                # Standard error for combined effect
                var_main <- vcov(model)[main_coef_idx, main_coef_idx]
                var_int <- vcov(model)[interaction_coef_idx, interaction_coef_idx]
                cov_main_int <- vcov(model)[main_coef_idx, interaction_coef_idx]
                se_combined <- sqrt(var_main + var_int + 2 * cov_main_int)

                if (outcome_type == "continuous") {
                    effect_est <- combined_coef
                    ci_lower <- combined_coef - 1.96 * se_combined
                    ci_upper <- combined_coef + 1.96 * se_combined
                } else {
                    effect_est <- exp(combined_coef)
                    ci_lower <- exp(combined_coef - 1.96 * se_combined)
                    ci_upper <- exp(combined_coef + 1.96 * se_combined)
                }

                # P-value for combined effect
                z_stat <- combined_coef / se_combined
                p_val <- 2 * (1 - pnorm(abs(z_stat)))
            } else {
                effect_est <- NA
                ci_lower <- NA
                ci_upper <- NA
                p_val <- NA
            }
        }

        subgroup_effects <- rbind(subgroup_effects, data.frame(
            subgroup_variable = original_var_name,
            subgroup_level = level,
            n_total = n_total,
            n_plaque = n_plaque,
            n_gksrs = n_gksrs,
            events_plaque = events_plaque,
            events_gksrs = events_gksrs,
            treatment_effect = effect_est,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            p_value = p_val,
            stringsAsFactors = FALSE
        ))
    }

    return(subgroup_effects)
}

#' Format p-values for display
#'
#' @param p_value Numeric p-value
#' @return Character string of formatted p-value
format_p_value <- function(p_value) {
    if (is.na(p_value) || is.null(p_value)) {
        return("")
    }
    if (p_value < 0.001) {
        return("<0.001")
    } else if (p_value < 0.01) {
        return(sprintf("%.3f", p_value))
    } else {
        return(sprintf("%.2f", p_value))
    }
}

#' Format Subgroup Analysis Tables (wrapper function for main.R)
#'
#' This is a wrapper function that main.R calls to format subgroup analysis tables.
#' It creates formatted tables for multiple subgroup results at once.
#'
#' @param subgroup_results List of subgroup analysis results
#' @param dataset_name Character string for the dataset name
#' @param subgroup_dir Character string for the output directory
#' @param prefix Character string for file prefix
#' @return None (saves tables as side effect)
format_subgroup_analysis_tables <- function(subgroup_results, dataset_name, subgroup_dir, prefix) {
    
    if (is.null(subgroup_results) || length(subgroup_results) == 0) {
        warning("No subgroup results provided for formatting")
        return(invisible(NULL))
    }
    
    # Handle nested list structure (subgroup_results and other_map)
    if ("subgroup_results" %in% names(subgroup_results)) {
        actual_subgroup_results <- subgroup_results$subgroup_results
        other_map <- if ("other_map" %in% names(subgroup_results)) subgroup_results$other_map else NULL
    } else {
        actual_subgroup_results <- subgroup_results
        other_map <- NULL
    }
    
    if (is.null(actual_subgroup_results) || length(actual_subgroup_results) == 0) {
        warning("No subgroup results provided for formatting")
        return(invisible(NULL))
    }
    
    # Create the output directory if it doesn't exist
    if (!dir.exists(subgroup_dir)) {
        dir.create(subgroup_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Format tables for each subgroup variable
    for (var_name in names(actual_subgroup_results)) {
        var_results <- actual_subgroup_results[[var_name]]
        
        # Skip if no valid results
        if (is.null(var_results) || is.null(var_results$subgroup_effects)) {
            next
        }
        
        # Get variable-specific other_map
        var_other_map <- if (!is.null(other_map) && length(other_map) > 0 && var_name %in% names(other_map) && !is.null(other_map[[var_name]]) && length(other_map[[var_name]]) > 0) {
            setNames(list(other_map[[var_name]]), var_name)
        } else {
            NULL
        }
        
        # Create formatted table using the existing function
        tryCatch({
            formatted_table <- format_subgroup_analysis_results(
                subgroup_results = setNames(list(var_results), var_name),
                outcome_name = paste("Tumor Height Change -", dataset_name),
                effect_measure = "MD",  # Mean Difference for height change
                output_path = file.path(subgroup_dir, paste0(prefix, var_name, "_subgroup_analysis.xlsx")),
                other_map = var_other_map
            )
            
        }, error = function(e) {
            warning(sprintf("Failed to format table for %s: %s", var_name, e$message))
        })
    }
    
    return(invisible(NULL))
}

#' Format subgroup analysis results into publication-ready table
#'
#' Creates a formatted table of subgroup analysis results for publication
#' with factor-grouped layout and interaction p-values as headers
#' Saves both Excel (.xlsx) and styled HTML versions
#'
#' @param subgroup_results List of subgroup analysis results
#' @param outcome_name Name of the outcome being analyzed
#' @param effect_measure Type of effect measure ("HR" for hazard ratio, "OR" for odds ratio, "MD" for mean difference)
#' @param output_path Full path for saving the Excel table (HTML will be saved with .html extension)
#' @param other_map A list mapping subgroup variable names to their "Other" categories
#' @return Formatted data frame
format_subgroup_analysis_results <- function(subgroup_results, outcome_name, effect_measure = "HR", output_path = NULL, other_map = NULL) {
    
    if (is.null(subgroup_results) || length(subgroup_results) == 0) {
        warning("No subgroup results provided for formatting")
        return(NULL)
        }
    
    # Create structured table data with factor grouping
    all_table_rows <- list()
        
        for (var_name in names(subgroup_results)) {
            result <- subgroup_results[[var_name]]

        # Skip if no valid results
        if (is.null(result) || is.null(result$subgroup_effects) || 
            !is.data.frame(result$subgroup_effects) || nrow(result$subgroup_effects) == 0) {
            next
        }
        
        # Get variable display name
        variable_display_name <- get_variable_labels()[[var_name]]
        if (is.null(variable_display_name)) {
            variable_display_name <- tools::toTitleCase(gsub("_", " ", var_name))
        }
        
        # Create factor header row with interaction p-value
        interaction_p_text <- if (!is.null(result$interaction_p) && !is.na(result$interaction_p)) {
            format_p_value(result$interaction_p)
        } else {
            "NA"
        }
        
        # Add factor header row
        header_row <- data.frame(
            subgroup_level = variable_display_name,
            sample_size = "",
            treatment_effect_ci = "",
            p_value = "",
            interaction_p = interaction_p_text,
            is_header = TRUE,
            variable_name = var_name,
            stringsAsFactors = FALSE
        )
        all_table_rows[[length(all_table_rows) + 1]] <- header_row
        
        # Add subgroup level rows
        subgroup_effects <- result$subgroup_effects
                required_cols <- c("subgroup_level", "n_total", "n_plaque", "n_gksrs", 
                                  "treatment_effect", "ci_lower", "ci_upper", "p_value")
                
        if (all(required_cols %in% names(subgroup_effects))) {
            for (i in 1:nrow(subgroup_effects)) {
                row_data <- subgroup_effects[i, ]
                
                # Skip rows with invalid data
                if (is.na(row_data$treatment_effect) || is.na(row_data$ci_lower) || is.na(row_data$ci_upper)) {
                    next
                }
                
                # Format subgroup level name with indentation
                level_name <- as.character(row_data$subgroup_level)
                # Remove the incorrect optic_nerve conversion - let the actual data values be used
                
                # Create subgroup row
                subgroup_row <- data.frame(
                    subgroup_level = paste0("  ", level_name),  # Indent subgroup levels
                    sample_size = sprintf("%d (%d Plaque + %d GKSRS)", 
                                        row_data$n_total, row_data$n_plaque, row_data$n_gksrs),
                    treatment_effect_ci = sprintf("%.2f (%.2f, %.2f)", 
                                                 row_data$treatment_effect, 
                                                 row_data$ci_lower, 
                                                 row_data$ci_upper),
                    p_value = format_p_value(row_data$p_value),
                    interaction_p = "",  # Only show in header row
                    is_header = FALSE,
                    variable_name = var_name,
                    stringsAsFactors = FALSE
                )
                all_table_rows[[length(all_table_rows) + 1]] <- subgroup_row
            }
        }
    }

    if (length(all_table_rows) == 0) {
        warning("No valid data to format")
        return(NULL)
    }

    # Combine all rows
    final_table <- do.call(rbind, all_table_rows)
    
    # Create diagnostics data frame from subgroup results
    diagnostics_rows <- list()
    for (var_name in names(subgroup_results)) {
        var_data <- subgroup_results[[var_name]]
        
        # Add header row diagnostics
        diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
            variable = var_name,
            level = "__HEADER__",
            n_total = NA,
            n_plaque = NA,
            n_gksrs = NA,
            treatment_effect = NA,
            ci_lower = NA,
            ci_upper = NA,
            p_value = if (!is.null(var_data$interaction_p)) var_data$interaction_p else NA,
            interaction_p_available = !is.null(var_data$interaction_p) && !is.na(var_data$interaction_p),
            failure_reason = if (is.null(var_data$interaction_p) || is.na(var_data$interaction_p)) {
                if (!is.null(var_data$error)) var_data$error else "Unknown - no interaction p-value calculated"
            } else "",
            stringsAsFactors = FALSE
        )
        
        # Add subgroup level diagnostics
        if (!is.null(var_data$subgroup_effects) && nrow(var_data$subgroup_effects) > 0) {
            for (i in 1:nrow(var_data$subgroup_effects)) {
                row_data <- var_data$subgroup_effects[i, ]
                diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
                    variable = var_name,
                    level = as.character(row_data$subgroup_level),
                    n_total = row_data$n_total,
                    n_plaque = row_data$n_plaque,
                    n_gksrs = row_data$n_gksrs,
                    treatment_effect = row_data$treatment_effect,
                    ci_lower = row_data$ci_lower,
                    ci_upper = row_data$ci_upper,
                    p_value = row_data$p_value,
                    interaction_p_available = !is.null(var_data$interaction_p) && !is.na(var_data$interaction_p),
                    failure_reason = "",
                    stringsAsFactors = FALSE
                )
            }
        }
    }
    
    diagnostics_df <- if (length(diagnostics_rows) > 0) {
        do.call(rbind, diagnostics_rows)
    } else {
        data.frame(
            variable = character(0),
            level = character(0),
            n_total = numeric(0),
            n_plaque = numeric(0),
            n_gksrs = numeric(0),
            treatment_effect = numeric(0),
            ci_lower = numeric(0),
            ci_upper = numeric(0),
            p_value = numeric(0),
            interaction_p_available = logical(0),
            failure_reason = character(0),
            stringsAsFactors = FALSE
        )
    }
    
    # ADD EXCLUDED LEVELS INFORMATION TO DIAGNOSTICS
    excluded_levels_info <- list()
    for (var_name in names(subgroup_results)) {
        if (!is.null(subgroup_results[[var_name]]$interaction_diagnostics)) {
            diag <- subgroup_results[[var_name]]$interaction_diagnostics
            
            # Add excluded levels information
            for (key in names(diag)) {
                if (grepl("^excluded_", key)) {
                    level_name <- gsub("^excluded_", "", key)
                    reason <- if (is.list(diag[[key]])) diag[[key]]$reason else diag[[key]]
                    
                    excluded_levels_info[[length(excluded_levels_info) + 1]] <- data.frame(
                        variable = var_name,
                        level = level_name,
                        n_total = NA,
                        n_plaque = NA,
                        n_gksrs = NA,
                        treatment_effect = NA,
                        ci_lower = NA,
                        ci_upper = NA,
                        p_value = NA,
                        status = "EXCLUDED",
                        reason = reason,
                        stringsAsFactors = FALSE
                    )
                }
            }
        }
    }
    
    # Add excluded levels to diagnostics_df
    if (length(excluded_levels_info) > 0) {
        excluded_df <- do.call(rbind, excluded_levels_info)
        # Make sure column names match
        if (nrow(diagnostics_df) > 0) {
            # Add missing columns to excluded_df to match diagnostics_df
            missing_cols <- setdiff(names(diagnostics_df), names(excluded_df))
            for (col in missing_cols) {
                excluded_df[[col]] <- NA
            }
            # Reorder columns to match
            excluded_df <- excluded_df[names(diagnostics_df)]
        }
        diagnostics_df <- rbind(diagnostics_df, excluded_df)
    }
    
    # Set appropriate column names
    colnames(final_table) <- c(
        "Subgroup Level",
        "Sample Size", 
        sprintf("%s (95%% CI)", effect_measure),
        "P-value",
        "Interaction P",
        "is_header",
        "variable_name"
    )

    # Create Excel version (clean, no formatting columns)
    excel_table <- final_table %>%
        select(-is_header, -variable_name)



    # Create styled HTML version
    if (!is.null(output_path)) {
        tryCatch({
            # Create HTML table with gtsummary-style formatting
            html_table <- final_table %>%
                select(-variable_name) %>%
                gt() %>%
                # Title and subtitle
                tab_header(
                    title = md(sprintf("**Subgroup Analysis: %s**", outcome_name)),
                    # subtitle = md(sprintf("**Treatment Effect on %s**", 
                    #                     gsub("Subgroup Analysis: ", "", outcome_name)))
                ) %>%
                # Style header rows (factor names) as bold
                tab_style(
                    style = cell_text(weight = "bold"),
                    locations = cells_body(
                        columns = everything(),
                        rows = is_header == TRUE
                    )
                ) %>%
                # Style subgroup levels as italic and indented
                tab_style(
                    style = list(cell_text(style = "italic"), cell_text(align = "left"), cell_text(indent = "1em")),
                    locations = cells_body(
                        columns = `Subgroup Level`,
                        rows = is_header == FALSE
                    )
                ) %>%
                # Hide the is_header column
                cols_hide(columns = is_header) %>%
                # Replace missing with blank
                sub_missing(columns = everything(), missing_text = "") %>%
                # Bold column headers
                tab_style(
                    style = cell_text(weight = "bold"),
                    locations = cells_column_labels()
                )
            # Add 'Other' info to source note if present
            other_caption <- ""
            has_other_categories_in_table <- any(grepl('Other', final_table$`Subgroup Level`))
            has_other_categories_in_map <- !is.null(other_map) && length(other_map) > 0 && any(sapply(other_map, function(x) !is.null(x) && length(x) > 0))
            
            if (has_other_categories_in_map) {
                for (var_name in names(subgroup_results)) {
                    if (var_name %in% names(other_map) && !is.null(other_map[[var_name]]) && length(other_map[[var_name]]) > 0) {
                        if (has_other_categories_in_table) {
                            # Other category is visible in table
                        other_caption <- paste0(other_caption, sprintf("\n\n'Other' in %s includes: %s", var_name, paste(other_map[[var_name]], collapse = ", ")))
                        } else {
                            # Other category was created but excluded due to extreme estimates
                            other_caption <- paste0(other_caption, sprintf("\n\n'Other' in %s (excluded due to extreme estimates) included: %s", var_name, paste(other_map[[var_name]], collapse = ", ")))
                        }
                    }
                }
            }
            
            if (other_caption == "" && has_other_categories_in_table) {
                other_caption <- "\n\n'Other' category exists but no specific categories were documented as collapsed."
            } else if (other_caption == "" && !has_other_categories_in_map) {
                other_caption <- "\n\nNo rare categories were collapsed into 'Other'."
            }
            
            html_table <- html_table %>%
                tab_source_note(
                    source_note = md(other_caption)
                )
            html_path <- gsub("\\.xlsx$", ".html", output_path)
            save_gt_html(html_table, filename = html_path)
            log_enhanced(sprintf("Styled HTML subgroup analysis table saved to: %s", html_path), level = "INFO")
        }, error = function(e) {
            warning(sprintf("Failed to save HTML table for %s: %s", outcome_name, e$message))
        })
    }

    return(excel_table)
} 