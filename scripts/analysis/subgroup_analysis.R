# Comprehensive Subgroup Analysis Functions
# Author: Nicholas Camarda
# Description: Unified functions for testing treatment interactions across all outcome types
# Consolidates functionality from subgroup_analysis.R and primary_outcomes_subgroup_analysis.R

# Source centralized configuration
source("scripts/utils/subgroup_config.R")



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
            
            # Calculate effects for each subgroup level
            subgroup_effects <- calculate_subgroup_effects(
                model_results$model,
                processed_results$data,
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
                cutoff_value = processed_results$cutoff_value
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

    return(subgroup_results)
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
            
            # Calculate effects for each subgroup level
            subgroup_effects <- calculate_subgroup_effects(
                model_results$model,
                processed_results$data,
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
                cutoff_value = processed_results$cutoff_value
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

    return(subgroup_results)
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
#' @param create_tables Logical, whether to create formatted HTML tables (default: FALSE for individual calls)
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
analyze_treatment_effect_subgroups_height <- function(data, subgroup_var, percentile_cut = 0.5, confounders = NULL, include_baseline_height = FALSE, create_tables = FALSE) {
    # Calculate tumor height change if not already present
    if (!("height_change" %in% names(data))) {
        data <- data %>%
            mutate(
                height_change = case_when(
                    recurrence1 == "Y" ~ initial_tumor_height - recurrence1_pretreatment_height,
                    TRUE ~ initial_tumor_height - last_height
                )
            )
    }
    
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
    
    # Calculate effects for each subgroup level
    subgroup_effects <- calculate_subgroup_effects(
        model_results$model,
        processed_results$data,
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
        confounders_used = processed_results$confounders_to_use
    ))
}

#' Process subgroup data (common steps for all outcome types)
#' @param data Input data
#' @param subgroup_var Subgroup variable name
#' @param confounders Vector of confounder names
#' @param include_baseline_height For tumor height analysis - include initial height
#' @return List with processed data and variable names
process_subgroup_data <- function(data, subgroup_var, confounders, include_baseline_height = FALSE) {
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
        cutoff_type <- if (USE_STANDARDIZED_CUTOFFS && subgroup_var %in% names(STANDARDIZED_CUTOFFS)) "standardized" else "median"
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
    } else {
        if (!is.factor(processed_data[[subgroup_var]])) {
            processed_data[[subgroup_var]] <- as.factor(processed_data[[subgroup_var]])
        }
        processed_data <- handle_rare_categories(processed_data, vars = subgroup_var, threshold = THRESHOLD_RARITY)
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
    processed_data <- ensure_consistent_contrasts(processed_data)
    
    return(list(
        data = processed_data,
        subgroup_var_to_use = subgroup_var_to_use,
        confounders_to_use = confounders_to_use,
        was_continuous = was_continuous,
        cutoff_value = cutoff_value
    ))
}

#' Fit model with interaction based on outcome type
#' @param data Processed data
#' @param outcome_config Outcome configuration
#' @param subgroup_var_to_use Processed subgroup variable name
#' @param confounders_to_use Valid confounders
#' @return List with fitted model and interaction p-value
fit_subgroup_model <- function(data, outcome_config, subgroup_var_to_use, confounders_to_use) {
    
    # Build base formula components
    confounders_str <- if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
        ""
    } else {
        paste(" + ", paste(confounders_to_use, collapse = " + "))
    }
    
    interaction_term <- paste0("treatment_group * ", subgroup_var_to_use)
    
    # Build formula based on outcome type
    if (outcome_config$type == "survival") {
        formula_str <- paste0("Surv(", outcome_config$time_var, ", ", outcome_config$event_var, ") ~ ", 
                             interaction_term, confounders_str)
        model <- coxph(as.formula(formula_str), data = data)
        no_interaction_formula <- paste0("Surv(", outcome_config$time_var, ", ", outcome_config$event_var, ") ~ ", 
                                        "treatment_group + ", subgroup_var_to_use, confounders_str)
        no_interaction_model <- coxph(as.formula(no_interaction_formula), data = data)
        
    } else if (outcome_config$type == "binary") {
        formula_str <- paste0(outcome_config$outcome_var, " ~ ", interaction_term, confounders_str)
        model <- glm(as.formula(formula_str), data = data, family = binomial())
        no_interaction_formula <- paste0(outcome_config$outcome_var, " ~ ", 
                                        "treatment_group + ", subgroup_var_to_use, confounders_str)
        no_interaction_model <- glm(as.formula(no_interaction_formula), data = data, family = binomial())
        
    } else if (outcome_config$type == "continuous") {
        formula_str <- paste0(outcome_config$outcome_var, " ~ ", interaction_term, confounders_str)
        model <- lm(as.formula(formula_str), data = data)
        no_interaction_formula <- paste0(outcome_config$outcome_var, " ~ ", 
                                        "treatment_group + ", subgroup_var_to_use, confounders_str)
        no_interaction_model <- lm(as.formula(no_interaction_formula), data = data)
    }
    
    # Calculate interaction p-value
    subgroup_levels <- levels(data[[subgroup_var_to_use]])
    
    if (length(subgroup_levels) == 2) {
        # Simple interaction test for binary subgroup
        interaction_coef_name <- get_interaction_coefficient_name(
            model, "treatment_group", subgroup_var_to_use, subgroup_levels[2], data
        )
        if (!is.null(interaction_coef_name)) {
            if (outcome_config$type == "survival") {
                interaction_p <- summary(model)$coefficients[interaction_coef_name, "Pr(>|z|)"]
            } else if (outcome_config$type == "binary") {
                interaction_p <- summary(model)$coefficients[interaction_coef_name, "Pr(>|z|)"]
            } else {
                interaction_p <- summary(model)$coefficients[interaction_coef_name, "Pr(>|t|)"]
            }
        } else {
            interaction_p <- NA
        }
    } else {
        # Multiple levels - use likelihood ratio test
        if (outcome_config$type == "survival") {
            interaction_test <- anova(no_interaction_model, model)
            interaction_p <- interaction_test$`Pr(>Chi)`[2]
        } else if (outcome_config$type == "binary") {
            interaction_test <- anova(no_interaction_model, model, test = "Chisq")
            interaction_p <- interaction_test$`Pr(>Chi)`[2]
        } else {
            interaction_test <- anova(no_interaction_model, model)
            interaction_p <- interaction_test$`Pr(>F)`[2]
        }
    }
    
    return(list(
        model = model,
        interaction_p = interaction_p,
        formula_used = formula_str
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
    subgroup_levels <- levels(data[[subgroup_var_to_use]])
    subgroup_effects <- data.frame()

    for (i in seq_along(subgroup_levels)) {
        level <- subgroup_levels[i]
        level_data <- data[data[[subgroup_var_to_use]] == level, ]

        n_total <- nrow(level_data)
        n_plaque <- sum(level_data$treatment_group == "Plaque", na.rm = TRUE)
        n_gksrs <- sum(level_data$treatment_group == "GKSRS", na.rm = TRUE)

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
            treatment_effect = effect_est,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            p_value = p_val,
            stringsAsFactors = FALSE
        ))
    }

    return(subgroup_effects)
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
    
    # Create the output directory if it doesn't exist
    if (!dir.exists(subgroup_dir)) {
        dir.create(subgroup_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Format tables for each subgroup variable
    for (var_name in names(subgroup_results)) {
        var_results <- subgroup_results[[var_name]]
        
        # Skip if no valid results
        if (is.null(var_results) || is.null(var_results$subgroup_effects)) {
            next
        }
        
        # Create formatted table using the existing function
        tryCatch({
            formatted_table <- format_subgroup_analysis_results(
                subgroup_results = setNames(list(var_results), var_name),
                outcome_name = paste("Tumor Height Change -", dataset_name),
                effect_measure = "MD",  # Mean Difference for height change
                output_path = file.path(subgroup_dir, paste0(prefix, var_name, "_subgroup_analysis.xlsx"))
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
#' Saves both Excel (.xlsx) and styled HTML versions
#'
#' @param subgroup_results List of subgroup analysis results
#' @param outcome_name Name of the outcome being analyzed
#' @param effect_measure Type of effect measure ("HR" for hazard ratio, "OR" for odds ratio, "MD" for mean difference)
#' @param output_path Full path for saving the Excel table (HTML will be saved with .html extension)
#' @return Formatted data frame
format_subgroup_analysis_results <- function(subgroup_results, outcome_name, effect_measure = "HR", output_path = NULL) {
    # Use the create_forest_plot_data function from forest_plot.R
    variable_order <- names(subgroup_results)
    table_data <- tryCatch({
        forest_data <- create_forest_plot_data(subgroup_results, variable_order, c("GKSRS", "Plaque"), effect_measure)
        # Extract relevant data from forest plot structure for table use
        if (!is.null(forest_data) && !is.null(forest_data$labeltext)) {
            # This is a forest plot data structure, return empty for manual processing
            data.frame()
        } else {
            data.frame()
        }
    }, error = function(e) {
        # If forest plot data creation fails, return empty data frame
        data.frame()
    })
    
    # If extract_forest_data failed or returned empty, try manual processing
    if (is.null(table_data) || nrow(table_data) == 0) {
        table_data <- data.frame()
        
        for (var_name in names(subgroup_results)) {
            result <- subgroup_results[[var_name]]

            # Check if we have valid subgroup effects data
            if (!is.null(result$subgroup_effects) && 
                is.data.frame(result$subgroup_effects) && 
                nrow(result$subgroup_effects) > 0) {
                
                # Ensure required columns exist
                required_cols <- c("subgroup_level", "n_total", "n_plaque", "n_gksrs", 
                                  "treatment_effect", "ci_lower", "ci_upper", "p_value")
                
                if (all(required_cols %in% names(result$subgroup_effects))) {
                    temp_data <- result$subgroup_effects %>%
                        mutate(
                            subgroup_variable = var_name,
                            variable_label = tools::toTitleCase(gsub("_", " ", var_name)),
                            level_label = case_when(
                                # Clean up common variable names
                                var_name == "treatment_group" ~ as.character(subgroup_level),
                                var_name == "sex" ~ as.character(subgroup_level),
                                var_name == "location" ~ as.character(subgroup_level),
                                var_name == "optic_nerve" ~ ifelse(as.character(subgroup_level) == "Y", "Yes", "No"),
                                var_name == "biopsy1_gep" ~ as.character(subgroup_level),
                                TRUE ~ as.character(subgroup_level)
                            ),
                            interaction_p = result$interaction_p
                        )
                    
                    # Safe rbind with column checking
                    if (is.null(table_data) || nrow(table_data) == 0) {
                        table_data <- temp_data
                    } else {
                        # Only rbind if columns match
                        if (all(names(temp_data) %in% names(table_data)) && 
                            all(names(table_data) %in% names(temp_data))) {
                            table_data <- rbind(table_data, temp_data)
                        } else {
                            warning(sprintf("Column mismatch for variable %s in format_subgroup_analysis_results", var_name))
                        }
                    }
                } else {
                    warning(sprintf("Missing required columns in subgroup_effects for variable %s", var_name))
                }
            }
        }
    }

    if (is.null(table_data) || nrow(table_data) == 0) {
        warning("No data available for table")
        return(NULL)
    }

    # Format the table for Excel output
    formatted_table <- table_data %>%
        mutate(
            # Format effect estimates and confidence intervals
            effect_ci = sprintf(
                "%.2f (%.2f-%.2f)",
                treatment_effect,
                ci_lower,
                ci_upper
            ),
            # Format p-values
            p_value_formatted = sprintf("%.3f", p_value),
            interaction_p_formatted = sprintf("%.3f", interaction_p),
            # Format sample sizes
            sample_size = sprintf("%d/%d", n_gksrs, n_plaque)
        ) %>%
        select(
            variable_label,
            level_label,
            effect_ci,
            p_value_formatted,
            interaction_p_formatted,
            sample_size
        ) %>%
        arrange(variable_label, level_label)

    # Add column headers for Excel version
    colnames(formatted_table) <- c(
        "Subgroup Variable",
        "Level", 
        paste0(effect_measure, " (95% CI)"),
        "p-value",
        "Interaction p-value",
        "Sample Size (GKSRS/Plaque)"
    )

    # Save Excel table if path provided
    if (!is.null(output_path)) {
        writexl::write_xlsx(formatted_table, output_path)
        log_enhanced(sprintf("Subgroup analysis table saved to: %s", output_path), level = "INFO")
        
                # Create styled HTML version using gt() but with tbl_regression styling
        tryCatch({
            # Extract clean cohort name from outcome_name
            clean_outcome_name <- gsub("Tumor Height Change - ", "", outcome_name)
            clean_outcome_name <- gsub(" - [A-Z]+ - ", " - ", clean_outcome_name)
            
            # Get the subgroup variable name and confounders from the first row for the title
            subgroup_variable_name <- if(nrow(formatted_table) > 0) {
                # Clean up the variable name for display
                var_name <- formatted_table$`Subgroup Variable`[1]
                tools::toTitleCase(gsub("_", " ", var_name))
            } else {
                "Unknown Variable"
            }
            
            # Get confounders used from the results (if available)
            confounders_used <- tryCatch({
                first_result <- subgroup_results[[1]]
                if (!is.null(first_result$confounders_used)) {
                    paste(first_result$confounders_used, collapse = ", ")
                } else {
                    "age_at_diagnosis, sex, location, initial_tumor_height"
                }
            }, error = function(e) {
                "age_at_diagnosis, sex, location, initial_tumor_height"
            })
            
            # Format confounders for model formula
            model_confounders <- gsub(", ", " + ", confounders_used)
            
                        # Prepare data for gtsummary table format
            # Create a restructured data frame for tbl_summary approach
            subgroup_data <- table_data %>%
                select(subgroup_variable, level_label, treatment_effect, ci_lower, ci_upper, p_value, interaction_p, n_total, n_plaque, n_gksrs) %>%
                mutate(
                    effect_ci = sprintf("%.2f (%.2f, %.2f)", treatment_effect, ci_lower, ci_upper),
                    p_value_formatted = sprintf("%.4f", p_value),
                    sample_size = sprintf("%d (%d Plaque + %d GKSRS)", n_total, n_plaque, n_gksrs)
                ) %>%
                select(subgroup_variable, level_label, sample_size, effect_ci, p_value_formatted, interaction_p)
            
            # Extract interaction p-value for the subtitle
            interaction_p_value <- ifelse(length(unique(table_data$interaction_p)) == 1, 
                                        unique(table_data$interaction_p)[1], NA)
            
            # Create a simple summary table structure that mimics tbl_summary styling
            # Using gt but with gtsummary-style formatting
            gt_tbl <- subgroup_data %>%
                select(-subgroup_variable, -interaction_p) %>%
                rename(
                    `Subgroup Level` = level_label,
                    `Sample Size` = sample_size,
                    `Treatment Effect (95% CI)` = effect_ci,
                    `P-value` = p_value_formatted
                ) %>%
                gt() %>%
                # Main title and subtitle matching the image style
                tab_header(
                    title = md(sprintf("**Subgroup Analysis: %s**", subgroup_variable_name)),
                    subtitle = md(sprintf("**Treatment Effect on %s | Interaction P-value: %.4f**", 
                                        gsub("Subgroup Analysis: ", "", outcome_name),
                                        interaction_p_value))
                ) %>%
                # Column headers with gtsummary-style formatting
                cols_label(
                    `Subgroup Level` = md("**Subgroup Level**"),
                    `Sample Size` = md("**Sample Size**"),
                    `Treatment Effect (95% CI)` = md(sprintf("**Treatment Effect (95%% CI)**")),
                    `P-value` = md("**P-value**")
                ) %>%
                # Add footnotes/caption information matching the image style
                tab_source_note(
                    source_note = md(sprintf(
                        "**Treatment Effect**: %s vs %s (reference). %s\n\n**Model**: %s ~ treatment_group * %s + %s\n\n**Confounders**: %s\n\n**Dataset**: %s",
                        "GKSRS",
                        "Plaque", 
                        case_when(
                            effect_measure == "HR" ~ "Values < 1 favor GKSRS",
                            effect_measure == "OR" ~ "Values < 1 favor GKSRS", 
                            effect_measure == "MD" ~ "Positive values indicate greater height reduction with GKSRS",
                            TRUE ~ "Comparison of treatment effects"
                        ),
                        tolower(gsub("Subgroup Analysis: ", "", gsub(" Analysis.*", "", outcome_name))),
                        tolower(subgroup_variable_name),
                        model_confounders,
                        confounders_used,
                        clean_outcome_name
                    ))
                ) %>%
                # Apply gtsummary-style formatting
                tab_style(
                    style = cell_text(weight = "bold"),
                    locations = cells_column_labels()
                ) %>%
                tab_style(
                    style = cell_text(style = "italic"),
                    locations = cells_body(columns = `Subgroup Level`)
                ) %>%
                # Replace missing with blank
                sub_missing(columns = everything(), missing_text = "")
            
            # Save HTML version
            html_path <- gsub("\\.xlsx$", ".html", output_path)
            save_gt_html(gt_tbl, filename = html_path)
            log_enhanced(sprintf("Styled HTML subgroup analysis table saved to: %s", html_path), level = "INFO")
            
        }, error = function(e) {
            warning(sprintf("Failed to create HTML version: %s", e$message))
        })
    }

    return(formatted_table)
} 