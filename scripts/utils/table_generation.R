# Table Generation Utilities
# Author: Nicholas Camarda
# Description: Simple, clean table generation focusing on diagnostic files first

#' Get cohort-specific other_map for consistent other_map handling
#'
#' @param dataset_name Character string for dataset name (e.g., "uveal_melanoma_full_cohort")
#' @param processed_data_dir Character string for processed data directory
#' @return List containing other_map for the specific cohort
get_cohort_specific_other_map <- function(dataset_name, processed_data_dir = "final_data/Analytic Dataset") {
    # Extract cohort name from dataset name
    cohort_name <- gsub("uveal_melanoma_", "", dataset_name)
    cohort_name <- gsub("_cohort", "", cohort_name)
    
    # Create cohort-specific other_map filename
    other_map_file <- file.path(processed_data_dir, paste0(cohort_name, "_other_map.rds"))
    
    if (file.exists(other_map_file)) {
        other_map <- readRDS(other_map_file)
        log_enhanced(sprintf("Loaded cohort-specific other_map for %s from %s", cohort_name, other_map_file), level = "INFO")
        return(other_map)
    } else {
        log_enhanced(sprintf("No cohort-specific other_map found for %s at %s, using empty list", cohort_name, other_map_file), level = "INFO")
        return(list())
    }
}

#' Build model formula for regression
#'
#' @param outcome_var Character string name of outcome variable
#' @param predictor_vars Character vector of predictor variables
#' @param confounders Character vector of confounder variables
#' @param model_type Character string for model type
#' @return Model formula
build_model_formula <- function(outcome_var, predictor_vars, confounders, model_type) {
    # Combine predictor and confounder variables
    all_vars <- c(predictor_vars, confounders)
    
    # Remove any NULL or empty variables
    all_vars <- all_vars[!is.null(all_vars) & all_vars != ""]
    
    if (length(all_vars) == 0) {
        # No variables to include
        formula_str <- paste(outcome_var, "~ 1")
    } else {
        # Create formula with all variables
        formula_str <- paste(outcome_var, "~", paste(all_vars, collapse = " + "))
    }
    
    return(as.formula(formula_str))
}

#' Fit regression model
#'
#' @param data Data frame
#' @param formula Model formula
#' @param model_type Character string for model type
#' @param time_var Character string for time variable (Cox models)
#' @param event_var Character string for event variable (Cox models)
#' @return Fitted model object
fit_regression_model <- function(data, formula, model_type, time_var = NULL, event_var = NULL) {
    
    tryCatch({
        if (model_type == "logistic") {
            # Check for perfect separation before fitting
            formula_vars <- all.vars(formula)
            outcome_var <- formula_vars[1]
            predictor_vars <- formula_vars[-1]
            
            # Check each predictor for perfect separation
            perfect_separation_vars <- c()
            for (var in predictor_vars) {
                if (var %in% names(data)) {
                    # Check if this variable perfectly predicts the outcome
                    if (is.factor(data[[var]]) || is.character(data[[var]])) {
                        # For categorical variables, check each level
                        for (level in unique(data[[var]])) {
                            level_data <- data[data[[var]] == level, ]
                            if (nrow(level_data) > 0) {
                                outcome_counts <- table(level_data[[outcome_var]])
                                if (length(outcome_counts) == 1 || any(outcome_counts == 0)) {
                                    # Perfect separation detected
                                    perfect_separation_vars <- c(perfect_separation_vars, var)
                                    break
                                }
                            }
                        }
                    }
                }
            }
            
            if (length(perfect_separation_vars) > 0) {
                log_enhanced(sprintf("Perfect separation detected in variables: %s. Fitting model with warnings.", 
                                   paste(perfect_separation_vars, collapse = ", ")), level = "WARN")
            }
            
            # Use more robust fitting for logistic regression
            model <- glm(formula, data = data, family = binomial(), 
                        control = list(maxit = 100, epsilon = 1e-8))
            
            # Check for convergence issues
            if (!model$converged) {
                log_enhanced("Warning: Logistic regression did not converge, but proceeding with results", level = "WARN")
            }
            
            # Add perfect separation info to model
            model$perfect_separation_vars <- perfect_separation_vars
            
            return(model)
        } else if (model_type == "cox") {
            if (is.null(time_var) || is.null(event_var)) {
                stop("Cox models require time_var and event_var")
            }
            # Create survival object
            surv_obj <- Surv(data[[time_var]], data[[event_var]])
            # Update formula to use survival object
            surv_formula <- update(formula, surv_obj ~ .)
            coxph(surv_formula, data = data)
        } else if (model_type == "linear") {
            lm(formula, data = data)
        } else {
            stop("Unsupported model type: ", model_type)
        }
    }, error = function(e) {
        log_enhanced(sprintf("Model fitting error: %s", e$message), level = "ERROR")
        return(NULL)
    })
}

#' Exclude events before treatment
#'
#' @param data Data frame


#' Create comprehensive diagnostics with all required tabs
#'
#' @param model_fit Fitted model object
#' @param data Data frame
#' @param outcome_var Character string name of outcome variable
#' @param predictor_vars Character vector of predictor variables
#' @param confounders Character vector of confounder variables
#' @param analysis_name Character string for analysis name
#' @param dataset_name Character string for dataset name
#' @param filtered_variables Character vector of variables that were filtered from the table
#' @return List containing all diagnostic data frames
create_comprehensive_diagnostics <- function(model_fit, data, outcome_var, predictor_vars, confounders, analysis_name, dataset_name, filtered_variables = NULL, other_map = list(), extreme_diagnostics = NULL) {
    
    # Get model coefficients and summary
    coefs <- coef(model_fit)
    model_summary <- summary(model_fit)
    
    # Create a temporary gtsummary table to get the confidence intervals
    # This ensures we use the same CI calculation method as the final table
    temp_table <- tryCatch({
        model_fit %>%
            tbl_regression(
                exponentiate = FALSE,  # Get raw coefficients for diagnostics
                conf.int = TRUE
            )
    }, error = function(e) {
        log_enhanced(sprintf("Warning: Could not create temporary table for CI extraction: %s", e$message), level = "WARN")
        NULL
    })
    
    # Extract confidence intervals from the gtsummary table
    if (!is.null(temp_table)) {
        table_data <- temp_table$table_body
        # Create a mapping from term names (full coefficient names) to CI values
        ci_mapping <- data.frame(
            term = table_data$term,
            conf.low = table_data$conf.low,
            conf.high = table_data$conf.high,
            stringsAsFactors = FALSE
        )
        
        # Create conf_int matrix with the same structure as before
        conf_int <- matrix(NA, nrow = length(coefs), ncol = 2,
                          dimnames = list(names(coefs), c("2.5 %", "97.5 %")))
        
        # Fill in the CI values from the gtsummary table
        for (i in 1:nrow(ci_mapping)) {
            term_name <- ci_mapping$term[i]
            if (term_name %in% names(coefs)) {
                conf_int[term_name, "2.5 %"] <- ci_mapping$conf.low[i]
                conf_int[term_name, "97.5 %"] <- ci_mapping$conf.high[i]
            }
        }
    } else {
        # Fallback to base R confint if gtsummary fails
        conf_int <- tryCatch({
            suppressWarnings(confint(model_fit))
        }, error = function(e) {
            log_enhanced(sprintf("Warning: Could not compute confidence intervals: %s", e$message), level = "WARN")
            matrix(NA, nrow = length(coefs), ncol = 2, 
                   dimnames = list(names(coefs), c("2.5 %", "97.5 %")))
        })
    }
    
    # Additional check: if conf_int contains all NA values, log this as a warning
    if (all(is.na(conf_int))) {
        log_enhanced("Warning: All confidence intervals are NA - this indicates severe model convergence issues", level = "WARN")
    }
    
    # 1. Model Summary Tab
    model_summary_tab <- data.frame(
        analysis_type = paste0("unified_", analysis_name),
        outcome = outcome_var,
        n_total = nrow(data),
        n_events = ifelse("coxph" %in% class(model_fit), 
                         sum(model_fit$y[, 2]), 
                         sum(as.numeric(model_fit$model[[1]]))),
        model_fitted = !is.null(model_fit),
        confounders_used = paste(confounders, collapse = ", "),
        notes = "Generated by unified table generation system",
        stringsAsFactors = FALSE
    )
    
    # 2. Model Diagnostics Tab
    model_diagnostics_tab <- data.frame(
        dataset_name = dataset_name,  # Add dataset name for clarity
        model_type = class(model_fit)[1],
        effect_measure = ifelse("coxph" %in% class(model_fit), "HR", "OR"),
        n_coefficients = length(coefs),
        model_converged = ifelse("glm" %in% class(model_fit), model_fit$converged, TRUE),
        log_likelihood = ifelse("glm" %in% class(model_fit), logLik(model_fit), NA),
        aic = AIC(model_fit),
        bic = BIC(model_fit),
        stringsAsFactors = FALSE
    )
    
    # 3. Data Characteristics Tab
    data_characteristics_tab <- data.frame(
        dataset_name = dataset_name,
        analysis_name = analysis_name,
        total_variables = length(c(predictor_vars, confounders)),
        predictor_variables = paste(predictor_vars, collapse = ", "),
        confounder_variables = paste(confounders, collapse = ", "),
        outcome_variable = outcome_var,
        sample_size = nrow(data),
        missing_data_pct = round(mean(is.na(data[c(predictor_vars, confounders, outcome_var)])) * 100, 1),
        stringsAsFactors = FALSE
    )
    
    # 3.5. "Other" Level Details Tab
    other_level_details_tab <- data.frame(
        variable = character(),
        has_other_level = logical(),
        other_categories = character(),
        other_count = integer(),
        stringsAsFactors = FALSE
    )
    
    # Check for "Other" levels in the model data
    model_data <- model_fit$model
    for (var_name in names(model_data)) {
        if (var_name != "(weights)" && var_name != "(offset)") {
            var_data <- model_data[[var_name]]
            
            if (is.factor(var_data) || is.character(var_data)) {
                levels_data <- levels(var_data) %||% unique(var_data)
                
                if ("Other" %in% levels_data) {
                    # Count how many observations are in "Other"
                    other_count <- sum(var_data == "Other", na.rm = TRUE)
                    
                    # Get the original categories that were collapsed from other_map
                    if (var_name %in% names(other_map) && length(other_map[[var_name]]) > 0) {
                        other_categories <- paste(other_map[[var_name]], collapse = ", ")
                    } else {
                        other_categories <- "Original categories not available in model data"
                    }
                    
                    other_level_details_tab <- rbind(other_level_details_tab, data.frame(
                        variable = var_name,
                        has_other_level = TRUE,
                        other_categories = other_categories,
                        other_count = other_count,
                        stringsAsFactors = FALSE
                    ))
                }
            }
        }
    }
    
    # 4. Excluded Rows Tab
    excluded_rows_tab <- data.frame(
        term = character(),
        variable = character(),
        label = character(),
        estimate = numeric(),
        conf_low = numeric(),
        conf_high = numeric(),
        exclusion_reason = character(),
        stringsAsFactors = FALSE
    )
    
    # 5. Calculate factor label p-values (overall variable significance)
    factor_label_pvalues_tab <- data.frame(
        variable = character(),
        factor_label_pvalue = numeric(),
        test_type = character(),
        stringsAsFactors = FALSE
    )
    
    # Get unique variables from the model (including treatment_group)
    model_terms <- attr(terms(model_fit), "term.labels")
    variables_to_test <- unique(c("treatment_group", model_terms))
    
    # Calculate factor label p-values for each variable
    for (var_name in variables_to_test) {
        # For each variable, exclude it from the confounders list
        var_confounders <- confounders[confounders != var_name]
        
        # Calculate overall significance using appropriate test for model type
        pval <- calculate_factor_label_pvalue(model_fit, var_name, data, outcome_var, var_confounders)
        
        factor_label_pvalues_tab <- rbind(factor_label_pvalues_tab, data.frame(
            variable = var_name,
            factor_label_pvalue = pval,
            test_type = "Likelihood Ratio Test",
            stringsAsFactors = FALSE
        ))
    }
    
    # Add treatment_group with NA (since it's the main predictor)
    
    # 5. Raw Model Output Tab
    # Handle case where conf_int has different dimensions than coefficients
    n_coefs <- length(coefs)
    n_conf_int <- nrow(conf_int)
    
    # Create a properly aligned confidence interval matrix
    conf_int_padded <- matrix(NA, nrow = n_coefs, ncol = 2, 
                             dimnames = list(names(coefs), c("2.5 %", "97.5 %")))
    
    # Fill in confidence intervals for coefficients that exist in conf_int
    available_coefs <- intersect(names(coefs), rownames(conf_int))
    if (length(available_coefs) > 0) {
        conf_int_padded[available_coefs, ] <- conf_int[available_coefs, ]
    }
    
    # Log warning if there are missing coefficients
    if (length(available_coefs) < n_coefs) {
        missing_coefs <- setdiff(names(coefs), rownames(conf_int))
        log_enhanced(sprintf("Warning: Missing confidence intervals for coefficients: %s", 
                           paste(missing_coefs, collapse = ", ")), level = "WARN")
    }
    
    # Ensure all vectors have the same length
    n_coefs <- length(coefs)
    
    # Get p-values, handling potential length mismatch and missing coefficients
    p_values <- tryCatch({
        # Create a vector of NAs for all coefficients
        p_values_vector <- rep(NA, n_coefs)
        names(p_values_vector) <- names(coefs)
        
        # Fill in p-values for coefficients that exist in the model summary
        available_coefs <- intersect(names(coefs), rownames(model_summary$coefficients))
        if (length(available_coefs) > 0) {
            p_values_vector[available_coefs] <- as.numeric(model_summary$coefficients[available_coefs, 4])
        }
        
        # Return the p-values vector in the same order as the coefficients
        p_values_vector
    }, error = function(e) {
        rep(NA, n_coefs)
    })
    
    # Create a mapping of factor label p-values for each variable
    factor_label_pvalue_map <- setNames(
        factor_label_pvalues_tab$factor_label_pvalue,
        factor_label_pvalues_tab$variable
    )
    
    # Extract variable names from coefficient names (remove level suffixes)
    variable_names <- sapply(names(coefs), function(term) {
        # For terms like "treatment_groupGKSRS", extract "treatment_group"
        # For terms like "age_at_diagnosis", keep as is
        if (grepl("^[a-zA-Z_]+[A-Z]", term)) {
            # Extract the base variable name before the level
            # Use a general pattern that works for all variables
            # Look for the pattern: variable_name + level (where level starts with uppercase)
            # The pattern matches: variable_name + uppercase_letter + anything
            # Use a non-greedy approach to find the first uppercase letter after the variable name
            base_name <- sub("^([a-zA-Z_]+?)[A-Z].*", "\\1", term)
            # If the substitution didn't change anything, it means no level was found
            # In that case, return the original term
            if (base_name == term) {
                term
            } else {
                base_name
            }
        } else {
            term
        }
    })
    
    # Get factor label p-values for each coefficient
    factor_label_pvalues_for_coefs <- sapply(variable_names, function(var_name) {
        if (var_name %in% names(factor_label_pvalue_map)) {
            factor_label_pvalue_map[[var_name]]
        } else {
            NA
        }
    })
    
    # Create the raw model output with factor level rows
    raw_model_output_tab <- data.frame(
        variable = names(coefs),  # Use full coefficient names instead of truncating
        variable_base = variable_names,  # Base variable name (without level)
        estimate = as.numeric(coefs),
        ci_lower = as.numeric(conf_int_padded[, 1]),
        ci_upper = as.numeric(conf_int_padded[, 2]),
        p_value = p_values,
        row_type = "Coefficient",  # All coefficient rows are "Coefficient" type
        inclusion_status = "Included",
        filtering_reason = "None",
        stringsAsFactors = FALSE
    )
    
    # Add factor label rows as separate rows ONLY for categorical variables
    # For continuous variables, don't create separate factor label rows since there's only one coefficient
    categorical_variables <- c()
    continuous_variables <- c()
    
    # Determine which variables are categorical vs continuous
    for (var_name in factor_label_pvalues_tab$variable) {
        if (var_name %in% names(data)) {
            if (is.factor(data[[var_name]]) || is.character(data[[var_name]])) {
                categorical_variables <- c(categorical_variables, var_name)
            } else if (is.numeric(data[[var_name]])) {
                continuous_variables <- c(continuous_variables, var_name)
            }
        } else {
            # If variable not found in data, assume it's categorical (like treatment_group)
            categorical_variables <- c(categorical_variables, var_name)
        }
    }
    
    # Only create factor label rows for categorical variables
    factor_label_rows <- data.frame(
        variable = factor_label_pvalues_tab$variable[factor_label_pvalues_tab$variable %in% categorical_variables],
        variable_base = factor_label_pvalues_tab$variable[factor_label_pvalues_tab$variable %in% categorical_variables],
        estimate = NA,  # No estimate for factor labels
        ci_lower = NA,  # No CI for factor labels
        ci_upper = NA,  # No CI for factor labels
        p_value = factor_label_pvalues_tab$factor_label_pvalue[factor_label_pvalues_tab$variable %in% categorical_variables],
        row_type = "Factor Label",  # Factor label rows are "Factor Label" type
        inclusion_status = "Included",  # Factor labels are always included
        filtering_reason = "None",  # Factor labels are not filtered, so no filtering reason
        stringsAsFactors = FALSE
    )
    
    # Combine factor label rows with factor level rows
    raw_model_output_tab <- rbind(factor_label_rows, raw_model_output_tab)
    
    # Implement logical sorting: (Intercept) first, then variables grouped by type
    # Create a custom sorting order that groups factor labels with their coefficients
    custom_order <- function(var_name) {
        if (var_name == "(Intercept)") {
            return(1)  # Always first
        } else {
            # For other variables, group by base variable name
            base_name <- if (grepl("^[a-zA-Z_]+[A-Z]", var_name)) {
                sub("^([a-zA-Z_]+?)[A-Z].*", "\\1", var_name)
            } else {
                var_name
            }
            
            # Get the row type for this variable
            row_type <- raw_model_output_tab$row_type[raw_model_output_tab$variable == var_name]
            
            # Create a grouping system:
            # - treatment_group gets priority 2
            # - Other variables get priority based on base name (alphabetical)
            # - Within each variable group, factor labels come before coefficients
            if (base_name == "treatment_group") {
                base_priority <- 2
            } else {
                # Get alphabetical position of base variable
                unique_bases <- unique(raw_model_output_tab$variable_base[raw_model_output_tab$variable_base != "(Intercept)"])
                unique_bases <- sort(unique_bases)
                base_priority <- 1000 + which(unique_bases == base_name)
            }
            
            # Factor labels come before coefficients within each variable group
            if (row_type == "Factor Label") {
                return(base_priority)
            } else {
                return(base_priority + 0.5)  # Coefficients come after factor labels
            }
        }
    }
    
    # Sort using custom order
    raw_model_output_tab <- raw_model_output_tab[order(sapply(raw_model_output_tab$variable, custom_order)), ]
    
    # Apply the SAME filtering logic as the table generation
    # This ensures consistency between diagnostics and table output
    
    # Use the Excluded Rows data to identify which variables were actually filtered out
    excluded_variables <- c()
    if (nrow(excluded_rows_tab) > 0) {
        # Extract variable names from excluded rows
        excluded_variables <- unique(excluded_rows_tab$variable[!is.na(excluded_rows_tab$variable)])
        log_enhanced(sprintf("Found %d variables in excluded rows: %s", 
                           length(excluded_variables), paste(excluded_variables, collapse = ", ")), 
                    level = "DEBUG")
        
        # Mark variables as filtered if they appear in excluded rows
        for (var_name in excluded_variables) {
            # Find all rows for this variable in raw_model_output_tab
            var_rows <- which(raw_model_output_tab$variable == var_name)
            if (length(var_rows) > 0) {
                raw_model_output_tab$inclusion_status[var_rows] <- "Filtered"
                raw_model_output_tab$filtering_reason[var_rows] <- "Extreme estimate or convergence issue"
                log_enhanced(sprintf("Marked variable %s as Filtered", var_name), level = "DEBUG")
            }
        }
    }
    
    # Use the sophisticated extreme estimate detection function for diagnostics
    # Note: raw_model_output_tab contains log-odds values (not exponentiated)
    log_enhanced("DEBUG: Starting diagnostic filtering logic using sophisticated detection", level = "DEBUG")
    
    # Only check coefficient rows (not factor labels)
    coeff_rows <- raw_model_output_tab$row_type == "Coefficient"
    if (any(coeff_rows)) {
        diagnostic_extreme_result <- detect_extreme_regression_estimates(
            estimate = raw_model_output_tab$estimate[coeff_rows],
            ci_lower = raw_model_output_tab$ci_lower[coeff_rows],
            ci_upper = raw_model_output_tab$ci_upper[coeff_rows],
            effect_measure = ifelse("coxph" %in% class(model_fit), "HR", "OR"),
            is_exponentiated = FALSE  # Raw diagnostics are on log scale
        )
        
        # Apply the sophisticated filtering results
        if (length(diagnostic_extreme_result$extreme_indices) > 0) {
            # Map back to full table indices
            coeff_indices <- which(coeff_rows)
            extreme_full_indices <- coeff_indices[diagnostic_extreme_result$extreme_indices]
            
            raw_model_output_tab$inclusion_status[extreme_full_indices] <- "Filtered"
            raw_model_output_tab$filtering_reason[extreme_full_indices] <- diagnostic_extreme_result$exclusion_reasons
            
            log_enhanced(sprintf("Marked %d variables as Filtered using sophisticated detection", 
                               length(extreme_full_indices)), level = "DEBUG")
            
            for (i in seq_along(extreme_full_indices)) {
                idx <- extreme_full_indices[i]
                log_enhanced(sprintf("DEBUG: Row %d (%s) filtered: %s", 
                                   idx, raw_model_output_tab$variable[idx], 
                                   diagnostic_extreme_result$exclusion_reasons[i]), level = "DEBUG")
            }
        }
    }
    
    # Check for infinite CIs (additional check)
    infinite_ci_mask <- is.infinite(raw_model_output_tab$ci_upper) | is.infinite(raw_model_output_tab$ci_lower)
    raw_model_output_tab$inclusion_status[infinite_ci_mask] <- "Filtered"
    raw_model_output_tab$filtering_reason[infinite_ci_mask] <- "Infinite CI"
    
    # Check for NA estimates (convergence issues) - but NOT for factor label rows
    na_estimate_mask <- is.na(raw_model_output_tab$estimate) & raw_model_output_tab$row_type != "Factor Label"
    raw_model_output_tab$inclusion_status[na_estimate_mask] <- "Filtered"
    raw_model_output_tab$filtering_reason[na_estimate_mask] <- "NA estimate (convergence issue)"
    
    # IMPORTANT: Mark variables that were actually filtered out of the table
    # This ensures consistency between diagnostics and table output
    if (!is.null(filtered_variables) && length(filtered_variables) > 0) {
        log_enhanced(sprintf("DEBUG: Marking %d variables as filtered based on table output", length(filtered_variables)), level = "DEBUG")
        
        for (filtered_var in filtered_variables) {
            # Find all rows that belong to this variable
            var_rows <- grep(paste0("^", filtered_var), raw_model_output_tab$variable)
            if (length(var_rows) > 0) {
                raw_model_output_tab$inclusion_status[var_rows] <- "Filtered"
                
                # Get specific filtering reason for this variable
                # Check if this variable was already marked as filtered in the diagnostic table with a specific reason
                var_row_with_reason <- var_rows[raw_model_output_tab$filtering_reason[var_rows] != "None"][1]
                
                # Check each row individually for specific filtering reasons
                for (row_idx in var_rows) {
                    row_term <- raw_model_output_tab$term[row_idx]
                    row_reason <- "Filtered from table (specific reason not available)"
                    
                    # First check if this specific row already has a reason from diagnostic filtering
                    if (!is.na(raw_model_output_tab$filtering_reason[row_idx]) && 
                        raw_model_output_tab$filtering_reason[row_idx] != "None" &&
                        raw_model_output_tab$filtering_reason[row_idx] != "") {
                        # Row already has a specific reason - keep it
                        next
                    } else {
                        # Check if we have detailed exclusion reasons from table-level extreme estimate detection
                        if (!is.null(extreme_diagnostics) && !is.null(extreme_diagnostics$exclusion_reasons)) {
                            # Find the specific reason for this term
                            extreme_term_index <- which(extreme_diagnostics$extreme_terms == row_term)
                            
                            if (length(extreme_term_index) > 0) {
                                row_reason <- extreme_diagnostics$exclusion_reasons[extreme_term_index[1]]
                                log_enhanced(sprintf("DEBUG: Found table-level reason for %s: %s", row_term, row_reason), level = "DEBUG")
                            } else {
                                log_enhanced(sprintf("DEBUG: No specific reason found for %s, using generic", row_term), level = "DEBUG")
                            }
                        } else {
                            log_enhanced(sprintf("DEBUG: No extreme_diagnostics available for %s", row_term), level = "DEBUG")
                        }
                    }
                    
                    raw_model_output_tab$filtering_reason[row_idx] <- row_reason
                                 }
                 log_enhanced(sprintf("DEBUG: Marked variable %s as filtered with individual reasons for each term", filtered_var), level = "DEBUG")
            }
        }
    }
    
    # Add excluded rows to excluded_rows_tab
    excluded_mask <- raw_model_output_tab$inclusion_status == "Filtered"
    if (any(excluded_mask)) {
        excluded_rows_tab <- data.frame(
            variable = raw_model_output_tab$variable[excluded_mask],  # Variable name
            variable_base = raw_model_output_tab$variable_base[excluded_mask],  # Base variable name
            estimate = raw_model_output_tab$estimate[excluded_mask],
            conf_low = raw_model_output_tab$ci_lower[excluded_mask],
            conf_high = raw_model_output_tab$ci_upper[excluded_mask],
            p_value = raw_model_output_tab$p_value[excluded_mask],
            row_type = raw_model_output_tab$row_type[excluded_mask],
            exclusion_reason = raw_model_output_tab$filtering_reason[excluded_mask],
            stringsAsFactors = FALSE
        )
    }
    
    # Update filtering summary based on actual filtering results
    filtered_count <- sum(raw_model_output_tab$inclusion_status == "Filtered", na.rm = TRUE)
    remaining_count <- sum(raw_model_output_tab$inclusion_status == "Included", na.rm = TRUE)
    
    # Also count from excluded rows as a backup
    excluded_count <- if (nrow(excluded_rows_tab) > 0) {
        length(unique(excluded_rows_tab$variable[!is.na(excluded_rows_tab$variable)]))
    } else {
        0
    }
    
    # Use the larger of the two counts to ensure we don't miss any
    final_filtered_count <- max(filtered_count, excluded_count)
    
    filtering_summary_tab <- data.frame(
        total_coefficients = nrow(raw_model_output_tab),
        extreme_estimates_removed = final_filtered_count,
        rows_removed = final_filtered_count,
        sparse_table_warning = FALSE,
        confint_error = all(is.na(conf_int)),  # TRUE if all confidence intervals are NA
        remaining_coefficients = nrow(raw_model_output_tab) - final_filtered_count,
        table_has_meaningful_content = (nrow(raw_model_output_tab) - final_filtered_count) > 0  # TRUE if at least one coefficient remains after filtering
    )
    
    return(list(
        model_summary = model_summary_tab,
        model_diagnostics = model_diagnostics_tab,
        data_characteristics = data_characteristics_tab,
        other_level_details = other_level_details_tab,
        excluded_rows = excluded_rows_tab,
        raw_model_output = raw_model_output_tab,
        filtering_summary = filtering_summary_tab
    ))
}

#' Get list of variables that were completely removed from the table
#'
#' @param table_result gtsummary table object
#' @param model_fit Fitted model object
#' @return Character vector of variable names that were completely removed
get_filtered_variables_from_table <- function(table_result, model_fit) {
    
    # Get all variables from the model
    model_terms <- attr(terms(model_fit), "term.labels")
    model_var_names <- unique(c("treatment_group", model_terms))
    
    # Get variables that appear in the final table
    table_vars <- unique(table_result$table_body$variable)
    
    # Find variables that were completely removed
    removed_vars <- setdiff(model_var_names, table_vars)
    
    return(removed_vars)
}

#' Get model type for caption generation
#'
#' @param model_fit Fitted model object
#' @return Character string describing the model type
get_model_type <- function(model_fit) {
    # Get the class of the model
    model_class <- class(model_fit)[1]
    
    # Determine model type based on class
    if (model_class == "glm") {
        family <- model_fit$family$family
        if (family == "binomial") {
            return("Logistic Regression")
        } else if (family == "gaussian") {
            return("Linear Regression")
        } else if (family == "poisson") {
            return("Poisson Regression")
        } else {
            return("Generalized Linear Model")
        }
    } else if (model_class == "coxph") {
        return("Cox Proportional Hazards")
    } else if (model_class == "lm") {
        return("Linear Regression")
    } else if (model_class == "polr") {
        return("Ordinal Logistic Regression")
    } else {
        # Fallback for unknown model types
        return("Regression")
    }
}

#' Generate regression table with comprehensive diagnostics
#'
#' @param data Data frame
#' @param outcome_var Character string name of outcome variable
#' @param predictor_vars Character vector of predictor variables
#' @param confounders Character vector of confounder variables
#' @param model_type Character string for model type ("logistic", "cox", "linear")
#' @param effect_measure Character string for effect measure ("OR", "HR", "beta")
#' @param analysis_name Character string for analysis name
#' @param dataset_name Character string for dataset name
#' @param output_dir Character string for output directory
#' @param prefix Character string for file prefix
#' @param analysis_type Character string for analysis type ("post_treatment_only" or "all_patients")
#' @param time_var Character string for time variable (Cox models)
#' @param event_var Character string for event variable (Cox models)
#' @param other_map List containing mapping of what categories were collapsed into "Other"
#' @return List containing table result and diagnostics
generate_regression_table <- function(data, outcome_var, predictor_vars, confounders, model_type, effect_measure, analysis_name, dataset_name, output_dir, prefix, time_var = NULL, event_var = NULL, other_map = NULL) {
    
    log_enhanced(sprintf("Generating regression table for %s", analysis_name), level = "INFO")
    
    # Data is already processed with appropriate analysis flags from data processing
    # No additional filtering needed here
    
    # Build model formula
    formula <- build_model_formula(outcome_var, predictor_vars, confounders, model_type)
    
    # Fit regression model
    model_fit <- fit_regression_model(data, formula, model_type, time_var, event_var)
    
    if (is.null(model_fit)) {
        log_enhanced("Model fitting failed", level = "ERROR")
        return(NULL)
    }
    
    # Check for perfect separation and handle gracefully
    if (!is.null(model_fit$perfect_separation_vars) && length(model_fit$perfect_separation_vars) > 0) {
        log_enhanced(sprintf("Perfect separation detected in variables: %s. Model fitted but these variables may have unreliable estimates.", 
                           paste(model_fit$perfect_separation_vars, collapse = ", ")), level = "WARN")
    }
    
    # Create gtsummary table
    table_result <- create_gtsummary_table(model_fit, effect_measure, analysis_name, other_map, 
                                          data, outcome_var, confounders, "binary")
    
    # Get list of variables that were completely removed from the table
    filtered_variables <- get_filtered_variables_from_table(table_result, model_fit)
    
    # DEBUG: Log what variables are missing
    if (!is.null(filtered_variables) && length(filtered_variables) > 0) {
        cat("*** FILTERED VARIABLES DETECTED: ", paste(filtered_variables, collapse = ", "), "\n")
        
        # Check specifically for age_at_diagnosis
        if ("age_at_diagnosis" %in% filtered_variables) {
            cat("*** AGE_AT_DIAGNOSIS FILTERED - INVESTIGATING WHY ***\n")
            
            # Show what variables ARE in the table
            table_vars <- unique(table_result$table_body$variable)
            cat("Variables in final table: ", paste(table_vars, collapse = ", "), "\n")
            
            # Show what variables SHOULD be in the table
            model_terms <- attr(terms(model_fit), "term.labels")
            model_var_names <- unique(c("treatment_group", model_terms))
            cat("Variables expected from model: ", paste(model_var_names, collapse = ", "), "\n")
        }
    }
    
    # Apply extreme estimate filtering to get detailed diagnostics
    extreme_filtering_result <- apply_extreme_estimate_filtering(table_result, model_fit, effect_measure, 
                                                               variables_to_check = unique(c(predictor_vars, confounders)), 
                                                               analysis_name)
    
    # Use the filtered table instead of the original
    filtered_table_result <- extreme_filtering_result$tbl_filtered
    
    # Create comprehensive diagnostics with all required tabs
    diagnostics <- create_comprehensive_diagnostics(model_fit, data, outcome_var, 
                                                   predictor_vars, confounders, analysis_name, 
                                                   dataset_name, filtered_variables, other_map, 
                                                   extreme_filtering_result$diagnostics)
    
    # Create raw_output from diagnostics for save_table_outputs
    raw_output <- diagnostics$raw_model_output
    
    # Save outputs using the filtered table
    output_files <- save_table_outputs(filtered_table_result, raw_output, model_fit, 
                                      analysis_name, dataset_name, output_dir, prefix, 
                                      diagnostics, data, outcome_var, confounders)
    
    log_enhanced(sprintf("Regression table generation completed for %s", analysis_name), level = "INFO")
    
    return(list(
        table = filtered_table_result,
        diagnostics = diagnostics,
        model = model_fit,
        output_files = output_files
    ))
}

#' Create gtsummary table with proper filtering and model summary
#'
#' @param model_fit Fitted model object
#' @param effect_measure Character string for effect measure type
#' @param analysis_name Character string for analysis name
#' @param other_map List containing mapping of what categories were collapsed into "Other"
#' @param data Data frame used for the model (for interaction p-value calculation)
#' @param outcome_var Name of the outcome variable
#' @param confounders Character vector of confounders
#' @param outcome_type Type of outcome ("binary", "survival", "continuous")
#' @param show_interaction_pvalues Logical, whether to show interaction p-values
#' @return gtsummary table object
create_gtsummary_table <- function(model_fit, effect_measure, analysis_name, other_map = NULL, 
                                  data = NULL, outcome_var = NULL, confounders = NULL, 
                                  outcome_type = "binary") {
    
    # Determine model type for caption
    model_type <- get_model_type(model_fit)
    
    # Get all variable labels and filter to only include variables in the model
    all_variable_labels <- get_variable_labels()
    model_terms <- attr(terms(model_fit), "term.labels")
    model_var_names <- unique(c("treatment_group", model_terms))
    variable_labels <- all_variable_labels[intersect(names(all_variable_labels), model_var_names)]
    
    # Create the complete table first
    table <- tryCatch({
        model_fit %>%
            tbl_regression(
                exponentiate = (effect_measure %in% c("OR", "HR")),
                label = variable_labels
            ) %>%
            bold_labels() %>%
            italicize_levels() %>%
            modify_header(
                estimate = paste0("**", effect_measure, "**"),
                conf.low = "**95% CI**",
                p.value = "**p-value**"
            ) %>%
            modify_caption(paste0(model_type, " Model for ", analysis_name)) %>%
            modify_post_fmt_fun(
                fmt_fun = ~format_confidence_intervals_post(.),
                columns = "conf.low"
            )
    }, error = function(e) {
        log_enhanced(sprintf("Error creating gtsummary table: %s", e$message), level = "ERROR")
        # Return a simple table with just the model summary
        model_fit %>%
            tbl_regression(
                exponentiate = (effect_measure %in% c("OR", "HR")),
                label = variable_labels
            ) %>%
            bold_labels() %>%
            italicize_levels() %>%
            modify_caption(paste0(model_type, " Model for ", analysis_name))
    })
    
    # Post-process the table to remove variables with only reference levels
    table <- remove_orphaned_variables(table, model_fit)
    
    # Apply extreme estimate filtering to the table
    table_data <- table$table_body
    
    # Detect extreme estimates in the table data
    # Note: Main table uses exponentiate = (effect_measure %in% c("OR", "HR")), so check accordingly
    is_main_table_exponentiated <- (effect_measure %in% c("OR", "HR"))
    

    
    # Only run detection on rows with valid estimates (not NA)
    valid_rows <- !is.na(as.numeric(table_data$estimate)) & 
                  !is.na(as.numeric(table_data$conf.low)) & 
                  !is.na(as.numeric(table_data$conf.high))
    
    if (any(valid_rows)) {
        extreme_result <- detect_extreme_regression_estimates(
            estimate = as.numeric(table_data$estimate[valid_rows]),
            ci_lower = as.numeric(table_data$conf.low[valid_rows]),
            ci_upper = as.numeric(table_data$conf.high[valid_rows]),
            effect_measure = effect_measure,
            is_exponentiated = is_main_table_exponentiated
        )
        
        # Map back to original table indices
        if (length(extreme_result$extreme_indices) > 0) {
            valid_indices <- which(valid_rows)
            extreme_result$extreme_indices <- valid_indices[extreme_result$extreme_indices]
        }
    } else {
        extreme_result <- list(extreme_indices = integer(0), exclusion_reasons = character(0))
    }
    

    
    # Filter out extreme estimates
    if (length(extreme_result$extreme_indices) > 0) {
        log_enhanced(sprintf("Filtering %d extreme estimates from table for %s", 
                           length(extreme_result$extreme_indices), analysis_name), level = "INFO")
        
        # Get the extreme terms to remove
        extreme_terms <- table_data$term[extreme_result$extreme_indices]
        
        # Apply filtering
        filter_result <- filter_extreme_estimates_from_table(
            tbl_data = table_data,
            extreme_terms = extreme_terms,
            variables_to_check = unique(table_data$variable),
            analysis_name = analysis_name
        )
        
        # Update the table with filtered data
        table$table_body <- filter_result$tbl_data_filtered
        
        log_enhanced(sprintf("Removed %d rows with extreme estimates from table", 
                           filter_result$rows_removed), level = "INFO")
    }
    
    # Remove variables that now only have reference levels (no coefficients)
    table_data_updated <- table$table_body
    variables_to_remove <- c()
    
    for (var in unique(table_data_updated$variable)) {
        var_rows <- table_data_updated[table_data_updated$variable == var, ]
        
        # Check if this is a continuous variable (has no "level" rows, only label rows)
        level_rows <- var_rows[var_rows$row_type == "level", ]
        non_level_rows <- var_rows[var_rows$row_type != "level", ]  # Includes "label" and "coefficient" rows
        
        # For continuous variables (no level rows), keep them if they have any non-level rows
        if (nrow(level_rows) == 0 && nrow(non_level_rows) > 0) {
            # This is a continuous variable with estimates - keep it
            next
        }
        
        # For categorical variables, count VALID level rows (those that will appear in final table)
        # Valid means: has estimate AND has both CI bounds (not NA/infinite)
        valid_level_rows <- level_rows[
            !is.na(level_rows$estimate) & 
            level_rows$estimate != "" &
            !is.na(level_rows$conf.low) & 
            !is.na(level_rows$conf.high) &
            is.finite(as.numeric(level_rows$conf.low)) &
            is.finite(as.numeric(level_rows$conf.high))
        , ]
        
        if (nrow(valid_level_rows) == 0) {
            # This is a categorical variable with no valid levels - remove it
            variables_to_remove <- c(variables_to_remove, var)
            log_enhanced(sprintf("Removing variable '%s' - no valid levels remain after filtering (total_levels = %d, valid_levels = %d)", 
                               var, nrow(level_rows), nrow(valid_level_rows)), level = "INFO")
        }
    }
    
    # Remove these variables completely from the table
    if (length(variables_to_remove) > 0) {
        table$table_body <- table_data_updated[!table_data_updated$variable %in% variables_to_remove, ]
        
        # Also remove their "Other" captions from other_map
        if (!is.null(other_map)) {
            for (var in variables_to_remove) {
                if (var %in% names(other_map)) {
                    other_map[[var]] <- NULL
                    log_enhanced(sprintf("Removed 'Other' caption for filtered variable '%s'", var), level = "INFO")
                }
            }
        }
    }
    
    # Add "Other" level details if present in the data
    table <- add_other_level_details(table, data, other_map)
    
    return(table)
}

#' Load cohort-specific other_map.rds file
#'
#' Unified function to load cohort-specific other_map files for consistent handling
#' across all analysis functions.
#'
#' @param dataset_name Character string for dataset name (e.g., "uveal_melanoma_full_cohort")
#' @param processed_data_dir Character string for processed data directory
#' @return List containing other_map information for the specific cohort
#' @examples
#' other_map <- get_cohort_specific_other_map("uveal_melanoma_full_cohort")
get_cohort_specific_other_map <- function(dataset_name, processed_data_dir = "final_data/Analytic Dataset") {
    # Extract cohort name from dataset name
    cohort_name <- gsub("uveal_melanoma_", "", dataset_name)
    cohort_name <- gsub("_cohort", "", cohort_name)
    
    # Create cohort-specific other_map filename
    other_map_file <- file.path(processed_data_dir, paste0(cohort_name, "_other_map.rds"))
    
    if (file.exists(other_map_file)) {
        other_map <- readRDS(other_map_file)
        log_enhanced(sprintf("Loaded cohort-specific other_map for %s with %d variables", cohort_name, length(other_map)), level = "INFO")
        return(other_map)
    } else {
        log_enhanced(sprintf("No cohort-specific other_map found for %s, using empty list", cohort_name), level = "INFO")
        return(list())
    }
}

#' Add factor label p-values to gtsummary table
#'
#' This function calculates overall variable significance p-values using likelihood ratio tests
#' and places them at the factor label level, while hiding individual factor level p-values.
#'
#' @param table gtsummary table object
#' @param data Data frame used for the model
#' @param outcome_var Name of the outcome variable
#' @param confounders Character vector of confounders
#' @param outcome_type Type of outcome ("binary" or "survival")
#' @return Modified gtsummary table object
add_factor_label_pvalues_to_table <- function(table, data, outcome_var, confounders = NULL, outcome_type = "binary") {
    
    log_enhanced("Starting add_factor_label_pvalues_to_table", level = "DEBUG")
    
    # Get table data
    table_data <- table$table_body
    
    # Get unique variables (including treatment_group for testing overall significance)
    # The table data uses original variable names, not display labels
    all_variables <- unique(table_data$variable)
    variables <- all_variables  # Include ALL variables, including treatment_group
    
    # Filter confounders to only include variables that are actually in the final table
    if (!is.null(confounders)) {
        filtered_confounders <- confounders[confounders %in% all_variables]
    } else {
        filtered_confounders <- NULL
    }
    
    log_enhanced(sprintf("Variables to test for overall significance: %s", paste(variables, collapse = ", ")), level = "DEBUG")
    
    # Calculate overall variable significance p-values for each variable
    factor_label_pvalues <- list()
    for (var_name in variables) {
        # For each variable, exclude it from the confounders list
        var_confounders <- filtered_confounders[filtered_confounders != var_name]
        
        # Calculate overall significance using likelihood ratio test
        pval <- calculate_variable_overall_significance(data, var_name, outcome_var, 
                                                       treatment_var = "treatment_group",
                                                       confounders = var_confounders, 
                                                       outcome_type = outcome_type)
        factor_label_pvalues[[var_name]] <- pval
    }
    
    # Get the current table data
    table_data <- table$table_body
    
    # For each variable, put overall significance p-value at the factor label level and clear factor level p-values
    for (var_name in all_variables) {
        # Get the overall significance p-value for this variable
        pval <- factor_label_pvalues[[var_name]]
        
        # Find rows for this variable
        var_rows <- which(table_data$variable == var_name)
        
        if (length(var_rows) > 0) {
            # Clear all p-values for this variable first
            table_data$p.value[var_rows] <- NA
            
            # Find the factor label row (first row for this variable)
            label_row <- var_rows[1]
            
            # Place overall significance p-value at the factor label level (keep as numeric)
            if (!is.na(pval)) {
                table_data$p.value[label_row] <- pval
            }
        }
    }
    
    # Update the table with modified p-values
    table$table_body <- table_data
    
    return(table)
}

#' Remove variables that have only reference levels or extreme estimates (orphaned variables)
#'
#' @param table gtsummary table object
#' @param model_fit Fitted model object
#' @return Processed gtsummary table object
remove_orphaned_variables <- function(table, model_fit) {
    
    table_data <- table$table_body
    
    # Determine if the table is exponentiated
    is_exponentiated <- any(grepl("OR|HR", table$table_header$label))
    
    # Use the centralized extreme estimate detection function
    # Only check rows that have numeric estimates
    valid_rows <- which(!is.na(suppressWarnings(as.numeric(table_data$estimate))))
    
    if (length(valid_rows) > 0) {
        extreme_result <- detect_extreme_regression_estimates(
            estimate = as.numeric(table_data$estimate[valid_rows]),
            ci_lower = as.numeric(table_data$conf.low[valid_rows]),
            ci_upper = as.numeric(table_data$conf.high[valid_rows]),
            effect_measure = ifelse(is_exponentiated, "OR", "estimate"),
            is_exponentiated = is_exponentiated
        )
        
        # Get the variables associated with extreme estimates
        if (length(extreme_result$extreme_indices) > 0) {
            extreme_original_indices <- valid_rows[extreme_result$extreme_indices]
            orphaned_vars <- unique(table_data$variable[extreme_original_indices])
            
            if (length(orphaned_vars) > 0) {
                log_enhanced(sprintf("Removing variables with extreme estimates detected by centralized function: %s", 
                                   paste(orphaned_vars, collapse = ", ")), level = "INFO")
                table$table_body <- table_data[!table_data$variable %in% orphaned_vars, ]
            }
        } else {
            log_enhanced("No variables with extreme estimates found by centralized function.", level = "INFO")
        }
    } else {
        log_enhanced("No valid numeric estimates to check for extreme values.", level = "INFO")
    }
    
    return(table)
}

#' Add details about "Other" categories to table source note
#'
#' @param table A gtsummary table object
#' @param data Data frame used to create the table
#' @param other_map List mapping variable names to categories collapsed into "Other" (optional)
#' @return Modified table with source note containing "Other" category details
add_other_level_details <- function(table, data, other_map = list()) {
    # Check for variables with "Other" categories
    other_details <- c()
    
    # Get variables that are actually present in the final table
    table_variables <- unique(table$table_body$variable)
    
    # Check only factor variables that are present in the table
    factor_vars <- names(data)[sapply(data, is.factor)]
    table_factor_vars <- intersect(factor_vars, table_variables)
    
    for (var_name in table_factor_vars) {
        if ("Other" %in% levels(data[[var_name]])) {
            # CRITICAL FIX: Check if "Other" actually appears in the final table content
            table_var_data <- table$table_body[table$table_body$variable == var_name, ]
            if (any(grepl("Other", table_var_data$label, ignore.case = TRUE))) {
                # Only add caption if "Other" is actually present in the final table
                if (var_name %in% names(other_map) && length(other_map[[var_name]]) > 0) {
                    # Use the actual collapsed categories
                    collapsed_cats <- other_map[[var_name]]
                    other_details <- c(other_details, sprintf("%s: 'Other' category contains %s", var_name, paste(collapsed_cats, collapse = ", ")))
                } else {
                    # Fallback to generic message if we don't have specific information
                    other_details <- c(other_details, sprintf("%s: 'Other' category present (specific levels not mapped)", var_name))
                }
            }
        }
    }
    
    # Create source note with "Other" details (appears below the table)
    source_note_parts <- c()
    
    # Get existing source note to preserve it
    existing_source_note <- table$source_note
    if (!is.null(existing_source_note) && existing_source_note != "") {
        source_note_parts <- c(source_note_parts, existing_source_note)
    }
    
    # Add "Other" details if present
    if (length(other_details) > 0) {
        other_note <- paste("Note:", paste(other_details, collapse = "; "))
        source_note_parts <- c(source_note_parts, other_note)
    }
    
    # Combine source note parts
    if (length(source_note_parts) > 0) {
        # Use a single line break to separate parts, not double line breaks
        final_source_note <- paste(source_note_parts, collapse = "\n")
        # Set the source note directly (this will preserve existing source note and add "Other" details)
        table <- table %>%
            modify_source_note(final_source_note)
    }
    
    return(table)
}

#' Save table outputs including consolidated raw output
#'
#' @param table_result gtsummary table object
#' @param raw_output Consolidated raw output data frame
#' @param model_fit Fitted model object
#' @param analysis_name Character string for analysis name
#' @param dataset_name Character string for dataset name
#' @param output_dir Character string for output directory
#' @param prefix Character string for file prefix
#' @param diagnostics Diagnostics object containing all diagnostic data
#' @param data Data frame used for the model
#' @param outcome_var Name of the outcome variable
#' @param confounders Character vector of confounders
#' @return List of output file paths
save_table_outputs <- function(table_result, raw_output, model_fit, analysis_name, 
                              dataset_name, output_dir, prefix, diagnostics = NULL, data = NULL, outcome_var = NULL, confounders = NULL) {
    
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Generate file names
    base_filename <- paste0(prefix, analysis_name)
    html_filename <- paste0(base_filename, "_", tolower(class(model_fit)[1]), ".html")
    diagnostics_filename <- paste0(base_filename, "_diagnostics.xlsx")
    
    # Save HTML table
    html_path <- file.path(output_dir, html_filename)
    tryCatch({
        # Convert to gt format first
        gt_table <- table_result %>% as_gt()
        
        # Modify p-values in the gt table directly
        gt_table <- modify_gt_table_pvalues(gt_table, table_result, data, outcome_var, confounders, model_fit)
        
        # Save the modified gt table
        gt_table %>% gtsave(html_path)
        log_enhanced(sprintf("HTML table saved to %s", html_path), level = "INFO")
    }, error = function(e) {
        log_enhanced(sprintf("Failed to save HTML table: %s", e$message), level = "ERROR")
    })
    
    # Save comprehensive diagnostics with all required tabs
    diagnostics_path <- file.path(output_dir, diagnostics_filename)
    if (!is.null(diagnostics)) {
    tryCatch({
        # Create workbook
        wb <- createWorkbook()
        
        # Add all required tabs
        addWorksheet(wb, "Model_summary")
        writeData(wb, "Model_summary", diagnostics$model_summary)
        
        addWorksheet(wb, "Model_diagnostics")
        writeData(wb, "Model_diagnostics", diagnostics$model_diagnostics)
        
        addWorksheet(wb, "Data_characteristics")
        writeData(wb, "Data_characteristics", diagnostics$data_characteristics)
            
            addWorksheet(wb, "Other_level_details")
            writeData(wb, "Other_level_details", diagnostics$other_level_details)
        
        addWorksheet(wb, "Excluded_Rows")
        writeData(wb, "Excluded_Rows", diagnostics$excluded_rows)
        
        addWorksheet(wb, "Raw_model_output")
        # Ensure p-values are properly formatted before writing to Excel
        raw_output_formatted <- diagnostics$raw_model_output
        # Convert p-values to character to preserve all digits
        raw_output_formatted$p_value <- as.character(raw_output_formatted$p_value)
        # Replace "NA" with empty string for better Excel display
        raw_output_formatted$p_value[raw_output_formatted$p_value == "NA"] <- ""
        writeData(wb, "Raw_model_output", raw_output_formatted)
        
        # Factor_label_pvalues worksheet removed - now combined into Raw_model_output
        
        addWorksheet(wb, "Filtering_summary")
        writeData(wb, "Filtering_summary", diagnostics$filtering_summary)
        
        # Save workbook
        saveWorkbook(wb, diagnostics_path, overwrite = TRUE)
        log_enhanced(sprintf("Comprehensive diagnostics saved to %s", diagnostics_path), level = "INFO")
        
    }, error = function(e) {
        log_enhanced(sprintf("Failed to save diagnostics: %s", e$message), level = "ERROR")
    })
    }
    
    return(list(
        html_path = html_path,
        diagnostics_path = diagnostics_path
    ))
} 

#' Modify p-values in gt table directly after as_gt conversion
#'
#' This function modifies the p-values in the gt table to place factor label p-values
#' at the variable level and clear factor level p-values.
#'
#' @param gt_table gt table object
#' @param table_result Original gtsummary table object
#' @param data Data frame used for the model
#' @param outcome_var Name of the outcome variable
#' @param confounders Character vector of confounders
#' @return Modified gt table object
modify_gt_table_pvalues <- function(gt_table, table_result, data, outcome_var, confounders, model_fit = NULL) {
    
    # Get the original table data to understand the structure
    table_data <- table_result$table_body
    
    # Get unique variables (including treatment_group for testing overall significance)
    all_variables <- unique(table_data$variable)
    variables <- all_variables  # Include ALL variables, including treatment_group
    
    # Filter confounders to only include variables that are actually in the final table
    if (!is.null(confounders)) {
        filtered_confounders <- confounders[confounders %in% all_variables]
    } else {
        filtered_confounders <- NULL
    }
    
    # Calculate overall variable significance p-values for each variable
    factor_label_pvalues <- list()
    
    if (!is.null(model_fit)) {
        # Use the new unified approach with model type detection
        for (var_name in variables) {
            pval <- calculate_factor_label_pvalue(model_fit, var_name, data, outcome_var, filtered_confounders)
            factor_label_pvalues[[var_name]] <- pval
        }
    } else {
        # Fallback to old approach for backward compatibility
        for (var_name in variables) {
            # For each variable, exclude it from the confounders list
            var_confounders <- filtered_confounders[filtered_confounders != var_name]
            
            # Calculate overall significance using likelihood ratio test (old approach)
            pval <- calculate_variable_overall_significance(data, var_name, outcome_var, 
                                                           treatment_var = "treatment_group",
                                                           confounders = var_confounders, 
                                                           outcome_type = "binary")
            factor_label_pvalues[[var_name]] <- pval
        }
    }
    
    # Instead of modifying the gt table directly, we'll modify the gtsummary table
    # and then convert to gt format
    modified_table <- table_result
    
    # For each variable, modify p-values in the gtsummary table
    for (var_name in all_variables) {
        # Get the overall significance p-value for this variable
        pval <- factor_label_pvalues[[var_name]]
        
        # Find rows for this variable
        var_rows <- which(table_data$variable == var_name)
        
        if (length(var_rows) > 0) {
            # Clear all p-values for this variable first
            modified_table$table_body$p.value[var_rows] <- NA
            
            # Find the factor label row (first row for this variable)
            label_row <- var_rows[1]
            
            # Place overall significance p-value at the factor label level
            if (!is.na(pval)) {
                modified_table$table_body$p.value[label_row] <- pval
            }
        }
    }
    
    # Convert the modified gtsummary table to gt format
    modified_gt_table <- modified_table %>% as_gt()
    
    return(modified_gt_table)
} 

#' Format confidence intervals to (X,X) format for post-processing
#'
#' This function is used with modify_post_fmt_fun to format confidence intervals
#' in the format (lower, upper) instead of the default format.
#'
#' @param x Vector of confidence interval values to format
#' @return Vector of formatted confidence interval strings
format_confidence_intervals_post <- function(x) {
    # Apply the formatting function to each element of the vector
    sapply(x, function(val) {
        # If val is already formatted or empty, return as is
        if (is.na(val) || val == "" || grepl("^\\(", val)) {
            return(val)
        }
        
        # Extract the lower and upper values from the original format
        # The original format is typically "lower, upper" or similar
        if (grepl(",", val)) {
            parts <- strsplit(val, ",")[[1]]
            if (length(parts) == 2) {
                lower <- trimws(parts[1])
                upper <- trimws(parts[2])
                return(paste0("(", lower, ", ", upper, ")"))
            }
        }
        
        # If we can't parse it, return the original value
        return(val)
    })
}

#' Detect the type of regression model
#'
#' @param model_fit Fitted model object
#' @return Character string indicating model type: "linear", "logistic", "cox", "other_glm", or "unknown"
detect_model_type <- function(model_fit) {
    if (is.null(model_fit)) {
        return("unknown")
    }
    
    # Check for GLM models
    if ("glm" %in% class(model_fit)) {
        if (model_fit$family$family == "binomial") {
            return("logistic")
        } else if (model_fit$family$family == "gaussian") {
            return("linear")
        } else {
            return("other_glm")
        }
    }
    
    # Check for Cox proportional hazards models
    if ("coxph" %in% class(model_fit)) {
        return("cox")
    }
    
    # Check for linear models
    if ("lm" %in% class(model_fit)) {
        return("linear")
    }
    
    return("unknown")
}

#' Calculate F-test p-value for linear regression models
#'
#' @param model_fit Fitted linear model object
#' @param variable_name Name of the variable to test
#' @param data Data frame used for the model
#' @param outcome_var Name of the outcome variable
#' @param confounders Character vector of confounders
#' @return P-value from F-test
calculate_ftest_pvalue <- function(model_fit, variable_name, data, outcome_var, confounders) {
    tryCatch({
        # Use car::Anova for F-tests
        if (require(car, quietly = TRUE)) {
            anova_result <- car::Anova(model_fit, type = 3)
            
            # Check if variable is in the Anova result
            if (variable_name %in% rownames(anova_result)) {
                return(anova_result[variable_name, "Pr(>F)"])
            } else {
                warning(sprintf("Variable '%s' not found in Anova result", variable_name))
                return(NA)
            }
        } else {
            warning("car package not available for F-test")
            return(NA)
        }
    }, error = function(e) {
        warning(sprintf("F-test failed for variable '%s': %s", variable_name, e$message))
        return(NA)
    })
}

#' Calculate Wald test p-value as fallback
#'
#' @param model_fit Fitted model object
#' @param variable_name Name of the variable to test
#' @return P-value from Wald test
calculate_wald_pvalue <- function(model_fit, variable_name) {
    tryCatch({
        # Get model summary
        summary_result <- summary(model_fit)
        
        if ("coefficients" %in% names(summary_result)) {
            # Find coefficients for this variable
            var_coefs <- grep(paste0("^", variable_name), rownames(summary_result$coefficients), value = TRUE)
            
            if (length(var_coefs) > 0) {
                # Get p-values for this variable's coefficients
                var_pvals <- summary_result$coefficients[var_coefs, 4]
                # Return the most significant (smallest) p-value
                min_pval <- min(var_pvals, na.rm = TRUE)
                if (is.finite(min_pval)) {
                    warning(sprintf("Using minimum Wald p-value for variable '%s': %f", variable_name, min_pval))
                    return(min_pval)
                }
            }
        }
        
        warning(sprintf("Wald test failed for variable '%s'", variable_name))
        return(NA)
    }, error = function(e) {
        warning(sprintf("Wald test failed for variable '%s': %s", variable_name, e$message))
        return(NA)
    })
}

#' Calculate factor label p-value using appropriate test for model type
#'
#' @param model_fit Fitted model object
#' @param variable_name Name of the variable to test
#' @param data Data frame used for the model
#' @param outcome_var Name of the outcome variable
#' @param confounders Character vector of confounders
#' @return P-value for the factor label
calculate_factor_label_pvalue <- function(model_fit, variable_name, data, outcome_var, confounders) {
    # Detect model type
    model_type <- detect_model_type(model_fit)
    
    # Remove the variable from confounders for the test
    var_confounders <- confounders[confounders != variable_name]
    
    switch(model_type,
        "linear" = {
            # For linear regression, use F-test
            calculate_ftest_pvalue(model_fit, variable_name, data, outcome_var, var_confounders)
        },
        "logistic" = {
            # For logistic regression, use likelihood ratio test
            calculate_variable_overall_significance(data, variable_name, outcome_var,
                                                  treatment_var = "treatment_group",
                                                  confounders = var_confounders,
                                                  outcome_type = "binary")
        },
        "cox" = {
            # For Cox models, use likelihood ratio test
            calculate_variable_overall_significance(data, variable_name, outcome_var,
                                                  treatment_var = "treatment_group",
                                                  confounders = var_confounders,
                                                  outcome_type = "survival")
        },
        "other_glm" = {
            # For other GLMs, use Wald test as fallback
            calculate_wald_pvalue(model_fit, variable_name)
        },
        {
            warning(sprintf("Unsupported model type '%s' for variable '%s'", model_type, variable_name))
            return(NA)
        }
    )
}