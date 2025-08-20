# Table Diagnostics Utilities

#' Create comprehensive diagnostic information for regression models
#'
#' @param model_fit Fitted model object
#' @param data Data frame used for the model
#' @param outcome_var Name of the outcome variable
#' @param predictor_vars Character vector of predictor variables
#' @param confounders Character vector of confounder variables
#' @param analysis_name Name of the analysis
#' @param dataset_name Name of the dataset
#' @param filtered_variables Character vector of filtered variables (optional)
#' @param other_map List mapping variable names to "Other" categories (optional)
#' @param extreme_diagnostics List containing extreme estimate diagnostics (optional)
#' @param treatment_var Name of treatment variable (default: "treatment_group")
#' @param effect_measure Effect measure type (optional, auto-detected if NULL)
#' @param table_result gtsummary table object (optional)
#' @return List containing all diagnostic data frames
create_comprehensive_diagnostics <- function(model_fit, data, outcome_var, predictor_vars, confounders, analysis_name, dataset_name, filtered_variables = NULL, other_map = list(), extreme_diagnostics = NULL, treatment_var = "treatment_group", effect_measure = NULL, table_result = NULL) {
    # === UNIFIED MODEL EXTRACTION ===
    # Single model summary call - no redundancy
    model_summary <- summary(model_fit)
    coefs <- coef(model_fit)

    # Unified confidence interval extraction - no gtsummary fallback complexity
    conf_int <- tryCatch(
        suppressWarnings(confint(model_fit)),
        error = function(e) {
            logger::log_warn(sprintf("Warning: Could not compute confidence intervals: %s", e$message))
            matrix(NA,
                nrow = length(coefs), ncol = 2,
                dimnames = list(names(coefs), c("2.5 %", "97.5 %"))
            )
        }
    )

    # Unified model type and effect measure detection - no duplication
    model_type <- class(model_fit)[1]
    if (is.null(effect_measure)) {
        effect_measure <- ifelse("coxph" %in% class(model_fit), "HR", "OR")
    }
    is_exponentiated <- effect_measure %in% c("OR", "HR")
    filtering_scale <- ifelse(is_exponentiated, "log_scale", "raw_scale")

    # === UNIFIED P-VALUE EXTRACTION ===
    p_values <- extract_p_values(model_summary, coefs)



    # === UNIFIED FACTOR LABEL P-VALUES ===
    factor_label_pvalues_tab <- create_factor_label_pvalues(model_fit, data, outcome_var, confounders, treatment_var)
    factor_label_pvalue_map <- setNames(factor_label_pvalues_tab$factor_label_pvalue, factor_label_pvalues_tab$variable)

    # === UNIFIED DIAGNOSTIC TABLES ===
    model_summary_tab <- create_model_summary_tab(model_fit, data, outcome_var, confounders, analysis_name, extreme_diagnostics, filtered_variables)
    model_diagnostics_tab <- create_model_diagnostics_tab(model_fit, dataset_name, analysis_name, effect_measure, coefs, extreme_diagnostics, filtered_variables)
    data_characteristics_tab <- create_data_characteristics_tab(dataset_name, analysis_name, predictor_vars, confounders, outcome_var, data)
    other_level_details_tab <- create_other_level_details_tab(model_fit, other_map)

    # === UNIFIED RAW MODEL OUTPUT ===
    raw_model_output_tab <- create_raw_model_output_tab(
        coefs, conf_int, p_values, factor_label_pvalue_map,
        effect_measure, filtering_scale, model_fit, data, factor_label_pvalues_tab,
        table_result  # Pass the gtsummary table to ensure consistent ordering
    )

    # === UNIFIED FILTERING LOGIC ===
    raw_model_output_tab <- apply_filtering_logic(
        raw_model_output_tab, filtered_variables, extreme_diagnostics, factor_label_pvalues_tab
    )

    # === UNIFIED EXCLUDED ROWS ===
    excluded_rows_tab <- create_excluded_rows_tab(raw_model_output_tab)

    # === UNIFIED FILTERING SUMMARY ===
    filtering_summary_tab <- create_filtering_summary_tab(raw_model_output_tab, excluded_rows_tab, conf_int, predictor_vars)

    # === UNIFIED REFERENCE LEVELS ===
    reference_levels_tab <- create_reference_levels_tab(extreme_diagnostics)

    return(list(
        model_summary = model_summary_tab,
        model_diagnostics_tab = model_diagnostics_tab,
        data_characteristics = data_characteristics_tab,
        other_level_details = other_level_details_tab,
        excluded_rows = excluded_rows_tab,
        raw_model_output = raw_model_output_tab,
        filtering_summary = filtering_summary_tab,
        reference_levels = reference_levels_tab
    ))
}

# === HELPER FUNCTIONS ===

#' Extract p-values from model summary
extract_p_values <- function(model_summary, coefs) {
    n_coefs <- length(coefs)
    p_values_vector <- rep(NA, n_coefs)
    names(p_values_vector) <- names(coefs)

    available_coefs <- intersect(names(coefs), rownames(model_summary$coefficients))
    if (length(available_coefs) > 0) {
        col_names <- colnames(model_summary$coefficients)
        p_value_col <- which(col_names %in% c("Pr(>|z|)", "Pr(>|t|)", "Pr(>F)"))
        if (length(p_value_col) == 0) {
            p_value_col <- ncol(model_summary$coefficients)
            logger::log_warn(sprintf("Warning: Could not find p-value column, using last column (%d)", p_value_col))
        } else {
            p_value_col <- p_value_col[1]
        }
        p_values_vector[available_coefs] <- as.numeric(model_summary$coefficients[available_coefs, p_value_col])
    }

    return(p_values_vector)
}



#' Create factor label p-values table
create_factor_label_pvalues <- function(model_fit, data, outcome_var, confounders, treatment_var) {
    model_terms <- attr(terms(model_fit), "term.labels")
    variables_to_test <- unique(c(treatment_var, model_terms))

    factor_label_pvalues_list <- list()
    for (var_name in variables_to_test) {
        var_confounders <- confounders[confounders != var_name]
        pval <- calculate_factor_label_pvalue(model_fit, var_name, data, outcome_var, var_confounders, treatment_var = treatment_var)
        factor_label_pvalues_list[[length(factor_label_pvalues_list) + 1]] <- data.frame(
            variable = var_name,
            factor_label_pvalue = pval,
            test_type = "Likelihood Ratio Test",
            stringsAsFactors = FALSE
        )
    }

    if (length(factor_label_pvalues_list) > 0) {
        do.call(rbind, factor_label_pvalues_list)
    } else {
        data.frame(
            variable = character(),
            factor_label_pvalue = numeric(),
            test_type = character(),
            stringsAsFactors = FALSE
        )
    }
}

#' Create model summary table
create_model_summary_tab <- function(model_fit, data, outcome_var, confounders, analysis_name, extreme_diagnostics, filtered_variables) {
    data.frame(
        analysis_type = paste0("unified_", analysis_name),
        outcome = outcome_var,
        n_total = nrow(data),
        n_events = ifelse("coxph" %in% class(model_fit),
            sum(model_fit$y[, 2]),
            sum(as.numeric(model_fit$model[[1]]))
        ),
        model_fitted = !is.null(model_fit),
        confounders_used = paste(confounders, collapse = ", "),
        notes = "Generated by unified table generation system",
        stringsAsFactors = FALSE
    )
}

#' Create model diagnostics table
create_model_diagnostics_tab <- function(model_fit, dataset_name, analysis_name, effect_measure, coefs, extreme_diagnostics, filtered_variables) {
    model_warnings <- c()
    if (!is.null(model_fit) && "glm" %in% class(model_fit) && !model_fit$converged) {
        model_warnings <- c(model_warnings, "Model did not converge")
    }
    if (!is.null(extreme_diagnostics) && length(extreme_diagnostics) > 0) {
        model_warnings <- c(model_warnings, "Extreme estimates detected")
    }
    if (!is.null(filtered_variables) && length(filtered_variables) > 0) {
        model_warnings <- c(model_warnings, "Variables were filtered due to extreme estimates")
    }
    model_warnings_text <- if (length(model_warnings) > 0) paste(model_warnings, collapse = "; ") else "None"

    data.frame(
        dataset_name = dataset_name,
        model_type = class(model_fit)[1],
        effect_measure = effect_measure,
        n_coefficients = length(coefs),
        model_converged = ifelse("glm" %in% class(model_fit), model_fit$converged, TRUE),
        log_likelihood = ifelse("glm" %in% class(model_fit), logLik(model_fit), NA),
        aic = AIC(model_fit),
        bic = BIC(model_fit),
        model_warnings = model_warnings_text,
        stringsAsFactors = FALSE
    )
}

#' Create data characteristics table
create_data_characteristics_tab <- function(dataset_name, analysis_name, predictor_vars, confounders, outcome_var, data) {
    data.frame(
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
}

#' Create other level details table
create_other_level_details_tab <- function(model_fit, other_map) {
    other_level_details_list <- list()
    model_data <- model_fit$model

    for (var_name in names(model_data)) {
        if (var_name != "(weights)" && var_name != "(offset)") {
            var_data <- model_data[[var_name]]
            if (is.factor(var_data) || is.character(var_data)) {
                levels_data <- levels(var_data) %||% unique(var_data)
                if ("Other" %in% levels_data) {
                    other_count <- sum(var_data == "Other", na.rm = TRUE)
                    other_categories <- if (var_name %in% names(other_map) && length(other_map[[var_name]]) > 0) {
                        paste(other_map[[var_name]], collapse = ", ")
                    } else {
                        "Original categories not available in model data"
                    }
                    other_level_details_list[[length(other_level_details_list) + 1]] <- data.frame(
                        variable = var_name,
                        has_other_level = TRUE,
                        other_categories = other_categories,
                        other_count = other_count,
                        stringsAsFactors = FALSE
                    )
                }
            }
        }
    }

    if (length(other_level_details_list) > 0) {
        do.call(rbind, other_level_details_list)
    } else {
        data.frame(
            variable = character(),
            has_other_level = logical(),
            other_categories = character(),
            other_count = integer(),
            stringsAsFactors = FALSE
        )
    }
}

#' Create raw model output table
create_raw_model_output_tab <- function(coefs, conf_int, p_values, factor_label_pvalue_map, effect_measure, filtering_scale, model_fit, data, factor_label_pvalues_tab, table_result) {
    # Use gtsummary table structure as the foundation
    gts_table_body <- table_result$table_body
    
    # Debug: Print the gtsummary structure to understand what we're working with
    logger::log_debug("gtsummary table structure:")
    logger::log_debug(paste("Total rows:", nrow(gts_table_body)))
    logger::log_debug(paste("Columns:", paste(colnames(gts_table_body), collapse = ", ")))
    logger::log_debug("First few rows:")
    for (i in seq_len(min(5, nrow(gts_table_body)))) {
        row <- gts_table_body[i, ]
        logger::log_debug(sprintf("Row %d: variable='%s', row_type='%s', reference_row=%s, term='%s'", 
                                 i, row$variable, row$row_type, row$reference_row, row$term))
    }
    
    # Create the table structure based on gtsummary order
    table_rows <- list()
    current_pos <- 1
    
    # Add intercept first if it exists in the model coefficients
    if ("(Intercept)" %in% names(coefs)) {
        intercept_row <- data.frame(
            variable_base = "(Intercept)",
            variable = "(Intercept)",
            effect_measure = effect_measure,
            filtering_scale = filtering_scale,
            raw_coefficient = as.numeric(coefs["(Intercept)"]),
            raw_ci_lower = if ("(Intercept)" %in% rownames(conf_int)) conf_int["(Intercept)", 1] else NA_real_,
            raw_ci_upper = if ("(Intercept)" %in% rownames(conf_int)) conf_int["(Intercept)", 2] else NA_real_,
            exp_estimate = if (effect_measure %in% c("OR", "HR")) exp(as.numeric(coefs["(Intercept)"])) else NA,
            exp_ci_lower = if (effect_measure %in% c("OR", "HR") && "(Intercept)" %in% rownames(conf_int)) exp(conf_int["(Intercept)", 1]) else NA,
            exp_ci_upper = if (effect_measure %in% c("OR", "HR") && "(Intercept)" %in% rownames(conf_int)) exp(conf_int["(Intercept)", 2]) else NA,
            p_value = p_values["(Intercept)"],
            row_type = "Coefficient",
            inclusion_status = "Included",
            filtering_reason = "None",
            stringsAsFactors = FALSE
        )
        
        # Add reporting columns
        is_survival_model <- "coxph" %in% class(model_fit)
        if (is_survival_model) {
            intercept_row$hazard_ratio <- intercept_row$exp_estimate
            intercept_row$hr_ci_lower <- intercept_row$exp_ci_lower
            intercept_row$hr_ci_upper <- intercept_row$exp_ci_upper
            intercept_row$odds_ratio <- NA_real_
            intercept_row$or_ci_lower <- NA_real_
            intercept_row$or_ci_upper <- NA_real_
        } else if ("glm" %in% class(model_fit) && effect_measure == "OR") {
            intercept_row$hazard_ratio <- NA_real_
            intercept_row$hr_ci_lower <- NA_real_
            intercept_row$hr_ci_upper <- NA_real_
            intercept_row$odds_ratio <- intercept_row$exp_estimate
            intercept_row$or_ci_lower <- intercept_row$exp_ci_lower
            intercept_row$or_ci_upper <- intercept_row$exp_ci_upper
        } else {
            intercept_row$hazard_ratio <- NA_real_
            intercept_row$hr_ci_lower <- NA_real_
            intercept_row$hr_ci_upper <- NA_real_
            intercept_row$odds_ratio <- NA_real_
            intercept_row$or_ci_lower <- NA_real_
            intercept_row$or_ci_upper <- NA_real_
        }
        
        table_rows[[current_pos]] <- intercept_row
        current_pos <- current_pos + 1
    }
    
    # Process each row in gtsummary order
    for (i in seq_len(nrow(gts_table_body))) {
        gts_row <- gts_table_body[i, ]
        var_name <- gts_row$variable
        row_type <- gts_row$row_type
        is_reference <- gts_row$reference_row
        
        if (row_type == "label") {
            # This is a factor label row - create it with interaction p-value
            factor_pvalue <- if (var_name %in% factor_label_pvalues_tab$variable) {
                factor_label_pvalues_tab$factor_label_pvalue[factor_label_pvalues_tab$variable == var_name]
            } else {
                NA_real_
            }
            
            factor_label_row <- data.frame(
                variable_base = var_name,
                variable = var_name,
                effect_measure = effect_measure,
                filtering_scale = filtering_scale,
                raw_coefficient = NA_real_,
                raw_ci_lower = NA_real_,
                raw_ci_upper = NA_real_,
                exp_estimate = NA_real_,
                exp_ci_lower = NA_real_,
                exp_ci_upper = NA_real_,
                p_value = factor_pvalue,
                row_type = "Factor Label",
                inclusion_status = "Included",
                filtering_reason = "None",
                stringsAsFactors = FALSE
            )
            
            # Add reporting columns
            factor_label_row$hazard_ratio <- NA_real_
            factor_label_row$hr_ci_lower <- NA_real_
            factor_label_row$hr_ci_upper <- NA_real_
            factor_label_row$odds_ratio <- NA_real_
            factor_label_row$or_ci_lower <- NA_real_
            factor_label_row$or_ci_upper <- NA_real_
            
            table_rows[[current_pos]] <- factor_label_row
            current_pos <- current_pos + 1
            
        } else if (row_type == "level") {
            # This is a level row - check if it's a reference level or coefficient
            if (!is_reference) {
                # This is a coefficient level - find the corresponding coefficient
                # The term column in gtsummary contains the actual coefficient name
                coeff_name <- gts_row$term
                if (!is.na(coeff_name) && coeff_name %in% names(coefs)) {
                    # Extract the actual level name by removing the base variable from the term
                    level_name <- sub(paste0("^", var_name), "", coeff_name)
                    if (level_name == "") {
                        level_name <- coeff_name
                    }
                    
                    # Get coefficient data
                    coeff_value <- coefs[coeff_name]
                    p_value <- p_values[coeff_name]
                    
                    # Get confidence interval if available
                    ci_lower <- NA_real_
                    ci_upper <- NA_real_
                    if (coeff_name %in% rownames(conf_int)) {
                        ci_lower <- conf_int[coeff_name, 1]
                        ci_upper <- conf_int[coeff_name, 2]
                    }
                    
                    # Create coefficient row
                    coeff_row <- data.frame(
                        variable_base = var_name,  # Base variable name (e.g., "treatment_group")
                        variable = level_name,     # Actual level name (e.g., "GKSRS")
                        effect_measure = effect_measure,
                        filtering_scale = filtering_scale,
                        raw_coefficient = as.numeric(coeff_value),
                        raw_ci_lower = ci_lower,
                        raw_ci_upper = ci_upper,
                        exp_estimate = if (effect_measure %in% c("OR", "HR")) exp(as.numeric(coeff_value)) else NA,
                        exp_ci_lower = if (effect_measure %in% c("OR", "HR")) exp(ci_lower) else NA,
                        exp_ci_upper = if (effect_measure %in% c("OR", "HR")) exp(ci_upper) else NA,
                        p_value = p_value,
                        row_type = "Coefficient",
                        inclusion_status = "Included",
                        filtering_reason = "None",
                        stringsAsFactors = FALSE
                    )
                    
                    # Add reporting columns based on model type
                    is_survival_model <- "coxph" %in% class(model_fit)
                    if (is_survival_model) {
                        coeff_row$hazard_ratio <- coeff_row$exp_estimate
                        coeff_row$hr_ci_lower <- coeff_row$exp_ci_lower
                        coeff_row$hr_ci_upper <- coeff_row$exp_ci_upper
                        coeff_row$odds_ratio <- NA_real_
                        coeff_row$or_ci_lower <- NA_real_
                        coeff_row$or_ci_upper <- NA_real_
                    } else if ("glm" %in% class(model_fit) && effect_measure == "OR") {
                        coeff_row$hazard_ratio <- NA_real_
                        coeff_row$hr_ci_lower <- NA_real_
                        coeff_row$hr_ci_upper <- NA_real_
                        coeff_row$odds_ratio <- coeff_row$exp_estimate
                        coeff_row$or_ci_lower <- coeff_row$exp_ci_lower
                        coeff_row$or_ci_upper <- coeff_row$exp_ci_upper
                    } else {
                        coeff_row$hazard_ratio <- NA_real_
                        coeff_row$hr_ci_lower <- NA_real_
                        coeff_row$hr_ci_upper <- NA_real_
                        coeff_row$odds_ratio <- NA_real_
                        coeff_row$or_ci_lower <- NA_real_
                        coeff_row$or_ci_upper <- NA_real_
                    }
                    
                    table_rows[[current_pos]] <- coeff_row
                    current_pos <- current_pos + 1
                }
            }
        }
    }
    
    # Combine all rows
    if (length(table_rows) > 0) {
        raw_model_output_tab <- do.call(rbind, table_rows)
        
        # Add explanatory note for single-variable models with missing label p-value
        # Detect number of non-intercept predictors from model terms
        model_terms <- attr(terms(model_fit), "term.labels")
        has_single_predictor <- length(model_terms) == 1
        if (has_single_predictor) {
            label_rows <- which(raw_model_output_tab$row_type == "Factor Label")
            if (length(label_rows) > 0) {
                for (idx in label_rows) {
                    if (is.na(raw_model_output_tab$p_value[idx])) {
                        raw_model_output_tab$filtering_reason[idx] <- "Overall p-value not computed (single-variable model; no interaction)"
                    }
                }
            }
        }
        
        return(raw_model_output_tab)
    } else {
        # Fallback to empty table if something went wrong
        return(data.frame(
            variable_base = character(),
            variable = character(),
            effect_measure = character(),
            filtering_scale = character(),
            raw_coefficient = numeric(),
            raw_ci_lower = numeric(),
            raw_ci_upper = numeric(),
            exp_estimate = numeric(),
            exp_ci_lower = numeric(),
            exp_ci_upper = numeric(),
            p_value = numeric(),
            row_type = character(),
            inclusion_status = character(),
            filtering_reason = character(),
            stringsAsFactors = FALSE
        ))
    }
}

#' Apply filtering logic to raw model output
apply_filtering_logic <- function(raw_model_output_tab, filtered_variables, extreme_diagnostics, factor_label_pvalues_tab) {
    # Apply basic filtering rules
    infinite_ci_mask <- is.infinite(raw_model_output_tab$raw_ci_upper) | is.infinite(raw_model_output_tab$raw_ci_lower)
    raw_model_output_tab$inclusion_status[infinite_ci_mask] <- "Filtered"
    raw_model_output_tab$filtering_reason[infinite_ci_mask] <- "Infinite CI"

    na_estimate_mask <- is.na(raw_model_output_tab$raw_coefficient) & raw_model_output_tab$row_type != "Factor Label"
    raw_model_output_tab$inclusion_status[na_estimate_mask] <- "Filtered"
    raw_model_output_tab$filtering_reason[na_estimate_mask] <- "NA estimate (convergence issue)"

    # Apply extreme estimates filtering logic
    if (!is.null(extreme_diagnostics) && !is.null(extreme_diagnostics$extreme_terms)) {
        extreme_terms <- extreme_diagnostics$extreme_terms
        exclusion_reasons <- extreme_diagnostics$exclusion_reasons
        
        for (i in seq_len(nrow(raw_model_output_tab))) {
            if (raw_model_output_tab$row_type[i] == "Coefficient") {
                # Reconstruct the full term name for matching
                full_term_name <- paste0(raw_model_output_tab$variable_base[i], raw_model_output_tab$variable[i])
                
                # Check if this term is in the extreme_terms list
                if (full_term_name %in% extreme_terms) {
                    raw_model_output_tab$inclusion_status[i] <- "Filtered"
                    
                    # Find the corresponding exclusion reason
                    term_index <- which(extreme_terms == full_term_name)
                    if (length(term_index) > 0 && length(exclusion_reasons) >= term_index[1]) {
                        raw_model_output_tab$filtering_reason[i] <- exclusion_reasons[term_index[1]]
                    } else {
                        raw_model_output_tab$filtering_reason[i] <- "Extreme estimate detected"
                    }
                }
            }
        }
    }

    # Apply completely removed variables logic
    if (!is.null(extreme_diagnostics) && !is.null(extreme_diagnostics$completely_removed_variables)) {
        completely_removed_vars <- extreme_diagnostics$completely_removed_variables
        for (removed_var in completely_removed_vars) {
            var_rows <- which(raw_model_output_tab$variable_base == removed_var)
            if (length(var_rows) > 0) {
                for (row_idx in var_rows) {
                    current_reason <- raw_model_output_tab$filtering_reason[row_idx]
                    if (is.na(current_reason) || current_reason == "None" || current_reason == "") {
                        raw_model_output_tab$filtering_reason[row_idx] <- "Variable completely removed (only reference levels remained)"
                    }
                }
                raw_model_output_tab$inclusion_status[var_rows] <- "Filtered"
            }
        }
    }

    return(raw_model_output_tab)
}

#' Create excluded rows table
create_excluded_rows_tab <- function(raw_model_output_tab) {
    excluded_mask <- raw_model_output_tab$inclusion_status == "Filtered"
    if (any(excluded_mask)) {
        data.frame(
            variable_base = raw_model_output_tab$variable_base[excluded_mask],
            variable = raw_model_output_tab$variable[excluded_mask],
            raw_coefficient = raw_model_output_tab$raw_coefficient[excluded_mask],
            raw_ci_lower = raw_model_output_tab$raw_ci_lower[excluded_mask],
            raw_ci_upper = raw_model_output_tab$raw_ci_upper[excluded_mask],
            hazard_ratio = raw_model_output_tab$hazard_ratio[excluded_mask],
            hr_ci_lower = raw_model_output_tab$hr_ci_lower[excluded_mask],
            hr_ci_upper = raw_model_output_tab$hr_ci_upper[excluded_mask],
            odds_ratio = raw_model_output_tab$odds_ratio[excluded_mask],
            or_ci_lower = raw_model_output_tab$or_ci_lower[excluded_mask],
            or_ci_upper = raw_model_output_tab$or_ci_upper[excluded_mask],
            p_value = raw_model_output_tab$p_value[excluded_mask],
            row_type = raw_model_output_tab$row_type[excluded_mask],
            exclusion_reason = raw_model_output_tab$filtering_reason[excluded_mask],
            stringsAsFactors = FALSE
        )
    } else {
        data.frame(
            term = character(),
            variable = character(),
            label = character(),
            estimate = numeric(),
            conf_low = numeric(),
            conf_high = numeric(),
            exclusion_reason = character(),
            stringsAsFactors = FALSE
        )
    }
}

#' Create filtering summary table
create_filtering_summary_tab <- function(raw_model_output_tab, excluded_rows_tab, conf_int, predictor_vars) {
    filtered_count <- sum(raw_model_output_tab$inclusion_status == "Filtered", na.rm = TRUE)
    remaining_count <- sum(raw_model_output_tab$inclusion_status == "Included", na.rm = TRUE)
    excluded_count <- if (nrow(excluded_rows_tab) > 0) {
        length(unique(excluded_rows_tab$variable[!is.na(excluded_rows_tab$variable)]))
    } else {
        0
    }
    final_filtered_count <- max(filtered_count, excluded_count)

    main_predictor_filtered <- FALSE
    if (!is.null(predictor_vars) && length(predictor_vars) > 0) {
        for (pred_var in predictor_vars) {
            # Check if any coefficient rows exist for this predictor variable
            var_rows <- which(raw_model_output_tab$variable_base == pred_var)
            if (length(var_rows) == 0) {
                # No rows found for this predictor - it was completely removed
                main_predictor_filtered <- TRUE
                break
            } else {
                # Check if all coefficient rows for this predictor are filtered
                coeff_rows <- var_rows[raw_model_output_tab$row_type[var_rows] == "Coefficient"]
                if (length(coeff_rows) > 0) {
                    all_filtered <- all(raw_model_output_tab$inclusion_status[coeff_rows] == "Filtered")
                    if (all_filtered) {
                        main_predictor_filtered <- TRUE
                        break
                    }
                }
            }
        }
    }

    data.frame(
        total_coefficients = nrow(raw_model_output_tab),
        extreme_estimates_removed = final_filtered_count,
        rows_removed = final_filtered_count,
        sparse_table_warning = FALSE,
        confint_error = all(is.na(conf_int)),
        remaining_coefficients = nrow(raw_model_output_tab) - final_filtered_count,
        table_has_meaningful_content = (nrow(raw_model_output_tab) - final_filtered_count) > 0,
        main_predictor_filtered = main_predictor_filtered
    )
}

#' Create reference levels table
create_reference_levels_tab <- function(extreme_diagnostics) {
    if (!is.null(extreme_diagnostics) && !is.null(extreme_diagnostics$reference_levels_info)) {
        extreme_diagnostics$reference_levels_info
    } else {
        data.frame(
            variable = character(),
            reference_level = character(),
            stringsAsFactors = FALSE
        )
    }
}

#' Get list of variables that were completely removed from the table
#'
#' @param table_result gtsummary table object
#' @param model_fit Fitted model object
#' @return Character vector of variable names that were completely removed
get_filtered_variables_from_table <- function(table_result, model_fit) {
    model_terms <- attr(terms(model_fit), "term.labels")
    model_var_names <- unique(c("treatment_group", model_terms))
    table_vars <- unique(table_result$table_body$variable)
    removed_vars <- setdiff(model_var_names, table_vars)
    return(removed_vars)
}

# NOTE: remove_orphaned_variables function has been removed as dead code
# This functionality is now handled by process_extreme_estimates

#' Calculate F-test p-value for linear regression models
#'
#' @param model_fit Fitted linear model object
#' @param variable_name Name of the variable to test
#' @param data Data frame used for the model
#' @param outcome_var Name of the outcome variable
#' @param confounders Character vector of confounders
#' @return Numeric p-value (or NA on failure)
calculate_ftest_pvalue <- function(model_fit, variable_name, data, outcome_var, confounders) {
    tryCatch(
        {
            if (require(car, quietly = TRUE)) {
                anova_result <- car::Anova(model_fit, type = 3)
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
        },
        error = function(e) {
            warning(sprintf("F-test failed for variable '%s': %s", variable_name, e$message))
            return(NA)
        }
    )
}

#' Calculate Wald test p-value as fallback
#'
#' @param model_fit Fitted model object
#' @param variable_name Name of the variable to test
#' @return Numeric p-value (or NA on failure)
calculate_wald_pvalue <- function(model_fit, variable_name) {
    tryCatch(
        {
            summary_result <- summary(model_fit)
            if ("coefficients" %in% names(summary_result)) {
                var_coefs <- grep(paste0("^", variable_name), rownames(summary_result$coefficients), value = TRUE)
                if (length(var_coefs) > 0) {
                    var_pvals <- summary_result$coefficients[var_coefs, 4]
                    min_pval <- suppressWarnings(min(var_pvals, na.rm = TRUE))
                    if (is.finite(min_pval)) {
                        warning(sprintf("Using minimum Wald p-value for variable '%s': %f", variable_name, min_pval))
                        return(min_pval)
                    }
                }
            }
            warning(sprintf("Wald test failed for variable '%s'", variable_name))
            return(NA)
        },
        error = function(e) {
            warning(sprintf("Wald test failed for variable '%s': %s", variable_name, e$message))
            return(NA)
        }
    )
}

#' Calculate factor label p-value using appropriate test for model type
#'
#' @param model_fit Fitted model object
#' @param variable_name Variable to test
#' @param data Data frame
#' @param outcome_var Outcome variable name
#' @param confounders Character vector of confounders
#' @param treatment_var Treatment variable name (default: "treatment_group")
#' @return Numeric p-value (or NA on failure)
calculate_factor_label_pvalue <- function(model_fit, variable_name, data, outcome_var, confounders, treatment_var = "treatment_group") {
    model_type <- detect_model_type(model_fit)
    var_confounders <- confounders[confounders != variable_name]
    switch(model_type,
        "linear" = {
            calculate_ftest_pvalue(model_fit, variable_name, data, outcome_var, var_confounders)
        },
        "logistic" = {
            calculate_variable_overall_significance(
                data, variable_name, outcome_var,
                treatment_var = treatment_var,
                confounders = var_confounders,
                outcome_type = model_type_to_outcome_type(model_type)
            )
        },
        "cox" = {
            # Extract overall p-value using Wald test on coefficients (no refitting needed)
            tryCatch({
                co <- coef(model_fit)
                V <- vcov(model_fit)
                
                # Find all coefficients for this variable
                idx <- grep(paste0("^", variable_name), names(co))
                if (length(idx) > 0) {
                    beta <- as.numeric(co[idx])
                    # Check if coefficients are finite and non-zero
                    if (all(is.finite(beta)) && any(beta != 0)) {
                        # Filter out zero coefficients to avoid singular matrix
                        non_zero_idx <- which(beta != 0)
                        if (length(non_zero_idx) > 0) {
                            beta_nonzero <- beta[non_zero_idx]
                            V_sub <- V[idx[non_zero_idx], idx[non_zero_idx], drop = FALSE]
                            # Compute Wald test statistic
                            chisq <- as.numeric(t(beta_nonzero) %*% solve(V_sub) %*% beta_nonzero)
                            df <- length(non_zero_idx)
                            if (is.finite(chisq) && df > 0 && chisq > 0) {
                                return(pchisq(chisq, df = df, lower.tail = FALSE))
                            }
                        }
                    }
                }
                NA_real_
            }, error = function(e) {
                warning(sprintf("Wald test failed for variable '%s': %s", variable_name, e$message))
                NA_real_
            })
        },
        "other_glm" = {
            calculate_wald_pvalue(model_fit, variable_name)
        },
        {
            warning(sprintf("Unsupported model type '%s' for variable '%s'", model_type, variable_name))
            NA
        }
    )
}
