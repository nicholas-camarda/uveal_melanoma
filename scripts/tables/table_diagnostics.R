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
#' @param extreme_diagnostics List containing extreme estimate diagnostics (optional)
#' @param treatment_var Name of treatment variable (default: "treatment_group")
#' @param effect_measure Effect measure type (optional, auto-detected if NULL)
#' @param table_result gtsummary table object (optional)
#' @param sparse_level_diagnostics Data frame of excluded sparse levels (optional)
#' @param filter_stats List summarizing pre/post filtering sample sizes (optional)
#' @return List containing all diagnostic data frames
create_comprehensive_diagnostics <- function(model_fit, data, outcome_var, predictor_vars, confounders, analysis_name, dataset_name, filtered_variables = NULL, extreme_diagnostics = NULL, treatment_var = "treatment_group", effect_measure = NULL, table_result = NULL, sparse_level_diagnostics = NULL, filter_stats = NULL) {
    # === UNIFIED MODEL EXTRACTION ===
    # Single model summary call - no redundancy
    model_summary <- summary(model_fit)
    coefs <- coef(model_fit)

    # Unified confidence interval extraction - no gtsummary fallback complexity
    conf_int <- tryCatch(
        suppressMessages(suppressWarnings(confint(model_fit))),
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
    sparse_level_diagnostics_tab <- create_sparse_level_diagnostics_tab(sparse_level_diagnostics)
    sample_size_summary_tab <- build_sample_size_summary_tab(filter_stats, dataset_name, analysis_name, modeled_n = nrow(data))

    # === UNIFIED RAW MODEL OUTPUT ===
    raw_model_output_tab <- create_raw_model_output_tab(
        coefs, conf_int, p_values, factor_label_pvalue_map,
        effect_measure, filtering_scale, model_fit, data, outcome_var, factor_label_pvalues_tab,
        table_result  # Pass the gtsummary table to ensure consistent ordering
    )

    # === UNIFIED FILTERING LOGIC ===
    raw_model_output_tab <- apply_filtering_logic(
        raw_model_output_tab, filtered_variables, extreme_diagnostics, factor_label_pvalues_tab
    )

    # === UNIFIED FILTERING SUMMARY ===
    filtering_summary_tab <- create_filtering_summary_tab(raw_model_output_tab, conf_int, predictor_vars)

    # === UNIFIED REFERENCE LEVELS ===
    reference_levels_tab <- create_reference_levels_tab(extreme_diagnostics)

    covariate_variation_tab <- build_covariate_variation_tab(
        removed_covariates = model_fit$removed_covariates %||% list(),
        dataset_name = dataset_name,
        analysis_name = analysis_name
    )

    return(list(
        model_summary = model_summary_tab,
        model_diagnostics_tab = model_diagnostics_tab,
        data_characteristics = data_characteristics_tab,
        sparse_level_diagnostics = sparse_level_diagnostics_tab,
        raw_model_output = raw_model_output_tab,
        filtering_summary = filtering_summary_tab,
        reference_levels = reference_levels_tab,
        sample_size_summary = sample_size_summary_tab,
        covariate_variation = covariate_variation_tab
    ))
}

#' Build a single-row sample size audit table used in diagnostics and HTML notes
build_sample_size_summary_tab <- function(filter_stats, dataset_name, analysis_name, modeled_n) {
    default_stats <- list(
        initial_n = modeled_n,
        model_n = modeled_n,
        removed_n = 0L,
        removed_pct = 0,
        removal_reason = "No rows removed prior to modeling"
    )

    if (is.null(filter_stats)) {
        stats <- default_stats
    } else {
        stats <- utils::modifyList(default_stats, filter_stats)
    }

    initial_n <- stats$initial_n %||% modeled_n
    model_n <- stats$model_n %||% modeled_n
    removed_n <- stats$removed_n %||% pmax(initial_n - modeled_n, 0)
    removed_pct <- stats$removed_pct
    if (is.null(removed_pct) || is.na(removed_pct)) {
        removed_pct <- if (initial_n > 0) round((initial_n - model_n) / initial_n * 100, 1) else 0
    }

    data.frame(
        dataset_name = dataset_name,
        analysis_name = analysis_name,
        initial_n = as.integer(initial_n),
        modeled_n = as.integer(model_n),
        removed_n = as.integer(removed_n),
        removed_pct = removed_pct,
        removal_reason = stats$removal_reason,
        stringsAsFactors = FALSE
    )
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
            t_value_col <- which(col_names %in% c("t value", "z value"))
            if (length(t_value_col) > 0) {
                test_stats <- as.numeric(model_summary$coefficients[available_coefs, t_value_col[1]])
                p_values_vector[available_coefs] <- 2 * stats::pnorm(abs(test_stats), lower.tail = FALSE)
                return(p_values_vector)
            }
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
    model_type <- detect_model_type(model_fit)
    outcome_values <- NULL
    if (!is.null(model_fit$model) && outcome_var %in% names(model_fit$model)) {
        outcome_values <- model_fit$model[[outcome_var]]
    }

    n_events <- NA_real_
    if (model_type == "cox" && !is.null(model_fit$y) && is.matrix(model_fit$y)) {
        n_events <- sum(model_fit$y[, 2])
    } else if (model_type == "logistic" && !is.null(outcome_values)) {
        n_events <- sum(as.numeric(outcome_values) == 1, na.rm = TRUE)
    }
    n_outcome_levels <- if (!is.null(outcome_values)) dplyr::n_distinct(stats::na.omit(outcome_values)) else NA_integer_

    data.frame(
        analysis_type = paste0("unified_", analysis_name),
        outcome = outcome_var,
        n_total = nrow(data),
        n_events = n_events,
        n_outcome_levels = n_outcome_levels,
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
    if (!is.null(model_fit) && "polr" %in% class(model_fit) && !is.null(model_fit$convergence) && model_fit$convergence != 0) {
        model_warnings <- c(model_warnings, "Model did not converge")
    }
    if (!is.null(extreme_diagnostics) && length(extreme_diagnostics) > 0) {
        model_warnings <- c(model_warnings, "Extreme estimates detected")
    }
    if (!is.null(filtered_variables) && length(filtered_variables) > 0) {
        model_warnings <- c(model_warnings, "Variables were filtered due to extreme estimates")
    }
    model_warnings_text <- if (length(model_warnings) > 0) paste(model_warnings, collapse = "; ") else "None"
    model_converged <- TRUE
    if ("glm" %in% class(model_fit)) {
        model_converged <- isTRUE(model_fit$converged)
    } else if ("polr" %in% class(model_fit) && !is.null(model_fit$convergence)) {
        model_converged <- identical(model_fit$convergence, 0L) || identical(model_fit$convergence, 0)
    }

    data.frame(
        dataset_name = dataset_name,
        model_type = class(model_fit)[1],
        effect_measure = effect_measure,
        n_coefficients = length(coefs),
        model_converged = model_converged,
        log_likelihood = suppressWarnings(as.numeric(logLik(model_fit))),
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

#' Normalize sparse-level diagnostics for workbook export
create_sparse_level_diagnostics_tab <- function(sparse_level_diagnostics = NULL) {
    if (!is.null(sparse_level_diagnostics) && is.data.frame(sparse_level_diagnostics) && nrow(sparse_level_diagnostics) > 0) {
        return(sparse_level_diagnostics)
    }

    data.frame(
        analysis_name = character(),
        variable = character(),
        level = character(),
        observed_n = integer(),
        action = character(),
        reason = character(),
        threshold = integer(),
        reference_level = character(),
        rows_removed = integer(),
        row_ids = character(),
        source = character(),
        stringsAsFactors = FALSE
    )
}

build_covariate_variation_tab <- function(removed_covariates, dataset_name, analysis_name) {
    if (is.null(removed_covariates) || length(removed_covariates) == 0) {
        return(NULL)
    }

    rows <- lapply(names(removed_covariates), function(var_name) {
        details <- removed_covariates[[var_name]]
        data.frame(
            dataset_name = dataset_name,
            analysis_name = analysis_name,
            variable = var_name,
            reason = details$reason %||% "Insufficient variation",
            retained_values = details$unique_values %||% "none",
            non_missing_n = details$non_missing_n %||% 0,
            stringsAsFactors = FALSE
        )
    })

    do.call(rbind, rows)
}

#' Create raw model output table
extract_model_event_values <- function(model_fit, model_frame, outcome_var) {
    if ("coxph" %in% class(model_fit)) {
        if (!is.null(model_fit$y)) {
            return(suppressWarnings(as.numeric(model_fit$y[, ncol(model_fit$y)])))
        }

        response <- tryCatch(stats::model.response(model_frame), error = function(e) NULL)
        if (inherits(response, "Surv")) {
            return(suppressWarnings(as.numeric(response[, ncol(response)])))
        }

        return(NULL)
    }

    if (outcome_var %in% names(model_frame)) {
        return(suppressWarnings(as.numeric(model_frame[[outcome_var]])))
    }

    response <- tryCatch(stats::model.response(model_frame), error = function(e) NULL)
    if (!is.null(response)) {
        return(suppressWarnings(as.numeric(response)))
    }

    NULL
}

build_group_outcome_summary_lookup <- function(model_fit, outcome_var) {
    model_type <- detect_model_type(model_fit)
    if (!model_type %in% c("logistic", "cox")) {
        return(list())
    }

    model_frame <- tryCatch(stats::model.frame(model_fit), error = function(e) NULL)
    if (is.null(model_frame) || nrow(model_frame) == 0) {
        return(list())
    }

    event_values <- extract_model_event_values(model_fit, model_frame, outcome_var)
    if (is.null(event_values) || length(event_values) != nrow(model_frame)) {
        return(list())
    }

    if (!("coxph" %in% class(model_fit))) {
        valid_event_values <- sort(unique(event_values[!is.na(event_values)]))
        if (length(valid_event_values) == 0 || !all(valid_event_values %in% c(0, 1))) {
            return(list())
        }
    }

    predictor_names <- names(model_frame)
    if (length(predictor_names) == 0) {
        return(list())
    }

    predictor_names <- predictor_names[-1]
    predictor_names <- setdiff(predictor_names, c("(weights)", "(offset)", "surv_obj"))

    group_lookup <- list()

    for (var_name in predictor_names) {
        var_data <- model_frame[[var_name]]
        if (!(is.factor(var_data) || is.character(var_data))) {
            next
        }

        complete_mask <- !is.na(var_data) & !is.na(event_values)
        if (!any(complete_mask)) {
            next
        }

        var_values <- as.character(var_data[complete_mask])
        event_subset <- event_values[complete_mask]
        level_order <- if (is.factor(var_data)) levels(var_data) else unique(var_values)
        present_levels <- level_order[level_order %in% unique(var_values)]

        if (length(present_levels) == 0) {
            next
        }

        level_rows <- lapply(present_levels, function(level_name) {
            level_mask <- var_values == level_name
            level_n <- sum(level_mask)
            level_events <- sum(event_subset[level_mask] == 1, na.rm = TRUE)
            level_non_events <- sum(!is.na(event_subset[level_mask])) - level_events

            data.frame(
                level = level_name,
                group_n = as.integer(level_n),
                group_events = as.integer(level_events),
                group_non_events = as.integer(level_non_events),
                group_event_rate_pct = if (level_n > 0) round(100 * level_events / level_n, 1) else NA_real_,
                stringsAsFactors = FALSE
            )
        })

        level_summary <- do.call(rbind, level_rows)
        reference_level <- present_levels[1]

        group_lookup[[var_name]] <- list(
            by_level = level_summary,
            reference_level = reference_level
        )
    }

    group_lookup
}

initialize_group_outcome_columns <- function(row_df) {
    row_df$group_n <- NA_integer_
    row_df$group_events <- NA_integer_
    row_df$group_non_events <- NA_integer_
    row_df$group_event_rate_pct <- NA_real_
    row_df$reference_level <- NA_character_
    row_df$reference_n <- NA_integer_
    row_df$reference_events <- NA_integer_
    row_df$reference_non_events <- NA_integer_
    row_df$reference_event_rate_pct <- NA_real_

    row_df
}

attach_group_outcome_summary <- function(row_df, variable_name, level_name = NA_character_, group_lookup) {
    row_df <- initialize_group_outcome_columns(row_df)

    lookup_entry <- group_lookup[[variable_name]]
    if (is.null(lookup_entry)) {
        return(row_df)
    }

    row_df$reference_level <- lookup_entry$reference_level

    reference_row <- lookup_entry$by_level[lookup_entry$by_level$level == lookup_entry$reference_level, , drop = FALSE]
    if (nrow(reference_row) == 1) {
        row_df$reference_n <- reference_row$group_n[[1]]
        row_df$reference_events <- reference_row$group_events[[1]]
        row_df$reference_non_events <- reference_row$group_non_events[[1]]
        row_df$reference_event_rate_pct <- reference_row$group_event_rate_pct[[1]]
    }

    if (!is.na(level_name) && nzchar(level_name)) {
        level_row <- lookup_entry$by_level[lookup_entry$by_level$level == level_name, , drop = FALSE]
        if (nrow(level_row) == 1) {
            row_df$group_n <- level_row$group_n[[1]]
            row_df$group_events <- level_row$group_events[[1]]
            row_df$group_non_events <- level_row$group_non_events[[1]]
            row_df$group_event_rate_pct <- level_row$group_event_rate_pct[[1]]
        }
    }

    row_df
}

create_group_detail_row <- function(variable_name, level_name, row_type_label, effect_measure, filtering_scale, group_lookup, p_value = NA_real_) {
    detail_row <- data.frame(
        variable_base = variable_name,
        variable = level_name,
        effect_measure = effect_measure,
        filtering_scale = filtering_scale,
        raw_coefficient = NA_real_,
        raw_ci_lower = NA_real_,
        raw_ci_upper = NA_real_,
        exp_estimate = NA_real_,
        exp_ci_lower = NA_real_,
        exp_ci_upper = NA_real_,
        p_value = p_value,
        row_type = row_type_label,
        inclusion_status = "Included",
        filtering_reason = "None",
        hazard_ratio = NA_real_,
        hr_ci_lower = NA_real_,
        hr_ci_upper = NA_real_,
        odds_ratio = NA_real_,
        or_ci_lower = NA_real_,
        or_ci_upper = NA_real_,
        stringsAsFactors = FALSE
    )

    detail_row <- attach_group_outcome_summary(detail_row, variable_name, level_name, group_lookup)
    detail_row$reference_level <- NA_character_
    detail_row$reference_n <- NA_integer_
    detail_row$reference_events <- NA_integer_
    detail_row$reference_non_events <- NA_integer_
    detail_row$reference_event_rate_pct <- NA_real_
    detail_row
}

create_raw_model_output_tab <- function(coefs, conf_int, p_values, factor_label_pvalue_map, effect_measure, filtering_scale, model_fit, data, outcome_var, factor_label_pvalues_tab, table_result) {
    # Use gtsummary table structure as the foundation
    gts_table_body <- table_result$table_body
    group_summary_lookup <- build_group_outcome_summary_lookup(model_fit, outcome_var)
    
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
        } else if (effect_measure == "OR" && ("glm" %in% class(model_fit) || "polr" %in% class(model_fit))) {
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

        intercept_row <- initialize_group_outcome_columns(intercept_row)

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
            factor_label_row <- initialize_group_outcome_columns(factor_label_row)

            table_rows[[current_pos]] <- factor_label_row
            current_pos <- current_pos + 1

            lookup_entry <- group_summary_lookup[[var_name]]
            if (!is.null(lookup_entry)) {
                reference_row <- create_group_detail_row(
                    variable_name = var_name,
                    level_name = lookup_entry$reference_level,
                    row_type_label = "Reference Level",
                    effect_measure = effect_measure,
                    filtering_scale = filtering_scale,
                    group_lookup = group_summary_lookup
                )

                table_rows[[current_pos]] <- reference_row
                current_pos <- current_pos + 1
            }
            
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
                    if ("label" %in% names(gts_row) && !is.na(gts_row$label) && nzchar(gts_row$label)) {
                        level_name <- as.character(gts_row$label)
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
                    } else if (effect_measure == "OR" && ("glm" %in% class(model_fit) || "polr" %in% class(model_fit))) {
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

                    coeff_row <- attach_group_outcome_summary(coeff_row, var_name, level_name, group_summary_lookup)
                    
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
        if (length(group_summary_lookup) > 0) {
            summary_rows <- lapply(names(group_summary_lookup), function(var_name) {
                factor_pvalue <- if (var_name %in% factor_label_pvalues_tab$variable) {
                    factor_label_pvalues_tab$factor_label_pvalue[factor_label_pvalues_tab$variable == var_name][1]
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
                    hazard_ratio = NA_real_,
                    hr_ci_lower = NA_real_,
                    hr_ci_upper = NA_real_,
                    odds_ratio = NA_real_,
                    or_ci_lower = NA_real_,
                    or_ci_upper = NA_real_,
                    stringsAsFactors = FALSE
                )
                factor_label_row <- initialize_group_outcome_columns(factor_label_row)

                lookup_entry <- group_summary_lookup[[var_name]]
                detail_rows <- lapply(lookup_entry$by_level$level, function(level_name) {
                    row_type_label <- if (identical(level_name, lookup_entry$reference_level)) "Reference Level" else "Group Summary"
                    create_group_detail_row(
                        variable_name = var_name,
                        level_name = level_name,
                        row_type_label = row_type_label,
                        effect_measure = effect_measure,
                        filtering_scale = filtering_scale,
                        group_lookup = group_summary_lookup
                    )
                })

                do.call(rbind, c(list(factor_label_row), detail_rows))
            })

            return(do.call(rbind, summary_rows))
        }

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
            group_n = integer(),
            group_events = integer(),
            group_non_events = integer(),
            group_event_rate_pct = numeric(),
            reference_level = character(),
            reference_n = integer(),
            reference_events = integer(),
            reference_non_events = integer(),
            reference_event_rate_pct = numeric(),
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

    na_estimate_mask <- is.na(raw_model_output_tab$raw_coefficient) & !raw_model_output_tab$row_type %in% c("Factor Label", "Reference Level", "Group Summary")
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

#' Create filtering summary table
create_filtering_summary_tab <- function(raw_model_output_tab, conf_int, predictor_vars) {
    coefficient_mask <- raw_model_output_tab$row_type == "Coefficient"
    filtered_count <- sum(raw_model_output_tab$inclusion_status == "Filtered" & coefficient_mask, na.rm = TRUE)
    remaining_count <- sum(raw_model_output_tab$inclusion_status == "Included" & coefficient_mask, na.rm = TRUE)

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
        total_coefficients = sum(coefficient_mask, na.rm = TRUE),
        extreme_estimates_removed = filtered_count,
        rows_removed = filtered_count,
        sparse_table_warning = FALSE,
        confint_error = all(is.na(conf_int)),
        remaining_coefficients = remaining_count,
        table_has_meaningful_content = remaining_count > 0,
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

calculate_ordinal_lrt_pvalue <- function(model_fit, variable_name, data, outcome_var, confounders, treatment_var = "treatment_group") {
    required_vars <- unique(c(outcome_var, variable_name, confounders, treatment_var))
    required_vars <- required_vars[required_vars %in% names(data)]

    data_clean <- data %>%
        filter(if_all(all_of(required_vars), ~ !is.na(.x)))

    if (nrow(data_clean) == 0) {
        return(NA_real_)
    }

    factor_cols <- names(data_clean)[vapply(data_clean, is.factor, logical(1))]
    if (length(factor_cols) > 0) {
        data_clean[factor_cols] <- lapply(data_clean[factor_cols], droplevels)
    }

    if (outcome_var %in% names(data_clean)) {
        data_clean[[outcome_var]] <- droplevels(data_clean[[outcome_var]])
        if (!is.ordered(data_clean[[outcome_var]])) {
            data_clean[[outcome_var]] <- ordered(data_clean[[outcome_var]], levels = levels(data_clean[[outcome_var]]))
        }
    }

    if (dplyr::n_distinct(stats::na.omit(data_clean[[outcome_var]])) < 2) {
        return(NA_real_)
    }

    full_terms <- unique(c(treatment_var, variable_name, confounders))
    if (variable_name == treatment_var) {
        reduced_terms <- confounders
    } else {
        reduced_terms <- unique(c(treatment_var, confounders))
    }

    full_formula <- if (length(full_terms) == 0) {
        as.formula(paste(outcome_var, "~ 1"))
    } else {
        as.formula(paste(outcome_var, "~", paste(full_terms, collapse = " + ")))
    }
    reduced_formula <- if (length(reduced_terms) == 0) {
        as.formula(paste(outcome_var, "~ 1"))
    } else {
        as.formula(paste(outcome_var, "~", paste(reduced_terms, collapse = " + ")))
    }

    full_model <- tryCatch(
        MASS::polr(full_formula, data = data_clean, Hess = TRUE, model = TRUE),
        error = function(e) NULL
    )
    reduced_model <- tryCatch(
        MASS::polr(reduced_formula, data = data_clean, Hess = TRUE, model = TRUE),
        error = function(e) NULL
    )

    if (is.null(full_model) || is.null(reduced_model)) {
        return(NA_real_)
    }

    ll_full <- suppressWarnings(as.numeric(logLik(full_model)))
    ll_reduced <- suppressWarnings(as.numeric(logLik(reduced_model)))
    df_full <- attr(logLik(full_model), "df")
    df_reduced <- attr(logLik(reduced_model), "df")
    chisq <- 2 * (ll_full - ll_reduced)
    df_diff <- df_full - df_reduced

    if (!is.finite(chisq) || !is.finite(df_diff) || df_diff <= 0) {
        return(NA_real_)
    }

    pchisq(chisq, df = df_diff, lower.tail = FALSE)
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
        "ordinal" = {
            calculate_ordinal_lrt_pvalue(
                model_fit = model_fit,
                variable_name = variable_name,
                data = data,
                outcome_var = outcome_var,
                confounders = var_confounders,
                treatment_var = treatment_var
            )
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
