# Table Formatting Utilities

#' Create gtsummary table with proper filtering and model summary
#'
#' @param model_fit Fitted model object
#' @param effect_measure Character string for effect measure type
#' @param analysis_name Character string for analysis name
#' @param data Data frame used for the model (for interaction p-value calculation)
#' @param outcome_var Name of the outcome variable
#' @param confounders Character vector of confounders
#' @param outcome_type Type of outcome ("binary", "survival", "continuous"). If NULL, will be detected from model_fit
#' @param show_interaction_pvalues Logical, whether to show interaction p-values
#' @param sparse_level_diagnostics Data frame with details about excluded sparse levels (optional)
#' @return gtsummary table object
create_gtsummary_table <- function(model_fit, effect_measure, analysis_name,
                                   data = NULL, outcome_var = NULL, confounders = NULL,
                                   outcome_type = NULL, sparse_level_diagnostics = NULL) {
    # Determine model type for caption
    model_type <- detect_model_type(model_fit)

    # Determine outcome type from model if not provided
    if (is.null(outcome_type)) {
        model_type <- detect_model_type(model_fit)
        outcome_type <- model_type_to_outcome_type(model_type)
        logger::log_info(sprintf("Detected outcome type '%s' from model type '%s'", outcome_type, model_type))
    }

    # Get all variable labels and filter to only include variables in the model
    all_variable_labels <- get_variable_labels()
    model_terms <- attr(terms(model_fit), "term.labels")
    model_var_names <- unique(c("treatment_group", model_terms))
    variable_labels <- all_variable_labels[intersect(names(all_variable_labels), model_var_names)]

    # Create the complete table first
    table <- tryCatch(
        {
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
                modify_caption(build_professional_caption(model_type, effect_measure, analysis_name)) %>%
                modify_post_fmt_fun(
                    fmt_fun = ~ format_confidence_intervals_post(.),
                    columns = "conf.low"
                )
        },
        error = function(e) {
            logger::log_error(sprintf("Error creating gtsummary table: %s", e$message))
            # Return a simple table with just the model summary
            model_fit %>%
                tbl_regression(
                    exponentiate = (effect_measure %in% c("OR", "HR")),
                    label = variable_labels
                ) %>%
                bold_labels() %>%
                italicize_levels() %>%
                modify_caption(build_professional_caption(model_type, effect_measure, analysis_name))
        }
    )

    if (detect_model_type(model_fit) == "ordinal") {
        table <- apply_ordinal_inference_to_table(
            table = table,
            model_fit = model_fit,
            outcome_var = outcome_var
        )
    }

    # Apply extreme estimate filtering to the table
    table_data <- table$table_body

    # NOTE: Extreme estimate filtering is now handled by process_extreme_estimates
    # This eliminates redundant filtering and simplifies the pipeline

    # Remove variables that now only have reference levels (no coefficients)
    table_data_updated <- table$table_body
    variables_to_remove <- c()

    for (var in unique(table_data_updated$variable)) {
        var_rows <- table_data_updated[table_data_updated$variable == var, ]

        # Check if this is a continuous variable (has no "level" rows, only label rows)
        level_rows <- var_rows[var_rows$row_type == "level", ]
        non_level_rows <- var_rows[var_rows$row_type != "level", ] # Includes "label" and "coefficient" rows

        # For continuous variables (no level rows), keep them if they have any non-level rows
        if (nrow(level_rows) == 0 && nrow(non_level_rows) > 0) {
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
                is.finite(as.numeric(level_rows$conf.high)),
        ]

        if (nrow(valid_level_rows) == 0) {
            variables_to_remove <- c(variables_to_remove, var)
            logger::log_info(sprintf(
                "Removing variable '%s' - no valid levels remain after filtering (total_levels = %d, valid_levels = %d)",
                var, nrow(level_rows), nrow(valid_level_rows)
            ))
        }
    }

    if (length(variables_to_remove) > 0) {
        table$table_body <- table_data_updated[!table_data_updated$variable %in% variables_to_remove, ]
    }

    table <- add_sparse_level_details(table, sparse_level_diagnostics = sparse_level_diagnostics)

    return(table)
}

apply_ordinal_inference_to_table <- function(table, model_fit, outcome_var) {
    coefficient_names <- names(stats::coef(model_fit))
    if (is.null(coefficient_names) || length(coefficient_names) == 0) {
        return(table)
    }

    wald_ci <- extract_wald_ci(model_fit, coefficient_names)
    term_pvalues <- extract_model_term_pvalues(
        model = model_fit,
        coefficient_names = coefficient_names,
        model_type = "ordinal",
        outcome_var = outcome_var,
        group_var = "treatment_group"
    )

    table_body <- table$table_body
    if (!"p.value" %in% names(table_body)) {
        table_body$p.value <- rep(NA_real_, nrow(table_body))
    }
    if ("p.value_fmt" %in% names(table_body)) {
        table_body$p.value_fmt <- as.character(table_body$p.value_fmt)
    }

    treatment_coefficients <- coefficient_names[grepl("^treatment_group", coefficient_names)]
    treatment_rows <- which(table_body$variable == "treatment_group" & table_body$row_type != "label")

    if (length(treatment_coefficients) == 0 || length(treatment_rows) == 0) {
        table$table_body <- table_body
        return(table)
    }

    clean_label <- function(x) {
        x <- trimws(gsub("\\u00a0", " ", x, fixed = TRUE))
        x <- trimws(gsub("^\\s+", "", x))
        x
    }

    treatment_labels <- clean_label(table_body$label[treatment_rows])

    for (coefficient_name in treatment_coefficients) {
        level_name <- sub("^treatment_group", "", coefficient_name)
        row_idx <- treatment_rows[match(level_name, treatment_labels)]

        if (length(row_idx) == 0 || is.na(row_idx)) {
            next
        }

        row_pvalue <- term_pvalues[[coefficient_name]]
        row_ci_low <- exp(wald_ci$lower[[coefficient_name]])
        row_ci_high <- exp(wald_ci$upper[[coefficient_name]])

        table_body$estimate[row_idx] <- exp(stats::coef(model_fit)[[coefficient_name]])
        table_body$conf.low[row_idx] <- row_ci_low
        table_body$conf.high[row_idx] <- row_ci_high
        table_body[["p.value"]][row_idx] <- row_pvalue

        if ("estimate_fmt" %in% names(table_body)) {
            table_body$estimate_fmt[row_idx] <- style_sigfig(table_body$estimate[row_idx])
        }
        if ("conf.low_fmt" %in% names(table_body)) {
            table_body$conf.low_fmt[row_idx] <- style_sigfig(row_ci_low)
        }
        if ("conf.high_fmt" %in% names(table_body)) {
            table_body$conf.high_fmt[row_idx] <- style_sigfig(row_ci_high)
        }
        if ("p.value_fmt" %in% names(table_body)) {
            table_body[["p.value_fmt"]][row_idx] <- gtsummary::style_pvalue(row_pvalue)
        }
    }

    table$table_body <- table_body
    table
}

#' Modify p-values in gt table directly after as_gt conversion
#'
#' @param gt_table gt table object
#' @param table_result Original gtsummary table object
#' @param data Data frame used for the model
#' @param outcome_var Name of the outcome variable
#' @param confounders Character vector of confounders
#' @param treatment_var Name of the treatment variable in the model (default: "treatment_group")
#' @return Modified gt table object
modify_gt_table_pvalues <- function(gt_table, table_result, data, outcome_var, confounders, model_fit = NULL, treatment_var = "treatment_group", factor_label_pvalue_map = NULL) {
    # Get the original table data to understand the structure
    table_data <- table_result$table_body

    # Get unique variables (including treatment_group for testing overall significance)
    all_variables <- unique(table_data$variable)
    variables <- all_variables

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
            if (!is.null(factor_label_pvalue_map) && var_name %in% names(factor_label_pvalue_map)) {
                pval <- factor_label_pvalue_map[[var_name]]
            } else {
                pval <- calculate_factor_label_pvalue(model_fit, var_name, data, outcome_var, filtered_confounders, treatment_var = treatment_var)
            }
                factor_label_pvalues[[var_name]] <- pval
        }
    } else {
        # Fallback to old approach for backward compatibility
        for (var_name in variables) {
            var_confounders <- filtered_confounders[filtered_confounders != var_name]
            pval <- calculate_variable_overall_significance(data, var_name, outcome_var,
                treatment_var = treatment_var,
                confounders = var_confounders,
                outcome_type = model_type_to_outcome_type(detect_model_type(model_fit))
            )
            factor_label_pvalues[[var_name]] <- pval
        }
    }

    # Modify the gtsummary table
    modified_table <- table_result
    if (!"p.value" %in% names(modified_table$table_body)) {
        modified_table$table_body$p.value <- NA_real_
    }

    for (var_name in all_variables) {
        pval <- factor_label_pvalues[[var_name]]
        var_rows <- which(table_data$variable == var_name)
        if (length(var_rows) > 0) {
            # Determine if this is a single-predictor model (no confounders/other predictors)
            is_single_predictor_model <- FALSE
            if (!is.null(model_fit)) {
                term_labels <- attr(terms(model_fit), "term.labels")
                is_single_predictor_model <- length(term_labels) == 1
            }
            # In multi-variable models, always hide p-values for all non-label rows
            if (!is_single_predictor_model) {
                non_label_rows <- var_rows[table_data$row_type[var_rows] != "label"]
                if (length(non_label_rows) > 0) {
                    modified_table$table_body$p.value[non_label_rows] <- NA_real_
                    if ("p.value_fmt" %in% names(modified_table$table_body)) {
                        modified_table$table_body$p.value_fmt[non_label_rows] <- ""
                    }
                }
            } else if (!is.na(pval) && detect_model_type(model_fit) == "ordinal") {
                non_label_rows <- var_rows[table_data$row_type[var_rows] != "label"]
                if (length(non_label_rows) > 0) {
                    modified_table$table_body$p.value[non_label_rows] <- pval
                    if ("p.value_fmt" %in% names(modified_table$table_body)) {
                        modified_table$table_body$p.value_fmt[non_label_rows] <- gtsummary::style_pvalue(pval)
                    }
                }
            }
            # Set overall p-value at the label row when available
            if (!is.na(pval)) {
                label_row <- var_rows[table_data$row_type[var_rows] == "label"][1]
                if (!is.na(label_row)) {
                    modified_table$table_body$p.value[label_row] <- pval
                    if ("p.value_fmt" %in% names(modified_table$table_body)) {
                        modified_table$table_body$p.value_fmt[label_row] <- gtsummary::style_pvalue(pval)
                    }
                }
            } else {
                label_row <- var_rows[table_data$row_type[var_rows] == "label"][1]
                if (!is.na(label_row)) {
                    modified_table$table_body$p.value[label_row] <- NA_real_
                    if ("p.value_fmt" %in% names(modified_table$table_body)) {
                        modified_table$table_body$p.value_fmt[label_row] <- ""
                    }
                }
            }
        }
    }

    modified_table <- modified_table %>%
        modify_table_styling(
            columns = "p.value",
            hide = FALSE,
            label = "**p-value**",
            fmt_fun = function(x) gtsummary::style_pvalue(x)
        )

    return(modified_table)
}

#' Format confidence intervals to (X,X) format for post-processing
#'
#' @param x Vector of confidence interval values to format
#' @return Vector of formatted confidence interval strings
format_confidence_intervals_post <- function(x) {
    sapply(x, function(val) {
        if (is.na(val) || val == "" || grepl("^\\(", val)) {
            return(val)
        }
        if (grepl(",", val)) {
            parts <- strsplit(val, ",")[[1]]
            if (length(parts) == 2) {
                lower <- trimws(parts[1])
                upper <- trimws(parts[2])
                return(paste0("(", lower, ", ", upper, ")"))
            }
        }
        return(val)
    })
}

#' Add sparse-level exclusion details to table source note
#'
#' @param table A gtsummary table object
#' @param sparse_level_diagnostics Data frame returned by sparse-level model prep
#' @return Modified table with source note containing "Other" category details
add_sparse_level_details <- function(table, sparse_level_diagnostics = NULL) {
    if (!is.null(sparse_level_diagnostics) && is.data.frame(sparse_level_diagnostics) && nrow(sparse_level_diagnostics) > 0) {
        total_unique_rows_removed <- length(unique(unlist(strsplit(
            paste(stats::na.omit(sparse_level_diagnostics$row_ids), collapse = ","),
            ",",
            fixed = TRUE
        ))))

        n_variables <- dplyr::n_distinct(stats::na.omit(sparse_level_diagnostics$variable))
        n_levels <- nrow(sparse_level_diagnostics)
        row_note <- if (total_unique_rows_removed > 0) {
            sprintf("%d rows removed", total_unique_rows_removed)
        } else {
            "row removal count available in diagnostics"
        }

        source_note_parts <- c()
        existing_source_note <- table$source_note
        if (!is.null(existing_source_note) && existing_source_note != "") {
            source_note_parts <- c(source_note_parts, existing_source_note)
        }
        sparse_note <- sprintf(
            "Note: Sparse factor levels were excluded before modeling (%d levels across %d variable%s; %s). See diagnostics workbook for details.",
            n_levels,
            n_variables,
            ifelse(n_variables == 1, "", "s"),
            row_note
        )
        source_note_parts <- c(source_note_parts, sparse_note)
        final_source_note <- paste(source_note_parts, collapse = "\n")
        table <- table %>% modify_source_note(final_source_note)
    }
    return(table)
}


#' Calculates overall variable significance p-values using likelihood ratio tests
#' and places them at the factor label level while clearing factor-level p-values.
#'
#' @param table gtsummary table object
#' @param data Data frame used for the model
#' @param outcome_var Name of the outcome variable
#' @param confounders Character vector of confounders
#' @param outcome_type Type of outcome ("binary", "survival", or "continuous"). If NULL, will be detected from model_fit
#' @param treatment_var Name of the treatment variable in the model (default: "treatment_group")
#' @param model_fit Fitted model object (optional)
#' @return Modified gtsummary table object
add_factor_label_pvalues_to_table <- function(
    table,
    data,
    outcome_var,
    confounders = NULL,
    outcome_type = NULL,
    treatment_var = "treatment_group",
    model_fit = NULL) {
    # Log the start of the function for debugging
    logger::log_info("Starting add_factor_label_pvalues_to_table")

    # Extract the table body for manipulation
    table_data <- table$table_body

    # Get all unique variables present in the table
    all_variables <- unique(table_data$variable)
    variables <- all_variables

    # Filter confounders to only those present in the table variables
    if (!is.null(confounders)) {
        filtered_confounders <- confounders[confounders %in% all_variables]
    } else {
        filtered_confounders <- NULL
    }

    # Log which variables will be tested for overall significance
    logger::log_info(
        sprintf("Variables to test for overall significance: %s", paste(variables, collapse = ", "))
    )

    # Initialize a list to store p-values for each variable
    factor_label_pvalues <- list()

    # If outcome_type is not provided, try to detect it from the model_fit if available
    if (is.null(outcome_type) && !is.null(model_fit)) {
        model_type <- detect_model_type(model_fit)
        outcome_type <- model_type_to_outcome_type(model_type)
        logger::log_info(
            sprintf("Detected outcome type '%s' from model type '%s'", outcome_type, model_type)
        )
    } else if (is.null(outcome_type)) {
        # Default to "binary" if outcome_type is still NULL
        outcome_type <- "binary"
        logger::log_info("No model provided, using default outcome type 'binary'")
    }

    # Loop through each variable to calculate the overall p-value
    for (var_name in variables) {
        # Exclude the current variable from the confounders for this test
        var_confounders <- filtered_confounders[filtered_confounders != var_name]

        # Calculate the p-value using the model_fit if available, otherwise use the data directly
        if (!is.null(model_fit)) {
            pval <- calculate_factor_label_pvalue(
                model_fit, var_name, data, outcome_var, var_confounders,
                reatment_var = treatment_var
            )
        } else {
            pval <- calculate_variable_overall_significance(
                data, var_name, outcome_var,
                treatment_var = treatment_var,
                confounders = var_confounders,
                outcome_type = outcome_type
            )
        }
        # Store the p-value for this variable
        factor_label_pvalues[[var_name]] <- pval
    }

    # Update the table: clear level p-values and set overall p-value at the label row
    table_data <- table$table_body
    for (var_name in all_variables) {
        pval <- factor_label_pvalues[[var_name]]
        var_rows <- which(table_data$variable == var_name)
        if (length(var_rows) > 0) {
            # Determine if this is a single-predictor model (no confounders/other predictors)
            is_single_predictor_model <- FALSE
            if (!is.null(model_fit)) {
                term_labels <- attr(terms(model_fit), "term.labels")
                is_single_predictor_model <- length(term_labels) == 1
            }
            # In multi-variable models, always hide p-values for all non-label rows
            if (!is_single_predictor_model) {
                non_label_rows <- var_rows[table_data$row_type[var_rows] != "label"]
                if (length(non_label_rows) > 0) {
                    table_data$p.value[non_label_rows] <- NA
                }
            }
            # Set overall p-value at the label row when available
            if (!is.na(pval)) {
                label_row <- var_rows[table_data$row_type[var_rows] == "label"][1]
                if (!is.na(label_row)) {
                    table_data$p.value[label_row] <- pval
                }
            }
        }
    }

    # Assign the modified table body back to the table object
    table$table_body <- table_data
    return(table)
}

#' Build a professional caption for regression tables
#' @param model_type Detected model type (e.g., "linear", "logistic", "cox")
#' @param effect_measure Effect measure string (e.g., "MD", "OR", "HR")
#' @param analysis_name Internal analysis name used in file naming
#' @return A human-friendly caption string
build_professional_caption <- function(model_type, effect_measure, analysis_name) {
    # Map model type to readable label
    model_label <- switch(model_type,
        linear = "Linear model",
        logistic = "Logistic regression",
        cox = "Cox proportional hazards model",
        ordinal = "Ordinal logistic regression",
        other_glm = "Generalized linear model",
        "Regression model"
    )
    # Keep captions concise; effect type is shown in the Estimate column header
    effect_label <- NULL

    explicit_labels <- c(
        logmar_vision_change_adjusted = "Adjusted LogMAR Vision Change",
        snellen_line_change_adjusted = "Adjusted Snellen Line Change",
        snellen_line_change_distribution_adjusted = "Adjusted Snellen Line Change Distribution"
    )
    if (analysis_name %in% names(explicit_labels)) {
        return(paste0(model_label, " - ", explicit_labels[[analysis_name]]))
    }

    # Derive a friendly analysis label
    friendly <- analysis_name
    friendly <- gsub("_cox$|_logistic$|_ordinal$", "", friendly)
    # If contains underscores, try to map first token via STANDARD_TABLE_LABELS
    if (grepl("_", friendly)) {
        parts <- strsplit(friendly, "_")[[1]]
        primary <- parts[1]
        remainder <- if (length(parts) > 1) paste(parts[-1], collapse = " ") else ""
        # Known mappings
        primary_label <- STANDARD_TABLE_LABELS[[primary]]
        if (is.null(primary_label)) {
            primary_label <- switch(primary,
                height_change = "Tumor Height Change",
                vision_change = "Vision Change",
                primary
            )
            # Title case if it wasn't mapped
            if (primary_label == primary) primary_label <- tools::toTitleCase(gsub("_", " ", primary))
        }
        remainder <- gsub("post treatment only", "post-treatment only", remainder, ignore.case = TRUE)
        remainder <- gsub("primary", "Primary analysis", remainder, ignore.case = TRUE)
        remainder <- gsub("sensitivity", "Sensitivity analysis", remainder, ignore.case = TRUE)
        remainder <- trimws(remainder)
        analysis_label <- if (nzchar(remainder)) paste(primary_label, "-", tools::toTitleCase(remainder)) else primary_label
    } else {
        analysis_label <- friendly
    }
    paste0(model_label, " - ", analysis_label)
}
