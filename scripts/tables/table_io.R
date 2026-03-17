# Table IO Utilities

empty_effect_summary_rows <- function() {
    data.frame(
        dataset = character(),
        analysis_label = character(),
        analysis_id = character(),
        model_label = character(),
        term = character(),
        effect_measure = character(),
        estimate = numeric(),
        ci_lower = numeric(),
        ci_upper = numeric(),
        p_value = numeric(),
        n_patients = numeric(),
        n_events = numeric(),
        n_outcome_non_missing = numeric(),
        data_source = character(),
        model_status = character(),
        notes = character(),
        stringsAsFactors = FALSE
    )
}

coerce_effect_summary_rows <- function(rows) {
    template <- empty_effect_summary_rows()
    if (is.null(rows) || !is.data.frame(rows) || nrow(rows) == 0) {
        return(template)
    }

    missing_cols <- setdiff(names(template), names(rows))
    for (col_name in missing_cols) {
        rows[[col_name]] <- template[[col_name]]
    }

    rows <- rows[, names(template), drop = FALSE]

    character_cols <- c(
        "dataset", "analysis_label", "analysis_id", "model_label", "term",
        "effect_measure", "data_source", "model_status", "notes"
    )
    numeric_cols <- c(
        "estimate", "ci_lower", "ci_upper", "p_value",
        "n_patients", "n_events", "n_outcome_non_missing"
    )

    for (col_name in character_cols) {
        rows[[col_name]] <- as.character(rows[[col_name]])
    }
    for (col_name in numeric_cols) {
        rows[[col_name]] <- suppressWarnings(as.numeric(rows[[col_name]]))
    }

    rows
}

bind_effect_summary_rows <- function(...) {
    row_sets <- list(...)
    row_sets <- Filter(Negate(is.null), row_sets)
    if (length(row_sets) == 0) {
        return(empty_effect_summary_rows())
    }

    dplyr::bind_rows(lapply(row_sets, coerce_effect_summary_rows))
}

create_effect_summary_rows <- function(dataset_name,
                                       analysis_label,
                                       model_label,
                                       term,
                                       effect_measure = NA_character_,
                                       estimate = NA_real_,
                                       ci_lower = NA_real_,
                                       ci_upper = NA_real_,
                                       p_value = NA_real_,
                                       n_patients = NA_real_,
                                       n_events = NA_real_,
                                       n_outcome_non_missing = NA_real_,
                                       data_source = NA_character_,
                                       model_status = "FIT",
                                       notes = NA_character_) {
    term <- as.character(term)
    n_rows <- length(term)

    recycle_value <- function(value, mode = c("character", "numeric")) {
        mode <- match.arg(mode)
        if (length(value) == 1) {
            value <- rep(value, n_rows)
        }
        if (mode == "character") {
            return(as.character(value))
        }
        suppressWarnings(as.numeric(value))
    }

    data.frame(
        dataset = rep(dataset_name %||% "unspecified_dataset", n_rows),
        analysis_label = rep(analysis_label, n_rows),
        analysis_id = rep(make_filename_safe(analysis_label), n_rows),
        model_label = rep(model_label, n_rows),
        term = term,
        effect_measure = recycle_value(effect_measure, "character"),
        estimate = recycle_value(estimate, "numeric"),
        ci_lower = recycle_value(ci_lower, "numeric"),
        ci_upper = recycle_value(ci_upper, "numeric"),
        p_value = recycle_value(p_value, "numeric"),
        n_patients = recycle_value(n_patients, "numeric"),
        n_events = recycle_value(n_events, "numeric"),
        n_outcome_non_missing = recycle_value(n_outcome_non_missing, "numeric"),
        data_source = recycle_value(data_source, "character"),
        model_status = recycle_value(model_status, "character"),
        notes = recycle_value(notes, "character"),
        stringsAsFactors = FALSE
    )
}

extract_effect_summary_pvalues <- function(model_summary, coefficient_names) {
    p_values <- rep(NA_real_, length(coefficient_names))
    names(p_values) <- coefficient_names

    if (is.null(model_summary) || is.null(model_summary$coefficients)) {
        return(p_values)
    }

    coeff_mat <- model_summary$coefficients
    matched_names <- intersect(coefficient_names, rownames(coeff_mat))
    if (length(matched_names) == 0) {
        return(p_values)
    }

    col_names <- colnames(coeff_mat)
    p_col <- which(col_names %in% c("Pr(>|z|)", "Pr(>|t|)", "Pr(>F)"))
    if (length(p_col) > 0) {
        p_values[matched_names] <- as.numeric(coeff_mat[matched_names, p_col[1]])
        return(p_values)
    }

    stat_col <- which(col_names %in% c("t value", "z value"))
    if (length(stat_col) > 0) {
        test_stats <- as.numeric(coeff_mat[matched_names, stat_col[1]])
        p_values[matched_names] <- 2 * stats::pnorm(abs(test_stats), lower.tail = FALSE)
    }

    p_values
}

extract_wald_ci <- function(model, coefficient_names, conf_level = 0.95) {
    ci_lower <- rep(NA_real_, length(coefficient_names))
    ci_upper <- rep(NA_real_, length(coefficient_names))
    names(ci_lower) <- coefficient_names
    names(ci_upper) <- coefficient_names

    vcov_mat <- tryCatch(stats::vcov(model), error = function(e) NULL)
    if (is.null(vcov_mat)) {
        return(list(lower = ci_lower, upper = ci_upper))
    }

    matched_names <- intersect(coefficient_names, colnames(vcov_mat))
    if (length(matched_names) == 0) {
        return(list(lower = ci_lower, upper = ci_upper))
    }

    std_errors <- sqrt(diag(vcov_mat[matched_names, matched_names, drop = FALSE]))
    z_value <- stats::qnorm(1 - (1 - conf_level) / 2)
    estimates <- stats::coef(model)[matched_names]

    ci_lower[matched_names] <- estimates - z_value * std_errors
    ci_upper[matched_names] <- estimates + z_value * std_errors

    list(lower = ci_lower, upper = ci_upper)
}

extract_model_confidence_intervals <- function(model, coefficient_names, model_type) {
    if (identical(model_type, "ordinal")) {
        return(extract_wald_ci(model, coefficient_names))
    }

    conf_int <- tryCatch(
        suppressWarnings(stats::confint(model)),
        error = function(e) {
            tryCatch(
                suppressWarnings(stats::confint.default(model)),
                error = function(e2) NULL
            )
        }
    )

    ci_lower <- rep(NA_real_, length(coefficient_names))
    ci_upper <- rep(NA_real_, length(coefficient_names))
    names(ci_lower) <- coefficient_names
    names(ci_upper) <- coefficient_names

    if (!is.null(conf_int)) {
        matched_ci_names <- intersect(coefficient_names, rownames(conf_int))
        if (length(matched_ci_names) > 0) {
            ci_lower[matched_ci_names] <- conf_int[matched_ci_names, 1]
            ci_upper[matched_ci_names] <- conf_int[matched_ci_names, 2]
        }
    }

    list(lower = ci_lower, upper = ci_upper)
}

extract_model_term_pvalues <- function(model,
                                       coefficient_names,
                                       model_type,
                                       outcome_var = NULL,
                                       group_var = "treatment_group") {
    model_summary <- tryCatch(summary(model), error = function(e) NULL)
    p_values <- extract_effect_summary_pvalues(model_summary, coefficient_names)

    if (!identical(model_type, "ordinal")) {
        return(p_values)
    }

    if (is.null(model$model) || is.null(outcome_var) || !outcome_var %in% names(model$model)) {
        return(p_values)
    }

    term_labels <- attr(terms(model), "term.labels")
    confounders <- setdiff(term_labels, group_var)
    lrt_pvalue <- calculate_factor_label_pvalue(
        model_fit = model,
        variable_name = group_var,
        data = model$model,
        outcome_var = outcome_var,
        confounders = confounders,
        treatment_var = group_var
    )

    target_rows <- grepl(paste0("^", group_var), coefficient_names)
    if (is.finite(lrt_pvalue) && any(target_rows)) {
        p_values[target_rows] <- lrt_pvalue
    }

    p_values
}

summarize_effect_model <- function(model,
                                   dataset_name,
                                   analysis_label,
                                   model_label,
                                   group_var,
                                   data_source_label,
                                   effect_measure = NULL,
                                   outcome_var = NULL,
                                   notes = NA_character_) {
    if (is.null(model)) {
        return(NULL)
    }

    model_type <- detect_model_type(model)
    if (is.null(effect_measure)) {
        effect_measure <- switch(model_type,
            cox = "HR",
            logistic = "OR",
            ordinal = "OR",
            linear = "MD",
            "Estimate"
        )
    }

    coefficient_names <- names(stats::coef(model))
    if (is.null(coefficient_names) || length(coefficient_names) == 0) {
        return(NULL)
    }

    target_rows <- grepl(paste0("^", group_var), coefficient_names)
    if (!any(target_rows)) {
        return(NULL)
    }

    coefficient_names <- coefficient_names[target_rows]
    coefficient_values <- stats::coef(model)[coefficient_names]
    model_summary <- tryCatch(summary(model), error = function(e) NULL)
    if (is.null(model_summary)) {
        return(NULL)
    }
    p_values <- extract_model_term_pvalues(
        model = model,
        coefficient_names = coefficient_names,
        model_type = model_type,
        outcome_var = outcome_var,
        group_var = group_var
    )

    n_patients <- tryCatch(as.numeric(stats::nobs(model)), error = function(e) NA_real_)
    outcome_values <- NULL
    if (!is.null(model$model) && !is.null(outcome_var) && outcome_var %in% names(model$model)) {
        outcome_values <- model$model[[outcome_var]]
    }
    n_outcome_non_missing <- if (!is.null(outcome_values)) {
        sum(!is.na(outcome_values))
    } else {
        n_patients
    }

    n_events <- NA_real_
    if (model_type == "cox" && !is.null(model_summary$nevent)) {
        n_events <- as.numeric(model_summary$nevent)
    } else if (model_type == "logistic" && !is.null(outcome_values)) {
        n_events <- sum(as.numeric(outcome_values) == 1, na.rm = TRUE)
    }

    if (model_type == "cox") {
        ci_mat <- model_summary$conf.int
        if (is.null(ci_mat)) {
            return(NULL)
        }
        return(create_effect_summary_rows(
            dataset_name = dataset_name,
            analysis_label = analysis_label,
            model_label = model_label,
            term = coefficient_names,
            effect_measure = effect_measure,
            estimate = round(ci_mat[coefficient_names, "exp(coef)"], 3),
            ci_lower = round(ci_mat[coefficient_names, "lower .95"], 3),
            ci_upper = round(ci_mat[coefficient_names, "upper .95"], 3),
            p_value = p_values[coefficient_names],
            n_patients = n_patients,
            n_events = n_events,
            n_outcome_non_missing = n_outcome_non_missing,
            data_source = data_source_label,
            model_status = "FIT",
            notes = notes
        ))
    }

    conf_int <- extract_model_confidence_intervals(model, coefficient_names, model_type)
    ci_lower <- conf_int$lower[coefficient_names]
    ci_upper <- conf_int$upper[coefficient_names]

    if (effect_measure %in% c("OR", "HR")) {
        estimate <- round(exp(coefficient_values), 3)
        ci_lower <- round(exp(ci_lower), 3)
        ci_upper <- round(exp(ci_upper), 3)
    } else {
        estimate <- round(as.numeric(coefficient_values), 3)
        ci_lower <- round(ci_lower, 3)
        ci_upper <- round(ci_upper, 3)
    }

    create_effect_summary_rows(
        dataset_name = dataset_name,
        analysis_label = analysis_label,
        model_label = model_label,
        term = coefficient_names,
        effect_measure = effect_measure,
        estimate = estimate,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        p_value = p_values[coefficient_names],
        n_patients = n_patients,
        n_events = n_events,
        n_outcome_non_missing = n_outcome_non_missing,
        data_source = data_source_label,
        model_status = "FIT",
        notes = notes
    )
}

write_effect_summary_workbook <- function(effect_summary_rows, output_dir, prefix, analysis_name) {
    rows_to_write <- coerce_effect_summary_rows(effect_summary_rows)
    if (nrow(rows_to_write) == 0) {
        return(invisible(NULL))
    }

    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    filename <- paste0(prefix, make_filename_safe(analysis_name), "_effect_summary.xlsx")
    output_path <- file.path(output_dir, filename)
    writexl::write_xlsx(rows_to_write, output_path)
    logger::log_info(sprintf("Effect summary saved to %s", output_path))

    output_path
}

#' Escape plain text for inclusion in lightweight HTML output
#'
#' @param text Character vector to escape for HTML rendering.
#'
#' @return Character vector with HTML-sensitive characters escaped.
escape_html_text <- function(text) {
    text <- as.character(text %||% "")
    text <- gsub("&", "&amp;", text, fixed = TRUE)
    text <- gsub("<", "&lt;", text, fixed = TRUE)
    text <- gsub(">", "&gt;", text, fixed = TRUE)
    text
}

#' Render a data frame as a compact HTML table
#'
#' @param data Data frame to render.
#' @param max_rows Optional integer cap on the number of rows shown.
#'
#' @return A single HTML string containing a table or an empty string.
render_simple_html_table <- function(data, max_rows = NULL) {
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
        return("")
    }

    if (!is.null(max_rows) && nrow(data) > max_rows) {
        data <- utils::head(data, max_rows)
    }

    header_cells <- paste0(
        "<tr>",
        paste0("<th>", escape_html_text(names(data)), "</th>", collapse = ""),
        "</tr>"
    )
    body_rows <- apply(data, 1, function(row_values) {
        paste0(
            "<tr>",
            paste0("<td>", escape_html_text(row_values), "</td>", collapse = ""),
            "</tr>"
        )
    })

    paste0(
        "<table border='1' cellspacing='0' cellpadding='4'>",
        header_cells,
        paste(body_rows, collapse = ""),
        "</table>"
    )
}

#' Write the model diagnostics workbook
#'
#' @param diagnostics Named list of diagnostics tables or text summaries.
#' @param diagnostics_path Character scalar path to the workbook output.
#'
#' @return Invisibly returns `NULL` after writing the workbook.
write_diagnostics_workbook <- function(diagnostics, diagnostics_path) {
    if (is.null(diagnostics)) {
        logger::log_warn("No diagnostics to save")
        return(invisible(NULL))
    }

    tryCatch(
        {
            wb <- createWorkbook()
            if (!is.null(diagnostics$model_summary)) {
                addWorksheet(wb, "Model_summary")
                writeData(wb, "Model_summary", diagnostics$model_summary)
            }
            if (!is.null(diagnostics$model_diagnostics)) {
                addWorksheet(wb, "Model_diagnostics")
                writeData(wb, "Model_diagnostics", diagnostics$model_diagnostics)
            }
            if (!is.null(diagnostics$data_characteristics)) {
                addWorksheet(wb, "Data_characteristics")
                writeData(wb, "Data_characteristics", diagnostics$data_characteristics)
            }
            if (!is.null(diagnostics$sparse_level_diagnostics)) {
                addWorksheet(wb, "Sparse_level_diagnostics")
                writeData(wb, "Sparse_level_diagnostics", diagnostics$sparse_level_diagnostics)
            }
            if (!is.null(diagnostics$raw_model_output)) {
                addWorksheet(wb, "Raw_model_output")
                if (is.data.frame(diagnostics$raw_model_output)) {
                    raw_output_formatted <- diagnostics$raw_model_output
                    if ("p_value" %in% names(raw_output_formatted)) {
                        raw_output_formatted$p_value <- as.character(raw_output_formatted$p_value)
                        raw_output_formatted$p_value[raw_output_formatted$p_value == "NA"] <- ""
                    }
                    writeData(wb, "Raw_model_output", raw_output_formatted)
                } else {
                    writeData(wb, "Raw_model_output", data.frame(
                        message = diagnostics$raw_model_output,
                        stringsAsFactors = FALSE
                    ))
                }
            }
            if (!is.null(diagnostics$filtering_summary)) {
                addWorksheet(wb, "Filtering_summary")
                writeData(wb, "Filtering_summary", diagnostics$filtering_summary)
            }
            if (!is.null(diagnostics$reference_levels)) {
                addWorksheet(wb, "Reference_Levels")
                writeData(wb, "Reference_Levels", diagnostics$reference_levels)
            }
            if (!is.null(diagnostics$sample_size_summary)) {
                addWorksheet(wb, "Sample_size_summary")
                writeData(wb, "Sample_size_summary", diagnostics$sample_size_summary)
            }
            if (!is.null(diagnostics$covariate_variation)) {
                addWorksheet(wb, "Covariate_variation")
                writeData(wb, "Covariate_variation", diagnostics$covariate_variation)
            }
            if (!is.null(diagnostics$skip_summary)) {
                addWorksheet(wb, "Skip_summary")
                writeData(wb, "Skip_summary", diagnostics$skip_summary)
            }
            if (!is.null(diagnostics$event_support)) {
                addWorksheet(wb, "Event_support")
                writeData(wb, "Event_support", diagnostics$event_support)
            }
            if (!is.null(diagnostics$narrative_summary)) {
                addWorksheet(wb, "Narrative_summary")
                writeData(wb, "Narrative_summary", diagnostics$narrative_summary)
            }
            saveWorkbook(wb, diagnostics_path, overwrite = TRUE)
            logger::log_info(sprintf("Comprehensive diagnostics saved to %s", diagnostics_path))
        },
        error = function(e) {
            logger::log_error(sprintf("Failed to save diagnostics: %s", e$message))
        }
    )
}

#' Save HTML and workbook outputs for a skipped model
#'
#' @param analysis_name Character scalar analysis identifier.
#' @param dataset_name Character scalar dataset identifier.
#' @param output_dir Character scalar output directory.
#' @param prefix Character scalar filename prefix.
#' @param reason Character scalar explaining why the model was skipped.
#' @param diagnostics Optional named list of diagnostics tables and summaries.
#'
#' @return A list with `html_path` and `diagnostics_path`.
save_skipped_model_outputs <- function(analysis_name,
                                       dataset_name,
                                       output_dir,
                                       prefix,
                                       reason,
                                       diagnostics = NULL) {
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    safe_analysis_name <- tryCatch(
        {
            make_filename_safe(analysis_name)
        },
        error = function(e) analysis_name
    )
    base_filename <- paste0(prefix, safe_analysis_name)
    html_path <- file.path(output_dir, paste0(base_filename, "_SKIPPED.html"))
    diagnostics_path <- file.path(output_dir, paste0(base_filename, "_diagnostics.xlsx"))

    narrative_block <- ""
    if (!is.null(diagnostics$narrative_summary) && nrow(diagnostics$narrative_summary) > 0) {
        narrative_items <- paste0(
            "<li>",
            escape_html_text(diagnostics$narrative_summary$detail),
            "</li>",
            collapse = ""
        )
        narrative_block <- paste0("<h3>Why The Model Was Not Fit</h3><ul>", narrative_items, "</ul>")
    }

    summary_block <- if (!is.null(diagnostics$skip_summary)) {
        paste0("<h3>Skip Summary</h3>", render_simple_html_table(diagnostics$skip_summary))
    } else {
        ""
    }

    event_support_block <- if (!is.null(diagnostics$event_support)) {
        paste0(
            "<h3>Modeled Outcome Counts By Covariate Level</h3>",
            render_simple_html_table(diagnostics$event_support)
        )
    } else {
        ""
    }

    skip_html <- paste0(
        "<html><body>",
        "<h2>Adjusted Analysis Not Fit</h2>",
        "<p><strong>Analysis:</strong> ", escape_html_text(analysis_name), "</p>",
        "<p><strong>Dataset:</strong> ", escape_html_text(dataset_name), "</p>",
        "<p><strong>Reason:</strong> ", escape_html_text(reason), "</p>",
        narrative_block,
        summary_block,
        event_support_block,
        "</body></html>"
    )
    writeLines(skip_html, html_path)
    logger::log_info(sprintf("Skipped-model HTML saved to %s", html_path))

    write_diagnostics_workbook(diagnostics, diagnostics_path)

    list(
        html_path = html_path,
        diagnostics_path = diagnostics_path
    )
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
#' @param treatment_var Name of the treatment variable in the model (default: "treatment_group")
#' @return List of output file paths
save_table_outputs <- function(table_result, raw_output, model_fit, analysis_name,
                               dataset_name, output_dir, prefix, diagnostics = NULL, data = NULL, outcome_var = NULL, confounders = NULL, treatment_var = "treatment_group") {
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Generate file names
    # Normalize analysis_name for filesystem safety and consistency
    safe_analysis_name <- tryCatch(
        {
            make_filename_safe(analysis_name)
        },
        error = function(e) analysis_name
    )
    base_filename <- paste0(prefix, safe_analysis_name)
    html_filename <- paste0(base_filename, "_", tolower(class(model_fit)[1]), ".html")
    diagnostics_filename <- paste0(base_filename, "_diagnostics.xlsx")

    # Save HTML table (only if table_result is not NULL)
    html_path <- file.path(output_dir, html_filename)
    diagnostics_path <- file.path(output_dir, diagnostics_filename)
    cat("DEBUG: generate_regression_table - HTML table generation\n")
    cat("  Table result is NULL:", is.null(table_result), "\n")
    cat("  HTML path:", html_path, "\n")
    cat("  Diagnostics path:", diagnostics_path, "\n")
    logger::log_info(sprintf(
        "DEBUG: save_table_outputs setup - analysis_name='%s' (safe='%s'), base_filename='%s', html_path='%s', diagnostics_path='%s'",
        analysis_name, safe_analysis_name, base_filename, html_path, diagnostics_path
    ))

    if (!is.null(table_result)) {
        cat("DEBUG: Table result is not NULL, proceeding with modification\n")

        # Check if the table has meaningful content before proceeding
        table_has_content <- FALSE
        table_has_content_source <- "unknown"
        if (!is.null(diagnostics) && !is.null(diagnostics$filtering_summary)) {
            table_has_content <- diagnostics$filtering_summary$table_has_meaningful_content
            table_has_content_source <- "diagnostics.filtering_summary"
        } else {
            table_data <- table_result$table_body
            if (!is.null(table_data) && nrow(table_data) > 0) {
                table_has_content <- any(!is.na(suppressWarnings(as.numeric(table_data$estimate))))
                table_has_content_source <- "table_body.estimates_fallback"
            }
        }

        # Track row counts at each step to identify where content is lost
        initial_rows <- nrow(table_result$table_body)
        logger::log_info(sprintf(
            "DEBUG: Pre-save checks - initial_rows=%d, table_has_content=%s (source=%s)",
            initial_rows, as.character(table_has_content), table_has_content_source
        ))

        if (!table_has_content) {
            logger::log_warn("Skipping HTML table generation - no meaningful content due to extreme estimates or model issues")
            diagnostic_html_path <- file.path(output_dir, paste0(base_filename, "_NO_CONTENT_DIAGNOSTIC.html"))
            diagnostic_content <- paste0(
                "<html><body>",
                "<h2>Table Generation Skipped</h2>",
                "<p><strong>Analysis:</strong> ", analysis_name, "</p>",
                "<p><strong>Dataset:</strong> ", dataset_name, "</p>",
                "<p><strong>Reason:</strong> No meaningful content available due to extreme estimates or model convergence issues</p>",
                "<p><strong>Recommendation:</strong> Check the diagnostics Excel file for detailed information about why coefficients were filtered out.</p>",
                "</body></html>"
            )
            writeLines(diagnostic_content, diagnostic_html_path)
            logger::log_info(sprintf("Diagnostic HTML file saved to %s", diagnostic_html_path))
        } else {
            tryCatch(
                {
                    cat("DEBUG: About to call modify_gt_table_pvalues\n")
                    cat("  Table class:", class(table_result), "\n")
                    cat("  Outcome var:", outcome_var, "\n")
                    cat("  Confounders:", paste(confounders, collapse = ", "), "\n")

                    factor_label_map <- NULL
                    if (!is.null(diagnostics) && !is.null(diagnostics$raw_model_output)) {
                        try({
                            raw_output_df <- diagnostics$raw_model_output
                            if (is.data.frame(raw_output_df) && "row_type" %in% names(raw_output_df)) {
                                factor_rows <- raw_output_df %>%
                                    dplyr::filter(row_type == "Factor Label", !is.na(p_value)) %>%
                                    dplyr::select(variable_base, p_value)
                                if (nrow(factor_rows) > 0) {
                                    factor_label_map <- setNames(factor_rows$p_value, factor_rows$variable_base)
                                }
                            }
                        }, silent = TRUE)
                    }

                    modified_table <- modify_gt_table_pvalues(
                        table_result %>% as_gt(),
                        table_result,
                        data,
                        outcome_var,
                        confounders,
                        model_fit,
                        treatment_var = treatment_var,
                        factor_label_pvalue_map = factor_label_map
                    )

                    cat("DEBUG: After modify_gt_table_pvalues\n")
                    cat("  Modified table class:", class(modified_table), "\n")

                    # Track row counts after modification
                    modified_rows <- nrow(modified_table$table_body)
                    logger::log_info(sprintf("DEBUG: Modified table has %d rows", modified_rows))

                    gt_table <- modified_table %>% as_gt()

                    sample_note <- build_sample_size_source_note(diagnostics$sample_size_summary)
                    if (!is.null(sample_note)) {
                        gt_table <- gt_table %>% gt::tab_source_note(gt::md(sample_note))
                    }

                    # Track row counts after gt conversion
                    gt_rows <- nrow(gt_table$table_body)
                    logger::log_info(sprintf("DEBUG: GT table has %d rows", gt_rows))

                    gt_table <- gt_table %>% gtsave(html_path)

                    if (!is.null(diagnostics) && !is.null(diagnostics$filtering_summary)) {
                        main_predictor_filtered <- diagnostics$filtering_summary$main_predictor_filtered
                        if (main_predictor_filtered) {
                            warning_text <- "⚠️ WARNING: Main predictor variable was filtered out due to perfect separation or extreme estimates"
                            html_content <- readLines(html_path)
                            footnote_pattern <- '<tfoot class="gt_sourcenotes">'
                            footnote_index <- grep(footnote_pattern, html_content)
                            if (length(footnote_index) > 0) {
                                tfoot_end_pattern <- "</tfoot>"
                                tfoot_end_index <- grep(tfoot_end_pattern, html_content)
                                tfoot_end_index <- tfoot_end_index[tfoot_end_index > footnote_index[1]]
                                if (length(tfoot_end_index) > 0) {
                                    warning_html <- sprintf('    <tr>\n      <td class="gt_sourcenote" colspan="4"><span class=\'gt_from_md\'>%s</span></td>\n    </tr>', warning_text)
                                    html_content <- c(
                                        html_content[1:(tfoot_end_index[1] - 1)],
                                        warning_html,
                                        html_content[tfoot_end_index[1]:length(html_content)]
                                    )
                                    writeLines(html_content, html_path)
                                }
                            }
                        }
                    }
                    logger::log_info(sprintf("HTML table saved to %s", html_path))
                },
                error = function(e) {
                    error_msg <- if (is.list(e) && !is.null(e$message)) e$message else as.character(e)
                    cat("DEBUG: Error in HTML table generation:", error_msg, "\n")
                    logger::log_error(sprintf("Failed to save HTML table: %s", error_msg))
                }
            )
        }
    } else {
        cat("DEBUG: Table result is NULL, skipping HTML generation\n")
        logger::log_info("No HTML table to save - model fitting failed")
    }

    # diagnostics_path computed above
    write_diagnostics_workbook(diagnostics, diagnostics_path)

    return(list(
        html_path = html_path,
        diagnostics_path = diagnostics_path
    ))
}

build_sample_size_source_note <- function(sample_size_summary) {
    if (is.null(sample_size_summary) || !is.data.frame(sample_size_summary) || nrow(sample_size_summary) == 0) {
        return(NULL)
    }

    row <- sample_size_summary[1, , drop = FALSE]
    initial_n <- row$initial_n
    modeled_n <- row$modeled_n
    removed_n <- row$removed_n
    removed_pct <- row$removed_pct
    reason <- row$removal_reason %||% "Pre-model exclusions"

    if (is.na(initial_n) || is.na(modeled_n) || is.na(removed_n)) {
        return(NULL)
    }

    if (removed_n == 0) {
        return(sprintf("Sample size audit: %d participants entered the model; no rows were excluded prior to fitting.", modeled_n))
    }

    pct_text <- if (!is.null(removed_pct) && !is.na(removed_pct)) sprintf("%.1f%%", removed_pct) else "n/a"
    sprintf(
        "Sample size audit: %d provided, %d modeled (%d removed; %s, %s).",
        as.integer(initial_n),
        as.integer(modeled_n),
        as.integer(removed_n),
        pct_text,
        reason
    )
}
