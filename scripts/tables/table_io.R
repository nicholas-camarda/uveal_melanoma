# Table IO Utilities

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
    if (!is.null(diagnostics)) {
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
                if (!is.null(diagnostics$other_level_details)) {
                    addWorksheet(wb, "Other_level_details")
                    writeData(wb, "Other_level_details", diagnostics$other_level_details)
                }
                if (!is.null(diagnostics$excluded_rows)) {
                    addWorksheet(wb, "Excluded_Rows")
                    writeData(wb, "Excluded_Rows", diagnostics$excluded_rows)
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
                saveWorkbook(wb, diagnostics_path, overwrite = TRUE)
                logger::log_info(sprintf("Comprehensive diagnostics saved to %s", diagnostics_path))
            },
            error = function(e) {
                logger::log_error(sprintf("Failed to save diagnostics: %s", e$message))
            }
        )
    } else {
        logger::log_warn("No diagnostics to save")
    }

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
