# Subgroup Formatting Utilities

#' Format p-values for display
#' @param p_value Numeric p-value
#' @return Character p-value string
format_p_value <- function(p_value) {
    if (is.na(p_value) || is.null(p_value)) {
        return("")
    }
    if (p_value < 0.001) {
        return("<0.001")
    }
    if (p_value < 0.01) {
        return(sprintf("%.3f", p_value))
    }
    sprintf("%.2f", p_value)
}

#' Format subgroup analysis tables (wrapper)
#' @param subgroup_results List or structured list with results
#' @param dataset_name Dataset label
#' @param subgroup_dir Output directory
#' @param prefix File prefix
#' @return Invisible NULL (writes files)
format_subgroup_analysis_tables <- function(subgroup_results, dataset_name, subgroup_dir, prefix) {
    if (is.null(subgroup_results) || length(subgroup_results) == 0) {
        warning("No subgroup results provided for formatting")
        return(invisible(NULL))
    }
    actual_subgroup_results <- if ("subgroup_results" %in% names(subgroup_results)) subgroup_results$subgroup_results else subgroup_results
    if (is.null(actual_subgroup_results) || length(actual_subgroup_results) == 0) {
        warning("No subgroup results provided for formatting")
        return(invisible(NULL))
    }
    if (!dir.exists(subgroup_dir)) dir.create(subgroup_dir, recursive = TRUE, showWarnings = FALSE)
    for (var_name in names(actual_subgroup_results)) {
        var_results <- actual_subgroup_results[[var_name]]
        if (is.null(var_results) || is.null(var_results$subgroup_effects)) next
        tryCatch(
            {
                formatted_table <- format_subgroup_analysis_results(
                    subgroup_results = setNames(list(var_results), var_name),
                    outcome_name = paste("Tumor Height Change -", dataset_name),
                    effect_measure = "MD",
                    output_path = file.path(subgroup_dir, paste0(prefix, var_name, "_subgroup_analysis.xlsx"))
                )
            },
            error = function(e) {
                warning(sprintf("Failed to format table for %s: %s", var_name, e$message))
            }
        )
    }
    invisible(NULL)
}

#' Create formatted, publication-ready subgroup results table
#'
#' This function takes a list of subgroup analysis results and formats them into a
#' publication-ready table suitable for Excel export and HTML preview. Each subgroup
#' variable is presented with a header row (variable label and interaction p-value)
#' followed by rows for each subgroup level (with sample sizes, effect estimates, CIs, and p-values).
#'
#' @param subgroup_results List of subgroup results (each element should contain $subgroup_effects and $interaction_p)
#' @param outcome_name Outcome name (for labeling, not directly used in this function)
#' @param effect_measure "HR"|"OR"|"MD" (for labeling, not directly used in this function)
#' @param output_path Path for Excel (HTML saved alongside if provided)
#' @return Data frame prepared for Excel (invisible if written to file)
format_subgroup_analysis_results <- function(subgroup_results, outcome_name, effect_measure = "HR", output_path = NULL) {
    # Check for empty or NULL input
    if (is.null(subgroup_results) || length(subgroup_results) == 0) {
        warning("No subgroup results provided for formatting")
        return(NULL)
    }

    # List to accumulate all rows for the final table
    all_table_rows <- list()
    sparse_notes <- c()

    # Iterate over each subgroup variable in the results
    for (var_name in names(subgroup_results)) {
        result <- subgroup_results[[var_name]]

        # Skip if result is missing or malformed
        if (is.null(result) || is.null(result$subgroup_effects) ||
            !is.data.frame(result$subgroup_effects) || nrow(result$subgroup_effects) == 0) {
            next
        }

        # Get display name for the variable (fall back to title-cased variable name)
        variable_display_name <- get_variable_labels()[[var_name]]
        if (is.null(variable_display_name)) {
            variable_display_name <- tools::toTitleCase(gsub("_", " ", var_name))
        }

        # Format the interaction p-value for the header row
        interaction_p_text <- if (!is.null(result$interaction_p) && !is.na(result$interaction_p)) {
            format_p_value(result$interaction_p)
        } else {
            "NA"
        }

        # Create a header row for this variable
        header_row <- data.frame(
            Subgroup.Level = variable_display_name,
            Sample.Size = "",
            Effect = "",
            CI = "",
            P.value = "",
            Interaction.P = interaction_p_text,
            is_header = TRUE,
            variable_name = var_name,
            stringsAsFactors = FALSE
        )
        all_table_rows[[length(all_table_rows) + 1]] <- header_row

        # Extract subgroup effects data frame
        se <- result$subgroup_effects
        # Required columns for a valid subgroup effect row
        req <- c("subgroup_level", "n_total", "n_plaque", "n_gksrs", "treatment_effect", "ci_lower", "ci_upper", "p_value")

        # Only proceed if all required columns are present
        if (all(req %in% names(se))) {
            if (!is.null(result$sparse_level_diagnostics) &&
                is.data.frame(result$sparse_level_diagnostics) &&
                nrow(result$sparse_level_diagnostics) > 0) {
                note_text <- paste(
                    sprintf(
                        "%s (n=%d; %s)",
                        result$sparse_level_diagnostics$level,
                        result$sparse_level_diagnostics$observed_n,
                        result$sparse_level_diagnostics$reason
                    ),
                    collapse = "; "
                )
                sparse_notes <- c(sparse_notes, sprintf(
                    "Excluded sparse levels for %s: %s",
                    variable_display_name,
                    note_text
                ))
            }
            # Iterate over each subgroup level row
            for (i in seq_len(nrow(se))) {
                rd <- se[i, ]
                # Skip rows with missing effect or CI
                if (is.na(rd$treatment_effect) || is.na(rd$ci_lower) || is.na(rd$ci_upper)) next

                # Create a data row for this subgroup level
                row <- data.frame(
                    Subgroup.Level = as.character(rd$subgroup_level),
                    Sample.Size = sprintf("%d (%d/%d)", rd$n_total, rd$n_plaque, rd$n_gksrs),
                    Effect = sprintf("%.2f", rd$treatment_effect),
                    CI = sprintf("(%.2f, %.2f)", rd$ci_lower, rd$ci_upper),
                    P.value = format_p_value(rd$p_value),
                    Interaction.P = "",
                    is_header = FALSE,
                    variable_name = var_name,
                    stringsAsFactors = FALSE
                )
                all_table_rows[[length(all_table_rows) + 1]] <- row
            }
        }
    }

    # If no valid rows were created, return NULL with a warning
    if (length(all_table_rows) == 0) {
        warning("No valid data to format")
        return(NULL)
    }

    # Combine all rows into a single data frame
    final_table <- do.call(rbind, all_table_rows)

    # Remove helper columns before export
    excel_table <- final_table %>% dplyr::select(-is_header, -variable_name)

    # If an output path is provided, save an HTML preview using gt
    if (!is.null(output_path)) {
        html_path <- gsub("\\.xlsx$", ".html", output_path)
        tryCatch(
            {
                gt_tbl <- excel_table %>%
                    gt() %>%
                    tab_header(
                        title = paste0(outcome_name, " - Subgroup Analysis (", effect_measure, ")")
                    ) %>%
                    cols_label(
                        Subgroup.Level = "Subgroup",
                        Sample.Size = "Sample Size",
                        Effect = "Effect",
                        CI = "95% CI",
                        P.value = "p-value",
                        Interaction.P = "Int p"
                    ) %>%
                    apply_publication_styling() %>%
                    # Bold variable header rows (where Sample.Size is empty and Interaction.P not empty)
                    tab_style(
                        style = cell_text(weight = "bold"),
                        locations = cells_body(rows = Sample.Size == "" & Interaction.P != "")
                    ) %>%
                    tab_source_note(gt::html(paste0(
                        as.character("CI = confidence interval"),
                        if (length(sparse_notes) > 0) paste0("<br><br>", paste(unique(sparse_notes), collapse = "<br><br>")) else ""
                    )))
                save_gt_html(gt_tbl, filename = html_path)
            },
            error = function(e) {
                warning(sprintf("Subgroup HTML generation failed for %s: %s", html_path, e$message))
                # Write a minimal diagnostic HTML so the file presence is obvious
                diagnostic <- paste0(
                    "<html><body><h3>Subgroup table generation failed</h3>",
                    "<p>", html_path, "</p>",
                    "<pre>", e$message, "</pre>",
                    "</body></html>"
                )
                try(writeLines(diagnostic, html_path), silent = TRUE)
            }
        )
    }

    # Return the formatted table (data frame)
    excel_table
}
