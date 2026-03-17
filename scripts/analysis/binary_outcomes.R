# Binary Outcomes Analysis

#' This function performs logistic regression for binary outcomes, computes event rates by group,
#' and outputs results and diagnostics. It supports both post-treatment and all-patient analyses,
#' and writes summary tables to Excel if output directories are provided.
#'
#' @param data Data frame containing the analysis data.
#' @param outcome_var Name of the binary outcome variable (string).
#' @param time_var Name of the time-to-event variable (string).
#' @param event_var Name of the event indicator variable (string).
#' @param group_var Name of the grouping variable (default: "treatment_group").
#' @param confounders Character vector of confounder variable names.
#' @param analysis_type Type of analysis: "post_treatment_only" or "all_patients".
#' @param dataset_name Optional label for the dataset.
#' @param output_dirs List of output directories by analysis type.
#' @param prefix File prefix for output files.
#' @return List with rates, regression table, model object, and diagnostics.
analyze_binary_outcome_rates <- function(
    data,
    outcome_var,
    time_var,
    event_var,
    group_var = "treatment_group",
    confounders = NULL,
    analysis_type = "post_treatment_only",
    dataset_name = NULL,
    output_dirs = NULL,
    prefix = NULL) {
    # Check that there are at least two groups to compare
    if (length(unique(data[[group_var]])) < 2) {
        warning(sprintf("Only one level of %s present; skipping logistic model.", group_var))
        early_output_dir <- if (!is.null(output_dirs)) {
            if (outcome_var == "recurrence1") {
                output_dirs$obj1_recurrence
            } else if (outcome_var == "mets_progression") {
                output_dirs$obj1_mets
            } else {
                "test_output"
            }
        } else {
            "test_output"
        }
        early_skip_diagnostics <- build_skip_report_diagnostics(
            status = "skipped",
            analysis_name = paste0(outcome_var, "_", analysis_type, "_logistic"),
            dataset_name = dataset_name %||% "unspecified_dataset",
            reason = sprintf(
                "Logistic regression was skipped because only one `%s` level was present in the analysis dataset.",
                group_var
            ),
            narrative_lines = c(
                sprintf(
                    "The incoming analysis dataset contains only one observed `%s` level.",
                    group_var
                ),
                "A logistic regression comparison requires at least two groups."
            ),
            skip_summary = build_skip_summary_tab(list(
                modeled_n = nrow(data),
                distinct_groups_remaining = length(unique(stats::na.omit(data[[group_var]])))
            )),
            event_support = build_level_support_tab(data, group_var, outcome_var = event_var),
            raw_model_output = sprintf(
                "Model skipped: only one level of %s present.",
                group_var
            )
        )
        save_skipped_model_outputs(
            analysis_name = paste0(outcome_var, "_", analysis_type, "_logistic"),
            dataset_name = dataset_name %||% "unspecified_dataset",
            output_dir = early_output_dir %||% "test_output",
            prefix = prefix %||% "",
            reason = early_skip_diagnostics$reason,
            diagnostics = early_skip_diagnostics
        )
        return(list(rates = NULL, table = NULL, model = NULL, diagnostics = early_skip_diagnostics))
    }

    # Subset data based on analysis type
    if (analysis_type == "post_treatment_only") {
        analysis_time_var <- time_var
        # Only include patients with non-negative time (i.e., post-treatment)
        fix_event_data <- data %>% dplyr::filter(!!sym(time_var) >= 0)
    } else if (analysis_type == "all_patients") {
        analysis_time_var <- time_var
        # Include all patients regardless of time
        fix_event_data <- data
    } else {
        stop(sprintf("Invalid analysis_type: %s", analysis_type))
    }

    # Ensure all factor variables are unordered for modeling consistency
    fix_event_data <- enforce_unordered_factors(fix_event_data)

    # Select confounders that exist in the data and have more than one unique value
    confounders_to_use <- confounders[
        sapply(confounders, function(c) c %in% names(fix_event_data) && length(unique(fix_event_data[[c]])) > 1)
    ]

    # Calculate event rates by group
    rates <- fix_event_data %>%
        dplyr::group_by(!!sym(group_var)) %>%
        dplyr::summarise(
            n = dplyr::n(),
            events = sum(!!sym(event_var), na.rm = TRUE),
            rate = events / n * 100,
            .groups = "drop"
        )

    output_dir <- NULL

    # Write rates summary to Excel if output directory is provided
    if (!is.null(output_dirs)) {
        output_dir <- if (outcome_var == "recurrence1") {
            output_dirs$obj1_recurrence
        } else if (outcome_var == "mets_progression") {
            output_dirs$obj1_mets
        } else {
            NULL
        }
        if (!is.null(output_dir)) {
            writexl::write_xlsx(
                rates,
                path = file.path(output_dir, paste0(prefix, outcome_var, "_rates_summary.xlsx"))
            )
        }
    }

    model_variables <- unique(c(group_var, confounders_to_use))
    analysis_label <- paste0(outcome_var, "_", analysis_type, "_logistic")
    exclusion_result <- apply_sparse_level_exclusions(
        data = fix_event_data,
        variables = model_variables[model_variables %in% names(fix_event_data)],
        analysis_name = analysis_label,
        id_col = pick_sparse_level_id_col(fix_event_data),
        level_exclusions = MODELING_LEVEL_EXCLUSIONS
    )

    if (exclusion_result$removed_row_count > 0) {
        logger::log_info(formatted(sprintf(
            "Excluded %d rows with sparse categorical levels prior to logistic regression (%s)",
            exclusion_result$removed_row_count,
            paste(model_variables, collapse = ", ")
        ), indent = 1))
    }

    model_data <- exclusion_result$data

    if (nrow(model_data) == 0 || length(unique(stats::na.omit(model_data[[group_var]]))) < 2) {
        logger::log_warn(formatted(
            "Insufficient data available after sparse-level exclusions; skipping logistic regression.",
            indent = 1
        ))
        sample_size_summary <- build_sample_size_summary_tab(
            filter_stats = exclusion_result$filter_stats,
            dataset_name = dataset_name,
            analysis_name = analysis_label,
            modeled_n = nrow(model_data)
        )
        support_variables <- unique(c(group_var, confounders_to_use))
        diagnostics_stub <- build_skip_report_diagnostics(
            status = "skipped",
            analysis_name = analysis_label,
            dataset_name = dataset_name %||% "unspecified_dataset",
            reason = "Logistic regression was skipped because the post-exclusion dataset did not retain enough usable rows or group variation.",
            narrative_lines = c(
                sprintf(
                    "After sparse-level exclusions, %d patients remained in the modeled dataset.",
                    nrow(model_data)
                ),
                sprintf(
                    "Adjusted logistic regression requires at least two non-missing `%s` groups after exclusions.",
                    group_var
                )
            ),
            sample_size_summary = sample_size_summary,
            skip_summary = build_skip_summary_tab(list(
                modeled_n = nrow(model_data),
                distinct_groups_remaining = length(unique(stats::na.omit(model_data[[group_var]]))),
                sparse_exclusion_reason = exclusion_result$filter_stats$removal_reason %||% "Sparse-level exclusions"
            )),
            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
            event_support = build_level_support_tab(model_data, support_variables, outcome_var = event_var),
            raw_model_output = "Model skipped: insufficient data after sparse-level exclusions."
        )
        save_skipped_model_outputs(
            analysis_name = analysis_label,
            dataset_name = dataset_name %||% "unspecified_dataset",
            output_dir = output_dir %||% "test_output",
            prefix = prefix %||% "",
            reason = diagnostics_stub$reason,
            diagnostics = diagnostics_stub
        )
        return(list(rates = rates, table = NULL, model = NULL, diagnostics = diagnostics_stub))
    }

    # Run logistic regression and generate regression table
    result <- generate_regression_table(
        data = model_data,
        outcome_var = outcome_var,
        predictor_vars = group_var,
        confounders = confounders_to_use,
        model_type = "logistic",
        effect_measure = "OR",
        analysis_name = analysis_label,
        dataset_name = dataset_name,
        output_dir = if (!is.null(output_dirs)) output_dir else "test_output",
        prefix = prefix,
        time_var = analysis_time_var,
        event_var = event_var,
        treatment_var = group_var,
        sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
        filter_stats = exclusion_result$filter_stats
    )

    # Return a list of results: rates, regression table, model object, and diagnostics
    list(
        rates = rates,
        table = result$table,
        model = result$model,
        diagnostics = result$diagnostics
    )
}
