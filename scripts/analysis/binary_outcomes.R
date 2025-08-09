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
#' @param other_map Optional mapping for "Other" category.
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
    other_map = NULL,
    output_dirs = NULL,
    prefix = NULL) {
    # Check that there are at least two groups to compare
    if (length(unique(data[[group_var]])) < 2) {
        warning(sprintf("Only one level of %s present; skipping logistic model.", group_var))
        return(list(rates = NULL, table = NULL, model = NULL, diagnostics = NULL))
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

    # Run logistic regression and generate regression table
    result <- generate_regression_table(
        data = fix_event_data,
        outcome_var = outcome_var,
        predictor_vars = group_var,
        confounders = confounders_to_use,
        model_type = "logistic",
        effect_measure = "OR",
        analysis_name = paste0(outcome_var, "_", analysis_type, "_logistic"),
        dataset_name = dataset_name,
        output_dir = if (!is.null(output_dirs)) output_dir else "test_output",
        prefix = prefix,
        time_var = analysis_time_var,
        event_var = event_var,
        other_map = other_map,
        treatment_var = group_var
    )

    # Return a list of results: rates, regression table, model object, and diagnostics
    list(
        rates = rates,
        table = result$table,
        model = result$model,
        diagnostics = result$diagnostics
    )
}
