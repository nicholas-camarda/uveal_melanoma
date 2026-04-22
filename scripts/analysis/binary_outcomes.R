# Binary Outcomes Analysis

#' Build Objective 1 co-primary estimand notes for binary endpoints
#'
#' @param outcome_var Character outcome variable name.
#' @param time_var Character event-time variable name used for cumulative incidence.
#' @param event_var Character event indicator variable name.
#' @return Data frame describing the binary and cumulative-incidence estimands.
build_objective1_binary_estimand_notes <- function(outcome_var, time_var, event_var) {
    outcome_label <- dplyr::case_when(
        identical(outcome_var, "recurrence1") ~ "local recurrence",
        identical(outcome_var, "mets_progression") ~ "metastatic progression",
        TRUE ~ outcome_var
    )

    data.frame(
        estimand = c("binary_ever_observed", "competing_risk_cumulative_incidence"),
        role = c("co-primary", "co-primary"),
        endpoint = outcome_label,
        interpretation = c(
            "Ever-observed event/rate comparison over available follow-up; not a censoring-aware fixed-horizon probability.",
            "Time-horizon event probability accounting for censoring and death before the event as a competing event."
        ),
        time_variable = c(NA_character_, time_var),
        event_variable = c(event_var, event_var),
        death_handling = c(
            "Deaths are reflected only through available follow-up for the binary ever-observed endpoint.",
            "Death before the event of interest is coded as a competing event."
        ),
        stringsAsFactors = FALSE
    )
}

#' Prepare Objective 1 competing-risk status data
#'
#' @param data Data frame containing event, follow-up, and treatment variables.
#' @param time_var Character event-time variable for the endpoint of interest.
#' @param event_var Character event indicator variable for the endpoint of interest.
#' @param group_var Character treatment/grouping variable.
#' @param death_time_var Character death-time variable.
#' @param death_event_var Character death event indicator variable.
#' @return Data frame with `.cr_time`, `.cr_status`, and group columns.
prepare_competing_risk_data <- function(data,
                                        time_var,
                                        event_var,
                                        group_var = "treatment_group",
                                        death_time_var = "tt_death_months",
                                        death_event_var = "death_event") {
    required_cols <- c(time_var, event_var, group_var, death_time_var, death_event_var)
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop(sprintf(
            "Competing-risk data cannot be prepared; missing columns: %s",
            paste(missing_cols, collapse = ", ")
        ))
    }

    event_indicator <- coerce_binary_outcome_vector(data[[event_var]])
    death_indicator <- coerce_binary_outcome_vector(data[[death_event_var]])
    event_time <- suppressWarnings(as.numeric(data[[time_var]]))
    death_time <- suppressWarnings(as.numeric(data[[death_time_var]]))
    has_event <- !is.na(event_indicator) & event_indicator == 1 & !is.na(event_time)
    has_competing_death <- !is.na(death_indicator) & death_indicator == 1 &
        !is.na(death_time) & (!has_event | death_time < event_time)

    cr_status <- dplyr::case_when(
        has_competing_death ~ 2L,
        has_event ~ 1L,
        TRUE ~ 0L
    )
    cr_time <- dplyr::case_when(
        has_competing_death ~ death_time,
        TRUE ~ event_time
    )

    data.frame(
        group = data[[group_var]],
        .cr_time = cr_time,
        .cr_status = cr_status,
        stringsAsFactors = FALSE
    ) %>%
        dplyr::filter(!is.na(.data$group), !is.na(.data$.cr_time), .data$.cr_time >= 0)
}

#' Estimate Objective 1 cumulative incidence by treatment group
#'
#' @param data Data frame containing the Objective 1 binary endpoint.
#' @param outcome_var Character outcome variable name.
#' @param time_var Character event-time variable name.
#' @param event_var Character event indicator variable name.
#' @param group_var Character grouping variable.
#' @param time_horizons_years Numeric vector of horizons in years.
#' @return List containing cumulative-incidence summary, support, and notes tables.
estimate_objective1_cumulative_incidence <- function(data,
                                                     outcome_var,
                                                     time_var,
                                                     event_var,
                                                     group_var = "treatment_group",
                                                     time_horizons_years = SURVIVAL_SUMMARY_TIMEPOINTS_YEARS) {
    notes <- build_objective1_binary_estimand_notes(outcome_var, time_var, event_var)
    cr_data <- tryCatch(
        prepare_competing_risk_data(
            data = data,
            time_var = time_var,
            event_var = event_var,
            group_var = group_var
        ),
        error = function(e) {
            return(NULL)
        }
    )

    if (is.null(cr_data) || nrow(cr_data) == 0) {
        skipped <- data.frame(
            endpoint = outcome_var,
            group = NA_character_,
            horizon_years = time_horizons_years,
            horizon_months = time_horizons_years * 12,
            cumulative_incidence_percent = NA_real_,
            ci_lower_percent = NA_real_,
            ci_upper_percent = NA_real_,
            gray_test_global_curve_p_value = NA_real_,
            status = "skipped",
            notes = "Cumulative incidence was skipped because no usable event-time rows were available.",
            stringsAsFactors = FALSE
        )
        return(list(summary = skipped, support = data.frame(), notes = notes))
    }

    cr_data$group <- as.factor(cr_data$group)
    support <- cr_data %>%
        dplyr::group_by(.data$group) %>%
        dplyr::summarise(
            n = dplyr::n(),
            events_of_interest = sum(.data$.cr_status == 1, na.rm = TRUE),
            competing_deaths = sum(.data$.cr_status == 2, na.rm = TRUE),
            censored = sum(.data$.cr_status == 0, na.rm = TRUE),
            median_follow_up_months = stats::median(.data$.cr_time, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        dplyr::rename(!!group_var := group)

    if (length(unique(cr_data$group)) < 2) {
        skipped <- tidyr::expand_grid(
            group = as.character(unique(cr_data$group)),
            horizon_years = time_horizons_years
        ) %>%
            dplyr::mutate(
                endpoint = outcome_var,
                horizon_months = .data$horizon_years * 12,
                cumulative_incidence_percent = NA_real_,
                ci_lower_percent = NA_real_,
                ci_upper_percent = NA_real_,
                gray_test_global_curve_p_value = NA_real_,
                status = "skipped",
                notes = "Cumulative incidence comparison was skipped because fewer than two treatment groups were available."
            ) %>%
            dplyr::select("endpoint", "group", "horizon_years", "horizon_months", dplyr::everything())
        return(list(summary = skipped, support = support, notes = notes))
    }

    ci_fit <- tryCatch(
        cmprsk::cuminc(
            ftime = cr_data$.cr_time,
            fstatus = cr_data$.cr_status,
            group = cr_data$group,
            cencode = 0
        ),
        error = function(e) NULL
    )

    gray_p <- NA_real_
    if (!is.null(ci_fit) && !is.null(ci_fit$Tests)) {
        tests <- as.data.frame(ci_fit$Tests)
        if ("pv" %in% names(tests) && nrow(tests) >= 1) {
            gray_p <- suppressWarnings(as.numeric(tests$pv[[1]]))
        }
    }

    rows <- list()
    for (group_name in levels(cr_data$group)) {
        component_name <- paste(group_name, "1")
        component <- ci_fit[[component_name]] %||% NULL
        for (horizon_years in time_horizons_years) {
            horizon_months <- horizon_years * 12
            if (is.null(component) || length(component$time) == 0) {
                estimate <- 0
                variance <- NA_real_
            } else {
                idx <- max(which(component$time <= horizon_months), na.rm = TRUE)
                if (!is.finite(idx)) {
                    estimate <- 0
                    variance <- 0
                } else {
                    estimate <- component$est[[idx]]
                    variance <- component$var[[idx]] %||% NA_real_
                }
            }
            se <- if (!is.na(variance) && variance >= 0) sqrt(variance) else NA_real_
            ci_lower <- if (!is.na(se)) max(0, estimate - stats::qnorm(0.975) * se) else NA_real_
            ci_upper <- if (!is.na(se)) min(1, estimate + stats::qnorm(0.975) * se) else NA_real_
            rows[[length(rows) + 1]] <- data.frame(
                endpoint = outcome_var,
                group = group_name,
                horizon_years = horizon_years,
                horizon_months = horizon_months,
                cumulative_incidence_percent = round(100 * estimate, 1),
                ci_lower_percent = round(100 * ci_lower, 1),
                ci_upper_percent = round(100 * ci_upper, 1),
                gray_test_global_curve_p_value = gray_p,
                status = "completed",
                notes = paste(
                    "Death before the endpoint is treated as a competing event;",
                    "estimates are cumulative incidence probabilities.",
                    "Gray test p-value is one global across-group curve comparison, not a per-horizon p-value."
                ),
                stringsAsFactors = FALSE
            )
        }
    }

    list(
        summary = dplyr::bind_rows(rows),
        support = support,
        notes = notes
    )
}

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
    data <- normalize_treatment_group_data(data)
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
        ) %>%
        dplyr::mutate(
            estimand = "binary_ever_observed",
            estimand_role = "co-primary",
            notes = "Ever-observed event/rate comparison over available follow-up; not a censoring-aware fixed-horizon probability."
        )

    cumulative_incidence <- estimate_objective1_cumulative_incidence(
        data = fix_event_data,
        outcome_var = outcome_var,
        time_var = time_var,
        event_var = event_var,
        group_var = group_var
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
            write_readable_xlsx(
                list(
                    binary_rates = rates,
                    cumulative_incidence = cumulative_incidence$summary,
                    competing_risk_support = cumulative_incidence$support,
                    estimand_notes = cumulative_incidence$notes
                ),
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
        cumulative_incidence = cumulative_incidence,
        table = result$table,
        model = result$model,
        diagnostics = result$diagnostics
    )
}
