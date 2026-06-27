# Binary Outcomes Analysis

#' Build Objective 1 reviewer-response event-support notes
#'
#' @param outcome_var Character outcome variable name.
#' @param time_var Character event-time variable name used for cumulative incidence.
#' @param event_var Character event indicator variable name.
#' @return Data frame describing descriptive and supportive event summaries.
build_objective1_binary_estimand_notes <- function(outcome_var, time_var, event_var) {
    outcome_label <- dplyr::case_when(
        identical(outcome_var, "recurrence1") ~ "local recurrence",
        identical(outcome_var, "mets_progression") ~ "metastatic progression",
        TRUE ~ outcome_var
    )

    data.frame(
        estimand = c("descriptive_ever_observed", "competing_risk_cumulative_incidence"),
        role = c("descriptive_support", "supportive_time_to_event_context"),
        endpoint = outcome_label,
        interpretation = c(
            "Ever-observed event counts over available follow-up; not a censoring-aware treatment-effect estimand.",
            "Time-horizon event probability accounting for censoring and death before the event as a competing event; used as supportive context for Cox-led inference."
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

    cr_data$group <- coerce_to_factor_preserving_levels(cr_data$group)
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
            estimand = "descriptive_ever_observed",
            estimand_role = "descriptive_support",
            notes = "Ever-observed event counts over available follow-up; adjusted Cox models are the lead reviewer-response inference."
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
                    descriptive_event_counts = rates,
                    cumulative_incidence = cumulative_incidence$summary,
                    competing_risk_support = cumulative_incidence$support,
                    estimand_notes = cumulative_incidence$notes
                ),
                path = file.path(output_dir, paste0(prefix, outcome_var, "_event_support_summary.xlsx"))
            )
        }
    }

    list(
        rates = rates,
        cumulative_incidence = cumulative_incidence,
        table = NULL,
        model = NULL,
        diagnostics = list(
            status = "not_fit",
            reason = "Reviewer-response analysis treats recurrence and metastasis as time-dependent endpoints; logistic regression is intentionally not fit."
        )
    )
}
