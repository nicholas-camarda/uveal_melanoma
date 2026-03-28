# GEP Extrapolation Assumption Checks

#' Get Objective 4 extrapolation endpoint settings
#'
#' Return the endpoint-specific time and event variables used to assess whether
#' the 7-year and 10-year exponential extrapolations are compatible with the
#' observed data.
#'
#' @param outcome_type Character scalar equal to `"MFS"` or `"MSS"`.
#' @return Named list containing endpoint-specific configuration values.
get_gep_extrapolation_endpoint_config <- function(outcome_type) {
    normalized_outcome <- toupper(outcome_type %||% "")

    if (identical(normalized_outcome, "MFS")) {
        return(list(
            outcome_type = "MFS",
            time_var = "tt_mets_months",
            event_var = "mets_event",
            time_unit = "months",
            event_label = "metastasis",
            title = "Metastasis-Free Survival",
            file_stub = "mfs"
        ))
    }

    if (identical(normalized_outcome, "MSS")) {
        return(list(
            outcome_type = "MSS",
            time_var = "tt_death_months",
            event_var = "melanoma_death_event",
            time_unit = "months",
            event_label = "melanoma-specific death",
            title = "Melanoma-Specific Survival",
            file_stub = "mss"
        ))
    }

    stop(sprintf("Unsupported outcome_type for extrapolation checks: %s", outcome_type))
}

#' Fit an intercept-only exponential survival model
#'
#' Fit an exponential model to the observed follow-up data and return the
#' implied constant hazard estimate.
#'
#' @param analysis_data Data frame containing the endpoint-specific time and
#'   event variables.
#' @param time_var Character name of the follow-up time variable in months.
#' @param event_var Character name of the event indicator variable.
#' @return Named list containing the fitted model, hazard estimate, confidence
#'   interval, and AIC.
fit_gep_exponential_model <- function(analysis_data, time_var, event_var) {
    model_formula <- stats::as.formula(sprintf("Surv(%s, %s) ~ 1", time_var, event_var))
    fitted_model <- survival::survreg(model_formula, data = analysis_data, dist = "exponential")

    intercept_estimate <- as.numeric(stats::coef(fitted_model)[[1]])
    intercept_se <- sqrt(diag(stats::vcov(fitted_model)))[[1]]
    log_hazard_estimate <- -intercept_estimate
    hazard_estimate <- exp(log_hazard_estimate)
    ci_bounds <- exp(log_hazard_estimate + c(-1, 1) * stats::qnorm(0.975) * intercept_se)

    list(
        model = fitted_model,
        hazard_per_month = hazard_estimate,
        hazard_per_year = hazard_estimate * 12,
        hazard_ci_lower_per_year = ci_bounds[[1]] * 12,
        hazard_ci_upper_per_year = ci_bounds[[2]] * 12,
        aic = stats::AIC(fitted_model)
    )
}

#' Fit an intercept-only Weibull survival model
#'
#' Fit a Weibull model and summarize the implied shape parameter, which equals 1
#' under a constant hazard assumption.
#'
#' @param analysis_data Data frame containing the endpoint-specific time and
#'   event variables.
#' @param time_var Character name of the follow-up time variable in months.
#' @param event_var Character name of the event indicator variable.
#' @return Named list containing the fitted model, Weibull shape estimate,
#'   confidence interval, and AIC.
fit_gep_weibull_model <- function(analysis_data, time_var, event_var) {
    model_formula <- stats::as.formula(sprintf("Surv(%s, %s) ~ 1", time_var, event_var))
    fitted_model <- survival::survreg(model_formula, data = analysis_data, dist = "weibull")

    log_scale_index <- which(rownames(stats::vcov(fitted_model)) == "Log(scale)")
    if (length(log_scale_index) != 1) {
        stop("Unable to identify Log(scale) variance term for Weibull model")
    }

    log_scale_estimate <- log(fitted_model$scale)
    log_scale_se <- sqrt(stats::vcov(fitted_model)[log_scale_index, log_scale_index])
    log_shape_estimate <- -log_scale_estimate
    shape_estimate <- exp(log_shape_estimate)
    shape_ci_bounds <- exp(log_shape_estimate + c(-1, 1) * stats::qnorm(0.975) * log_scale_se)

    list(
        model = fitted_model,
        shape = shape_estimate,
        shape_ci_lower = shape_ci_bounds[[1]],
        shape_ci_upper = shape_ci_bounds[[2]],
        aic = stats::AIC(fitted_model)
    )
}

#' Summarize pre- and post-5-year piecewise hazards
#'
#' Compute crude piecewise hazard estimates before and after 5 years to detect
#' obvious departures from a constant-hazard pattern.
#'
#' @param analysis_data Data frame containing the endpoint-specific time and
#'   event variables.
#' @param time_var Character name of the follow-up time variable in months.
#' @param event_var Character name of the event indicator variable.
#' @param split_months Numeric split point in months. Defaults to 60.
#' @return Named list containing piecewise person-time, event counts, hazards,
#'   and a post/pre hazard ratio when estimable.
calculate_gep_piecewise_hazard_summary <- function(analysis_data,
                                                   time_var,
                                                   event_var,
                                                   split_months = 60) {
    follow_up_months <- analysis_data[[time_var]]
    event_indicator <- analysis_data[[event_var]]

    pre_time_months <- pmin(follow_up_months, split_months)
    post_time_months <- pmax(follow_up_months - split_months, 0)

    pre_events <- sum(event_indicator == 1 & follow_up_months <= split_months, na.rm = TRUE)
    post_events <- sum(event_indicator == 1 & follow_up_months > split_months, na.rm = TRUE)

    pre_time_years <- sum(pre_time_months, na.rm = TRUE) / 12
    post_time_years <- sum(post_time_months, na.rm = TRUE) / 12

    pre_hazard <- if (is.finite(pre_time_years) && pre_time_years > 0) pre_events / pre_time_years else NA_real_
    post_hazard <- if (is.finite(post_time_years) && post_time_years > 0) post_events / post_time_years else NA_real_
    hazard_ratio <- if (is.finite(pre_hazard) && pre_hazard > 0 && is.finite(post_hazard)) post_hazard / pre_hazard else NA_real_

    list(
        split_months = split_months,
        pre_events = pre_events,
        post_events = post_events,
        pre_time_years = pre_time_years,
        post_time_years = post_time_years,
        pre_hazard_per_year = pre_hazard,
        post_hazard_per_year = post_hazard,
        post_vs_pre_hazard_ratio = hazard_ratio
    )
}

#' Build cumulative hazard diagnostic data
#'
#' Create a compact data frame for visual inspection of cumulative hazard
#' linearity over time using the Kaplan-Meier survival estimate.
#'
#' @param analysis_data Data frame containing the endpoint-specific time and
#'   event variables.
#' @param time_var Character name of the follow-up time variable in months.
#' @param event_var Character name of the event indicator variable.
#' @return Data frame with time in years and cumulative hazard values.
build_gep_cumulative_hazard_diagnostic_data <- function(analysis_data, time_var, event_var) {
    km_formula <- stats::as.formula(sprintf("Surv(%s, %s) ~ 1", time_var, event_var))
    km_fit <- survival::survfit(km_formula, data = analysis_data)

    plot_data <- data.frame(
        time_years = km_fit$time / 12,
        survival = km_fit$surv,
        stringsAsFactors = FALSE
    )
    plot_data <- plot_data[is.finite(plot_data$time_years) & plot_data$time_years >= 0, , drop = FALSE]
    plot_data$cumulative_hazard <- -log(pmax(plot_data$survival, 1e-08))
    plot_data <- plot_data[is.finite(plot_data$cumulative_hazard), , drop = FALSE]

    plot_data
}

#' Format extrapolation metric values for narrative output
#'
#' Convert numeric diagnostic values into concise strings used in support notes
#' and text summaries.
#'
#' @param value Numeric value to format.
#' @param digits Integer number of decimal places.
#' @return Character scalar containing a formatted number or `NA`.
format_gep_extrapolation_value <- function(value, digits = 2) {
    if (!is.finite(value)) {
        return("NA")
    }

    sprintf(paste0("%.", digits, "f"), value)
}

#' Prepare endpoint data for extrapolation assumption checks
#'
#' Remove rows that cannot inform a continuous-time parametric fit and apply a
#' documented small positive offset only when an event is recorded at time zero.
#'
#' @param analysis_data Data frame containing the endpoint-specific time and
#'   event variables.
#' @param time_var Character name of the follow-up time variable in months.
#' @param event_var Character name of the event indicator variable.
#' @return Named list containing the cleaned data and a plain-language note about
#'   any zero-time handling that was required.
prepare_gep_extrapolation_analysis_data <- function(analysis_data, time_var, event_var) {
    cleaned_data <- analysis_data %>%
        dplyr::filter(
            !is.na(.data[[time_var]]),
            !is.na(.data[[event_var]]),
            .data[[time_var]] >= 0
        )

    zero_time_event_count <- sum(cleaned_data[[time_var]] == 0 & cleaned_data[[event_var]] == 1, na.rm = TRUE)
    zero_time_censored_count <- sum(cleaned_data[[time_var]] == 0 & cleaned_data[[event_var]] == 0, na.rm = TRUE)

    handling_notes <- character()

    if (zero_time_censored_count > 0) {
        cleaned_data <- cleaned_data %>%
            dplyr::filter(!( .data[[time_var]] == 0 & .data[[event_var]] == 0))
        handling_notes <- c(
            handling_notes,
            sprintf(
                "Dropped %d zero-time censored row%s from the parametric extrapolation check because they provide no positive follow-up information for a continuous-time fit.",
                zero_time_censored_count,
                ifelse(zero_time_censored_count == 1, "", "s")
            )
        )
    }

    if (zero_time_event_count > 0) {
        positive_times <- cleaned_data[[time_var]][cleaned_data[[time_var]] > 0]
        epsilon_months <- if (length(positive_times) > 0) {
            max(min(positive_times, na.rm = TRUE) / 2, 1e-04)
        } else {
            1 / 30
        }
        cleaned_data[[time_var]] <- ifelse(
            cleaned_data[[time_var]] == 0 & cleaned_data[[event_var]] == 1,
            epsilon_months,
            cleaned_data[[time_var]]
        )
        handling_notes <- c(
            handling_notes,
            sprintf(
                "Shifted %d zero-time event row%s to %.4f months so the continuous-time parametric fit could proceed under a minimal positive-time convention.",
                zero_time_event_count,
                ifelse(zero_time_event_count == 1, "", "s"),
                epsilon_months
            )
        )
    }

    list(
        data = cleaned_data,
        note = if (length(handling_notes) > 0) paste(handling_notes, collapse = " ") else NA_character_,
        zero_time_censored_count = zero_time_censored_count,
        zero_time_event_count = zero_time_event_count
    )
}

#' Classify Objective 4 extrapolation support
#'
#' Convert the exponential-versus-Weibull comparison and the crude piecewise
#' hazard summary into an interpretive support tier for the exponential
#' extrapolation.
#'
#' @param event_count Integer number of endpoint events in the analyzable data.
#' @param exponential_fit Named list returned by `fit_gep_exponential_model()`.
#' @param weibull_fit Named list returned by `fit_gep_weibull_model()`.
#' @param piecewise_summary Named list returned by
#'   `calculate_gep_piecewise_hazard_summary()`.
#' @return Named list containing the support status, a support note, and the
#'   diagnostic flags used for classification.
classify_gep_extrapolation_support <- function(event_count,
                                               exponential_fit,
                                               weibull_fit,
                                               piecewise_summary) {
    delta_aic <- weibull_fit$aic - exponential_fit$aic
    shape_ci_contains_one <- is.finite(weibull_fit$shape_ci_lower) &&
        is.finite(weibull_fit$shape_ci_upper) &&
        weibull_fit$shape_ci_lower <= 1 &&
        weibull_fit$shape_ci_upper >= 1

    hazard_ratio <- piecewise_summary$post_vs_pre_hazard_ratio
    enough_piecewise_information <- piecewise_summary$post_time_years > 0 &&
        (piecewise_summary$pre_events + piecewise_summary$post_events) >= 10
    piecewise_clear_break <- enough_piecewise_information &&
        is.finite(hazard_ratio) &&
        (hazard_ratio < (2 / 3) || hazard_ratio > 1.5)

    shape_note <- sprintf(
        "Weibull shape %.2f (95%% CI %.2f to %.2f)",
        weibull_fit$shape,
        weibull_fit$shape_ci_lower,
        weibull_fit$shape_ci_upper
    )
    aic_note <- sprintf(
        "Delta AIC (Weibull - exponential) = %.2f",
        delta_aic
    )
    piecewise_note <- sprintf(
        "pre-5-year hazard %.3f/year vs post-5-year hazard %.3f/year (ratio %.2f)",
        piecewise_summary$pre_hazard_per_year,
        piecewise_summary$post_hazard_per_year,
        hazard_ratio
    )

    if (!is.finite(event_count) || event_count < 10) {
        return(list(
            status = "Unsupported",
            note = sprintf(
                "Fewer than 10 events were available (%d observed events), so the constant-hazard assumption could not be meaningfully interrogated.",
                event_count
            ),
            weibull_shape_supports_constant = FALSE,
            aic_supports_constant = FALSE,
            piecewise_supports_constant = FALSE
        ))
    }

    if (!shape_ci_contains_one || delta_aic <= -2 || piecewise_clear_break) {
        failure_reasons <- c()
        if (!shape_ci_contains_one) {
            failure_reasons <- c(failure_reasons, sprintf("%s does not stay comfortably centered on 1", shape_note))
        }
        if (delta_aic <= -2) {
            failure_reasons <- c(failure_reasons, sprintf("%s favors Weibull over exponential", aic_note))
        }
        if (piecewise_clear_break) {
            failure_reasons <- c(failure_reasons, sprintf("%s indicates materially lower late hazard", piecewise_note))
        }
        return(list(
            status = "Unsupported",
            note = sprintf(
                "The exponential extrapolation is not well supported by the observed data because %s.",
                paste(failure_reasons, collapse = "; ")
            ),
            weibull_shape_supports_constant = shape_ci_contains_one,
            aic_supports_constant = delta_aic > -2,
            piecewise_supports_constant = !piecewise_clear_break
        ))
    }

    if (delta_aic < 2 || !enough_piecewise_information) {
        limitation_reasons <- c(piecewise_note)
        if (delta_aic < 2) {
            limitation_reasons <- c(
                limitation_reasons,
                sprintf("%s is only borderline in favor of exponential", aic_note)
            )
        }
        if (!enough_piecewise_information) {
            limitation_reasons <- c(limitation_reasons, "post-5-year information is limited")
        }
        return(list(
            status = "Weakly Supported",
            note = sprintf(
                "The observed data did not clearly contradict constant hazard: %s. %s. Support is therefore only weak.",
                shape_note,
                paste(limitation_reasons, collapse = "; ")
            ),
            weibull_shape_supports_constant = shape_ci_contains_one,
            aic_supports_constant = delta_aic >= 0,
            piecewise_supports_constant = !piecewise_clear_break
        ))
    }

    list(
        status = "Supported",
        note = sprintf(
            "The exponential extrapolation is reasonably supported: %s, %s, and %s show no major departure from constant hazard.",
            shape_note,
            aic_note,
            piecewise_note
        ),
        weibull_shape_supports_constant = TRUE,
        aic_supports_constant = TRUE,
        piecewise_supports_constant = TRUE
    )
}

#' Create extrapolation support metadata for a timepoint
#'
#' Generate the reporting fields used to label imported versus extrapolated
#' horizons in consolidated Objective 4 summary tables.
#'
#' @param timepoint_label Character timepoint label such as `"5yr"` or `"7yr"`.
#' @param extrapolation_assessment Named list returned by
#'   `evaluate_gep_extrapolation_assumption()`, or `NULL`.
#' @return Named list of summary-table metadata fields.
create_gep_extrapolation_metadata <- function(timepoint_label, extrapolation_assessment = NULL) {
    normalized_timepoint <- gsub("\\s+", "", as.character(timepoint_label %||% ""))

    if (identical(normalized_timepoint, "5yr")) {
        return(list(
            Prediction_Source = "Imported",
            Extrapolation_Assumption = "Not Applicable",
            Assumption_Support_Status = "Not Applicable",
            Assumption_Support_Notes = "The 5-year value is used directly from the imported GEP prediction."
        ))
    }

    support_status <- extrapolation_assessment$status %||% NA_character_
    support_note <- extrapolation_assessment$note %||% NA_character_

    list(
        Prediction_Source = "Extrapolated from imported 5-year value",
        Extrapolation_Assumption = "Exponential constant hazard",
        Assumption_Support_Status = support_status,
        Assumption_Support_Notes = support_note
    )
}

#' Evaluate Objective 4 extrapolation support for one endpoint
#'
#' Run the focused Objective 4 assumption checks needed to judge whether the
#' 7-year and 10-year exponential extrapolations are reasonably supported by the
#' observed endpoint data for the current cohort.
#'
#' @param analysis_data Data frame restricted to the analyzable endpoint subset.
#' @param outcome_type Character scalar equal to `"MFS"` or `"MSS"`.
#' @param output_dir Optional directory path for saving the diagnostic plot and
#'   text summary.
#' @param prefix Character filename prefix for saved artifacts.
#' @param dataset_name Optional character dataset label for reporting.
#' @return Named list containing model summaries, support status, and workbook-
#'   ready summary data.
evaluate_gep_extrapolation_assumption <- function(analysis_data,
                                                  outcome_type,
                                                  output_dir = NULL,
                                                  prefix = "",
                                                  dataset_name = NULL) {
    endpoint_config <- get_gep_extrapolation_endpoint_config(outcome_type)

    prepared_data <- prepare_gep_extrapolation_analysis_data(
        analysis_data = analysis_data,
        time_var = endpoint_config$time_var,
        event_var = endpoint_config$event_var
    )
    cleaned_data <- prepared_data$data

    event_count <- sum(cleaned_data[[endpoint_config$event_var]] == 1, na.rm = TRUE)
    follow_up_beyond_5yr <- sum(cleaned_data[[endpoint_config$time_var]] > 60, na.rm = TRUE)

    if (nrow(cleaned_data) == 0) {
        empty_summary <- data.frame(
            Outcome = endpoint_config$outcome_type,
            Dataset = dataset_name %||% NA_character_,
            N = 0,
            Events = 0,
            Followup_Beyond_5yr_n = 0,
            Exponential_Hazard_Per_Year = NA_real_,
            Exponential_Hazard_CI_Lower = NA_real_,
            Exponential_Hazard_CI_Upper = NA_real_,
            Weibull_Shape = NA_real_,
            Weibull_Shape_CI_Lower = NA_real_,
            Weibull_Shape_CI_Upper = NA_real_,
            Exponential_AIC = NA_real_,
            Weibull_AIC = NA_real_,
            Delta_AIC_Weibull_minus_Exponential = NA_real_,
            Pre5yr_Hazard_Per_Year = NA_real_,
            Post5yr_Hazard_Per_Year = NA_real_,
            Post_vs_Pre_Hazard_Ratio = NA_real_,
            Support_Status = "Unsupported",
            Support_Note = "No analyzable data were available for the extrapolation check.",
            stringsAsFactors = FALSE
        )

        return(list(
            outcome_type = endpoint_config$outcome_type,
            status = "Unsupported",
            note = "No analyzable data were available for the extrapolation check.",
            summary_table = empty_summary,
            plot_data = data.frame()
        ))
    }

    model_results <- tryCatch(
        {
            exponential_fit <- fit_gep_exponential_model(cleaned_data, endpoint_config$time_var, endpoint_config$event_var)
            weibull_fit <- fit_gep_weibull_model(cleaned_data, endpoint_config$time_var, endpoint_config$event_var)
            piecewise_summary <- calculate_gep_piecewise_hazard_summary(cleaned_data, endpoint_config$time_var, endpoint_config$event_var)
            plot_data <- build_gep_cumulative_hazard_diagnostic_data(cleaned_data, endpoint_config$time_var, endpoint_config$event_var)
            support <- classify_gep_extrapolation_support(event_count, exponential_fit, weibull_fit, piecewise_summary)

            list(
                exponential_fit = exponential_fit,
                weibull_fit = weibull_fit,
                piecewise_summary = piecewise_summary,
                plot_data = plot_data,
                support = support
            )
        },
        error = function(e) {
            list(error = e$message)
        }
    )

    if (!is.null(model_results$error)) {
        failure_summary <- data.frame(
            Outcome = endpoint_config$outcome_type,
            Dataset = dataset_name %||% NA_character_,
            N = nrow(cleaned_data),
            Events = event_count,
            Followup_Beyond_5yr_n = follow_up_beyond_5yr,
            Exponential_Hazard_Per_Year = NA_real_,
            Exponential_Hazard_CI_Lower = NA_real_,
            Exponential_Hazard_CI_Upper = NA_real_,
            Weibull_Shape = NA_real_,
            Weibull_Shape_CI_Lower = NA_real_,
            Weibull_Shape_CI_Upper = NA_real_,
            Exponential_AIC = NA_real_,
            Weibull_AIC = NA_real_,
            Delta_AIC_Weibull_minus_Exponential = NA_real_,
            Pre5yr_Hazard_Per_Year = NA_real_,
            Post5yr_Hazard_Per_Year = NA_real_,
            Post_vs_Pre_Hazard_Ratio = NA_real_,
            Support_Status = "Unsupported",
            Support_Note = paste(
                sprintf("The extrapolation check could not be completed: %s", model_results$error),
                prepared_data$note %||% ""
            ),
            stringsAsFactors = FALSE
        )

        return(list(
            outcome_type = endpoint_config$outcome_type,
            status = "Unsupported",
            note = failure_summary$Support_Note[[1]],
            summary_table = failure_summary,
            plot_data = data.frame()
        ))
    }

    summary_table <- data.frame(
        Outcome = endpoint_config$outcome_type,
        Dataset = dataset_name %||% NA_character_,
        N = nrow(cleaned_data),
        Events = event_count,
        Followup_Beyond_5yr_n = follow_up_beyond_5yr,
        Exponential_Hazard_Per_Year = model_results$exponential_fit$hazard_per_year,
        Exponential_Hazard_CI_Lower = model_results$exponential_fit$hazard_ci_lower_per_year,
        Exponential_Hazard_CI_Upper = model_results$exponential_fit$hazard_ci_upper_per_year,
        Weibull_Shape = model_results$weibull_fit$shape,
        Weibull_Shape_CI_Lower = model_results$weibull_fit$shape_ci_lower,
        Weibull_Shape_CI_Upper = model_results$weibull_fit$shape_ci_upper,
        Exponential_AIC = model_results$exponential_fit$aic,
        Weibull_AIC = model_results$weibull_fit$aic,
        Delta_AIC_Weibull_minus_Exponential = model_results$weibull_fit$aic - model_results$exponential_fit$aic,
        Pre5yr_Hazard_Per_Year = model_results$piecewise_summary$pre_hazard_per_year,
        Post5yr_Hazard_Per_Year = model_results$piecewise_summary$post_hazard_per_year,
        Post_vs_Pre_Hazard_Ratio = model_results$piecewise_summary$post_vs_pre_hazard_ratio,
        Support_Status = model_results$support$status,
        Support_Note = paste(
            model_results$support$note,
            prepared_data$note %||% ""
        ),
        stringsAsFactors = FALSE
    )

    if (!is.null(output_dir) && dir.exists(output_dir)) {
        if (nrow(model_results$plot_data) > 0) {
            plot_title <- sprintf(
                "%s Cumulative Hazard Diagnostic%s",
                endpoint_config$title,
                if (!is.null(dataset_name) && nzchar(dataset_name)) sprintf("\n%s", dataset_name) else ""
            )
            diagnostic_plot <- ggplot2::ggplot(
                model_results$plot_data,
                ggplot2::aes(x = .data$time_years, y = .data$cumulative_hazard)
            ) +
                ggplot2::geom_step(color = "#0B4F6C", linewidth = 0.8) +
                ggplot2::geom_smooth(method = "lm", se = FALSE, color = "#C84C09", linewidth = 0.7) +
                ggplot2::labs(
                    title = plot_title,
                    subtitle = "Approximate linearity supports the exponential constant-hazard assumption",
                    x = "Follow-up (years)",
                    y = "Cumulative hazard"
                ) +
                ggplot2::theme_minimal(base_size = 11)

            ggplot2::ggsave(
                filename = file.path(output_dir, paste0(prefix, endpoint_config$file_stub, "_extrapolation_cumhaz_diagnostic.png")),
                plot = diagnostic_plot,
                width = 7,
                height = 5,
                dpi = 300
            )
        }

        summary_lines <- c(
            md_heading(sprintf("%s Extrapolation Assumption Check", endpoint_config$title), 1L),
            if (!is.null(dataset_name) && nzchar(dataset_name)) sprintf("Dataset: %s", dataset_name) else NULL,
            md_bullet(sprintf("Analyzable patients: %d", nrow(cleaned_data))),
            md_bullet(sprintf("Observed %s events: %d", endpoint_config$event_label, event_count)),
            md_bullet(sprintf("Patients with follow-up beyond 5 years: %d", follow_up_beyond_5yr)),
            md_bullet(sprintf("Support status: %s", model_results$support$status)),
            md_bullet(sprintf("Interpretation: %s", model_results$support$note))
        )
        writeLines(
            summary_lines,
            file.path(output_dir, paste0(prefix, endpoint_config$file_stub, "_extrapolation_assumption_summary.md"))
        )
    }

    list(
        outcome_type = endpoint_config$outcome_type,
        status = model_results$support$status,
        note = model_results$support$note,
        summary_table = summary_table,
        plot_data = model_results$plot_data,
        exponential_fit = model_results$exponential_fit,
        weibull_fit = model_results$weibull_fit,
        piecewise_summary = model_results$piecewise_summary
    )
}
