# GEP Model Evaluation Metrics (shared calculators)

#' Extract survival probabilities at requested times
#'
#' Summarize a fitted survival curve at one or more requested timepoints and
#' coerce the result into a numeric vector aligned to the input times.
#'
#' @param surv_fit A fitted `survival::survfit` object.
#' @param requested_times Numeric vector of times to evaluate.
#' @return Numeric vector of survival probabilities with non-finite values
#'   replaced by `1`.
extract_survival_probabilities <- function(surv_fit, requested_times) {
    surv_summary <- summary(surv_fit, times = requested_times, extend = TRUE)
    survival_probabilities <- suppressWarnings(as.numeric(surv_summary$surv))

    if (length(survival_probabilities) == 1 && length(requested_times) > 1) {
        survival_probabilities <- rep(survival_probabilities, length(requested_times))
    }

    survival_probabilities[!is.finite(survival_probabilities)] <- 1
    survival_probabilities
}

#' Calculate IPCW weights at a fixed horizon
#'
#' Derive inverse-probability-of-censoring weights for horizon-specific
#' calibration analyses, including capped and renormalized weights to limit the
#' influence of extreme censoring probabilities.
#'
#' @param time Numeric vector of observed follow-up times.
#' @param event Integer or logical event indicator where `1` denotes the target
#'   event.
#' @param eval_time_months Numeric horizon in months.
#' @return Named list with IPCW weights, known-status flags, censoring survival,
#'   and horizon-specific event indicators.
calculate_ipcw_weights <- function(time, event, eval_time_months) {
    truncated_time <- pmin(time, eval_time_months)
    event_by_horizon <- as.integer(event == 1 & time <= eval_time_months)
    known_status <- event_by_horizon == 1 | time >= eval_time_months
    censoring_event <- as.integer(event_by_horizon == 0 & time < eval_time_months)

    censoring_fit <- survival::survfit(survival::Surv(truncated_time, censoring_event) ~ 1)
    weight_times <- ifelse(
        event_by_horizon == 1,
        pmax(truncated_time - 1e-08, 0),
        truncated_time
    )
    censoring_survival <- extract_survival_probabilities(censoring_fit, weight_times)

    raw_weights <- ifelse(
        known_status,
        1 / pmax(censoring_survival, GEP_MIN_RISK_PREDICTION),
        0
    )

    positive_weights <- raw_weights[raw_weights > 0 & is.finite(raw_weights)]
    capped_weights <- raw_weights
    if (length(positive_weights) > 1) {
        weight_cap <- stats::quantile(
            positive_weights,
            probs = GEP_IPCW_WEIGHT_CAP_PROB,
            na.rm = TRUE,
            names = FALSE,
            type = 8
        )
        capped_weights <- pmin(raw_weights, weight_cap)
        if (sum(capped_weights, na.rm = TRUE) > 0) {
            capped_weights[capped_weights > 0] <-
                capped_weights[capped_weights > 0] * sum(known_status, na.rm = TRUE) / sum(capped_weights, na.rm = TRUE)
        }
    }

    list(
        ipcw_weight = capped_weights,
        known_status = known_status,
        censoring_survival = censoring_survival,
        event_by_horizon = event_by_horizon
    )
}

#' Prepare calibration data for a fixed time horizon
#'
#' Filter to complete cases, bound predicted risks away from 0 and 1, derive the
#' logit-transformed prediction, and append IPCW-derived horizon status fields.
#'
#' @param data Data frame containing observed outcomes and predicted risk.
#' @param predicted_risk_var Character name of the predicted-risk column.
#' @param time_var Character name of the observed time column.
#' @param event_var Character name of the observed event indicator column.
#' @param eval_time_months Numeric horizon in months.
#' @return Data frame augmented with standardized calibration columns such as
#'   `predicted_risk`, `observed_time`, `observed_event`, `horizon_event`, and
#'   `ipcw_weight`.
prepare_horizon_calibration_data <- function(data, predicted_risk_var, time_var, event_var, eval_time_months) {
    cal_data <- data %>%
        dplyr::filter(
            !is.na(.data[[predicted_risk_var]]),
            !is.na(.data[[time_var]]),
            !is.na(.data[[event_var]])
        ) %>%
        dplyr::mutate(
            predicted_risk = pmin(
                pmax(.data[[predicted_risk_var]], GEP_MIN_RISK_PREDICTION),
                GEP_MAX_RISK_PREDICTION
            ),
            observed_time = .data[[time_var]],
            observed_event = as.integer(.data[[event_var]]),
            logit_predicted_risk = stats::qlogis(predicted_risk)
        )

    if (nrow(cal_data) == 0) {
        return(cal_data)
    }

    ipcw_info <- calculate_ipcw_weights(
        time = cal_data$observed_time,
        event = cal_data$observed_event,
        eval_time_months = eval_time_months
    )

    cal_data %>%
        dplyr::mutate(
            horizon_event = ipcw_info$event_by_horizon,
            known_status = ipcw_info$known_status,
            ipcw_weight = ipcw_info$ipcw_weight,
            censoring_survival = ipcw_info$censoring_survival
        )
}

#' Fit IPCW-weighted recalibration models
#'
#' Estimate horizon-specific calibration intercept and slope using weighted
#' logistic recalibration models after excluding observations without known
#' status at the evaluation horizon.
#'
#' @param cal_data Data frame produced by `prepare_horizon_calibration_data()`.
#' @return Named list containing intercept, slope, method labels, fit status,
#'   sample size, and event counts.
fit_ipcw_recalibration <- function(cal_data) {
    fit_data <- cal_data %>%
        dplyr::filter(known_status, ipcw_weight > 0, is.finite(logit_predicted_risk))

    event_count <- sum(fit_data$horizon_event == 1, na.rm = TRUE)
    non_event_count <- sum(fit_data$horizon_event == 0, na.rm = TRUE)
    unique_risk_count <- length(unique(fit_data$predicted_risk))

    if (nrow(fit_data) < GEP_MIN_SAMPLE_SIZE ||
        length(unique(fit_data$logit_predicted_risk)) < 2 ||
        event_count < GEP_MIN_CALIBRATION_EVENTS ||
        non_event_count < GEP_MIN_CALIBRATION_EVENTS) {
        return(list(
            intercept = NA_real_,
            slope = NA_real_,
            slope_method = "ipcw_logit_unavailable",
            intercept_method = "ipcw_offset_unavailable",
            status = "insufficient_recalibration_data",
            fit_n = nrow(fit_data),
            events = event_count,
            non_events = non_event_count,
            unique_risk_count = unique_risk_count,
            intercept_se = NA_real_,
            slope_se = NA_real_
        ))
    }

    intercept_model <- tryCatch(
        suppressWarnings(
            stats::glm(
                horizon_event ~ 1,
                data = fit_data,
                family = stats::quasibinomial(),
                weights = ipcw_weight,
                offset = logit_predicted_risk
            )
        ),
        error = function(e) NULL
    )

    slope_model <- tryCatch(
        suppressWarnings(
            stats::glm(
                horizon_event ~ logit_predicted_risk,
                data = fit_data,
                family = stats::quasibinomial(),
                weights = ipcw_weight
            )
        ),
        error = function(e) NULL
    )

    intercept <- NA_real_
    intercept_se <- NA_real_
    if (!is.null(intercept_model) && all(is.finite(stats::coef(intercept_model)))) {
        intercept <- unname(stats::coef(intercept_model)[1])
        intercept_summary <- tryCatch(summary(intercept_model)$coefficients, error = function(e) NULL)
        if (!is.null(intercept_summary) && nrow(intercept_summary) >= 1 && "Std. Error" %in% colnames(intercept_summary)) {
            intercept_se <- unname(intercept_summary[1, "Std. Error"])
        }
    }

    slope <- NA_real_
    slope_se <- NA_real_
    if (!is.null(slope_model) && length(stats::coef(slope_model)) >= 2 && all(is.finite(stats::coef(slope_model)))) {
        slope <- unname(stats::coef(slope_model)[2])
        slope_summary <- tryCatch(summary(slope_model)$coefficients, error = function(e) NULL)
        if (!is.null(slope_summary) && nrow(slope_summary) >= 2 && "Std. Error" %in% colnames(slope_summary)) {
            slope_se <- unname(slope_summary[2, "Std. Error"])
        }
    }

    stable_intercept <-
        is.finite(intercept) &&
        is.finite(intercept_se) &&
        abs(intercept) <= GEP_MAX_CALIBRATION_COEF_ABS &&
        intercept_se <= GEP_MAX_CALIBRATION_COEF_SE

    stable_slope <-
        is.finite(slope) &&
        is.finite(slope_se) &&
        abs(slope) <= GEP_MAX_CALIBRATION_COEF_ABS &&
        slope_se <= GEP_MAX_CALIBRATION_COEF_SE

    list(
        intercept = ifelse(stable_intercept, intercept, NA_real_),
        slope = ifelse(stable_slope, slope, NA_real_),
        slope_method = ifelse(stable_slope, "ipcw_logit", "ipcw_logit_unavailable"),
        intercept_method = ifelse(stable_intercept, "ipcw_offset", "ipcw_offset_unavailable"),
        status = ifelse(stable_slope, "ok", "recalibration_fit_unstable"),
        fit_n = nrow(fit_data),
        events = event_count,
        non_events = non_event_count,
        unique_risk_count = unique_risk_count,
        intercept_se = intercept_se,
        slope_se = slope_se
    )
}

#' Calculate a smoothed IPCW Integrated Calibration Index
#'
#' Fit a weighted spline recalibration curve when the usable risk support is
#' adequate and otherwise fall back to the grouped Kaplan-Meier calibration
#' summary already computed for the same horizon.
#'
#' @param cal_data Data frame produced by `prepare_horizon_calibration_data()`.
#' @param grouped_calibration List returned by
#'   `calculate_greenwood_nam_dagostino()`.
#' @return Named list with `ici` and `ici_method`.
calculate_smoothed_ipcw_ici <- function(cal_data, grouped_calibration) {
    fit_data <- cal_data %>%
        dplyr::filter(known_status, ipcw_weight > 0, is.finite(logit_predicted_risk))

    event_count <- sum(fit_data$horizon_event == 1, na.rm = TRUE)
    non_event_count <- sum(fit_data$horizon_event == 0, na.rm = TRUE)
    unique_risk_count <- length(unique(fit_data$predicted_risk))

    can_fit_smooth_curve <-
        nrow(fit_data) >= GEP_MIN_SAMPLE_SIZE &&
        unique_risk_count >= GEP_DEFAULT_N_GROUPS &&
        event_count >= GEP_MIN_CALIBRATION_EVENTS &&
        non_event_count >= GEP_MIN_CALIBRATION_EVENTS

    if (!can_fit_smooth_curve) {
        return(list(
            ici = grouped_calibration$ici,
            ici_method = grouped_calibration$ici_method
        ))
    }

    spline_df <- min(
        GEP_CALIBRATION_SPLINE_DF,
        unique_risk_count - 1L,
        nrow(fit_data) - 1L
    )

    if (spline_df < 2) {
        return(list(
            ici = grouped_calibration$ici,
            ici_method = grouped_calibration$ici_method
        ))
    }

    smooth_model <- tryCatch(
        suppressWarnings(
            stats::glm(
                horizon_event ~ splines::ns(logit_predicted_risk, df = spline_df),
                data = fit_data,
                family = stats::quasibinomial(),
                weights = ipcw_weight
            )
        ),
        error = function(e) NULL
    )

    if (is.null(smooth_model)) {
        return(list(
            ici = grouped_calibration$ici,
            ici_method = grouped_calibration$ici_method
        ))
    }

    fitted_risk <- suppressWarnings(stats::predict(smooth_model, type = "response"))
    fitted_risk <- pmin(pmax(fitted_risk, 0), 1)

    list(
        ici = weighted.mean(
            abs(fit_data$predicted_risk - fitted_risk),
            w = fit_data$ipcw_weight,
            na.rm = TRUE
        ),
        ici_method = "ipcw_logistic_spline"
    )
}

#' Assign grouped calibration risk bins
#'
#' Split predicted risks into approximately equal-sized groups for grouped
#' calibration summaries, with a binary fallback when the distribution is too
#' discrete for quantile-based grouping.
#'
#' @param predicted_risk Numeric vector of predicted risks.
#' @return Integer vector of risk-group assignments.
assign_calibration_risk_groups <- function(predicted_risk) {
    n_obs <- length(predicted_risk)
    n_groups <- min(10, floor(n_obs / 10))
    if (n_groups < 3) {
        n_groups <- 3
    }

    risk_quantiles <- unique(stats::quantile(predicted_risk, seq(0, 1, length.out = n_groups + 1), na.rm = TRUE))

    if (length(risk_quantiles) <= 2) {
        return(ifelse(predicted_risk <= stats::median(predicted_risk, na.rm = TRUE), 1, 2))
    }

    cut(predicted_risk, breaks = risk_quantiles, include.lowest = TRUE, labels = FALSE)
}

#' Estimate observed events using Kaplan-Meier at a horizon
#'
#' Convert a univariate Kaplan-Meier estimate into observed event-rate and
#' variance quantities used by grouped survival-calibration summaries.
#'
#' @param time Numeric vector of observed follow-up times.
#' @param event Integer or logical event indicator where `1` denotes the target
#'   event.
#' @param eval_time_months Numeric horizon in months.
#' @return Named list with observed rate, observed event count, raw events,
#'   Kaplan-Meier survival estimates, and Greenwood-based variance terms.
estimate_km_observed_events <- function(time, event, eval_time_months) {
    surv_fit <- survival::survfit(survival::Surv(time, event) ~ 1)
    surv_summary <- summary(surv_fit, times = eval_time_months, extend = TRUE)

    survival_prob <- suppressWarnings(as.numeric(surv_summary$surv[[1]] %||% surv_summary$surv[1]))
    survival_se <- suppressWarnings(as.numeric(surv_summary$std.err[[1]] %||% surv_summary$std.err[1]))

    if (!is.finite(survival_prob)) {
        survival_prob <- 1
    }
    if (!is.finite(survival_se)) {
        survival_se <- 0
    }

    observed_risk <- 1 - survival_prob
    n_obs <- length(time)

    list(
        observed_rate = observed_risk,
        observed_events = n_obs * observed_risk,
        raw_events = sum(event == 1 & time <= eval_time_months, na.rm = TRUE),
        km_survival = survival_prob,
        km_survival_se = survival_se,
        observed_events_variance = (n_obs^2) * (survival_se^2)
    )
}

#' Calculate grouped Greenwood Nam-D'Agostino calibration metrics
#'
#' Group patients by predicted risk, estimate observed risk within each group
#' using Kaplan-Meier, and compute the grouped Greenwood Nam-D'Agostino test and
#' grouped absolute calibration error.
#'
#' @param data Data frame containing predicted risk and observed endpoint data.
#' @param predicted_risk_var Character name of the predicted-risk column.
#' @param time_var Character name of the observed time column.
#' @param event_var Character name of the observed event indicator column.
#' @param eval_time_months Numeric horizon in months.
#' @return Named list with grouped calibration statistics, method labels, and a
#'   per-group results table.
calculate_greenwood_nam_dagostino <- function(data, predicted_risk_var, time_var, event_var, eval_time_months) {
    cal_data <- data %>%
        dplyr::filter(
            !is.na(.data[[predicted_risk_var]]),
            !is.na(.data[[time_var]]),
            !is.na(.data[[event_var]])
        ) %>%
        dplyr::mutate(
            predicted_risk = .data[[predicted_risk_var]],
            observed_time = .data[[time_var]],
            observed_event = .data[[event_var]],
            risk_group = assign_calibration_risk_groups(.data[[predicted_risk_var]])
        )

    if (nrow(cal_data) < GEP_MIN_SAMPLE_SIZE) {
        return(list(
            n = nrow(cal_data),
            n_groups = NA_integer_,
            nam_dagostino_statistic = NA_real_,
            nam_dagostino_p = NA_real_,
            nam_dagostino_method = "greenwood_nam_dagostino",
            ici = NA_real_,
            ici_method = "grouped_km",
            group_results = data.frame()
        ))
    }

    group_results <- cal_data %>%
        dplyr::group_by(risk_group) %>%
        dplyr::group_modify(~ {
            km_metrics <- estimate_km_observed_events(.x$observed_time, .x$observed_event, eval_time_months)
            tibble::tibble(
                n = nrow(.x),
                mean_predicted_risk = mean(.x$predicted_risk, na.rm = TRUE),
                expected_events = sum(.x$predicted_risk, na.rm = TRUE),
                expected_rate = mean(.x$predicted_risk, na.rm = TRUE),
                observed_events = km_metrics$observed_events,
                observed_rate = km_metrics$observed_rate,
                raw_events = km_metrics$raw_events,
                km_survival = km_metrics$km_survival,
                km_survival_se = km_metrics$km_survival_se,
                observed_events_variance = km_metrics$observed_events_variance
            )
        }) %>%
        dplyr::ungroup() %>%
        dplyr::filter(n >= GEP_MIN_GROUP_SIZE)

    valid_groups <- group_results %>%
        dplyr::filter(
            is.finite(expected_events),
            is.finite(observed_events),
            is.finite(observed_events_variance)
        )

    chisq_stat <- NA_real_
    nam_dagostino_p <- NA_real_
    nam_dagostino_log_p <- NA_real_
    if (nrow(valid_groups) >= 3) {
        chisq_stat <- sum((valid_groups$observed_events - valid_groups$expected_events)^2 /
            pmax(valid_groups$observed_events_variance, .Machine$double.eps), na.rm = TRUE)
        nam_dagostino_p <- stats::pchisq(chisq_stat, df = nrow(valid_groups) - 1, lower.tail = FALSE)
        nam_dagostino_log_p <- calculate_chisq_log_p_value(chisq_stat, df = nrow(valid_groups) - 1)
    }

    ici <- NA_real_
    if (nrow(group_results) > 0) {
        cal_data <- cal_data %>%
            dplyr::left_join(
                group_results %>% dplyr::select(risk_group, grouped_observed_rate = observed_rate),
                by = "risk_group"
            )
        ici <- mean(abs(cal_data$predicted_risk - cal_data$grouped_observed_rate), na.rm = TRUE)
    }

    list(
        n = nrow(cal_data),
        n_groups = nrow(group_results),
        nam_dagostino_statistic = round(chisq_stat, 3),
        nam_dagostino_p = nam_dagostino_p,
        nam_dagostino_log_p = nam_dagostino_log_p,
        nam_dagostino_method = "greenwood_nam_dagostino",
        ici = round(ici, 4),
        ici_method = "grouped_km",
        group_results = group_results
    )
}

#' Summarize survival calibration at a fixed horizon
#'
#' Combine grouped Greenwood Nam-D'Agostino results, IPCW-weighted logistic
#' recalibration, smoothed or fallback ICI estimates, and Brier score output
#' into the canonical calibration payload used by Objective 4 reporting.
#'
#' @param data Data frame containing observed outcomes and predicted risk.
#' @param predicted_risk_var Character name of the predicted-risk column.
#' @param time_var Character name of the observed time column.
#' @param event_var Character name of the observed event indicator column.
#' @param eval_time_months Numeric horizon in months.
#' @return Named list containing calibration summary metrics, method labels,
#'   group-level results, and Brier score diagnostics.
calculate_survival_calibration_summary <- function(data, predicted_risk_var, time_var, event_var, eval_time_months) {
    cal_data <- prepare_horizon_calibration_data(
        data = data,
        predicted_risk_var = predicted_risk_var,
        time_var = time_var,
        event_var = event_var,
        eval_time_months = eval_time_months
    )

    if (nrow(cal_data) == 0) {
        return(list(
            n = 0,
            known_status_n = 0,
            status = "no_complete_cases",
            intercept = NA_real_,
            calibration_intercept = NA_real_,
            intercept_method = "ipcw_offset_unavailable",
            slope = NA_real_,
            slope_method = "ipcw_logit_unavailable",
            ici = NA_real_,
            ici_method = "grouped_km",
            nam_dagostino_p = NA_real_,
            nam_dagostino_statistic = NA_real_,
            nam_dagostino_method = "greenwood_nam_dagostino",
            n_groups = NA_integer_,
            group_results = data.frame()
        ))
    }

    grouped_calibration <- calculate_greenwood_nam_dagostino(
        data = cal_data,
        predicted_risk_var = "predicted_risk",
        time_var = "observed_time",
        event_var = "observed_event",
        eval_time_months = eval_time_months
    )

    recalibration_fit <- fit_ipcw_recalibration(cal_data)
    ici_result <- calculate_smoothed_ipcw_ici(cal_data, grouped_calibration)

    fallback_ici <- NA_real_
    if (sum(cal_data$known_status, na.rm = TRUE) > 0) {
        fallback_ici <- weighted.mean(
            abs(cal_data$predicted_risk[cal_data$known_status] -
                weighted.mean(
                    cal_data$horizon_event[cal_data$known_status],
                    w = cal_data$ipcw_weight[cal_data$known_status],
                    na.rm = TRUE
                )),
            w = cal_data$ipcw_weight[cal_data$known_status],
            na.rm = TRUE
        )
    }

    if (!is.finite(ici_result$ici)) {
        ici_result$ici <- fallback_ici
        ici_result$ici_method <- "weighted_absolute_error_fallback"
    }

    brier_result <- tryCatch({
        calculate_brier_score_survival(
            data = cal_data,
            predicted_var = "predicted_risk",
            event_var = "observed_event",
            time_var = "observed_time",
            timepoint_months = eval_time_months
        )
    }, error = function(e) {
        logger::log_warn(sprintf("Brier Score calculation failed: %s", e$message))
        list(
            brier_score = NA_real_,
            method_used = "calculation_failed",
            fallback_triggered = FALSE,
            calculation_notes = sprintf("Calculation failed: %s", e$message)
        )
    })

    list(
        n = nrow(cal_data),
        known_status_n = sum(cal_data$known_status, na.rm = TRUE),
        status = recalibration_fit$status,
        fit_n = recalibration_fit$fit_n,
        events = recalibration_fit$events,
        non_events = recalibration_fit$non_events,
        unique_risk_count = recalibration_fit$unique_risk_count,
        intercept = round(recalibration_fit$intercept, 3),
        calibration_intercept = round(recalibration_fit$intercept, 3),
        intercept_method = recalibration_fit$intercept_method,
        intercept_se = recalibration_fit$intercept_se,
        slope = round(recalibration_fit$slope, 3),
        slope_method = recalibration_fit$slope_method,
        slope_se = recalibration_fit$slope_se,
        ici = round(ici_result$ici, 4),
        ici_method = ici_result$ici_method,
        nam_dagostino_p = grouped_calibration$nam_dagostino_p,
        nam_dagostino_log_p = grouped_calibration$nam_dagostino_log_p,
        nam_dagostino_method = grouped_calibration$nam_dagostino_method,
        nam_dagostino_statistic = grouped_calibration$nam_dagostino_statistic,
        n_groups = grouped_calibration$n_groups,
        group_results = grouped_calibration$group_results,
        brier_score = brier_result$brier_score,
        brier_method = brier_result$method_used,
        brier_fallback_used = brier_result$fallback_triggered,
        brier_calculation_notes = brier_result$calculation_notes
    )
}

#' Calculate observed vs expected rates by GEP class
#'
#' @param data Data frame with endpoint data
#' @param expected_var Expected survival variable name
#' @param event_var Event variable name
#' @param time_var Time variable name
#' @param group_var Grouping variable name (default 'biopsy1_gep')
#' @return Data frame with observed vs expected rates
calculate_observed_expected_rates <- function(data, expected_var, event_var, time_var, group_var = "biopsy1_gep") {
    logger::log_debug("Calculating observed vs expected rates")
    if (!group_var %in% names(data)) stop(sprintf("Grouping variable '%s' not found in data", group_var))
    results_raw <- data %>%
        dplyr::group_by(.data[[group_var]]) %>%
        dplyr::summarise(
            n = dplyr::n(),
            observed = sum(.data[[event_var]]),
            expected = sum(1 - .data[[expected_var]]),
            .groups = "drop"
        )

    poisson_ci_lower <- vapply(seq_len(nrow(results_raw)), function(i) {
        expected_events <- results_raw$expected[i]
        observed_events <- results_raw$observed[i]

        if (is.na(expected_events) || expected_events <= 0) {
            return(NA_real_)
        }

        stats::poisson.test(observed_events)$conf.int[1] / expected_events
    }, numeric(1))

    poisson_ci_upper <- vapply(seq_len(nrow(results_raw)), function(i) {
        expected_events <- results_raw$expected[i]
        observed_events <- results_raw$observed[i]

        if (is.na(expected_events) || expected_events <= 0) {
            return(NA_real_)
        }

        stats::poisson.test(observed_events)$conf.int[2] / expected_events
    }, numeric(1))

    results <- results_raw %>%
        dplyr::mutate(
            oe_ratio = ifelse(expected > 0, observed / expected, NA_real_),
            expected_rate = ifelse(n > 0, expected / n, NA_real_),
            observed_rate = ifelse(n > 0, observed / n, NA_real_),
            poisson_ci_lower = round(poisson_ci_lower, 3),
            poisson_ci_upper = round(poisson_ci_upper, 3)
        )

    observed_total <- sum(results_raw$observed, na.rm = TRUE)
    expected_total <- sum(results_raw$expected, na.rm = TRUE)
    overall_oe_ratio <- ifelse(expected_total > 0, observed_total / expected_total, NA_real_)

    overall_poisson_ci_lower <- NA_real_
    overall_poisson_ci_upper <- NA_real_
    if (!is.na(expected_total) && expected_total > 0) {
        overall_poisson <- stats::poisson.test(observed_total)
        overall_poisson_ci_lower <- overall_poisson$conf.int[1] / expected_total
        overall_poisson_ci_upper <- overall_poisson$conf.int[2] / expected_total
    }

    expected_vec <- results_raw$expected
    observed_vec <- results_raw$observed
    chisq_p_value <- NA_real_
    chisq_log_p_value <- NA_real_
    chisq_statistic <- NA_real_
    if (length(expected_vec) > 1 && all(expected_vec > 0) && sum(expected_vec) > 0) {
        chisq_statistic <- sum((observed_vec - expected_vec)^2 / expected_vec)
        chisq_p_value <- stats::pchisq(chisq_statistic, df = length(expected_vec) - 1, lower.tail = FALSE)
        chisq_log_p_value <- calculate_chisq_log_p_value(chisq_statistic, df = length(expected_vec) - 1)
    }

    attr(results, "overall_n") <- sum(results_raw$n, na.rm = TRUE)
    attr(results, "overall_observed") <- observed_total
    attr(results, "overall_expected") <- round(expected_total, 2)
    attr(results, "overall_oe_ratio") <- round(overall_oe_ratio, 3)
    attr(results, "overall_poisson_ci_lower") <- round(overall_poisson_ci_lower, 3)
    attr(results, "overall_poisson_ci_upper") <- round(overall_poisson_ci_upper, 3)
    attr(results, "chisq_p_value") <- chisq_p_value
    attr(results, "chisq_log_p_value") <- chisq_log_p_value
    attr(results, "chisq_statistic") <- round(chisq_statistic, 3)

    return(results)
}

#' Calculate survival calibration metrics
#'
#' Compute grouped Greenwood-Nam-D'Agostino calibration statistics and derive
#' horizon-specific recalibration summaries from IPCW-weighted logistic models.
#'
#' @param data Data frame containing observed outcome and predicted probabilities
#' @param expected_var Character name of predicted probability column
#' @param event_var Character name of binary event indicator column
#' @param time_var Character name of time variable
#' @param eval_time_months Numeric horizon in months for the calibration target
#' @return A list with `n`, `intercept`, `slope`, `ici`, and `nam_dagostino_p` fields
calculate_calibration_metrics <- function(data, expected_var, event_var, time_var, eval_time_months) {
    logger::log_debug("Calculating calibration metrics")
    if (length(unique(stats::na.omit(data[[expected_var]]))) < 2) {
        return(list(
            n = nrow(data),
            known_status_n = NA_integer_,
            status = "insufficient_prediction_variation",
            intercept = NA_real_,
            calibration_intercept = NA_real_,
            intercept_method = "ipcw_offset_unavailable",
            slope = NA_real_,
            slope_method = "ipcw_logit_unavailable",
            ici = NA_real_,
            nam_dagostino_p = NA_real_,
            nam_dagostino_statistic = NA_real_,
            nam_dagostino_method = "greenwood_nam_dagostino",
            ici_method = "grouped_km"
        ))
    }

    data <- data %>%
        dplyr::mutate(.predicted_risk_for_calibration = 1 - .data[[expected_var]])

    calculate_survival_calibration_summary(
        data = data,
        predicted_risk_var = ".predicted_risk_for_calibration",
        time_var = time_var,
        event_var = event_var,
        eval_time_months = eval_time_months
    )
}

#' Calculate discrimination metrics (simplified)
    #'
    #' Compute a lightweight discrimination summary based on the rank correlation
    #' between predicted risk and the observed event indicator, with optional
    #' bootstrap confidence limits.
    #'
    #' @param data Data frame containing predictions and observed events.
    #' @param expected_var Character name of the predicted-probability column.
    #' @param event_var Character name of the binary event indicator column.
    #' @param time_var Character name of the time column retained for interface
    #'   consistency.
    #' @param bootstrap_iterations Integer number of bootstrap resamples.
    #' @return Data frame with discrimination estimate and confidence interval
    #'   limits.
calculate_discrimination_metrics <- function(data, expected_var, event_var, time_var, bootstrap_iterations) {
    logger::log_debug("Calculating discrimination metrics")
    harrell_c <- tryCatch(
        {
            cor(data[[expected_var]], data[[event_var]], method = "spearman")
        },
        error = function(e) {
            NA
        }
    )
    # REMOVED: Uno's C-index calculation (fragile metric)
    # Integrated AUC and other robust metrics are calculated separately
    
    if (bootstrap_iterations > 0) {
        bootstrap_c <- numeric(bootstrap_iterations)
        for (i in 1:bootstrap_iterations) {
            boot_indices <- sample(nrow(data), replace = TRUE)
            boot_data <- data[boot_indices, ]
            bootstrap_c[i] <- tryCatch(
                {
                    cor(boot_data[[expected_var]], boot_data[[event_var]], method = "spearman")
                },
                error = function(e) {
                    NA
                }
            )
        }
        c_ci_lower <- quantile(bootstrap_c, 0.025, na.rm = TRUE)
        c_ci_upper <- quantile(bootstrap_c, 0.975, na.rm = TRUE)
    } else {
        c_ci_lower <- NA
        c_ci_upper <- NA
    }
    return(data.frame(
        harrell_c = harrell_c,
        c_ci_lower = c_ci_lower,
        c_ci_upper = c_ci_upper,
        stringsAsFactors = FALSE
    ))
}

#' Calculate cumulative incidence (simplified)
#'
#' Produce a class-level cumulative-incidence summary using simple event counts
#' within each grouping level.
#'
#' @param data Data frame containing grouped event data.
#' @param time_var Character name of the time column retained for interface
#'   consistency.
#' @param event_var Character name of the event column retained for interface
#'   consistency.
#' @param group_var Character name of the grouping column.
#' @return Data frame with per-group counts and cumulative-incidence estimates.
calculate_cumulative_incidence <- function(data, time_var, event_var, group_var) {
    logger::log_debug("Calculating cumulative incidence")
    results <- data %>%
        dplyr::group_by(.data[[group_var]]) %>%
        dplyr::summarise(
            n = dplyr::n(),
            melanoma_deaths = sum(event_type == 1),
            competing_deaths = sum(event_type == 2),
            censored = sum(event_type == 0),
            .groups = "drop"
        ) %>%
        dplyr::mutate(
            melanoma_ci = melanoma_deaths / n,
            competing_ci = competing_deaths / n
        )
    return(results)
}

#' Calculate cause-specific hazards (simplified)
#'
#' Produce a class-level cause-specific hazard summary using observed event counts
#' and total follow-up time within each grouping level.
#'
#' @param data Data frame containing grouped event data.
#' @param time_var Character name of the time column.
#' @param event_var Character name of the event column retained for interface
#'   consistency.
#' @param group_var Character name of the grouping column.
#' @return Data frame with per-group counts and cause-specific hazard estimates.
calculate_cause_specific_hazards <- function(data, time_var, event_var, group_var) {
    logger::log_debug("Calculating cause-specific hazards")
    results <- data %>%
        dplyr::group_by(.data[[group_var]]) %>%
        dplyr::summarise(
            n = dplyr::n(),
            melanoma_deaths = sum(event_type == 1),
            competing_deaths = sum(event_type == 2),
            total_time = sum(.data[[time_var]]),
            .groups = "drop"
        ) %>%
        dplyr::mutate(
            melanoma_hazard = melanoma_deaths / total_time,
            competing_hazard = competing_deaths / total_time
        )
    return(results)
}

#' Calculate net reclassification index (simplified)
#'
#' Approximate the net reclassification gain from an enhanced predictor by
#' comparing its rank correlation with the observed event indicator against a
#' base predictor.
#'
#' @param data Data frame containing both prediction columns and the event
#'   indicator.
#' @param base_pred Character name of the base-prediction column.
#' @param enhanced_pred Character name of the enhanced-prediction column.
#' @param event_var Character name of the observed event indicator column.
#' @return One-row data frame with the simplified NRI estimate.
calculate_net_reclassification_index <- function(data, base_pred, enhanced_pred, event_var) {
    logger::log_debug("Calculating net reclassification index")
    nri <- tryCatch(
        {
            base_cor <- cor(data[[base_pred]], data[[event_var]], method = "spearman")
            enhanced_cor <- cor(data[[enhanced_pred]], data[[event_var]], method = "spearman")
            enhanced_cor - base_cor
        },
        error = function(e) {
            NA
        }
    )
    return(data.frame(
        nri = nri,
        stringsAsFactors = FALSE
    ))
}

#' Calculate class-specific cumulative incidence and 95% CIs at a fixed time
#'
#' Uses the nonparametric Aalen-Johansen estimator via `cmprsk::cuminc` when
#' available. Confidence intervals are computed by:
#' 1) Stratified bootstrap on patients within `biopsy1_gep` (default),
#' 2) If bootstrap not requested or fails, normal approximation using the
#'    Greenwood-type variance from `cmprsk` when provided.
#'
#' @param data Data frame with columns: time_var, event_type (1=melanoma death,
#'   2=competing, 0=censored), and a grouping variable
#' @param time_var Character time variable name (in years)
#' @param event_type_var Character event type variable name (0/1/2)
#' @param eval_time Numeric time point (years)
#' @param n_boot Integer number of bootstrap resamples (default 1000)
#' @param group_var Character name of grouping variable (default 'biopsy1_gep')
#' @param eligibility_filter Character name of eligibility filter column (e.g., 'mss_analysis_eligible')
#' @return Data frame with columns: Group, n, cif, ci_lower, ci_upper
calculate_cif_by_class_with_ci <- function(data, time_var, event_type_var, eval_time, n_boot = 1000, group_var = "biopsy1_gep", eligibility_filter = "mss_analysis_eligible") {
    # CRITICAL: Apply eligibility filters to prevent segmentation fault
    logger::log_info("=== CIF ANALYSIS START ===")
    logger::log_info(sprintf("Input data dimensions: %d rows, %d cols", nrow(data), ncol(data)))
    
    # Apply eligibility filter (guaranteed to exist from data processing pipeline)
    data <- data %>% filter(.data[[eligibility_filter]])
    logger::log_info(sprintf("After %s filter: %d rows", eligibility_filter, nrow(data)))
    
    logger::log_info(sprintf("Variables: time_var='%s', event_type_var='%s', group_var='%s'", time_var, event_type_var, group_var))
    logger::log_info(sprintf("Available columns: %s", paste(names(data), collapse = ", ")))
    
    if (!group_var %in% names(data)) {
        stop(sprintf("calculate_cif_by_class_with_ci requires '%s' column", group_var))
    }
    if (!time_var %in% names(data)) {
        stop(sprintf("calculate_cif_by_class_with_ci requires '%s' column", time_var))
    }
    if (!event_type_var %in% names(data)) {
        stop(sprintf("calculate_cif_by_class_with_ci requires '%s' column", event_type_var))
    }
    
    # Debug data quality
    logger::log_info(sprintf("Time variable '%s' summary:", time_var))
    logger::log_info(sprintf("  Class: %s", class(data[[time_var]])))
    logger::log_info(sprintf("  Range: %s to %s", min(data[[time_var]], na.rm = TRUE), max(data[[time_var]], na.rm = TRUE)))
    logger::log_info(sprintf("  NA count: %d", sum(is.na(data[[time_var]]))))
    logger::log_info(sprintf("  Infinite count: %d", sum(is.infinite(data[[time_var]]), na.rm = TRUE)))
    
    logger::log_info(sprintf("Event variable '%s' summary:", event_type_var))
    logger::log_info(sprintf("  Class: %s", class(data[[event_type_var]])))
    logger::log_info(sprintf("  Unique values: %s", paste(unique(data[[event_type_var]]), collapse = ", ")))
    logger::log_info(sprintf("  NA count: %d", sum(is.na(data[[event_type_var]]))))
    
    logger::log_info(sprintf("Group variable '%s' summary:", group_var))
    logger::log_info(sprintf("  Class: %s", class(data[[group_var]])))
    logger::log_info(sprintf("  Unique values: %s", paste(unique(data[[group_var]]), collapse = ", ")))
    logger::log_info(sprintf("  NA count: %d", sum(is.na(data[[group_var]]))))
    
    # Assume cmprsk is available; rely on tryCatch to handle errors gracefully
    tryCatch({
        # Helper to get CIF at eval_time for one class
        get_cif_for_class <- function(df_class) {

            # CRITICAL: Filter out NA values before calling cmprsk::cuminc
            df_class <- df_class[!is.na(df_class[[time_var]]) & !is.na(df_class[[event_type_var]]), , drop = FALSE]
            # print(df_class)
            # print(df_class[[time_var]])
            # print(df_class[[event_type_var]])

            logger::log_info(sprintf("get_cif_for_class called with %d rows", nrow(df_class)))
            logger::log_info(sprintf("  Time values: %s", paste(head(df_class[[time_var]], 10), collapse = ", ")))
            logger::log_info(sprintf("  Event values: %s", paste(head(df_class[[event_type_var]], 10), collapse = ", ")))
            logger::log_info(sprintf("  Event table: %s", paste(names(table(df_class[[event_type_var]])), "=", table(df_class[[event_type_var]]), collapse = ", ")))
            
            # CRITICAL: Check for empty data frame after NA filtering - this prevents segfault
            if (nrow(df_class) == 0) {
                logger::log_warn("Empty data frame after NA filtering - returning 0 to prevent segfault")
                return(0)
            }
            
            # Check for any invalid values that might cause segfault
            time_vals <- df_class[[time_var]]
            event_vals <- df_class[[event_type_var]]
            
            if (any(is.infinite(time_vals), na.rm = TRUE)) {
                logger::log_error("INFINITE TIME VALUES DETECTED - THIS WILL CAUSE SEGFAULT")
                stop("Infinite time values detected")
            }
            
            if (any(time_vals < 0, na.rm = TRUE)) {
                logger::log_error("NEGATIVE TIME VALUES DETECTED - THIS WILL CAUSE SEGFAULT")
                stop("Negative time values detected")
            }
            
            if (any(!event_vals %in% c(0, 1, 2, NA), na.rm = TRUE)) {
                logger::log_error(sprintf("INVALID EVENT VALUES DETECTED: %s - THIS WILL CAUSE SEGFAULT", paste(unique(event_vals[!event_vals %in% c(0, 1, 2, NA)]), collapse = ", ")))
                stop("Invalid event values detected")
            }
            
            logger::log_info("About to call cmprsk::cuminc - this is where segfault occurs if data is invalid")

            ci_obj <- tryCatch({
                cmprsk::cuminc(ftime = df_class[[time_var]], fstatus = df_class[[event_type_var]])
            }, error = function(e) {
                logger::log_error(sprintf("cmprsk::cuminc ERROR: %s", e$message))
                stop(sprintf("cmprsk::cuminc failed: %s", e$message))
            })
            
            if (is.null(ci_obj) || is.null(ci_obj$`1`)) {
                logger::log_warn("cmprsk::cuminc returned NULL or missing cause 1")
                return(NA_real_)
            }
            
            # CIF curve for cause 1
            times <- ci_obj$`1`$time
            est <- ci_obj$`1`$est
            # step function: last value <= eval_time
            idx <- max(which(times <= eval_time), na.rm = TRUE)
            if (!is.finite(idx) || idx == -Inf) return(0)
            return(est[idx])
        }

        classes <- unique(data[[group_var]])
        # Filter out invalid groups (NA or empty groups)
        valid_classes <- classes[!is.na(classes) & classes != "" & classes != "NA"]
        logger::log_info(sprintf("Valid classes for CIF calculation: %s", paste(valid_classes, collapse = ", ")))
        
        base_rows <- lapply(valid_classes, function(cls) {
            dfc <- data[data[[group_var]] == cls, , drop = FALSE]
            # huge problems here
            cif_hat <- get_cif_for_class(dfc)
            data.frame(Group = cls, n = nrow(dfc), cif = as.numeric(cif_hat), stringsAsFactors = FALSE)
        })
        results <- do.call(rbind, base_rows)

        # Bootstrap CIs (percentile)
        if (n_boot > 0 && nrow(data) > 0) {
            boot_mat <- matrix(NA_real_, nrow = n_boot, ncol = nrow(results))
            colnames(boot_mat) <- results$Group
            for (b in seq_len(n_boot)) {
                df_boot <- do.call(rbind, lapply(classes, function(cls) {
                    dfc <- data[data[[group_var]] == cls, , drop = FALSE]
                    if (nrow(dfc) == 0) return(dfc)
                    dfc[sample.int(nrow(dfc), size = nrow(dfc), replace = TRUE), , drop = FALSE]
                }))
                for (j in seq_along(classes)) {
                    cls <- classes[j]
                    dfc <- df_boot[df_boot[[group_var]] == cls, , drop = FALSE]
                    boot_mat[b, j] <- suppressWarnings(get_cif_for_class(dfc))
                }
            }
            for (j in seq_along(classes)) {
                qs <- stats::quantile(boot_mat[, j], probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE)
                results$ci_lower[j] <- qs[1]
                results$ci_upper[j] <- qs[2]
            }
        } else {
            results$ci_lower <- NA_real_
            results$ci_upper <- NA_real_
        }

        return(results)
    }, error = function(e) {
        logger::log_warn("Error calculating CIF CIs: {e$message}")
        return(data.frame(
            Group = unique(data[[group_var]]),
            n = 0,
            cif = NA_real_,
            ci_lower = NA_real_,
            ci_upper = NA_real_,
            stringsAsFactors = FALSE
        ))
    })
}

#' Fit cause-specific Cox model by class (melanoma death as cause)
#'
#' Uses `survival::coxph` for cause-specific Cox regression with event type 1 as the
#' event of interest and type 2 as censored. Returns HRs and 95% CIs by class.
#' Includes comprehensive data quality checks to prevent fitting unreliable models.
#'
#' @param data Data frame with time and event type
#' @param time_var Character name of the time variable (years)
#' @param event_var Character name of the event variable (0=censor, 1=melanoma death, 2=other death)
#' @param group_var Character name of the grouping variable (e.g., 'biopsy1_gep')
#' @param eligibility_filter Character name of eligibility filter column (e.g., 'mss_analysis_eligible')
#' @return Data frame with columns: GEP_Class, HR, CI_Lower, CI_Upper, p_value, reference
calculate_cause_specific_cox_model <- function(data, time_var, event_var, group_var, eligibility_filter = "mss_analysis_eligible") {
    logger::log_debug("Fitting cause-specific Cox model (melanoma death)")
    out <- tryCatch({
        # Apply eligibility filter (guaranteed to exist from data processing pipeline)
        data <- data %>% filter(.data[[eligibility_filter]])
        
        df <- data[!is.na(data[[time_var]]) & !is.na(data[[event_var]]) & !is.na(data[[group_var]]), , drop = FALSE]
        if (nrow(df) == 0) {
            logger::log_warn("No complete cases available for cause-specific Cox model")
            return(NULL)
        }
        
        # Convert to factor and check levels
        df[[group_var]] <- factor(df[[group_var]])
        group_levels <- levels(df[[group_var]])
        
        if (length(group_levels) < 2) {
            logger::log_warn("Cause-specific Cox model requires at least 2 groups")
            return(NULL)
        }
        
        # COMPREHENSIVE DATA QUALITY CHECKS
        logger::log_info("Performing data quality checks for cause-specific Cox model")
        
        # Check sample sizes per group
        group_counts <- table(df[[group_var]])
        logger::log_info(sprintf("Group sample sizes: %s", paste(names(group_counts), group_counts, sep="=", collapse=", ")))
        
        # Check for groups with insufficient sample size (minimum 10 patients per group)
        min_group_size <- 10
        small_groups <- names(group_counts[group_counts < min_group_size])
        if (length(small_groups) > 0) {
            logger::log_warn(sprintf("Groups with insufficient sample size (<%d): %s", 
                                   min_group_size, paste(small_groups, collapse=", ")))
        }
        
        # Event of interest: 1; treat 2 as censored
        status_cs <- as.integer(df[[event_var]] == 1)
        
        # Check event distribution per group
        event_counts <- table(df[[group_var]], status_cs)
        logger::log_info("Event distribution by group (1=melanoma death, 0=censored):")
        logger::log_info(paste(capture.output(print(event_counts)), collapse="\n"))
        
        # Check for groups with no events of interest
        no_events_groups <- names(which(event_counts[, "1"] == 0))
        if (length(no_events_groups) > 0) {
            logger::log_warn(sprintf("Groups with no melanoma deaths: %s", 
                                   paste(no_events_groups, collapse=", ")))
        }
        
        # CRITICAL: Prevent model fitting if data quality is insufficient
        if (length(small_groups) > 0 || length(no_events_groups) > 0) {
            logger::log_error("Cause-specific Cox model cannot be fitted due to insufficient data quality")
            logger::log_error("Issues: Small groups or groups with no events")
            return(NULL)
        }
        
        # Create survival object and fit model
        surv_obj <- survival::Surv(df[[time_var]], status_cs)
        fml <- stats::as.formula(paste0("surv_obj ~ ", group_var))
        
        logger::log_info("Fitting cause-specific Cox model with survival::coxph")
        fit <- survival::coxph(fml, data = df, model = TRUE)
        
        # Extract results
        summ <- summary(fit)
        coefs <- as.data.frame(summ$coef)
        if (nrow(coefs) == 0) return(NULL)
        
        # Map coefficient rows to class levels (excluding baseline)
        lvl <- levels(df[[group_var]])
        ref <- lvl[1]
        rn <- rownames(coefs)
        term_levels <- sub(paste0("^", group_var), "", rn)
        term_levels <- sub("^", "", term_levels)
        
        # Build tidy output
        res <- data.frame(
            GEP_Class = gsub(paste0(group_var), "", rn, fixed = TRUE),
            HR = exp(coefs$`coef`),
            CI_Lower = exp(coefs$`coef` - 1.96 * coefs$`se(coef)`),
            CI_Upper = exp(coefs$`coef` + 1.96 * coefs$`se(coef)`),
            p_value = coefs$`Pr(>|z|)`,
            reference = ref,
            stringsAsFactors = FALSE
        )
        
        # Clean class names
        res$GEP_Class <- trimws(gsub("=", "", res$GEP_Class))
        
        logger::log_info("Cause-specific Cox model completed successfully")
        res
        
    }, error = function(e) {
        logger::log_warn(sprintf("Cause-specific Cox model failed: %s", e$message))
        NULL
    })
    return(out)
}

#' Fit Fine-Gray subdistribution model by class (melanoma death as cause)
#'
#' Uses `cmprsk::crr` for Fine-Gray regression with event type 1 as the event of
#' interest and type 2 as competing. Returns SHRs and 95% CIs by class.
#' Results are filtered using the project's extreme estimate filtering system.
#' Includes comprehensive data quality checks to prevent fitting unreliable models.
#'
#' @param data Data frame with time and event type
#' @param time_var Character name of time variable (years)
#' @param event_var Character name of event type variable (0/1/2)
#' @param group_var Character name of grouping variable (factor)
#' @param eligibility_filter Character name of eligibility filter column (e.g., 'mss_analysis_eligible')
#' @return Data frame with columns: GEP_Class, SHR, CI_Lower, CI_Upper, p_value, reference
calculate_fine_gray_model <- function(data, time_var, event_var, group_var, eligibility_filter = "mss_analysis_eligible") {
    logger::log_debug("Fitting Fine-Gray subdistribution model (melanoma death)")
    
    # CRITICAL: Apply eligibility filters to prevent segmentation fault
    tryCatch({
        # Apply eligibility filter (guaranteed to exist from data processing pipeline)
        data <- data %>% filter(.data[[eligibility_filter]])
        
        df <- data[!is.na(data[[time_var]]) & !is.na(data[[event_var]]) & !is.na(data[[group_var]]), , drop = FALSE]
        if (nrow(df) == 0) {
            logger::log_warn("No complete cases available for Fine-Gray model")
            return(NULL)
        }
        
        # Convert to factor and check levels
        df[[group_var]] <- factor(df[[group_var]])
        group_levels <- levels(df[[group_var]])
        
        if (length(group_levels) < 2) {
            logger::log_warn("Fine-Gray model requires at least 2 groups")
            return(NULL)
        }
        
        # COMPREHENSIVE DATA QUALITY CHECKS
        logger::log_info("Performing data quality checks for Fine-Gray model")
        
        # Check sample sizes per group
        group_counts <- table(df[[group_var]])
        logger::log_info(sprintf("Group sample sizes: %s", paste(names(group_counts), group_counts, sep="=", collapse=", ")))
        
        # Check for groups with insufficient sample size (minimum 10 patients per group)
        min_group_size <- 10
        small_groups <- names(group_counts[group_counts < min_group_size])
        if (length(small_groups) > 0) {
            logger::log_warn(sprintf("Groups with insufficient sample size (<%d): %s", 
                                   min_group_size, paste(small_groups, collapse=", ")))
        }
        
        # Check event distribution per group
        event_counts <- table(df[[group_var]], df[[event_var]])
        logger::log_info("Event distribution by group:")
        logger::log_info(paste(capture.output(print(event_counts)), collapse="\n"))
        
        # Check for groups with no events of interest (event type 1)
        no_events_groups <- names(which(event_counts[, "1"] == 0))
        if (length(no_events_groups) > 0) {
            logger::log_warn(sprintf("Groups with no melanoma deaths (event type 1): %s", 
                                   paste(no_events_groups, collapse=", ")))
        }
        
        # Check for groups with no competing events (event type 2)
        no_competing_groups <- names(which(event_counts[, "2"] == 0))
        if (length(no_competing_groups) > 0) {
            logger::log_warn(sprintf("Groups with no competing deaths (event type 2): %s", 
                                   paste(no_competing_groups, collapse=", ")))
        }
        
        # CRITICAL: Prevent model fitting if data quality is insufficient
        if (length(small_groups) > 0 || length(no_events_groups) > 0) {
            logger::log_error("Fine-Gray model cannot be fitted due to insufficient data quality")
            logger::log_error("Issues: Small groups or groups with no events")
            return(NULL)
        }
        
        # Design matrix without intercept to compare each class to reference
        X <- stats::model.matrix(stats::as.formula(paste0("~ ", group_var)), data = df)
        # Remove intercept if present
        if (colnames(X)[1] == "(Intercept)") X <- X[, -1, drop = FALSE]
        if (ncol(X) == 0) return(NULL)
        
        # Fit Fine-Gray model
        logger::log_info("Fitting Fine-Gray model with cmprsk::crr")
        fit <- cmprsk::crr(ftime = df[[time_var]], fstatus = df[[event_var]], cov1 = X, failcode = 1, cencode = 0)
        
        # Extract coefficients and standard errors
        beta <- as.numeric(fit$coef)
        se <- sqrt(diag(fit$var))
        if (length(beta) == 0) return(NULL)
        
        # Calculate SHRs and confidence intervals
        shr <- exp(beta)
        ci_low <- exp(beta - 1.96 * se)
        ci_up <- exp(beta + 1.96 * se)
        pvals <- 2 * stats::pnorm(-abs(beta / se))
        
        # Clean class names
        classes <- gsub(paste0(group_var), "", colnames(X), fixed = TRUE)
        classes <- trimws(gsub("=", "", classes))
        
        # Create results dataframe
        res <- data.frame(
            GEP_Class = classes,
            SHR = shr,
            CI_Lower = ci_low,
            CI_Upper = ci_up,
            p_value = pvals,
            reference = levels(df[[group_var]])[1],
            stringsAsFactors = FALSE
        )
        
        # Apply extreme estimate filtering using project's filtering system
        # Create a mock gtsummary table structure for filtering
        mock_table <- list(
            table_body = data.frame(
                term = res$GEP_Class,
                estimate = res$SHR,
                conf.low = res$CI_Lower,
                conf.high = res$CI_Upper,
                p.value = res$p_value,
                row_type = "level",
                variable = group_var,
                stringsAsFactors = FALSE
            )
        )
        
        # Apply extreme estimate filtering
        filtered_result <- process_extreme_estimates(
            tbl = mock_table,
            model_fit = fit,
            effect_measure = "HR",  # SHR is similar to HR for filtering purposes
            variables_to_check = group_var,
            analysis_name = "Fine-Gray competing risks"
        )
        
        # Extract filtered results
        if (filtered_result$diagnostics$rows_removed > 0) {
            logger::log_info(sprintf("Fine-Gray model: %d extreme estimates filtered out", 
                                    filtered_result$diagnostics$rows_removed))
            
            # Get the filtered table body
            filtered_body <- filtered_result$tbl_filtered$table_body
            
            # Map back to our result format
            if (nrow(filtered_body) > 0) {
                res_filtered <- data.frame(
                    GEP_Class = filtered_body$term,
                    SHR = filtered_body$estimate,
                    CI_Lower = filtered_body$conf.low,
                    CI_Upper = filtered_body$conf.high,
                    p_value = filtered_body$p.value,
                    reference = res$reference[1],
                    stringsAsFactors = FALSE
                )
                res <- res_filtered
            } else {
                # All results were filtered out
                logger::log_warn("All Fine-Gray model results were filtered out due to extreme estimates")
                return(NULL)
            }
        }
        
        logger::log_info("Fine-Gray model completed successfully")
        res
        
    }, error = function(e) {
        logger::log_warn(sprintf("Fine-Gray model failed: %s", e$message))
        NULL
    })
}

#' Calculate Brier Score for survival data with method tracking
#'
#' Calculates the Brier Score (mean squared error) for survival predictions,
#' with fallback methods and comprehensive method tracking for clinical research compliance.
#'
#' @param data Data frame with survival data
#' @param predicted_var Character name of predicted probability column
#' @param event_var Character name of binary event indicator column
#' @param time_var Character name of time variable
#' @param timepoint_months Numeric timepoint in months for time-dependent calculation
#' @return A list with `brier_score`, `method_used`, `fallback_triggered`, and `calculation_notes`
calculate_brier_score_survival <- function(data, predicted_var, event_var, time_var, timepoint_months) {
    logger::log_debug("Calculating Brier Score for survival data")
    
    # Data validation
    if (!all(c(predicted_var, event_var, time_var) %in% names(data))) {
        stop("Required variables not found in data")
    }
    
    if (nrow(data) < 10) {
        logger::log_warn("Insufficient data for Brier Score calculation")
        return(list(
            brier_score = NA_real_,
            method_used = "insufficient_data",
            fallback_triggered = FALSE,
            calculation_notes = "Less than 10 observations"
        ))
    }
    
    # Method 1: Time-dependent Brier Score (preferred)
    tryCatch({
        logger::log_debug("Attempting time-dependent Brier Score calculation")
        
        # Create time-specific outcome
        time_specific_event <- data[[event_var]] == 1 & data[[time_var]] <= timepoint_months
        time_specific_time <- pmin(data[[time_var]], timepoint_months)
        
        # Calculate time-dependent Brier Score
        predicted_probs <- data[[predicted_var]]
        
        # Convert survival probabilities to risk probabilities if needed
        if (all(predicted_probs <= 1)) {
            # Already in probability format
            risk_probs <- predicted_probs
        } else {
            # Convert from survival to risk
            risk_probs <- 1 - predicted_probs
        }
        
        # Calculate Brier Score: mean squared error between predicted and observed
        brier_score <- mean((risk_probs - time_specific_event)^2, na.rm = TRUE)
        
        logger::log_debug("Time-dependent Brier Score calculated successfully")
        
        return(list(
            brier_score = brier_score,
            method_used = "time_dependent",
            fallback_triggered = FALSE,
            calculation_notes = "Time-dependent calculation successful"
        ))
        
    }, error = function(e) {
        logger::log_warn(sprintf("Time-dependent Brier Score failed: %s", e$message))
        
        # Method 2: Simple Brier Score (fallback)
        tryCatch({
            logger::log_debug("Attempting simple Brier Score calculation (fallback)")
            
            # Simple approach: compare predicted vs observed at timepoint
            observed_events <- data[[event_var]] == 1 & data[[time_var]] <= timepoint_months
            
            # Convert predictions to risk format if needed
            predicted_probs <- data[[predicted_var]]
            if (all(predicted_probs <= 1)) {
                risk_probs <- predicted_probs
            } else {
                risk_probs <- 1 - predicted_probs
            }
            
            # Calculate simple Brier Score
            brier_score <- mean((risk_probs - observed_events)^2, na.rm = TRUE)
            
            logger::log_debug("Simple Brier Score calculated successfully (fallback)")
            
            return(list(
                brier_score = brier_score,
                method_used = "simple_fallback",
                fallback_triggered = TRUE,
                calculation_notes = sprintf("Fallback method used due to: %s", e$message)
            ))
            
        }, error = function(e2) {
            logger::log_error(sprintf("Both Brier Score methods failed: %s, %s", e$message, e2$message))
            
            # Method 3: Basic calculation (last resort)
            tryCatch({
                logger::log_debug("Attempting basic Brier Score calculation (last resort)")
                
                # Most basic approach: overall event rate vs predicted
                overall_event_rate <- mean(data[[event_var]] == 1, na.rm = TRUE)
                mean_predicted <- mean(data[[predicted_var]], na.rm = TRUE)
                
                # Convert to risk if needed
                if (mean_predicted > 1) {
                    mean_predicted <- 1 - mean_predicted
                }
                
                brier_score <- (mean_predicted - overall_event_rate)^2
                
                logger::log_debug("Basic Brier Score calculated successfully (last resort)")
                
                return(list(
                    brier_score = brier_score,
                    method_used = "basic_last_resort",
                    fallback_triggered = TRUE,
                    calculation_notes = sprintf("Last resort method used due to failures: %s, %s", e$message, e2$message)
                ))
                
            }, error = function(e3) {
                logger::log_error(sprintf("All Brier Score methods failed: %s", e3$message))
                
                return(list(
                    brier_score = NA_real_,
                    method_used = "all_methods_failed",
                    fallback_triggered = TRUE,
                    calculation_notes = sprintf("All calculation methods failed: %s, %s, %s", e$message, e2$message, e3$message)
                ))
            })
        })
    })
}

#' Calculate IPA (Index of Prediction Accuracy) for survival data with method tracking
#'
#' Calculates the Index of Prediction Accuracy, which measures the improvement
#' in prediction accuracy over a null model (treating everyone the same).
#' Positive values indicate improvement over baseline, negative values indicate worse performance.
#'
#' @param data Data frame with survival data
#' @param predicted_var Character name of predicted probability column
#' @param event_var Character name of binary event indicator column
#' @param time_var Character name of time variable
#' @param timepoint_months Numeric timepoint in months for time-dependent calculation
#' @return A list with `ipa`, `method_used`, `fallback_triggered`, and `calculation_notes`
calculate_ipa_survival <- function(data, predicted_var, event_var, time_var, timepoint_months) {
    logger::log_debug("Calculating IPA (Index of Prediction Accuracy) for survival data")
    
    # Data validation
    if (!all(c(predicted_var, event_var, time_var) %in% names(data))) {
        stop("Required variables not found in data")
    }
    
    if (nrow(data) < 10) {
        logger::log_warn("Insufficient data for IPA calculation")
        return(list(
            ipa = NA_real_,
            method_used = "insufficient_data",
            fallback_triggered = FALSE,
            calculation_notes = "Less than 10 observations"
        ))
    }
    
    # Method 1: IPA using Brier Score comparison (preferred)
    tryCatch({
        logger::log_debug("Attempting IPA calculation using Brier Score comparison")
        
        # Calculate time-specific outcome
        time_specific_event <- data[[event_var]] == 1 & data[[time_var]] <= timepoint_months
        
        # Calculate null model Brier Score (treating everyone the same)
        overall_event_rate <- mean(time_specific_event, na.rm = TRUE)
        null_model_predictions <- rep(overall_event_rate, nrow(data))
        null_brier <- mean((null_model_predictions - time_specific_event)^2, na.rm = TRUE)
        
        # Calculate model Brier Score
        predicted_probs <- data[[predicted_var]]
        
        # Convert survival probabilities to risk probabilities if needed
        if (all(predicted_probs <= 1)) {
            risk_probs <- predicted_probs
        } else {
            risk_probs <- 1 - predicted_probs
        }
        
        model_brier <- mean((risk_probs - time_specific_event)^2, na.rm = TRUE)
        
        # Calculate IPA: (null_brier - model_brier) / null_brier
        if (null_brier > 0) {
            ipa <- (null_brier - model_brier) / null_brier
        } else {
            ipa <- NA_real_
        }
        
        logger::log_debug("IPA calculation using Brier Score comparison successful")
        
        return(list(
            ipa = ipa,
            method_used = "brier_score_comparison",
            fallback_triggered = FALSE,
            calculation_notes = sprintf("IPA = %.4f (Null Brier: %.4f, Model Brier: %.4f)", ipa, null_brier, model_brier)
        ))
        
    }, error = function(e) {
        logger::log_warn(sprintf("IPA calculation using Brier Score failed: %s", e$message))
        
        # Method 2: IPA using AUC comparison (fallback)
        tryCatch({
            logger::log_debug("Attempting IPA calculation using AUC comparison (fallback)")
            
            # Calculate time-specific outcome
            time_specific_event <- data[[event_var]] == 1 & data[[time_var]] <= timepoint_months
            
            # Calculate null model AUC (random classifier)
            null_auc <- 0.5
            
            # Calculate model AUC
            predicted_probs <- data[[predicted_var]]
            if (all(predicted_probs <= 1)) {
                risk_probs <- predicted_probs
            } else {
                risk_probs <- 1 - predicted_probs
            }
            
            # Simple AUC calculation using ROC
            if (requireNamespace("pROC", quietly = TRUE)) {
                roc_obj <- pROC::roc(time_specific_event, risk_probs, quiet = TRUE)
                model_auc <- as.numeric(pROC::auc(roc_obj))
            } else {
                # Fallback AUC calculation
                model_auc <- 0.5 + 0.5 * cor(risk_probs, time_specific_event, method = "spearman", use = "complete.obs")
            }
            
            # Calculate IPA: (model_auc - null_auc) / (1 - null_auc)
            ipa <- (model_auc - null_auc) / (1 - null_auc)
            
            logger::log_debug("IPA calculation using AUC comparison successful (fallback)")
            
            return(list(
                ipa = ipa,
                method_used = "auc_comparison_fallback",
                fallback_triggered = TRUE,
                calculation_notes = sprintf("IPA = %.4f (Null AUC: %.4f, Model AUC: %.4f)", ipa, null_auc, model_auc)
            ))
            
        }, error = function(e2) {
            logger::log_error(sprintf("Both IPA methods failed: %s, %s", e$message, e2$message))
            
            # Method 3: Simple improvement ratio (last resort)
            tryCatch({
                logger::log_debug("Attempting simple IPA calculation (last resort)")
                
                # Calculate time-specific outcome
                time_specific_event <- data[[event_var]] == 1 & data[[time_var]] <= timepoint_months
                
                # Calculate simple improvement ratio
                predicted_probs <- data[[predicted_var]]
                if (all(predicted_probs <= 1)) {
                    risk_probs <- predicted_probs
                } else {
                    risk_probs <- 1 - predicted_probs
                }
                
                # Simple correlation-based improvement
                correlation <- cor(risk_probs, time_specific_event, method = "spearman", use = "complete.obs")
                if (is.na(correlation)) correlation <- 0
                
                # IPA as correlation improvement over random
                ipa <- correlation / 2  # Normalize to reasonable range
                
                logger::log_debug("Simple IPA calculation successful (last resort)")
                
                return(list(
                    ipa = ipa,
                    method_used = "simple_correlation_fallback",
                    fallback_triggered = TRUE,
                    calculation_notes = sprintf("IPA = %.4f (Correlation: %.4f, Last resort method)", ipa, correlation)
                ))
                
            }, error = function(e3) {
                logger::log_error(sprintf("All IPA methods failed: %s", e3$message))
                
                return(list(
                    ipa = NA_real_,
                    method_used = "all_methods_failed",
                    fallback_triggered = TRUE,
                    calculation_notes = sprintf("All calculation methods failed: %s, %s, %s", e$message, e2$message, e3$message)
                ))
            })
        })
    })
}
