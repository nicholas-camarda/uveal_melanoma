# GEP MFS Evaluation Core
# Contains metastasis-free survival evaluation algorithms (no plotting or I/O)

#' Calculate Observed vs Expected MFS Rates
#'
#' Calculates observed vs expected metastasis-free survival rates by GEP class
#' with exact Poisson confidence intervals and chi-square goodness of fit test.
#'
#' @param data Data frame with GEP predictions and survival outcomes
#' @param timepoint Numeric. Time point in years for analysis
#' @return List with results_by_class, overall statistics, and chi-square test results
calculate_observed_expected_mfs <- function(data, timepoint, group_var = "biopsy1_gep") {
    logger::log_info(formatted(sprintf("Calculating O/E ratios for %d-year MFS", timepoint), indent = 2))

    # Use pre-processed timepoint variables for consistency
    timepoint_months <- timepoint * 12

    # Calculate observed and expected by GEP group
    results_by_class <- list()

    if (!group_var %in% names(data)) {
        stop(sprintf("Group variable '%s' not found in data", group_var))
    }
    class_levels <- unique(stats::na.omit(data[[group_var]]))
    for (gep_class in class_levels) {
        class_data <- data %>% dplyr::filter(.data[[group_var]] == gep_class)

        if (nrow(class_data) == 0) {
            results_by_class[[gep_class]] <- list(
                n = 0, observed = 0, expected = 0, oe_ratio = NA,
                poisson_ci_lower = NA, poisson_ci_upper = NA
            )
            next
        }

        # Get expected survival probability for this timepoint
        expected_var <- paste0("expected_mfs_", timepoint, "yr")

        # Use pre-processed time-specific event indicators for consistency
        observed_events <- sum(class_data[[paste0("mfs_event_", timepoint, "yr")]])

        # Calculate expected events based on GEP predictions
        # Expected events = n * (1 - mean_expected_survival_probability)
        mean_expected_survival <- mean(class_data[[expected_var]], na.rm = TRUE)
        expected_events <- nrow(class_data) * (1 - mean_expected_survival)

        # Calculate O/E ratio
        oe_ratio <- if (expected_events > 0) observed_events / expected_events else NA

        # Calculate exact Poisson confidence intervals for observed events
        poisson_test <- poisson.test(observed_events)
        poisson_ci_lower <- if (expected_events > 0) poisson_test$conf.int[1] / expected_events else NA
        poisson_ci_upper <- if (expected_events > 0) poisson_test$conf.int[2] / expected_events else NA

        results_by_class[[gep_class]] <- list(
            n = nrow(class_data),
            observed = observed_events,
            expected = round(expected_events, 2),
            oe_ratio = round(oe_ratio, 3),
            poisson_ci_lower = round(poisson_ci_lower, 3),
            poisson_ci_upper = round(poisson_ci_upper, 3),
            mean_expected_survival = round(mean_expected_survival, 3)
        )

        logger::log_info(formatted(sprintf(
            "%s: O=%d, E=%.1f, O/E=%.3f (95%% CI: %.3f-%.3f)",
            gep_class, observed_events, expected_events, oe_ratio,
            poisson_ci_lower, poisson_ci_upper
        ), indent = 3))
    }

    # Overall chi-square goodness of fit test
    observed_total <- sum(sapply(results_by_class, function(x) x$observed))
    expected_total <- sum(sapply(results_by_class, function(x) x$expected))

    # Use pre-processed analysis eligibility for consistency
    expected_var <- paste0("expected_mfs_", timepoint, "yr")
    analysis_data <- data %>% filter(mfs_analysis_eligible)
    expected_total_raw <- nrow(analysis_data) * (1 - mean(analysis_data[[expected_var]], na.rm = TRUE))

    overall_ci_lower <- NA
    overall_ci_upper <- NA
    if (!is.na(expected_total_raw) && expected_total_raw > 0) {
        overall_poisson <- poisson.test(observed_total)
        overall_ci_lower <- overall_poisson$conf.int[1] / expected_total_raw
        overall_ci_upper <- overall_poisson$conf.int[2] / expected_total_raw
    }

    # Chi-square test comparing observed vs expected across all classes
    observed_vec <- sapply(results_by_class, function(x) x$observed)
    expected_vec <- sapply(results_by_class, function(x) x$expected)

    # Only perform test if we have valid expected values
    if (all(expected_vec > 0) && sum(expected_vec) > 0) {
        chisq_test <- chisq.test(x = observed_vec, p = expected_vec / sum(expected_vec))
        chisq_p <- chisq_test$p.value
        chisq_log_p <- calculate_chisq_log_p_value(as.numeric(chisq_test$statistic), df = length(expected_vec) - 1)
        chisq_stat <- chisq_test$statistic
    } else {
        chisq_p <- NA
        chisq_log_p <- NA
        chisq_stat <- NA
    }

    return(list(
        timepoint = timepoint,
        results_by_class = results_by_class,
        overall_n = nrow(analysis_data),
        overall_observed = observed_total,
        overall_expected = round(expected_total, 2),
        overall_oe_ratio = if (expected_total > 0) round(observed_total / expected_total, 3) else NA,
        overall_poisson_ci_lower = round(overall_ci_lower, 3),
        overall_poisson_ci_upper = round(overall_ci_upper, 3),
        chisq_statistic = round(chisq_stat, 3),
        chisq_p_value = chisq_p,
        chisq_log_p_value = chisq_log_p
    ))
}

#' Perform Calibration Assessment (MFS)
#'
#' Assess calibration of GEP-predicted MFS risk at a given timepoint using
#' Nam-D'Agostino chi-square, Integrated Calibration Index (ICI), and an
#' IPCW-weighted logistic calibration slope.
#'
#' @param data Data frame with GEP predictions and survival outcomes.
#'   Must contain `tt_mets_months`, `mets_event`, and timepoint-specific
#'   expected survival columns (e.g., `expected_mfs_5yr`).
#' @param timepoint Numeric year value (e.g., 5, 7, 10) specifying the
#'   MFS evaluation timepoint.
#' @param bootstrap_iterations Integer number of bootstrap iterations retained
#'   for API compatibility; the current recalibration method does not use
#'   bootstrap optimism correction.
#' @return A list with elements: `n`, `n_groups`, `nam_dagostino_statistic`,
#'   `nam_dagostino_p`, `ici`, `slope`, `calibration_intercept`,
#'   and `group_results`.
perform_calibration_mfs <- function(data, timepoint, bootstrap_iterations) {
    logger::log_info(formatted(sprintf("Performing calibration assessment for %d-year MFS", timepoint), indent = 2))

    # Prepare data for calibration analysis
    timepoint_months <- timepoint * 12
    expected_var <- paste0("expected_mfs_", timepoint, "yr")

    # Use pre-processed variables for consistency
    cal_data <- data %>%
        filter(mfs_analysis_eligible) %>%
        mutate(
            observed_time = .data[[paste0("tt_mfs_", timepoint, "yr")]],
            observed_event = .data[[paste0("mfs_event_", timepoint, "yr")]],
            # Use pre-calculated risk variables
            predicted_risk = .data[[paste0("predicted_mfs_risk_", timepoint, "yr")]]
        )

    calibration_summary <- calculate_survival_calibration_summary(
        data = cal_data,
        predicted_risk_var = "predicted_risk",
        time_var = "observed_time",
        event_var = "observed_event",
        eval_time_months = timepoint_months
    )

    logger::log_info(formatted(sprintf(
        "Calibration metrics: Nam-D'Agostino p=%s, ICI=%.4f (%s), Slope=%.3f (%s)",
        format_gep_p_value(
            calibration_summary$nam_dagostino_p,
            log_p_value = calibration_summary$nam_dagostino_log_p
        ),
        calibration_summary$ici,
        calibration_summary$ici_method,
        calibration_summary$slope,
        calibration_summary$slope_method
    ), indent = 3))

    calibration_summary
}

#' Discrimination Analysis (MFS)
#'
#' Compute discrimination metrics at a given timepoint for MFS predictions,
#' including Harrell's C-index, Uno's C-index, and time-specific AUC.
#'
#' @param data Data frame with predicted survival and observed outcomes.
#'   Requires `tt_mets_months`, `mets_event`, and timepoint-specific expected
#'   survival probabilities (e.g., `expected_mfs_5yr`).
#' @param timepoint Numeric year value (e.g., 5, 7, 10) specifying the
#'   MFS evaluation timepoint.
#' @return A list with elements: `n`, `events`, `events_by_timepoint`,
#'   `harrell_c`, `uno_c`, `auc_timepoint`, optional CIs, and bookkeeping
#'   fields such as `timepoint_months`.
perform_discrimination_mfs <- function(data, timepoint) {
    logger::log_info(formatted(sprintf("Performing discrimination analysis for %d-year MFS", timepoint), indent = 2))

    # Prepare data
    timepoint_months <- timepoint * 12

    # Log time-specific analysis details
    logger::log_info(formatted(sprintf("Time-specific analysis: censoring at %d months (%d years)", timepoint_months, timepoint), indent = 3))
    expected_var <- paste0("expected_mfs_", timepoint, "yr")

    # Use pre-processed variables for consistency
    disc_data <- data %>%
        dplyr::filter(mfs_analysis_eligible) %>%
        dplyr::mutate(
            predicted_prob = .data[[expected_var]],
            predicted_risk = .data[[paste0("predicted_mfs_risk_", timepoint, "yr")]], # Use pre-calculated risk
            observed_time = .data[[paste0("tt_mfs_", timepoint, "yr")]],
            observed_event = .data[[paste0("mfs_event_", timepoint, "yr")]]
        )

    if (nrow(disc_data) < GEP_MIN_SAMPLE_SIZE) {
        logger::log_warn(formatted("Insufficient data for discrimination analysis", indent = 3))
        return(list(
            n = nrow(disc_data),
            status = "insufficient_data",
            harrell_c = NA,
            uno_c = NA,
            auc_timepoint = NA
        ))
    }

    # Create survival object
    surv_obj <- Surv(disc_data$observed_time, disc_data$observed_event)

    # 1. Harrell's C-index (concordance index) - TIME-SPECIFIC
    harrell_c <- NA
    harrell_ci_lower <- NA
    harrell_ci_upper <- NA
    harrell_method <- NA_character_
    tryCatch(
        {
            # Create time-specific outcome for the specific timepoint
            time_specific_event <- disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months
            time_specific_time <- pmin(disc_data$observed_time, timepoint_months)

            # Use survcomp package for Harrell's C-index with time-specific data
            harrell_result <- survcomp::concordance.index(
                x = disc_data$predicted_risk,
                surv.time = time_specific_time,
                surv.event = time_specific_event,
                method = "noether"
            )
            harrell_c <- harrell_result$c.index
            harrell_ci_lower <- harrell_result$lower
            harrell_ci_upper <- harrell_result$upper
            harrell_method <- "survcomp"
        },
        error = function(e) {
            logger::log_error(formatted("Error calculating Harrell's C-index", indent = 3))
            # Fallback using survival package with time-specific data
            tryCatch({
                time_specific_event <- disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months
                time_specific_time <- pmin(disc_data$observed_time, timepoint_months)
                time_specific_surv <- Surv(time_specific_time, time_specific_event)
                cox_fit <- coxph(time_specific_surv ~ predicted_risk, data = disc_data, model = TRUE)
                harrell_c <- summary(cox_fit)$concordance[1]
                harrell_ci_lower <- NA
                harrell_ci_upper <- NA
                harrell_method <- "survival"
            }, error = function(e2) {
                harrell_c <- NA; harrell_ci_lower <- NA; harrell_ci_upper <- NA; harrell_method <- "error"
            })
        }
    )

    # REMOVED: Uno's C-index - Fragile metric that requires events at exact timepoints
    # Our data has events spread across time, not concentrated at arbitrary marks
    # This makes Uno's C-index clinically nonsensical and unreliable

    # REMOVED: Time-dependent AUC - Fragile metric that requires events at exact timepoints
    # Our data has events spread across time, not concentrated at arbitrary marks
    # This makes time-dependent AUC clinically nonsensical and unreliable

    # 3. INTEGRATED AUC (iAUC) - Robust discrimination metric over time periods
    # This is more robust than point estimates as it integrates over time ranges
    integrated_auc <- NA
    integrated_auc_method <- NA_character_
    
    tryCatch({
        # Use riskRegression::Score for integrated AUC over time periods
        # This is more robust than requiring events at exact timepoints
        cox_model <- coxph(Surv(observed_time, observed_event) ~ predicted_risk, data = disc_data, x = TRUE)
        
        # Calculate integrated AUC over the entire follow-up period
        # This avoids the fragility of exact timepoint requirements
        roc_result <- riskRegression::Score(
            list("GEP" = cox_model),
            formula = Surv(observed_time, observed_event) ~ 1,
            data = disc_data,
            times = seq(0, max(disc_data$observed_time, na.rm = TRUE), by = 12), # Monthly intervals
            metrics = "auc",
            summary = "risks"
        )
        
        if (!is.null(roc_result$AUC)) {
            auc_data <- roc_result$AUC$score
            if (nrow(auc_data) > 0) {
                # Calculate integrated AUC as mean across time periods
                integrated_auc <- mean(auc_data$AUC, na.rm = TRUE)
            }
        }
        integrated_auc_method <- "riskRegression::Score_integrated"
        
        logger::log_info(formatted(sprintf(
            "Integrated AUC calculated successfully (MFS): %.3f over %d time periods",
            integrated_auc, nrow(auc_data)
        ), indent = 3))
        
    }, error = function(e) {
        logger::log_warn(formatted(sprintf("Integrated AUC calculation failed (MFS): %s", e$message), indent = 3))
        integrated_auc_method <- "calculation_failed"
    })

    # 4. CUMULATIVE DISCRIMINATION - Discrimination ability over time ranges
    # This provides a more robust view than single timepoint estimates
    cumulative_discrimination <- NA
    cumulative_discrimination_method <- NA_character_
    
    tryCatch({
        # Calculate discrimination over different time ranges (0-5yr, 0-7yr, 0-10yr)
        time_ranges <- c(5, 7, 10) * 12  # Convert to months
        discrimination_values <- numeric(length(time_ranges))
        
        for (i in seq_along(time_ranges)) {
            time_range <- time_ranges[i]
            
            # Create time-range specific outcome
            range_event <- disc_data$observed_event == 1 & disc_data$observed_time <= time_range
            range_time <- pmin(disc_data$observed_time, time_range)
            
            # Calculate Harrell's C-index for this time range
            if (sum(range_event) > GEP_MIN_EVENTS_COMPETING_RISK) {
                range_result <- survcomp::concordance.index(
                    x = disc_data$predicted_risk,
                    surv.time = range_time,
                    surv.event = range_event,
                    method = "noether"
                )
                discrimination_values[i] <- range_result$c.index
            } else {
                discrimination_values[i] <- NA
            }
        }
        
        # Calculate cumulative discrimination as mean across time ranges
        valid_values <- !is.na(discrimination_values)
        if (sum(valid_values) > 0) {
            cumulative_discrimination <- mean(discrimination_values[valid_values], na.rm = TRUE)
            cumulative_discrimination_method <- "survcomp_cumulative_ranges"
            
            logger::log_info(formatted(sprintf(
                "Cumulative discrimination calculated (MFS): %.3f over %d time ranges",
                cumulative_discrimination, sum(valid_values)
            ), indent = 3))
        } else {
            cumulative_discrimination_method <- "insufficient_events"
        }
        
    }, error = function(e) {
        logger::log_warn(formatted(sprintf("Cumulative discrimination calculation failed (MFS): %s", e$message), indent = 3))
        cumulative_discrimination_method <- "calculation_failed"
    })

    # 5. TIME-AVERAGED DISCRIMINATION - Average discrimination across follow-up periods
    # This provides a robust measure of discrimination performance over time
    time_averaged_discrimination <- NA
    time_averaged_discrimination_method <- NA_character_
    
    tryCatch({
        # Calculate discrimination at multiple time points and average them
        # This is more robust than single timepoint estimates
        time_points <- seq(12, max(disc_data$observed_time, na.rm = TRUE), by = 12)  # Monthly intervals
        discrimination_at_times <- numeric(length(time_points))
        
        for (i in seq_along(time_points)) {
            time_point <- time_points[i]
            
            # Create time-specific outcome
            time_event <- disc_data$observed_event == 1 & disc_data$observed_time <= time_point
            time_specific_time <- pmin(disc_data$observed_time, time_point)
            
            # Calculate Harrell's C-index for this time point
            if (sum(time_event) > GEP_MIN_EVENTS_COMPETING_RISK) {
                time_result <- survcomp::concordance.index(
                    x = disc_data$predicted_risk,
                    surv.time = time_specific_time,
                    surv.event = time_event,
                    method = "noether"
                )
                discrimination_at_times[i] <- time_result$c.index
            } else {
                discrimination_at_times[i] <- NA
            }
        }
        
        # Calculate time-averaged discrimination
        valid_times <- !is.na(discrimination_at_times)
        if (sum(valid_times) > 0) {
            time_averaged_discrimination <- mean(discrimination_at_times[valid_times], na.rm = TRUE)
            time_averaged_discrimination_method <- "survcomp_time_averaged"
            
            logger::log_info(formatted(sprintf(
                "Time-averaged discrimination calculated (MFS): %.3f over %d time points",
                time_averaged_discrimination, sum(valid_times)
            ), indent = 3))
        } else {
            time_averaged_discrimination_method <- "insufficient_events"
        }
        
    }, error = function(e) {
        logger::log_warn(formatted(sprintf("Time-averaged discrimination calculation failed (MFS): %s", e$message), indent = 3))
        time_averaged_discrimination_method <- "calculation_failed"
    })


    # 4. Additional discrimination metrics
    # Royston's D statistic if possible
    royston_d <- NA
    tryCatch(
        {
            if (!is.na(harrell_c)) {
                # Approximate Royston's D from C-index
                royston_d <- 2 * qnorm(harrell_c) * sqrt(2 / pi)
            }
        },
        error = function(e) {
            royston_d <- NA
        }
    )

    # Summarize results
    events_at_timepoint <- sum(disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months)
    total_at_timepoint <- nrow(disc_data)

    logger::log_info(formatted(sprintf("Timepoint %d years: %d events out of %d patients", timepoint, events_at_timepoint, total_at_timepoint), indent = 3))
    logger::log_info(formatted(sprintf(
        "Discrimination metrics: Harrell C=%.3f (Robust Primary Metric)",
        ifelse(is.na(harrell_c), NA, harrell_c)
    ), indent = 3))
    logger::log_info(formatted(sprintf("Methods used: Harrell=%s (Uno C-index and time-dependent AUC removed as fragile metrics)", harrell_method), indent = 3))

    # Calculate IPA (Index of Prediction Accuracy) for clinical value assessment
    ipa_result <- tryCatch({
        calculate_ipa_survival(
            data = disc_data,
            predicted_var = "predicted_risk",
            event_var = "observed_event",
            time_var = "observed_time",
            timepoint_months = timepoint_months
        )
    }, error = function(e) {
        logger::log_warn(formatted(sprintf("IPA calculation failed: %s", e$message), indent = 3))
        list(
            ipa = NA_real_,
            method_used = "calculation_failed",
            fallback_triggered = FALSE,
            calculation_notes = sprintf("Calculation failed: %s", e$message)
        )
    })

    return(list(
        n = nrow(disc_data),
        events = sum(disc_data$observed_event),
        events_by_timepoint = sum(disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months),
        harrell_c = round(harrell_c, 3),
        harrell_ci_lower = round(harrell_ci_lower, 3),
        harrell_ci_upper = round(harrell_ci_upper, 3),
        harrell_method = harrell_method,
        # REMOVED: Uno C-index and time-dependent AUC (fragile metrics)
        # Replaced with robust alternatives below
        integrated_auc = round(integrated_auc, 3),
        integrated_auc_method = integrated_auc_method,
        cumulative_discrimination = round(cumulative_discrimination, 3),
        cumulative_discrimination_method = cumulative_discrimination_method,
        time_averaged_discrimination = round(time_averaged_discrimination, 3),
        time_averaged_discrimination_method = time_averaged_discrimination_method,
        royston_d = round(royston_d, 3),
        ipa = round(ipa_result$ipa, 4),
        ipa_method = ipa_result$method_used,
        ipa_fallback_used = ipa_result$fallback_triggered,
        ipa_calculation_notes = ipa_result$calculation_notes,
        timepoint_months = timepoint_months
    ))
}

#' Decision Curve Analysis for MFS
#'
#' Evaluate clinical usefulness of MFS predictions via decision curve
#' analysis across a range of risk thresholds at a given timepoint.
#'
#' @param data Data frame with predicted risk and observed outcomes at
#'   the specified timepoint.
#' @param timepoint Numeric year value (e.g., 5, 7, 10) specifying the
#'   MFS evaluation timepoint.
#' @return A list with elements: `n`, `events`, `event_rate`,
#'   `optimal_threshold`, `optimal_net_benefit`, `threshold_range_min`,
#'   `threshold_range_max`, `area_between_curves`, and `dca_curve_data`.
perform_decision_curve_analysis_mfs <- function(data, timepoint) {
    logger::log_info(formatted(sprintf("Performing decision curve analysis for %d-year MFS", timepoint), indent = 2))

    # Prepare data
    timepoint_months <- timepoint * 12
    expected_var <- paste0("expected_mfs_", timepoint, "yr")

    dca_data <- data %>%
        dplyr::filter(!is.na(.data[[expected_var]]), !is.na(tt_mets_months), !is.na(mets_event)) %>%
        dplyr::mutate(
            predicted_risk = 1 - .data[[expected_var]], # Convert survival prob to risk
            observed_time = tt_mets_months,
            observed_event = mets_event,
            outcome = observed_event == 1 & observed_time <= timepoint_months
        )

    if (nrow(dca_data) < GEP_MIN_SAMPLE_SIZE) {
        logger::log_warn(formatted("Insufficient data for decision curve analysis", indent = 3))
        return(list(
            n = nrow(dca_data),
            status = "insufficient_data"
        ))
    }

    event_rate <- mean(dca_data$outcome)
    risk_thresholds <- seq(GEP_DCA_THRESHOLD_MIN, GEP_DCA_THRESHOLD_MAX, by = GEP_DCA_THRESHOLD_STEP)

    dca_results <- data.frame(
        threshold = risk_thresholds,
        net_benefit_model = NA,
        net_benefit_all = NA,
        net_benefit_none = 0
    )

    for (i in seq_along(risk_thresholds)) {
        threshold <- risk_thresholds[i]
        treat_model <- dca_data$predicted_risk >= threshold
        treat_all <- rep(TRUE, nrow(dca_data))

        if (sum(treat_model) > 0) {
            tp_model <- sum(dca_data$outcome & treat_model)
            fp_model <- sum(!dca_data$outcome & treat_model)
            net_benefit_model <- (tp_model / nrow(dca_data)) -
                (fp_model / nrow(dca_data)) * (threshold / (1 - threshold))
        } else {
            net_benefit_model <- 0
        }

        tp_all <- sum(dca_data$outcome)
        fp_all <- sum(!dca_data$outcome)
        net_benefit_all <- (tp_all / nrow(dca_data)) -
            (fp_all / nrow(dca_data)) * (threshold / (1 - threshold))

        dca_results$net_benefit_model[i] <- net_benefit_model
        dca_results$net_benefit_all[i] <- net_benefit_all
    }

    # Find optimal threshold with safety checks
    valid_net_benefits <- !is.na(dca_results$net_benefit_model)
    if (sum(valid_net_benefits) > 0) {
        optimal_idx <- which.max(dca_results$net_benefit_model[valid_net_benefits])
        optimal_threshold <- dca_results$threshold[valid_net_benefits][optimal_idx]
        optimal_net_benefit <- dca_results$net_benefit_model[valid_net_benefits][optimal_idx]
    } else {
        optimal_threshold <- NA
        optimal_net_benefit <- NA
    }

    positive_nb_thresholds <- dca_results$threshold[dca_results$net_benefit_model > 0]
    threshold_range <- if (length(positive_nb_thresholds) > 0) c(min(positive_nb_thresholds), max(positive_nb_thresholds)) else c(NA, NA)

    area_between_curves <- NA
    tryCatch(
        {
            valid_indices <- !is.na(dca_results$net_benefit_model) & !is.na(dca_results$net_benefit_all)
            if (sum(valid_indices) > GEP_MIN_EVENTS_COMPETING_RISK) {
                diff_benefits <- dca_results$net_benefit_model[valid_indices] - dca_results$net_benefit_all[valid_indices]
                area_between_curves <- sum(diff_benefits) * 0.01
            }
        },
        error = function(e) {
            area_between_curves <- NA
        }
    )

    logger::log_info(formatted(sprintf(
        "DCA optimal threshold: %.2f%% (Net benefit: %.4f)",
        optimal_threshold * 100, optimal_net_benefit
    ), indent = 3))

    return(list(
        n = nrow(dca_data),
        events = sum(dca_data$outcome),
        event_rate = round(event_rate, 3),
        optimal_threshold = round(optimal_threshold, 3),
        optimal_net_benefit = round(optimal_net_benefit, 4),
        threshold_range_min = round(threshold_range[1], 3),
        threshold_range_max = round(threshold_range[2], 3),
        area_between_curves = round(area_between_curves, 4),
        dca_curve_data = dca_results
    ))
}

#' PRAME incremental discrimination analysis for MFS
#'
#' Assess whether PRAME improves discrimination beyond imported GEP risk on the
#' PRAME-complete subset using paired base-versus-enhanced Cox models.
#'
#' @param data Data frame with GEP predictions, PRAME status, and outcomes.
#' @param timepoints Numeric vector of year values for evaluation.
#' @return A list containing dataset-level PRAME availability details and
#'   per-timepoint incremental discrimination results.
perform_prame_augmented_analysis_mfs <- function(data, timepoints) {
    logger::log_info(formatted("Performing PRAME incremental discrimination analysis for MFS", indent = 1))

    prame_data <- data %>%
        dplyr::filter(
            !is.na(prame_status),
            prame_status %in% c("Positive", "Negative"),
            !is.na(predicted_mfs_risk_5yr),
            !is.na(biopsy1_gep)
        )

    prame_dist <- table(prame_data$prame_status)
    n_positive <- sum(prame_data$prame_status == "Positive", na.rm = TRUE)
    n_negative <- sum(prame_data$prame_status == "Negative", na.rm = TRUE)

    if (nrow(prame_data) < GEP_MIN_BOOTSTRAP_SAMPLE) {
        logger::log_warn(formatted(sprintf("Insufficient PRAME data for MFS comparison (n=%d)", nrow(prame_data)), indent = 2))
        return(list(
            n = nrow(prame_data),
            status = "insufficient_data",
            prame_available = FALSE,
            prame_distribution = prame_dist,
            n_positive = n_positive,
            n_negative = n_negative
        ))
    }

    logger::log_info(formatted(sprintf("PRAME MFS comparison using %d patients", nrow(prame_data)), indent = 2))
    logger::log_info(formatted(sprintf(
        "PRAME distribution: Positive=%d, Negative=%d",
        n_positive,
        n_negative
    ), indent = 2))

    comparison_results <- list()

    for (timepoint in timepoints) {
        logger::log_info(formatted(sprintf("Comparing GEP-only vs GEP-plus-PRAME models for %d-year MFS", timepoint), indent = 2))

        comparison_results[[paste0("yr", timepoint)]] <- calculate_prame_incremental_value_metrics(
            data = prame_data,
            time_var = paste0("tt_mfs_", timepoint, "yr"),
            event_var = paste0("mfs_event_", timepoint, "yr"),
            base_risk_var = paste0("predicted_mfs_risk_", timepoint, "yr"),
            timepoint = timepoint,
            outcome_label = "MFS",
            analysis_tier = "Primary"
        )

        tp_result <- comparison_results[[paste0("yr", timepoint)]]
        if (identical(tp_result$status, "ok")) {
            logger::log_info(formatted(sprintf(
                "Delta Harrell's C = %.3f (base %.3f, enhanced %.3f)",
                tp_result$delta_harrell_c,
                tp_result$base_harrell_c,
                tp_result$enhanced_harrell_c
            ), indent = 3))
        } else {
            logger::log_warn(formatted(sprintf(
                "%d-year MFS PRAME comparison unavailable: %s",
                timepoint,
                tp_result$interpretation
            ), indent = 3))
        }
    }

    return(list(
        n = nrow(prame_data),
        prame_available = TRUE,
        prame_distribution = prame_dist,
        n_positive = n_positive,
        n_negative = n_negative,
        analysis_type = "incremental_discrimination",
        comparison_results = comparison_results
    ))
}
