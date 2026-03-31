# GEP MSS Evaluation Core
# Contains MSS model evaluation algorithms (no plotting or I/O)

#' Estimate MSS Cumulative Incidence at a Horizon
#'
#' Calculates a censoring-aware Aalen-Johansen cumulative incidence estimate for
#' melanoma-specific death at a requested month horizon and converts that
#' estimate to an observed pseudo-event count on the original sample size scale.
#'
#' @param data Data frame containing the time and event columns.
#' @param timepoint_months Numeric horizon in months.
#' @param time_var Character name of the MSS follow-up column.
#' @param melanoma_event_var Character name of the melanoma-death indicator.
#' @param competing_event_var Character name of the competing-death indicator.
#'
#' @return Named list with cumulative incidence, pseudo-event counts, and raw
#'   event bookkeeping.
estimate_mss_cif_at_horizon <- function(data,
                                        timepoint_months,
                                        time_var = "tt_death_months",
                                        melanoma_event_var = "melanoma_death_event",
                                        competing_event_var = "competing_death_event") {
    valid_data <- data %>%
        dplyr::mutate(
            .time_value = suppressWarnings(as.numeric(.data[[time_var]])),
            .melanoma_value = suppressWarnings(as.integer(.data[[melanoma_event_var]])),
            .competing_value = suppressWarnings(as.integer(.data[[competing_event_var]]))
        ) %>%
        dplyr::filter(
            is.finite(.data$.time_value),
            !is.na(.data$.melanoma_value),
            !is.na(.data$.competing_value)
        )

    raw_events_by_horizon <- sum(
        valid_data$.melanoma_value == 1 & valid_data$.time_value <= timepoint_months,
        na.rm = TRUE
    )

    if (nrow(valid_data) == 0) {
        return(list(
            n = 0L,
            cif = NA_real_,
            observed_events = NA_real_,
            raw_events_by_horizon = raw_events_by_horizon,
            observed_method = "aalen_johansen_cif_at_horizon"
        ))
    }

    event_status <- dplyr::case_when(
        valid_data$.melanoma_value == 1 ~ 1L,
        valid_data$.competing_value == 1 ~ 2L,
        TRUE ~ 0L
    )

    if (!any(event_status == 1L)) {
        return(list(
            n = nrow(valid_data),
            cif = 0,
            observed_events = 0,
            raw_events_by_horizon = raw_events_by_horizon,
            observed_method = "aalen_johansen_cif_at_horizon"
        ))
    }

    cif_fit <- cmprsk::cuminc(
        ftime = valid_data$.time_value,
        fstatus = event_status,
        cencode = 0
    )

    curve_names <- names(cif_fit)
    melanoma_curve_name <- curve_names[grepl("(^1$| 1$)", curve_names)][1]
    cif_curve <- if (!is.na(melanoma_curve_name)) cif_fit[[melanoma_curve_name]] else NULL
    cif_value <- if (!is.null(cif_curve) && length(cif_curve$time) > 0) {
        eligible_idx <- which(cif_curve$time <= timepoint_months)
        if (length(eligible_idx) == 0) {
            0
        } else {
            as.numeric(cif_curve$est[max(eligible_idx)])
        }
    } else {
        NA_real_
    }

    list(
        n = nrow(valid_data),
        cif = cif_value,
        observed_events = if (!is.na(cif_value)) nrow(valid_data) * cif_value else NA_real_,
        raw_events_by_horizon = raw_events_by_horizon,
        observed_method = "aalen_johansen_cif_at_horizon"
    )
}

#' Calculate Observed vs Expected MSS Rates
#'
#' Calculates observed vs expected melanoma-specific death rates by GEP class
#' using censoring-aware cumulative incidence estimates at the requested horizon
#' and a Pearson goodness-of-fit comparison against summed expected risks.
#'
#' @param data Data frame with GEP predictions and survival outcomes.
#' @param timepoint Numeric time point in years for analysis.
#' @param group_var Character grouping variable.
#' @param time_var Character follow-up column.
#' @param melanoma_event_var Character melanoma-death indicator column.
#' @param competing_event_var Character competing-death indicator column.
#' @param eligibility_filter Character eligibility flag column.
#'
#' @return Data frame with grouped observed/expected summaries plus overall
#'   attributes matching the legacy MSS shape.
calculate_observed_expected_mss <- function(data,
                                            timepoint,
                                            group_var = "biopsy1_gep",
                                            time_var = "tt_death_months",
                                            melanoma_event_var = "melanoma_death_event",
                                            competing_event_var = "competing_death_event",
                                            eligibility_filter = "mss_analysis_eligible") {
    logger::log_info(formatted(sprintf("Calculating O/E ratios for %d-year MSS", timepoint), indent = 2))

    timepoint_months <- timepoint * 12
    expected_var <- paste0("expected_mss_", timepoint, "yr")

    required_vars <- c(group_var, expected_var, time_var, melanoma_event_var, competing_event_var)
    missing_vars <- setdiff(required_vars, names(data))
    if (length(missing_vars) > 0) {
        stop(sprintf(
            "calculate_observed_expected_mss() requires columns: %s",
            paste(missing_vars, collapse = ", ")
        ))
    }

    grouped_results <- data %>%
        dplyr::filter(!is.na(.data[[group_var]])) %>%
        dplyr::group_by(.data[[group_var]]) %>%
        dplyr::group_modify(function(.x, .y) {
            cif_metrics <- estimate_mss_cif_at_horizon(
                data = .x,
                timepoint_months = timepoint_months,
                time_var = time_var,
                melanoma_event_var = melanoma_event_var,
                competing_event_var = competing_event_var
            )
            expected_events <- sum(1 - .x[[expected_var]], na.rm = TRUE)

            tibble::tibble(
                n = nrow(.x),
                observed = round(cif_metrics$observed_events, 2),
                expected = round(expected_events, 2),
                oe_ratio = ifelse(expected_events > 0, round(cif_metrics$observed_events / expected_events, 3), NA_real_),
                expected_rate = ifelse(nrow(.x) > 0, expected_events / nrow(.x), NA_real_),
                observed_rate = cif_metrics$cif,
                poisson_ci_lower = NA_real_,
                poisson_ci_upper = NA_real_,
                observed_method = cif_metrics$observed_method,
                raw_events_by_horizon = cif_metrics$raw_events_by_horizon
            )
        }) %>%
        dplyr::ungroup()

    analysis_data <- if (eligibility_filter %in% names(data)) {
        data %>% dplyr::filter(.data[[eligibility_filter]])
    } else {
        data
    }

    overall_cif <- estimate_mss_cif_at_horizon(
        data = analysis_data,
        timepoint_months = timepoint_months,
        time_var = time_var,
        melanoma_event_var = melanoma_event_var,
        competing_event_var = competing_event_var
    )
    overall_expected <- sum(1 - analysis_data[[expected_var]], na.rm = TRUE)

    observed_vec <- grouped_results$observed
    expected_vec <- grouped_results$expected
    valid_rows <- is.finite(observed_vec) & is.finite(expected_vec) & expected_vec > 0

    chisq_statistic <- NA_real_
    chisq_p_value <- NA_real_
    chisq_log_p_value <- NA_real_
    if (sum(valid_rows) >= 2) {
        chisq_statistic <- sum(
            (observed_vec[valid_rows] - expected_vec[valid_rows])^2 / expected_vec[valid_rows],
            na.rm = TRUE
        )
        chisq_p_value <- stats::pchisq(
            chisq_statistic,
            df = sum(valid_rows) - 1,
            lower.tail = FALSE
        )
        chisq_log_p_value <- calculate_chisq_log_p_value(chisq_statistic, df = sum(valid_rows) - 1)
    }

    attr(grouped_results, "overall_n") <- nrow(analysis_data)
    attr(grouped_results, "overall_observed") <- round(overall_cif$observed_events, 2)
    attr(grouped_results, "overall_expected") <- round(overall_expected, 2)
    attr(grouped_results, "overall_oe_ratio") <- ifelse(
        overall_expected > 0,
        round(overall_cif$observed_events / overall_expected, 3),
        NA_real_
    )
    attr(grouped_results, "overall_poisson_ci_lower") <- NA_real_
    attr(grouped_results, "overall_poisson_ci_upper") <- NA_real_
    attr(grouped_results, "overall_observed_method") <- overall_cif$observed_method
    attr(grouped_results, "chisq_p_value") <- chisq_p_value
    attr(grouped_results, "chisq_log_p_value") <- chisq_log_p_value
    attr(grouped_results, "chisq_statistic") <- round(chisq_statistic, 3)

    grouped_results
}

#' Perform standard MSS validation
#'
#' Validate melanoma-specific survival (MSS) predictions at a given timepoint
#' by computing observed vs expected rates, calibration, and discrimination
#' metrics using shared calculators.
#'
#' @param data Data frame prepared for MSS analysis
#' @param timepoint Numeric year value for evaluation (e.g., 5, 7, 10)
#' @param bootstrap_iterations Integer number of bootstrap iterations for discrimination metrics where applicable
#' @param time_var Character name of time variable (default 'tt_death_months' for months-based analysis)
#' @param oe_data Optional data frame used only for reader-facing observed vs expected summaries
#' @param group_var Character grouping variable used for observed/expected summaries
#' @return A list with `observed_expected`, `calibration`, `discrimination`, `decision_curve`, and `timepoint`.
perform_standard_mss_validation <- function(data, timepoint, bootstrap_iterations, time_var = "tt_death_months", oe_data = NULL, group_var = "biopsy1_gep") {
    logger::log_debug(sprintf("Performing standard MSS validation for %d-year timepoint", timepoint))

    # Convert timepoint to months for consistency with survival time units
    timepoint_months <- timepoint * 12

    # Use pre-processed time-specific variables for consistency
    analysis_data <- data %>%
        mutate(
            time_to_event = .data[[time_var]],  # Use the specified time variable
            event_occurred = .data[[paste0("mss_event_", timepoint, "yr")]]
        )

    # Calculate observed vs expected rates
    observed_expected <- calculate_observed_expected_mss(
        data = if (is.null(oe_data)) data else oe_data,
        timepoint = timepoint,
        time_var = time_var,
        group_var = group_var
    )

    # Calculate calibration metrics
    calibration_metrics <- calculate_calibration_metrics(
        data = analysis_data,
        expected_var = paste0("expected_mss_", timepoint, "yr"),
        event_var = "event_occurred",
        time_var = "time_to_event",
        eval_time_months = timepoint_months
    )

    # Time-dependent discrimination metrics (Harrell C, Uno C, AUC at timepoint)
    discrimination_metrics <- perform_discrimination_mss(
        data = analysis_data,
        timepoint = timepoint
    )

    # Decision curve analysis at this timepoint
    decision_curve <- perform_decision_curve_analysis_mss(
        data = analysis_data,
        timepoint = timepoint,
        time_unit = "months"
    )

    return(list(
        observed_expected = observed_expected,
        calibration = calibration_metrics,
        discrimination = discrimination_metrics,
        decision_curve = decision_curve,
        timepoint = timepoint
    ))
}

#' Perform competing risk MSS validation
#'
#' Evaluate MSS predictions using competing risks by computing cumulative
#' incidence functions (AJ), cause-specific Cox regression (CSC proxy), and
#' Fine-Gray subdistribution regression (FGR) by GEP class.
#'
#' @param data Data frame prepared for MSS competing risk analysis
#' @param timepoint Numeric year value for evaluation (e.g., 5, 7, 10)
#' @param time_var Character name of time variable (default 'tt_death_months' for months-based competing risks)
#' @param cif_data Optional data frame used only for reader-facing cumulative incidence summaries
#' @param group_var Character grouping variable used for competing-risk summaries
#' @return A list with `cumulative_incidence`, `cause_specific_hazards`, and `timepoint`.
perform_competing_risk_mss_validation <- function(data, timepoint, time_var = "tt_death_months", cif_data = NULL, group_var = "biopsy1_gep") {
    logger::log_debug(sprintf("Performing competing risk MSS validation for %d-year timepoint", timepoint))

    # Convert timepoint to months for consistency with survival time units
    timepoint_months <- timepoint * 12

    # Use pre-processed competing risk variables for consistency
    # Data is already filtered by orchestration layer
    analysis_data <- data %>%
        mutate(
            time_to_event = .data[[time_var]],  # Use the specified time variable
            event_type = .data[[paste0("event_type_mss_", timepoint, "yr")]]
        )

    cif_source_data <- if (is.null(cif_data)) data else cif_data
    cif_analysis_data <- cif_source_data %>%
        mutate(
            time_to_event = .data[[time_var]],
            event_type = .data[[paste0("event_type_mss_", timepoint, "yr")]]
        )

    analysis_feasibility <- assess_competing_risk_feasibility(
        data = analysis_data,
        time_var = "time_to_event",
        event_var = "event_type",
        group_var = group_var,
        eligibility_filter = "mss_analysis_eligible"
    )
    cif_feasibility <- assess_competing_risk_feasibility(
        data = cif_analysis_data,
        time_var = "time_to_event",
        event_var = "event_type",
        group_var = group_var,
        eligibility_filter = "mss_analysis_eligible"
    )
    unexpected_failures <- character()

    # Calculate cumulative incidence functions
    cumulative_incidence <- if (nrow(cif_feasibility$data) > 0) {
        calculate_cumulative_incidence(
            data = cif_feasibility$data,
            time_var = "time_to_event",
            event_var = "event_type",
            group_var = group_var
        )
    } else {
        NULL
    }

    # Add CIF with 95% CI at the evaluation time (bootstrap)
    cif_ci <- tryCatch({
        calculate_cif_by_class_with_ci(
            data = cif_analysis_data,
            time_var = "time_to_event",
            event_type_var = "event_type",
            eval_time = timepoint_months,
            n_boot = GEP_BOOTSTRAP_ITERATIONS,
            group_var = group_var,
            eligibility_filter = "mss_analysis_eligible",
            feasibility = cif_feasibility
        )
    }, error = function(e) {
        unexpected_failures <<- c(unexpected_failures, sprintf("cif_with_ci:%s", e$message))
        logger::log_error(sprintf("Unable to compute CIF CIs: %s", e$message))
        NULL
    })

    # Cause-specific Cox regression (proxy for CSC)
    csc_model <- tryCatch(
        calculate_cause_specific_cox_model(
            data = analysis_data,
            time_var = "time_to_event",
            event_var = "event_type",
            group_var = group_var,
            eligibility_filter = "mss_analysis_eligible",
            feasibility = analysis_feasibility
        ),
        error = function(e) {
            unexpected_failures <<- c(unexpected_failures, sprintf("cause_specific_cox:%s", e$message))
            logger::log_error(sprintf("Cause-specific Cox fitting failed unexpectedly: %s", e$message))
            NULL
        }
    )

    # Fine-Gray subdistribution regression (FGR)
    fgr_model <- tryCatch(
        calculate_fine_gray_model(
            data = analysis_data,
            time_var = "time_to_event",
            event_var = "event_type",
            group_var = group_var,
            eligibility_filter = "mss_analysis_eligible",
            feasibility = analysis_feasibility
        ),
        error = function(e) {
            unexpected_failures <<- c(unexpected_failures, sprintf("fine_gray:%s", e$message))
            logger::log_error(sprintf("Fine-Gray fitting failed unexpectedly: %s", e$message))
            NULL
        }
    )

    return(list(
        cumulative_incidence = cumulative_incidence,
        cause_specific_cox = csc_model,
        fine_gray = fgr_model,
        cif_with_ci = cif_ci,
        feasibility = analysis_feasibility,
        timepoint = timepoint
        ,
        unexpected_failures = unexpected_failures
    ))
}

#' Perform PRAME incremental discrimination analysis for MSS
#'
#' Assess whether PRAME improves discrimination beyond imported GEP risk on the
#' PRAME-complete subset, reported as exploratory MSS support.
#'
#' @param data Data frame prepared for MSS with PRAME status.
#' @param timepoints Numeric vector of year values for evaluation.
#' @return A named list of incremental discrimination results by timepoint.
perform_prame_augmented_analysis_mss <- function(data, timepoints) {
    logger::log_info(formatted("Performing PRAME incremental discrimination analysis (MSS exploratory)", indent = 1))

    prame_data <- data %>%
        dplyr::filter(
            !is.na(prame_status),
            prame_status %in% c("Positive", "Negative"),
            !is.na(predicted_mss_risk_5yr),
            !is.na(tt_death_months),
            !is.na(biopsy1_gep)
        )

    prame_dist <- table(prame_data$prame_status)
    n_positive <- sum(prame_data$prame_status == "Positive", na.rm = TRUE)
    n_negative <- sum(prame_data$prame_status == "Negative", na.rm = TRUE)

    if (nrow(prame_data) < GEP_MIN_BOOTSTRAP_SAMPLE) {
        logger::log_warn(formatted(sprintf("Insufficient PRAME data for MSS analysis (n=%d)", nrow(prame_data)), indent = 2))
        return(list(
            n = nrow(prame_data),
            status = "insufficient_data",
            prame_available = FALSE,
            prame_distribution = prame_dist,
            n_positive = n_positive,
            n_negative = n_negative
        ))
    }

    logger::log_info(formatted(sprintf("MSS PRAME comparison using %d patients", nrow(prame_data)), indent = 2))
    logger::log_info(formatted(sprintf(
        "PRAME distribution (MSS): Positive=%d, Negative=%d",
        n_positive,
        n_negative
    ), indent = 2))

    comparison_results <- list()

    for (timepoint in timepoints) {
        logger::log_info(formatted(sprintf("Comparing GEP-only vs GEP-plus-PRAME models for %d-year MSS", timepoint), indent = 2))

        comparison_results[[paste0("yr", timepoint)]] <- calculate_prame_incremental_value_metrics(
            data = prame_data,
            time_var = paste0("tt_mss_", timepoint, "yr"),
            event_var = paste0("mss_event_", timepoint, "yr"),
            base_risk_var = paste0("predicted_mss_risk_", timepoint, "yr"),
            timepoint = timepoint,
            outcome_label = "MSS",
            analysis_tier = "Exploratory"
        )

        tp_result <- comparison_results[[paste0("yr", timepoint)]]
        if (identical(tp_result$status, "ok")) {
            logger::log_info(formatted(sprintf(
                "MSS delta Harrell's C = %.3f (base %.3f, enhanced %.3f)",
                tp_result$delta_harrell_c,
                tp_result$base_harrell_c,
                tp_result$enhanced_harrell_c
            ), indent = 3))
        } else {
            logger::log_warn(formatted(sprintf(
                "%d-year MSS PRAME comparison unavailable: %s",
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

#' Time-dependent discrimination for MSS at a given timepoint
#'
#' Computes Harrell's C (and CI when available), Uno's C (if survcomp is present),
#' and time-dependent AUC using riskRegression or pROC as fallback. Uses a
#' time-specific event definition at the given timepoint in years.
#'
#' @param data Data frame with columns: time_to_event (years), event_occurred (0/1), and expected_mss_*yr
#' @param timepoint numeric Time in years for evaluation (e.g., 5)
#' @return List of discrimination metrics similar to perform_discrimination_mfs
perform_discrimination_mss <- function(data, timepoint) {
    logger::log_info(formatted(sprintf("Performing discrimination analysis for %d-year MSS", timepoint), indent = 2))

    # Convert timepoint to months for consistency with survival time units
    timepoint_months <- timepoint * 12

    # Prepare data
    expected_var <- paste0("expected_mss_", timepoint, "yr")

    disc_data <- data %>%
        dplyr::filter(!is.na(.data[[expected_var]]), !is.na(time_to_event), !is.na(event_occurred)) %>%
        dplyr::mutate(
            predicted_prob = .data[[expected_var]],
            predicted_risk = 1 - predicted_prob, # Convert survival prob to risk
            observed_time = time_to_event,
            observed_event = as.integer(event_occurred)
        )

    if (nrow(disc_data) < GEP_MIN_SAMPLE_SIZE) {
        logger::log_warn(formatted("Insufficient data for discrimination analysis (MSS)", indent = 3))
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

    # 1. HARRELL'S C-INDEX - PRIMARY DISCRIMINATION METRIC (Robust and Clinically Meaningful)
    # This is our primary discrimination metric because:
    # - It measures overall discrimination ability across the entire follow-up period
    # - It's robust to data distribution and doesn't require events at exact timepoints
    # - It's widely accepted in survival analysis literature
    # - It provides clinically meaningful information about risk stratification
    harrell_c <- NA
    harrell_ci_lower <- NA
    harrell_ci_upper <- NA
    harrell_method <- NA_character_
    tryCatch({
        harrell_result <- survcomp::concordance.index(
            x = disc_data$predicted_risk,
            surv.time = disc_data$observed_time,
            surv.event = disc_data$observed_event,
            method = "noether"
        )
        harrell_c <- harrell_result$c.index
        harrell_ci_lower <- harrell_result$lower
        harrell_ci_upper <- harrell_result$upper
        harrell_method <- "survcomp"
        
        logger::log_info(formatted(sprintf(
            "PRIMARY METRIC: Harrell's C-index calculated successfully (MSS): %.3f (95%% CI: %.3f-%.3f)",
            harrell_c, harrell_ci_lower, harrell_ci_upper
        ), indent = 3))
        
    }, error = function(e) {
        logger::log_error(formatted("Error calculating Harrell's C-index (MSS): %s", e$message), indent = 3)
        harrell_method <- "error"
    })

    # REMOVED: Uno's C-index - Fragile metric that requires events at exact timepoints
    # Our data has events spread across time, not concentrated at arbitrary marks
    # This makes Uno's C-index clinically nonsensical and unreliable

    # 2. INTEGRATED AUC (iAUC) - Robust discrimination metric over time periods
    # This is more robust than point estimates as it integrates over time ranges
    integrated_auc <- NA_real_
    integrated_auc_method <- NA_character_
    integrated_auc_status <- "not_attempted"
    integrated_auc_na_reason <- NA_character_
    integrated_auc_time_periods <- 0L
    
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
        
        auc_data <- NULL
        if (!is.null(roc_result$AUC)) {
            auc_data <- roc_result$AUC$score
            if (nrow(auc_data) > 0 && any(is.finite(auc_data$AUC))) {
                # Calculate integrated AUC as mean across time periods
                integrated_auc <- mean(auc_data$AUC, na.rm = TRUE)
                integrated_auc_status <- "ok"
            } else {
                integrated_auc_status <- "not_estimable"
                integrated_auc_na_reason <- "riskRegression::Score returned no finite AUC estimates"
            }
        }
        integrated_auc_method <- "riskRegression::Score_integrated"
        integrated_auc_time_periods <- ifelse(!is.null(auc_data) && nrow(auc_data) > 0, nrow(auc_data), 0)
        logger::log_info(formatted(sprintf(
            "Integrated AUC calculated successfully (MSS): %.3f over %d time periods",
            integrated_auc, integrated_auc_time_periods
        ), indent = 3))
        
    }, error = function(e) {
        logger::log_warn(formatted(sprintf("Integrated AUC calculation failed (MSS): %s", e$message), indent = 3))
        integrated_auc_method <- "riskRegression::Score_integrated"
        integrated_auc_status <- "not_estimable"
        integrated_auc_na_reason <- e$message
    })

    # 3. CUMULATIVE DISCRIMINATION - Discrimination ability over time ranges
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
                "Cumulative discrimination calculated (MSS): %.3f over %d time ranges",
                cumulative_discrimination, sum(valid_values)
            ), indent = 3))
        } else {
            cumulative_discrimination_method <- "insufficient_events"
        }
        
    }, error = function(e) {
        logger::log_warn(formatted(sprintf("Cumulative discrimination calculation failed (MSS): %s", e$message), indent = 3))
        cumulative_discrimination_method <- "calculation_failed"
    })

    # 4. TIME-AVERAGED DISCRIMINATION - Average discrimination across follow-up periods
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
                "Time-averaged discrimination calculated (MSS): %.3f over %d time points",
                time_averaged_discrimination, sum(valid_times)
            ), indent = 3))
        } else {
            time_averaged_discrimination_method <- "insufficient_events"
        }
        
    }, error = function(e) {
        logger::log_warn(formatted(sprintf("Time-averaged discrimination calculation failed (MSS): %s", e$message), indent = 3))
        time_averaged_discrimination_method <- "calculation_failed"
    })

    # REMOVED: Time-dependent AUC - Fragile metric that requires events at exact timepoints
    # Our data has events spread across time, not concentrated at arbitrary marks
    # This makes time-dependent AUC clinically nonsensical and unreliable

    events_at_timepoint <- sum(disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months)
    total_at_timepoint <- nrow(disc_data)

    logger::log_info(formatted(sprintf("MSS %d years: %d events of %d", timepoint, events_at_timepoint, total_at_timepoint), indent = 3))
    logger::log_info(formatted(sprintf(
        "Discrimination (MSS): Harrell C=%s (Robust Primary Metric)",
        ifelse(is.na(harrell_c), "NA", sprintf("%.3f", harrell_c))
    ), indent = 3))
    logger::log_info(formatted(sprintf("Methods used (MSS): Harrell=%s (Uno C-index and time-dependent AUC removed as fragile metrics)", harrell_method), indent = 3))

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
        logger::log_warn(formatted(sprintf("IPA calculation failed (MSS): %s", e$message), indent = 3))
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
        events_by_timepoint = events_at_timepoint,
        # PRIMARY DISCRIMINATION METRIC
        harrell_c = round(harrell_c, 3),
        harrell_ci_lower = round(harrell_ci_lower, 3),
        harrell_ci_upper = round(harrell_ci_upper, 3),
        harrell_method = harrell_method,
        # ROBUST DISCRIMINATION METRICS (replacing fragile timepoint-dependent metrics)
        integrated_auc = round(integrated_auc, 3),
        integrated_auc_method = integrated_auc_method,
        integrated_auc_status = integrated_auc_status,
        integrated_auc_na_reason = integrated_auc_na_reason,
        integrated_auc_time_periods = integrated_auc_time_periods,
        cumulative_discrimination = round(cumulative_discrimination, 3),
        cumulative_discrimination_method = cumulative_discrimination_method,
        time_averaged_discrimination = round(time_averaged_discrimination, 3),
        time_averaged_discrimination_method = time_averaged_discrimination_method,
        ipa = round(ipa_result$ipa, 4),
        ipa_method = ipa_result$method_used,
        ipa_fallback_used = ipa_result$fallback_triggered,
        ipa_calculation_notes = ipa_result$calculation_notes
    ))
}

#' Decision Curve Analysis for MSS at a given timepoint
#'
#' Computes net benefit across threshold probabilities using observed events by
#' the specified timepoint (in years) and predicted risk from expected MSS.
#'
#' @param data Data frame with columns: `time_to_event`, `event_occurred`
#'   (0/1), and `expected_mss_*yr`.
#' @param timepoint numeric Time in years for evaluation (e.g., 5)
#' @param time_unit Character scalar giving the units stored in
#'   `data$time_to_event`. Supported values are `"months"` and `"years"`.
#' @return List with DCA summary and curve data, similar to MFS
perform_decision_curve_analysis_mss <- function(data, timepoint, time_unit = c("months", "years")) {
    logger::log_info(formatted(sprintf("Performing decision curve analysis for %d-year MSS", timepoint), indent = 2))
    time_unit <- match.arg(time_unit)

    expected_var <- paste0("expected_mss_", timepoint, "yr")
    evaluation_horizon <- if (identical(time_unit, "months")) timepoint * 12 else timepoint

    dca_data <- data %>%
        dplyr::filter(!is.na(.data[[expected_var]]), !is.na(time_to_event), !is.na(event_occurred)) %>%
        dplyr::mutate(
            predicted_risk = 1 - .data[[expected_var]],
            outcome = as.integer(event_occurred == 1 & time_to_event <= evaluation_horizon)
        )

    if (nrow(dca_data) < GEP_MIN_SAMPLE_SIZE) {
        logger::log_warn(formatted("Insufficient data for decision curve analysis (MSS)", indent = 3))
        return(list(n = nrow(dca_data), status = "insufficient_data"))
    }

    event_rate <- mean(dca_data$outcome)
    risk_thresholds <- seq(GEP_DCA_THRESHOLD_MIN, GEP_DCA_THRESHOLD_MAX, by = GEP_DCA_THRESHOLD_STEP)

    dca_results <- data.frame(
        threshold = risk_thresholds,
        net_benefit_model = NA_real_,
        net_benefit_all = NA_real_,
        net_benefit_none = 0
    )

    for (i in seq_along(risk_thresholds)) {
        threshold <- risk_thresholds[i]
        treat_model <- dca_data$predicted_risk >= threshold
        treat_all <- rep(TRUE, nrow(dca_data))

        if (sum(treat_model) > 0) {
            tp_model <- sum(dca_data$outcome & treat_model)
            fp_model <- sum(!dca_data$outcome & treat_model)
            net_benefit_model <- (tp_model / nrow(dca_data)) - (fp_model / nrow(dca_data)) * (threshold / (1 - threshold))
        } else {
            net_benefit_model <- 0
        }

        tp_all <- sum(dca_data$outcome)
        fp_all <- sum(!dca_data$outcome)
        net_benefit_all <- (tp_all / nrow(dca_data)) - (fp_all / nrow(dca_data)) * (threshold / (1 - threshold))

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
    tryCatch({
        valid_indices <- !is.na(dca_results$net_benefit_model) & !is.na(dca_results$net_benefit_all)
        if (sum(valid_indices) > GEP_MIN_EVENTS_COMPETING_RISK) {
            diff_benefits <- dca_results$net_benefit_model[valid_indices] - dca_results$net_benefit_all[valid_indices]
            area_between_curves <- sum(diff_benefits) * 0.01
        }
    }, error = function(e) {
        area_between_curves <- NA
    })

    logger::log_info(formatted(sprintf(
        "DCA (MSS) optimal threshold: %.2f%% (Net benefit: %.4f)",
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
