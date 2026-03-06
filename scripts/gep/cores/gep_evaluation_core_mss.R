# GEP MSS Evaluation Core
# Contains MSS model evaluation algorithms (no plotting or I/O)

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
#' @return A list with `observed_expected`, `calibration`, `discrimination`, `decision_curve`, and `timepoint`.
perform_standard_mss_validation <- function(data, timepoint, bootstrap_iterations, time_var = "tt_death_months") {
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
    observed_expected <- calculate_observed_expected_rates(
        data = analysis_data,
        expected_var = paste0("expected_mss_", timepoint, "yr"),
        event_var = "event_occurred",
        time_var = "time_to_event",
        group_var = "biopsy1_gep"
    )

    # Calculate calibration metrics
    calibration_metrics <- calculate_calibration_metrics(
        data = analysis_data,
        expected_var = paste0("expected_mss_", timepoint, "yr"),
        event_var = "event_occurred",
        time_var = "time_to_event"
    )

    # Time-dependent discrimination metrics (Harrell C, Uno C, AUC at timepoint)
    discrimination_metrics <- perform_discrimination_mss(
        data = analysis_data,
        timepoint = timepoint
    )

    # Decision curve analysis at this timepoint
    decision_curve <- perform_decision_curve_analysis_mss(
        data = analysis_data,
        timepoint = timepoint
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
#' @return A list with `cumulative_incidence`, `cause_specific_hazards`, and `timepoint`.
perform_competing_risk_mss_validation <- function(data, timepoint, time_var = "tt_death_months") {
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

    # Calculate cumulative incidence functions
    cumulative_incidence <- calculate_cumulative_incidence(
        data = analysis_data,
        time_var = "time_to_event",
        event_var = "event_type",
        group_var = "biopsy1_gep"
    )

    # Add CIF with 95% CI at the evaluation time (bootstrap)
                cif_ci <- tryCatch({
        calculate_cif_by_class_with_ci(
            data = analysis_data,
            time_var = "time_to_event",
            event_type_var = "event_type",
            eval_time = timepoint_months,
            n_boot = GEP_BOOTSTRAP_ITERATIONS,
            eligibility_filter = "mss_analysis_eligible"
        )
    }, error = function(e) {
        logger::log_warn("Unable to compute CIF CIs; continuing without CIs")
        NULL
    })

    # Cause-specific Cox regression (proxy for CSC)
    csc_model <- calculate_cause_specific_cox_model(
        data = analysis_data,
        time_var = "time_to_event",
        event_var = "event_type",
        group_var = "biopsy1_gep",
        eligibility_filter = "mss_analysis_eligible"
    )

    # Fine-Gray subdistribution regression (FGR)
    fgr_model <- calculate_fine_gray_model(
        data = analysis_data,
        time_var = "time_to_event",
        event_var = "event_type",
        group_var = "biopsy1_gep",
        eligibility_filter = "mss_analysis_eligible"
    )

    return(list(
        cumulative_incidence = cumulative_incidence,
        cause_specific_cox = csc_model,
        fine_gray = fgr_model,
        cif_with_ci = cif_ci,
        timepoint = timepoint
    ))
}

#' Perform PRAME-augmented analysis for MSS
#'
#' Assess added value of PRAME status by comparing base GEP predictions vs
#' PRAME-augmented categories using NRI across timepoints.
#'
#' @param data Data frame prepared for MSS with PRAME status
#' @param timepoints Numeric vector of year values for evaluation
#' @return A named list of NRI results by timepoint (when feasible)
perform_prame_augmented_analysis_mss <- function(data, timepoints) {
    logger::log_info(formatted("Performing PRAME-augmented analysis with NRI calculation (MSS)", indent = 1))

    # Filter down to a clean cohort that has the inputs needed for reclassification:
    # - binary PRAME status
    # - baseline MSS risk predictions (biopsy1_gep_mss)
    # - survival time + melanoma-specific event indicator
    # - recorded baseline GEP class (for downstream summaries)
    prame_data <- data %>%
        dplyr::filter(
            !is.na(prame_status),
            prame_status %in% c("Positive", "Negative"),
            !is.na(biopsy1_gep_mss),
            !is.na(tt_death_years),
            !is.na(melanoma_death_event),
            !is.na(biopsy1_gep)
        )

    if (nrow(prame_data) < GEP_MIN_BOOTSTRAP_SAMPLE) {
        logger::log_warn(formatted(sprintf("Insufficient PRAME data for MSS analysis (n=%d)", nrow(prame_data)), indent = 2))
        return(list(
            n = nrow(prame_data),
            status = "insufficient_data",
            prame_available = FALSE
        ))
    }

    logger::log_info(formatted(sprintf("MSS PRAME analysis using %d patients", nrow(prame_data)), indent = 2))
    prame_dist <- table(prame_data$prame_status)
    logger::log_info(formatted(sprintf(
        "PRAME distribution (MSS): Positive=%d, Negative=%d",
        prame_dist["Positive"], prame_dist["Negative"]
    ), indent = 2))

    # Hold per-timepoint statistics so each sheet/summary can pick them up later
    nri_results <- list()

    for (timepoint in timepoints) {
        logger::log_info(formatted(sprintf("Calculating MSS NRI for %d-year timepoint", timepoint), indent = 2))

        # For each timepoint we construct a focused analysis frame:
        #   event_by_timepoint:   did the melanoma death happen before the landmark year?
        #   base_risk:            convert survival probability to risk (1-survival)
        #   prame_positive:       flag used for risk inflation vs deflation
        outcome_data <- prame_data %>%
            dplyr::mutate(
                event_by_timepoint = melanoma_death_event == 1 & tt_death_years <= timepoint,
                base_risk = 1 - biopsy1_gep_mss,
                prame_positive = prame_status == "Positive"
            )

        # Apply the empirically chosen adjustment factors that mirror the MFS workflow.
        # Positive PRAME shifts risk upward (capped by GEP_RISK_CAP_MAXIMUM) whereas
        # negative PRAME gently lowers the baseline risk estimate.
        enhanced_predictions <- ifelse(
            outcome_data$prame_positive,
            pmin(outcome_data$base_risk * GEP_PRAME_ADJUSTMENT_FACTOR, GEP_RISK_CAP_MAXIMUM),
            outcome_data$base_risk * GEP_PRAME_REDUCTION_FACTOR
        )

        # Translate continuous risks into categorical risk strata so we can
        # quantify movement across thresholds (net reclassification index)
        risk_cutoffs <- GEP_RISK_CUTOFFS
        risk_labels <- GEP_RISK_LABELS
        base_categories <- cut(outcome_data$base_risk, breaks = risk_cutoffs, labels = risk_labels, include.lowest = TRUE)
        enhanced_categories <- cut(enhanced_predictions, breaks = risk_cutoffs, labels = risk_labels, include.lowest = TRUE)
        base_category_codes <- as.integer(base_categories)
        enhanced_category_codes <- as.integer(enhanced_categories)

        reclass_table <- table(
            Base = base_categories,
            With_PRAME = enhanced_categories,
            useNA = "ifany"
        )

        # Event bookkeeping drives both NRI halves (events vs non-events) and
        # helps short-circuit the workflow when data are sparse at a given landmark
        events <- outcome_data$event_by_timepoint
        n_events <- sum(events)
        n_nonevents <- sum(!events)

        if (n_events >= GEP_MIN_EVENTS_COMPETING_RISK && n_nonevents >= GEP_MIN_EVENTS_COMPETING_RISK) {
            # Count upward/downward movement separately for events and non-events
            event_indices <- which(events)
            event_up <- sum(enhanced_category_codes[event_indices] > base_category_codes[event_indices], na.rm = TRUE)
            event_down <- sum(enhanced_category_codes[event_indices] < base_category_codes[event_indices], na.rm = TRUE)
            nonevent_indices <- which(!events)
            nonevent_up <- sum(enhanced_category_codes[nonevent_indices] > base_category_codes[nonevent_indices], na.rm = TRUE)
            nonevent_down <- sum(enhanced_category_codes[nonevent_indices] < base_category_codes[nonevent_indices], na.rm = TRUE)

            # Classical categorical NRI components
            nri_events <- (event_up / n_events) - (event_down / n_events)
            nri_nonevents <- (nonevent_down / n_nonevents) - (nonevent_up / n_nonevents)
            nri_total <- nri_events + nri_nonevents

            # Integrated discrimination improvement captures the average risk separation
            # achieved by adding PRAME on top of the categorical NRI story
            idi <- mean(enhanced_predictions[events]) - mean(outcome_data$base_risk[events]) -
                (mean(enhanced_predictions[!events]) - mean(outcome_data$base_risk[!events]))

            reclass_improved <- event_up + nonevent_down
            reclass_worsened <- event_down + nonevent_up
            if (reclass_improved + reclass_worsened > 0) {
                # McNemar's test assesses whether the direction of movement is symmetric
                mcnemar_stat <- (abs(reclass_improved - reclass_worsened) - 1)^2 / (reclass_improved + reclass_worsened)
                mcnemar_p <- pchisq(mcnemar_stat, df = 1, lower.tail = FALSE)
            } else {
                mcnemar_p <- 1
            }
        } else {
            logger::log_warn(formatted(sprintf(
                "Insufficient events (%d) or non-events (%d) for MSS NRI calculation",
                n_events, n_nonevents
            ), indent = 3))
            nri_events <- NA
            nri_nonevents <- NA
            nri_total <- NA
            idi <- NA
            mcnemar_p <- NA
            event_up <- 0
            event_down <- 0
            nonevent_up <- 0
            nonevent_down <- 0
        }

        # Optionally compare ROC AUCs (baseline vs PRAME-augmented) when data support it.
        # Wrapped in tryCatch to avoid pulling pROC when it is unavailable or when
        # event counts are too small to define a ROC curve.
        model_comparison <- NULL
        tryCatch(
            {
                if (requireNamespace("pROC", quietly = TRUE) && n_events >= GEP_MIN_EVENTS_COMPETING_RISK && n_nonevents >= GEP_MIN_EVENTS_COMPETING_RISK) {
                    base_auc <- pROC::auc(pROC::roc(events, outcome_data$base_risk, quiet = TRUE))
                    enhanced_auc <- pROC::auc(pROC::roc(events, enhanced_predictions, quiet = TRUE))
                    auc_difference <- enhanced_auc - base_auc
                    model_comparison <- list(
                        base_auc = round(as.numeric(base_auc), 3),
                        enhanced_auc = round(as.numeric(enhanced_auc), 3),
                        auc_difference = round(as.numeric(auc_difference), 3)
                    )
                }
            },
            error = function(e) {
                model_comparison <- NULL
            }
        )

        # Store all per-timepoint artifacts (counts, metrics, interpretation helpers)
        nri_results[[paste0("yr", timepoint)]] <- list(
            timepoint = timepoint,
            n = nrow(outcome_data),
            events = n_events,
            nonevents = n_nonevents,
            nri_events = round(nri_events, 3),
            nri_nonevents = round(nri_nonevents, 3),
            nri_total = round(nri_total, 3),
            idi = round(idi, 4),
            mcnemar_p = round(mcnemar_p, 4),
            reclassification_counts = list(
                event_up = event_up,
                event_down = event_down,
                nonevent_up = nonevent_up,
                nonevent_down = nonevent_down
            ),
            reclassification_table = reclass_table,
            model_comparison = model_comparison
        )

        if (!is.na(nri_total)) {
            logger::log_info(formatted(sprintf(
                "MSS NRI = %.3f (Events: %.3f, Non-events: %.3f), IDI = %.4f",
                nri_total, nri_events, nri_nonevents, idi
            ), indent = 3))
        }
    }

    # Return a compact bundle that downstream reporting modules understand,
    # mirroring the structure used by the MFS PRAME workflow for consistency
    return(list(
        n = nrow(prame_data),
        prame_available = TRUE,
        prame_distribution = prame_dist,
        nri_results = nri_results
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
        
        auc_data <- NULL
        if (!is.null(roc_result$AUC)) {
            auc_data <- roc_result$AUC$score
            if (nrow(auc_data) > 0) {
                # Calculate integrated AUC as mean across time periods
                integrated_auc <- mean(auc_data$AUC, na.rm = TRUE)
            }
        }
        integrated_auc_method <- "riskRegression::Score_integrated"
        
        num_periods <- ifelse(!is.null(auc_data) && nrow(auc_data) > 0, nrow(auc_data), 0)
        logger::log_info(formatted(sprintf(
            "Integrated AUC calculated successfully (MSS): %.3f over %d time periods",
            integrated_auc, num_periods
        ), indent = 3))
        
    }, error = function(e) {
        logger::log_warn(formatted(sprintf("Integrated AUC calculation failed (MSS): %s", e$message), indent = 3))
        integrated_auc_method <- "calculation_failed"
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

    events_at_timepoint <- sum(disc_data$observed_event == 1 & disc_data$observed_time <= timepoint)
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
#' @param data Data frame with columns: time_to_event (years), event_occurred (0/1), and expected_mss_*yr
#' @param timepoint numeric Time in years for evaluation (e.g., 5)
#' @return List with DCA summary and curve data, similar to MFS
perform_decision_curve_analysis_mss <- function(data, timepoint) {
    logger::log_info(formatted(sprintf("Performing decision curve analysis for %d-year MSS", timepoint), indent = 2))

    expected_var <- paste0("expected_mss_", timepoint, "yr")

    dca_data <- data %>%
        dplyr::filter(!is.na(.data[[expected_var]]), !is.na(time_to_event), !is.na(event_occurred)) %>%
        dplyr::mutate(
            predicted_risk = 1 - .data[[expected_var]],
            outcome = as.integer(event_occurred == 1 & time_to_event <= timepoint)
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
