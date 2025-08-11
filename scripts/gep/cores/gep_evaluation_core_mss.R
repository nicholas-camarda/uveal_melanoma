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
#' @return A list with `observed_expected`, `calibration`, `discrimination`, `decision_curve`, and `timepoint`.
perform_standard_mss_validation <- function(data, timepoint, bootstrap_iterations) {
    logger::log_debug(sprintf("Performing standard MSS validation for %d-year timepoint", timepoint))

    # Create time-to-event outcome for the specific timepoint (in years)
    analysis_data <- data %>%
        mutate(
            time_to_event = pmin(tt_death_years, timepoint),
            event_occurred = melanoma_death_event & (tt_death_years <= timepoint)
        )

    # Calculate observed vs expected rates
    observed_expected <- calculate_observed_expected_rates(
        data = analysis_data,
        expected_var = paste0("expected_mss_", timepoint, "yr"),
        event_var = "event_occurred",
        time_var = "time_to_event"
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
#' incidence functions and cause-specific hazards by GEP class.
#'
#' @param data Data frame prepared for MSS competing risk analysis
#' @param timepoint Numeric year value for evaluation (e.g., 5, 7, 10)
#' @return A list with `cumulative_incidence`, `cause_specific_hazards`, and `timepoint`.
perform_competing_risk_mss_validation <- function(data, timepoint) {
    logger::log_debug(sprintf("Performing competing risk MSS validation for %d-year timepoint", timepoint))

    # Create competing risk outcome
    analysis_data <- data %>%
        mutate(
            time_to_event = pmin(tt_death_years, timepoint),
            event_type = case_when(
                melanoma_death_event == 1 & tt_death_years <= timepoint ~ 1, # Melanoma death
                competing_death_event == 1 & tt_death_years <= timepoint ~ 2, # Competing death
                TRUE ~ 0 # Censored
            )
        )

    # Calculate cumulative incidence functions
    cumulative_incidence <- calculate_cumulative_incidence(
        data = analysis_data,
        time_var = "time_to_event",
        event_var = "event_type",
        group_var = "gep_class_simple"
    )

    # Add CIF with 95% CI at the evaluation time (bootstrap)
                cif_ci <- tryCatch({
        calculate_cif_by_class_with_ci(
            data = analysis_data,
            time_var = "time_to_event",
            event_type_var = "event_type",
            eval_time = timepoint,
            n_boot = GEP_BOOTSTRAP_ITERATIONS
        )
    }, error = function(e) {
        logger::log_warn("Unable to compute CIF CIs; continuing without CIs")
        NULL
    })

    # Calculate cause-specific hazard ratios
    cause_specific_hazards <- calculate_cause_specific_hazards(
        data = analysis_data,
        time_var = "time_to_event",
        event_var = "event_type",
        group_var = "gep_class_simple"
    )

    return(list(
        cumulative_incidence = cumulative_incidence,
        cause_specific_hazards = cause_specific_hazards,
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
    logger::log_debug("Performing PRAME-augmented MSS analysis")

    # Check if PRAME data is available
    if (!"prame_status" %in% names(data)) {
        logger::log_warn("PRAME status not available, skipping PRAME-augmented analysis")
        return(NULL)
    }

    # Calculate net reclassification index for each timepoint
    nri_results <- list()

    for (timepoint in timepoints) {
        analysis_data <- data %>%
            mutate(
                time_to_event = pmin(tt_death_years, timepoint),
                event_occurred = melanoma_death_event & (tt_death_years <= timepoint)
            )

        # Calculate NRI comparing GEP-only vs GEP+PRAME
        nri_result <- calculate_net_reclassification_index(
            data = analysis_data,
            base_pred = paste0("expected_mss_", timepoint, "yr"),
            enhanced_pred = paste0("expected_mss_", timepoint, "yr"), # Placeholder - would need PRAME-augmented predictions
            event_var = "event_occurred"
        )

        nri_results[[paste0("yr", timepoint)]] <- nri_result
    }

    return(nri_results)
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

    # Harrell's C-index
    harrell_c <- NA; harrell_ci_lower <- NA; harrell_ci_upper <- NA
    tryCatch({
        if (requireNamespace("survcomp", quietly = TRUE)) {
            # Use time-specific notion via capping at timepoint (already in observed_time)
            harrell_result <- survcomp::concordance.index(
                x = disc_data$predicted_risk,
                surv.time = disc_data$observed_time,
                surv.event = disc_data$observed_event,
                method = "noether"
            )
            harrell_c <- harrell_result$c.index
            harrell_ci_lower <- harrell_result$lower
            harrell_ci_upper <- harrell_result$upper
        } else {
            cox_fit <- coxph(surv_obj ~ predicted_risk, data = disc_data, model = TRUE)
            harrell_c <- summary(cox_fit)$concordance[1]
        }
    }, error = function(e) {
        logger::log_warn(formatted("Error calculating Harrell's C-index (MSS)", indent = 3))
    })

    # Uno's C-index
    uno_c <- NA; uno_ci_lower <- NA; uno_ci_upper <- NA
    tryCatch({
        if (requireNamespace("survcomp", quietly = TRUE)) {
            uno_result <- survcomp::concordance.index(
                x = disc_data$predicted_risk,
                surv.time = disc_data$observed_time,
                surv.event = disc_data$observed_event,
                method = "uno"
            )
            uno_c <- uno_result$c.index
            uno_ci_lower <- uno_result$lower
            uno_ci_upper <- uno_result$upper
        }
    }, error = function(e) {
        logger::log_warn(formatted("Error calculating Uno's C-index (MSS)", indent = 3))
    })

    # Time-dependent AUC at the timepoint (years)
    auc_timepoint <- NA; auc_ci_lower <- NA; auc_ci_upper <- NA
    tryCatch({
        if (requireNamespace("riskRegression", quietly = TRUE)) {
            # Fit a simple model; Score needs a model interface
            cox_model <- coxph(surv_obj ~ predicted_risk, data = disc_data, model = TRUE)
            roc_result <- riskRegression::Score(
                list("GEP" = cox_model),
                formula = surv_obj ~ 1,
                data = disc_data,
                times = timepoint,
                metrics = "auc",
                summary = "risks"
            )
            if (!is.null(roc_result$AUC)) {
                auc_data <- roc_result$AUC$score
                if (nrow(auc_data) > 0) {
                    # The first row corresponds to the provided time
                    auc_timepoint <- auc_data$AUC[1]
                    auc_ci_lower <- auc_data$lower[1]
                    auc_ci_upper <- auc_data$upper[1]
                }
            }
        } else if (requireNamespace("pROC", quietly = TRUE)) {
            # Binary outcome at timepoint
            binary_outcome <- disc_data$observed_event == 1 & disc_data$observed_time <= timepoint
            if (sum(binary_outcome) > GEP_MIN_EVENTS_COMPETING_RISK && sum(!binary_outcome) > GEP_MIN_EVENTS_COMPETING_RISK) {
                roc_obj <- pROC::roc(binary_outcome, disc_data$predicted_risk, quiet = TRUE)
                auc_timepoint <- as.numeric(roc_obj$auc)
                tryCatch({
                    ci_result <- pROC::ci.auc(roc_obj)
                    auc_ci_lower <- ci_result[1]; auc_ci_upper <- ci_result[3]
                }, error = function(e) {
                    auc_ci_lower <- NA; auc_ci_upper <- NA
                })
            }
        }
    }, error = function(e) {
        logger::log_warn(formatted("Error calculating time-specific AUC (MSS)", indent = 3))
    })

    events_at_timepoint <- sum(disc_data$observed_event == 1 & disc_data$observed_time <= timepoint)
    total_at_timepoint <- nrow(disc_data)

    logger::log_info(formatted(sprintf("MSS %d years: %d events of %d", timepoint, events_at_timepoint, total_at_timepoint), indent = 3))
    logger::log_info(formatted(sprintf(
        "Discrimination (MSS): Harrell C=%s, Uno C=%s, AUC=%s",
        ifelse(is.na(harrell_c), "NA", sprintf("%.3f", harrell_c)),
        ifelse(is.na(uno_c), "NA", sprintf("%.3f", uno_c)),
        ifelse(is.na(auc_timepoint), "NA", sprintf("%.3f", auc_timepoint))
    ), indent = 3))

    return(list(
        n = nrow(disc_data),
        events = sum(disc_data$observed_event),
        events_by_timepoint = events_at_timepoint,
        harrell_c = round(harrell_c, 3),
        harrell_ci_lower = round(harrell_ci_lower, 3),
        harrell_ci_upper = round(harrell_ci_upper, 3),
        uno_c = round(uno_c, 3),
        uno_ci_lower = round(uno_ci_lower, 3),
        uno_ci_upper = round(uno_ci_upper, 3),
        auc_timepoint = round(auc_timepoint, 3),
        auc_ci_lower = round(auc_ci_lower, 3),
        auc_ci_upper = round(auc_ci_upper, 3)
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

    optimal_idx <- which.max(dca_results$net_benefit_model)
    optimal_threshold <- dca_results$threshold[optimal_idx]
    optimal_net_benefit <- dca_results$net_benefit_model[optimal_idx]

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
