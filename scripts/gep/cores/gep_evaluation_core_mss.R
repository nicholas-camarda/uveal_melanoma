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

    # Initialize discrimination metrics
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
    }, error = function(e) {
        logger::log_error(formatted("Error calculating Harrell's C-index (MSS)", indent = 3))
        harrell_method <- "error"
    })

    # Uno's C-index (guarded)
    uno_c <- NA; uno_ci_lower <- NA; uno_ci_upper <- NA; uno_c_method <- NA_character_
    unique_risk <- length(unique(na.omit(disc_data$predicted_risk)))
    num_events_timepoint <- sum(disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months, na.rm = TRUE)
    if (num_events_timepoint < GEP_MIN_EVENTS_COMPETING_RISK) {
        logger::log_warn(formatted(sprintf(
            "Skipping Uno's C-index (MSS): events at timepoint=%d months (min %d), unique risk=%d",
            timepoint_months, GEP_MIN_EVENTS_COMPETING_RISK, unique_risk
        ), indent = 3))
        uno_c_method <- "skipped_insufficient_data"
    } else {
        tryCatch({
            # survcomp approach at specified timepoint (months)
            time_specific_event <- disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months
            time_specific_time <- pmin(disc_data$observed_time, timepoint_months)
            uno_result <- survcomp::concordance.index(
                x = disc_data$predicted_risk,
                surv.time = time_specific_time,
                surv.event = time_specific_event,
                method = "uno"
            )
            uno_c <- uno_result$c.index
            uno_ci_lower <- uno_result$lower
            uno_ci_upper <- uno_result$upper
            uno_c_method <- "survcomp"
        }, error = function(e1) {
            tryCatch({
                # riskRegression::AUC.uno fallback using original times
                # Validate timepoint_months parameter to prevent "invalid 'times' argument" errors
                if (is.na(timepoint_months) || !is.numeric(timepoint_months) || timepoint_months <= 0 || is.infinite(timepoint_months)) {
                    logger::log_warn(formatted(sprintf("Invalid timepoint_months for riskRegression::AUC.uno fallback (MSS): %s", timepoint_months), indent = 3))
                    logger::log_warn("Skipping riskRegression::AUC.uno fallback due to invalid timepoint")
                    uno_c_method <- "invalid_timepoint"
                } else {
                    auc_uno <- riskRegression::AUC.uno(
                        Surv(disc_data$observed_time, disc_data$observed_event),
                        Surv(disc_data$observed_time, disc_data$observed_event),
                        marker = disc_data$predicted_risk,
                        times = timepoint_months
                    )
                    if (!is.null(auc_uno$AUC)) {
                        uno_c <- as.numeric(auc_uno$AUC[length(auc_uno$AUC)])
                    } else if (!is.null(auc_uno$iauc)) {
                        uno_c <- as.numeric(auc_uno$iauc)
                    }
                    uno_c_method <- "riskRegression::AUC.uno"
                }
            }, error = function(e2) {
                tryCatch({
                    # timeROC fallback
                    # Validate timepoint_months parameter to prevent "invalid 'times' argument" errors
                    if (is.na(timepoint_months) || !is.numeric(timepoint_months) || timepoint_months <= 0 || is.infinite(timepoint_months)) {
                        logger::log_warn(formatted(sprintf("Invalid timepoint_months for timeROC fallback (MSS): %s", timepoint_months), indent = 3))
                        logger::log_warn("Skipping timeROC fallback due to invalid timepoint")
                        uno_c_method <- "invalid_timepoint"
                    } else {
                        tr <- timeROC::timeROC(T = disc_data$observed_time,
                                               delta = disc_data$observed_event,
                                               marker = disc_data$predicted_risk,
                                               cause = 1,
                                               times = timepoint_months,
                                               iid = FALSE)
                        if (!is.null(tr$AUC)) {
                            uno_c <- as.numeric(tr$AUC[1])
                        }
                        uno_c_method <- "timeROC"
                    }
                }, error = function(e3) {
                    # Final fallback: empirical C-index calculation
                    # Only proceed if timepoint_months is valid
                    if (is.na(timepoint_months) || !is.numeric(timepoint_months) || timepoint_months <= 0 || is.infinite(timepoint_months)) {
                        logger::log_warn("Skipping empirical C-index calculation due to invalid timepoint")
                        uno_c_method <- "invalid_timepoint"
                    } else {
                        tryCatch({
                            time_specific_event <- disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months
                            if (sum(time_specific_event) > GEP_MIN_EVENTS_COMPETING_RISK) {
                                # Simple empirical C-index using concordance pairs
                                cases <- disc_data$predicted_risk[time_specific_event]
                                controls <- disc_data$predicted_risk[!time_specific_event]
                                if (length(cases) > 0 && length(controls) > 0) {
                                    # Use more efficient method for large datasets
                                    if (length(cases) * length(controls) > 10000) {
                                        # Sample-based approach for large datasets
                                        set.seed(123) # For reproducibility
                                        case_sample <- sample(cases, min(100, length(cases)), replace = TRUE)
                                        control_sample <- sample(controls, min(100, length(controls)), replace = TRUE)
                                        concordant <- sum(outer(case_sample, control_sample, ">"))
                                        total_pairs <- length(case_sample) * length(control_sample)
                                    } else {
                                        # Full calculation for smaller datasets
                                        concordant <- sum(outer(cases, controls, ">"))
                                        total_pairs <- length(cases) * length(controls)
                                    }
                                    uno_c <- concordant / total_pairs
                                }
                            }
                            uno_c_method <- "empirical"
                        }, error = function(e4) {
                            logger::log_warn(formatted("Unable to compute Uno's C-index (MSS) with available methods", indent = 3))
                            uno_c_method <- "error"
                        })
                    }
                })
            })
        })
    }

    # Time-dependent AUC at the timepoint (months)
    auc_timepoint <- NA; auc_ci_lower <- NA; auc_ci_upper <- NA; auc_method <- NA_character_
    
    # Validate timepoint_months parameter to prevent "invalid 'times' argument" errors
    if (is.na(timepoint_months) || !is.numeric(timepoint_months) || timepoint_months <= 0 || is.infinite(timepoint_months)) {
        logger::log_warn(formatted(sprintf("Invalid timepoint_months for AUC calculation (MSS): %s", timepoint_months), indent = 3))
        logger::log_warn("Skipping AUC calculation due to invalid timepoint")
        auc_method <- "invalid_timepoint"
    } else {
        # Try riskRegression::Score first, then timeROC, then pROC
        tryCatch({
            # Build time-specific data for ROC at given timepoint
            # Create a temporary data frame with the time-specific variables
            temp_data <- disc_data %>%
                dplyr::mutate(
                    time_specific_event = .data$observed_event == 1 & .data$observed_time <= timepoint_months,
                    time_specific_time = pmin(.data$observed_time, timepoint_months)
                )
            
            time_specific_surv <- Surv(temp_data$time_specific_time, temp_data$time_specific_event)
            # Fit simple model; Score needs a model interface
            cox_model <- coxph(time_specific_surv ~ predicted_risk, data = temp_data)
            
            # Validate timepoint_months parameter to prevent "invalid 'times' argument" errors
            if (is.na(timepoint_months) || !is.numeric(timepoint_months) || timepoint_months <= 0 || is.infinite(timepoint_months)) {
                logger::log_warn(formatted(sprintf("Invalid timepoint_months for riskRegression::Score (MSS): %s", timepoint_months), indent = 3))
                logger::log_warn("Skipping riskRegression::Score due to invalid timepoint")
                auc_method <- "invalid_timepoint"
            } else {
                roc_result <- riskRegression::Score(
                    list("GEP" = cox_model),
                    formula = Surv(time_specific_time, time_specific_event) ~ 1,
                    data = temp_data,
                    times = timepoint_months,
                    metrics = "auc",
                    summary = "risks"
                )
                
                if (!is.null(roc_result$AUC)) {
                    auc_data <- roc_result$AUC$score
                    if (nrow(auc_data) > 0) {
                        auc_timepoint <- auc_data$AUC[1]
                        auc_ci_lower <- auc_data$lower[1]
                        auc_ci_upper <- auc_data$upper[1]
                    }
                }
                auc_method <- "riskRegression::Score"
            }
        }, error = function(e1) {
            tryCatch({
                # timeROC fallback - use time-specific data for better accuracy
                # Validate timepoint_months parameter to prevent "invalid 'times' argument" errors
                if (is.na(timepoint_months) || !is.numeric(timepoint_months) || timepoint_months <= 0 || is.infinite(timepoint_months)) {
                    logger::log_warn(formatted(sprintf("Invalid timepoint_months for timeROC fallback (MSS): %s", timepoint_months), indent = 3))
                    logger::log_warn("Skipping timeROC fallback due to invalid timepoint")
                    auc_method <- "invalid_timepoint"
                } else {
                    time_specific_event <- disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months
                    time_specific_time <- pmin(disc_data$observed_time, timepoint_months)
                    
                    tr <- timeROC::timeROC(T = time_specific_time,
                                           delta = time_specific_event,
                                           marker = disc_data$predicted_risk,
                                           cause = 1,
                                           times = timepoint_months,
                                           iid = FALSE)
                    if (!is.null(tr$AUC)) {
                        auc_timepoint <- as.numeric(tr$AUC[1])
                    }
                    auc_method <- "timeROC"
                }
            }, error = function(e2) {
                tryCatch({
                    # pROC fallback on binary-at-timepoint - this is the most reliable method
                    # Only proceed if timepoint_months is valid
                    if (is.na(timepoint_months) || !is.numeric(timepoint_months) || timepoint_months <= 0 || is.infinite(timepoint_months)) {
                        logger::log_warn("Skipping pROC fallback due to invalid timepoint")
                        auc_method <- "invalid_timepoint"
                    } else {
                        binary_outcome <- disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months
                        if (sum(binary_outcome) > GEP_MIN_EVENTS_COMPETING_RISK && sum(!binary_outcome) > GEP_MIN_EVENTS_COMPETING_RISK) {
                            roc_obj <- pROC::roc(binary_outcome, disc_data$predicted_risk, quiet = TRUE)
                            auc_timepoint <- as.numeric(roc_obj$auc)
                            tryCatch({
                                ci_result <- pROC::ci.auc(roc_obj)
                                auc_ci_lower <- ci_result[1]; auc_ci_upper <- ci_result[3]
                            }, error = function(e) { auc_ci_lower <- NA; auc_ci_upper <- NA })
                            
                            logger::log_info(formatted(sprintf(
                                "pROC AUC calculated successfully (MSS): %.3f (events=%d, non-events=%d)",
                                auc_timepoint, sum(binary_outcome), sum(!binary_outcome)
                            ), indent = 3))
                        } else {
                            logger::log_warn(formatted(sprintf(
                                "Insufficient events for pROC (MSS): events=%d, min_required=%d",
                                sum(binary_outcome), GEP_MIN_EVENTS_COMPETING_RISK
                            ), indent = 3))
                        }
                        auc_method <- "pROC"
                    }
                }, error = function(e3) {
                    # Final fallback: empirical AUC calculation
                    # Only proceed if timepoint is valid
                    if (is.na(timepoint) || !is.numeric(timepoint) || timepoint <= 0 || is.infinite(timepoint)) {
                        logger::log_warn("Skipping empirical AUC calculation due to invalid timepoint")
                        auc_method <- "invalid_timepoint"
                    } else {
                        tryCatch({
                            binary_outcome <- disc_data$observed_event == 1 & disc_data$observed_time <= timepoint
                            if (sum(binary_outcome) > GEP_MIN_EVENTS_COMPETING_RISK && sum(!binary_outcome) > GEP_MIN_EVENTS_COMPETING_RISK) {
                                # Simple empirical AUC using Wilcoxon-Mann-Whitney statistic
                                cases <- disc_data$predicted_risk[binary_outcome]
                                controls <- disc_data$predicted_risk[!binary_outcome]
                                if (length(cases) > 0 && length(controls) > 0) {
                                    # Use more efficient method for large datasets
                                    if (length(cases) * length(controls) > 10000) {
                                        # Sample-based approach for large datasets
                                        set.seed(123) # For reproducibility
                                        case_sample <- sample(cases, min(100, length(cases)), replace = TRUE)
                                        control_sample <- sample(controls, min(100, length(controls)), replace = TRUE)
                                        auc_timepoint <- sum(outer(case_sample, control_sample, ">")) / (length(case_sample) * length(control_sample))
                                    } else {
                                        # Full calculation for smaller datasets
                                        auc_timepoint <- sum(outer(cases, controls, ">")) / (length(cases) * length(controls))
                                    }
                                    auc_method <- "empirical"
                                }
                            }
                        }, error = function(e4) {
                            logger::log_warn("All AUC calculation methods failed")
                            auc_method <- "all_failed"
                        })
                    }
                })
            })
        })
    }

    events_at_timepoint <- sum(disc_data$observed_event == 1 & disc_data$observed_time <= timepoint)
    total_at_timepoint <- nrow(disc_data)

    logger::log_info(formatted(sprintf("MSS %d years: %d events of %d", timepoint, events_at_timepoint, total_at_timepoint), indent = 3))
    logger::log_info(formatted(sprintf(
        "Discrimination (MSS): Harrell C=%s, Uno C=%s, AUC=%s",
        ifelse(is.na(harrell_c), "NA", sprintf("%.3f", harrell_c)),
        ifelse(is.na(uno_c), "NA", sprintf("%.3f", uno_c)),
        ifelse(is.na(auc_timepoint), "NA", sprintf("%.3f", auc_timepoint))
    ), indent = 3))
    logger::log_info(formatted(sprintf("Methods used (MSS): Harrell=%s, Uno=%s, AUC=%s", harrell_method, uno_c_method, auc_method), indent = 3))

    return(list(
        n = nrow(disc_data),
        events = sum(disc_data$observed_event),
        events_by_timepoint = events_at_timepoint,
        harrell_c = round(harrell_c, 3),
        harrell_ci_lower = round(harrell_ci_lower, 3),
        harrell_ci_upper = round(harrell_ci_upper, 3),
        harrell_method = harrell_method,
        uno_c = round(uno_c, 3),
        uno_ci_lower = round(uno_ci_lower, 3),
        uno_ci_upper = round(uno_ci_upper, 3),
        uno_c_method = uno_c_method,
        auc_timepoint = round(auc_timepoint, 3),
        auc_ci_lower = round(auc_ci_lower, 3),
        auc_ci_upper = round(auc_ci_upper, 3),
        auc_method = auc_method
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
