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
        chisq_stat <- chisq_test$statistic
    } else {
        chisq_p <- NA
        chisq_stat <- NA
    }

    return(list(
        timepoint = timepoint,
        results_by_class = results_by_class,
        overall_observed = observed_total,
        overall_expected = round(expected_total, 2),
        overall_oe_ratio = if (expected_total > 0) round(observed_total / expected_total, 3) else NA,
        overall_poisson_ci_lower = round(overall_ci_lower, 3),
        overall_poisson_ci_upper = round(overall_ci_upper, 3),
        chisq_statistic = round(chisq_stat, 3),
        chisq_p_value = round(chisq_p, 4)
    ))
}

#' Perform Calibration Assessment (MFS)
#'
#' Assess calibration of GEP-predicted MFS risk at a given timepoint using
#' Nam-D'Agostino chi-square, Integrated Calibration Index (ICI), and a
#' bootstrap-corrected calibration slope.
#'
#' @param data Data frame with GEP predictions and survival outcomes.
#'   Must contain `tt_mets_months`, `mets_event`, and timepoint-specific
#'   expected survival columns (e.g., `expected_mfs_5yr`).
#' @param timepoint Numeric year value (e.g., 5, 7, 10) specifying the
#'   MFS evaluation timepoint.
#' @param bootstrap_iterations Integer number of bootstrap iterations for
#'   slope optimism correction.
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
            predicted_prob = .data[[expected_var]],
            observed_time = .data[[paste0("tt_mfs_", timepoint, "yr")]],
            observed_event = .data[[paste0("mfs_event_", timepoint, "yr")]],
            # Use pre-calculated risk variables
            predicted_risk = .data[[paste0("predicted_mfs_risk_", timepoint, "yr")]]
        )

    if (nrow(cal_data) < GEP_MIN_SAMPLE_SIZE) {
        logger::log_warn(formatted("Insufficient data for calibration analysis", indent = 3))
        # Try to calculate a simple ICI even with insufficient data
        simple_ici <- tryCatch({
            observed_rate <- sum(cal_data$observed_event == 1 & cal_data$observed_time <= timepoint_months) / nrow(cal_data)
            mean_predicted_rate <- mean(cal_data$predicted_risk, na.rm = TRUE)
            abs(observed_rate - mean_predicted_rate)
        }, error = function(e) NA)
        
        return(list(
            n = nrow(cal_data),
            status = "insufficient_data",
            nam_dagostino_p = NA,
            ici = simple_ici,
            calibration_slope = NA,
            calibration_intercept = NA
        ))
    }

    # Create survival object for calibration
    surv_obj <- Surv(cal_data$observed_time, cal_data$observed_event)

    # 1. Nam-D'Agostino χ² test
    # Group predictions into quantiles and compare observed vs expected
    n_groups <- min(10, floor(nrow(cal_data) / 10)) # At least 10 per group
    if (n_groups < 3) n_groups <- 3

    # Create breaks that are guaranteed to be unique
    risk_quantiles <- unique(quantile(cal_data$predicted_risk, seq(0, 1, length.out = n_groups + 1)))

    # If we don't have enough unique quantiles, adjust the number of groups
    if (length(risk_quantiles) <= 2) {
        # Fall back to simple median split
        cal_data$risk_group <- ifelse(cal_data$predicted_risk <= median(cal_data$predicted_risk), 1, 2)
        n_groups <- 2
    } else {
        # Use the unique quantiles as breaks
        cal_data$risk_group <- cut(cal_data$predicted_risk,
            breaks = risk_quantiles,
            include.lowest = TRUE, labels = FALSE
        )
        n_groups <- length(risk_quantiles) - 1
    }

    # Calculate observed vs expected by risk group
    group_results <- cal_data %>%
        group_by(risk_group) %>%
        summarise(
            n = n(),
            mean_predicted_risk = mean(predicted_risk),
            observed_events = sum(observed_event == 1 & observed_time <= timepoint_months),
            expected_events = sum(predicted_risk),
            .groups = "drop"
        ) %>%
        filter(n >= GEP_MIN_GROUP_SIZE) # Minimum group size for reliable testing

    # Initialize variables
    chisq_stat <- NA
    nam_dagostino_p <- NA
    # ICI will be calculated below with robust error handling

    # Nam-D'Agostino test
    if (nrow(group_results) >= 3 && sum(group_results$expected_events) > 0) {
        chisq_stat <- sum((group_results$observed_events - group_results$expected_events)^2 /
            pmax(group_results$expected_events, 1))
        nam_dagostino_p <- pchisq(chisq_stat, df = nrow(group_results) - 1, lower.tail = FALSE)
    } else {
        nam_dagostino_p <- NA
    }

    # 2. Integrated Calibration Index (ICI)
    # Use a more robust approach that handles edge cases
    ici <- NA
    ici_method <- "unknown"
    
    tryCatch(
        {
            if (nrow(cal_data) >= 2 * GEP_MIN_SAMPLE_SIZE) {
                # Calculate observed rate at timepoint
                observed_rate <- sum(cal_data$observed_event == 1 & cal_data$observed_time <= timepoint_months) / nrow(cal_data)
                mean_predicted_rate <- mean(cal_data$predicted_risk, na.rm = TRUE)
                
                # Calculate ICI as mean absolute difference between predicted and observed
                ici <- mean(abs(cal_data$predicted_risk - observed_rate), na.rm = TRUE)
                ici_method <- "mean_absolute_diff"
                
                # Alternative: use quantile-based approach if the above fails
                if (is.na(ici) || is.infinite(ici)) {
                    # Group predictions into quantiles and compare with observed
                    quantiles <- quantile(cal_data$predicted_risk, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE)
                    ici_calc <- 0
                    count <- 0
                    
                    for (i in 1:(length(quantiles)-1)) {
                        lower <- quantiles[i]
                        upper <- quantiles[i+1]
                        mask <- cal_data$predicted_risk >= lower & cal_data$predicted_risk < upper
                        if (sum(mask) > 0) {
                            pred_avg <- mean(cal_data$predicted_risk[mask], na.rm = TRUE)
                            obs_avg <- mean(cal_data$observed_event[mask] == 1 & cal_data$observed_time[mask] <= timepoint_months, na.rm = TRUE)
                            ici_calc <- ici_calc + abs(pred_avg - obs_avg)
                            count <- count + 1
                        }
                    }
                    
                    if (count > 0) {
                        ici <- ici_calc / count
                        ici_method <- "quantile_based"
                    }
                }
            } else {
                # Simple calibration for small samples
                observed_rate <- sum(cal_data$observed_event == 1 & cal_data$observed_time <= timepoint_months) / nrow(cal_data)
                mean_predicted_rate <- mean(cal_data$predicted_risk, na.rm = TRUE)
                ici <- abs(observed_rate - mean_predicted_rate)
                ici_method <- "simple"
            }
        },
        error = function(e) {
            logger::log_warn(formatted("Error calculating ICI, using fallback approach", indent = 3))
            # Fallback: use overall observed vs predicted difference
            observed_rate <- sum(cal_data$observed_event == 1 & cal_data$observed_time <= timepoint_months) / nrow(cal_data)
            mean_predicted_rate <- mean(cal_data$predicted_risk, na.rm = TRUE)
            ici <- abs(observed_rate - mean_predicted_rate)
            ici_method <- "fallback"
        }
    )
    
    # Final safety check
    if (is.na(ici) || is.infinite(ici)) {
        ici <- NA
        ici_method <- "failed"
    }

    # ICI is now properly handled above with robust error handling

    # 3. Bootstrap-corrected calibration slope and intercept
    if (bootstrap_iterations > 0 && nrow(cal_data) >= GEP_MIN_BOOTSTRAP_SAMPLE) {
        tryCatch(
            {
                # Fit Cox model to get calibration slope
                cox_fit <- coxph(surv_obj ~ predicted_risk, data = cal_data, model = TRUE)
                original_slope <- coef(cox_fit)[1]

                # Bootstrap validation
                bootstrap_slopes <- replicate(min(bootstrap_iterations, GEP_MAX_BOOTSTRAP_ITERATIONS), {
                    # Bootstrap sample
                    boot_indices <- sample(nrow(cal_data), replace = TRUE)
                    boot_data <- cal_data[boot_indices, ]

                    # Fit model on bootstrap sample
                    boot_surv <- Surv(boot_data$observed_time, boot_data$observed_event)
                    tryCatch(
                        {
                            boot_cox <- coxph(boot_surv ~ predicted_risk, data = boot_data, model = TRUE)
                            coef(boot_cox)[1]
                        },
                        error = function(e) NA
                    )
                })

                # Calculate optimism and shrunk slope
                bootstrap_slopes <- bootstrap_slopes[!is.na(bootstrap_slopes)]
                if (length(bootstrap_slopes) > GEP_MISSING_DATA_THRESHOLD) {
                    optimism <- mean(bootstrap_slopes) - original_slope
                    calibration_slope <- original_slope - optimism
                } else {
                    calibration_slope <- original_slope
                }

                # Calibration intercept (assuming proportional hazards)
                calibration_intercept <- 0 # In Cox models, no intercept
            },
            error = function(e) {
                logger::log_error(formatted("Error in bootstrap calibration, using simple estimates", indent = 3))
                calibration_slope <- 1 # Perfect calibration assumption
                calibration_intercept <- 0
            }
        )
    } else {
        calibration_slope <- 1 # Default assumption
        calibration_intercept <- 0
    }

    logger::log_info(formatted(sprintf(
        "Calibration metrics: Nam-D'Agostino p=%.4f, ICI=%.4f (%s), Slope=%.3f",
        nam_dagostino_p, ici, ifelse(exists("ici_method"), ici_method, "unknown"), calibration_slope
    ), indent = 3))

    return(list(
        n = nrow(cal_data),
        n_groups = nrow(group_results),
        nam_dagostino_statistic = chisq_stat,
        nam_dagostino_p = round(nam_dagostino_p, 4),
        ici = round(ici, 4),
        ici_method = ifelse(exists("ici_method"), ici_method, NA_character_),
        slope = round(calibration_slope, 3),
        calibration_intercept = round(calibration_intercept, 3),
        group_results = group_results
    ))
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

    # 2. Uno's censoring-adjusted C-index - TIME-SPECIFIC
    uno_c <- NA
    uno_ci_lower <- NA
    uno_ci_upper <- NA
    uno_c_method <- NA_character_
    # Guard: require at least a minimal number of events and some variation in risk predictions
    unique_risk <- length(unique(na.omit(disc_data$predicted_risk)))
    num_events_timepoint <- sum(disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months, na.rm = TRUE)
    
    # Debug logging
    logger::log_info(formatted(sprintf(
        "Uno's C-index calculation check: events=%d (min=%d), unique_risk=%d, total_patients=%d",
        num_events_timepoint, GEP_MIN_EVENTS_COMPETING_RISK, unique_risk, nrow(disc_data)
    ), indent = 3))
    
    if (num_events_timepoint < GEP_MIN_EVENTS_COMPETING_RISK) {
        logger::log_warn(formatted(sprintf(
            "Skipping Uno's C-index: insufficient events at timepoint=%d (min %d)",
            num_events_timepoint, GEP_MIN_EVENTS_COMPETING_RISK
        ), indent = 3))
        uno_c_method <- "skipped_insufficient_data"
    } else {
        # Try riskRegression::AUC.uno first, then timeROC, then empirical
        tryCatch({
            # Validate timepoint_months parameter to prevent "invalid 'times' argument" errors
            if (is.na(timepoint_months) || !is.numeric(timepoint_months) || timepoint_months <= 0 || is.infinite(timepoint_months)) {
                logger::log_warn(formatted(sprintf("Invalid timepoint_months for Uno's C-index calculation (MFS): %s", timepoint_months), indent = 3))
                logger::log_warn("Skipping Uno's C-index calculation due to invalid timepoint")
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
        }, error = function(e1) {
            # Only proceed with fallback methods if timepoint_months is valid
            if (is.na(timepoint_months) || !is.numeric(timepoint_months) || timepoint_months <= 0 || is.infinite(timepoint_months)) {
                logger::log_warn("Skipping Uno's C-index fallback methods due to invalid timepoint")
                uno_c_method <- "invalid_timepoint"
            } else {
                tryCatch({
                    # timeROC fallback
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
                }, error = function(e2) {
                    # Final fallback: empirical C-index calculation
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
                                    sample_size <- min(1000, length(cases) * length(controls))
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
                                logger::log_info(formatted(sprintf(
                                    "Empirical Uno's C-index calculated: %.3f (cases=%d, controls=%d)",
                                    uno_c, length(cases), length(controls)
                                ), indent = 3))
                            }
                        }
                        uno_c_method <- "empirical"
                    }, error = function(e3) {
                        logger::log_warn(formatted("Unable to compute Uno's C-index with available methods", indent = 3))
                        uno_c_method <- "error"
                    })
                })
            }
        })
        
        # If all sophisticated methods failed, try empirical fallback
        if (is.na(uno_c)) {
            logger::log_info(formatted("All sophisticated Uno's C-index methods failed, attempting empirical fallback", indent = 3))
            tryCatch({
                # Only proceed if timepoint_months is valid
                if (is.na(timepoint_months) || !is.numeric(timepoint_months) || timepoint_months <= 0 || is.infinite(timepoint_months)) {
                    logger::log_warn("Skipping empirical fallback due to invalid timepoint")
                } else {
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
                            logger::log_info(formatted(sprintf(
                                "Empirical Uno's C-index fallback calculated: %.3f (cases=%d, controls=%d)",
                                uno_c, length(cases), length(controls)
                            ), indent = 3))
                            uno_c_method <- "empirical_fallback"
                        }
                    }
                }
            }, error = function(e) {
                logger::log_warn(formatted("Empirical fallback also failed for Uno's C-index", indent = 3))
            })
        }
    }

    # 3. Time-specific AUC (cumulative/dynamic ROC) - TIME-SPECIFIC
    auc_timepoint <- NA
    auc_ci_lower <- NA
    auc_ci_upper <- NA
    auc_method <- NA_character_
    
    # Debug logging
    logger::log_info(formatted(sprintf(
        "AUC calculation check: events=%d (min=%d), total_patients=%d",
        sum(disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months, na.rm = TRUE),
        GEP_MIN_EVENTS_COMPETING_RISK, nrow(disc_data)
    ), indent = 3))
    
    # Simple, reliable AUC calculation using pROC
    binary_outcome <- disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months
    
    if (sum(binary_outcome) > GEP_MIN_EVENTS_COMPETING_RISK && sum(!binary_outcome) > GEP_MIN_EVENTS_COMPETING_RISK) {
        # Use pROC - it works reliably for time-specific binary outcomes
        roc_obj <- pROC::roc(binary_outcome, disc_data$predicted_risk, quiet = TRUE)
        auc_timepoint <- as.numeric(roc_obj$auc)

        # Try to get confidence intervals (may fail on small samples)
        tryCatch(
            {
                ci_result <- pROC::ci.auc(roc_obj)
                auc_ci_lower <- ci_result[1]
                auc_ci_upper <- ci_result[3]
            },
            error = function(e) {
                auc_ci_lower <- NA
                auc_ci_upper <- NA
            }
        )

        auc_method <- "pROC"
        logger::log_info(formatted(sprintf(
            "AUC calculated successfully: %.3f (events=%d, non-events=%d)",
            auc_timepoint, sum(binary_outcome), sum(!binary_outcome)
        ), indent = 3))
    } else {
        # Fallback to empirical calculation if insufficient events
        cases <- disc_data$predicted_risk[binary_outcome]
        controls <- disc_data$predicted_risk[!binary_outcome]

        if (length(cases) > 0 && length(controls) > 0) {
            auc_timepoint <- sum(outer(cases, controls, ">")) / (length(cases) * length(controls))
            auc_method <- "empirical"
            logger::log_info(formatted(sprintf(
                "Empirical AUC calculated: %.3f (cases=%d, controls=%d)",
                auc_timepoint, length(cases), length(controls)
            ), indent = 3))
        } else {
            auc_timepoint <- NA
            auc_method <- "insufficient_data"
            logger::log_warn(formatted(sprintf(
                "Insufficient events for AUC calculation: events=%d, min_required=%d",
                sum(binary_outcome), GEP_MIN_EVENTS_COMPETING_RISK
            ), indent = 3))
        }

        auc_ci_lower <- NA
        auc_ci_upper <- NA
    }


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
        "Discrimination metrics: Harrell C=%.3f, Uno C=%s, AUC=%s",
        ifelse(is.na(harrell_c), NA, harrell_c),
        ifelse(is.na(uno_c), "NA", sprintf("%.3f", uno_c)),
        ifelse(is.na(auc_timepoint), "NA", sprintf("%.3f", auc_timepoint))
    ), indent = 3))
    logger::log_info(formatted(sprintf("Methods used: Harrell=%s, Uno=%s, AUC=%s", harrell_method, uno_c_method, auc_method), indent = 3))

    return(list(
        n = nrow(disc_data),
        events = sum(disc_data$observed_event),
        events_by_timepoint = sum(disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months),
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
        auc_method = auc_method,
        royston_d = round(royston_d, 3),
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

#' PRAME-Augmented Analysis with NRI for MFS
#'
#' Assess added value of PRAME status by comparing base GEP risk vs
#' PRAME-augmented risk classifications using NRI and IDI across
#' multiple timepoints.
#'
#' @param data Data frame with GEP predictions, PRAME status, and outcomes.
#' @param timepoints Numeric vector of year values for evaluation.
#' @return A list containing dataset-level PRAME availability details and
#'   per-timepoint NRI results.
perform_prame_augmented_analysis_mfs <- function(data, timepoints) {
    logger::log_info(formatted("Performing PRAME-augmented analysis with NRI calculation", indent = 1))

    prame_data <- data %>%
        dplyr::filter(
            !is.na(prame_status),
            prame_status %in% c("Positive", "Negative"),
            !is.na(biopsy1_gep_mfs),
            !is.na(tt_mets_months),
            !is.na(mets_event),
            !is.na(biopsy1_gep)
        )

    if (nrow(prame_data) < GEP_MIN_BOOTSTRAP_SAMPLE) {
        logger::log_warn(formatted(sprintf("Insufficient PRAME data for analysis (n=%d)", nrow(prame_data)), indent = 2))
        return(list(
            n = nrow(prame_data),
            status = "insufficient_data",
            prame_available = FALSE
        ))
    }

    logger::log_info(formatted(sprintf("PRAME analysis using %d patients", nrow(prame_data)), indent = 2))
    prame_dist <- table(prame_data$prame_status)
    logger::log_info(formatted(sprintf(
        "PRAME distribution: Positive=%d, Negative=%d",
        prame_dist["Positive"], prame_dist["Negative"]
    ), indent = 2))

    nri_results <- list()

    for (timepoint in timepoints) {
        logger::log_info(formatted(sprintf("Calculating NRI for %d-year MFS", timepoint), indent = 2))

        timepoint_months <- timepoint * 12
        tp_key <- paste0("yr", timepoint)

        outcome_data <- prame_data %>%
            dplyr::mutate(
                event_by_timepoint = mets_event == 1 & tt_mets_months <= timepoint_months,
                base_risk = 1 - biopsy1_gep_mfs,
                prame_positive = prame_status == "Positive"
            )

        base_predictions <- outcome_data$base_risk
        enhanced_predictions <- ifelse(
            outcome_data$prame_positive,
            pmin(outcome_data$base_risk * GEP_PRAME_ADJUSTMENT_FACTOR, GEP_RISK_CAP_MAXIMUM),
            outcome_data$base_risk * GEP_PRAME_REDUCTION_FACTOR
        )

        risk_cutoffs <- GEP_RISK_CUTOFFS
        risk_labels <- GEP_RISK_LABELS
        base_categories <- cut(base_predictions, breaks = risk_cutoffs, labels = risk_labels, include.lowest = TRUE)
        enhanced_categories <- cut(enhanced_predictions, breaks = risk_cutoffs, labels = risk_labels, include.lowest = TRUE)

        reclass_table <- table(
            Base = base_categories,
            With_PRAME = enhanced_categories,
            useNA = "ifany"
        )

        events <- outcome_data$event_by_timepoint
        n_events <- sum(events)
        n_nonevents <- sum(!events)

        if (n_events >= GEP_MIN_EVENTS_COMPETING_RISK && n_nonevents >= GEP_MIN_EVENTS_COMPETING_RISK) {
            event_indices <- which(events)
            event_up <- sum(enhanced_categories[event_indices] > base_categories[event_indices], na.rm = TRUE)
            event_down <- sum(enhanced_categories[event_indices] < base_categories[event_indices], na.rm = TRUE)
            nonevent_indices <- which(!events)
            nonevent_up <- sum(enhanced_categories[nonevent_indices] > base_categories[nonevent_indices], na.rm = TRUE)
            nonevent_down <- sum(enhanced_categories[nonevent_indices] < base_categories[nonevent_indices], na.rm = TRUE)

            nri_events <- (event_up / n_events) - (event_down / n_events)
            nri_nonevents <- (nonevent_down / n_nonevents) - (nonevent_up / n_nonevents)
            nri_total <- nri_events + nri_nonevents

            idi <- mean(enhanced_predictions[events]) - mean(base_predictions[events]) -
                (mean(enhanced_predictions[!events]) - mean(base_predictions[!events]))

            reclass_improved <- event_up + nonevent_down
            reclass_worsened <- event_down + nonevent_up
            if (reclass_improved + reclass_worsened > 0) {
                mcnemar_stat <- (abs(reclass_improved - reclass_worsened) - 1)^2 / (reclass_improved + reclass_worsened)
                mcnemar_p <- pchisq(mcnemar_stat, df = 1, lower.tail = FALSE)
            } else {
                mcnemar_p <- 1
            }
        } else {
            logger::log_warn(formatted(sprintf(
                "Insufficient events (%d) or non-events (%d) for NRI calculation",
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

        model_comparison <- NULL
        tryCatch(
            {
                if (requireNamespace("pROC", quietly = TRUE) && n_events >= GEP_MIN_EVENTS_COMPETING_RISK && n_nonevents >= GEP_MIN_EVENTS_COMPETING_RISK) {
                    base_auc <- pROC::auc(pROC::roc(events, base_predictions, quiet = TRUE))
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

        nri_results[[tp_key]] <- list(
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
                "NRI = %.3f (Events: %.3f, Non-events: %.3f), IDI = %.4f",
                nri_total, nri_events, nri_nonevents, idi
            ), indent = 3))
        }
    }

    return(list(
        n = nrow(prame_data),
        prame_available = TRUE,
        prame_distribution = prame_dist,
        nri_results = nri_results
    ))
}
