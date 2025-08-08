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
calculate_observed_expected_mfs <- function(data, timepoint) {
    log_enhanced(sprintf("Calculating O/E ratios for %d-year MFS", timepoint), level = "INFO", indent = 2)
    
    # Convert timepoint to months for comparison
    timepoint_months <- timepoint * 12
    
    # Calculate observed and expected by GEP class
    results_by_class <- list()
    
    for (gep_class in c("Class 1A", "Class 1B", "Class 2")) {
        class_data <- data %>% filter(gep_class_simple == gep_class)
        
        if (nrow(class_data) == 0) {
            results_by_class[[gep_class]] <- list(
                n = 0, observed = 0, expected = 0, oe_ratio = NA,
                poisson_ci_lower = NA, poisson_ci_upper = NA
            )
            next
        }
        
        # Get expected survival probability for this timepoint
        expected_var <- paste0("expected_mfs_", timepoint, "yr")
        
        # Calculate observed events (metastasis within timepoint)
        observed_events <- sum(class_data$mets_event == 1 & class_data$tt_mets_months <= timepoint_months)
        
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
        
        log_enhanced(sprintf("%s: O=%d, E=%.1f, O/E=%.3f (95%% CI: %.3f-%.3f)", 
                           gep_class, observed_events, expected_events, oe_ratio, 
                           poisson_ci_lower, poisson_ci_upper), level = "INFO", indent = 3)
    }
    
    # Overall chi-square goodness of fit test
    observed_total <- sum(sapply(results_by_class, function(x) x$observed))
    expected_total <- sum(sapply(results_by_class, function(x) x$expected))
    
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
#'   `nam_dagostino_p`, `ici`, `calibration_slope`, `calibration_intercept`,
#'   and `group_results`.
perform_calibration_mfs <- function(data, timepoint, bootstrap_iterations) {
    log_enhanced(sprintf("Performing calibration assessment for %d-year MFS", timepoint), level = "INFO", indent = 2)
    
    # Prepare data for calibration analysis
    timepoint_months <- timepoint * 12
    expected_var <- paste0("expected_mfs_", timepoint, "yr")
    
    # Create analysis dataset with complete cases
    cal_data <- data %>%
        filter(!is.na(.data[[expected_var]]), !is.na(tt_mets_months), !is.na(mets_event)) %>%
        mutate(
            predicted_prob = .data[[expected_var]],
            observed_time = tt_mets_months,
            observed_event = mets_event,
            # Convert predicted survival to risk (1 - survival probability)
            predicted_risk = 1 - predicted_prob
        )
    
    if (nrow(cal_data) < GEP_MIN_SAMPLE_SIZE) {
        log_enhanced("Insufficient data for calibration analysis", level = "WARN", indent = 3)
        return(list(
            n = nrow(cal_data),
            status = "insufficient_data",
            nam_dagostino_p = NA,
            ici = NA,
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
                                  include.lowest = TRUE, labels = FALSE)
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
    ici <- NA
    
    # Nam-D'Agostino test
    if (nrow(group_results) >= 3 && sum(group_results$expected_events) > 0) {
        chisq_stat <- sum((group_results$observed_events - group_results$expected_events)^2 / 
                         pmax(group_results$expected_events, 1))
        nam_dagostino_p <- pchisq(chisq_stat, df = nrow(group_results) - 1, lower.tail = FALSE)
    } else {
        nam_dagostino_p <- NA
    }
    
    # 2. Integrated Calibration Index (ICI)
    # Use loess to estimate calibration curve
    if (nrow(cal_data) >= 2 * GEP_MIN_SAMPLE_SIZE) {
        tryCatch({
            # Calculate observed rates using Kaplan-Meier at timepoint
            km_fit <- survfit(surv_obj ~ 1)
            observed_survival_rate <- summary(km_fit, times = timepoint_months)$surv
            if (length(observed_survival_rate) == 0) observed_survival_rate <- 1
            
            # Create loess smooth of observed vs predicted
            loess_data <- cal_data %>%
                arrange(predicted_risk) %>%
                mutate(
                    # Estimate local observed rate using moving window
                    window_obs_rate = sapply(1:n(), function(i) {
                        window_indices <- max(1, i-10):min(nrow(.), i+10)
                        window_data <- cal_data[window_indices, ]
                        window_events <- sum(window_data$observed_event == 1 & window_data$observed_time <= timepoint_months)
                        window_events / length(window_indices)
                    })
                )
            
            # Fit loess
            loess_fit <- loess(window_obs_rate ~ predicted_risk, data = loess_data, span = GEP_LOESS_SPAN)
            loess_pred <- predict(loess_fit, newdata = loess_data$predicted_risk)
            
            # Calculate ICI as mean absolute difference
            ici <- mean(abs(loess_data$predicted_risk - loess_pred), na.rm = TRUE)
            
        }, error = function(e) {
            log_enhanced("Error calculating ICI, using simpler approach", level = "WARN", indent = 3)
            # Simpler ICI calculation
            observed_rate <- sum(cal_data$observed_event == 1 & cal_data$observed_time <= timepoint_months) / nrow(cal_data)
            mean_predicted_rate <- mean(cal_data$predicted_risk)
            ici <- abs(observed_rate - mean_predicted_rate)
        })
    } else {
        # Simple calibration for small samples
        observed_rate <- sum(cal_data$observed_event == 1 & cal_data$observed_time <= timepoint_months) / nrow(cal_data)
        mean_predicted_rate <- mean(cal_data$predicted_risk)
        ici <- abs(observed_rate - mean_predicted_rate)
    }
    
    # Ensure ici is defined in all cases
    if (!exists("ici") || is.null(ici) || is.na(ici)) {
        ici <- NA
    }
    
    # 3. Bootstrap-corrected calibration slope and intercept
    if (bootstrap_iterations > 0 && nrow(cal_data) >= GEP_MIN_BOOTSTRAP_SAMPLE) {
        tryCatch({
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
                tryCatch({
                    boot_cox <- coxph(boot_surv ~ predicted_risk, data = boot_data, model = TRUE)
                    coef(boot_cox)[1]
                }, error = function(e) NA)
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
            calibration_intercept <- 0  # In Cox models, no intercept
            
        }, error = function(e) {
            log_enhanced("Error in bootstrap calibration, using simple estimates", level = "WARN", indent = 3)
            calibration_slope <- 1  # Perfect calibration assumption
            calibration_intercept <- 0
        })
    } else {
        calibration_slope <- 1  # Default assumption
        calibration_intercept <- 0
    }
    
    log_enhanced(sprintf("Calibration metrics: Nam-D'Agostino p=%.4f, ICI=%.4f, Slope=%.3f", 
                        nam_dagostino_p, ici, calibration_slope), level = "INFO", indent = 3)
    
    return(list(
        n = nrow(cal_data),
        n_groups = nrow(group_results),
        nam_dagostino_statistic = chisq_stat,
        nam_dagostino_p = round(nam_dagostino_p, 4),
        ici = round(ici, 4),
        calibration_slope = round(calibration_slope, 3),
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
    log_enhanced(sprintf("Performing discrimination analysis for %d-year MFS", timepoint), level = "INFO", indent = 2)
    
    # Prepare data
    timepoint_months <- timepoint * 12
    
    # Log time-specific analysis details
    log_enhanced(sprintf("Time-specific analysis: censoring at %d months (%d years)", timepoint_months, timepoint), level = "INFO", indent = 3)
    expected_var <- paste0("expected_mfs_", timepoint, "yr")
    
    disc_data <- data %>%
        dplyr::filter(!is.na(.data[[expected_var]]), !is.na(tt_mets_months), !is.na(mets_event)) %>%
        dplyr::mutate(
            predicted_prob = .data[[expected_var]],
            predicted_risk = 1 - predicted_prob,  # Convert survival prob to risk
            observed_time = tt_mets_months,
            observed_event = mets_event
        )
    
    if (nrow(disc_data) < GEP_MIN_SAMPLE_SIZE) {
        log_enhanced("Insufficient data for discrimination analysis", level = "WARN", indent = 3)
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
    tryCatch({
        # Create time-specific outcome for the specific timepoint
        time_specific_event <- disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months
        time_specific_time <- pmin(disc_data$observed_time, timepoint_months)
        
        # Use survcomp package for Harrell's C-index with time-specific data
        if (requireNamespace("survcomp", quietly = TRUE)) {
            harrell_result <- survcomp::concordance.index(
                x = disc_data$predicted_risk,
                surv.time = time_specific_time,
                surv.event = time_specific_event,
                method = "noether"
            )
            harrell_c <- harrell_result$c.index
            harrell_ci_lower <- harrell_result$lower
            harrell_ci_upper <- harrell_result$upper
        } else {
            # Fallback using survival package with time-specific data
            time_specific_surv <- Surv(time_specific_time, time_specific_event)
            cox_fit <- coxph(time_specific_surv ~ predicted_risk, data = disc_data, model = TRUE)
            harrell_c <- summary(cox_fit)$concordance[1]
            harrell_ci_lower <- NA
            harrell_ci_upper <- NA
        }
    }, error = function(e) {
        log_enhanced("Error calculating Harrell's C-index", level = "WARN", indent = 3)
        harrell_c <- NA
        harrell_ci_lower <- NA
        harrell_ci_upper <- NA
    })
    
    # 2. Uno's censoring-adjusted C-index - TIME-SPECIFIC
    uno_c <- NA
    uno_ci_lower <- NA
    uno_ci_upper <- NA
    tryCatch({
        if (requireNamespace("survcomp", quietly = TRUE)) {
            # Use same time-specific data for Uno's C-index
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
        }
    }, error = function(e) {
        log_enhanced("Error calculating Uno's C-index", level = "WARN", indent = 3)
    })
    
    # 3. Time-specific AUC (cumulative/dynamic ROC) - TIME-SPECIFIC
    auc_timepoint <- NA
    auc_ci_lower <- NA
    auc_ci_upper <- NA
    tryCatch({
        # Use riskRegression package for time-dependent ROC
        if (requireNamespace("riskRegression", quietly = TRUE)) {
            # Create time-specific survival object for ROC analysis
            time_specific_event <- disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months
            time_specific_time <- pmin(disc_data$observed_time, timepoint_months)
            time_specific_surv <- Surv(time_specific_time, time_specific_event)
            
            # Create a simple model for ROC analysis with time-specific data
            cox_model <- coxph(time_specific_surv ~ predicted_risk, data = disc_data, model = TRUE)
            
            # Calculate AUC at specific timepoint
            roc_result <- riskRegression::Score(
                list("GEP" = cox_model),
                formula = time_specific_surv ~ 1,
                data = disc_data,
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
        } else {
            # Alternative using pROC package for binary classification at timepoint
            if (requireNamespace("pROC", quietly = TRUE)) {
                # Create binary outcome: event within timepoint (already time-specific)
                binary_outcome <- disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months
                
                if (sum(binary_outcome) > GEP_MIN_EVENTS_COMPETING_RISK && sum(!binary_outcome) > GEP_MIN_EVENTS_COMPETING_RISK) {
                    roc_obj <- pROC::roc(binary_outcome, disc_data$predicted_risk, quiet = TRUE)
                    auc_timepoint <- as.numeric(roc_obj$auc)
                    
                    # Calculate confidence interval
                    tryCatch({
                        ci_result <- pROC::ci.auc(roc_obj)
                        auc_ci_lower <- ci_result[1]
                        auc_ci_upper <- ci_result[3]
                    }, error = function(e) {
                        auc_ci_lower <- NA
                        auc_ci_upper <- NA
                    })
                }
            }
        }
    }, error = function(e) {
        log_enhanced("Error calculating time-specific AUC", level = "WARN", indent = 3)
    })
    
    # 4. Additional discrimination metrics
    # Royston's D statistic if possible
    royston_d <- NA
    tryCatch({
        if (!is.na(harrell_c)) {
            # Approximate Royston's D from C-index
            royston_d <- 2 * qnorm(harrell_c) * sqrt(2/pi)
        }
    }, error = function(e) {
        royston_d <- NA
    })
    
    # Summarize results
    events_at_timepoint <- sum(disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months)
    total_at_timepoint <- nrow(disc_data)
    
    log_enhanced(sprintf("Timepoint %d years: %d events out of %d patients", timepoint, events_at_timepoint, total_at_timepoint), level = "INFO", indent = 3)
    log_enhanced(sprintf("Discrimination metrics: Harrell C=%.3f, Uno C=%s, AUC=%s", 
                        ifelse(is.na(harrell_c), NA, harrell_c), 
                        ifelse(is.na(uno_c), "NA", sprintf("%.3f", uno_c)), 
                        ifelse(is.na(auc_timepoint), "NA", sprintf("%.3f", auc_timepoint))), level = "INFO", indent = 3)
    
    return(list(
        n = nrow(disc_data),
        events = sum(disc_data$observed_event),
        events_by_timepoint = sum(disc_data$observed_event == 1 & disc_data$observed_time <= timepoint_months),
        harrell_c = round(harrell_c, 3),
        harrell_ci_lower = round(harrell_ci_lower, 3),
        harrell_ci_upper = round(harrell_ci_upper, 3),
        uno_c = round(uno_c, 3),
        uno_ci_lower = round(uno_ci_lower, 3),
        uno_ci_upper = round(uno_ci_upper, 3),
        auc_timepoint = round(auc_timepoint, 3),
        auc_ci_lower = round(auc_ci_lower, 3),
        auc_ci_upper = round(auc_ci_upper, 3),
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
    log_enhanced(sprintf("Performing decision curve analysis for %d-year MFS", timepoint), level = "INFO", indent = 2)
    
    # Prepare data
    timepoint_months <- timepoint * 12
    expected_var <- paste0("expected_mfs_", timepoint, "yr")
    
    dca_data <- data %>%
        dplyr::filter(!is.na(.data[[expected_var]]), !is.na(tt_mets_months), !is.na(mets_event)) %>%
        dplyr::mutate(
            predicted_risk = 1 - .data[[expected_var]],  # Convert survival prob to risk
            observed_time = tt_mets_months,
            observed_event = mets_event,
            outcome = observed_event == 1 & observed_time <= timepoint_months
        )
    
    if (nrow(dca_data) < GEP_MIN_SAMPLE_SIZE) {
        log_enhanced("Insufficient data for decision curve analysis", level = "WARN", indent = 3)
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
    
    log_enhanced(sprintf("DCA optimal threshold: %.2f%% (Net benefit: %.4f)", 
                        optimal_threshold * 100, optimal_net_benefit), level = "INFO", indent = 3)
    
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
    log_enhanced("Performing PRAME-augmented analysis with NRI calculation", level = "INFO", indent = 1)
    
    prame_data <- data %>%
        dplyr::filter(
            !is.na(prame_status),
            prame_status %in% c("Positive", "Negative"),
            !is.na(biopsy1_gep_mfs),
            !is.na(tt_mets_months),
            !is.na(mets_event),
            gep_class_simple %in% c("Class 1A", "Class 1B", "Class 2")
        )
    
    if (nrow(prame_data) < GEP_MIN_BOOTSTRAP_SAMPLE) {
        log_enhanced(sprintf("Insufficient PRAME data for analysis (n=%d)", nrow(prame_data)), level = "WARN", indent = 2)
        return(list(
            n = nrow(prame_data),
            status = "insufficient_data",
            prame_available = FALSE
        ))
    }
    
    log_enhanced(sprintf("PRAME analysis using %d patients", nrow(prame_data)), level = "INFO", indent = 2)
    prame_dist <- table(prame_data$prame_status)
    log_enhanced(sprintf("PRAME distribution: Positive=%d, Negative=%d", 
                        prame_dist["Positive"], prame_dist["Negative"]), level = "INFO", indent = 2)
    
    nri_results <- list()
    
    for (timepoint in timepoints) {
        log_enhanced(sprintf("Calculating NRI for %d-year MFS", timepoint), level = "INFO", indent = 2)
        
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
            log_enhanced(sprintf("Insufficient events (%d) or non-events (%d) for NRI calculation", 
                               n_events, n_nonevents), level = "WARN", indent = 3)
            nri_events <- NA
            nri_nonevents <- NA
            nri_total <- NA
            idi <- NA
            mcnemar_p <- NA
            event_up <- 0; event_down <- 0; nonevent_up <- 0; nonevent_down <- 0
        }
        
        model_comparison <- NULL
        tryCatch({
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
        }, error = function(e) {
            model_comparison <- NULL
        })
        
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
            log_enhanced(sprintf("NRI = %.3f (Events: %.3f, Non-events: %.3f), IDI = %.4f", 
                               nri_total, nri_events, nri_nonevents, idi), level = "INFO", indent = 3)
        }
    }
    
    return(list(
        n = nrow(prame_data),
        prame_available = TRUE,
        prame_distribution = prame_dist,
        nri_results = nri_results
    ))
}
