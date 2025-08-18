# GEP Model Evaluation Metrics (shared calculators)

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
    results <- data %>%
        dplyr::group_by(.data[[group_var]]) %>%
        dplyr::summarise(
            n = dplyr::n(),
            observed = sum(.data[[event_var]]),
            expected = sum(1 - .data[[expected_var]]),
            .groups = "drop"
        ) %>%
        dplyr::mutate(
            oe_ratio = ifelse(expected > 0, observed / expected, NA_real_),
            expected_rate = ifelse(n > 0, expected / n, NA_real_),
            observed_rate = ifelse(n > 0, observed / n, NA_real_)
        )
    return(results)
}

#' Calculate calibration metrics (simple logistic calibration)
#'
#' Fit a logistic calibration model of observed event vs predicted probability
#' and derive intercept, slope, Integrated Calibration Index (ICI), and a
#' Nam-D'Agostino p-value proxy.
#'
#' @param data Data frame containing observed outcome and predicted probabilities
#' @param expected_var Character name of predicted probability column
#' @param event_var Character name of binary event indicator column
#' @param time_var Character name of time variable (not used in simplified method)
#' @return A list with `n`, `intercept`, `slope`, `ici`, and `nam_dagostino_p` fields
calculate_calibration_metrics <- function(data, expected_var, event_var, time_var) {
    logger::log_debug("Calculating calibration metrics")
    # Guard: need at least two distinct predicted probabilities and at least one event/non-event
    if (length(unique(stats::na.omit(data[[expected_var]]))) < 2 ||
        sum(data[[event_var]] == 1, na.rm = TRUE) == 0 ||
        sum(data[[event_var]] == 0, na.rm = TRUE) == 0) {
        return(list(
            n = nrow(data),
            intercept = NA_real_,
            slope = NA_real_,
            ici = NA_real_,
            nam_dagostino_p = NA_real_
        ))
    }
    calibration_model <- glm(stats::as.formula(paste(event_var, "~", expected_var)),
        data = data, family = binomial()
    )
    co <- coef(calibration_model)
    intercept <- unname(ifelse(length(co) >= 1, co[1], NA_real_))
    slope <- unname(ifelse(length(co) >= 2, co[2], NA_real_))
    predicted_probs <- predict(calibration_model, type = "response")
    ici <- mean(abs(predicted_probs - data[[expected_var]]))
    summ <- summary(calibration_model)
    nam_dagostino_p <- tryCatch(summ$coefficients[2, 4], error = function(e) NA_real_)
    return(list(
        n = nrow(data),
        intercept = intercept,
        slope = slope,
        ici = ici,
        nam_dagostino_p = nam_dagostino_p
    ))
}

#' Calculate discrimination metrics (simplified)
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
    uno_c <- harrell_c
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
        uno_c = uno_c,
        c_ci_lower = c_ci_lower,
        c_ci_upper = c_ci_upper,
        stringsAsFactors = FALSE
    ))
}

#' Calculate cumulative incidence (simplified)
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
            print(df_class)
            print(df_class[[time_var]])
            print(df_class[[event_type_var]])

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
