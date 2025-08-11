# GEP Model Evaluation Metrics (shared calculators)

#' Calculate observed vs expected rates by GEP class
#'
#' @param data Data frame with endpoint data
#' @param expected_var Expected survival variable name
#' @param event_var Event variable name
#' @param time_var Time variable name
#' @return Data frame with observed vs expected rates
calculate_observed_expected_rates <- function(data, expected_var, event_var, time_var) {
    logger::log_debug("Calculating observed vs expected rates")
    results <- data %>%
        dplyr::group_by(gep_class_simple) %>%
        dplyr::summarise(
            n = dplyr::n(),
            observed = sum(.data[[event_var]]),
            expected = sum(1 - .data[[expected_var]]),
            .groups = "drop"
        ) %>%
        dplyr::mutate(
            oe_ratio = ifelse(expected > 0, observed / expected, NA),
            expected_rate = ifelse(n > 0, expected / n, NA),
            observed_rate = ifelse(n > 0, observed / n, NA)
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
#' @return A data.frame with `intercept`, `slope`, `ici`, and `nam_dagostino_p` columns
calculate_calibration_metrics <- function(data, expected_var, event_var, time_var) {
    logger::log_debug("Calculating calibration metrics")
    calibration_model <- glm(as.formula(paste(event_var, "~", expected_var)),
        data = data, family = binomial()
    )
    intercept <- coef(calibration_model)[1]
    slope <- coef(calibration_model)[2]
    predicted_probs <- predict(calibration_model, type = "response")
    ici <- mean(abs(predicted_probs - data[[expected_var]]))
    nam_dagostino_p <- summary(calibration_model)$coefficients[2, 4]
    return(data.frame(
        intercept = intercept,
        slope = slope,
        ici = ici,
        nam_dagostino_p = nam_dagostino_p,
        stringsAsFactors = FALSE
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
#' Uses the nonparametric Aalen–Johansen estimator via `cmprsk::cuminc` when
#' available. Confidence intervals are computed by:
#' 1) Stratified bootstrap on patients within `gep_class_simple` (default),
#' 2) If bootstrap not requested or fails, normal approximation using the
#'    Greenwood-type variance from `cmprsk` when provided.
#'
#' @param data Data frame with columns: time_var, event_type (1=melanoma death,
#'   2=competing, 0=censored), gep_class_simple
#' @param time_var Character time variable name (in years)
#' @param event_type_var Character event type variable name (0/1/2)
#' @param eval_time Numeric time point (years)
#' @param n_boot Integer number of bootstrap resamples (default 1000)
#' @return Data frame with columns: gep_class_simple, n, cif, ci_lower, ci_upper
calculate_cif_by_class_with_ci <- function(data, time_var, event_type_var, eval_time, n_boot = 1000) {
    if (!"gep_class_simple" %in% names(data)) {
        stop("calculate_cif_by_class_with_ci requires 'gep_class_simple' column")
    }
    if (!requireNamespace("cmprsk", quietly = TRUE)) {
        logger::log_warn("'cmprsk' not installed; cannot compute CIF CIs. Returning NA CIs.")
        base <- data %>% dplyr::count(gep_class_simple, name = "n") %>% dplyr::mutate(cif = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_)
        return(base)
    }

    # Helper to get CIF at eval_time for one class
    get_cif_for_class <- function(df_class) {
        ci_obj <- tryCatch({
            cmprsk::cuminc(ftime = df_class[[time_var]], fstatus = df_class[[event_type_var]])
        }, error = function(e) NULL)
        if (is.null(ci_obj) || is.null(ci_obj$`1`)) return(NA_real_)
        # CIF curve for cause 1
        times <- ci_obj$`1`$time
        est <- ci_obj$`1`$est
        # step function: last value <= eval_time
        idx <- max(which(times <= eval_time), na.rm = TRUE)
        if (!is.finite(idx) || idx == -Inf) return(0)
        return(est[idx])
    }

    classes <- unique(data$gep_class_simple)
    base_rows <- lapply(classes, function(cls) {
        dfc <- data[data$gep_class_simple == cls, , drop = FALSE]
        cif_hat <- suppressWarnings(get_cif_for_class(dfc))
        data.frame(gep_class_simple = cls, n = nrow(dfc), cif = as.numeric(cif_hat), stringsAsFactors = FALSE)
    })
    results <- do.call(rbind, base_rows)

    # Bootstrap CIs (percentile)
    if (n_boot > 0 && nrow(data) > 0) {
        set.seed(123)
        boot_mat <- matrix(NA_real_, nrow = n_boot, ncol = nrow(results))
        colnames(boot_mat) <- results$gep_class_simple
        for (b in seq_len(n_boot)) {
            df_boot <- do.call(rbind, lapply(classes, function(cls) {
                dfc <- data[data$gep_class_simple == cls, , drop = FALSE]
                if (nrow(dfc) == 0) return(dfc)
                dfc[sample.int(nrow(dfc), size = nrow(dfc), replace = TRUE), , drop = FALSE]
            }))
            for (j in seq_along(classes)) {
                cls <- classes[j]
                dfc <- df_boot[df_boot$gep_class_simple == cls, , drop = FALSE]
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
}
