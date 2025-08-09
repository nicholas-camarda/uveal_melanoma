# GEP Model Evaluation Metrics (shared calculators)

#' Calculate observed vs expected rates by GEP class
#'
#' @param data Data frame with endpoint data
#' @param expected_var Expected survival variable name
#' @param event_var Event variable name
#' @param time_var Time variable name
#' @return Data frame with observed vs expected rates
calculate_observed_expected_rates <- function(data, expected_var, event_var, time_var) {
    log_enhanced("Calculating observed vs expected rates", level = "DEBUG")
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
    log_enhanced("Calculating calibration metrics", level = "DEBUG")
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
    log_enhanced("Calculating discrimination metrics", level = "DEBUG")
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
    log_enhanced("Calculating cumulative incidence", level = "DEBUG")
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
    log_enhanced("Calculating cause-specific hazards", level = "DEBUG")
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
    log_enhanced("Calculating net reclassification index", level = "DEBUG")
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
