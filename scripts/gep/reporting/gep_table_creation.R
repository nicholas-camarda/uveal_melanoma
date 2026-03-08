# GEP Table Creation Functions
# Table generation and formatting for GEP validation results

#' Extract overall observed/expected metrics
extract_overall_oe_metrics <- function(observed_expected) {
    if (is.null(observed_expected)) {
        return(NULL)
    }

    derive_overall_n <- function(observed_expected) {
        overall_n <- observed_expected$overall_n %||% NA_real_
        if (!is.na(overall_n)) {
            return(overall_n)
        }

        results_by_class <- observed_expected$results_by_class %||% NULL
        if (!is.list(results_by_class)) {
            return(NA_real_)
        }

        class_n <- vapply(results_by_class, function(result) {
            result$n %||% NA_real_
        }, numeric(1))

        if (all(is.na(class_n))) {
            return(NA_real_)
        }

        sum(class_n, na.rm = TRUE)
    }

    if (is.list(observed_expected) && !is.data.frame(observed_expected)) {
        if (all(c("overall_observed", "overall_expected", "overall_oe_ratio") %in% names(observed_expected))) {
            return(list(
                n = derive_overall_n(observed_expected),
                observed = observed_expected$overall_observed %||% NA_real_,
                expected = observed_expected$overall_expected %||% NA_real_,
                oe_ratio = observed_expected$overall_oe_ratio %||% NA_real_,
                poisson_ci_lower = observed_expected$overall_poisson_ci_lower %||% NA_real_,
                poisson_ci_upper = observed_expected$overall_poisson_ci_upper %||% NA_real_,
                chi_square_p = observed_expected$chisq_p_value %||% NA_real_
            ))
        }
    }

    if (is.data.frame(observed_expected)) {
        if (all(c("observed", "expected") %in% names(observed_expected))) {
            observed_total <- sum(observed_expected$observed, na.rm = TRUE)
            expected_total <- sum(observed_expected$expected, na.rm = TRUE)
        } else if (all(c("observed_rate", "expected_rate", "n") %in% names(observed_expected))) {
            observed_total <- sum(observed_expected$observed_rate * observed_expected$n, na.rm = TRUE)
            expected_total <- sum(observed_expected$expected_rate * observed_expected$n, na.rm = TRUE)
        } else {
            return(NULL)
        }

        n_total <- if ("n" %in% names(observed_expected)) sum(observed_expected$n, na.rm = TRUE) else NA_real_

        poisson_ci_lower <- attr(observed_expected, "overall_poisson_ci_lower", exact = TRUE)
        poisson_ci_upper <- attr(observed_expected, "overall_poisson_ci_upper", exact = TRUE)
        chi_square_p <- attr(observed_expected, "chisq_p_value", exact = TRUE)

        if (is.null(poisson_ci_lower) || is.null(poisson_ci_upper)) {
            if (is.finite(expected_total) && expected_total > 0) {
                overall_poisson <- stats::poisson.test(observed_total)
                poisson_ci_lower <- overall_poisson$conf.int[1] / expected_total
                poisson_ci_upper <- overall_poisson$conf.int[2] / expected_total
            } else {
                poisson_ci_lower <- NA_real_
                poisson_ci_upper <- NA_real_
            }
        }

        if (is.null(chi_square_p)) {
            if (length(observed_expected$expected) > 1 && all(observed_expected$expected > 0, na.rm = TRUE) && sum(observed_expected$expected, na.rm = TRUE) > 0) {
                chisq_statistic <- sum(
                    (observed_expected$observed - observed_expected$expected)^2 / observed_expected$expected,
                    na.rm = TRUE
                )
                chi_square_p <- stats::pchisq(
                    chisq_statistic,
                    df = length(observed_expected$expected) - 1,
                    lower.tail = FALSE
                )
            } else {
                chi_square_p <- NA_real_
            }
        }

        return(list(
            n = n_total,
            observed = observed_total,
            expected = expected_total,
            oe_ratio = ifelse(expected_total > 0, observed_total / expected_total, NA_real_),
            poisson_ci_lower = round(poisson_ci_lower, 3),
            poisson_ci_upper = round(poisson_ci_upper, 3),
            chi_square_p = round(chi_square_p, 4)
        ))
    }

    NULL
}

#' Create Detailed Metrics Table
create_detailed_metrics_table <- function(validation_results) {
    table_lines <- c()
    
    for (tp in names(validation_results)) {
        result <- validation_results[[tp]]
        table_lines <- c(table_lines, sprintf("Timepoint: %s", tp))
        
        # Calibration - with defensive programming
        if (!is.null(result$calibration)) {
            cal <- result$calibration
            slope_val <- ifelse(is.null(cal$slope), NA_real_, cal$slope)
            ici_val <- ifelse(is.null(cal$ici), NA_real_, cal$ici)
            nam_dagostino_val <- ifelse(is.null(cal$nam_dagostino_p), NA_real_, cal$nam_dagostino_p)
            
            table_lines <- c(table_lines, 
                sprintf("  Calibration: slope=%.3f, ICI=%.3f, Nam-D'Agostino p=%.4f", 
                    slope_val, ici_val, nam_dagostino_val))
        }
        
        # Discrimination - with defensive programming
        if (!is.null(result$discrimination)) {
            disc <- result$discrimination
            harrell_val <- ifelse(is.null(disc$harrell_c), NA_real_, disc$harrell_c)
            integrated_auc_val <- ifelse(is.null(disc$integrated_auc), NA_real_, disc$integrated_auc)
            cumulative_disc_val <- ifelse(is.null(disc$cumulative_discrimination), NA_real_, disc$cumulative_discrimination)
            
            table_lines <- c(table_lines,
                sprintf("  Discrimination: Harrell's C=%.3f, Integrated AUC=%.3f, Cumulative Disc=%.3f",
                    harrell_val, integrated_auc_val, cumulative_disc_val))
        }
        
        # Observed/Expected - support both nested and tibble-based result shapes
        oe <- extract_overall_oe_metrics(result$observed_expected)
        if (!is.null(oe)) {
            oe_ratio_val <- oe$oe_ratio %||% NA_real_
            ci_lower_val <- oe$poisson_ci_lower %||% NA_real_
            ci_upper_val <- oe$poisson_ci_upper %||% NA_real_
            chi_square_val <- oe$chi_square_p %||% NA_real_

            table_lines <- c(table_lines,
                sprintf("  Overall O/E: %.2f (%.2f-%.2f); Chi-square p=%.4f",
                    oe_ratio_val, ci_lower_val, ci_upper_val, chi_square_val))
        }
        
        table_lines <- c(table_lines, "")
    }
    
    return(paste(table_lines, collapse = "\n"))
}

#' Create GEP comparison table
#'
#' Build a tidy comparison table of calibration and discrimination metrics
#' across outcomes (MFS/MSS) and timepoints.
#'
#' @param mfs_results MFS validation results (may be NULL)
#' @param mss_results MSS validation results (may be NULL)
#' @return A data.frame with rows for outcome/timepoint and key metrics
create_gep_comparison_table <- function(mfs_results, mss_results) {
    logger::log_info(formatted("Creating GEP comparison table"))
    comparison_data <- data.frame()
    if (!is.null(mfs_results) && !is.null(mfs_results$validation_results)) {
        for (tp_name in names(mfs_results$validation_results)) {
            tp_results <- mfs_results$validation_results[[tp_name]]
            cal_slope <- if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$slope)) tp_results$calibration$slope else tp_results$calibration$calibration_slope
            cal_intercept <- if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$intercept)) tp_results$calibration$intercept else NA
            harrell_c <- if (!is.null(tp_results$discrimination) && !is.null(tp_results$discrimination$harrell_c)) tp_results$discrimination$harrell_c else NA
            integrated_auc <- if (!is.null(tp_results$discrimination) && !is.null(tp_results$discrimination$integrated_auc)) tp_results$discrimination$integrated_auc else NA
            comparison_data <- rbind(comparison_data, data.frame(
                outcome = "MFS",
                timepoint = tp_name,
                calibration_slope = cal_slope,
                calibration_intercept = cal_intercept,
                harrell_c = harrell_c,
                integrated_auc = integrated_auc,
                stringsAsFactors = FALSE
            ))
        }
    }
    if (!is.null(mss_results)) {
        mss_container <- if (!is.null(mss_results$standard_validation)) mss_results$standard_validation else mss_results$standard_results
        if (!is.null(mss_container)) {
            for (tp_name in names(mss_container)) {
                tp_results <- mss_container[[tp_name]]
                cal_slope <- if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$slope)) tp_results$calibration$slope else tp_results$calibration$calibration_slope
                cal_intercept <- if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$intercept)) tp_results$calibration$intercept else NA
                harrell_c <- if (!is.null(tp_results$discrimination) && !is.null(tp_results$discrimination$harrell_c)) tp_results$discrimination$harrell_c else NA
                integrated_auc <- if (!is.null(tp_results$discrimination) && !is.null(tp_results$discrimination$integrated_auc)) tp_results$discrimination$integrated_auc else NA
                comparison_data <- rbind(comparison_data, data.frame(
                    outcome = "MSS",
                    timepoint = tp_name,
                    calibration_slope = cal_slope,
                    calibration_intercept = cal_intercept,
                    harrell_c = harrell_c,
                    integrated_auc = integrated_auc,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
    return(comparison_data)
}
