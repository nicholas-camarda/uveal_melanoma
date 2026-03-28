# GEP Table Creation Functions
# Table generation and formatting for GEP validation results

#' Extract overall observed/expected metrics
#'
#' Normalize the different observed/expected result shapes used across the GEP
#' pipeline into a single overall summary structure.
#'
#' @param observed_expected Either a nested list result or a data frame with
#'   observed/expected quantities.
#' @return A list with overall counts, observed and expected totals, O/E ratio,
#'   confidence interval bounds, and goodness-of-fit p-value, or `NULL` if the
#'   input cannot be interpreted.
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
                chi_square_p = observed_expected$chisq_p_value %||% NA_real_,
                chi_square_log_p = observed_expected$chisq_log_p_value %||% NA_real_
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
        chi_square_log_p <- attr(observed_expected, "chisq_log_p_value", exact = TRUE)

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
                chi_square_p = chi_square_p,
                chi_square_log_p = chi_square_log_p %||% NA_real_
        ))
    }

    NULL
}

#' Create Detailed Metrics Table
#'
#' Render a Markdown table of calibration, discrimination, and overall O/E
#' metrics for each available validation timepoint.
#'
#' @param validation_results Named list of timepoint-specific validation result
#'   objects.
#' @return Character vector containing Markdown table lines.
create_detailed_metrics_table <- function(validation_results) {
    if (is.null(validation_results) || length(validation_results) == 0) {
        return(md_bullet("No detailed timepoint metrics were available."))
    }

    format_scalar <- function(value, digits = 3) {
        value <- suppressWarnings(as.numeric(value))
        if (length(value) == 0 || is.na(value[[1]]) || !is.finite(value[[1]])) {
            return("NA")
        }

        sprintf(paste0("%.", digits, "f"), value[[1]])
    }

    format_calibration_cell <- function(result) {
        if (is.null(result$calibration)) {
            return("Unavailable")
        }

        cal <- result$calibration
        slope_val <- cal$slope %||% cal$calibration_slope %||% NA_real_
        ici_val <- cal$ici %||% NA_real_
        slope_method <- cal$slope_method %||% NA_character_
        ici_method <- cal$ici_method %||% NA_character_
        status <- cal$status %||% NA_character_
        p_value <- format_gep_p_value(
            cal$nam_dagostino_p %||% NA_real_,
            log_p_value = cal$nam_dagostino_log_p %||% NA_real_
        )
        slope_method_text <- if (length(slope_method) > 0 && !is.na(slope_method[[1]])) as.character(slope_method[[1]]) else NULL
        ici_method_text <- if (length(ici_method) > 0 && !is.na(ici_method[[1]])) as.character(ici_method[[1]]) else "NA"
        status_text <- if (length(status) > 0 && !is.na(status[[1]])) as.character(status[[1]]) else NULL

        pieces <- c(
            sprintf("Slope %s", format_scalar(slope_val)),
            sprintf("ICI %s [%s]", format_scalar(ici_val), ici_method_text),
            sprintf("Nam-D'Agostino p=%s", p_value)
        )

        if (!is.null(slope_method_text)) {
            pieces <- c(pieces, sprintf("Slope method %s", slope_method_text))
        }
        if (!is.null(status_text)) {
            pieces <- c(pieces, sprintf("Status %s", status_text))
        }

        paste(pieces, collapse = "; ")
    }

    format_discrimination_cell <- function(result) {
        if (is.null(result$discrimination)) {
            return("Unavailable")
        }

        disc <- result$discrimination
        pieces <- c(
            sprintf("Harrell's C %s", format_scalar(disc$harrell_c)),
            sprintf("Integrated AUC %s", format_scalar(disc$integrated_auc)),
            sprintf("Cumulative Disc %s", format_scalar(disc$cumulative_discrimination))
        )

        paste(pieces, collapse = "; ")
    }

    format_oe_cell <- function(result) {
        oe <- extract_overall_oe_metrics(result$observed_expected)
        if (is.null(oe)) {
            return("Unavailable")
        }

        sprintf(
            "Overall O/E %s (%s-%s); Chi-square p=%s",
            format_scalar(oe$oe_ratio),
            format_scalar(oe$poisson_ci_lower),
            format_scalar(oe$poisson_ci_upper),
            format_gep_p_value(oe$chi_square_p, log_p_value = oe$chi_square_log_p)
        )
    }

    table_rows <- lapply(names(validation_results), function(tp) {
        result <- validation_results[[tp]]
        data.frame(
            Timepoint = tp,
            Calibration = format_calibration_cell(result),
            Discrimination = format_discrimination_cell(result),
            `Observed vs Expected` = format_oe_cell(result),
            stringsAsFactors = FALSE,
            check.names = FALSE
        )
    })

    md_table(do.call(rbind, table_rows))
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
                calibration_slope_method = if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$slope_method)) tp_results$calibration$slope_method else NA,
                calibration_ici_method = if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$ici_method)) tp_results$calibration$ici_method else NA,
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
                    calibration_slope_method = if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$slope_method)) tp_results$calibration$slope_method else NA,
                    calibration_ici_method = if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$ici_method)) tp_results$calibration$ici_method else NA,
                    harrell_c = harrell_c,
                    integrated_auc = integrated_auc,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
    return(comparison_data)
}
