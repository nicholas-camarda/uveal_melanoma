# GEP Table Creation Functions
# Table generation and formatting for GEP validation results

#' Create Detailed Metrics Table
create_detailed_metrics_table <- function(validation_results, outcome_type) {
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
        
        # Observed/Expected - with defensive programming
        if (!is.null(result$observed_expected) && !is.null(result$observed_expected$overall)) {
            oe <- result$observed_expected$overall
            oe_ratio_val <- ifelse(is.null(oe$oe_ratio), NA_real_, oe$oe_ratio)
            ci_lower_val <- ifelse(is.null(oe$poisson_ci_lower), NA_real_, oe$poisson_ci_lower)
            ci_upper_val <- ifelse(is.null(oe$poisson_ci_upper), NA_real_, oe$poisson_ci_upper)
            chi_square_val <- ifelse(is.null(oe$chi_square_p), NA_real_, oe$chi_square_p)
            
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
