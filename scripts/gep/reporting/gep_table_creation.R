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

#' Create Consolidated GEP Summary Tables
#'
#' Generate comprehensive summary tables that consolidate redundant information
#' across timepoints, replacing multiple similar plots with interpretable tables.
#'
#' @param validation_results Named list of per-timepoint validation results
#' @param outcome_type Either "MFS" or "MSS" for appropriate clinical context
#' @param output_dir Directory to save the consolidated tables
#' @param prefix Filename prefix for saved files
#' @return List of paths to saved consolidated tables
create_consolidated_gep_tables <- function(validation_results, outcome_type, output_dir, prefix) {
    logger::log_info(sprintf("Creating consolidated GEP tables for %s", outcome_type))
    
    # Create calibration summary table across timepoints
    calibration_table <- create_calibration_summary_table(validation_results, outcome_type)
    
    # Create decision curve summary table across timepoints
    decision_curve_table <- create_decision_curve_summary_table(validation_results, outcome_type)
    
    # Create performance comparison table across timepoints
    performance_table <- create_performance_summary_table(validation_results, outcome_type)
    
    # Save consolidated tables
    tables <- list()
    
    if (nrow(calibration_table) > 0) {
        cal_path <- file.path(output_dir, paste0(prefix, outcome_type, "_calibration_summary.xlsx"))
        writexl::write_xlsx(list("Calibration_Summary" = calibration_table), cal_path)
        tables$calibration <- cal_path
        logger::log_info(sprintf("Calibration summary saved: %s", cal_path))
    }
    
    if (nrow(decision_curve_table) > 0) {
        dca_path <- file.path(output_dir, paste0(prefix, outcome_type, "_decision_curve_summary.xlsx"))
        writexl::write_xlsx(list("Decision_Curve_Summary" = decision_curve_table), dca_path)
        tables$decision_curve <- dca_path
        logger::log_info(sprintf("Decision curve summary saved: %s", dca_path))
    }
    
    if (nrow(performance_table) > 0) {
        perf_path <- file.path(output_dir, paste0(prefix, outcome_type, "_performance_summary.xlsx"))
        writexl::write_xlsx(list("Performance_Summary" = performance_table), perf_path)
        tables$performance <- perf_path
        logger::log_info(sprintf("Performance summary saved: %s", perf_path))
    }
    
    return(tables)
}

#' Create Calibration Summary Table
create_calibration_summary_table <- function(validation_results, outcome_type) {
    # Initialize with proper column structure
    calibration_data <- data.frame(
        Timepoint = character(),
        Calibration_Slope = numeric(),
        ICI = numeric(),
        Nam_D_Agostino_p = numeric(),
        Clinical_Interpretation = character(),
        stringsAsFactors = FALSE
    )
    
    for (tp in names(validation_results)) {
        result <- validation_results[[tp]]
        if (!is.null(result$calibration)) {
            cal <- result$calibration
            slope_val <- ifelse(is.null(cal$slope), 
                              ifelse(is.null(cal$calibration_slope), NA_real_, cal$calibration_slope), 
                              cal$slope)
            ici_val <- ifelse(is.null(cal$ici), NA_real_, cal$ici)
            nam_dagostino_val <- ifelse(is.null(cal$nam_dagostino_p), NA_real_, cal$nam_dagostino_p)
            
            new_row <- data.frame(
                Timepoint = tp,
                Calibration_Slope = slope_val,
                ICI = ici_val,
                Nam_D_Agostino_p = nam_dagostino_val,
                Clinical_Interpretation = tryCatch({
                    get_calibration_interpretation(slope_val)
                }, error = function(e) "Interpretation not available"),
                stringsAsFactors = FALSE
            )
            calibration_data <- rbind(calibration_data, new_row)
        }
    }
    
    if (nrow(calibration_data) > 0) {
        # Add cross-timepoint analysis with error handling
        calibration_data$Slope_Trend <- tryCatch({
            get_slope_trend(calibration_data$Calibration_Slope)
        }, error = function(e) rep("Trend analysis failed", nrow(calibration_data)))
        
        calibration_data$Overall_Quality <- tryCatch({
            sapply(calibration_data$Calibration_Slope, get_calibration_quality)
        }, error = function(e) rep("Quality assessment failed", nrow(calibration_data)))
    }
    
    return(calibration_data)
}

#' Create Decision Curve Summary Table
create_decision_curve_summary_table <- function(validation_results, outcome_type) {
    # Initialize with proper column structure
    decision_curve_data <- data.frame(
        Timepoint = character(),
        Optimal_Threshold_Percent = numeric(),
        Max_Net_Benefit = numeric(),
        Clinical_Interpretation = character(),
        stringsAsFactors = FALSE
    )
    
    for (tp in names(validation_results)) {
        result <- validation_results[[tp]]
        if (!is.null(result$decision_curve)) {
            dca <- result$decision_curve
            threshold_val <- ifelse(is.null(dca$optimal_threshold), NA_real_, dca$optimal_threshold * 100)
            net_benefit_val <- ifelse(is.null(dca$max_net_benefit), NA_real_, dca$max_net_benefit)
            
            new_row <- data.frame(
                Timepoint = tp,
                Optimal_Threshold_Percent = threshold_val,
                Max_Net_Benefit = net_benefit_val,
                Clinical_Interpretation = tryCatch({
                    get_decision_curve_interpretation(threshold_val, net_benefit_val)
                }, error = function(e) "Interpretation not available"),
                stringsAsFactors = FALSE
            )
            decision_curve_data <- rbind(decision_curve_data, new_row)
        }
    }
    
    if (nrow(decision_curve_data) > 0) {
        # Add cross-timepoint analysis with error handling
        decision_curve_data$Threshold_Trend <- tryCatch({
            get_threshold_trend(decision_curve_data$Optimal_Threshold_Percent)
        }, error = function(e) rep("Trend analysis failed", nrow(decision_curve_data)))
        
        decision_curve_data$Net_Benefit_Trend <- tryCatch({
            get_net_benefit_trend(decision_curve_data$Max_Net_Benefit)
        }, error = function(e) rep("Trend analysis failed", nrow(decision_curve_data)))
    }
    
    return(decision_curve_data)
}

#' Create Performance Summary Table
create_performance_summary_table <- function(validation_results, outcome_type) {
    # Initialize with proper column structure
    performance_data <- data.frame(
        Timepoint = character(),
        Harrell_C = numeric(),
        Integrated_AUC = numeric(),
        Cumulative_Discrimination = numeric(),
        OE_Ratio = numeric(),
        Chi_Square_p = numeric(),
        Discrimination_Quality = character(),
        Calibration_Quality = character(),
        Overall_Assessment = character(),
        stringsAsFactors = FALSE
    )
    
    for (tp in names(validation_results)) {
        result <- validation_results[[tp]]
        
        # Discrimination metrics - with defensive programming
        disc <- result$discrimination
        harrell_c <- if (!is.null(disc) && !is.null(disc$harrell_c)) disc$harrell_c else NA_real_
        integrated_auc <- if (!is.null(disc) && !is.null(disc$integrated_auc)) disc$integrated_auc else NA_real_
        cumulative_disc <- if (!is.null(disc) && !is.null(disc$cumulative_discrimination)) disc$cumulative_discrimination else NA_real_
        
        # Observed/Expected metrics - support MSS nested overall and MFS flat fields
        oe <- result$observed_expected
        if (!is.null(oe) && !is.null(oe$overall)) {
            oe_ratio <- if (!is.null(oe$overall$oe_ratio)) oe$overall$oe_ratio else NA_real_
            chisq_p <- if (!is.null(oe$overall$chi_square_p)) oe$overall$chi_square_p else NA_real_
        } else {
            oe_ratio <- if (!is.null(oe) && !is.null(oe$overall_oe_ratio)) oe$overall_oe_ratio else NA_real_
            chisq_p <- if (!is.null(oe) && !is.null(oe$chisq_p_value)) oe$chisq_p_value else NA_real_
        }
        
        new_row <- data.frame(
            Timepoint = tp,
            Harrell_C = harrell_c,
            Integrated_AUC = integrated_auc,
            Cumulative_Discrimination = cumulative_disc,
            OE_Ratio = oe_ratio,
            Chi_Square_p = chisq_p,
            Discrimination_Quality = tryCatch({
                get_discrimination_quality(harrell_c)
            }, error = function(e) "Quality assessment failed"),
            Calibration_Quality = tryCatch({
                get_oe_calibration_quality(oe_ratio)
            }, error = function(e) "Quality assessment failed"),
            Overall_Assessment = tryCatch({
                get_overall_performance_assessment(harrell_c, oe_ratio)
            }, error = function(e) "Assessment failed"),
            stringsAsFactors = FALSE
        )
        performance_data <- rbind(performance_data, new_row)
    }
    
    if (nrow(performance_data) > 0) {
        # Add cross-timepoint analysis
        performance_data$Discrimination_Trend <- get_discrimination_trend(performance_data$Harrell_C)
        performance_data$Calibration_Trend <- get_calibration_trend(performance_data$OE_Ratio)
    }
    
    return(performance_data)
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
