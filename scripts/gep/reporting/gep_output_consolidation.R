# GEP Output Consolidation
# Consolidates redundant outputs while maintaining essential visualizations

#' Create consolidated GEP tables to replace redundant visualizations
#'
#' Consolidates multiple timepoint outputs into comprehensive summary tables
#' that eliminate redundancy while maintaining all statistical information.
#'
#' @param validation_results Named list of per-timepoint validation results
#' @param outcome_type Character string ("MFS" or "MSS")
#' @param output_dir Directory path to save consolidated outputs
#' @param prefix Filename prefix for saved files
#' @return List of created table names
create_consolidated_gep_tables <- function(validation_results, outcome_type, output_dir, prefix) {
    logger::log_info(formatted(sprintf("Creating consolidated %s tables to replace redundant outputs", outcome_type), indent = 1))
    
    # Create consolidated calibration table across all timepoints
    cal_consolidated <- create_consolidated_calibration_table(validation_results, outcome_type)
    
    # Create consolidated discrimination table across all timepoints
    disc_consolidated <- create_consolidated_discrimination_table(validation_results, outcome_type)
    
    # REMOVED: Redundant performance table that duplicates discrimination metrics
    # perf_consolidated <- create_consolidated_performance_table(validation_results, outcome_type)
    
    # Create consolidated decision curve table across all timepoints
    dca_consolidated <- create_consolidated_decision_curve_table(validation_results, outcome_type)
    
    # Combine all consolidated tables into a single Excel workbook
    consolidated_workbook <- list()
    if (nrow(cal_consolidated) > 0) consolidated_workbook[["Calibration_Summary"]] <- cal_consolidated
    if (nrow(disc_consolidated) > 0) consolidated_workbook[["Discrimination_Summary"]] <- disc_consolidated
    # REMOVED: Redundant performance summary
    # if (nrow(perf_consolidated) > 0) consolidated_workbook[["Performance_Summary"]] <- perf_consolidated
    if (nrow(dca_consolidated) > 0) consolidated_workbook[["Decision_Curve_Summary"]] <- dca_consolidated
    
    # Save consolidated workbook
    if (length(consolidated_workbook) > 0) {
        consolidated_path <- file.path(output_dir, paste0(prefix, outcome_type, "_consolidated_summary.xlsx"))
        writexl::write_xlsx(consolidated_workbook, consolidated_path)
        logger::log_info(formatted(sprintf("Consolidated %s tables saved: %s", outcome_type, consolidated_path), indent = 2))
    }
    
    # Create comprehensive text summary (but don't save to file to avoid redundancy)
    text_summary <- create_comprehensive_text_summary(
        validation_results, outcome_type, 
        cal_consolidated, disc_consolidated, 
        NULL, dca_consolidated  # Pass NULL for performance table since it's removed
    )
    
    # REMOVED: Text file generation to eliminate redundancy
    # text_path <- file.path(output_dir, paste0(prefix, outcome_type, "_consolidated_summary.txt"))
    # writeLines(text_summary, text_path)
    # logger::log_info(formatted(sprintf("Consolidated %s text summary saved: %s", outcome_type, text_path), indent = 2))
    
    return(list(
        calibration = cal_consolidated,
        discrimination = disc_consolidated,
        performance = NULL,  # Set to NULL since performance table is removed
        decision_curves = dca_consolidated,
        text_summary = text_summary
    ))
}

#' Create consolidated calibration table across all timepoints
create_consolidated_calibration_table <- function(validation_results, outcome_type) {
    cal_data <- data.frame()
    
    for (tp_name in names(validation_results)) {
        tp_results <- validation_results[[tp_name]]
        if (!is.null(tp_results$calibration)) {
            cal <- tp_results$calibration
            cal_data <- rbind(cal_data, data.frame(
                Timepoint = tp_name,
                N = cal$n %||% NA,
                Nam_D_Agostino_p = cal$nam_dagostino_p %||% NA,
                ICI = cal$ici %||% NA,
                Slope = cal$slope %||% cal$calibration_slope %||% NA,
                stringsAsFactors = FALSE
            ))
        }
    }
    
    return(cal_data)
}

#' Create consolidated discrimination table across all timepoints
create_consolidated_discrimination_table <- function(validation_results, outcome_type) {
    disc_data <- data.frame()
    
    for (tp_name in names(validation_results)) {
        tp_results <- validation_results[[tp_name]]
        if (!is.null(tp_results$discrimination)) {
            disc <- tp_results$discrimination
            disc_data <- rbind(disc_data, data.frame(
                Timepoint = tp_name,
                N = disc$n %||% NA,
                Events = disc$events %||% NA,
                Harrell_C = disc$harrell_c %||% NA,
                Uno_C = disc$uno_c %||% NA,
                AUC = disc$auc_timepoint %||% NA,
                stringsAsFactors = FALSE
            ))
        }
    }
    
    return(disc_data)
}

#' Create consolidated performance table across all timepoints
create_consolidated_performance_table <- function(validation_results, outcome_type) {
    perf_data <- data.frame()
    
    for (tp_name in names(validation_results)) {
        tp_results <- validation_results[[tp_name]]
        if (!is.null(tp_results$discrimination)) {
            disc <- tp_results$discrimination
            # Extract performance metrics if available
            perf_data <- rbind(perf_data, data.frame(
                Timepoint = tp_name,
                N = disc$n %||% NA,
                Events = disc$events %||% NA,
                C_Index = disc$harrell_c %||% NA,
                AUC = disc$auc_timepoint %||% NA,
                stringsAsFactors = FALSE
            ))
        }
    }
    
    return(perf_data)
}

#' Create consolidated decision curve table across all timepoints
create_consolidated_decision_curve_table <- function(validation_results, outcome_type) {
    dca_data <- data.frame()
    
    for (tp_name in names(validation_results)) {
        tp_results <- validation_results[[tp_name]]
        if (!is.null(tp_results$decision_curve)) {
            dca <- tp_results$decision_curve
            # Extract key decision curve metrics if available
            dca_data <- rbind(dca_data, data.frame(
                Timepoint = tp_name,
                N = dca$n %||% NA,
                Net_Benefit_Threshold = dca$net_benefit_threshold %||% dca$optimal_threshold %||% NA,
                stringsAsFactors = FALSE
            ))
        }
    }
    
    return(dca_data)
}

#' Create comprehensive text summary from consolidated tables
create_comprehensive_text_summary <- function(validation_results, outcome_type, 
                                            cal_consolidated, disc_consolidated, 
                                            perf_consolidated, dca_consolidated) {
    
    summary_lines <- c()
    summary_lines <- c(summary_lines, paste("=", outcome_type, "Validation - Consolidated Summary", "="))
    summary_lines <- c(summary_lines, "")
    
    # Calibration summary
    if (nrow(cal_consolidated) > 0) {
        summary_lines <- c(summary_lines, "CALIBRATION SUMMARY:")
        summary_lines <- c(summary_lines, "")
        summary_lines <- c(summary_lines, sprintf("%-10s %-8s %-20s %-12s %-10s", "Timepoint", "N", "Nam-D'Agostino p", "ICI", "Slope"))
        summary_lines <- c(summary_lines, paste(rep("-", 70), collapse = ""))
        for (i in seq_len(nrow(cal_consolidated))) {
            row <- cal_consolidated[i, ]
            summary_lines <- c(summary_lines, 
                sprintf("%-10s %-8s %-20s %-12s %-10s",
                    row$Timepoint,
                    ifelse(is.na(row$N), "NA", as.character(row$N)),
                    ifelse(is.na(row$Nam_D_Agostino_p), "NA", sprintf("%.3f", row$Nam_D_Agostino_p)),
                    ifelse(is.na(row$ICI), "NA", sprintf("%.3f", row$ICI)),
                    ifelse(is.na(row$Slope), "NA", sprintf("%.3f", row$Slope))
                )
            )
        }
        summary_lines <- c(summary_lines, "")
    }
    
    # Discrimination summary
    if (nrow(disc_consolidated) > 0) {
        summary_lines <- c(summary_lines, "DISCRIMINATION SUMMARY:")
        summary_lines <- c(summary_lines, "")
        summary_lines <- c(summary_lines, sprintf("%-10s %-8s %-10s %-15s %-12s %-10s", "Timepoint", "N", "Events", "Harrell's C", "Uno's C", "AUC"))
        summary_lines <- c(summary_lines, paste(rep("-", 75), collapse = ""))
        for (i in seq_len(nrow(disc_consolidated))) {
            row <- disc_consolidated[i, ]
            summary_lines <- c(summary_lines, 
                sprintf("%-10s %-8s %-10s %-15s %-12s %-10s",
                    row$Timepoint,
                    ifelse(is.na(row$N), "NA", as.character(row$N)),
                    ifelse(is.na(row$Events), "NA", as.character(row$Events)),
                    ifelse(is.na(row$Harrell_C), "NA", sprintf("%.3f", row$Harrell_C)),
                    ifelse(is.na(row$Uno_C), "NA", sprintf("%.3f", row$Uno_C)),
                    ifelse(is.na(row$AUC), "NA", sprintf("%.3f", row$AUC))
                )
            )
        }
        summary_lines <- c(summary_lines, "")
    }
    
    # REMOVED: Performance summary section that duplicated discrimination metrics
    # The Performance Summary was redundant because C-Index = Harrell's C (same metric, different name)
    # This eliminates user confusion and data redundancy
    
    # Decision curve summary
    if (nrow(dca_consolidated) > 0) {
        summary_lines <- c(summary_lines, "DECISION CURVE SUMMARY:")
        summary_lines <- c(summary_lines, "")
        summary_lines <- c(summary_lines, sprintf("%-10s %-8s %-20s", "Timepoint", "N", "Net Benefit Threshold"))
        summary_lines <- c(summary_lines, paste(rep("-", 40), collapse = ""))
        for (i in seq_len(nrow(dca_consolidated))) {
            row <- dca_consolidated[i, ]
            summary_lines <- c(summary_lines, 
                sprintf("%-10s %-8s %-20s",
                    row$Timepoint,
                    ifelse(is.na(row$N), "NA", as.character(row$N)),
                    ifelse(is.na(row$Net_Benefit_Threshold), "NA", sprintf("%.3f", row$Net_Benefit_Threshold))
                )
            )
        }
        summary_lines <- c(summary_lines, "")
    }
    
    # Key findings summary
    summary_lines <- c(summary_lines, "KEY FINDINGS:")
    if (nrow(cal_consolidated) > 0) {
        # Find best calibration timepoint
        best_cal_idx <- which.max(cal_consolidated$Slope %||% 0)
        if (best_cal_idx > 0) {
            best_tp <- cal_consolidated$Timepoint[best_cal_idx]
            best_slope <- cal_consolidated$Slope[best_cal_idx]
            summary_lines <- c(summary_lines, 
                sprintf("- Best calibration at %s (slope: %.3f)", best_tp, best_slope))
        }
    }
    
    if (nrow(disc_consolidated) > 0) {
        # Find best discrimination timepoint
        best_disc_idx <- which.max(disc_consolidated$Harrell_C %||% 0)
        if (best_disc_idx > 0) {
            best_tp <- disc_consolidated$Timepoint[best_disc_idx]
            best_c <- disc_consolidated$Harrell_C[best_disc_idx]
            summary_lines <- c(summary_lines, 
                sprintf("- Best discrimination at %s (Harrell's C: %.3f)", best_tp, best_c))
        }
    }
    
    summary_lines <- c(summary_lines, "")
    summary_lines <- c(summary_lines, "Note: This consolidated summary replaces multiple redundant plots")
    summary_lines <- c(summary_lines, "while maintaining all statistical information.")
    summary_lines <- c(summary_lines, "Performance Summary removed to eliminate redundancy with Discrimination Summary.")
    
    return(paste(summary_lines, collapse = "\n"))
}

#' Create unified GEP validation summary across outcomes
#'
#' Creates a single comprehensive summary that combines MFS and MSS results
#' to eliminate redundancy while maintaining all information.
#'
#' @param mfs_results MFS validation results
#' @param mss_results MSS validation results
#' @param output_dir Output directory
#' @param prefix Filename prefix
#' @return List of created summary files
create_unified_gep_validation_summary <- function(mfs_results, mss_results, output_dir, prefix) {
    logger::log_info(formatted("Creating unified GEP validation summary to eliminate redundancy", indent = 1))
    
    # Create unified calibration comparison
    unified_cal <- create_unified_calibration_summary(mfs_results, mss_results)
    
    # Create unified discrimination comparison
    unified_disc <- create_unified_discrimination_summary(mfs_results, mss_results)
    
    # REMOVED: Unified performance comparison to eliminate redundancy with discrimination metrics
    # unified_perf <- create_unified_performance_summary(mfs_results, mss_results)
    
    # Combine into single workbook
    unified_workbook <- list()
    if (nrow(unified_cal) > 0) unified_workbook[["Unified_Calibration"]] <- unified_cal
    if (nrow(unified_disc) > 0) unified_workbook[["Unified_Discrimination"]] <- unified_disc
    # REMOVED: Performance summary to eliminate redundancy
    # if (nrow(unified_perf) > 0) unified_workbook[["Unified_Performance"]] <- unified_perf
    
    # Save unified workbook
    if (length(unified_workbook) > 0) {
        unified_path <- file.path(output_dir, paste0(prefix, "unified_gep_validation_summary.xlsx"))
        writexl::write_xlsx(unified_workbook, unified_path)
        logger::log_info(formatted(sprintf("Unified GEP validation summary saved: %s", unified_path), indent = 2))
    }
    
    # Create unified text summary (but don't save to file to avoid redundancy)
    unified_text <- create_unified_text_summary(mfs_results, mss_results, unified_cal, unified_disc, NULL)  # Pass NULL for performance table
    # REMOVED: Text file generation to eliminate redundancy
    # text_path <- file.path(output_dir, paste0(prefix, "unified_gep_validation_summary.txt"))
    # writeLines(unified_text, text_path)
    
    return(list(
        calibration = unified_cal,
        discrimination = unified_disc,
        performance = NULL,  # Set to NULL since performance table is removed
        text_summary = unified_text
    ))
}

#' Create unified calibration summary across outcomes
create_unified_calibration_summary <- function(mfs_results, mss_results) {
    unified_cal <- data.frame()
    
    # Add MFS calibration data
    if (!is.null(mfs_results$validation_results)) {
        for (tp_name in names(mfs_results$validation_results)) {
            tp_results <- mfs_results$validation_results[[tp_name]]
            if (!is.null(tp_results$calibration)) {
                cal <- tp_results$calibration
                unified_cal <- rbind(unified_cal, data.frame(
                    Outcome = "MFS",
                    Timepoint = tp_name,
                    N = cal$n %||% NA,
                    Nam_D_Agostino_p = cal$nam_dagostino_p %||% NA,
                    ICI = cal$ici %||% NA,
                    Slope = cal$slope %||% cal$calibration_slope %||% NA,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
    
    # Add MSS calibration data
    if (!is.null(mss_results$standard_validation)) {
        for (tp_name in names(mss_results$standard_validation)) {
            tp_results <- mss_results$standard_validation[[tp_name]]
            if (!is.null(tp_results$calibration)) {
                cal <- tp_results$calibration
                unified_cal <- rbind(unified_cal, data.frame(
                    Outcome = "MSS",
                    Timepoint = tp_name,
                    N = cal$n %||% NA,
                    Nam_D_Agostino_p = cal$nam_dagostino_p %||% NA,
                    ICI = cal$ici %||% NA,
                    Slope = cal$slope %||% cal$calibration_slope %||% NA,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
    
    return(unified_cal)
}

#' Create unified discrimination summary across outcomes
create_unified_discrimination_summary <- function(mfs_results, mss_results) {
    unified_disc <- data.frame()
    
    # Add MFS discrimination data
    if (!is.null(mfs_results$validation_results)) {
        for (tp_name in names(mfs_results$validation_results)) {
            tp_results <- mfs_results$validation_results[[tp_name]]
            if (!is.null(tp_results$discrimination)) {
                disc <- tp_results$discrimination
                unified_disc <- rbind(unified_disc, data.frame(
                    Outcome = "MFS",
                    Timepoint = tp_name,
                    N = disc$n %||% NA,
                    Events = disc$events %||% NA,
                    Harrell_C = disc$harrell_c %||% NA,
                    Uno_C = disc$uno_c %||% NA,
                    AUC = disc$auc_timepoint %||% NA,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
    
    # Add MSS discrimination data
    if (!is.null(mss_results$standard_validation)) {
        for (tp_name in names(mss_results$standard_validation)) {
            tp_results <- mss_results$standard_validation[[tp_name]]
            if (!is.null(tp_results$discrimination)) {
                disc <- tp_results$discrimination
                unified_disc <- rbind(unified_disc, data.frame(
                    Outcome = "MSS",
                    Timepoint = tp_name,
                    N = disc$n %||% NA,
                    Events = disc$events %||% NA,
                    Harrell_C = disc$harrell_c %||% NA,
                    Uno_C = disc$uno_c %||% NA,
                    AUC = disc$auc_timepoint %||% NA,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
    
    return(unified_disc)
}

#' Create unified performance summary across outcomes
create_unified_performance_summary <- function(mfs_results, mss_results) {
    unified_perf <- data.frame()
    
    # Add MFS performance data
    if (!is.null(mfs_results$validation_results)) {
        for (tp_name in names(mfs_results$validation_results)) {
            tp_results <- mfs_results$validation_results[[tp_name]]
            if (!is.null(tp_results$discrimination)) {
                disc <- tp_results$discrimination
                unified_perf <- rbind(unified_perf, data.frame(
                    Outcome = "MFS",
                    Timepoint = tp_name,
                    N = disc$n %||% NA,
                    Events = disc$events %||% NA,
                    C_Index = disc$harrell_c %||% NA,
                    AUC = disc$auc_timepoint %||% NA,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
    
    # Add MSS performance data
    if (!is.null(mss_results$standard_validation)) {
        for (tp_name in names(mss_results$standard_validation)) {
            tp_results <- mss_results$standard_validation[[tp_name]]
            if (!is.null(tp_results$discrimination)) {
                disc <- tp_results$discrimination
                unified_perf <- rbind(unified_perf, data.frame(
                    Outcome = "MSS",
                    Timepoint = tp_name,
                    N = disc$n %||% NA,
                    Events = disc$events %||% NA,
                    C_Index = disc$harrell_c %||% NA,
                    AUC = disc$auc_timepoint %||% NA,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
    
    return(unified_perf)
}

#' Create unified text summary
create_unified_text_summary <- function(mfs_results, mss_results, unified_cal, unified_disc, unified_perf) {
    summary_lines <- c()
    summary_lines <- c(summary_lines, "=", "Unified GEP Validation Summary", "=")
    summary_lines <- c(summary_lines, "Combines MFS and MSS results to eliminate redundancy")
    summary_lines <- c(summary_lines, "")
    
    # Calibration comparison
    if (nrow(unified_cal) > 0) {
        summary_lines <- c(summary_lines, "CALIBRATION COMPARISON (MFS vs MSS):")
        summary_lines <- c(summary_lines, "Outcome | Timepoint | N | Nam-D'Agostino p | ICI | Slope")
        summary_lines <- c(summary_lines, "---------|-----------|----|------------------|-----|------")
        for (i in seq_len(nrow(unified_cal))) {
            row <- unified_cal[i, ]
            summary_lines <- c(summary_lines, 
                sprintf("%s | %s | %s | %s | %s | %s",
                    row$Outcome,
                    row$Timepoint,
                    ifelse(is.na(row$N), "NA", as.character(row$N)),
                    ifelse(is.na(row$Nam_D_Agostino_p), "NA", sprintf("%.3f", row$Nam_D_Agostino_p)),
                    ifelse(is.na(row$ICI), "NA", sprintf("%.3f", row$ICI)),
                    ifelse(is.na(row$Slope), "NA", sprintf("%.3f", row$Slope))
                )
            )
        }
        summary_lines <- c(summary_lines, "")
    }
    
    # Discrimination comparison
    if (nrow(unified_disc) > 0) {
        summary_lines <- c(summary_lines, "DISCRIMINATION COMPARISON (MFS vs MSS):")
        summary_lines <- c(summary_lines, "Outcome | Timepoint | N | Events | Harrell's C | Uno's C | AUC")
        summary_lines <- c(summary_lines, "---------|-----------|----|--------|-------------|---------|-----")
        for (i in seq_len(nrow(unified_disc))) {
            row <- unified_disc[i, ]
            summary_lines <- c(summary_lines, 
                sprintf("%s | %s | %s | %s | %s | %s | %s",
                    row$Outcome,
                    row$Timepoint,
                    ifelse(is.na(row$N), "NA", as.character(row$N)),
                    ifelse(is.na(row$Events), "NA", as.character(row$Events)),
                    ifelse(is.na(row$Harrell_C), "NA", sprintf("%.3f", row$Harrell_C)),
                    ifelse(is.na(row$Uno_C), "NA", sprintf("%.3f", row$Uno_C)),
                    ifelse(is.na(row$AUC), "NA", sprintf("%.3f", row$AUC))
                )
            )
        }
        summary_lines <- c(summary_lines, "")
    }
    
    # REMOVED: Performance comparison to eliminate redundancy with discrimination metrics
    # Performance comparison was redundant because C-Index = Harrell's C (same metric, different name)
    
    # Key findings
    summary_lines <- c(summary_lines, "KEY FINDINGS:")
    if (nrow(unified_cal) > 0) {
        # Find best calibration by outcome
        mfs_cal <- unified_cal[unified_cal$Outcome == "MFS", ]
        mss_cal <- unified_cal[unified_cal$Outcome == "MSS", ]
        
        if (nrow(mfs_cal) > 0) {
            best_mfs_idx <- which.max(mfs_cal$Slope %||% 0)
            if (best_mfs_idx > 0) {
                best_tp <- mfs_cal$Timepoint[best_mfs_idx]
                best_slope <- mfs_cal$Slope[best_mfs_idx]
                summary_lines <- c(summary_lines, 
                    sprintf("- MFS: Best calibration at %s (slope: %.3f)", best_tp, best_slope))
            }
        }
        
        if (nrow(mss_cal) > 0) {
            best_mss_idx <- which.max(mss_cal$Slope %||% 0)
            if (best_mss_idx > 0) {
                best_tp <- mss_cal$Timepoint[best_mss_idx]
                best_slope <- mss_cal$Slope[best_mss_idx]
                summary_lines <- c(summary_lines, 
                    sprintf("- MSS: Best calibration at %s (slope: %.3f)", best_tp, best_slope))
            }
        }
    }
    
    if (nrow(unified_disc) > 0) {
        # Find best discrimination by outcome
        mfs_disc <- unified_disc[unified_disc$Outcome == "MFS", ]
        mss_disc <- unified_disc[unified_disc$Outcome == "MSS", ]
        
        if (nrow(mfs_disc) > 0) {
            best_mfs_idx <- which.max(mfs_disc$Harrell_C %||% 0)
            if (best_mfs_idx > 0) {
                best_tp <- mfs_disc$Timepoint[best_mfs_idx]
                best_c <- mfs_disc$Harrell_C[best_mfs_idx]
                summary_lines <- c(summary_lines, 
                    sprintf("- MFS: Best discrimination at %s (Harrell's C: %.3f)", best_tp, best_c))
            }
        }
        
        if (nrow(mss_disc) > 0) {
            best_mss_idx <- which.max(mss_disc$Harrell_C %||% 0)
            if (best_mss_idx > 0) {
                best_tp <- mss_disc$Timepoint[best_mss_idx]
                best_c <- mss_disc$Harrell_C[best_mss_idx]
                summary_lines <- c(summary_lines, 
                    sprintf("- MSS: Best discrimination at %s (Harrell's C: %.3f)", best_tp, best_c))
            }
        }
    }
    
    summary_lines <- c(summary_lines, "")
    summary_lines <- c(summary_lines, "Note: This unified summary eliminates redundant outputs")
    summary_lines <- c(summary_lines, "while maintaining all statistical information across outcomes.")
    
    return(paste(summary_lines, collapse = "\n"))
}
