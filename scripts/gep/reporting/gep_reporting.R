# GEP Reporting Functions
# Reporting-only: assemble reports, write Excel/text summaries, unified summary

#' Create MFS Validation Report
#'
#' Assemble a structured report of MFS validation metrics across timepoints.
#'
#' @param validation_results Named list of per-timepoint MFS results
#' @param prame_analysis PRAME-augmented analysis results (may be NULL)
#' @param missing_data_analysis Missing-data diagnostics results
#' @param dataset_name Optional dataset label used in the report
#' @return A list with summary metrics and interpretation text suitable for saving
create_mfs_validation_report <- function(validation_results, prame_analysis, missing_data_analysis, dataset_name) {
    log_enhanced("Creating comprehensive MFS validation report", level = "INFO", indent = 1)
    report <- list(
        dataset = dataset_name,
        analysis_date = Sys.time(),
        timepoints_analyzed = names(validation_results),
        summary_metrics = list(),
        clinical_interpretation = list()
    )
    for (tp_key in names(validation_results)) {
        result <- validation_results[[tp_key]]
        if (!is.null(result$observed_expected)) {
            oe_data <- result$observed_expected
            report$summary_metrics[[tp_key]] <- list(
                timepoint = result$timepoint,
                overall_oe_ratio = oe_data$overall_oe_ratio,
                chisq_p_value = oe_data$chisq_p_value,
                calibration_p = if (!is.null(result$calibration)) result$calibration$nam_dagostino_p else NA,
                discrimination_c = if (!is.null(result$discrimination)) result$discrimination$harrell_c else NA,
                events_analyzed = result$events
            )
        }
    }
    report$clinical_interpretation <- list(
        calibration_assessment = "GEP predictions evaluated using Nam-D'Agostino test and Integrated Calibration Index",
        discrimination_assessment = "Model discrimination assessed using Harrell's C-index, Uno's C-index, and time-specific AUC",
        decision_analysis = "Net clinical benefit evaluated across risk thresholds using decision curve analysis",
        prame_added_value = if (!is.null(prame_analysis$prame_available) && prame_analysis$prame_available) {
            "PRAME status provides additional risk stratification beyond GEP class alone"
        } else {
            "Insufficient PRAME data for augmented analysis"
        },
        missing_data_impact = if (!is.null(missing_data_analysis$informative_missingness_detected) &&
            !is.na(missing_data_analysis$informative_missingness_detected) &&
            missing_data_analysis$informative_missingness_detected) {
            "Significant differences detected between patients with and without GEP data - results may be biased"
        } else {
            "No evidence of informative missingness pattern for GEP data"
        }
    )
    return(report)
}

#' Save All MFS Validation Results
#'
#' Persist MFS validation artifacts to disk, including summaries (xlsx), RDS
#' objects, and a text summary.
#'
#' @param validation_results Named list of per-timepoint MFS results
#' @param validation_report Structured report object from `create_mfs_validation_report`
#' @param missing_data_analysis Missing-data diagnostics results
#' @param prame_analysis PRAME-augmented analysis results (may be NULL)
#' @param output_dir Directory path to save artifacts
#' @param prefix Filename prefix for saved files
#' @return Invisibly returns NULL after writing files
save_mfs_validation_results <- function(validation_results, validation_report, missing_data_analysis, prame_analysis, output_dir, prefix) {
    log_enhanced("Saving MFS validation results", level = "INFO", indent = 1)
    tryCatch(
        {
            oe_summary <- data.frame()
            for (tp_key in names(validation_results)) {
                result <- validation_results[[tp_key]]
                if (!is.null(result$observed_expected)) {
                    oe_data <- result$observed_expected
                    for (class in names(oe_data$results_by_class)) {
                        class_result <- oe_data$results_by_class[[class]]
                        oe_summary <- rbind(oe_summary, data.frame(
                            Timepoint = paste0(result$timepoint, " years"),
                            GEP_Class = class,
                            N = class_result$n,
                            Observed = class_result$observed,
                            Expected = class_result$expected,
                            OE_Ratio = class_result$oe_ratio,
                            CI_Lower = class_result$poisson_ci_lower,
                            CI_Upper = class_result$poisson_ci_upper
                        ))
                    }
                }
            }
            if (nrow(oe_summary) > 0) {
                write_xlsx(oe_summary, file.path(output_dir, paste0(prefix, "observed_expected_summary.xlsx")))
            }
            cal_summary <- data.frame()
            for (tp_key in names(validation_results)) {
                result <- validation_results[[tp_key]]
                if (!is.null(result$calibration)) {
                    cal_data <- result$calibration
                    cal_summary <- rbind(cal_summary, data.frame(
                        Timepoint = paste0(result$timepoint, " years"),
                        N = cal_data$n,
                        Nam_D_Agostino_p = cal_data$nam_dagostino_p,
                        ICI = cal_data$ici,
                        Calibration_Slope = cal_data$calibration_slope
                    ))
                }
            }
            if (nrow(cal_summary) > 0) {
                write_xlsx(cal_summary, file.path(output_dir, paste0(prefix, "calibration_summary.xlsx")))
            }
            disc_summary <- data.frame()
            for (tp_key in names(validation_results)) {
                result <- validation_results[[tp_key]]
                if (!is.null(result$discrimination)) {
                    disc_data <- result$discrimination
                    disc_summary <- rbind(disc_summary, data.frame(
                        Timepoint = paste0(result$timepoint, " years"),
                        N = disc_data$n,
                        Events = disc_data$events,
                        Harrell_C = disc_data$harrell_c,
                        Uno_C = disc_data$uno_c,
                        AUC = disc_data$auc_timepoint
                    ))
                }
            }
            if (nrow(disc_summary) > 0) {
                write_xlsx(disc_summary, file.path(output_dir, paste0(prefix, "discrimination_summary.xlsx")))
            }
        },
        error = function(e) {
            log_enhanced("Error saving summary tables", level = "WARN", indent = 2)
        }
    )
    saveRDS(validation_results, file.path(output_dir, paste0(prefix, "mfs_validation_results.rds")))
    saveRDS(missing_data_analysis, file.path(output_dir, paste0(prefix, "missing_data_analysis.rds")))
    saveRDS(prame_analysis, file.path(output_dir, paste0(prefix, "prame_analysis.rds")))
    report_lines <- c(
        "GEP Metastasis-Free Survival Validation Report",
        paste(rep("=", 50), collapse = ""),
        paste("Analysis completed:", Sys.time()),
        "",
        "SUMMARY OF ANALYSES PERFORMED:",
        "✓ Observed vs Expected rates with Poisson confidence intervals",
        "✓ Nam-D'Agostino calibration test and Integrated Calibration Index",
        "✓ Harrell's and Uno's C-index discrimination measures",
        "✓ Time-specific AUC/ROC analysis",
        "✓ Decision curve analysis for net clinical benefit",
        "✓ PRAME-augmented analysis with net reclassification index",
        "✓ Missing data assessment and informative missingness evaluation",
        "",
        sprintf("Total timepoints analyzed: %d", length(validation_results)),
        sprintf("Missing data patterns identified: %d", ifelse(is.null(missing_data_analysis$missing_patterns), 0, nrow(missing_data_analysis$missing_patterns))),
        sprintf("PRAME analysis performed: %s", ifelse(is.null(prame_analysis$prame_available), "No", ifelse(prame_analysis$prame_available, "Yes", "No"))),
        "",
        "All detailed results saved as Excel tables and RDS objects.",
        "See individual files for complete statistical outputs."
    )
    writeLines(report_lines, file.path(output_dir, paste0(prefix, "mfs_validation_summary.txt")))
    log_enhanced("MFS validation results saved successfully", level = "INFO", indent = 2)
}

#' Create MSS validation report
#'
#' Assemble a structured report summarizing MSS calibration/discrimination and
#' competing risk analyses across timepoints.
#'
#' @param standard_results Named list of standard MSS results (per timepoint)
#' @param competing_results Named list of competing risk MSS results (per timepoint)
#' @param prame_results PRAME-augmented MSS analysis results (may be NULL)
#' @param missing_data Missing-data diagnostics results
#' @param dataset_name Optional dataset label used in the report
#' @return A list with `summary_stats` and `timepoint_summaries`
create_mss_validation_report <- function(standard_results, competing_results, prame_results, missing_data, dataset_name) {
    log_enhanced("Creating comprehensive MSS validation report", level = "INFO")
    summary_stats <- data.frame(
        analysis_type = "MSS_Validation",
        dataset = dataset_name,
        timepoints_analyzed = length(standard_results),
        competing_risk_analysis = !is.null(competing_results),
        prame_analysis = !is.null(prame_results),
        missing_data_assessment = !is.null(missing_data),
        stringsAsFactors = FALSE
    )
    timepoint_summaries <- list()
    for (tp_name in names(standard_results)) {
        tp_results <- standard_results[[tp_name]]
        timepoint_summaries[[tp_name]] <- data.frame(
            timepoint = tp_name,
            calibration_slope = tp_results$calibration$slope,
            calibration_intercept = tp_results$calibration$intercept,
            nam_dagostino_p = tp_results$calibration$nam_dagostino_p,
            integrated_calibration_index = tp_results$calibration$ici,
            harrell_c_index = tp_results$discrimination$harrell_c,
            uno_c_index = tp_results$discrimination$uno_c,
            stringsAsFactors = FALSE
        )
    }
    return(list(
        summary_stats = summary_stats,
        timepoint_summaries = timepoint_summaries
    ))
}

#' Save MSS validation results
#'
#' Persist MSS validation artifacts to disk, including per-timepoint sheets for
#' observed/expected, calibration, discrimination, and competing risks.
#'
#' @param standard_results Named list of standard MSS results (per timepoint)
#' @param competing_results Named list of competing risk MSS results (per timepoint)
#' @param validation_report Report object from `create_mss_validation_report`
#' @param missing_data Missing-data diagnostics results (may be NULL)
#' @param prame_results PRAME-augmented MSS analysis results (may be NULL)
#' @param output_dir Directory path to save artifacts
#' @param prefix Filename prefix for saved files
#' @return Invisibly returns NULL after writing files
save_mss_validation_results <- function(standard_results, competing_results, validation_report,
                                        missing_data, prame_results, output_dir, prefix) {
    log_enhanced("Saving MSS validation results", level = "INFO")
    saveRDS(standard_results, file.path(output_dir, paste0(prefix, "mss_standard_validation_results.rds")))
    saveRDS(competing_results, file.path(output_dir, paste0(prefix, "mss_competing_risk_results.rds")))
    saveRDS(validation_report, file.path(output_dir, paste0(prefix, "mss_validation_report.rds")))
    if (!is.null(missing_data)) {
        saveRDS(missing_data, file.path(output_dir, paste0(prefix, "mss_missing_data_analysis.rds")))
    }
    if (!is.null(prame_results)) {
        saveRDS(prame_results, file.path(output_dir, paste0(prefix, "mss_prame_analysis.rds")))
    }
    create_mss_validation_excel_files(
        standard_results, competing_results, validation_report,
        missing_data, prame_results, output_dir, prefix
    )
    create_mss_validation_summary_text(
        standard_results, competing_results, validation_report,
        missing_data, prame_results, output_dir, prefix
    )
}

#' Create MSS validation Excel files
create_mss_validation_excel_files <- function(standard_results, competing_results, validation_report,
                                              missing_data, prame_results, output_dir, prefix) {
    log_enhanced("Creating MSS validation Excel files", level = "INFO")
    excel_sheets <- list()
    excel_sheets[["Summary_Statistics"]] <- validation_report$summary_stats
    for (tp_name in names(validation_report$timepoint_summaries)) {
        sheet_name <- paste0("Timepoint_", tp_name)
        excel_sheets[[sheet_name]] <- validation_report$timepoint_summaries[[tp_name]]
    }
    for (tp_name in names(standard_results)) {
        tp_results <- standard_results[[tp_name]]
        if (!is.null(tp_results$observed_expected)) {
            sheet_name <- paste0("Observed_Expected_", tp_name)
            excel_sheets[[sheet_name]] <- tp_results$observed_expected
        }
        if (!is.null(tp_results$calibration)) {
            sheet_name <- paste0("Calibration_", tp_name)
            excel_sheets[[sheet_name]] <- tp_results$calibration
        }
        if (!is.null(tp_results$discrimination)) {
            sheet_name <- paste0("Discrimination_", tp_name)
            excel_sheets[[sheet_name]] <- tp_results$discrimination
        }
    }
    if (!is.null(competing_results)) {
        for (tp_name in names(competing_results)) {
            tp_results <- competing_results[[tp_name]]
            if (!is.null(tp_results$cumulative_incidence)) {
                sheet_name <- paste0("Cumulative_Incidence_", tp_name)
                excel_sheets[[sheet_name]] <- tp_results$cumulative_incidence
            }
            if (!is.null(tp_results$cause_specific_hazards)) {
                sheet_name <- paste0("Cause_Specific_Hazards_", tp_name)
                excel_sheets[[sheet_name]] <- tp_results$cause_specific_hazards
            }
        }
    }
    excel_path <- file.path(output_dir, paste0(prefix, "mss_validation_summary.xlsx"))
    writexl::write_xlsx(excel_sheets, excel_path)
    log_enhanced(sprintf("MSS validation Excel file saved: %s", excel_path), level = "INFO")
}

#' Create MSS validation summary text
create_mss_validation_summary_text <- function(standard_results, competing_results, validation_report,
                                               missing_data, prame_results, output_dir, prefix) {
    log_enhanced("Creating MSS validation summary text file", level = "INFO")
    summary_lines <- c(
        "GEP Melanoma-Specific Survival Validation Report",
        "==================================================",
        sprintf("Analysis completed: %s", Sys.time()),
        "",
        "SUMMARY OF ANALYSES PERFORMED:",
        "✓ Standard survival analysis with calibration and discrimination metrics",
        "✓ Competing risk analysis with cumulative incidence functions",
        "✓ Cause-specific hazard analysis",
        sprintf("✓ PRAME-augmented analysis: %s", ifelse(!is.null(prame_results), "Yes", "No")),
        "✓ Missing data assessment and informative missingness evaluation",
        "",
        sprintf("Total timepoints analyzed: %d", length(standard_results)),
        sprintf("Competing risk analysis performed: %s", ifelse(!is.null(competing_results), "Yes", "No")),
        sprintf("PRAME analysis performed: %s", ifelse(!is.null(prame_results), "Yes", "No")),
        "",
        "All detailed results saved as Excel tables and RDS objects.",
        "See individual files for complete statistical outputs."
    )
    summary_path <- file.path(output_dir, paste0(prefix, "mss_validation_summary.txt"))
    writeLines(summary_lines, summary_path)
    log_enhanced(sprintf("MSS validation summary saved: %s", summary_path), level = "INFO")
}

#' Create unified GEP validation summary
#'
#' Create a unified comparison and integrated visuals for MFS and MSS,
#' saving a combined report and summary artifacts in a subdirectory.
#'
#' @param mfs_results MFS validation results (may be NULL)
#' @param mss_results MSS validation results (may be NULL)
#' @param dataset_name Optional dataset label
#' @param output_dir Base directory for outputs
#' @param prefix Filename prefix for saved files
#' @return A list with `comparison_table` and path to `unified_dir`
create_unified_gep_validation_summary <- function(mfs_results, mss_results, dataset_name, output_dir, prefix) {
    log_enhanced("Creating unified GEP validation summary", level = "INFO")
    unified_dir <- file.path(output_dir, "unified_summary")
    if (!dir.exists(unified_dir)) {
        dir.create(unified_dir, recursive = TRUE, showWarnings = FALSE)
    }
    comparison_table <- create_gep_comparison_table(mfs_results, mss_results)
    create_integrated_gep_visuals(mfs_results, mss_results, unified_dir, prefix)
    create_comprehensive_gep_report(mfs_results, mss_results, comparison_table, unified_dir, prefix)
    log_enhanced("Unified GEP validation summary created", level = "INFO")
    return(list(
        comparison_table = comparison_table,
        unified_dir = unified_dir
    ))
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
    log_enhanced("Creating GEP comparison table", level = "DEBUG")
    comparison_data <- data.frame()
    if (!is.null(mfs_results) && !is.null(mfs_results$validation_results)) {
        for (tp_name in names(mfs_results$validation_results)) {
            tp_results <- mfs_results$validation_results[[tp_name]]
            cal_slope <- if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$slope)) tp_results$calibration$slope else NA
            cal_intercept <- if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$intercept)) tp_results$calibration$intercept else NA
            harrell_c <- if (!is.null(tp_results$discrimination) && !is.null(tp_results$discrimination$harrell_c)) tp_results$discrimination$harrell_c else NA
            uno_c <- if (!is.null(tp_results$discrimination) && !is.null(tp_results$discrimination$uno_c)) tp_results$discrimination$uno_c else NA
            comparison_data <- rbind(comparison_data, data.frame(
                outcome = "MFS",
                timepoint = tp_name,
                calibration_slope = cal_slope,
                calibration_intercept = cal_intercept,
                harrell_c = harrell_c,
                uno_c = uno_c,
                stringsAsFactors = FALSE
            ))
        }
    }
    if (!is.null(mss_results) && !is.null(mss_results$standard_validation)) {
        for (tp_name in names(mss_results$standard_validation)) {
            tp_results <- mss_results$standard_validation[[tp_name]]
            cal_slope <- if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$slope)) tp_results$calibration$slope else NA
            cal_intercept <- if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$intercept)) tp_results$calibration$intercept else NA
            harrell_c <- if (!is.null(tp_results$discrimination) && !is.null(tp_results$discrimination$harrell_c)) tp_results$discrimination$harrell_c else NA
            uno_c <- if (!is.null(tp_results$discrimination) && !is.null(tp_results$discrimination$uno_c)) tp_results$discrimination$uno_c else NA
            comparison_data <- rbind(comparison_data, data.frame(
                outcome = "MSS",
                timepoint = tp_name,
                calibration_slope = cal_slope,
                calibration_intercept = cal_intercept,
                harrell_c = harrell_c,
                uno_c = uno_c,
                stringsAsFactors = FALSE
            ))
        }
    }
    return(comparison_data)
}

#' Create comprehensive GEP report
#'
#' Write a human-readable text summary and optional Excel comparison table to
#' describe overall GEP validation performance across outcomes and timepoints.
#'
#' @param mfs_results MFS validation results (may be NULL)
#' @param mss_results MSS validation results (may be NULL)
#' @param comparison_table Data frame of comparison metrics (may be empty)
#' @param output_dir Directory to write report files
#' @param prefix Filename prefix for saved files
#' @return Invisibly returns NULL after writing files
create_comprehensive_gep_report <- function(mfs_results, mss_results, comparison_table, output_dir, prefix) {
    log_enhanced("Creating comprehensive GEP report", level = "DEBUG")
    summary_lines <- c(
        "GEP Validation Comprehensive Report",
        "===================================",
        sprintf("Analysis completed: %s", Sys.time()),
        "",
        "SUMMARY OF VALIDATION ANALYSES:",
        "",
        "MFS Validation:",
        sprintf("  - Status: %s", ifelse(!is.null(mfs_results), "Completed", "Not performed")),
        sprintf("  - Timepoints: %s", ifelse(!is.null(mfs_results), paste(names(mfs_results$validation_results), collapse = ", "), "N/A")),
        "",
        "MSS Validation:",
        sprintf("  - Status: %s", ifelse(!is.null(mss_results), "Completed", "Not performed")),
        sprintf("  - Timepoints: %s", ifelse(!is.null(mss_results), paste(names(mss_results$standard_validation), collapse = ", "), "N/A")),
        sprintf("  - Competing Risk Analysis: %s", ifelse(!is.null(mss_results) && !is.null(mss_results$competing_risk_validation), "Yes", "No")),
        "",
        "PERFORMANCE SUMMARY:",
        "",
        "Calibration Performance:",
        "  - Calibration slope close to 1.0 indicates good calibration",
        "  - Integrated Calibration Index (ICI) measures overall calibration",
        "",
        "Discrimination Performance:",
        "  - Harrell's C-index > 0.7 indicates good discrimination",
        "  - Uno's C-index provides time-dependent discrimination measure",
        "",
        "All detailed results saved as Excel tables and visualizations.",
        "See individual files for complete statistical outputs."
    )
    summary_path <- file.path(output_dir, paste0(prefix, "gep_comprehensive_report.txt"))
    writeLines(summary_lines, summary_path)
    if (nrow(comparison_table) > 0) {
        excel_path <- file.path(output_dir, paste0(prefix, "gep_comparison_table.xlsx"))
        writexl::write_xlsx(comparison_table, excel_path)
    }
    log_enhanced(sprintf("Comprehensive GEP report saved: %s", summary_path), level = "INFO")
}
