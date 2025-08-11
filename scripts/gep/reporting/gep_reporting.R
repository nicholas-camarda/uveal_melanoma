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
    logger::log_info(formatted("Creating comprehensive MFS validation report", indent = 1))
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
                overall_oe_ci_low = oe_data$overall_poisson_ci_lower,
                overall_oe_ci_high = oe_data$overall_poisson_ci_upper,
                chisq_p_value = oe_data$chisq_p_value,
                calibration_p = if (!is.null(result$calibration)) result$calibration$nam_dagostino_p else NA,
                ici = if (!is.null(result$calibration)) result$calibration$ici else NA,
                calibration_slope = if (!is.null(result$calibration)) result$calibration$calibration_slope else NA,
                discrimination_c = if (!is.null(result$discrimination)) result$discrimination$harrell_c else NA,
                uno_c = if (!is.null(result$discrimination)) result$discrimination$uno_c else NA,
                auc = if (!is.null(result$discrimination)) result$discrimination$auc_timepoint else NA,
                dca_opt_threshold = if (!is.null(result$decision_curve) && !is.null(result$decision_curve$optimal_threshold)) result$decision_curve$optimal_threshold else NA,
                dca_max_net_benefit = if (!is.null(result$decision_curve) && !is.null(result$decision_curve$max_net_benefit)) result$decision_curve$max_net_benefit else NA,
                events_analyzed = result$events,
                n = result$n
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
    logger::log_info(formatted("Saving MFS validation results", indent = 1))
    tryCatch(
        {
            # Build unified workbook sheets (canonical output)
            oe_summary <- data.frame()
            for (tp_key in names(validation_results)) {
                result <- validation_results[[tp_key]]
                if (!is.null(result$observed_expected)) {
                    oe_data <- result$observed_expected
                    # Per-class rows
                    for (class in names(oe_data$results_by_class)) {
                        class_result <- oe_data$results_by_class[[class]]
                        oe_summary <- rbind(oe_summary, data.frame(
                            Timepoint = tp_key,
                            GEP_Class = class,
                            N = class_result$n,
                            Observed = class_result$observed,
                            Expected = class_result$expected,
                            OE_Ratio = class_result$oe_ratio,
                            CI_Lower = class_result$poisson_ci_lower,
                            CI_Upper = class_result$poisson_ci_upper,
                            stringsAsFactors = FALSE
                        ))
                    }
                    # Overall row per timepoint
                    oe_summary <- rbind(oe_summary, data.frame(
                        Timepoint = tp_key,
                        GEP_Class = "Overall",
                        N = sum(vapply(oe_data$results_by_class, function(x) x$n, numeric(1))),
                        Observed = oe_data$overall_observed,
                        Expected = oe_data$overall_expected,
                        OE_Ratio = oe_data$overall_oe_ratio,
                        CI_Lower = oe_data$overall_poisson_ci_lower,
                        CI_Upper = oe_data$overall_poisson_ci_upper,
                        stringsAsFactors = FALSE
                    ))
                }
            }

            cal_summary <- data.frame()
            for (tp_key in names(validation_results)) {
                result <- validation_results[[tp_key]]
                if (!is.null(result$calibration)) {
                    cal_data <- result$calibration
                    cal_summary <- rbind(cal_summary, data.frame(
                        Timepoint = tp_key,
                        N = cal_data$n,
                        Nam_D_Agostino_p = cal_data$nam_dagostino_p,
                        ICI = cal_data$ici,
                        Slope = cal_data$calibration_slope,
                        stringsAsFactors = FALSE
                    ))
                }
            }

            disc_summary <- data.frame()
            for (tp_key in names(validation_results)) {
                result <- validation_results[[tp_key]]
                if (!is.null(result$discrimination)) {
                    disc_data <- result$discrimination
                    disc_summary <- rbind(disc_summary, data.frame(
                        Timepoint = tp_key,
                        N = disc_data$n,
                        Events = disc_data$events,
                        Harrell_C = disc_data$harrell_c,
                        Uno_C = disc_data$uno_c,
                        AUC = disc_data$auc_timepoint,
                        stringsAsFactors = FALSE
                    ))
                }
            }

            unified_sheets <- list()
            if (nrow(oe_summary) > 0) unified_sheets[["Observed_Expected_by_class"]] <- oe_summary
            if (nrow(cal_summary) > 0) unified_sheets[["Calibration"]] <- cal_summary
            if (nrow(disc_summary) > 0) unified_sheets[["Discrimination"]] <- disc_summary
            if (length(unified_sheets) > 0) {
                writexl::write_xlsx(unified_sheets, file.path(output_dir, paste0(prefix, "mfs_validation_summary.xlsx")))
            }
        },
        error = function(e) {
            logger::log_warn(formatted("Error saving summary tables", indent = 2))
        }
    )
    # Optional: persist R objects for reproducibility or downstream analyses
    if (GEP_SAVE_RDS) {
        saveRDS(validation_results, file.path(output_dir, paste0(prefix, "mfs_validation_results.rds")))
        saveRDS(missing_data_analysis, file.path(output_dir, paste0(prefix, "missing_data_analysis.rds")))
        saveRDS(prame_analysis, file.path(output_dir, paste0(prefix, "prame_analysis.rds")))
    }

    # Build richer human-readable summary
    tp_sections <- c()
    for (tp_key in names(validation_results)) {
        res <- validation_results[[tp_key]]
        oe <- res$observed_expected
        cal <- res$calibration
        disc <- res$discrimination
        dca <- res$decision_curve
        cls_lines <- c()
        if (!is.null(oe) && !is.null(oe$results_by_class)) {
            for (cls in names(oe$results_by_class)) {
                x <- oe$results_by_class[[cls]]
                cls_lines <- c(cls_lines, sprintf("      %s: N=%d, Obs=%d, Exp=%.2f, O/E=%.2f (%.2f–%.2f)",
                    cls, x$n, x$observed, x$expected, x$oe_ratio, x$poisson_ci_lower, x$poisson_ci_upper))
            }
        }
        tp_sections <- c(tp_sections,
            sprintf("Timepoint: %s", tp_key),
            sprintf("  Overall O/E: %s; Chi-square p=%s",
                ifelse(is.null(oe$overall_oe_ratio) || is.na(oe$overall_oe_ratio), "NA", sprintf("%.2f (%.2f–%.2f)",
                    oe$overall_oe_ratio,
                    ifelse(is.null(oe$overall_poisson_ci_lower) || is.na(oe$overall_poisson_ci_lower), NA, oe$overall_poisson_ci_lower),
                    ifelse(is.null(oe$overall_poisson_ci_upper) || is.na(oe$overall_poisson_ci_upper), NA, oe$overall_poisson_ci_upper)
                )),
                ifelse(is.null(oe$chisq_p_value) || is.na(oe$chisq_p_value), "NA", sprintf("%.4f", oe$chisq_p_value))
            ),
            sprintf("  Calibration: slope=%.3f, ICI=%.3f, Nam-D'Agostino p=%.4f",
                ifelse(is.null(cal$calibration_slope), NA, cal$calibration_slope),
                ifelse(is.null(cal$ici), NA, cal$ici),
                ifelse(is.null(cal$nam_dagostino_p), NA, cal$nam_dagostino_p)
            ),
            sprintf("  Discrimination: Harrell's C=%.3f, Uno's C=%s, AUC=%s",
                ifelse(is.null(disc$harrell_c), NA, disc$harrell_c),
                ifelse(is.null(disc$uno_c) || is.na(disc$uno_c), "NA", sprintf("%.3f", disc$uno_c)),
                ifelse(is.null(disc$auc_timepoint) || is.na(disc$auc_timepoint), "NA", sprintf("%.3f", disc$auc_timepoint))
            ),
            sprintf("  Decision curve: optimal threshold=%s%%, max net benefit=%s",
                ifelse(is.null(dca$optimal_threshold), "NA", sprintf("%.2f", dca$optimal_threshold * 100)),
                ifelse(is.null(dca$max_net_benefit), "NA", sprintf("%.4f", dca$max_net_benefit))
            ),
            if (length(cls_lines) > 0) c("  By GEP class:", cls_lines) else NULL,
            ""
        )
    }

    prame_line <- if (!is.null(prame_analysis) && !is.null(prame_analysis$nri)) {
        nri_val <- tryCatch({
            if (is.list(prame_analysis$nri) && !is.null(prame_analysis$nri$total)) prame_analysis$nri$total else as.numeric(prame_analysis$nri)
        }, error = function(e) NA_real_)
        sprintf("PRAME added value (NRI): %s", ifelse(is.na(nri_val), "NA", sprintf("%.3f", nri_val)))
    } else if (!is.null(prame_analysis) && !is.null(prame_analysis$summary)) {
        sprintf("PRAME analysis summary: %s", prame_analysis$summary)
    } else {
        "PRAME analysis: insufficient data or not applicable"
    }

    report_lines <- c(
        "GEP Metastasis-Free Survival Validation Report",
        paste(rep("=", 50), collapse = ""),
        paste("Analysis completed:", Sys.time()),
        "",
        sprintf("Dataset: %s", ifelse(is.null(validation_report$dataset), "", validation_report$dataset)),
        sprintf("Timepoints analyzed: %s", paste(names(validation_results), collapse = ", ")),
        "",
        "Per-timepoint results:",
        tp_sections,
        "",
        prame_line,
        sprintf("Missing data patterns: %d", ifelse(is.null(missing_data_analysis$missing_patterns), 0, nrow(missing_data_analysis$missing_patterns))),
        "",
        "All detailed tables (O/E, calibration, discrimination) saved as Excel files.",
        "Decision curve CSV data saved when available.",
        "See PNGs for calibration, discrimination, and DCA visuals."
    )
    writeLines(report_lines, file.path(output_dir, paste0(prefix, "mfs_validation_summary.txt")))
    logger::log_info(formatted("MFS validation results saved successfully", indent = 2))
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
    logger::log_info("Creating comprehensive MSS validation report")
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
    logger::log_info("Saving MSS validation results")
    if (GEP_SAVE_RDS) {
        saveRDS(standard_results, file.path(output_dir, paste0(prefix, "mss_standard_validation_results.rds")))
        saveRDS(competing_results, file.path(output_dir, paste0(prefix, "mss_competing_risk_results.rds")))
        saveRDS(validation_report, file.path(output_dir, paste0(prefix, "mss_validation_report.rds")))
        if (!is.null(missing_data)) {
            saveRDS(missing_data, file.path(output_dir, paste0(prefix, "mss_missing_data_analysis.rds")))
        }
        if (!is.null(prame_results)) {
            saveRDS(prame_results, file.path(output_dir, paste0(prefix, "mss_prame_analysis.rds")))
        }
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
    logger::log_info("Creating MSS validation Excel files")
    excel_sheets <- list()
    excel_sheets[["Summary_Statistics"]] <- validation_report$summary_stats

    # Stacked tables parallel to MFS workbook
    obs_exp_df <- data.frame()
    cal_df <- data.frame()
    disc_df <- data.frame()
    counts_df <- data.frame()
    cif_ci_df <- data.frame()

    for (tp_name in names(standard_results)) {
        tp_results <- standard_results[[tp_name]]
        # Observed/Expected by class using available fields
        if (!is.null(tp_results$observed_expected)) {
            oe <- tp_results$observed_expected
            if (is.data.frame(oe)) {
                # Accept either counts or rates input; compute rates if needed
                if (all(c("gep_class_simple","n") %in% names(oe))) {
                    tmp_counts <- oe[, intersect(names(oe), c("gep_class_simple","n","observed","expected","expected_rate","observed_rate"))]
                    tmp_counts$Timepoint <- tp_name
                    counts_df <- rbind(counts_df, tmp_counts)
                }
                if (all(c("expected","observed","n") %in% names(oe))) {
                    tmp <- data.frame(
                        Timepoint = tp_name,
                        GEP_Class = oe$gep_class_simple,
                        N = oe$n,
                        Expected = oe$expected,
                        Observed = oe$observed,
                        OE_Ratio = ifelse(oe$expected > 0, oe$observed / oe$expected, NA_real_),
                        stringsAsFactors = FALSE
                    )
                    obs_exp_df <- rbind(obs_exp_df, tmp)
                } else if (all(c("expected_rate","observed_rate","n") %in% names(oe))) {
                    tmp <- data.frame(
                        Timepoint = tp_name,
                        GEP_Class = oe$gep_class_simple,
                        N = oe$n,
                        Expected = oe$expected_rate * oe$n,
                        Observed = oe$observed_rate * oe$n,
                        OE_Ratio = ifelse(oe$expected_rate > 0, (oe$observed_rate / oe$expected_rate), NA_real_),
                        stringsAsFactors = FALSE
                    )
                    obs_exp_df <- rbind(obs_exp_df, tmp)
                }
            }
        }
        # Calibration
        if (!is.null(tp_results$calibration)) {
            cal <- tp_results$calibration
            cal_df <- rbind(cal_df, data.frame(
                Timepoint = tp_name,
                N = cal$n %||% NA,
                Nam_D_Agostino_p = cal$nam_dagostino_p %||% cal$nam_dagostino_p,
                ICI = cal$ici %||% NA,
                Slope = cal$slope %||% cal$calibration_slope %||% NA,
                stringsAsFactors = FALSE
            ))
        }
        # Discrimination
        if (!is.null(tp_results$discrimination)) {
            d <- tp_results$discrimination
            if (!is.data.frame(d)) d <- as.data.frame(lapply(d, identity))
            d$Timepoint <- tp_name
            disc_df <- rbind(disc_df, d)
        }
    }

    if (nrow(obs_exp_df) > 0) excel_sheets[["Observed_Expected_by_class"]] <- obs_exp_df
    if (nrow(cal_df) > 0) excel_sheets[["Calibration"]] <- cal_df
    if (nrow(disc_df) > 0) excel_sheets[["Discrimination"]] <- disc_df
    if (nrow(counts_df) > 0) excel_sheets[["Counts"]] <- counts_df

    if (!is.null(competing_results)) {
        # Stack competing risks tables with a Timepoint column
        ci_df <- data.frame(); csh_df <- data.frame()
        for (tp_name in names(competing_results)) {
            tp_results <- competing_results[[tp_name]]
            if (!is.null(tp_results$cumulative_incidence)) {
                tmp <- tp_results$cumulative_incidence
                tmp$Timepoint <- tp_name
                ci_df <- rbind(ci_df, tmp)
            }
            if (!is.null(tp_results$cause_specific_hazards)) {
                tmp <- tp_results$cause_specific_hazards
                tmp$Timepoint <- tp_name
                csh_df <- rbind(csh_df, tmp)
            }
            if (!is.null(tp_results$cif_with_ci)) {
                tmp <- tp_results$cif_with_ci
                tmp$Timepoint <- tp_name
                names(tmp) <- c("GEP_Class","N","CIF","CI_Lower","CI_Upper","Timepoint")
                cif_ci_df <- rbind(cif_ci_df, tmp)
            }
        }
        if (nrow(ci_df) > 0) excel_sheets[["CompetingRisk_CumulativeIncidence"]] <- ci_df
        if (nrow(csh_df) > 0) excel_sheets[["CompetingRisk_CauseSpecificHazards"]] <- csh_df
        if (nrow(cif_ci_df) > 0) excel_sheets[["CompetingRisk_CIF_with_CI"]] <- cif_ci_df
    }
    excel_path <- file.path(output_dir, paste0(prefix, "mss_validation_summary.xlsx"))
    writexl::write_xlsx(excel_sheets, excel_path)
    logger::log_info(sprintf("MSS validation Excel file saved: %s", excel_path))
}

#' Create MSS validation summary text
create_mss_validation_summary_text <- function(standard_results, competing_results, validation_report,
                                               missing_data, prame_results, output_dir, prefix) {
    logger::log_info("Creating MSS validation summary text file")
    tp_sections <- c()
    for (tp_name in names(standard_results)) {
        res <- standard_results[[tp_name]]
        oe <- res$observed_expected
        cal <- res$calibration
        disc <- res$discrimination
        dca <- res$decision_curve
        tp_sections <- c(tp_sections,
            sprintf("Timepoint: %s", tp_name),
            if (!is.null(oe) && is.data.frame(oe) && all(c("expected_rate","observed_rate") %in% names(oe))) {
                sprintf("  O/E summary: mean expected=%.3f, mean observed=%.3f", mean(oe$expected_rate, na.rm = TRUE), mean(oe$observed_rate, na.rm = TRUE))
            } else { "  O/E summary: see Excel for details" },
            sprintf("  Calibration: slope=%.3f, ICI=%.3f, Nam-D'Agostino p=%.4f",
                ifelse(is.null(cal$slope), NA, cal$slope),
                ifelse(is.null(cal$ici), NA, cal$ici),
                ifelse(is.null(cal$nam_dagostino_p), NA, cal$nam_dagostino_p)
            ),
            sprintf("  Discrimination: Harrell's C=%.3f, Uno's C=%s, AUC=%s",
                ifelse(is.null(disc$harrell_c), NA, disc$harrell_c),
                ifelse(is.null(disc$uno_c) || is.na(disc$uno_c), "NA", sprintf("%.3f", disc$uno_c)),
                ifelse(is.null(disc$auc_timepoint) || is.na(disc$auc_timepoint), "NA", sprintf("%.3f", disc$auc_timepoint))
            ),
            sprintf("  Decision curve: optimal threshold=%s%%, max net benefit=%s",
                ifelse(is.null(dca$optimal_threshold), "NA", sprintf("%.2f", dca$optimal_threshold * 100)),
                ifelse(is.null(dca$max_net_benefit), "NA", sprintf("%.4f", dca$max_net_benefit))
            ),
            ""
        )
    }

    prame_line <- if (!is.null(prame_results) && !is.null(prame_results$nri)) {
        nri_val <- tryCatch({
            if (is.list(prame_results$nri) && !is.null(prame_results$nri$total)) prame_results$nri$total else as.numeric(prame_results$nri)
        }, error = function(e) NA_real_)
        sprintf("PRAME added value (NRI): %s", ifelse(is.na(nri_val), "NA", sprintf("%.3f", nri_val)))
    } else if (!is.null(prame_results) && !is.null(prame_results$summary)) {
        sprintf("PRAME analysis summary: %s", prame_results$summary)
    } else {
        "PRAME analysis: insufficient data or not applicable"
    }

    summary_lines <- c(
        "GEP Melanoma-Specific Survival Validation Report",
        "==================================================",
        sprintf("Analysis completed: %s", Sys.time()),
        "",
        sprintf("Dataset: %s", ifelse(is.null(validation_report$summary_stats$dataset), "", validation_report$summary_stats$dataset)),
        sprintf("Timepoints analyzed: %s", paste(names(standard_results), collapse = ", ")),
        "",
        "Per-timepoint results:",
        tp_sections,
        "",
        prame_line,
        sprintf("Missing data assessment performed: %s", ifelse(!is.null(missing_data), "Yes", "No")),
        "",
        "All detailed results saved as Excel tables and RDS objects.",
        "See individual files for complete statistical outputs."
    )
    summary_path <- file.path(output_dir, paste0(prefix, "mss_validation_summary.txt"))
    writeLines(summary_lines, summary_path)
    logger::log_info(sprintf("MSS validation summary saved: %s", summary_path))
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
    logger::log_info("Creating unified GEP validation summary")
    unified_dir <- file.path(output_dir, "unified_summary")
    if (!dir.exists(unified_dir)) {
        dir.create(unified_dir, recursive = TRUE, showWarnings = FALSE)
    }
    comparison_table <- create_gep_comparison_table(mfs_results, mss_results)
    create_integrated_gep_visuals(mfs_results, mss_results, unified_dir, prefix)
    create_comprehensive_gep_report(mfs_results, mss_results, comparison_table, unified_dir, prefix)
    logger::log_info("Unified GEP validation summary created")
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
    logger::log_info(formatted("Creating GEP comparison table"))
    comparison_data <- data.frame()
    if (!is.null(mfs_results) && !is.null(mfs_results$validation_results)) {
        for (tp_name in names(mfs_results$validation_results)) {
            tp_results <- mfs_results$validation_results[[tp_name]]
            cal_slope <- if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$slope)) tp_results$calibration$slope else tp_results$calibration$calibration_slope
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
    if (!is.null(mss_results)) {
        mss_container <- if (!is.null(mss_results$standard_validation)) mss_results$standard_validation else mss_results$standard_results
        if (!is.null(mss_container)) {
            for (tp_name in names(mss_container)) {
                tp_results <- mss_container[[tp_name]]
                cal_slope <- if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$slope)) tp_results$calibration$slope else tp_results$calibration$calibration_slope
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
    logger::log_info(formatted("Creating comprehensive GEP report"))
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
        sprintf("  - Timepoints: %s", ifelse(!is.null(mss_results), paste(if (!is.null(mss_results$standard_validation)) names(mss_results$standard_validation) else names(mss_results$standard_results), collapse = ", "), "N/A")),
        sprintf("  - Competing Risk Analysis: %s", ifelse(!is.null(mss_results) && (!is.null(mss_results$competing_risk_validation) || !is.null(mss_results$competing_results)), "Yes", "No")),
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
    logger::log_info(sprintf("Comprehensive GEP report saved: %s", summary_path))
}