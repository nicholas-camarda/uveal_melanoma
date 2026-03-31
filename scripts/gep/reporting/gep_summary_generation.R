# GEP Summary Generation Functions
# Comprehensive summary creation for GEP validation results

create_gep_prediction_source_note <- function(extrapolation_assessment = NULL) {
    extrapolation_lines <- if (!is.null(extrapolation_assessment)) {
        c(
            md_bullet(sprintf(
                "Current extrapolation support status: %s",
                extrapolation_assessment$status %||% "Unavailable"
            )),
            md_bullet(sprintf(
                "Current extrapolation support note: %s",
                extrapolation_assessment$note %||% "No extrapolation support note available."
            ))
        )
    } else {
        md_bullet("The 7-year and 10-year values should be interpreted as assumption-dependent extensions.")
    }

    c(
        md_heading("Prediction Source Note", 2L),
        md_bullet("The base GEP predictions are imported lab-reported 5-year survival probabilities, not newly fit model outputs."),
        md_bullet("At 5 years the pipeline uses the supplied lab value directly; at 7 and 10 years it derives survival from that same 5-year value using exponential-decay extrapolation."),
        md_bullet("7-year survival = (5-year survival)^(7/5)", indent = 1L),
        md_bullet("10-year survival = (5-year survival)^(10/5)", indent = 1L),
        extrapolation_lines
    )
}

#' Create narrative lines for extrapolation interpretation
#'
#' Summarize the later-horizon extrapolation support result in plain language for
#' the narrative summary files.
#'
#' @param extrapolation_assessment Objective 4 extrapolation-support summary for
#'   later horizons.
#' @return Character vector containing a formatted narrative section.
create_gep_extrapolation_narrative_section <- function(extrapolation_assessment = NULL) {
    if (is.null(extrapolation_assessment)) {
        return(c(
            md_heading("Extrapolation Interpretation", 2L),
            md_bullet("No later-horizon extrapolation-support assessment was supplied.")
        ))
    }

    c(
        md_heading("Extrapolation Interpretation", 2L),
        md_bullet(sprintf(
            "Current extrapolation support status: %s",
            extrapolation_assessment$status %||% "Unavailable"
        )),
        md_bullet(sprintf(
            "Current extrapolation support note: %s",
            extrapolation_assessment$note %||% "No extrapolation support note available."
        )),
        md_bullet("Later-horizon issues usually reflect the 7-year and 10-year extension rule, not the imported 5-year assay output itself.")
    )
}

create_prame_added_value_summary_line <- function(prame_analysis) {
    if (!is.null(prame_analysis) && !is.null(prame_analysis$comparison_results) && length(prame_analysis$comparison_results) > 0) {
        comparison_rows <- Filter(function(x) is.list(x) && is.finite(x$delta_harrell_c %||% NA_real_), prame_analysis$comparison_results)
        if (length(comparison_rows) > 0) {
            best_row <- comparison_rows[[which.max(vapply(comparison_rows, function(x) x$delta_harrell_c, numeric(1)))]]
            return(sprintf(
                "PRAME ADDED VALUE: Best delta Harrell's C = %.3f at %s (%s)",
                best_row$delta_harrell_c,
                paste0(best_row$timepoint, "yr"),
                best_row$interpretation %||% "interpretation unavailable"
            ))
        }
    }

    if (is.null(prame_analysis)) {
        return("PRAME ADDED VALUE: Analysis not run")
    }

    n_prame <- prame_analysis$n %||% NA_real_
    n_positive <- prame_analysis$n_positive %||% NA_real_
    n_negative <- prame_analysis$n_negative %||% NA_real_
    status <- as.character(prame_analysis$status %||% "unavailable")

    if (!is.null(prame_analysis$prame_distribution) && (is.na(n_positive) || is.na(n_negative))) {
        n_positive <- as.numeric(prame_analysis$prame_distribution[["Positive"]] %||% NA_real_)
        n_negative <- as.numeric(prame_analysis$prame_distribution[["Negative"]] %||% NA_real_)
    }

    if (!is.null(prame_analysis$prame_available) && identical(prame_analysis$prame_available, FALSE)) {
        if (!is.na(n_prame) && identical(status, "insufficient_data")) {
            return(sprintf(
                "PRAME ADDED VALUE: Not run due to insufficient PRAME-complete sample (n=%d; Positive=%d, Negative=%d; required minimum=%d)",
                as.integer(n_prame),
                as.integer(n_positive %||% NA_integer_),
                as.integer(n_negative %||% NA_integer_),
                GEP_MIN_BOOTSTRAP_SAMPLE
            ))
        }

        return(sprintf(
            "PRAME ADDED VALUE: Not run (status=%s)",
            status
        ))
    }

    sprintf(
        "PRAME ADDED VALUE: Analysis unavailable (status=%s)",
        status
    )
}

#' Create Comprehensive GEP Validation Summary
#' 
#' Generate a comprehensive, interpretable summary that consolidates redundant information
#' across timepoints while maintaining all statistical information. This replaces the
#' repetitive per-timepoint approach with clinical interpretation and pattern analysis.
#'
#' @param validation_results Named list of per-timepoint validation results
#' @param outcome_type Either "MFS" or "MSS" for appropriate clinical context
#' @param prame_analysis PRAME-augmented analysis results (may be NULL)
#' @param missing_data_analysis Missing-data diagnostics results
#' @param dataset_name Optional dataset label used in the report
#' @param extrapolation_assessment Objective 4 extrapolation-support summary for
#'   later horizons.
#' @param include_prediction_source_note Whether to include the base-prediction
#'   source note in this summary body.
#' @param source_data Optional analytic dataset used to derive compact
#'   follow-up limitation summaries.
#' @return A comprehensive summary text suitable for saving
create_comprehensive_gep_summary <- function(validation_results, outcome_type, prame_analysis, missing_data_analysis, dataset_name,
                                             extrapolation_assessment = NULL,
                                             include_prediction_source_note = TRUE,
                                             source_data = NULL) {
    logger::log_info(sprintf("Creating comprehensive GEP validation summary for %s", outcome_type))

    if (is.null(dataset_name) || !nzchar(dataset_name)) {
        dataset_name <- "dataset_not_provided"
    }
    
    # Extract key metrics across timepoints for pattern analysis
    timepoints <- names(validation_results)
    
    # Initialize data frames with proper column structure to avoid rbind errors
    calibration_data <- data.frame(
        Timepoint = character(),
        Slope = numeric(),
        Slope_Method = character(),
        Status = character(),
        Fit_N = numeric(),
        Events = numeric(),
        Non_Events = numeric(),
        Unique_Risk_Count = numeric(),
        Slope_SE = numeric(),
        ICI = numeric(),
        ICI_Method = character(),
        Nam_D_Agostino_p = numeric(),
        stringsAsFactors = FALSE
    )
    
    discrimination_data <- data.frame(
        Timepoint = character(),
        Harrell_C = numeric(),
        Integrated_AUC = numeric(),
        Integrated_AUC_Status = character(),
        Integrated_AUC_Method = character(),
        Integrated_AUC_Unavailable_Reason = character(),
        Cumulative_Discrimination = numeric(),
        Time_averaged_Discrimination = numeric(),
        stringsAsFactors = FALSE
    )
    
    oe_data <- data.frame(
        Timepoint = character(),
        Overall_OE = numeric(),
        CI_Lower = numeric(),
        CI_Upper = numeric(),
        OE_Chi_Square_p = numeric(),
        stringsAsFactors = FALSE
    )
    
    for (tp in timepoints) {
        result <- validation_results[[tp]]
        
        # Calibration metrics - with defensive programming
        if (!is.null(result$calibration)) {
            cal <- result$calibration
            # Debug: Log calibration data structure
            logger::log_debug(sprintf("Calibration data for %s: class=%s, names=%s", tp, class(cal)[1], paste(names(cal), collapse=", ")))
            
            # Both MFS and MSS now use 'slope' field
            if (!is.null(cal$slope)) {
                new_cal_row <- data.frame(
                    Timepoint = tp,
                    Slope = cal$slope,
                    Slope_Method = ifelse(is.null(cal$slope_method), NA_character_, cal$slope_method),
                    Status = ifelse(is.null(cal$status), NA_character_, cal$status),
                    Fit_N = ifelse(is.null(cal$fit_n), NA_real_, cal$fit_n),
                    Events = ifelse(is.null(cal$events), NA_real_, cal$events),
                    Non_Events = ifelse(is.null(cal$non_events), NA_real_, cal$non_events),
                    Unique_Risk_Count = ifelse(is.null(cal$unique_risk_count), NA_real_, cal$unique_risk_count),
                    Slope_SE = ifelse(is.null(cal$slope_se), NA_real_, cal$slope_se),
                    ICI = ifelse(is.null(cal$ici), NA_real_, cal$ici),
                    ICI_Method = ifelse(is.null(cal$ici_method), NA_character_, cal$ici_method),
                    Nam_D_Agostino_p = ifelse(is.null(cal$nam_dagostino_p), NA_real_, cal$nam_dagostino_p),
                    stringsAsFactors = FALSE
                )
                calibration_data <- rbind(calibration_data, new_cal_row)
            } else {
                logger::log_warn(sprintf("No slope found in calibration data for %s", tp))
            }
        }
        
        # Discrimination metrics - with defensive programming
        if (!is.null(result$discrimination) && !is.null(result$discrimination$harrell_c)) {
            disc <- result$discrimination
            # Debug: Log what's in discrimination data
            logger::log_info(sprintf("Discrimination data for %s: Harrell_C=%s, Integrated_AUC=%s, Cumulative_Disc=%s", 
                                   tp, 
                                   ifelse(is.null(disc$harrell_c), "NULL", as.character(disc$harrell_c)),
                                   ifelse(is.null(disc$integrated_auc), "NULL", as.character(disc$integrated_auc)),
                                   ifelse(is.null(disc$cumulative_discrimination), "NULL", as.character(disc$cumulative_discrimination))))
            new_disc_row <- data.frame(
                Timepoint = tp,
                Harrell_C = ifelse(is.null(disc$harrell_c), NA_real_, disc$harrell_c),
                Integrated_AUC = ifelse(is.null(disc$integrated_auc), NA_real_, disc$integrated_auc),
                Integrated_AUC_Status = ifelse(is.null(disc$integrated_auc_status), NA_character_, disc$integrated_auc_status),
                Integrated_AUC_Method = ifelse(is.null(disc$integrated_auc_method), NA_character_, disc$integrated_auc_method),
                Integrated_AUC_Unavailable_Reason = ifelse(is.null(disc$integrated_auc_na_reason), NA_character_, disc$integrated_auc_na_reason),
                Cumulative_Discrimination = ifelse(is.null(disc$cumulative_discrimination), NA_real_, disc$cumulative_discrimination),
                Time_averaged_Discrimination = ifelse(is.null(disc$time_averaged_discrimination), NA_real_, disc$time_averaged_discrimination),
                stringsAsFactors = FALSE
            )
            discrimination_data <- rbind(discrimination_data, new_disc_row)
        }
        
        # Observed/Expected metrics - support both MFS list outputs and MSS tibble outputs
        if (!is.null(result$observed_expected)) {
            oe <- extract_overall_oe_metrics(result$observed_expected)
            if (!is.null(oe)) {
                new_oe_row <- data.frame(
                    Timepoint = tp,
                    Overall_OE = oe$oe_ratio %||% NA_real_,
                    CI_Lower = oe$poisson_ci_lower %||% NA_real_,
                    CI_Upper = oe$poisson_ci_upper %||% NA_real_,
                    OE_Chi_Square_p = oe$chi_square_p %||% NA_real_,
                    stringsAsFactors = FALSE
                )
                oe_data <- rbind(oe_data, new_oe_row)
            }
        }
    }
    
    # Debug: Log data frame dimensions before clinical interpretation
    logger::log_info(sprintf("Data dimensions for clinical interpretation - Calibration: %d rows, Discrimination: %d rows, O/E: %d rows", 
                            nrow(calibration_data), nrow(discrimination_data), nrow(oe_data)))
    
    # Clinical interpretation and pattern analysis - with error handling
    clinical_summary <- tryCatch({
        # Check if we have any data to interpret
        if (nrow(calibration_data) == 0 && nrow(discrimination_data) == 0 && nrow(oe_data) == 0) {
            logger::log_warn("No validation data available for clinical interpretation")
            list(
                overall_assessment = sprintf("GEP %s validation analysis completed. No validation metrics available for clinical interpretation.", outcome_type),
                calibration_interpretation = "Calibration metrics not available",
                discrimination_interpretation = "Discrimination metrics not available", 
                oe_interpretation = "Observed/Expected metrics not available",
                temporal_patterns = "Temporal pattern analysis not available",
                clinical_implications = "Clinical implications not available"
            )
        } else {
            create_clinical_interpretation(calibration_data, discrimination_data, oe_data, outcome_type)
        }
    }, error = function(e) {
        logger::log_warn(sprintf("Clinical interpretation failed: %s", e$message))
        # Return default clinical summary if interpretation fails
        list(
            overall_assessment = sprintf("GEP %s validation analysis completed. Clinical interpretation could not be generated due to data structure issues.", outcome_type),
            calibration_interpretation = "Calibration interpretation not available",
            discrimination_interpretation = "Discrimination interpretation not available", 
            oe_interpretation = "Observed/Expected interpretation not available",
            temporal_patterns = "Temporal pattern analysis not available",
            clinical_implications = "Clinical implications not available"
        )
    })

    prediction_source_note <- if (isTRUE(include_prediction_source_note)) {
        c("", create_gep_prediction_source_note(extrapolation_assessment = extrapolation_assessment))
    } else {
        character()
    }
    extrapolation_narrative <- c("", create_gep_extrapolation_narrative_section(extrapolation_assessment = extrapolation_assessment))
    followup_narrative <- character()
    if (!is.null(source_data)) {
        followup_summary <- collect_objective4_endpoint_followup_summary(
            data = source_data,
            dataset_name = dataset_name,
            eligibility_filter = if (identical(outcome_type, "MFS")) "mfs_analysis_eligible" else "mss_analysis_eligible",
            event_prefix = tolower(outcome_type),
            time_horizon_years = 5
        )
        followup_narrative <- c("", build_objective4_followup_limitation_block(followup_summary))
    }
    
    detailed_metrics_block <- tryCatch({
        create_detailed_metrics_table(validation_results)
    }, error = function(e) {
        logger::log_warn(sprintf("Detailed metrics table creation failed: %s", e$message))
        c("Detailed metrics table could not be generated due to data structure issues")
    })

    missing_data_count <- if (!is.null(missing_data_analysis) && !is.null(missing_data_analysis$missing_patterns)) {
        nrow(missing_data_analysis$missing_patterns)
    } else {
        0L
    }

    report_lines <- c(
        md_heading(sprintf("GEP %s Validation - Comprehensive Clinical Summary", outcome_type), 1L),
        "",
        sprintf("Analysis completed: %s", Sys.time()),
        "",
        md_bullet(sprintf("Dataset: %s", dataset_name)),
        md_bullet(sprintf("Outcome: %s", outcome_type)),
        md_bullet(sprintf("Timepoints analyzed: %s", paste(timepoints, collapse = ", "))),
        "",
        prediction_source_note,
        "",
        extrapolation_narrative,
        "",
        followup_narrative,
        "",
        md_heading("Clinical Summary", 2L),
        clinical_summary$overall_assessment,
        "",
        md_heading("Calibration Analysis", 2L),
        clinical_summary$calibration_interpretation,
        "",
        md_heading("Discrimination Analysis", 2L),
        clinical_summary$discrimination_interpretation,
        "",
        md_heading("Observed vs Expected Analysis", 2L),
        clinical_summary$oe_interpretation,
        "",
        md_heading("Time-Dependent Patterns", 2L),
        clinical_summary$temporal_patterns,
        "",
        md_heading("Clinical Implications", 2L),
        clinical_summary$clinical_implications,
        "",
        md_heading("Detailed Metrics by Timepoint", 2L),
        detailed_metrics_block,
        "",
        md_heading("PRAME Added Value", 2L),
        md_bullet(create_prame_added_value_summary_line(prame_analysis)),
        "",
        md_bullet(sprintf("Missing data patterns: %d", missing_data_count)),
        "",
        md_bullet("This comprehensive summary consolidates information from multiple timepoints to provide clinical interpretation and pattern analysis."),
        md_bullet("All detailed statistical outputs remain available in the accompanying Excel files and visualizations.")
    )
    
    return(paste(report_lines, collapse = "\n"))
}

#' Create Comprehensive GEP Report
#'
#' Write a human-readable Markdown summary and optional Excel comparison table to
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
        md_heading("GEP Validation Comprehensive Report", 1L),
        sprintf("Analysis completed: %s", Sys.time()),
        "",
        md_heading("Summary of Validation Analyses", 2L),
        "",
        md_heading("MFS Validation", 3L),
        md_bullet(sprintf("Status: %s", ifelse(!is.null(mfs_results), "Completed", "Not performed"))),
        md_bullet(sprintf("Timepoints: %s", ifelse(!is.null(mfs_results), paste(names(mfs_results$validation_results), collapse = ", "), "N/A"))),
        "",
        md_heading("MSS Validation", 3L),
        md_bullet(sprintf("Status: %s", ifelse(!is.null(mss_results), "Completed", "Not performed"))),
        md_bullet(sprintf("Timepoints: %s", ifelse(!is.null(mss_results), paste(if (!is.null(mss_results$standard_validation)) names(mss_results$standard_validation) else names(mss_results$standard_results), collapse = ", "), "N/A"))),
        md_bullet(sprintf("Competing risk analysis: %s", ifelse(!is.null(mss_results) && (!is.null(mss_results$competing_risk_validation) || !is.null(mss_results$competing_results)), "Yes", "No"))),
        "",
        md_heading("Performance Summary", 2L),
        "",
        md_heading("Calibration Performance", 3L),
        md_bullet("Calibration slope close to 1.0 indicates good calibration."),
        md_bullet("Integrated Calibration Index (ICI) measures overall calibration."),
        "",
        md_heading("Discrimination Performance", 3L),
        md_bullet("Harrell's C-index > 0.7 indicates good discrimination."),
        md_bullet("Integrated AUC provides robust discrimination over time periods."),
        "",
        md_bullet("All detailed results are saved as Excel tables and visualizations."),
        md_bullet("See individual files for complete statistical outputs.")
    )
    summary_path <- file.path(output_dir, paste0(prefix, "gep_comprehensive_report.md"))
    writeLines(summary_lines, summary_path)
    if (nrow(comparison_table) > 0) {
        excel_path <- file.path(output_dir, paste0(prefix, "gep_comparison_table.xlsx"))
        write_gep_workbook(comparison_table, excel_path)
    }
    logger::log_info(sprintf("Comprehensive GEP report saved: %s", summary_path))
}

#' Create Comprehensive GEP Validation Summary
    #'
    #' Combine the outcome-specific comprehensive summaries into a single narrative
    #' document spanning both MFS and MSS analyses.
    #'
    #' @param mfs_results MFS validation result object.
    #' @param mss_results MSS validation result object.
    #' @param prefix Filename prefix retained for interface consistency.
    #' @return Character scalar containing the consolidated summary text.
create_comprehensive_gep_validation_summary <- function(mfs_results, mss_results, prefix) {
    dataset_name <- mfs_results$dataset_name %||% mss_results$dataset_name %||% "dataset_not_provided"
    summary_lines <- c(
        md_heading("Comprehensive GEP Validation Summary", 1L),
        sprintf("Analysis completed: %s", Sys.time()),
        "",
        md_heading("Overview", 2L),
        md_bullet(sprintf("Dataset: %s", dataset_name)),
        md_bullet("This consolidated summary replaces multiple separate reports and eliminates redundant information across timepoints while maintaining all statistical details."),
        "",
        create_gep_prediction_source_note(),
        "",
        md_heading("Metastasis-Free Survival (MFS) Analysis", 2L)
    )
    
    if (!is.null(mfs_results) && !is.null(mfs_results$validation_results)) {
        mfs_summary <- create_comprehensive_gep_summary(
            validation_results = mfs_results$validation_results,
            outcome_type = "MFS",
            prame_analysis = mfs_results$prame_analysis,
            missing_data_analysis = mfs_results$missing_data_analysis,
            dataset_name = mfs_results$dataset_name %||% dataset_name,
            include_prediction_source_note = FALSE,
            source_data = mfs_results$source_data
        )
        summary_lines <- c(summary_lines, "", mfs_summary)
    } else {
        summary_lines <- c(summary_lines, "MFS analysis results not available")
    }

    summary_lines <- c(summary_lines, "", md_heading("Melanoma-Specific Survival (MSS) Analysis", 2L))
    
    if (!is.null(mss_results)) {
        mss_container <- if (!is.null(mss_results$standard_validation)) mss_results$standard_validation else mss_results$standard_results
        if (!is.null(mss_container)) {
            mss_summary <- create_comprehensive_gep_summary(
                validation_results = mss_container,
                outcome_type = "MSS",
                prame_analysis = mss_results$prame_results,
                missing_data_analysis = mss_results$missing_data_analysis,
                dataset_name = mss_results$dataset_name %||% dataset_name,
                include_prediction_source_note = FALSE,
                source_data = mss_results$source_data
            )
            summary_lines <- c(summary_lines, "", mss_summary)
        } else {
            summary_lines <- c(summary_lines, "MSS analysis results not available")
        }
    } else {
        summary_lines <- c(summary_lines, "MSS analysis results not available")
    }
    
    summary_lines <- c(summary_lines, "",
        md_heading("Consolidation Benefits", 2L),
        md_bullet("Calibration is summarized as one full-spectrum curve per outcome, faceted by timepoint."),
        md_bullet("Redundant decision curve plots are eliminated in favor of one comprehensive table."),
        md_bullet("Redundant performance plots are eliminated in favor of one comprehensive table."),
        md_bullet("Unnecessary subfolder structure is removed."),
        md_bullet("All information is consolidated into interpretable clinical summaries."),
        md_bullet("All statistical information is retained while readability improves."),
        "",
        md_bullet("This consolidated approach provides the same information as the previous scattered outputs but in a more organized, interpretable format.")
    )
    
    return(paste(summary_lines, collapse = "\n"))
}

#' Create Clinical Interpretation Summary
#'
#' Generate a clinician-facing narrative overview of how to interpret GEP
#' validation results and apply them in practice.
#'
#' @param mfs_results MFS validation result object.
#' @param mss_results MSS validation result object.
#' @param prefix Filename prefix retained for interface consistency.
#' @return Character scalar containing the interpretation summary.
create_clinical_interpretation_summary <- function(mfs_results, mss_results, prefix) {
    clinical_lines <- c(
        md_heading("GEP Validation - Clinical Interpretation Summary", 1L),
        sprintf("Generated: %s", Sys.time()),
        "",
        md_heading("Purpose", 2L),
        md_bullet("This summary provides clinical interpretation of GEP validation results to guide clinical decision-making and patient counseling."),
        "",
        create_gep_prediction_source_note(),
        "",
        md_heading("Key Clinical Insights", 2L),
        "",
        md_heading("Model Performance Assessment", 3L),
        md_bullet("Discrimination: How well the model separates high and low-risk patients", indent = 1L),
        md_bullet("Calibration: How accurate the model's risk predictions are", indent = 1L),
        md_bullet("Clinical utility: Whether the model provides actionable information", indent = 1L),
        "",
        md_heading("Timepoint Analysis", 3L),
        md_bullet("Short-term (5yr): Immediate risk assessment and treatment planning", indent = 1L),
        md_bullet("Medium-term (7yr): Intermediate surveillance and intervention decisions", indent = 1L),
        md_bullet("Long-term (10yr): Long-term prognosis and patient counseling", indent = 1L),
        "",
        md_heading("Clinical Applications", 3L),
        md_bullet("Risk stratification: Identifying patients for different surveillance intensities", indent = 1L),
        md_bullet("Treatment decisions: Guiding adjuvant therapy and follow-up protocols", indent = 1L),
        md_bullet("Patient counseling: Providing accurate prognostic information", indent = 1L),
        md_bullet("Research applications: Supporting clinical trial design and analysis", indent = 1L),
        "",
        md_heading("Interpretation Guidelines", 3L),
        md_bullet("Excellent discrimination (C-index >= 0.9): Model provides strong prognostic information", indent = 1L),
        md_bullet("Good discrimination (C-index >= 0.8): Model provides reliable prognostic information", indent = 1L),
        md_bullet("Good calibration (slope approximately 1.0): Risk estimates can be used directly", indent = 1L),
        md_bullet("Moderate calibration: Risk estimates should be interpreted with caution", indent = 1L),
        "",
        md_heading("Limitations and Considerations", 3L),
        md_bullet("Model performance may vary across different patient populations", indent = 1L),
        md_bullet("Clinical context should always be considered alongside model predictions", indent = 1L),
        md_bullet("Regular model validation is recommended as practice patterns evolve", indent = 1L),
        "",
        md_bullet("For detailed statistical results, refer to the comprehensive validation summary and consolidated Excel workbook in this directory.")
    )
    
    return(paste(clinical_lines, collapse = "\n"))
}

#' Create consolidated GEP Excel workbook directly in unified directory
#'
#' Creates a consolidated Excel workbook with all GEP validation results
#' directly in the unified_summary directory to eliminate redundancy.
#'
#' @param mfs_results MFS validation results
#' @param mss_results MSS validation results
#' @param unified_dir Path to unified summary directory
#' @param prefix Filename prefix
#' @return List of paths to consolidated output files
create_consolidated_gep_excel_workbook_unified <- function(mfs_results, mss_results, unified_dir, prefix) {
    logger::log_info(formatted("Creating consolidated GEP Excel workbook in unified directory", indent = 1))
    
    consolidated_files <- list()
    
    # 1. Create comprehensive validation summary (replaces separate MFS/MSS text files)
    comprehensive_summary <- create_comprehensive_gep_validation_summary(
        mfs_results = mfs_results,
        mss_results = mss_results,
        prefix = prefix
    )
    
    summary_path <- file.path(unified_dir, paste0(prefix, "comprehensive_gep_validation_summary.md"))
    writeLines(comprehensive_summary, summary_path)
    consolidated_files$comprehensive_summary <- summary_path
    
    # 2. Create simplified Excel workbook (replaces multiple separate Excel files)
    simplified_excel <- create_consolidated_gep_excel_workbook(
        mfs_results = mfs_results,
        mss_results = mss_results,
        prefix = prefix
    )
    
    excel_path <- file.path(unified_dir, paste0(prefix, "consolidated_gep_validation.xlsx"))
    write_gep_workbook(simplified_excel, excel_path)
    consolidated_files$consolidated_excel <- excel_path
    
    # 3. Create clinical interpretation summary (replaces scattered information)
    clinical_summary <- create_clinical_interpretation_summary(
        mfs_results = mfs_results,
        mss_results = mss_results,
        prefix = prefix
    )
    
    clinical_path <- file.path(unified_dir, paste0(prefix, "clinical_interpretation_summary.md"))
    writeLines(clinical_summary, clinical_path)
    consolidated_files$clinical_summary <- clinical_path
    
    logger::log_info(sprintf("Consolidated outputs created in: %s", unified_dir))
    logger::log_info(sprintf("Files created: %s", paste(names(consolidated_files), collapse = ", ")))
    
    return(consolidated_files)
}
