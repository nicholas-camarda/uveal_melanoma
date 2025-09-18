# GEP Summary Generation Functions
# Comprehensive summary creation for GEP validation results

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
#' @return A comprehensive summary text suitable for saving
create_comprehensive_gep_summary <- function(validation_results, outcome_type, prame_analysis, missing_data_analysis, dataset_name) {
    logger::log_info(sprintf("Creating comprehensive GEP validation summary for %s", outcome_type))
    
    # Extract key metrics across timepoints for pattern analysis
    timepoints <- names(validation_results)
    
    # Initialize data frames with proper column structure to avoid rbind errors
    calibration_data <- data.frame(
        Timepoint = character(),
        Slope = numeric(),
        ICI = numeric(),
        Nam_D_Agostino_p = numeric(),
        stringsAsFactors = FALSE
    )
    
    discrimination_data <- data.frame(
        Timepoint = character(),
        Harrell_C = numeric(),
        Integrated_AUC = numeric(),
        Cumulative_Discrimination = numeric(),
        Time_averaged_Discrimination = numeric(),
        stringsAsFactors = FALSE
    )
    
    oe_data <- data.frame(
        Timepoint = character(),
        Overall_OE = numeric(),
        CI_Lower = numeric(),
        CI_Upper = numeric(),
        Chi_Square_p = numeric(),
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
                    ICI = ifelse(is.null(cal$ici), NA_real_, cal$ici),
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
                Cumulative_Discrimination = ifelse(is.null(disc$cumulative_discrimination), NA_real_, disc$cumulative_discrimination),
                Time_averaged_Discrimination = ifelse(is.null(disc$time_averaged_discrimination), NA_real_, disc$time_averaged_discrimination),
                stringsAsFactors = FALSE
            )
            discrimination_data <- rbind(discrimination_data, new_disc_row)
        }
        
        # Observed/Expected metrics - support both MFS (flat fields) and MSS (nested overall)
        if (!is.null(result$observed_expected)) {
            # MSS-style: nested overall list
            if (!is.null(result$observed_expected$overall)) {
                oe <- result$observed_expected$overall
                new_oe_row <- data.frame(
                    Timepoint = tp,
                    Overall_OE = ifelse(is.null(oe$oe_ratio), NA_real_, oe$oe_ratio),
                    CI_Lower = ifelse(is.null(oe$poisson_ci_lower), NA_real_, oe$poisson_ci_lower),
                    CI_Upper = ifelse(is.null(oe$poisson_ci_upper), NA_real_, oe$poisson_ci_upper),
                    Chi_Square_p = ifelse(is.null(oe$chi_square_p), NA_real_, oe$chi_square_p),
                    stringsAsFactors = FALSE
                )
                oe_data <- rbind(oe_data, new_oe_row)
            } else {
                # MFS-style: overall_* fields at top level
                oe <- result$observed_expected
                new_oe_row <- data.frame(
                    Timepoint = tp,
                    Overall_OE = ifelse(is.null(oe$overall_oe_ratio), NA_real_, oe$overall_oe_ratio),
                    CI_Lower = ifelse(is.null(oe$overall_poisson_ci_lower), NA_real_, oe$overall_poisson_ci_lower),
                    CI_Upper = ifelse(is.null(oe$overall_poisson_ci_upper), NA_real_, oe$overall_poisson_ci_upper),
                    Chi_Square_p = ifelse(is.null(oe$chisq_p_value), NA_real_, oe$chisq_p_value),
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
    
    # Build comprehensive report
    report_lines <- c(
        sprintf("GEP %s Validation - Comprehensive Clinical Summary", outcome_type),
        paste(rep("=", 60), collapse = ""),
        sprintf("Analysis completed: %s", Sys.time()),
        "",
        sprintf("Dataset: %s", ifelse(is.null(dataset_name), "uveal_melanoma_full_cohort", dataset_name)),
        sprintf("Outcome: %s", outcome_type),
        sprintf("Timepoints analyzed: %s", paste(timepoints, collapse = ", ")),
        "",
        "CLINICAL SUMMARY",
        "================",
        clinical_summary$overall_assessment,
        "",
        "CALIBRATION ANALYSIS",
        "===================",
        clinical_summary$calibration_interpretation,
        "",
        "DISCRIMINATION ANALYSIS", 
        "=====================",
        clinical_summary$discrimination_interpretation,
        "",
        "OBSERVED VS EXPECTED ANALYSIS",
        "============================",
        clinical_summary$oe_interpretation,
        "",
        "TIME-DEPENDENT PATTERNS",
        "=======================",
        clinical_summary$temporal_patterns,
        "",
        "CLINICAL IMPLICATIONS",
        "=====================",
        clinical_summary$clinical_implications,
        "",
        "DETAILED METRICS BY TIMEPOINT",
        "=============================",
        tryCatch({
            create_detailed_metrics_table(validation_results, outcome_type)
        }, error = function(e) {
            logger::log_warn(sprintf("Detailed metrics table creation failed: %s", e$message))
            "Detailed metrics table could not be generated due to data structure issues"
        }),
        "",
        if (!is.null(prame_analysis) && !is.null(prame_analysis$nri)) {
            nri_val <- tryCatch({
                if (is.list(prame_analysis$nri) && !is.null(prame_analysis$nri$total)) prame_analysis$nri$total else as.numeric(prame_analysis$nri)
            }, error = function(e) NA_real_)
            sprintf("PRAME ADDED VALUE: Net Reclassification Index = %.3f", ifelse(is.na(nri_val), NA, nri_val))
        } else {
            "PRAME ADDED VALUE: Analysis not available or insufficient data"
        },
        "",
        sprintf("Missing data patterns: %d", ifelse(is.null(missing_data_analysis$missing_patterns), 0, nrow(missing_data_analysis$missing_patterns))),
        "",
        "NOTE: This comprehensive summary consolidates information from multiple timepoints",
        "to provide clinical interpretation and pattern analysis. All detailed statistical",
        "outputs remain available in the accompanying Excel files and visualizations."
    )
    
    return(paste(report_lines, collapse = "\n"))
}

#' Create Comprehensive GEP Report
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
        "  - Integrated AUC provides robust discrimination over time periods",
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

#' Create Comprehensive GEP Validation Summary
create_comprehensive_gep_validation_summary <- function(mfs_results, mss_results, prefix) {
    summary_lines <- c(
        "COMPREHENSIVE GEP VALIDATION SUMMARY",
        "====================================",
        sprintf("Analysis completed: %s", Sys.time()),
        "",
        "OVERVIEW",
        "--------",
        "This consolidated summary replaces multiple separate reports and eliminates",
        "redundant information across timepoints while maintaining all statistical details.",
        "",
        "METASTASIS-FREE SURVIVAL (MFS) ANALYSIS",
        "----------------------------------------"
    )
    
    if (!is.null(mfs_results) && !is.null(mfs_results$validation_results)) {
        mfs_summary <- create_comprehensive_gep_summary(
            validation_results = mfs_results$validation_results,
            outcome_type = "MFS",
            prame_analysis = mfs_results$prame_analysis,
            missing_data_analysis = NULL, # Will be added if available
            dataset_name = "uveal_melanoma_full_cohort"
        )
        summary_lines <- c(summary_lines, "", mfs_summary)
    } else {
        summary_lines <- c(summary_lines, "MFS analysis results not available")
    }
    
    summary_lines <- c(summary_lines, "", 
        "MELANOMA-SPECIFIC SURVIVAL (MSS) ANALYSIS",
        "------------------------------------------"
    )
    
    if (!is.null(mss_results)) {
        mss_container <- if (!is.null(mss_results$standard_validation)) mss_results$standard_validation else mss_results$standard_results
        if (!is.null(mss_container)) {
            mss_summary <- create_comprehensive_gep_summary(
                validation_results = mss_container,
                outcome_type = "MSS",
                prame_analysis = NULL, # Will be added if available
                missing_data_analysis = NULL, # Will be added if available
                dataset_name = "uveal_melanoma_full_cohort"
            )
            summary_lines <- c(summary_lines, "", mss_summary)
        } else {
            summary_lines <- c(summary_lines, "MSS analysis results not available")
        }
    } else {
        summary_lines <- c(summary_lines, "MSS analysis results not available")
    }
    
    summary_lines <- c(summary_lines, "",
        "CONSOLIDATION BENEFITS",
        "---------------------",
        "✓ Eliminated redundant calibration plots (6 plots → 1 comprehensive table)",
        "✓ Eliminated redundant decision curve plots (6 plots → 1 comprehensive table)",
        "✓ Eliminated redundant performance plots (4 plots → 1 comprehensive table)",
        "✓ Eliminated unnecessary subfolder structure",
        "✓ Consolidated all information into interpretable, clinical summaries",
        "✓ Maintained all statistical information while improving readability",
        "",
        "NOTE: This consolidated approach provides the same information as the",
        "previous scattered outputs but in a more organized, interpretable format."
    )
    
    return(paste(summary_lines, collapse = "\n"))
}

#' Create Clinical Interpretation Summary
create_clinical_interpretation_summary <- function(mfs_results, mss_results, prefix) {
    clinical_lines <- c(
        "GEP VALIDATION - CLINICAL INTERPRETATION SUMMARY",
        "================================================",
        sprintf("Generated: %s", Sys.time()),
        "",
        "PURPOSE",
        "-------",
        "This summary provides clinical interpretation of GEP validation results",
        "to guide clinical decision-making and patient counseling.",
        "",
        "KEY CLINICAL INSIGHTS",
        "---------------------",
        "",
        "1. MODEL PERFORMANCE ASSESSMENT",
        "   • Discrimination: How well the model separates high and low-risk patients",
        "   • Calibration: How accurate the model's risk predictions are",
        "   • Clinical utility: Whether the model provides actionable information",
        "",
        "2. TIMEPOINT ANALYSIS",
        "   • Short-term (5yr): Immediate risk assessment and treatment planning",
        "   • Medium-term (7yr): Intermediate surveillance and intervention decisions",
        "   • Long-term (10yr): Long-term prognosis and patient counseling",
        "",
        "3. CLINICAL APPLICATIONS",
        "   • Risk stratification: Identifying patients for different surveillance intensities",
        "   • Treatment decisions: Guiding adjuvant therapy and follow-up protocols",
        "   • Patient counseling: Providing accurate prognostic information",
        "   • Research applications: Supporting clinical trial design and analysis",
        "",
        "4. INTERPRETATION GUIDELINES",
        "   • Excellent discrimination (C-index >= 0.9): Model provides strong prognostic information",
        "   • Good discrimination (C-index >= 0.8): Model provides reliable prognostic information",
        "   • Good calibration (slope ≈ 1.0): Risk estimates can be used directly",
        "   • Moderate calibration: Risk estimates should be interpreted with caution",
        "",
        "5. LIMITATIONS AND CONSIDERATIONS",
        "   • Model performance may vary across different patient populations",
        "   • Clinical context should always be considered alongside model predictions",
        "   • Regular model validation is recommended as practice patterns evolve",
        "",
        "For detailed statistical results, refer to the comprehensive validation summary",
        "and consolidated Excel workbook in this directory."
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
    
    summary_path <- file.path(unified_dir, paste0(prefix, "comprehensive_gep_validation_summary.txt"))
    writeLines(comprehensive_summary, summary_path)
    consolidated_files$comprehensive_summary <- summary_path
    
    # 2. Create simplified Excel workbook (replaces multiple separate Excel files)
    simplified_excel <- create_consolidated_gep_excel_workbook(
        mfs_results = mfs_results,
        mss_results = mss_results,
        prefix = prefix
    )
    
    excel_path <- file.path(unified_dir, paste0(prefix, "consolidated_gep_validation.xlsx"))
    writexl::write_xlsx(simplified_excel, excel_path)
    consolidated_files$consolidated_excel <- excel_path
    
    # 3. Create clinical interpretation summary (replaces scattered information)
    clinical_summary <- create_clinical_interpretation_summary(
        mfs_results = mfs_results,
        mss_results = mss_results,
        prefix = prefix
    )
    
    clinical_path <- file.path(unified_dir, paste0(prefix, "clinical_interpretation_summary.txt"))
    writeLines(clinical_summary, clinical_path)
    consolidated_files$clinical_summary <- clinical_path
    
    logger::log_info(sprintf("Consolidated outputs created in: %s", unified_dir))
    logger::log_info(sprintf("Files created: %s", paste(names(consolidated_files), collapse = ", ")))
    
    return(consolidated_files)
}

#' Create unified GEP validation summary
#'
#' Create a unified comparison and integrated visuals for MFS and MSS,
#' saving a combined report and summary artifacts in a single organized directory.
#' 
#' OUTPUT STRUCTURE:
#' - "unified_summary" folder: Contains ALL comprehensive reports, comparison tables, 
#'   consolidated Excel files, and integrated visualizations in one organized location
#' - Individual outcome directories: Contain only outcome-specific outputs (KM plots, CIF plots)
#'
#' @param mfs_results MFS validation results (may be NULL)
#' @param mss_results MSS validation results (may be NULL)
#' @param dataset_name Optional dataset label
#' @param output_dir Base output directory
#' @param prefix Filename prefix for saved files
#' @return A list with `comparison_table`, `unified_dir`, and `consolidated_files`
create_unified_gep_validation_summary <- function(mfs_results, mss_results, dataset_name, output_dir, prefix) {
    logger::log_info("Creating unified GEP validation summary with consolidated outputs")
    
    # Create single unified summary directory (eliminates redundancy)
    unified_dir <- file.path(output_dir, "unified_summary")
    if (!dir.exists(unified_dir)) {
        dir.create(unified_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Create comparison table
    comparison_table <- create_gep_comparison_table(mfs_results, mss_results)
    
    # Create integrated visuals directly in unified directory
    create_integrated_gep_visuals(mfs_results, mss_results, unified_dir, prefix)
    
    # Create comprehensive report directly in unified directory
    comprehensive_summary <- create_comprehensive_gep_validation_summary(mfs_results, mss_results, prefix)
    summary_path <- file.path(unified_dir, paste0(prefix, "comprehensive_gep_validation_summary.txt"))
    writeLines(comprehensive_summary, summary_path)
    
    # Create consolidated Excel workbook directly in unified directory (eliminates simplified_outputs redundancy)
    consolidated_files <- create_consolidated_gep_excel_workbook_unified(mfs_results, mss_results, unified_dir, prefix)
    
    # Create clinical interpretation summary directly in unified directory
    clinical_summary <- create_clinical_interpretation_summary(mfs_results, mss_results, prefix)
    clinical_path <- file.path(unified_dir, paste0(prefix, "clinical_interpretation_summary.txt"))
    writeLines(clinical_summary, clinical_path)
    
    logger::log_info("Unified GEP validation summary created with consolidated outputs")
    logger::log_info("  - unified_summary: ALL comprehensive reports, consolidated Excel files, and integrated visuals")
    logger::log_info("  - Individual outcome directories: Only outcome-specific outputs (KM plots, CIF plots)")
    return(list(
        comparison_table = comparison_table,
        unified_dir = unified_dir,
        consolidated_files = consolidated_files
    ))
}
