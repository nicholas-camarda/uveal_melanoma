# GEP Core Reporting Functions
# Core reporting functions and main orchestration for GEP validation results

#' Save All MFS Validation Results
#'
#' Persist MFS validation artifacts to disk, including comprehensive summaries (xlsx), RDS
#' objects, and consolidated outputs.
#'
#' @param validation_results Named list of per-timepoint MFS results
#' @param missing_data_analysis Missing-data diagnostics results
#' @param prame_analysis PRAME-augmented analysis results (may be NULL)
#' @param extrapolation_assessment Objective 4 extrapolation-support summary for
#'   later horizons.
#' @param source_data Optional analytic dataset used for compact follow-up
#'   limitation summaries.
#' @param output_dir Directory path to save artifacts
#' @param prefix Filename prefix for saved files
#' @param dataset_name Optional dataset label for saved narrative summaries
#' @return Invisibly returns NULL after writing files
save_mfs_validation_results <- function(validation_results,
                                        missing_data_analysis,
                                        prame_analysis,
                                        extrapolation_assessment,
                                        source_data = NULL,
                                        output_dir,
                                        prefix,
                                        dataset_name = NULL) {
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

            unified_sheets <- list()
            # The technical workbook keeps only lower-level detail that is not already
            # summarized in the consolidated review-facing workbook.
            if (nrow(oe_summary) > 0) unified_sheets[["Observed_Expected_by_class"]] <- oe_summary
            if (length(unified_sheets) > 0) {
                write_gep_workbook(unified_sheets, file.path(output_dir, paste0(prefix, "mfs_validation_technical_details.xlsx")))
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

    # Create comprehensive, interpretable summary instead of repetitive per-timepoint approach
    comprehensive_summary <- create_comprehensive_gep_summary(
        validation_results = validation_results,
        outcome_type = "MFS",
        prame_analysis = prame_analysis,
        missing_data_analysis = missing_data_analysis,
        dataset_name = dataset_name,
        extrapolation_assessment = extrapolation_assessment,
        source_data = source_data
    )
    
    summary_path <- file.path(output_dir, paste0(prefix, "mfs_validation_narrative_summary.txt"))
    writeLines(comprehensive_summary, summary_path)
    
    # Create consolidated tables to replace redundant visualizations
    consolidated_tables <- create_consolidated_gep_tables(
        validation_results = validation_results,
        outcome_type = "MFS",
        output_dir = output_dir,
        prefix = prefix,
        prame_results = prame_analysis,
        missing_data = missing_data_analysis,
        extrapolation_assessment = extrapolation_assessment
    )
    
    logger::log_info(sprintf("MFS validation summary saved: %s", summary_path))
    logger::log_info("MFS validation results saved successfully")
    logger::log_info(sprintf("Consolidated tables created: %s", paste(names(consolidated_tables), collapse = ", ")))
    
    # REMOVED: Simplified outputs directory creation to eliminate redundancy
    # simplified_dir <- file.path(output_dir, "simplified_outputs")
    # if (!dir.exists(simplified_dir)) {
    #     dir.create(simplified_dir, recursive = TRUE, showWarnings = FALSE)
    # }
    
    # REMOVED: Duplicate consolidated summary saving to eliminate redundancy
    # simplified_summary_path <- file.path(simplified_dir, paste0(prefix, "mfs_consolidated_summary.txt"))
    # writeLines(consolidated_tables$text_summary, simplified_summary_path)
    # logger::log_info(formatted("Simplified MFS summary saved: %s", simplified_summary_path), indent = 2)
}

#' Save MSS validation results
#'
#' Persist MSS validation artifacts to disk, including per-timepoint sheets for
#' observed/expected, calibration, discrimination, and competing risks.
#'
#' @param standard_results Named list of standard MSS results (per timepoint)
#' @param competing_results Named list of competing risk MSS results (per timepoint)
#' @param missing_data Missing-data diagnostics results (may be NULL)
#' @param prame_results PRAME-augmented MSS analysis results (may be NULL)
#' @param extrapolation_assessment Objective 4 extrapolation-support summary for
#'   later horizons.
#' @param source_data Optional analytic dataset used for compact follow-up
#'   limitation summaries.
#' @param output_dir Directory path to save artifacts
#' @param prefix Filename prefix for saved files
#' @param group_var Variable name for grouping (default: "biopsy1_gep")
#' @param dataset_name Optional dataset label for saved narrative summaries
#' @return Invisibly returns NULL after writing files
save_mss_validation_results <- function(standard_results, competing_results,
                                        missing_data, prame_results, extrapolation_assessment, source_data = NULL, output_dir, prefix,
                                        group_var = get_gep_grouping_for_context("mss", "reporting")$var, dataset_name = NULL) {
    logger::log_info("Saving MSS validation results")
    if (GEP_SAVE_RDS) {
        saveRDS(standard_results, file.path(output_dir, paste0(prefix, "mss_standard_validation_results.rds")))
        saveRDS(competing_results, file.path(output_dir, paste0(prefix, "mss_competing_risk_results.rds")))
        if (!is.null(missing_data)) {
            saveRDS(missing_data, file.path(output_dir, paste0(prefix, "mss_missing_data_analysis.rds")))
        }
        if (!is.null(prame_results)) {
            saveRDS(prame_results, file.path(output_dir, paste0(prefix, "mss_prame_analysis.rds")))
        }
    }
    create_mss_validation_excel_files(
        standard_results = standard_results,
        competing_results = competing_results,
        missing_data = missing_data,
        prame_results = prame_results,
        output_dir = output_dir,
        prefix = prefix,
        group_var = group_var
    )
    create_mss_validation_summary_text(
        standard_results = standard_results,
        competing_results = competing_results,
        missing_data = missing_data,
        prame_results = prame_results,
        extrapolation_assessment = extrapolation_assessment,
        source_data = source_data,
        output_dir = output_dir,
        prefix = prefix,
        group_var = group_var,
        dataset_name = dataset_name
    )
}

#' Create MSS validation Excel files
create_mss_validation_excel_files <- function(standard_results, competing_results,
                                              missing_data, prame_results, output_dir, prefix,
                                              group_var = get_gep_grouping_for_context("mss", "reporting")$var) {
    logger::log_info("Creating MSS validation Excel files")
    excel_sheets <- list()
    # Summary statistics now generated by comprehensive summary system

    # The technical workbook keeps detail tables only; summary calibration and
    # discrimination tables now live in the consolidated workbook.
    obs_exp_df <- data.frame()
    counts_df <- data.frame()
    cif_ci_df <- data.frame()

    for (tp_name in names(standard_results)) {
        tp_results <- standard_results[[tp_name]]
        # Observed/Expected by class using available fields
        if (!is.null(tp_results$observed_expected)) {
            oe <- tp_results$observed_expected
            if (is.data.frame(oe)) {
                # Accept either counts or rates input; compute rates if needed
                if (all(c(group_var, "n") %in% names(oe))) {
                    keep_cols <- intersect(names(oe), c(group_var, "n", "observed", "expected", "expected_rate", "observed_rate"))
                    tmp_counts <- oe[, keep_cols]
                    names(tmp_counts)[names(tmp_counts) == group_var] <- "GEP_Class"
                    tmp_counts$Timepoint <- tp_name
                    counts_df <- rbind(counts_df, tmp_counts)
                }
                if (all(c("expected", "observed", "n", group_var) %in% names(oe))) {
                    tmp <- data.frame(
                        Timepoint = tp_name,
                        GEP_Class = oe[[group_var]],
                        N = oe$n,
                        Expected = oe$expected,
                        Observed = oe$observed,
                        OE_Ratio = ifelse(oe$expected > 0, oe$observed / oe$expected, NA_real_),
                        stringsAsFactors = FALSE
                    )
                    obs_exp_df <- rbind(obs_exp_df, tmp)
                } else if (all(c("expected_rate", "observed_rate", "n", group_var) %in% names(oe))) {
                    tmp <- data.frame(
                        Timepoint = tp_name,
                        GEP_Class = oe[[group_var]],
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
    }

    if (nrow(obs_exp_df) > 0) excel_sheets[["Observed_Expected_by_class"]] <- obs_exp_df
    if (nrow(counts_df) > 0) excel_sheets[["Counts"]] <- counts_df

    if (!is.null(competing_results)) {
        # Stack competing risks tables with a Timepoint column
        ci_df <- data.frame()
        csh_df <- data.frame()
        feasibility_df <- data.frame()
        for (tp_name in names(competing_results)) {
            tp_results <- competing_results[[tp_name]]
            if (!is.null(tp_results$cumulative_incidence)) {
                tmp <- tp_results$cumulative_incidence
                tmp$Timepoint <- tp_name
                # Standardize class column deterministically using group_var parameter
                tmp$GEP_Class <- tmp[[group_var]]
                tmp <- tmp[, c("GEP_Class", setdiff(names(tmp), c(group_var, "GEP_Class"))), drop = FALSE]
                ci_df <- rbind(ci_df, tmp)
            }
            # Cause-specific Cox model (CSC proxy)
            if (!is.null(tp_results$cause_specific_cox)) {
                tmp <- tp_results$cause_specific_cox
                tmp$Timepoint <- tp_name
                names(tmp) <- c("GEP_Class", "HR", "CI_Lower", "CI_Upper", "p_value", "reference", "Timepoint")
                csh_df <- rbind(csh_df, tmp)
            }
            # Fine-Gray subdistribution model
            if (!is.null(tp_results$fine_gray)) {
                fg_tmp <- tp_results$fine_gray
                fg_tmp$Timepoint <- tp_name
                if (!"GEP_Class" %in% names(fg_tmp)) names(fg_tmp)[1] <- "GEP_Class"
                excel_sheets[["CompetingRisk_FineGray"]] <- rbind(excel_sheets[["CompetingRisk_FineGray"]] %||% data.frame(), fg_tmp)
            }
            if (!is.null(tp_results$cif_with_ci)) {
                tmp <- tp_results$cif_with_ci
                tmp$Timepoint <- tp_name
                cif_ci_df <- rbind(cif_ci_df, tmp)
            }
            if (!is.null(tp_results$feasibility)) {
                if (!is.null(tp_results$feasibility$by_group) && nrow(tp_results$feasibility$by_group) > 0) {
                    group_tmp <- tp_results$feasibility$by_group
                    group_tmp$Timepoint <- tp_name
                    group_tmp$CSC_Status <- tp_results$feasibility$models$cause_specific_cox$status %||% NA_character_
                    group_tmp$CSC_Reason <- tp_results$feasibility$models$cause_specific_cox$reason %||% NA_character_
                    group_tmp$FineGray_Status <- tp_results$feasibility$models$fine_gray$status %||% NA_character_
                    group_tmp$FineGray_Reason <- tp_results$feasibility$models$fine_gray$reason %||% NA_character_
                    group_tmp$CIF_CI_Status <- tp_results$feasibility$models$cif_with_ci$status %||% NA_character_
                    group_tmp$CIF_CI_Reason <- tp_results$feasibility$models$cif_with_ci$reason %||% NA_character_
                    feasibility_df <- rbind(feasibility_df, group_tmp)
                } else {
                    feasibility_df <- rbind(
                        feasibility_df,
                        data.frame(
                            GEP_Class = NA_character_,
                            n = NA_integer_,
                            melanoma_deaths = NA_integer_,
                            competing_deaths = NA_integer_,
                            censored = NA_integer_,
                            zero_melanoma_deaths = NA,
                            zero_competing_deaths = NA,
                            below_minimum_size = NA,
                            Timepoint = tp_name,
                            CSC_Status = tp_results$feasibility$models$cause_specific_cox$status %||% NA_character_,
                            CSC_Reason = tp_results$feasibility$models$cause_specific_cox$reason %||% NA_character_,
                            FineGray_Status = tp_results$feasibility$models$fine_gray$status %||% NA_character_,
                            FineGray_Reason = tp_results$feasibility$models$fine_gray$reason %||% NA_character_,
                            CIF_CI_Status = tp_results$feasibility$models$cif_with_ci$status %||% NA_character_,
                            CIF_CI_Reason = tp_results$feasibility$models$cif_with_ci$reason %||% NA_character_,
                            stringsAsFactors = FALSE
                        )
                    )
                }
            }
        }
        if (nrow(ci_df) > 0) excel_sheets[["CompRisk_CIF"]] <- ci_df
        if (nrow(csh_df) > 0) excel_sheets[["CompRisk_CSC"]] <- csh_df
        if (nrow(cif_ci_df) > 0) excel_sheets[["CompRisk_CIF_with_CI"]] <- cif_ci_df
        if (nrow(feasibility_df) > 0) excel_sheets[["CompRisk_Feasibility"]] <- feasibility_df
    }
    excel_path <- file.path(output_dir, paste0(prefix, "mss_validation_technical_details.xlsx"))
    write_gep_workbook(excel_sheets, excel_path)
    logger::log_info(sprintf("MSS validation Excel file saved: %s", excel_path))
}

#' Create MSS validation summary text
#'
#' Render the narrative and consolidated workbook outputs for MSS validation.
#'
#' @param standard_results Named list of standard MSS validation results.
#' @param competing_results Named list of competing-risk MSS results.
#' @param missing_data Missing-data diagnostics results.
#' @param prame_results PRAME analysis results.
#' @param extrapolation_assessment Objective 4 extrapolation-support summary for
#'   later horizons.
#' @param source_data Optional analytic dataset used for compact follow-up
#'   limitation summaries.
#' @param output_dir Directory path to save artifacts.
#' @param prefix Character filename prefix for saved files.
#' @param group_var Character grouping variable retained for interface
#'   compatibility.
#' @param dataset_name Optional dataset label for narrative reporting.
#' @return Invisibly returns `NULL` after writing the outputs.
create_mss_validation_summary_text <- function(standard_results, competing_results,
                                               missing_data, prame_results, extrapolation_assessment, source_data = NULL, output_dir, prefix,
                                               group_var = get_gep_grouping_for_context("mss", "reporting")$var, dataset_name = NULL) {
    logger::log_info("Creating MSS validation summary text file")
    
    # Create comprehensive, interpretable summary instead of repetitive per-timepoint approach
    # Convert standard_results to the format expected by create_comprehensive_gep_summary
    validation_results <- standard_results
    
    comprehensive_summary <- create_comprehensive_gep_summary(
        validation_results = validation_results,
        outcome_type = "MSS",
        prame_analysis = prame_results,
        missing_data_analysis = missing_data,
        dataset_name = dataset_name,
        extrapolation_assessment = extrapolation_assessment,
        source_data = source_data
    )
    
    # Save comprehensive summary
    summary_path <- file.path(output_dir, paste0(prefix, "mss_validation_narrative_summary.txt"))
    writeLines(comprehensive_summary, summary_path)
    
    # Create consolidated tables to replace redundant visualizations
    consolidated_tables <- create_consolidated_gep_tables(
        validation_results = validation_results,
        outcome_type = "MSS",
        output_dir = output_dir,
        prefix = prefix,
        prame_results = prame_results,
        missing_data = missing_data,
        extrapolation_assessment = extrapolation_assessment
    )
    
    logger::log_info(sprintf("MSS validation summary saved: %s", summary_path))
    logger::log_info(sprintf("Consolidated tables created: %s", paste(names(consolidated_tables), collapse = ", ")))
    
    # REMOVED: Simplified outputs directory creation to eliminate redundancy
    # simplified_dir <- file.path(output_dir, "simplified_outputs")
    # if (!dir.exists(simplified_dir)) {
    #     dir.create(simplified_dir, recursive = TRUE, showWarnings = FALSE)
    # }
    
    # REMOVED: Duplicate consolidated summary saving to eliminate redundancy
    # simplified_summary_path <- file.path(simplified_dir, paste0(prefix, "mss_consolidated_summary.txt"))
    # writeLines(consolidated_tables$text_summary, simplified_summary_path)
    # logger::log_info(formatted("Simplified MSS summary saved: %s", simplified_summary_path), indent = 2)
}
