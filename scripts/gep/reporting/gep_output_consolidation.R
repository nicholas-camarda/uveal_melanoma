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
#' @param prame_results Optional PRAME analysis results object (may be NULL)
#' @param missing_data Optional missing-data analysis results object (may be NULL)
#' @param extrapolation_assessment Optional Objective 4 extrapolation-support
#'   summary for later horizons.
#' @return List of created table names
create_consolidated_gep_tables <- function(validation_results,
                                           outcome_type,
                                           output_dir,
                                           prefix,
                                           prame_results = NULL,
                                           missing_data = NULL,
                                           extrapolation_assessment = NULL) {
    logger::log_info(formatted(sprintf("Creating consolidated %s tables to replace redundant outputs", outcome_type), indent = 1))

    prame_note <- get_prame_availability_note(prame_results, sprintf("%s PRAME analysis", outcome_type))

    # Create consolidated observed/expected table across all timepoints
    oe_consolidated <- create_consolidated_oe_summary_table(validation_results, extrapolation_assessment = extrapolation_assessment)

    # Create consolidated calibration table across all timepoints
    cal_consolidated <- create_consolidated_calibration_table(validation_results, outcome_type, extrapolation_assessment = extrapolation_assessment)

    # Create consolidated discrimination table across all timepoints
    disc_consolidated <- create_consolidated_discrimination_table(validation_results, outcome_type, extrapolation_assessment = extrapolation_assessment)

    # REMOVED: Redundant performance table that duplicates discrimination metrics
    # perf_consolidated <- create_consolidated_performance_table(validation_results, outcome_type)

    # Create consolidated decision curve table across all timepoints (enriched)
    dca_consolidated <- create_consolidated_decision_curve_table(validation_results, outcome_type, extrapolation_assessment = extrapolation_assessment)
    extrapolation_consolidated <- create_consolidated_extrapolation_assumption_table(
        extrapolation_assessment = extrapolation_assessment,
        outcome_type = outcome_type
    )
    if (nrow(dca_consolidated) > 0) {
        # Add enriched columns when available
        if (!"Optimal_Threshold" %in% names(dca_consolidated) && "Net_Benefit_Threshold" %in% names(dca_consolidated)) {
            dca_consolidated$Optimal_Threshold <- dca_consolidated$Net_Benefit_Threshold
        }
    }

    # Build PRAME summary table using the incremental discrimination structure
    prame_consolidated <- data.frame()
    if (!is.null(prame_results) && !is.null(prame_results$comparison_results) && is.list(prame_results$comparison_results)) {
        rows <- lapply(names(prame_results$comparison_results), function(tp_name) {
            res <- prame_results$comparison_results[[tp_name]]
            data.frame(
                Timepoint = res$timepoint %||% tp_name,
                N = res$n %||% prame_results$n %||% NA,
                N_PRAME_Positive = res$n_positive %||% NA,
                N_PRAME_Negative = res$n_negative %||% NA,
                Events = res$events %||% NA,
                Events_PRAME_Positive = res$events_positive %||% NA,
                Events_PRAME_Negative = res$events_negative %||% NA,
                Non_Events = res$non_events %||% NA,
                Event_Rate_Pct = {
                    n_val <- res$n %||% prame_results$n %||% NA
                    e_val <- res$events %||% NA
                    if (is.numeric(n_val) && is.numeric(e_val) && !is.na(n_val) && !is.na(e_val) && n_val > 0)
                        round(100 * e_val / n_val, 1)
                    else NA_real_
                },
                Bootstrap_Valid_Resamples = res$bootstrap_valid_resamples %||% NA,
                Base_Harrell_C = suppressWarnings(as.numeric(res$base_harrell_c)),
                Enhanced_Harrell_C = suppressWarnings(as.numeric(res$enhanced_harrell_c)),
                Delta_Harrell_C = suppressWarnings(as.numeric(res$delta_harrell_c)),
                Delta_CI_Lower = suppressWarnings(as.numeric(res$delta_ci_lower)),
                Delta_CI_Upper = suppressWarnings(as.numeric(res$delta_ci_upper)),
                LR_p = suppressWarnings(as.numeric(res$lr_p)),
                PRAME_HR = suppressWarnings(as.numeric(res$prame_hr)),
                PRAME_HR_CI_Lower = suppressWarnings(as.numeric(res$prame_hr_ci_lower)),
                PRAME_HR_CI_Upper = suppressWarnings(as.numeric(res$prame_hr_ci_upper)),
                Analysis_Tier = res$analysis_tier %||% NA_character_,
                Interpretation = res$interpretation %||% "Analysis not supportable for this timepoint",
                stringsAsFactors = FALSE
            )
        })
        if (length(rows) > 0) {
            prame_consolidated <- do.call(rbind, rows)
        }

        if (nrow(prame_consolidated) > 0) {
            # Ensure deterministic column order for downstream Excel generation
            desired_cols <- c(
                "Timepoint", "N", "N_PRAME_Positive", "N_PRAME_Negative",
                "Events", "Events_PRAME_Positive", "Events_PRAME_Negative",
                "Non_Events", "Event_Rate_Pct", "Bootstrap_Valid_Resamples",
                "Base_Harrell_C", "Enhanced_Harrell_C", "Delta_Harrell_C",
                "Delta_CI_Lower", "Delta_CI_Upper", "LR_p",
                "PRAME_HR", "PRAME_HR_CI_Lower", "PRAME_HR_CI_Upper",
                "Analysis_Tier", "Interpretation"
            )
            missing_cols <- setdiff(desired_cols, names(prame_consolidated))
            if (length(missing_cols) > 0) {
                for (col in missing_cols) {
                    prame_consolidated[[col]] <- NA
                }
            }
            prame_consolidated <- prame_consolidated[, desired_cols, drop = FALSE]
        }
    }
        if (nrow(prame_consolidated) == 0) {
            prame_consolidated <- create_prame_placeholder_table(prame_note)
        }

    # Build Missing Data summary (compact and human-readable)
    missing_consolidated <- data.frame()
    if (!is.null(missing_data)) {
        # Patterns table if available
        patterns <- tryCatch(missing_data$missing_patterns, error = function(e) NULL)
        n_sig <- tryCatch(missing_data$baseline_comparison$n_significant, error = function(e) NA)
        logrank_p <- tryCatch(missing_data$outcome_by_missing$logrank_p, error = function(e) NA)
        n_imputable <- tryCatch(missing_data$imputation_analysis$n_imputable, error = function(e) NA)
        sanitize_value <- function(value) {
            if (is.null(value) || length(value) == 0) {
                return(NA)
            }
            value
        }
        missing_consolidated <- data.frame(
            Metric = c(
                "Total_Patients_n",
                "Missingness_Groups_n",
                "Baseline_Variables_with_Significant_Differences_n",
                "Survival_by_Missingness_Logrank_p",
                "Imputable_Patients_n"
            ),
            Value = c(
                sanitize_value(tryCatch(missing_data$n_total, error = function(e) NA)),
                sanitize_value(if (!is.null(patterns)) nrow(patterns) else NA),
                sanitize_value(n_sig),
                sanitize_value(logrank_p),
                sanitize_value(n_imputable)
            ),
            stringsAsFactors = FALSE
        )
    }

    # Combine all consolidated tables into a single Excel workbook
    consolidated_workbook <- list()
    if (nrow(oe_consolidated) > 0) consolidated_workbook[["Observed_Expected_Summary"]] <- oe_consolidated
    if (nrow(cal_consolidated) > 0) consolidated_workbook[["Calibration_Summary"]] <- cal_consolidated
    if (nrow(disc_consolidated) > 0) consolidated_workbook[["Discrimination_Summary"]] <- disc_consolidated
    # REMOVED: Redundant performance summary
    # if (nrow(perf_consolidated) > 0) consolidated_workbook[["Performance_Summary"]] <- perf_consolidated
    if (nrow(dca_consolidated) > 0) consolidated_workbook[["Decision_Curve_Summary"]] <- dca_consolidated
    if (nrow(extrapolation_consolidated) > 0) consolidated_workbook[["Extrapolation_Assumption_Checks"]] <- extrapolation_consolidated
    consolidated_workbook[["PRAME_Summary"]] <- prame_consolidated
    if (nrow(missing_consolidated) > 0) consolidated_workbook[["Missing_Data_Summary"]] <- missing_consolidated

    # Save consolidated workbook
    if (length(consolidated_workbook) > 0) {
        consolidated_path <- file.path(output_dir, paste0(prefix, outcome_type, "_consolidated_summary.xlsx"))
        writexl::write_xlsx(consolidated_workbook, consolidated_path)
        logger::log_info(formatted(sprintf("Consolidated %s tables saved: %s", outcome_type, consolidated_path), indent = 2))
    }

    # Create comprehensive text summary (but don't save to file to avoid redundancy)
    text_summary <- create_comprehensive_text_summary(
        validation_results = validation_results,
        outcome_type = outcome_type,
        cal_consolidated = cal_consolidated,
        disc_consolidated = disc_consolidated,
        dca_consolidated = dca_consolidated,
        extrapolation_consolidated = extrapolation_consolidated
    )

    # REMOVED: Text file generation to eliminate redundancy
    # text_path <- file.path(output_dir, paste0(prefix, outcome_type, "_consolidated_summary.txt"))
    # writeLines(text_summary, text_path)
    # logger::log_info(formatted(sprintf("Consolidated %s text summary saved: %s", outcome_type, text_path), indent = 2))

    return(list(
        observed_expected = oe_consolidated,
        calibration = cal_consolidated,
        discrimination = disc_consolidated,
        decision_curves = dca_consolidated,
        extrapolation_assumptions = extrapolation_consolidated,
        prame = prame_consolidated,
        missing_data = missing_consolidated,
        text_summary = text_summary
    ))
}

#' Create an overall observed-versus-expected summary table
#'
#' Collapse timepoint-specific observed/expected outputs into a single table with
#' overall counts, O/E ratios, confidence intervals, and goodness-of-fit
#' p-values.
#'
#' @param validation_results Named list of per-timepoint validation results.
#' @param extrapolation_assessment Optional Objective 4 extrapolation-support
#'   summary for later horizons.
#' @return Data frame with one row per timepoint.
create_consolidated_oe_summary_table <- function(validation_results, extrapolation_assessment = NULL) {
    oe_data <- data.frame()

    for (tp_name in names(validation_results)) {
        tp_results <- validation_results[[tp_name]]
        oe <- extract_overall_oe_metrics(tp_results$observed_expected)
        if (!is.null(oe)) {
            oe_data <- rbind(oe_data, data.frame(
                Timepoint = tp_name,
                N = oe$n %||% NA,
                Observed = oe$observed %||% NA,
                Expected = oe$expected %||% NA,
                OE_Ratio = oe$oe_ratio %||% NA,
                CI_Lower = oe$poisson_ci_lower %||% NA,
                CI_Upper = oe$poisson_ci_upper %||% NA,
                OE_Chi_Square_p = oe$chi_square_p %||% NA,
                stringsAsFactors = FALSE
            ))
        }
    }

    append_gep_extrapolation_metadata(oe_data, extrapolation_assessment = extrapolation_assessment)
}

#' Describe PRAME result availability
#'
#' Generate a human-readable status note explaining why PRAME results are
#' present, absent, or not supportable for a given context.
#'
#' @param prame_results PRAME result object, or `NULL` if no analysis was run.
#' @param context_label Character label used in the note.
#' @return Character scalar status note.
get_prame_availability_note <- function(prame_results, context_label = "PRAME analysis") {
    if (is.null(prame_results)) {
        return(sprintf("%s was not run for this output.", context_label))
    }

    if (!is.null(prame_results$error) && nzchar(prame_results$error)) {
        return(sprintf("%s was not supportable: %s", context_label, prame_results$error))
    }

    if (!is.null(prame_results$status) && nzchar(prame_results$status)) {
        return(sprintf("%s status: %s", context_label, prame_results$status))
    }

    if (!is.null(prame_results$prame_available) && identical(prame_results$prame_available, FALSE)) {
        return(sprintf("%s was not supportable for this cohort/outcome.", context_label))
    }

    sprintf("%s did not produce incremental comparison results for this cohort/outcome.", context_label)
}

#' Create a placeholder PRAME summary table
#'
#' Build a one-row PRAME summary table used when the analysis is unavailable or
#' not supportable.
#'
#' @param note Character explanation to store in the `Interpretation` column.
#' @return Data frame matching the PRAME summary schema.
create_prame_placeholder_table <- function(note) {
    data.frame(
        Timepoint = "Not available",
        N = NA_real_,
        N_PRAME_Positive = NA_real_,
        N_PRAME_Negative = NA_real_,
        Events = NA_real_,
        Events_PRAME_Positive = NA_real_,
        Events_PRAME_Negative = NA_real_,
        Non_Events = NA_real_,
        Event_Rate_Pct = NA_real_,
        Bootstrap_Valid_Resamples = NA_real_,
        Base_Harrell_C = NA_real_,
        Enhanced_Harrell_C = NA_real_,
        Delta_Harrell_C = NA_real_,
        Delta_CI_Lower = NA_real_,
        Delta_CI_Upper = NA_real_,
        LR_p = NA_real_,
        PRAME_HR = NA_real_,
        PRAME_HR_CI_Lower = NA_real_,
        PRAME_HR_CI_Upper = NA_real_,
        Analysis_Tier = NA_character_,
        Interpretation = note,
        stringsAsFactors = FALSE
    )
}

#' Collect unified PRAME comparison rows
#'
#' Convert PRAME incremental discrimination results into a cross-outcome comparison table
#' used by the unified workbook.
#'
#' @param prame_results PRAME analysis result object.
#' @param outcome_label Character outcome label, such as `"MFS"` or `"MSS"`.
#' @return Data frame with one row per timepoint, or an empty data frame if no
#'   comparison rows can be created.
collect_unified_prame_rows <- function(prame_results, outcome_label) {
    if (is.null(prame_results) || is.null(prame_results$comparison_results) || !is.list(prame_results$comparison_results)) {
        return(data.frame())
    }

    rows <- lapply(names(prame_results$comparison_results), function(tp_name) {
        res <- prame_results$comparison_results[[tp_name]]
        data.frame(
            Outcome = outcome_label,
            Timepoint = res$timepoint %||% tp_name,
            N = res$n %||% prame_results$n %||% NA,
            Base_Harrell_C = suppressWarnings(as.numeric(res$base_harrell_c)),
            Enhanced_Harrell_C = suppressWarnings(as.numeric(res$enhanced_harrell_c)),
            Delta_Harrell_C = suppressWarnings(as.numeric(res$delta_harrell_c)),
            LR_p = suppressWarnings(as.numeric(res$lr_p)),
            Interpretation = res$interpretation %||% "Analysis not supportable for this timepoint",
            stringsAsFactors = FALSE
        )
    })

    do.call(rbind, rows)
}

#' Create a unified PRAME placeholder row
#'
#' Build a single-row placeholder for the unified PRAME comparison workbook when
#' one outcome has no supportable PRAME results.
#'
#' @param outcome_label Character outcome label.
#' @param note Character explanation to store in the `Interpretation` column.
#' @return One-row data frame matching the unified PRAME comparison schema.
create_unified_prame_placeholder_row <- function(outcome_label, note) {
    data.frame(
        Outcome = outcome_label,
        Timepoint = "Not available",
        N = NA_real_,
        Base_Harrell_C = NA_real_,
        Enhanced_Harrell_C = NA_real_,
        Delta_Harrell_C = NA_real_,
        LR_p = NA_real_,
        Interpretation = note,
        stringsAsFactors = FALSE
    )
}

#' Append extrapolation metadata to a consolidated table
#'
#' Add the imported-versus-extrapolated reporting fields required for Objective
#' 4 later-horizon summaries.
#'
#' @param summary_table Data frame containing a `Timepoint` column.
#' @param extrapolation_assessment Optional Objective 4 extrapolation-support
#'   summary for later horizons.
#' @return Data frame with the metadata columns appended.
append_gep_extrapolation_metadata <- function(summary_table, extrapolation_assessment = NULL) {
    if (!is.data.frame(summary_table) || nrow(summary_table) == 0 || !"Timepoint" %in% names(summary_table)) {
        return(summary_table)
    }

    metadata_rows <- lapply(summary_table$Timepoint, function(timepoint_label) {
        create_gep_extrapolation_metadata(
            timepoint_label = timepoint_label,
            extrapolation_assessment = extrapolation_assessment
        )
    })
    metadata_frame <- dplyr::bind_rows(metadata_rows)

    dplyr::bind_cols(summary_table, metadata_frame)
}

#' Create the workbook-ready extrapolation-assumption table
#'
#' Normalize the endpoint-level extrapolation-support result into a single-sheet
#' workbook table.
#'
#' @param extrapolation_assessment Optional Objective 4 extrapolation-support
#'   summary for later horizons.
#' @param outcome_type Character outcome label used when a placeholder row is
#'   required.
#' @return Data frame containing the extrapolation-assumption summary.
create_consolidated_extrapolation_assumption_table <- function(extrapolation_assessment, outcome_type) {
    if (!is.null(extrapolation_assessment) &&
        !is.null(extrapolation_assessment$summary_table) &&
        is.data.frame(extrapolation_assessment$summary_table) &&
        nrow(extrapolation_assessment$summary_table) > 0) {
        return(extrapolation_assessment$summary_table)
    }

    data.frame(
        Outcome = outcome_type,
        Dataset = NA_character_,
        N = NA_real_,
        Events = NA_real_,
        Followup_Beyond_5yr_n = NA_real_,
        Exponential_Hazard_Per_Year = NA_real_,
        Exponential_Hazard_CI_Lower = NA_real_,
        Exponential_Hazard_CI_Upper = NA_real_,
        Weibull_Shape = NA_real_,
        Weibull_Shape_CI_Lower = NA_real_,
        Weibull_Shape_CI_Upper = NA_real_,
        Exponential_AIC = NA_real_,
        Weibull_AIC = NA_real_,
        Delta_AIC_Weibull_minus_Exponential = NA_real_,
        Pre5yr_Hazard_Per_Year = NA_real_,
        Post5yr_Hazard_Per_Year = NA_real_,
        Post_vs_Pre_Hazard_Ratio = NA_real_,
        Support_Status = "Unavailable",
        Support_Note = "No extrapolation-assumption summary was supplied.",
        stringsAsFactors = FALSE
    )
}

#' Create consolidated calibration table across all timepoints
#'
#' Gather timepoint-specific calibration outputs into a single workbook-ready
#' table with explicit method fields.
#'
#' @param validation_results Named list of per-timepoint validation results.
#' @param outcome_type Character outcome label retained for interface
#'   consistency.
#' @param extrapolation_assessment Optional Objective 4 extrapolation-support
#'   summary for later horizons.
#' @return Data frame with one row per timepoint.
create_consolidated_calibration_table <- function(validation_results, outcome_type, extrapolation_assessment = NULL) {
    cal_data <- data.frame()

    for (tp_name in names(validation_results)) {
        tp_results <- validation_results[[tp_name]]
        if (!is.null(tp_results$calibration)) {
            cal <- tp_results$calibration
            cal_data <- rbind(cal_data, data.frame(
                Timepoint = tp_name,
                N = cal$n %||% NA,
                Fit_N = cal$fit_n %||% NA,
                Status = cal$status %||% NA,
                Events = cal$events %||% NA,
                Non_Events = cal$non_events %||% NA,
                Unique_Risk_Count = cal$unique_risk_count %||% NA,
                Nam_D_Agostino_p = cal$nam_dagostino_p %||% NA,
                Nam_D_Agostino_log_p = cal$nam_dagostino_log_p %||% NA,
                Nam_D_Agostino_Method = cal$nam_dagostino_method %||% NA,
                ICI = cal$ici %||% NA,
                ICI_Method = cal$ici_method %||% NA,
                Slope = cal$slope %||% cal$calibration_slope %||% NA,
                Slope_Method = cal$slope_method %||% NA,
                Slope_SE = cal$slope_se %||% NA,
                Slope_Unavailable_Reason = if (is.finite(cal$slope %||% cal$calibration_slope %||% NA_real_)) {
                    NA_character_
                } else {
                    describe_gep_slope_problem(
                        status = cal$status %||% NA_character_,
                        fit_n = cal$fit_n %||% NA_real_,
                        events = cal$events %||% NA_real_,
                        non_events = cal$non_events %||% NA_real_,
                        unique_risk_count = cal$unique_risk_count %||% NA_real_,
                        slope_se = cal$slope_se %||% NA_real_,
                        include_counts = TRUE
                    )
                },
                Brier_Score = cal$brier_score %||% NA,
                Brier_Method = cal$brier_method %||% NA,
                Brier_Fallback_Used = cal$brier_fallback_used %||% NA,
                stringsAsFactors = FALSE
            ))
        }
    }

    append_gep_extrapolation_metadata(cal_data, extrapolation_assessment = extrapolation_assessment)
}

#' Create consolidated discrimination table across all timepoints
#'
#' Gather timepoint-specific discrimination outputs into a single workbook-ready
#' table.
#'
#' @param validation_results Named list of per-timepoint validation results.
#' @param outcome_type Character outcome label retained for interface
#'   consistency.
#' @param extrapolation_assessment Optional Objective 4 extrapolation-support
#'   summary for later horizons.
#' @return Data frame with one row per timepoint.
create_consolidated_discrimination_table <- function(validation_results, outcome_type, extrapolation_assessment = NULL) {
    disc_data <- data.frame()

    for (tp_name in names(validation_results)) {
        tp_results <- validation_results[[tp_name]]
        if (!is.null(tp_results$discrimination)) {
            disc <- tp_results$discrimination
            disc_data <- rbind(disc_data, data.frame(
                Timepoint = tp_name,
                N = disc$n %||% NA,
                Events = disc$events %||% NA,
                # PRIMARY DISCRIMINATION METRIC
                Harrell_C = disc$harrell_c %||% NA,
                # ROBUST DISCRIMINATION METRICS (replacing fragile timepoint-dependent metrics)
                Integrated_AUC = disc$integrated_auc %||% NA,
                Cumulative_Discrimination = disc$cumulative_discrimination %||% NA,
                Time_averaged_Discrimination = disc$time_averaged_discrimination %||% NA,
                # CLINICAL VALUE ASSESSMENT
                IPA = disc$ipa %||% NA,
                IPA_Method = disc$ipa_method %||% NA,
                IPA_Fallback_Used = disc$ipa_fallback_used %||% NA,
                stringsAsFactors = FALSE
            ))
        }
    }

    append_gep_extrapolation_metadata(disc_data, extrapolation_assessment = extrapolation_assessment)
}

#' Create consolidated performance table across all timepoints
#'
#' Build a reduced performance table for legacy consumers that still expect a
#' separate performance summary distinct from the discrimination table.
#'
#' @param validation_results Named list of per-timepoint validation results.
#' @param outcome_type Character outcome label retained for interface
#'   consistency.
#' @return Data frame with one row per timepoint.
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
                Harrell_C = disc$harrell_c %||% NA,
                Integrated_AUC = disc$integrated_auc %||% NA,
                stringsAsFactors = FALSE
            ))
        }
    }

    return(perf_data)
}

#' Create consolidated decision curve table across all timepoints
#'
#' Gather decision-curve metrics into a single workbook-ready table spanning all
#' available timepoints.
#'
#' @param validation_results Named list of per-timepoint validation results.
#' @param outcome_type Character outcome label retained for interface
#'   consistency.
#' @param extrapolation_assessment Optional Objective 4 extrapolation-support
#'   summary for later horizons.
#' @return Data frame with one row per timepoint.
create_consolidated_decision_curve_table <- function(validation_results, outcome_type, extrapolation_assessment = NULL) {
    dca_data <- data.frame()

    for (tp_name in names(validation_results)) {
        tp_results <- validation_results[[tp_name]]
        if (!is.null(tp_results$decision_curve)) {
            dca <- tp_results$decision_curve
            # Extract key decision curve metrics if available
            dca_data <- rbind(dca_data, data.frame(
                Timepoint = tp_name,
                N = dca$n %||% NA,
                Events = dca$events %||% NA,
                Event_Rate = dca$event_rate %||% NA,
                Optimal_Threshold = dca$optimal_threshold %||% dca$net_benefit_threshold %||% NA,
                Optimal_Net_Benefit = dca$optimal_net_benefit %||% NA,
                Threshold_Range_Min = dca$threshold_range_min %||% NA,
                Threshold_Range_Max = dca$threshold_range_max %||% NA,
                Area_Between_Curves = dca$area_between_curves %||% NA,
                stringsAsFactors = FALSE
            ))
        }
    }

    append_gep_extrapolation_metadata(dca_data, extrapolation_assessment = extrapolation_assessment)
}

#' Create comprehensive text summary from consolidated tables
#'
#' Render a plain-text summary of calibration, discrimination, and decision-curve
#' results from the consolidated workbook tables.
#'
#' @param validation_results Named list of per-timepoint validation results.
#' @param outcome_type Character outcome label.
#' @param cal_consolidated Consolidated calibration data frame.
#' @param disc_consolidated Consolidated discrimination data frame.
#' @param dca_consolidated Consolidated decision-curve data frame.
#' @param extrapolation_consolidated Consolidated extrapolation-support table.
#' @return Character scalar containing a newline-delimited summary.
create_comprehensive_text_summary <- function(validation_results, outcome_type,
                                              cal_consolidated, disc_consolidated,
                                              dca_consolidated, extrapolation_consolidated = data.frame()) {
    summary_lines <- c()
    summary_lines <- c(summary_lines, paste("=", outcome_type, "Validation - Consolidated Summary", "="))
    summary_lines <- c(summary_lines, "")

    # Calibration summary
    if (nrow(cal_consolidated) > 0) {
        summary_lines <- c(summary_lines, "CALIBRATION SUMMARY:")
        summary_lines <- c(summary_lines, "")
        summary_lines <- c(summary_lines, sprintf("%-10s %-8s %-20s %-12s %-18s %-10s %-18s", "Timepoint", "N", "Nam-D'Agostino p", "ICI", "ICI Method", "Slope", "Slope Method"))
        summary_lines <- c(summary_lines, paste(rep("-", 110), collapse = ""))
        for (i in seq_len(nrow(cal_consolidated))) {
            row <- cal_consolidated[i, ]
            summary_lines <- c(
                summary_lines,
                sprintf(
                    "%-10s %-8s %-20s %-12s %-18s %-10s %-18s",
                    row$Timepoint,
                    ifelse(is.na(row$N), "NA", as.character(row$N)),
                    format_gep_p_value(
                        row$Nam_D_Agostino_p,
                        log_p_value = row$Nam_D_Agostino_log_p,
                        decimal_places = 3
                    ),
                    ifelse(is.na(row$ICI), "NA", sprintf("%.3f", row$ICI)),
                    ifelse(is.na(row$ICI_Method), "NA", as.character(row$ICI_Method)),
                    ifelse(is.na(row$Slope), "NA", sprintf("%.3f", row$Slope)),
                    ifelse(is.na(row$Slope_Method), "NA", as.character(row$Slope_Method))
                )
            )
        }
        summary_lines <- c(summary_lines, "")
    }

    # Discrimination summary
    if (nrow(disc_consolidated) > 0) {
        summary_lines <- c(summary_lines, "DISCRIMINATION SUMMARY:")
        summary_lines <- c(summary_lines, "")
        summary_lines <- c(summary_lines, sprintf("%-10s %-8s %-10s %-15s %-15s %-15s", "Timepoint", "N", "Events", "Harrell's C", "Integrated_AUC", "Cumulative_Disc"))
        summary_lines <- c(summary_lines, paste(rep("-", 80), collapse = ""))
        for (i in seq_len(nrow(disc_consolidated))) {
            row <- disc_consolidated[i, ]
            summary_lines <- c(
                summary_lines,
                sprintf(
                    "%-10s %-8s %-10s %-15s %-15s %-15s",
                    row$Timepoint,
                    ifelse(is.na(row$N), "NA", as.character(row$N)),
                    ifelse(is.na(row$Events), "NA", as.character(row$Events)),
                    ifelse(is.na(row$Harrell_C), "NA", sprintf("%.3f", row$Harrell_C)),
                    ifelse(is.na(row$Integrated_AUC), "NA", sprintf("%.3f", row$Integrated_AUC)),
                    ifelse(is.na(row$Cumulative_Discrimination), "NA", sprintf("%.3f", row$Cumulative_Discrimination))
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
        summary_lines <- c(summary_lines, sprintf("%-10s %-8s %-12s %-10s", "Timepoint", "N", "Opt_Threshold", "Opt_Net_Ben"))
        summary_lines <- c(summary_lines, paste(rep("-", 50), collapse = ""))
        for (i in seq_len(nrow(dca_consolidated))) {
            row <- dca_consolidated[i, ]
            summary_lines <- c(
                summary_lines,
                sprintf(
                    "%-10s %-8s %-12s %-10s",
                    row$Timepoint,
                    ifelse(is.na(row$N), "NA", as.character(row$N)),
                    ifelse(is.na(row$Optimal_Threshold), "NA", sprintf("%.3f", row$Optimal_Threshold)),
                    ifelse(is.na(row$Optimal_Net_Benefit), "NA", sprintf("%.4f", row$Optimal_Net_Benefit))
                )
            )
        }
        summary_lines <- c(summary_lines, "")
    }

    if (nrow(extrapolation_consolidated) > 0) {
        summary_lines <- c(summary_lines, "EXTRAPOLATION ASSUMPTION CHECK:")
        summary_lines <- c(summary_lines, "")
        for (i in seq_len(nrow(extrapolation_consolidated))) {
            row <- extrapolation_consolidated[i, ]
            summary_lines <- c(summary_lines, sprintf(
                "%s support: %s",
                row$Outcome %||% outcome_type,
                row$Support_Status %||% "Unavailable"
            ))
            summary_lines <- c(summary_lines, sprintf(
                "Note: %s",
                row$Support_Note %||% "No extrapolation support note available."
            ))
        }
        summary_lines <- c(summary_lines, "")
    }

    # Key findings summary
    summary_lines <- c(summary_lines, "KEY FINDINGS:")
    if (nrow(cal_consolidated) > 0) {
        valid_cal_idx <- which(!is.na(cal_consolidated$Slope))
        if (length(valid_cal_idx) > 0) {
            best_cal_idx <- valid_cal_idx[which.max(cal_consolidated$Slope[valid_cal_idx])]
            best_tp <- cal_consolidated$Timepoint[best_cal_idx]
            best_slope <- cal_consolidated$Slope[best_cal_idx]
            summary_lines <- c(
                summary_lines,
                sprintf("- Best calibration at %s (slope: %.3f)", best_tp, best_slope)
            )
        } else {
            slope_issue_summary <- summarize_gep_slope_issue_pattern(cal_consolidated)
            summary_lines <- c(
                summary_lines,
                if (nzchar(slope_issue_summary)) {
                    paste0("- ", slope_issue_summary)
                } else {
                    "- The calibration slope could not be estimated across timepoints"
                }
            )
        }
    }

    if (nrow(disc_consolidated) > 0) {
        valid_disc_idx <- which(!is.na(disc_consolidated$Harrell_C))
        if (length(valid_disc_idx) > 0) {
            best_disc_idx <- valid_disc_idx[which.max(disc_consolidated$Harrell_C[valid_disc_idx])]
            best_tp <- disc_consolidated$Timepoint[best_disc_idx]
            best_c <- disc_consolidated$Harrell_C[best_disc_idx]
            summary_lines <- c(
                summary_lines,
                sprintf("- Best discrimination at %s (Harrell's C: %.3f)", best_tp, best_c)
            )
        } else {
            summary_lines <- c(
                summary_lines,
                "- No finite discrimination estimate was available across timepoints"
            )
        }
    }

    summary_lines <- c(summary_lines, "")
    summary_lines <- c(summary_lines, "Note: This consolidated summary replaces multiple redundant plots")
    summary_lines <- c(summary_lines, "while maintaining all statistical information.")
    summary_lines <- c(summary_lines, "Performance Summary removed to eliminate redundancy with Discrimination Summary.")

    return(paste(summary_lines, collapse = "\n"))
}

#' Create unified GEP validation comparison workbook across outcomes
#'
#' Creates a cross-outcome comparison workbook that is intentionally distinct
#' from the outcome-specific consolidated summaries.
#'
#' @param mfs_results MFS validation results
#' @param mss_results MSS validation results
#' @param no_gep_results Optional exploratory no-GEP summary object for the full
#'   cohort. When provided, compact no-GEP comparison sheets are appended.
#' @param output_dir Output directory
#' @param prefix Filename prefix
#' @return List of created summary files
create_unified_gep_validation_summary <- function(mfs_results, mss_results, output_dir, prefix, no_gep_results = NULL) {
    logger::log_info(formatted("Creating unified GEP validation summary to eliminate redundancy", indent = 1))

    # Create unified calibration comparison
    unified_cal <- create_unified_calibration_summary(mfs_results, mss_results)

    # Create unified discrimination comparison
    unified_disc <- create_unified_discrimination_summary(mfs_results, mss_results)

    # Build unified PRAME summary if available
    unified_prame <- data.frame()
    try(
        {
            if (!is.null(mfs_results)) {
                mfs_prame <- collect_unified_prame_rows(mfs_results$prame_analysis, "MFS")
                if (nrow(mfs_prame) == 0) {
                    mfs_prame <- create_unified_prame_placeholder_row(
                        "MFS",
                        get_prame_availability_note(mfs_results$prame_analysis, "MFS PRAME analysis")
                    )
                }
                unified_prame <- rbind(unified_prame, mfs_prame)
            }

            if (!is.null(mss_results)) {
                mss_prame <- collect_unified_prame_rows(mss_results$prame_results, "MSS")
                if (nrow(mss_prame) == 0) {
                    mss_prame <- create_unified_prame_placeholder_row(
                        "MSS",
                        get_prame_availability_note(mss_results$prame_results, "MSS PRAME analysis")
                    )
                }
                unified_prame <- rbind(unified_prame, mss_prame)
            }
        },
        silent = TRUE
    )

    # Build unified Missing Data summary if available
    unified_missing <- data.frame()
    try(
        {
            # Helper to extract compact metrics
            compact_missing <- function(md, outcome_label) {
                if (is.null(md)) {
                    return(NULL)
                }
                patterns <- tryCatch(md$missing_patterns, error = function(e) NULL)
                n_sig <- tryCatch(md$baseline_comparison$n_significant, error = function(e) NA)
                logrank_p <- tryCatch(md$outcome_by_missing$logrank_p, error = function(e) NA)
                n_imputable <- tryCatch(md$imputation_analysis$n_imputable, error = function(e) NA)
                data.frame(
                    Outcome = outcome_label,
                    Total_Patients_n = tryCatch(md$n_total, error = function(e) NA),
                    Missingness_Groups_n = if (!is.null(patterns)) nrow(patterns) else NA,
                    Baseline_Significant_Differences_n = n_sig,
                    Survival_by_Missingness_Logrank_p = logrank_p,
                    Imputable_Patients_n = n_imputable,
                    stringsAsFactors = FALSE
                )
            }
            if (!is.null(mfs_results) && !is.null(mfs_results$missing_data_analysis)) {
                unified_missing <- rbind(unified_missing, compact_missing(mfs_results$missing_data_analysis, "MFS"))
            }
            if (!is.null(mss_results) && !is.null(mss_results$missing_data_analysis)) {
                unified_missing <- rbind(unified_missing, compact_missing(mss_results$missing_data_analysis, "MSS"))
            }
        },
        silent = TRUE
    )

    # Combine into a comparison-only workbook so this file is not mistaken for
    # another outcome-level summary export.
    unified_workbook <- list()
    if (nrow(unified_cal) > 0) unified_workbook[["Calibration_Comparison"]] <- unified_cal
    if (nrow(unified_disc) > 0) unified_workbook[["Discrimination_Comparison"]] <- unified_disc
    if (nrow(unified_prame) > 0) unified_workbook[["PRAME_Comparison"]] <- unified_prame
    if (nrow(unified_missing) > 0) unified_workbook[["Missing_Data_Comparison"]] <- unified_missing
    if (!is.null(no_gep_results)) {
        if (!is.null(no_gep_results$unified_no_gep_overview) && nrow(no_gep_results$unified_no_gep_overview) > 0) {
            unified_workbook[["No_GEP_Overview"]] <- no_gep_results$unified_no_gep_overview
        }
        if (!is.null(no_gep_results$unified_no_gep_model_comparison) && nrow(no_gep_results$unified_no_gep_model_comparison) > 0) {
            unified_workbook[["No_GEP_Model_Comparison"]] <- no_gep_results$unified_no_gep_model_comparison
        }
        if (!is.null(no_gep_results$unified_no_gep_risk_strata) && nrow(no_gep_results$unified_no_gep_risk_strata) > 0) {
            unified_workbook[["No_GEP_Risk_Strata"]] <- no_gep_results$unified_no_gep_risk_strata
        }
        if (!is.null(no_gep_results$unified_no_gep_risk_ladder) && nrow(no_gep_results$unified_no_gep_risk_ladder) > 0) {
            unified_workbook[["No_GEP_Risk_Ladder"]] <- no_gep_results$unified_no_gep_risk_ladder
        }
    }

    # Save unified workbook
    if (length(unified_workbook) > 0) {
        unified_path <- file.path(output_dir, paste0(prefix, "unified_gep_validation_summary.xlsx"))
        writexl::write_xlsx(unified_workbook, unified_path)
        logger::log_info(formatted(sprintf("Unified GEP validation summary saved: %s", unified_path), indent = 2))
    }

    # Create unified text summary (but don't save to file to avoid redundancy)
    unified_text <- create_unified_text_summary(mfs_results = mfs_results, mss_results = mss_results, unified_cal = unified_cal, unified_disc = unified_disc)
    # REMOVED: Text file generation to eliminate redundancy
    # text_path <- file.path(output_dir, paste0(prefix, "unified_gep_validation_summary.txt"))
    # writeLines(unified_text, text_path)

    return(list(
        calibration = unified_cal,
        discrimination = unified_disc,
        no_gep_overview = no_gep_results$unified_no_gep_overview %||% data.frame(),
        no_gep_model_comparison = no_gep_results$unified_no_gep_model_comparison %||% data.frame(),
        no_gep_risk_strata = no_gep_results$unified_no_gep_risk_strata %||% data.frame(),
        no_gep_risk_ladder = no_gep_results$unified_no_gep_risk_ladder %||% data.frame(),
        text_summary = unified_text
    ))
}

#' Create unified calibration summary across outcomes
#'
#' Combine MFS and MSS calibration outputs into a single cross-outcome
#' comparison table.
#'
#' @param mfs_results MFS validation result object.
#' @param mss_results MSS validation result object.
#' @return Data frame with one row per outcome and timepoint.
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
                    Fit_N = cal$fit_n %||% NA,
                    Status = cal$status %||% NA,
                    Events = cal$events %||% NA,
                    Non_Events = cal$non_events %||% NA,
                    Unique_Risk_Count = cal$unique_risk_count %||% NA,
                    Nam_D_Agostino_p = cal$nam_dagostino_p %||% NA,
                    Nam_D_Agostino_Method = cal$nam_dagostino_method %||% NA,
                    ICI = cal$ici %||% NA,
                    ICI_Method = cal$ici_method %||% NA,
                    Slope = cal$slope %||% cal$calibration_slope %||% NA,
                    Slope_Method = cal$slope_method %||% NA,
                    Slope_SE = cal$slope_se %||% NA,
                    Slope_Unavailable_Reason = if (is.finite(cal$slope %||% cal$calibration_slope %||% NA_real_)) {
                        NA_character_
                    } else {
                        describe_gep_slope_problem(
                            status = cal$status %||% NA_character_,
                            fit_n = cal$fit_n %||% NA_real_,
                            events = cal$events %||% NA_real_,
                            non_events = cal$non_events %||% NA_real_,
                            unique_risk_count = cal$unique_risk_count %||% NA_real_,
                            slope_se = cal$slope_se %||% NA_real_,
                            include_counts = TRUE
                        )
                    },
                    stringsAsFactors = FALSE
                ))
            }
        }
    }

    # Add MSS calibration data
    if (!is.null(mss_results$standard_results)) {
        for (tp_name in names(mss_results$standard_results)) {
            tp_results <- mss_results$standard_results[[tp_name]]
            if (!is.null(tp_results$calibration)) {
                cal <- tp_results$calibration
                unified_cal <- rbind(unified_cal, data.frame(
                    Outcome = "MSS",
                    Timepoint = tp_name,
                    N = cal$n %||% NA,
                    Fit_N = cal$fit_n %||% NA,
                    Status = cal$status %||% NA,
                    Events = cal$events %||% NA,
                    Non_Events = cal$non_events %||% NA,
                    Unique_Risk_Count = cal$unique_risk_count %||% NA,
                    Nam_D_Agostino_p = cal$nam_dagostino_p %||% NA,
                    Nam_D_Agostino_Method = cal$nam_dagostino_method %||% NA,
                    ICI = cal$ici %||% NA,
                    ICI_Method = cal$ici_method %||% NA,
                    Slope = cal$slope %||% cal$calibration_slope %||% NA,
                    Slope_Method = cal$slope_method %||% NA,
                    Slope_SE = cal$slope_se %||% NA,
                    Slope_Unavailable_Reason = if (is.finite(cal$slope %||% cal$calibration_slope %||% NA_real_)) {
                        NA_character_
                    } else {
                        describe_gep_slope_problem(
                            status = cal$status %||% NA_character_,
                            fit_n = cal$fit_n %||% NA_real_,
                            events = cal$events %||% NA_real_,
                            non_events = cal$non_events %||% NA_real_,
                            unique_risk_count = cal$unique_risk_count %||% NA_real_,
                            slope_se = cal$slope_se %||% NA_real_,
                            include_counts = TRUE
                        )
                    },
                    stringsAsFactors = FALSE
                ))
            }
        }
    }

    if (nrow(unified_cal) == 0) {
        return(unified_cal)
    }

    unified_cal <- dplyr::bind_rows(
        append_gep_extrapolation_metadata(
            unified_cal %>% dplyr::filter(.data$Outcome == "MFS"),
            extrapolation_assessment = mfs_results$extrapolation_assessment %||% NULL
        ),
        append_gep_extrapolation_metadata(
            unified_cal %>% dplyr::filter(.data$Outcome == "MSS"),
            extrapolation_assessment = mss_results$extrapolation_assessment %||% NULL
        )
    )

    unified_cal
}

#' Create unified discrimination summary across outcomes
#'
#' Combine MFS and MSS discrimination outputs into a single cross-outcome
#' comparison table.
#'
#' @param mfs_results MFS validation result object.
#' @param mss_results MSS validation result object.
#' @return Data frame with one row per outcome and timepoint.
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
                    # PRIMARY DISCRIMINATION METRIC
                    Harrell_C = disc$harrell_c %||% NA,
                    # ROBUST DISCRIMINATION METRICS (replacing fragile timepoint-dependent metrics)
                    Integrated_AUC = disc$integrated_auc %||% NA,
                    Cumulative_Discrimination = disc$cumulative_discrimination %||% NA,
                    Time_averaged_Discrimination = disc$time_averaged_discrimination %||% NA,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }

    # Add MSS discrimination data
    if (!is.null(mss_results$standard_results)) {
        for (tp_name in names(mss_results$standard_results)) {
            tp_results <- mss_results$standard_results[[tp_name]]
            if (!is.null(tp_results$discrimination)) {
                disc <- tp_results$discrimination
                unified_disc <- rbind(unified_disc, data.frame(
                    Outcome = "MSS",
                    Timepoint = tp_name,
                    N = disc$n %||% NA,
                    Events = disc$events %||% NA,
                    # PRIMARY DISCRIMINATION METRIC
                    Harrell_C = disc$harrell_c %||% NA,
                    # ROBUST DISCRIMINATION METRICS (replacing fragile timepoint-dependent metrics)
                    Integrated_AUC = disc$integrated_auc %||% NA,
                    Cumulative_Discrimination = disc$cumulative_discrimination %||% NA,
                    Time_averaged_Discrimination = disc$time_averaged_discrimination %||% NA,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }

    if (nrow(unified_disc) == 0) {
        return(unified_disc)
    }

    unified_disc <- dplyr::bind_rows(
        append_gep_extrapolation_metadata(
            unified_disc %>% dplyr::filter(.data$Outcome == "MFS"),
            extrapolation_assessment = mfs_results$extrapolation_assessment %||% NULL
        ),
        append_gep_extrapolation_metadata(
            unified_disc %>% dplyr::filter(.data$Outcome == "MSS"),
            extrapolation_assessment = mss_results$extrapolation_assessment %||% NULL
        )
    )

    unified_disc
}

# REMOVED: create_unified_performance_summary function
# This function was removed to eliminate redundancy with discrimination metrics
# Performance metrics (C-Index, AUC) are the same as discrimination metrics (Harrell's C, Integrated AUC)

#' Create unified text summary
#'
#' Render a plain-text comparison of unified calibration and discrimination
#' results across outcomes.
#'
#' @param mfs_results MFS validation result object.
#' @param mss_results MSS validation result object.
#' @param unified_cal Unified calibration summary table.
#' @param unified_disc Unified discrimination summary table.
#' @return Character scalar containing a newline-delimited summary.
create_unified_text_summary <- function(mfs_results, mss_results, unified_cal, unified_disc) {
    summary_lines <- c()
    summary_lines <- c(summary_lines, "=", "Unified GEP Validation Summary", "=")
    summary_lines <- c(summary_lines, "Combines MFS and MSS results to eliminate redundancy")
    summary_lines <- c(summary_lines, "")

    # Calibration comparison
    if (nrow(unified_cal) > 0) {
        summary_lines <- c(summary_lines, "CALIBRATION COMPARISON (MFS vs MSS):")
        summary_lines <- c(summary_lines, "Outcome | Timepoint | N | Nam-D'Agostino p | ICI | ICI Method | Slope | Slope Method")
        summary_lines <- c(summary_lines, "---------|-----------|----|------------------|-----|------------|-------|-------------")
        for (i in seq_len(nrow(unified_cal))) {
            row <- unified_cal[i, ]
            summary_lines <- c(
                summary_lines,
                sprintf(
                    "%s | %s | %s | %s | %s | %s | %s | %s",
                    row$Outcome,
                    row$Timepoint,
                    ifelse(is.na(row$N), "NA", as.character(row$N)),
                    format_gep_p_value(row$Nam_D_Agostino_p, decimal_places = 3),
                    ifelse(is.na(row$ICI), "NA", sprintf("%.3f", row$ICI)),
                    ifelse(is.na(row$ICI_Method), "NA", as.character(row$ICI_Method)),
                    ifelse(is.na(row$Slope), "NA", sprintf("%.3f", row$Slope)),
                    ifelse(is.na(row$Slope_Method), "NA", as.character(row$Slope_Method))
                )
            )
        }
        summary_lines <- c(summary_lines, "")
    }

    # Discrimination comparison
    if (nrow(unified_disc) > 0) {
        summary_lines <- c(summary_lines, "DISCRIMINATION COMPARISON (MFS vs MSS):")
        summary_lines <- c(summary_lines, "Outcome | Timepoint | N | Events | Harrell's C | Integrated_AUC | Cumulative_Disc | Time_Avg_Disc")
        summary_lines <- c(summary_lines, "---------|-----------|----|--------|-------------|---------------|----------------|-------------")
        for (i in seq_len(nrow(unified_disc))) {
            row <- unified_disc[i, ]
            summary_lines <- c(
                summary_lines,
                sprintf(
                    "%s | %s | %s | %s | %s | %s | %s | %s",
                    row$Outcome,
                    row$Timepoint,
                    ifelse(is.na(row$N), "NA", as.character(row$N)),
                    ifelse(is.na(row$Events), "NA", as.character(row$Events)),
                    ifelse(is.na(row$Harrell_C), "NA", sprintf("%.3f", row$Harrell_C)),
                    ifelse(is.na(row$Integrated_AUC), "NA", sprintf("%.3f", row$Integrated_AUC)),
                    ifelse(is.na(row$Cumulative_Discrimination), "NA", sprintf("%.3f", row$Cumulative_Discrimination)),
                    ifelse(is.na(row$Time_averaged_Discrimination), "NA", sprintf("%.3f", row$Time_averaged_Discrimination))
                )
            )
        }
        summary_lines <- c(summary_lines, "")
    }

    # REMOVED: Performance comparison to eliminate redundancy with discrimination metrics
    # Performance comparison was redundant because C-Index = Harrell's C (same metric, different name)

    # Brief PRAME / Missing Data summaries
    # PRAME
    try(
        {
            pr_lines <- c()
            if (!is.null(mfs_results) && !is.null(mfs_results$prame_analysis)) {
                vals <- mfs_results$prame_analysis$comparison_results
                if (is.list(vals) && length(vals) > 0) {
                    pairs <- vapply(vals, function(x) {
                        sprintf("%s=%.3f", x$timepoint %||% NA, suppressWarnings(as.numeric(x$delta_harrell_c)))
                    }, character(1))
                    pr_lines <- c(pr_lines, sprintf("MFS PRAME delta C: %s", paste(pairs, collapse = ", ")))
                }
            }
            if (!is.null(mss_results) && !is.null(mss_results$prame_results)) {
                vals <- mss_results$prame_results$comparison_results
                if (is.list(vals) && length(vals) > 0) {
                    pairs <- vapply(vals, function(x) {
                        sprintf("%s=%.3f", x$timepoint %||% NA, suppressWarnings(as.numeric(x$delta_harrell_c)))
                    }, character(1))
                    pr_lines <- c(pr_lines, sprintf("MSS PRAME delta C: %s", paste(pairs, collapse = ", ")))
                }
            }
            if (length(pr_lines) > 0) {
                summary_lines <- c(summary_lines, "PRAME SUMMARY:", pr_lines, "")
            }
        },
        silent = TRUE
    )
    # Missing Data
    try(
        {
            md_lines <- c()
            if (!is.null(mfs_results) && !is.null(mfs_results$missing_data_analysis)) {
                md <- mfs_results$missing_data_analysis
                md_lines <- c(md_lines, sprintf(
                    "MFS Missing: total=%s, patterns=%s, baseline_diffs_sig=%s, logrank_p=%s, imputable=%s",
                    tryCatch(md$n_total, error = function(e) NA),
                    tryCatch(nrow(md$missing_patterns), error = function(e) NA),
                    tryCatch(md$baseline_comparison$n_significant, error = function(e) NA),
                    tryCatch(md$outcome_by_missing$logrank_p, error = function(e) NA),
                    tryCatch(md$imputation_analysis$n_imputable, error = function(e) NA)
                ))
            }
            if (!is.null(mss_results) && !is.null(mss_results$missing_data_analysis)) {
                md <- mss_results$missing_data_analysis
                md_lines <- c(md_lines, sprintf(
                    "MSS Missing: total=%s, patterns=%s, baseline_diffs_sig=%s, logrank_p=%s, imputable=%s",
                    tryCatch(md$n_total, error = function(e) NA),
                    tryCatch(nrow(md$missing_patterns), error = function(e) NA),
                    tryCatch(md$baseline_comparison$n_significant, error = function(e) NA),
                    tryCatch(md$outcome_by_missing$logrank_p, error = function(e) NA),
                    tryCatch(md$imputation_analysis$n_imputable, error = function(e) NA)
                ))
            }
            if (length(md_lines) > 0) {
                summary_lines <- c(summary_lines, "MISSING DATA SUMMARY:", md_lines, "")
            }
        },
        silent = TRUE
    )

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
                summary_lines <- c(
                    summary_lines,
                    sprintf("- MFS: Best calibration at %s (slope: %.3f)", best_tp, best_slope)
                )
            }
        }

        if (nrow(mss_cal) > 0) {
            best_mss_idx <- which.max(mss_cal$Slope %||% 0)
            if (best_mss_idx > 0) {
                best_tp <- mss_cal$Timepoint[best_mss_idx]
                best_slope <- mss_cal$Slope[best_mss_idx]
                summary_lines <- c(
                    summary_lines,
                    sprintf("- MSS: Best calibration at %s (slope: %.3f)", best_tp, best_slope)
                )
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
                summary_lines <- c(
                    summary_lines,
                    sprintf("- MFS: Best discrimination at %s (Harrell's C: %.3f)", best_tp, best_c)
                )
            }
        }

        if (nrow(mss_disc) > 0) {
            best_mss_idx <- which.max(mss_disc$Harrell_C %||% 0)
            if (best_mss_idx > 0) {
                best_tp <- mss_disc$Timepoint[best_mss_idx]
                best_c <- mss_disc$Harrell_C[best_mss_idx]
                summary_lines <- c(
                    summary_lines,
                    sprintf("- MSS: Best discrimination at %s (Harrell's C: %.3f)", best_tp, best_c)
                )
            }
        }
    }

    summary_lines <- c(summary_lines, "")
    summary_lines <- c(summary_lines, "Note: This unified summary eliminates redundant outputs")
    summary_lines <- c(summary_lines, "while maintaining all statistical information across outcomes.")

    return(paste(summary_lines, collapse = "\n"))
}
