# Survival Outcomes Analysis

#' Build combined RMST and survival summary table
#'
#' Joins RMST outputs with Kaplan-Meier survival percentages for each time point
#' and treatment arm, producing a single table suitable for supplementary tables.
#'
#' @param rmst_results Data frame returned from the RMST calculation loop, containing
#'   RMST estimates for each time point and treatment arm.
#' @param surv_rates Data frame of survival percentages generated from `surv_summary`
#'   (long format with one row per treatment/time combination).
#' @param group_var Name of the grouping variable (used to enforce PBT/GKSRS only for treatment analyses)
#' @return A data frame where each row corresponds to a time point and includes
#'   survival percentages, RMST in years for both arms, the difference metrics, and
#'   the associated p-value. Returns an empty data frame if inputs are missing.
build_rmst_survival_summary <- function(rmst_results, surv_rates, group_var = "treatment_group") {
    if (is.null(rmst_results) || nrow(rmst_results) == 0) {
        return(data.frame())
    }
    if (is.null(surv_rates) || nrow(surv_rates) == 0) {
        return(data.frame())
    }

    survival_lookup <- surv_rates %>%
        dplyr::transmute(
            Treatment_Group = as.character(Treatment_Group),
            Time_Point_Years = Time_Years,
            Survival_Percent = as.numeric(surv_pct)
        )

    summary_df <- rmst_results %>%
        dplyr::mutate(
            Group1_Name = as.character(Group1_Name),
            Group2_Name = as.character(Group2_Name)
        ) %>%
        dplyr::left_join(
            survival_lookup,
            by = c("Group1_Name" = "Treatment_Group", "Time_Point_Years" = "Time_Point_Years")
        ) %>%
        dplyr::rename(Group1_Survival_Percent = Survival_Percent)

    survival_lookup_group2 <- survival_lookup %>%
        dplyr::rename(
            Group2_Name = Treatment_Group,
            Group2_Survival_Percent = Survival_Percent
        )

    summary_df <- summary_df %>%
        dplyr::left_join(
            survival_lookup_group2,
            by = c("Group2_Name", "Time_Point_Years" = "Time_Point_Years")
        ) %>%
        dplyr::mutate(
            Group1_Survival_Percent = round(Group1_Survival_Percent, 1),
            Group2_Survival_Percent = round(Group2_Survival_Percent, 1),
            RMST_Group1_Years = round(RMST_Group1_Years, 2),
            RMST_Group2_Years = round(RMST_Group2_Years, 2),
            RMST_Difference_Months = round(RMST_Difference_Months, 2),
            RMST_Difference_Years = round(RMST_Difference_Years, 3),
            RMST_P_Value = round(RMST_P_Value, 4),
            Time_Point_Label = paste0(Time_Point_Years, "-year")
        ) %>%
        dplyr::select(
            Time_Point_Label,
            Group1_Name,
            Group1_Survival_Percent,
            RMST_Group1_Years,
            Group2_Name,
            Group2_Survival_Percent,
            RMST_Group2_Years,
            RMST_Difference_Months,
            RMST_Difference_Years,
            RMST_P_Value,
            Analysis_Type
        )

    # Enforce PBT/GKSRS labeling only for treatment-group analyses
    if (identical(group_var, "treatment_group")) {
        valid_group1 <- all(sort(unique(stats::na.omit(summary_df$Group1_Name))) == "PBT")
        valid_group2 <- all(sort(unique(stats::na.omit(summary_df$Group2_Name))) == "GKSRS")
        if (!valid_group1 || !valid_group2) {
            logger::log_warn(format("RMST summary skipped: treatment group names not limited to PBT (Group 1) and GKSRS (Group 2).", indent = 1))
            return(data.frame())
        }
        logger::log_info(format("RMST summary treatment groups correctly labeled as 'PBT' (Group 1) and 'GKSRS' (Group 2).", indent = 1))
    }

    group1_summary_label <- if (identical(group_var, "treatment_group")) {
        "PBT"
    } else {
        valid_group1 <- unique(stats::na.omit(summary_df$Group1_Name))
        if (length(valid_group1) > 0) valid_group1[1] else "Group 1"
    }
    group2_summary_label <- if (identical(group_var, "treatment_group")) {
        "GKSRS"
    } else {
        valid_group2 <- unique(stats::na.omit(summary_df$Group2_Name))
        if (length(valid_group2) > 0) valid_group2[1] else "Group 2"
    }

    summary_df_final <- summary_df %>%
        dplyr::select(
            Time_Point_Label,
            Group1_Survival_Percent,
            RMST_Group1_Years,
            Group2_Survival_Percent,
            RMST_Group2_Years,
            RMST_Difference_Years,
            RMST_P_Value
        )
    names(summary_df_final) <- c(
        "Time Point",
        sprintf("%s Survival (%%)", group1_summary_label),
        sprintf("%s RMST (Years)", group1_summary_label),
        sprintf("%s Survival (%%)", group2_summary_label),
        sprintf("%s RMST (Years)", group2_summary_label),
        "RMST Diff (Years)",
        "RMST P-Value"
    )

    return(summary_df_final)
}

#' Build a publication-style RMST table (e.g., Supplemental Table 1)
#'
#' Takes the long-form RMST results and reshapes them so each row corresponds to a
#' metric (PBT RMST, GKSRS RMST, p-value, difference) and each column corresponds
#' to a time horizon. The resulting data frame mirrors the layout of Supplemental
#' Table 1 in the manuscript draft.
#'
#' @param rmst_results Data frame returned from RMST calculations, containing
#'   per-horizon estimates. Must include the columns
#'   `Time_Point_Years`, `RMST_Group1_Years`, `RMST_Group2_Years`,
#'   `RMST_P_Value`, and `RMST_Difference_Months`.
#' @param group1_label Display name for treatment group 1 (defaults to "PBT").
#' @param group2_label Display name for treatment group 2 (defaults to "GKSRS").
#' @param display_unit Unit used for RMST rows and the difference row. Accepted
#'   values are "months" or "years"; the default is "months" so everything is
#'   on the same scale as RMST Difference (months).
#' @param digits_rmst Number of decimal places to show for RMST estimates.
#' @param digits_diff Decimal places for the difference row.
#' @param digits_p Decimal places for p-values. Values smaller than the
#'   corresponding power-of-ten threshold are formatted as "<0.01", etc.
#' @return A tibble where the first column is `Treatment Group` (row labels) and
#'   subsequent columns correspond to horizons (`1-year`, `3-year`, ...). Returns
#'   an empty tibble if rmst_results is NULL/empty.
build_rmst_timepoint_table <- function(
        rmst_results,
        group1_label = "PBT",
        group2_label = "GKSRS",
        display_unit = c("months", "years"),
        digits_rmst = 2,
        digits_diff = 2,
        digits_p = 2) {

    display_unit <- match.arg(display_unit)
    if (is.null(rmst_results) || nrow(rmst_results) == 0) {
        return(tibble::tibble())
    }

    required_cols <- c(
        "Time_Point_Years",
        "RMST_Group1_Years",
        "RMST_Group2_Years",
        "RMST_Group1_Months",
        "RMST_Group2_Months",
        "RMST_P_Value",
        "RMST_Difference_Months"
    )
    missing_cols <- setdiff(required_cols, names(rmst_results))
    if (length(missing_cols) > 0) {
        stop(sprintf(
            "RMST table cannot be built; missing columns: %s",
            paste(missing_cols, collapse = ", ")
        ))
    }

    format_fixed <- function(values, digits) {
        vapply(values, function(val) {
            if (is.na(val)) {
                return("NA")
            }
            formatC(round(val, digits), format = "f", digits = digits)
        }, character(1))
    }

    format_p_value <- function(values, digits) {
        cutoff <- 10^(-digits)
        vapply(values, function(val) {
            if (is.na(val)) {
                return("NA")
            }
            if (val < cutoff) {
                return(sprintf("<%.*f", digits, cutoff))
            }
            formatC(round(val, digits), format = "f", digits = digits)
        }, character(1))
    }

    format_rmst_cell <- function(status, skip_reason, value, digits, skipped_label = "Skipped") {
        if (identical(status, "skipped")) {
            return(skipped_label)
        }
        if (identical(status, "failed")) {
            return("Unexpected failure")
        }
        format_fixed(value, digits)
    }

    format_rmst_p_value_cell <- function(status, skip_reason, value, digits) {
        if (identical(status, "skipped")) {
            return(paste0("Skipped: ", skip_reason %||% "not feasible"))
        }
        if (identical(status, "failed")) {
            return("Unexpected failure")
        }
        format_p_value(value, digits)
    }

    if (!"Analysis_Status" %in% names(rmst_results)) {
        rmst_results$Analysis_Status <- "completed"
    }
    if (!"Skip_Reason" %in% names(rmst_results)) {
        rmst_results$Skip_Reason <- NA_character_
    }

    table_data <- rmst_results %>%
        dplyr::arrange(Time_Point_Years) %>%
        dplyr::mutate(
            Analysis_Status = dplyr::coalesce(.data$Analysis_Status, "completed"),
            Time_Label = paste0(Time_Point_Years, "-year"),
            Group1_Value = dplyr::case_when(
                display_unit == "months" ~ RMST_Group1_Months,
                TRUE ~ RMST_Group1_Years
            ),
            Group2_Value = dplyr::case_when(
                display_unit == "months" ~ RMST_Group2_Months,
                TRUE ~ RMST_Group2_Years
            ),
            Diff_Value = dplyr::case_when(
                display_unit == "months" ~ RMST_Difference_Months,
                TRUE ~ RMST_Difference_Months / 12
            )
        )

    unit_label <- ifelse(display_unit == "months", "months", "years")
    time_levels <- unique(table_data$Time_Label)

    long_rows <- dplyr::bind_rows(
        tibble::tibble(
            Row_Label = sprintf("%s (%s)", group1_label, unit_label),
            Time_Label = table_data$Time_Label,
            Value = vapply(
                seq_len(nrow(table_data)),
                function(i) format_rmst_cell(
                    status = table_data$Analysis_Status[[i]],
                    skip_reason = table_data$Skip_Reason[[i]],
                    value = table_data$Group1_Value[[i]],
                    digits = digits_rmst
                ),
                character(1)
            )
        ),
        tibble::tibble(
            Row_Label = sprintf("%s (%s)", group2_label, unit_label),
            Time_Label = table_data$Time_Label,
            Value = vapply(
                seq_len(nrow(table_data)),
                function(i) format_rmst_cell(
                    status = table_data$Analysis_Status[[i]],
                    skip_reason = table_data$Skip_Reason[[i]],
                    value = table_data$Group2_Value[[i]],
                    digits = digits_rmst
                ),
                character(1)
            )
        ),
        tibble::tibble(
            Row_Label = sprintf("RMST Difference (%s)", unit_label),
            Time_Label = table_data$Time_Label,
            Value = vapply(
                seq_len(nrow(table_data)),
                function(i) format_rmst_cell(
                    status = table_data$Analysis_Status[[i]],
                    skip_reason = table_data$Skip_Reason[[i]],
                    value = table_data$Diff_Value[[i]],
                    digits = digits_diff
                ),
                character(1)
            )
        ),
        tibble::tibble(
            Row_Label = "RMST P-Value",
            Time_Label = table_data$Time_Label,
            Value = vapply(
                seq_len(nrow(table_data)),
                function(i) format_rmst_p_value_cell(
                    status = table_data$Analysis_Status[[i]],
                    skip_reason = table_data$Skip_Reason[[i]],
                    value = table_data$RMST_P_Value[[i]],
                    digits = digits_p
                ),
                character(1)
            )
        )
    ) %>%
        dplyr::mutate(Time_Label = factor(Time_Label, levels = time_levels))

    long_rows %>%
        tidyr::pivot_wider(names_from = Time_Label, values_from = Value) %>%
        dplyr::rename(`Treatment Group` = Row_Label)
}

# survRM2::rmst2 returns a 3x4 matrix with columns "Est.", "lower .95",
# "upper .95", and "p" (verified via synthetic test data on 2025-11-21).
# This helper grabs the requested CI bound while gracefully handling renamed
# columns (e.g., if alpha changes and the "95" label shifts) and missing rows.
extract_rmst_ci <- function(result_matrix, bound = c("lower", "upper")) {
    bound <- match.arg(bound)
    if (is.null(result_matrix) || nrow(result_matrix) == 0) {
        return(NA_real_)
    }

    matched_cols <- grep(
        pattern = paste0("^", bound),
        x = colnames(result_matrix),
        ignore.case = TRUE,
        value = TRUE
    )
    if (length(matched_cols) == 0) {
        return(NA_real_)
    }

    suppressWarnings(as.numeric(result_matrix[1, matched_cols[1]]))
}

#' Initialize an empty RMST result table
#'
#' @return Data frame with the canonical RMST output schema, including analysis
#'   status and skip metadata columns.
initialize_rmst_results <- function() {
    data.frame(
        Time_Point_Years = numeric(),
        Time_Point_Months = numeric(),
        Analysis_Status = character(),
        Skip_Reason = character(),
        Group1_Name = character(),
        RMST_Group1 = numeric(),
        RMST_Group1_Months = numeric(),
        RMST_Group1_Years = numeric(),
        Group2_Name = character(),
        RMST_Group2 = numeric(),
        RMST_Group2_Months = numeric(),
        RMST_Group2_Years = numeric(),
        RMST_Difference = numeric(),
        RMST_Difference_Months = numeric(),
        RMST_Difference_Years = numeric(),
        RMST_Difference_Lower_Months = numeric(),
        RMST_Difference_Upper_Months = numeric(),
        RMST_Difference_Lower_Years = numeric(),
        RMST_Difference_Upper_Years = numeric(),
        RMST_P_Value = numeric(),
        Analysis_Type = character(),
        stringsAsFactors = FALSE
    )
}

#' Build one RMST result row with explicit feasibility metadata
#'
#' @param time_years Numeric RMST horizon in years.
#' @param time_point Numeric RMST horizon in months.
#' @param analysis_status Character scalar such as `completed`, `skipped`, or
#'   `failed`.
#' @param skip_reason Optional character scalar describing why a timepoint was
#'   skipped.
#' @param group1_name Character label for group 1.
#' @param group2_name Character label for group 2.
#' @param rmst_group1_months Numeric RMST estimate for group 1 in months.
#' @param rmst_group2_months Numeric RMST estimate for group 2 in months.
#' @param rmst_diff_months Numeric between-group RMST difference in months.
#' @param ci_lower_months Numeric lower confidence bound in months.
#' @param ci_upper_months Numeric upper confidence bound in months.
#' @param rmst_p_value Numeric RMST p-value.
#' @param analysis_type Character descriptor of the RMST analysis.
#' @return One-row data frame matching the canonical RMST output schema.
build_rmst_result_row <- function(time_years, time_point, analysis_status, skip_reason = NA_character_,
                                  group1_name = NA_character_, group2_name = NA_character_,
                                  rmst_group1_months = NA_real_, rmst_group2_months = NA_real_,
                                  rmst_diff_months = NA_real_, ci_lower_months = NA_real_,
                                  ci_upper_months = NA_real_, rmst_p_value = NA_real_,
                                  analysis_type = NA_character_) {
    data.frame(
        Time_Point_Years = time_years,
        Time_Point_Months = time_point,
        Analysis_Status = analysis_status,
        Skip_Reason = skip_reason,
        Group1_Name = group1_name,
        RMST_Group1 = rmst_group1_months,
        RMST_Group1_Months = rmst_group1_months,
        RMST_Group1_Years = ifelse(is.na(rmst_group1_months), NA_real_, round(rmst_group1_months / 12, 2)),
        Group2_Name = group2_name,
        RMST_Group2 = rmst_group2_months,
        RMST_Group2_Months = rmst_group2_months,
        RMST_Group2_Years = ifelse(is.na(rmst_group2_months), NA_real_, round(rmst_group2_months / 12, 2)),
        RMST_Difference = rmst_diff_months,
        RMST_Difference_Months = rmst_diff_months,
        RMST_Difference_Years = ifelse(is.na(rmst_diff_months), NA_real_, round(rmst_diff_months / 12, 3)),
        RMST_Difference_Lower_Months = ci_lower_months,
        RMST_Difference_Upper_Months = ci_upper_months,
        RMST_Difference_Lower_Years = ifelse(is.na(ci_lower_months), NA_real_, round(ci_lower_months / 12, 3)),
        RMST_Difference_Upper_Years = ifelse(is.na(ci_upper_months), NA_real_, round(ci_upper_months / 12, 3)),
        RMST_P_Value = rmst_p_value,
        Analysis_Type = analysis_type,
        stringsAsFactors = FALSE
    )
}

#' Assess whether RMST can be fit at a requested horizon
#'
#' @param data Data frame containing time, event, and grouping columns.
#' @param time_var Character name of the follow-up time column in months.
#' @param event_var Character name of the event indicator column.
#' @param group_var Character name of the grouping column.
#' @param time_point Numeric requested RMST horizon in months.
#' @return Named list containing `status`, `skip_reason`, filtered `data`, and
#'   grouping metadata for downstream RMST fitting.
assess_rmst_feasibility <- function(data, time_var, event_var, group_var, time_point) {
    complete_data <- data %>%
        dplyr::filter(
            !is.na(.data[[time_var]]),
            !is.na(.data[[event_var]]),
            !is.na(.data[[group_var]])
        ) %>%
        as.data.frame()

    if (nrow(complete_data) == 0) {
        return(list(
            status = "skipped",
            skip_reason = "no_complete_case_data",
            data = complete_data
        ))
    }

    if (group_var == "treatment_group") {
        complete_data[[group_var]] <- factor(
            as.character(complete_data[[group_var]]),
            levels = TREATMENT_FACTOR_LEVELS
        )
    } else {
        complete_data[[group_var]] <- coerce_to_factor_preserving_levels(complete_data[[group_var]])
    }
    complete_data[[group_var]] <- droplevels(complete_data[[group_var]])
    factor_levels <- levels(complete_data[[group_var]])

    if (length(factor_levels) < 2) {
        return(list(
            status = "skipped",
            skip_reason = "insufficient_groups_after_filtering",
            data = complete_data,
            factor_levels = factor_levels
        ))
    }
    if (length(factor_levels) != 2) {
        return(list(
            status = "skipped",
            skip_reason = "non_binary_grouping",
            data = complete_data,
            factor_levels = factor_levels
        ))
    }

    max_followup_by_group <- tapply(
        complete_data[[time_var]],
        complete_data[[group_var]],
        max,
        na.rm = TRUE
    )
    max_followup_by_group <- max_followup_by_group[!is.na(max_followup_by_group)]
    if (length(max_followup_by_group) != 2) {
        return(list(
            status = "skipped",
            skip_reason = "missing_group_followup",
            data = complete_data,
            factor_levels = factor_levels
        ))
    }

    feasible_tau <- min(max_followup_by_group)
    if (time_point > feasible_tau) {
        return(list(
            status = "skipped",
            skip_reason = sprintf("tau_exceeds_followup_minimum(%.1f>%.1f)", time_point, feasible_tau),
            data = complete_data,
            factor_levels = factor_levels,
            feasible_tau = feasible_tau
        ))
    }

    list(
        status = "completed",
        skip_reason = NA_character_,
        data = complete_data,
        factor_levels = factor_levels,
        group_binary = ifelse(complete_data[[group_var]] == factor_levels[2], 1, 0)
    )
}

#' Write a text artifact explaining why RMST summary outputs were skipped
#'
#' @param rmst_dir Directory where the skip note should be written.
#' @param prefix Filename prefix for the output artifact.
#' @param ylab Character outcome label used in filenames and explanatory text.
#' @param rmst_results RMST result table containing status metadata.
#' @return `NULL`, invoked for its side effect of writing a text file.
write_rmst_skip_artifact <- function(rmst_dir, prefix, ylab, rmst_results) {
    completed_rows <- rmst_results %>%
        dplyr::filter(Analysis_Status == "completed")
    skipped_rows <- rmst_results %>%
        dplyr::filter(Analysis_Status == "skipped")

    note_lines <- c(
        "RMST Summary Not Generated",
        "",
        paste0("Outcome: ", ylab),
        paste0("Completed timepoints: ", nrow(completed_rows)),
        paste0("Skipped timepoints: ", nrow(skipped_rows)),
        "",
        "RMST plots and combined summary tables were not created because no timepoint passed the feasibility screen.",
        "See the companion RMST analysis workbook for timepoint-level skip reasons."
    )

    if (nrow(skipped_rows) > 0) {
        skip_lines <- sprintf(
            "  - %s-year: %s",
            skipped_rows$Time_Point_Years,
            skipped_rows$Skip_Reason
        )
        note_lines <- c(note_lines, "", "Skip reasons:", skip_lines)
    }

    note_path <- file.path(rmst_dir, paste0(prefix, make_filename_safe(ylab), "_rmst_summary_skipped.txt"))
    writeLines(note_lines, note_path)
    logger::log_info(sprintf("RMST skip note saved: %s", basename(note_path)))
}

determine_survival_output_dir <- function(ylab, output_dirs) {
    if (is.null(output_dirs)) {
        return(getwd())
    }

    default_dir <- output_dirs$baseline_characteristics %||% getwd()

    if (grepl("Overall Survival", ylab) && !is.null(output_dirs$obj1_os)) {
        return(output_dirs$obj1_os)
    }
    if (grepl("Progression-Free Survival", ylab) && !is.null(output_dirs$obj1_pfs)) {
        return(output_dirs$obj1_pfs)
    }
    if (grepl("PFS-2", ylab) && !is.null(output_dirs$obj3_pfs2)) {
        return(output_dirs$obj3_pfs2)
    }
    if (grepl("Metastasis-Free Survival", ylab)) {
        if (!is.null(output_dirs$obj4_mfs)) {
            return(output_dirs$obj4_mfs)
        }
        if (!is.null(output_dirs$obj1_pfs)) {
            return(output_dirs$obj1_pfs)
        }
        logger::log_warn("Output directory for Metastasis-Free Survival not provided; using baseline_characteristics as fallback")
    }

    default_dir
}

#' Summarize censoring and follow-up support for a survival endpoint
#'
#' Builds an overall and optional by-group support table for time-to-event
#' outputs, including event counts, censored counts, censoring percentage, and
#' follow-up distribution. The table is intended for diagnostics workbooks and
#' interpretation guardrails rather than hypothesis testing.
#'
#' @param data Data frame containing survival time, event, and optional group columns.
#' @param time_var Character scalar naming the follow-up time column in months.
#' @param event_var Character scalar naming the event indicator column.
#' @param group_var Optional character scalar naming the grouping column.
#' @param horizon_months Optional numeric reporting horizon used to count short follow-up.
#' @return Tibble with censoring/follow-up support rows, or an empty tibble if
#'   required inputs are unavailable.
build_survival_censoring_support <- function(data,
                                             time_var,
                                             event_var,
                                             group_var = NULL,
                                             horizon_months = NULL) {
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0 ||
        !time_var %in% names(data) || !event_var %in% names(data)) {
        return(tibble::tibble())
    }

    analysis_data <- data %>%
        dplyr::filter(!is.na(.data[[time_var]]), !is.na(.data[[event_var]])) %>%
        dplyr::mutate(
            .event_value = coerce_binary_outcome_vector(.data[[event_var]]),
            .follow_up_time = as.numeric(.data[[time_var]])
        ) %>%
        dplyr::filter(!is.na(.event_value), !is.na(.follow_up_time))

    if (nrow(analysis_data) == 0) {
        return(tibble::tibble())
    }

    summarize_support <- function(df, scope, group) {
        n_total <- nrow(df)
        n_events <- sum(df$.event_value == 1, na.rm = TRUE)
        n_censored <- sum(df$.event_value == 0, na.rm = TRUE)
        tibble::tibble(
            scope = scope,
            group = group,
            analyzable_n = n_total,
            second_recurrence_events = n_events,
            censored_n = n_censored,
            censoring_percent = round(100 * n_censored / n_total, 1),
            median_follow_up_months = round(stats::median(df$.follow_up_time, na.rm = TRUE), 2),
            q1_follow_up_months = round(stats::quantile(df$.follow_up_time, 0.25, na.rm = TRUE), 2),
            q3_follow_up_months = round(stats::quantile(df$.follow_up_time, 0.75, na.rm = TRUE), 2),
            min_follow_up_months = round(min(df$.follow_up_time, na.rm = TRUE), 2),
            max_follow_up_months = round(max(df$.follow_up_time, na.rm = TRUE), 2),
            below_horizon_n = if (!is.null(horizon_months)) {
                sum(df$.follow_up_time < horizon_months, na.rm = TRUE)
            } else {
                NA_integer_
            }
        )
    }

    support <- summarize_support(analysis_data, "overall", "All patients")

    if (!is.null(group_var) && group_var %in% names(analysis_data)) {
        group_support <- analysis_data %>%
            dplyr::mutate(.support_group = dplyr::case_when(
                is.na(.data[[group_var]]) ~ "Missing",
                TRUE ~ as.character(.data[[group_var]])
            )) %>%
            dplyr::group_split(.support_group) %>%
            purrr::map_dfr(function(group_df) {
                summarize_support(group_df, "by_treatment", group_df$.support_group[[1]])
            })

        support <- dplyr::bind_rows(support, group_support)
    }

    support
}

#' Assess PFS-2 censoring support against interpretation guardrails
#'
#' Converts the PFS-2 censoring support table into pass/downgrade guardrails for
#' heavy censoring, insufficient follow-up at the reporting horizon, and
#' between-arm censoring imbalance.
#'
#' @param censoring_support Tibble returned by `build_survival_censoring_support()`.
#' @param horizon_months Numeric reporting horizon in months.
#' @return List with `status`, `notes`, and `guardrail_table` for diagnostics
#'   and narrative interpretation.
assess_pfs2_censoring_support <- function(censoring_support,
                                          horizon_months = PFS2_REPORT_HORIZON_MONTHS) {
    if (is.null(censoring_support) || nrow(censoring_support) == 0) {
        return(list(
            status = "unavailable",
            notes = "Censoring support could not be assessed.",
            guardrail_table = tibble::tibble(
                guardrail = "censoring_support",
                status = "unavailable",
                detail = "Censoring support could not be assessed."
            )
        ))
    }

    overall <- censoring_support %>%
        dplyr::filter(.data$scope == "overall") %>%
        dplyr::slice_head(n = 1)
    by_treatment <- censoring_support %>%
        dplyr::filter(.data$scope == "by_treatment")

    heavy_censoring <- nrow(overall) > 0 &&
        !is.na(overall$censoring_percent[[1]]) &&
        overall$censoring_percent[[1]] >= 100 * PFS2_HEAVY_CENSORING_THRESHOLD
    short_follow_up <- nrow(overall) > 0 &&
        !is.na(overall$median_follow_up_months[[1]]) &&
        overall$median_follow_up_months[[1]] < horizon_months
    censoring_imbalance <- nrow(by_treatment) >= 2 &&
        diff(range(by_treatment$censoring_percent, na.rm = TRUE)) >= 100 * PFS2_CENSORING_IMBALANCE_THRESHOLD
    overall_censoring_percent <- if (nrow(overall) > 0) overall$censoring_percent[[1]] else NA_real_
    overall_median_follow_up <- if (nrow(overall) > 0) overall$median_follow_up_months[[1]] else NA_real_

    # These guardrails downgrade interpretation rather than suppressing KM/RMST:
    # censoring affects reliability, but it does not make censoring-aware curves invalid.
    detail_rows <- tibble::tibble(
        guardrail = c("heavy_censoring", "short_follow_up", "imbalanced_censoring"),
        status = c(
            if (heavy_censoring) "downgrade" else "pass",
            if (short_follow_up) "downgrade" else "pass",
            if (censoring_imbalance) "downgrade" else "pass"
        ),
        detail = c(
            sprintf(
                "Overall censoring is %s%%; downgrade threshold is %s%%.",
                overall_censoring_percent,
                100 * PFS2_HEAVY_CENSORING_THRESHOLD
            ),
            sprintf(
                "Median follow-up is %s months; reported PFS-2 horizon is %s months.",
                overall_median_follow_up,
                horizon_months
            ),
            if (nrow(by_treatment) >= 2) {
                sprintf(
                    "Treatment-arm censoring ranges from %.1f%% to %.1f%%.",
                    min(by_treatment$censoring_percent, na.rm = TRUE),
                    max(by_treatment$censoring_percent, na.rm = TRUE)
                )
            } else {
                "Treatment-arm censoring imbalance could not be assessed with fewer than two arms."
            }
        )
    )

    downgrade_reasons <- detail_rows$guardrail[detail_rows$status == "downgrade"]
    if (length(downgrade_reasons) == 0) {
        notes <- "Censoring and follow-up support do not trigger Objective 3 downgrade guardrails."
        status <- "supported"
    } else {
        notes <- paste(
            "Interpret PFS-2 treatment comparisons cautiously because support guardrails were triggered:",
            paste(downgrade_reasons, collapse = ", ")
        )
        status <- "downgraded"
    }

    list(
        status = status,
        notes = notes,
        guardrail_table = detail_rows
    )
}

#' Assess whether PFS-2 Cox treatment comparison is estimable
#'
#' Checks treatment-arm event support before fitting a Cox treatment comparison.
#' KM/RMST outputs can still be produced when Cox treatment effects are not
#' reportable, but the Cox artifact should be explicitly skipped with a reason.
#'
#' @param data PFS-2 analysis data after sparse treatment exclusions.
#' @param group_var Character scalar naming the salvage-treatment group column.
#' @param event_var Character scalar naming the PFS-2 event indicator column.
#' @return List with `reportable`, `reason`, `support`, and the Cox reference
#'   group when it can be identified.
assess_pfs2_treatment_estimability <- function(data,
                                               group_var = "recurrence1_treatment_clean",
                                               event_var = "pfs2_event") {
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0 ||
        !group_var %in% names(data) || !event_var %in% names(data)) {
        return(list(
            reportable = FALSE,
            reason = "PFS-2 Cox treatment comparison was skipped because treatment/event columns were unavailable.",
            support = tibble::tibble()
        ))
    }

    support <- data %>%
        dplyr::filter(!is.na(.data[[group_var]]), !is.na(.data[[event_var]])) %>%
        dplyr::mutate(
            treatment_group = as.character(.data[[group_var]]),
            event_value = coerce_binary_outcome_vector(.data[[event_var]])
        ) %>%
        dplyr::filter(!is.na(event_value)) %>%
        dplyr::group_by(treatment_group) %>%
        dplyr::summarise(
            n = dplyr::n(),
            events = sum(event_value == 1, na.rm = TRUE),
            censored = sum(event_value == 0, na.rm = TRUE),
            .groups = "drop"
        )

    if (nrow(support) < 2) {
        return(list(
            reportable = FALSE,
            reason = "PFS-2 Cox treatment comparison was skipped because fewer than two salvage-treatment groups were analyzable.",
            support = support
        ))
    }

    # Preserve the model reference arm from existing factor levels; using
    # alphabetical factor coercion here would silently change the Cox contrast.
    observed_groups <- unique(as.character(stats::na.omit(data[[group_var]])))
    group_levels <- get_stable_factor_levels(data[[group_var]])
    group_levels <- group_levels[group_levels %in% observed_groups]
    reference_group <- if (length(group_levels) > 0) group_levels[[1]] else support$treatment_group[[1]]
    reference_row <- support %>% dplyr::filter(.data$treatment_group == reference_group)

    if (nrow(reference_row) == 0 || reference_row$events[[1]] == 0) {
        return(list(
            reportable = FALSE,
            reason = sprintf(
                "PFS-2 Cox treatment comparison was skipped because the reference salvage-treatment arm `%s` had zero second-recurrence events.",
                reference_group
            ),
            support = support,
            reference_group = reference_group
        ))
    }

    contrast_support <- support %>% dplyr::filter(.data$treatment_group != reference_group)
    if (nrow(contrast_support) == 0 || all(contrast_support$events == 0)) {
        return(list(
            reportable = FALSE,
            reason = "PFS-2 Cox treatment comparison was skipped because no non-reference treatment contrast had observed second-recurrence events.",
            support = support,
            reference_group = reference_group
        ))
    }

    list(
        reportable = TRUE,
        reason = "PFS-2 Cox treatment comparison passed treatment-arm event-support guardrails.",
        support = support,
        reference_group = reference_group
    )
}

#' Build standardized skip diagnostics for survival and Cox outputs
#'
#' @param data Data frame representing the modeled survival dataset.
#' @param event_var Character scalar naming the event indicator column.
#' @param variables Character vector of modeled variables to summarize.
#' @param analysis_name Character scalar analysis identifier.
#' @param dataset_name Character scalar dataset identifier.
#' @param reason Character scalar one-line explanation.
#' @param narrative_lines Character vector of explanatory bullets.
#' @param filter_stats Optional list summarizing pre-model exclusions.
#' @param sparse_level_diagnostics Optional data frame of sparse-level exclusions.
#' @param modeled_n Integer modeled sample size.
#' @param status Character scalar such as `"skipped"` or `"unavailable"`.
#' @param time_var Optional character scalar naming the follow-up time column.
#'
#' @return Named list compatible with the shared skip-report renderer.
build_survival_skip_diagnostics <- function(data,
                                            event_var,
                                            variables,
                                            analysis_name,
                                            dataset_name,
                                            reason,
                                            narrative_lines,
                                            filter_stats = NULL,
                                            sparse_level_diagnostics = NULL,
                                            modeled_n = nrow(data),
                                            status = "skipped",
                                            time_var = NULL) {
    sample_size_summary <- build_sample_size_summary_tab(
        filter_stats = filter_stats,
        dataset_name = dataset_name,
        analysis_name = analysis_name,
        modeled_n = modeled_n
    )
    total_events <- if (!is.null(data) && is.data.frame(data) && event_var %in% names(data)) {
        sum(coerce_binary_outcome_vector(data[[event_var]]) == 1, na.rm = TRUE)
    } else {
        NA_integer_
    }
    follow_up_range <- if (!is.null(time_var) && !is.null(data) && is.data.frame(data) && time_var %in% names(data) && any(!is.na(data[[time_var]]))) {
        sprintf("%.2f to %.2f", min(data[[time_var]], na.rm = TRUE), max(data[[time_var]], na.rm = TRUE))
    } else {
        ""
    }
    censoring_support <- if (!is.null(time_var)) {
        build_survival_censoring_support(
            data = data,
            time_var = time_var,
            event_var = event_var,
            group_var = NULL
        )
    } else {
        tibble::tibble()
    }
    overall_censoring <- if (nrow(censoring_support) > 0) {
        censoring_support %>%
            dplyr::filter(.data$scope == "overall") %>%
            dplyr::slice_head(n = 1)
    } else {
        tibble::tibble()
    }
    censored_count <- if (nrow(overall_censoring) > 0) overall_censoring$censored_n[[1]] else NA_integer_
    censoring_percent <- if (nrow(overall_censoring) > 0) overall_censoring$censoring_percent[[1]] else NA_real_
    median_follow_up <- if (nrow(overall_censoring) > 0) overall_censoring$median_follow_up_months[[1]] else NA_real_

    build_skip_report_diagnostics(
        status = status,
        analysis_name = analysis_name,
        dataset_name = dataset_name,
        reason = reason,
        narrative_lines = narrative_lines,
        sample_size_summary = sample_size_summary,
        skip_summary = build_skip_summary_tab(list(
            modeled_n = modeled_n,
            total_events = total_events,
            censored_n = censored_count,
            censoring_percent = censoring_percent,
            median_follow_up_months = median_follow_up
        )),
        sparse_level_diagnostics = sparse_level_diagnostics,
        event_support = build_level_support_tab(data, variables, outcome_var = event_var),
        model_context = build_model_context_tab(list(
            event_var = event_var,
            time_var = time_var %||% "",
            follow_up_range = follow_up_range,
            censored_n = censored_count,
            censoring_percent = censoring_percent,
            median_follow_up_months = median_follow_up
        )),
        raw_model_output = paste(narrative_lines, collapse = " ")
    )
}

summarize_cox_hr <- function(model, dataset_name, analysis_label, model_label, group_var, data_source_label) {
    summarize_effect_model(
        model = model,
        dataset_name = dataset_name,
        analysis_label = analysis_label,
        model_label = model_label,
        group_var = group_var,
        data_source_label = data_source_label,
        effect_measure = "HR"
    )
}

#' Analyze time-to-event outcomes (KM + Cox)
#' @param data Data frame
#' @param time_var Time variable
#' @param event_var Event indicator
#' @param group_var Grouping variable (default 'treatment_group')
#' @param confounders Confounders
#' @param ylab Plot y-axis label
#' @param analysis_type 'post_treatment_only' or 'all_patients'
#' @param dataset_name Dataset label
#' @param legend_labels Optional legend labels
#' @param output_dirs Output directories by analysis type
#' @param prefix File prefix for outputs
#' @return List with KM/cox outputs and diagnostics
analyze_time_to_event_outcomes <- function(data, time_var, event_var, group_var = "treatment_group", model_group_var = group_var, confounders = NULL, ylab = "Survival Probability", analysis_type = "post_treatment_only", dataset_name = NULL, legend_labels = NULL, output_dirs = NULL, prefix = NULL, risk_table_height = 0.18, risk_table_rel_heights = c(0.78, 0.22), risk_table_y_expand = c(0.18, 0.18), saved_plot_height = NULL, allow_cox = TRUE, cox_skip_reason = NULL, cox_skip_narrative = NULL) {
    data <- normalize_treatment_group_data(data)
    plot_group_var <- group_var
    palette_group_var <- group_var

    # Check that there are at least two groups for analysis; otherwise, skip Cox model
    if (length(unique(data[[plot_group_var]])) < 2) {
        warning(sprintf("Only one level of %s present; skipping cox model.", plot_group_var))
        skip_output_dir <- if (!is.null(output_dirs)) {
            ensure_output_dir(resolve_obj4_output_dir(output_dirs, determine_survival_output_dir(ylab, output_dirs), "cox"))
        } else {
            "test_output"
        }
        early_skip_diagnostics <- build_survival_skip_diagnostics(
            data = data,
            event_var = event_var,
            variables = plot_group_var,
            analysis_name = paste0(ylab, "_cox"),
            dataset_name = dataset_name %||% "unspecified_dataset",
            reason = sprintf(
                "Cox regression was skipped because only one `%s` group was present in the analysis dataset.",
                plot_group_var
            ),
            narrative_lines = c(
                sprintf(
                    "The incoming analysis dataset contains only one observed `%s` level.",
                    plot_group_var
                ),
                "A Cox model requires at least two comparison groups."
            ),
            modeled_n = nrow(data),
            status = "skipped",
            time_var = time_var
        )
        save_skipped_model_outputs(
            analysis_name = paste0(ylab, "_cox"),
            dataset_name = dataset_name %||% "unspecified_dataset",
            output_dir = skip_output_dir,
            prefix = prefix %||% "",
            reason = early_skip_diagnostics$reason,
            diagnostics = early_skip_diagnostics
        )
        return(list(
            fit = NULL,
            plot = NULL,
            median_times = NULL,
            cox_model = NULL,
            cox_table = NULL,
            diagnostics = early_skip_diagnostics
        ))
    }

    # Filter data based on analysis type
    fix_event_data <- if (analysis_type == "post_treatment_only") {
        data %>% dplyr::filter(!!sym(time_var) >= 0)
    } else if (analysis_type == "all_patients") {
        data
    } else {
        stop(sprintf("Invalid analysis_type: %s", analysis_type))
    }

    # Ensure factors are not ordered (for plotting/analysis consistency)
    fix_event_data <- enforce_unordered_factors(fix_event_data)
    confounders_to_use <- confounders

    # Construct survival formula for KM and Cox
    surv_formula <- as.formula(
        paste0("Surv(", time_var, ", ", event_var, ") ~ ", plot_group_var)
    )
    model_surv_formula <- as.formula(
        paste0("Surv(", time_var, ", ", event_var, ") ~ ", model_group_var)
    )

    # Select relevant columns for KM/RMST analysis (retain "Other" rows)
    km_data <- fix_event_data %>%
        dplyr::select(all_of(c(time_var, event_var, plot_group_var)))

    model_data <- fix_event_data %>%
        dplyr::select(all_of(c(time_var, event_var, model_group_var, confounders_to_use)))

    if (nrow(km_data) == 0 || length(unique(stats::na.omit(km_data[[plot_group_var]]))) < 2) {
        logger::log_warn(formatted(
            "Insufficient data available for Kaplan-Meier fit; skipping survival analysis.",
            indent = 1
        ))
        skip_output_dir <- if (!is.null(output_dirs)) {
            ensure_output_dir(resolve_obj4_output_dir(output_dirs, determine_survival_output_dir(ylab, output_dirs), "cox"))
        } else {
            "test_output"
        }
        skip_diagnostics <- build_survival_skip_diagnostics(
            data = km_data,
            event_var = event_var,
            variables = plot_group_var,
            analysis_name = paste0(ylab, "_cox"),
            dataset_name = dataset_name %||% "unspecified_dataset",
            reason = "Survival analysis was skipped because the Kaplan-Meier dataset did not retain enough rows or grouping variation.",
            narrative_lines = c(
                sprintf(
                    "The Kaplan-Meier dataset retained %d rows after filtering.",
                    nrow(km_data)
                ),
                sprintf(
                    "A survival analysis requires at least two non-missing `%s` groups with analyzable follow-up.",
                    plot_group_var
                )
            ),
            modeled_n = nrow(km_data),
            status = "skipped",
            time_var = time_var
        )
        save_skipped_model_outputs(
            analysis_name = paste0(ylab, "_cox"),
            dataset_name = dataset_name %||% "unspecified_dataset",
            output_dir = skip_output_dir,
            prefix = prefix %||% "",
            reason = skip_diagnostics$reason,
            diagnostics = skip_diagnostics
        )
        empty_df <- data.frame()
        return(list(
            fit = NULL,
            plot = NULL,
            survival_rates = empty_df,
            survival_rates_wide = empty_df,
            rmst_analysis = empty_df,
            rmst_plot = NULL,
            cox_model = NULL,
            cox_table = NULL,
            ph_diagnostics = NULL,
            diagnostics = skip_diagnostics
        ))
    }

    survival_variables <- unique(c(model_group_var, confounders_to_use))
    cox_exclusion_result <- apply_sparse_level_exclusions(
        data = model_data,
        variables = survival_variables[survival_variables %in% names(model_data)],
        analysis_name = paste0(make_filename_safe(ylab), "_cox"),
        id_col = pick_sparse_level_id_col(model_data),
        level_exclusions = MODELING_LEVEL_EXCLUSIONS
    )

    if (cox_exclusion_result$removed_row_count > 0) {
        logger::log_info(formatted(sprintf(
            "Excluded %d rows with sparse categorical levels prior to Cox modeling (%s)",
            cox_exclusion_result$removed_row_count,
            paste(survival_variables, collapse = ", ")
        ), indent = 1))
    }

    cox_data <- cox_exclusion_result$data

    cox_ready <- isTRUE(allow_cox) && nrow(cox_data) > 0 && length(unique(stats::na.omit(cox_data[[model_group_var]]))) >= 2
    if (!cox_ready) {
        logger::log_warn(formatted(cox_skip_reason %||% "Cox model will be skipped: insufficient data after sparse-level exclusions.", indent = 1))
    }

    km_unadjusted_cox_model <- NULL
    unadjusted_ready <- isTRUE(allow_cox) && nrow(model_data) > 0 && length(unique(stats::na.omit(model_data[[model_group_var]]))) >= 2
    if (unadjusted_ready) {
        km_unadjusted_cox_model <- tryCatch({
            survival::coxph(model_surv_formula, data = model_data)
        }, error = function(e) {
            logger::log_error(sprintf("Unadjusted Cox model failed for %s: %s", ylab, e$message))
            NULL
        })
    } else {
        logger::log_warn(formatted(cox_skip_reason %||% "Unadjusted Cox model skipped: insufficient groups in KM dataset.", indent = 1))
    }

    cox_unadjusted_model <- NULL
    if (cox_ready) {
        cox_unadjusted_model <- tryCatch({
            survival::coxph(model_surv_formula, data = cox_data)
        }, error = function(e) {
            logger::log_error(sprintf("Unadjusted Cox (Cox data) model failed for %s: %s", ylab, e$message))
            NULL
        })
    }

    # Fit Kaplan-Meier survival curves
    surv_fit <- survival::survfit(surv_formula, data = km_data)
    surv_fit$call$formula <- surv_formula

    # Set up time axis breaks (in months) with legacy cap to avoid extreme tails
    raw_max_time <- max(km_data[[time_var]], na.rm = TRUE)
    max_time <- min(raw_max_time, SURVIVAL_XAXIS_MAX_MONTHS)
    base_by <- if (max_time <= 60) 6 else 12
    x_breaks <- seq(0, ceiling(max_time / base_by) * base_by, by = base_by)

    clean_strata_label <- function(x) {
        x_chr <- as.character(x)
        ifelse(grepl("=", x_chr), sub("^[^=]*=", "", x_chr), x_chr)
    }

    fit_strata_order <- names(surv_fit$strata)
    fit_strata_order <- unique(stats::na.omit(clean_strata_label(fit_strata_order)))
    if (length(fit_strata_order) == 0) {
        fit_strata_order <- unique(stats::na.omit(as.character(km_data[[plot_group_var]])))
    }

    # Set legend labels and color palette (centralized)
    if (is.null(legend_labels)) {
        legend_labels <- fit_strata_order
    }
    legend_labels <- unique(as.character(legend_labels))

    legend_labels_are_group_names <- all(fit_strata_order %in% legend_labels)
    legend_order_is_relabel_only <- length(legend_labels) == length(fit_strata_order) &&
        setequal(legend_labels, fit_strata_order)
    if (legend_order_is_relabel_only && !identical(legend_labels, fit_strata_order)) {
        logger::log_info(formatted(
            "Risk table display order differs from survfit strata order; reordering rows to keep counts attached to the correct labels.",
            indent = 1
        ))
    }

    display_strata_order <- if (legend_order_is_relabel_only) {
        legend_labels
    } else {
        present_requested_levels <- legend_labels[legend_labels %in% fit_strata_order]
        unique(c(present_requested_levels, setdiff(fit_strata_order, present_requested_levels)))
    }
    if (length(display_strata_order) == 0) {
        display_strata_order <- fit_strata_order
    }

    legend_labels_for_fit <- if (legend_order_is_relabel_only || legend_labels_are_group_names) {
        fit_strata_order
    } else if (length(legend_labels) == length(fit_strata_order)) {
        legend_labels
    } else {
        fit_strata_order
    }
    color_palette <- get_palette_by_variable(
        palette_group_var,
        unique(c(display_strata_order, legend_labels_for_fit))
    )
    # Identify strata requiring de-emphasis (thinner line/partial transparency)
    deemphasised_levels <- intersect(display_strata_order, c("GEP Failed/Indeterminate"))

    # Get plot scaling factor from config (allows global adjustment via SURVIVAL_PLOT_SCALE)
    plot_scale <- SURVIVAL_PLOT_SCALE

    n_risk_rows <- length(display_strata_order)

    remap_risk_table_rows <- function(table_frame) {
        if (is.null(table_frame) || nrow(table_frame) == 0) {
            return(table_frame)
        }

        mapped_labels <- NULL
        if ("strata" %in% names(table_frame)) {
            candidate_labels <- clean_strata_label(table_frame$strata)
            if (all(stats::na.omit(candidate_labels) %in% fit_strata_order)) {
                mapped_labels <- candidate_labels
            }
        }

        if (is.null(mapped_labels) && "y" %in% names(table_frame)) {
            y_as_integer <- suppressWarnings(as.integer(as.character(table_frame$y)))
            if (length(y_as_integer) > 0 && any(!is.na(y_as_integer))) {
                candidate_labels <- fit_strata_order[y_as_integer]
                if (all(stats::na.omit(candidate_labels) %in% fit_strata_order)) {
                    mapped_labels <- candidate_labels
                }
            }

            if (is.null(mapped_labels)) {
                candidate_labels <- clean_strata_label(table_frame$y)
                if (all(stats::na.omit(candidate_labels) %in% fit_strata_order)) {
                    mapped_labels <- candidate_labels
                }
            }
        }

        if (is.null(mapped_labels)) {
            return(table_frame)
        }

        row_levels <- display_strata_order
        mapped_factor <- factor(mapped_labels, levels = row_levels)

        if ("strata" %in% names(table_frame)) {
            table_frame$strata <- mapped_factor
        }
        if ("y" %in% names(table_frame)) {
            table_frame$y <- mapped_factor
        }

        ordering_time <- if ("time" %in% names(table_frame)) {
            table_frame$time
        } else if ("x" %in% names(table_frame)) {
            table_frame$x
        } else {
            seq_len(nrow(table_frame))
        }

        table_frame[order(mapped_factor, ordering_time), , drop = FALSE]
    }
    
    # Dynamically calculate risk table spacing based on number of rows
    # Principle: allocate ~3.5% of figure per row for the table, with minimum 0.15 and maximum 0.25
    # Then adjust row padding (y_expand) inversely: fewer rows = more padding, more rows = less padding
    if (is.null(risk_table_height) || risk_table_height == 0.18) {
        risk_table_height <- min(0.25, max(0.15, n_risk_rows * 0.035))
    }
    if (all(risk_table_rel_heights == c(0.78, 0.22))) {
        plot_fraction <- 1 - risk_table_height
        risk_table_rel_heights <- c(plot_fraction, risk_table_height)
    }
    if (all(risk_table_y_expand == c(0.18, 0.18))) {
        risk_table_y_expand <- c(
            max(0.05, 0.35 - n_risk_rows * 0.04),  # More rows = less top/bottom padding
            max(0.05, 0.35 - n_risk_rows * 0.04)
        )
    }

    # Generate Kaplan-Meier plot with risk table (all sizes scaled proportionally)
    surv_plot <- survminer::ggsurvplot(
        fit = surv_fit,
        data = km_data,
        palette = color_palette,
        risk.table = TRUE,
        conf.int = FALSE,
        pval = TRUE,
        pval.size = 6 * plot_scale,       # p-value text (scaled)
        title = paste("Kaplan-Meier Survival Curves", ylab, sep = "\n"),
        subtitle = if (!is.null(dataset_name)) paste("Cohort:", dataset_name) else NULL,
        xlab = "Time (months)",
        ylab = ylab,
        risk.table.height = risk_table_height,
        ggtheme = theme_minimal(),
        break.time.by = base_by,
        xlim = c(0, max(x_breaks)),
        ylim = c(0, 1),
        legend.labs = legend_labels_for_fit,
        risk.table.y.text = TRUE,
        tables.y.text = TRUE,
        risk.table.title = "Number at risk",
        font.x = 14 * plot_scale,         # x-axis label (scaled)
        font.y = 14 * plot_scale,         # y-axis label (scaled)
        font.tickslab = 12 * plot_scale,  # axis tick labels (scaled)
        font.legend = 14 * plot_scale,    # legend text (scaled)
        censor.size = 7 * plot_scale,     # censor tick marks (scaled larger)
        size = 1.2 * plot_scale           # survival line thickness (scaled larger)
    )

    surv_plot$plot <- remove_plot_scales(surv_plot$plot, aesthetics = c("colour", "color", "y"))
    surv_plot$table <- remove_plot_scales(surv_plot$table, aesthetics = c("y"))

    legend_override <- NULL
    if (length(deemphasised_levels) > 0) {
        if (!is.null(surv_plot$plot$data)) {
            surv_plot$plot$data <- surv_plot$plot$data %>%
                dplyr::mutate(
                    line_alpha = ifelse(clean_strata_label(as.character(strata)) %in% deemphasised_levels, 0.6, 1),
                    line_size = ifelse(clean_strata_label(as.character(strata)) %in% deemphasised_levels, 0.7, 1)
                )
        }
        if (length(surv_plot$plot$layers) > 0) {
            for (layer_idx in seq_along(surv_plot$plot$layers)) {
                layer_data <- surv_plot$plot$layers[[layer_idx]]$data
                if (!is.null(layer_data) && "strata" %in% names(layer_data)) {
                    surv_plot$plot$layers[[layer_idx]]$data <- layer_data %>%
                        dplyr::mutate(
                            line_alpha = ifelse(clean_strata_label(as.character(strata)) %in% deemphasised_levels, 0.6, 1),
                            line_size = ifelse(clean_strata_label(as.character(strata)) %in% deemphasised_levels, 0.7, 1)
                        )
                }
            }
        }

        surv_plot$plot <- surv_plot$plot +
            ggplot2::aes(alpha = line_alpha, size = line_size) +
            ggplot2::scale_color_manual(values = color_palette, breaks = display_strata_order, guide = "none") +
            ggplot2::scale_alpha_identity(guide = "none") +
            ggplot2::scale_size_identity(guide = "none")

        legend_override_alpha <- ifelse(display_strata_order %in% deemphasised_levels, 0.6, 1)
        legend_override_size <- ifelse(display_strata_order %in% deemphasised_levels, 0.7, 1)
        legend_override <- list(
            alpha = legend_override_alpha,
            size = legend_override_size,
            colour = color_palette[display_strata_order]
        )
    } else {
        surv_plot$plot <- surv_plot$plot +
            ggplot2::scale_color_manual(values = color_palette, breaks = display_strata_order, guide = "none")
    }

    legend_cols <- if (length(display_strata_order) > 4) 2 else 1
    has_linetype <- "linetype" %in% names(surv_plot$plot$mapping) || any(vapply(surv_plot$plot$layers, function(layer) "linetype" %in% names(layer$mapping), logical(1)))
    guide_params <- list(ncol = legend_cols, byrow = TRUE)
    if (!is.null(legend_override)) {
        legend_override$colour <- color_palette[display_strata_order]
        guide_params$override.aes <- legend_override
    }
    guide_args <- list(color = do.call(ggplot2::guide_legend, guide_params))
    if (has_linetype) {
        guide_args$linetype <- ggplot2::guide_legend(ncol = legend_cols, byrow = TRUE)
    }
    surv_plot$plot <- surv_plot$plot +
        do.call(ggplot2::guides, guide_args) +
        ggplot2::theme(
            legend.position = "bottom",
            legend.box = "vertical"
        )
    # Further increase text sizes for clarity in publication figures
    surv_plot$plot <- surv_plot$plot +
        ggplot2::theme(
            legend.text = ggplot2::element_text(size = 16 * plot_scale, color = "black"),
            legend.title = ggplot2::element_text(size = 16 * plot_scale, color = "black"),
            axis.title = ggplot2::element_text(size = 18 * plot_scale, color = "black"),
            axis.title.x = ggplot2::element_text(size = 18 * plot_scale, color = "black", face = "bold", 
                                                  margin = ggplot2::margin(t = 15, r = 0, b = 0, l = 0)),  # Push x-axis title further away
            axis.title.y = ggplot2::element_text(size = 18 * plot_scale, color = "black", face = "bold",
                                                  margin = ggplot2::margin(t = 0, r = 6, b = 0, l = 0)),
            axis.text = ggplot2::element_text(size = 14 * plot_scale, color = "black"),
            axis.text.x = ggplot2::element_text(color = "black"),
            axis.text.y = ggplot2::element_text(color = "black"),
            axis.line = ggplot2::element_blank(),      # Remove axis lines
            axis.ticks = ggplot2::element_blank(),     # Remove tick marks
            plot.title = ggplot2::element_text(size = 18 * plot_scale, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 14 * plot_scale),
            plot.margin = ggplot2::margin(t = 8, r = 8, b = 0, l = 0)
        )
    # Format y-axis as percent (keep after theme to preserve colors)
    surv_plot$plot <- surv_plot$plot +
        ggplot2::scale_y_continuous(
            limits = c(0, 1),
            breaks = seq(0, 1, by = 0.1),
            labels = function(x) x * 100,
            name = paste0(ylab, " (%)")
        ) +
        ggplot2::labs(x = "Time (months)") +  # Explicitly set x-axis label with black color via theme
        ggplot2::geom_hline(yintercept = 0.5, linetype = "solid", color = "black", linewidth = 0.9)  # 50% reference line
    # Make risk table text larger and easier to read
    surv_plot$table <- surv_plot$table + theme_minimal() +
        ggplot2::theme(
            axis.title = ggplot2::element_text(size = 12 * plot_scale, color = "black"),
            axis.text.y = ggplot2::element_text(size = 12 * plot_scale, color = "black"),
            axis.text.x = ggplot2::element_text(size = 12 * plot_scale, color = "black"),
            strip.text = ggplot2::element_text(size = 12 * plot_scale, color = "black"),
            plot.margin = ggplot2::margin(t = 8, r = 8, b = 0, l = 8)
        )
    
    # Increase the size of the actual numbers in the risk table
    if (length(surv_plot$table$layers) > 0) {
        for (i in seq_along(surv_plot$table$layers)) {
            if ("GeomText" %in% class(surv_plot$table$layers[[i]]$geom)) {
                surv_plot$table$layers[[i]]$aes_params$size <- 3.4 * plot_scale
            }

            if (!is.null(surv_plot$table$layers[[i]]$data)) {
                surv_plot$table$layers[[i]]$data <- remap_risk_table_rows(surv_plot$table$layers[[i]]$data)
            }
        }
    }

    surv_plot$table$data <- remap_risk_table_rows(surv_plot$table$data)

    # ggsurvplot builds the risk table with y = rev(strata), which flips the
    # row/count association after we reorder strata labels. Replace that mapping
    # so the rendered row positions use the remapped strata directly.
    surv_plot$table$mapping <- ggplot2::aes(
        x = time,
        y = strata,
        label = llabels,
        shape = strata
    )

    surv_plot$table <- surv_plot$table +
        ggplot2::scale_y_discrete(
            limits = rev(display_strata_order),
            expand = ggplot2::expansion(mult = risk_table_y_expand)
        )
    
    # Save KM plot if output_dirs are provided
    if (!is.null(output_dirs)) {
        output_dir <- determine_survival_output_dir(ylab, output_dirs)
        km_dir <- ensure_output_dir(resolve_obj4_output_dir(output_dirs, output_dir, "km"))
        km_path <- file.path(km_dir, paste0(prefix, make_filename_safe(ylab), "_km.png"))
        # Combine main plot and risk table vertically so the saved image includes both
        combined_km <- cowplot::plot_grid(
            surv_plot$plot,
            surv_plot$table,
            ncol = 1,
            align = "v",
            rel_heights = risk_table_rel_heights
        )
        # Dynamic height scaling: base on number of strata in the KM fit
        n_groups <- tryCatch(
            {
                length(surv_plot$plot$data$strata %||% levels(km_data[[plot_group_var]]))
            },
            error = function(e) length(levels(km_data[[plot_group_var]]))
        )
        # Calculate dynamic height based on number of strata
        extra_groups <- max(0, n_groups - 2)
        dynamic_height <- KM_BASE_HEIGHT + extra_groups * KM_HEIGHT_PER_STRATUM
        # Prefer taller PFS-2 default if applicable, but cap at KM_MAX_HEIGHT
        base_pref <- if (grepl("PFS-2", ylab)) max(PFS2_PLOT_HEIGHT, SURVIVAL_PLOT_HEIGHT) else SURVIVAL_PLOT_HEIGHT
        plot_height <- if (!is.null(saved_plot_height)) {
            min(KM_MAX_HEIGHT, saved_plot_height)
        } else {
            min(KM_MAX_HEIGHT, max(base_pref, dynamic_height))
        }
        # Save the combined plot with dynamic height
        ggplot2::ggsave(km_path, combined_km, width = SURVIVAL_PLOT_WIDTH, height = plot_height, dpi = PLOT_DPI, bg = "white")
        logger::log_info(sprintf("KM plot (with risk table) saved: %s", km_path))
    }

    # Define time points (in months) for summary and RMST
    time_points <- SURVIVAL_SUMMARY_TIMEPOINTS_YEARS * 12
    
    # Add debugging and error handling for the summary call
    logger::log_info(sprintf("DEBUG: Time points for summary: %s", paste(time_points, collapse = ", ")))
    logger::log_info(sprintf("DEBUG: Max time in data: %.2f", max(km_data[[time_var]], na.rm = TRUE)))
    logger::log_info(sprintf("DEBUG: Min time in data: %.2f", min(km_data[[time_var]], na.rm = TRUE)))
    
    # Filter time points to only include those within the data range to prevent "invalid 'times' argument" error
    max_data_time <- max(km_data[[time_var]], na.rm = TRUE)
    valid_time_points <- time_points[time_points <= max_data_time]
    
    if (length(valid_time_points) == 0) {
        logger::log_warn("No valid time points for summary - all requested times exceed data range")
        valid_time_points <- c(max_data_time)  # Use max data time as fallback
    }
    
    logger::log_info(sprintf("DEBUG: Valid time points for summary: %s", paste(valid_time_points, collapse = ", ")))

    # Summarize survival at key time points with error handling
    surv_summary <- tryCatch({
        summary(surv_fit, times = valid_time_points)
    }, error = function(e) {
        logger::log_error(sprintf("ERROR in surv_fit summary: %s", e$message))
        logger::log_error("This is likely the source of the 'invalid times argument' error")
        # Return NULL to prevent further errors
        NULL
    })
    rmst_unexpected_failures <- character()
    
    if (is.null(surv_summary)) {
        logger::log_warn("Survival summary failed - skipping summary statistics and RMST analysis")
        surv_rates <- data.frame(
            Treatment_Group = character(),
            Time_Years = numeric(),
            surv_pct = numeric(),
            lower_pct = numeric(),
            upper_pct = numeric(),
            stringsAsFactors = FALSE
        )
        rmst_results <- initialize_rmst_results()

        rmst_survival_summary <- data.frame()
    } else {
        surv_rates <- as.data.frame(surv_summary[c("strata", "time", "surv", "lower", "upper")]) %>%
            dplyr::mutate(
                Treatment_Group = sub(".*=", "", strata),
                Time_Years = round(time / 12, 1)
            ) %>%
            dplyr::mutate(
                across(c(surv, lower, upper), ~ round(100 * ., 1), .names = "{.col}_pct")
            ) %>%
            dplyr::select(Treatment_Group, Time_Years, surv_pct, lower_pct, upper_pct)

        # Initialize RMST results table
        rmst_data <- km_data
        rmst_results <- initialize_rmst_results()
        # Calculate RMST for each time point
        logger::log_info(sprintf("DEBUG: Starting RMST analysis for %d time points", length(valid_time_points)))
        for (time_point in valid_time_points) {
            time_years <- round(time_point / 12, 1)
            logger::log_info(sprintf("DEBUG: Processing RMST for %s years (%.1f months)", time_years, time_point))
            rmst_feasibility <- assess_rmst_feasibility(
                data = rmst_data,
                time_var = time_var,
                event_var = event_var,
                group_var = plot_group_var,
                time_point = time_point
            )

            if (!identical(rmst_feasibility$status, "completed")) {
                logger::log_info(sprintf(
                    "Skipping RMST at %.1f years for %s: %s",
                    time_years,
                    ylab,
                    rmst_feasibility$skip_reason
                ))
                rmst_results <- rbind(
                    rmst_results,
                    build_rmst_result_row(
                        time_years = time_years,
                        time_point = time_point,
                        analysis_status = "skipped",
                        skip_reason = rmst_feasibility$skip_reason,
                        analysis_type = paste0("Skipped RMST at ", time_years, " years")
                    )
                )
                next
            }

            rmst_complete_data <- rmst_feasibility$data
            factor_levels <- rmst_feasibility$factor_levels
            group1_name <- as.character(factor_levels[1])
            group2_name <- as.character(factor_levels[2])

            rmst_result <- tryCatch(
                {
                    logger::log_info(sprintf(
                        "DEBUG: Running RMST for binary comparison: %s (arm=0) vs %s (arm=1)",
                        group1_name,
                        group2_name
                    ))
                    rmst2(
                        time = rmst_complete_data[[time_var]],
                        status = rmst_complete_data[[event_var]],
                        arm = rmst_feasibility$group_binary,
                        tau = time_point
                    )
                },
                error = function(e) {
                    rmst_unexpected_failures <<- c(
                        rmst_unexpected_failures,
                        sprintf("%.1f-year:%s", time_years, e$message)
                    )
                    logger::log_error(sprintf("ERROR in RMST calculation for %.1f years: %s", time_years, e$message))
                    NULL
                }
            )

            if (is.null(rmst_result)) {
                rmst_results <- rbind(
                    rmst_results,
                    build_rmst_result_row(
                        time_years = time_years,
                        time_point = time_point,
                        analysis_status = "failed",
                        skip_reason = "unexpected_rmst_error",
                        group1_name = group1_name,
                        group2_name = group2_name,
                        analysis_type = paste0("Failed RMST at ", time_years, " years")
                    )
                )
                next
            }

            rmst_group1_months <- round(rmst_result$RMST.arm0$rmst[1], 2)
            rmst_group2_months <- round(rmst_result$RMST.arm1$rmst[1], 2)
            rmst_diff_months <- round(rmst_result$unadjusted.result[1, 1], 2)
            rmst_diff_months <- ifelse(abs(rmst_diff_months) < 1e-10, 0, rmst_diff_months)

            ci_lower_months <- extract_rmst_ci(rmst_result$unadjusted.result, bound = "lower")
            ci_upper_months <- extract_rmst_ci(rmst_result$unadjusted.result, bound = "upper")
            if (is.na(ci_lower_months)) {
                ci_lower_months <- rmst_diff_months
            }
            if (is.na(ci_upper_months)) {
                ci_upper_months <- rmst_diff_months
            }
            ci_lower_months <- round(ci_lower_months, 2)
            ci_upper_months <- round(ci_upper_months, 2)

            rmst_results <- rbind(
                rmst_results,
                build_rmst_result_row(
                    time_years = time_years,
                    time_point = time_point,
                    analysis_status = "completed",
                    group1_name = group1_name,
                    group2_name = group2_name,
                    rmst_group1_months = rmst_group1_months,
                    rmst_group2_months = rmst_group2_months,
                    rmst_diff_months = rmst_diff_months,
                    ci_lower_months = ci_lower_months,
                    ci_upper_months = ci_upper_months,
                    rmst_p_value = round(rmst_result$unadjusted.result[1, 4], 4),
                    analysis_type = paste0("Mean survival up to ", time_years, " years")
                )
            )
        }
    }
    completed_rmst_results <- if (exists("rmst_results", inherits = FALSE)) {
        rmst_results %>% dplyr::filter(Analysis_Status == "completed")
    } else {
        initialize_rmst_results()
    }
    rmst_survival_summary <- if (nrow(completed_rmst_results) > 0) {
        build_rmst_survival_summary(completed_rmst_results, surv_rates, group_var = plot_group_var)
    } else {
        data.frame()
    }

    rmst_timepoint_table <- tibble::tibble()
    if (nrow(completed_rmst_results) > 0) {
        first_label <- function(values, fallback) {
            valid_idx <- which(!is.na(values) & values != "")
            if (length(valid_idx) == 0) {
                return(fallback)
            }
            as.character(values[valid_idx[1]])
        }
        group1_label <- first_label(completed_rmst_results$Group1_Name, "Group 1")
        group2_label <- first_label(completed_rmst_results$Group2_Name, "Group 2")

        rmst_timepoint_table <- tryCatch(
            build_rmst_timepoint_table(
                rmst_results = completed_rmst_results,
                group1_label = group1_label,
                group2_label = group2_label,
                display_unit = "months",
                digits_rmst = 2,
                digits_diff = 2,
                digits_p = 2
            ),
            error = function(e) {
                logger::log_warn(sprintf("Unable to build RMST timepoint table: %s", e$message))
                tibble::tibble()
            }
        )
    }

    # Prepare wide-format survival rates for reporting
    surv_rates_wide <- surv_rates %>%
        dplyr::mutate(Time_Label = paste0(Time_Years, "-year")) %>%
        dplyr::select(Treatment_Group, Time_Label, surv_pct) %>%
        tidyr::pivot_wider(names_from = Time_Label, values_from = surv_pct)
    surv_rates_wide_char <- surv_rates_wide %>%
        dplyr::mutate(across(everything(), as.character))

    # Add RMST P-value and difference rows to wide table
    rmst_pvalue_row <- data.frame(Treatment_Group = "RMST P-Value", stringsAsFactors = FALSE)
    for (i in seq_len(nrow(rmst_results))) {
        time_label <- paste0(rmst_results$Time_Point_Years[i], "-year")
        p_val <- rmst_results$RMST_P_Value[i]
        if (time_label %in% names(surv_rates_wide)) {
            rmst_pvalue_row[[time_label]] <- if (identical(rmst_results$Analysis_Status[i], "skipped")) {
                paste0("Skipped: ", rmst_results$Skip_Reason[i])
            } else if (identical(rmst_results$Analysis_Status[i], "failed")) {
                "Unexpected failure"
            } else if (is.na(p_val)) {
                "NA"
            } else if (p_val < 0.0001) {
                "<0.0001"
            } else {
                sprintf("%.3f", p_val)
            }
        }
    }
    rmst_diff_row <- data.frame(Treatment_Group = "RMST Difference (months)", stringsAsFactors = FALSE)
    for (i in seq_len(nrow(rmst_results))) {
        time_label <- paste0(rmst_results$Time_Point_Years[i], "-year")
        # Column renamed to RMST_Difference_Months; guard for backward compatibility
        rmst_diff <- if ("RMST_Difference_Months" %in% names(rmst_results)) {
            rmst_results$RMST_Difference_Months[i]
        } else if ("RMST_Difference" %in% names(rmst_results)) {
            rmst_results$RMST_Difference[i]
        } else {
            NA_real_
        }
        if (time_label %in% names(surv_rates_wide)) {
            rmst_diff_row[[time_label]] <- if (identical(rmst_results$Analysis_Status[i], "skipped")) {
                "Skipped"
            } else if (identical(rmst_results$Analysis_Status[i], "failed")) {
                "Failed"
            } else if (is.na(rmst_diff)) {
                "NA"
            } else {
                sprintf("%.2f", rmst_diff)
            }
        }
    }
    surv_rates_wide_with_rmst <- dplyr::bind_rows(
        surv_rates_wide_char,
        rmst_pvalue_row,
        rmst_diff_row
    )

    # Write outputs to Excel files if output_dirs provided
    if (!is.null(output_dirs)) {
        output_dir <- determine_survival_output_dir(ylab, output_dirs)
        summary_dir <- ensure_output_dir(resolve_obj4_output_dir(output_dirs, output_dir, "summary"))
        writexl::write_xlsx(
            surv_rates,
            path = file.path(summary_dir, paste0(prefix, make_filename_safe(ylab), "_survival_rates.xlsx"))
        )
        writexl::write_xlsx(
            surv_rates_wide_with_rmst,
            path = file.path(summary_dir, paste0(prefix, make_filename_safe(ylab), "_survival_rates_wide.xlsx"))
        )
        rmst_dir <- ensure_output_dir(resolve_obj4_output_dir(output_dirs, output_dir, "rmst"))
        rmst_has_completed_results <- nrow(completed_rmst_results) > 0
        if (nrow(rmst_results) > 0) {
            writexl::write_xlsx(
                rmst_results,
                path = file.path(rmst_dir, paste0(prefix, make_filename_safe(ylab), "_rmst_analysis.xlsx"))
            )
            logger::log_info(sprintf("RMST analysis file saved: %s", paste0(prefix, make_filename_safe(ylab), "_rmst_analysis.xlsx")))
        } else {
            logger::log_info(sprintf("Skipping RMST file creation - no RMST rows generated for %s", ylab))
        }
        if (rmst_has_completed_results && nrow(rmst_survival_summary) > 0) {
            combined_path <- file.path(rmst_dir, paste0(prefix, make_filename_safe(ylab), "_survival_rmst_summary.xlsx"))
            writexl::write_xlsx(rmst_survival_summary, path = combined_path)
            logger::log_info(sprintf("Survival + RMST summary saved: %s", basename(combined_path)))
        } else if (nrow(rmst_results) > 0) {
            logger::log_info(sprintf(
                "Skipping survival + RMST summary for %s - no feasible RMST timepoints completed",
                ylab
            ))
            write_rmst_skip_artifact(rmst_dir, prefix, ylab, rmst_results)
        }
        if (rmst_has_completed_results && nrow(rmst_timepoint_table) > 0) {
            rmst_table_path <- file.path(rmst_dir, paste0(prefix, make_filename_safe(ylab), "_rmst_timepoint_table.xlsx"))
            writexl::write_xlsx(rmst_timepoint_table, path = rmst_table_path)
            logger::log_info(sprintf("RMST timepoint table saved: %s", basename(rmst_table_path)))
        } else if (nrow(rmst_results) > 0 && !rmst_has_completed_results) {
            logger::log_info(sprintf("RMST timepoint table skipped for %s - all timepoints were infeasible", ylab))
        }
    }

    # Run Cox regression and generate regression table
    cox_result <- NULL
    cox_analysis_name <- paste0(ylab, "_cox")
    if (cox_ready) {
        logger::log_info(sprintf("DEBUG: About to call generate_regression_table for %s", paste0(ylab, "_cox")))
        cox_result <- tryCatch({
            cox_dir <- if (!is.null(output_dirs)) {
                cox_output_dir <- determine_survival_output_dir(ylab, output_dirs)
                ensure_output_dir(resolve_obj4_output_dir(output_dirs, cox_output_dir, "cox"))
            } else {
                "test_output"
            }
            
            generate_regression_table(
                data = cox_data,
                outcome_var = event_var,
                predictor_vars = model_group_var,
                confounders = confounders_to_use,
                model_type = "cox",
                effect_measure = "HR",
                analysis_name = cox_analysis_name,
                dataset_name = dataset_name,
                output_dir = cox_dir,
                prefix = prefix,
                time_var = time_var,
                event_var = event_var,
                treatment_var = model_group_var,
                sparse_level_diagnostics = cox_exclusion_result$sparse_level_diagnostics,
                filter_stats = cox_exclusion_result$filter_stats
            )
        }, error = function(e) {
            logger::log_error(sprintf("ERROR in generate_regression_table: %s", e$message))
            return(NULL)
        })
    } else {
        cox_dir <- if (!is.null(output_dirs)) {
            ensure_output_dir(resolve_obj4_output_dir(output_dirs, determine_survival_output_dir(ylab, output_dirs), "cox"))
        } else {
            "test_output"
        }
        diagnostics_stub <- build_survival_skip_diagnostics(
            data = cox_data,
            event_var = event_var,
            variables = survival_variables,
            analysis_name = cox_analysis_name,
            dataset_name = dataset_name %||% "unspecified_dataset",
            reason = cox_skip_reason %||% "Cox regression was skipped because the post-exclusion survival dataset did not retain enough usable rows or group variation.",
            narrative_lines = cox_skip_narrative %||% c(
                sprintf(
                    "After sparse-level exclusions, %d rows remained in the Cox dataset.",
                    nrow(cox_data)
                ),
                sprintf(
                    "A Cox model requires at least two non-missing `%s` groups after exclusions.",
                    model_group_var
                )
            ),
            filter_stats = cox_exclusion_result$filter_stats,
            sparse_level_diagnostics = cox_exclusion_result$sparse_level_diagnostics,
            modeled_n = nrow(cox_data),
            status = "skipped",
            time_var = time_var
        )
        save_skipped_model_outputs(
            analysis_name = cox_analysis_name,
            dataset_name = dataset_name %||% "unspecified_dataset",
            output_dir = cox_dir,
            prefix = prefix %||% "",
            reason = diagnostics_stub$reason,
            diagnostics = diagnostics_stub
        )
        cox_result <- list(
            model = NULL,
            table = NULL,
            diagnostics = diagnostics_stub
        )
    }

    if (is.null(cox_result)) {
        cox_dir <- if (!is.null(output_dirs)) {
            ensure_output_dir(resolve_obj4_output_dir(output_dirs, determine_survival_output_dir(ylab, output_dirs), "cox"))
        } else {
            "test_output"
        }
        diagnostics_stub <- build_survival_skip_diagnostics(
            data = cox_data,
            event_var = event_var,
            variables = survival_variables,
            analysis_name = cox_analysis_name,
            dataset_name = dataset_name %||% "unspecified_dataset",
            reason = "Cox model fitting failed before a usable output could be generated.",
            narrative_lines = c(
                sprintf(
                    "Cox fitting was attempted for `%s`, but the shared regression layer returned no result.",
                    cox_analysis_name
                ),
                "Check the logs for the underlying fitting error or numerical failure."
            ),
            filter_stats = cox_exclusion_result$filter_stats,
            sparse_level_diagnostics = cox_exclusion_result$sparse_level_diagnostics,
            modeled_n = nrow(cox_data),
            status = "unavailable",
            time_var = time_var
        )
        save_skipped_model_outputs(
            analysis_name = cox_analysis_name,
            dataset_name = dataset_name %||% "unspecified_dataset",
            output_dir = cox_dir,
            prefix = prefix %||% "",
            reason = diagnostics_stub$reason,
            diagnostics = diagnostics_stub
        )
        cox_result <- list(
            model = NULL,
            table = NULL,
            diagnostics = diagnostics_stub
        )
    }

    hr_rows <- list(
        summarize_cox_hr(
            model = km_unadjusted_cox_model,
            dataset_name = dataset_name,
            analysis_label = ylab,
            model_label = "Unadjusted (KM data)",
            group_var = model_group_var,
            data_source_label = "KM dataset (before rare-category exclusions, no covariates)"
        ),
        summarize_cox_hr(
            model = cox_unadjusted_model,
            dataset_name = dataset_name,
            analysis_label = ylab,
            model_label = "Unadjusted (Cox data)",
            group_var = model_group_var,
            data_source_label = "Cox dataset (after rare-category exclusions, no covariates)"
        ),
        summarize_cox_hr(
            model = cox_result$model,
            dataset_name = dataset_name,
            analysis_label = ylab,
            model_label = "Adjusted Cox (confounders)",
            group_var = model_group_var,
            data_source_label = "Cox dataset (after rare-category exclusions, includes covariates)"
        )
    )
    hr_rows <- hr_rows[!vapply(hr_rows, is.null, logical(1))]
    hazard_ratio_summary <- if (length(hr_rows) > 0) do.call(bind_effect_summary_rows, hr_rows) else empty_effect_summary_rows()

    if (!is.null(output_dirs) && nrow(hazard_ratio_summary) > 0) {
        hr_output_dir <- determine_survival_output_dir(ylab, output_dirs)
        hr_dir <- ensure_output_dir(resolve_obj4_output_dir(output_dirs, hr_output_dir, "cox"))
        
        hr_filename <- paste0(prefix, make_filename_safe(ylab), "_effect_summary.xlsx")
        writexl::write_xlsx(hazard_ratio_summary, file.path(hr_dir, hr_filename))
        logger::log_info(sprintf("Effect summary saved: %s", hr_filename))
    }

    logger::log_info(sprintf(
        "DEBUG: RMST summary for %s - rows: %d, any valid RMST rows: %s",
        ylab,
        nrow(rmst_results),
        if (nrow(rmst_results) > 0) {
            any(rmst_results$Analysis_Status == "completed", na.rm = TRUE)
        } else {
            FALSE
        }
    ))

    # Return all results as a list
    list(
        fit = surv_fit,
        plot = surv_plot,
        survival_rates = surv_rates,
        survival_rates_wide = surv_rates_wide_with_rmst,
        rmst_analysis = rmst_results,
        rmst_survival_summary = rmst_survival_summary,
        rmst_timepoint_table = rmst_timepoint_table,
        rmst_plot = tryCatch({
            # Only generate RMST plot if we have any RMST rows; downstream handles missing pieces
            rmst_has_rows <- nrow(completed_rmst_results) > 0
            if (!rmst_has_rows) {
                logger::log_info(sprintf("Skipping RMST plot generation - no RMST rows available for %s", ylab))
                NULL
            } else {
                # Get group names for RMST plot - use levels() to match factor order
                factor_levels <- levels(km_data[[plot_group_var]])
                
                # If not a factor or no levels, fall back to unique values in sorted order
                if (is.null(factor_levels) || length(factor_levels) == 0) {
                    factor_levels <- sort(unique(km_data[[plot_group_var]]))
                }
                
                group1_name <- as.character(factor_levels[1])
                group2_name <- as.character(factor_levels[2])
                
                plot_rmst_pvalue_progression(completed_rmst_results, ylab, output_dirs, prefix, group1_name, group2_name, plot_group_var)
            }
        }, error = function(e) {
            logger::log_warn(sprintf("RMST plot generation failed: %s", e$message))
            NULL
        }),
        cox_model = cox_result$model,
        cox_table = cox_result$table,
        ph_diagnostics = NULL,
        diagnostics = cox_result$diagnostics,
        hazard_ratio_summary = hazard_ratio_summary,
        unexpected_failures = rmst_unexpected_failures %||% character()
    )
}

#' Overall survival stratified by local recurrence status (recurrence1)
#'
#' Thin wrapper around analyze_time_to_event_outcomes to keep recurrence-stratified
#' KM and summary outputs within the recurrence objective folder.
#'
#' @param data Analytic dataset with recurrence1, tt_death_months, death_event
#' @param dataset_name Cohort name for labeling
#' @param output_dirs Output directory list (obj1_recurrence will be used for files)
#' @param prefix File prefix (e.g., "full_cohort_")
#' @return Result list from analyze_time_to_event_outcomes
analyze_os_by_local_recurrence <- function(data, dataset_name, output_dirs, prefix, confounders = NULL) {
    required_cols <- c("recurrence1", "tt_death_months", "death_event")
    if (!all(required_cols %in% names(data))) {
        logger::log_warn(sprintf(
            "Recurrence-stratified OS skipped: missing columns %s",
            paste(setdiff(required_cols, names(data)), collapse = ", ")
        ))
        return(NULL)
    }

    recurrence_dir <- output_dirs$obj1_recurrence %||% output_dirs$obj1_os %||% getwd()
    os_subdir <- output_dirs$obj1_recurrence_1a1 %||% file.path(recurrence_dir, "1a1_recurrence_stratified_os")
    if (!dir.exists(os_subdir)) {
        dir.create(os_subdir, recursive = TRUE, showWarnings = FALSE)
        if (exists("USE_LOGS") && USE_LOGS) {
            logger::log_debug(formatted(sprintf("Created directory: %s", os_subdir)))
        }
    }
    logger::log_info(formatted(sprintf("Routing 1a1 recurrence OS outputs to %s", os_subdir), indent = 2))
    local_dirs <- output_dirs
    local_dirs$obj1_os <- os_subdir
    local_dirs$baseline_characteristics <- os_subdir

    analyze_time_to_event_outcomes(
        data = data,
        time_var = "tt_death_months",
        event_var = "death_event",
        group_var = "recurrence1",
        confounders = confounders,
        ylab = "Overall Survival by Local Recurrence Status",
        analysis_type = "post_treatment_only",
        dataset_name = dataset_name,
        output_dirs = local_dirs,
        prefix = paste0(prefix, "1a1_recurrence_stratified_")
    )
}

#' Progression-free survival stratified by local recurrence status (recurrence1)
#'
#' Mirrors analyze_os_by_local_recurrence but uses PFS endpoints to understand
#' how recurrence status impacts progression-free survival curves.
#'
#' @inheritParams analyze_os_by_local_recurrence
#' @return Result list from analyze_time_to_event_outcomes
analyze_pfs_by_local_recurrence <- function(data, dataset_name, output_dirs, prefix, confounders = NULL) {
    required_cols <- c("recurrence1", "tt_pfs_months", "pfs_event")
    if (!all(required_cols %in% names(data))) {
        logger::log_warn(sprintf(
            "Recurrence-stratified PFS skipped: missing columns %s",
            paste(setdiff(required_cols, names(data)), collapse = ", ")
        ))
        return(NULL)
    }

    recurrence_dir <- output_dirs$obj1_recurrence %||% output_dirs$obj1_pfs %||% getwd()
    pfs_subdir <- output_dirs$obj1_recurrence_1a2 %||% file.path(recurrence_dir, "1a2_recurrence_stratified_pfs")
    if (!dir.exists(pfs_subdir)) {
        dir.create(pfs_subdir, recursive = TRUE, showWarnings = FALSE)
        if (exists("USE_LOGS") && USE_LOGS) {
            logger::log_debug(formatted(sprintf("Created directory: %s", pfs_subdir)))
        }
    }
    logger::log_info(formatted(sprintf("Routing 1a2 recurrence PFS outputs to %s", pfs_subdir), indent = 2))
    local_dirs <- output_dirs
    local_dirs$obj1_pfs <- pfs_subdir
    local_dirs$baseline_characteristics <- pfs_subdir

    analyze_time_to_event_outcomes(
        data = data,
        time_var = "tt_pfs_months",
        event_var = "pfs_event",
        group_var = "recurrence1",
        confounders = confounders,
        ylab = "Progression-Free Survival by Local Recurrence Status",
        analysis_type = "post_treatment_only",
        dataset_name = dataset_name,
        output_dirs = local_dirs,
        prefix = paste0(prefix, "1a2_recurrence_stratified_")
    )
}

#' Overall survival stratified by metastatic progression status
#'
#' Mirrors recurrence helper but isolates OS curves by mets progression status.
analyze_os_by_metastatic_progression <- function(data, dataset_name, output_dirs, prefix, confounders = NULL) {
    required_cols <- c("mets_progression", "tt_death_months", "death_event")
    if (!all(required_cols %in% names(data))) {
        logger::log_warn(sprintf(
            "Metastasis-stratified OS skipped: missing columns %s",
            paste(setdiff(required_cols, names(data)), collapse = ", ")
        ))
        return(NULL)
    }

    mets_dir <- output_dirs$obj1_mets %||% output_dirs$obj1_os %||% getwd()
    os_subdir <- output_dirs$obj1_mets_2a1 %||% file.path(mets_dir, "2a1_metastasis_stratified_os")
    if (!dir.exists(os_subdir)) {
        dir.create(os_subdir, recursive = TRUE, showWarnings = FALSE)
        if (exists("USE_LOGS") && USE_LOGS) {
            logger::log_debug(formatted(sprintf("Created directory: %s", os_subdir)))
        }
    }
    logger::log_info(formatted(sprintf("Routing 2a1 metastasis OS outputs to %s", os_subdir), indent = 2))

    local_dirs <- output_dirs
    local_dirs$obj1_os <- os_subdir
    local_dirs$baseline_characteristics <- os_subdir

    analyze_time_to_event_outcomes(
        data = data,
        time_var = "tt_death_months",
        event_var = "death_event",
        group_var = "mets_progression",
        confounders = confounders,
        ylab = "Overall Survival by Metastatic Progression Status",
        analysis_type = "post_treatment_only",
        dataset_name = dataset_name,
        output_dirs = local_dirs,
        prefix = paste0(prefix, "2a1_metastasis_stratified_")
    )
}

#' Progression-free survival stratified by metastatic progression status
analyze_pfs_by_metastatic_progression <- function(data, dataset_name, output_dirs, prefix, confounders = NULL) {
    required_cols <- c("mets_progression", "tt_pfs_months", "pfs_event")
    if (!all(required_cols %in% names(data))) {
        logger::log_warn(sprintf(
            "Metastasis-stratified PFS skipped: missing columns %s",
            paste(setdiff(required_cols, names(data)), collapse = ", ")
        ))
        return(NULL)
    }

    mets_dir <- output_dirs$obj1_mets %||% output_dirs$obj1_pfs %||% getwd()
    pfs_subdir <- output_dirs$obj1_mets_2a2 %||% file.path(mets_dir, "2a2_metastasis_stratified_pfs")
    if (!dir.exists(pfs_subdir)) {
        dir.create(pfs_subdir, recursive = TRUE, showWarnings = FALSE)
        if (exists("USE_LOGS") && USE_LOGS) {
            logger::log_debug(formatted(sprintf("Created directory: %s", pfs_subdir)))
        }
    }
    logger::log_info(formatted(sprintf("Routing 2a2 metastasis PFS outputs to %s", pfs_subdir), indent = 2))

    local_dirs <- output_dirs
    local_dirs$obj1_pfs <- pfs_subdir
    local_dirs$baseline_characteristics <- pfs_subdir

    analyze_time_to_event_outcomes(
        data = data,
        time_var = "tt_pfs_months",
        event_var = "pfs_event",
        group_var = "mets_progression",
        confounders = confounders,
        ylab = "Progression-Free Survival by Metastatic Progression Status",
        analysis_type = "post_treatment_only",
        dataset_name = dataset_name,
        output_dirs = local_dirs,
        prefix = paste0(prefix, "2a2_metastasis_stratified_")
    )
}

# PFS-2 Analysis

#' Analyze second progression survival (PFS-2)
#'
#' Analyzes survival from second progression (PFS-2) for patients who experienced a first recurrence.
#'
#' @param data Data frame
#' @param confounders Character vector of confounder variable names
#' @param dataset_name Name of the dataset
#' @param output_dirs List of output directories organized by analysis type
#' @param prefix Character string used as a file prefix for output files
#' @details PFS-2 uses separate feasibility checks for analyzable patient count
#'   and second-recurrence event count before attempting survival modeling.
#' @return List with elements: pfs2_data (data frame), survival_analysis (list), summary_table (gtsummary object)
analyze_pfs2 <- function(data, confounders = NULL, dataset_name = NULL, output_dirs = NULL, prefix = NULL) {
    logger::log_info("Starting PFS-2 analysis for recurrent patients")

    # Filter to patients with valid PFS-2 data (variables now created in data processing)
    pfs2_data <- data %>%
        filter(
            !is.na(tt_pfs2_months),
            tt_pfs2_months >= 0,
            !is.na(recurrence1_treatment_clean)
        )

    logger::log_info(sprintf("Found %d patients with valid PFS-2 data", nrow(pfs2_data)))

    if (nrow(pfs2_data) == 0) {
        logger::log_info("No patients with valid PFS-2 data found")
        return(list(
            pfs2_data = NULL,
            survival_analysis = NULL,
            summary_table = NULL
        ))
    }

    # Capture raw salvage treatment distribution before filtering
    pfs2_data_raw <- pfs2_data
    raw_primary_vs_salvage <- pfs2_data_raw %>%
        mutate(
            primary_treatment = as.character(treatment_group),
            salvage_treatment = dplyr::case_when(
                is.na(recurrence1_treatment) | recurrence1_treatment == "" ~ "No Salvage Treatment Recorded",
                TRUE ~ as.character(recurrence1_treatment)
            )
        ) %>%
        group_by(primary_treatment, salvage_treatment) %>%
        summarise(
            n = n(),
            events = sum(pfs2_event, na.rm = TRUE),
            event_rate_pct = ifelse(n > 0, round(100 * events / n, 1), NA_real_),
            .groups = "drop"
        ) %>%
        arrange(primary_treatment, desc(n))

    exclusion_vars <- unique(c("recurrence1_treatment_clean", confounders))
    exclusion_result <- apply_sparse_level_exclusions(
        pfs2_data,
        variables = exclusion_vars[exclusion_vars %in% names(pfs2_data)],
        analysis_name = "pfs2_survival",
        id_col = pick_sparse_level_id_col(pfs2_data),
        level_exclusions = MODELING_LEVEL_EXCLUSIONS
    )
    if (exclusion_result$removed_row_count > 0) {
        logger::log_info(sprintf(
            "Excluded %d rows with sparse categorical levels prior to PFS-2 analysis",
            exclusion_result$removed_row_count
        ))
    }
    pfs2_data <- exclusion_result$data

    # Summarise treatment distribution and write to file
    treatment_counts <- table(pfs2_data$recurrence1_treatment_clean)
    logger::log_info("Treatment distribution:")
    print(treatment_counts)

    model_primary_vs_salvage <- pfs2_data %>%
        mutate(
            primary_treatment = as.character(treatment_group),
            salvage_treatment = dplyr::case_when(
                is.na(recurrence1_treatment_clean) | recurrence1_treatment_clean == "" ~ "No Salvage Treatment Recorded",
                TRUE ~ as.character(recurrence1_treatment_clean)
            )
        ) %>%
        group_by(primary_treatment, salvage_treatment) %>%
        summarise(
            n = n(),
            events = sum(pfs2_event, na.rm = TRUE),
            event_rate_pct = ifelse(n > 0, round(100 * events / n, 1), NA_real_),
            .groups = "drop"
        ) %>%
        arrange(primary_treatment, desc(n))

    pfs2_censoring_support <- build_survival_censoring_support(
        data = pfs2_data,
        time_var = "tt_pfs2_months",
        event_var = "pfs2_event",
        group_var = "recurrence1_treatment_clean",
        horizon_months = PFS2_REPORT_HORIZON_MONTHS
    )
    pfs2_interpretation_guardrails <- assess_pfs2_censoring_support(pfs2_censoring_support)
    pfs2_treatment_estimability <- assess_pfs2_treatment_estimability(
        data = pfs2_data,
        group_var = "recurrence1_treatment_clean",
        event_var = "pfs2_event"
    )

    if (!is.null(output_dirs) && !is.null(output_dirs$obj3_pfs2)) {
        summary_path <- file.path(output_dirs$obj3_pfs2, paste0(prefix, "pfs2_treatment_summary.xlsx"))
        writexl::write_xlsx(
            list(
                raw_primary_vs_salvage = raw_primary_vs_salvage,
                model_primary_vs_salvage = model_primary_vs_salvage,
                censoring_support = pfs2_censoring_support,
                interpretation_guardrails = pfs2_interpretation_guardrails$guardrail_table,
                treatment_estimability = pfs2_treatment_estimability$support
            ),
            summary_path
        )
        logger::log_info(sprintf("PFS-2 treatment summary saved to %s", summary_path))
    }

    logger::log_info(sprintf("Final PFS-2 analysis dataset: %d patients", nrow(pfs2_data)))
    total_events <- sum(pfs2_data$pfs2_event, na.rm = TRUE)
    logger::log_info(sprintf("PFS-2 events (2nd recurrence): %d", total_events))

    write_pfs2_skip_outputs <- function(reason, narrative_lines, explanation_text, status = "skipped") {
        pfs2_skip_diagnostics <- build_survival_skip_diagnostics(
            data = pfs2_data,
            event_var = "pfs2_event",
            variables = unique(c("recurrence1_treatment_clean", confounders)),
            analysis_name = "pfs2_analysis",
            dataset_name = dataset_name %||% "unspecified_dataset",
            reason = reason,
            narrative_lines = c(narrative_lines, pfs2_interpretation_guardrails$notes),
            filter_stats = exclusion_result$filter_stats,
            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
            modeled_n = nrow(pfs2_data),
            status = status,
            time_var = "tt_pfs2_months"
        )
        pfs2_skip_diagnostics$compatibility_text <- explanation_text

        if (!is.null(output_dirs)) {
            output_targets <- list(output_dirs$obj3_pfs2, output_dirs$obj3_ph_diagnostics)
            for (target_dir in output_targets) {
                if (!is.null(target_dir) && dir.exists(target_dir)) {
                    explanation_file <- file.path(target_dir, paste0(prefix, "pfs2_analysis_skipped_explanation.txt"))
                    writeLines(explanation_text, explanation_file)
                    logger::log_info(sprintf("Explanation saved to: %s", explanation_file))
                    save_skipped_model_outputs(
                        analysis_name = "pfs2_analysis",
                        dataset_name = dataset_name %||% "unspecified_dataset",
                        output_dir = target_dir,
                        prefix = prefix,
                        reason = pfs2_skip_diagnostics$reason,
                        diagnostics = pfs2_skip_diagnostics
                    )
                }
            }
        }

        pfs2_skip_diagnostics
    }

    # Check if we have enough patients and events for analysis
    if (nrow(pfs2_data) < MINIMUM_PFS2_PATIENTS) {
        logger::log_info(sprintf(
            "Insufficient patients for PFS-2 analysis (minimum %d required)",
            MINIMUM_PFS2_PATIENTS
        ))
        explanation_text <- sprintf(
            "PFS-2 Analysis Skipped - Insufficient Patients

            The Issue:
            %s cohort: %d patients total
            PFS-2 eligible patients: %d patients
            Minimum required: %d analyzable patients for survival analysis

            Analysis was skipped because there are insufficient analyzable patients to perform a meaningful PFS-2 survival analysis.

            This is expected behavior for cohorts with limited recurrence data and does not indicate an error.",
            tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name))),
            nrow(data),
            nrow(pfs2_data),
            MINIMUM_PFS2_PATIENTS
        )
        pfs2_skip_diagnostics <- write_pfs2_skip_outputs(
            reason = sprintf(
                "PFS-2 survival analysis was skipped because only %d analyzable patients were available; at least %d are required.",
                nrow(pfs2_data),
                MINIMUM_PFS2_PATIENTS
            ),
            narrative_lines = c(
                sprintf(
                    "%s cohort contained %d total patients, with %d PFS-2-eligible patients.",
                    tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name))),
                    nrow(data),
                    nrow(pfs2_data)
                ),
                sprintf(
                    "Only %d analyzable PFS-2 patients were available; the minimum requirement is %d.",
                    nrow(pfs2_data),
                    MINIMUM_PFS2_PATIENTS
                )
            ),
            explanation_text = explanation_text
        )
        pfs2_survival <- list(
            fit = NULL,
            plot = NULL,
            survival_rates = NULL,
            cox_model = NULL,
            cox_table = NULL,
            diagnostics = pfs2_skip_diagnostics,
            censoring_support = pfs2_censoring_support,
            interpretation_guardrails = pfs2_interpretation_guardrails,
            treatment_estimability = pfs2_treatment_estimability
        )
        return(list(
            pfs2_data = pfs2_data,
            survival_analysis = pfs2_survival,
            summary_table = NULL,
            raw_primary_vs_salvage = raw_primary_vs_salvage,
            model_primary_vs_salvage = model_primary_vs_salvage,
            censoring_support = pfs2_censoring_support,
            interpretation_guardrails = pfs2_interpretation_guardrails,
            treatment_estimability = pfs2_treatment_estimability,
            ph_diagnostics = NULL
        ))
    }

    # Check if we have enough events for survival analysis

    if (total_events < MINIMUM_SURVIVAL_EVENTS) {
        logger::log_error("ERROR: Insufficient events for PFS-2 survival analysis")
        logger::log_info(sprintf(
            "Total events: %d (minimum %d required)",
            total_events,
            MINIMUM_SURVIVAL_EVENTS
        ))
        logger::log_info("Skipping survival analysis due to insufficient data")

        # Create explanation text file for skipped analysis
        explanation_text <- sprintf(
            "PFS-2 Analysis Skipped - Insufficient Events

            The Issue:
            %s cohort: %d patients total
            PFS-2 eligible patients: %d patients (those with first recurrence)
            PFS-2 events: %d patients (second recurrence)
            Minimum required: %d events for survival analysis

            Analysis was skipped because there are insufficient events (%d) to perform a meaningful survival analysis. 
            The minimum requirement of %d events ensures statistical validity and reliable results.

            This is expected behavior for cohorts with limited recurrence data and does not indicate an error.",
            tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name))),
            nrow(data),
            nrow(pfs2_data),
            total_events,
            MINIMUM_SURVIVAL_EVENTS,
            total_events,
            MINIMUM_SURVIVAL_EVENTS
        )

        pfs2_skip_diagnostics <- build_survival_skip_diagnostics(
            data = pfs2_data,
            event_var = "pfs2_event",
            variables = unique(c("recurrence1_treatment_clean", confounders)),
            analysis_name = "pfs2_analysis",
            dataset_name = dataset_name %||% "unspecified_dataset",
            reason = sprintf(
                "PFS-2 survival analysis was skipped because only %d events were observed; at least %d are required.",
                total_events,
                MINIMUM_SURVIVAL_EVENTS
            ),
            narrative_lines = c(
                sprintf(
                    "%s cohort contained %d total patients, with %d PFS-2-eligible patients.",
                    tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name))),
                    nrow(data),
                    nrow(pfs2_data)
                ),
                sprintf(
                    "Only %d second-recurrence events were observed; the minimum requirement is %d events for a meaningful survival analysis.",
                    total_events,
                    MINIMUM_SURVIVAL_EVENTS
                ),
                "This is expected for cohorts with limited recurrence data and does not indicate a pipeline error.",
                pfs2_interpretation_guardrails$notes
            ),
            filter_stats = exclusion_result$filter_stats,
            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
            modeled_n = nrow(pfs2_data),
            status = "skipped",
            time_var = "tt_pfs2_months"
        )
        pfs2_skip_diagnostics$compatibility_text <- explanation_text

        # Save explanation to both a_pfs2 and b_proportional_hazards_diagnostics directories
        if (!is.null(output_dirs)) {
            # Save to a_pfs2 directory
            pfs2_dir <- output_dirs$obj3_pfs2
            if (!is.null(pfs2_dir) && dir.exists(pfs2_dir)) {
                explanation_file <- file.path(pfs2_dir, paste0(prefix, "pfs2_analysis_skipped_explanation.txt"))
                writeLines(explanation_text, explanation_file)
                logger::log_info(sprintf("Explanation saved to: %s", explanation_file))
                save_skipped_model_outputs(
                    analysis_name = "pfs2_analysis",
                    dataset_name = dataset_name %||% "unspecified_dataset",
                    output_dir = pfs2_dir,
                    prefix = prefix,
                    reason = pfs2_skip_diagnostics$reason,
                    diagnostics = pfs2_skip_diagnostics
                )
            }
            
            # Save to b_proportional_hazards_diagnostics directory
            ph_dir <- output_dirs$obj3_ph_diagnostics
            if (!is.null(ph_dir) && dir.exists(ph_dir)) {
                explanation_file <- file.path(ph_dir, paste0(prefix, "pfs2_analysis_skipped_explanation.txt"))
                writeLines(explanation_text, explanation_file)
                logger::log_info(sprintf("Explanation saved to: %s", explanation_file))
                save_skipped_model_outputs(
                    analysis_name = "pfs2_analysis",
                    dataset_name = dataset_name %||% "unspecified_dataset",
                    output_dir = ph_dir,
                    prefix = prefix,
                    reason = pfs2_skip_diagnostics$reason,
                    diagnostics = pfs2_skip_diagnostics
                )
            }
        }

        pfs2_survival <- list(
            fit = NULL,
            plot = NULL,
            survival_rates = NULL,
            cox_model = NULL,
            cox_table = NULL,
            diagnostics = pfs2_skip_diagnostics,
            censoring_support = pfs2_censoring_support,
            interpretation_guardrails = pfs2_interpretation_guardrails,
            treatment_estimability = pfs2_treatment_estimability
        )
    } else {
        # Use existing analyze_time_to_event_outcomes function with dynamic legend labels
        # Perfect separation handling is already implemented in fit_regression_model()
        logger::log_info("Performing PFS-2 survival analysis")
        pfs2_survival <- analyze_time_to_event_outcomes(
            data = pfs2_data,
            time_var = "tt_pfs2_months",
            event_var = "pfs2_event",
            group_var = "recurrence1_treatment_clean",
            confounders = confounders,
            ylab = "PFS-2 Probability (Freedom from 2nd Recurrence)",
            analysis_type = "all_patients", # PFS-2 analysis includes all recurrent patients
            dataset_name = paste0(dataset_name, "_pfs2_recurrent"),
            output_dirs = output_dirs,
            prefix = prefix,
            allow_cox = isTRUE(pfs2_treatment_estimability$reportable),
            cox_skip_reason = if (!isTRUE(pfs2_treatment_estimability$reportable)) pfs2_treatment_estimability$reason else NULL,
            cox_skip_narrative = if (!isTRUE(pfs2_treatment_estimability$reportable)) {
                c(
                    pfs2_treatment_estimability$reason,
                    pfs2_interpretation_guardrails$notes
                )
            } else {
                NULL
            }
        )
    }

    pfs2_survival$censoring_support <- pfs2_censoring_support
    pfs2_survival$interpretation_guardrails <- pfs2_interpretation_guardrails
    pfs2_survival$treatment_estimability <- pfs2_treatment_estimability

    logger::log_info("PFS-2 analysis completed")

    ph_diag_result <- run_or_skip_proportional_hazards_diagnostics(
        cox_model = pfs2_survival$cox_model,
        outcome_name = "PFS-2 Probability (Freedom from 2nd Recurrence)",
        output_dir = if (!is.null(output_dirs)) output_dirs$obj3_ph_diagnostics else getwd(),
        file_prefix = paste0(prefix, make_filename_safe("PFS-2 Probability (Freedom from 2nd Recurrence)"), "_"),
        dataset_name = dataset_name,
        data = pfs2_data,
        time_var = "tt_pfs2_months",
        event_var = "pfs2_event",
        variables = c("recurrence1_treatment_clean", confounders),
        reason = paste(
            "PFS-2 proportional hazards diagnostics were not run because no Cox model was fit.",
            pfs2_treatment_estimability$reason %||% "The Cox model was unavailable."
        ),
        narrative_lines = pfs2_interpretation_guardrails$notes,
        filter_stats = exclusion_result$filter_stats,
        sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
        modeled_n = nrow(pfs2_data)
    )

    return(list(
        pfs2_data = pfs2_data,
        survival_analysis = pfs2_survival,
        summary_table = pfs2_survival$cox_table, # Use the standardized table from generate_regression_table
        raw_primary_vs_salvage = raw_primary_vs_salvage,
        model_primary_vs_salvage = model_primary_vs_salvage,
        censoring_support = pfs2_censoring_support,
        interpretation_guardrails = pfs2_interpretation_guardrails,
        treatment_estimability = pfs2_treatment_estimability,
        ph_diagnostics = ph_diag_result
    ))
}


#' Run PH diagnostics or write an explicit skip artifact
#'
#' Proportional hazards diagnostics require a fitted Cox model. This orchestration
#' helper centralizes the "model unavailable" decision so objectives do not
#' silently leave PH folders empty when Cox fitting is skipped or fails. When a
#' valid Cox model is supplied, event-floor checks, Schoenfeld residual tests, and
#' diagnostic plot generation are delegated to
#' `test_proportional_hazards_assumption()`.
#'
#' @param cox_model Optional fitted `coxph` model.
#' @param outcome_name Character outcome label for logs and artifacts.
#' @param output_dir Directory where PH diagnostics or skip artifacts are saved.
#' @param file_prefix Prefix used for PH diagnostic files.
#' @param dataset_name Optional dataset label.
#' @param data Optional survival-analysis data used to build skip diagnostics.
#' @param time_var Optional follow-up time column for skip diagnostics.
#' @param event_var Optional event indicator column for skip diagnostics.
#' @param variables Character vector of modeled variables for event-support diagnostics.
#' @param reason Optional skip reason when no Cox model is available.
#' @param narrative_lines Optional additional skip explanation lines.
#' @param filter_stats Optional sample-size audit details.
#' @param sparse_level_diagnostics Optional sparse-level exclusion diagnostics.
#' @param modeled_n Optional modeled sample size.
#'
#' @return PH diagnostic results when testing is feasible. If no Cox model is
#'   available, returns skip diagnostics after writing skipped HTML/workbook
#'   artifacts. If a Cox model is available but PH testing is not feasible, the
#'   delegated `test_proportional_hazards_assumption()` return value records that
#'   skip or unavailability reason.
run_or_skip_proportional_hazards_diagnostics <- function(cox_model,
                                                         outcome_name = "Survival",
                                                         output_dir = NULL,
                                                         file_prefix = "",
                                                         dataset_name = NULL,
                                                         data = NULL,
                                                         time_var = NULL,
                                                         event_var = NULL,
                                                         variables = character(),
                                                         reason = NULL,
                                                         narrative_lines = NULL,
                                                         filter_stats = NULL,
                                                         sparse_level_diagnostics = NULL,
                                                         modeled_n = NULL) {
    if (!is.null(cox_model) && inherits(cox_model, "coxph")) {
        return(test_proportional_hazards_assumption(
            cox_model = cox_model,
            outcome_name = outcome_name,
            output_dir = output_dir,
            file_prefix = file_prefix,
            dataset_name = dataset_name
        ))
    }

    if (is.null(output_dir)) {
        output_dir <- getwd()
    }
    if (is.null(modeled_n)) {
        modeled_n <- if (!is.null(data) && is.data.frame(data)) nrow(data) else NA_integer_
    }

    skip_reason <- reason %||% sprintf(
        "%s proportional hazards diagnostics were not run because no Cox model was fit.",
        outcome_name
    )
    skip_narrative <- c(
        skip_reason,
        "Schoenfeld residual proportional hazards tests require a fitted Cox model.",
        narrative_lines
    )
    skip_analysis_name <- paste0(outcome_name, "_proportional_hazards_diagnostics")

    if (!is.null(data) && is.data.frame(data) && !is.null(event_var) && event_var %in% names(data)) {
        diagnostics <- build_survival_skip_diagnostics(
            data = data,
            event_var = event_var,
            variables = variables[variables %in% names(data)],
            analysis_name = skip_analysis_name,
            dataset_name = dataset_name %||% "unspecified_dataset",
            reason = skip_reason,
            narrative_lines = skip_narrative,
            filter_stats = filter_stats,
            sparse_level_diagnostics = sparse_level_diagnostics,
            modeled_n = modeled_n,
            time_var = time_var
        )
    } else {
        diagnostics <- build_skip_report_diagnostics(
            status = "skipped",
            analysis_name = skip_analysis_name,
            dataset_name = dataset_name %||% "unspecified_dataset",
            reason = skip_reason,
            narrative_lines = skip_narrative
        )
    }

    save_skipped_model_outputs(
        analysis_name = "proportional_hazards_diagnostics",
        dataset_name = dataset_name %||% "unspecified_dataset",
        output_dir = output_dir,
        prefix = file_prefix,
        reason = skip_reason,
        diagnostics = diagnostics
    )

    diagnostics
}


#' Test Proportional Hazards Assumption using Schoenfeld Residuals
#'
#' Performs comprehensive testing of the proportional hazards assumption for Cox models
#' using Schoenfeld residuals. Creates diagnostic plots and statistical tests to identify
#' time-varying treatment effects and other PH violations.
#'
#' @param cox_model A fitted coxph model object
#' @param outcome_name Character string describing the outcome (e.g., "Overall Survival")
#' @param output_dir Directory path where diagnostic files should be saved
#' @param file_prefix Prefix for output files
#' @param dataset_name Name of the dataset for labeling
#' @details PH diagnostics are only attempted when the fitted Cox model has at
#'   least `MINIMUM_PH_TEST_EVENTS` observed events. Below that event floor, the
#'   function writes skipped HTML/workbook artifacts instead of returning `NULL`
#'   silently. If Schoenfeld residual testing fails after model fitting, the
#'   function writes an unavailable-artifact bundle with model context.
#' @return List containing Schoenfeld test results and plot paths when testing
#'   succeeds, or skip/unavailable diagnostics when PH testing is not feasible.
test_proportional_hazards_assumption <- function(cox_model, outcome_name = "Survival", output_dir = NULL, file_prefix = "", dataset_name = NULL) {
    logger::log_info(sprintf("Testing proportional hazards assumption for %s", outcome_name))

    # Check if model is valid
    if (is.null(cox_model) || !inherits(cox_model, "coxph")) {
        logger::log_warn("Invalid Cox model provided - skipping PH assumption testing")
        return(NULL)
    }

    # Set default output directory if not provided
    if (is.null(output_dir)) {
        warning("No output directory provided for proportional hazards testing. Files will be saved to current directory.")
        output_dir <- "."
    }

    # Ensure output directory exists
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    total_events <- tryCatch(cox_model$nevent, error = function(...) NA_integer_)
    if (!is.na(total_events) && total_events < MINIMUM_PH_TEST_EVENTS) {
        dataset_label <- dataset_name %||% "unspecified_dataset"
        total_patients <- tryCatch(cox_model$n, error = function(...) NA_integer_)
        model_terms <- tryCatch(attr(cox_model$terms, "term.labels"), error = function(...) character())
        model_formula <- tryCatch(paste(stats::deparse(stats::formula(cox_model)), collapse = " "), error = function(...) "Unavailable")
        model_frame <- tryCatch(stats::model.frame(cox_model), error = function(...) NULL)
        event_support <- NULL

        if (!is.null(model_frame) && length(model_terms) > 0) {
            response <- model_frame[[1]]
            if (inherits(response, "Surv")) {
                model_frame$.ph_event_status <- as.numeric(response[, "status"])
                event_support <- build_level_support_tab(
                    data = model_frame,
                    variables = model_terms,
                    outcome_var = ".ph_event_status"
                )
            }
        }

        skip_reason <- sprintf(
            "%s proportional hazards diagnostics were not run because only %d events were available (<%d minimum).",
            outcome_name,
            total_events,
            MINIMUM_PH_TEST_EVENTS
        )
        # Low-event Schoenfeld tests are materialized as skip reports so PH
        # output folders do not look silently incomplete.
        skip_diagnostics <- build_skip_report_diagnostics(
            status = "skipped",
            analysis_name = "proportional_hazards_diagnostics",
            dataset_name = dataset_label,
            reason = skip_reason,
            narrative_lines = c(
                skip_reason,
                "Schoenfeld residual proportional hazards tests require adequate event support."
            ),
            skip_summary = build_skip_summary_tab(list(
                patients_in_model = total_patients,
                events_observed = total_events,
                minimum_required_events = MINIMUM_PH_TEST_EVENTS
            )),
            event_support = event_support,
            model_context = build_model_context_tab(list(
                outcome = outcome_name,
                dataset = dataset_label,
                model_formula = model_formula,
                variables_in_model = if (length(model_terms) > 0) paste(model_terms, collapse = ", ") else ""
            )),
            raw_model_output = skip_reason
        )
        save_skipped_model_outputs(
            analysis_name = "proportional_hazards_diagnostics",
            dataset_name = dataset_label,
            output_dir = output_dir,
            prefix = file_prefix,
            reason = skip_reason,
            diagnostics = skip_diagnostics
        )
        logger::log_warn(formatted(
            sprintf(
                "Skipping PH diagnostics: only %d events available (<%d minimum).",
                total_events,
                MINIMUM_PH_TEST_EVENTS
            ),
            indent = 1
        ))
        return(skip_diagnostics)
    }

    build_ph_failure_note <- function(error_obj) {
        note_filename <- paste0(file_prefix, "proportional_hazards_unavailable.txt")
        note_path <- file.path(output_dir, note_filename)
        dataset_label <- ifelse(is.null(dataset_name), "Not specified", dataset_name)
        model_terms <- tryCatch(attr(cox_model$terms, "term.labels"), error = function(...) character())
        model_formula <- tryCatch(paste(stats::deparse(stats::formula(cox_model)), collapse = " "), error = function(...) "Unavailable")
        total_patients <- tryCatch(cox_model$n, error = function(...) NA_integer_)
        total_events <- tryCatch(cox_model$nevent, error = function(...) NA_integer_)

        note_lines <- c(
            "PROPORTIONAL HAZARDS DIAGNOSTICS NOT AVAILABLE",
            "",
            paste0("Outcome: ", outcome_name),
            paste0("Dataset: ", dataset_label),
            paste0("Error: ", error_obj$message),
            "",
            paste0("Model formula: ", model_formula)
        )

        if (!is.na(total_patients)) {
            note_lines <- c(note_lines, paste0("Patients included: ", total_patients))
        }
        if (!is.na(total_events)) {
            note_lines <- c(note_lines, paste0("Events observed: ", total_events))
        }
        if (length(model_terms) > 0) {
            note_lines <- c(note_lines, paste0("Variables in model: ", paste(model_terms, collapse = ", ")))
        }

        model_frame <- tryCatch(stats::model.frame(cox_model), error = function(...) NULL)
        reason_lines <- character()
        event_section_added <- FALSE

        if (!is.null(model_frame)) {
            response <- model_frame[[1]]
            status <- NULL
            time_values <- NULL
            if (inherits(response, "Surv")) {
                status <- as.numeric(response[, "status"])
                time_values <- as.numeric(response[, "time"])
            }

            if (!is.null(time_values) && length(time_values) > 0 && any(!is.na(time_values))) {
                note_lines <- c(note_lines, paste0(
                    "Follow-up time range (months): ",
                    sprintf("%.2f to %.2f", min(time_values, na.rm = TRUE), max(time_values, na.rm = TRUE))
                ))
            }

            if (!is.null(status) && length(model_terms) > 0) {
                for (term in model_terms) {
                    if (!term %in% names(model_frame)) next
                    var_data <- model_frame[[term]]
                    if (is.null(var_data)) next

                    if (is.factor(var_data) || is.character(var_data) || length(unique(var_data)) <= 10) {
                        if (!event_section_added) {
                            note_lines <- c(note_lines, "", "Event distribution by predictor level:")
                            event_section_added <- TRUE
                        }

                        var_factor <- if (is.factor(var_data)) droplevels(var_data) else coerce_to_factor_preserving_levels(var_data)
                        level_counts <- table(var_factor, useNA = "ifany")
                        event_counts <- tapply(status, var_factor, function(x) sum(x == 1, na.rm = TRUE))
                        event_counts <- event_counts[names(level_counts)]
                        event_counts[is.na(event_counts)] <- 0

                        note_lines <- c(note_lines, paste0("  ", term, ":"))
                        for (lvl in names(level_counts)) {
                            lvl_label <- ifelse(is.na(lvl) || lvl == "", "<Missing>", lvl)
                            note_lines <- c(note_lines, sprintf(
                                "    - %s: n = %d, events = %d",
                                lvl_label,
                                level_counts[[lvl]],
                                event_counts[[lvl]]
                            ))
                        }

                        zero_evt <- names(level_counts)[event_counts == 0]
                        if (length(zero_evt) > 0) {
                            cleaned <- ifelse(zero_evt == "" | is.na(zero_evt), "<Missing>", zero_evt)
                            reason_lines <- c(reason_lines, sprintf(
                                "  * %s has zero events for: %s",
                                term,
                                paste(cleaned, collapse = ", ")
                            ))
                        }

                        saturated_levels <- names(level_counts)[event_counts == level_counts]
                        if (length(saturated_levels) > 0) {
                            cleaned <- ifelse(saturated_levels == "" | is.na(saturated_levels), "<Missing>", saturated_levels)
                            reason_lines <- c(reason_lines, sprintf(
                                "  * %s has events in every patient for: %s",
                                term,
                                paste(cleaned, collapse = ", ")
                            ))
                        }
                    } else {
                        if (!event_section_added) {
                            note_lines <- c(note_lines, "", "Event distribution by predictor level:")
                            event_section_added <- TRUE
                        }
                        unique_vals <- length(unique(stats::na.omit(var_data)))
                        note_lines <- c(note_lines, paste0(
                            "  ", term, ": numeric predictor with ", unique_vals, " unique values"
                        ))
                        reason_lines <- c(reason_lines, sprintf(
                            "  * %s may contribute to singularity (numeric predictor with limited variability)",
                            term
                        ))
                    }
                }
            }
        } else {
            reason_lines <- c(reason_lines, "  * Unable to reconstruct the model frame to summarise predictor levels.")
        }

        coef_values <- tryCatch(stats::coef(cox_model), error = function(...) numeric())
        if (length(coef_values) > 0) {
            non_finite_coefs <- names(coef_values)[!is.finite(coef_values)]
            if (length(non_finite_coefs) > 0) {
                reason_lines <- c(reason_lines, sprintf(
                    "  * Non-finite coefficient estimates detected for: %s",
                    paste(non_finite_coefs, collapse = ", ")
                ))
            }
        }

        note_lines <- c(note_lines, "", "Why diagnostics failed:")
        if (length(reason_lines) > 0) {
            note_lines <- c(note_lines, reason_lines)
        } else {
            note_lines <- c(note_lines, "  * Schoenfeld residual diagnostics require an invertible variance matrix. The fitted Cox model resulted in a singular matrix, typically triggered by sparse events or redundant predictors.")
        }

        note_lines <- c(
            note_lines,
            "",
            "Suggested follow-up actions:",
            "  * Collapse or remove levels with zero events to stabilise the variance matrix.",
            "  * Simplify the model or consider time-varying effects when events are sparse.",
            "  * Verify that each GEP group has at least one event and adequate sample size."
        )

        writeLines(note_lines, note_path)
        logger::log_warn(formatted(sprintf("PH diagnostics unavailable note saved: %s", note_path), indent = 1))

        event_support <- NULL
        if (!is.null(model_frame) && !is.null(status) && length(model_terms) > 0) {
            model_frame$.ph_event_status <- status
            event_support <- build_level_support_tab(
                data = model_frame,
                variables = model_terms,
                outcome_var = ".ph_event_status"
            )
        }

        follow_up_text <- if (!is.null(time_values) && length(time_values) > 0 && any(!is.na(time_values))) {
            sprintf("%.2f to %.2f", min(time_values, na.rm = TRUE), max(time_values, na.rm = TRUE))
        } else {
            ""
        }
        ph_reason <- sprintf(
            "Proportional hazards diagnostics were unavailable because Schoenfeld residual testing failed: %s",
            error_obj$message
        )
        ph_diagnostics <- build_skip_report_diagnostics(
            status = "unavailable",
            analysis_name = "proportional_hazards_unavailable",
            dataset_name = dataset_label,
            reason = ph_reason,
            narrative_lines = c(
                sprintf("Schoenfeld residual diagnostics failed for `%s`.", outcome_name),
                "The fitted Cox model produced a singular or otherwise unusable variance structure for PH testing.",
                if (length(reason_lines) > 0) {
                    paste("Key instability signals:", paste(trimws(reason_lines), collapse = " "))
                } else {
                    "No specific predictor-level instability signal was isolated beyond the failed diagnostic calculation."
                }
            ),
            skip_summary = build_skip_summary_tab(list(
                status = "unavailable",
                patients_in_model = total_patients,
                events_observed = total_events,
                error = error_obj$message
            )),
            event_support = event_support,
            model_context = build_model_context_tab(list(
                outcome = outcome_name,
                dataset = dataset_label,
                model_formula = model_formula,
                variables_in_model = if (length(model_terms) > 0) paste(model_terms, collapse = ", ") else "",
                follow_up_range_months = follow_up_text
            )),
            compatibility_text = note_lines,
            raw_model_output = ph_reason
        )
        save_skipped_model_outputs(
            analysis_name = "proportional_hazards_unavailable",
            dataset_name = dataset_label,
            output_dir = output_dir,
            prefix = file_prefix,
            reason = ph_diagnostics$reason,
            diagnostics = ph_diagnostics
        )
    }

    ph_error <- NULL
    ph_results <- tryCatch(
        {
            # Perform Schoenfeld residuals test
            logger::log_info(formatted("Computing Schoenfeld residuals and correlation tests", indent = 1))
            schoenfeld_test <- survival::cox.zph(cox_model)

            # Extract variable names and test statistics
            var_names <- rownames(schoenfeld_test$table)
            p_values <- schoenfeld_test$table[, "p"]

            # Create summary of PH violations
            ph_summary <- data.frame(
                Variable = var_names,
                Chi_Square = schoenfeld_test$table[, "chisq"],
                DF = schoenfeld_test$table[, "df"],
                P_Value = p_values,
                PH_Assumption = ifelse(p_values < 0.05, "VIOLATED", "OK"),
                Interpretation = case_when(
                    p_values < 0.001 ~ "Strong evidence against PH (p < 0.001)",
                    p_values < 0.01 ~ "Moderate evidence against PH (p < 0.01)",
                    p_values < 0.05 ~ "Some evidence against PH (p < 0.05)",
                    TRUE ~ "No evidence against PH assumption"
                ),
                stringsAsFactors = FALSE
            )

            # Add overall test result
            global_test <- data.frame(
                Variable = "GLOBAL",
                Chi_Square = schoenfeld_test$table["GLOBAL", "chisq"],
                DF = schoenfeld_test$table["GLOBAL", "df"],
                P_Value = schoenfeld_test$table["GLOBAL", "p"],
                PH_Assumption = ifelse(schoenfeld_test$table["GLOBAL", "p"] < 0.05, "VIOLATED", "OK"),
                Interpretation = case_when(
                    schoenfeld_test$table["GLOBAL", "p"] < 0.001 ~ "Strong evidence against PH globally (p < 0.001)",
                    schoenfeld_test$table["GLOBAL", "p"] < 0.01 ~ "Moderate evidence against PH globally (p < 0.01)",
                    schoenfeld_test$table["GLOBAL", "p"] < 0.05 ~ "Some evidence against PH globally (p < 0.05)",
                    TRUE ~ "No evidence against PH assumption globally"
                ),
                stringsAsFactors = FALSE
            )

            ph_summary_with_global <- rbind(ph_summary[var_names != "GLOBAL", ], global_test)

            # Save summary table
            writexl::write_xlsx(
                ph_summary_with_global,
                path = file.path(output_dir, paste0(file_prefix, "proportional_hazards_tests.xlsx"))
            )

            logger::log_info(formatted(
                sprintf(
                    "PH assumption tests saved to: %s",
                    file.path(output_dir, paste0(file_prefix, "proportional_hazards_tests.xlsx"))
                ),
                indent = 1
            ))

            # Log key findings
            violations <- ph_summary_with_global[ph_summary_with_global$PH_Assumption == "VIOLATED", ]
            if (nrow(violations) > 0) {
                logger::log_warn(formatted(sprintf("PH ASSUMPTION VIOLATIONS DETECTED for %d variable(s):", nrow(violations)), indent = 1))
                for (i in seq_len(nrow(violations))) {
                    logger::log_warn(formatted(
                        sprintf(
                            "- %s: p = %.4f (%s)",
                            violations$Variable[i],
                            violations$P_Value[i],
                            violations$Interpretation[i]
                        ),
                        indent = 2
                    ))
                }
            } else {
                logger::log_info(formatted("No PH assumption violations detected", indent = 1))
            }

            # Create diagnostic plots
            logger::log_info(formatted("Creating Schoenfeld residual diagnostic plots", indent = 1))

            # Individual plots for each variable
            individual_plots <- list()
            n_vars <- length(var_names[var_names != "GLOBAL"])

            for (i in seq_along(var_names)) {
                var_name <- var_names[i]
                if (var_name == "GLOBAL") next # Skip global test for individual plots

                logger::log_info(formatted(sprintf("Creating plot for variable: %s", var_name), indent = 2))

                # Create individual plot
                plot_filename <- file.path(output_dir, paste0(file_prefix, "schoenfeld_", gsub("[^A-Za-z0-9]", "_", var_name), ".png"))

                png(plot_filename, width = DEFAULT_PLOT_WIDTH, height = SMALL_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)

                # Set margins to provide more space at top for title
                par(mar = c(5, 4, 6, 2))

                # Plot Schoenfeld residuals vs time
                plot(schoenfeld_test[i],
                    main = sprintf(
                        "Schoenfeld Residuals: %s\n%s (%s)",
                        var_name, outcome_name,
                        ifelse(is.null(dataset_name), "", dataset_name)
                    ),
                    xlab = "Time",
                    ylab = "Schoenfeld Residuals"
                )

                # Add p-value annotation
                p_val <- p_values[i]
                p_text <- if (p_val < 0.001) {
                    "p < 0.001"
                } else {
                    sprintf("p = %.3f", p_val)
                }

                mtext(
                    sprintf(
                        "Correlation test: %s %s",
                        p_text,
                        ifelse(p_val < 0.05, "(PH VIOLATED)", "(PH OK)")
                    ),
                    side = 3, line = 0.5, cex = 0.9,
                    col = ifelse(p_val < 0.05, "red", "darkgreen")
                )

                dev.off()

                individual_plots[[var_name]] <- plot_filename
            }

            # Create combined plot showing all variables
            logger::log_info(formatted("Creating combined diagnostic plot", indent = 1))
            combined_plot_filename <- file.path(output_dir, paste0(file_prefix, "schoenfeld_combined.png"))

            # Calculate grid dimensions
            n_plots <- length(individual_plots)
            n_cols <- min(3, n_plots) # Max 3 columns
            n_rows <- ceiling(n_plots / n_cols)

            png(combined_plot_filename, width = SMALL_PLOT_WIDTH * n_cols, height = SMALL_PLOT_HEIGHT * n_rows + 1.5, units = PLOT_UNITS, res = PLOT_DPI)
            par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 2), oma = c(0, 0, 6, 0))

            for (i in seq_along(var_names)) {
                var_name <- var_names[i]
                if (var_name == "GLOBAL") next

                plot(schoenfeld_test[i],
                    main = sprintf(
                        "%s\n%s", var_name,
                        if (p_values[i] < 0.001) "p < 0.001" else sprintf("p = %.3f", p_values[i])
                    ),
                    xlab = "Time",
                    ylab = "Schoenfeld Residuals",
                    cex.main = 0.9
                )

                # Color-code title based on p-value
                title(
                    main = sprintf(
                        "%s\n%s", var_name,
                        if (p_values[i] < 0.001) "p < 0.001" else sprintf("p = %.3f", p_values[i])
                    ),
                    col.main = ifelse(p_values[i] < 0.05, "red", "darkgreen"),
                    cex.main = 0.9
                )
            }

            # Add overall title with proper spacing from top
            mtext(
                sprintf(
                    "Proportional Hazards Diagnostics: %s\n%s",
                    outcome_name,
                    ifelse(is.null(dataset_name), "", paste("Dataset:", dataset_name))
                ),
                outer = TRUE, cex = 1.1, line = 2.5
            )

            dev.off()

            logger::log_info(formatted(sprintf("Combined diagnostic plot saved: %s", combined_plot_filename), indent = 1))

            # Create summary text file with interpretation
            summary_filename <- file.path(output_dir, paste0(file_prefix, "proportional_hazards_summary.txt"))

            cat("PROPORTIONAL HAZARDS ASSUMPTION TESTING SUMMARY\n", file = summary_filename)
            cat(paste(rep("=", 50), collapse = ""), "\n\n", file = summary_filename, append = TRUE)
            cat(sprintf("Analysis: %s\n", outcome_name), file = summary_filename, append = TRUE)
            cat(sprintf("Dataset: %s\n", ifelse(is.null(dataset_name), "Not specified", dataset_name)),
                file = summary_filename, append = TRUE
            )
            cat(sprintf("Test Date: %s\n\n", Sys.time()), file = summary_filename, append = TRUE)

            cat("INTERPRETATION:\n", file = summary_filename, append = TRUE)
            cat("The proportional hazards assumption requires that hazard ratios remain\n",
                file = summary_filename, append = TRUE
            )
            cat("constant over time. Violations suggest time-varying treatment effects.\n\n",
                file = summary_filename, append = TRUE
            )

            cat("TEST RESULTS:\n", file = summary_filename, append = TRUE)
            cat(
                sprintf(
                    "Global test p-value: %.4f %s\n\n",
                    schoenfeld_test$table["GLOBAL", "p"],
                    ifelse(schoenfeld_test$table["GLOBAL", "p"] < 0.05, "(VIOLATION)", "(OK)")
                ),
                file = summary_filename, append = TRUE
            )

            cat("Individual variable tests:\n", file = summary_filename, append = TRUE)
            for (i in seq_len(nrow(ph_summary_with_global))) {
                row <- ph_summary_with_global[i, ]
                cat(
                    sprintf(
                        "- %s: p = %.4f (%s)\n",
                        row$Variable, row$P_Value, row$PH_Assumption
                    ),
                    file = summary_filename, append = TRUE
                )
            }

            if (nrow(violations) > 0) {
                cat("\nVIOLATIONS DETECTED:\n", file = summary_filename, append = TRUE)
                cat("Variables with p < 0.05 violate the proportional hazards assumption.\n",
                    file = summary_filename, append = TRUE
                )
                cat("Consider stratification, time-varying coefficients, or alternative models.\n",
                    file = summary_filename, append = TRUE
                )
            }

            cat("\nFILES CREATED:\n", file = summary_filename, append = TRUE)
            cat(sprintf("- Test results: %s\n", basename(paste0(file_prefix, "proportional_hazards_tests.xlsx"))),
                file = summary_filename, append = TRUE
            )
            cat(sprintf("- Combined plot: %s\n", basename(combined_plot_filename)),
                file = summary_filename, append = TRUE
            )
            cat("- Individual plots: ", file = summary_filename, append = TRUE)
            cat(paste(basename(unlist(individual_plots)), collapse = ", "), file = summary_filename, append = TRUE)
            cat("\n", file = summary_filename, append = TRUE)

            logger::log_info(formatted(sprintf("Summary interpretation saved: %s", summary_filename), indent = 1))

            logger::log_info("Proportional hazards assumption testing completed")

            list(
                schoenfeld_test = schoenfeld_test,
                individual_tests = p_values,
                ph_summary = ph_summary_with_global,
                plots = list(
                    individual = individual_plots,
                    combined = combined_plot_filename
                ),
                summary_file = summary_filename
            )
        },
        error = function(e) {
            logger::log_error(sprintf("Error in PH assumption testing: %s", e$message))
            ph_error <<- e
            NULL
        }
    )

    if (!is.null(ph_error)) {
        try(build_ph_failure_note(ph_error), silent = TRUE)
    }

    return(ph_results)
}
