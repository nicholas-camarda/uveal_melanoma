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

    table_data <- rmst_results %>%
        dplyr::arrange(Time_Point_Years) %>%
        dplyr::mutate(
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
            Value = format_fixed(table_data$Group1_Value, digits_rmst)
        ),
        tibble::tibble(
            Row_Label = sprintf("%s (%s)", group2_label, unit_label),
            Time_Label = table_data$Time_Label,
            Value = format_fixed(table_data$Group2_Value, digits_rmst)
        ),
        tibble::tibble(
            Row_Label = sprintf("RMST Difference (%s)", unit_label),
            Time_Label = table_data$Time_Label,
            Value = format_fixed(table_data$Diff_Value, digits_diff)
        ),
        tibble::tibble(
            Row_Label = "RMST P-Value",
            Time_Label = table_data$Time_Label,
            Value = format_p_value(table_data$RMST_P_Value, digits_p)
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

summarize_cox_hr <- function(model, dataset_name, analysis_label, model_label, group_var, data_source_label) {
    if (is.null(model)) {
        return(NULL)
    }

    model_summary <- tryCatch(summary(model), error = function(e) {
        logger::log_warn(sprintf("Unable to summarise Cox model for %s: %s", analysis_label, e$message))
        NULL
    })
    if (is.null(model_summary) || is.null(model_summary$coefficients)) {
        return(NULL)
    }

    coef_rows <- rownames(model_summary$coefficients)
    if (is.null(coef_rows)) {
        return(NULL)
    }

    target_rows <- grepl(paste0("^", group_var), coef_rows)
    if (!any(target_rows)) {
        return(NULL)
    }

    ci_mat <- model_summary$conf.int
    coeff_mat <- model_summary$coefficients
    if (is.null(ci_mat)) {
        return(NULL)
    }

    data.frame(
        dataset = dataset_name %||% "unspecified_dataset",
        analysis_label = analysis_label,
        analysis_id = make_filename_safe(analysis_label),
        model_label = model_label,
        term = coef_rows[target_rows],
        hazard_ratio = round(ci_mat[target_rows, "exp(coef)"], 3),
        ci_lower = round(ci_mat[target_rows, "lower .95"], 3),
        ci_upper = round(ci_mat[target_rows, "upper .95"], 3),
        p_value = coeff_mat[target_rows, "Pr(>|z|)"],
        n_patients = model_summary$n,
        n_events = model_summary$nevent,
        data_source = data_source_label,
        stringsAsFactors = FALSE
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
analyze_time_to_event_outcomes <- function(data, time_var, event_var, group_var = "treatment_group", model_group_var = group_var, confounders = NULL, ylab = "Survival Probability", analysis_type = "post_treatment_only", dataset_name = NULL, legend_labels = NULL, output_dirs = NULL, prefix = NULL, risk_table_height = 0.18, risk_table_rel_heights = c(0.78, 0.22), risk_table_y_expand = c(0.18, 0.18), saved_plot_height = NULL) {
    plot_group_var <- group_var
    palette_group_var <- group_var

    # Check that there are at least two groups for analysis; otherwise, skip Cox model
    if (length(unique(data[[plot_group_var]])) < 2) {
        warning(sprintf("Only one level of %s present; skipping cox model.", plot_group_var))
        return(list(
            fit = NULL,
            plot = NULL,
            median_times = NULL,
            cox_model = NULL,
            cox_table = NULL
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
            diagnostics = list(
                sparse_level_diagnostics = data.frame(),
                raw_model_output = "Model skipped: insufficient data for KM fit."
            )
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

    cox_ready <- nrow(cox_data) > 0 && length(unique(stats::na.omit(cox_data[[model_group_var]]))) >= 2
    if (!cox_ready) {
        logger::log_warn(formatted(
            "Cox model will be skipped: insufficient data after sparse-level exclusions.",
            indent = 1
        ))
    }

    km_unadjusted_cox_model <- NULL
    unadjusted_ready <- nrow(model_data) > 0 && length(unique(stats::na.omit(model_data[[model_group_var]]))) >= 2
    if (unadjusted_ready) {
        km_unadjusted_cox_model <- tryCatch({
            survival::coxph(model_surv_formula, data = model_data)
        }, error = function(e) {
            logger::log_error(sprintf("Unadjusted Cox model failed for %s: %s", ylab, e$message))
            NULL
        })
    } else {
        logger::log_warn(formatted(
            "Unadjusted Cox model skipped: insufficient groups in KM dataset.",
            indent = 1
        ))
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
        ggplot2::geom_hline(yintercept = 0.5, linetype = "solid", color = "black", size = 0.9)  # 50% reference line
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
        rmst_results <- data.frame(
            Time_Point_Years = numeric(),
            Time_Point_Months = numeric(),
            Group1_Name = character(),
            RMST_Group1_Months = numeric(),
            RMST_Group1_Years = numeric(),
            Group2_Name = character(),
            RMST_Group2_Months = numeric(),
            RMST_Group2_Years = numeric(),
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
        rmst_results <- data.frame(
            Time_Point_Years = numeric(),
            Time_Point_Months = numeric(),
            Group1_Name = character(),
            RMST_Group1_Months = numeric(),
            RMST_Group1_Years = numeric(),
            Group2_Name = character(),
            RMST_Group2_Months = numeric(),
            RMST_Group2_Years = numeric(),
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

        # Calculate RMST for each time point
        logger::log_info(sprintf("DEBUG: Starting RMST analysis for %d time points", length(valid_time_points)))
        for (time_point in valid_time_points) {
            time_years <- round(time_point / 12, 1)
            logger::log_info(sprintf("DEBUG: Processing RMST for %s years (%.1f months)", time_years, time_point))
            rmst_result <- tryCatch(
                {
                    # Handle RMST for any number of groups (binary or multi-group)
                    unique_groups <- unique(rmst_data[[plot_group_var]])
                    logger::log_info(sprintf("DEBUG: Unique groups for RMST: %s", paste(unique_groups, collapse = ", ")))
                    
                    if (length(unique_groups) == 2) {
                        # Binary comparison: use 0/1 coding based on **treatment** factor levels
                        # We know for this pipeline that treatment_group should always be
                        # coded with PBT as reference and GKSRS as comparison. Any deviation
                        # should be coerced back to that convention rather than inferred.

                        if (plot_group_var == "treatment_group") {
                            rmst_data[[plot_group_var]] <- factor(
                                as.character(rmst_data[[plot_group_var]]),
                                levels = TREATMENT_FACTOR_LEVELS
                            )
                            factor_levels <- TREATMENT_FACTOR_LEVELS
                        } else {
                            # For non-treatment groupings (e.g., GEP strata), fall back to
                            # simple factor coercion but keep the natural ordering.
                            rmst_data[[plot_group_var]] <- factor(rmst_data[[plot_group_var]])
                            factor_levels <- levels(rmst_data[[plot_group_var]])
                        }

                        # Require at least two levels before proceeding
                        if (length(factor_levels) < 2) {
                            logger::log_warn(sprintf(
                                "RMST: group_var '%s' has <2 levels after coercion; skipping RMST at time_point=%.1f months",
                                plot_group_var, time_point
                            ))
                            return(NULL)
                        }

                        group_binary <- ifelse(rmst_data[[plot_group_var]] == factor_levels[2], 1, 0)
                        logger::log_info(sprintf("DEBUG: Running RMST for binary comparison: %s (arm=0) vs %s (arm=1)", factor_levels[1], factor_levels[2]))
                        
                        rmst2(
                            time = rmst_data[[time_var]],
                            status = rmst_data[[event_var]],
                            arm = group_binary,
                            tau = time_point
                        )
                    } else {
                        # Non-binary groups: skip RMST analysis entirely and log informative message
                        logger::log_info(sprintf("DEBUG: Skipping RMST analysis - non-binary grouping detected (%d groups: %s). RMST analysis requires exactly 2 groups.", 
                                               length(unique_groups), paste(unique_groups, collapse = ", ")))
                        NULL
                    }
                },
                error = function(e) {
                    logger::log_error(sprintf("ERROR in RMST calculation for %.1f years: %s", time_years, e$message))
                    NULL
                }
            )
            if (!is.null(rmst_result)) {
                # Get group names for clear labeling
                if (plot_group_var == "treatment_group") {
                    # We know the correct order from config: PBT (arm 0), GKSRS (arm 1)
                    factor_levels <- TREATMENT_FACTOR_LEVELS
                } else {
                    factor_levels <- levels(rmst_data[[plot_group_var]])
                    if (is.null(factor_levels) || length(factor_levels) == 0) {
                        rmst_data[[plot_group_var]] <- factor(rmst_data[[plot_group_var]])
                        factor_levels <- levels(rmst_data[[plot_group_var]])
                    }
                }

                if (length(factor_levels) < 2) {
                    logger::log_warn(sprintf(
                        "RMST row build: group_var '%s' has <2 levels; skipping RMST row for time_point=%.1f months",
                        plot_group_var, time_point
                    ))
                    next
                }

                group1_name <- as.character(factor_levels[1])  # arm=0 (reference, e.g., PBT)
                group2_name <- as.character(factor_levels[2])  # arm=1 (comparison, e.g., GKSRS)
                
                # Calculate RMST values
                rmst_group1_months <- round(rmst_result$RMST.arm0$rmst[1], 2)
                rmst_group2_months <- round(rmst_result$RMST.arm1$rmst[1], 2)
                rmst_diff_months <- round(rmst_result$unadjusted.result[1, 1], 2)
                rmst_diff_months <- ifelse(abs(rmst_diff_months) < 1e-10, 0, rmst_diff_months)
                rmst_diff_years <- round(rmst_diff_months / 12, 3)
                rmst_diff_years <- ifelse(abs(rmst_diff_years) < 1e-10, 0, rmst_diff_years)

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
                ci_lower_years <- round(ci_lower_months / 12, 3)
                ci_upper_years <- round(ci_upper_months / 12, 3)

                rmst_results <- rbind(
                    rmst_results,
                    data.frame(
                        Time_Point_Years = time_years,
                        Time_Point_Months = time_point,
                        Group1_Name = group1_name,
                        RMST_Group1_Months = rmst_group1_months,
                        RMST_Group1_Years = round(rmst_group1_months / 12, 2),
                        Group2_Name = group2_name,
                        RMST_Group2_Months = rmst_group2_months,
                        RMST_Group2_Years = round(rmst_group2_months / 12, 2),
                        RMST_Difference_Months = rmst_diff_months,
                        RMST_Difference_Years = rmst_diff_years,
                        RMST_Difference_Lower_Months = ci_lower_months,
                        RMST_Difference_Upper_Months = ci_upper_months,
                        RMST_Difference_Lower_Years = ci_lower_years,
                        RMST_Difference_Upper_Years = ci_upper_years,
                        RMST_P_Value = round(rmst_result$unadjusted.result[1, 4], 4),
                        Analysis_Type = paste0("Mean survival up to ", time_years, " years"),
                        stringsAsFactors = FALSE
                    )
                )
            } else {
                # Check if we skipped RMST due to non-binary grouping
                unique_groups <- unique(rmst_data[[plot_group_var]])
                analysis_type_msg <- if (length(unique_groups) < 2) {
                    "Not applicable (insufficient groups)"
                } else if (length(unique_groups) > 2) {
                    "Not applicable (non-binary grouping)"
                } else {
                    "Analysis failed"
                }
                rmst_results <- rbind(
                    rmst_results,
                    data.frame(
                        Time_Point_Years = time_years,
                        Time_Point_Months = time_point,
                        Group1_Name = NA_character_,
                        RMST_Group1_Months = NA,
                        RMST_Group1_Years = NA,
                        Group2_Name = NA_character_,
                        RMST_Group2_Months = NA,
                        RMST_Group2_Years = NA,
                        RMST_Difference_Months = NA,
                        RMST_Difference_Years = NA,
                        RMST_Difference_Lower_Months = NA,
                        RMST_Difference_Upper_Months = NA,
                        RMST_Difference_Lower_Years = NA,
                        RMST_Difference_Upper_Years = NA,
                        RMST_P_Value = NA,
                        Analysis_Type = analysis_type_msg,
                        stringsAsFactors = FALSE
                    )
                )
            }
        }
    }
    rmst_survival_summary <- if (exists("rmst_results", inherits = FALSE)) {
        build_rmst_survival_summary(rmst_results, surv_rates, group_var = plot_group_var)
    } else {
        data.frame()
    }

    rmst_timepoint_table <- tibble::tibble()
    if (nrow(rmst_results) > 0) {
        first_label <- function(values, fallback) {
            valid_idx <- which(!is.na(values) & values != "")
            if (length(valid_idx) == 0) {
                return(fallback)
            }
            as.character(values[valid_idx[1]])
        }
        group1_label <- first_label(rmst_results$Group1_Name, "Group 1")
        group2_label <- first_label(rmst_results$Group2_Name, "Group 2")

        rmst_timepoint_table <- tryCatch(
            build_rmst_timepoint_table(
                rmst_results = rmst_results,
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
            rmst_pvalue_row[[time_label]] <- if (is.na(p_val)) {
                "Analysis failed"
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
            rmst_diff_row[[time_label]] <- if (is.na(rmst_diff)) "NA" else sprintf("%.2f", rmst_diff)
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
        # Only save RMST file if there's actual RMST data (not just "Not applicable" rows)
        rmst_has_data <- nrow(rmst_results) > 0 && any(
            (!is.na(rmst_results$RMST_P_Value) & !grepl("Not applicable", rmst_results$Analysis_Type)) |
                (!is.na(rmst_results$RMST_Group1_Months) & !is.na(rmst_results$RMST_Group2_Months))
        )
        if (rmst_has_data) {
            rmst_dir <- ensure_output_dir(resolve_obj4_output_dir(output_dirs, output_dir, "rmst"))
            writexl::write_xlsx(
                rmst_results,
                path = file.path(rmst_dir, paste0(prefix, make_filename_safe(ylab), "_rmst_analysis.xlsx"))
            )
            logger::log_info(sprintf("RMST analysis file saved: %s", paste0(prefix, make_filename_safe(ylab), "_rmst_analysis.xlsx")))
        } else {
            logger::log_info(sprintf("Skipping RMST file creation - no valid RMST data available for %s", ylab))
        }
        if (rmst_has_data && nrow(rmst_survival_summary) > 0) {
            rmst_dir <- ensure_output_dir(resolve_obj4_output_dir(output_dirs, output_dir, "rmst"))
            combined_path <- file.path(rmst_dir, paste0(prefix, make_filename_safe(ylab), "_survival_rmst_summary.xlsx"))
            writexl::write_xlsx(rmst_survival_summary, path = combined_path)
            logger::log_info(sprintf("Survival + RMST summary saved: %s", basename(combined_path)))
        } else if (!rmst_has_data) {
            logger::log_info(sprintf(
                "Skipping survival + RMST summary for %s - no valid RMST data (likely non-binary grouping)",
                ylab
            ))
        }
        if (rmst_has_data && nrow(rmst_timepoint_table) > 0) {
            rmst_dir <- ensure_output_dir(resolve_obj4_output_dir(output_dirs, output_dir, "rmst"))
            rmst_table_path <- file.path(rmst_dir, paste0(prefix, make_filename_safe(ylab), "_rmst_timepoint_table.xlsx"))
            writexl::write_xlsx(rmst_timepoint_table, path = rmst_table_path)
            logger::log_info(sprintf("RMST timepoint table saved: %s", basename(rmst_table_path)))
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
        diagnostics_stub <- list(
            sparse_level_diagnostics = cox_exclusion_result$sparse_level_diagnostics,
            raw_model_output = "Cox model skipped: insufficient data after sparse-level exclusions."
        )
        diagnostics_stub$sample_size_summary <- build_sample_size_summary_tab(
            filter_stats = cox_exclusion_result$filter_stats,
            dataset_name = dataset_name,
            analysis_name = cox_analysis_name,
            modeled_n = nrow(cox_data)
        )
        cox_result <- list(
            model = NULL,
            table = NULL,
            diagnostics = diagnostics_stub
        )
    }

    if (is.null(cox_result)) {
        diagnostics_stub <- list(
            sparse_level_diagnostics = cox_exclusion_result$sparse_level_diagnostics,
            raw_model_output = "Cox model failed to fit; see logs for details."
        )
        diagnostics_stub$sample_size_summary <- build_sample_size_summary_tab(
            filter_stats = cox_exclusion_result$filter_stats,
            dataset_name = dataset_name,
            analysis_name = cox_analysis_name,
            modeled_n = nrow(cox_data)
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
    hazard_ratio_summary <- if (length(hr_rows) > 0) dplyr::bind_rows(hr_rows) else data.frame()

    if (!is.null(output_dirs) && nrow(hazard_ratio_summary) > 0) {
        hr_output_dir <- determine_survival_output_dir(ylab, output_dirs)
        hr_dir <- ensure_output_dir(resolve_obj4_output_dir(output_dirs, hr_output_dir, "cox"))
        
        hr_filename <- paste0(prefix, make_filename_safe(ylab), "_hazard_ratio_summary.xlsx")
        writexl::write_xlsx(hazard_ratio_summary, file.path(hr_dir, hr_filename))
        logger::log_info(sprintf("Hazard ratio summary saved: %s", hr_filename))
    }

    logger::log_info(sprintf(
        "DEBUG: RMST summary for %s - rows: %d, any valid RMST rows: %s",
        ylab,
        nrow(rmst_results),
        if (nrow(rmst_results) > 0) {
            any(
                (!is.na(rmst_results$RMST_P_Value) & !grepl("Not applicable", rmst_results$Analysis_Type)) |
                    (!is.na(rmst_results$RMST_Group1_Months) & !is.na(rmst_results$RMST_Group2_Months))
            )
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
            rmst_has_rows <- nrow(rmst_results) > 0
            if (!rmst_has_rows) {
                logger::log_info(sprintf("Skipping RMST plot generation - no RMST rows available for %s", ylab))
                return(NULL)
            }

            # Get group names for RMST plot - use levels() to match factor order
            factor_levels <- levels(km_data[[plot_group_var]])
            
            # If not a factor or no levels, fall back to unique values in sorted order
            if (is.null(factor_levels) || length(factor_levels) == 0) {
                factor_levels <- sort(unique(km_data[[plot_group_var]]))
            }
            
            group1_name <- as.character(factor_levels[1])
            group2_name <- as.character(factor_levels[2])
            
            plot_rmst_pvalue_progression(rmst_results, ylab, output_dirs, prefix, group1_name, group2_name, plot_group_var)
        }, error = function(e) {
            logger::log_warn(sprintf("RMST plot generation failed: %s", e$message))
            NULL
        }),
        cox_model = cox_result$model,
        cox_table = cox_result$table,
        ph_diagnostics = NULL,
        diagnostics = cox_result$diagnostics,
        hazard_ratio_summary = hazard_ratio_summary
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

    if (!is.null(output_dirs) && !is.null(output_dirs$obj3_pfs2)) {
        summary_path <- file.path(output_dirs$obj3_pfs2, paste0(prefix, "pfs2_treatment_summary.xlsx"))
        writexl::write_xlsx(
            list(
                raw_primary_vs_salvage = raw_primary_vs_salvage,
                model_primary_vs_salvage = model_primary_vs_salvage
            ),
            summary_path
        )
        logger::log_info(sprintf("PFS-2 treatment summary saved to %s", summary_path))
    }

    logger::log_info(sprintf("Final PFS-2 analysis dataset: %d patients", nrow(pfs2_data)))
    logger::log_info(sprintf("PFS-2 events (2nd recurrence): %d", sum(pfs2_data$pfs2_event)))

    # Check if we have enough patients and events for analysis
    if (nrow(pfs2_data) < 10) {
        logger::log_info("Insufficient patients for PFS-2 analysis")
        return(list(
            pfs2_data = pfs2_data,
            survival_analysis = NULL,
            summary_table = NULL
        ))
    }

    # Check if we have enough events for survival analysis
    total_events <- sum(pfs2_data$pfs2_event)

    if (total_events < 5) {
        logger::log_error("ERROR: Insufficient events for PFS-2 survival analysis")
        logger::log_info(sprintf("Total events: %d (minimum 5 required)", total_events))
        logger::log_info("Skipping survival analysis due to insufficient data")

        # Create explanation text file for skipped analysis
        explanation_text <- sprintf(
            "PFS-2 Analysis Skipped - Insufficient Events

            The Issue:
            %s cohort: %d patients total
            PFS-2 eligible patients: %d patients (those with first recurrence)
            PFS-2 events: %d patients (second recurrence)
            Minimum required: 5 events for survival analysis

            Analysis was skipped because there are insufficient events (%d) to perform a meaningful survival analysis. 
            The minimum requirement of 5 events ensures statistical validity and reliable results.

            This is expected behavior for cohorts with limited recurrence data and does not indicate an error.",
            tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name))),
            nrow(data),
            nrow(pfs2_data),
            total_events,
            total_events
        )

        # Save explanation to both a_pfs2 and b_proportional_hazards_diagnostics directories
        if (!is.null(output_dirs)) {
            # Save to a_pfs2 directory
            pfs2_dir <- output_dirs$obj3_pfs2
            if (!is.null(pfs2_dir) && dir.exists(pfs2_dir)) {
                explanation_file <- file.path(pfs2_dir, paste0(prefix, "pfs2_analysis_skipped_explanation.txt"))
                writeLines(explanation_text, explanation_file)
                logger::log_info(sprintf("Explanation saved to: %s", explanation_file))
            }
            
            # Save to b_proportional_hazards_diagnostics directory
            ph_dir <- output_dirs$obj3_ph_diagnostics
            if (!is.null(ph_dir) && dir.exists(ph_dir)) {
                explanation_file <- file.path(ph_dir, paste0(prefix, "pfs2_analysis_skipped_explanation.txt"))
                writeLines(explanation_text, explanation_file)
                logger::log_info(sprintf("Explanation saved to: %s", explanation_file))
            }
        }

        pfs2_survival <- list(
            fit = NULL,
            plot = NULL,
            survival_rates = NULL,
            cox_model = NULL,
            cox_table = NULL
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
            prefix = prefix
        )
    }

    logger::log_info("PFS-2 analysis completed")

    # Generate proportional hazards diagnostics for PFS-2 (Objective 3)
    ph_diag_result <- NULL
    if (!is.null(pfs2_survival$cox_model)) {
        ph_output_dir <- if (!is.null(output_dirs)) output_dirs$obj3_ph_diagnostics else getwd()
        ph_file_prefix <- paste0(prefix, make_filename_safe("PFS-2 Probability (Freedom from 2nd Recurrence)"), "_")
        ph_diag_result <- test_proportional_hazards_assumption(
            cox_model = pfs2_survival$cox_model,
            outcome_name = "PFS-2 Probability (Freedom from 2nd Recurrence)",
            output_dir = ph_output_dir,
            file_prefix = ph_file_prefix,
            dataset_name = dataset_name
        )
    }

    return(list(
        pfs2_data = pfs2_data,
        survival_analysis = pfs2_survival,
        summary_table = pfs2_survival$cox_table, # Use the standardized table from generate_regression_table
        raw_primary_vs_salvage = raw_primary_vs_salvage,
        model_primary_vs_salvage = model_primary_vs_salvage,
        ph_diagnostics = ph_diag_result
    ))
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
#' @return List containing schoenfeld_test, individual_tests, plots, summary
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

                        var_factor <- factor(var_data, exclude = NULL)
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
