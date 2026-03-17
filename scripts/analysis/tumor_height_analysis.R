# Tumor Height Analysis Functions
# Author: Nicholas Camarda
# Description: Functions specific to tumor height change analysis and subgroup interactions

#' Analyze tumor height reduction
#'
#' Calculates and summarizes changes in tumor height by treatment group, returning summary statistics and a table.
#' Now includes both primary analysis (without baseline height adjustment) and sensitivity analysis (with baseline height adjustment).
#'
#'
#' @param data Data frame containing tumor height variables, including `height_change` and `treatment_group`.
#' @param output_dirs Named list of output directories for saving analysis results (e.g., recurrence, mets, os, pfs, height, subgroups).
#' @param prefix Character string used as a prefix for output files (e.g., "full_cohort_") to identify cohort or analysis context.
#' @param confounders Character vector of confounder variable names to include in regression models.
#'
#' @return A list with the following elements:
#'   - `changes`: Summary data frame of tumor height changes by treatment group.
#'   - `table`: gtsummary object summarizing tumor height changes.
#'   - `primary_regression_model`: Linear model (lm) object for primary analysis (unadjusted).
#'   - `primary_regression_table`: gtsummary object for the primary regression model.
#'   - `sensitivity_regression_model`: Linear model (lm) object for sensitivity analysis (adjusted for baseline height).
#'   - `sensitivity_regression_table`: gtsummary object for the sensitivity regression model.
#'
#' @examples
#' analyze_tumor_height_changes(
#'     data = analytic_data,
#'     output_dirs = list(obj1_height_primary = "output/height"),
#'     prefix = "full_cohort_",
#'     confounders = c("age_at_diagnosis", "sex")
#' )
analyze_tumor_height_changes <- function(data, output_dirs, prefix, confounders) {
    # Use height_change variable that was already calculated in data_processing.R
    data_with_height_change <- enforce_unordered_factors(data)

    height_model_vars <- unique(c("treatment_group", confounders, "initial_tumor_height"))
    exclusion_result <- apply_sparse_level_exclusions(
        data = data_with_height_change,
        variables = height_model_vars[height_model_vars %in% names(data_with_height_change)],
        analysis_name = "tumor_height_modeling",
        id_col = pick_sparse_level_id_col(data_with_height_change),
        level_exclusions = MODELING_LEVEL_EXCLUSIONS
    )

    if (exclusion_result$removed_row_count > 0) {
        logger::log_info(sprintf(
            "Excluded %d rows with sparse categorical levels prior to tumor height modeling",
            exclusion_result$removed_row_count
        ))
    }

    data_model_ready <- exclusion_result$data
    sufficient_height_data <- nrow(data_model_ready) > 0 && length(unique(stats::na.omit(data_model_ready$treatment_group))) >= 2

    if (!sufficient_height_data) {
        logger::log_warn("Insufficient data available after sparse-level exclusions; regression models will be skipped.")
    }

    # Summary statistics (grouped)
    height_changes <- data_model_ready %>%
        group_by(treatment_group) %>%
        summarise(
            n = n(),
            mean_change = mean(height_change, na.rm = TRUE),
            sd_change = sd(height_change, na.rm = TRUE),
            median_change = median(height_change, na.rm = TRUE),
            iqr_change = IQR(height_change, na.rm = TRUE),
            .groups = "drop"
        )

    plaque <- data_model_ready %>% filter(treatment_group == "PBT")
    gk <- data_model_ready %>% filter(treatment_group == "GKSRS")
    wilcox.test(height_change ~ treatment_group, data = data_model_ready)

    # Table for publication (row-level input)
    # Custom for this because we are showing something simple
    tbl_summary_obj <- data_model_ready %>%
        select(treatment_group, height_change) %>%
        tbl_summary(
            by = treatment_group,
            missing = "no",
            label = get_variable_labels(),
            statistic = list(
                all_continuous() ~ "{median} ({min}, {max})",
                all_categorical() ~ "{n} ({p}%)"
            ),
            digits = list(all_continuous() ~ 1, all_categorical() ~ 0)
        ) %>%
        add_overall() %>%
        add_p(test = list(all_continuous() ~ "wilcox.test")) %>%
        bold_labels() %>%
        modify_header(
            label = "**Characteristic**",
            stat_0 = "**Overall**\nN = {N}",
            stat_1 = "**PBT**\nN = {n}",
            stat_2 = "**GKSRS**\nN = {n}",
            p.value = "**p-value**"
        ) %>%
        modify_caption("Tumor Height Changes Analysis") %>%
        as_gt()

    # Save table
    save_gt_html(
        tbl_summary_obj,
        filename = file.path(output_dirs$obj1_height_primary, paste0(prefix, "height_changes.html"))
    )

    # PRIMARY ANALYSIS: Linear regression WITHOUT initial tumor height adjustment
    logger::log_info("Fitting PRIMARY linear regression model for tumor height changes (without baseline height adjustment)")

    # Use the unified table generation system for primary analysis
    primary_result <- if (sufficient_height_data) {
        generate_regression_table(
            data = data_model_ready,
            outcome_var = "height_change",
            predictor_vars = "treatment_group",
            confounders = confounders,
            model_type = "linear",
            effect_measure = "MD", # Mean Difference for continuous outcome
            analysis_name = "height_change_primary",
            dataset_name = "tumor_height",
            output_dir = output_dirs$obj1_height_primary,
            prefix = prefix,
            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
            filter_stats = exclusion_result$filter_stats
        )
    } else {
        primary_sample_size_summary <- build_sample_size_summary_tab(
            filter_stats = exclusion_result$filter_stats,
            dataset_name = "tumor_height",
            analysis_name = "height_change_primary",
            modeled_n = nrow(data_model_ready)
        )
        diagnostics_stub <- list(
            table = NULL,
            model = NULL,
            diagnostics = NULL
        )
        diagnostics_stub$diagnostics <- build_skip_report_diagnostics(
            status = "skipped",
            analysis_name = "height_change_primary",
            dataset_name = "tumor_height",
            reason = "Primary tumor-height regression was skipped because the post-exclusion dataset did not retain enough usable rows or treatment-group variation.",
            narrative_lines = c(
                sprintf(
                    "After sparse-level exclusions, %d rows remained for the primary tumor-height model.",
                    nrow(data_model_ready)
                ),
                "A regression model is only attempted when the filtered dataset retains at least two treatment groups."
            ),
            sample_size_summary = primary_sample_size_summary,
            skip_summary = build_skip_summary_tab(list(
                modeled_n = nrow(data_model_ready),
                distinct_treatment_groups = length(unique(stats::na.omit(data_model_ready$treatment_group)))
            )),
            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
            level_support = build_level_support_tab(data_model_ready, unique(c("treatment_group", confounders))),
            raw_model_output = "Model skipped: insufficient data after sparse-level exclusions."
        )
        save_skipped_model_outputs(
            analysis_name = "height_change_primary",
            dataset_name = "tumor_height",
            output_dir = output_dirs$obj1_height_primary,
            prefix = prefix,
            reason = diagnostics_stub$diagnostics$reason,
            diagnostics = diagnostics_stub$diagnostics
        )
        diagnostics_stub
    }

    primary_height_lm <- primary_result$model
    primary_height_lm_tbl <- primary_result$table

    # SENSITIVITY ANALYSIS: Linear regression WITH initial tumor height adjustment
    logger::log_info("Fitting SENSITIVITY linear regression model for tumor height changes (with baseline height adjustment)")

    # Use the unified table generation system for sensitivity analysis
    sensitivity_result <- if (sufficient_height_data) {
        generate_regression_table(
            data = data_model_ready,
            outcome_var = "height_change",
            predictor_vars = "treatment_group",
            confounders = c(confounders, "initial_tumor_height"),
            model_type = "linear",
            effect_measure = "MD", # Mean Difference for continuous outcome
            analysis_name = "height_change_sensitivity",
            dataset_name = "tumor_height",
            output_dir = output_dirs$obj1_height_sensitivity,
            prefix = prefix,
            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
            filter_stats = exclusion_result$filter_stats
        )
    } else {
        sensitivity_sample_size_summary <- build_sample_size_summary_tab(
            filter_stats = exclusion_result$filter_stats,
            dataset_name = "tumor_height",
            analysis_name = "height_change_sensitivity",
            modeled_n = nrow(data_model_ready)
        )
        diagnostics_stub <- list(
            table = NULL,
            model = NULL,
            diagnostics = NULL
        )
        diagnostics_stub$diagnostics <- build_skip_report_diagnostics(
            status = "skipped",
            analysis_name = "height_change_sensitivity",
            dataset_name = "tumor_height",
            reason = "Sensitivity tumor-height regression was skipped because the post-exclusion dataset did not retain enough usable rows or treatment-group variation.",
            narrative_lines = c(
                sprintf(
                    "After sparse-level exclusions, %d rows remained for the sensitivity tumor-height model.",
                    nrow(data_model_ready)
                ),
                "The sensitivity model adds baseline tumor height, so it is only attempted when the filtered dataset retains enough rows and at least two treatment groups."
            ),
            sample_size_summary = sensitivity_sample_size_summary,
            skip_summary = build_skip_summary_tab(list(
                modeled_n = nrow(data_model_ready),
                distinct_treatment_groups = length(unique(stats::na.omit(data_model_ready$treatment_group)))
            )),
            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
            level_support = build_level_support_tab(data_model_ready, unique(c("treatment_group", confounders, "initial_tumor_height"))),
            raw_model_output = "Model skipped: insufficient data after sparse-level exclusions."
        )
        save_skipped_model_outputs(
            analysis_name = "height_change_sensitivity",
            dataset_name = "tumor_height",
            output_dir = output_dirs$obj1_height_sensitivity,
            prefix = prefix,
            reason = diagnostics_stub$diagnostics$reason,
            diagnostics = diagnostics_stub$diagnostics
        )
        diagnostics_stub
    }

    sensitivity_height_lm <- sensitivity_result$model
    sensitivity_height_lm_tbl <- sensitivity_result$table

    return(list(
        changes = height_changes,
        table = tbl_summary_obj,
        primary_regression_model = primary_height_lm,
        primary_regression_table = primary_height_lm_tbl,
        sensitivity_regression_model = sensitivity_height_lm,
        sensitivity_regression_table = sensitivity_height_lm_tbl
    ))
}

#' Summarize baseline tumor size by treatment group and generate a box/violin plot
#'
#' Produces a tidy summary table and publication-style plot for baseline tumor size
#' split by treatment group. Saves outputs only if an output directory is provided.
#'
#' @param data Data frame with `treatment_group` and size variable (default: `initial_tumor_height`)
#' @param size_var Name of the tumor size column to summarise (e.g., "initial_tumor_height")
#' @param output_dir Directory to write outputs (PNG and XLSX). If NULL, nothing is written.
#' @param prefix Filename prefix (e.g., "full_cohort_")
#' @return List with `summary`, `plot`, and `output_files` (paths may be NULL if not written)
summarize_tumor_size_by_treatment <- function(data, size_var = "initial_tumor_height", output_dir = NULL, prefix = "") {
    required_cols <- c("treatment_group", size_var)
    if (!all(required_cols %in% names(data))) {
        logger::log_warn(sprintf("Tumor size summary skipped: missing columns %s", paste(setdiff(required_cols, names(data)), collapse = ", ")))
        return(list(summary = NULL, plot = NULL, output_files = list(summary = NULL, plot = NULL)))
    }

    tumor_df <- data %>%
        enforce_unordered_factors() %>%
        dplyr::select(treatment_group, dplyr::all_of(size_var)) %>%
        dplyr::filter(!is.na(.data[[size_var]]))

    if (nrow(tumor_df) == 0 || length(unique(tumor_df$treatment_group)) < 2) {
        logger::log_warn("Tumor size summary skipped: insufficient data or only one treatment group present.")
        return(list(summary = NULL, plot = NULL, output_files = list(summary = NULL, plot = NULL)))
    }

    treatment_levels <- levels(factor(tumor_df$treatment_group))
    palette <- get_palette_by_variable("treatment_group", treatment_levels)
    size_label <- if (size_var == "initial_tumor_height") "Baseline tumor height (mm)" else size_var

    summary_tbl <- tumor_df %>%
        dplyr::group_by(treatment_group) %>%
        dplyr::summarise(
            n = dplyr::n(),
            mean = round(mean(.data[[size_var]], na.rm = TRUE), 2),
            sd = round(sd(.data[[size_var]], na.rm = TRUE), 2),
            median = round(stats::median(.data[[size_var]], na.rm = TRUE), 2),
            q1 = round(stats::quantile(.data[[size_var]], 0.25, na.rm = TRUE, type = 7), 2),
            q3 = round(stats::quantile(.data[[size_var]], 0.75, na.rm = TRUE, type = 7), 2),
            iqr = round(stats::IQR(.data[[size_var]], na.rm = TRUE), 2),
            min = round(min(.data[[size_var]], na.rm = TRUE), 2),
            max = round(max(.data[[size_var]], na.rm = TRUE), 2),
            .groups = "drop"
        ) %>%
        dplyr::mutate(variable = size_var, .before = 1)

    # Wilcoxon rank-sum test for group difference
    wilcox_p <- tryCatch(
        stats::wilcox.test(
            reformulate("treatment_group", response = size_var),
            data = tumor_df
        )$p.value,
        error = function(e) NA_real_
    )
    test_table <- data.frame(
        test = "Wilcoxon rank-sum",
        p_value = wilcox_p,
        significance = ifelse(is.na(wilcox_p), "Unavailable", ifelse(wilcox_p < 0.05, "Yes (p < 0.05)", "No")),
        stringsAsFactors = FALSE
    )

    # Pre-compute formatted p-value label for annotation
    format_p_for_label <- function(p) {
        if (is.na(p)) return("p = NA")
        if (p < 0.001) return("p < 0.001")
        sprintf("p = %.3f", p)
    }
    y_max <- max(tumor_df[[size_var]], na.rm = TRUE)
    y_min <- min(tumor_df[[size_var]], na.rm = TRUE)
    y_span <- if (is.finite(y_max - y_min) && (y_max - y_min) > 0) y_max - y_min else max(abs(c(y_max, y_min)), na.rm = TRUE)
    y_offset <- if (is.finite(y_span) && y_span > 0) y_span * 0.08 else 1
    annotation_df <- data.frame(
        group1 = treatment_levels[1],
        group2 = treatment_levels[2],
        y.position = y_max + y_offset,
        label = format_p_for_label(wilcox_p)
    )

    plot_obj <- ggplot2::ggplot(tumor_df, ggplot2::aes(x = treatment_group, y = .data[[size_var]], fill = treatment_group, color = treatment_group)) +
        ggplot2::geom_boxplot(width = 0.35, alpha = 0.5, color = "black", outlier.shape = NA) +
        ggplot2::geom_jitter(width = 0.12, alpha = 0.6, size = 2.4) +
        ggplot2::scale_fill_manual(values = palette) +
        ggplot2::scale_color_manual(values = palette) +
        ggplot2::labs(
            title = "Tumor size by treatment group",
            x = "Treatment group",
            y = size_label
        ) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(legend.position = "none")

    if (!any(is.na(annotation_df$group1), is.na(annotation_df$group2))) {
        plot_obj <- plot_obj + ggpubr::stat_pvalue_manual(
            annotation_df,
            label = "label",
            xmin = "group1",
            xmax = "group2",
            y.position = "y.position",
            bracket.size = 0.6,
            tip.length = 0.01,
            inherit.aes = FALSE
        )
    }

    summary_path <- NULL
    plot_path <- NULL
    if (!is.null(output_dir)) {
        if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
        }
        size_suffix <- paste0(make_filename_safe(size_var), "_")
        summary_path <- file.path(output_dir, paste0(prefix, size_suffix, "tumor_size_by_treatment_summary.xlsx"))
        plot_path <- file.path(output_dir, paste0(prefix, size_suffix, "tumor_size_by_treatment.png"))
        try(
            writexl::write_xlsx(
                list(
                    summary = summary_tbl,
                    test = test_table
                ),
                summary_path
            ),
            silent = TRUE
        )
        ggplot2::ggsave(plot_path, plot_obj, width = DEFAULT_PLOT_WIDTH, height = DEFAULT_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white")
        logger::log_info(sprintf("Tumor size summary written to %s and %s", basename(summary_path), basename(plot_path)))
    }

    list(
        summary = summary_tbl,
        test = test_table,
        plot = plot_obj,
        output_files = list(summary = summary_path, plot = plot_path)
    )
}
