# Vision and Safety Analysis Functions
# Author: Nicholas Camarda
# Description: Functions for vision change and radiation sequelae analysis

get_ordered_treatment_groups <- function(data, group_var = "treatment_group") {
    if (!group_var %in% names(data)) {
        return(character())
    }

    group_values <- data[[group_var]]
    group_values <- group_values[!is.na(group_values)]
    if (length(group_values) == 0) {
        return(character())
    }

    if (identical(group_var, "treatment_group")) {
        group_values <- normalize_treatment_group_values(group_values)
    }

    if (is.factor(group_values)) {
        return(levels(droplevels(group_values)))
    }

    unique(as.character(group_values))
}

format_effect_summary_pvalue <- function(p_value) {
    if (is.null(p_value) || length(p_value) == 0 || is.na(p_value)) {
        return("p = NA")
    }
    if (p_value < 0.001) {
        return("p < 0.001")
    }
    sprintf("p = %.3f", p_value)
}

format_continuous_summary_string <- function(values, digits = 1) {
    values <- values[!is.na(values)]
    if (length(values) == 0) {
        return(NA_character_)
    }

    sprintf(
        paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
        stats::median(values),
        min(values),
        max(values)
    )
}

build_grouped_continuous_summary <- function(data, value_var, digits = 1) {
    group_var <- "treatment_group"
    overall_stat <- format_continuous_summary_string(data[[value_var]], digits = digits)
    overall_median <- suppressWarnings(round(stats::median(data[[value_var]], na.rm = TRUE), digits))
    grouped_stats <- c(Overall = overall_stat)

    for (group_name in get_ordered_treatment_groups(data, group_var = group_var)) {
        group_values <- data %>%
            filter(as.character(.data[[group_var]]) == group_name) %>%
            pull(.data[[value_var]])
        grouped_stats[[group_name]] <- format_continuous_summary_string(group_values, digits = digits)
    }

    list(
        display_stats = grouped_stats,
        overall_estimate = ifelse(is.finite(overall_median), overall_median, NA_real_),
        n_outcome_non_missing = sum(!is.na(data[[value_var]]))
    )
}

build_summary_note <- function(display_stats, p_value = NA_real_, suffix = NULL) {
    parts <- vapply(
        names(display_stats),
        FUN.VALUE = character(1),
        FUN = function(name) sprintf("%s: %s", name, display_stats[[name]])
    )
    parts <- c(parts, format_effect_summary_pvalue(p_value))
    if (!is.null(suffix) && nzchar(suffix)) {
        parts <- c(parts, suffix)
    }
    paste(parts, collapse = "; ")
}

build_distribution_note <- function(data, category_var, detail_file_label) {
    non_missing_data <- data %>%
        filter(!is.na(.data[[category_var]]))

    p_value <- NA_real_
    if (nrow(non_missing_data) > 0 && dplyr::n_distinct(non_missing_data$treatment_group) > 1) {
        p_value <- tryCatch(
            stats::fisher.test(table(non_missing_data$treatment_group, non_missing_data[[category_var]]), simulate.p.value = TRUE)$p.value,
            error = function(e) NA_real_
        )
    }

    category_count <- dplyr::n_distinct(stats::na.omit(non_missing_data[[category_var]]))
    paste(
        sprintf("Observed %d non-missing ordered categories.", category_count),
        format_effect_summary_pvalue(p_value),
        sprintf("Detailed counts are saved in %s.", detail_file_label)
    )
}

build_binary_rate_note <- function(data, outcome_var) {
    group_var <- "treatment_group"
    overall_n <- sum(!is.na(data[[outcome_var]]))
    overall_events <- sum(data[[outcome_var]] == 1, na.rm = TRUE)
    overall_rate <- if (overall_n > 0) round(100 * overall_events / overall_n, 1) else NA_real_

    parts <- sprintf("Overall: %d/%d (%.1f%%)", overall_events, overall_n, overall_rate)

    for (group_name in get_ordered_treatment_groups(data, group_var = group_var)) {
        group_data <- data %>%
            filter(as.character(.data[[group_var]]) == group_name)
        group_n <- sum(!is.na(group_data[[outcome_var]]))
        group_events <- sum(group_data[[outcome_var]] == 1, na.rm = TRUE)
        group_rate <- if (group_n > 0) round(100 * group_events / group_n, 1) else NA_real_
        parts <- c(parts, sprintf("%s: %d/%d (%.1f%%)", group_name, group_events, group_n, group_rate))
    }

    p_value <- tryCatch(
        stats::fisher.test(table(data$treatment_group, data[[outcome_var]]), simulate.p.value = TRUE)$p.value,
        error = function(e) NA_real_
    )

    paste(c(parts, format_effect_summary_pvalue(p_value)), collapse = "; ")
}

#' Analyze visual acuity changes by treatment group
#'
#' Calculates and summarizes changes in visual acuity by treatment group.
#' This function is used for objective 2a and does not include subgroup interactions.
#'
#' @param data A data frame containing vision-related variables.
#' @param output_dirs A named list of output directories organized by analysis type (e.g., recurrence, mets, os, pfs, height, subgroups).
#' @param prefix A character string used as a file prefix for output files (e.g., "full_cohort_") to identify cohort or analysis context in filenames.
#' @param confounders Character vector of confounders to adjust for in the analysis.
#' @param dataset_name Character string dataset identifier for diagnostics and effect summaries.
#'
#' @return A list with the following elements:
#'   - changes: summary data frame of vision changes by treatment group
#'   - table: gtsummary object with formatted summary statistics
#'   - regression_model: linear model (lm) object for vision change by treatment group
#'   - regression_table: gtsummary object summarizing the regression results
#'
#' @examples
#' analyze_visual_acuity_changes(data, output_dirs, prefix)
analyze_visual_acuity_changes <- function(data, output_dirs, prefix, confounders = NULL, dataset_name = NULL) {
    data <- normalize_treatment_group_data(data)
    # Calculate vision changes (row-level)
    # Vision change is already calculated in data derivation (Objective 0)
    # Positive values = improvement (lower logMAR), negative = worsening
    
    # Ensure consistent factor contrasts for modeling
    data_with_vision_change <- enforce_unordered_factors(data)

    # Preserve the full analytic set for descriptive summaries/tests (no location filtering)
    summary_data <- data_with_vision_change %>%
        mutate(
            vision_line_change = compute_line_change_lines(vision_change),
            vision_line_change_label = categorize_line_change(vision_change),
            vision_line_change_bucket = assign_line_change_bucket(vision_line_change)
        )

    line_levels <- line_change_label_levels(summary_data$vision_line_change)
    line_values <- line_change_ordered_values(summary_data$vision_line_change)

    if (length(line_levels) > 0) {
        summary_data <- summary_data %>%
            mutate(
                vision_line_change_label = factor(vision_line_change_label, levels = line_levels, ordered = TRUE)
            )
    }

    if (!is.null(summary_data$vision_line_change_bucket)) {
        summary_data <- summary_data %>%
            mutate(
                vision_line_change_bucket = factor(
                    vision_line_change_bucket,
                    levels = VISION_LINE_CHANGE_CATEGORY_LEVELS,
                    ordered = TRUE
                )
            )
    }

    confounders_for_model <- confounders %||% character()
    confounders_for_model <- confounders_for_model[confounders_for_model %in% names(data_with_vision_change)]
    exclusion_vars <- unique(c("treatment_group", confounders_for_model))
    exclusion_result <- apply_sparse_level_exclusions(
        data_with_vision_change,
        variables = exclusion_vars[exclusion_vars %in% names(data_with_vision_change)],
        analysis_name = "vision_change_linear",
        id_col = pick_sparse_level_id_col(data_with_vision_change),
        level_exclusions = MODELING_LEVEL_EXCLUSIONS
    )

    if (exclusion_result$removed_row_count > 0) {
        logger::log_info(sprintf(
            "Excluded %d rows with sparse categorical levels prior to vision change analysis",
            exclusion_result$removed_row_count
        ))
    }

    vision_model_data <- exclusion_result$data
    line_change_model_data <- vision_model_data %>%
        mutate(
            vision_line_change = compute_line_change_lines(vision_change)
        ) %>%
        filter(!is.na(vision_line_change))

    line_change_filter_stats <- exclusion_result$filter_stats
    if (!is.null(line_change_filter_stats)) {
        line_change_removed_n <- nrow(vision_model_data) - nrow(line_change_model_data)
        line_change_filter_stats$model_n <- nrow(line_change_model_data)
        line_change_filter_stats$removed_n <- line_change_filter_stats$removed_n + line_change_removed_n
        line_change_filter_stats$removed_pct <- if (line_change_filter_stats$initial_n > 0) {
            round(100 * line_change_filter_stats$removed_n / line_change_filter_stats$initial_n, 1)
        } else {
            0
        }
        if (line_change_removed_n > 0) {
            line_change_filter_stats$removal_reason <- paste(
                exclusion_result$filter_stats$removal_reason,
                sprintf("Excluded %d additional rows with missing Snellen line-change outcome.", line_change_removed_n)
            )
        }
    }

    ordinal_model_data <- vision_model_data %>%
        mutate(
            vision_line_change = compute_line_change_lines(vision_change),
            vision_line_change_bucket = assign_line_change_bucket(vision_line_change)
        ) %>%
        filter(!is.na(vision_line_change_bucket)) %>%
        mutate(
            vision_line_change_bucket = factor(
                vision_line_change_bucket,
                levels = VISION_LINE_CHANGE_CATEGORY_LEVELS,
                ordered = TRUE
            )
        )

    ordinal_filter_stats <- exclusion_result$filter_stats
    if (!is.null(ordinal_filter_stats)) {
        ordinal_removed_n <- nrow(vision_model_data) - nrow(ordinal_model_data)
        ordinal_filter_stats$model_n <- nrow(ordinal_model_data)
        ordinal_filter_stats$removed_n <- ordinal_filter_stats$removed_n + ordinal_removed_n
        ordinal_filter_stats$removed_pct <- if (ordinal_filter_stats$initial_n > 0) {
            round(100 * ordinal_filter_stats$removed_n / ordinal_filter_stats$initial_n, 1)
        } else {
            0
        }
        if (ordinal_removed_n > 0) {
            ordinal_filter_stats$removal_reason <- paste(
                exclusion_result$filter_stats$removal_reason,
                sprintf("Excluded %d additional rows with missing Snellen line-change distribution outcome.", ordinal_removed_n)
            )
        }
    }

    # Summary statistics (grouped)
    vision_changes <- summary_data %>%
        group_by(treatment_group) %>%
        summarise(
            n = n(),
            mean_change = mean(vision_change, na.rm = TRUE),
            sd_change = sd(vision_change, na.rm = TRUE),
            median_change = median(vision_change, na.rm = TRUE),
            iqr_change = IQR(vision_change, na.rm = TRUE),
            .groups = "drop"
        )

    line_change_distribution <- tibble()
    line_change_bucket_distribution <- tibble()

    if (length(line_levels) > 0) {
        level_lookup <- tibble(
            vision_line_change_label = factor(line_levels, levels = line_levels, ordered = TRUE),
            line_change_lines = line_values
        )

        by_group <- summary_data %>%
            filter(!is.na(vision_line_change_label)) %>%
            count(treatment_group, vision_line_change_label, name = "count") %>%
            tidyr::complete(
                treatment_group,
                vision_line_change_label = level_lookup$vision_line_change_label,
                fill = list(count = 0)
            ) %>%
            group_by(treatment_group) %>%
            mutate(
                total = sum(count),
                percent = dplyr::if_else(total > 0, round(100 * count / total, 1), NA_real_)
            ) %>%
            ungroup()

        overall_distribution <- summary_data %>%
            filter(!is.na(vision_line_change_label)) %>%
            mutate(treatment_group = factor("Overall", levels = "Overall")) %>%
            count(treatment_group, vision_line_change_label, name = "count") %>%
            tidyr::complete(
                treatment_group,
                vision_line_change_label = level_lookup$vision_line_change_label,
                fill = list(count = 0)
            ) %>%
            group_by(treatment_group) %>%
            mutate(
                total = sum(count),
                percent = dplyr::if_else(total > 0, round(100 * count / total, 1), NA_real_)
            ) %>%
            ungroup()

        line_change_distribution <- bind_rows(by_group, overall_distribution) %>%
            left_join(level_lookup, by = "vision_line_change_label") %>%
            arrange(line_change_lines)
    }

    if (!all(is.na(summary_data$vision_line_change_bucket))) {
        bucket_counts <- summary_data %>%
            filter(!is.na(vision_line_change_bucket)) %>%
            count(treatment_group, vision_line_change_bucket, name = "count") %>%
            tidyr::complete(
                treatment_group,
                vision_line_change_bucket = factor(VISION_LINE_CHANGE_CATEGORY_LEVELS, levels = VISION_LINE_CHANGE_CATEGORY_LEVELS, ordered = TRUE),
                fill = list(count = 0)
            ) %>%
            group_by(treatment_group) %>%
            mutate(
                total = sum(count),
                percent = dplyr::if_else(total > 0, round(100 * count / total, 1), NA_real_)
            ) %>%
            ungroup()

        overall_bucket_counts <- summary_data %>%
            filter(!is.na(vision_line_change_bucket)) %>%
            mutate(treatment_group = factor("Overall", levels = "Overall")) %>%
            count(treatment_group, vision_line_change_bucket, name = "count") %>%
            tidyr::complete(
                treatment_group,
                vision_line_change_bucket = factor(VISION_LINE_CHANGE_CATEGORY_LEVELS, levels = VISION_LINE_CHANGE_CATEGORY_LEVELS, ordered = TRUE),
                fill = list(count = 0)
            ) %>%
            group_by(treatment_group) %>%
            mutate(
                total = sum(count),
                percent = dplyr::if_else(total > 0, round(100 * count / total, 1), NA_real_)
            ) %>%
            ungroup()

        line_change_bucket_distribution <- bind_rows(bucket_counts, overall_bucket_counts)
    }

    # Statistical test
    wilcox.test(vision_change ~ treatment_group, data = summary_data)

    # Table for publication (row-level input)
    tbl_summary_obj <- summary_data %>%
        select(treatment_group, vision_change) %>%
        tbl_summary(
            missing = "no",
            by = treatment_group,
            type = list(vision_change ~ "continuous"),
            statistic = list(vision_change ~ "{median} ({min}, {max})"),
            digits = list(all_continuous() ~ 1, all_categorical() ~ 0),
            label = list(vision_change ~ "Vision Change (logMAR)")
        ) %>%
        add_p(
            test = list(
                all_continuous() ~ "wilcox.test"
            )
        ) %>%
        add_overall() %>%
        bold_labels() %>% # Built-in gtsummary function for bold variable labels!
        modify_header(
            label = "**Characteristic**",
            stat_0 = "**Overall**\nN = {N}"
        ) %>%
        modify_caption("Vision change (logMAR)")


    line_change_bucket_tbl <- NULL
    line_change_tbl <- NULL
    if (length(line_levels) > 0) {
        line_change_tbl <- summary_data %>%
            filter(!is.na(vision_line_change_label)) %>%
            select(treatment_group, vision_line_change_label) %>%
            tbl_summary(
                missing = "no",
                by = treatment_group,
                type = list(vision_line_change_label ~ "categorical"),
                statistic = list(all_categorical() ~ "{n} ({p}%)"),
                digits = list(all_categorical() ~ 1),
                label = list(vision_line_change_label ~ "Snellen Line Change Integer Distribution")
            ) %>%
            add_p(
                test = list(
                    all_categorical() ~ "fisher.test"
                ),
                test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))
            ) %>%
            add_overall() %>%
            format_count_percent_columns() %>%
            bold_labels() %>%
            modify_header(
                label = "**Snellen Line Change**",
                stat_0 = "**Overall**\nN = {N}",
                stat_1 = "**PBT**\nN = {n}",
                stat_2 = "**GKSRS**\nN = {n}",
                p.value = "**p-value**"
            ) %>%
            modify_caption("Snellen Line Change Integer Distribution")
    }

    if (!all(is.na(summary_data$vision_line_change_bucket))) {
        line_change_bucket_tbl <- summary_data %>%
            filter(!is.na(vision_line_change_bucket)) %>%
            select(treatment_group, vision_line_change_bucket) %>%
            tbl_summary(
                missing = "no",
                by = treatment_group,
                type = list(vision_line_change_bucket ~ "categorical"),
                statistic = list(all_categorical() ~ "{n} ({p}%)"),
                digits = list(all_categorical() ~ 1),
                label = list(vision_line_change_bucket ~ "Snellen Line Change Distribution")
            ) %>%
            add_p(
                test = list(
                    all_categorical() ~ "fisher.test"
                ),
                test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))
            ) %>%
            add_overall() %>%
            format_count_percent_columns() %>%
            bold_labels() %>%
            modify_header(
                label = "**Snellen Line Change Distribution**",
                stat_0 = "**Overall**\nN = {N}",
                stat_1 = "**PBT**\nN = {n}",
                stat_2 = "**GKSRS**\nN = {n}",
                p.value = "**p-value**"
            ) %>%
            modify_caption("Snellen Line Change Distribution")
    }

    line_change_summary_tbl <- summary_data %>%
        select(treatment_group, vision_change) %>%
        tbl_summary(
            missing = "no",
            by = treatment_group,
            type = list(vision_change ~ "continuous"),
            statistic = list(vision_change ~ "{median} ({min}, {max})"),
            digits = list(vision_change ~ 1),
            label = list(vision_change ~ "Vision Change (logMAR)")
        ) %>%
        add_p(
            test = list(
                all_continuous() ~ "wilcox.test"
            )
        ) %>%
        add_overall() %>%
        bold_labels() %>%
        modify_header(
            label = "**Characteristic**",
            stat_0 = "**Overall**\nN = {N}"
        ) %>%
        convert_logmar_summary_table_to_line_summary(
            label = "Snellen Line Change",
            caption = "Snellen Line Change Summary"
        )

    # Save tables
    stacked_tbls <- Filter(
        Negate(is.null),
        list(tbl_summary_obj, line_change_summary_tbl, line_change_bucket_tbl)
    )

    combined_tbl <- quiet_tbl_stack(tbls = stacked_tbls) %>%
        modify_caption("Vision changes overview") %>%
        modify_table_styling(
            columns = "p.value",
            rows = .data$row_type == "label",
            footnote = NA_character_
        ) %>%
        modify_table_styling(
            columns = "p.value",
            footnote = "Wilcoxon rank-sum test for continuous rows; Fisher's exact test (simulated p-value) for categorical rows."
        )

    save_gt_html(
        combined_tbl,
        filename = file.path(output_dirs$obj2_vision, paste0(prefix, "vision_changes.html"))
    )

    if (nrow(line_change_distribution) > 0) {
        write_readable_xlsx(
            line_change_distribution,
            path = file.path(output_dirs$obj2_vision, paste0(prefix, "snellen_line_change_integer_distribution.xlsx"))
        )
    }

    if (nrow(line_change_bucket_distribution) > 0) {
        write_readable_xlsx(
            line_change_bucket_distribution,
            path = file.path(output_dirs$obj2_vision, paste0(prefix, "snellen_line_change_distribution_summary.xlsx"))
        )
    }

    snellen_section_tbls <- Filter(
        Negate(is.null),
        list(line_change_summary_tbl, line_change_bucket_tbl, line_change_tbl)
    )

    if (length(snellen_section_tbls) > 0) {
        snellen_combo_tbl <- quiet_tbl_stack(snellen_section_tbls) %>%
            modify_caption("Snellen Line Change Descriptive Summary")

        save_gt_html(
            snellen_combo_tbl,
            filename = file.path(output_dirs$obj2_vision, paste0(prefix, "snellen_line_change_descriptive_summary.html"))
        )
    }

    # Linear regression model
    logger::log_info("Fitting linear regression model for vision changes")

    # Use the unified table generation system for linear regression
    # Use the same standardized confounders as all other analyses
    vision_result <- generate_regression_table(
        data = vision_model_data,
        outcome_var = "vision_change",
        predictor_vars = "treatment_group",
        confounders = confounders_for_model,
        model_type = "linear",
        effect_measure = "MD", # Mean Difference for continuous outcome
        analysis_name = "logmar_vision_change_adjusted",
        dataset_name = dataset_name %||% "vision_safety",
        output_dir = output_dirs$obj2_vision,
        prefix = prefix,
        sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
        filter_stats = exclusion_result$filter_stats
    )

    vision_lm <- vision_result$model
    vision_lm_tbl <- vision_result$table

    line_change_result <- generate_regression_table(
        data = line_change_model_data,
        outcome_var = "vision_line_change",
        predictor_vars = "treatment_group",
        confounders = confounders_for_model,
        model_type = "linear",
        effect_measure = "MD",
        analysis_name = "snellen_line_change_adjusted",
        dataset_name = dataset_name %||% "vision_safety",
        output_dir = output_dirs$obj2_vision,
        prefix = prefix,
        sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
        filter_stats = line_change_filter_stats
    )

    line_change_lm <- line_change_result$model
    line_change_lm_tbl <- line_change_result$table

    line_change_ordinal_result <- generate_regression_table(
        data = ordinal_model_data,
        outcome_var = "vision_line_change_bucket",
        predictor_vars = "treatment_group",
        confounders = confounders_for_model,
        model_type = "ordinal",
        effect_measure = "OR",
        analysis_name = "snellen_line_change_distribution_adjusted",
        dataset_name = dataset_name %||% "vision_safety",
        output_dir = output_dirs$obj2_vision,
        prefix = prefix,
        sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
        filter_stats = ordinal_filter_stats
    )

    line_change_ordinal_model <- line_change_ordinal_result$model
    line_change_ordinal_tbl <- line_change_ordinal_result$table

    logmar_summary <- build_grouped_continuous_summary(summary_data, "vision_change", digits = 1)
    logmar_p_value <- tryCatch(
        stats::wilcox.test(vision_change ~ treatment_group, data = summary_data)$p.value,
        error = function(e) NA_real_
    )

    snellen_summary_strings <- convert_logmar_summary_stat_to_line_summary(unname(logmar_summary$display_stats))
    names(snellen_summary_strings) <- names(logmar_summary$display_stats)
    snellen_overall_estimate <- if (is.na(logmar_summary$overall_estimate)) {
        NA_real_
    } else {
        compute_line_change_lines(logmar_summary$overall_estimate)
    }

    logmar_unadjusted_model <- fit_regression_model(
        data = vision_model_data,
        formula = build_model_formula("vision_change", "treatment_group", character(), "linear"),
        model_type = "linear"
    )
    snellen_line_unadjusted_model <- fit_regression_model(
        data = line_change_model_data,
        formula = build_model_formula("vision_line_change", "treatment_group", character(), "linear"),
        model_type = "linear"
    )
    snellen_distribution_unadjusted_model <- fit_regression_model(
        data = ordinal_model_data,
        formula = build_model_formula("vision_line_change_bucket", "treatment_group", character(), "ordinal"),
        model_type = "ordinal"
    )

    vision_effect_summary <- bind_effect_summary_rows(
        create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "LogMAR Vision Change",
            model_label = "Descriptive",
            term = "summary",
            model_formula = "Descriptive summary",
            covariates_used = "None",
            effect_measure = "Median (Min, Max)",
            estimate = logmar_summary$overall_estimate,
            n_patients = nrow(summary_data),
            n_outcome_non_missing = logmar_summary$n_outcome_non_missing,
            data_source = "Displayed descriptive summary",
            model_status = "DESCRIPTIVE",
            notes = build_summary_note(logmar_summary$display_stats, logmar_p_value)
        ),
        summarize_effect_model(
            model = logmar_unadjusted_model,
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "LogMAR Vision Change",
            model_label = "Unadjusted linear",
            group_var = "treatment_group",
            data_source_label = "Filtered vision-change dataset without covariates",
            effect_measure = "MD",
            outcome_var = "vision_change"
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "LogMAR Vision Change",
            model_label = "Unadjusted linear",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula("vision_change", "treatment_group"),
            covariates_used = "None",
            effect_measure = "MD",
            n_patients = nrow(vision_model_data),
            n_outcome_non_missing = sum(!is.na(vision_model_data$vision_change)),
            data_source = "Filtered vision-change dataset without covariates",
            model_status = "SKIPPED",
            notes = "Unadjusted linear model could not be fit."
        ),
        summarize_effect_model(
            model = vision_lm,
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "LogMAR Vision Change",
            model_label = "Adjusted linear",
            group_var = "treatment_group",
            data_source_label = "Filtered vision-change dataset with confounders",
            effect_measure = "MD",
            outcome_var = "vision_change"
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "LogMAR Vision Change",
            model_label = "Adjusted linear",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula("vision_change", "treatment_group", confounders_for_model),
            covariates_used = format_effect_summary_covariates(confounders_for_model),
            effect_measure = "MD",
            n_patients = nrow(vision_model_data),
            n_outcome_non_missing = sum(!is.na(vision_model_data$vision_change)),
            data_source = "Filtered vision-change dataset with confounders",
            model_status = "SKIPPED",
            notes = as.character(vision_result$diagnostics$raw_model_output %||% "Adjusted linear model could not be fit.")
        ),
        create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change",
            model_label = "Descriptive",
            term = "summary",
            model_formula = "Descriptive summary",
            covariates_used = "None",
            effect_measure = "Median (Min, Max)",
            estimate = snellen_overall_estimate,
            n_patients = nrow(summary_data),
            n_outcome_non_missing = logmar_summary$n_outcome_non_missing,
            data_source = "Displayed descriptive summary converted from logMAR",
            model_status = "DESCRIPTIVE",
            notes = build_summary_note(snellen_summary_strings, logmar_p_value)
        ),
        summarize_effect_model(
            model = snellen_line_unadjusted_model,
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change",
            model_label = "Unadjusted linear",
            group_var = "treatment_group",
            data_source_label = "Filtered Snellen line-change dataset without covariates",
            effect_measure = "MD",
            outcome_var = "vision_line_change"
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change",
            model_label = "Unadjusted linear",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula("vision_line_change", "treatment_group"),
            covariates_used = "None",
            effect_measure = "MD",
            n_patients = nrow(line_change_model_data),
            n_outcome_non_missing = sum(!is.na(line_change_model_data$vision_line_change)),
            data_source = "Filtered Snellen line-change dataset without covariates",
            model_status = "SKIPPED",
            notes = "Unadjusted Snellen line-change model could not be fit."
        ),
        summarize_effect_model(
            model = line_change_lm,
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change",
            model_label = "Adjusted linear",
            group_var = "treatment_group",
            data_source_label = "Filtered Snellen line-change dataset with confounders",
            effect_measure = "MD",
            outcome_var = "vision_line_change"
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change",
            model_label = "Adjusted linear",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula("vision_line_change", "treatment_group", confounders_for_model),
            covariates_used = format_effect_summary_covariates(confounders_for_model),
            effect_measure = "MD",
            n_patients = nrow(line_change_model_data),
            n_outcome_non_missing = sum(!is.na(line_change_model_data$vision_line_change)),
            data_source = "Filtered Snellen line-change dataset with confounders",
            model_status = "SKIPPED",
            notes = as.character(line_change_result$diagnostics$raw_model_output %||% "Adjusted Snellen line-change model could not be fit.")
        ),
        create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change Distribution",
            model_label = "Descriptive",
            term = "summary",
            model_formula = "Descriptive summary",
            covariates_used = "None",
            effect_measure = "Distribution",
            n_patients = nrow(summary_data),
            n_outcome_non_missing = sum(!is.na(summary_data$vision_line_change_bucket)),
            data_source = "Displayed categorical distribution summary",
            model_status = "DESCRIPTIVE",
            notes = build_distribution_note(
                summary_data,
                category_var = "vision_line_change_bucket",
                detail_file_label = paste0(prefix, "snellen_line_change_distribution_summary.xlsx")
            )
        ),
        summarize_effect_model(
            model = snellen_distribution_unadjusted_model,
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change Distribution",
            model_label = "Unadjusted ordinal logistic",
            group_var = "treatment_group",
            data_source_label = "Filtered Snellen line-change distribution dataset without covariates",
            effect_measure = "OR",
            outcome_var = "vision_line_change_bucket"
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change Distribution",
            model_label = "Unadjusted ordinal logistic",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula("vision_line_change_bucket", "treatment_group"),
            covariates_used = "None",
            effect_measure = "OR",
            n_patients = nrow(ordinal_model_data),
            n_outcome_non_missing = sum(!is.na(ordinal_model_data$vision_line_change_bucket)),
            data_source = "Filtered Snellen line-change distribution dataset without covariates",
            model_status = "SKIPPED",
            notes = "Unadjusted ordinal Snellen distribution model could not be fit."
        ),
        summarize_effect_model(
            model = line_change_ordinal_model,
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change Distribution",
            model_label = "Adjusted ordinal logistic",
            group_var = "treatment_group",
            data_source_label = "Filtered Snellen line-change distribution dataset with confounders",
            effect_measure = "OR",
            outcome_var = "vision_line_change_bucket"
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change Distribution",
            model_label = "Adjusted ordinal logistic",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula("vision_line_change_bucket", "treatment_group", confounders_for_model),
            covariates_used = format_effect_summary_covariates(confounders_for_model),
            effect_measure = "OR",
            n_patients = nrow(ordinal_model_data),
            n_outcome_non_missing = sum(!is.na(ordinal_model_data$vision_line_change_bucket)),
            data_source = "Filtered Snellen line-change distribution dataset with confounders",
            model_status = "SKIPPED",
            notes = as.character(line_change_ordinal_result$diagnostics$raw_model_output %||% "Adjusted ordinal Snellen distribution model could not be fit.")
        )
    )

    write_effect_summary_workbook(
        effect_summary_rows = vision_effect_summary,
        output_dir = output_dirs$obj2_vision,
        prefix = prefix,
        analysis_name = "vision"
    )

    # Note: Table formatting and saving are now handled by the unified table generation system

    return(list(
        changes = vision_changes,
        table = tbl_summary_obj,
        line_change_distribution = line_change_distribution,
        line_change_bucket_distribution = line_change_bucket_distribution,
        line_change_table = line_change_tbl,
        line_change_bucket_table = line_change_bucket_tbl,
        line_change_summary_table = line_change_summary_tbl,
        regression_model = vision_lm,
        regression_table = vision_lm_tbl,
        line_change_regression_model = line_change_lm,
        line_change_regression_table = line_change_lm_tbl,
        line_change_regression_diagnostics = line_change_result$diagnostics,
        line_change_bucket_regression_model = line_change_ordinal_model,
        line_change_bucket_regression_table = line_change_ordinal_tbl,
        line_change_bucket_regression_diagnostics = line_change_ordinal_result$diagnostics,
        effect_summary = vision_effect_summary
    ))
}

#' Build skip diagnostics for sparse binary outcome models
#'
#' Creates a compact summary of why an adjusted binary outcome model was not fit,
#' including the total number of modeled events, sparse-level exclusions, and
#' outcome counts within each modeled covariate level. Levels with zero events or
#' all events are flagged because they indicate separation risk if the model were
#' forced.
#'
#' @param data Data frame used for the adjusted model after exclusions.
#' @param outcome_var Character scalar naming the binary outcome column encoded as 0/1.
#' @param variables Character vector of modeled variables to summarize.
#' @param minimum_events Integer minimum number of events required to attempt fitting
#'   (defaults to `MINIMUM_ADJUSTED_LOGISTIC_EVENTS`).
#' @param sparse_level_diagnostics Optional data frame of rows removed before modeling.
#' @param analysis_name Character scalar analysis identifier.
#' @param dataset_name Character scalar dataset identifier.
#'
#' @return Named list compatible with the shared skip-report renderer.
build_binary_skip_diagnostics <- function(data,
                                          outcome_var,
                                          variables,
                                          minimum_events = MINIMUM_ADJUSTED_LOGISTIC_EVENTS,
                                          sparse_level_diagnostics = NULL,
                                          analysis_name = "analysis",
                                          dataset_name = "unspecified_dataset") {
    modeled_n <- nrow(data)
    modeled_events <- sum(data[[outcome_var]] == 1, na.rm = TRUE)
    modeled_nonevents <- sum(data[[outcome_var]] == 0, na.rm = TRUE)
    event_support <- build_level_support_tab(data, variables, outcome_var = outcome_var)

    sparse_exclusion_summary <- if (is.null(sparse_level_diagnostics) || nrow(sparse_level_diagnostics) == 0) {
        "None"
    } else {
        sparse_level_diagnostics %>%
            dplyr::mutate(level_label = paste0(variable, "=", level, " (n=", observed_n, ")")) %>%
            dplyr::pull(level_label) %>%
            paste(collapse = "; ")
    }

    flagged_levels <- if (is.null(event_support) || nrow(event_support) == 0) {
        character()
    } else {
        event_support %>%
            dplyr::filter(support_flag != "usable") %>%
            dplyr::mutate(level_label = paste0(variable, "=", level, " [", support_flag, "]")) %>%
            dplyr::pull(level_label)
    }

    narrative_lines <- c(
        sprintf(
            "Adjusted model not attempted because only %d outcome events remained in %d modeled patients after exclusions; the pipeline requires at least %d events for adjusted logistic regression.",
            modeled_events,
            modeled_n,
            minimum_events
        ),
        sprintf(
            "Modeled data contained %d non-events and %d events.",
            modeled_nonevents,
            modeled_events
        ),
        sprintf(
            "Sparse-level exclusions before modeling: %s.",
            sparse_exclusion_summary
        )
    )

    if (length(flagged_levels) > 0) {
        narrative_lines <- c(
            narrative_lines,
            sprintf(
                "If the model were forced, these covariate levels show separation risk because all observed outcomes fall in one category: %s.",
                paste(flagged_levels, collapse = "; ")
            )
        )
    }

    build_skip_report_diagnostics(
        status = "skipped",
        analysis_name = analysis_name,
        dataset_name = dataset_name,
        reason = paste(narrative_lines, collapse = " "),
        narrative_lines = narrative_lines,
        skip_summary = build_skip_summary_tab(list(
            modeled_n = modeled_n,
            modeled_events = modeled_events,
            modeled_non_events = modeled_nonevents,
            minimum_events_required = minimum_events,
            events_shortfall = max(minimum_events - modeled_events, 0),
            sparse_exclusions = sparse_exclusion_summary,
            separation_risk_levels = if (length(flagged_levels) > 0) {
                paste(flagged_levels, collapse = "; ")
            } else {
                "None detected"
            }
        )),
        sparse_level_diagnostics = sparse_level_diagnostics,
        event_support = event_support,
        raw_model_output = sprintf(
            "Model skipped: only %d events available after sparse-level exclusions.",
            modeled_events
        )
    )
}

#' Analyze radiation complications
#'
#' Analyze rates of radiation complications (retinopathy, nvg, srd) by treatment group.
#' This function reuses the existing analyze_binary_outcome_rates function for consistency.
#'
#' @param data A data frame containing radiation sequelae variables.
#' @param sequela_type Character. The type of sequela to analyze. Must be one of "retinopathy", "nvg", or "srd".
#' @param confounders Character vector of confounders to adjust for in the analysis. Default is NULL.
#' @param dataset_name Character. Name of the dataset for output files. Default is NULL.
#' @param output_dirs List of output directories organized by analysis type (recurrence, mets, os, pfs, height, subgroups, etc.).
#' @param prefix Character string used as a file prefix for output files (e.g., "full_cohort_"). Used to identify cohort or analysis context in filenames.
#'
#' @return A list of results from analyze_binary_outcome_rates, including model output and summary tables.
#' @examples
#' analyze_radiation_complications(data, "retinopathy", confounders, "uveal_full", output_dirs, prefix)
analyze_radiation_complications <- function(data, sequela_type, confounders = NULL, dataset_name = NULL, output_dirs = NULL, prefix = NULL) {
    data <- normalize_treatment_group_data(data)
    # Validate sequela type
    valid_sequelae <- c("retinopathy", "nvg", "srd")
    if (!sequela_type %in% valid_sequelae) {
        stop(sprintf(
            "Invalid sequela_type '%s'. Must be one of: %s",
            sequela_type, paste(valid_sequelae, collapse = ", ")
        ))
    }
    sequela_label <- switch(sequela_type,
        retinopathy = "Retinopathy",
        nvg = "Neovascular Glaucoma",
        srd = "Serous Retinal Detachment"
    )

    collapse_binary_summary_to_cases <- function(tbl) {
        tbl %>%
            modify_table_body(function(body) {
                case_rows <- body %>%
                    filter(row_type == "level", label %in% c("Y", "Yes")) %>%
                    select(variable, dplyr::starts_with("stat_"), dplyr::any_of("p.value"))

                label_rows <- body %>%
                    filter(row_type == "label") %>%
                    left_join(case_rows, by = "variable", suffix = c("", "_cases")) %>%
                    mutate(
                        stat_0 = coalesce(stat_0_cases, stat_0),
                        stat_1 = coalesce(stat_1_cases, stat_1),
                        stat_2 = coalesce(stat_2_cases, stat_2),
                        p.value = coalesce(p.value_cases, p.value)
                    ) %>%
                    select(names(body))

                label_rows
            })
    }

    # Historical note: earlier docs described radiation-induced-only SRD, but the
    # published collaborator-aligned implementation intentionally keeps all recorded
    # SRD causes. The old filter is retained here commented for provenance only.
    # if (sequela_type == "srd") {
    #     logger::log_info("Filtering SRD to only radiation-induced causes")
    #     original_n <- nrow(data)
    #     # Check what values exist in srd_cause
    #     if ("srd_cause" %in% names(data)) {
    #         logger::log_info("Available srd_cause values:")
    #         print(table(data$srd_cause, useNA = "ifany"))
    #     }

    #     # Filter for radiation-induced SRD analysis: exclude patients with mass-induced SRD
    #     data <- data %>%
    #         filter(
    #             # Keep patients without SRD
    #             srd == "N" | is.na(srd) |
    #                 # Keep patients with radiation-induced SRD (exclude mass-induced)
    #                 (srd == "Y" & srd_cause == "Radiation")
    #         )
    #     logger::log_info(sprintf("Data filtered for radiation-induced SRD: %d -> %d patients", original_n, nrow(data)))
    # }

    # Ensure consistent factor contrasts for modeling
    data <- enforce_unordered_factors(data)

    # Retain a copy without additional filtering for descriptive outputs
    summary_data <- data

    confounders_for_model <- if (is.null(confounders)) character() else confounders
    exclusion_vars <- unique(c("treatment_group", confounders_for_model))
    exclusion_result <- apply_sparse_level_exclusions(
        data,
        variables = exclusion_vars[exclusion_vars %in% names(data)],
        analysis_name = paste0(sequela_type, "_logistic"),
        id_col = pick_sparse_level_id_col(data),
        level_exclusions = MODELING_LEVEL_EXCLUSIONS
    )

    if (exclusion_result$removed_row_count > 0) {
        logger::log_info(sprintf(
            "Excluded %d rows with sparse categorical levels prior to %s analysis",
            exclusion_result$removed_row_count,
            sequela_type
        ))
    }

    model_data <- exclusion_result$data

    # Check if outcome variable exists
    outcome_var <- sequela_type
    if (!outcome_var %in% names(data)) {
        stop(sprintf(
            "Missing required variable for %s analysis: %s",
            sequela_type, outcome_var
        ))
    }

    logger::log_info(sprintf("Analyzing %s rates (binary outcome)", toupper(sequela_type)))

    # Convert to binary if needed and ensure it's numeric for glm
    model_data <- model_data %>%
        mutate(
            !!outcome_var := case_when(
                .data[[outcome_var]] == "Y" ~ 1,
                .data[[outcome_var]] == "N" ~ 0,
                is.na(.data[[outcome_var]]) ~ 0,
                TRUE ~ 0
            )
        )

    summary_rates_data <- summary_data %>%
        mutate(
            !!outcome_var := case_when(
                .data[[outcome_var]] == "Y" ~ 1,
                .data[[outcome_var]] == "N" ~ 0,
                is.na(.data[[outcome_var]]) ~ 0,
                TRUE ~ 0
            )
        )

    # Calculate rates by treatment group
    sequela_rates <- summary_rates_data %>%
        group_by(treatment_group) %>%
        summarise(
            n_total = n(),
            n_events = sum(.data[[outcome_var]] == 1, na.rm = TRUE),
            rate_percent = round(100 * n_events / n_total, 1),
            .groups = "drop"
        )

    # Determine output directory
    output_dir <- switch(sequela_type,
        "retinopathy" = output_dirs$obj2_retinopathy,
        "nvg" = output_dirs$obj2_nvg,
        "srd" = output_dirs$obj2_srd,
        output_dirs$obj2_retinopathy
    ) # fallback to retinopathy folder

    # Save rates summary
    write_readable_xlsx(
        sequela_rates,
        file.path(output_dir, paste0(prefix, sequela_type, "_rates_summary.xlsx"))
    )

    # Create summary table
    tbl_summary_obj <- summary_data %>%
        select(treatment_group, all_of(outcome_var)) %>%
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
        add_p(
            test = list(
                all_categorical() ~ "fisher.test",
                all_continuous() ~ "wilcox.test"
            ),
            test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))
        ) %>%
        bold_labels() %>%
        modify_header(
            label = "**Characteristic**",
            stat_0 = "**Overall**\nN = {N}",
            stat_1 = "**PBT**\nN = {n}",
            stat_2 = "**GKSRS**\nN = {n}",
            p.value = "**p-value**"
        ) %>%
        modify_caption(paste("Rates of", tools::toTitleCase(sequela_type), "by Treatment Group")) %>%
        collapse_binary_summary_to_cases()

    # Convert to gt table and save
    tbl <- tbl_summary_obj %>%
        as_gt() %>%
        tab_source_note(
            source_note = md("Summary table generated automatically.")
        )

    # Save summary table
    save_gt_html(
        tbl,
        filename = file.path(output_dir, paste0(prefix, sequela_type, "_summary_table.html"))
    )

    # Fit logistic regression if there are enough events and confounders
    model_result <- NULL
    safety_diagnostics <- NULL
    regression_table <- NULL
    logistic_analysis_name <- paste0(sequela_type, "_logistic")
    if (sum(model_data[[outcome_var]] == 1, na.rm = TRUE) >= MINIMUM_ADJUSTED_LOGISTIC_EVENTS) {

        # Use the unified table generation system for logistic regression
        # Use standardized confounders from centralized configuration
        srd_confounders <- confounders_for_model

        regression_result <- generate_regression_table(
            data = model_data,
            outcome_var = outcome_var,
            predictor_vars = "treatment_group",
            confounders = srd_confounders,
            model_type = "logistic",
            effect_measure = "OR",
            analysis_name = logistic_analysis_name,
            dataset_name = dataset_name,
            output_dir = output_dir,
            prefix = prefix,
            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
            filter_stats = exclusion_result$filter_stats
        )

        # Extract the model and table from the result
        model_result <- regression_result$model
        safety_diagnostics <- regression_result$diagnostics
        regression_table <- regression_result$table # Get the regression table
    } else {
        modeled_events <- sum(model_data[[outcome_var]] == 1, na.rm = TRUE)
        skip_diagnostics <- build_binary_skip_diagnostics(
            data = model_data,
            outcome_var = outcome_var,
            variables = unique(c("treatment_group", confounders_for_model)),
            minimum_events = MINIMUM_ADJUSTED_LOGISTIC_EVENTS,
            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
            analysis_name = logistic_analysis_name,
            dataset_name = dataset_name
        )
        logger::log_warn(sprintf("Insufficient events for regression modeling (%d events)", modeled_events))
        safety_diagnostics <- skip_diagnostics
        safety_diagnostics$sample_size_summary <- build_sample_size_summary_tab(
            filter_stats = exclusion_result$filter_stats,
            dataset_name = dataset_name,
            analysis_name = logistic_analysis_name,
            modeled_n = nrow(model_data)
        )

        save_skipped_model_outputs(
            analysis_name = logistic_analysis_name,
            dataset_name = dataset_name,
            output_dir = output_dir,
            prefix = prefix,
            reason = skip_diagnostics$reason,
            diagnostics = safety_diagnostics
        )
    }

    # Note: Diagnostics are now handled by the unified table generation system

    unadjusted_model <- fit_regression_model(
        data = model_data,
        formula = build_model_formula(outcome_var, "treatment_group", character(), "logistic"),
        model_type = "logistic"
    )

    effect_summary_rows <- bind_effect_summary_rows(
        create_effect_summary_rows(
            dataset_name = dataset_name %||% "unspecified_dataset",
            analysis_label = sequela_label,
            model_label = "Descriptive",
            term = "summary",
            model_formula = "Descriptive summary",
            covariates_used = "None",
            effect_measure = "Rate (%)",
            estimate = if (nrow(sequela_rates) > 0) round(100 * sum(summary_rates_data[[outcome_var]] == 1, na.rm = TRUE) / nrow(summary_rates_data), 1) else NA_real_,
            n_patients = nrow(summary_rates_data),
            n_events = sum(summary_rates_data[[outcome_var]] == 1, na.rm = TRUE),
            n_outcome_non_missing = sum(!is.na(summary_rates_data[[outcome_var]])),
            data_source = "Displayed descriptive rates summary",
            model_status = "DESCRIPTIVE",
            notes = build_binary_rate_note(summary_rates_data, outcome_var)
        ),
        summarize_effect_model(
            model = unadjusted_model,
            dataset_name = dataset_name %||% "unspecified_dataset",
            analysis_label = sequela_label,
            model_label = "Unadjusted logistic",
            group_var = "treatment_group",
            data_source_label = "Filtered sequela dataset without covariates",
            effect_measure = "OR",
            outcome_var = outcome_var
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "unspecified_dataset",
            analysis_label = sequela_label,
            model_label = "Unadjusted logistic",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula(outcome_var, "treatment_group"),
            covariates_used = "None",
            effect_measure = "OR",
            n_patients = nrow(model_data),
            n_events = sum(model_data[[outcome_var]] == 1, na.rm = TRUE),
            n_outcome_non_missing = sum(!is.na(model_data[[outcome_var]])),
            data_source = "Filtered sequela dataset without covariates",
            model_status = "SKIPPED",
            notes = "Unadjusted logistic model could not be fit."
        ),
        summarize_effect_model(
            model = model_result,
            dataset_name = dataset_name %||% "unspecified_dataset",
            analysis_label = sequela_label,
            model_label = "Adjusted logistic",
            group_var = "treatment_group",
            data_source_label = "Filtered sequela dataset with confounders",
            effect_measure = "OR",
            outcome_var = outcome_var
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "unspecified_dataset",
            analysis_label = sequela_label,
            model_label = "Adjusted logistic",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula(outcome_var, "treatment_group", confounders_for_model),
            covariates_used = format_effect_summary_covariates(confounders_for_model),
            effect_measure = "OR",
            n_patients = nrow(model_data),
            n_events = sum(model_data[[outcome_var]] == 1, na.rm = TRUE),
            n_outcome_non_missing = sum(!is.na(model_data[[outcome_var]])),
            data_source = "Filtered sequela dataset with confounders",
            model_status = "SKIPPED",
            notes = if (is.list(safety_diagnostics) && !is.null(safety_diagnostics$raw_model_output)) {
                paste(as.character(safety_diagnostics$raw_model_output), collapse = " ")
            } else {
                "Adjusted logistic model could not be fit."
            }
        )
    )

    write_effect_summary_workbook(
        effect_summary_rows = effect_summary_rows,
        output_dir = output_dir,
        prefix = prefix,
        analysis_name = sequela_label
    )

    return(list(
        rates = sequela_rates,
        table = if (!is.null(regression_table)) regression_table else tbl, # Return regression table if available, otherwise summary table
        model = model_result,
        diagnostics = safety_diagnostics, # Add diagnostics for consolidation
        effect_summary = effect_summary_rows
    ))
}
