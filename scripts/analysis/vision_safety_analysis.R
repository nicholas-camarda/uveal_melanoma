# Vision and Safety Analysis Functions
# Author: Nicholas Camarda
# Description: Functions for vision change and radiation sequelae analysis

#' Analyze visual acuity changes by treatment group
#'
#' Calculates and summarizes changes in visual acuity by treatment group.
#' This function is used for objective 2a and does not include subgroup interactions.
#'
#' @param data A data frame containing vision-related variables.
#' @param output_dirs A named list of output directories organized by analysis type (e.g., recurrence, mets, os, pfs, height, subgroups).
#' @param prefix A character string used as a file prefix for output files (e.g., "full_cohort_") to identify cohort or analysis context in filenames.
#'
#' @return A list with the following elements:
#'   - changes: summary data frame of vision changes by treatment group
#'   - table: gtsummary object with formatted summary statistics
#'   - regression_model: linear model (lm) object for vision change by treatment group
#'   - regression_table: gtsummary object summarizing the regression results
#'
#' @examples
#' analyze_visual_acuity_changes(data, output_dirs, prefix)
analyze_visual_acuity_changes <- function(data, output_dirs, prefix) {
    # Calculate vision changes (row-level)
    # Vision change is already calculated in data derivation (Objective 0)
    # Positive values = vision worsening (higher logMAR), negative = improvement
    
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
    line_values <- if (length(line_levels) > 0) {
        seq(
            min(summary_data$vision_line_change, na.rm = TRUE),
            max(summary_data$vision_line_change, na.rm = TRUE)
        )
    } else {
        numeric()
    }

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

    confounders_for_model <- confounders
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
                label = list(vision_line_change_label ~ "Snellen Line Change Distribution")
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
            modify_caption("Snellen Line-Change Distribution")
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
            modify_caption("Snellen Line-Change Summary (Bucketed)")
    }

    line_change_summary_tbl <- summary_data %>%
        select(treatment_group, vision_line_change) %>%
        tbl_summary(
            missing = "no",
            by = treatment_group,
            type = list(vision_line_change ~ "continuous"),
            statistic = list(vision_line_change ~ "{median} ({min}, {max})"),
            digits = list(vision_line_change ~ 0),
            label = list(vision_line_change ~ "Snellen Line Change")
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
    modify_caption("Snellen Line Change Summary")

    # Save tables
    stacked_tbls <- Filter(
        Negate(is.null),
        list(tbl_summary_obj, line_change_summary_tbl, line_change_bucket_tbl)
    )

    combined_tbl <- tbl_stack(tbls = stacked_tbls) %>%
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
        writexl::write_xlsx(
            line_change_distribution,
            path = file.path(output_dirs$obj2_vision, paste0(prefix, "vision_line_change_distribution.xlsx"))
        )
    }

    if (nrow(line_change_bucket_distribution) > 0) {
        writexl::write_xlsx(
            line_change_bucket_distribution,
            path = file.path(output_dirs$obj2_vision, paste0(prefix, "vision_line_change_bucket_summary.xlsx"))
        )
    }

    snellen_section_tbls <- Filter(
        Negate(is.null),
        list(line_change_summary_tbl, line_change_bucket_tbl, line_change_tbl)
    )

    if (length(snellen_section_tbls) > 0) {
        snellen_combo_tbl <- tbl_stack(snellen_section_tbls) %>%
            modify_caption("Snellen Line-Change Summary")

        save_gt_html(
            snellen_combo_tbl,
            filename = file.path(output_dirs$obj2_vision, paste0(prefix, "vision_line_change_summary.html"))
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
        analysis_name = "vision_change_linear",
        dataset_name = "vision_safety",
        output_dir = output_dirs$obj2_vision,
        prefix = prefix,
        sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
        filter_stats = exclusion_result$filter_stats
    )

    vision_lm <- vision_result$model
    vision_lm_tbl <- vision_result$table

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
        regression_table = vision_lm_tbl
    ))
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
    # Validate sequela type
    valid_sequelae <- c("retinopathy", "nvg", "srd")
    if (!sequela_type %in% valid_sequelae) {
        stop(sprintf(
            "Invalid sequela_type '%s'. Must be one of: %s",
            sequela_type, paste(valid_sequelae, collapse = ", ")
        ))
    }

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

    # # For SRD, filter to only radiation-induced cases as per objectives
    # Per discussion with Tim, we are no longer restricting to radiation-induced SRD only
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
    writexl::write_xlsx(
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
    if (sum(model_data[[outcome_var]] == 1, na.rm = TRUE) >= 10) { # Require at least 10 events

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
        logger::log_warn(sprintf("Insufficient events for regression modeling (%d events)", sum(data[[outcome_var]] == "Y", na.rm = TRUE)))
        safety_diagnostics <- list(
            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
            raw_model_output = sprintf(
                "Model skipped: only %d events available after sparse-level exclusions.",
                sum(model_data[[outcome_var]] == 1, na.rm = TRUE)
            ),
            sample_size_summary = build_sample_size_summary_tab(
                filter_stats = exclusion_result$filter_stats,
                dataset_name = dataset_name,
                analysis_name = logistic_analysis_name,
                modeled_n = nrow(model_data)
            )
        )
    }

    # Note: Diagnostics are now handled by the unified table generation system

    return(list(
        rates = sequela_rates,
        table = if (!is.null(regression_table)) regression_table else tbl, # Return regression table if available, otherwise summary table
        model = model_result,
        diagnostics = safety_diagnostics # Add diagnostics for consolidation
    ))
}
