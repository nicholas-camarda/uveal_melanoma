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
#' @param other_map Optional named list for additional mapping or customization used by downstream table generation.
#'
#' @return A list with the following elements:
#'   - changes: summary data frame of vision changes by treatment group
#'   - table: gtsummary object with formatted summary statistics
#'   - regression_model: linear model (lm) object for vision change by treatment group
#'   - regression_table: gtsummary object summarizing the regression results
#'
#' @examples
#' analyze_visual_acuity_changes(data, output_dirs, prefix)
analyze_visual_acuity_changes <- function(data, output_dirs, prefix, other_map = list()) {
    # Calculate vision changes (row-level)
    data_with_vision_change <- data %>%
        mutate(
            # Calculate vision change as the difference between the initial
            # vision and the vision at the time of recurrence *or* last follow-up
            # Post treatment1 vision = recurrence1 pretreatment vision
            vision_change = case_when(
                recurrence1 == "Y" ~ initial_vision - recurrence1_pretreatment_vision,
                TRUE ~ initial_vision - last_vision
            )
        )

    # Ensure consistent factor contrasts for modeling
    data_with_vision_change <- enforce_unordered_factors(data_with_vision_change)

    # Summary statistics (grouped)
    vision_changes <- data_with_vision_change %>%
        group_by(treatment_group) %>%
        summarise(
            n = n(),
            mean_change = mean(vision_change, na.rm = TRUE),
            sd_change = sd(vision_change, na.rm = TRUE),
            median_change = median(vision_change, na.rm = TRUE),
            iqr_change = IQR(vision_change, na.rm = TRUE),
            .groups = "drop"
        )

    # Statistical test
    wilcox.test(vision_change ~ treatment_group, data = data_with_vision_change)

    # Table for publication (row-level input)
    tbl_summary_obj <- data_with_vision_change %>%
        select(treatment_group, vision_change) %>%
        tbl_summary(
            missing = "no",
            by = treatment_group,
            type = list(vision_change ~ "continuous"),
            statistic = list(vision_change ~ "{mean} ({sd})"),
            digits = list(vision_change ~ 2)
        ) %>%
        add_p(test = list(all_continuous() ~ "wilcox.test")) %>%
        add_overall() %>%
        bold_labels() %>% # Built-in gtsummary function for bold variable labels!
        modify_header(
            label = "**Characteristic**",
            stat_0 = "**Overall**\nN = {N}"
        ) %>%
        modify_caption("Vision Changes Analysis") %>%
        as_gt()

    # Save table
    save_gt_html(
        tbl_summary_obj,
        filename = file.path(output_dirs$obj2_vision, paste0(prefix, "vision_changes.html"))
    )

    # Linear regression model
    log_enhanced("Fitting linear regression model for vision changes")

    # Use the unified table generation system for linear regression
    # Use the same standardized confounders as all other analyses
    vision_result <- generate_regression_table(
        data = data_with_vision_change,
        outcome_var = "vision_change",
        predictor_vars = "treatment_group",
        confounders = confounders,
        model_type = "linear",
        effect_measure = "MD", # Mean Difference for continuous outcome
        analysis_name = "vision_change_linear",
        dataset_name = "vision_safety",
        output_dir = output_dirs$obj2_vision,
        prefix = prefix,
        # handle_rare = FALSE, # REMOVED
        other_map = other_map
    )

    vision_lm <- vision_result$model
    vision_lm_tbl <- vision_result$table

    # Note: Table formatting and saving are now handled by the unified table generation system

    return(list(
        changes = vision_changes,
        table = tbl_summary_obj,
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
#' @param other_map List. Additional mapping or arguments to pass to the analysis. Default is empty list.
#' @param output_dirs List of output directories organized by analysis type (recurrence, mets, os, pfs, height, subgroups, etc.).
#' @param prefix Character string used as a file prefix for output files (e.g., "full_cohort_"). Used to identify cohort or analysis context in filenames.
#'
#' @return A list of results from analyze_binary_outcome_rates, including model output and summary tables.
#' @examples
#' analyze_radiation_complications(data, "retinopathy", confounders, "uveal_full", other_map, output_dirs, prefix)
analyze_radiation_complications <- function(data, sequela_type, confounders = NULL, dataset_name = NULL, other_map = list(), output_dirs = NULL, prefix = NULL) {
    # Validate sequela type
    valid_sequelae <- c("retinopathy", "nvg", "srd")
    if (!sequela_type %in% valid_sequelae) {
        stop(sprintf(
            "Invalid sequela_type '%s'. Must be one of: %s",
            sequela_type, paste(valid_sequelae, collapse = ", ")
        ))
    }

    # For SRD, filter to only radiation-induced cases as per objectives
    if (sequela_type == "srd") {
        log_enhanced("Filtering SRD to only radiation-induced causes")
        original_n <- nrow(data)
        # Check what values exist in srd_cause
        if ("srd_cause" %in% names(data)) {
            log_enhanced("Available srd_cause values:")
            print(table(data$srd_cause, useNA = "ifany"))
        }

        # Filter for radiation-induced SRD analysis: exclude patients with mass-induced SRD
        data <- data %>%
            filter(
                # Keep patients without SRD
                srd == "N" | is.na(srd) |
                    # Keep patients with radiation-induced SRD (exclude mass-induced)
                    (srd == "Y" & srd_cause == "Radiation")
            )
        log_enhanced(sprintf("Data filtered for radiation-induced SRD: %d -> %d patients", original_n, nrow(data)))
    }

    # Ensure consistent factor contrasts for modeling
    data <- enforce_unordered_factors(data)

    # Check if outcome variable exists
    outcome_var <- sequela_type
    if (!outcome_var %in% names(data)) {
        stop(sprintf(
            "Missing required variable for %s analysis: %s",
            sequela_type, outcome_var
        ))
    }

    log_enhanced(sprintf("Analyzing %s rates (binary outcome)", toupper(sequela_type)))

    # Convert to binary if needed and ensure it's numeric for glm
    data <- data %>%
        mutate(
            !!outcome_var := case_when(
                .data[[outcome_var]] == "Y" ~ 1,
                .data[[outcome_var]] == "N" ~ 0,
                is.na(.data[[outcome_var]]) ~ 0,
                TRUE ~ 0
            )
        )

    # Calculate rates by treatment group
    sequela_rates <- data %>%
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
    tbl_summary_obj <- data %>%
        select(treatment_group, all_of(outcome_var)) %>%
        tbl_summary(
            by = treatment_group,
            missing = "no",
            label = get_variable_labels(),
            statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
            )
        ) %>%
        add_overall() %>%
        add_p(test = list(all_continuous() ~ "wilcox.test")) %>%
        bold_labels() %>%
        modify_header(
            label = "**Characteristic**",
            stat_0 = "**Overall**\nN = {N}",
            stat_1 = "**Plaque**\nN = {n}",
            stat_2 = "**GKSRS**\nN = {n}",
            p.value = "**p-value**"
        ) %>%
        modify_caption(paste("Rates of", tools::toTitleCase(sequela_type), "by Treatment Group"))

    # Convert to gt table and save
    other_caption <- ""
    if ("Other" %in% levels(data[[outcome_var]]) && !is.null(other_map[[outcome_var]]) && length(other_map[[outcome_var]]) > 0) {
        other_caption <- sprintf("\n\n'Other' includes: %s", paste(other_map[[outcome_var]], collapse = ", "))
    }
    tbl <- tbl_summary_obj %>%
        as_gt() %>%
        tab_source_note(
            source_note = md(paste0("Summary table generated automatically.", other_caption))
        )

    # Add caption for 'Other' if present
    if ("Other" %in% levels(data[[outcome_var]]) && !is.null(other_map[[outcome_var]]) && length(other_map[[outcome_var]]) > 0) {
        tbl <- tbl %>%
            tab_footnote(
                footnote = md(sprintf("'Other' includes: %s", paste(other_map[[outcome_var]], collapse = ", "))),
                locations = cells_title(groups = "title")
            )
    }

    # Save summary table
    save_gt_html(
        tbl,
        filename = file.path(output_dir, paste0(prefix, sequela_type, "_summary_table.html"))
    )

    # Fit logistic regression if there are enough events and confounders
    model_result <- NULL
    if (sum(data[[outcome_var]] == 1, na.rm = TRUE) >= 10) { # Require at least 10 events

        # Use the unified table generation system for logistic regression
        # Use standardized confounders from centralized configuration
        srd_confounders <- confounders

        regression_result <- generate_regression_table(
            data = data,
            outcome_var = outcome_var,
            predictor_vars = "treatment_group",
            confounders = srd_confounders,
            model_type = "logistic",
            effect_measure = "OR",
            analysis_name = paste0(sequela_type, "_logistic"),
            dataset_name = dataset_name,
            output_dir = output_dir,
            prefix = prefix,
            # handle_rare = FALSE, # REMOVED
            other_map = other_map
        )

        # Extract the model and table from the result
        model_result <- regression_result$model
        safety_diagnostics <- regression_result$diagnostics
        regression_table <- regression_result$table # Get the regression table
    } else {
        log_enhanced(sprintf("Insufficient events for regression modeling (%d events)", sum(data[[outcome_var]] == "Y", na.rm = TRUE)))
    }

    # Note: Diagnostics are now handled by the unified table generation system

    return(list(
        rates = sequela_rates,
        table = if (exists("regression_table")) regression_table else tbl, # Return regression table if available, otherwise summary table
        model = model_result,
        diagnostics = if (exists("safety_diagnostics")) safety_diagnostics else NULL # Add diagnostics for consolidation
    ))
}
