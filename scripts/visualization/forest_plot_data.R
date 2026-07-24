# Forest Plot Data Preparation

#' Create formatted data for single cohort forest plot using forestploter format
#'
#' @param subgroup_results List of subgroup analysis results
#' @param variable_order Character vector of variables to include (enforced for consistency)
#' @param treatment_labels Character vector of treatment labels
#' @param effect_measure Character string for effect measure
#' @return List with formatted data for forestploter
create_forest_plot_data <- function(subgroup_results, variable_order, treatment_labels, effect_measure) {
    treatment_labels <- as.character(treatment_labels)
    treatment_count_columns <- c(PBT = "PBT_n", GKSRS = "GKSRS_n")
    if (
        length(treatment_labels) != length(treatment_count_columns) ||
            anyDuplicated(treatment_labels) ||
            !setequal(treatment_labels, names(treatment_count_columns))
    ) {
        stop("treatment_labels must contain exactly PBT and GKSRS, once each")
    }

    # Initialize data collection
    all_rows <- list()
    est_values <- c()
    lower_values <- c()
    upper_values <- c()
    is_summary <- c()
    font_face <- c()
    text_size <- c()
    missing_interaction_vars <- character(0) # Track variables where interaction p could not be estimated
    diagnostics_rows <- list()
    show_event_counts <- toupper(effect_measure) %in% c("HR", "OR", "RR")
    arm_count_header_label <- "n/N"
    arm_count_headers <- stats::setNames(
        sprintf("%s %s", treatment_labels, arm_count_header_label),
        treatment_labels
    )
    effect_header <- sprintf("%s (95%% CI)", effect_measure)

    # Handle empty variable_order case
    if (length(variable_order) == 0) {
        # Return empty data structure
        empty_data_frame <- data.frame(
            Subgroup = character(),
            PBT_n = character(),
            GKSRS_n = character(),
            ` ` = character(),
            `HR (95% CI)` = character(),
            `p-value` = character(),
            `Int p` = character(),
            stringsAsFactors = FALSE
        )
        colnames(empty_data_frame) <- c(
            "Subgroup",
            unname(arm_count_headers),
            " ",
            effect_header,
            "p-value",
            "Int p"
        )

        return(list(
            data_frame = empty_data_frame,
            est_values = numeric(),
            lower_values = numeric(),
            upper_values = numeric(),
            is_summary = logical(),
            font_face = character(),
            text_size = numeric(),
            missing_interaction_vars = character(),
            diagnostics = data.frame()
        ))
    }

    # DO NOT create header row as data - forestploter creates headers from column names automatically

    # Process each variable in order
    for (var_name in variable_order) {
        # Check if variable exists in results at all
        if (!(var_name %in% names(subgroup_results))) {
            # Variable missing from results - create a "no data" header
            no_data_row <- data.frame(
                Subgroup = format_variable_name(var_name),
                PBT_n = "",
                GKSRS_n = "",
                stringsAsFactors = FALSE
            )

            # Add blank column for CI, subgroup p-value, and interaction p-value
            no_data_row$` ` <- paste(rep(" ", 20), collapse = " ")
            no_data_row$`HR (95% CI)` <- ""
            no_data_row$`p-value` <- ""
            no_data_row$`Interaction p` <- ""

            all_rows[[length(all_rows) + 1]] <- no_data_row
            est_values <- c(est_values, NaN)
            lower_values <- c(lower_values, NaN)
            upper_values <- c(upper_values, NaN)
            is_summary <- c(is_summary, TRUE)
            font_face <- c(font_face, "italic")
            text_size <- c(text_size, 0.8)
            next
        }

        # Variable header row
        var_header <- data.frame(
            Subgroup = format_variable_name(var_name),
            PBT_n = "",
            GKSRS_n = "",
            stringsAsFactors = FALSE
        )

        # Add blank column for CI, subgroup p-value, and interaction p-value columns
        var_header$` ` <- paste(rep(" ", 20), collapse = " ")
        var_header$`HR (95% CI)` <- ""
        var_header$`p-value` <- ""
        # Check for interaction p-value and capture failure reason
        if (!is.null(subgroup_results[[var_name]]$interaction_p) && !is.na(subgroup_results[[var_name]]$interaction_p)) {
            var_header$`Interaction p` <- forest_format_p_value(subgroup_results[[var_name]]$interaction_p)
            interaction_failure_reason <- "" # No reason needed when successful
        } else {
            var_header$`Interaction p` <- ""
            missing_interaction_vars <- c(missing_interaction_vars, var_name)

            # Get failure reason from interaction diagnostics
            if (!is.null(subgroup_results[[var_name]]$interaction_diagnostics) &&
                !is.null(subgroup_results[[var_name]]$interaction_diagnostics$failure_reason)) {
                interaction_failure_reason <- subgroup_results[[var_name]]$interaction_diagnostics$failure_reason
            } else if (!is.null(subgroup_results[[var_name]]$error)) {
                interaction_failure_reason <- subgroup_results[[var_name]]$error
            } else {
                interaction_failure_reason <- "Unknown - no diagnostics available"
            }
        }

        # diagnostics for header
        diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
            variable = var_name,
            level = "__HEADER__",
            n_total = NA,
            n_plaque = NA,
            n_gksrs = NA,
            events_plaque = NA,
            events_gksrs = NA,
            treatment_effect = NA,
            ci_lower = NA,
            ci_upper = NA,
            p_value = subgroup_results[[var_name]]$interaction_p,
            status = "header",
            reason = if (interaction_failure_reason == "") "" else paste("Missing interaction p-value:", interaction_failure_reason),
            stringsAsFactors = FALSE
        )

        all_rows[[length(all_rows) + 1]] <- var_header
        est_values <- c(est_values, NaN)
        lower_values <- c(lower_values, NaN)
        upper_values <- c(upper_values, NaN)
        is_summary <- c(is_summary, TRUE)
        font_face <- c(font_face, "bold")
        text_size <- c(text_size, 1.0)

        # Check if data exists for this variable
        if (var_name %in% names(subgroup_results)) {
            var_data <- subgroup_results[[var_name]]
            rendered_level_keys <- character(0)

            effects_data <- if (!is.null(var_data$subgroup_effects) && nrow(var_data$subgroup_effects) > 0) {
                var_data$subgroup_effects
            } else {
                data.frame()
            }

            sparse_rows <- data.frame()
            if (!is.null(var_data$sparse_level_diagnostics) &&
                is.data.frame(var_data$sparse_level_diagnostics) &&
                nrow(var_data$sparse_level_diagnostics) > 0) {
                sparse_rows <- var_data$sparse_level_diagnostics
                if ("variable" %in% names(sparse_rows)) {
                    sparse_rows <- sparse_rows[sparse_rows$variable == var_name, , drop = FALSE]
                }
            }

            interaction_excluded_levels <- get_interaction_excluded_levels(var_data)
            required_levels <- get_required_forest_levels(var_name)
            ordered_levels <- get_forest_level_order(
                var_name = var_name,
                var_data = var_data,
                effects_data = effects_data,
                sparse_rows = sparse_rows,
                interaction_excluded_levels = interaction_excluded_levels,
                required_levels = required_levels
            )

            if (isTRUE(var_data$modeled_continuously)) {
                next
            } else if (length(ordered_levels) == 0) {
                no_data_row <- data.frame(
                    Subgroup = "  No data available",
                    PBT_n = "",
                    GKSRS_n = "",
                    stringsAsFactors = FALSE
                )
                no_data_row$` ` <- paste(rep(" ", 20), collapse = " ")
                no_data_row$`HR (95% CI)` <- ""
                no_data_row$`p-value` <- ""
                no_data_row$`Interaction p` <- ""

                all_rows[[length(all_rows) + 1]] <- no_data_row
                est_values <- c(est_values, NaN)
                lower_values <- c(lower_values, NaN)
                upper_values <- c(upper_values, NaN)
                is_summary <- c(is_summary, TRUE)
                font_face <- c(font_face, "italic")
                text_size <- c(text_size, 0.8)
            } else {
                effect_levels <- if (nrow(effects_data) > 0) as.character(effects_data$subgroup_level) else character(0)

                for (raw_level in ordered_levels) {
                    raw_level <- as.character(raw_level)
                    display_level <- format_subgroup_level(var_name, raw_level)

                    effect_idx <- if (length(effect_levels) > 0) {
                        match(raw_level, effect_levels)
                    } else {
                        NA
                    }

                    if (!is.na(effect_idx)) {
                        row_data <- effects_data[effect_idx, ]
                        rendered_level_keys <- c(rendered_level_keys, raw_level)
                        events_plaque <- if ("events_plaque" %in% names(row_data)) row_data$events_plaque else NA
                        events_gksrs <- if ("events_gksrs" %in% names(row_data)) row_data$events_gksrs else NA

                        if (diagnostics_invalid_numeric(row_data$treatment_effect) ||
                            diagnostics_invalid_numeric(row_data$ci_lower) ||
                            diagnostics_invalid_numeric(row_data$ci_upper)) {
                            diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
                                variable = var_name,
                                level = raw_level,
                                n_total = row_data$n_total,
                                n_plaque = row_data$n_plaque,
                                n_gksrs = row_data$n_gksrs,
                                events_plaque = if ("events_plaque" %in% names(row_data)) row_data$events_plaque else NA,
                                events_gksrs = if ("events_gksrs" %in% names(row_data)) row_data$events_gksrs else NA,
                                treatment_effect = row_data$treatment_effect,
                                ci_lower = row_data$ci_lower,
                                ci_upper = row_data$ci_upper,
                                p_value = row_data$p_value,
                                status = "skipped_non_finite",
                                reason = "Treatment effect, CI bounds, or both are NA/non-finite",
                                stringsAsFactors = FALSE
                            )

                            non_estimable_row <- data.frame(
                                Subgroup = sprintf("  %s", display_level),
                                PBT_n = format_forest_treatment_count(events_plaque, row_data$n_plaque, row_data$n_total, show_event_counts),
                                GKSRS_n = format_forest_treatment_count(events_gksrs, row_data$n_gksrs, row_data$n_total, show_event_counts),
                                stringsAsFactors = FALSE
                            )
                            non_estimable_row$` ` <- paste(rep(" ", 20), collapse = " ")
                            non_estimable_row$`HR (95% CI)` <- "Not estimable"
                            non_estimable_row$`p-value` <- ""
                            non_estimable_row$`Interaction p` <- ""

                            all_rows[[length(all_rows) + 1]] <- non_estimable_row
                            est_values <- c(est_values, NaN)
                            lower_values <- c(lower_values, NaN)
                            upper_values <- c(upper_values, NaN)
                            is_summary <- c(is_summary, FALSE)
                            font_face <- c(font_face, "italic")
                            text_size <- c(text_size, 0.85)
                            next
                        }

                        if (toupper(effect_measure) %in% c("HR", "OR", "RR")) {
                            if (row_data$treatment_effect <= 0 || row_data$ci_lower <= 0) {
                                diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
                                    variable = var_name,
                                    level = raw_level,
                                    n_total = row_data$n_total,
                                    n_plaque = row_data$n_plaque,
                                    n_gksrs = row_data$n_gksrs,
                                    events_plaque = NA,
                                    events_gksrs = NA,
                                    treatment_effect = row_data$treatment_effect,
                                    ci_lower = row_data$ci_lower,
                                    ci_upper = row_data$ci_upper,
                                    p_value = row_data$p_value,
                                    status = "skipped_non_positive",
                                    reason = "Treatment effect or CI bounds <= 0 (invalid for ratio measures)",
                                    stringsAsFactors = FALSE
                                )

                                non_estimable_row <- data.frame(
                                    Subgroup = sprintf("  %s", display_level),
                                    PBT_n = format_forest_treatment_count(events_plaque, row_data$n_plaque, row_data$n_total, show_event_counts),
                                    GKSRS_n = format_forest_treatment_count(events_gksrs, row_data$n_gksrs, row_data$n_total, show_event_counts),
                                    stringsAsFactors = FALSE
                                )
                                non_estimable_row$` ` <- paste(rep(" ", 20), collapse = " ")
                                non_estimable_row$`HR (95% CI)` <- "Not estimable"
                                non_estimable_row$`p-value` <- ""
                                non_estimable_row$`Interaction p` <- ""

                                all_rows[[length(all_rows) + 1]] <- non_estimable_row
                                est_values <- c(est_values, NaN)
                                lower_values <- c(lower_values, NaN)
                                upper_values <- c(upper_values, NaN)
                                is_summary <- c(is_summary, FALSE)
                                font_face <- c(font_face, "italic")
                                text_size <- c(text_size, 0.85)
                                next
                            }
                        }

                        if (abs(row_data$treatment_effect) > EXTREME_ESTIMATE_THRESHOLD) {
                            diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
                                variable = var_name,
                                level = raw_level,
                                n_total = row_data$n_total,
                                n_plaque = row_data$n_plaque,
                                n_gksrs = row_data$n_gksrs,
                                events_plaque = if ("events_plaque" %in% names(row_data)) row_data$events_plaque else NA,
                                events_gksrs = if ("events_gksrs" %in% names(row_data)) row_data$events_gksrs else NA,
                                treatment_effect = row_data$treatment_effect,
                                ci_lower = row_data$ci_lower,
                                ci_upper = row_data$ci_upper,
                                p_value = row_data$p_value,
                                status = "skipped_extreme",
                                reason = sprintf("Estimate (%.2f) exceeds threshold of %.0f", row_data$treatment_effect, EXTREME_ESTIMATE_THRESHOLD),
                                stringsAsFactors = FALSE
                            )

                            non_estimable_row <- data.frame(
                                Subgroup = sprintf("  %s", display_level),
                                PBT_n = format_forest_treatment_count(events_plaque, row_data$n_plaque, row_data$n_total, show_event_counts),
                                GKSRS_n = format_forest_treatment_count(events_gksrs, row_data$n_gksrs, row_data$n_total, show_event_counts),
                                stringsAsFactors = FALSE
                            )
                            non_estimable_row$` ` <- paste(rep(" ", 20), collapse = " ")
                            non_estimable_row$`HR (95% CI)` <- "Not estimable"
                            non_estimable_row$`p-value` <- ""
                            non_estimable_row$`Interaction p` <- ""

                            all_rows[[length(all_rows) + 1]] <- non_estimable_row
                            est_values <- c(est_values, NaN)
                            lower_values <- c(lower_values, NaN)
                            upper_values <- c(upper_values, NaN)
                            is_summary <- c(is_summary, FALSE)
                            font_face <- c(font_face, "italic")
                            text_size <- c(text_size, 0.85)
                            next
                        }

                        diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
                            variable = var_name,
                            level = raw_level,
                            n_total = row_data$n_total,
                            n_plaque = row_data$n_plaque,
                            n_gksrs = row_data$n_gksrs,
                            events_plaque = events_plaque,
                            events_gksrs = events_gksrs,
                            treatment_effect = row_data$treatment_effect,
                            ci_lower = row_data$ci_lower,
                            ci_upper = row_data$ci_upper,
                            p_value = row_data$p_value,
                            status = "plotted",
                            reason = "",
                            stringsAsFactors = FALSE
                        )

                        subgroup_row <- data.frame(
                            Subgroup = sprintf("  %s", display_level),
                            PBT_n = format_forest_treatment_count(events_plaque, row_data$n_plaque, row_data$n_total, show_event_counts),
                            GKSRS_n = format_forest_treatment_count(events_gksrs, row_data$n_gksrs, row_data$n_total, show_event_counts),
                            stringsAsFactors = FALSE
                        )
                        subgroup_row$` ` <- paste(rep(" ", 20), collapse = " ")
                        subgroup_row$`HR (95% CI)` <- sprintf(
                            "%.2f (%.2f, %.2f)",
                            row_data$treatment_effect,
                            row_data$ci_lower,
                            row_data$ci_upper
                        )
                        subgroup_row$`p-value` <- forest_format_p_value(row_data$p_value)
                        subgroup_row$`Interaction p` <- ""

                        all_rows[[length(all_rows) + 1]] <- subgroup_row
                        est_values <- c(est_values, row_data$treatment_effect)
                        lower_values <- c(lower_values, row_data$ci_lower)
                        upper_values <- c(upper_values, row_data$ci_upper)
                        is_summary <- c(is_summary, FALSE)
                        font_face <- c(font_face, "plain")
                        text_size <- c(text_size, 0.9)
                        next
                    }

                    non_estimable_meta <- get_non_estimable_level_metadata(
                        var_data = var_data,
                        level_name = raw_level,
                        sparse_rows = sparse_rows,
                        interaction_excluded_levels = interaction_excluded_levels,
                        required_levels = required_levels
                    )
                    if (is.null(non_estimable_meta)) {
                        next
                    }

                    level_label <- if (non_estimable_meta$has_arm_counts) {
                        display_level
                    } else if (!is.na(non_estimable_meta$n_total)) {
                        sprintf("%s (n=%d)", display_level, non_estimable_meta$n_total)
                    } else {
                        display_level
                    }

                    non_estimable_row <- data.frame(
                        Subgroup = sprintf("  %s", level_label),
                        PBT_n = if (non_estimable_meta$has_arm_counts) {
                            format_forest_treatment_count(
                                non_estimable_meta$events_plaque,
                                non_estimable_meta$n_plaque,
                                non_estimable_meta$n_total,
                                show_event_counts
                            )
                        } else "",
                        GKSRS_n = if (non_estimable_meta$has_arm_counts) {
                            format_forest_treatment_count(
                                non_estimable_meta$events_gksrs,
                                non_estimable_meta$n_gksrs,
                                non_estimable_meta$n_total,
                                show_event_counts
                            )
                        } else "",
                        stringsAsFactors = FALSE
                    )
                    non_estimable_row$` ` <- paste(rep(" ", 20), collapse = " ")
                    non_estimable_row$`HR (95% CI)` <- "Not estimable"
                    non_estimable_row$`p-value` <- ""
                    non_estimable_row$`Interaction p` <- ""

                    all_rows[[length(all_rows) + 1]] <- non_estimable_row
                    est_values <- c(est_values, NaN)
                    lower_values <- c(lower_values, NaN)
                    upper_values <- c(upper_values, NaN)
                    is_summary <- c(is_summary, FALSE)
                    font_face <- c(font_face, "italic")
                    text_size <- c(text_size, 0.85)
                    rendered_level_keys <- c(rendered_level_keys, raw_level)

                    diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
                        variable = var_name,
                        level = raw_level,
                        n_total = if (!is.na(non_estimable_meta$n_total)) non_estimable_meta$n_total else NA,
                        n_plaque = if (!is.na(non_estimable_meta$n_plaque)) non_estimable_meta$n_plaque else NA,
                        n_gksrs = if (!is.na(non_estimable_meta$n_gksrs)) non_estimable_meta$n_gksrs else NA,
                        events_plaque = if (!is.na(non_estimable_meta$events_plaque)) non_estimable_meta$events_plaque else NA,
                        events_gksrs = if (!is.na(non_estimable_meta$events_gksrs)) non_estimable_meta$events_gksrs else NA,
                        treatment_effect = NA,
                        ci_lower = NA,
                        ci_upper = NA,
                        p_value = NA,
                        status = non_estimable_meta$status,
                        reason = non_estimable_meta$reason,
                        stringsAsFactors = FALSE
                    )
                }
            }
        } else {
            # Variable missing from results
            no_data_row <- data.frame(
                Subgroup = "  No data available",
                PBT_n = "",
                GKSRS_n = "",
                stringsAsFactors = FALSE
            )

            # Add blank column for CI, subgroup p-value, and interaction p-value
            no_data_row$` ` <- paste(rep(" ", 20), collapse = " ")
            no_data_row$`HR (95% CI)` <- ""
            no_data_row$`p-value` <- ""
            no_data_row$`Interaction p` <- ""

            all_rows[[length(all_rows) + 1]] <- no_data_row
            est_values <- c(est_values, NaN)
            lower_values <- c(lower_values, NaN)
            upper_values <- c(upper_values, NaN)
            is_summary <- c(is_summary, TRUE)
            font_face <- c(font_face, "italic")
            text_size <- c(text_size, 0.8)
        }
    }

    # Combine all rows into a data frame
    # Filter out NULL or invalid elements
    valid_indices <- sapply(all_rows, function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0)
    valid_rows <- all_rows[valid_indices]

    # Filter the corresponding vectors to maintain alignment
    if (length(valid_rows) > 0) {
        final_df <- do.call(rbind, valid_rows)
        # Filter vectors to match the valid rows
        est_values <- est_values[valid_indices]
        lower_values <- lower_values[valid_indices]
        upper_values <- upper_values[valid_indices]
        is_summary <- is_summary[valid_indices]
        font_face <- font_face[valid_indices]
        text_size <- text_size[valid_indices]
    } else {
        # Create empty data frame with proper structure
        final_df <- data.frame(
            Subgroup = character(),
            PBT_n = character(),
            GKSRS_n = character(),
            ` ` = character(),
            `HR (95% CI)` = character(),
            `p-value` = character(),
            `Int p` = character(),
            stringsAsFactors = FALSE
        )
        colnames(final_df) <- c(
            "Subgroup",
            unname(arm_count_headers),
            " ",
            effect_header,
            "p-value",
            "Int p"
        )
        # Reset vectors to empty
        est_values <- numeric()
        lower_values <- numeric()
        upper_values <- numeric()
        is_summary <- logical()
        font_face <- character()
        text_size <- numeric()
    }

    source_count_values <- final_df[, unname(treatment_count_columns), drop = FALSE]
    displayed_count_columns <- unname(treatment_count_columns[treatment_labels])
    final_df <- final_df[
        ,
        c(
            "Subgroup",
            displayed_count_columns,
            " ",
            "HR (95% CI)",
            "p-value",
            "Interaction p"
        ),
        drop = FALSE
    ]

    # Set proper column names that will become the forestploter headers
    colnames(final_df) <- c(
        "Subgroup",
        unname(arm_count_headers),
        " ", # Blank column for CI
        effect_header,
        "p-value",
        "Int p"
    )
    for (treatment_key in treatment_labels) {
        displayed_header <- unname(arm_count_headers[treatment_key])
        source_column <- unname(treatment_count_columns[treatment_key])
        if (!identical(final_df[[displayed_header]], source_count_values[[source_column]])) {
            stop(sprintf(
                "Forest plot arm-count contract violated: %s values do not match the %s header",
                treatment_key,
                displayed_header
            ))
        }
    }

    # If using a ratio measure (HR, OR, RR), ensure positive values; otherwise keep as is.
    ratio_measures <- c("HR", "OR", "RR")
    if (toupper(effect_measure) %in% ratio_measures) {
        for (i in seq_along(est_values)) {
            if (!is.na(est_values[i]) && est_values[i] <= 0) {
                est_values[i] <- NaN
                lower_values[i] <- NaN
                upper_values[i] <- NaN
                is_summary[i] <- TRUE
            }
            if (!is.na(lower_values[i]) && lower_values[i] <= 0) {
                est_values[i] <- NaN
                lower_values[i] <- NaN
                upper_values[i] <- NaN
                is_summary[i] <- TRUE
            }
        }
    }

    # USE THE EXISTING SUBGROUP ANALYSIS DIAGNOSTICS INSTEAD OF CREATING OUR OWN
    # Extract diagnostics from subgroup_results instead of re-calculating everything
    combined_diagnostics <- list()
    for (var_name in variable_order) {
        # Determine interaction p-value and reason
        interaction_p <- if (!is.null(subgroup_results[[var_name]]$interaction_p)) subgroup_results[[var_name]]$interaction_p else NA
        interaction_diag <- subgroup_results[[var_name]]$interaction_diagnostics
        interaction_failure_reason <- ""
        if (is.null(interaction_p) || is.na(interaction_p)) {
            if (!is.null(interaction_diag) && !is.null(interaction_diag$failure_reason)) {
                interaction_failure_reason <- paste("Missing interaction p-value:", interaction_diag$failure_reason)
            } else {
                interaction_failure_reason <- "Missing interaction p-value: No valid test could be performed (insufficient data or model failure)"
            }
        }
        # Always add header row to combined_diagnostics, using the same column names as the factor level rows
        header_row <- data.frame(
            variable = var_name,
            subgroup_level = format_variable_name(var_name),
            n_total = NA,
            n_plaque = NA,
            n_gksrs = NA,
            events_plaque = NA,
            events_gksrs = NA,
            treatment_effect = NA,
            ci_lower = NA,
            ci_upper = NA,
            p_value = interaction_p,
            status = "header",
            reason = interaction_failure_reason,
            stringsAsFactors = FALSE
        )
        combined_diagnostics[[length(combined_diagnostics) + 1]] <- header_row
        if (var_name %in% names(subgroup_results)) {
            var_result <- subgroup_results[[var_name]]

            # Add the actual subgroup effects as diagnostics
            if (!is.null(var_result$subgroup_effects) && nrow(var_result$subgroup_effects) > 0) {
                effects_df <- var_result$subgroup_effects

                # Rename subgroup_variable to variable for consistency
                if ("subgroup_variable" %in% names(effects_df)) {
                    effects_df$variable <- effects_df$subgroup_variable
                    effects_df$subgroup_variable <- NULL
                } else {
                    effects_df$variable <- var_name
                }

                effects_df$status <- "plotted"
                effects_df$reason <- ""
                combined_diagnostics[[length(combined_diagnostics) + 1]] <- effects_df
            }

            # Add interaction diagnostics information
            if (!is.null(var_result$interaction_diagnostics)) {
                diag <- var_result$interaction_diagnostics

                # Add excluded levels information
                for (key in names(diag)) {
                    if (grepl("^excluded_", key)) {
                        level_name <- gsub("^excluded_", "", key)
                        reason <- if (is.list(diag[[key]])) diag[[key]]$reason else diag[[key]]

                        excluded_row <- data.frame(
                            variable = var_name,
                            subgroup_level = level_name,
                            n_total = NA,
                            n_plaque = NA,
                            n_gksrs = NA,
                            events_plaque = NA,
                            events_gksrs = NA,
                            treatment_effect = NA,
                            ci_lower = NA,
                            ci_upper = NA,
                            p_value = NA,
                            status = "EXCLUDED",
                            reason = reason,
                            stringsAsFactors = FALSE
                        )
                        combined_diagnostics[[length(combined_diagnostics) + 1]] <- excluded_row
                    }
                }

                # Add interaction p-value failure information
                if (!is.null(diag$failure_reason) && diag$failure_reason != "None") {
                    header_row <- data.frame(
                        variable = var_name,
                        subgroup_level = "__HEADER__",
                        n_total = NA,
                        n_plaque = NA,
                        n_gksrs = NA,
                        events_plaque = NA,
                        events_gksrs = NA,
                        treatment_effect = NA,
                        ci_lower = NA,
                        ci_upper = NA,
                        p_value = var_result$interaction_p,
                        status = "header",
                        reason = paste("Missing interaction p-value:", diag$failure_reason),
                        stringsAsFactors = FALSE
                    )
                    combined_diagnostics[[length(combined_diagnostics) + 1]] <- header_row
                }
            }
        }
    }

    if (length(diagnostics_rows) > 0) {
        plotting_diagnostics <- do.call(rbind, diagnostics_rows)
        if ("level" %in% names(plotting_diagnostics)) {
            plotting_diagnostics$subgroup_level <- plotting_diagnostics$level
            plotting_diagnostics$level <- NULL
        }
        combined_diagnostics[[length(combined_diagnostics) + 1]] <- plotting_diagnostics
    }

    # After collecting all rows, enforce column order and names for all rows
    if (length(combined_diagnostics) > 0) {
        col_order <- c("variable", "subgroup_level", "n_total", "n_plaque", "n_gksrs", "events_plaque", "events_gksrs", "treatment_effect", "ci_lower", "ci_upper", "p_value", "status", "reason")
        combined_diagnostics <- lapply(combined_diagnostics, function(df) {
            # Add any missing columns as NA
            for (col in setdiff(col_order, names(df))) df[[col]] <- NA
            # Reorder columns
            df <- df[, col_order, drop = FALSE]
            return(df)
        })
    }

    # Combine all diagnostics
    diagnostics_df <- if (length(combined_diagnostics) > 0) {
        # Filter out NULL or invalid elements
        valid_diagnostics <- combined_diagnostics[sapply(combined_diagnostics, function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0)]
        if (length(valid_diagnostics) > 0) {
            do.call(rbind, valid_diagnostics)
        } else {
            data.frame()
        }
    } else {
        data.frame()
    }

    return(list(
        data_frame = final_df,
        est_values = est_values,
        lower_values = lower_values,
        upper_values = upper_values,
        is_summary = is_summary,
        font_face = font_face,
        text_size = text_size,
        missing_interaction_vars = missing_interaction_vars,
        diagnostics = diagnostics_df
    ))
}

#' Format variable names for display
#'
#' @param var_name Character string of variable name
#' @return Character string of formatted variable name
format_variable_name <- function(var_name) {
    # Create a mapping of variable names to display names

    if (var_name %in% names(FORESTPLOT_NAME_MAPPING)) {
        return(FORESTPLOT_NAME_MAPPING[[var_name]])
    } else {
        return(tools::toTitleCase(gsub("_", " ", var_name)))
    }
}

#' Format subgroup level names for display
#'
#' @param var_name Character string of subgroup variable name
#' @param level_value Character string of subgroup level value
#' @return Character string of formatted subgroup level name
format_subgroup_level <- function(var_name, level_value) {
    level_label <- as.character(level_value)

    if (exists("STANDARD_LEVEL_LABELS", inherits = TRUE)) {
        label_maps <- get("STANDARD_LEVEL_LABELS", inherits = TRUE)
        if (var_name %in% names(label_maps) && level_label %in% names(label_maps[[var_name]])) {
            level_label <- label_maps[[var_name]][[level_label]]
        }
    }

    if (exists("AUTO_CLEAN_LEVELS", inherits = TRUE) && isTRUE(get("AUTO_CLEAN_LEVELS", inherits = TRUE))) {
        level_label <- gsub("_", " ", level_label)
    }

    level_label
}

#' Get required levels for forest-plot display
#'
#' @param var_name Character string of subgroup variable name
#' @return Character vector of required raw subgroup levels
get_required_forest_levels <- function(var_name) {
    if (exists("FOREST_PLOT_REQUIRED_LEVELS", inherits = TRUE)) {
        required_maps <- get("FOREST_PLOT_REQUIRED_LEVELS", inherits = TRUE)
        if (var_name %in% names(required_maps)) {
            return(as.character(required_maps[[var_name]]))
        }
    }
    character(0)
}

#' Get levels excluded from interaction modeling feasibility diagnostics
#'
#' @param var_data List for one subgroup variable result
#' @param rendered_level_keys Character vector of levels already rendered
#' @return Data frame with level, observed_n, reason
get_interaction_excluded_levels <- function(var_data, rendered_level_keys = character(0)) {
    if (is.null(var_data$interaction_diagnostics) ||
        is.null(var_data$interaction_diagnostics$level_statistics) ||
        !is.list(var_data$interaction_diagnostics$level_statistics)) {
        return(data.frame())
    }

    level_stats <- var_data$interaction_diagnostics$level_statistics
    level_names <- names(level_stats)
    if (is.null(level_names) || length(level_names) == 0) {
        return(data.frame())
    }

    out_rows <- list()
    for (level_name in level_names) {
        if (level_name %in% rendered_level_keys) {
            next
        }

        level_info <- level_stats[[level_name]]
        exclusion_reason <- if (!is.null(level_info$exclusion_reason)) as.character(level_info$exclusion_reason) else ""
        if (is.na(exclusion_reason) || exclusion_reason == "") {
            next
        }

        observed_n <- if (!is.null(level_info$n_total) && !is.na(level_info$n_total)) as.numeric(level_info$n_total) else NA
        n_plaque <- if (!is.null(level_info$n_plaque) && !is.na(level_info$n_plaque)) as.numeric(level_info$n_plaque) else NA
        n_gksrs <- if (!is.null(level_info$n_gksrs) && !is.na(level_info$n_gksrs)) as.numeric(level_info$n_gksrs) else NA
        events_plaque <- if (!is.null(level_info$events_plaque) && !is.na(level_info$events_plaque)) as.numeric(level_info$events_plaque) else NA
        events_gksrs <- if (!is.null(level_info$events_gksrs) && !is.na(level_info$events_gksrs)) as.numeric(level_info$events_gksrs) else NA
        out_rows[[length(out_rows) + 1]] <- data.frame(
            level = as.character(level_name),
            observed_n = observed_n,
            n_plaque = n_plaque,
            n_gksrs = n_gksrs,
            events_plaque = events_plaque,
            events_gksrs = events_gksrs,
            reason = exclusion_reason,
            stringsAsFactors = FALSE
        )
    }

    if (length(out_rows) == 0) {
        return(data.frame())
    }

    do.call(rbind, out_rows)
}

#' Get subgroup arm counts from interaction diagnostics for one level
#'
#' @param var_data List for one subgroup variable result
#' @param level_name Character subgroup level
#' @return List with n_total, n_plaque, n_gksrs, events_plaque, events_gksrs or NULL
get_level_counts_from_interaction_stats <- function(var_data, level_name) {
    if (is.null(var_data$interaction_diagnostics) ||
        is.null(var_data$interaction_diagnostics$level_statistics) ||
        !is.list(var_data$interaction_diagnostics$level_statistics)) {
        return(NULL)
    }

    level_stats <- var_data$interaction_diagnostics$level_statistics
    if (!(level_name %in% names(level_stats))) {
        return(NULL)
    }

    level_info <- level_stats[[level_name]]
    list(
        n_total = if (!is.null(level_info$n_total) && !is.na(level_info$n_total)) as.numeric(level_info$n_total) else NA,
        n_plaque = if (!is.null(level_info$n_plaque) && !is.na(level_info$n_plaque)) as.numeric(level_info$n_plaque) else NA,
        n_gksrs = if (!is.null(level_info$n_gksrs) && !is.na(level_info$n_gksrs)) as.numeric(level_info$n_gksrs) else NA,
        events_plaque = if (!is.null(level_info$events_plaque) && !is.na(level_info$events_plaque)) as.numeric(level_info$events_plaque) else NA,
        events_gksrs = if (!is.null(level_info$events_gksrs) && !is.na(level_info$events_gksrs)) as.numeric(level_info$events_gksrs) else NA
    )
}

#' Build canonical forest-plot level order for one subgroup variable
#'
#' @param var_name Character string of subgroup variable name
#' @param var_data List for one subgroup variable result
#' @param effects_data Data frame of estimable subgroup effects
#' @param sparse_rows Data frame of sparse excluded levels
#' @param interaction_excluded_levels Data frame of interaction-excluded levels
#' @param required_levels Character vector of required fallback levels
#' @return Character vector of ordered raw subgroup levels
get_forest_level_order <- function(var_name, var_data, effects_data, sparse_rows, interaction_excluded_levels, required_levels) {
    configured_levels <- get_configured_level_order(var_name)
    diagnostics_levels <- character(0)

    if (!is.null(var_data$interaction_diagnostics) &&
        !is.null(var_data$interaction_diagnostics$original_level_order)) {
        diagnostics_levels <- as.character(var_data$interaction_diagnostics$original_level_order)
    } else if (!is.null(var_data$interaction_diagnostics) &&
        !is.null(var_data$interaction_diagnostics$level_statistics) &&
        is.list(var_data$interaction_diagnostics$level_statistics)) {
        diagnostics_levels <- names(var_data$interaction_diagnostics$level_statistics)
    }

    diagnostics_levels <- diagnostics_levels[!is.na(diagnostics_levels) & diagnostics_levels != ""]

    ordered_levels <- c(configured_levels, diagnostics_levels)

    if (nrow(effects_data) > 0) {
        ordered_levels <- c(ordered_levels, as.character(effects_data$subgroup_level))
    }

    if (nrow(sparse_rows) > 0 && "level" %in% names(sparse_rows)) {
        ordered_levels <- c(ordered_levels, as.character(sparse_rows$level))
    }

    if (nrow(interaction_excluded_levels) > 0 && "level" %in% names(interaction_excluded_levels)) {
        ordered_levels <- c(ordered_levels, as.character(interaction_excluded_levels$level))
    }

    if (length(required_levels) > 0) {
        ordered_levels <- c(ordered_levels, as.character(required_levels))
    }

    ordered_levels <- ordered_levels[!is.na(ordered_levels) & ordered_levels != ""]
    unique(ordered_levels)
}

#' Get configured display order for subgroup levels
#'
#' @param var_name Character string of subgroup variable name
#' @return Character vector of configured raw level order
get_configured_level_order <- function(var_name) {
    if (!exists("STANDARD_LEVEL_LABELS", inherits = TRUE)) {
        return(character(0))
    }

    label_maps <- get("STANDARD_LEVEL_LABELS", inherits = TRUE)
    if (!(var_name %in% names(label_maps))) {
        return(character(0))
    }

    configured <- names(label_maps[[var_name]])
    configured <- configured[!is.na(configured) & configured != ""]
    as.character(configured)
}

#' Get metadata for rendering one non-estimable subgroup level
#'
#' @param var_data List for one subgroup variable result
#' @param level_name Character raw subgroup level
#' @param sparse_rows Data frame of sparse excluded levels
#' @param interaction_excluded_levels Data frame of interaction-excluded levels
#' @param required_levels Character vector of required fallback levels
#' @return List with rendering metadata or NULL if no non-estimable row should be added
get_non_estimable_level_metadata <- function(var_data, level_name, sparse_rows, interaction_excluded_levels, required_levels) {
    if (nrow(sparse_rows) > 0 && "level" %in% names(sparse_rows)) {
        sparse_match <- which(as.character(sparse_rows$level) == level_name)
        if (length(sparse_match) > 0) {
            i <- sparse_match[1]
            n_total <- if ("observed_n" %in% names(sparse_rows)) suppressWarnings(as.numeric(sparse_rows$observed_n[i])) else NA
            n_plaque <- if ("n_plaque" %in% names(sparse_rows)) suppressWarnings(as.numeric(sparse_rows$n_plaque[i])) else NA
            n_gksrs <- if ("n_gksrs" %in% names(sparse_rows)) suppressWarnings(as.numeric(sparse_rows$n_gksrs[i])) else NA
            events_plaque <- if ("events_plaque" %in% names(sparse_rows)) suppressWarnings(as.numeric(sparse_rows$events_plaque[i])) else NA
            events_gksrs <- if ("events_gksrs" %in% names(sparse_rows)) suppressWarnings(as.numeric(sparse_rows$events_gksrs[i])) else NA

            return(list(
                n_total = n_total,
                n_plaque = n_plaque,
                n_gksrs = n_gksrs,
                events_plaque = events_plaque,
                events_gksrs = events_gksrs,
                has_arm_counts = !is.na(n_total) && !is.na(n_plaque) && !is.na(n_gksrs),
                status = "not_estimable_sparse",
                reason = if ("reason" %in% names(sparse_rows)) as.character(sparse_rows$reason[i]) else "Sparse level excluded before modeling"
            ))
        }
    }

    if (nrow(interaction_excluded_levels) > 0 && "level" %in% names(interaction_excluded_levels)) {
        interaction_match <- which(as.character(interaction_excluded_levels$level) == level_name)
        if (length(interaction_match) > 0) {
            i <- interaction_match[1]
            n_total <- suppressWarnings(as.numeric(interaction_excluded_levels$observed_n[i]))
            n_plaque <- suppressWarnings(as.numeric(interaction_excluded_levels$n_plaque[i]))
            n_gksrs <- suppressWarnings(as.numeric(interaction_excluded_levels$n_gksrs[i]))
            events_plaque <- suppressWarnings(as.numeric(interaction_excluded_levels$events_plaque[i]))
            events_gksrs <- suppressWarnings(as.numeric(interaction_excluded_levels$events_gksrs[i]))
            reason <- as.character(interaction_excluded_levels$reason[i])
            return(list(
                n_total = n_total,
                n_plaque = n_plaque,
                n_gksrs = n_gksrs,
                events_plaque = events_plaque,
                events_gksrs = events_gksrs,
                has_arm_counts = !is.na(n_total) && !is.na(n_plaque) && !is.na(n_gksrs),
                status = "not_estimable_interaction_exclusion",
                reason = if (!is.na(reason) && reason != "") reason else "Subgroup level excluded during interaction feasibility checks"
            ))
        }
    }

    if (!(level_name %in% required_levels)) {
        return(NULL)
    }

    required_counts <- get_level_counts_from_interaction_stats(var_data, level_name)
    required_n_total <- if (!is.null(required_counts)) required_counts$n_total else NA
    required_n_plaque <- if (!is.null(required_counts)) required_counts$n_plaque else NA
    required_n_gksrs <- if (!is.null(required_counts)) required_counts$n_gksrs else NA
    required_events_plaque <- if (!is.null(required_counts)) required_counts$events_plaque else NA
    required_events_gksrs <- if (!is.null(required_counts)) required_counts$events_gksrs else NA

    list(
        n_total = required_n_total,
        n_plaque = required_n_plaque,
        n_gksrs = required_n_gksrs,
        events_plaque = required_events_plaque,
        events_gksrs = required_events_gksrs,
        has_arm_counts = !is.na(required_n_total) && !is.na(required_n_plaque) && !is.na(required_n_gksrs),
        status = "not_estimable_required_level",
        reason = "Required forest-plot level not estimable after subgroup modeling"
    )
}

#' Format treatment count for forest table
#'
#' @param event_count Numeric event count for treatment arm
#' @param n_group Numeric arm sample size
#' @param n_total Numeric subgroup total
#' @param show_events Logical; when TRUE prefer events/n_group if available
#' @return Character string count for forest table cell
format_forest_treatment_count <- function(event_count, n_group, n_total, show_events) {
    if (isTRUE(show_events) && !is.na(event_count) && !is.na(n_group)) {
        return(format_sample_size(event_count, n_group))
    }

    format_sample_size(n_group, n_total)
}

#' Format sample size for display
#'
#' @param n_group Numeric value for group size
#' @param n_total Numeric value for total size (optional)
#' @return Character string of formatted sample size
format_sample_size <- function(n_group, n_total = NULL) {
    if (is.na(n_group) || is.null(n_group)) {
        return("")
    }
    if (!is.null(n_total) && !is.na(n_total)) {
        return(sprintf("%d/%d", n_group, n_total))
    } else {
        return(as.character(n_group))
    }
}

#' Format p-values for display
#'
#' @param p_value Numeric p-value
#' @return Character string of formatted p-value
forest_format_p_value <- function(p_value) {
    if (is.na(p_value) || is.null(p_value)) {
        return("")
    }
    if (p_value < 0.001) {
        return("<0.001")
    } else if (p_value < 0.01) {
        return(sprintf("%.3f", p_value))
    } else {
        return(sprintf("%.2f", p_value))
    }
}
