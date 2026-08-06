#' Resolve the diagnostic status for a forest-plot interaction header
#'
#' The status is intentionally separate from the numeric interaction p-value:
#' an unavailable p-value is a diagnostic state, not a numeric result.
get_forest_interaction_header_status <- function(var_data) {
    interaction_p <- if (!is.null(var_data)) var_data$interaction_p else NA_real_
    if (length(interaction_p) == 1L && is.finite(interaction_p)) {
        return("header")
    }

    diagnostics <- if (!is.null(var_data) && is.list(var_data$interaction_diagnostics)) {
        var_data$interaction_diagnostics
    } else {
        list()
    }
    test_status <- diagnostics$interaction_test_status %||% ""
    model_status <- diagnostics$model_status %||% ""

    if (identical(test_status, "not_testable_single_supported_level")) {
        return("interaction_not_testable_single_level")
    }
    if (identical(model_status, "no_supported_levels")) {
        return("not_estimable_no_supported_levels")
    }
    if (identical(model_status, "model_failure") || identical(test_status, "model_failure")) {
        return("model_failure")
    }
    if (identical(test_status, "reduced_model_failure") || identical(test_status, "interaction_test_failure")) {
        return("interaction_test_failure")
    }
    "header"
}

#' Build an explicit diagnostic explanation for an unavailable interaction p-value
get_forest_interaction_failure_reason <- function(var_data) {
    interaction_p <- if (!is.null(var_data)) var_data$interaction_p else NA_real_
    if (length(interaction_p) == 1L && is.finite(interaction_p)) {
        return("")
    }

    diagnostics <- if (!is.null(var_data) && is.list(var_data$interaction_diagnostics)) {
        var_data$interaction_diagnostics
    } else {
        list()
    }
    status <- get_forest_interaction_header_status(var_data)
    detail <- diagnostics$failure_reason %||% "No valid interaction test was available"
    if (is.null(detail) || is.na(detail) || !nzchar(as.character(detail))) {
        detail <- "No valid interaction test was available"
    }

    if (identical(status, "interaction_not_testable_single_level")) {
        return(paste("Interaction testing not possible:", detail))
    }
    if (identical(status, "not_estimable_no_supported_levels")) {
        return(paste("Interaction not estimable:", detail))
    }
    if (identical(status, "model_failure")) {
        model_error <- diagnostics$model_error %||% ""
        detail_parts <- c(detail, if (nzchar(as.character(model_error))) paste("Model error:", model_error) else character(0))
        return(paste("Interaction model failure:", paste(unique(detail_parts), collapse = "; ")))
    }
    if (identical(status, "interaction_test_failure")) {
        return(paste("Interaction testing failure:", detail))
    }
    paste("Interaction p-value unavailable:", detail)
}

#' Create forest plot diagnostics from raw subgroup data
#'
#' @param subgroup_results List of subgroup analysis results
#' @param effect_measure Character string for effect measure ("HR", "OR", etc.)
#' @param variable_order Character vector of variables to include (optional)
#' @return Data frame with forest plot diagnostics
create_forest_plot_diagnostics <- function(subgroup_results, effect_measure = "HR", variable_order = NULL) {
    # Initialize diagnostics collection
    diagnostics_rows <- list()

    # Use default variable order if not provided
    if (is.null(variable_order)) {
        variable_order <- c(
            "age_at_diagnosis", "sex", "location", "initial_t_stage",
            "initial_tumor_height", "initial_tumor_diameter", "biopsy1_gep", "optic_nerve"
        )
    }

    # Process each variable in order
    for (var_index in seq_along(variable_order)) {
        var_name <- variable_order[var_index]
        # Check if variable exists in results
        if (!(var_name %in% names(subgroup_results))) {
            # Variable missing from results - create a "no data" header
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
                p_value = NA,
                status = "header",
                reason = "Variable missing from results",
                other_variable_contents = "",
                level_order = -1,
                stringsAsFactors = FALSE
            )
            next
        }

        # Variable header row with interaction p-value if available
        var_data <- subgroup_results[[var_name]]
        level_stats <- if (!is.null(var_data$interaction_diagnostics)) var_data$interaction_diagnostics$level_statistics else NULL
        level_names <- if (!is.null(level_stats)) names(level_stats) else character(0)
        header_interaction_p <- if (!is.null(var_data$interaction_p)) var_data$interaction_p else NA
        header_reason <- get_forest_interaction_failure_reason(var_data)
        sparse_removal_note <- ""
        if (!is.null(var_data$sparse_level_diagnostics) &&
            is.data.frame(var_data$sparse_level_diagnostics) &&
            nrow(var_data$sparse_level_diagnostics) > 0) {
            detail_df <- var_data$sparse_level_diagnostics[
                var_data$sparse_level_diagnostics$variable == var_name,
                ,
                drop = FALSE
            ]

            if (nrow(detail_df) > 0) {
                detail_lines <- vapply(seq_len(nrow(detail_df)), function(i) {
                    detail_row <- detail_df[i, , drop = FALSE]
                    sprintf(
                        "%s level '%s' (observed n=%s, rows removed=%s): %s",
                        tools::toTitleCase(gsub("_", " ", as.character(detail_row$action %||% "excluded"))),
                        as.character(detail_row$level %||% NA_character_),
                        detail_row$observed_n %||% NA,
                        detail_row$rows_removed %||% 0L,
                        detail_row$reason %||% "Sparse level excluded"
                    )
                }, character(1))
                sparse_removal_note <- paste(unique(detail_lines), collapse = "; ")
            }
        }

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
            p_value = header_interaction_p,
            status = get_forest_interaction_header_status(var_data),
            reason = header_reason,
            other_variable_contents = sparse_removal_note,
            variable_order = var_index,
            level_order = -1,
            stringsAsFactors = FALSE
        )

        # Check if data exists for this variable
        if (var_name %in% names(subgroup_results)) {
            var_data <- subgroup_results[[var_name]]

            if (!is.null(var_data$subgroup_effects) && nrow(var_data$subgroup_effects) > 0) {
                # Add subgroup rows
                effects_data <- var_data$subgroup_effects
                for (i in seq_len(nrow(effects_data))) {
                    row_data <- effects_data[i, ]

                    # Check for infinite CIs specifically
                    has_infinite_ci <- (is.character(row_data$ci_upper) && row_data$ci_upper == "Inf") ||
                        (is.character(row_data$ci_lower) && row_data$ci_lower == "Inf")

                    # Skip rows with NA, non-finite, or infinite values
                    if (diagnostics_invalid_numeric(row_data$treatment_effect) ||
                        diagnostics_invalid_numeric(row_data$ci_lower) ||
                        diagnostics_invalid_numeric(row_data$ci_upper) ||
                        has_infinite_ci) {
                        # Record diagnostics for skipped rows
                        level_idx <- if (length(level_names) > 0) match(as.character(row_data$subgroup_level), level_names) else NA
                        if (is.na(level_idx)) level_idx <- length(level_names) + 1
                        diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
                            variable = var_name,
                            level = as.character(row_data$subgroup_level),
                            n_total = row_data$n_total,
                            n_plaque = row_data$n_plaque,
                            n_gksrs = row_data$n_gksrs,
                            events_plaque = if ("events_plaque" %in% names(row_data)) row_data$events_plaque else NA,
                            events_gksrs = if ("events_gksrs" %in% names(row_data)) row_data$events_gksrs else NA,
                            treatment_effect = row_data$treatment_effect,
                            ci_lower = row_data$ci_lower,
                            ci_upper = row_data$ci_upper,
                            p_value = row_data$p_value,
                            status = if (has_infinite_ci) "skipped_infinite_ci" else "skipped_non_finite",
                            reason = if (has_infinite_ci) "Infinite confidence interval bounds" else "Treatment effect, CI bounds, or both are NA/non-finite",
                            other_variable_contents = "",
                            variable_order = var_index,
                            level_order = level_idx,
                            stringsAsFactors = FALSE
                        )
                        next # skip this subgroup level completely
                    }

                    # Additional check for ratio measures (must be > 0)
                    if (toupper(effect_measure) %in% c("HR", "OR", "RR")) {
                        if (row_data$treatment_effect <= 0 || row_data$ci_lower <= 0) {
                            # Record diagnostics for skipped rows
                            level_idx <- if (length(level_names) > 0) match(as.character(row_data$subgroup_level), level_names) else NA
                            if (is.na(level_idx)) level_idx <- length(level_names) + 1
                            diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
                                variable = var_name,
                                level = as.character(row_data$subgroup_level),
                                n_total = row_data$n_total,
                                n_plaque = row_data$n_plaque,
                                n_gksrs = row_data$n_gksrs,
                                events_plaque = NA, # Don't calculate events for invalid rows
                                events_gksrs = NA,
                                treatment_effect = row_data$treatment_effect,
                                ci_lower = row_data$ci_lower,
                                ci_upper = row_data$ci_upper,
                                p_value = row_data$p_value,
                                status = "skipped_non_positive",
                                reason = "Treatment effect or CI bounds <= 0 (invalid for ratio measures)",
                                other_variable_contents = "",
                                variable_order = var_index,
                                level_order = level_idx,
                                stringsAsFactors = FALSE
                            )
                            next
                        }
                    }

                    # Check for extreme estimates (above threshold)
                    if (abs(row_data$treatment_effect) > EXTREME_ESTIMATE_THRESHOLD) { # 
                        # Record diagnostics for skipped rows
                        level_idx <- if (length(level_names) > 0) match(as.character(row_data$subgroup_level), level_names) else NA
                        if (is.na(level_idx)) level_idx <- length(level_names) + 1
                        diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
                            variable = var_name,
                            level = as.character(row_data$subgroup_level),
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
                            reason = sprintf("Estimate (%.2f) exceeds threshold of %.2f", row_data$treatment_effect, EXTREME_ESTIMATE_THRESHOLD),
                            other_variable_contents = "",
                            variable_order = var_index,
                            level_order = level_idx,
                            stringsAsFactors = FALSE
                        )
                        next
                    }

                    # This row will be plotted - get events from subgroup effects data
                    events_plaque <- if ("events_plaque" %in% names(row_data)) row_data$events_plaque else NA
                    events_gksrs <- if ("events_gksrs" %in% names(row_data)) row_data$events_gksrs else NA

                    # Record valid subgroup level
                    level_idx <- if (length(level_names) > 0) match(as.character(row_data$subgroup_level), level_names) else NA
                    if (is.na(level_idx)) level_idx <- length(level_names) + 1
                diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
                    variable = var_name,
                    level = as.character(row_data$subgroup_level),
                        n_total = row_data$n_total,
                        n_plaque = row_data$n_plaque,
                        n_gksrs = row_data$n_gksrs,
                        events_plaque = events_plaque,
                        events_gksrs = events_gksrs,
                        treatment_effect = row_data$treatment_effect,
                        ci_lower = row_data$ci_lower,
                        ci_upper = row_data$ci_upper,
                        p_value = row_data$p_value,
                        status = if (isTRUE(var_data$modeled_continuously)) "continuous_interaction" else "plotted",
                        reason = if (isTRUE(var_data$modeled_continuously)) {
                            "Age entered as a numeric linear term; treatment effect evaluated at the cohort median."
                        } else {
                            ""
                        },
                        other_variable_contents = "",
                        variable_order = var_index,
                        level_order = level_idx,
                        stringsAsFactors = FALSE
                    )
                }
            }

            # Process excluded levels from interaction_diagnostics
            if (!is.null(var_data$interaction_diagnostics)) {
                diag <- var_data$interaction_diagnostics
                level_stats <- diag$level_statistics
                if (!is.null(level_stats)) {
                    level_names <- names(level_stats)
                }
                if (!is.null(diag$excluded_level_names) && diag$excluded_level_names != "") {
                    # Split the excluded level names by comma and process each one
                    excluded_levels <- strsplit(diag$excluded_level_names, ", ")[[1]]
                    for (level_name_raw in excluded_levels) {
                        level_name <- trimws(level_name_raw)
                        stats <- if (!is.null(level_stats) && level_name %in% names(level_stats)) level_stats[[level_name]] else list()
                        level_idx <- if (length(level_names) > 0) match(level_name, level_names) else NA
                        if (is.na(level_idx)) level_idx <- length(level_names) + 1
                        # Record excluded level
                    diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
                        variable = var_name,
                        level = level_name,
                            n_total = if (!is.null(stats$n_total)) stats$n_total else NA,
                            n_plaque = if (!is.null(stats$n_plaque)) stats$n_plaque else NA,
                            n_gksrs = if (!is.null(stats$n_gksrs)) stats$n_gksrs else NA,
                            events_plaque = if (!is.null(stats$events_plaque)) stats$events_plaque else NA,
                            events_gksrs = if (!is.null(stats$events_gksrs)) stats$events_gksrs else NA,
                            treatment_effect = NA,
                            ci_lower = NA,
                            ci_upper = NA,
                            p_value = NA,
                            status = if (identical(diag$model_status, "no_supported_levels")) {
                                "not_estimable_no_supported_levels"
                            } else if (identical(diag$model_status, "model_failure")) {
                                "not_estimable_model_failure"
                            } else {
                                "not_estimable_interaction_exclusion"
                            },
                            reason = if (!is.null(stats$exclusion_reason) && nzchar(stats$exclusion_reason)) stats$exclusion_reason else "Insufficient data to fit model",
                        other_variable_contents = "",
                        variable_order = var_index,
                        level_order = level_idx,
                        stringsAsFactors = FALSE
                    )
                    }
                }
            }
        } else {
            # No data available
            diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
                variable = var_name,
                level = "No data available",
                n_total = NA,
                n_plaque = NA,
                n_gksrs = NA,
                events_plaque = NA,
                events_gksrs = NA,
                treatment_effect = NA,
                ci_lower = NA,
                ci_upper = NA,
                p_value = NA,
                status = "no_data",
                reason = "No subgroup effects data available",
                other_variable_contents = "",
                variable_order = var_index,
                level_order = Inf,
                stringsAsFactors = FALSE
            )
        }
    }

    # Combine all diagnostics rows
    if (length(diagnostics_rows) == 0) {
        return(data.frame())
    }

    # Normalize columns across all row frames to avoid rbind column mismatch
    normalized <- lapply(diagnostics_rows, function(df) {
        all_cols <- c(
            "variable", "level", "n_total", "n_plaque", "n_gksrs",
            "events_plaque", "events_gksrs", "treatment_effect", "ci_lower", "ci_upper",
            "p_value", "status", "reason", "other_variable_contents", "variable_order", "level_order"
        )
        # Add any missing columns as NA
        for (col in setdiff(all_cols, names(df))) df[[col]] <- NA
        # Reorder
        df <- df[, all_cols, drop = FALSE]
        df
    })

    diagnostics_df <- do.call(rbind, normalized)

    diagnostics_df$estimability_method_note <- if (exists("get_subgroup_estimability_method_note", mode = "function")) {
        get_subgroup_estimability_method_note()
    } else {
        paste(
            "Unsupported subgroup levels remain displayed as not estimable.",
            "Treatment effects are reported only for finite supported models; interaction p-values are omitted when fewer than two levels are estimable."
        )
    }

    if ("variable_order" %in% names(diagnostics_df)) {
        diagnostics_df$variable_order[is.na(diagnostics_df$variable_order)] <- Inf
    }
    if ("level_order" %in% names(diagnostics_df)) {
        diagnostics_df$level_order[is.na(diagnostics_df$level_order)] <- Inf
        diagnostics_df <- diagnostics_df[order(
            diagnostics_df$variable_order,
            diagnostics_df$level_order,
            seq_len(nrow(diagnostics_df))
        ), ]
        diagnostics_df$variable_order <- NULL
        diagnostics_df$level_order <- NULL
    }

    return(diagnostics_df)
}

#' Determine whether a numeric value is invalid for diagnostics
#' Accepts numeric or character "Inf" entries and flags them as invalid
#' @param x numeric or character
#' @return TRUE if NA, non-finite, or string "Inf"
diagnostics_invalid_numeric <- function(x) {
    is.na(x) || !is.finite(x) || (is.character(x) && x == "Inf")
}
