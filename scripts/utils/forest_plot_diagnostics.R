#' Create forest plot diagnostics from raw subgroup data
#'
#' @param subgroup_results List of subgroup analysis results
#' @param other_map List mapping variable names to "Other" category contents (optional)
#' @param effect_measure Character string for effect measure ("HR", "OR", etc.)
#' @param variable_order Character vector of variables to include (optional)
#' @return Data frame with forest plot diagnostics
create_forest_plot_diagnostics <- function(subgroup_results, other_map = NULL, effect_measure = "HR", variable_order = NULL) {
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
    for (var_name in variable_order) {
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
                stringsAsFactors = FALSE
            )
            next
        }

        # Variable header row with interaction p-value if available
        var_data <- subgroup_results[[var_name]]
        header_interaction_p <- if (!is.null(var_data$interaction_p)) var_data$interaction_p else NA
        header_reason <- ""
        if (is.null(header_interaction_p) || is.na(header_interaction_p)) {
            if (!is.null(var_data$interaction_diagnostics) && !is.null(var_data$interaction_diagnostics$failure_reason)) {
                header_reason <- paste("Missing interaction p-value:", var_data$interaction_diagnostics$failure_reason)
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
            status = "header",
            reason = header_reason,
            other_variable_contents = "",
            stringsAsFactors = FALSE
        )

        # Check if data exists for this variable
        if (var_name %in% names(subgroup_results)) {
            var_data <- subgroup_results[[var_name]]

            if (!is.null(var_data$subgroup_effects) && nrow(var_data$subgroup_effects) > 0) {
                # Add subgroup rows
                effects_data <- var_data$subgroup_effects
                for (i in 1:nrow(effects_data)) {
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
                            stringsAsFactors = FALSE
                        )
                        next # skip this subgroup level completely
                    }

                    # Additional check for ratio measures (must be > 0)
                    if (toupper(effect_measure) %in% c("HR", "OR", "RR")) {
                        if (row_data$treatment_effect <= 0 || row_data$ci_lower <= 0) {
                            # Record diagnostics for skipped rows
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
                                reason = "Treatment effect or CI bounds ≤ 0 (invalid for ratio measures)",
                                other_variable_contents = "",
                                stringsAsFactors = FALSE
                            )
                            next
                        }
                    }

                    # Check for extreme estimates (above threshold)
                    if (abs(row_data$treatment_effect) > 100) { # EXTREME_ESTIMATE_THRESHOLD
                        # Record diagnostics for skipped rows
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
                            reason = sprintf("Estimate (%.2f) exceeds threshold of 100", row_data$treatment_effect),
                            other_variable_contents = "",
                            stringsAsFactors = FALSE
                        )
                        next
                    }

                    # This row will be plotted - get events from subgroup effects data
                    events_plaque <- if ("events_plaque" %in% names(row_data)) row_data$events_plaque else NA
                    events_gksrs <- if ("events_gksrs" %in% names(row_data)) row_data$events_gksrs else NA

                    # Get "other" variable contents if applicable
                    other_contents <- ""
                    if (!is.null(other_map) && var_name %in% names(other_map) &&
                        as.character(row_data$subgroup_level) == "Other") {
                        other_contents <- paste(other_map[[var_name]], collapse = ", ")
                    }

                    # Record valid subgroup level
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
                        status = "plotted",
                        reason = "",
                        other_variable_contents = other_contents,
                        stringsAsFactors = FALSE
                    )
                }
            }

            # Process excluded levels from interaction_diagnostics
            if (!is.null(var_data$interaction_diagnostics)) {
                diag <- var_data$interaction_diagnostics
                if (!is.null(diag$excluded_level_names) && diag$excluded_level_names != "") {
                    # Split the excluded level names by comma and process each one
                    excluded_levels <- strsplit(diag$excluded_level_names, ", ")[[1]]
                    for (level_name in excluded_levels) {
                        # Record excluded level
                        diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
                            variable = var_name,
                            level = level_name,
                            n_total = NA,
                            n_plaque = NA,
                            n_gksrs = NA,
                            events_plaque = NA,
                            events_gksrs = NA,
                            treatment_effect = NA,
                            ci_lower = NA,
                            ci_upper = NA,
                            p_value = NA,
                            status = "skipped_insufficient_sample",
                            reason = "Insufficient sample size",
                            other_variable_contents = "",
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
            "p_value", "status", "reason", "other_variable_contents"
        )
        # Add any missing columns as NA
        for (col in setdiff(all_cols, names(df))) df[[col]] <- NA
        # Reorder
        df <- df[, all_cols, drop = FALSE]
        df
    })

    diagnostics_df <- do.call(rbind, normalized)
    return(diagnostics_df)
}

#' Determine whether a numeric value is invalid for diagnostics
#' Accepts numeric or character "Inf" entries and flags them as invalid
#' @param x numeric or character
#' @return TRUE if NA, non-finite, or string "Inf"
diagnostics_invalid_numeric <- function(x) {
    is.na(x) || !is.finite(x) || (is.character(x) && x == "Inf")
}
