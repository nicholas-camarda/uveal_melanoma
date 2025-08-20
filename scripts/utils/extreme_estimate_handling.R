# Extreme Estimate Handling Utilities
# Author: Nicholas Camarda
# Date: 7/26/2025
# Description: Consolidated function for processing extreme regression estimates
#              across all analysis types (binary, survival, continuous outcomes)

#' Process extreme regression estimates in a unified workflow
#'
#' Consolidated function that detects and filters extreme estimates from regression tables.
#' Replaces the previous three-function approach (detect, filter, apply) with a single
#' streamlined process that eliminates redundancy while maintaining all functionality.
#'
#' @param tbl gtsummary table object
#' @param model_fit Fitted model object (glm, coxph, etc.)
#' @param effect_measure Character string for effect measure type ("OR", "HR", "MD", etc.)
#' @param variables_to_check Character vector of variables to check for sparse tables
#' @param analysis_name Character string for logging
#' @return List with filtered table, diagnostics, and filtering summary
process_extreme_estimates <- function(tbl, model_fit, effect_measure = "OR", variables_to_check = NULL, analysis_name = "analysis") {
    # Extract table data
    tbl_data <- tbl$table_body

    # Initialize completely_removed_variables
    completely_removed_variables <- c()

    # DEBUG: Check initial table state
    logger::log_info(sprintf("DEBUG: process_extreme_estimates for %s - initial table has %d rows", analysis_name, nrow(tbl_data)))
    if (nrow(tbl_data) > 0) {
        logger::log_info(sprintf("DEBUG: Initial table variables: %s", paste(unique(tbl_data$variable), collapse = ", ")))
    }

    # If no variables specified, use all unique variables in the table
    if (is.null(variables_to_check)) {
        variables_to_check <- unique(tbl_data$variable)
    }

    # Extract reference level information for all variables
    reference_levels_info <- data.frame(
        variable = character(),
        reference_level = character(),
        stringsAsFactors = FALSE
    )

    for (var in unique(tbl_data$variable)) {
        var_rows <- tbl_data[tbl_data$variable == var, ]
        level_rows <- var_rows[var_rows$row_type == "level", ]

        # Find reference level (rows with empty estimates)
        reference_rows <- level_rows[is.na(level_rows$estimate) | level_rows$estimate == "" | level_rows$estimate == "-", ]

        if (nrow(reference_rows) > 0) {
            reference_level <- paste(reference_rows$term, collapse = ", ")
        } else if (nrow(level_rows) == 0) {
            # Continuous variable
            reference_level <- "(continuous)"
        } else {
            # No clear reference level found
            reference_level <- "(unknown)"
        }

        reference_levels_info <- rbind(reference_levels_info, data.frame(
            variable = var,
            reference_level = reference_level,
            stringsAsFactors = FALSE
        ))
    }

    # Check if the table has confidence interval columns
    if (!("conf.low" %in% colnames(tbl_data) && "conf.high" %in% colnames(tbl_data))) {
        # No confidence interval columns in table, return original table with warning
        return(list(
            tbl_filtered = tbl,
            diagnostics = list(
                extreme_terms = character(0),
                rows_removed = 0,
                sparse_table_warning = FALSE,
                confint_error = TRUE,
                reference_levels_info = reference_levels_info,
                completely_removed_variables = completely_removed_variables
            )
        ))
    }

    # Use the table's confidence intervals for extreme detection
    table_ci_lower <- tbl_data$conf.low
    table_ci_upper <- tbl_data$conf.high
    table_estimates <- tbl_data$estimate
    table_row_types <- tbl_data$row_type

    # DETERMINISTIC APPROACH: Use effect_measure to determine scale
    # Exponentiated measures: OR, HR (always positive when exponentiated)
    # Raw scale measures: MD, beta, estimate (can be negative)
    is_exponentiated <- effect_measure %in% c("OR", "HR")

    # Initialize collections
    extreme_indices <- c()
    exclusion_reasons <- c()

    # STEP 0: Remove non-estimable factor-level rows from HTML output
    # Keep exactly one NA-level per variable (reference); drop other NA-levels
    for (var in unique(tbl_data$variable)) {
        idx_var <- which(tbl_data$variable == var & tbl_data$row_type == "level")
        if (length(idx_var) == 0) next
        # Identify NA/blank estimate and CI rows
        na_rows <- idx_var[
            (is.na(table_estimates[idx_var]) | table_estimates[idx_var] == "" | table_estimates[idx_var] == "-") &
                (is.na(table_ci_lower[idx_var]) | table_ci_lower[idx_var] == "") &
                (is.na(table_ci_upper[idx_var]) | table_ci_upper[idx_var] == "")
        ]
        if (length(na_rows) > 1) {
            # Keep the first NA level (reference), remove the rest
            drop_rows <- na_rows[-1]
            extreme_indices <- c(extreme_indices, drop_rows)
            exclusion_reasons <- c(exclusion_reasons, rep("Non-estimable level (no coefficient/CI); removed from HTML", length(drop_rows)))
        }
    }

    # DETECT EXTREME ESTIMATES
    for (i in seq_along(table_estimates)) {
        # Skip already recorded removals
        if (i %in% extreme_indices) next
        reason <- NULL

        # Skip Factor Label rows - these contain interaction p-values and should never be filtered
        if (!is.null(table_row_types) && length(table_row_types) >= i && table_row_types[i] == "Factor Label") {
            next
        }

        if (toupper(effect_measure) %in% c("HR", "OR", "RR", "MD", "BETA", "ESTIMATE", "LOG-ODDS", "LOG-HAZARD")) {
            # 1. Infinite CIs (always extreme regardless of scale)
            if (is.infinite(table_ci_upper[i]) || is.infinite(table_ci_lower[i])) {
                reason <- sprintf("Infinite CI detected: (%.2f, %.2f)", table_ci_lower[i], table_ci_upper[i])
            }
            # 2. Handle based on whether values are exponentiated or on log scale
            else if (is_exponentiated) {
                # EXPONENTIATED SCALE (OR/HR values, always positive, around 1)
                # Perfect separation: CI = (0,0) - shouldn't happen on ratio scale
                if (!is.na(table_ci_lower[i]) && !is.na(table_ci_upper[i]) && table_ci_lower[i] == table_ci_upper[i] && table_ci_lower[i] == 0) {
                    reason <- "Perfect separation detected: CI = (0,0)"
                }
                # Degenerate zero-width CI at a finite value (e.g., 1.00, 1.00) indicates non-estimable/aliased level
                else if (!is.na(table_ci_lower[i]) && !is.na(table_ci_upper[i]) && is.finite(table_ci_lower[i]) && is.finite(table_ci_upper[i]) && table_ci_lower[i] == table_ci_upper[i]) {
                    reason <- sprintf("Degenerate zero-width CI detected (exponentiated): (%.2f, %.2f)", table_ci_lower[i], table_ci_upper[i])
                }
                # Extremely wide CIs on exponentiated scale
                else if (!is.na(table_ci_lower[i]) && !is.na(table_ci_upper[i]) &&
                    (table_ci_upper[i] - table_ci_lower[i]) > EXPONENTIATED_CI_THRESHOLD) {
                    reason <- sprintf(
                        "Extremely wide CI detected (exponentiated): (%.2f, %.2f) - width = %.2f",
                        table_ci_lower[i], table_ci_upper[i], table_ci_upper[i] - table_ci_lower[i]
                    )
                }
                # Very small lower CI (near-perfect separation on ratio scale)
                else if (!is.na(table_ci_lower[i]) && table_ci_lower[i] < NEAR_PERFECT_SEPARATION_THRESHOLD) {
                    reason <- sprintf("Near-perfect separation detected: CI lower bound = %.8f", table_ci_lower[i])
                }
            } else {
                # LOG SCALE (log-odds/log-hazard values, can be negative, around 0)
                if (!is.na(table_ci_lower[i]) && !is.na(table_ci_upper[i])) {
                    # Check for extremely wide CIs on log scale
                    ci_width <- table_ci_upper[i] - table_ci_lower[i]
                    if (ci_width > LOG_SCALE_CI_THRESHOLD) {
                        reason <- sprintf(
                            "Extremely wide CI detected (log scale): (%.2f, %.2f) - width = %.2f",
                            table_ci_lower[i], table_ci_upper[i], ci_width
                        )
                    }
                    # Check for extremely large absolute values (perfect separation)
                    else if (abs(table_estimates[i]) > 10) {
                        reason <- sprintf("Perfect separation detected (log scale): estimate = %.2f", table_estimates[i])
                    }
                    # Check for near-perfect separation: very large absolute estimates with tight CIs
                    else if (abs(table_estimates[i]) > 5 && ci_width < LOG_SCALE_NEAR_PERFECT_SEPARATION_THRESHOLD) {
                        reason <- sprintf(
                            "Near-perfect separation detected (log scale): estimate = %.2f, CI = (%.2f, %.2f)",
                            table_estimates[i], table_ci_lower[i], table_ci_upper[i]
                        )
                    }
                }
            }
        }

        # Record if extreme
        if (!is.null(reason)) {
            extreme_indices <- c(extreme_indices, i)
            exclusion_reasons <- c(exclusion_reasons, reason)
        }
    }

    # Get term names from table
    extreme_terms <- tbl_data$term[extreme_indices]

    # DEBUG: Print what was detected
    logger::log_info(sprintf("DEBUG: Extreme detection for %s found %d extreme estimates", analysis_name, length(extreme_indices)))
    if (length(extreme_indices) > 0) {
        for (i in seq_along(extreme_indices)) {
            idx <- extreme_indices[i]
            logger::log_info(sprintf(
                "DEBUG: Row %d (%s) filtered: %s",
                idx, tbl_data$term[idx], exclusion_reasons[i]
            ))
        }
    }

    # FILTER EXTREME ESTIMATES
    if (length(extreme_terms) == 0) {
        return(list(
            tbl_filtered = tbl,
            diagnostics = list(
                extreme_terms = character(0),
                exclusion_reasons = character(0),
                rows_removed = 0,
                sparse_table_warning = FALSE,
                confint_error = FALSE,
                reference_levels_info = reference_levels_info,
                completely_removed_variables = completely_removed_variables
            )
        ))
    }

    # Find rows to remove based on term names
    rows_to_remove <- which(tbl_data$term %in% extreme_terms)

    # DEBUG: Print filtering info
    logger::log_info(sprintf(
        "DEBUG: Filtering for %s - found %d extreme terms, %d matching rows in table",
        analysis_name, length(extreme_terms), length(rows_to_remove)
    ))

    if (length(rows_to_remove) > 0) {
        # Track which rows to actually remove (some may be kept to avoid empty variables)
        final_rows_to_remove <- rows_to_remove
        sparse_table_warning <- FALSE

        # Group extreme rows by variable
        extreme_rows_by_var <- list()
        for (i in rows_to_remove) {
            var_name <- tbl_data$variable[i]
            if (!(var_name %in% names(extreme_rows_by_var))) {
                extreme_rows_by_var[[var_name]] <- c()
            }
            extreme_rows_by_var[[var_name]] <- c(extreme_rows_by_var[[var_name]], i)
        }

        # Check each variable independently
        for (var in names(extreme_rows_by_var)) {
            var_rows <- which(tbl_data$variable == var)
            var_extreme_rows <- extreme_rows_by_var[[var]]
            remaining_rows <- var_rows[!var_rows %in% var_extreme_rows]

            # Check if remaining rows are all reference levels (empty estimates)
            if (length(remaining_rows) > 0) {
                remaining_data <- tbl_data[remaining_rows, ]
                all_reference_levels <- all(is.na(remaining_data$estimate) | remaining_data$estimate == "" | remaining_data$estimate == "-")

                if (all_reference_levels) {
                    logger::log_info(sprintf(
                        "Removing entire variable '%s' in %s - only reference levels remain after filtering extreme estimates",
                        var, analysis_name
                    ))
                    # Add ALL rows for this variable to the removal list
                    final_rows_to_remove <- c(final_rows_to_remove, var_rows)
                    completely_removed_variables <- c(completely_removed_variables, var)
                }
            } else {
                # If removing extreme estimates would leave NO rows for this variable, remove the entire variable
                logger::log_info(sprintf(
                    "Removing entire variable '%s' in %s - no valid levels remain after filtering extreme estimates",
                    var, analysis_name
                ))
                # Add ALL rows for this variable to the removal list
                final_rows_to_remove <- c(final_rows_to_remove, var_rows)
                completely_removed_variables <- c(completely_removed_variables, var)
            }
        }

        # Remove the rows that are safe to remove
        if (length(final_rows_to_remove) > 0) {
            tbl_data_filtered <- tbl_data[-final_rows_to_remove, ]
            logger::log_info(sprintf(
                "Removed %d extreme estimates from %s table output",
                length(final_rows_to_remove), analysis_name
            ))

            # Update table with filtered data
            tbl_filtered <- tbl
            tbl_filtered$table_body <- tbl_data_filtered

            # DEBUG: Show final table state
            logger::log_info(sprintf("DEBUG: After filtering, table has %d rows", nrow(tbl_data_filtered)))
            if (nrow(tbl_data_filtered) > 0) {
                logger::log_info(sprintf("DEBUG: Remaining terms: %s", paste(tbl_data_filtered$term, collapse = ", ")))
            }

            logger::log_info(sprintf(
                "DEBUG: process_extreme_estimates returning completely_removed_variables: %s",
                paste(completely_removed_variables, collapse = ", ")
            ))
            return(list(
                tbl_filtered = tbl_filtered,
                diagnostics = list(
                    extreme_terms = extreme_terms,
                    exclusion_reasons = exclusion_reasons,
                    rows_removed = length(final_rows_to_remove),
                    sparse_table_warning = sparse_table_warning,
                    confint_error = FALSE,
                    reference_levels_info = reference_levels_info,
                    completely_removed_variables = completely_removed_variables
                )
            ))
        } else {
            logger::log_info(sprintf(
                "No rows safe to remove in %s after checking for empty variables",
                analysis_name
            ))
            return(list(
                tbl_filtered = tbl,
                diagnostics = list(
                    extreme_terms = extreme_terms,
                    exclusion_reasons = exclusion_reasons,
                    rows_removed = 0,
                    sparse_table_warning = TRUE,
                    confint_error = FALSE,
                    reference_levels_info = reference_levels_info,
                    completely_removed_variables = completely_removed_variables
                )
            ))
        }
    }

    return(list(
        tbl_filtered = tbl,
        diagnostics = list(
            extreme_terms = extreme_terms,
            exclusion_reasons = exclusion_reasons,
            rows_removed = 0,
            sparse_table_warning = FALSE,
            confint_error = FALSE,
            reference_levels_info = reference_levels_info,
            completely_removed_variables = completely_removed_variables
        )
    ))
}

# DEPRECATED FUNCTIONS - These are kept for backward compatibility but will be removed
# in the next phase of simplification. Use process_extreme_estimates instead.

#' @deprecated Use process_extreme_estimates instead
detect_extreme_regression_estimates <- function(estimate, ci_lower, ci_upper, effect_measure = "HR", is_exponentiated = TRUE, row_types = NULL) {
    .Deprecated("process_extreme_estimates", package = "extreme_estimate_handling")
    stop("This function is deprecated. Use process_extreme_estimates instead.")
}

#' @deprecated Use process_extreme_estimates instead
filter_extreme_estimates_from_table <- function(tbl_data, extreme_terms, variables_to_check, analysis_name = "analysis") {
    .Deprecated("process_extreme_estimates", package = "extreme_estimate_handling")
    stop("This function is deprecated. Use process_extreme_estimates instead.")
}

#' @deprecated Use process_extreme_estimates instead
apply_extreme_estimate_filtering <- function(tbl, model_fit, effect_measure = "OR", variables_to_check = NULL, analysis_name = "analysis") {
    .Deprecated("process_extreme_estimates", package = "extreme_estimate_handling")
    stop("This function is deprecated. Use process_extreme_estimates instead.")
}
