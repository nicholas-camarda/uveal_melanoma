# Extreme Estimate Handling Utilities
# Author: Nicholas Camarda
# Date: 7/26/2025
# Description: Centralized functions for detecting and filtering extreme regression estimates
#              across all analysis types (binary, survival, continuous outcomes)

#' Detect extreme regression estimates using comprehensive criteria
#'
#' Identifies problematic regression estimates that should be excluded from tables
#' based on multiple criteria including infinite CIs, perfect separation, near-perfect
#' separation, and extremely wide CIs.
#'
#' @param estimate Numeric vector of effect estimates (OR, HR, etc.)
#' @param ci_lower Numeric vector of lower CI bounds
#' @param ci_upper Numeric vector of upper CI bounds
#' @param effect_measure Character string indicating measure type ("OR", "HR", etc.)
#' @param is_exponentiated Logical indicating if values are on exponentiated scale (TRUE) or log scale (FALSE)
#' @return List with extreme_indices (rows to exclude) and reasons for exclusion
detect_extreme_regression_estimates <- function(estimate, ci_lower, ci_upper, effect_measure = "HR", is_exponentiated = TRUE) {
    
    extreme_indices <- c()
    exclusion_reasons <- c()
    
    for (i in seq_along(estimate)) {
        reason <- NULL
        
        if (toupper(effect_measure) %in% c("HR", "OR", "RR", "MD", "BETA", "ESTIMATE", "LOG-ODDS", "LOG-HAZARD")) {
            # 1. Infinite CIs (always extreme regardless of scale)
            if (is.infinite(ci_upper[i]) || is.infinite(ci_lower[i])) {
                reason <- sprintf("Infinite CI detected: (%.2f, %.2f)", ci_lower[i], ci_upper[i])
            }
            # 2. Handle based on whether values are exponentiated or on log scale
            else if (is_exponentiated) {
                # EXPONENTIATED SCALE (OR/HR values, always positive, around 1)
                # Perfect separation: CI = (0,0) - shouldn't happen on ratio scale
                if (!is.na(ci_lower[i]) && !is.na(ci_upper[i]) && ci_lower[i] == ci_upper[i] && ci_lower[i] == 0) {
                    reason <- "Perfect separation detected: CI = (0,0)"
                }
                # Extremely wide CIs on exponentiated scale - use appropriate threshold
                else if (!is.na(ci_lower[i]) && !is.na(ci_upper[i]) && 
                         (ci_upper[i] - ci_lower[i]) > EXPONENTIATED_CI_THRESHOLD) {
                    reason <- sprintf("Extremely wide CI detected (exponentiated): (%.2f, %.2f) - width = %.2f", 
                                    ci_lower[i], ci_upper[i], ci_upper[i] - ci_lower[i])
                }
                # Very small lower CI (near-perfect separation on ratio scale)
                # For exponentiated OR/HR, check if lower bound is very close to 0
                else if (!is.na(ci_lower[i]) && ci_lower[i] < NEAR_PERFECT_SEPARATION_THRESHOLD) {
                    reason <- sprintf("Near-perfect separation detected: CI lower bound = %.8f", ci_lower[i])
                }
            } else {
                # LOG SCALE (log-odds/log-hazard values, can be negative, around 0)
                # Perfect separation: extremely large absolute values
                if (!is.na(ci_lower[i]) && !is.na(ci_upper[i])) {
                    # Check for extremely wide CIs on log scale using appropriate threshold
                    ci_width <- ci_upper[i] - ci_lower[i]
                    if (ci_width > LOG_SCALE_CI_THRESHOLD) {
                        reason <- sprintf("Extremely wide CI detected (log scale): (%.2f, %.2f) - width = %.2f", 
                                        ci_lower[i], ci_upper[i], ci_width)
                    }
                    # Check for extremely large absolute values (perfect separation)
                    else if (abs(estimate[i]) > 10) {
                        reason <- sprintf("Perfect separation detected (log scale): estimate = %.2f", estimate[i])
                    }
                    # Check for near-perfect separation: very large absolute estimates with tight CIs
                    else if (abs(estimate[i]) > 5 && ci_width < LOG_SCALE_NEAR_PERFECT_SEPARATION_THRESHOLD) {
                        reason <- sprintf("Near-perfect separation detected (log scale): estimate = %.2f, CI = (%.2f, %.2f)", 
                                        estimate[i], ci_lower[i], ci_upper[i])
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
    
    return(list(
        extreme_indices = extreme_indices,
        exclusion_reasons = exclusion_reasons
    ))
}

#' Filter extreme estimates from regression table data
#'
#' Applies extreme estimate filtering to tbl_regression output, ensuring
#' that only reliable estimates are included in final tables.
#'
#' @param tbl_data Data frame from tbl_regression$table_body
#' @param extreme_terms Character vector of extreme term names to remove
#' @param variables_to_check Character vector of variable names to check for sparse tables
#' @param analysis_name Character string for logging (e.g., "srd", "retinopathy")
#' @return List with filtered table data and diagnostics
filter_extreme_estimates_from_table <- function(tbl_data, extreme_terms, variables_to_check, analysis_name = "analysis") {
    
    if (length(extreme_terms) == 0) {
        return(list(
            tbl_data_filtered = tbl_data,
            rows_removed = 0,
            sparse_table_warning = FALSE
        ))
    }
    
    # Find rows to remove based on term names
    rows_to_remove <- which(tbl_data$term %in% extreme_terms)
    
    # DEBUG: Print filtering info
    log_enhanced(sprintf("DEBUG: Filtering for %s - found %d extreme terms, %d matching rows in table", 
                       analysis_name, length(extreme_terms), length(rows_to_remove)), level = "DEBUG")
    if (length(extreme_terms) > 0) {
        log_enhanced(sprintf("DEBUG: Available terms in table: %s", paste(unique(tbl_data$term), collapse = ", ")), level = "DEBUG")
    }
    
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
            
            # If removing extreme estimates would leave NO rows for this variable, keep its extreme estimates
            if (length(remaining_rows) == 0) {
                log_enhanced(sprintf("Keeping extreme estimates for %s in %s to avoid empty variable (would leave no levels)", 
                                   var, analysis_name), level = "WARN")
                sparse_table_warning <- TRUE
                # Remove these rows from the list of rows to remove
                final_rows_to_remove <- setdiff(final_rows_to_remove, var_extreme_rows)
            }
        }
        
        # Remove the rows that are safe to remove
        if (length(final_rows_to_remove) > 0) {
            tbl_data_filtered <- tbl_data[-final_rows_to_remove, ]
            log_enhanced(sprintf("Removed %d extreme estimates from %s table output", 
                               length(final_rows_to_remove), analysis_name), level = "INFO")
            return(list(
                tbl_data_filtered = tbl_data_filtered,
                rows_removed = length(final_rows_to_remove),
                sparse_table_warning = sparse_table_warning
            ))
        } else {
            log_enhanced(sprintf("No rows safe to remove in %s after checking for empty variables", 
                               analysis_name), level = "INFO")
            return(list(
                tbl_data_filtered = tbl_data,
                rows_removed = 0,
                sparse_table_warning = TRUE
            ))
        }
    }
    
    return(list(
        tbl_data_filtered = tbl_data,
        rows_removed = 0,
        sparse_table_warning = FALSE
    ))
}



#' Apply extreme estimate filtering to a regression table
#'
#' Complete workflow for detecting and filtering extreme estimates from
#' regression tables, including diagnostics collection.
#'
#' @param tbl gtsummary table object
#' @param model_fit Fitted model object (glm, coxph, etc.)
#' @param effect_measure Character string for effect measure type
#' @param variables_to_check Character vector of variables to check for sparse tables
#' @param analysis_name Character string for logging
#' @return List with filtered table, diagnostics, and filtering summary
apply_extreme_estimate_filtering <- function(tbl, model_fit, effect_measure = "OR", variables_to_check = NULL, analysis_name = "analysis") {
    
    # Extract table data
    tbl_data <- tbl$table_body
    
    # If no variables specified, use all unique variables in the table
    if (is.null(variables_to_check)) {
        variables_to_check <- unique(tbl_data$variable)
    }
    
    # ALWAYS use table-based detection to ensure all infinite CIs are caught
    # Check if the table has confidence interval columns
    if ("conf.low" %in% colnames(tbl_data) && "conf.high" %in% colnames(tbl_data)) {
        # Use the table's confidence intervals for extreme detection
        table_ci_lower <- tbl_data$conf.low
        table_ci_upper <- tbl_data$conf.high
        table_estimates <- tbl_data$estimate
        
        # Detect extreme estimates from table data
        # DETERMINISTIC APPROACH: Use effect_measure to determine scale instead of fragile value detection
        # Exponentiated measures: OR, HR (always positive when exponentiated)
        # Raw scale measures: MD, beta, estimate (can be negative)
        is_table_exponentiated <- effect_measure %in% c("OR", "HR")
        
        extreme_detection <- detect_extreme_regression_estimates(table_estimates, table_ci_lower, table_ci_upper, effect_measure, is_exponentiated = is_table_exponentiated)
        
        # Get term names from table
        extreme_terms <- tbl_data$term[extreme_detection$extreme_indices]
        
        # DEBUG: Print what was detected
        log_enhanced(sprintf("DEBUG: Table-based extreme detection for %s found %d extreme estimates", analysis_name, length(extreme_detection$extreme_indices)), level = "DEBUG")
        if (length(extreme_detection$extreme_indices) > 0) {
            for (i in seq_along(extreme_detection$extreme_indices)) {
                idx <- extreme_detection$extreme_indices[i]
                log_enhanced(sprintf("DEBUG: Table extreme estimate %d: estimate=%.2e, CI=(%.2e, %.2e), reason=%s", 
                                   i, table_estimates[idx], table_ci_lower[idx], table_ci_upper[idx], extreme_detection$exclusion_reasons[i]), level = "DEBUG")
            }
        }
        
        # Filter table
        filtering_result <- filter_extreme_estimates_from_table(
            tbl_data = tbl_data,
            extreme_terms = extreme_terms,
            variables_to_check = variables_to_check,
            analysis_name = analysis_name
        )
        
        # Update table with filtered data
        tbl_filtered <- tbl
        tbl_filtered$table_body <- filtering_result$tbl_data_filtered
        
        # Prepare diagnostics
        diagnostics <- list(
            extreme_terms = extreme_terms,
            exclusion_reasons = extreme_detection$exclusion_reasons,
            rows_removed = filtering_result$rows_removed,
            sparse_table_warning = filtering_result$sparse_table_warning,
            confint_error = FALSE
        )
        
        return(list(
            tbl_filtered = tbl_filtered,
            diagnostics = diagnostics
        ))
    } else {
        # No confidence interval columns in table, return original table with warning
        return(list(
            tbl_filtered = tbl,
            diagnostics = list(
                extreme_terms = character(0),
                rows_removed = 0,
                sparse_table_warning = FALSE,
                confint_error = TRUE
            )
        ))
    }
} 