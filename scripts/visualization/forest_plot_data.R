# Forest Plot Data Preparation

#' Create formatted data for single cohort forest plot using forestploter format
#'
#' @param subgroup_results List of subgroup analysis results
#' @param variable_order Character vector of variables to include (enforced for consistency)
#' @param treatment_labels Character vector of treatment labels
#' @param effect_measure Character string for effect measure
#' @param other_map List mapping variable names to "Other" category contents (optional)
#' @return List with formatted data for forestploter
create_forest_plot_data <- function(subgroup_results, variable_order, treatment_labels, effect_measure, other_map = NULL) {
    
    # Initialize data collection
    all_rows <- list()
    est_values <- c()
    lower_values <- c()
    upper_values <- c()
    is_summary <- c()
    font_face <- c()
    text_size <- c()
    missing_interaction_vars <- character(0)  # Track variables where interaction p could not be estimated
    diagnostics_rows <- list()
    
    # Handle empty variable_order case
    if (length(variable_order) == 0) {
        # Return empty data structure
        return(list(
            data_frame = data.frame(
                Subgroup = character(),
                `GKSRS n/N` = character(),
                `Plaque n/N` = character(),
                ` ` = character(),
                `HR (95% CI)` = character(),
                `p-value` = character(),
                `Int p` = character(),
                stringsAsFactors = FALSE
            ),
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
                GKSRS_n = "",
                Plaque_n = "",
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
            GKSRS_n = "",
            Plaque_n = "",
            stringsAsFactors = FALSE
        )
        
        # Add blank column for CI, subgroup p-value, and interaction p-value columns
        var_header$` ` <- paste(rep(" ", 20), collapse = " ")
        var_header$`HR (95% CI)` <- ""
        var_header$`p-value` <- ""
        # Check for interaction p-value and capture failure reason
        if (!is.null(subgroup_results[[var_name]]$interaction_p) && !is.na(subgroup_results[[var_name]]$interaction_p)) {
            var_header$`Interaction p` <- forest_format_p_value(subgroup_results[[var_name]]$interaction_p)
            interaction_failure_reason <- ""  # No reason needed when successful
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
        diagnostics_rows[[length(diagnostics_rows)+1]] <- data.frame(
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
            
            if (!is.null(var_data$subgroup_effects) && nrow(var_data$subgroup_effects) > 0) {
                # Add subgroup rows
                effects_data <- var_data$subgroup_effects
                for (i in 1:nrow(effects_data)) {
                    row_data <- effects_data[i, ]
                    
                    # Skip rows with NA, non-finite, or (for ratio measures) non-positive values
                    if (diagnostics_invalid_numeric(row_data$treatment_effect) ||
                        diagnostics_invalid_numeric(row_data$ci_lower) ||
                        diagnostics_invalid_numeric(row_data$ci_upper)) {
                        
                        # Still record diagnostics for skipped rows
                        diagnostics_rows[[length(diagnostics_rows)+1]] <- data.frame(
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
                            status = "skipped_non_finite",
                            reason = "Treatment effect, CI bounds, or both are NA/non-finite",
                            stringsAsFactors = FALSE
                        )
                        next  # skip this subgroup level completely
                    }

                    # Additional check for ratio measures (must be > 0)
                    if (toupper(effect_measure) %in% c("HR", "OR", "RR")) {
                        if (row_data$treatment_effect <= 0 || row_data$ci_lower <= 0) {
                            # Still record diagnostics for skipped rows
                            diagnostics_rows[[length(diagnostics_rows)+1]] <- data.frame(
                                variable = var_name,
                                level = as.character(row_data$subgroup_level),
                                n_total = row_data$n_total,
                                n_plaque = row_data$n_plaque,
                                n_gksrs = row_data$n_gksrs,
                                events_plaque = NA,  # Don't calculate events for invalid rows
                                events_gksrs = NA,
                                treatment_effect = row_data$treatment_effect,
                                ci_lower = row_data$ci_lower,
                                ci_upper = row_data$ci_upper,
                                p_value = row_data$p_value,
                                status = "skipped_non_positive",
                                reason = "Treatment effect or CI bounds ≤ 0 (invalid for ratio measures)",
                                stringsAsFactors = FALSE
                            )
                            next
                        }
                    }

                    # Check for extreme estimates (above threshold)
                    if (abs(row_data$treatment_effect) > EXTREME_ESTIMATE_THRESHOLD) {
                        # Still record diagnostics for skipped rows
                        diagnostics_rows[[length(diagnostics_rows)+1]] <- data.frame(
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
                            reason = sprintf("Estimate (%.2f) exceeds threshold of %.0f", row_data$treatment_effect, EXTREME_ESTIMATE_THRESHOLD),
                            stringsAsFactors = FALSE
                        )
                        next
                    }

                    # This row will be plotted - get events from subgroup effects data
                    events_plaque <- if ("events_plaque" %in% names(row_data)) row_data$events_plaque else NA
                    events_gksrs <- if ("events_gksrs" %in% names(row_data)) row_data$events_gksrs else NA
                    
                    # Record valid subgroup level
                    diagnostics_rows[[length(diagnostics_rows)+1]] <- data.frame(
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
                        stringsAsFactors = FALSE
                    )
                    
                    # Add this row to the plot
                    subgroup_row <- data.frame(
                        Subgroup = sprintf("  %s", row_data$subgroup_level),  # Indented subgroup levels
                        GKSRS_n = format_sample_size(row_data$n_gksrs, row_data$n_total),
                        Plaque_n = format_sample_size(row_data$n_plaque, row_data$n_total),
                        stringsAsFactors = FALSE
                    )
                    
                    # Add blank column for CI, subgroup p-value, and interaction p-value
                    subgroup_row$` ` <- paste(rep(" ", 20), collapse = " ")
                    subgroup_row$`HR (95% CI)` <- sprintf("%.2f (%.2f, %.2f)", 
                                                         row_data$treatment_effect,
                                                         row_data$ci_lower,
                                                         row_data$ci_upper)
                    subgroup_row$`p-value` <- forest_format_p_value(row_data$p_value)
                    subgroup_row$`Interaction p` <- ""
                    
                    all_rows[[length(all_rows) + 1]] <- subgroup_row
                    est_values <- c(est_values, row_data$treatment_effect)
                    lower_values <- c(lower_values, row_data$ci_lower)
                    upper_values <- c(upper_values, row_data$ci_upper)
                    is_summary <- c(is_summary, FALSE)
                    font_face <- c(font_face, "plain")
                    text_size <- c(text_size, 0.9)
                }
            } else {
                # No data available
                no_data_row <- data.frame(
                    Subgroup = "  No data available",
                    GKSRS_n = "",
                    Plaque_n = "",
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
        } else {
            # Variable missing from results
            no_data_row <- data.frame(
                Subgroup = "  No data available",
                GKSRS_n = "",
                Plaque_n = "",
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
            `GKSRS n/N` = character(),
            `Plaque n/N` = character(),
            ` ` = character(),
            `HR (95% CI)` = character(),
            `p-value` = character(),
            `Int p` = character(),
            stringsAsFactors = FALSE
        )
        # Reset vectors to empty
        est_values <- numeric()
        lower_values <- numeric()
        upper_values <- numeric()
        is_summary <- logical()
        font_face <- character()
        text_size <- numeric()
    }
    
    # Set proper column names that will become the forestploter headers
    colnames(final_df) <- c(
        "Subgroup",
        sprintf("%s n/N", treatment_labels[1]),
        sprintf("%s n/N", treatment_labels[2]),
        " ",  # Blank column for CI
        sprintf("%s (95%% CI)", effect_measure),
        "p-value",
        "Int p"
    )
    
    # If using a ratio measure (HR, OR, RR), ensure positive values; otherwise keep as is.
    ratio_measures <- c("HR", "OR", "RR")
    if (toupper(effect_measure) %in% ratio_measures) {
        for (i in seq_along(est_values)) {
            if (!is.na(est_values[i]) && est_values[i] <= 0) {
                est_values[i] <- NaN; lower_values[i] <- NaN; upper_values[i] <- NaN; is_summary[i] <- TRUE
            }
            if (!is.na(lower_values[i]) && lower_values[i] <= 0) {
                est_values[i] <- NaN; lower_values[i] <- NaN; upper_values[i] <- NaN; is_summary[i] <- TRUE
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
    # After collecting all rows, enforce column order and names for all rows
    if (length(combined_diagnostics) > 0) {
        col_order <- c("variable", "subgroup_level", "n_total", "n_plaque", "n_gksrs", "events_plaque", "events_gksrs", "treatment_effect", "ci_lower", "ci_upper", "p_value", "status", "reason")
        combined_diagnostics <- lapply(combined_diagnostics, function(df) {
            # Add any missing columns as NA
            for (col in setdiff(col_order, names(df))) df[[col]] <- NA
            # Reorder columns
            df <- df[, col_order, drop=FALSE]
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
    
    # Add "Other" category information to diagnostics if available
    if (!is.null(other_map) && length(other_map) > 0) {
        other_info_rows <- list()
        for (var_name in names(other_map)) {
            if (!is.null(other_map[[var_name]]) && length(other_map[[var_name]]) > 0) {
                other_row <- data.frame(
                    variable = var_name,
                    subgroup_level = "Other",
                    n_total = NA,
                    n_plaque = NA,
                    n_gksrs = NA,
                    events_plaque = NA,
                    events_gksrs = NA,
                    treatment_effect = NA,
                    ci_lower = NA,
                    ci_upper = NA,
                    p_value = NA,
                    status = "OTHER_CATEGORY",
                    reason = sprintf("Categories collapsed into 'Other': %s", paste(other_map[[var_name]], collapse = ", ")),
                    stringsAsFactors = FALSE
                )
                other_info_rows[[length(other_info_rows) + 1]] <- other_row
            }
        }
        
        # Add other category information to diagnostics
        if (length(other_info_rows) > 0) {
            # Filter out NULL or invalid elements
            valid_other_rows <- other_info_rows[sapply(other_info_rows, function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0)]
            if (length(valid_other_rows) > 0) {
                other_df <- do.call(rbind, valid_other_rows)
                diagnostics_df <- rbind(diagnostics_df, other_df)
            }
        }
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
    name_mapping <- list(
        "age_at_diagnosis" = "Age at Diagnosis",
        "sex" = "Sex",
        "location" = "Location",
        "initial_overall_stage" = "Initial Overall Stage",
        "initial_t_stage" = "Initial T Stage",
        "initial_tumor_height" = "Initial Tumor Height",
        "initial_tumor_diameter" = "Initial Tumor Diameter",
        "biopsy1_gep" = "GEP Class",
        "optic_nerve" = "Optic Nerve"
    )
    
    if (var_name %in% names(name_mapping)) {
        return(name_mapping[[var_name]])
    } else {
        return(tools::toTitleCase(gsub("_", " ", var_name)))
    }
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

#' Determine whether a numeric value is invalid for diagnostics
#' Accepts numeric or character "Inf" entries and flags them as invalid
#' @param x numeric or character
#' @return TRUE if NA, non-finite, or string "Inf"
diagnostics_invalid_numeric <- function(x) {
  is.na(x) || !is.finite(x) || (is.character(x) && x == "Inf")
}
