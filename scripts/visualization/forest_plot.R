#' Create publication-style forest plots for subgroup analysis results
#' 
#' This module creates forest plots that match the format shown in published papers,
#' with grouped variables, indented subgroups, and proper formatting for publication.

# Libraries loaded in main.R

#' Create forest plot wrapper function (called by main.R)
#'
#' This is a wrapper function that main.R calls. It creates a single cohort forest plot
#' using the subgroup results and determines the variable order automatically.
#'
#' @param subgroup_results List of subgroup analysis results
#' @param outcome_name Character string for the outcome name
#' @param effect_measure Character string for the effect measure (default: "HR") 
#' @param dataset_name Character string for the dataset name
#' @param output_path Character string for output file path (optional)
#' @param other_map List mapping variable names to "Other" category contents (optional)
#' @return A forestploter object
create_forest_plot <- function(subgroup_results, 
                               outcome_name,
                               effect_measure = "HR",
                               dataset_name = "Dataset",
                               output_path = NULL,
                               other_map = NULL) {
    
    # Handle empty or NULL results
    if (is.null(subgroup_results) || length(subgroup_results) == 0) {
        warning("No subgroup results provided for forest plot")
        return(NULL)
    }
    
    # Create variable order from available results
    variable_order <- names(subgroup_results)
    if (length(variable_order) == 0) {
        warning("No valid subgroup variables found")
        return(NULL)
    }
    
    # Create the forest plot using the single cohort function
    tryCatch({
        plot <- create_single_cohort_forest_plot(
            subgroup_results = subgroup_results,
            outcome_name = outcome_name,
            cohort_name = dataset_name,
            treatment_labels = TREATMENT_LABELS,
            variable_order = variable_order,
            effect_measure = effect_measure,
            favours_labels = FAVOURS_LABELS,
            clip = NULL,
            other_map = other_map
        )
        
        # Save to file if output_path is provided
        if (!is.null(output_path)) {
            # Create directory if it doesn't exist
            dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
            
            # Save as PNG
            png(output_path, width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)
            plot(plot)
            dev.off()
        }
        
        return(plot)
        
    }, error = function(e) {
        warning(sprintf("Failed to create forest plot for %s: %s", outcome_name, e$message))
        return(NULL)
    })
}

#' Create a forest plot for a single cohort's subgroup analysis results
#'
#' @param subgroup_results List of subgroup analysis results from analyze_treatment_effect_subgroups_*
#' @param outcome_name Character string for the outcome name (e.g., "Overall Survival")
#' @param cohort_name Character string for the cohort name (e.g., "Full Cohort")
#' @param treatment_labels Character vector of length 2 with treatment labels (e.g., c("GKSRS", "Plaque"))
#' @param variable_order Character vector specifying the order of variables to display (REQUIRED for consistency)
#' @param effect_measure Character string for the effect measure (default: "HR")
#' @param favours_labels Character vector of length 2 for axis labels (e.g., c("Favours GKSRS", "Favours Plaque"))
#' @param clip Numeric vector of length 2 for clipping range (default: c(0.1, 10))
#' @param title Character string for plot title (optional)
#' @param other_map List mapping variable names to "Other" category contents (optional)
#' @return A forestploter object
create_single_cohort_forest_plot <- function(subgroup_results, 
                                            outcome_name,
                                            cohort_name = "Cohort",
                                            treatment_labels = TREATMENT_LABELS,
                                            variable_order,  # Now required for consistency
                                            effect_measure = "HR",
                                            favours_labels = NULL,
                                            clip = NULL,
                                            title = NULL,
                                            other_map = NULL) {
    
    # Check that variable_order is provided
    if (missing(variable_order) || is.null(variable_order)) {
        stop("variable_order must be provided to ensure consistency across cohorts")
    }
    
    # Set default favours labels if not provided
    if (is.null(favours_labels)) {
        favours_labels <- paste0("Favours ", treatment_labels)
    }
    
    # Create the formatted data for forestploter
    plot_data <- create_forest_plot_data(subgroup_results, variable_order, treatment_labels, effect_measure, other_map)
    
    # Set default title
    if (is.null(title)) {
        title <- sprintf("Subgroup Analysis: %s", outcome_name)
    }
    
    # Set scale parameters: data-driven detection of ratio vs difference measures
    # If all estimates and CI bounds are positive, assume this is a ratio measure (HR/OR/RR)
    all_values <- c(plot_data$est_values, plot_data$lower_values, plot_data$upper_values)
    all_values <- all_values[!is.na(all_values)]
    use_log_scale <- length(all_values) > 0 && all(all_values > 0)
    
    # Check for problematic values (≤ 0) when using log scale
    if (use_log_scale) {
        problematic_values <- any(
            !is.na(plot_data$est_values) & plot_data$est_values <= 0 |
            !is.na(plot_data$lower_values) & plot_data$lower_values <= 0 |
            !is.na(plot_data$upper_values) & plot_data$upper_values <= 0
        )
        
        if (problematic_values) {
            warning("Found values ≤ 0 in forest plot data. Switching to linear scale to avoid log transformation errors.")
            use_log_scale <- FALSE
        }
    }
    
    # Dynamic clipping: ensure reference line (1 or 0) is centered visually
    if (is.null(clip)) {
        if (use_log_scale) {
            clip <- symmetric_log_clip(plot_data$lower_values, plot_data$upper_values)
        } else {
            clip <- symmetric_linear_clip(plot_data$lower_values, plot_data$upper_values)
        }
    }
    
    # Calculate clean x-axis ticks
    if (use_log_scale) {
        # For log scale, use clean powers and half-powers of 10
        if (clip[2] <= 2) {
            xticks <- c(0.5, 1, 2)
        } else if (clip[2] <= 5) {
            xticks <- c(0.25, 0.5, 1, 2, 4)
        } else {
            xticks <- c(0.1, 0.5, 1, 2, 5, 10)
        }
        # Keep only ticks within clip range
        xticks <- xticks[xticks >= clip[1] & xticks <= clip[2]]
    } else {
        # For linear scale, use clean intervals
        span <- max(abs(clip))
        if (span <= 2) {
            xticks <- seq(-2, 2, by = 1)
        } else if (span <= 5) {
            xticks <- seq(-5, 5, by = 2.5)
        } else {
            xticks <- seq(-10, 10, by = 5)
        }
        # Keep only ticks within clip range
        xticks <- xticks[xticks >= clip[1] & xticks <= clip[2]]
    }
    
    # Create improved theme for forestploter with proper formatting following documentation
    tm <- forest_theme(
        base_size = 11,
        ci_pch = 15,
        ci_col = "black",
        ci_fill = "black",
        ci_alpha = 0.8,
        ci_lty = 1,
        ci_lwd = 1.5,
        refline_gp = gpar(lwd = 1, lty = "solid", col = "black"),
        vertline_lwd = 1,
        vertline_lty = "solid",
        vertline_col = "black",
        footnote_gp = gpar(cex = 0.8),
        # Header formatting - this controls the column headers
        colhead = list(
            fg_params = list(
                fontface = "bold",
                cex = 1.0,
                hjust = 0.5,
                x = 0.5
            )
        ),
        # Core content formatting with dynamic font face and size
        core = list(
            fg_params = list(
                fontface = plot_data$font_face,  # Dynamic font faces
                cex = plot_data$text_size        # Dynamic text sizes
            )
        )
    )
    
    # Optional footnote disabled by default to avoid clutter
    footnote_text <- NULL
    
    # Create the forest plot using correct forestploter syntax following documentation
    # CI column is position 4 (blank column after Subgroup, GKSRS_n, Plaque_n)
    fp <- forest(
        plot_data$data_frame,
        est = plot_data$est_values,
        lower = plot_data$lower_values,
        upper = plot_data$upper_values,
        sizes = 0.4,
        is_summary = plot_data$is_summary,
        ci_column = 4,  # Position of blank column
        ref_line = if (use_log_scale) 1 else 0,
        arrow_lab = favours_labels,
        xlim = clip,
        xticks = xticks,
        x_trans = if (use_log_scale) "log" else "none",
        theme = tm,
        title = title
    )
    
    # Attach diagnostics for external retrieval
    attr(fp, "diagnostics") <- plot_data$diagnostics
    
    return(fp)
}

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
            var_header$`Interaction p` <- format_p_value(subgroup_results[[var_name]]$interaction_p)
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
                    invalid_numeric <- function(x) { is.na(x) || !is.finite(x) }
                    if (invalid_numeric(row_data$treatment_effect) ||
                        invalid_numeric(row_data$ci_lower) ||
                        invalid_numeric(row_data$ci_upper)) {
                        
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
                    subgroup_row$`p-value` <- format_p_value(row_data$p_value)
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
format_p_value <- function(p_value) {
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

#' Apply post-processing formatting for better appearance
#'
#' @param fp A forestploter object
#' @param plot_data List with formatted data for forestploter
#' @return A formatted forestploter object
apply_forest_plot_formatting <- function(fp, plot_data) {
    
    # Find rows that should be bold (variable headers)
    bold_rows <- which(plot_data$is_summary & plot_data$font_face == "bold")
    
    # Skip the first row (main header) for variable-specific formatting
    variable_header_rows <- bold_rows[-1]
    
    # Apply bold formatting to variable headers
    for (row_idx in variable_header_rows) {
        fp <- edit_plot(fp, 
                       row = row_idx, 
                       col = 1,  # First column (subgroup names)
                       gp = gpar(fontface = "bold"))
    }
    
    # Apply italic formatting to "No data available" rows
    italic_rows <- which(plot_data$font_face == "italic")
    for (row_idx in italic_rows) {
        fp <- edit_plot(fp, 
                       row = row_idx, 
                       col = 1,  # First column (subgroup names)
                       gp = gpar(fontface = "italic", col = "grey50"))
    }
    
    return(fp)
}

#' Helper: compute symmetric clip range around 1 on log scale with
#' intelligent trimming so extreme outliers do not blow-out the axis.
#'
#' Logic:
#' 1. Keep only positive, finite limits.
#' 2. Work on base-10 logs centred at 0.
#' 3. Trim the outer `trim_pct` fraction of |log| values (default 5 % on each tail)
#'    so the axis is driven by the central 90 % of the data.
#' 4. Convert the resulting span back to the original scale, add a small buffer,
#'    and cap the span at `max_span_log` so axes never become absurdly wide.
#'
#' @param lower_vals Numeric vector of lower CI bounds.
#' @param upper_vals Numeric vector of upper CI bounds.
#' @param buffer Proportion (e.g. 0.1 = 10 %) added to each side after trimming.
#' @param trim_pct Proportion to trim from each tail when determining span.
#' @param max_span_log Maximum half-width (in log10 units) allowed for the axis.
#' @return Numeric length-2 vector giving c(min, max) clip values.
symmetric_log_clip <- function(lower_vals, upper_vals,
                               buffer = 0.15, trim_pct = 0.05,
                               max_span_log = 1.5) {
    # Combine and clean values
    vals <- c(lower_vals, upper_vals)
    vals <- vals[is.finite(vals) & vals > 0]
    if (length(vals) == 0) return(c(0.1, 10))

    # Work on absolute log10 distances from 1
    log_abs <- abs(log10(vals))
    if (length(log_abs) < 3) {
        span <- max(log_abs)
    } else {
        # Trim extreme tails symmetrically
        span <- stats::quantile(log_abs, probs = 1 - trim_pct, names = FALSE)
    }

    # Cap to prevent comically wide axes
    span <- min(span, max_span_log)

    # Add buffer but don't round aggressively
    span_buffered <- span * (1 + buffer)

    clip_min <- 10^(-span_buffered)
    clip_max <- 10^(span_buffered)

    # Safety fallback
    if (!is.finite(clip_min) || !is.finite(clip_max) || clip_min <= 0) {
        return(c(0.1, 10))
    }
    c(clip_min, clip_max)
}

#' Helper: compute symmetric clip range for linear scales (e.g., mean
#' differences) centred at 0. Extreme outliers are trimmed so they no longer
#' explode the axis.
#'
#' @param lower_vals Numeric vector of lower CI bounds.
#' @param upper_vals Numeric vector of upper CI bounds.
#' @param buffer Proportion (e.g. 0.1 = 10 %) added to each side after trimming.
#' @param trim_pct Proportion to trim from each tail when determining span.
#' @param max_span Maximum half-width allowed for the axis (absolute units).
#' @return Numeric length-2 vector giving c(min, max) clip values centred on 0.
symmetric_linear_clip <- function(lower_vals, upper_vals,
                                   buffer = 0.1, trim_pct = 0.05,
                                   max_span = 5) {
    vals <- c(lower_vals, upper_vals)
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) return(c(-1, 1))

    # Work with absolute magnitude (distance from 0)
    abs_vals <- abs(vals)

    # Robust span: use high quantile after trimming extremes
    if (length(abs_vals) < 3) {
        span <- max(abs_vals)
    } else {
        span <- stats::quantile(abs_vals, probs = 1 - trim_pct, names = FALSE)
    }

    # Cap span
    span <- min(span, max_span)

    # Apply buffer
    span <- span * (1 + buffer)

    if (!is.finite(span) || span <= 0) span <- 1

    c(-span, span)
}

#' Retrieve diagnostics from a forestploter object created by this script
get_forest_plot_diagnostics <- function(fp) {
    attr(fp, "diagnostics")
}

#' Write diagnostics list to an Excel workbook with one sheet per plot
#'
#' @param diagnostics_list Named list where each element is a data.frame of diagnostics
#' @param file_path Full path of the .xlsx to create
write_diagnostics_excel <- function(diagnostics_list, file_path) {
    if (length(diagnostics_list) == 0) return(invisible(NULL))
    writexl::write_xlsx(diagnostics_list, file_path)
}