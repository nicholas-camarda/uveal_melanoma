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
#' @return A forestploter object
create_forest_plot <- function(subgroup_results, 
                               outcome_name,
                               effect_measure = "HR",
                               dataset_name = "Dataset",
                               output_path = NULL) {
    
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
            clip = c(0.1, 10)
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
#' @return A forestploter object
create_single_cohort_forest_plot <- function(subgroup_results, 
                                            outcome_name,
                                            cohort_name = "Cohort",
                                            treatment_labels = TREATMENT_LABELS,
                                            variable_order,  # Now required for consistency
                                            effect_measure = "HR",
                                            favours_labels = NULL,
                                            clip = NULL,
                                            title = NULL) {
    
    # Check that variable_order is provided
    if (missing(variable_order) || is.null(variable_order)) {
        stop("variable_order must be provided to ensure consistency across cohorts")
    }
    
    # Set default favours labels if not provided
    if (is.null(favours_labels)) {
        favours_labels <- paste0("Favours ", treatment_labels)
    }
    
    # Create the formatted data for forestploter
    plot_data <- create_forest_plot_data(subgroup_results, variable_order, treatment_labels, effect_measure)
    
    # Set default title
    if (is.null(title)) {
        title <- sprintf("Subgroup Analysis: %s", outcome_name)
    }
    
    # Set scale parameters based on effect measure
    use_log_scale <- effect_measure %in% c("HR", "OR", "RR")
    
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
    
    # Set default clipping if not provided
    if (is.null(clip)) {
        clip <- if (use_log_scale) c(0.1, 10) else c(-5, 5)
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
                hjust = 0.5
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
        x_trans = if (use_log_scale) "log" else "none",
        theme = tm,
        title = title
    )
    
    return(fp)
}

#' Create a combined forest plot for two cohorts
#'
#' @param full_results List of subgroup analysis results for full cohort
#' @param restricted_results List of subgroup analysis results for restricted cohort
#' @param outcome_name Character string for the outcome name
#' @param treatment_labels Character vector of length 2 with treatment labels
#' @param variable_order Character vector specifying the order of variables to display (REQUIRED for consistency)
#' @param effect_measure Character string for the effect measure (default: "HR")
#' @param favours_labels Character vector of length 2 for axis labels
#' @param clip Numeric vector of length 2 for clipping range (default: c(0.1, 10))
#' @param title Character string for plot title (optional)
#' @return A combined forestploter object
create_combined_forest_plot <- function(full_results, 
                                       restricted_results,
                                       outcome_name,
                                       treatment_labels = TREATMENT_LABELS,
                                       variable_order,  # Now required for consistency
                                       effect_measure = "HR",
                                       favours_labels = NULL,
                                       clip = NULL,
                                       title = NULL) {
    
    # Check that variable_order is provided
    if (missing(variable_order) || is.null(variable_order)) {
        stop("variable_order must be provided to ensure consistency across cohorts")
    }
    
    # Set default favours labels if not provided
    if (is.null(favours_labels)) {
        favours_labels <- paste0("Favours ", treatment_labels)
    }
    
    # Create data for both cohorts using the same variable order
    plot_data <- create_combined_forest_plot_data(full_results, restricted_results, 
                                                 variable_order, treatment_labels, effect_measure)
    
    # Set default title
    if (is.null(title)) {
        title <- sprintf("Subgroup Analysis: %s", outcome_name)
    }
    
    # Set scale parameters based on effect measure
    use_log_scale <- effect_measure %in% c("HR", "OR", "RR")
    
    # Check for problematic values (≤ 0) when using log scale
    if (use_log_scale) {
        problematic_values <- any(
            !is.na(plot_data$est_values_full) & plot_data$est_values_full <= 0 |
            !is.na(plot_data$lower_values_full) & plot_data$lower_values_full <= 0 |
            !is.na(plot_data$upper_values_full) & plot_data$upper_values_full <= 0 |
            !is.na(plot_data$est_values_restricted) & plot_data$est_values_restricted <= 0 |
            !is.na(plot_data$lower_values_restricted) & plot_data$lower_values_restricted <= 0 |
            !is.na(plot_data$upper_values_restricted) & plot_data$upper_values_restricted <= 0
        )
        
        if (problematic_values) {
            warning("Found values ≤ 0 in combined forest plot data. Switching to linear scale to avoid log transformation errors.")
            use_log_scale <- FALSE
        }
    }
    
    # Set default clipping if not provided
    if (is.null(clip)) {
        clip <- if (use_log_scale) c(0.1, 10) else c(-5, 5)
    }
    
    # Create theme for forestploter with multiple cohort colors
    tm <- forest_theme(
        base_size = 11,
        ci_pch = c(15, 18),  # Different shapes for full vs restricted
        ci_col = c("blue", "red"),
        ci_fill = c("blue", "red"),
        ci_alpha = 0.8,
        ci_lty = 1,
        ci_lwd = 1.5,
        refline_gp = gpar(lwd = 1, lty = "solid", col = "black"),
        vertline_lwd = 1,
        vertline_lty = "solid",
        vertline_col = "black",
        footnote_gp = gpar(cex = 0.8),
        legend_name = "Cohort",
        legend_value = c("Full Cohort", "Restricted Cohort"),
        # Set header rows to bold with improved formatting
        colhead = list(fg_params = list(fontface = "bold", cex = 1.1, hjust = 0.5, x = 0.5)),
        # Use dynamic row-specific formatting
        core = list(
            fg_params = list(
                fontface = plot_data$font_face,  # Dynamic font faces
                cex = plot_data$text_size        # Dynamic text sizes
            )
        )
    )
    
    # Find CI columns
    ci_columns <- c(2, 3)  # Full CI and Restricted CI blank columns
    
    # Create the forest plot with multiple CI columns
    fp <- forest(
        plot_data$data_frame,
        est = list(plot_data$est_values_full, plot_data$est_values_restricted),
        lower = list(plot_data$lower_values_full, plot_data$lower_values_restricted),
        upper = list(plot_data$upper_values_full, plot_data$upper_values_restricted),
        sizes = 0.25,
        is_summary = plot_data$is_summary,
        ci_column = ci_columns,
        ref_line = if (use_log_scale) 1 else 0,
        arrow_lab = favours_labels,
        xlim = clip,
        x_trans = if (use_log_scale) "log" else "none",
        theme = tm,
        title = title
    )
    
    return(fp)
}

#' Create formatted data for single cohort forest plot using forestploter format
#'
#' @param subgroup_results List of subgroup analysis results
#' @param variable_order Character vector of variables to include (enforced for consistency)
#' @param treatment_labels Character vector of treatment labels
#' @param effect_measure Character string for effect measure
#' @return List with formatted data for forestploter
create_forest_plot_data <- function(subgroup_results, variable_order, treatment_labels, effect_measure) {
    
    # Initialize data collection
    all_rows <- list()
    est_values <- c()
    lower_values <- c()
    upper_values <- c()
    is_summary <- c()
    font_face <- c()
    text_size <- c()
    
    # DO NOT create header row as data - forestploter creates headers from column names automatically
    
    # Process each variable in order
    for (var_name in variable_order) {
        
        # Variable header row 
        var_header <- data.frame(
            Subgroup = format_variable_name(var_name),
            GKSRS_n = "",
            Plaque_n = "",
            stringsAsFactors = FALSE
        )
        
        # Add blank column for CI and p-value column
        var_header$` ` <- paste(rep(" ", 20), collapse = " ")
        var_header$`HR (95% CI)` <- ""
        var_header$`p-value` <- ""
        
        all_rows[[length(all_rows) + 1]] <- var_header
        est_values <- c(est_values, NA)
        lower_values <- c(lower_values, NA)
        upper_values <- c(upper_values, NA)
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
                    
                    subgroup_row <- data.frame(
                        Subgroup = sprintf("  %s", row_data$subgroup_level),  # Indented subgroup levels
                        GKSRS_n = format_sample_size(row_data$n_gksrs, row_data$n_total),
                        Plaque_n = format_sample_size(row_data$n_plaque, row_data$n_total),
                        stringsAsFactors = FALSE
                    )
                    
                    # Add blank column for CI and p-value
                    subgroup_row$` ` <- paste(rep(" ", 20), collapse = " ")
                    subgroup_row$`HR (95% CI)` <- sprintf("%.2f (%.2f-%.2f)", 
                                                         row_data$treatment_effect,
                                                         row_data$ci_lower,
                                                         row_data$ci_upper)
                    subgroup_row$`p-value` <- format_p_value(row_data$p_value)
                    
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
                
                # Add blank column for CI and p-value
                no_data_row$` ` <- paste(rep(" ", 20), collapse = " ")
                no_data_row$`HR (95% CI)` <- ""
                no_data_row$`p-value` <- ""
                
                all_rows[[length(all_rows) + 1]] <- no_data_row
                est_values <- c(est_values, NA)
                lower_values <- c(lower_values, NA)
                upper_values <- c(upper_values, NA)
                is_summary <- c(is_summary, FALSE)
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
            
            # Add blank column for CI and p-value
            no_data_row$` ` <- paste(rep(" ", 20), collapse = " ")
            no_data_row$`HR (95% CI)` <- ""
            no_data_row$`p-value` <- ""
            
            all_rows[[length(all_rows) + 1]] <- no_data_row
            est_values <- c(est_values, NA)
            lower_values <- c(lower_values, NA)
            upper_values <- c(upper_values, NA)
            is_summary <- c(is_summary, FALSE)
            font_face <- c(font_face, "italic")
            text_size <- c(text_size, 0.8)
        }
    }
    
    # Combine all rows into a data frame
    final_df <- do.call(rbind, all_rows)
    
    # Set proper column names that will become the forestploter headers
    colnames(final_df) <- c(
        "Subgroup",
        sprintf("%s n/N", treatment_labels[1]),
        sprintf("%s n/N", treatment_labels[2]),
        " ",  # Blank column for CI
        sprintf("%s (95%% CI)", effect_measure),
        "p-value"
    )
    
    return(list(
        data_frame = final_df,
        est_values = est_values,
        lower_values = lower_values,
        upper_values = upper_values,
        is_summary = is_summary,
        font_face = font_face,
        text_size = text_size
    ))
}

#' Create formatted data for combined cohort forest plot using forestploter format
#'
#' @param full_results List of subgroup analysis results for full cohort
#' @param restricted_results List of subgroup analysis results for restricted cohort
#' @param variable_order Character vector of variables to include (enforced for consistency)
#' @param treatment_labels Character vector of treatment labels
#' @param effect_measure Character string for effect measure
#' @return List with formatted data for forestploter
create_combined_forest_plot_data <- function(full_results, restricted_results, variable_order, treatment_labels, effect_measure) {
    
    # Initialize data collection
    all_rows <- list()
    est_values_full <- c()
    lower_values_full <- c()
    upper_values_full <- c()
    est_values_restricted <- c()
    lower_values_restricted <- c()
    upper_values_restricted <- c()
    is_summary <- c()
    font_face <- c()
    text_size <- c()
    
    # Note: Headers are set via column names, not as data rows in forestploter
    
    # Process each variable in order
    for (var_name in variable_order) {
        
        # Variable header row with bold formatting
        var_header <- data.frame(
            Subgroup = format_variable_name(var_name),  # Clean variable names (will be made bold via theme)
            Full_CI = "",
            Restricted_CI = "",
            Full_n = "",
            Restricted_n = "",
            p_value = "",
            stringsAsFactors = FALSE
        )
        
        all_rows[[length(all_rows) + 1]] <- var_header
        est_values_full <- c(est_values_full, NA)
        lower_values_full <- c(lower_values_full, NA)
        upper_values_full <- c(upper_values_full, NA)
        est_values_restricted <- c(est_values_restricted, NA)
        lower_values_restricted <- c(lower_values_restricted, NA)
        upper_values_restricted <- c(upper_values_restricted, NA)
        is_summary <- c(is_summary, TRUE)
        font_face <- c(font_face, "bold")
        text_size <- c(text_size, 1.0)
        
        # Check if data exists for this variable in either cohort
        full_var <- if (var_name %in% names(full_results)) full_results[[var_name]] else NULL
        restricted_var <- if (var_name %in% names(restricted_results)) restricted_results[[var_name]] else NULL
        
        if (!is.null(full_var) || !is.null(restricted_var)) {
            # Align subgroup levels between cohorts
            aligned_data <- data.frame(subgroup_level = character(0), stringsAsFactors = FALSE)
            
            if (!is.null(full_var) && !is.null(restricted_var)) {
                aligned_data <- align_subgroup_levels(full_var$subgroup_effects, restricted_var$subgroup_effects)
            } else if (!is.null(full_var)) {
                aligned_data <- full_var$subgroup_effects
                # Add empty columns for restricted data
                for (col in c("treatment_effect", "ci_lower", "ci_upper", "p_value", "n_gksrs", "n_plaque", "n_total")) {
                    aligned_data[[paste0("restricted_", col)]] <- NA
                    if (!paste0("full_", col) %in% names(aligned_data)) {
                        aligned_data[[paste0("full_", col)]] <- aligned_data[[col]]
                    }
                }
            } else {
                aligned_data <- restricted_var$subgroup_effects
                # Add empty columns for full data
                for (col in c("treatment_effect", "ci_lower", "ci_upper", "p_value", "n_gksrs", "n_plaque", "n_total")) {
                    aligned_data[[paste0("full_", col)]] <- NA
                    if (!paste0("restricted_", col) %in% names(aligned_data)) {
                        aligned_data[[paste0("restricted_", col)]] <- aligned_data[[col]]
                    }
                }
            }
            
            # Add subgroup rows
            for (i in 1:nrow(aligned_data)) {
                row_data <- aligned_data[i, ]
                
                subgroup_row <- data.frame(
                    Subgroup = sprintf("  %s", row_data$subgroup_level),  # Indented subgroup levels
                    Full_CI = "",
                    Restricted_CI = "",
                    Full_n = if (!is.na(row_data$full_n_gksrs)) sprintf("%d/%d", row_data$full_n_gksrs, row_data$full_n_plaque) else "No data",
                    Restricted_n = if (!is.na(row_data$restricted_n_gksrs)) sprintf("%d/%d", row_data$restricted_n_gksrs, row_data$restricted_n_plaque) else "No data",
                    p_value = if (!is.na(row_data$full_p_value)) format_p_value(row_data$full_p_value) else if (!is.na(row_data$restricted_p_value)) format_p_value(row_data$restricted_p_value) else "",
                    stringsAsFactors = FALSE
                )
                
                all_rows[[length(all_rows) + 1]] <- subgroup_row
                
                # Full cohort values
                est_values_full <- c(est_values_full, if (!is.na(row_data$full_treatment_effect)) row_data$full_treatment_effect else NA)
                lower_values_full <- c(lower_values_full, if (!is.na(row_data$full_ci_lower)) row_data$full_ci_lower else NA)
                upper_values_full <- c(upper_values_full, if (!is.na(row_data$full_ci_upper)) row_data$full_ci_upper else NA)
                
                # Restricted cohort values
                est_values_restricted <- c(est_values_restricted, if (!is.na(row_data$restricted_treatment_effect)) row_data$restricted_treatment_effect else NA)
                lower_values_restricted <- c(lower_values_restricted, if (!is.na(row_data$restricted_ci_lower)) row_data$restricted_ci_lower else NA)
                upper_values_restricted <- c(upper_values_restricted, if (!is.na(row_data$restricted_ci_upper)) row_data$restricted_ci_upper else NA)
                
                is_summary <- c(is_summary, FALSE)
                font_face <- c(font_face, "plain")  # Plain font for subgroup levels
                text_size <- c(text_size, 0.9)
            }
        } else {
            # No data available
            no_data_row <- data.frame(
                Subgroup = "  No data available",  # Clean text for no data
                Full_CI = "",
                Restricted_CI = "",
                Full_n = "",
                Restricted_n = "",
                p_value = "",
                stringsAsFactors = FALSE
            )
            
            all_rows[[length(all_rows) + 1]] <- no_data_row
            est_values_full <- c(est_values_full, NA)
            lower_values_full <- c(lower_values_full, NA)
            upper_values_full <- c(upper_values_full, NA)
            est_values_restricted <- c(est_values_restricted, NA)
            lower_values_restricted <- c(lower_values_restricted, NA)
            upper_values_restricted <- c(upper_values_restricted, NA)
            is_summary <- c(is_summary, FALSE)
            font_face <- c(font_face, "italic")
            text_size <- c(text_size, 0.9)
        }
    }
    
    # Combine all rows into a data frame
    final_df <- do.call(rbind, all_rows)
    
    # Set proper column names for forestploter headers
    colnames(final_df) <- c(
        "Subgroup",
        " ",  # Full CI blank column
        " ",  # Restricted CI blank column  
        sprintf("Full Cohort (%s/%s)", treatment_labels[1], treatment_labels[2]),
        sprintf("Restricted Cohort (%s/%s)", treatment_labels[1], treatment_labels[2]),
        "p-value"
    )
    
    return(list(
        data_frame = final_df,
        est_values_full = est_values_full,
        lower_values_full = lower_values_full,
        upper_values_full = upper_values_full,
        est_values_restricted = est_values_restricted,
        lower_values_restricted = lower_values_restricted,
        upper_values_restricted = upper_values_restricted,
        is_summary = is_summary,
        font_face = font_face,
        text_size = text_size
    ))
}

#' Align subgroup levels between two cohorts
#'
#' @param full_effects Data frame of subgroup effects for full cohort
#' @param restricted_effects Data frame of subgroup effects for restricted cohort
#' @return Data frame with aligned subgroup data
align_subgroup_levels <- function(full_effects, restricted_effects) {
    
    # Get all unique subgroup levels
    all_levels <- union(full_effects$subgroup_level, restricted_effects$subgroup_level)
    
    # Create aligned data frame with all necessary columns
    aligned <- data.frame(
        subgroup_level = all_levels,
        stringsAsFactors = FALSE
    )
    
    # Add full cohort data
    full_cols <- c("treatment_effect", "ci_lower", "ci_upper", "p_value", "n_gksrs", "n_plaque", "n_total")
    for (col in full_cols) {
        aligned[[paste0("full_", col)]] <- NA
    }
    
    # Add restricted cohort data
    for (col in full_cols) {
        aligned[[paste0("restricted_", col)]] <- NA
    }
    
    # Fill in full cohort data
    for (i in 1:nrow(aligned)) {
        level <- aligned$subgroup_level[i]
        if (level %in% full_effects$subgroup_level) {
            full_row <- full_effects[full_effects$subgroup_level == level, ]
            if (nrow(full_row) > 0) {
                for (col in full_cols) {
                    if (col %in% names(full_row)) {
                        aligned[[paste0("full_", col)]][i] <- full_row[[col]][1]
                    }
                }
            }
        }
    }
    
    # Fill in restricted cohort data
    for (i in 1:nrow(aligned)) {
        level <- aligned$subgroup_level[i]
        if (level %in% restricted_effects$subgroup_level) {
            restricted_row <- restricted_effects[restricted_effects$subgroup_level == level, ]
            if (nrow(restricted_row) > 0) {
                for (col in full_cols) {
                    if (col %in% names(restricted_row)) {
                        aligned[[paste0("restricted_", col)]][i] <- restricted_row[[col]][1]
                    }
                }
            }
        }
    }
    
    return(aligned)
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