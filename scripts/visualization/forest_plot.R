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
            treatment_labels = c("GKSRS", "Plaque"),
            variable_order = variable_order,
            effect_measure = effect_measure,
            favours_labels = c("Favours GKSRS", "Favours Plaque"),
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
                                            treatment_labels = c("GKSRS", "Plaque"),
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
    
    # Set default clipping if not provided
    if (is.null(clip)) {
        clip <- if (use_log_scale) c(0.1, 10) else c(-5, 5)
    }
    
    # Create theme for forestploter
    tm <- forest_theme(
        base_size = 10,
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
        # Set header rows to bold
        colhead = list(fg_params = list(fontface = "bold")),
        # Control text formatting per row - use manual bold/plain setting
        core = list(
            fg_params = list(fontface = rep(c("bold", "plain"), length.out = nrow(plot_data$data_frame)))
        )
    )
    
    # Find the CI column (the one with spaces)
    ci_col_idx <- which(sapply(plot_data$data_frame, function(x) any(grepl("^\\s+$", x))))
    if (length(ci_col_idx) == 0) {
        ci_col_idx <- ncol(plot_data$data_frame) - 1  # Default to second-to-last column
    }
    
    # Create the forest plot
    fp <- forest(
        plot_data$data_frame,
        est = plot_data$est_values,
        lower = plot_data$lower_values,
        upper = plot_data$upper_values,
        sizes = 0.3,
        is_summary = plot_data$is_summary,
        ci_column = ci_col_idx,
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
                                       treatment_labels = c("GKSRS", "Plaque"),
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
    
    # Set default clipping if not provided
    if (is.null(clip)) {
        clip <- if (use_log_scale) c(0.1, 10) else c(-5, 5)
    }
    
    # Create theme for forestploter with multiple cohort colors
    tm <- forest_theme(
        base_size = 10,
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
        # Set header rows to bold
        colhead = list(fg_params = list(fontface = "bold")),
        # Control text formatting per row
        core = list(
            fg_params = list(fontface = rep(c("bold", "plain"), length.out = nrow(plot_data$data_frame)))
        )
    )
    
    # Find CI columns
    ci_columns <- which(names(plot_data$data_frame) %in% c("Full_CI", "Restricted_CI"))
    if (length(ci_columns) < 2) {
        # Fallback: find columns with spaces
        ci_columns <- which(sapply(plot_data$data_frame, function(x) any(grepl("^\\s+$", x))))
    }
    
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
    
    # Add header row
    header_row <- data.frame(
        Subgroup = "Subgroup",
        GKSRS_n = paste(treatment_labels[1], "n/N"),
        Plaque_n = paste(treatment_labels[2], "n/N"),
        CI_space = paste(rep(" ", 20), collapse = " "),  # Blank column for CI
        Effect_CI = sprintf("%s (95%% CI)", effect_measure),
        p_value = "p value",
        stringsAsFactors = FALSE
    )
    
    all_rows[[1]] <- header_row
    est_values <- c(est_values, NA)
    lower_values <- c(lower_values, NA)
    upper_values <- c(upper_values, NA)
    is_summary <- c(is_summary, TRUE)
    
    # Process each variable in order
    for (var_name in variable_order) {
        
        # Variable header row
        var_header <- data.frame(
            Subgroup = format_variable_name(var_name),
            GKSRS_n = "",
            Plaque_n = "",
            CI_space = "",
            Effect_CI = "",
            p_value = "",
            stringsAsFactors = FALSE
        )
        
        all_rows[[length(all_rows) + 1]] <- var_header
        est_values <- c(est_values, NA)
        lower_values <- c(lower_values, NA)
        upper_values <- c(upper_values, NA)
        is_summary <- c(is_summary, TRUE)
        
        # Check if data exists for this variable
        if (var_name %in% names(subgroup_results)) {
            var_data <- subgroup_results[[var_name]]
            
            if (!is.null(var_data$subgroup_effects) && nrow(var_data$subgroup_effects) > 0) {
                # Add subgroup rows
                effects_data <- var_data$subgroup_effects
                for (i in 1:nrow(effects_data)) {
                    row_data <- effects_data[i, ]
                    
                    subgroup_row <- data.frame(
                        Subgroup = paste0("  ", row_data$subgroup_level),
                        GKSRS_n = format_sample_size(row_data$n_gksrs, row_data$n_total),
                        Plaque_n = format_sample_size(row_data$n_plaque, row_data$n_total),
                        CI_space = "",
                        Effect_CI = sprintf("%.2f (%.2f-%.2f)", 
                                          row_data$treatment_effect,
                                          row_data$ci_lower,
                                          row_data$ci_upper),
                        p_value = format_p_value(row_data$p_value),
                        stringsAsFactors = FALSE
                    )
                    
                    all_rows[[length(all_rows) + 1]] <- subgroup_row
                    est_values <- c(est_values, row_data$treatment_effect)
                    lower_values <- c(lower_values, row_data$ci_lower)
                    upper_values <- c(upper_values, row_data$ci_upper)
                    is_summary <- c(is_summary, FALSE)
                }
            } else {
                # No data available
                no_data_row <- data.frame(
                    Subgroup = "  No data available",
                    GKSRS_n = "",
                    Plaque_n = "",
                    CI_space = "",
                    Effect_CI = "",
                    p_value = "",
                    stringsAsFactors = FALSE
                )
                
                all_rows[[length(all_rows) + 1]] <- no_data_row
                est_values <- c(est_values, NA)
                lower_values <- c(lower_values, NA)
                upper_values <- c(upper_values, NA)
                is_summary <- c(is_summary, FALSE)
            }
        } else {
            # Variable missing from results
            no_data_row <- data.frame(
                Subgroup = "  No data available",
                GKSRS_n = "",
                Plaque_n = "",
                CI_space = "",
                Effect_CI = "",
                p_value = "",
                stringsAsFactors = FALSE
            )
            
            all_rows[[length(all_rows) + 1]] <- no_data_row
            est_values <- c(est_values, NA)
            lower_values <- c(lower_values, NA)
            upper_values <- c(upper_values, NA)
            is_summary <- c(is_summary, FALSE)
        }
    }
    
    # Combine all rows into a data frame
    final_df <- do.call(rbind, all_rows)
    
    return(list(
        data_frame = final_df,
        est_values = est_values,
        lower_values = lower_values,
        upper_values = upper_values,
        is_summary = is_summary
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
    
    # Add header row
    header_row <- data.frame(
        Subgroup = "Subgroup",
        Full_CI = paste(rep(" ", 20), collapse = " "),
        Restricted_CI = paste(rep(" ", 20), collapse = " "),
        Full_n = "Full n_GKSRS/n_Plaque",
        Restricted_n = "Restricted n_GKSRS/n_Plaque",
        p_value = "p value",
        stringsAsFactors = FALSE
    )
    
    all_rows[[1]] <- header_row
    est_values_full <- c(est_values_full, NA)
    lower_values_full <- c(lower_values_full, NA)
    upper_values_full <- c(upper_values_full, NA)
    est_values_restricted <- c(est_values_restricted, NA)
    lower_values_restricted <- c(lower_values_restricted, NA)
    upper_values_restricted <- c(upper_values_restricted, NA)
    is_summary <- c(is_summary, TRUE)
    
    # Process each variable in order
    for (var_name in variable_order) {
        
        # Variable header row
        var_header <- data.frame(
            Subgroup = format_variable_name(var_name),
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
        
        # Get data for both cohorts
        full_var <- if (var_name %in% names(full_results)) full_results[[var_name]] else NULL
        restricted_var <- if (var_name %in% names(restricted_results)) restricted_results[[var_name]] else NULL
        
        has_full_data <- !is.null(full_var) && !is.null(full_var$subgroup_effects) && nrow(full_var$subgroup_effects) > 0
        has_restricted_data <- !is.null(restricted_var) && !is.null(restricted_var$subgroup_effects) && nrow(restricted_var$subgroup_effects) > 0
        
        if (has_full_data || has_restricted_data) {
            # Get aligned subgroup data
            if (has_full_data && has_restricted_data) {
                aligned_data <- align_subgroup_levels(full_var$subgroup_effects, restricted_var$subgroup_effects)
            } else if (has_full_data) {
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
                    Subgroup = paste0("  ", row_data$subgroup_level),
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
            }
        } else {
            # No data available
            no_data_row <- data.frame(
                Subgroup = "  No data available",
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
        }
    }
    
    # Combine all rows into a data frame
    final_df <- do.call(rbind, all_rows)
    
    return(list(
        data_frame = final_df,
        est_values_full = est_values_full,
        lower_values_full = lower_values_full,
        upper_values_full = upper_values_full,
        est_values_restricted = est_values_restricted,
        lower_values_restricted = lower_values_restricted,
        upper_values_restricted = upper_values_restricted,
        is_summary = is_summary
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
