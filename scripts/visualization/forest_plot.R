#' Create publication-style forest plots for subgroup analysis results
#' 
#' This module creates forest plots that match the format shown in published papers,
#' with grouped variables, indented subgroups, and proper formatting for publication.

library(forestplot)
library(dplyr)
library(grid)

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
#' @return A forestplot object
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
            print(plot)
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
#' @return A forestplot object
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
    
    # Create the formatted data for forestplot
    plot_data <- create_forest_plot_data(subgroup_results, variable_order, treatment_labels, effect_measure)
    
    # Set default title
    if (is.null(title)) {
        title <- sprintf("Subgroup Analysis: %s\nDataset: %s", outcome_name, cohort_name)
    }
    
    # Set scale parameters based on effect measure
    use_log_scale <- effect_measure %in% c("HR", "OR", "RR")
    zero_line <- if (use_log_scale) 1 else 0
    
    # Set default clipping if not provided
    if (is.null(clip)) {
        clip <- if (use_log_scale) c(0.1, 10) else c(-5, 5)
    }
    
    # Create the forest plot with proper bold formatting only for variable headers
    fp <- forestplot(
        labeltext = plot_data$labeltext,
        mean = plot_data$mean,
        lower = plot_data$lower,
        upper = plot_data$upper,
        is.summary = plot_data$is_summary,
        clip = clip,
        xlog = use_log_scale,
        zero = zero_line,
        boxsize = 0.3,
        lineheight = unit(8, "mm"),
        colgap = unit(2, "mm"),
        col = fpColors(
            box = "black",
            line = "black",
            summary = "black",
            hrz_lines = "black"
        ),
        txt_gp = fpTxtGp(
            label = gpar(fontface = plot_data$label_fontface, cex = 0.8),  # Use dynamic fontface
            ticks = gpar(cex = 0.8),
            xlab = gpar(cex = 0.9, fontface = "bold")
        ),
        graph.pos = 5,  # Position of the forest plot (after 5 text columns)
        hrzl_lines = plot_data$hrzl_lines,
        vertices = TRUE,
        ci.vertices = TRUE,
        ci.vertices.height = 0.1,
        title = title,
        xlab = sprintf("%s (95%% CI)", effect_measure),
        new_page = FALSE
    )
    
    # Add favours labels
    grid.text(favours_labels[1], x = 0.1, y = 0.02, 
              gp = gpar(cex = 0.8, fontface = "italic"))
    grid.text(favours_labels[2], x = 0.9, y = 0.02, 
              gp = gpar(cex = 0.8, fontface = "italic"))
    
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
#' @return A combined forestplot object
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
    zero_line <- if (use_log_scale) 1 else 0
    
    # Set default clipping if not provided
    if (is.null(clip)) {
        clip <- if (use_log_scale) c(0.1, 10) else c(-5, 5)
    }
    
    # Create the combined forest plot with proper bold formatting
    fp <- forestplot(
        labeltext = plot_data$labeltext,
        mean = plot_data$mean,
        lower = plot_data$lower,
        upper = plot_data$upper,
        is.summary = plot_data$is_summary,
        clip = clip,
        xlog = use_log_scale,
        zero = zero_line,
        boxsize = 0.25,
        lineheight = unit(8, "mm"),
        colgap = unit(2, "mm"),
        col = fpColors(
            box = c("blue", "red"),
            line = c("blue", "red"),
            summary = c("blue", "red"),
            hrz_lines = "black"
        ),
        txt_gp = fpTxtGp(
            label = gpar(fontface = plot_data$label_fontface, cex = 0.8),  # Use dynamic fontface
            ticks = gpar(cex = 0.8),
            xlab = gpar(cex = 0.9, fontface = "bold")
        ),
        graph.pos = 3,  # Position after 6 text columns
        hrzl_lines = plot_data$hrzl_lines,
        vertices = TRUE,
        ci.vertices = TRUE,
        ci.vertices.height = 0.1,
        title = title,
        xlab = sprintf("%s (95%% CI)", effect_measure),
        legend = c("Full Cohort", "Restricted Cohort"),
        legend_args = fpLegend(
            pos = list(x = 0.85, y = 0.95),
            gp = gpar(col = "#CCCCCC", fill = "#F9F9F9")
        ),
        new_page = FALSE
    )
    
    # Add favours labels
    grid.text(favours_labels[1], x = 0.1, y = 0.02, 
              gp = gpar(cex = 0.8, fontface = "italic"))
    grid.text(favours_labels[2], x = 0.9, y = 0.02, 
              gp = gpar(cex = 0.8, fontface = "italic"))
    
    return(fp)
}

#' Create formatted data for single cohort forest plot
#'
#' @param subgroup_results List of subgroup analysis results
#' @param variable_order Character vector of variables to include (enforced for consistency)
#' @param treatment_labels Character vector of treatment labels
#' @param effect_measure Character string for effect measure
#' @return List with formatted data for forestplot
create_forest_plot_data <- function(subgroup_results, variable_order, treatment_labels, effect_measure) {
    
    # Calculate total rows needed - accurately count actual data structure
    total_rows <- 1  # Header row
    for (var_name in variable_order) {
        total_rows <- total_rows + 1  # Variable header (always included)
        
        # Count actual subgroups if data exists
        if (var_name %in% names(subgroup_results)) {
            var_data <- subgroup_results[[var_name]]
            if (!is.null(var_data$subgroup_effects) && nrow(var_data$subgroup_effects) > 0) {
                total_rows <- total_rows + nrow(var_data$subgroup_effects)
            } else {
                total_rows <- total_rows + 1  # "No data available" row
            }
        } else {
            total_rows <- total_rows + 1  # "No data available" row for missing variable
        }
    }
    
    # Create simple labeltext matrix
    labeltext <- matrix("", nrow = total_rows, ncol = 5)
    labeltext[1, ] <- c("Subgroup", 
                       paste(treatment_labels[1], "n/N"), 
                       paste(treatment_labels[2], "n/N"), 
                       sprintf("%s (95%% CI)", effect_measure),
                       "p value")
    
    # Initialize arrays
    mean_vals <- rep(NA, total_rows)
    lower_vals <- rep(NA, total_rows)
    upper_vals <- rep(NA, total_rows)
    is_summary <- rep(FALSE, total_rows)
    is_summary[1] <- TRUE  # Header is summary
    
    # Track fontface for each row (bold for headers, plain for subgroups)
    label_fontface <- rep("plain", total_rows)
    label_fontface[1] <- "bold"  # Header
    
    current_row <- 2
    
    # Process ALL variables in variable_order for consistency
    for (var_name in variable_order) {
        
        # Variable header row (always included for consistency)
        labeltext[current_row, 1] <- format_variable_name(var_name)
        is_summary[current_row] <- TRUE
        label_fontface[current_row] <- "bold"  # Variable headers are bold
        current_row <- current_row + 1
        
        # Check if data exists for this variable
        if (var_name %in% names(subgroup_results)) {
            var_data <- subgroup_results[[var_name]]
            
            if (!is.null(var_data$subgroup_effects) && nrow(var_data$subgroup_effects) > 0) {
                # Subgroup rows with data
                effects_data <- var_data$subgroup_effects
                for (i in 1:nrow(effects_data)) {
                    row_data <- effects_data[i, ]
                    
                    labeltext[current_row, 1] <- paste0("  ", row_data$subgroup_level)
                    labeltext[current_row, 2] <- format_sample_size(row_data$n_gksrs, row_data$n_total)
                    labeltext[current_row, 3] <- format_sample_size(row_data$n_plaque, row_data$n_total)
                    labeltext[current_row, 4] <- sprintf("%.2f (%.2f-%.2f)", 
                                                       row_data$treatment_effect,
                                                       row_data$ci_lower,
                                                       row_data$ci_upper)
                    labeltext[current_row, 5] <- format_p_value(row_data$p_value)
                    
                    mean_vals[current_row] <- row_data$treatment_effect
                    lower_vals[current_row] <- row_data$ci_lower
                    upper_vals[current_row] <- row_data$ci_upper
                    label_fontface[current_row] <- "plain"  # Subgroup levels are plain
                    current_row <- current_row + 1
                }
            } else {
                # Variable exists but has no subgroup data - show "No data available"
                labeltext[current_row, 1] <- "  No data available"
                label_fontface[current_row] <- "italic"
                current_row <- current_row + 1
            }
        } else {
            # Variable doesn't exist in results - show "No data available"
            labeltext[current_row, 1] <- "  No data available"
            label_fontface[current_row] <- "italic"
            current_row <- current_row + 1
        }
    }
    
    # Convert matrix to list format for forestplot
    labeltext_list <- list()
    for (i in 1:5) {
        labeltext_list[[i]] <- labeltext[, i]
    }
    
    return(list(
        labeltext = labeltext_list,
        mean = mean_vals,
        lower = lower_vals,
        upper = upper_vals,
        is_summary = is_summary,
        label_fontface = label_fontface,  # Add fontface information
        hrzl_lines = list("1" = gpar(lwd = 2, col = "black"))
    ))
}

#' Create formatted data for combined cohort forest plot
#'
#' @param full_results List of subgroup analysis results for full cohort
#' @param restricted_results List of subgroup analysis results for restricted cohort
#' @param variable_order Character vector of variables to include (enforced for consistency)
#' @param treatment_labels Character vector of treatment labels
#' @param effect_measure Character string for effect measure
#' @return List with formatted data for forestplot
create_combined_forest_plot_data <- function(full_results, restricted_results, variable_order, treatment_labels, effect_measure) {
    
    # Calculate total rows needed using ALL variables in variable_order for consistency
    total_rows <- 1  # Header row
    for (var_name in variable_order) {
        total_rows <- total_rows + 1  # Variable header
        
        # Use the maximum number of subgroups from either cohort
        max_subgroups <- 0
        if (var_name %in% names(full_results)) {
            full_var <- full_results[[var_name]]
            if (!is.null(full_var$subgroup_effects) && nrow(full_var$subgroup_effects) > 0) {
                max_subgroups <- max(max_subgroups, nrow(full_var$subgroup_effects))
            }
        }
        if (var_name %in% names(restricted_results)) {
            restricted_var <- restricted_results[[var_name]]
            if (!is.null(restricted_var$subgroup_effects) && nrow(restricted_var$subgroup_effects) > 0) {
                max_subgroups <- max(max_subgroups, nrow(restricted_var$subgroup_effects))
            }
        }
        
        total_rows <- total_rows + max(max_subgroups, 1)  # At least 1 row for "No data"
    }
    
    # Create simple labeltext matrix
    labeltext <- matrix("", nrow = total_rows, ncol = 6)
    labeltext[1, ] <- c("Subgroup", 
                       "Full Cohort HR (95% CI)", 
                       "Restricted Cohort HR (95% CI)",
                       "Full n_GKSRS/n_Plaque",
                       "Restricted n_GKSRS/n_Plaque",
                       "p value")
    
    # Initialize arrays for both cohorts
    mean_vals1 <- rep(NA, total_rows)   # Full cohort
    mean_vals2 <- rep(NA, total_rows)   # Restricted cohort
    lower_vals1 <- rep(NA, total_rows)
    lower_vals2 <- rep(NA, total_rows)
    upper_vals1 <- rep(NA, total_rows)
    upper_vals2 <- rep(NA, total_rows)
    is_summary <- rep(FALSE, total_rows)
    is_summary[1] <- TRUE  # Header is summary
    
    # Track fontface for each row
    label_fontface <- rep("plain", total_rows)
    label_fontface[1] <- "bold"  # Header
    
    current_row <- 2
    
    # Process ALL variables in variable_order for consistency
    for (var_name in variable_order) {
        
        # Variable header row (always included)
        labeltext[current_row, 1] <- format_variable_name(var_name)
        is_summary[current_row] <- TRUE
        label_fontface[current_row] <- "bold"  # Variable headers are bold
        current_row <- current_row + 1
        
        # Check if either cohort has data for this variable
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
            
            # Subgroup rows
            for (i in 1:nrow(aligned_data)) {
                row_data <- aligned_data[i, ]
                
                labeltext[current_row, 1] <- paste0("  ", row_data$subgroup_level)
                
                # Full cohort data
                if (!is.na(row_data$full_treatment_effect)) {
                    labeltext[current_row, 2] <- sprintf("%.2f (%.2f-%.2f)", 
                                                       row_data$full_treatment_effect,
                                                       row_data$full_ci_lower,
                                                       row_data$full_ci_upper)
                    labeltext[current_row, 4] <- sprintf("%d/%d", row_data$full_n_gksrs, row_data$full_n_plaque)
                    mean_vals1[current_row] <- row_data$full_treatment_effect
                    lower_vals1[current_row] <- row_data$full_ci_lower
                    upper_vals1[current_row] <- row_data$full_ci_upper
                } else {
                    labeltext[current_row, 2] <- "No data"
                    labeltext[current_row, 4] <- "No data"
                }
                
                # Restricted cohort data
                if (!is.na(row_data$restricted_treatment_effect)) {
                    labeltext[current_row, 3] <- sprintf("%.2f (%.2f-%.2f)", 
                                                       row_data$restricted_treatment_effect,
                                                       row_data$restricted_ci_lower,
                                                       row_data$restricted_ci_upper)
                    labeltext[current_row, 5] <- sprintf("%d/%d", row_data$restricted_n_gksrs, row_data$restricted_n_plaque)
                    mean_vals2[current_row] <- row_data$restricted_treatment_effect
                    lower_vals2[current_row] <- row_data$restricted_ci_lower
                    upper_vals2[current_row] <- row_data$restricted_ci_upper
                } else {
                    labeltext[current_row, 3] <- "No data"
                    labeltext[current_row, 5] <- "No data"
                }
                
                # P-value (use full cohort p-value if available)
                if (!is.na(row_data$full_p_value)) {
                    labeltext[current_row, 6] <- format_p_value(row_data$full_p_value)
                } else if (!is.na(row_data$restricted_p_value)) {
                    labeltext[current_row, 6] <- format_p_value(row_data$restricted_p_value)
                }
                
                label_fontface[current_row] <- "plain"  # Subgroup levels are plain
                current_row <- current_row + 1
            }
        } else {
            # No data available for either cohort
            labeltext[current_row, 1] <- "  No data available"
            label_fontface[current_row] <- "italic"
            current_row <- current_row + 1
        }
    }
    
    # Convert matrix to list format for forestplot
    labeltext_list <- list()
    for (i in 1:6) {
        labeltext_list[[i]] <- labeltext[, i]
    }
    
    return(list(
        labeltext = labeltext_list,
        mean = cbind(mean_vals1, mean_vals2),
        lower = cbind(lower_vals1, lower_vals2),
        upper = cbind(upper_vals1, upper_vals2),
        is_summary = is_summary,
        label_fontface = label_fontface,  # Add fontface information
        hrzl_lines = list("1" = gpar(lwd = 2, col = "black"))
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
