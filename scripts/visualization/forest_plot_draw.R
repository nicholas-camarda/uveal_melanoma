# Forest Plot Drawing

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
    tryCatch(
        {
            plot <- create_single_cohort_forest_plot(
                subgroup_results = subgroup_results,
                outcome_name = outcome_name,
                cohort_name = dataset_name,
                treatment_labels = TREATMENT_LABELS,
                variable_order = variable_order,
                effect_measure = effect_measure,
                favours_labels = FAVOURS_LABELS,
                clip = NULL
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
        },
        error = function(e) {
            warning(sprintf("Failed to create forest plot for %s: %s", outcome_name, e$message))
            return(NULL)
        }
    )
}

#' Create a forest plot for a single cohort's subgroup analysis results
#'
#' @param subgroup_results List of subgroup analysis results from analyze_treatment_effect_subgroups_*
#' @param outcome_name Character string for the outcome name (e.g., "Overall Survival")
#' @param cohort_name Character string for the cohort name (e.g., "Full Cohort")
#' @param treatment_labels Exact treatment keys `PBT` and `GKSRS`, in display order.
#' @param variable_order Character vector specifying the order of variables to display (REQUIRED for consistency)
#' @param effect_measure Character string for the effect measure (default: "HR")
#' @param favours_labels Character vector of length 2 for axis labels (e.g., c("Favors GKSRS", "Favors PBT"))
#' @param clip Numeric vector of length 2 for clipping range (default: c(0.1, 10))
#' @param title Character string for plot title (optional)
#' @param include_interaction_p Logical; include the interaction p-value column
#'   (default `TRUE` for subgroup plots).
#' @param label_column Character string for the first table-column header.
#' @param include_variable_header Logical; include a per-variable header row
#'   (default `TRUE` for subgroup plots).
#' @return A forestploter object
create_single_cohort_forest_plot <- function(subgroup_results,
                                             outcome_name,
                                             cohort_name = "Cohort",
                                             treatment_labels = TREATMENT_LABELS,
                                             variable_order, # Now required for consistency
                                             effect_measure = "HR",
                                             favours_labels = NULL,
                                             clip = NULL,
                                             title = NULL,
                                             include_interaction_p = TRUE,
                                             label_column = "Subgroup",
                                             include_variable_header = TRUE) {
    # Check that variable_order is provided
    if (missing(variable_order) || is.null(variable_order)) {
        stop("variable_order must be provided to ensure consistency across cohorts")
    }

    # Set default favours labels if not provided
    if (is.null(favours_labels)) {
        favours_labels <- paste0("Favors ", treatment_labels)
    }

    # Resolve treatment colors via centralized palette (if two labels provided)
    treatment_levels <- as.character(treatment_labels)
    treatment_colors <- tryCatch(get_treatment_palette(treatment_levels), error = function(e) NULL)

    # Create the formatted data for forestploter
    plot_data <- create_forest_plot_data(
        subgroup_results = subgroup_results,
        variable_order = variable_order,
        treatment_labels = treatment_labels,
        effect_measure = effect_measure,
        include_interaction_p = include_interaction_p,
        label_column = label_column,
        include_variable_header = include_variable_header
    )

    # Set default title
    if (is.null(title)) {
        title <- sprintf("Subgroup Analysis: %s", outcome_name)
    }

    # Set scale parameters: data-driven detection of ratio vs difference measures
    # If all estimates and CI bounds are positive, assume this is a ratio measure (HR/OR/RR)
    all_values <- c(plot_data$est_values, plot_data$lower_values, plot_data$upper_values)
    all_values <- all_values[!is.na(all_values)]
    use_log_scale <- length(all_values) > 0 && all(all_values > 0)

    # Check for problematic values (<= 0) when using log scale
    if (use_log_scale) {
        problematic_values <- any(
            !is.na(plot_data$est_values) & plot_data$est_values <= 0 |
                !is.na(plot_data$lower_values) & plot_data$lower_values <= 0 |
                !is.na(plot_data$upper_values) & plot_data$upper_values <= 0
        )

        if (problematic_values) {
            warning("Found values <= 0 in forest plot data. Switching to linear scale to avoid log transformation errors.")
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
        # Establish a publication-style hierarchy for the title and table
        # headers while preserving the existing title alignment.
        title_gp = gpar(cex = 1.30, fontface = "bold", col = "black"),
        # Header formatting - this controls the column headers
        colhead = list(
            fg_params = list(
                fontface = "bold",
                cex = 1.05,
                hjust = 0.5,
                x = 0.5
            )
        ),
        # Core content formatting with dynamic font face and size
        core = list(
            fg_params = list(
                fontface = plot_data$font_face, # Dynamic font faces
                cex = plot_data$text_size # Dynamic text sizes
            )
        )
    )

    # Optional footnote disabled by default to avoid clutter
    footnote_text <- NULL

    # forestploter::forest() calls grid::convertHeight() internally. In
    # non-interactive runs (Rscript), that can open the default file device and
    # create Rplots.pdf if no graphics device is active yet.
    opened_temp_device <- FALSE
    temp_plot_path <- NULL
    if (grDevices::dev.cur() == 1L) {
        temp_plot_path <- tempfile(pattern = "forest_plot_device_", fileext = ".png")
        grDevices::png(filename = temp_plot_path, width = 72, height = 72, units = "px", res = 72)
        opened_temp_device <- TRUE
    }

    on.exit({
        if (opened_temp_device && grDevices::dev.cur() > 1L) {
            grDevices::dev.off()
        }
        if (!is.null(temp_plot_path) && file.exists(temp_plot_path)) {
            unlink(temp_plot_path)
        }
    }, add = TRUE)

    # Create the forest plot using correct forestploter syntax following documentation
    # CI column is position 4 (blank column after Subgroup and keyed arm counts).
    fp <- forest(
        plot_data$data_frame,
        est = plot_data$est_values,
        lower = plot_data$lower_values,
        upper = plot_data$upper_values,
        sizes = 0.4,
        is_summary = plot_data$is_summary,
        ci_column = 4, # Position of blank column
        ref_line = if (use_log_scale) 1 else 0,
        arrow_lab = favours_labels,
        xlim = clip,
        xticks = xticks,
        x_trans = if (use_log_scale) "log" else "none",
        theme = tm,
        title = title
    )
    fp <- style_forest_interaction_status_cells(
        fp,
        plot_data,
        include_interaction_p = include_interaction_p
    )

    # Attach diagnostics for external retrieval
    attr(fp, "diagnostics") <- plot_data$diagnostics
    attr(fp, "forest_row_count") <- nrow(plot_data$data_frame)

    return(fp)
}
