# Forest Plot Formatting

#' Align forest-plot body columns and style interaction-test status text
#'
#' Keep subgroup labels left-aligned with explicit row-role anchors, center
#' numeric/table columns under their headers, and retain a restrained style for
#' non-numeric interaction-test statuses.
align_forest_plot_body_columns <- function(fp, status_column = 7L,
                                           include_interaction_p = TRUE) {
    # forestploter's table body is laid out by gridExtra. Explicitly editing
    # the text grobs is required; placing hjust only in gpar() does not move
    # an existing text grob's x anchor.
    column_anchors <- c(
        subgroup = 0.05,
        treatment_1 = 0.5,
        treatment_2 = 0.5,
        ci = 0.5,
        estimate = 0.5,
        p_value = 0.5
    )
    if (isTRUE(include_interaction_p)) {
        column_anchors <- c(column_anchors, interaction_p = 0.5)
    }

    if (!is.null(status_column) && status_column >= 1L && status_column <= length(column_anchors)) {
        column_anchors[status_column] <- 0.5
    }

    for (column_index in seq_along(column_anchors)) {
        anchor <- unname(column_anchors[[column_index]])
        hjust <- if (column_index == 1L) 0 else anchor
        fp <- edit_plot(
            fp,
            col = column_index,
            which = "text",
            x = grid::unit(anchor, "npc"),
            hjust = hjust
        )
    }

    fp
}

#' Align the subgroup header with the left-anchored row-label column
align_forest_plot_headers <- function(fp, subgroup_column = 1L) {
    edit_plot(
        fp,
        part = "header",
        col = subgroup_column,
        which = "text",
        x = grid::unit(0.05, "npc"),
        hjust = 0
    )
}

#' Align every subgroup level at one stable indentation anchor
#'
#' Leading spaces are not a reliable visual indentation mechanism because their
#' rendered width varies with font face and device. Use explicit grob
#' positioning instead so plain, italic, short, and long level labels share the
#' same left edge across every forest plot.
align_forest_plot_subgroup_levels <- function(fp, plot_data,
                                              subgroup_column = 1L,
                                              level_anchor = 0.10) {
    level_rows <- plot_data$subgroup_level_rows %||% logical()
    if (is.logical(level_rows)) {
        level_rows <- which(level_rows)
    } else {
        level_rows <- as.integer(level_rows)
    }

    if (length(level_rows) == 0L) {
        return(fp)
    }

    for (row_idx in level_rows) {
        fp <- edit_plot(
            fp,
            row = row_idx,
            col = subgroup_column,
            which = "text",
            x = grid::unit(level_anchor, "npc"),
            hjust = 0
        )
    }

    fp
}

#' Style explicit interaction-test status text without weakening header hierarchy
#'
#' Variable headers remain bold; only the non-numeric status in the interaction
#' column is italicized and slightly smaller, matching the existing unsupported-level style.
style_forest_interaction_status_cells <- function(fp, plot_data, status_column = 7L,
                                                  include_interaction_p = TRUE) {
    fp <- align_forest_plot_body_columns(
        fp,
        status_column = status_column,
        include_interaction_p = include_interaction_p
    )
    fp <- align_forest_plot_headers(fp)
    fp <- align_forest_plot_subgroup_levels(fp, plot_data)

    if (!isTRUE(include_interaction_p)) {
        return(fp)
    }

    status_rows <- plot_data$interaction_status_rows %||% integer(0)
    if (length(status_rows) == 0) {
        return(fp)
    }

    for (row_idx in status_rows) {
        fp <- edit_plot(
            fp,
            row = row_idx,
            col = status_column,
            which = "text",
            x = grid::unit(0.5, "npc"),
            hjust = 0.5,
            gp = gpar(fontface = "italic", col = "black", cex = 0.85)
        )
    }
    fp
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
            col = 1, # First column (subgroup names)
            gp = gpar(fontface = "bold")
        )
    }

    # Apply italic formatting to "No data available" rows
    italic_rows <- which(plot_data$font_face == "italic")
    for (row_idx in italic_rows) {
        fp <- edit_plot(fp,
            row = row_idx,
            col = 1, # First column (subgroup names)
            gp = gpar(fontface = "italic", col = "grey50")
        )
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
    if (length(vals) == 0) {
        return(c(0.1, 10))
    }

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
    if (length(vals) == 0) {
        return(c(-1, 1))
    }

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
#'
#' @param fp A forestploter object
#' @return Data frame with diagnostics information
get_forest_plot_diagnostics <- function(fp) {
    attr(fp, "diagnostics")
}

#' Write diagnostics list to an Excel workbook with one sheet per plot
#'
#' @param diagnostics_list Named list where each element is a data.frame of diagnostics
#' @param file_path Full path of the .xlsx to create
write_diagnostics_excel <- function(diagnostics_list, file_path) {
    if (length(diagnostics_list) == 0) {
        return(invisible(NULL))
    }
    write_readable_xlsx(diagnostics_list, file_path)
}

#' Compute dynamic height for a single forest plot grob
#'
#' Calculates height based on row count. Accounts for title, column headers,
#' data rows, axis labels, and arrows.
#'
#' @param fp A forestploter grob (with forest_row_count attribute)
#' @param min_height Minimum height in inches (default 4)
#' @param max_height Maximum height in inches (default 14)
#' @return Numeric height in inches
compute_forest_plot_height <- function(fp, min_height = 4, max_height = 14) {
    row_count <- attr(fp, "forest_row_count")
    if (is.null(row_count) || !is.finite(row_count) || row_count <= 0) {
        return(7) # Safe default
    }
    
    # Components:
    # - Title: ~0.4"
    # - Column headers: ~0.35"
    # - Data rows: ~0.28" each
    # - X-axis ticks + labels: ~0.35"
    # - Favors arrows/labels: ~0.4"
    # Total overhead: ~1.5"
    height <- 1.5 + row_count * 0.28
    
    max(min_height, min(max_height, height))
}

#' Combine multiple forest plots into a labelled grid grob
#'
#' @param grobs List of forestploter grobs (NULL entries are ignored)
#' @param panel_labels Optional vector of labels ("a. ...") matching grobs
#' @param ncol Number of columns in the output grid
#' @return A grob suitable for grid::grid.draw(), or NULL if no valid grobs
combine_forest_plot_panels <- function(grobs, panel_labels = NULL, ncol = 2) {
    if (length(grobs) == 0) {
        return(NULL)
    }

    valid_idx <- which(vapply(grobs, function(x) !is.null(x), logical(1)))
    if (length(valid_idx) == 0) {
        return(NULL)
    }

    grobs <- grobs[valid_idx]
    if (!is.null(panel_labels)) {
        panel_labels <- panel_labels[valid_idx]
        if (length(panel_labels) != length(grobs)) {
            stop("panel_labels must match the number of grobs when provided")
        }
    } else {
        panel_labels <- rep("", length(grobs))
    }

    # Use the shared helper for each panel
    panel_heights <- vapply(grobs, compute_forest_plot_height, numeric(1))

    labelled_grobs <- Map(function(g, lbl) {
        if (is.null(lbl) || lbl == "") {
            return(g)
        }
        # Place label in its own row above the forest plot
        label_grob <- grid::textGrob(
            lbl,
            x = grid::unit(4, "mm"),
            y = grid::unit(0, "npc"),
            just = c("left", "bottom"),
            gp = grid::gpar(fontface = "bold", cex = 1.5) # Panel label text size and formatting
        )
        gridExtra::arrangeGrob(
            grobs = list(label_grob, g),
            ncol = 1,
            heights = grid::unit.c(
                grid::unit(0.22, "inches"),
                grid::unit(1, "null")
            )
        )
    }, grobs, panel_labels)

    n_panels <- length(labelled_grobs)
    grid_rows <- ceiling(n_panels / ncol)
    
    # Label adds 0.025" to each panel
    label_overhead <- 0.025
    
    row_height_inches <- numeric(grid_rows)
    for (r in seq_len(grid_rows)) {
        idx <- which(((seq_len(n_panels) - 1) %/% ncol) + 1 == r)
        # Row height = max forest plot height + label overhead
        row_height_inches[r] <- max(panel_heights[idx], na.rm = TRUE) + label_overhead
    }
    if (any(!is.finite(row_height_inches))) {
        row_height_inches[!is.finite(row_height_inches)] <- pmax(5, stats::median(row_height_inches[is.finite(row_height_inches)], na.rm = TRUE))
    }
    row_height_units <- grid::unit(row_height_inches, "inches")

    base_combined <- do.call(
        gridExtra::arrangeGrob,
        c(labelled_grobs, list(ncol = ncol, heights = row_height_units, padding = grid::unit(0, "lines")))
    )
    attr(base_combined, "row_height_inches") <- row_height_inches
    attr(base_combined, "column_count") <- min(ncol, n_panels)
    attr(base_combined, "panel_heights") <- panel_heights

    top_margin_inches <- 0.1
    bottom_margin_inches <- 0.5
    combined <- gridExtra::arrangeGrob(
        grobs = list(grid::nullGrob(), base_combined, grid::nullGrob()),
        ncol = 1,
        heights = grid::unit.c(
            grid::unit(top_margin_inches, "inches"),
            grid::unit(1, "null"),
            grid::unit(bottom_margin_inches, "inches")
        )
    )
    attr(combined, "row_height_inches") <- attr(base_combined, "row_height_inches")
    attr(combined, "column_count") <- attr(base_combined, "column_count")
    attr(combined, "panel_heights") <- attr(base_combined, "panel_heights")
    combined
}
