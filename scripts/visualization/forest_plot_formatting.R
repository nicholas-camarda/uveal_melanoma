# Forest Plot Formatting

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
    if (length(diagnostics_list) == 0) return(invisible(NULL))
    writexl::write_xlsx(diagnostics_list, file_path)
}
