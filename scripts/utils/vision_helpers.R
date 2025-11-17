#' Compute line change counts from logMAR differences
#'
#' @param logmar_change Numeric vector of logMAR changes computed as 
#'   (baseline - followup), so positive = improvement.
#' @param step Numeric size of one Snellen line in logMAR units (default 0.1).
#' @param digits Number of decimal places to keep when converting to lines.
#' @return Numeric vector of line changes.
compute_line_change_lines <- function(logmar_change, step = VISION_LINE_CHANGE_STEP, digits = 0) {
    if (is.null(logmar_change)) {
        return(numeric())
    }
    # Convert logMAR change to line change counts, rounding up for improvements and down for losses 
    # This ensures that any partial line change is counted as a full line change in the appropriate direction
    result <- ifelse(logmar_change >= 0,
        ceiling(logmar_change / step),
        floor(logmar_change / step))
    return(result)
}

#' Categorize logMAR changes into clinically meaningful Snellen line buckets
#'
#' @param logmar_change Numeric vector of logMAR changes.
#' @param step Numeric size of one Snellen line in logMAR units (default 0.1).
#' @return Factor vector with ordered Snellen line-change categories.
categorize_line_change <- function(logmar_change, step = VISION_LINE_CHANGE_STEP) {
    if (is.null(logmar_change)) {
        return(character())
    }
    line_counts <- compute_line_change_lines(logmar_change, step = step)
    format_line_change_label(line_counts)
}

#' Format integer line changes into human-readable labels
#'
#' @param line_counts Integer vector of Snellen line changes (positive = improvement).
#' @return Character vector of labels (e.g., "1-line loss").
format_line_change_label <- function(line_counts) {
    if (is.null(line_counts)) {
        return(character())
    }

    case_when(
        is.na(line_counts) ~ NA_character_,
        line_counts == 0 ~ "Stable (0-line change)",
        line_counts > 0 ~ sprintf("%d-line improvement", line_counts),
        TRUE ~ sprintf("%d-line loss", abs(line_counts))
    )
}

#' Determine ordered label levels spanning the observed line changes
#'
#' @param line_counts Integer vector of Snellen line changes.
#' @return Character vector of ordered labels for use in factors.
line_change_label_levels <- function(line_counts) {
    line_counts <- line_counts[!is.na(line_counts)]

    if (length(line_counts) == 0) {
        return(character())
    }

    line_range <- seq(min(line_counts), max(line_counts))
    positives <- sort(unique(line_range[line_range > 0]), decreasing = TRUE)
    zeros <- if (any(line_range == 0)) 0 else numeric()
    negatives <- sort(unique(line_range[line_range < 0]), decreasing = TRUE)
    ordered_range <- c(positives, zeros, negatives)

    if (length(ordered_range) == 0) {
        ordered_range <- line_range
    }

    format_line_change_label(ordered_range)
}

#' Bucket Snellen line changes into clinically meaningful categories
#'
#' @param line_counts Integer vector of Snellen line changes (positive = improvement).
#' @return Ordered factor with predefined category levels
assign_line_change_bucket <- function(line_counts) {
    if (is.null(line_counts)) {
        return(factor())
    }

    categories <- case_when(
        is.na(line_counts) ~ NA_character_,
        line_counts >= 3 ~ "≥3-line improvement",
        line_counts == 2 ~ "2-line improvement",
        line_counts == 1 ~ "1-line improvement",
    line_counts == 0 ~ "Stable (0-line change)",
        line_counts == -1 ~ "1-line loss",
        line_counts == -2 ~ "2-line loss",
        line_counts <= -3 ~ "≥3-line loss"
    )

    factor(categories, levels = VISION_LINE_CHANGE_CATEGORY_LEVELS, ordered = TRUE)
}
