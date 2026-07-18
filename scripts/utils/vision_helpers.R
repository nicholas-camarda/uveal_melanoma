#' Round numeric values away from zero at half steps
#'
#' @param x Numeric vector.
#' @return Numeric vector rounded to the nearest integer, with halves rounded away from zero.
round_half_away_from_zero <- function(x) {
    sign(x) * floor(abs(x) + 0.5)
}

#' Compute line change counts from logMAR differences
#'
#' @param logmar_change Numeric vector of logMAR changes computed as
#'   (baseline - followup), so positive = improvement.
#' @param step Numeric size of one Snellen line in logMAR units (default 0.1).
#' @param digits Number of decimal places to keep when converting to lines.
#' @details Uses nearest-line rounding with halves rounded away from zero.
#'   This replaced the historical implementation from before commit
#'   `6df27eb` (March 16, 2026), which used `ceiling()` for positive changes
#'   and `floor()` for negative changes and therefore treated any non-zero
#'   partial line as a full extra line away from zero.
#' @return Numeric vector of line changes.
compute_line_change_lines <- function(logmar_change, step = VISION_LINE_CHANGE_STEP, digits = 0) {
    if (is.null(logmar_change)) {
        return(numeric())
    }

    result <- round_half_away_from_zero(logmar_change / step)

    if (!is.null(digits) && digits > 0) {
        result <- round(result, digits = digits)
    }

    result
}

#' Categorize logMAR changes into exact integer Snellen line labels
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
line_change_ordered_values <- function(line_counts) {
    line_counts <- line_counts[!is.na(line_counts)]

    if (length(line_counts) == 0) {
        return(numeric())
    }

    line_range <- seq(min(line_counts), max(line_counts))
    positives <- sort(unique(line_range[line_range > 0]), decreasing = TRUE)
    zeros <- if (any(line_range == 0)) 0 else numeric()
    negatives <- sort(unique(line_range[line_range < 0]), decreasing = TRUE)
    ordered_range <- c(positives, zeros, negatives)

    if (length(ordered_range) == 0) {
        ordered_range <- line_range
    }

    ordered_range
}

#' Determine ordered label levels spanning the observed line changes
#'
#' @param line_counts Integer vector of Snellen line changes.
#' @return Character vector of ordered labels for use in factors.
line_change_label_levels <- function(line_counts) {
    ordered_range <- line_change_ordered_values(line_counts)
    if (length(ordered_range) == 0) {
        return(character())
    }

    format_line_change_label(ordered_range)
}

#' Aggregate Snellen line changes into the 7-level Snellen Line Change Distribution
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

#' Convert a formatted logMAR summary string to Snellen line units
#'
#' @param stat_string Character string formatted like "{median} ({min}, {max})" with optional "; mean {mean}" suffix.
#' @param step Numeric size of one Snellen line in logMAR units.
#' @return Character string with the same summary expressed in Snellen lines.
convert_logmar_summary_stat_to_line_summary <- function(stat_string, step = VISION_LINE_CHANGE_STEP) {
    if (is.null(stat_string) || length(stat_string) == 0) {
        return(character())
    }

    unname(vapply(
        stat_string,
        FUN.VALUE = character(1),
        FUN = function(value) {
            if (is.na(value) || !nzchar(value)) {
                return(value)
            }

            matches <- stringr::str_extract_all(value, "-?\\d+(?:\\.\\d+)?")[[1]]
            if (!length(matches) %in% c(3L, 4L)) {
                return(value)
            }

            line_values <- compute_line_change_lines(as.numeric(matches), step = step)
            if (length(line_values) == 4L) {
                sprintf("%d (%d, %d); mean %d", line_values[1], line_values[2], line_values[3], line_values[4])
            } else {
                sprintf("%d (%d, %d)", line_values[1], line_values[2], line_values[3])
            }
        }
    ))
}

#' Convert a single-row logMAR gtsummary table into a Snellen summary table
#'
#' @param tbl gtsummary object with logMAR summary statistics.
#' @param label Label to apply to the converted row.
#' @param caption Caption to apply to the converted table.
#' @return Modified gtsummary object.
convert_logmar_summary_table_to_line_summary <- function(tbl,
                                                         label = "Snellen Line Change",
                                                         caption = "Snellen Line Change Summary") {
    tbl %>%
        modify_table_body(function(body) {
            stat_cols <- grep("^stat_", names(body), value = TRUE)
            for (col_name in stat_cols) {
                body[[col_name]] <- convert_logmar_summary_stat_to_line_summary(body[[col_name]])
            }
            body$label <- label
            body
        }) %>%
        modify_caption(caption)
}
