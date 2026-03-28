# Markdown formatting helpers used by human-facing report writers.

#' Create a Markdown heading
#'
#' @param text Heading text.
#' @param level Heading level from 1 to 6.
#'
#' @return Character scalar containing the Markdown heading.
md_heading <- function(text, level = 1L) {
    level <- max(1L, min(6L, as.integer(level)))
    paste0(strrep("#", level), " ", text)
}

#' Create a Markdown bullet line
#'
#' @param text Bullet text.
#' @param indent Number of two-space indent levels.
#'
#' @return Character scalar containing the Markdown bullet line.
md_bullet <- function(text, indent = 0L) {
    paste0(strrep("  ", max(0L, as.integer(indent))), "- ", text)
}

#' Render a data frame as a Markdown pipe table
#'
#' @param data_frame Data frame with columns already formatted as strings.
#'
#' @return Character vector containing the Markdown table lines.
md_table <- function(data_frame) {
    if (is.null(data_frame) || !is.data.frame(data_frame) || nrow(data_frame) == 0) {
        return(character())
    }

    table_df <- data.frame(
        lapply(data_frame, function(column) {
            if (is.factor(column)) {
                column <- as.character(column)
            }
            as.character(column)
        }),
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    table_df[] <- lapply(table_df, function(column) {
        ifelse(is.na(column), "", gsub("|", "\\\\|", column, fixed = TRUE))
    })

    header <- paste0("| ", paste(names(table_df), collapse = " | "), " |")
    separator <- paste0("| ", paste(rep("---", ncol(table_df)), collapse = " | "), " |")
    rows <- vapply(seq_len(nrow(table_df)), function(i) {
        paste0("| ", paste(unlist(table_df[i, , drop = TRUE]), collapse = " | "), " |")
    }, character(1))

    c(header, separator, rows)
}

