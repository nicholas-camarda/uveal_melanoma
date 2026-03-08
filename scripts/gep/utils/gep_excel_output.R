# GEP Excel output helpers

#' Sanitize Excel sheet names for openxlsx output
#'
#' Excel sheet names must be unique, short, and free of reserved characters.
#'
#' @param sheet_name Proposed sheet name.
#' @param existing_names Character vector of already-used sheet names.
#' @return A unique sheet name safe for Excel workbooks.
sanitize_gep_sheet_name <- function(sheet_name, existing_names = character()) {
    safe_name <- gsub("[:\\\\/?*\\[\\]]", "_", as.character(sheet_name))
    safe_name <- substr(safe_name, 1, 31)

    if (!nzchar(safe_name)) {
        safe_name <- "Sheet"
    }

    candidate <- safe_name
    suffix <- 1
    while (candidate %in% existing_names) {
        suffix_label <- paste0("_", suffix)
        candidate <- paste0(substr(safe_name, 1, max(1, 31 - nchar(suffix_label))), suffix_label)
        suffix <- suffix + 1
    }

    candidate
}

#' Write a GEP workbook with automatic column widths
#'
#' Writes one or more data frames to an Excel workbook and applies automatic
#' column sizing so the saved sheets are readable without manual resizing.
#'
#' @param workbook_data A data frame or named list of data frames.
#' @param output_path File path for the workbook.
#' @param freeze_header Logical; if `TRUE`, freeze the header row on each sheet.
#' @param min_width Minimum width used by openxlsx auto-sizing.
#' @param max_width Maximum width used by openxlsx auto-sizing.
#' @return Invisibly returns `output_path`.
write_gep_workbook <- function(workbook_data,
                               output_path,
                               freeze_header = TRUE,
                               min_width = 10,
                               max_width = 60) {
    if (is.data.frame(workbook_data)) {
        workbook_data <- list(Data = workbook_data)
    }

    if (!is.list(workbook_data) || length(workbook_data) == 0) {
        stop("write_gep_workbook() requires a data frame or non-empty named list of data frames")
    }

    if (is.null(names(workbook_data)) || any(!nzchar(names(workbook_data)))) {
        names(workbook_data) <- paste0("Sheet", seq_along(workbook_data))
    }

    workbook <- openxlsx::createWorkbook()
    used_sheet_names <- character()
    old_options <- options(openxlsx.minWidth = min_width, openxlsx.maxWidth = max_width)
    on.exit(options(old_options), add = TRUE)

    for (sheet_name in names(workbook_data)) {
        sheet_data <- workbook_data[[sheet_name]]
        if (is.null(sheet_data)) {
            next
        }

        if (!is.data.frame(sheet_data)) {
            sheet_data <- as.data.frame(sheet_data, stringsAsFactors = FALSE)
        }

        safe_sheet_name <- sanitize_gep_sheet_name(sheet_name, used_sheet_names)
        used_sheet_names <- c(used_sheet_names, safe_sheet_name)

        openxlsx::addWorksheet(workbook, safe_sheet_name)
        openxlsx::writeData(workbook, safe_sheet_name, sheet_data)

        if (freeze_header && nrow(sheet_data) > 0) {
            openxlsx::freezePane(workbook, safe_sheet_name, firstActiveRow = 2)
        }

        if (ncol(sheet_data) > 0) {
            openxlsx::setColWidths(workbook, safe_sheet_name, cols = seq_len(ncol(sheet_data)), widths = "auto")
        }
    }

    openxlsx::saveWorkbook(workbook, output_path, overwrite = TRUE)
    invisible(output_path)
}