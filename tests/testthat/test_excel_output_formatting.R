test_that("readable Excel writer caps note columns and sizes ordinary rows", {
    output_path <- tempfile(fileext = ".xlsx")
    note_text <- paste(rep("massive narrative note", 80), collapse = " ")
    ordinary_text <- paste(rep("ordinary visible text", 12), collapse = " ")

    write_readable_xlsx(
        data.frame(
            id = c("A1", "A2"),
            summary = c("compact", ordinary_text),
            notes = c(note_text, "short"),
            stringsAsFactors = FALSE
        ),
        output_path,
        long_text_threshold = 300
    )

    extract_dir <- tempfile("xlsx-contents-")
    dir.create(extract_dir)
    utils::unzip(output_path, files = "xl/worksheets/sheet1.xml", exdir = extract_dir)
    sheet_xml <- paste(readLines(file.path(extract_dir, "xl/worksheets/sheet1.xml"), warn = FALSE), collapse = "")

    col_width <- function(col_index) {
        pattern <- sprintf("<col min=\"%d\" max=\"%d\" width=\"([0-9.]+)\"", col_index, col_index)
        match <- regexec(pattern, sheet_xml)
        as.numeric(regmatches(sheet_xml, match)[[1]][[2]])
    }
    row_height <- function(row_index) {
        pattern <- sprintf("<row r=\"%d\" ht=\"([0-9.]+)\"", row_index)
        match <- regexec(pattern, sheet_xml)
        as.numeric(regmatches(sheet_xml, match)[[1]][[2]])
    }

    expect_lte(col_width(3), 46)
    expect_equal(row_height(2), 15)
    expect_gt(row_height(3), 15)
})
