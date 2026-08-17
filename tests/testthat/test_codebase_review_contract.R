coverage_path <- here::here("docs", "maintenance", "codebase_review_coverage.yaml")
contract_path <- here::here("docs", "maintenance", "important_results_contract.yaml")
comparator_path <- here::here("scripts", "tools", "compare_important_results.R")

#' Check that a protected-lane file exists and report a useful test failure.
#'
#' @param path Expected repository file path.
#' @return `TRUE` when the file exists; otherwise `FALSE` after recording an
#'   expectation failure.
#' @noRd
require_file_for_contract_test <- function(path) {
    testthat::expect_true(file.exists(path), info = paste("missing protected-lane file:", path))
    if (!file.exists(path)) {
        return(FALSE)
    }
    TRUE
}

#' Rewrite a workbook's OOXML style reference for semantic comparator tests.
#'
#' @param path Workbook archive to rewrite in place.
#' @param style_id Optional zero-based cell style reference to assign to A1.
#' @param format_id Optional OOXML number-format ID to assign to a cell style.
#' @param format_code Optional custom number-format code to assign to the first
#'   custom format entry.
#' @return Invisibly returns `path` after rebuilding the workbook archive.
#' @noRd
rewrite_workbook_style_for_test <- function(path, style_id = NULL, format_id = NULL, format_code = NULL) {
    extraction_dir <- withr::local_tempdir("comparator-style-")
    utils::unzip(path, exdir = extraction_dir)

    styles_path <- file.path(extraction_dir, "xl", "styles.xml")
    styles <- xml2::read_xml(styles_path)
    cell_xfs <- xml2::xml_find_all(styles, ".//*[local-name()='cellXfs']/*[local-name()='xf']")
    target_index <- if (is.null(style_id)) length(cell_xfs) else as.integer(style_id) + 1L
    if (!length(cell_xfs) || is.na(target_index) || target_index < 1L || target_index > length(cell_xfs)) {
        stop("Test workbook style reference is invalid", call. = FALSE)
    }
    if (!is.null(format_id)) {
        xml2::xml_set_attr(cell_xfs[[target_index]], "numFmtId", as.character(format_id))
    }
    if (!is.null(format_code)) {
        custom_formats <- xml2::xml_find_all(styles, ".//*[local-name()='numFmts']/*[local-name()='numFmt']")
        if (!length(custom_formats)) {
            stop("Test workbook has no custom number format", call. = FALSE)
        }
        xml2::xml_set_attr(custom_formats[[1L]], "formatCode", format_code)
    }
    xml2::write_xml(styles, styles_path)

    if (!is.null(style_id)) {
        worksheet_path <- file.path(extraction_dir, "xl", "worksheets", "sheet1.xml")
        worksheet <- xml2::read_xml(worksheet_path)
        first_cell <- xml2::xml_find_all(worksheet, ".//*[local-name()='sheetData']//*[local-name()='c']")[[1L]]
        xml2::xml_set_attr(first_cell, "s", as.character(style_id))
        xml2::write_xml(worksheet, worksheet_path)
    }

    members <- list.files(extraction_dir, recursive = TRUE, all.files = TRUE, full.names = FALSE)
    members <- members[!grepl("(^|/)\\.\\.?$", members)]
    unlink(path, force = TRUE)
    withr::with_dir(extraction_dir, utils::zip(path, files = members, flags = "-q"))
    invisible(path)
}

test_that("protected review contracts and comparator are present", {
    expect_true(require_file_for_contract_test(coverage_path))
    expect_true(require_file_for_contract_test(contract_path))
    expect_true(require_file_for_contract_test(comparator_path))
})

test_that("coverage ledger declares bounded review units without fabricated completion", {
    if (!require_file_for_contract_test(coverage_path)) {
        return(invisible(NULL))
    }

    ledger <- yaml::read_yaml(coverage_path)
    expect_identical(ledger$version, 1L)
    expect_true(is.list(ledger$units))

    unit_ids <- vapply(ledger$units, `[[`, character(1), "id")
    expect_identical(
        unit_ids,
        c(
            "cohort-construction",
            "endpoint-and-censoring",
            "modeling-policy",
            "survival-analysis",
            "gep-analysis",
            "tables-and-figures",
            "documentation-and-paths"
        )
    )
    expect_true(all(vapply(ledger$units, function(unit) identical(unit$status, "not_reviewed"), logical(1))))
    expect_true(all(vapply(ledger$units, function(unit) length(unit$evidence) == 0L, logical(1))))
})

test_that("important-results contract declares supported semantic comparison types", {
    if (!require_file_for_contract_test(contract_path)) {
        return(invisible(NULL))
    }

    contract <- yaml::read_yaml(contract_path)
    expect_identical(contract$version, 1L)
    expect_identical(contract$numeric_tolerance$absolute, 1e-12)
    expect_identical(contract$numeric_tolerance$relative, 1e-10)

    comparisons <- contract$comparisons
    expect_true(length(comparisons) >= 5L)
    expect_true(all(vapply(comparisons, function(item) {
        is.character(item$type) &&
            length(item$type) == 1L &&
            item$type %in% c("json", "text", "cohort", "plot_metadata", "workbook") &&
            is.character(item$path) &&
            length(item$path) == 1L &&
            !grepl("^/|^[A-Za-z]:", item$path)
    }, logical(1))))
})

test_that("comparator passes exact and tolerance-compatible synthetic artifacts", {
    if (!require_file_for_contract_test(comparator_path)) {
        return(invisible(NULL))
    }

    test_root <- withr::local_tempdir("important-results-positive-")
    base_runtime <- file.path(test_root, "base")
    candidate_runtime <- file.path(test_root, "candidate")
    dir.create(base_runtime, recursive = TRUE, showWarnings = FALSE)
    dir.create(candidate_runtime, recursive = TRUE, showWarnings = FALSE)

    relative_dirs <- c("json", "cohort", "plot", "text", "workbook")
    for (root in c(base_runtime, candidate_runtime)) {
        for (directory in relative_dirs) {
            dir.create(file.path(root, directory), recursive = TRUE, showWarnings = FALSE)
        }
    }

    jsonlite::write_json(
        list(internal_value = 1, displayed_value = "1.000"),
        file.path(base_runtime, "json", "results.json"),
        auto_unbox = TRUE,
        pretty = TRUE
    )
    jsonlite::write_json(
        list(internal_value = 1 + 1e-13, displayed_value = "1.000"),
        file.path(candidate_runtime, "json", "results.json"),
        auto_unbox = TRUE,
        pretty = TRUE
    )

    jsonlite::write_json(
        c("unit-a", "unit-b"),
        file.path(base_runtime, "cohort", "membership.json"),
        auto_unbox = FALSE,
        pretty = TRUE
    )
    jsonlite::write_json(
        c("unit-a", "unit-b"),
        file.path(candidate_runtime, "cohort", "membership.json"),
        auto_unbox = FALSE,
        pretty = TRUE
    )

    plot_metadata <- list(direction = "left-favors-a", labels = c("A", "B"), x_limits = c(0, 2))
    jsonlite::write_json(plot_metadata, file.path(base_runtime, "plot", "metadata.json"), pretty = TRUE)
    jsonlite::write_json(plot_metadata, file.path(candidate_runtime, "plot", "metadata.json"), pretty = TRUE)

    writeLines("Estimate: 1.000", file.path(base_runtime, "text", "display.txt"))
    writeLines("Estimate: 1.000", file.path(candidate_runtime, "text", "display.txt"))

    for (root in c(base_runtime, candidate_runtime)) {
        workbook <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(workbook, "Results")
        openxlsx::writeData(workbook, "Results", x = data.frame(value = 1), colNames = FALSE)
        openxlsx::writeFormula(workbook, "Results", x = "=A1*2", startCol = 2, startRow = 1)
        openxlsx::saveWorkbook(workbook, file.path(root, "workbook", "results.xlsx"), overwrite = TRUE)
    }

    contract <- list(
        version = 1L,
        numeric_tolerance = list(absolute = 1e-12, relative = 1e-10),
        comparisons = list(
            list(id = "json-results", type = "json", path = "json/results.json"),
            list(id = "cohort-membership", type = "cohort", path = "cohort/membership.json"),
            list(id = "plot-metadata", type = "plot_metadata", path = "plot/metadata.json"),
            list(id = "display-text", type = "text", path = "text/display.txt"),
            list(id = "results-workbook", type = "workbook", path = "workbook/results.xlsx")
        )
    )
    contract_file <- file.path(test_root, "contract.yaml")
    yaml::write_yaml(contract, contract_file)
    report_file <- file.path(test_root, "report.json")

    exit_code <- system2(
        "Rscript",
        c(
            comparator_path,
            "--base-runtime", base_runtime,
            "--candidate-runtime", candidate_runtime,
            "--contract", contract_file,
            "--report", report_file
        )
    )
    expect_identical(exit_code, 0L)
    expect_true(file.exists(report_file))

    report <- jsonlite::read_json(report_file, simplifyVector = FALSE)
    expect_identical(report$status, "pass")
    expect_true(all(vapply(report$comparisons, function(item) {
        identical(sort(names(item)), sort(c("id", "type", "status", "reason")))
    }, logical(1))))
    expect_false(grepl("unit-a|unit-b|1\\.000|1e-13", paste(readLines(report_file), collapse = "\n")))
})

test_that("comparator rejects displayed, ordered-cohort, formula, and missing-artifact changes", {
    if (!require_file_for_contract_test(comparator_path)) {
        return(invisible(NULL))
    }

    test_root <- withr::local_tempdir("important-results-negative-")
    base_runtime <- file.path(test_root, "base")
    candidate_runtime <- file.path(test_root, "candidate")
    dir.create(file.path(base_runtime, "cohort"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(candidate_runtime, "cohort"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(base_runtime, "text"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(candidate_runtime, "text"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(base_runtime, "workbook"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(candidate_runtime, "workbook"), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(c("unit-a", "unit-b"), file.path(base_runtime, "cohort", "membership.json"), auto_unbox = FALSE)
    jsonlite::write_json(c("unit-b", "unit-a"), file.path(candidate_runtime, "cohort", "membership.json"), auto_unbox = FALSE)
    writeLines("Estimate: 1.000", file.path(base_runtime, "text", "display.txt"))
    writeLines("Estimate: 1.001", file.path(candidate_runtime, "text", "display.txt"))

    for (root in c(base_runtime, candidate_runtime)) {
        workbook <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(workbook, "Results")
        openxlsx::writeData(workbook, "Results", x = data.frame(value = 1), colNames = FALSE)
        formula <- if (identical(root, base_runtime)) "=A1*2" else "=A1*3"
        openxlsx::writeFormula(workbook, "Results", x = formula, startCol = 2, startRow = 1)
        openxlsx::saveWorkbook(workbook, file.path(root, "workbook", "results.xlsx"), overwrite = TRUE)
    }

    contract_file <- file.path(test_root, "contract.yaml")
    yaml::write_yaml(
        list(
            version = 1L,
            numeric_tolerance = list(absolute = 1e-12, relative = 1e-10),
            comparisons = list(
                list(id = "ordered-cohort", type = "cohort", path = "cohort/membership.json"),
                list(id = "displayed-text", type = "text", path = "text/display.txt"),
                list(id = "formula-workbook", type = "workbook", path = "workbook/results.xlsx"),
                list(id = "missing-json", type = "json", path = "missing/result.json")
            )
        ),
        contract_file
    )
    report_file <- file.path(test_root, "report.json")

    exit_code <- system2(
        "Rscript",
        c(
            comparator_path,
            "--base-runtime", base_runtime,
            "--candidate-runtime", candidate_runtime,
            "--contract", contract_file,
            "--report", report_file
        )
    )
    expect_identical(exit_code, 1L)
    expect_true(file.exists(report_file))
    report <- jsonlite::read_json(report_file, simplifyVector = FALSE)
    expect_identical(report$status, "fail")
    expect_true(all(vapply(report$comparisons, function(item) {
        identical(sort(names(item)), sort(c("id", "type", "status", "reason")))
    }, logical(1))))
    reasons <- vapply(report$comparisons, `[[`, character(1), "reason")
    expect_true(any(grepl("ordered cohort", reasons, fixed = TRUE)))
    expect_true(any(grepl("displayed text", reasons, fixed = TRUE)))
    expect_true(any(grepl("workbook sheets", reasons, fixed = TRUE)))
    expect_false(grepl("unit-a|unit-b", paste(readLines(report_file), collapse = "\n")))
})

test_that("workbook comparison is semantic across style IDs but rejects format changes", {
    if (!require_file_for_contract_test(comparator_path)) {
        return(invisible(NULL))
    }

    test_root <- withr::local_tempdir("important-results-format-")
    base_runtime <- file.path(test_root, "base")
    candidate_runtime <- file.path(test_root, "candidate")
    dir.create(file.path(base_runtime, "workbook"), recursive = TRUE)
    dir.create(file.path(candidate_runtime, "workbook"), recursive = TRUE)

    for (root in c(base_runtime, candidate_runtime)) {
        workbook <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(workbook, "Results")
        openxlsx::writeData(workbook, "Results", x = data.frame(value = 1), colNames = FALSE)
        openxlsx::addStyle(
            workbook,
            "Results",
            openxlsx::createStyle(numFmt = "0.00"),
            rows = 1,
            cols = 1,
            gridExpand = FALSE
        )
        openxlsx::saveWorkbook(workbook, file.path(root, "workbook", "results.xlsx"), overwrite = TRUE)
    }

    # Baseline uses a custom 0.00 format. Candidate uses a different cell-XF
    # reference and built-in format ID 2, which is semantically equivalent.
    candidate_workbook <- file.path(candidate_runtime, "workbook", "results.xlsx")
    rewrite_workbook_style_for_test(candidate_workbook, style_id = 0L, format_id = 2L)

    contract_file <- file.path(test_root, "contract.yaml")
    yaml::write_yaml(
        list(
            version = 1L,
            numeric_tolerance = list(absolute = 1e-12, relative = 1e-10),
            comparisons = list(list(id = "formatted-workbook", type = "workbook", path = "workbook/results.xlsx"))
        ),
        contract_file
    )
    report_file <- file.path(test_root, "equivalent-report.json")
    exit_code <- system2(
        "Rscript",
        c(
            comparator_path,
            "--base-runtime", base_runtime,
            "--candidate-runtime", candidate_runtime,
            "--contract", contract_file,
            "--report", report_file
        )
    )
    expect_identical(exit_code, 0L)

    # Keep the value and style reference intact, but change only the displayed
    # precision. This must fail even though the cached numeric value is equal.
    rewrite_workbook_style_for_test(candidate_workbook, style_id = 1L, format_code = "0.000")
    report_file <- file.path(test_root, "different-format-report.json")
    exit_code <- system2(
        "Rscript",
        c(
            comparator_path,
            "--base-runtime", base_runtime,
            "--candidate-runtime", candidate_runtime,
            "--contract", contract_file,
            "--report", report_file
        )
    )
    expect_identical(exit_code, 1L)
    report <- jsonlite::read_json(report_file, simplifyVector = FALSE)
    expect_identical(report$status, "fail")

    # Rebuild the candidate with the baseline format and formula, then mutate
    # only the stored numeric value to prove value comparison remains active.
    file.copy(
        file.path(base_runtime, "workbook", "results.xlsx"),
        candidate_workbook,
        overwrite = TRUE
    )
    candidate_book <- openxlsx::loadWorkbook(candidate_workbook)
    openxlsx::writeData(
        candidate_book,
        "Results",
        x = data.frame(value = 2),
        colNames = FALSE
    )
    openxlsx::saveWorkbook(
        candidate_book,
        candidate_workbook,
        overwrite = TRUE
    )
    report_file <- file.path(test_root, "different-value-report.json")
    exit_code <- system2(
        "Rscript",
        c(
            comparator_path,
            "--base-runtime", base_runtime,
            "--candidate-runtime", candidate_runtime,
            "--contract", contract_file,
            "--report", report_file
        )
    )
    expect_identical(exit_code, 1L)
    value_report <- jsonlite::read_json(report_file, simplifyVector = FALSE)
    expect_identical(value_report$status, "fail")
})
