coverage_path <- here::here("docs", "maintenance", "codebase_review_coverage.yaml")
contract_path <- here::here("docs", "maintenance", "important_results_contract.yaml")
comparator_path <- here::here("scripts", "maintenance", "compare_important_results.R")

require_file_for_contract_test <- function(path) {
    testthat::expect_true(file.exists(path), info = paste("missing protected-lane file:", path))
    if (!file.exists(path)) {
        return(FALSE)
    }
    TRUE
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

    base_runtime <- file.path(tempdir(), "important-results-base")
    candidate_runtime <- file.path(tempdir(), "important-results-candidate")
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
    contract_file <- file.path(tempdir(), "important-results-test-contract.yaml")
    yaml::write_yaml(contract, contract_file)
    report_file <- file.path(tempdir(), "important-results-test-report.json")

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

    base_runtime <- file.path(tempdir(), "important-results-negative-base")
    candidate_runtime <- file.path(tempdir(), "important-results-negative-candidate")
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

    contract_file <- file.path(tempdir(), "important-results-negative-contract.yaml")
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
    report_file <- file.path(tempdir(), "important-results-negative-report.json")

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
