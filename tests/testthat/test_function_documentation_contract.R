test_that("PR 18 and PR 19 named functions retain complete documentation", {
    contract_files <- c(
        "scripts/tools/compare_important_results.R",
        "scripts/ci/run_portable_suite.R",
        "scripts/ci/run_testthat.R",
        "scripts/workflow/objective_1_primary_outcomes.R",
        "tests/integration/helper-bootstrap.R",
        "tests/testthat/helper-fixture-data.R",
        "tests/testthat/helper-objective-fixtures.R",
        "tests/testthat/test_ci_contract.R",
        "tests/testthat/test_codebase_review_contract.R",
        "tests/testthat/test_integration_routing.R"
    )
    issues <- character()
    audited <- 0L

    for (relative_path in contract_files) {
        path <- here::here(relative_path)
        lines <- readLines(path, warn = FALSE)
        expressions <- parse(path, keep.source = TRUE)

        for (expression in expressions) {
            # Only top-level named function assignments form the repository's
            # docstring contract; anonymous test callbacks are intentionally
            # excluded because their purpose is documented by the test label.
            is_named_function <- is.call(expression) &&
                as.character(expression[[1L]]) %in% c("<-", "=") &&
                is.symbol(expression[[2L]]) &&
                is.call(expression[[3L]]) &&
                identical(as.character(expression[[3L]][[1L]]), "function")
            if (!is_named_function) {
                next
            }

            audited <- audited + 1L
            function_name <- as.character(expression[[2L]])
            definition_line <- grep(
                paste0(function_name, " <- function"),
                lines,
                fixed = TRUE
            )[[1L]]
            cursor <- definition_line - 1L
            documentation <- character()
            while (cursor >= 1L && startsWith(lines[[cursor]], "#'")) {
                documentation <- c(lines[[cursor]], documentation)
                cursor <- cursor - 1L
            }

            parameter_lines <- grep("@param", documentation, value = TRUE)
            documented_parameters <- sub(
                " .*",
                "",
                sub("^#' @param ", "", parameter_lines)
            )
            function_parameters <- names(expression[[3L]][[2L]])
            purpose_lines <- documentation[
                !grepl("^#'([[:space:]]*$|[[:space:]]*@)", documentation)
            ]

            if (!length(purpose_lines)) {
                issues <- c(issues, paste(relative_path, function_name, "purpose"))
            }
            if (!all(function_parameters %in% documented_parameters)) {
                issues <- c(issues, paste(relative_path, function_name, "parameters"))
            }
            if (!any(grepl("@return", documentation, fixed = TRUE))) {
                issues <- c(issues, paste(relative_path, function_name, "return"))
            }
        }
    }

    # The lower bound records the completed audit without making a newly added,
    # fully documented function fail merely because an exact count changed.
    expect_true(audited >= 67L)
    expect_identical(issues, character())
})
