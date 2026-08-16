#!/usr/bin/env Rscript

#' Summarize a testthat directory result
#'
#' @param result A `testthat_results` object.
#' @param test_dir Directory passed to `testthat::test_dir()`.
#' @param filter Optional testthat filename filter.
#' @return A named list of discovered and executed files and result counts.
count_test_declarations <- function(paths) {
    walk <- function(expression) {
        if (!is.call(expression)) {
            return(0L)
        }
        head <- expression[[1L]]
        call_name <- if (is.symbol(head)) {
            as.character(head)
        } else if (
            is.call(head) &&
                identical(as.character(head[[1L]]), "::")
        ) {
            as.character(head[[3L]])
        } else {
            ""
        }
        as.integer(call_name %in% c("test_that", "it")) +
            sum(vapply(as.list(expression)[-1L], walk, integer(1L)))
    }

    sum(vapply(
        paths,
        function(path) {
            sum(vapply(as.list(parse(path)), walk, integer(1L)))
        },
        integer(1L)
    ))
}

read_expected_count <- function(name) {
    value <- Sys.getenv(name, unset = "")
    if (!nzchar(value)) {
        return(NULL)
    }
    parsed <- suppressWarnings(as.integer(value))
    if (is.na(parsed) || parsed < 0L || !identical(as.character(parsed), value)) {
        stop(sprintf("%s must be a non-negative integer.", name), call. = FALSE)
    }
    parsed
}

summarize_testthat_result <- function(result, test_dir, filter = NULL) {
    result_frame <- as.data.frame(result)
    discovered_files <- sort(list.files(
        test_dir,
        pattern = "^test.*\\.[rR]$",
        recursive = FALSE,
        full.names = FALSE
    ))
    if (!is.null(filter)) {
        filter_names <- sub(
            "^test[-_]?",
            "",
            tools::file_path_sans_ext(discovered_files)
        )
        discovered_files <- discovered_files[grepl(filter, filter_names)]
    }

    executed_files <- if (nrow(result_frame) == 0L) {
        character()
    } else {
        sort(unique(basename(result_frame$file)))
    }

    list(
        discovered_files = discovered_files,
        executed_files = executed_files,
        declared_cases = count_test_declarations(file.path(test_dir, discovered_files)),
        cases = nrow(result_frame),
        failures = sum(result_frame$failed) + sum(result_frame$error),
        warnings = sum(result_frame$warning),
        skips = sum(result_frame$skipped)
    )
}

#' Reject an incomplete or non-clean testthat result
#'
#' @param summary Result from `summarize_testthat_result()`.
#' @param fail_on_warning Whether warnings make the run fail.
#' @param fail_on_skip Whether skips make the run fail.
#' @return Invisibly returns `summary` when it is clean and complete.
assert_testthat_result <- function(
    summary,
    fail_on_warning = TRUE,
    fail_on_skip = TRUE
) {
    unexecuted <- setdiff(summary$discovered_files, summary$executed_files)
    problems <- character()
    if (summary$failures > 0L) {
        problems <- c(problems, sprintf("failures=%d", summary$failures))
    }
    if (isTRUE(fail_on_warning) && summary$warnings > 0L) {
        problems <- c(problems, sprintf("warnings=%d", summary$warnings))
    }
    if (isTRUE(fail_on_skip) && summary$skips > 0L) {
        problems <- c(problems, sprintf("skips=%d", summary$skips))
    }
    if (length(unexecuted) > 0L) {
        problems <- c(
            problems,
            sprintf("Unexecuted test files: %s", paste(unexecuted, collapse = ", "))
        )
    }
    expected_files <- read_expected_count("OCULAR_EXPECTED_TEST_FILES")
    expected_cases <- read_expected_count("OCULAR_EXPECTED_TEST_CASES")
    if (!is.null(expected_files) && length(summary$discovered_files) != expected_files) {
        problems <- c(problems, sprintf(
            "Expected %d test files but discovered %d",
            expected_files,
            length(summary$discovered_files)
        ))
    }
    if (summary$cases != summary$declared_cases) {
        problems <- c(problems, sprintf(
            "Declared %d test cases but executed %d",
            summary$declared_cases,
            summary$cases
        ))
    }
    if (!is.null(expected_cases) && summary$declared_cases != expected_cases) {
        problems <- c(problems, sprintf(
            "Expected %d test cases but declared %d",
            expected_cases,
            summary$declared_cases
        ))
    }
    if (length(problems) > 0L) {
        stop(
            sprintf("Test suite rejected: %s", paste(problems, collapse = "; ")),
            call. = FALSE
        )
    }
    invisible(summary)
}

#' Run a testthat directory with failure-sensitive process status
#'
#' `testthat::test_dir()` is called with explicit failure propagation so test
#' failures and errors terminate `Rscript` with a non-zero status in local and
#' CI execution.
#'
#' @param args Command-line arguments containing a test directory and an
#'   optional `--filter <regular-expression>` pair.
#' @return Invisibly returns the testthat result when all tests pass.
run_testthat_directory <- function(args = commandArgs(trailingOnly = TRUE)) {
    if (length(args) < 1L || length(args) > 3L || !nzchar(trimws(args[[1]]))) {
        stop(
            paste(
                "Usage: Rscript scripts/tools/run_testthat.R <test-directory>",
                "[--filter <regular-expression>]"
            ),
            call. = FALSE
        )
    }

    filter <- NULL
    if (length(args) > 1L) {
        if (length(args) != 3L || !identical(args[[2]], "--filter") || !nzchar(trimws(args[[3]]))) {
            stop(
                paste(
                    "Usage: Rscript scripts/tools/run_testthat.R <test-directory>",
                    "[--filter <regular-expression>]"
                ),
                call. = FALSE
            )
        }
        filter <- args[[3]]
    }

    test_dir <- normalizePath(args[[1]], winslash = "/", mustWork = FALSE)
    if (!dir.exists(test_dir)) {
        stop(sprintf("Test directory does not exist: %s", test_dir), call. = FALSE)
    }
    if (!requireNamespace("testthat", quietly = TRUE)) {
        stop(
            paste(
                "The locked 'testthat' package is unavailable.",
                "Run Rscript scripts/bootstrap_packages.R from the repository root."
            ),
            call. = FALSE
        )
    }

    result <- testthat::test_dir(
        test_dir,
        filter = filter,
        stop_on_failure = FALSE,
        stop_on_warning = FALSE
    )
    summary <- summarize_testthat_result(result, test_dir, filter)
    assert_testthat_result(summary)
    invisible(result)
}

if (sys.nframe() == 0L) {
    run_testthat_directory()
}
