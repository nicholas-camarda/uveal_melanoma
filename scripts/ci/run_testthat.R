#!/usr/bin/env Rscript

#' Read the required test-file manifest for a test directory
#'
#' @param test_dir Directory containing `required-test-files.txt`.
#' @return Sorted test-file basenames, or `NULL` when no manifest is present.
read_required_test_files <- function(test_dir) {
    manifest_path <- file.path(test_dir, "required-test-files.txt")
    if (!file.exists(manifest_path)) {
        return(NULL)
    }

    entries <- trimws(readLines(manifest_path, warn = FALSE))
    entries <- entries[nzchar(entries) & !startsWith(entries, "#")]
    invalid <- entries[
        grepl("[/\\\\]", entries) |
            !grepl("^test.*\\.[rR]$", entries)
    ]
    if (length(invalid) > 0L || anyDuplicated(entries)) {
        stop(
            paste(
                "required-test-files.txt must contain unique test-file basenames;",
                "invalid entries:",
                paste(unique(invalid), collapse = ", ")
            ),
            call. = FALSE
        )
    }
    sort(entries)
}

#' Summarize a testthat directory result
#'
#' @param result A `testthat_results` object.
#' @param test_dir Directory passed to `testthat::test_dir()`.
#' @param filter Optional testthat filename filter.
#' @param warning_messages Warning messages captured outside testthat's result
#'   frame during suite setup, execution, or teardown.
#' @return A named list of discovered, required, and executed files plus result
#'   counts. `cases` is the dynamic number reported by testthat; no static
#'   source-code case count is inferred.
summarize_testthat_result <- function(
    result,
    test_dir,
    filter = NULL,
    warning_messages = character()
) {
    result_frame <- as.data.frame(result)
    all_discovered_files <- sort(list.files(
        test_dir,
        pattern = "^test.*\\.[rR]$",
        recursive = FALSE,
        full.names = FALSE
    ))
    discovered_files <- all_discovered_files
    if (!is.null(filter)) {
        filter_names <- sub(
            "^test[-_]?",
            "",
            tools::file_path_sans_ext(discovered_files)
        )
        discovered_files <- discovered_files[grepl(filter, filter_names)]
    }

    manifest_files <- read_required_test_files(test_dir)
    required_files <- if (is.null(manifest_files)) {
        discovered_files
    } else {
        manifest_files
    }
    if (!is.null(filter) && !is.null(manifest_files)) {
        filter_names <- sub(
            "^test[-_]?",
            "",
            tools::file_path_sans_ext(required_files)
        )
        required_files <- required_files[grepl(filter, filter_names)]
    }
    executed_files <- if (nrow(result_frame) == 0L) {
        character()
    } else {
        sort(unique(basename(result_frame$file)))
    }

    list(
        discovered_files = discovered_files,
        required_files = required_files,
        manifest_files = manifest_files,
        missing_required_files = if (is.null(manifest_files)) {
            character()
        } else {
            setdiff(required_files, discovered_files)
        },
        unexpected_files = if (is.null(manifest_files)) {
            character()
        } else {
            setdiff(discovered_files, required_files)
        },
        executed_files = executed_files,
        cases = nrow(result_frame),
        failures = sum(result_frame$failed) + sum(result_frame$error),
        warnings = sum(result_frame$warning) + length(warning_messages),
        warning_messages = warning_messages,
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
    # A manifest-listed file absent from disk is already reported as missing;
    # exclude it here so one drift produces one actionable diagnostic.
    unexecuted <- setdiff(
        setdiff(summary$required_files, summary$missing_required_files),
        summary$executed_files
    )
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
    if (length(summary$missing_required_files) > 0L) {
        problems <- c(
            problems,
            sprintf(
                "Missing required test files: %s",
                paste(summary$missing_required_files, collapse = ", ")
            )
        )
    }
    if (length(summary$unexpected_files) > 0L) {
        problems <- c(
            problems,
            sprintf(
                "Unexpected test files: %s",
                paste(summary$unexpected_files, collapse = ", ")
            )
        )
    }
    if (length(unexecuted) > 0L) {
        problems <- c(
            problems,
            sprintf("Unexecuted test files: %s", paste(unexecuted, collapse = ", "))
        )
    }
    if (length(summary$warning_messages) > 0L) {
        problems <- c(
            problems,
            sprintf(
                "Unexpected warnings: %s",
                paste(unique(summary$warning_messages), collapse = " | ")
            )
        )
    }
    if (length(problems) > 0L) {
        stop(
            sprintf("Test suite rejected: %s", paste(problems, collapse = "; ")),
            call. = FALSE
        )
    }
    invisible(summary)
}

#' Run testthat while collecting warnings outside expected warning assertions
#'
#' `testthat` normally captures warnings raised by test expressions, setup,
#' and teardown in different places. Running with `warn = 2` makes unhandled
#' warnings reach this boundary; the condition handler records and muffles them
#' so the suite can finish and report one concise fail-closed diagnostic.
#'
#' @param test_dir Directory passed to `testthat::test_dir()`.
#' @param filter Optional testthat filename filter.
#' @return A named list containing the testthat result and captured warnings.
run_testthat_with_warning_capture <- function(test_dir, filter = NULL) {
    warning_messages <- character()
    # Testthat handles `expect_warning()` before this outer handler, preserving
    # expected-warning assertions while exposing all other warning conditions.
    withr::local_options(warn = 2L)
    result <- withCallingHandlers(
        testthat::test_dir(
            test_dir,
            filter = filter,
            stop_on_failure = FALSE,
            stop_on_warning = FALSE
        ),
        warning = function(condition) {
            warning_messages <<- c(warning_messages, conditionMessage(condition))
            invokeRestart("muffleWarning")
        }
    )
    list(result = result, warning_messages = warning_messages)
}

#' Run a testthat directory with failure-sensitive process status
#'
#' `testthat::test_dir()` is called with explicit failure propagation so test
#' failures, warnings, skips, and manifest drift terminate `Rscript` with a
#' non-zero status in local and CI execution.
#'
#' @param args Command-line arguments containing a test directory and an
#'   optional `--filter <regular-expression>` pair.
#' @return Invisibly returns the testthat result when all tests pass.
run_testthat_directory <- function(args = commandArgs(trailingOnly = TRUE)) {
    if (length(args) < 1L || length(args) > 3L || !nzchar(trimws(args[[1]]))) {
        stop(
            paste(
                "Usage: Rscript scripts/ci/run_testthat.R <test-directory>",
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
                    "Usage: Rscript scripts/ci/run_testthat.R <test-directory>",
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
    # Canonical repository suites must carry a checked-in manifest; ad hoc
    # temporary directories remain usable for focused developer runs.
    manifest_required <- identical(
        Sys.getenv("OCULAR_REQUIRE_TEST_MANIFEST", unset = ""),
        "true"
    ) || basename(test_dir) %in% c("testthat", "portable", "integration")
    if (manifest_required && !file.exists(file.path(test_dir, "required-test-files.txt"))) {
        stop(
            sprintf("Required test manifest is missing: %s", file.path(test_dir, "required-test-files.txt")),
            call. = FALSE
        )
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

    captured <- run_testthat_with_warning_capture(test_dir, filter)
    summary <- summarize_testthat_result(
        captured$result,
        test_dir,
        filter,
        captured$warning_messages
    )
    assert_testthat_result(summary)
    message(sprintf(
        "Validated %d dynamically executed test cases across %d test files.",
        summary$cases,
        length(summary$executed_files)
    ))
    invisible(captured$result)
}

if (sys.nframe() == 0L) {
    run_testthat_directory()
}
