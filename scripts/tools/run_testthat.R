#!/usr/bin/env Rscript

#' Run a testthat directory with failure-sensitive process status
#'
#' `testthat::test_dir()` is called with explicit failure propagation so test
#' failures and errors terminate `Rscript` with a non-zero status in local and
#' CI execution.
#'
#' @param args Command-line arguments containing one test directory.
#' @return Invisibly returns the testthat result when all tests pass.
run_testthat_directory <- function(args = commandArgs(trailingOnly = TRUE)) {
    if (length(args) != 1L || !nzchar(trimws(args[[1]]))) {
        stop(
            "Usage: Rscript scripts/tools/run_testthat.R <test-directory>",
            call. = FALSE
        )
    }

    test_dir <- normalizePath(args[[1]], winslash = "/", mustWork = FALSE)
    if (!dir.exists(test_dir)) {
        stop(sprintf("Test directory does not exist: %s", test_dir), call. = FALSE)
    }
    if (!requireNamespace("testthat", quietly = TRUE)) {
        stop("Package 'testthat' is required to run the test suite.", call. = FALSE)
    }

    result <- testthat::test_dir(
        test_dir,
        stop_on_failure = TRUE,
        stop_on_warning = FALSE
    )
    invisible(result)
}

if (sys.nframe() == 0L) {
    run_testthat_directory()
}
