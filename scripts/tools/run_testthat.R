#!/usr/bin/env Rscript

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
        stop_on_failure = TRUE,
        stop_on_warning = FALSE
    )
    invisible(result)
}

if (sys.nframe() == 0L) {
    run_testthat_directory()
}
