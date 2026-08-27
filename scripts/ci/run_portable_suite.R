#!/usr/bin/env Rscript

#' Run the repository's complete portable validation gate
#'
#' The gate invokes the canonical testthat runner for unit and synthetic
#' integration tests, then runs repository lint in isolated subprocess stages.
#' The runner validates its checked-in file manifest, so an omitted file,
#' warning, or lint is fail-closed without a brittle hard-coded case count.
#'
#' @return Invisibly returns `TRUE` when every validation stage succeeds;
#'   otherwise throws an error.
run_portable_suite <- function() {
    rscript <- file.path(R.home("bin"), "Rscript")
    # Keep the stage list declarative so CI and local validation share one
    # command while each subprocess retains its own routing contract.
    stages <- list(
        list(
            label = "Complete portable testthat suite",
            args = c("scripts/ci/run_testthat.R", "tests/testthat"),
            env = "OCULAR_PORTABLE_SUITE=true"
        ),
        list(
            label = "Synthetic integration suite",
            args = c("scripts/ci/run_testthat.R", "tests/portable"),
            env = character()
        ),
        list(
            label = "Repository lint",
            args = c(
                "-e",
                shQuote(paste0(
                    "options(warn = 2); lints <- lintr::lint_package(); ",
                    "if (length(lints) > 0L) { print(lints); ",
                    "stop(sprintf('%d lint(s) found', length(lints))) }"
                ))
            ),
            env = character()
        )
    )

    for (stage in stages) {
        message(sprintf("\n== %s ==", stage$label))
        status <- system2(rscript, stage$args, env = stage$env)
        if (!identical(status, 0L)) {
            stop(sprintf("Portable suite stage failed: %s", stage$label), call. = FALSE)
        }
    }
    invisible(TRUE)
}

if (sys.nframe() == 0L) {
    run_portable_suite()
}
