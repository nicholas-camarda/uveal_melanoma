#!/usr/bin/env Rscript

run_portable_suite <- function() {
    rscript <- file.path(R.home("bin"), "Rscript")
    stages <- list(
        list(
            label = "Complete portable testthat suite",
            args = c("scripts/tools/run_testthat.R", "tests/testthat"),
            env = c(
                "OCULAR_PORTABLE_SUITE=true",
                "OCULAR_EXPECTED_TEST_FILES=41",
                "OCULAR_EXPECTED_TEST_CASES=271"
            )
        ),
        list(
            label = "Synthetic integration suite",
            args = c("scripts/tools/run_testthat.R", "tests/portable"),
            env = c(
                "OCULAR_EXPECTED_TEST_FILES=1",
                "OCULAR_EXPECTED_TEST_CASES=1"
            )
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
