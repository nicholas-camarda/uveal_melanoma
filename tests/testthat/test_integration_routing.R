#' Exercise integration bootstrap routing in a fresh R subprocess
#'
#' @param raw_dir Explicit read-only raw-data directory to pass to bootstrap.
#' @param processed_dir Explicit read-only processed-data directory to pass to
#'   bootstrap.
#' @return A list with captured subprocess output and its integer exit status.
run_integration_bootstrap_subprocess <- function(raw_dir = "", processed_dir = "") {
    script <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            sprintf(
                "source(%s)",
                dQuote(here::here("tests", "integration", "helper-bootstrap.R"))
            ),
            "cat(paste(RAW_DATA_DIR, PROCESSED_DATA_DIR, OUTPUT_DIR, sep = '\\n'))"
        ),
        script
    )
    # A fresh process prevents the test runner's global state from masking
    # missing environment variables or accidental fallback paths.
    output <- suppressWarnings(system2(
        file.path(R.home("bin"), "Rscript"),
        script,
        stdout = TRUE,
        stderr = TRUE,
        env = c(
            paste0("OCULAR_INTEGRATION_RAW_DATA_DIR=", raw_dir),
            paste0("OCULAR_INTEGRATION_PROCESSED_DATA_DIR=", processed_dir),
            "OCULAR_INTEGRATION_BOOTSTRAPPED=false"
        )
    ))
    status <- attr(output, "status")
    list(output = output, status = if (is.null(status)) 0L else status)
}

test_that("actual-data integration uses explicit read-only inputs and temporary outputs", {
    raw_dir <- withr::local_tempdir()
    processed_dir <- withr::local_tempdir()
    file.create(file.path(raw_dir, INPUT_FILENAME))
    for (dataset in c(
        "uveal_melanoma_full_cohort.rds",
        "uveal_melanoma_restricted_cohort.rds",
        "uveal_melanoma_gksrs_only_cohort.rds"
    )) {
        file.create(file.path(processed_dir, dataset))
    }

    run <- run_integration_bootstrap_subprocess(raw_dir, processed_dir)
    expect_identical(run$status, 0L)

    reported <- tail(run$output, 3L)
    expect_identical(normalizePath(reported[[1]]), normalizePath(raw_dir))
    expect_identical(normalizePath(reported[[2]]), normalizePath(processed_dir))
    reported_output <- normalizePath(reported[[3]], winslash = "/", mustWork = FALSE)
    expect_false(startsWith(reported_output, normalizePath(raw_dir)))
    expect_false(startsWith(reported_output, normalizePath(processed_dir)))
})

test_that("actual-data integration fails closed when explicit inputs are absent", {
    missing_raw <- run_integration_bootstrap_subprocess(
        raw_dir = "",
        processed_dir = withr::local_tempdir()
    )
    expect_gt(missing_raw$status, 0L)
    expect_match(
        paste(missing_raw$output, collapse = "\n"),
        "OCULAR_INTEGRATION_RAW_DATA_DIR",
        fixed = TRUE
    )

    missing_processed <- run_integration_bootstrap_subprocess(
        raw_dir = withr::local_tempdir(),
        processed_dir = ""
    )
    expect_gt(missing_processed$status, 0L)
    expect_match(
        paste(missing_processed$output, collapse = "\n"),
        "OCULAR_INTEGRATION_PROCESSED_DATA_DIR",
        fixed = TRUE
    )
})
