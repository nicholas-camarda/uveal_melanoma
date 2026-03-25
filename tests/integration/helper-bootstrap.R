if (!identical(Sys.getenv("OCULAR_INTEGRATION_BOOTSTRAPPED"), "true")) {
    integration_output_root <- tempfile("ocular-integration-")
    dir.create(integration_output_root, recursive = TRUE, showWarnings = FALSE)

    Sys.setenv(TEST_OUTPUT_DIR = integration_output_root)
    Sys.setenv(OUTPUT_DIR = file.path(integration_output_root, "analysis"))
    Sys.setenv(TOOLS_OUTPUT_DIR = file.path(integration_output_root, "tools_output"))
    Sys.setenv(MERGED_TABLES_DIR = file.path(integration_output_root, "analysis", "merged_tables"))
    Sys.setenv(LOGS_DIR = file.path(integration_output_root, "logs"))

    withr::defer(
        unlink(integration_output_root, recursive = TRUE, force = TRUE),
        envir = testthat::teardown_env()
    )

    source(here::here("scripts", "load_all.R"))
    source(here::here("tests", "testthat", "test_helper_data.R"))

    assign("TEST_OUTPUT_DIR", integration_output_root, envir = .GlobalEnv)
    assign("DATA_DIR", EXPORT_ROOT, envir = .GlobalEnv)
    assign("OUTPUT_DIR", file.path(integration_output_root, "analysis"), envir = .GlobalEnv)
    assign("TOOLS_OUTPUT_DIR", file.path(integration_output_root, "tools_output"), envir = .GlobalEnv)
    assign("MERGED_TABLES_DIR", file.path(integration_output_root, "analysis", "merged_tables"), envir = .GlobalEnv)
    assign("LOGS_DIR", file.path(integration_output_root, "logs"), envir = .GlobalEnv)

    Sys.setenv(OCULAR_INTEGRATION_BOOTSTRAPPED = "true")
}

is_integration_enabled <- function() {
    tolower(Sys.getenv("OCULAR_RUN_INTEGRATION_TESTS", "false")) %in% c("1", "true", "yes")
}

skip_if_integration_disabled <- function() {
    testthat::skip_if_not(
        is_integration_enabled(),
        "Integration tests are disabled. Set OCULAR_RUN_INTEGRATION_TESTS=true to run."
    )
}

skip_if_local_data_unavailable <- function() {
    data_root <- PROCESSED_DATA_DIR
    raw_file <- file.path(RAW_DATA_DIR, INPUT_FILENAME)
    testthat::skip_if_not(
        dir.exists(data_root),
        paste("Local analytic data directory is unavailable:", data_root)
    )
    testthat::skip_if_not(
        file.exists(raw_file),
        paste("Local raw source file is unavailable:", raw_file)
    )
}
