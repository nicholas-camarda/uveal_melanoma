if (!identical(Sys.getenv("OCULAR_TESTTHAT_BOOTSTRAPPED"), "true")) {
    test_output_root <- tempfile("ocular-testthat-")
    dir.create(test_output_root, recursive = TRUE, showWarnings = FALSE)

    Sys.setenv(TEST_OUTPUT_DIR = test_output_root)
    Sys.setenv(DATA_DIR = file.path(test_output_root, "data"))
    Sys.setenv(OUTPUT_DIR = file.path(test_output_root, "analysis"))
    Sys.setenv(RAW_DATA_DIR = file.path(test_output_root, "raw"))
    Sys.setenv(TOOLS_OUTPUT_DIR = file.path(test_output_root, "tools"))
    Sys.setenv(MERGED_TABLES_DIR = file.path(test_output_root, "merged_tables"))
    Sys.setenv(LOGS_DIR = file.path(test_output_root, "logs"))

    withr::defer(
        unlink(test_output_root, recursive = TRUE, force = TRUE),
        envir = testthat::teardown_env()
    )

    source(here::here("scripts", "load_all.R"))
    source(here::here("tests", "testthat", "test_helper_data.R"))

    assign("TEST_OUTPUT_DIR", test_output_root, envir = .GlobalEnv)
    assign("DATA_DIR", file.path(test_output_root, "data"), envir = .GlobalEnv)
    assign("OUTPUT_DIR", file.path(test_output_root, "analysis"), envir = .GlobalEnv)
    assign("RAW_DATA_DIR", file.path(test_output_root, "raw"), envir = .GlobalEnv)
    assign("TOOLS_OUTPUT_DIR", file.path(test_output_root, "tools"), envir = .GlobalEnv)
    assign("MERGED_TABLES_DIR", file.path(test_output_root, "merged_tables"), envir = .GlobalEnv)
    assign("LOGS_DIR", file.path(test_output_root, "logs"), envir = .GlobalEnv)

    Sys.setenv(OCULAR_TESTTHAT_BOOTSTRAPPED = "true")
}
