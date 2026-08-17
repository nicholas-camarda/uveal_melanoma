if (!identical(Sys.getenv("OCULAR_PORTABLE_BOOTSTRAPPED"), "true")) {
    portable_output_root <- tempfile("ocular-portable-")
    dir.create(portable_output_root, recursive = TRUE, showWarnings = FALSE)
    runtime_root <- file.path(portable_output_root, "runtime")
    export_parent_dir <- file.path(portable_output_root, "export_parent")
    export_root <- file.path(export_parent_dir, "uveal-melanoma")
    export_analysis_dir <- file.path(export_root, "outputs")
    raw_data_dir <- file.path(export_root, "Original Files")
    processed_data_dir <- file.path(runtime_root, "Analytic Dataset")
    dir.create(raw_data_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(export_analysis_dir, recursive = TRUE, showWarnings = FALSE)

    Sys.setenv(
        OCULAR_RUNTIME_ROOT = runtime_root,
        OCULAR_EXPORT_PARENT_DIR = export_parent_dir,
        TEST_OUTPUT_DIR = portable_output_root,
        PROCESSED_DATA_DIR = processed_data_dir,
        OUTPUT_DIR = file.path(runtime_root, "Analysis"),
        RAW_DATA_DIR = raw_data_dir,
        TOOLS_OUTPUT_DIR = file.path(runtime_root, "tools_output"),
        MERGED_TABLES_DIR = file.path(runtime_root, "Analysis", "merged_tables"),
        LOGS_DIR = file.path(runtime_root, "logs")
    )

    withr::defer(
        unlink(portable_output_root, recursive = TRUE, force = TRUE),
        envir = parent.frame()
    )

    source(here::here("scripts", "load_all.R"))
    source(here::here("tests", "testthat", "helper-fixture-data.R"))

    assign("RUNTIME_ROOT", runtime_root, envir = .GlobalEnv)
    assign("EXPORT_PARENT_DIR", export_parent_dir, envir = .GlobalEnv)
    assign("EXPORT_ROOT", export_root, envir = .GlobalEnv)
    assign("EXPORT_ANALYSIS_DIR", export_analysis_dir, envir = .GlobalEnv)
    assign("TEST_OUTPUT_DIR", portable_output_root, envir = .GlobalEnv)
    assign("DATA_DIR", export_root, envir = .GlobalEnv)
    assign("PROCESSED_DATA_DIR", processed_data_dir, envir = .GlobalEnv)
    assign("OUTPUT_DIR", file.path(runtime_root, "Analysis"), envir = .GlobalEnv)
    assign("RAW_DATA_DIR", raw_data_dir, envir = .GlobalEnv)
    assign("TOOLS_OUTPUT_DIR", file.path(runtime_root, "tools_output"), envir = .GlobalEnv)
    assign("MERGED_TABLES_DIR", file.path(runtime_root, "Analysis", "merged_tables"), envir = .GlobalEnv)
    assign("LOGS_DIR", file.path(runtime_root, "logs"), envir = .GlobalEnv)

    Sys.setenv(OCULAR_PORTABLE_BOOTSTRAPPED = "true")
}
