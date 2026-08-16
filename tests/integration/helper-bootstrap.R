if (!identical(Sys.getenv("OCULAR_INTEGRATION_BOOTSTRAPPED"), "true")) {
    raw_data_dir <- Sys.getenv("OCULAR_INTEGRATION_RAW_DATA_DIR", unset = "")
    processed_data_dir <- Sys.getenv(
        "OCULAR_INTEGRATION_PROCESSED_DATA_DIR",
        unset = ""
    )
    if (!nzchar(trimws(raw_data_dir))) {
        stop(
            "OCULAR_INTEGRATION_RAW_DATA_DIR must name the read-only raw-data directory.",
            call. = FALSE
        )
    }
    if (!nzchar(trimws(processed_data_dir))) {
        stop(
            paste(
                "OCULAR_INTEGRATION_PROCESSED_DATA_DIR must name the read-only",
                "processed-data directory."
            ),
            call. = FALSE
        )
    }
    raw_data_dir <- normalizePath(raw_data_dir, winslash = "/", mustWork = TRUE)
    processed_data_dir <- normalizePath(
        processed_data_dir,
        winslash = "/",
        mustWork = TRUE
    )

    integration_output_root <- tempfile("ocular-integration-")
    dir.create(integration_output_root, recursive = TRUE, showWarnings = FALSE)
    runtime_root <- file.path(integration_output_root, "runtime")
    export_parent_dir <- file.path(integration_output_root, "export_parent")
    export_root <- file.path(export_parent_dir, "uveal-melanoma")
    export_analysis_dir <- file.path(export_root, "outputs")
    dir.create(export_analysis_dir, recursive = TRUE, showWarnings = FALSE)

    Sys.setenv(
        OCULAR_RUNTIME_ROOT = runtime_root,
        OCULAR_EXPORT_PARENT_DIR = export_parent_dir,
        TEST_OUTPUT_DIR = integration_output_root,
        PROCESSED_DATA_DIR = processed_data_dir,
        OUTPUT_DIR = file.path(runtime_root, "analysis"),
        TOOLS_OUTPUT_DIR = file.path(runtime_root, "tools_output"),
        MERGED_TABLES_DIR = file.path(runtime_root, "analysis", "merged_tables"),
        LOGS_DIR = file.path(runtime_root, "logs"),
        RAW_DATA_DIR = raw_data_dir
    )

    withr::defer(
        unlink(integration_output_root, recursive = TRUE, force = TRUE),
        envir = parent.frame()
    )

    source(here::here("scripts", "load_all.R"))
    source(here::here("tests", "testthat", "test_helper_data.R"))

    required_inputs <- c(
        file.path(raw_data_dir, INPUT_FILENAME),
        file.path(processed_data_dir, "uveal_melanoma_full_cohort.rds"),
        file.path(processed_data_dir, "uveal_melanoma_restricted_cohort.rds"),
        file.path(processed_data_dir, "uveal_melanoma_gksrs_only_cohort.rds")
    )
    missing_inputs <- required_inputs[!file.exists(required_inputs)]
    if (length(missing_inputs) > 0L) {
        stop(
            sprintf(
                "Actual-data integration prerequisites are missing: %s",
                paste(basename(missing_inputs), collapse = ", ")
            ),
            call. = FALSE
        )
    }

    assign("RUNTIME_ROOT", runtime_root, envir = .GlobalEnv)
    assign("EXPORT_PARENT_DIR", export_parent_dir, envir = .GlobalEnv)
    assign("EXPORT_ROOT", export_root, envir = .GlobalEnv)
    assign("EXPORT_ANALYSIS_DIR", export_analysis_dir, envir = .GlobalEnv)
    assign("TEST_OUTPUT_DIR", integration_output_root, envir = .GlobalEnv)
    assign("DATA_DIR", export_root, envir = .GlobalEnv)
    assign("PROCESSED_DATA_DIR", processed_data_dir, envir = .GlobalEnv)
    assign("OUTPUT_DIR", file.path(runtime_root, "analysis"), envir = .GlobalEnv)
    assign("RAW_DATA_DIR", raw_data_dir, envir = .GlobalEnv)
    assign("TOOLS_OUTPUT_DIR", file.path(runtime_root, "tools_output"), envir = .GlobalEnv)
    assign("MERGED_TABLES_DIR", file.path(runtime_root, "analysis", "merged_tables"), envir = .GlobalEnv)
    assign("LOGS_DIR", file.path(runtime_root, "logs"), envir = .GlobalEnv)

    Sys.setenv(OCULAR_INTEGRATION_BOOTSTRAPPED = "true")
}
