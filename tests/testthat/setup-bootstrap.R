# Suite-scoped bootstrap is intentionally a setup file: testthat sources helpers
# first, then establishes the temporary runtime before executing any test file.
.ocular_testthat_output_root <- tempfile("ocular-testthat-")
dir.create(.ocular_testthat_output_root, recursive = TRUE, showWarnings = FALSE)

.ocular_testthat_runtime_root <- file.path(.ocular_testthat_output_root, "runtime")
.ocular_testthat_export_parent <- file.path(.ocular_testthat_output_root, "export_parent")
.ocular_testthat_export_root <- file.path(
    .ocular_testthat_export_parent,
    "uveal-melanoma"
)
.ocular_testthat_export_analysis <- file.path(
    .ocular_testthat_export_root,
    "outputs"
)
dir.create(
    file.path(.ocular_testthat_export_root, "Original Files"),
    recursive = TRUE,
    showWarnings = FALSE
)
dir.create(
    .ocular_testthat_export_analysis,
    recursive = TRUE,
    showWarnings = FALSE
)

.ocular_testthat_binding_names <- c(
    "RUNTIME_ROOT",
    "EXPORT_PARENT_DIR",
    "EXPORT_ROOT",
    "EXPORT_ANALYSIS_DIR",
    "TEST_OUTPUT_DIR",
    "DATA_DIR",
    "PROCESSED_DATA_DIR",
    "OUTPUT_DIR",
    "RAW_DATA_DIR",
    "TOOLS_OUTPUT_DIR",
    "MERGED_TABLES_DIR",
    "LOGS_DIR"
)
.ocular_testthat_binding_existed <- vapply(
    .ocular_testthat_binding_names,
    exists,
    logical(1),
    envir = .GlobalEnv,
    inherits = FALSE
)
.ocular_testthat_binding_values <- mget(
    .ocular_testthat_binding_names[.ocular_testthat_binding_existed],
    envir = .GlobalEnv,
    inherits = FALSE
)

.ocular_testthat_environment_names <- c(
    "OCULAR_RUNTIME_ROOT",
    "OCULAR_EXPORT_PARENT_DIR",
    "TEST_OUTPUT_DIR",
    "DATA_DIR",
    "PROCESSED_DATA_DIR",
    "OUTPUT_DIR",
    "RAW_DATA_DIR",
    "TOOLS_OUTPUT_DIR",
    "MERGED_TABLES_DIR",
    "LOGS_DIR"
)
.ocular_testthat_environment_values <- Sys.getenv(
    .ocular_testthat_environment_names,
    unset = NA_character_
)
.ocular_testthat_logger_was_attached <- "package:logger" %in% search()

# Teardown restores every path binding and environment variable that setup
# changes, even when testthat exits because of a failure, error, or warning.
withr::defer(
    {
        for (.setting_name in .ocular_testthat_binding_names) {
            if (.ocular_testthat_binding_existed[[.setting_name]]) {
                assign(
                    .setting_name,
                    .ocular_testthat_binding_values[[.setting_name]],
                    envir = .GlobalEnv
                )
            } else if (exists(
                .setting_name,
                envir = .GlobalEnv,
                inherits = FALSE
            )) {
                rm(list = .setting_name, envir = .GlobalEnv)
            }
        }

        .set_environment <- !is.na(.ocular_testthat_environment_values)
        if (any(.set_environment)) {
            do.call(
                Sys.setenv,
                as.list(.ocular_testthat_environment_values[.set_environment])
            )
        }
        if (any(!.set_environment)) {
            Sys.unsetenv(.ocular_testthat_environment_names[!.set_environment])
        }
        if (!.ocular_testthat_logger_was_attached &&
            "package:logger" %in% search()) {
            detach("package:logger", unload = FALSE, character.only = TRUE)
        }
        unlink(
            .ocular_testthat_output_root,
            recursive = TRUE,
            force = TRUE
        )
    },
    envir = testthat::teardown_env()
)

Sys.setenv(
    OCULAR_RUNTIME_ROOT = .ocular_testthat_runtime_root,
    OCULAR_EXPORT_PARENT_DIR = .ocular_testthat_export_parent,
    TEST_OUTPUT_DIR = file.path(.ocular_testthat_runtime_root, "test_output"),
    DATA_DIR = .ocular_testthat_export_root,
    PROCESSED_DATA_DIR = file.path(
        .ocular_testthat_runtime_root,
        "Analytic Dataset"
    ),
    OUTPUT_DIR = file.path(.ocular_testthat_runtime_root, "Analysis"),
    RAW_DATA_DIR = file.path(.ocular_testthat_export_root, "Original Files"),
    TOOLS_OUTPUT_DIR = file.path(.ocular_testthat_runtime_root, "tools_output"),
    MERGED_TABLES_DIR = file.path(
        .ocular_testthat_runtime_root,
        "Analysis",
        "merged_tables"
    ),
    LOGS_DIR = file.path(.ocular_testthat_runtime_root, "logs")
)

if (!.ocular_testthat_logger_was_attached) {
    library(logger)
}
source(here::here("scripts", "utils", "logging_utilities.R"))
source(here::here("scripts", "load_all.R"))

.ocular_testthat_paths <- list(
    RUNTIME_ROOT = .ocular_testthat_runtime_root,
    EXPORT_PARENT_DIR = .ocular_testthat_export_parent,
    EXPORT_ROOT = .ocular_testthat_export_root,
    EXPORT_ANALYSIS_DIR = .ocular_testthat_export_analysis,
    TEST_OUTPUT_DIR = file.path(.ocular_testthat_runtime_root, "test_output"),
    DATA_DIR = .ocular_testthat_export_root,
    PROCESSED_DATA_DIR = file.path(
        .ocular_testthat_runtime_root,
        "Analytic Dataset"
    ),
    OUTPUT_DIR = file.path(.ocular_testthat_runtime_root, "Analysis"),
    RAW_DATA_DIR = file.path(.ocular_testthat_export_root, "Original Files"),
    TOOLS_OUTPUT_DIR = file.path(.ocular_testthat_runtime_root, "tools_output"),
    MERGED_TABLES_DIR = file.path(
        .ocular_testthat_runtime_root,
        "Analysis",
        "merged_tables"
    ),
    LOGS_DIR = file.path(.ocular_testthat_runtime_root, "logs")
)
for (.setting_name in names(.ocular_testthat_paths)) {
    assign(
        .setting_name,
        .ocular_testthat_paths[[.setting_name]],
        envir = .GlobalEnv
    )
}
