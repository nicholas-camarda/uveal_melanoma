publish_contract_dataset_ids <- c(
    "uveal_melanoma_full_cohort",
    "uveal_melanoma_restricted_cohort",
    "uveal_melanoma_gksrs_only_cohort"
)

install_publish_contract_fixture <- function(
    tmp_root,
    dataset_ids = publish_contract_dataset_ids,
    log_lines = c(
        "[INFO] === MAIN EXECUTION PHASE ===",
        "[WARNING] >>> ANALYSES COMPLETED WITH WARNINGS <<<",
        "[INFO] >>> Datasets analyzed: 3",
        "[INFO] >>> COMPLETED MAIN EXECUTION PHASE <<<"
    ),
    log_basename = "run_log_20260813_120000.txt"
) {
    runtime_root <- file.path(tmp_root, "runtime")
    output_root <- file.path(runtime_root, "Analysis")
    processed_root <- file.path(runtime_root, "Analytic Dataset")
    logs_root <- file.path(runtime_root, "logs")
    export_root <- file.path(tmp_root, "export")

    old_values <- mget(
        c(
            "RUNTIME_ROOT", "OUTPUT_DIR", "PROCESSED_DATA_DIR", "LOGS_DIR",
            "MERGED_TABLES_DIR", "EXPORT_ROOT", "EXPORT_ANALYSIS_DIR"
        ),
        envir = .GlobalEnv,
        inherits = TRUE
    )
    withr::defer({
        for (setting_name in names(old_values)) {
            assign(setting_name, old_values[[setting_name]], envir = .GlobalEnv)
        }
        unlink(tmp_root, recursive = TRUE, force = TRUE)
    }, envir = parent.frame())

    assign("RUNTIME_ROOT", runtime_root, envir = .GlobalEnv)
    assign("OUTPUT_DIR", output_root, envir = .GlobalEnv)
    assign("PROCESSED_DATA_DIR", processed_root, envir = .GlobalEnv)
    assign("LOGS_DIR", logs_root, envir = .GlobalEnv)
    assign("MERGED_TABLES_DIR", file.path(output_root, "merged_tables"), envir = .GlobalEnv)
    assign("EXPORT_ROOT", export_root, envir = .GlobalEnv)
    assign("EXPORT_ANALYSIS_DIR", file.path(export_root, "outputs"), envir = .GlobalEnv)

    dir.create(processed_root, recursive = TRUE, showWarnings = FALSE)
    analytic_data <- data.frame(
        id = 1:3,
        treatment_group = factor(c("PBT", "GKSRS", "PBT")),
        value = c(1.5, 2, NA_real_),
        notes = c("review", NA_character_, "complete")
    )
    for (dataset_id in dataset_ids) {
        saveRDS(analytic_data, file.path(processed_root, paste0(dataset_id, ".rds")))
        openxlsx::write.xlsx(
            analytic_data,
            file.path(processed_root, paste0(dataset_id, ".xlsx")),
            overwrite = TRUE
        )
    }

    log_path <- file.path(logs_root, "txt", log_basename)
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(log_lines, log_path)
    Sys.setFileTime(log_path, Sys.time() + 5)

    list(
        runtime_root = runtime_root,
        output_root = output_root,
        processed_root = processed_root,
        log_path = log_path,
        export_root = export_root
    )
}
