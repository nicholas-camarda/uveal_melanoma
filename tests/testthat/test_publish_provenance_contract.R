test_that("unfiltered publishing includes three authoritative RDS and three review XLSX files", {
    fixture <- install_publish_contract_fixture(tempfile("publish-contract-"))
    dir.create(file.path(fixture$output_root, "uveal_full", "01_Efficacy"), recursive = TRUE)
    writeLines("report", file.path(fixture$output_root, "uveal_full", "01_Efficacy", "summary.xlsx"))

    result <- publish_outputs(
        snapshot_id = "2026-08-13-contract",
        include_merged_tables = FALSE,
        dry_run = TRUE
    )
    planned <- result$manifest[result$manifest$status == "would_copy", , drop = FALSE]
    analytic <- planned[basename(dirname(planned$destination_path)) == "analytic_data", , drop = FALSE]

    expect_equal(nrow(analytic), 6L)
    expect_setequal(
        basename(analytic$source_path),
        as.vector(outer(publish_contract_dataset_ids, c(".rds", ".xlsx"), paste0))
    )
    expect_true(any(basename(planned$destination_path) == basename(fixture$log_path)))
    expect_equal(result$summary$analytic_data_files, 6L)
    expect_identical(
        result$latest_analysis_log,
        normalizePath(fixture$log_path, winslash = "/", mustWork = TRUE)
    )
})

test_that("cohort filtering includes the matching analytic-data pair", {
    fixture <- install_publish_contract_fixture(tempfile("publish-filtered-"))
    dir.create(file.path(fixture$output_root, "uveal_restricted", "01_Efficacy"), recursive = TRUE)
    writeLines("report", file.path(fixture$output_root, "uveal_restricted", "01_Efficacy", "summary.xlsx"))

    result <- publish_outputs(
        cohorts = "uveal_melanoma_restricted_cohort",
        snapshot_id = "2026-08-13-filtered",
        include_merged_tables = FALSE,
        dry_run = TRUE
    )
    planned <- result$manifest$source_path[result$manifest$status == "would_copy"]

    expect_setequal(
        basename(planned[grepl("Analytic Dataset", planned, fixed = TRUE)]),
        c("uveal_melanoma_restricted_cohort.rds", "uveal_melanoma_restricted_cohort.xlsx")
    )
})

test_that("publishing rejects a mismatched analytic review workbook", {
    fixture <- install_publish_contract_fixture(tempfile("publish-mismatch-"))
    dir.create(file.path(fixture$output_root, "uveal_full", "01_Efficacy"), recursive = TRUE)
    writeLines("report", file.path(fixture$output_root, "uveal_full", "01_Efficacy", "summary.xlsx"))
    openxlsx::write.xlsx(
        data.frame(id = 999L, treatment_group = "PBT", value = 1, notes = "wrong"),
        file.path(fixture$processed_root, "uveal_melanoma_full_cohort.xlsx"),
        overwrite = TRUE
    )

    expect_error(
        publish_outputs(
            cohorts = "uveal_melanoma_full_cohort",
            snapshot_id = "2026-08-13-mismatch",
            include_merged_tables = FALSE,
            dry_run = TRUE
        ),
        regexp = "does not match its authoritative RDS"
    )
})

test_that("publishing rejects a missing selected analytic file", {
    fixture <- install_publish_contract_fixture(tempfile("publish-missing-"))
    dir.create(file.path(fixture$output_root, "uveal_full", "01_Efficacy"), recursive = TRUE)
    writeLines("report", file.path(fixture$output_root, "uveal_full", "01_Efficacy", "summary.xlsx"))
    unlink(file.path(fixture$processed_root, "uveal_melanoma_full_cohort.xlsx"))

    expect_error(
        publish_outputs(
            cohorts = "uveal_melanoma_full_cohort",
            snapshot_id = "2026-08-13-missing",
            include_merged_tables = FALSE,
            dry_run = TRUE
        ),
        regexp = "required analytic data file is missing or unreadable"
    )
})

test_that("publishing rejects the newest failed full attempt instead of falling back", {
    fixture <- install_publish_contract_fixture(tempfile("publish-failed-log-"))
    dir.create(file.path(fixture$output_root, "uveal_full", "01_Efficacy"), recursive = TRUE)
    writeLines("report", file.path(fixture$output_root, "uveal_full", "01_Efficacy", "summary.xlsx"))
    failed_log <- file.path(LOGS_DIR, "txt", "run_log_20260813_130000.txt")
    writeLines(
        c("[INFO] === MAIN EXECUTION PHASE ===", "[ERROR] Execution halted"),
        failed_log
    )
    Sys.setFileTime(failed_log, Sys.time() + 10)

    expect_error(
        publish_outputs(
            cohorts = "uveal_melanoma_full_cohort",
            snapshot_id = "2026-08-13-failed-log",
            include_merged_tables = FALSE,
            dry_run = TRUE
        ),
        regexp = "newest full analysis attempt.*failed or is incomplete"
    )
})

test_that("publishing rejects a partial newest full attempt", {
    fixture <- install_publish_contract_fixture(tempfile("publish-partial-log-"))
    dir.create(file.path(fixture$output_root, "uveal_full", "01_Efficacy"), recursive = TRUE)
    writeLines("report", file.path(fixture$output_root, "uveal_full", "01_Efficacy", "summary.xlsx"))
    partial_log <- file.path(LOGS_DIR, "txt", "run_log_20260813_140000.txt")
    writeLines(
        c("[INFO] === MAIN EXECUTION PHASE ===", "[INFO] Starting Objective 1"),
        partial_log
    )

    expect_error(
        publish_outputs(
            cohorts = "uveal_melanoma_full_cohort",
            snapshot_id = "2026-08-13-partial-log",
            include_merged_tables = FALSE,
            dry_run = TRUE
        ),
        regexp = "newest full analysis attempt.*failed or is incomplete"
    )
})

test_that("successful completed full runs are valid publish provenance", {
    fixture <- install_publish_contract_fixture(
        tempfile("publish-success-log-"),
        log_lines = c(
            "[INFO] === MAIN EXECUTION PHASE ===",
            "[INFO] >>> ALL ANALYSES COMPLETED SUCCESSFULLY!",
            "[INFO] >>> Datasets analyzed: 3",
            "[INFO] >>> COMPLETED MAIN EXECUTION PHASE <<<"
        )
    )
    dir.create(file.path(fixture$output_root, "uveal_full", "01_Efficacy"), recursive = TRUE)
    writeLines("report", file.path(fixture$output_root, "uveal_full", "01_Efficacy", "summary.xlsx"))

    result <- publish_outputs(
        cohorts = "uveal_melanoma_full_cohort",
        snapshot_id = "2026-08-13-success-log",
        include_merged_tables = FALSE,
        dry_run = TRUE
    )

    expect_identical(result$summary$analysis_log_basename, basename(fixture$log_path))
})

test_that("publishing rejects selected files newer than the completed full-run log", {
    fixture <- install_publish_contract_fixture(tempfile("publish-stale-log-"))
    output_path <- file.path(fixture$output_root, "uveal_full", "01_Efficacy", "summary.xlsx")
    dir.create(dirname(output_path), recursive = TRUE)
    writeLines("report", output_path)
    Sys.setFileTime(output_path, Sys.time() + 20)

    expect_error(
        publish_outputs(
            cohorts = "uveal_melanoma_full_cohort",
            snapshot_id = "2026-08-13-stale-log",
            include_merged_tables = FALSE,
            dry_run = TRUE
        ),
        regexp = "newer than the validated analysis log"
    )
})
