test_that("event/date reconciliation writes a single row-level review workbook into 00_General", {
    test_general_dir <- file.path(TEST_OUTPUT_DIR, "event_date_audit_logs", "uveal_full", "00_General")
    dir.create(test_general_dir, recursive = TRUE, showWarnings = FALSE)
    withr::defer(unlink(dirname(dirname(test_general_dir)), recursive = TRUE, force = TRUE), envir = parent.frame())

    log_path <- file.path(LOGS_DIR, "event_date_audit_test.txt")
    setup_logging(log_path = log_path, level = "INFO", progress = FALSE, context_in_file = TRUE)

    audit_input <- tibble::tibble(
        id = c(22, 45, 99),
        mets_progression = c("N", "N", "Y"),
        mets_progression_date = as.Date(c("2020-01-01", "2020-02-02", NA))
    )

    reconciliation_result <- fix_event_date_consistency(
        audit_input,
        event_var = "mets_progression",
        date_var = "mets_progression_date",
        id_col = "id",
        source_workbook = "unit_source.xlsx"
    )
    audit_paths <- write_event_date_reconciliation_audit(
        audit_rows = reconciliation_result$audit_rows,
        audit_summary = reconciliation_result$audit_summary,
        source_workbook = "unit_source.xlsx",
        id_column = "id",
        output_dir = test_general_dir,
        artifact_filename = "uveal_full_event_data_reconcilitation.xlsx"
    )

    expect_equal(reconciliation_result$data$mets_progression, c("Y", "Y", "N"))
    expect_true(file.exists(audit_paths$xlsx_path))

    audit_sheet <- readxl::read_xlsx(audit_paths$xlsx_path, sheet = "Reconciled_Changes")
    expect_true(all(c("study_id", "action_taken", "original_state", "reconciled_state") %in% names(audit_sheet)))
    expect_equal(sort(as.character(audit_sheet$study_id)), c("22", "45", "99"))
    expect_true(any(audit_sheet$action_taken == "set_event_to_yes_from_present_date"))
    expect_true(any(audit_sheet$action_taken == "set_event_to_no_and_clear_missing_date"))

    text_log_path <- file.path(dirname(log_path), "txt", basename(log_path))
    log_lines <- readLines(text_log_path, warn = FALSE)
    expect_true(any(grepl("Event/date reconciliation audit written to", log_lines, fixed = TRUE)))
    expect_true(any(grepl(audit_paths$xlsx_path, log_lines, fixed = TRUE)))
    expect_true(grepl("/00_General/", audit_paths$xlsx_path, fixed = TRUE))
    expect_match(basename(audit_paths$xlsx_path), "^uveal_full_event_data_reconcilitation\\.xlsx$")
})
