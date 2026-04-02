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
        artifact_filename = "uveal_full_event_data_reconcilitation.xlsx",
        manual_date_corrections = tibble::tibble(
            source_workbook = "unit_source.xlsx",
            id_column = "id",
            study_id = "22",
            column_name = "date_diagnosis",
            original_value = "2020-01-03",
            corrected_value = "2020-01-01",
            correction_reason = "Unit-test manual correction",
            action_taken = "manual_source_date_correction"
        )
    )

    expect_equal(reconciliation_result$data$mets_progression, c("Y", "Y", "N"))
    expect_true(file.exists(audit_paths$xlsx_path))

    audit_sheet <- readxl::read_xlsx(audit_paths$xlsx_path, sheet = "Reconciled_Changes")
    expect_true(all(c("study_id", "action_taken", "original_state", "reconciled_state") %in% names(audit_sheet)))
    expect_equal(sort(as.character(audit_sheet$study_id)), c("22", "45", "99"))
    expect_true(any(audit_sheet$action_taken == "set_event_to_yes_from_present_date"))
    expect_true(any(audit_sheet$action_taken == "set_event_to_no_and_clear_missing_date"))

    correction_sheet <- readxl::read_xlsx(audit_paths$xlsx_path, sheet = "Manual_Date_Corrections")
    expect_true("correction_reason" %in% names(correction_sheet))
    expect_equal(correction_sheet$study_id[[1]], "22")

    text_log_path <- file.path(dirname(log_path), "txt", basename(log_path))
    log_lines <- readLines(text_log_path, warn = FALSE)
    expect_true(any(grepl("Event/date reconciliation audit written to", log_lines, fixed = TRUE)))
    expect_true(any(grepl(audit_paths$xlsx_path, log_lines, fixed = TRUE)))
    expect_true(grepl("/00_General/", audit_paths$xlsx_path, fixed = TRUE))
    expect_match(basename(audit_paths$xlsx_path), "^uveal_full_event_data_reconcilitation\\.xlsx$")
})

test_that("manual date corrections apply configured raw-date overrides and emit an audit trail", {
    input_data <- tibble::tibble(
        id = c(125, 211),
        date_diagnosis = as.POSIXct(c("2017-05-16 00:00:00", "2024-01-28 00:00:00"), tz = "UTC"),
        date_ophtho_consult = as.POSIXct(c("2017-05-16 00:00:00", "2024-01-21 00:00:00"), tz = "UTC"),
        date_rad_onc_consult = as.POSIXct(c("2017-05-16 00:00:00", "2014-03-01 00:00:00"), tz = "UTC"),
        date_initial_liver_staging = as.POSIXct(c("2017-05-26 00:00:00", NA), tz = "UTC"),
        initial_gk_date = as.POSIXct(c("2007-06-13 00:00:00", NA), tz = "UTC"),
        initial_plaque_date = as.POSIXct(c(NA, "2014-03-31 00:00:00"), tz = "UTC"),
        last_followup = as.POSIXct(c("2024-09-24 00:00:00", "2019-08-01 00:00:00"), tz = "UTC")
    )

    correction_result <- apply_manual_date_corrections(
        input_data,
        corrections = MANUAL_DATE_CORRECTIONS,
        id_col = "id",
        source_workbook = "unit_source.xlsx"
    )

    expect_equal(as.Date(correction_result$data$initial_gk_date[correction_result$data$id == 125]), as.Date("2017-06-13"))
    expect_equal(as.Date(correction_result$data$date_diagnosis[correction_result$data$id == 211]), as.Date("2014-01-28"))
    expect_equal(as.Date(correction_result$data$date_ophtho_consult[correction_result$data$id == 211]), as.Date("2014-01-21"))
    expect_true(inherits(correction_result$data$date_diagnosis, "POSIXct"))
    expect_equal(nrow(correction_result$audit_rows), 3)
    expect_true(all(c(
        "study_id",
        "column_name",
        "original_value",
        "corrected_value",
        "confidence_tier",
        "supporting_columns",
        "supporting_values",
        "gap_improvement_days"
    ) %in% names(correction_result$audit_rows)))
    expect_true(all(correction_result$audit_rows$confidence_tier %in% c("high", "moderate")))
})
