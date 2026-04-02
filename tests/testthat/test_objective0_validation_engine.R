make_objective0_validation_dataset <- function() {
    tibble::tibble(
        id = 1:3,
        treatment_group = factor(c("PBT", "GKSRS", "PBT"), levels = c("PBT", "GKSRS")),
        age_at_diagnosis = c(55, 63, 70),
        age_at_diagnosis_binned = factor(c("Younger", "Older", "Older")),
        age_at_diagnosis_general_pop_median = factor(c("Younger", "Older", "Older")),
        sex = factor(c("Female", "Male", "Female"), levels = c("Female", "Male")),
        location = factor(c("Choroidal", "Ciliary Body", "Choroidal"), levels = c("Choroidal", "Ciliary Body")),
        initial_tumor_height = c(5, 6, 7),
        initial_tumor_diameter = c(12, 14, 16),
        initial_t_stage_simple = factor(c("T1", "T2", "T2"), levels = c("T1", "T2", "T3", "T4")),
        recurrence1 = factor(c("No", "Yes", "No"), levels = c("No", "Yes")),
        mets_progression = factor(c("No", "No", "Yes"), levels = c("No", "Yes")),
        last_known_alive_date = as.Date(c("2025-02-01", "2025-02-15", "2025-03-01")),
        last_known_alive_source = c("last_height_date", "date_diagnosis", "dod"),
        treatment_date = as.Date(c("2020-01-01", "2020-01-15", "2020-02-01")),
        date_diagnosis = as.Date(c("2019-12-15", "2020-01-01", "2020-01-20")),
        dob = as.Date(c("1965-01-01", "1960-01-01", "1955-01-01")),
        initial_tumor_height_binned = factor(c("<=10", "<=10", "<=10")),
        initial_tumor_diameter_binned = factor(c("<=20", "<=20", "<=20")),
        initial_stage_binary = factor(c("Stage I-III", "Stage I-III", "Stage I-III"), levels = c("Stage I-III", "Stage IV")),
        gep_class_simple = factor(c("Class 1", "Class 2", "Class 1"), levels = c("Class 1", "Class 2", "GEP Failed/Indeterminate", "GEP Not Tested")),
        prame_status = factor(c("Negative", "Positive", "Negative"), levels = c("Negative", "Positive", "Unknown", "Not Available")),
        gep12_prame_status = factor(c("Negative", "Positive", "Negative"), levels = c("Negative", "Positive")),
        recurrence1_treatment_clean = factor(c("No recurrence", "GKSRS", "No recurrence")),
        biopsy1_gep = factor(
            c("Class 1 PRAME Negative", "Class 2 PRAME Positive", "Class 1 PRAME Negative"),
            levels = c(
                "Class 1 PRAME Negative",
                "Class 1 PRAME Positive",
                "Class 2 PRAME Negative",
                "Class 2 PRAME Positive",
                "GEP Failed/Indeterminate",
                "GEP Not Tested"
            )
        ),
        tt_recurrence_months = c(12, 18, 24),
        tt_mets_months = c(20, 22, 10),
        tt_death_months = c(40, 36, 30),
        death_event = c(0, 0, 1),
        consort_group = c("eligible_both", "eligible_both", "eligible_both"),
        optic_nerve = factor(c("No", "No", "No"), levels = c("No", "Yes"))
    )
}

test_that("structured validation result treats warnings as non-blocking", {
    validation_result <- validate_processing_pipeline(
        make_objective0_validation_dataset(),
        stop_on_failure = FALSE
    )

    expect_true(validation_result$success)
    expect_true("validation_findings" %in% names(validation_result))
    expect_true(all(c("check_id", "scope", "cohort", "severity", "status", "metric", "value", "message") %in% names(validation_result$validation_findings)))
    expect_true(any(validation_result$validation_findings$severity == "warning"))
})

test_that("structured validation result blocks hard errors", {
    duplicate_id_data <- make_objective0_validation_dataset()
    duplicate_id_data$id[3] <- duplicate_id_data$id[1]

    validation_result <- validate_processing_pipeline(
        duplicate_id_data,
        stop_on_failure = FALSE
    )

    expect_false(validation_result$success)
    expect_true(validation_result$has_hard_errors)
    expect_true(any(validation_result$validation_findings$check_id == "duplicate_patient_ids"))
    expect_true(any(validation_result$validation_findings$severity == "hard_error"))
})

test_that("last_known_alive_source is treated as provenance text, not a date field", {
    validation_result <- validate_processing_pipeline(
        make_objective0_validation_dataset(),
        stop_on_failure = FALSE
    )

    date_storage_finding <- validation_result$validation_findings %>%
        dplyr::filter(.data$check_id == "date_columns_are_date_like")

    expect_equal(date_storage_finding$status[[1]], "pass")
    expect_false(grepl("last_known_alive_source", date_storage_finding$value[[1]] %||% ""))
})

test_that("minor treatment-before-diagnosis gaps are warnings, not hard errors", {
    minor_gap_data <- make_objective0_validation_dataset()
    minor_gap_data$treatment_date[2] <- as.Date("2019-12-30")
    minor_gap_data$date_diagnosis[2] <- as.Date("2020-01-01")

    validation_result <- validate_processing_pipeline(
        minor_gap_data,
        stop_on_failure = FALSE
    )

    expect_true(validation_result$success)
    expect_true(any(validation_result$validation_findings$check_id == "treatment_before_diagnosis_minor_gap"))
    expect_false(any(
        validation_result$validation_findings$check_id == "treatment_after_diagnosis" &
            validation_result$validation_findings$status == "fail"
    ))
})

test_that("major treatment-before-diagnosis gaps remain hard errors", {
    major_gap_data <- make_objective0_validation_dataset()
    major_gap_data$treatment_date[2] <- as.Date("2019-10-01")
    major_gap_data$date_diagnosis[2] <- as.Date("2020-01-01")

    validation_result <- validate_processing_pipeline(
        major_gap_data,
        stop_on_failure = FALSE
    )

    expect_false(validation_result$success)
    expect_true(any(
        validation_result$validation_findings$check_id == "treatment_after_diagnosis" &
            validation_result$validation_findings$status == "fail"
    ))
})

test_that("Objective 0 validation artifacts are written into 00_General", {
    output_root <- tempfile("objective0-validation-bundle-")
    dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
    withr::defer(unlink(output_root, recursive = TRUE, force = TRUE), envir = parent.frame())

    output_dirs <- list(
        full_cohort = list(
            baseline_characteristics = file.path(output_root, "uveal_full", "00_General", "baseline_characteristics")
        ),
        restricted_cohort = list(
            baseline_characteristics = file.path(output_root, "uveal_restricted", "00_General", "baseline_characteristics")
        ),
        gksrs_only_cohort = list(
            baseline_characteristics = file.path(output_root, "gksrs", "00_General", "baseline_characteristics")
        )
    )

    validation_result <- validate_processing_pipeline(
        make_objective0_validation_dataset(),
        stop_on_failure = FALSE
    )
    reconciliation_audit <- list(
        audit_summary = tibble::tibble(
            source_workbook = "unit.xlsx",
            id_column = "id",
            event_var = "mets_progression",
            date_var = "mets_progression_date",
            records_with_present_date = 1L,
            records_marked_event_yes_after = 1L,
            n_event_set_to_yes = 0L,
            n_event_set_to_no_missing_date = 0L,
            n_rows_reconciled = 0L
        ),
        audit_rows = empty_event_date_audit_rows(),
        manual_date_corrections = tibble::tibble(
            source_workbook = "unit.xlsx",
            id_column = "id",
            study_id = "11",
            column_name = "date_diagnosis",
            original_value = "2020-01-01",
            corrected_value = "2010-01-01",
            correction_reason = "Unit-test audit row",
            action_taken = "manual_source_date_correction"
        )
    )

    written_paths <- write_objective0_validation_artifacts(
        validation_result = validation_result,
        output_dirs = output_dirs,
        reconciliation_audit = reconciliation_audit
    )

    expect_true(file.exists(written_paths$full_cohort$summary_path))
    expect_true(file.exists(written_paths$full_cohort$bundle_path))

    bundle_sheets <- readxl::excel_sheets(written_paths$full_cohort$bundle_path)
    expect_true(all(c(
        "Validation_Summary",
        "Validation_Findings",
        "Critical_Variable_Checks",
        "Factor_Level_Checks",
        "Cohort_Rule_Checks",
        "Data_Quality_Checks",
        "Reconciliation_Summary",
        "Manual_Date_Corrections"
    ) %in% bundle_sheets))
})
