source(here::here("scripts", "tools", "peer_review_followup_audit.R"))

test_that("peer-review follow-up audit derives latest VA follow-up from last follow-up", {
    data <- create_test_dataset() %>%
        dplyr::mutate(
            treatment_date = as.Date("2020-01-01"),
            last_followup = as.Date("2024-01-01"),
            follow_up_months = 48,
            follow_up_years = 4
        )

    audit <- build_peer_review_followup_audit(data, "test", raw_data_dir = RAW_DATA_DIR)

    expect_true("followup_availability" %in% names(audit))
    expect_true("latest_vision_followup_months" %in% audit$followup_availability$field)
    latest_va_row <- audit$followup_availability %>%
        dplyr::filter(.data$field == "latest_vision_followup_months")
    expect_true(latest_va_row$present)
    expect_gt(latest_va_row$median_value, 47)
    expect_equal(audit$data_profile$latest_va_followup_36mo_n, nrow(data))
})

test_that("peer-review audit records radiation details and absent dosimetry fields", {
    data <- create_test_dataset() %>%
        dplyr::mutate(
            treatment_date = as.Date("2020-01-01"),
            last_followup = as.Date("2022-01-01")
        )

    radiation <- summarize_peer_review_radiation_availability(data)

    expect_true("optic_nerve" %in% radiation$field)
    expect_true(radiation$present[radiation$field == "optic_nerve"])
    expect_true("dose_to_optic_nerve" %in% radiation$field)
    expect_false(radiation$present[radiation$field == "dose_to_optic_nerve"])
})

test_that("restricted-cohort audit includes optic nerve abutment check", {
    data <- create_test_dataset() %>%
        dplyr::mutate(
            initial_tumor_diameter = 10,
            initial_tumor_height = 5,
            optic_nerve = "Not Involved"
        )

    passed <- summarize_restricted_cohort_eligibility(data)
    expect_true("restricted_optic_nerve_status" %in% passed$check)
    expect_equal(
        passed$status[passed$check == "restricted_optic_nerve_status"],
        "passed"
    )

    data$optic_nerve[1] <- "Involved"
    failed <- summarize_restricted_cohort_eligibility(data)
    expect_equal(
        failed$status[failed$check == "restricted_optic_nerve_status"],
        "failed"
    )
    expect_equal(
        failed$n_violations[failed$check == "restricted_optic_nerve_status"],
        1L
    )
})

test_that("peer-review follow-up audit writes expected workbook sheets", {
    data <- create_test_dataset() %>%
        dplyr::mutate(
            treatment_date = as.Date("2020-01-01"),
            last_followup = as.Date("2021-01-01")
        )
    raw_dir <- tempfile("raw-source-")
    dir.create(raw_dir, recursive = TRUE)
    openxlsx::write.xlsx(
        list(Sheet1 = data.frame(last_followup = as.Date("2021-01-01"), last_vision = 0.3)),
        file.path(raw_dir, "active_stats.xlsx")
    )
    openxlsx::write.xlsx(
        list(Sheet1 = data.frame(Distance.of.Tumor.from.Optic.Nerve = "2DD")),
        file.path(raw_dir, "non_authoritative_old_file.xlsx")
    )

    audit <- build_peer_review_followup_audit(
        data,
        "test",
        raw_data_dir = raw_dir,
        input_filename = "active_stats.xlsx"
    )
    path <- file.path(TEST_OUTPUT_DIR, "peer_review_followup_audit.xlsx")
    write_peer_review_followup_audit(audit, path)

    expect_workbook_has_sheets(
        path,
        c(
            "evidence_boundary",
            "data_profile",
            "followup_availability",
            "radiation_availability",
            "restricted_eligibility_check",
            "curated_input_workbook_columns"
        )
    )
    expect_true("last_followup" %in% audit$curated_input_workbook_columns$column_name)
    expect_false("Distance.of.Tumor.from.Optic.Nerve" %in% audit$curated_input_workbook_columns$column_name)
    expect_true(any(audit$evidence_boundary$evidence_source == "other_raw_folder_workbooks"))
    expect_false(audit$evidence_boundary$included_in_audit[
        audit$evidence_boundary$evidence_source == "other_raw_folder_workbooks"
    ])
})

test_that("peer-review on-demand audit tools are not sourced by load_all", {
    load_all_text <- readLines(here::here("scripts", "load_all.R"), warn = FALSE)

    expect_false(any(grepl("peer_review_followup_audit\\.R", load_all_text)))
    expect_false(any(grepl("propensity_score_feasibility\\.R", load_all_text)))
})
