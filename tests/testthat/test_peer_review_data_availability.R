source(here::here("scripts", "tools", "peer_review_followup_audit.R"))

test_that("peer-review audit reuses the analytic latest-VA timing definition", {
    data <- tibble::tibble(
        treatment_date = as.Date("2020-01-31"),
        last_followup = as.Date("2020-02-29"),
        follow_up_months = 1
    )

    analytic_timing <- add_last_vision_followup_months(data)
    audit_timing <- add_peer_review_latest_va_timing(data)

    expect_equal(
        audit_timing$explicit_latest_va_followup_months,
        analytic_timing$last_vision_followup_months_explicit
    )
    expect_equal(
        audit_timing$proxy_latest_va_followup_months,
        analytic_timing$last_vision_followup_months_proxy
    )
})

test_that("treatment-arm follow-up summary uses treatment-anchored survival time", {
    data <- tibble::tibble(
        treatment_group = factor(c("PBT", "PBT", "GKSRS", "GKSRS")),
        follow_up_months = c(100, 200, 300, 400),
        tt_death_months = c(12, 24, 30, 42),
        treatment_date = as.Date("2020-01-01"),
        last_followup = as.Date("2021-01-01")
    )

    summary <- summarize_followup_by_treatment_arm(data)
    treatment_followup <- summary %>%
        dplyr::filter(.data$variable == "tt_death_months")

    expect_equal(nrow(treatment_followup), 2L)
    expect_equal(
        treatment_followup$median[as.character(treatment_followup$treatment_group) == "PBT"],
        18
    )
    expect_equal(
        treatment_followup$median[as.character(treatment_followup$treatment_group) == "GKSRS"],
        36
    )
    expect_false("follow_up_months" %in% summary$variable)
})

test_that("peer-review follow-up audit separates explicit and proxy latest-VA timing", {
    data <- create_test_dataset() %>%
        dplyr::mutate(
            treatment_date = as.Date("2020-01-01"),
            last_followup = as.Date("2024-01-01"),
            follow_up_months = 48,
            follow_up_years = 4
        )
    data$last_followup[1] <- NA

    audit <- build_peer_review_followup_audit(data, "test", raw_data_dir = RAW_DATA_DIR)

    expect_true("followup_availability" %in% names(audit))
    expect_true("latest_va_timing_sources" %in% names(audit))
    expect_true("explicit_latest_va_followup_months" %in% audit$followup_availability$field)
    expect_true("proxy_latest_va_followup_months" %in% audit$followup_availability$field)
    latest_va_row <- audit$followup_availability %>%
        dplyr::filter(.data$field == "explicit_latest_va_followup_months")
    expect_true(latest_va_row$present)
    expect_gt(latest_va_row$median_value, 47)
    proxy_row <- audit$followup_availability %>%
        dplyr::filter(.data$field == "proxy_latest_va_followup_months")
    expect_equal(proxy_row$non_missing_n, nrow(data))
    expect_equal(
        audit$latest_va_timing_sources$n_patients[
            audit$latest_va_timing_sources$timing_definition == "recovered_by_proxy_when_explicit_missing"
        ],
        1L
    )
    expect_equal(audit$data_profile$proxy_latest_va_followup_36mo_n, nrow(data))
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

test_that("peer-review audit formats one or more local paths as markdown file links", {
    paths <- file.path(TEST_OUTPUT_DIR, c("first workbook.xlsx", "second workbook.xlsx"))

    links <- format_markdown_file_link(paths)

    expect_length(links, 2)
    expect_true(all(grepl("^\\[.*\\]\\(file://", links)))
    expect_true(any(grepl("first%20workbook\\.xlsx", links)))
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
        input_filename = "active_stats.xlsx",
        cohort_path = file.path(TEST_OUTPUT_DIR, "test_cohort.rds"),
        output_path = file.path(TEST_OUTPUT_DIR, "peer_review_followup_audit.xlsx")
    )
    path <- file.path(TEST_OUTPUT_DIR, "peer_review_followup_audit.xlsx")
    write_peer_review_followup_audit(audit, path)

    expect_workbook_has_sheets(
        path,
        c(
            "evidence_boundary",
            "data_profile",
            "clickable_paths",
            "followup_availability",
            "followup_by_treatment_arm",
            "latest_va_timing_sources",
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
    expect_true(all(c("path_role", "path", "markdown_link") %in% names(audit$clickable_paths)))
    expect_true(any(grepl("^\\[.*\\]\\(file://", audit$clickable_paths$markdown_link)))
    expect_true(any(audit$clickable_paths$path_role == "audit_workbook_output"))
})

test_that("peer-review audit tools and production propensity analysis are routed correctly", {
    load_all_text <- readLines(here::here("scripts", "load_all.R"), warn = FALSE)

    expect_false(any(grepl("peer_review_followup_audit\\.R", load_all_text)))
    expect_false(any(grepl("tools.*propensity_score_feasibility\\.R", load_all_text)))
    expect_true(any(grepl("analysis.*propensity_score_sensitivity\\.R", load_all_text)))
    expect_false(file.exists(here::here("scripts", "tools", "propensity_score_feasibility.R")))
})
