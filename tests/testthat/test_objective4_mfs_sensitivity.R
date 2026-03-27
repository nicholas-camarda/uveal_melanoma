build_objective4_mfs_sensitivity_output_dirs <- function(test_output_dir) {
    list(
        obj4_mfs = file.path(test_output_dir, "04_GEP_Validation", "a_mfs_validation"),
        obj4_mss = file.path(test_output_dir, "04_GEP_Validation", "b_mss_validation"),
        obj4_ph_diagnostics = file.path(test_output_dir, "04_GEP_Validation", "c_proportional_hazards_diagnostics")
    )
}

test_that("Objective 4 MFS sensitivity collector builds follow-up and repeat-radiation summaries", {
    test_data <- create_test_dataset() %>%
        dplyr::mutate(
            recurrence1_treatment_clean = factor(
                c("GKSRS", "Plaque", rep(NA_character_, 18)),
                levels = c("GKSRS", "Plaque")
            ),
            initial_gk = replace(initial_gk, 1, "Y"),
            initial_plaque = replace(initial_plaque, 1, "Y")
        )

    sensitivity <- collect_objective4_mfs_sensitivity_results(
        data = test_data,
        dataset_name = "uveal_melanoma_full_cohort"
    )

    expect_true(all(c(
        "mfs_followup_sensitivity",
        "mfs_treatment_mix_sensitivity",
        "mfs_repeat_radiation_sensitivity",
        "guardrail_notes"
    ) %in% names(sensitivity)))

    horizon_overall <- sensitivity$mfs_followup_sensitivity$horizon_overall
    expect_equal(sum(horizon_overall$n), 20)
    expect_equal(
        horizon_overall$n[horizon_overall$five_year_followup_view == "event_by_5yr"],
        10
    )
    expect_equal(
        horizon_overall$n[horizon_overall$five_year_followup_view == "followup_ge_5yr"],
        2
    )
    expect_equal(
        horizon_overall$n[horizon_overall$five_year_followup_view == "censored_pre_5yr"],
        8
    )

    repeat_quality <- sensitivity$mfs_repeat_radiation_sensitivity$data_quality_checks
    expect_equal(repeat_quality$repeat_radiation_exposure_n, 2)
    expect_equal(repeat_quality$both_initial_modalities_n, 1)

    pbt_only <- sensitivity$mfs_treatment_mix_sensitivity$pbt_only
    expect_true(nrow(pbt_only) > 0)
    expect_equal(sum(pbt_only$n), 10)
})

test_that("Shared Objective 4 follow-up helper derives endpoint-specific counts and definition text", {
    test_data <- create_test_dataset() %>%
        dplyr::mutate(
            mss_event_5yr = as.integer(.data$melanoma_death_event == 1 & .data$tt_death_months <= 60),
            mss_event_7yr = as.integer(.data$melanoma_death_event == 1 & .data$tt_death_months <= 84),
            mss_event_10yr = as.integer(.data$melanoma_death_event == 1 & .data$tt_death_months <= 120)
        )

    mfs_followup <- collect_objective4_endpoint_followup_summary(
        data = test_data,
        dataset_name = "uveal_melanoma_full_cohort",
        eligibility_filter = "mfs_analysis_eligible",
        event_prefix = "mfs",
        time_horizon_years = 5
    )
    mss_followup <- collect_objective4_endpoint_followup_summary(
        data = test_data,
        dataset_name = "uveal_melanoma_full_cohort",
        eligibility_filter = "mss_analysis_eligible",
        event_prefix = "mss",
        time_horizon_years = 5
    )

    prepared <- refresh_gep_analysis_flags(test_data)
    expected_mfs <- prepared %>%
        dplyr::filter(.data$mfs_analysis_eligible) %>%
        dplyr::mutate(
            expected_view = dplyr::case_when(
                .data$mfs_event_5yr == 1 ~ "event_by_5yr",
                .data$tt_mets_months >= 60 ~ "followup_ge_5yr",
                TRUE ~ "censored_pre_5yr"
            )
        ) %>%
        dplyr::count(.data$expected_view, name = "n") %>%
        dplyr::arrange(.data$expected_view)
    expected_mss <- prepared %>%
        dplyr::filter(.data$mss_analysis_eligible) %>%
        dplyr::mutate(
            expected_view = dplyr::case_when(
                .data$melanoma_death_event == 1 & .data$tt_death_months <= 60 ~ "event_by_5yr",
                .data$tt_death_months >= 60 ~ "followup_ge_5yr",
                TRUE ~ "censored_pre_5yr"
            )
        ) %>%
        dplyr::count(.data$expected_view, name = "n") %>%
        dplyr::arrange(.data$expected_view)

    actual_mfs <- mfs_followup$horizon_overall %>%
        dplyr::select(expected_view = horizon_followup_view, n) %>%
        dplyr::arrange(.data$expected_view)
    actual_mss <- mss_followup$horizon_overall %>%
        dplyr::select(expected_view = horizon_followup_view, n) %>%
        dplyr::arrange(.data$expected_view)

    expect_equal(actual_mfs, expected_mfs)
    expect_equal(actual_mss, expected_mss)

    block_lines <- build_objective4_followup_limitation_block(mfs_followup)
    expect_true(any(grepl("FOLLOW-UP LIMITATION", block_lines, fixed = TRUE)))
    expect_true(any(grepl("`followup_ge_5yr` means", block_lines, fixed = TRUE)))
    expect_true(any(grepl("`censored_pre_5yr` means", block_lines, fixed = TRUE)))
    expect_true(any(grepl("Among the", block_lines, fixed = TRUE)))
})

test_that("Shared Objective 4 follow-up helper escalates the impact line for heavy class-imbalanced censoring", {
    stressed_data <- create_test_dataset() %>%
        dplyr::mutate(
            tt_mets_months = c(rep(24, 8), rep(72, 2), rep(24, 10)),
            mets_event = c(rep(0, 10), rep(1, 10)),
            mfs_event_5yr = c(rep(0, 10), rep(1, 10)),
            mfs_event_7yr = .data$mfs_event_5yr,
            mfs_event_10yr = .data$mfs_event_5yr
        )

    followup_summary <- collect_objective4_endpoint_followup_summary(
        data = stressed_data,
        dataset_name = "uveal_melanoma_full_cohort",
        eligibility_filter = "mfs_analysis_eligible",
        event_prefix = "mfs",
        time_horizon_years = 5
    )
    block_lines <- build_objective4_followup_limitation_block(followup_summary)

    expect_identical(followup_summary$impact_level, "high")
    expect_true(any(grepl("may be unstable", block_lines, fixed = TRUE)))
    expect_true(any(grepl("Class 1 had more pre-5-year censoring than Class 2", block_lines, fixed = TRUE)))
})

test_that("Objective 4 MFS sensitivity guardrail reports PBT rows inside gksrs-only cohorts", {
    test_data <- create_test_dataset() %>%
        dplyr::mutate(
            recurrence1_treatment_clean = factor(
                rep(NA_character_, 20),
                levels = c("GKSRS", "Plaque")
            )
        )

    sensitivity <- collect_objective4_mfs_sensitivity_results(
        data = test_data,
        dataset_name = "uveal_melanoma_gksrs_only_cohort"
    )

    expect_true(any(grepl(
        "PBT-labeled MFS-eligible row",
        sensitivity$guardrail_notes$note,
        fixed = TRUE
    )))
})

test_that("Simple MFS plot surfaces cohort and class event annotations", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_simple_plot_annotations")
    output_dirs <- build_objective4_mfs_sensitivity_output_dirs(test_output_dir)
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

    test_data <- create_test_dataset() %>%
        dplyr::mutate(
            recurrence1_treatment_clean = factor(
                rep("GKSRS", 20),
                levels = c("GKSRS", "Plaque")
            )
        )

    results <- simple_gep_validation(
        test_data,
        output_dirs,
        "annot_",
        dataset_name = "uveal_melanoma_full_cohort"
    )

    expect_true("plot_x_label" %in% names(results$mfs_results))
    expect_true(all(grepl("5-year mets:", results$mfs_results$plot_x_label, fixed = TRUE)))
    expect_true(all(grepl("Tx mix:", results$mfs_results$plot_x_label, fixed = TRUE)))

    plot_obj <- build_simple_gep_plot(
        results$mfs_results,
        "5-Year MFS: Expected vs Actual Rates",
        cohort_label = "Full Cohort"
    )
    expect_match(plot_obj$labels$subtitle, "Full Cohort")
    expect_gte(as.numeric(plot_obj$theme$plot.margin[[3]]), 20)

    x_scale <- Filter(function(scale) "x" %in% scale$aesthetics, plot_obj$scales$scales)[[1]]
    expect_true(any(grepl("5-year mets:", unname(x_scale$labels), fixed = TRUE)))
})

test_that("MFS calibration caption includes cohort and class event counts", {
    caption <- build_objective4_mfs_calibration_caption(
        results = list(
            simple_class_summary_5yr = data.frame(
                gep_class_simple = c("Class 1", "Class 2"),
                observed_events_5yr = c(1, 5),
                n = c(10, 8),
                stringsAsFactors = FALSE
            )
        ),
        dataset_name = "uveal_melanoma_full_cohort"
    )

    expect_match(caption, "Full Cohort")
    expect_match(caption, "Class 1: 1/10")
    expect_match(caption, "Class 2: 5/8")
})

test_that("Objective 4 MFS sensitivity writer saves workbook and summary text", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_mfs_sensitivity_writer")
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

    sensitivity <- collect_objective4_mfs_sensitivity_results(
        data = create_test_dataset(),
        dataset_name = "uveal_melanoma_full_cohort"
    )

    paths <- write_objective4_mfs_sensitivity_outputs(
        sensitivity_results = sensitivity,
        output_dir = test_output_dir,
        prefix = "writer_"
    )

    expect_true(file.exists(paths$workbook))
    expect_true(file.exists(paths$summary))

    workbook_sheets <- readxl::excel_sheets(paths$workbook)
    expect_true(all(c(
        "Followup_Operational",
        "Followup_5yr",
        "TxMix_ByClass",
        "Repeat_Comparison",
        "Guardrail_Notes"
    ) %in% workbook_sheets))

    summary_text <- paste(readLines(paths$summary, warn = FALSE), collapse = "\n")
    expect_match(summary_text, "`followup_ge_5yr` means")
    expect_match(summary_text, "EVENT-ROW DIAGNOSTICS")
    expect_match(summary_text, "event row IDs:")
})
