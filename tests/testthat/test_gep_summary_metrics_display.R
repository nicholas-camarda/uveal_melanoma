test_that("Comprehensive GEP summary displays current robust discrimination metrics", {
    mock_validation_results <- list(
        "5yr" = list(
            calibration = list(
                n = 100,
                nam_dagostino_p = 0.05,
                ici = 0.10,
                slope = 0.95
            ),
            discrimination = list(
                n = 100,
                events = 25,
                harrell_c = 0.75,
                integrated_auc = 0.78,
                cumulative_discrimination = 0.81,
                time_averaged_discrimination = 0.79
            )
        )
    )

    summary_text <- create_comprehensive_gep_summary(
        validation_results = mock_validation_results,
        outcome_type = "MFS",
        prame_analysis = NULL,
        missing_data_analysis = list(missing_patterns = data.frame(pattern = character())),
        dataset_name = "unit_test_dataset"
    )

    expect_true(grepl("Harrell's C 0.750", summary_text, fixed = TRUE))
    expect_true(grepl("Integrated AUC 0.780", summary_text, fixed = TRUE))
    expect_true(grepl("Cumulative Disc 0.810", summary_text, fixed = TRUE))
    expect_false(grepl("Uno's C", summary_text))
    expect_false(grepl("AUC=0.780[^\\n]*Uno", summary_text))
})

test_that("Comprehensive GEP summary includes the compact follow-up limitation block for MFS and MSS", {
    mock_validation_results <- list(
        "5yr" = list(
            calibration = list(
                n = 100,
                nam_dagostino_p = 0.05,
                ici = 0.10,
                slope = 0.95
            ),
            discrimination = list(
                n = 100,
                events = 25,
                harrell_c = 0.75,
                integrated_auc = 0.78,
                cumulative_discrimination = 0.81,
                time_averaged_discrimination = 0.79
            )
        )
    )

    shared_missing <- list(
        missing_patterns = data.frame(pattern = character(), stringsAsFactors = FALSE)
    )
    full_source_data <- create_test_dataset() %>%
        dplyr::mutate(
            follow_up_years = as.numeric(1:20),
            consort_group = rep(c("eligible_both", "gksrs_only"), each = 10),
            mss_event_5yr = as.integer(.data$melanoma_death_event == 1 & .data$tt_death_months <= 60),
            mss_event_7yr = as.integer(.data$melanoma_death_event == 1 & .data$tt_death_months <= 84),
            mss_event_10yr = as.integer(.data$melanoma_death_event == 1 & .data$tt_death_months <= 120)
        )
    gksrs_source_data <- full_source_data %>%
        dplyr::filter(.data$consort_group == "gksrs_only")

    mfs_summary <- create_comprehensive_gep_summary(
        validation_results = mock_validation_results,
        outcome_type = "MFS",
        prame_analysis = NULL,
        missing_data_analysis = shared_missing,
        dataset_name = "uveal_melanoma_full_cohort",
        source_data = full_source_data
    )
    mss_summary <- create_comprehensive_gep_summary(
        validation_results = mock_validation_results,
        outcome_type = "MSS",
        prame_analysis = NULL,
        missing_data_analysis = shared_missing,
        dataset_name = "uveal_melanoma_full_cohort",
        source_data = full_source_data
    )
    gksrs_summary <- create_comprehensive_gep_summary(
        validation_results = mock_validation_results,
        outcome_type = "MFS",
        prame_analysis = NULL,
        missing_data_analysis = shared_missing,
        dataset_name = "uveal_melanoma_gksrs_only_cohort",
        source_data = gksrs_source_data
    )

    expect_true(grepl("## Follow-Up Limitation (5-year)", mfs_summary, fixed = TRUE))
    expect_true(grepl("Median follow-up among the 20-patient Full Cohort GEP validation subset: 10.5 years.", mfs_summary, fixed = TRUE))
    expect_false(grepl("GKSRS-Only Cohort GEP validation subset", mfs_summary, fixed = TRUE))
    expect_true(grepl("`followup_ge_5yr` means", mfs_summary, fixed = TRUE))
    expect_true(grepl("`censored_pre_5yr` means", mfs_summary, fixed = TRUE))
    expect_true(grepl("Among the", mfs_summary, fixed = TRUE))
    expect_true(grepl("- 5-year view:", mfs_summary, fixed = TRUE))

    expect_true(grepl("## Follow-Up Limitation (5-year)", mss_summary, fixed = TRUE))
    expect_true(grepl("Median follow-up among the 20-patient Full Cohort GEP validation subset: 10.5 years.", mss_summary, fixed = TRUE))
    expect_false(grepl("GKSRS-Only Cohort GEP validation subset", mss_summary, fixed = TRUE))
    expect_true(grepl("`followup_ge_5yr` means", mss_summary, fixed = TRUE))
    expect_true(grepl("- Operational view:", mss_summary, fixed = TRUE))

    expect_true(grepl("Median follow-up among the 10-patient GKSRS-Only Cohort GEP validation subset: 15.5 years.", gksrs_summary, fixed = TRUE))
    expect_false(grepl("Full Cohort GEP validation subset", gksrs_summary, fixed = TRUE))
})
