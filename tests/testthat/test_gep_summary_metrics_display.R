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

    expect_true(grepl("Harrell's C=0.750", summary_text))
    expect_true(grepl("Integrated AUC=0.780", summary_text))
    expect_true(grepl("Cumulative Disc=0.810", summary_text))
    expect_false(grepl("Uno's C", summary_text))
    expect_false(grepl("AUC=0.780[^\\n]*Uno", summary_text))
})
