test_that("Objective 0 preprocessing creates expected derived fields on synthetic data", {
    test_data <- create_test_dataset()
    derived <- create_derived_variables(test_data)

    expect_true(all(c(
        "treatment_group",
        "tt_mets_months",
        "tt_death_months",
        "pfs_event",
        "mfs_event_5yr",
        "mss_event_5yr"
    ) %in% names(derived)))
    expect_true(all(stats::na.omit(derived$gep_validation_set) %in% c("Eligible", "No GEP Data")))
    expect_false(any(derived$gep_validation_set %in% c("Training", "Testing"), na.rm = TRUE))
})

test_that("Objective 0 factor preparation and cohort criteria run on synthetic data", {
    synthetic_cohort_input <- tibble::tibble(
        id = 1:6,
        initial_stage_binary = c("Stage I-III", "Stage I-III", "Stage IV", "Stage I-III", "Stage I-III", NA),
        initial_overall_stage = c("2A", "2B", "4", "3A", "1", NA),
        consort_group = c("eligible_both", "gksrs_only", "eligible_both", "eligible_both", NA, "gksrs_only"),
        treatment_group = c("PBT", "GKSRS", "PBT", "GKSRS", "PBT", NA)
    )

    cohort_result <- apply_criteria(synthetic_cohort_input)
    expect_true("cohorts" %in% names(cohort_result))
    expect_true(all(c(
        "uveal_melanoma_full_cohort",
        "uveal_melanoma_restricted_cohort",
        "uveal_melanoma_gksrs_only_cohort"
    ) %in% names(cohort_result$cohorts)))
    expect_true(nrow(cohort_result$cohorts$uveal_melanoma_full_cohort) > 0)
})
