test_that("Objective 0 preprocessing creates expected derived fields on synthetic data", {
    test_data <- create_test_dataset()
    derived <- create_derived_variables(test_data)

    expect_true(all(c(
        "treatment_group",
        "tt_mets_months",
        "tt_death_months",
        "pfs_event",
        "mfs_event_5yr",
        "mss_event_5yr",
        "retinopathy_burden_event",
        "nvg_burden_event",
        "srd_burden_event"
    ) %in% names(derived)))
    expect_true(all(stats::na.omit(derived$gep_validation_set) %in% c("Eligible", "No GEP Data")))
    expect_false(any(derived$gep_validation_set %in% c("Training", "Testing"), na.rm = TRUE))
    expect_equal(derived$retinopathy_burden_event, as.integer(derived$retinopathy == "Y"))
    expect_equal(derived$nvg_burden_event, as.integer(derived$nvg == "Y"))
    expect_equal(derived$srd_burden_event, as.integer(derived$srd == "Y"))
})

test_that("Objective 0 derives integer treatment year from treatment date", {
    test_data <- create_test_dataset()
    derived <- create_derived_variables(test_data)

    expect_true("treatment_year" %in% names(derived))
    expect_type(derived$treatment_year, "integer")
    expect_equal(
        derived$treatment_year,
        as.integer(format(as.Date(derived$treatment_date), "%Y"))
    )
})

test_that("Objective 0 derivation preserves impossible endpoint times for validation", {
    test_data <- create_test_dataset()
    test_data$recurrence1[1] <- "Y"
    test_data$recurrence1_date[1] <- test_data$initial_gk_date[1] - 30
    test_data$mets_progression[2] <- "Y"
    test_data$mets_progression_date[2] <- test_data$initial_gk_date[2] - 30
    test_data$dod[3] <- test_data$initial_gk_date[3] - 30

    derived <- create_derived_variables(test_data)

    expect_lt(derived$tt_recurrence_months_analysis[[1]], 0)
    expect_lt(derived$tt_mets_months_analysis[[2]], 0)
    expect_lt(derived$tt_death_months_analysis[[3]], 0)
    expect_lt(derived$tt_pfs_months_analysis[[1]], 0)
})

test_that("Objective 0 derives PFS from the first recurrence, metastasis, death, or censoring time", {
    test_data <- create_test_dataset()[1:4, ]
    treatment_date <- as.Date("2020-01-01")
    test_data$initial_gk <- "Y"
    test_data$initial_plaque <- "N"
    test_data$initial_gk_date <- treatment_date
    test_data$treatment_date <- treatment_date
    test_data$last_known_alive_date <- as.Date("2021-01-01")
    test_data$recurrence1 <- c("Y", "N", "N", "N")
    test_data$recurrence1_date <- as.Date(c("2020-04-01", NA, NA, NA))
    test_data$mets_progression <- c("N", "Y", "N", "N")
    test_data$mets_progression_date <- as.Date(c(NA, "2020-03-01", NA, NA))
    test_data$dod <- as.Date(c(NA, NA, "2020-02-01", NA))

    derived <- create_derived_variables(test_data)

    expect_equal(derived$pfs_event, c(1L, 1L, 1L, 0L))
    expect_equal(
        derived$tt_pfs_months,
        pmin(
            derived$tt_recurrence_months,
            derived$tt_mets_months,
            derived$tt_death_months,
            na.rm = FALSE
        )
    )
    expect_equal(derived$tt_pfs_months_analysis, derived$tt_pfs_months)
})

test_that("Objective 0 factor preparation and cohort criteria run on synthetic data", {
    synthetic_cohort_input <- tibble::tibble(
        id = c(1, 2, 3, 4, 5, 271),
        initial_stage_binary = c("Stage I-III", "Stage I-III", "Stage IV", "Stage I-III", "Stage I-III", NA),
        initial_overall_stage = c("2A", "2B", "4", "3A", "1", NA),
        consort_group = c(
            CONSORT_GROUP_ELIGIBLE_BOTH,
            CONSORT_GROUP_GKSRS_ONLY,
            CONSORT_GROUP_ELIGIBLE_BOTH,
            CONSORT_GROUP_FULL_ONLY_SPECIAL_CASE,
            CONSORT_GROUP_UNCLASSIFIED_FIELDS,
            CONSORT_GROUP_UNCLASSIFIED_FIELDS
        ),
        treatment_group = c("PBT", "GKSRS", "PBT", "PBT", "PBT", "PBT")
    )

    cohort_result <- apply_criteria(synthetic_cohort_input)
    expect_true("cohorts" %in% names(cohort_result))
    expect_true(all(c(
        "uveal_melanoma_full_cohort",
        "uveal_melanoma_restricted_cohort",
        "uveal_melanoma_gksrs_only_cohort"
    ) %in% names(cohort_result$cohorts)))
    expect_true(nrow(cohort_result$cohorts$uveal_melanoma_full_cohort) > 0)
    expect_true(4 %in% cohort_result$cohorts$uveal_melanoma_full_cohort$id)
    expect_false(4 %in% cohort_result$cohorts$uveal_melanoma_restricted_cohort$id)
    expect_false(4 %in% cohort_result$cohorts$uveal_melanoma_gksrs_only_cohort$id)
    expect_false(any(cohort_result$cohorts$uveal_melanoma_full_cohort$consort_group == "other"))
    expect_false(any(cohort_result$cohorts$uveal_melanoma_full_cohort$consort_group == CONSORT_GROUP_UNCLASSIFIED_FIELDS))
    expect_true(any(cohort_result$removal_log$id == 5 & cohort_result$removal_log$removal_step == "missing_cohort_fields"))
    expect_true(any(cohort_result$removal_log$id == 271 & cohort_result$removal_log$removal_step == "manual_exclusion"))
})

test_that("iris optic nerve N/A helper records only explicit non-applicability cases", {
    raw_data <- tibble::tibble(
        id = c(247, 248, 249),
        location = c("Iris", "Choroidal", "Iris"),
        optic_nerve = c("N/A", "N/A", NA_character_)
    )

    special_cases <- collect_iris_optic_nerve_special_cases(raw_data)

    expect_equal(special_cases$id, 247)
    expect_equal(special_cases$special_case, IRIS_OPTIC_NERVE_SPECIAL_CASE)
    expect_match(special_cases$interpretation, "full cohort only")
})
