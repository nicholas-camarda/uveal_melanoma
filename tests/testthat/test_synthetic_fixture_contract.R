test_that("synthetic CI fixture is deterministic and has the portable schema", {
    first <- create_synthetic_ci_dataset()
    second <- create_synthetic_ci_dataset()

    expect_identical(first, second)
    expect_identical(attr(first, "synthetic_fixture_version"), SYNTHETIC_CI_FIXTURE_VERSION)
    expect_identical(attr(first, "synthetic_fixture_seed"), SYNTHETIC_CI_FIXTURE_SEED)
    expect_true(all(synthetic_ci_required_columns() %in% names(first)))
    expect_equal(nrow(first), 48L)
    expect_true(is.factor(first$treatment_group))
    expect_identical(levels(first$treatment_group), SYNTHETIC_CI_TREATMENT_LEVELS)
    expect_true(is.factor(first$synthetic_cohort))
    expect_identical(levels(first$synthetic_cohort), SYNTHETIC_CI_COHORT_LEVELS)
    expect_true(is.factor(first$gep_class_simple))
    expect_identical(levels(first$gep_class_simple), SYNTHETIC_CI_GEP_LEVELS)
})

test_that("synthetic CI fixture covers treatments, cohorts, missingness, and censoring", {
    data <- create_synthetic_ci_dataset()

    expect_setequal(unique(as.character(data$treatment_group)), SYNTHETIC_CI_TREATMENT_LEVELS)
    expect_setequal(unique(as.character(data$synthetic_cohort)), SYNTHETIC_CI_COHORT_LEVELS)
    expect_true(anyNA(data$prame_status))
    expect_true(anyNA(data$initial_tumor_height))
    expect_true(any(data$mets_event == 1L) && any(data$mets_event == 0L))
    expect_true(any(data$melanoma_death_event == 1L) && any(data$melanoma_death_event == 0L))
    expect_true(any(data$synthetic_cohort == "gksrs_only"))
    expect_true(all(data$treatment_group[data$synthetic_cohort == "gksrs_only"] == "GKSRS"))

    sparse_one_arm <- dplyr::filter(data, synthetic_cohort == "gksrs_only")
    expect_no_error(stats::model.matrix(~ treatment_group, data = sparse_one_arm))
})

test_that("synthetic fixture does not depend on private files or identifying fields", {
    generator_text <- paste(deparse(body(create_synthetic_ci_dataset)), collapse = "\n")
    data <- create_synthetic_ci_dataset()

    expect_false(grepl("read\\.(xlsx|csv|rds)|RAW_DATA_DIR|Original Files", generator_text, ignore.case = TRUE))
    expect_false(any(grepl("(^|_)(patient|study|record|case)?_?id$|date", names(data), ignore.case = TRUE)))
    expect_false(any(vapply(data, is.list, logical(1))))
})

test_that("full-pipeline fixture is deterministic, balanced, and distribution-shaped", {
    first <- create_pipeline_test_dataset()
    second <- create_pipeline_test_dataset()

    expect_identical(first, second)
    expect_equal(nrow(first), 96L)
    expect_identical(levels(first$treatment_group), c("PBT", "GKSRS"))
    expect_setequal(unique(as.character(first$sex)), c("Female", "Male"))
    expect_setequal(
        unique(as.character(first$initial_t_stage_simple)),
        c("T1", "T2", "T3", "T4")
    )
    expect_true(all(first$tt_recurrence_months >= 0))
    expect_true(all(first$tt_mets_months >= 0))
    expect_true(all(first$tt_death_months >= 0))
    expect_true(stats::median(first$age_at_diagnosis) >= 55)
    expect_true(stats::median(first$age_at_diagnosis) <= 75)
    expect_true(all(first$initial_tumor_height > 0))

    support <- first %>%
        dplyr::group_by(treatment_group) %>%
        dplyr::summarise(
            recurrence_events = sum(recurrence_event),
            metastasis_events = sum(mets_event),
            death_events = sum(death_event),
            .groups = "drop"
        )
    expect_true(all(support$recurrence_events >= 10L))
    expect_true(all(support$metastasis_events >= 10L))
    expect_true(all(support$death_events >= 10L))

    generator_text <- paste(deparse(body(create_pipeline_test_dataset)), collapse = "\n")
    expect_false(grepl(
        "read\\.(xlsx|csv|rds)|RAW_DATA_DIR|PROCESSED_DATA_DIR|Original Files",
        generator_text,
        ignore.case = TRUE
    ))
})
