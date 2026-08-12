test_that("survival population fingerprints detect patient-level endpoint drift", {
    data <- tibble::tibble(
        id = c(3, 1, 2),
        treatment_group = factor(c("GKSRS", "PBT", "PBT"), levels = c("PBT", "GKSRS")),
        tt_death_months = c(18, 6, 12),
        death_event = c(0, 1, 0)
    )

    fingerprint <- compute_survival_population_fingerprint(
        data,
        time_var = "tt_death_months",
        event_var = "death_event",
        group_var = "treatment_group"
    )

    expect_identical(
        fingerprint,
        compute_survival_population_fingerprint(
            data[c(2, 3, 1), ],
            time_var = "tt_death_months",
            event_var = "death_event",
            group_var = "treatment_group"
        )
    )

    changed <- data
    changed$tt_death_months[changed$id == 2] <- 11
    expect_false(identical(
        fingerprint,
        compute_survival_population_fingerprint(
            changed,
            time_var = "tt_death_months",
            event_var = "death_event",
            group_var = "treatment_group"
        )
    ))

    contract <- tibble::tibble(
        dataset_name = "unit_cohort",
        time_var = "tt_death_months",
        event_var = "death_event",
        group_var = "treatment_group",
        n_patients = 3L,
        n_events = 1L,
        population_fingerprint = fingerprint,
        approval_note = "Unit-test approved population"
    )
    contract <- dplyr::bind_rows(
        contract,
        dplyr::mutate(
            contract,
            dataset_name = "different_cohort",
            population_fingerprint = "different-fingerprint"
        )
    )

    expect_no_error(assert_survival_population_contract(data, "unit_cohort", contract))
    expect_error(
        assert_survival_population_contract(changed, "unit_cohort", contract),
        "Survival population contract violation"
    )
})

test_that("KM risk-set audit preserves counts and patient membership", {
    data <- tibble::tibble(
        id = c(1, 2, 3),
        treatment_group = factor(c("PBT", "PBT", "GKSRS"), levels = c("PBT", "GKSRS")),
        tt_death_months = c(6, 12, 18),
        death_event = c(1, 0, 0),
        treatment_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01")),
        last_known_alive_date = as.Date(c("2020-07-01", "2021-01-01", "2021-07-01")),
        last_known_alive_source = c("dod", "last_followup", "last_height_date")
    )

    audit <- build_km_risk_set_audit(
        data = data,
        time_var = "tt_death_months",
        event_var = "death_event",
        group_var = "treatment_group",
        time_points = c(0, 12),
        dataset_name = "unit_cohort"
    )

    expect_equal(
        audit$risk_set_counts$n_at_risk,
        c(2L, 1L, 1L, 1L)
    )
    expect_equal(
        audit$risk_set_members$at_risk_ids,
        c("1, 2", "2", "3", "3")
    )
    expect_true(all(c(
        "id", "treatment_group", "tt_death_months", "death_event",
        "treatment_date", "last_known_alive_date", "last_known_alive_source"
    ) %in% names(audit$patient_endpoints)))
    expect_false("configured_corrections" %in% names(audit))
})
