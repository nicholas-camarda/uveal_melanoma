test_that("Objective 1 writes a separate age-decade subgroup sensitivity workbook", {
    pipeline <- get_objective1_pipeline()

    workbook_path <- file.path(
        dirname(pipeline$output_dirs$obj1_forest_plots),
        "test_age_decade_subgroup_sensitivity.xlsx"
    )
    expect_workbook_has_sheets(
        workbook_path,
        c("local_recurrence", "metastatic_progression", "overall_survival", "progression_free_survival")
    )
    expected_metadata <- c(
        "outcome_key", "outcome", "endpoint_type", "model_family",
        "effect_measure", "time_variable", "event_variable", "estimand"
    )
    for (outcome_key in names(OBJECTIVE1_SUBGROUP_OUTCOME_SPECS)) {
        outcome_rows <- readxl::read_xlsx(workbook_path, sheet = outcome_key)
        outcome_spec <- OBJECTIVE1_SUBGROUP_OUTCOME_SPECS[[outcome_key]]

        expect_true(all(expected_metadata %in% names(outcome_rows)))
        expect_identical(unique(outcome_rows$outcome_key), outcome_key)
        expect_identical(unique(outcome_rows$model_family), outcome_spec$model_family)
        expect_identical(unique(outcome_rows$effect_measure), outcome_spec$effect_measure)
        expect_identical(unique(outcome_rows$time_variable), outcome_spec$time_var)
        expect_identical(unique(outcome_rows$event_variable), outcome_spec$event_var)
    }

    age_rows <- readxl::read_xlsx(workbook_path, sheet = "overall_survival")
    expect_true(any(age_rows$variable == "age_at_diagnosis_binned"))
    expect_true(all(age_rows$subgroup_surface == "age_decade_sensitivity"))
})

test_that("Objective 1 subgroup outcome specifications use Cox hazard ratios", {
    expect_identical(
        names(OBJECTIVE1_SUBGROUP_OUTCOME_SPECS),
        c(
            "local_recurrence",
            "metastatic_progression",
            "overall_survival",
            "progression_free_survival"
        )
    )

    expect_true(all(vapply(
        OBJECTIVE1_SUBGROUP_OUTCOME_SPECS,
        function(spec) identical(spec$model_family, "Cox proportional hazards"),
        logical(1)
    )))
    expect_true(all(vapply(
        OBJECTIVE1_SUBGROUP_OUTCOME_SPECS,
        function(spec) identical(spec$effect_measure, "HR"),
        logical(1)
    )))
})
