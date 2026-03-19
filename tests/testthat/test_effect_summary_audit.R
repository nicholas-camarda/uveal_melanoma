test_that("summarize_effect_model reports Cox sample size separately from events", {
    test_data <- tibble::tibble(
        follow_up_months = c(6, 12, 18, 24, 30, 36, 42, 48, 54, 60),
        event = c(1, 1, 0, 0, 1, 0, 0, 1, 0, 0),
        grp = factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B"))
    )

    cox_model <- suppressWarnings(survival::coxph(
        survival::Surv(follow_up_months, event) ~ grp,
        data = test_data,
        model = TRUE
    ))

    effect_summary <- summarize_effect_model(
        model = cox_model,
        dataset_name = "unit_test_dataset",
        analysis_label = "Unit Test Survival",
        model_label = "Unadjusted (Cox data)",
        group_var = "grp",
        data_source_label = "Unit test Cox dataset",
        effect_measure = "HR"
    )

    expect_equal(effect_summary$n_patients[[1]], nrow(test_data))
    expect_equal(effect_summary$n_events[[1]], sum(test_data$event == 1))
    expect_equal(effect_summary$n_outcome_non_missing[[1]], nrow(test_data))
    expect_gt(effect_summary$n_patients[[1]], effect_summary$n_events[[1]])
})

test_that("effect-summary audit flags Cox sample-size and missing-adjustment issues", {
    audit_dir <- file.path(TEST_OUTPUT_DIR, "effect_summary_audit_unit")
    dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
    withr::defer(unlink(audit_dir, recursive = TRUE), envir = parent.frame())

    effect_summary_rows <- bind_effect_summary_rows(
        create_effect_summary_rows(
            dataset_name = "unit_test_dataset",
            analysis_label = "Metastasis-Free Survival Probability",
            model_label = "Unadjusted (Cox data)",
            term = "gep_class_simpleClass 2",
            model_formula = "surv_obj ~ gep_class_simple",
            covariates_used = "None",
            effect_measure = "HR",
            estimate = 2.5,
            ci_lower = 1.2,
            ci_upper = 5.1,
            p_value = 0.02,
            n_patients = 5,
            n_events = 5,
            n_outcome_non_missing = 5,
            data_source = "Unit test",
            model_status = "FIT"
        ),
        create_effect_summary_rows(
            dataset_name = "unit_test_dataset",
            analysis_label = "Metastasis-Free Survival Probability",
            model_label = "Adjusted Cox (confounders)",
            term = "gep_class_simpleClass 2",
            model_formula = "surv_obj ~ gep_class_simple",
            covariates_used = "None",
            effect_measure = "HR",
            estimate = 2.5,
            ci_lower = 1.2,
            ci_upper = 5.1,
            p_value = 0.02,
            n_patients = 5,
            n_events = 5,
            n_outcome_non_missing = 5,
            data_source = "Unit test",
            model_status = "FIT"
        )
    )

    gep_audit_dir <- file.path(
        audit_dir,
        "04_GEP_Validation",
        "a_metastasis_free_survival",
        "02_cox_models"
    )
    dir.create(gep_audit_dir, recursive = TRUE, showWarnings = FALSE)

    effect_summary_path <- file.path(
        gep_audit_dir,
        "unit_metastasis_free_survival_probability_effect_summary.xlsx"
    )
    diagnostics_path <- file.path(
        gep_audit_dir,
        "unit_metastasis_free_survival_probability_cox_diagnostics.xlsx"
    )

    writexl::write_xlsx(effect_summary_rows, effect_summary_path)
    writexl::write_xlsx(
        list(
            Model_summary = tibble::tibble(
                analysis_type = "unified_Metastasis-Free Survival Probability",
                outcome = "mets_event",
                n_total = 20,
                n_events = 5,
                n_outcome_levels = NA_real_,
                model_fitted = TRUE,
                confounders_used = "age_at_diagnosis_general_pop_median, sex, location",
                notes = "Unit test diagnostics"
            )
        ),
        diagnostics_path
    )

    findings <- audit_effect_summary_workbook(
        effect_summary_path = effect_summary_path,
        expected_confounders = c("age_at_diagnosis_general_pop_median", "sex", "location")
    )

    expect_true("cox_n_patients_misreported" %in% findings$issue_type)
    expect_true("adjusted_model_missing_covariates" %in% findings$issue_type)
    expect_true("adjusted_model_formula_missing_expected_confounders" %in% findings$issue_type)
    expect_true("adjusted_matches_unadjusted_without_covariates" %in% findings$issue_type)
})
