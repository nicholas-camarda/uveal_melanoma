test_that("binary event counting infers affirmative levels across common encodings", {
    expect_equal(count_binary_outcome_events(factor(c("No", "Yes", "No", "Yes"))), 2)
    expect_equal(count_binary_outcome_events(c("N", "Y", "N", "Y")), 2)
    expect_equal(count_binary_outcome_events(c(0, 1, 0, 1)), 2)
    expect_equal(count_binary_outcome_events(c(FALSE, TRUE, FALSE, TRUE)), 2)
})

test_that("logistic diagnostics and effect summaries count affirmative factor levels", {
    regression_data <- tibble::tibble(
        treatment_group = factor(rep(c("PBT", "GKSRS"), each = 5)),
        outcome = factor(c("No", "No", "Yes", "No", "Yes", "No", "Yes", "Yes", "No", "Yes")),
        age = c(50, 52, 54, 56, 58, 51, 53, 55, 57, 59)
    )

    model_fit <- stats::glm(
        outcome ~ treatment_group + age,
        data = regression_data,
        family = stats::binomial(),
        model = TRUE
    )

    model_summary_tab <- create_model_summary_tab(
        model_fit = model_fit,
        data = regression_data,
        outcome_var = "outcome",
        confounders = "age",
        analysis_name = "unit_logistic_count",
        extreme_diagnostics = NULL,
        filtered_variables = NULL
    )
    effect_summary <- summarize_effect_model(
        model = model_fit,
        dataset_name = "unit_test_dataset",
        analysis_label = "Unit Logistic Count",
        model_label = "Adjusted Logistic",
        group_var = "treatment_group",
        data_source_label = "Unit test logistic dataset",
        effect_measure = "OR",
        outcome_var = "outcome"
    )

    expect_equal(model_summary_tab$n_events[[1]], sum(regression_data$outcome == "Yes"))
    expect_equal(unique(effect_summary$n_events), sum(regression_data$outcome == "Yes"))
})
