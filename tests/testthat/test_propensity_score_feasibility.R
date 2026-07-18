source(here::here("scripts", "tools", "propensity_score_feasibility.R"))

test_that("propensity-score feasibility uses baseline covariates and records limits", {
    set.seed(100)
    data <- tibble::tibble(
        treatment_group = rep(c("GKSRS", "Plaque"), 40),
        age_at_diagnosis = seq(45, 84, length.out = 80),
        sex = rep(c("Male", "Female"), 40),
        location = rep(c("Choroid", "Ciliary Body"), each = 40),
        initial_tumor_height = stats::runif(80, 2, 8),
        initial_tumor_diameter = stats::runif(80, 5, 18),
        initial_vision = stats::runif(80, 0.2, 0.9)
    )

    ps_fit <- fit_treatment_propensity_score(data)

    expect_true(".propensity_score" %in% names(ps_fit$data))
    expect_true(all(ps_fit$data$.propensity_score > 0 & ps_fit$data$.propensity_score < 1))
    expect_true("reportable_sensitivity" %in% ps_fit$diagnostics$diagnostic)
    expect_true(all(ps_fit$covariate_screen$status %in% c("candidate", "excluded_all_missing", "excluded_single_level", "excluded_zero_variance")))
})

test_that("propensity-score screening records excluded covariates", {
    data <- tibble::tibble(
        treatment_group = rep(c("GKSRS", "Plaque"), 10),
        age_at_diagnosis = 60:79,
        all_missing_candidate = NA_real_,
        single_level_candidate = "same",
        zero_variance_candidate = 1
    )

    screen <- screen_propensity_covariates(
        data,
        c("age_at_diagnosis", "all_missing_candidate", "single_level_candidate", "zero_variance_candidate")
    )

    expect_equal(screen$status[screen$covariate == "age_at_diagnosis"], "candidate")
    expect_equal(screen$status[screen$covariate == "all_missing_candidate"], "excluded_all_missing")
    expect_equal(screen$status[screen$covariate == "single_level_candidate"], "excluded_single_level")
    expect_equal(screen$status[screen$covariate == "zero_variance_candidate"], "excluded_single_level")
})

test_that("propensity-score feasibility records aliased covariates", {
    data <- tibble::tibble(
        treatment_group = rep(c("GKSRS", "Plaque"), 30),
        initial_tumor_height = seq_len(60),
        initial_tumor_diameter = seq_len(60)
    )

    alias_screen <- drop_aliased_propensity_covariates(
        data,
        c("initial_tumor_height", "initial_tumor_diameter")
    )

    expect_true(any(alias_screen$alias_diagnostics$status == "excluded_aliased_or_collinear"))
    expect_true(length(alias_screen$retained_covariates) >= 1L)
})

test_that("propensity-score feasibility blocks separation-prone models from reportable sensitivity", {
    data <- tibble::tibble(
        treatment_group = rep(c("GKSRS", "Plaque"), each = 40),
        initial_vision = c(seq(0.01, 0.40, length.out = 40), seq(0.60, 0.99, length.out = 40))
    )

    ps_fit <- fit_treatment_propensity_score(data)

    expect_true(any(ps_fit$data$.propensity_score < 0.02 | ps_fit$data$.propensity_score > 0.98))
    expect_false(ps_fit$reportable_sensitivity)
    expect_equal(
        ps_fit$diagnostics$value[ps_fit$diagnostics$diagnostic == "separation_prone"],
        1
    )
})

test_that("propensity-score feasibility writes expected workbook sheets", {
    data <- tibble::tibble(
        treatment_group = rep(c("GKSRS", "Plaque"), 40),
        age_at_diagnosis = rep(45:84, 2),
        sex = rep(c("Male", "Female"), 40),
        initial_tumor_height = seq(2, 8, length.out = 80),
        initial_tumor_diameter = seq(5, 15, length.out = 80)
    )

    ps_fit <- fit_treatment_propensity_score(data)
    path <- file.path(TEST_OUTPUT_DIR, "propensity_score_feasibility.xlsx")
    write_propensity_score_feasibility(ps_fit, path)

    expect_workbook_has_sheets(
        path,
        c(
            "diagnostics",
            "complete_case_summary",
            "covariate_screen",
            "alias_diagnostics",
            "retained_covariates",
            "overlap_by_treatment",
            "propensity_scores"
        )
    )
})
