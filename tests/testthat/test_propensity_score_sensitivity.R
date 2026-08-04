create_propensity_test_data <- function(n_per_arm = 40L) {
    n <- 2L * n_per_arm
    arm_index <- rep(seq_len(n_per_arm), times = 2L)
    treatment_year <- 2010L + ((arm_index * 3L) %% 16L)
    treatment_date <- as.Date(sprintf("%d-01-01", treatment_year)) + seq_len(n)
    tibble::tibble(
        id = seq_len(n),
        treatment_group = factor(
            rep(c("PBT", "GKSRS"), each = n_per_arm),
            levels = TREATMENT_FACTOR_LEVELS
        ),
        treatment_date = treatment_date,
        treatment_year = treatment_year,
        age_at_diagnosis = 42 + ((arm_index * 7L) %% 43L),
        sex = factor(ifelse(arm_index %% 2L == 0L, "Female", "Male")),
        location = factor(ifelse(arm_index %% 3L == 0L, "Cilio-Choroidal", "Choroidal")),
        initial_tumor_height = 2 + ((arm_index * 11L) %% 70L) / 10,
        initial_tumor_diameter = 5 + ((arm_index * 13L) %% 130L) / 10,
        srf = factor(ifelse(arm_index %% 5L < 2L, "No", "Yes")),
        tt_recurrence_months = seq(6, 85, length.out = n),
        recurrence_event = rep(c(0L, 0L, 0L, 1L), length.out = n),
        tt_mets_months = seq(7, 86, length.out = n),
        mets_event = rep(c(0L, 0L, 1L), length.out = n),
        tt_death_months = seq(8, 87, length.out = n),
        death_event = rep(c(0L, 0L, 0L, 1L), length.out = n),
        tt_pfs_months = seq(5, 84, length.out = n),
        pfs_event = rep(c(0L, 1L), length.out = n)
    )
}

test_that("propensity sensitivity uses the approved centralized policy", {
    expect_identical(
        OBJECTIVE1_PROPENSITY_COVARIATES,
        c(
            "age_at_diagnosis", "sex", "location", "initial_tumor_height",
            "initial_tumor_diameter", "srf", "treatment_year"
        )
    )
    expect_identical(OBJECTIVE1_PROPENSITY_DATASET, "uveal_melanoma_restricted_cohort")
    expect_identical(OBJECTIVE1_PROPENSITY_ESTIMAND, "ATO")
    expect_identical(OBJECTIVE1_PROPENSITY_BALANCE_THRESHOLD, 0.10)
    expect_identical(TREATMENT_REFERENCE_LEVEL, "PBT")
    expect_identical(TREATMENT_COMPARISON_LEVEL, "GKSRS")
    expect_identical(
        names(OBJECTIVE1_PROPENSITY_EXPECTED_POPULATIONS),
        c("surface", "n_patients", "n_events", "population_fingerprint")
    )
    expect_setequal(
        names(OBJECTIVE1_SUBGROUP_OUTCOME_SPECS),
        c(
            "local_recurrence", "metastatic_progression",
            "overall_survival", "progression_free_survival"
        )
    )
})

test_that("propensity population preparation is read-only and deterministic", {
    data <- create_propensity_test_data()
    original <- data

    prepared <- prepare_objective1_propensity_population(data, "synthetic_restricted")

    expect_identical(data, original)
    expect_identical(prepared$data, original)
    expect_identical(prepared$selection_index, seq_len(nrow(data)))
    expect_equal(prepared$population_audit$input_n, nrow(data))
    expect_equal(prepared$population_audit$model_n, nrow(data))
    expect_match(prepared$population_audit$population_fingerprint, "^[0-9a-f]{64}$")
})

test_that("propensity population preparation applies only the location sparsity rule", {
    data <- create_propensity_test_data()
    data$location <- as.character(data$location)
    data$location[1] <- "Ciliary Body"
    data$location <- factor(data$location)

    prepared <- prepare_objective1_propensity_population(data, "synthetic_restricted")

    expect_equal(prepared$selection_index, 2:nrow(data))
    expect_equal(prepared$sparse_audit$excluded_levels, "Ciliary Body")
    expect_equal(prepared$sparse_audit$excluded_n, 1L)
})

test_that("propensity population preparation fails closed on invalid inputs", {
    data <- create_propensity_test_data()

    expect_error(
        prepare_objective1_propensity_population(dplyr::select(data, -"srf"), "synthetic"),
        "missing required propensity columns"
    )

    missing_height <- data
    missing_height$initial_tumor_height[1] <- NA_real_
    expect_error(
        prepare_objective1_propensity_population(missing_height, "synthetic"),
        "missing required propensity values"
    )

    reversed <- data
    reversed$treatment_group <- factor(
        reversed$treatment_group,
        levels = rev(TREATMENT_FACTOR_LEVELS)
    )
    expect_error(
        prepare_objective1_propensity_population(reversed, "synthetic"),
        "treatment factor levels"
    )

    unexpected <- data
    unexpected$treatment_group <- factor(
        as.character(unexpected$treatment_group),
        levels = c(TREATMENT_FACTOR_LEVELS, "Other")
    )
    expect_error(
        prepare_objective1_propensity_population(unexpected, "synthetic"),
        "treatment factor levels"
    )
})

test_that("propensity design fits the exact model and named overlap weights", {
    prepared <- prepare_objective1_propensity_population(
        create_propensity_test_data(),
        "synthetic_restricted"
    )
    design <- fit_objective1_propensity_weights(prepared)

    expect_identical(
        design$formula_text,
        paste(
            "treatment_group ~ age_at_diagnosis + sex + location +",
            "initial_tumor_height + initial_tumor_diameter + srf + treatment_year"
        )
    )
    expect_true(all(design$data$.propensity_score > 0 & design$data$.propensity_score < 1))
    expect_true(all(design$data$.overlap_weight > 0 & design$data$.overlap_weight < 1))
    expect_equal(
        design$data$.overlap_weight[design$data$treatment_group == TREATMENT_COMPARISON_LEVEL],
        1 - design$data$.propensity_score[design$data$treatment_group == TREATMENT_COMPARISON_LEVEL],
        tolerance = 1e-14
    )
    expect_equal(
        design$data$.overlap_weight[design$data$treatment_group == TREATMENT_REFERENCE_LEVEL],
        design$data$.propensity_score[design$data$treatment_group == TREATMENT_REFERENCE_LEVEL],
        tolerance = 1e-14
    )
    expect_setequal(attr(stats::terms(design$model), "term.labels"), OBJECTIVE1_PROPENSITY_COVARIATES)
    expect_false(any(c(
        "optic_nerve", "initial_t_stage_simple", "initial_vision",
        "orange_pigment", "treatment_year_centered"
    ) %in% attr(stats::terms(design$model), "term.labels")))
    expect_match(design$weight_fingerprint, "^[0-9a-f]{64}$")
})

test_that("propensity design reports positive ESS and exact modeled-term balance", {
    prepared <- prepare_objective1_propensity_population(
        create_propensity_test_data(),
        "synthetic_restricted"
    )
    design <- fit_objective1_propensity_weights(prepared)

    expect_setequal(design$ess_summary$treatment_group, c(TREATMENT_FACTOR_LEVELS, "Total"))
    expect_true(all(design$ess_summary$effective_sample_size > 0))
    expect_true(all(is.finite(design$balance_table$weighted_smd)))
    expect_lt(max(design$balance_table$weighted_abs_smd), 1e-8)
    expect_false(any(design$balance_table$weighted_exceeds_threshold))
    expect_identical(design$diagnostics$estimand[[1]], OBJECTIVE1_PROPENSITY_ESTIMAND)
})

test_that("propensity design rejects aliased required terms", {
    data <- create_propensity_test_data()
    data$initial_tumor_diameter <- data$initial_tumor_height
    prepared <- prepare_objective1_propensity_population(data, "synthetic_restricted")

    expect_error(
        fit_objective1_propensity_weights(prepared),
        "aliased or non-finite coefficients"
    )
})
