library(testthat)

test_that("observed level counts ignore unused and explicitly excluded levels", {
    values <- factor(
        c(rep("Choroidal", 74), rep("Cilio-Choroidal", 7)),
        levels = c("Choroidal", "Cilio-Choroidal", "Other")
    )

    counts <- get_observed_level_counts(values)
    retained_counts <- get_observed_level_counts(values, excluded_levels = "Other")

    expect_equal(counts$level, c("Choroidal", "Cilio-Choroidal"))
    expect_equal(counts$observed_n, c(74L, 7L))
    expect_equal(retained_counts$level, c("Choroidal", "Cilio-Choroidal"))
    expect_equal(retained_counts$observed_n, c(74L, 7L))
})

test_that("apply_sparse_level_exclusions reports original excluded levels and removed rows", {
    test_data <- tibble::tibble(
        patient_id = sprintf("P%02d", 1:14),
        location = factor(
            c(rep("Choroidal", 7), rep("Cilio-Choroidal", 5), rep("Other", 2)),
            levels = c("Choroidal", "Cilio-Choroidal", "Other")
        ),
        treatment_group = factor(rep(c("PBT", "GKSRS"), length.out = 14))
    )

    result <- apply_sparse_level_exclusions(
        data = test_data,
        variables = "location",
        analysis_name = "unit_test",
        min_level_count = 5,
        id_col = "patient_id",
        level_exclusions = list(location = "Other")
    )

    expect_equal(nrow(result$data), 12L)
    expect_equal(levels(result$data$location), c("Choroidal", "Cilio-Choroidal"))
    expect_equal(result$removed_row_count, 2L)
    expect_equal(result$removed_row_ids, c("P13", "P14"))
    expect_s3_class(result$sparse_level_diagnostics, "data.frame")
    expect_equal(result$sparse_level_diagnostics$variable, "location")
    expect_equal(result$sparse_level_diagnostics$level, "Other")
    expect_equal(result$sparse_level_diagnostics$rows_removed, 2L)
    expect_equal(result$sparse_level_diagnostics$source, "explicit_exclusion")
})

test_that("generate_valid_confounders ignores unused factor levels", {
    test_data <- tibble::tibble(
        location = factor(
            c(rep("Choroidal", 20), rep("Cilio-Choroidal", 6)),
            levels = c("Choroidal", "Cilio-Choroidal", "Other")
        ),
        sex = factor(rep(c("Male", "Female"), 13)),
        age_at_diagnosis = c(rep(55, 13), rep(65, 13))
    )

    valid <- generate_valid_confounders(
        data = test_data,
        confounders = c("location", "sex", "age_at_diagnosis"),
        threshold = 5,
        verbose = FALSE
    )

    expect_true("location" %in% valid)
    expect_true("sex" %in% valid)
    expect_true("age_at_diagnosis" %in% valid)
})

test_that("interaction and overall significance ignore unused factor levels", {
    set.seed(42)
    n <- 80
    test_data_with_unused <- tibble::tibble(
        height_change = stats::rnorm(
            n,
            mean = 1.5 +
                0.6 * (rep(c(0, 1), each = n / 2)) +
                0.4 * c(rep(0, 60), rep(1, 20)),
            sd = 0.5
        ),
        treatment_group = factor(rep(c("PBT", "GKSRS"), each = n / 2)),
        location = factor(
            c(rep("Choroidal", 60), rep("Cilio-Choroidal", 20)),
            levels = c("Choroidal", "Cilio-Choroidal", "Other")
        )
    )

    test_data_dropped <- test_data_with_unused %>%
        dplyr::mutate(location = droplevels(.data$location))

    interaction_with_unused <- expect_warning(
        calculate_variable_interaction_pvalue(
            data = test_data_with_unused,
            variable_name = "location",
            outcome_var = "height_change",
            treatment_var = "treatment_group",
            outcome_type = "continuous"
        ),
        NA
    )

    interaction_dropped <- expect_warning(
        calculate_variable_interaction_pvalue(
            data = test_data_dropped,
            variable_name = "location",
            outcome_var = "height_change",
            treatment_var = "treatment_group",
            outcome_type = "continuous"
        ),
        NA
    )

    overall_with_unused <- expect_warning(
        calculate_variable_overall_significance(
            data = test_data_with_unused,
            variable_name = "location",
            outcome_var = "height_change",
            treatment_var = "treatment_group",
            outcome_type = "continuous"
        ),
        NA
    )

    overall_dropped <- expect_warning(
        calculate_variable_overall_significance(
            data = test_data_dropped,
            variable_name = "location",
            outcome_var = "height_change",
            treatment_var = "treatment_group",
            outcome_type = "continuous"
        ),
        NA
    )

    expect_identical(interaction_with_unused, interaction_dropped)
    expect_identical(overall_with_unused, overall_dropped)
})
