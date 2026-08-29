test_that("Exploratory no-GEP follow-up block summarizes follow-up and operational status", {
    test_data <- create_test_dataset() %>%
        dplyr::slice(1:8) %>%
        dplyr::mutate(
            exploratory_gep_group = factor(
                c(
                    rep("GEP Failed/Indeterminate", 4),
                    rep("GEP Not Tested", 4)
                ),
                levels = c("GEP Failed/Indeterminate", "GEP Not Tested")
            ),
            no_gep_group = as.character(.data$exploratory_gep_group),
            follow_up_years = c(1.0, 2.0, 3.5, 4.0, 5.0, 6.5, 7.0, 8.5),
            last_known_alive_date = as.Date(c(
                "2025-02-20",
                "2025-01-10",
                "2023-01-01",
                "2024-12-01",
                "2025-02-15",
                "2023-06-01",
                "2025-01-30",
                "2024-04-01"
            )),
            death_event = c(0, 0, 1, 0, 0, 1, 0, 0)
        )

    block <- build_exploratory_no_gep_followup_block(
        prepared_data = list(no_gep_scoring = test_data),
        dataset_name = "uveal_melanoma_full_cohort"
    )

    expect_true(any(grepl("## Follow-Up Context", block, fixed = TRUE)))
    expect_true(any(grepl("no-GEP scoring cohort", block, fixed = TRUE)))
    expect_true(any(grepl("reached at least 5 years", block, fixed = TRUE)))
    expect_true(any(grepl("Operational view:", block, fixed = TRUE)))
    expect_true(any(grepl("By no-GEP group:", block, fixed = TRUE)))
})

test_that("presentation data contract is aggregate-only and keyed by stable semantic identifiers", {
    analysis_results <- list(
        data_audit = tibble::tibble(
            section = "Group Counts",
            group = c("Class 1", "Class 2", "GEP Failed/Indeterminate", "GEP Not Tested"),
            n = c(10L, 8L, 2L, 12L)
        ),
        prepared_data = list(
            no_gep_scoring = tibble::tibble(
                no_gep_group = c("GEP Failed/Indeterminate", "GEP Failed/Indeterminate", "GEP Not Tested", "GEP Not Tested"),
                follow_up_years = c(2, 4, 5, 7)
            )
        ),
        no_gep_subgroups = tibble::tibble(
            no_gep_group = c("GEP Failed/Indeterminate", "GEP Not Tested"),
            n = c(2L, 12L),
            observed_5yr_mfs_event_rate = c(0.4, 0.2),
            observed_5yr_mss_event_rate = c(0.3, 0.1),
            median_predicted_5yr_mfs_risk = c(0.5, 0.25),
            median_predicted_60mo_melanoma_death_cumulative_incidence_risk = c(0.35, 0.15)
        ),
        baseline_comparisons = tibble::tibble(
            variable = c("initial_tumor_diameter", "age_at_diagnosis"),
            test = c("Kruskal-Wallis", "Kruskal-Wallis"),
            p_value = c(0.01, 0.04)
        ),
        overlap_diagnostics = tibble::tibble(
            predictor = c("initial_tumor_diameter", "age_at_diagnosis"),
            worst_level = c(NA_character_, NA_character_),
            abs_smd = c(0.84, 0.20),
            overlap_flag = c("high_shift", "acceptable_overlap")
        ),
        sensitivity_summary = tibble::tibble(
            analysis = c("Direct_MFS_5yr_Risk", "Direct_MFS_5yr_Risk"),
            bin = c("High", NA_character_),
            n = c(5L, 1L),
            mean_predicted = c(0.6, NaN),
            observed_mfs_5yr_event_rate = c(0.5, NA_real_),
            observed_mss_5yr_event_rate = c(0.3, NA_real_)
        ),
        direct_models = list(
            mfs = list(predictor_contributions = tibble::tibble(
                predictor = "initial_tumor_diameter",
                dominant_term = "initial_tumor_diameter",
                standardized_coefficient = 0.72,
                direction = "higher risk"
            )),
            mss = list(predictor_contributions = tibble::tibble(
                predictor = "initial_tumor_height",
                dominant_term = "initial_tumor_height",
                standardized_coefficient = -0.31,
                direction = "lower risk"
            ))
        )
    )

    payload <- build_exploratory_no_gep_presentation_data(analysis_results)

    expect_true(all(c(
        "semantic_id", "section", "group", "label", "value_numeric",
        "value_character", "unit", "reader_role", "reason_for_missing_gep_available"
    ) %in% names(payload)))
    expect_identical(anyDuplicated(payload$semantic_id), 0L)
    expect_equal(
        payload$value_numeric[payload$semantic_id == "no_gep_scoreable_count"],
        4
    )
    expect_setequal(
        c(
            "cohort_total_count",
            "gep_usable_count",
            "gep_not_tested_count",
            "gep_failed_indeterminate_count",
            "no_gep_without_usable_count",
            "no_gep_scoreable_count",
            "followup_no_gep_ge_5yr_count",
            "followup_gep_not_tested_median_years",
            "observed_gep_failed_indeterminate_mfs_5yr_event_rate",
            "observed_gep_not_tested_mss_60mo_event_rate",
            "direct_model_gep_not_tested_mfs_5yr_median_risk",
            "direct_model_gep_failed_indeterminate_mss_60mo_median_risk",
            "direct_model_mfs_5yr_high_count",
            "direct_model_mfs_5yr_high_observed_event_rate",
            "predictor_direct_mfs_1_standardized_coefficient",
            "baseline_contrast_initial_tumor_diameter_p_value",
            "overlap_initial_tumor_diameter_abs_smd",
            "reason_for_missing_gep_available"
        ),
        intersect(c(
            "cohort_total_count",
            "gep_usable_count",
            "gep_not_tested_count",
            "gep_failed_indeterminate_count",
            "no_gep_without_usable_count",
            "no_gep_scoreable_count",
            "followup_no_gep_ge_5yr_count",
            "followup_gep_not_tested_median_years",
            "observed_gep_failed_indeterminate_mfs_5yr_event_rate",
            "observed_gep_not_tested_mss_60mo_event_rate",
            "direct_model_gep_not_tested_mfs_5yr_median_risk",
            "direct_model_gep_failed_indeterminate_mss_60mo_median_risk",
            "direct_model_mfs_5yr_high_count",
            "direct_model_mfs_5yr_high_observed_event_rate",
            "predictor_direct_mfs_1_standardized_coefficient",
            "baseline_contrast_initial_tumor_diameter_p_value",
            "overlap_initial_tumor_diameter_abs_smd",
            "reason_for_missing_gep_available"
        ), payload$semantic_id)
    )
    expect_true(all(is.finite(stats::na.omit(payload$value_numeric))))
    probability_rows <- payload %>% dplyr::filter(.data$unit == "probability_0_to_1")
    expect_true(all(probability_rows$value_numeric >= 0 & probability_rows$value_numeric <= 1))
    expect_true(all(payload$reason_for_missing_gep_available == FALSE))
    expect_false(any(grepl("direct_model_mfs_5yr_na", payload$semantic_id, fixed = TRUE)))
    expect_identical(
        payload$value_character[payload$semantic_id == "reason_for_missing_gep_available"],
        "FALSE"
    )
    expect_match(
        payload$reader_role[payload$semantic_id == "predictor_direct_mfs_1_standardized_coefficient"],
        "non-causal",
        fixed = TRUE
    )
    expect_false(any(grepl("patient|record|case|study", names(payload), ignore.case = TRUE)))
})

test_that("presentation data fails closed when a required aggregate is non-finite", {
    analysis_results <- list(
        data_audit = tibble::tibble(
            section = "Group Counts",
            group = c("Class 1", "Class 2", "GEP Failed/Indeterminate", "GEP Not Tested"),
            n = c(10L, 8L, 2L, 12L)
        ),
        prepared_data = list(no_gep_scoring = tibble::tibble(
            no_gep_group = c("GEP Failed/Indeterminate", "GEP Not Tested"),
            follow_up_years = c(2, 5)
        )),
        no_gep_subgroups = tibble::tibble(
            no_gep_group = c("GEP Failed/Indeterminate", "GEP Not Tested"),
            n = c(1L, 1L),
            observed_5yr_mfs_event_rate = c(NA_real_, 0.2),
            observed_5yr_mss_event_rate = c(0.3, 0.1),
            median_predicted_5yr_mfs_risk = c(0.5, 0.25),
            median_predicted_60mo_melanoma_death_cumulative_incidence_risk = c(0.35, 0.15)
        ),
        baseline_comparisons = tibble::tibble(variable = "diameter", test = "Kruskal-Wallis", p_value = 0.01),
        overlap_diagnostics = tibble::tibble(predictor = "diameter", worst_level = NA_character_, abs_smd = 0.84, overlap_flag = "high_shift"),
        sensitivity_summary = tibble::tibble(),
        direct_models = list(
            mfs = list(predictor_contributions = tibble::tibble(predictor = "diameter", dominant_term = "diameter", standardized_coefficient = 0.72, direction = "higher risk")),
            mss = list(predictor_contributions = tibble::tibble(predictor = "height", dominant_term = "height", standardized_coefficient = -0.31, direction = "lower risk"))
        )
    )

    expect_error(
        build_exploratory_no_gep_presentation_data(analysis_results),
        "required aggregate is non-finite"
    )
})

test_that("probability-density plotting excludes non-finite predictions without warnings", {
    output_path <- tempfile(fileext = ".png")
    withr::defer(unlink(output_path), teardown_env())
    plot_data <- tibble::tibble(
        no_gep_group = factor(c("GEP Failed/Indeterminate", "GEP Failed/Indeterminate", "GEP Not Tested", "GEP Not Tested", "GEP Not Tested")),
        predicted_mfs_5yr_risk = c(0.2, 0.3, 0.4, 0.5, NA_real_)
    )

    expect_no_warning(create_probability_density_plot(
        plot_data,
        probability_col = "predicted_mfs_5yr_risk",
        plot_title = "Test probability density",
        output_path = output_path
    ))
    expect_true(file.exists(output_path))
})

test_that("presentation data rejects duplicate keys and out-of-range probabilities", {
    base_results <- list(
        data_audit = tibble::tibble(section = "Group Counts", group = c("Class 1", "Class 2", "GEP Failed/Indeterminate", "GEP Not Tested"), n = c(10L, 8L, 2L, 12L)),
        prepared_data = list(no_gep_scoring = tibble::tibble(no_gep_group = c("GEP Failed/Indeterminate", "GEP Not Tested"), follow_up_years = c(2, 5))),
        no_gep_subgroups = tibble::tibble(no_gep_group = c("GEP Failed/Indeterminate", "GEP Not Tested"), n = c(1L, 1L), observed_5yr_mfs_event_rate = c(0.4, 0.2), observed_5yr_mss_event_rate = c(0.3, 0.1), median_predicted_5yr_mfs_risk = c(0.5, 0.25), median_predicted_60mo_melanoma_death_cumulative_incidence_risk = c(0.35, 0.15)),
        baseline_comparisons = tibble::tibble(variable = c("initial_tumor_diameter", "initial_tumor_diameter"), test = c("Kruskal-Wallis", "Kruskal-Wallis"), p_value = c(0.01, 0.02)),
        overlap_diagnostics = tibble::tibble(predictor = "diameter", worst_level = NA_character_, abs_smd = 0.84, overlap_flag = "high_shift"),
        sensitivity_summary = tibble::tibble(),
        direct_models = list(mfs = list(predictor_contributions = tibble::tibble(predictor = "diameter", dominant_term = "diameter", standardized_coefficient = 0.72, direction = "higher risk")), mss = list(predictor_contributions = tibble::tibble(predictor = "height", dominant_term = "height", standardized_coefficient = -0.31, direction = "lower risk")))
    )

    expect_error(build_exploratory_no_gep_presentation_data(base_results), "selected baseline contrast variables must be unique")

    base_results$baseline_comparisons <- base_results$baseline_comparisons[1, ]
    base_results$no_gep_subgroups$observed_5yr_mfs_event_rate[[1]] <- 1.1
    expect_error(build_exploratory_no_gep_presentation_data(base_results), "0-1 interval")
})
