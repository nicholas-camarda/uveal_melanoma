test_that("Objective 1 age subgroup policy uses the 63-year split without changing adjusted-model age", {
    expect_true("age_at_diagnosis" %in% confounders)
    expect_identical(
        OBJECTIVE1_AGE_SUBGROUP_VAR,
        "age_at_diagnosis_general_pop_median"
    )
    expect_identical(subgroup_vars[[1]], OBJECTIVE1_AGE_SUBGROUP_VAR)
    expect_identical(FOREST_PLOT_VARIABLE_ORDER[[1]], OBJECTIVE1_AGE_SUBGROUP_VAR)
    expect_false("age_at_diagnosis" %in% CONTINUOUS_INTERACTION_SUBGROUP_VARS)

    processed <- process_subgroup_data(
        data = tibble::tibble(
            age_at_diagnosis = c(50, 60, 70, 80),
            age_at_diagnosis_general_pop_median = factor(
                c("< 63 years", "< 63 years", "≥ 63 years", "≥ 63 years"),
                levels = c("< 63 years", "≥ 63 years")
            ),
            treatment_group = factor(c("PBT", "GKSRS", "PBT", "GKSRS")),
            sex = factor(c("Female", "Male", "Female", "Male"))
        ),
        subgroup_var = OBJECTIVE1_AGE_SUBGROUP_VAR,
        confounders = c("age_at_diagnosis", "sex")
    )

    expect_identical(
        processed$subgroup_var_to_use,
        "age_at_diagnosis_general_pop_median"
    )
    expect_false(processed$modeled_continuously)
    expect_null(processed$cutoff_value)
    expect_identical(processed$confounders_to_use, "sex")
})

test_that("subgroup preparation normalizes the legacy Plaque treatment label", {
    processed <- process_subgroup_data(
        data = tibble::tibble(
            subgroup = factor(c("A", "A", "B", "B")),
            treatment_group = factor(c("Plaque", "GKSRS", "Plaque", "GKSRS"))
        ),
        subgroup_var = "subgroup",
        confounders = NULL
    )

    expect_identical(
        levels(processed$data$treatment_group),
        c("PBT", "GKSRS")
    )
    expect_identical(
        as.character(processed$data$treatment_group),
        c("PBT", "GKSRS", "PBT", "GKSRS")
    )
})

test_that("subgroup preparation rejects unsupported treatment labels", {
    expect_error(
        process_subgroup_data(
            data = tibble::tibble(
                subgroup = factor(c("A", "A")),
                treatment_group = c("PBT", "Unknown")
            ),
            subgroup_var = "subgroup",
            confounders = NULL
        ),
        "Unsupported treatment_group values"
    )
})

test_that("the configured 63-year age split renders as ordinary forest-plot levels", {
    plot_data <- create_forest_plot_data(
        subgroup_results = list(
            age_at_diagnosis_general_pop_median = list(
                interaction_p = 0.42,
                subgroup_effects = data.frame(
                    subgroup_variable = rep(
                        "age_at_diagnosis_general_pop_median",
                        2
                    ),
                    subgroup_level = c("< 63 years", "≥ 63 years"),
                    n_total = c(90, 110),
                    n_plaque = c(45, 55),
                    n_gksrs = c(45, 55),
                    events_plaque = c(8, 12),
                    events_gksrs = c(7, 11),
                    treatment_effect = c(1.10, 1.30),
                    ci_lower = c(0.60, 0.75),
                    ci_upper = c(2.00, 2.20),
                    p_value = c(0.75, 0.35)
                ),
                modeled_continuously = FALSE,
                interaction_diagnostics = list()
            )
        ),
        variable_order = "age_at_diagnosis_general_pop_median",
        treatment_labels = TREATMENT_LABELS,
        effect_measure = "HR"
    )

    expect_identical(
        trimws(plot_data$data_frame$Subgroup),
        c("Age at Diagnosis", "< 63 years", "≥ 63 years")
    )
    expect_identical(plot_data$data_frame$`Int p`[[1]], "0.42")
    expect_identical(names(plot_data$data_frame)[2:3], c("PBT n/N", "GKSRS n/N"))
})

test_that("the existing general-population age factor remains available without duplicating it", {
    derived <- create_binned_continuous_variables(
        tibble::tibble(
            age_at_diagnosis = c(64, 65, 70),
            initial_tumor_height = c(4, 5, 6),
            initial_tumor_diameter = c(10, 11, 12)
        )
    )

    expect_true("age_at_diagnosis_general_pop_median" %in% names(derived))
    expect_false("age_at_diagnosis_subgroup_cutoff" %in% names(derived))
    expect_identical(
        levels(derived$age_at_diagnosis_general_pop_median),
        c("< 63 years", "≥ 63 years")
    )
    expect_identical(
        as.character(derived$age_at_diagnosis_general_pop_median),
        c("≥ 63 years", "≥ 63 years", "≥ 63 years")
    )
})

test_that("T4 remains available for outcome-specific subgroup fitting", {
    processed <- process_subgroup_data(
        data = tibble::tibble(
            initial_t_stage_simple = factor(
                c("T1", "T1", "T4", "T4"),
                levels = c("T1", "T2", "T3", "T4")
            ),
            treatment_group = factor(c("PBT", "GKSRS", "PBT", "GKSRS")),
            age_at_diagnosis = c(60, 61, 62, 63)
        ),
        subgroup_var = "initial_t_stage_simple",
        confounders = "age_at_diagnosis"
    )

    expect_true("T4" %in% as.character(processed$data$initial_t_stage_simple))
})

test_that("forest plots show a T4 estimate when the outcome-specific model estimates it", {
    subgroup_results <- list(
        initial_t_stage_simple = list(
            interaction_p = 0.58,
            subgroup_effects = data.frame(
                subgroup_variable = rep("initial_t_stage_simple", 2),
                subgroup_level = c("T1", "T4"),
                n_total = c(91, 26),
                n_plaque = c(48, 22),
                n_gksrs = c(43, 4),
                events_plaque = c(7, 11),
                events_gksrs = c(7, 3),
                treatment_effect = c(1.5, 1.2),
                ci_lower = c(0.52, 0.25),
                ci_upper = c(4.32, 5.75),
                p_value = c(0.45, 0.82),
                stringsAsFactors = FALSE
            ),
            interaction_diagnostics = list(
                original_level_order = c("T1", "T4")
            )
        )
    )

    plot_data <- create_forest_plot_data(
        subgroup_results = subgroup_results,
        variable_order = "initial_t_stage_simple",
        treatment_labels = TREATMENT_LABELS,
        effect_measure = "HR"
    )

    t4_row <- grepl("T4", plot_data$data_frame$Subgroup, fixed = TRUE)
    expect_equal(sum(t4_row), 1L)
    expect_identical(plot_data$data_frame$`PBT n/N`[t4_row], "11/22")
    expect_identical(plot_data$data_frame$`GKSRS n/N`[t4_row], "3/4")
    expect_match(plot_data$data_frame$`HR (95% CI)`[t4_row], "^1.20 ")
    expect_true(all(
        plot_data$diagnostics$status[plot_data$diagnostics$subgroup_level == "T4"] == "plotted"
    ))
})

test_that("forest plots label T4 not estimable only when the outcome-specific model excludes it", {
    subgroup_results <- list(
        initial_t_stage_simple = list(
            interaction_p = NA_real_,
            subgroup_effects = data.frame(),
            interaction_diagnostics = list(
                original_level_order = "T4",
                level_statistics = list(
                    T4 = list(
                        n_total = 26,
                        n_plaque = 22,
                        n_gksrs = 4,
                        events_plaque = 11,
                        events_gksrs = 0,
                        exclusion_reason = "Event count: Requires ≥1 event per arm; observed PBT events=11, GKSRS events=0"
                    )
                )
            )
        )
    )

    plot_data <- create_forest_plot_data(
        subgroup_results = subgroup_results,
        variable_order = "initial_t_stage_simple",
        treatment_labels = TREATMENT_LABELS,
        effect_measure = "HR"
    )

    t4_row <- grepl("T4", plot_data$data_frame$Subgroup, fixed = TRUE)
    expect_equal(sum(t4_row), 1L)
    expect_identical(plot_data$data_frame$`HR (95% CI)`[t4_row], "Not estimable")
    expect_identical(
        plot_data$diagnostics$status[plot_data$diagnostics$subgroup_level == "T4"],
        "not_estimable_interaction_exclusion"
    )
})

test_that("all GEP Class levels remain visible with outcome-specific estimability", {
    required_gep_levels <- c(
        "Class 1",
        "Class 2",
        "GEP Failed/Indeterminate",
        "GEP Not Tested"
    )
    expect_identical(FOREST_PLOT_REQUIRED_LEVELS$gep_class_simple, required_gep_levels)

    subgroup_results <- list(
        gep_class_simple = list(
            interaction_p = NA_real_,
            subgroup_effects = data.frame(
                subgroup_variable = rep("gep_class_simple", 3),
                subgroup_level = required_gep_levels[c(1, 2, 4)],
                n_total = c(55, 26, 160),
                n_plaque = c(28, 13, 70),
                n_gksrs = c(27, 13, 90),
                events_plaque = c(2, 6, 13),
                events_gksrs = c(0, 7, 12),
                treatment_effect = c(NA_real_, 1.35, 0.63),
                ci_lower = c(NA_real_, 0.45, 0.29),
                ci_upper = c(NA_real_, 4.07, 1.37),
                p_value = c(NA_real_, 0.60, 0.24),
                stringsAsFactors = FALSE
            ),
            interaction_diagnostics = list(
                original_level_order = required_gep_levels,
                level_statistics = list(
                    "Class 1" = list(
                        n_total = 55,
                        n_plaque = 28,
                        n_gksrs = 27,
                        events_plaque = 2,
                        events_gksrs = 0,
                        exclusion_reason = "Event count: Requires ≥1 event per arm"
                    ),
                    "GEP Failed/Indeterminate" = list(
                        n_total = 13,
                        n_plaque = 6,
                        n_gksrs = 7,
                        events_plaque = 2,
                        events_gksrs = 1,
                        exclusion_reason = ""
                    )
                )
            )
        )
    )

    plot_data <- create_forest_plot_data(
        subgroup_results = subgroup_results,
        variable_order = "gep_class_simple",
        treatment_labels = TREATMENT_LABELS,
        effect_measure = "HR"
    )
    displayed_levels <- trimws(plot_data$data_frame$Subgroup[-1])

    expect_identical(displayed_levels, c(
        "Class 1",
        "Class 2",
        "Failed or Indeterminate",
        "Not Tested"
    ))
    class1_row <- displayed_levels == "Class 1"
    expect_identical(plot_data$data_frame$`HR (95% CI)`[-1][class1_row], "Not estimable")
    expect_match(
        plot_data$data_frame$`HR (95% CI)`[-1][displayed_levels == "Class 2"],
        "^1.35 "
    )
})

test_that("both PRAME status levels remain visible with outcome-specific estimability", {
    required_prame_levels <- c("Negative", "Positive")
    expect_identical(FOREST_PLOT_REQUIRED_LEVELS$gep12_prame_status, required_prame_levels)

    subgroup_results <- list(
        gep12_prame_status = list(
            interaction_p = NA_real_,
            subgroup_effects = data.frame(
                subgroup_variable = "gep12_prame_status",
                subgroup_level = "Negative",
                n_total = 55,
                n_plaque = 28,
                n_gksrs = 27,
                events_plaque = 2,
                events_gksrs = 1,
                treatment_effect = 1.20,
                ci_lower = 0.55,
                ci_upper = 2.60,
                p_value = 0.64,
                stringsAsFactors = FALSE
            ),
            interaction_diagnostics = list(
                original_level_order = required_prame_levels,
                level_statistics = list(
                    Negative = list(
                        n_total = 55,
                        n_plaque = 28,
                        n_gksrs = 27,
                        events_plaque = 2,
                        events_gksrs = 1,
                        exclusion_reason = ""
                    ),
                    Positive = list(
                        n_total = 26,
                        n_plaque = 13,
                        n_gksrs = 13,
                        events_plaque = 0,
                        events_gksrs = 0,
                        exclusion_reason = "Event count: Requires ≥1 event per arm"
                    )
                )
            )
        )
    )

    plot_data <- create_forest_plot_data(
        subgroup_results = subgroup_results,
        variable_order = "gep12_prame_status",
        treatment_labels = TREATMENT_LABELS,
        effect_measure = "HR"
    )
    displayed_levels <- trimws(plot_data$data_frame$Subgroup[-1])

    expect_identical(displayed_levels, required_prame_levels)
    expect_match(
        plot_data$data_frame$`HR (95% CI)`[-1][displayed_levels == "Negative"],
        "^1.20 "
    )
    expect_identical(
        plot_data$data_frame$`HR (95% CI)`[-1][displayed_levels == "Positive"],
        "Not estimable"
    )
})

test_that("interaction status labels are explicit and render without weakening header hierarchy", {
    status_results <- list(
        single_supported = list(
            interaction_p = NA_real_,
            subgroup_effects = data.frame(),
            interaction_diagnostics = list(
                model_status = "single_supported_level_treatment_model",
                interaction_test_status = "not_testable_single_supported_level",
                original_level_order = "A",
                level_statistics = list(A = list(n_total = 20, n_plaque = 10, n_gksrs = 10))
            )
        ),
        no_supported = list(
            interaction_p = NA_real_,
            subgroup_effects = data.frame(),
            interaction_diagnostics = list(
                model_status = "no_supported_levels",
                interaction_test_status = "not_testable_no_supported_levels",
                original_level_order = c("A", "B"),
                level_statistics = list(
                    A = list(n_total = 8, n_plaque = 4, n_gksrs = 4),
                    B = list(n_total = 6, n_plaque = 3, n_gksrs = 3)
                )
            )
        ),
        model_failure = list(
            interaction_p = NA_real_,
            subgroup_effects = data.frame(),
            interaction_diagnostics = list(
                model_status = "model_failure",
                interaction_test_status = "model_failure",
                failure_reason = "Interaction model fitting failed",
                model_error = "synthetic failure",
                original_level_order = "A",
                level_statistics = list(A = list(n_total = 20, n_plaque = 10, n_gksrs = 10))
            )
        )
    )

    plot_data <- create_forest_plot_data(
        subgroup_results = status_results,
        variable_order = names(status_results),
        treatment_labels = TREATMENT_LABELS,
        effect_measure = "HR"
    )

    header_rows <- c(1L, 2L, 3L)
    expect_identical(
        plot_data$data_frame$`Int p`[header_rows],
        c("Not estimable", "Not estimable", "Model failed")
    )
    expect_identical(plot_data$interaction_status_rows, header_rows)
    expect_true(all(plot_data$font_face[header_rows] == "bold"))
    expect_true(all(plot_data$text_size[header_rows] == 1.0))

    diagnostics <- create_forest_plot_diagnostics(
        status_results,
        variable_order = names(status_results)
    )
    header_diagnostics <- diagnostics[diagnostics$level == "__HEADER__", , drop = FALSE]
    expect_identical(
        header_diagnostics$status,
        c(
            "interaction_not_testable_single_level",
            "not_estimable_no_supported_levels",
            "model_failure"
        )
    )
    expect_true(any(grepl("Interaction testing not possible", header_diagnostics$reason, fixed = TRUE)))
    expect_true(any(grepl("Interaction not estimable", header_diagnostics$reason, fixed = TRUE)))
    expect_true(any(grepl("synthetic failure", header_diagnostics$reason, fixed = TRUE)))

    output_path <- file.path(tempdir(), "forest_interaction_status_smoke.png")
    plot_obj <- create_forest_plot(
        subgroup_results = status_results,
        outcome_name = "Interaction Status Smoke Test",
        effect_measure = "HR",
        dataset_name = "Test",
        output_path = output_path
    )
    expect_s3_class(plot_obj, "forestplot")
    expect_true(file.exists(output_path))

    core_ids <- which(plot_obj$layout$name == "core-fg")
    expect_true(length(core_ids) > 0)
    expected_anchors <- c(0.05, rep(0.5, 6))
    for (column_index in seq_along(expected_anchors)) {
        column_ids <- core_ids[plot_obj$layout$l[core_ids] == column_index + 1L]
        expect_true(length(column_ids) > 0)
        for (grob_id in column_ids) {
            expect_equal(as.numeric(plot_obj$grobs[[grob_id]]$x), expected_anchors[[column_index]])
            expect_equal(plot_obj$grobs[[grob_id]]$hjust, expected_anchors[[column_index]])
        }
    }

    header_ids <- which(plot_obj$layout$name == "colhead-fg")
    expected_header_anchors <- c(0.05, rep(0.5, 6))
    for (column_index in seq_along(expected_header_anchors)) {
        column_ids <- header_ids[plot_obj$layout$l[header_ids] == column_index + 1L]
        expect_equal(length(column_ids), 1L)
        expect_equal(as.numeric(plot_obj$grobs[[column_ids]]$x), expected_header_anchors[[column_index]])
        expect_equal(plot_obj$grobs[[column_ids]]$hjust, if (column_index == 1L) 0 else 0.5)
    }
})
