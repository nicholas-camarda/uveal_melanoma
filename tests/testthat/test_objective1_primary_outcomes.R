build_objective1_output_dirs <- function(test_output_dir, include_propensity_sensitivity = FALSE) {
    dirs <- create_output_structure(
        test_output_dir,
        include_propensity_sensitivity = include_propensity_sensitivity
    )
    dirs[grepl("^obj1_", names(dirs))]
}

run_objective1_test <- function(data, output_tag = "objective1_test") {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, output_tag)
    output_dirs <- build_objective1_output_dirs(test_output_dir)

    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }

    result <- testthat::expect_no_error(
        run_objective_1(
            data = data,
            dataset_name = "test_cohort",
            output_dirs = output_dirs,
            prefix = "test_",
            confounders = c("age_at_diagnosis", "sex")
        )
    )
    list(results = result, output_dirs = output_dirs, test_output_dir = test_output_dir)
}

test_that("Objective 1 pipeline returns expected top-level analyses", {
    pipeline <- run_objective1_test(create_test_dataset())
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    expect_true(all(c(
        "recurrence_rates",
        "mets_rates",
        "os_analysis",
        "pfs_analysis",
        "height_changes",
        "primary_subgroup_results",
        "sensitivity_subgroup_results",
        "outcome_subgroup_results"
    ) %in% names(pipeline$results)))

    expect_true(file.exists(file.path(pipeline$output_dirs$obj1_os_cox, "test_overall_survival_probability_effect_summary.xlsx")))
    expect_true(file.exists(file.path(pipeline$output_dirs$obj1_pfs_cox, "test_progression_free_survival_probability_effect_summary.xlsx")))
})

test_that("Objective 1 invokes propensity sensitivity only for the restricted cohort", {
    original_runner <- run_objective1_propensity_sensitivity
    original_population_check <- assert_survival_population_contract
    calls <- new.env(parent = emptyenv())
    calls$records <- list()
    assign(
        "run_objective1_propensity_sensitivity",
        function(data, dataset_name, output_dir, prefix) {
            calls$records[[length(calls$records) + 1L]] <- list(
                dataset_name = dataset_name,
                output_dir = output_dir,
                prefix = prefix
            )
            list(stubbed = TRUE)
        },
        envir = .GlobalEnv
    )
    withr::defer(
        assign("run_objective1_propensity_sensitivity", original_runner, envir = .GlobalEnv),
        envir = parent.frame()
    )
    assign(
        "assert_survival_population_contract",
        function(...) invisible(NULL),
        envir = .GlobalEnv
    )
    withr::defer(
        assign("assert_survival_population_contract", original_population_check, envir = .GlobalEnv),
        envir = parent.frame()
    )

    restricted_dir <- file.path(TEST_OUTPUT_DIR, "objective1_propensity_route_restricted")
    other_dir <- file.path(TEST_OUTPUT_DIR, "objective1_propensity_route_other")
    restricted_outputs <- build_objective1_output_dirs(
        restricted_dir,
        include_propensity_sensitivity = TRUE
    )
    other_outputs <- build_objective1_output_dirs(other_dir)
    purrr::walk(c(restricted_outputs, other_outputs), dir.create, recursive = TRUE, showWarnings = FALSE)
    withr::defer(unlink(restricted_dir, recursive = TRUE, force = TRUE), envir = parent.frame())
    withr::defer(unlink(other_dir, recursive = TRUE, force = TRUE), envir = parent.frame())

    restricted_result <- run_objective_1(
        create_test_dataset(),
        OBJECTIVE1_PROPENSITY_DATASET,
        restricted_outputs,
        "restricted_cohort_",
        confounders = c("age_at_diagnosis", "sex")
    )
    other_result <- run_objective_1(
        create_test_dataset(),
        "other_test_cohort",
        other_outputs,
        "other_",
        confounders = c("age_at_diagnosis", "sex")
    )

    expect_length(calls$records, 1L)
    expect_identical(calls$records[[1]]$dataset_name, OBJECTIVE1_PROPENSITY_DATASET)
    expect_identical(
        calls$records[[1]]$output_dir,
        restricted_outputs$obj1_propensity_sensitivity
    )
    expect_true(isTRUE(restricted_result$propensity_sensitivity$stubbed))
    expect_null(other_result$propensity_sensitivity)
})

test_that("Objective 1 recurrence and metastasis subgroup outputs use the Cox HR contract", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "objective1_subgroup_model_contract")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    for (outcome_key in c("local_recurrence", "metastatic_progression")) {
        outcome_results <- pipeline$results$outcome_subgroup_results[[outcome_key]]
        expect_true(is.list(outcome_results))
        expect_identical(
            get_objective1_subgroup_outcome_spec(outcome_key)$model_family,
            "Cox proportional hazards"
        )
    }

    diagnostics_path <- file.path(
        pipeline$output_dirs$obj1_forest_plots,
        "test_forest_plot_diagnostics.xlsx"
    )
    for (outcome_key in c("local_recurrence", "metastatic_progression")) {
        diagnostics <- readxl::read_xlsx(diagnostics_path, sheet = outcome_key)
        expect_identical(unique(diagnostics$model_family), "Cox proportional hazards")
        expect_identical(unique(diagnostics$effect_measure), "HR")
    }
})

test_that("Objective 1 records an empty one-arm subgroup workbook as not estimable", {
    one_arm_data <- data.frame(
        treatment_group = factor(rep("GKSRS", 4), levels = c("PBT", "GKSRS"))
    )
    sheets <- finalize_objective1_subgroup_diagnostic_sheets(
        sheets = list(),
        data = one_arm_data,
        dataset_name = "gksrs_only_test",
        analysis_name = "primary_tumor_height_subgroup_analysis"
    )

    expect_identical(names(sheets), "Analysis_Status")
    expect_identical(sheets$Analysis_Status$model_status[[1]], "NOT_ESTIMABLE")
    expect_match(sheets$Analysis_Status$reason[[1]], "one treatment arm", ignore.case = TRUE)

    workbook_path <- tempfile(fileext = ".xlsx")
    withr::defer(unlink(workbook_path), envir = parent.frame())
    expect_no_error(write_readable_xlsx(sheets, workbook_path))
    expect_identical(readxl::excel_sheets(workbook_path), "Analysis_Status")
})

test_that("Objective 1 records empty sparse two-arm subgroup diagnostics without error", {
    two_arm_data <- data.frame(
        treatment_group = factor(c("PBT", "GKSRS"), levels = c("PBT", "GKSRS"))
    )
    sheets <- finalize_objective1_subgroup_diagnostic_sheets(
        sheets = list(),
        data = two_arm_data,
        dataset_name = "sparse_two_arm_test",
        analysis_name = "primary_tumor_height_subgroup_analysis"
    )

    expect_identical(names(sheets), "Analysis_Status")
    expect_identical(sheets$Analysis_Status$model_status[[1]], "NOT_ESTIMABLE")
    expect_match(sheets$Analysis_Status$reason[[1]], "modeling-feasibility requirements")
})

test_that("reviewer-facing subgroup diagnostics retain T4 support information", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "objective1_subgroup_reviewer_support")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    diagnostics_files <- list.files(
        pipeline$output_dirs$obj1_forest_plots,
        pattern = "diagnostics.*\\.xlsx$",
        recursive = TRUE,
        full.names = TRUE
    )
    expect_true(length(diagnostics_files) > 0)

    diagnostics <- purrr::map_dfr(diagnostics_files, function(path) {
        sheets <- setdiff(
            readxl::excel_sheets(path),
            "reviewer_subgroup_support_audit"
        )
        purrr::map_dfr(sheets, ~ readxl::read_xlsx(path, sheet = .x))
    })
    expect_true("reviewer_support_note" %in% names(diagnostics))
    subgroup_levels <- if ("subgroup_level" %in% names(diagnostics)) diagnostics$subgroup_level else if ("level" %in% names(diagnostics)) diagnostics$level else character()
    support_notes <- if ("reviewer_support_note" %in% names(diagnostics)) diagnostics$reviewer_support_note else character()
    expect_false(any(grepl("T4", subgroup_levels, fixed = TRUE) & is.na(support_notes)))
    expect_true(any(grepl("T4 is retained", support_notes, fixed = TRUE)))

    prame_rows <- diagnostics[diagnostics$variable == "gep12_prame_status", , drop = FALSE]
    expect_true(nrow(prame_rows) > 0)
    prame_label_columns <- intersect(c("subgroup_level", "level"), names(prame_rows))
    prame_labels <- unique(unlist(prame_rows[prame_label_columns], use.names = FALSE))
    expect_true(all(c("Negative", "Positive") %in% as.character(prame_labels)))

    audit_files <- diagnostics_files[purrr::map_lgl(diagnostics_files, ~ "reviewer_subgroup_support_audit" %in% readxl::excel_sheets(.x))]
    expect_true(length(audit_files) > 0)
    support_audit <- purrr::map_dfr(audit_files, ~ readxl::read_xlsx(.x, sheet = "reviewer_subgroup_support_audit"))
    expect_false(any(support_audit$subgroup_var == "gep12_prame_status"))
    expect_true(any(support_audit$subgroup_var == "initial_t_stage_simple" & support_audit$level == "T4"))
    expect_true(all(support_audit$observed_n >= 0, na.rm = TRUE))
    expect_true(any(
        support_audit$level == "T4" &
            grepl("retained in every reviewer-facing subgroup display", support_audit$reason, fixed = TRUE)
    ))
})

test_that("Objective 1 KM plots cap display at SURVIVAL_XAXIS_MAX_MONTHS without log-rank p-values", {
    source_text <- readLines(testthat::test_path("../../scripts/analysis/survival_outcomes.R"), warn = FALSE)
    expect_true(any(grepl("SURVIVAL_XAXIS_MAX_MONTHS", source_text, fixed = TRUE)))
    expect_true(any(grepl("surv_fit_plot", source_text, fixed = TRUE)))

    ggsurvplot_line <- grep("survminer::ggsurvplot\\(", source_text, fixed = FALSE)[1]
    expect_false(is.na(ggsurvplot_line))

    call_end <- which(seq_along(source_text) > ggsurvplot_line & grepl("^    \\)", source_text))
    expect_true(length(call_end) > 0)

    ggsurvplot_call <- source_text[ggsurvplot_line:call_end[1]]
    expect_true(any(grepl("pval = FALSE", ggsurvplot_call, fixed = TRUE)))
    expect_false(any(grepl("pval = TRUE", ggsurvplot_call, fixed = TRUE)))
})

test_that("Objective 1 tumor-height analysis writes timing summary", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "objective1_tumor_height_timing")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    timing_path <- file.path(
        pipeline$output_dirs$obj1_height_primary_timing_audit,
        "test_tumor_height_timing_summary.xlsx"
    )
    expect_true(file.exists(timing_path))
    expect_true(all(c("timing_summary", "negative_interval_detail") %in% readxl::excel_sheets(timing_path)))
    timing_rows <- readxl::read_xlsx(timing_path, sheet = "timing_summary")
    expect_true("variable" %in% names(timing_rows))
    expect_true(all(c("mean_months", "median_months") %in% names(timing_rows)))
    expect_true(any(timing_rows$variable == "last_height_followup_months"))

    negative_rows <- readxl::read_xlsx(timing_path, sheet = "negative_interval_detail")
    if (nrow(negative_rows) > 0) {
        expect_true("patient_id" %in% names(negative_rows))
    }
})

test_that("Objective 1 recurrence and metastasis event-support summaries include cumulative incidence", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "objective1_cumulative_incidence_test")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    recurrence_summary_path <- file.path(pipeline$output_dirs$obj1_recurrence_event_support, "test_recurrence1_event_support_summary.xlsx")
    mets_summary_path <- file.path(pipeline$output_dirs$obj1_mets_event_support, "test_mets_progression_event_support_summary.xlsx")

    for (summary_path in c(recurrence_summary_path, mets_summary_path)) {
        expect_true(file.exists(summary_path))
        expect_true(all(c(
            "descriptive_event_counts",
            "cumulative_incidence",
            "competing_risk_support",
            "estimand_notes"
        ) %in% readxl::excel_sheets(summary_path)))

        descriptive_counts <- readxl::read_xlsx(summary_path, sheet = "descriptive_event_counts")
        cumulative_incidence <- readxl::read_xlsx(summary_path, sheet = "cumulative_incidence")
        estimand_notes <- readxl::read_xlsx(summary_path, sheet = "estimand_notes")

        expect_true(all(descriptive_counts$estimand == "descriptive_ever_observed"))
        expect_true(any(grepl("adjusted Cox models are the lead", descriptive_counts$notes, fixed = TRUE)))
        expect_true(any(cumulative_incidence$status == "completed"))
        expect_true(any(grepl("competing event", cumulative_incidence$notes, fixed = TRUE)))
        expect_true("gray_test_global_curve_p_value" %in% names(cumulative_incidence))
        expect_false("gray_test_p_value" %in% names(cumulative_incidence))
        expect_true(any(grepl("not a per-horizon p-value", cumulative_incidence$notes, fixed = TRUE)))
        expect_true(all(c("descriptive_ever_observed", "competing_risk_cumulative_incidence") %in% estimand_notes$estimand))
        expect_false(any(estimand_notes$role == "co-primary"))
    }

    expect_false(is.null(pipeline$results$recurrence_rates$cumulative_incidence))
    expect_false(is.null(pipeline$results$mets_rates$cumulative_incidence))
})

test_that("Objective 1 survival effect summaries include canonical columns", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "objective1_effect_summary_test")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    os_summary <- readxl::read_xlsx(file.path(
        pipeline$output_dirs$obj1_os_cox,
        "test_overall_survival_probability_effect_summary.xlsx"
    ))
    pfs_summary <- readxl::read_xlsx(file.path(
        pipeline$output_dirs$obj1_pfs_cox,
        "test_progression_free_survival_probability_effect_summary.xlsx"
    ))

    expect_true(all(c("effect_measure", "estimate", "model_status") %in% names(os_summary)))
    expect_true(all(c("effect_measure", "estimate", "model_status") %in% names(pfs_summary)))
})

test_that("Objective 1 survival effect summaries include graded PH interpretation", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "objective1_ph_interpretation_test")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    os_summary <- readxl::read_xlsx(file.path(
        pipeline$output_dirs$obj1_os_cox,
        "test_overall_survival_probability_effect_summary.xlsx"
    ))
    pfs_summary <- readxl::read_xlsx(file.path(
        pipeline$output_dirs$obj1_pfs_cox,
        "test_progression_free_survival_probability_effect_summary.xlsx"
    ))

    expected_cols <- c("PH_Interpretation", "PH_Interpretation_Reason", "Interpretation_Priority")
    expect_true(all(expected_cols %in% names(os_summary)))
    expect_true(all(expected_cols %in% names(pfs_summary)))
    expect_true(all(os_summary$PH_Interpretation %in% c(
        "cox_forward",
        "cox_with_ph_caution",
        "rmst_km_forward",
        "cox_limited_ph_untestable"
    )))
})

test_that("Objective 1 survival effect summaries separate modeled patients from events", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective1_survival_n_metadata")
    output_dirs <- build_objective1_output_dirs(test_output_dir)

    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

    expect_no_error({
        analyze_time_to_event_outcomes(
            data = create_test_dataset(),
            time_var = "tt_death_months",
            event_var = "death_event",
            group_var = "treatment_group",
            model_group_var = "treatment_group",
            confounders = c("age_at_diagnosis", "sex"),
            ylab = "Overall Survival Probability",
            analysis_type = "post_treatment_only",
            dataset_name = "test_cohort",
            output_dirs = output_dirs,
            prefix = "test_"
        )
    })

    os_summary <- readxl::read_xlsx(file.path(
        output_dirs$obj1_os_cox,
        "test_overall_survival_probability_effect_summary.xlsx"
    ))

    cox_rows <- os_summary %>%
        dplyr::filter(.data$effect_measure == "HR")

    expect_true(all(cox_rows$n_patients >= cox_rows$n_events))
    expect_true(any(cox_rows$n_patients > cox_rows$n_events))
    expect_true(all(cox_rows$n_outcome_non_missing == cox_rows$n_patients))
})

test_that("Objective 1 writes patient-level KM risk-set audit workbooks", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective1_km_risk_set_audit")
    output_dirs <- build_objective1_output_dirs(test_output_dir)

    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

    analyze_time_to_event_outcomes(
        data = create_test_dataset(),
        time_var = "tt_death_months",
        event_var = "death_event",
        group_var = "treatment_group",
        confounders = c("age_at_diagnosis", "sex"),
        ylab = "Overall Survival Probability",
        analysis_type = "post_treatment_only",
        dataset_name = "test_cohort",
        output_dirs = output_dirs,
        prefix = "test_"
    )

    audit_path <- file.path(
        output_dirs$obj1_os_km,
        "test_overall_survival_probability_km_risk_set_audit.xlsx"
    )
    expect_true(file.exists(audit_path))
    expect_setequal(
        readxl::excel_sheets(audit_path),
        c("Audit_Metadata", "Risk_Set_Counts", "Risk_Set_Members", "Patient_Endpoints", "Configured_Corrections")
    )
})

test_that("Objective 1 diagnostics keep factor labels grouped before coefficients", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "objective1_diagnostics_ordering")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    diagnostics_files <- list.files(
        path = pipeline$test_output_dir,
        pattern = "_diagnostics\\.xlsx$",
        recursive = TRUE,
        full.names = TRUE
    )
    expect_true(length(diagnostics_files) > 0)

    for (file_path in diagnostics_files) {
        sheets <- readxl::excel_sheets(file_path)
        raw_sheet <- sheets[grepl("raw.*model", sheets, ignore.case = TRUE)][1]
        if (is.na(raw_sheet)) {
            next
        }

        raw_output <- readxl::read_excel(file_path, sheet = raw_sheet)
        if (!all(c("row_type", "variable") %in% names(raw_output))) {
            next
        }

        factor_indices <- which(raw_output$row_type == "Factor Label")
        for (factor_index in factor_indices) {
            factor_name <- raw_output$variable[[factor_index]]
            following_indices <- seq.int(factor_index + 1, nrow(raw_output))
            if (length(following_indices) == 0) {
                next
            }

            next_factor <- following_indices[raw_output$row_type[following_indices] == "Factor Label"][1]
            coeff_end <- if (is.na(next_factor)) nrow(raw_output) else next_factor - 1
            if (coeff_end < factor_index + 1) {
                next
            }

            coeff_rows <- raw_output[(factor_index + 1):coeff_end, , drop = FALSE]
            valid_detail_rows <- coeff_rows$row_type %in% c("Coefficient", "Reference Level", "Group Summary")
            expect_true(all(valid_detail_rows), info = basename(file_path))

            same_factor_rows <- coeff_rows$variable_base == factor_name
            expect_true(all(same_factor_rows), info = basename(file_path))
        }
    }
})

test_that("Objective 1 omits post-baseline event-status survival analyses", {
    pipeline <- run_objective1_test(
        create_test_dataset(),
        output_tag = "objective1_no_post_baseline_status_survival"
    )
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    retired_result_names <- c(
        "recurrence_os",
        "recurrence_pfs",
        "metastasis_os",
        "metastasis_pfs"
    )
    retired_route_names <- c(
        "obj1_recurrence_1a1",
        "obj1_recurrence_1a2",
        "obj1_mets_2a1",
        "obj1_mets_2a2"
    )
    retired_function_names <- c(
        "analyze_os_by_local_recurrence",
        "analyze_pfs_by_local_recurrence",
        "analyze_os_by_metastatic_progression",
        "analyze_pfs_by_metastatic_progression"
    )

    expect_false(any(retired_result_names %in% names(pipeline$results)))
    expect_false(any(retired_route_names %in% names(pipeline$output_dirs)))
    expect_false(any(vapply(retired_function_names, exists, logical(1), mode = "function")))

    output_paths <- list.files(
        pipeline$test_output_dir,
        recursive = TRUE,
        full.names = FALSE,
        all.files = TRUE
    )
    retired_output_pattern <- paste(
        c("recurrence_stratified", "metastasis_stratified", "post_baseline_exploratory_note"),
        collapse = "|"
    )
    expect_false(any(grepl(retired_output_pattern, output_paths, ignore.case = TRUE)))
})

test_that("Objective 1 centralized interpretation and subgroup contract notes are emitted once per cohort", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "objective1_contract_notes_test")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    obj1_note <- file.path(
        pipeline$test_output_dir,
        "01_Efficacy",
        "test_objective1_interpretation_notes.txt"
    )
    subgroup_note <- file.path(
        pipeline$test_output_dir,
        "01_Efficacy",
        "g_subgroup_analysis",
        "test_subgroup_analysis_contract_note.txt"
    )

    expect_true(file.exists(obj1_note))
    expect_true(file.exists(subgroup_note))
    note_lines <- readLines(obj1_note, warn = FALSE)
    expect_true(any(grepl("Cox-led time-to-event inference", note_lines, fixed = TRUE)))
    expect_true(any(grepl("descriptive support", note_lines, fixed = TRUE)))
    subgroup_note_lines <- readLines(subgroup_note, warn = FALSE)
    expect_true(any(grepl("consolidated multi-sheet Excel", subgroup_note_lines, fixed = TRUE)))
    expect_true(any(grepl("Unsupported subgroup levels remain displayed as not estimable", subgroup_note_lines, fixed = TRUE)))
})

test_that("Objective 1 subgroup diagnostics label exploratory support surfaces", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "objective1_subgroup_contract_test")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    primary_workbook <- file.path(
        pipeline$output_dirs$obj1_subgroup_primary,
        "test_primary_tumor_height_diagnostics.xlsx"
    )
    forest_workbook <- file.path(
        pipeline$output_dirs$obj1_forest_plots,
        "test_forest_plot_diagnostics.xlsx"
    )

    expect_true(file.exists(primary_workbook))
    expect_true(file.exists(forest_workbook))

    primary_sheet <- readxl::read_xlsx(primary_workbook, sheet = readxl::excel_sheets(primary_workbook)[[1]])
    forest_sheet <- readxl::read_xlsx(forest_workbook, sheet = readxl::excel_sheets(forest_workbook)[[1]])

    expect_true(all(c("analysis_role", "subgroup_surface", "interpretation_note", "estimability_method_note") %in% names(primary_sheet)))
    expect_true(all(c("analysis_role", "subgroup_surface", "interpretation_note", "estimability_method_note") %in% names(forest_sheet)))
    expect_true(all(primary_sheet$analysis_role == "exploratory_support"))
    expect_true(any(grepl("exploratory support", primary_sheet$interpretation_note, fixed = TRUE)))
    expect_true(any(grepl("interaction p-values are omitted", primary_sheet$estimability_method_note, fixed = TRUE)))
})

test_that("Objective 1 subgroup event diagnostics use the modeled endpoint", {
    subgroup_data <- tibble::tibble(
        treatment_group = rep(c("PBT", "PBT", "GKSRS", "GKSRS"), times = 2),
        subgroup_flag = factor(rep(c("A", "B"), each = 4), levels = c("A", "B")),
        recurrence1 = c(0, 0, 0, 0, 1, 0, 1, 1),
        mets_progression = c(1, 1, 1, 0, 1, 0, 1, 1),
        tt_pfs_months = c(6, 8, 7, 9, 10, 12, 11, 13),
        death_event = rep(0, 8),
        pfs_event = c(1, 1, 1, 0, 1, 0, 1, 1)
    )

    mets_result <- fit_subgroup_model(
        data = subgroup_data,
        outcome_config = list(type = "binary", outcome_var = "mets_progression"),
        subgroup_var_to_use = "subgroup_flag",
        confounders_to_use = NULL
    )
    expect_equal(mets_result$interaction_diagnostics$level_statistics$A$events_plaque, 2)
    expect_equal(mets_result$interaction_diagnostics$level_statistics$A$events_gksrs, 1)
    expect_equal(levels(mets_result$filtered_data$subgroup_flag), c("A", "B"))

    pfs_result <- fit_subgroup_model(
        data = subgroup_data,
        outcome_config = list(type = "survival", time_var = "tt_pfs_months", event_var = "pfs_event"),
        subgroup_var_to_use = "subgroup_flag",
        confounders_to_use = NULL
    )
    expect_equal(pfs_result$interaction_diagnostics$level_statistics$A$events_plaque, 2)
    expect_equal(pfs_result$interaction_diagnostics$level_statistics$A$events_gksrs, 1)
    expect_equal(attr(pfs_result$model, "subgroup_event_var"), "pfs_event")
    expect_equal(levels(pfs_result$filtered_data$subgroup_flag), c("A", "B"))
    expect_equal(pfs_result$interaction_diagnostics$supported_level_count, 2L)
    expect_identical(pfs_result$interaction_diagnostics$model_status, "interaction_model_fitted")
    expect_identical(pfs_result$interaction_diagnostics$interaction_test_status, "tested")
})

test_that("a single supported categorical level receives a treatment effect without interaction testing", {
    single_level_data <- tibble::tibble(
        treatment_group = factor(
            c("PBT", "PBT", "GKSRS", "GKSRS", "PBT", "PBT", "GKSRS", "GKSRS"),
            levels = c("PBT", "GKSRS")
        ),
        subgroup_flag = factor(
            c("A", "A", "A", "A", "B", "B", "B", "B"),
            levels = c("A", "B")
        ),
        binary_outcome = c(0, 1, 1, 1, 0, 0, 1, 1),
        follow_up = c(4, 8, 5, 9, 4, 7, 5, 8),
        survival_event = c(1, 0, 1, 1, 0, 0, 1, 1),
        continuous_outcome = c(1, 2, 3, 4, 1, 2, 3, 4)
    )

    binary_result <- fit_subgroup_model(
        data = single_level_data,
        outcome_config = list(type = "binary", outcome_var = "binary_outcome"),
        subgroup_var_to_use = "subgroup_flag",
        confounders_to_use = NULL
    )
    survival_result <- fit_subgroup_model(
        data = single_level_data,
        outcome_config = list(type = "survival", time_var = "follow_up", event_var = "survival_event"),
        subgroup_var_to_use = "subgroup_flag",
        confounders_to_use = NULL
    )
    linear_data <- single_level_data[c(1:4, 5, 7), ]
    linear_result <- fit_subgroup_model(
        data = linear_data,
        outcome_config = list(type = "continuous", outcome_var = "continuous_outcome"),
        subgroup_var_to_use = "subgroup_flag",
        confounders_to_use = NULL
    )

    expect_identical(binary_result$interaction_diagnostics$supported_levels, "A")
    expect_identical(binary_result$interaction_diagnostics$interaction_test_status, "not_testable_single_supported_level")
    expect_identical(binary_result$interaction_diagnostics$model_status, "single_supported_level_treatment_model")
    expect_true(is.na(binary_result$interaction_p))
    expect_equal(levels(binary_result$filtered_data$subgroup_flag), "A")
    expect_equal(nrow(calculate_subgroup_effects(binary_result$model, binary_result$filtered_data, "subgroup_flag", "binary", "subgroup_flag")), 1L)

    expect_identical(survival_result$interaction_diagnostics$supported_levels, "A")
    expect_identical(survival_result$interaction_diagnostics$interaction_test_status, "not_testable_single_supported_level")
    expect_true(is.finite(calculate_subgroup_effects(survival_result$model, survival_result$filtered_data, "subgroup_flag", "survival", "subgroup_flag")$treatment_effect))

    expect_identical(linear_result$interaction_diagnostics$supported_levels, "A")
    expect_identical(linear_result$interaction_diagnostics$interaction_test_status, "not_testable_single_supported_level")
    expect_true(is.finite(calculate_subgroup_effects(linear_result$model, linear_result$filtered_data, "subgroup_flag", "continuous", "subgroup_flag")$treatment_effect))
})

test_that("PRAME uses the generic single-supported-level path and retains Positive in the forest display", {
    prame_data <- tibble::tibble(
        treatment_group = factor(
            c("PBT", "PBT", "GKSRS", "GKSRS", "PBT", "PBT", "GKSRS", "GKSRS"),
            levels = c("PBT", "GKSRS")
        ),
        gep12_prame_status = factor(
            c("Negative", "Negative", "Negative", "Negative", "Positive", "Positive", "Positive", "Positive"),
            levels = c("Negative", "Positive")
        ),
        follow_up = c(4, 8, 5, 9, 4, 7, 5, 8),
        recurrence_event = c(1, 0, 1, 1, 0, 0, 1, 1)
    )

    result <- fit_subgroup_model(
        data = prame_data,
        outcome_config = list(type = "survival", time_var = "follow_up", event_var = "recurrence_event"),
        subgroup_var_to_use = "gep12_prame_status",
        confounders_to_use = NULL
    )
    effects <- calculate_subgroup_effects(
        result$model,
        result$filtered_data,
        "gep12_prame_status",
        "survival",
        "gep12_prame_status"
    )
    plot_data <- create_forest_plot_data(
        subgroup_results = list(
            gep12_prame_status = list(
                interaction_p = result$interaction_p,
                subgroup_effects = effects,
                interaction_diagnostics = result$interaction_diagnostics
            )
        ),
        variable_order = "gep12_prame_status",
        treatment_labels = TREATMENT_LABELS,
        effect_measure = "HR"
    )

    negative_row <- grepl("Negative", plot_data$data_frame$Subgroup, fixed = TRUE)
    positive_row <- grepl("Positive", plot_data$data_frame$Subgroup, fixed = TRUE)
    expect_equal(sum(negative_row), 1L)
    expect_equal(sum(positive_row), 1L)
    expect_match(plot_data$data_frame$`HR (95% CI)`[negative_row], "^[0-9.]+ ")
    expect_identical(plot_data$data_frame$`HR (95% CI)`[positive_row], "Not estimable")
    expect_identical(plot_data$data_frame$`Int p`[[1]], "Not estimable")
    expect_identical(
        plot_data$diagnostics$status[plot_data$diagnostics$subgroup_level == "Positive"],
        "not_estimable_interaction_exclusion"
    )
})

test_that("zero-supported levels and genuine model failures remain distinct diagnostics", {
    zero_supported <- fit_subgroup_model(
        data = tibble::tibble(
            treatment_group = factor(c("PBT", "PBT", "GKSRS", "GKSRS", "PBT", "PBT", "GKSRS", "GKSRS"), levels = c("PBT", "GKSRS")),
            subgroup_flag = factor(rep(c("A", "B"), each = 4), levels = c("A", "B")),
            follow_up = rep(5, 8),
            event = rep(0, 8)
        ),
        outcome_config = list(type = "survival", time_var = "follow_up", event_var = "event"),
        subgroup_var_to_use = "subgroup_flag",
        confounders_to_use = NULL
    )
    expect_null(zero_supported$model)
    expect_identical(zero_supported$interaction_diagnostics$model_status, "no_supported_levels")
    expect_identical(zero_supported$interaction_diagnostics$interaction_test_status, "not_testable_no_supported_levels")
    expect_equal(names(zero_supported$interaction_diagnostics$level_statistics), c("A", "B"))

    model_failure <- fit_subgroup_model(
        data = tibble::tibble(
            treatment_group = factor(rep(c("PBT", "GKSRS"), 4), levels = c("PBT", "GKSRS")),
            subgroup_flag = factor(rep(c("A", "B"), each = 4), levels = c("A", "B")),
            outcome = c(1, 2, 3, 4, 2, 3, 4, 5)
        ),
        outcome_config = list(type = "continuous", outcome_var = "outcome"),
        subgroup_var_to_use = "subgroup_flag",
        confounders_to_use = "missing_confounder"
    )
    expect_null(model_failure$model)
    expect_identical(model_failure$interaction_diagnostics$model_status, "model_failure")
    expect_true(nzchar(model_failure$interaction_diagnostics$model_error))
})

test_that("Objective 1 survival endpoints register typed artifact subfolders", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "objective1_output_subdivision_contract")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    expect_true(dir.exists(pipeline$output_dirs$obj1_os_km))
    expect_true(dir.exists(pipeline$output_dirs$obj1_os_cox))
    expect_true(dir.exists(pipeline$output_dirs$obj1_os_ph))
    expect_true(dir.exists(pipeline$output_dirs$obj1_recurrence_event_support))
    expect_true(file.exists(file.path(pipeline$output_dirs$obj1_os_summary, "test_overall_survival_probability_survival_rates.xlsx")))

    root_primary_files <- list.files(
        pipeline$output_dirs$obj1_os,
        pattern = "effect_summary|survival_rates|_km\\.png$",
        full.names = FALSE
    )
    expect_equal(length(root_primary_files), 0)
})
