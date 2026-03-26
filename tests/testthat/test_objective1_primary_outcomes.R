build_objective1_output_dirs <- function(test_output_dir) {
    list(
        obj1_recurrence = file.path(test_output_dir, "01_Efficacy", "a_recurrence"),
        obj1_mets = file.path(test_output_dir, "01_Efficacy", "b_metastatic_progression"),
        obj1_os = file.path(test_output_dir, "01_Efficacy", "c_overall_survival"),
        obj1_pfs = file.path(test_output_dir, "01_Efficacy", "d_progression_free_survival"),
        obj1_height_primary = file.path(test_output_dir, "01_Efficacy", "e_tumor_height_primary"),
        obj1_height_sensitivity = file.path(test_output_dir, "01_Efficacy", "f_tumor_height_sensitivity"),
        obj1_subgroup_primary = file.path(test_output_dir, "01_Efficacy", "g_subgroup_analysis", "tumor_height_primary"),
        obj1_subgroup_sensitivity = file.path(test_output_dir, "01_Efficacy", "g_subgroup_analysis", "tumor_height_sensitivity"),
        obj1_forest_plots = file.path(test_output_dir, "01_Efficacy", "g_subgroup_analysis", "forest_plots"),
        obj1_ph_diagnostics = file.path(test_output_dir, "01_Efficacy", "h_proportional_hazards_diagnostics")
    )
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
        "sensitivity_subgroup_results"
    ) %in% names(pipeline$results)))

    expect_true(file.exists(file.path(pipeline$output_dirs$obj1_os, "test_overall_survival_probability_effect_summary.xlsx")))
    expect_true(file.exists(file.path(pipeline$output_dirs$obj1_pfs, "test_progression_free_survival_probability_effect_summary.xlsx")))
})

test_that("Objective 1 survival effect summaries include canonical columns", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "objective1_effect_summary_test")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    os_summary <- readxl::read_xlsx(file.path(
        pipeline$output_dirs$obj1_os,
        "test_overall_survival_probability_effect_summary.xlsx"
    ))
    pfs_summary <- readxl::read_xlsx(file.path(
        pipeline$output_dirs$obj1_pfs,
        "test_progression_free_survival_probability_effect_summary.xlsx"
    ))

    expect_true(all(c("effect_measure", "estimate", "model_status") %in% names(os_summary)))
    expect_true(all(c("effect_measure", "estimate", "model_status") %in% names(pfs_summary)))
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
        output_dirs$obj1_os,
        "test_overall_survival_probability_effect_summary.xlsx"
    ))

    cox_rows <- os_summary %>%
        dplyr::filter(.data$effect_measure == "HR")

    expect_true(all(cox_rows$n_patients >= cox_rows$n_events))
    expect_true(any(cox_rows$n_patients > cox_rows$n_events))
    expect_true(all(cox_rows$n_outcome_non_missing == cox_rows$n_patients))
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
