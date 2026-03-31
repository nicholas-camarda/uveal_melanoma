build_objective4_output_dirs <- function(test_output_dir) {
    list(
        obj4_mfs = file.path(test_output_dir, "04_GEP_Validation", "a_mfs_validation"),
        obj4_mss = file.path(test_output_dir, "04_GEP_Validation", "b_mss_validation"),
        obj4_ph_diagnostics = file.path(test_output_dir, "04_GEP_Validation", "c_proportional_hazards_diagnostics")
    )
}

test_that("Objective 4 simple validation works on synthetic data", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_simple_validation")
    output_dirs <- build_objective4_output_dirs(test_output_dir)

    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

    results <- simple_gep_validation(create_test_dataset(), output_dirs, "test_")

    expect_true(all(c("mfs_results", "mss_results", "overall_summary") %in% names(results)))
    expect_s3_class(results$mfs_results, "data.frame")
    expect_s3_class(results$mss_results, "data.frame")
    expect_true(all(results$overall_summary$total_patients > 0))
})

test_that("Objective 4 writes expected simple validation workbook", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_workbook_output")
    output_dirs <- build_objective4_output_dirs(test_output_dir)

    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

    simple_gep_validation(create_test_dataset(), output_dirs, "test_")

    workbook_path <- file.path(dirname(output_dirs$obj4_mfs), "unified_summary", "test_simple_gep_validation.xlsx")
    expect_true(file.exists(workbook_path))
})

test_that("Simple GEP validation keeps endpoint-specific eligible patients", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_endpoint_specific_filtering")
    output_dirs <- build_objective4_output_dirs(test_output_dir)
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

    test_data <- create_test_dataset()
    test_data$expected_mss_5yr[1:4] <- NA_real_

    results <- simple_gep_validation(test_data, output_dirs, "endpoint_")

    mfs_total <- results$overall_summary$total_patients[results$overall_summary$outcome == "MFS"]
    mss_total <- results$overall_summary$total_patients[results$overall_summary$outcome == "MSS"]

    expect_equal(mfs_total, nrow(test_data))
    expect_equal(mss_total, nrow(test_data) - 4)
})

test_that("Simple GEP validation uses KM-adjusted MFS at 5 years", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_mfs_km_actual")
    output_dirs <- build_objective4_output_dirs(test_output_dir)
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

    test_data <- tibble::tibble(
        biopsy1_gep = c("Class 1", "Class 1", "Class 2", "Class 2"),
        gep_class_simple = c("Class 1", "Class 1", "Class 2", "Class 2"),
        biopsy1_gep_mfs = c(0.80, 0.80, 0.20, 0.20),
        biopsy1_gep_mss = c(0.85, 0.85, 0.15, 0.15),
        expected_mfs_5yr = c(0.80, 0.80, 0.20, 0.20),
        expected_mss_5yr = c(0.85, 0.85, 0.15, 0.15),
        tt_mets_months = c(12, 60, 72, 72),
        mets_event = c(0, 1, 0, 0),
        mfs_event_5yr = c(0, 1, 0, 0),
        tt_death_months = c(72, 72, 72, 72),
        tt_death_years = c(6, 6, 6, 6),
        death_event = c(0, 0, 0, 0),
        melanoma_death_event = c(0, 0, 0, 0),
        competing_death_event = c(0, 0, 0, 0),
        mss_event_5yr = c(0, 0, 0, 0),
        mfs_analysis_eligible = c(TRUE, TRUE, TRUE, TRUE),
        mss_analysis_eligible = c(TRUE, TRUE, TRUE, TRUE)
    )

    results <- simple_gep_validation(test_data, output_dirs, "km_")

    class1_mfs <- results$mfs_results %>%
        dplyr::filter(gep_class_simple == "Class 1") %>%
        dplyr::pull(actual_rate)
    class2_mfs <- results$mfs_results %>%
        dplyr::filter(gep_class_simple == "Class 2") %>%
        dplyr::pull(actual_rate)
    overall_mfs <- results$overall_summary %>%
        dplyr::filter(outcome == "MFS") %>%
        dplyr::pull(overall_actual)

    expect_equal(class1_mfs, 0)
    expect_equal(class2_mfs, 1)
    expect_equal(overall_mfs, 2 / 3)
})

test_that("MSS decision curve analysis respects month-based horizons", {
    dca_data <- tibble::tibble(
        time_to_event = c(6, 24, 66, 90, 72, 120, 48, 30, 80, 110, 55, 95, 12, 18, 45, 75, 85, 100, 58, 62),
        event_occurred = c(1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0),
        expected_mss_5yr = c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.22, 0.35, 0.42, 0.18, 0.28, 0.33, 0.14, 0.24, 0.31, 0.45, 0.39, 0.21, 0.29, 0.34)
    )

    dca_result <- perform_decision_curve_analysis_mss(
        data = dca_data,
        timepoint = 5,
        time_unit = "months"
    )
    dca_year_result <- perform_decision_curve_analysis_mss(
        data = dca_data,
        timepoint = 5,
        time_unit = "years"
    )

    expect_equal(dca_result$events, 4)
    expect_equal(dca_result$n, nrow(dca_data))
    expect_equal(dca_year_result$events, 0)
})

test_that("MSS discrimination event counts respect month-based horizons", {
    disc_data <- tibble::tibble(
        time_to_event = c(6, 24, 48, 12, 18, 30, 36, 42, 54, 60, 61, 66, 72, 75, 80, 84, 90, 95, 100, 110),
        event_occurred = c(1, 1, 1, rep(0, 17)),
        expected_mss_5yr = seq(0.15, 0.95, length.out = 20)
    )

    disc_result <- suppressWarnings(perform_discrimination_mss(disc_data, timepoint = 5))

    expect_equal(disc_result$events, 3)
    expect_equal(disc_result$events_by_timepoint, 3)
})

test_that("Unified GEP summary accepts MSS standard_validation containers", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_unified_standard_validation")
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

    validation_results <- list(
        `5yr` = list(
            calibration = list(n = 20, slope = 0.95, ici = 0.04, nam_dagostino_p = 0.3),
            discrimination = list(
                n = 20,
                events = 4,
                harrell_c = 0.81,
                integrated_auc = 0.77,
                integrated_auc_status = "ok",
                integrated_auc_method = "riskRegression::Score_integrated",
                integrated_auc_na_reason = NA_character_,
                cumulative_discrimination = 0.79,
                time_averaged_discrimination = 0.78
            )
        )
    )

    summary <- create_unified_gep_validation_summary(
        mfs_results = list(validation_results = validation_results),
        mss_results = list(standard_validation = validation_results),
        output_dir = test_output_dir,
        prefix = "portable_"
    )

    expect_true(any(summary$calibration$Outcome == "MSS"))
    expect_true(any(summary$discrimination$Outcome == "MSS"))
    expect_true(all(c("Integrated_AUC_Status", "Integrated_AUC_Method", "Integrated_AUC_Unavailable_Reason") %in% names(summary$discrimination)))
})

test_that("Objective 4 returns fatal issues and failed run_state when MSS analysis errors", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_failed_run_state")
    output_dirs <- build_objective4_output_dirs(test_output_dir)
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

    old_mfs <- analyze_gep_mfs_validation
    old_mss <- analyze_gep_mss_validation
    old_unified <- create_unified_gep_validation_summary
    old_visuals <- create_unified_gep_visuals
    old_simple <- simple_gep_validation

    withr::defer(assign("analyze_gep_mfs_validation", old_mfs, envir = .GlobalEnv), envir = parent.frame())
    withr::defer(assign("analyze_gep_mss_validation", old_mss, envir = .GlobalEnv), envir = parent.frame())
    withr::defer(assign("create_unified_gep_validation_summary", old_unified, envir = .GlobalEnv), envir = parent.frame())
    withr::defer(assign("create_unified_gep_visuals", old_visuals, envir = .GlobalEnv), envir = parent.frame())
    withr::defer(assign("simple_gep_validation", old_simple, envir = .GlobalEnv), envir = parent.frame())

    assign("analyze_gep_mfs_validation", function(...) list(validation_results = list()), envir = .GlobalEnv)
    assign("analyze_gep_mss_validation", function(...) stop("forced MSS failure"), envir = .GlobalEnv)
    assign("create_unified_gep_validation_summary", function(...) invisible(list()), envir = .GlobalEnv)
    assign("create_unified_gep_visuals", function(...) invisible(NULL), envir = .GlobalEnv)
    assign(
        "simple_gep_validation",
        function(...) list(
            mfs_results = data.frame(),
            mss_results = data.frame(),
            overall_summary = data.frame()
        ),
        envir = .GlobalEnv
    )

    results <- run_objective_4(
        data = create_test_dataset(),
        dataset_name = "uveal_melanoma_restricted_cohort",
        output_dirs = output_dirs,
        prefix = "portable_"
    )

    expect_equal(results$run_state, "failed")
    expect_true(length(results$fatal_issues) > 0)
    expect_true(any(grepl("^mss_validation:", results$fatal_issues)))
    expect_true(results$had_errors)
})
