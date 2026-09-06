test_that("Objective 0 orchestration delegates cohort persistence to save_cohorts", {
    orchestration_lines <- readLines(
        here::here("scripts", "data_helper", "cohort_orchestration.R"),
        warn = FALSE
    )

    expect_length(grep(
        "save_cohorts\\(factored_filtered_data\\)",
        orchestration_lines
    ), 1)
    expect_false(any(grepl(
        "write_readable_xlsx\\(factored_filtered_data",
        orchestration_lines
    )))
    expect_false(any(grepl(
        "saveRDS\\(factored_filtered_data",
        orchestration_lines
    )))
})

test_that("validation_utilities does not redefine the canonical engine validators", {
    utility_env <- new.env(parent = baseenv())
    sys.source(
        here::here("scripts", "utils", "validation_utilities.R"),
        envir = utility_env
    )
    engine_validators <- c(
        "validate_cohort_integrity",
        "validate_factor_level_consistency",
        "validate_processing_pipeline",
        "generate_validation_report",
        "validate_single_cohort_comprehensive",
        "validate_cross_cohort_consistency",
        "validate_processed_files_exist"
    )

    expect_length(intersect(ls(utility_env, all.names = TRUE), engine_validators), 0L)
    expect_true(is.function(utility_env$get_expected_analytic_cohort_names))
})
