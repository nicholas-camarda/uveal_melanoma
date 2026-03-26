test_that("main_execution discovers datasets created during objective 0 preflight", {
    temp_processed_dir <- tempfile("processed-clean-start-")
    dir.create(temp_processed_dir, recursive = TRUE, showWarnings = FALSE)

    old_processed_data_dir <- PROCESSED_DATA_DIR
    old_run_objective_0 <- run_objective_0
    old_run_my_analysis <- run_my_analysis
    old_merge_baseline_tables_with_data <- merge_baseline_tables_with_data

    analyzed_datasets <- character()

    on.exit({
        assign("PROCESSED_DATA_DIR", old_processed_data_dir, envir = .GlobalEnv)
        assign("run_objective_0", old_run_objective_0, envir = .GlobalEnv)
        assign("run_my_analysis", old_run_my_analysis, envir = .GlobalEnv)
        assign("merge_baseline_tables_with_data", old_merge_baseline_tables_with_data, envir = .GlobalEnv)
        unlink(temp_processed_dir, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    assign("PROCESSED_DATA_DIR", temp_processed_dir, envir = .GlobalEnv)
    assign("run_objective_0", function() {
        saveRDS(tibble::tibble(id = 1), file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
        saveRDS(tibble::tibble(id = 2), file.path(PROCESSED_DATA_DIR, "uveal_melanoma_restricted_cohort.rds"))
        list(
            success = TRUE,
            validated_cohorts = c("uveal_melanoma_full_cohort", "uveal_melanoma_restricted_cohort"),
            validation_errors = character(),
            created_datasets = c("uveal_melanoma_full_cohort", "uveal_melanoma_restricted_cohort")
        )
    }, envir = .GlobalEnv)
    assign("run_my_analysis", function(dataset_name, objectives_to_run = c(0, 1, 2, 3, 4)) {
        analyzed_datasets <<- c(analyzed_datasets, dataset_name)
        list(
            fatal_issues = character(),
            warning_issues = character(),
            run_state = "success",
            had_errors = FALSE,
            had_warnings = FALSE
        )
    }, envir = .GlobalEnv)
    assign("merge_baseline_tables_with_data", function(full_data, restricted_data, gksrs_only_data = NULL) {
        invisible(NULL)
    }, envir = .GlobalEnv)

    expect_no_error(main_execution())
    expect_setequal(
        analyzed_datasets,
        c("uveal_melanoma_full_cohort", "uveal_melanoma_restricted_cohort")
    )
})
