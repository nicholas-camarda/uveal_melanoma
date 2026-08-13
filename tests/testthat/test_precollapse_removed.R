test_that("isolated Objective 0 output contains only canonical analytic dataset pairs", {
    processed_root <- tempfile("objective0-canonical-output-")
    dir.create(processed_root, recursive = TRUE)
    old_processed_root <- PROCESSED_DATA_DIR
    withr::defer({
        assign("PROCESSED_DATA_DIR", old_processed_root, envir = .GlobalEnv)
        unlink(processed_root, recursive = TRUE, force = TRUE)
    })
    assign("PROCESSED_DATA_DIR", processed_root, envir = .GlobalEnv)

    cohort_ids <- PUBLISH_ANALYTIC_DATASET_IDS
    cohort_data <- stats::setNames(
        lapply(cohort_ids, function(dataset_id) {
            data.frame(
                id = 1:2,
                biopsy1_gep = factor(c("Class 1", "GEP Not Tested")),
                baseline_value = c(10, 20)
            )
        }),
        cohort_ids
    )
    rlang::local_bindings(
        load_and_clean_data = function(...) data.frame(id = 1:2),
        create_derived_variables = function(data) data,
        prepare_factor_levels = function(data) list(data = data),
        apply_criteria = function(data) list(cohorts = cohort_data, removal_log = tibble::tibble()),
        create_summary_tables = function(...) list(),
        export_cohort_summary = function(...) invisible(NULL),
        .env = .GlobalEnv
    )

    result <- create_analytic_dataset(
        output_dirs = NULL,
        validate_after_saving = FALSE
    )
    created_files <- list.files(processed_root)

    expect_setequal(
        created_files,
        as.vector(outer(cohort_ids, c(".rds", ".xlsx"), paste0))
    )
    expect_false(any(grepl("precollapse|pre-collapse", created_files, ignore.case = TRUE)))
    expect_equal(result$analytic_data[[cohort_ids[[1]]]]$baseline_value, c(10, 20))
    expect_equal(
        as.character(result$analytic_data[[cohort_ids[[1]]]]$biopsy1_gep),
        c("Class 1", "GEP Not Tested")
    )
})

test_that("live code, tests, and present-state documentation contain no precollapse path", {
    roots <- c("scripts", "tests", "docs")
    paths <- unlist(lapply(roots, function(root) {
        list.files(root, recursive = TRUE, full.names = TRUE)
    }))
    paths <- c(paths, "README.md")
    paths <- paths[basename(paths) != "test_precollapse_removed.R"]
    text <- unlist(lapply(paths[file.exists(paths)], readLines, warn = FALSE), use.names = FALSE)

    expect_false(any(grepl(
        "precollapse|pre-collapse|pre_collaps|restore_gep_display|restore_precollapse|apply_precollapse",
        text,
        ignore.case = TRUE
    )))
})
