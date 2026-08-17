test_that("objective fixture wrappers are isolated and exactly restored", {
    target <- new.env(parent = emptyenv())
    first_original <- identity
    second_original <- abs
    assign("first", first_original, envir = target)
    assign("second", second_original, envir = target)

    state <- create_objective_fixture_state(
        target_env = target,
        entrypoints = c(first = "objective1", second = "objective2")
    )
    install_objective_entrypoint_wrappers(state)

    expect_false(identical(target$first, first_original))
    expect_false(identical(target$second, second_original))
    expect_identical(target$first(2L), 2L)
    expect_identical(target$second(-3L), 3L)
    expect_identical(
        state$counts,
        c(objective1 = 1L, objective2 = 1L)
    )

    dispose_objective_fixture_state(state, assert_counts = TRUE)
    expect_identical(target$first, first_original)
    expect_identical(target$second, second_original)
    expect_false(state$active)
    expect_length(state$results, 0L)
    expect_length(state$wrappers, 0L)
})

test_that("failed count validation still restores wrappers and clears cache", {
    target <- new.env(parent = emptyenv())
    original <- identity
    assign("entrypoint", original, envir = target)
    state <- create_objective_fixture_state(
        target_env = target,
        entrypoints = c(entrypoint = "objective1")
    )
    state$results$sentinel <- "must be cleared"
    install_objective_entrypoint_wrappers(state)

    expect_error(
        dispose_objective_fixture_state(state, assert_counts = TRUE),
        "Full-pipeline execution counts rejected",
        fixed = TRUE
    )
    expect_identical(target$entrypoint, original)
    expect_false(state$active)
    expect_length(state$results, 0L)
    expect_length(state$originals, 0L)
})

test_that("two same-session suites use fresh setup and leave no wrapper state", {
    script <- withr::local_tempfile(fileext = ".R")
    writeLines(
        c(
            sprintf("setwd(%s)", dQuote(here::here())),
            # These are deliberately filtered lifecycle probes, not complete
            # portable-suite runs, so they must not inherit the parent's
            # full-suite execution-count assertion.
            "Sys.unsetenv('OCULAR_PORTABLE_SUITE')",
            "source('scripts/load_all.R')",
            paste0(
                "entrypoints <- c('run_objective_1', 'run_objective_2', ",
                "'run_objective_3', 'run_objective_4', ",
                "'merge_baseline_tables_with_data')"
            ),
            "originals <- mget(entrypoints, envir = .GlobalEnv, inherits = FALSE)",
            paste0(
                "path_names <- c('RUNTIME_ROOT', 'EXPORT_PARENT_DIR', ",
                "'EXPORT_ROOT', 'EXPORT_ANALYSIS_DIR', 'TEST_OUTPUT_DIR', ",
                "'DATA_DIR', 'PROCESSED_DATA_DIR', 'OUTPUT_DIR', ",
                "'RAW_DATA_DIR', 'TOOLS_OUTPUT_DIR', 'MERGED_TABLES_DIR', ",
                "'LOGS_DIR')"
            ),
            "path_values <- mget(path_names, envir = .GlobalEnv, inherits = FALSE)",
            "temporary_before <- list.files(tempdir(), pattern = '^ocular-testthat-', full.names = TRUE)",
            paste0(
                "first <- testthat::test_dir('tests/testthat', ",
                "filter = 'synthetic_fixture_contract', reporter = 'silent', ",
                "stop_on_failure = FALSE, stop_on_warning = FALSE)"
            ),
            "stopifnot(sum(as.data.frame(first)$failed) == 0L)",
            "after_first <- mget(entrypoints, envir = .GlobalEnv, inherits = FALSE)",
            "stopifnot(all(mapply(identical, after_first, originals)))",
            "stopifnot(identical(mget(path_names, envir = .GlobalEnv, inherits = FALSE), path_values))",
            paste0(
                "second <- testthat::test_dir('tests/testthat', ",
                "filter = 'synthetic_fixture_contract', reporter = 'silent', ",
                "stop_on_failure = FALSE, stop_on_warning = FALSE)"
            ),
            "stopifnot(sum(as.data.frame(second)$failed) == 0L)",
            "after_second <- mget(entrypoints, envir = .GlobalEnv, inherits = FALSE)",
            "stopifnot(all(mapply(identical, after_second, originals)))",
            "stopifnot(identical(mget(path_names, envir = .GlobalEnv, inherits = FALSE), path_values))",
            "stopifnot(!nzchar(Sys.getenv('OCULAR_TESTTHAT_BOOTSTRAPPED', unset = '')))",
            "temporary_after <- list.files(tempdir(), pattern = '^ocular-testthat-', full.names = TRUE)",
            "stopifnot(identical(sort(temporary_after), sort(temporary_before)))"
        ),
        script
    )

    output <- suppressWarnings(system2(
        file.path(R.home("bin"), "Rscript"),
        script,
        stdout = TRUE,
        stderr = TRUE
    ))
    status <- attr(output, "status")
    expect_identical(if (is.null(status)) 0L else status, 0L, info = output)
})

test_that("parallel test-file execution remains explicitly disabled", {
    description <- read.dcf(here::here("DESCRIPTION"))
    expect_identical(unname(description[1L, "Config/testthat/parallel"]), "false")
})
