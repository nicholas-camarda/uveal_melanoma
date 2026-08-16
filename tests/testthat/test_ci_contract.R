run_testthat_subprocess <- function(test_dir, filter = NULL, env = character()) {
    args <- c(here::here("scripts", "tools", "run_testthat.R"), test_dir)
    if (!is.null(filter)) {
        args <- c(args, "--filter", filter)
    }

    output <- suppressWarnings(system2(
        file.path(R.home("bin"), "Rscript"),
        args,
        env = env,
        stdout = TRUE,
        stderr = TRUE
    ))
    status <- attr(output, "status")
    list(output = output, status = if (is.null(status)) 0L else status)
}

test_that("the directory runner rejects unexpected warnings and skips", {
    warning_dir <- withr::local_tempdir()
    writeLines(
        'testthat::test_that("warns", { warning("sentinel warning"); testthat::succeed() })',
        file.path(warning_dir, "test_warn.R")
    )
    warning_run <- run_testthat_subprocess(warning_dir)

    expect_gt(warning_run$status, 0L)
    expect_match(paste(warning_run$output, collapse = "\n"), "sentinel warning", fixed = TRUE)

    skip_dir <- withr::local_tempdir()
    writeLines(
        'testthat::test_that("skips", testthat::skip("sentinel skip"))',
        file.path(skip_dir, "test_skip.R")
    )
    skip_run <- run_testthat_subprocess(skip_dir)

    expect_gt(skip_run$status, 0L)
    expect_match(paste(skip_run$output, collapse = "\n"), "sentinel skip", fixed = TRUE)
})

test_that("runner result summary detects a discovered file that did not execute", {
    runner <- new.env(parent = globalenv())
    sys.source(here::here("scripts", "tools", "run_testthat.R"), envir = runner)

    test_dir <- withr::local_tempdir()
    writeLines("testthat::test_that('one', testthat::succeed())", file.path(test_dir, "test_one.R"))
    writeLines("testthat::test_that('two', testthat::succeed())", file.path(test_dir, "test_two.R"))
    result <- testthat::test_dir(
        test_dir,
        filter = "one",
        reporter = "silent",
        stop_on_failure = FALSE,
        stop_on_warning = FALSE
    )

    summary <- runner$summarize_testthat_result(result, test_dir)
    expect_error(
        runner$assert_testthat_result(summary),
        "Unexecuted test files: test_two.R",
        fixed = TRUE
    )

    inventory_dir <- withr::local_tempdir()
    inventory_path <- file.path(inventory_dir, "test_inventory.R")
    writeLines(c(
        "testthat::test_that('one', testthat::succeed())",
        "testthat::test_that('two', testthat::succeed())"
    ), inventory_path)
    inventory_env <- c(
        "OCULAR_EXPECTED_TEST_FILES=1",
        "OCULAR_EXPECTED_TEST_CASES=2"
    )
    expect_identical(run_testthat_subprocess(inventory_dir, env = inventory_env)$status, 0L)

    writeLines("testthat::test_that('one', testthat::succeed())", inventory_path)
    omitted_case <- run_testthat_subprocess(inventory_dir, env = inventory_env)
    expect_gt(omitted_case$status, 0L)
    expect_match(
        paste(omitted_case$output, collapse = "\n"),
        "Expected 2 test cases but declared 1",
        fixed = TRUE
    )
})

test_that("portable CI exposes one complete required check", {
    workflow_path <- here::here(".github", "workflows", "portable-tests.yml")
    expect_true(file.exists(workflow_path))

    workflow_text <- paste(readLines(workflow_path, warn = FALSE), collapse = "\n")
    expect_match(workflow_text, "pull_request:", fixed = TRUE)
    expect_match(workflow_text, "push:", fixed = TRUE)
    expect_match(workflow_text, "- master", fixed = TRUE)
    expect_match(workflow_text, "workflow_dispatch:", fixed = TRUE)
    expect_match(workflow_text, "name: required", fixed = TRUE)
    expect_match(workflow_text, "cancel-in-progress:", fixed = TRUE)
    expect_match(workflow_text, "runs-on: ubuntu-24.04", fixed = TRUE)
    expect_false(grepl("ubuntu-latest", workflow_text, fixed = TRUE))
    expect_false(grepl("continue-on-error", workflow_text, fixed = TRUE))
    expect_false(grepl("\\|\\|[[:space:]]+true", workflow_text))
})

test_that("portable CI pins current action implementations and restores renv", {
    workflow_text <- paste(
        readLines(here::here(".github", "workflows", "portable-tests.yml"), warn = FALSE),
        collapse = "\n"
    )

    expect_match(
        workflow_text,
        "actions/checkout@8e8c483db84b4bee98b60c0593521ed34d9990e8",
        fixed = TRUE
    )
    expect_match(
        workflow_text,
        "r-lib/actions/setup-r@d3c5be51b12e724e68f33216ca3c148b66d5f0b6",
        fixed = TRUE
    )
    expect_match(
        workflow_text,
        "r-lib/actions/setup-renv@d3c5be51b12e724e68f33216ca3c148b66d5f0b6",
        fixed = TRUE
    )
    expect_match(workflow_text, 'r-version: "4.4.3"', fixed = TRUE)
    expect_match(workflow_text, "renv.lock", fixed = TRUE)
    expect_match(workflow_text, "status$synchronized", fixed = TRUE)
    expect_match(workflow_text, "renv environment is not synchronized", fixed = TRUE)
    expect_match(workflow_text, "RENV_CONFIG_REPOS_OVERRIDE", fixed = TRUE)
    expect_false(grepl("actions/checkout@v4", workflow_text, fixed = TRUE))
    expect_false(grepl("setup-r-dependencies", workflow_text, fixed = TRUE))
    expect_false(grepl("extra-packages", workflow_text, fixed = TRUE))
    expect_false(grepl("npm", workflow_text, ignore.case = TRUE))
})

test_that("portable CI runs only the canonical complete command", {
    workflow_text <- paste(
        readLines(here::here(".github", "workflows", "portable-tests.yml"), warn = FALSE),
        collapse = "\n"
    )

    canonical_command <- "Rscript scripts/tools/run_portable_suite.R"
    expect_equal(stringr::str_count(workflow_text, fixed(canonical_command)), 1L)
    expect_false(grepl("--filter", workflow_text, fixed = TRUE))
    expect_false(grepl("FAST_TEST_FILTER", workflow_text, fixed = TRUE))
    expect_false(grepl("inputs.suite", workflow_text, fixed = TRUE))
    expect_false(grepl("  full:", workflow_text, fixed = TRUE))
})

test_that("canonical portable command owns every portable stage", {
    command_path <- here::here("scripts", "tools", "run_portable_suite.R")
    expect_true(file.exists(command_path))
    command_text <- paste(readLines(command_path, warn = FALSE), collapse = "\n")

    expect_match(command_text, "tests/testthat", fixed = TRUE)
    expect_match(command_text, "tests/portable", fixed = TRUE)
    expect_match(command_text, "lintr::lint_package()", fixed = TRUE)
    expect_match(command_text, "OCULAR_PORTABLE_SUITE=true", fixed = TRUE)
    expect_match(command_text, "OCULAR_EXPECTED_TEST_FILES=41", fixed = TRUE)
    expect_match(command_text, "OCULAR_EXPECTED_TEST_CASES=271", fixed = TRUE)
})

test_that("the lockfile records the safe Deriv build and pinned rmda source", {
    lock <- jsonlite::fromJSON(here::here("renv.lock"), simplifyVector = FALSE)

    expect_equal(lock$R$Version, "4.4.3")
    expect_equal(lock$Packages$Deriv$Version, "4.1.6")
    expect_equal(lock$Packages$rmda$Source, "GitHub")
    expect_equal(
        lock$Packages$rmda$RemoteSha,
        "84a11aff0e21793a834e1127968da644739c6bed"
    )
})

test_that("bootstrap and runner are lockfile- and failure-sensitive", {
    bootstrap_text <- paste(
        readLines(here::here("scripts", "bootstrap_packages.R"), warn = FALSE),
        collapse = "\n"
    )
    runner_text <- paste(
        readLines(here::here("scripts", "tools", "run_testthat.R"), warn = FALSE),
        collapse = "\n"
    )

    expect_match(bootstrap_text, "renv::restore", fixed = TRUE)
    expect_false(grepl("required_packages", bootstrap_text, fixed = TRUE))
    expect_false(grepl("install.packages", bootstrap_text, fixed = TRUE))
    expect_match(runner_text, "filter = filter", fixed = TRUE)
    expect_match(runner_text, "assert_testthat_result(summary)", fixed = TRUE)
    expect_match(runner_text, "fail_on_warning = TRUE", fixed = TRUE)
    expect_match(runner_text, "fail_on_skip = TRUE", fixed = TRUE)
})

test_that("OpenSpec records remain available without active CI enforcement", {
    workflow_text <- paste(
        readLines(here::here(".github", "workflows", "portable-tests.yml"), warn = FALSE),
        collapse = "\n"
    )

    expect_false(grepl("@fission-ai/openspec", workflow_text, fixed = TRUE))
    expect_false(grepl("openspec validate", workflow_text, fixed = TRUE))
    expect_true(file.exists(here::here("openspec", "config.yaml")))
    expect_gt(length(list.files(here::here("openspec", "specs"), recursive = TRUE)), 0L)
    expect_gt(length(list.files(here::here("openspec", "changes", "archive"), recursive = TRUE)), 0L)
})

test_that("lintr contract permits the repository's established mixed pipe syntax", {
    lintr_text <- paste(readLines(here::here(".lintr"), warn = FALSE), collapse = "\n")

    expect_false(grepl("pipe_consistency_linter", lintr_text, fixed = TRUE))
})

test_that("documented test commands use the fail-sensitive runner", {
    readme_text <- paste(readLines(here::here("README.md"), warn = FALSE), collapse = "\n")

    expect_match(readme_text, "Rscript scripts/bootstrap_packages.R", fixed = TRUE)
    expect_match(readme_text, "Rscript scripts/tools/run_portable_suite.R", fixed = TRUE)
    expect_match(readme_text, "Rscript scripts/tools/run_testthat.R tests/integration", fixed = TRUE)
})

test_that("standard testthat entrypoint delegates to the canonical portable suite", {
    entrypoint_text <- paste(readLines(here::here("tests", "testthat.R"), warn = FALSE), collapse = "\n")

    expect_match(entrypoint_text, "run_portable_suite", fixed = TRUE)
    expect_false(grepl("testthat::test_dir", entrypoint_text, fixed = TRUE))
})
