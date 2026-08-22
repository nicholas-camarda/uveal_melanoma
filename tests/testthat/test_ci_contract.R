#' Execute the fail-closed runner in a fresh R process
#'
#' @param test_dir Temporary or repository test directory to execute.
#' @param filter Optional testthat filename filter.
#' @param env Optional environment assignments for the child process.
#' @return A list containing combined child output and its integer exit status.
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

test_that("the runner rejects warnings from every unhandled testthat lifecycle boundary", {
    warning_cases <- list(
        helper = list(
            source = "warning(\"helper sentinel\")",
            file = "helper-warning.R",
            sentinel = "helper sentinel",
            test = "testthat::test_that(\"helper\", testthat::succeed())"
        ),
        setup = list(
            source = "warning(\"setup sentinel\")",
            file = "setup-warning.R",
            sentinel = "setup sentinel",
            test = "testthat::test_that(\"setup\", testthat::succeed())"
        ),
        top_level = list(
            source = c(
                "warning(\"top-level sentinel\")",
                "testthat::test_that(\"top-level\", testthat::succeed())"
            ),
            file = "test-top-level.R",
            sentinel = "top-level sentinel",
            test = NULL
        ),
        test_body = list(
            source = "testthat::test_that(\"body\", { warning(\"body sentinel\"); testthat::succeed() })",
            file = "test-body.R",
            sentinel = "body sentinel",
            test = NULL
        ),
        teardown = list(
            source = paste(
                "withr::defer(warning(\"teardown sentinel\"),",
                "envir = testthat::teardown_env())"
            ),
            file = "setup-teardown.R",
            sentinel = "teardown sentinel",
            test = "testthat::test_that(\"teardown\", testthat::succeed())"
        )
    )

    for (case_name in names(warning_cases)) {
        case <- warning_cases[[case_name]]
        warning_dir <- withr::local_tempdir()
        writeLines(case$source, file.path(warning_dir, case$file))
        if (!is.null(case$test)) {
            writeLines(case$test, file.path(warning_dir, "test-one.R"))
        }

        warning_run <- run_testthat_subprocess(warning_dir)
        expect_true(warning_run$status > 0L, info = case_name)
        expect_match(
            paste(warning_run$output, collapse = "\n"),
            case$sentinel,
            fixed = TRUE
        )
    }
})

test_that("expected warnings remain valid when the lifecycle gate is active", {
    expected_dir <- withr::local_tempdir()
    writeLines(
        paste(
            "testthat::test_that(\"expected warning\",",
            "testthat::expect_warning(warning(\"expected sentinel\"),",
            "\"expected sentinel\"))",
            sep = " "
        ),
        file.path(expected_dir, "test-expected.R")
    )

    expected_run <- run_testthat_subprocess(expected_dir)
    expect_identical(expected_run$status, 0L)
})

test_that("runner manifests replace static case counting and detect file drift", {
    runner <- new.env(parent = globalenv())
    sys.source(here::here("scripts", "tools", "run_testthat.R"), envir = runner)

    test_dir <- withr::local_tempdir()
    writeLines("testthat::test_that('one', testthat::succeed())", file.path(test_dir, "test_one.R"))
    writeLines("testthat::test_that('two', testthat::succeed())", file.path(test_dir, "test_two.R"))
    writeLines(c("test_one.R", "test_two.R"), file.path(test_dir, "required-test-files.txt"))
    result <- testthat::test_dir(
        test_dir,
        filter = "one",
        reporter = "silent",
        stop_on_failure = FALSE,
        stop_on_warning = FALSE
    )

    summary <- runner$summarize_testthat_result(result, test_dir, filter = "one")
    expect_identical(summary$cases, 1L)
    expect_identical(summary$missing_required_files, character())
    expect_identical(summary$unexpected_files, character())
    expect_invisible(runner$assert_testthat_result(summary))

    inventory_dir <- withr::local_tempdir()
    inventory_path <- file.path(inventory_dir, "test_inventory.R")
    writeLines(c(
        "for (i in seq_len(2L)) {",
        "    testthat::test_that(paste('generated', i), testthat::succeed())",
        "}"
    ), inventory_path)
    writeLines("test_inventory.R", file.path(inventory_dir, "required-test-files.txt"))
    expect_identical(run_testthat_subprocess(inventory_dir)$status, 0L)

    writeLines(c("test_inventory.R", "test_missing.R"), file.path(inventory_dir, "required-test-files.txt"))
    missing_file <- run_testthat_subprocess(inventory_dir)
    expect_gt(missing_file$status, 0L)
    expect_match(
        paste(missing_file$output, collapse = "\n"),
        "Missing required test files: test_missing.R",
        fixed = TRUE
    )

    writeLines("test_inventory.R", file.path(inventory_dir, "required-test-files.txt"))
    writeLines("testthat::test_that('unexpected', testthat::succeed())", file.path(inventory_dir, "test-extra.R"))
    unexpected_file <- run_testthat_subprocess(inventory_dir)
    expect_gt(unexpected_file$status, 0L)
    expect_match(
        paste(unexpected_file$output, collapse = "\n"),
        "Unexpected test files: test-extra.R",
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
    expect_match(workflow_text, "setdiff(names(locked_packages)", fixed = TRUE)
    expect_match(workflow_text, 'c("Version", "Source", "RemoteSha")', fixed = TRUE)
    expect_match(workflow_text, "Restored library does not match renv.lock", fixed = TRUE)
    expect_false(grepl("status$synchronized", workflow_text, fixed = TRUE))
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
    expect_false(grepl("OCULAR_EXPECTED_TEST_CASES", command_text, fixed = TRUE))
    expect_true(file.exists(here::here("tests", "testthat", "required-test-files.txt")))
    expect_true(file.exists(here::here("tests", "portable", "required-test-files.txt")))
})

test_that("actual-data integration inventory is checked in and complete", {
    integration_dir <- here::here("tests", "integration")
    manifest_path <- file.path(integration_dir, "required-test-files.txt")
    expect_true(file.exists(manifest_path))

    discovered_files <- sort(list.files(
        integration_dir,
        pattern = "^test.*\\.[rR]$",
        full.names = FALSE
    ))
    required_files <- sort(readLines(manifest_path, warn = FALSE))
    expect_identical(required_files, discovered_files)

    runner_text <- paste(
        readLines(here::here("scripts", "tools", "run_testthat.R"), warn = FALSE),
        collapse = "\n"
    )
    expect_match(
        runner_text,
        'c("testthat", "portable", "integration")',
        fixed = TRUE
    )

    temporary_root <- withr::local_tempdir()
    missing_manifest_dir <- file.path(temporary_root, "integration")
    dir.create(missing_manifest_dir)
    writeLines(
        "testthat::test_that('sentinel', testthat::succeed())",
        file.path(missing_manifest_dir, "test_sentinel.R")
    )
    missing_manifest <- run_testthat_subprocess(missing_manifest_dir)
    expect_gt(missing_manifest$status, 0L)
    expect_match(
        paste(missing_manifest$output, collapse = "\n"),
        "Required test manifest is missing",
        fixed = TRUE
    )
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
    expect_match(runner_text, "required-test-files.txt", fixed = TRUE)
    expect_match(runner_text, "warn = 2L", fixed = TRUE)
    expect_false(grepl("count_test_declarations", runner_text, fixed = TRUE))
    expect_false(grepl("OCULAR_EXPECTED_TEST_CASES", runner_text, fixed = TRUE))
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

test_that("pull request protocol requires complete local and remote validation", {
    contributing_text <- paste(
        readLines(here::here("CONTRIBUTING.md"), warn = FALSE),
        collapse = "\n"
    )
    template_path <- here::here(".github", "pull_request_template.md")

    expect_match(
        contributing_text,
        "Targeted tests do not replace this complete gate.",
        fixed = TRUE
    )
    expect_match(contributing_text, "gh pr checks <number> --watch", fixed = TRUE)
    expect_match(contributing_text, "Do not report a pull request as ready", fixed = TRUE)
    expect_true(file.exists(template_path))

    template_text <- paste(readLines(template_path, warn = FALSE), collapse = "\n")
    expect_match(
        template_text,
        "Rscript scripts/tools/run_portable_suite.R",
        fixed = TRUE
    )
    expect_match(template_text, "required GitHub check is green", fixed = TRUE)
})

test_that("standard testthat entrypoint delegates to the canonical portable suite", {
    entrypoint_text <- paste(readLines(here::here("tests", "testthat.R"), warn = FALSE), collapse = "\n")

    expect_match(entrypoint_text, "run_portable_suite", fixed = TRUE)
    expect_false(grepl("testthat::test_dir", entrypoint_text, fixed = TRUE))
})
