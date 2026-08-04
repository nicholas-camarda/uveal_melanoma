test_that("portable CI runs required checks with fail-sensitive commands", {
    workflow_path <- here::here(".github", "workflows", "portable-tests.yml")
    expect_true(file.exists(workflow_path))

    workflow_text <- paste(readLines(workflow_path, warn = FALSE), collapse = "\n")
    expect_match(workflow_text, "scripts/tools/run_testthat.R tests/testthat", fixed = TRUE)
    expect_match(workflow_text, "scripts/tools/run_testthat.R tests/integration", fixed = TRUE)
    expect_match(workflow_text, "OCULAR_RUN_INTEGRATION_TESTS: true", fixed = TRUE)
    expect_match(workflow_text, "lintr::lint_package()", fixed = TRUE)
    expect_false(grepl("continue-on-error", workflow_text, fixed = TRUE))
    expect_false(grepl("\\|\\|[[:space:]]+true", workflow_text))
})

test_that("portable CI does not duplicate feature-branch push and PR runs", {
    workflow_text <- paste(
        readLines(here::here(".github", "workflows", "portable-tests.yml"), warn = FALSE),
        collapse = "\n"
    )

    expect_match(workflow_text, "push:\n    branches:\n      - master", fixed = TRUE)
    expect_match(workflow_text, "  pull_request:", fixed = TRUE)
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

test_that("portable CI installs rmda from its tracked upstream repository", {
    workflow_text <- paste(
        readLines(here::here(".github", "workflows", "portable-tests.yml"), warn = FALSE),
        collapse = "\n"
    )

    expect_match(workflow_text, "github::mdbrown/rmda", fixed = TRUE)
    expect_false(grepl("any::rmda", workflow_text, fixed = TRUE))
})

test_that("required portable CI uses the verified frozen R environment", {
    workflow_text <- paste(
        readLines(here::here(".github", "workflows", "portable-tests.yml"), warn = FALSE),
        collapse = "\n"
    )

    expect_match(workflow_text, 'r-version: "4.4.3"', fixed = TRUE)
    expect_match(workflow_text, "use-public-rspm: false", fixed = TRUE)
    expect_match(
        workflow_text,
        'cran: "https://packagemanager.posit.co/cran/__linux__/noble/2026-07-19"',
        fixed = TRUE
    )
    expect_false(grepl(
        "packagemanager\\.posit\\.co/cran/__linux__/noble/latest",
        workflow_text
    ))
})

test_that("lintr contract permits the repository's established mixed pipe syntax", {
    lintr_text <- paste(readLines(here::here(".lintr"), warn = FALSE), collapse = "\n")

    expect_match(lintr_text, "pipe_consistency_linter = NULL", fixed = TRUE)
})

test_that("documented test commands use the fail-sensitive runner", {
    readme_text <- paste(readLines(here::here("README.md"), warn = FALSE), collapse = "\n")

    expect_match(readme_text, "Rscript scripts/tools/run_testthat.R tests/testthat", fixed = TRUE)
    expect_match(readme_text, "Rscript scripts/tools/run_testthat.R tests/integration", fixed = TRUE)
})

test_that("standard testthat entrypoint explicitly stops on failure", {
    entrypoint_text <- paste(readLines(here::here("tests", "testthat.R"), warn = FALSE), collapse = "\n")

    expect_match(entrypoint_text, "stop_on_failure = TRUE", fixed = TRUE)
})
