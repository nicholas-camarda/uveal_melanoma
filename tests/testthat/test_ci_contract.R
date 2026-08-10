test_that("portable CI exposes one stable fast required check, post-merge validation, and a manual full suite", {
    workflow_path <- here::here(".github", "workflows", "portable-tests.yml")
    expect_true(file.exists(workflow_path))

    workflow_text <- paste(readLines(workflow_path, warn = FALSE), collapse = "\n")
    expect_match(workflow_text, "pull_request:", fixed = TRUE)
    expect_match(workflow_text, "push:", fixed = TRUE)
    expect_match(workflow_text, "- master", fixed = TRUE)
    expect_match(workflow_text, "workflow_dispatch:", fixed = TRUE)
    expect_match(workflow_text, "suite:", fixed = TRUE)
    expect_match(workflow_text, "- fast", fixed = TRUE)
    expect_match(workflow_text, "- full", fixed = TRUE)
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
    expect_match(workflow_text, "RENV_CONFIG_REPOS_OVERRIDE", fixed = TRUE)
    expect_false(grepl("actions/checkout@v4", workflow_text, fixed = TRUE))
    expect_false(grepl("setup-r-dependencies", workflow_text, fixed = TRUE))
    expect_false(grepl("extra-packages", workflow_text, fixed = TRUE))
    expect_false(grepl("npm", workflow_text, ignore.case = TRUE))
})

test_that("portable CI runs fail-sensitive fast and synthetic commands", {
    workflow_text <- paste(
        readLines(here::here(".github", "workflows", "portable-tests.yml"), warn = FALSE),
        collapse = "\n"
    )

    expect_match(workflow_text, "scripts/tools/run_testthat.R tests/testthat", fixed = TRUE)
    expect_match(workflow_text, "--filter", fixed = TRUE)
    expect_match(workflow_text, "scripts/tools/run_testthat.R tests/integration --filter", fixed = TRUE)
    expect_match(workflow_text, "portable_smoke", fixed = TRUE)
    expect_match(workflow_text, "lintr::lint_package()", fixed = TRUE)
    expect_match(workflow_text, "github.event_name == 'push'", fixed = TRUE)
    expect_match(workflow_text, "if: ${{ github.event_name == 'pull_request'", fixed = TRUE)
    expect_match(workflow_text, "inputs.suite == 'full'", fixed = TRUE)
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
    expect_match(runner_text, "filter = filter", fixed = TRUE)
    expect_match(runner_text, "stop_on_failure = TRUE", fixed = TRUE)
    expect_false(grepl("stop_on_warning = TRUE", runner_text, fixed = TRUE))
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

    expect_match(lintr_text, "pipe_consistency_linter = NULL", fixed = TRUE)
})

test_that("documented test commands use the fail-sensitive runner", {
    readme_text <- paste(readLines(here::here("README.md"), warn = FALSE), collapse = "\n")

    expect_match(readme_text, "Rscript scripts/bootstrap_packages.R", fixed = TRUE)
    expect_match(readme_text, "Rscript scripts/tools/run_testthat.R tests/testthat", fixed = TRUE)
    expect_match(readme_text, "Rscript scripts/tools/run_testthat.R tests/integration", fixed = TRUE)
})

test_that("standard testthat entrypoint explicitly stops on failure", {
    entrypoint_text <- paste(readLines(here::here("tests", "testthat.R"), warn = FALSE), collapse = "\n")

    expect_match(entrypoint_text, "stop_on_failure = TRUE", fixed = TRUE)
})
