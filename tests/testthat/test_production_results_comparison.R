production_contract_path <- here::here(
    "docs",
    "maintenance",
    "production_results_comparison_contract.yaml"
)
production_comparator_path <- here::here(
    "scripts",
    "tools",
    "compare_important_results.R"
)

test_that("production contract reuses the publish registry without fixed cohort counts", {
    expect_true(file.exists(production_contract_path))
    contract <- yaml::read_yaml(production_contract_path)

    expect_identical(contract$version, 1L)
    expect_identical(contract$contract_kind, "production")
    expect_identical(contract$runtime_root_subdir, "uveal_full")
    expect_identical(contract$publish_root_kind, "cohort")
    expect_true(length(contract$comparisons) > 0L)

    expectations <- vapply(contract$comparisons, `[[`, character(1), "expectation")
    paths <- vapply(contract$comparisons, `[[`, character(1), "path")
    expect_true(all(expectations %in% c("must_equal", "must_change", "may_change")))
    expect_true(all(startsWith(paths, "uveal_full/")))
    expect_false(any(grepl("(^|/)(n|count|rows|events?)($|[_-])", paths, ignore.case = TRUE)))
    expect_true(all(vapply(
        sub("^uveal_full/", "", paths),
        is_publishable_relative_artifact,
        logical(1),
        root_kind = "cohort"
    )))
})

test_that("comparator applies expected-change semantics without changing synthetic defaults", {
    test_root <- withr::local_tempdir("production-results-comparison-")
    base_runtime <- file.path(test_root, "base")
    candidate_runtime <- file.path(test_root, "candidate")
    dir.create(file.path(base_runtime, "uveal_full", "results"), recursive = TRUE)
    dir.create(file.path(candidate_runtime, "uveal_full", "results"), recursive = TRUE)

    write_artifact <- function(root, relative_path, value) {
        path <- file.path(root, "uveal_full", relative_path)
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE)
    }

    write_artifact(base_runtime, "results/equal.json", list(value = 1))
    write_artifact(candidate_runtime, "results/equal.json", list(value = 1))
    write_artifact(base_runtime, "results/changed.json", list(value = 1))
    write_artifact(candidate_runtime, "results/changed.json", list(value = 2))
    write_artifact(base_runtime, "results/permitted.json", list(value = 1))
    write_artifact(candidate_runtime, "results/permitted.json", list(value = 2))
    writeLines("unlisted", file.path(candidate_runtime, "uveal_full", "results", "auxiliary.txt"))

    contract_file <- file.path(test_root, "contract.yaml")
    yaml::write_yaml(
        list(
            version = 1L,
            contract_kind = "production",
            runtime_root_subdir = "uveal_full",
            numeric_tolerance = list(absolute = 1e-12, relative = 1e-10),
            comparisons = list(
                list(
                    id = "equal",
                    objective = "objective1",
                    domain = "stable",
                    expectation = "must_equal",
                    type = "json",
                    path = "uveal_full/results/equal.json"
                ),
                list(
                    id = "changed",
                    objective = "objective4",
                    domain = "remediation",
                    expectation = "must_change",
                    type = "json",
                    path = "uveal_full/results/changed.json"
                ),
                list(
                    id = "permitted",
                    objective = "objective0",
                    domain = "summary",
                    expectation = "may_change",
                    type = "json",
                    path = "uveal_full/results/permitted.json"
                )
            )
        ),
        contract_file
    )
    report_file <- file.path(test_root, "report.json")
    exit_code <- system2(
        "Rscript",
        c(
            production_comparator_path,
            "--base-runtime", base_runtime,
            "--candidate-runtime", candidate_runtime,
            "--contract", contract_file,
            "--report", report_file
        )
    )

    expect_identical(exit_code, 0L)
    report <- jsonlite::read_json(report_file, simplifyVector = FALSE)
    expect_identical(report$status, "pass")
    expect_identical(report$contract_version, 1L)
    expect_length(report$comparisons, 3L)
    by_id <- setNames(report$comparisons, vapply(report$comparisons, `[[`, character(1), "id"))
    expect_identical(by_id$equal$status, "pass")
    expect_identical(by_id$changed$reason, "expected artifact difference observed")
    expect_identical(by_id$permitted$reason, "artifact difference permitted")
    expect_identical(by_id$changed$objective, "objective4")
    expect_false(grepl("value", paste(readLines(report_file), collapse = "\n"), fixed = TRUE))

    write_artifact(candidate_runtime, "results/changed.json", list(value = 1))
    expect_true(system2(
        "Rscript",
        c(
            production_comparator_path,
            "--base-runtime", base_runtime,
            "--candidate-runtime", candidate_runtime,
            "--contract", contract_file,
            "--report", report_file
        ),
        stdout = FALSE,
        stderr = FALSE
    ) != 0L)

    write_artifact(base_runtime, "results/broken.json", list(value = 1))
    writeLines("{not-json", file.path(candidate_runtime, "uveal_full", "results", "broken.json"))
    malformed_contract <- file.path(test_root, "malformed.yaml")
    yaml::write_yaml(
        list(
            version = 1L,
            numeric_tolerance = list(absolute = 1e-12, relative = 1e-10),
            comparisons = list(
                list(
                    id = "malformed",
                    expectation = "must_change",
                    type = "json",
                    path = "uveal_full/results/broken.json"
                )
            )
        ),
        malformed_contract
    )
    expect_true(system2(
        "Rscript",
        c(
            production_comparator_path,
            "--base-runtime", base_runtime,
            "--candidate-runtime", candidate_runtime,
            "--contract", malformed_contract,
            "--report", report_file
        ),
        stdout = FALSE,
        stderr = FALSE
    ) != 0L)

    missing_contract <- file.path(test_root, "missing.yaml")
    yaml::write_yaml(
        list(
            version = 1L,
            numeric_tolerance = list(absolute = 1e-12, relative = 1e-10),
            comparisons = list(
                list(
                    id = "missing-permitted",
                    expectation = "may_change",
                    type = "json",
                    path = "uveal_full/results/not-present.json"
                )
            )
        ),
        missing_contract
    )
    expect_true(system2(
        "Rscript",
        c(
            production_comparator_path,
            "--base-runtime", base_runtime,
            "--candidate-runtime", candidate_runtime,
            "--contract", missing_contract,
            "--report", report_file
        ),
        stdout = FALSE,
        stderr = FALSE
    ) != 0L)
})
