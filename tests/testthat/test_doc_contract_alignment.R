test_that("documentation keeps Objective 2d aligned with the inclusive SRD endpoint", {
    objectives_text <- paste(readLines(here::here("docs", "OBJECTIVES.md"), warn = FALSE), collapse = "\n")
    technical_text <- paste(readLines(here::here("docs", "TECHNICAL.md"), warn = FALSE), collapse = "\n")
    methods_text <- paste(readLines(here::here("docs", "STATISTICAL_METHODS.md"), warn = FALSE), collapse = "\n")

    expect_match(objectives_text, "compare recorded burden using `srd_burden_event` regardless of attributed cause", fixed = TRUE)
    expect_match(objectives_text, "Objective 2d retains all recorded SRD causes", fixed = TRUE)
    expect_match(technical_text, "all recorded SRD causes", fixed = TRUE)
    expect_match(methods_text, "Objective 2d includes all recorded SRD causes", fixed = TRUE)
})

test_that("documentation labels recurrence/metastasis-stratified survival outputs as legacy exploratory one-offs", {
    objectives_text <- paste(readLines(here::here("docs", "OBJECTIVES.md"), warn = FALSE), collapse = "\n")
    technical_text <- paste(readLines(here::here("docs", "TECHNICAL.md"), warn = FALSE), collapse = "\n")
    methods_text <- paste(readLines(here::here("docs", "STATISTICAL_METHODS.md"), warn = FALSE), collapse = "\n")

    expect_match(objectives_text, "one-off exploratory post-baseline summaries", fixed = TRUE)
    expect_match(technical_text, "retained historical one-off post-baseline summaries", fixed = TRUE)
    expect_match(methods_text, "outside the formal objective contract", fixed = TRUE)
})

test_that("current documentation does not revive retired GEP Other sidecars", {
    current_doc_paths <- c(
        here::here("docs", "TECHNICAL.md"),
        here::here("docs", "INTERPRETATION_GUIDE.md"),
        here::here("docs", "STATISTICAL_METHODS.md"),
        here::here("docs", "METHODS_SECTION_PAPER.md"),
        here::here("docs", "GEP_OBJECTIVE4_GOAL_IMPLEMENTATION_MAP.md")
    )
    current_doc_text <- paste(
        unlist(lapply(current_doc_paths, readLines, warn = FALSE)),
        collapse = "\n"
    )

    expect_false(grepl("other_map\\.rds", current_doc_text))
    expect_false(grepl("Unknown`, `Other`|`Other`, and `No`|collapsed `Other`", current_doc_text))
})

test_that("repo docs and contract-facing utilities avoid stale path contract tokens", {
    repo_doc_paths <- c(
        here::here("README.md"),
        here::here("AGENTS.md"),
        here::here("CLAUDE.md"),
        list.files(
            path = here::here("docs"),
            pattern = "\\.(md|qmd|Rmd)$",
            recursive = TRUE,
            full.names = TRUE
        ),
        here::here("scripts", "utils", "output_utilities.R"),
        here::here("scripts", "tools", "derived_variables_documentation.R")
    )
    repo_doc_paths <- unique(repo_doc_paths[file.exists(repo_doc_paths)])

    repo_doc_text <- vapply(
        repo_doc_paths,
        function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
        character(1)
    )

    # The managed Research Partner block intentionally records local path truth.
    repo_doc_text[names(repo_doc_text) == here::here("AGENTS.md")] <- gsub(
        "(?s)<!-- BEGIN RESEARCH PARTNER -->.*<!-- END RESEARCH PARTNER -->",
        "",
        repo_doc_text[names(repo_doc_text) == here::here("AGENTS.md")],
        perl = TRUE
    )

    stale_path_hits <- names(repo_doc_text)[grepl(
        "final_data/|OCULAR_EXPORT_ROOT|residency/.+research projects",
        repo_doc_text
    )]
    maintainer_path_hits <- names(repo_doc_text)[grepl("/Users/ncamarda/", repo_doc_text, fixed = TRUE)]

    expect(
        length(stale_path_hits) == 0,
        paste("Files with stale path contract tokens:", paste(stale_path_hits, collapse = ", "))
    )
    expect(
        length(maintainer_path_hits) == 0,
        paste("Files leaking maintainer-specific absolute paths:", paste(maintainer_path_hits, collapse = ", "))
    )
})

test_that("active Objective 4 docs and code-facing outputs do not revive split-role vocabulary", {
    split_contract_paths <- c(
        here::here("docs", "STATISTICAL_METHODS.md"),
        here::here("docs", "GEP_OBJECTIVE4_GOAL_IMPLEMENTATION_MAP.md"),
        here::here("scripts", "config", "gep_policy.R"),
        here::here("scripts", "gep", "orchestration", "gep_exploratory_no_gep_report.R"),
        here::here("tests", "integration", "test_exploratory_no_gep_report.R"),
        here::here("tests", "testthat", "test_exploratory_no_gep_followup_context.R")
    )

    split_contract_text <- vapply(
        split_contract_paths,
        function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
        character(1)
    )

    revived_split_hits <- names(split_contract_text)[grepl(
        "\\bTraining_Set\\b|\\btraining_set\\b|\\bdefinitive_training\\b|\\bno_gep_prediction\\b|GEP_RECOMMENDED_TESTING_SAMPLE|definitive-GEP training set|no-GEP prediction set|testing set",
        split_contract_text
    )]

    expect(
        length(revived_split_hits) == 0,
        paste("Files reviving split-role vocabulary:", paste(revived_split_hits, collapse = ", "))
    )
})
