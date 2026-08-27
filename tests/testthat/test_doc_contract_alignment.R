test_that("documentation keeps Objective 2d aligned with the inclusive SRD endpoint", {
    objectives_text <- paste(readLines(here::here("docs", "OBJECTIVES.md"), warn = FALSE), collapse = "\n")
    technical_text <- paste(readLines(here::here("docs", "TECHNICAL.md"), warn = FALSE), collapse = "\n")
    methods_text <- paste(readLines(here::here("docs", "STATISTICAL_METHODS.md"), warn = FALSE), collapse = "\n")

    expect_match(objectives_text, "compare recorded burden using `srd_burden_event` regardless of attributed cause", fixed = TRUE)
    expect_match(objectives_text, "Objective 2d retains all recorded SRD causes", fixed = TRUE)
    expect_match(technical_text, "all recorded SRD causes", fixed = TRUE)
    expect_match(methods_text, "Objective 2d includes all recorded SRD causes", fixed = TRUE)
})

test_that("current contracts do not claim post-baseline event-status survival outputs", {
    contract_paths <- c(
        here::here("README.md"),
        here::here("docs", "OBJECTIVES.md"),
        here::here("docs", "TECHNICAL.md"),
        here::here("docs", "STATISTICAL_METHODS.md"),
        here::here("openspec", "specs", "objective1-cohort-interpretation-guardrails", "spec.md")
    )
    contract_text <- paste(unlist(lapply(contract_paths, readLines, warn = FALSE)), collapse = "\n")

    expect_false(grepl("recurrence-stratified", contract_text, ignore.case = TRUE))
    expect_false(grepl("metastasis-stratified", contract_text, ignore.case = TRUE))
    expect_false(grepl("post-baseline output bundle", contract_text, ignore.case = TRUE))
})

test_that("README uses the canonical repository identity", {
    readme_text <- paste(readLines(here::here("README.md"), warn = FALSE), collapse = "\n")

    expect_match(readme_text, "github.com/nicholas-camarda/uveal-melanoma.git", fixed = TRUE)
    expect_match(readme_text, "cd uveal-melanoma", fixed = TRUE)
    expect_false(grepl("github.com/nicholas-camarda/uveal_melanoma", readme_text, fixed = TRUE))
    expect_false(grepl("cd uveal_melanoma", readme_text, fixed = TRUE))
})

test_that("current documentation does not revive retired GEP Other sidecars", {
    current_doc_paths <- c(
        here::here("docs", "TECHNICAL.md"),
        here::here("docs", "INTERPRETATION_GUIDE.md"),
        here::here("docs", "STATISTICAL_METHODS.md"),
        here::here("docs", "METHODS_SECTION_PAPER.md"),
        here::here("docs", "GEP_OBJECTIVE4_GOAL_IMPLEMENTATION_MAP.md")
    )
    current_doc_paths <- current_doc_paths[file.exists(current_doc_paths)]
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
    clickable_path_exception_docs <- c(
        here::here("docs", "superpowers", "plans", "2026-06-26-peer-review-statistical-revision.md")
    )
    repo_doc_paths_for_absolute_path_check <- setdiff(repo_doc_paths, clickable_path_exception_docs)

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
    maintainer_path_text <- repo_doc_text[names(repo_doc_text) %in% repo_doc_paths_for_absolute_path_check]
    maintainer_path_hits <- names(maintainer_path_text)[grepl("/Users/ncamarda/", maintainer_path_text, fixed = TRUE)]

    expect(
        length(stale_path_hits) == 0,
        paste("Files with stale path contract tokens:", paste(stale_path_hits, collapse = ", "))
    )
    expect(
        length(maintainer_path_hits) == 0,
        paste("Files leaking maintainer-specific absolute paths outside approved clickable-path exceptions:", paste(maintainer_path_hits, collapse = ", "))
    )
})

test_that("active Objective 4 docs and code-facing outputs do not revive split-role vocabulary", {
    split_contract_paths <- c(
        here::here("docs", "STATISTICAL_METHODS.md"),
        here::here("docs", "GEP_OBJECTIVE4_GOAL_IMPLEMENTATION_MAP.md"),
        here::here("scripts", "config", "objective4_policy.R"),
        here::here("scripts", "gep", "orchestration", "gep_exploratory_no_gep_report.R"),
        here::here("tests", "integration", "test_exploratory_no_gep_report.R"),
        here::here("tests", "testthat", "test_exploratory_no_gep_followup_context.R")
    )
    split_contract_paths <- split_contract_paths[file.exists(split_contract_paths)]

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

test_that("documentation and maintenance hints name normalized config modules", {
    repository_root <- here::here()
    tracked_documentation_paths <- system2(
        "git",
        c("-C", repository_root, "ls-files", "--", "README.md", "docs"),
        stdout = TRUE
    )
    expect_gt(length(tracked_documentation_paths), 0L)
    documentation_paths <- file.path(repository_root, tracked_documentation_paths)
    documentation_paths <- documentation_paths[file.exists(documentation_paths)]
    documentation_text <- paste(
        unlist(lapply(documentation_paths, readLines, warn = FALSE)),
        collapse = "\n"
    )
    figure_audit_text <- paste(
        readLines(here::here("docs", "FIGURE_COUNTS_AUDIT.md"), warn = FALSE),
        collapse = "\n"
    )
    study_doc_generator_text <- paste(
        readLines(here::here("scripts", "tools", "study_doc_generators.R"), warn = FALSE),
        collapse = "\n"
    )
    confounder_analysis_text <- paste(
        readLines(here::here("scripts", "tools", "confounder_analysis.R"), warn = FALSE),
        collapse = "\n"
    )
    model_utilities_text <- paste(
        readLines(here::here("scripts", "utils", "model_utilities.R"), warn = FALSE),
        collapse = "\n"
    )

    retired_objective4_policy_path <- "(^|[/`])gep_policy\\.R"
    expect_false(grepl(retired_objective4_policy_path, documentation_text, perl = TRUE))
    expect_false(grepl(retired_objective4_policy_path, study_doc_generator_text, perl = TRUE))
    expect_false(grepl(
        "INPUT_FILENAME` in `scripts/utils/config_constants.R",
        documentation_text,
        fixed = TRUE
    ))
    expect_false(grepl(
        "cutoff constants centralized in `scripts/utils/config_constants.R`",
        documentation_text,
        fixed = TRUE
    ))
    expect_false(grepl(
        "Centralized level labels in `config_constants.R`",
        documentation_text,
        fixed = TRUE
    ))
    expect_false(grepl("updating config_constants.R", confounder_analysis_text, fixed = TRUE))
    expect_false(grepl("confounders line in config_constants.R", confounder_analysis_text, fixed = TRUE))
    expect_false(grepl("from config_constants.R", model_utilities_text, fixed = TRUE))

    readme_text <- paste(readLines(here::here("README.md"), warn = FALSE), collapse = "\n")
    technical_text <- paste(readLines(here::here("docs", "TECHNICAL.md"), warn = FALSE), collapse = "\n")
    expect_match(
        readme_text,
        "`INPUT_FILENAME` in `scripts/config/data_processing_policy.R`",
        fixed = TRUE
    )
    expect_match(
        technical_text,
        "cutoff constants centralized in `scripts/config/data_processing_policy.R`",
        fixed = TRUE
    )
    expect_match(technical_text, "Centralized level labels in `scripts/config/labels_display.R`", fixed = TRUE)
    expect_match(confounder_analysis_text, "scripts/config/modeling_policy.R", fixed = TRUE)
    expect_match(model_utilities_text, "scripts/config/labels_display.R", fixed = TRUE)
    expect_match(
        figure_audit_text,
        "`scripts/config/data_processing_policy.R`",
        fixed = TRUE
    )
    expect_match(
        figure_audit_text,
        "`scripts/utils/config_constants.R` (public configuration loader)",
        fixed = TRUE
    )
    expect_match(
        study_doc_generator_text,
        "`scripts/config/data_processing_policy.R`",
        fixed = TRUE
    )
    expect_match(
        study_doc_generator_text,
        "`scripts/utils/config_constants.R` (public configuration loader)",
        fixed = TRUE
    )
})
