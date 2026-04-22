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
