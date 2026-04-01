test_that("documentation keeps Objective 2d aligned with the inclusive published SRD endpoint", {
    objectives_text <- paste(readLines(here::here("docs", "OBJECTIVES.md"), warn = FALSE), collapse = "\n")
    technical_text <- paste(readLines(here::here("docs", "TECHNICAL.md"), warn = FALSE), collapse = "\n")
    methods_text <- paste(readLines(here::here("docs", "STATISTICAL_METHODS.md"), warn = FALSE), collapse = "\n")

    expect_match(objectives_text, "compare rates of `srd` regardless of attributed cause", fixed = TRUE)
    expect_match(objectives_text, "published implementation is broader and intentionally retains all recorded SRD causes", fixed = TRUE)
    expect_match(technical_text, "all recorded SRD causes in the published implementation", fixed = TRUE)
    expect_match(methods_text, "current published implementation intentionally includes all recorded SRD causes", fixed = TRUE)
})

test_that("documentation labels recurrence/metastasis-stratified survival outputs as legacy exploratory one-offs", {
    objectives_text <- paste(readLines(here::here("docs", "OBJECTIVES.md"), warn = FALSE), collapse = "\n")
    technical_text <- paste(readLines(here::here("docs", "TECHNICAL.md"), warn = FALSE), collapse = "\n")
    methods_text <- paste(readLines(here::here("docs", "STATISTICAL_METHODS.md"), warn = FALSE), collapse = "\n")

    expect_match(objectives_text, "one-off exploratory post-baseline summaries", fixed = TRUE)
    expect_match(technical_text, "retained historical one-off post-baseline summaries", fixed = TRUE)
    expect_match(methods_text, "outside the formal objective contract", fixed = TRUE)
})
