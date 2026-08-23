test_that("dependency diagram generator includes current workflow layers and loader inventory", {
    output_file <- tempfile("dependency-diagram-", fileext = ".md")
    withr::defer(unlink(output_file, force = TRUE), envir = parent.frame())

    result <- generate_dependency_diagram_doc(output_file = output_file)
    doc_text <- paste(readLines(output_file, warn = FALSE), collapse = "\n")

    expect_equal(result$status, "success")
    expect_match(doc_text, "workflow/publish_outputs.R", fixed = TRUE)
    expect_match(doc_text, "data_helper/gep_missing_data_analysis.R", fixed = TRUE)
    expect_match(doc_text, "## Loader Inventory", fixed = TRUE)
})

test_that("figure counts audit generator renders current-state runtime summary language", {
    summary_data <- list(
        exclusions = list(
            total_excluded = 4L,
            by_step = list(
                stage_iv_exclusion = 3L,
                manual_exclusion = 1L
            )
        ),
        cohorts = list(
            full_cohort = list(
                total = 260L,
                treatments = list(
                    PBT = list(n = 121L, local_recurrence = 16L, metastasis = 23L, alive = 73L, lost_to_followup = 15L, dead = 33L),
                    GKSRS = list(n = 139L, local_recurrence = 17L, metastasis = 21L, alive = 90L, lost_to_followup = 25L, dead = 24L)
                )
            ),
            restricted_cohort = list(
                total = 167L,
                treatments = list(
                    PBT = list(n = 103L, local_recurrence = 11L, metastasis = 19L, alive = 65L, lost_to_followup = 11L, dead = 27L),
                    GKSRS = list(n = 64L, local_recurrence = 8L, metastasis = 9L, alive = 40L, lost_to_followup = 12L, dead = 12L)
                )
            )
        )
    )

    doc_text <- paste(render_figure_counts_audit_markdown(summary_data = summary_data), collapse = "\n")

    expect_match(doc_text, "current-state cohort and figure-count audit", fixed = TRUE)
    expect_match(doc_text, "Total exclusions: **4**", fixed = TRUE)
    expect_match(doc_text, "Stage IV exclusions: **3**", fixed = TRUE)
    expect_match(doc_text, "Row-level exclusion details remain in the private runtime audit workbook", fixed = TRUE)
    expect_no_match(doc_text, "| ID |", fixed = TRUE)
    expect_no_match(doc_text, "legacy figure")
    expect_no_match(doc_text, "5 replacements")
})

test_that("tool refresh suite writes its complete audit summary", {
    publish_main <- main
    refresh_env <- new.env(parent = environment())
    sys.source(
        here::here("scripts", "tools", "run_tool_refreshes.R"),
        envir = refresh_env
    )
    expect_identical(main, publish_main)

    output_dir <- tempfile("tool-refresh-suite-")
    dir.create(output_dir, recursive = TRUE)
    withr::defer(unlink(output_dir, recursive = TRUE, force = TRUE), envir = parent.frame())
    original_processed_data_dir <- PROCESSED_DATA_DIR
    PROCESSED_DATA_DIR <<- output_dir
    withr::defer(
        PROCESSED_DATA_DIR <<- original_processed_data_dir,
        envir = parent.frame()
    )

    result <- suppressWarnings(refresh_env$run_tool_refresh_suite(output_dir = output_dir))
    summary_text <- paste(readLines(result$txt_path, warn = FALSE), collapse = "\n")

    expect_match(summary_text, "tool outputs:", fixed = TRUE)
    expect_match(summary_text, "- derived_variables_documentation:", fixed = TRUE)
    expect_match(summary_text, "- figure_counts_audit_doc:", fixed = TRUE)
})
