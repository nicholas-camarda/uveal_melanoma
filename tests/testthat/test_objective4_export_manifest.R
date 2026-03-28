source(here::here("scripts", "tools", "export_gep_objective4_to_downloads.R"))

test_that("Objective 4 export manifest treats markdown summaries as text", {
    expect_identical(get_objective4_artifact_category("summary.md"), "text")
    expect_identical(get_objective4_artifact_category("summary.txt"), "text")
    expect_identical(get_objective4_artifact_category("figure.png"), "png")

    objective4_root <- tempfile("objective4-root-")
    dir.create(objective4_root, recursive = TRUE, showWarnings = FALSE)
    withr::defer(unlink(objective4_root, recursive = TRUE), envir = parent.frame())

    manifest <- build_objective4_export_manifest(objective4_root, "test_", include_prame = TRUE)
    markdown_labels <- c(
        "simple_report",
        "sensitivity_report",
        "mfs_narrative",
        "mss_narrative",
        "mfs_extrapolation_assumption",
        "mss_extrapolation_assumption"
    )
    expect_true(all(grepl("\\.md$", unname(manifest[markdown_labels]))))

    no_gep_manifest <- build_exploratory_no_gep_export_manifest(objective4_root, "test_")
    expect_true(grepl("\\.md$", unname(no_gep_manifest[["no_gep_summary"]])))

    source_file <- file.path(tempdir(), "objective4_markdown_summary.md")
    writeLines(c("# Test", "", "This is a markdown summary."), source_file)

    export_root <- tempfile("objective4-export-")
    dir.create(export_root, recursive = TRUE, showWarnings = FALSE)
    withr::defer(unlink(export_root, recursive = TRUE), envir = parent.frame())

    copied <- copy_objective4_artifacts(
        source_paths = c(markdown_report = source_file),
        export_root = export_root,
        cohort_label = "full"
    )

    expect_equal(copied$status[[1]], "copied")
    expect_equal(copied$category[[1]], "text")
    expect_true(file.exists(file.path(export_root, "full", "text", basename(source_file))))
})
