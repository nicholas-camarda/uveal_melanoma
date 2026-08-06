test_that("portable smoke path writes simple GEP outputs and dry-run publish on synthetic data", {
    cohort_root <- file.path(OUTPUT_DIR, "uveal_restricted")
    output_dirs <- list(
        obj4_mfs = file.path(cohort_root, "04_GEP_Validation", "a_metastasis_free_survival"),
        obj4_mss = file.path(cohort_root, "04_GEP_Validation", "b_melanoma_specific_survival"),
        obj4_ph_diagnostics = file.path(cohort_root, "04_GEP_Validation", "c_proportional_hazards_diagnostics")
    )

    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    withr::defer(unlink(cohort_root, recursive = TRUE), envir = parent.frame())

    results <- simple_gep_validation(
        data = create_synthetic_ci_dataset(),
        output_dirs = output_dirs,
        prefix = "smoke_",
        dataset_name = "uveal_melanoma_restricted_cohort"
    )

    unified_workbook <- file.path(
        cohort_root,
        "04_GEP_Validation",
        "unified_summary",
        "smoke_simple_gep_validation.xlsx"
    )

    expect_true(all(c("mfs_results", "mss_results", "overall_summary") %in% names(results)))
    expect_true(file.exists(unified_workbook))

    publish_result <- publish_outputs(
        cohorts = "uveal_melanoma_restricted_cohort",
        snapshot_id = "portable-smoke",
        include_merged_tables = FALSE,
        dry_run = TRUE
    )

    expect_true(publish_result$dry_run)
    expect_gt(publish_result$summary$would_copy, 0)
    expect_equal(
        publish_result$snapshot_dir,
        file.path(EXPORT_ANALYSIS_DIR, "portable-smoke")
    )
    expect_true(any(grepl("04_GEP_Validation", publish_result$manifest$source_path)))
})
