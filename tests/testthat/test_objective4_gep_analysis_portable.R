build_objective4_output_dirs <- function(test_output_dir) {
    list(
        obj4_mfs = file.path(test_output_dir, "04_GEP_Validation", "a_mfs_validation"),
        obj4_mss = file.path(test_output_dir, "04_GEP_Validation", "b_mss_validation"),
        obj4_ph_diagnostics = file.path(test_output_dir, "04_GEP_Validation", "c_proportional_hazards_diagnostics")
    )
}

test_that("Objective 4 simple validation works on synthetic data", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_simple_validation")
    output_dirs <- build_objective4_output_dirs(test_output_dir)

    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

    results <- simple_gep_validation(create_test_dataset(), output_dirs, "test_")

    expect_true(all(c("mfs_results", "mss_results", "overall_summary") %in% names(results)))
    expect_s3_class(results$mfs_results, "data.frame")
    expect_s3_class(results$mss_results, "data.frame")
    expect_true(all(results$overall_summary$total_patients > 0))
})

test_that("Objective 4 writes expected simple validation workbook", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_workbook_output")
    output_dirs <- build_objective4_output_dirs(test_output_dir)

    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

    simple_gep_validation(create_test_dataset(), output_dirs, "test_")

    workbook_path <- file.path(dirname(output_dirs$obj4_mfs), "unified_summary", "test_simple_gep_validation.xlsx")
    expect_true(file.exists(workbook_path))
})
