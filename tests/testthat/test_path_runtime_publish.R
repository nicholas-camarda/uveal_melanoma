test_that("project slug matches project root basename", {
    expect_equal(PROJECT_SLUG, basename(PROJECT_ROOT))
})

test_that("initialize_runtime_dirs creates configured runtime directories", {
    tmp_root <- tempfile("runtime-init-")
    runtime_root <- file.path(tmp_root, "runtime")

    old_runtime_root <- RUNTIME_ROOT
    old_processed <- PROCESSED_DATA_DIR
    old_output <- OUTPUT_DIR
    old_logs <- LOGS_DIR
    old_tools <- TOOLS_OUTPUT_DIR
    old_test <- TEST_OUTPUT_DIR
    old_merged <- MERGED_TABLES_DIR

    on.exit({
        assign("RUNTIME_ROOT", old_runtime_root, envir = .GlobalEnv)
        assign("PROCESSED_DATA_DIR", old_processed, envir = .GlobalEnv)
        assign("OUTPUT_DIR", old_output, envir = .GlobalEnv)
        assign("LOGS_DIR", old_logs, envir = .GlobalEnv)
        assign("TOOLS_OUTPUT_DIR", old_tools, envir = .GlobalEnv)
        assign("TEST_OUTPUT_DIR", old_test, envir = .GlobalEnv)
        assign("MERGED_TABLES_DIR", old_merged, envir = .GlobalEnv)
        unlink(tmp_root, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    assign("RUNTIME_ROOT", runtime_root, envir = .GlobalEnv)
    assign("PROCESSED_DATA_DIR", file.path(runtime_root, "Analytic Dataset"), envir = .GlobalEnv)
    assign("OUTPUT_DIR", file.path(runtime_root, "Analysis"), envir = .GlobalEnv)
    assign("LOGS_DIR", file.path(runtime_root, "logs"), envir = .GlobalEnv)
    assign("TOOLS_OUTPUT_DIR", file.path(runtime_root, "tools_output"), envir = .GlobalEnv)
    assign("TEST_OUTPUT_DIR", file.path(runtime_root, "test_output"), envir = .GlobalEnv)
    assign("MERGED_TABLES_DIR", file.path(runtime_root, "Analysis", "merged_tables"), envir = .GlobalEnv)

    initialize_runtime_dirs()

    expect_true(dir.exists(RUNTIME_ROOT))
    expect_true(dir.exists(PROCESSED_DATA_DIR))
    expect_true(dir.exists(OUTPUT_DIR))
    expect_true(dir.exists(LOGS_DIR))
    expect_true(dir.exists(TOOLS_OUTPUT_DIR))
    expect_true(dir.exists(TEST_OUTPUT_DIR))
    expect_true(dir.exists(MERGED_TABLES_DIR))
})

test_that("assert_required_input_paths fails fast for missing raw inputs", {
    tmp_root <- tempfile("runtime-raw-check-")
    dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)

    old_raw <- RAW_DATA_DIR
    old_input <- INPUT_FILENAME

    on.exit({
        assign("RAW_DATA_DIR", old_raw, envir = .GlobalEnv)
        assign("INPUT_FILENAME", old_input, envir = .GlobalEnv)
        unlink(tmp_root, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    assign("RAW_DATA_DIR", file.path(tmp_root, "missing_raw"), envir = .GlobalEnv)
    assign("INPUT_FILENAME", "missing_input.xlsx", envir = .GlobalEnv)

    expect_error(
        assert_required_input_paths(),
        regexp = "Required raw input path checks failed"
    )
})

test_that("get_export_snapshot_dir constructs dated snapshot path", {
    tmp_root <- tempfile("runtime-export-")
    export_root <- file.path(tmp_root, "export")
    export_analysis_root <- file.path(export_root, "Analysis")

    old_export_root <- EXPORT_ROOT
    old_export_analysis <- EXPORT_ANALYSIS_DIR

    on.exit({
        assign("EXPORT_ROOT", old_export_root, envir = .GlobalEnv)
        assign("EXPORT_ANALYSIS_DIR", old_export_analysis, envir = .GlobalEnv)
        unlink(tmp_root, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    assign("EXPORT_ROOT", export_root, envir = .GlobalEnv)
    assign("EXPORT_ANALYSIS_DIR", export_analysis_root, envir = .GlobalEnv)

    snapshot_dir <- get_export_snapshot_dir("2026-03-25")
    expect_equal(
        snapshot_dir,
        file.path(EXPORT_ANALYSIS_DIR, "2026-03-25")
    )
})

test_that("resolve_config_path rejects relative runtime or export overrides", {
    expect_error(
        resolve_config_path("relative/runtime", "/tmp/runtime-default"),
        regexp = "must be absolute"
    )
})

test_that("artifact registry allowlists only explicit publishable outputs", {
    expect_true(is_publishable_relative_artifact("01_Efficacy/summary.xlsx", "cohort"))
    expect_true(is_publishable_relative_artifact("merged_tables/final_table.csv", "merged_tables"))
    expect_false(is_publishable_relative_artifact("01_Efficacy/model.rds", "cohort"))
    expect_false(is_publishable_relative_artifact("01_Efficacy/summary_diagnostics.xlsx", "cohort"))
    expect_false(is_publishable_relative_artifact("cache/intermediate.csv", "cohort"))
})

test_that("publish_outputs dry run reports publishable outputs and excludes runtime artifacts", {
    tmp_root <- tempfile("runtime-publish-dryrun-")
    runtime_root <- file.path(tmp_root, "runtime")
    output_root <- file.path(runtime_root, "Analysis")
    export_root <- file.path(tmp_root, "export")
    export_analysis_root <- file.path(export_root, "Analysis")

    dir.create(file.path(output_root, "uveal_full", "01_Efficacy"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(output_root, "uveal_full", "cache"), recursive = TRUE, showWarnings = FALSE)

    writeLines("report", file.path(output_root, "uveal_full", "01_Efficacy", "summary.xlsx"))
    writeLines("model", file.path(output_root, "uveal_full", "01_Efficacy", "model.rds"))
    writeLines("diagnostics", file.path(output_root, "uveal_full", "01_Efficacy", "summary_diagnostics.xlsx"))
    writeLines("cache", file.path(output_root, "uveal_full", "cache", "cache.csv"))

    old_output <- OUTPUT_DIR
    old_merged <- MERGED_TABLES_DIR
    old_export_root <- EXPORT_ROOT
    old_export_analysis <- EXPORT_ANALYSIS_DIR

    on.exit({
        assign("OUTPUT_DIR", old_output, envir = .GlobalEnv)
        assign("MERGED_TABLES_DIR", old_merged, envir = .GlobalEnv)
        assign("EXPORT_ROOT", old_export_root, envir = .GlobalEnv)
        assign("EXPORT_ANALYSIS_DIR", old_export_analysis, envir = .GlobalEnv)
        unlink(tmp_root, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    assign("OUTPUT_DIR", output_root, envir = .GlobalEnv)
    assign("MERGED_TABLES_DIR", file.path(output_root, "merged_tables"), envir = .GlobalEnv)
    assign("EXPORT_ROOT", export_root, envir = .GlobalEnv)
    assign("EXPORT_ANALYSIS_DIR", export_analysis_root, envir = .GlobalEnv)

    result <- publish_outputs(snapshot_id = "2026-03-25-unit", dry_run = TRUE)

    expect_true(result$dry_run)
    expect_equal(result$summary$would_copy, 1)
    expect_equal(result$summary$copied, 0)
    expect_true(any(result$manifest$status == "skipped_not_publishable"))
    expect_true(any(result$manifest$status == "optional_root_absent"))
    expect_equal(result$summary$missing, 0)
    expect_false(dir.exists(result$snapshot_dir))
})

test_that("publish_outputs creates a new snapshot and rejects existing snapshot overwrite", {
    tmp_root <- tempfile("runtime-publish-copy-")
    runtime_root <- file.path(tmp_root, "runtime")
    output_root <- file.path(runtime_root, "Analysis")
    export_root <- file.path(tmp_root, "export")
    export_analysis_root <- file.path(export_root, "Analysis")

    dir.create(file.path(output_root, "uveal_full", "01_Efficacy"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(output_root, "merged_tables"), recursive = TRUE, showWarnings = FALSE)
    writeLines("report", file.path(output_root, "uveal_full", "01_Efficacy", "summary.xlsx"))
    writeLines("merged", file.path(output_root, "merged_tables", "merged_baseline.csv"))

    old_output <- OUTPUT_DIR
    old_merged <- MERGED_TABLES_DIR
    old_export_root <- EXPORT_ROOT
    old_export_analysis <- EXPORT_ANALYSIS_DIR

    on.exit({
        assign("OUTPUT_DIR", old_output, envir = .GlobalEnv)
        assign("MERGED_TABLES_DIR", old_merged, envir = .GlobalEnv)
        assign("EXPORT_ROOT", old_export_root, envir = .GlobalEnv)
        assign("EXPORT_ANALYSIS_DIR", old_export_analysis, envir = .GlobalEnv)
        unlink(tmp_root, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    assign("OUTPUT_DIR", output_root, envir = .GlobalEnv)
    assign("MERGED_TABLES_DIR", file.path(output_root, "merged_tables"), envir = .GlobalEnv)
    assign("EXPORT_ROOT", export_root, envir = .GlobalEnv)
    assign("EXPORT_ANALYSIS_DIR", export_analysis_root, envir = .GlobalEnv)

    first_publish <- publish_outputs(snapshot_id = "2026-03-25-publish", dry_run = FALSE)
    expect_true(dir.exists(first_publish$snapshot_dir))
    expect_true(file.exists(file.path(first_publish$snapshot_dir, "uveal_full", "01_Efficacy", "summary.xlsx")))
    expect_true(file.exists(file.path(first_publish$snapshot_dir, "merged_tables", "merged_baseline.csv")))
    expect_true(file.exists(file.path(first_publish$snapshot_dir, "publish_manifest.csv")))

    expect_error(
        publish_outputs(snapshot_id = "2026-03-25-publish", dry_run = FALSE),
        regexp = "Snapshot target already exists"
    )
})
