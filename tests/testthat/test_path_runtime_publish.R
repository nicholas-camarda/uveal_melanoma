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

    old_export_analysis <- EXPORT_ANALYSIS_DIR
    old_slug <- PROJECT_SLUG

    on.exit({
        assign("EXPORT_ANALYSIS_DIR", old_export_analysis, envir = .GlobalEnv)
        assign("PROJECT_SLUG", old_slug, envir = .GlobalEnv)
        unlink(tmp_root, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    assign("EXPORT_ANALYSIS_DIR", file.path(export_root, "Analysis"), envir = .GlobalEnv)
    assign("PROJECT_SLUG", "om_workspace", envir = .GlobalEnv)

    snapshot_dir <- get_export_snapshot_dir("2026-03-25")
    expect_equal(
        snapshot_dir,
        file.path(EXPORT_ANALYSIS_DIR, PROJECT_SLUG, "2026-03-25")
    )
})

test_that("is_publishable_artifact enforces extension and directory rules", {
    tmp_root <- tempfile("runtime-publishable-")
    dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)

    allowed_file <- file.path(tmp_root, "summary.xlsx")
    blocked_ext_file <- file.path(tmp_root, "model.rds")
    blocked_dir <- file.path(tmp_root, "logs")
    blocked_dir_file <- file.path(blocked_dir, "log.txt")

    dir.create(blocked_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines("ok", allowed_file)
    writeLines("model", blocked_ext_file)
    writeLines("log", blocked_dir_file)

    on.exit(unlink(tmp_root, recursive = TRUE, force = TRUE), add = TRUE)

    expect_true(is_publishable_artifact(allowed_file))
    expect_false(is_publishable_artifact(blocked_ext_file))
    expect_false(is_publishable_artifact(blocked_dir_file))
})

test_that("publish_outputs dry run reports publishable outputs and excludes runtime artifacts", {
    tmp_root <- tempfile("runtime-publish-dryrun-")
    runtime_root <- file.path(tmp_root, "runtime")
    output_root <- file.path(runtime_root, "Analysis")
    export_root <- file.path(tmp_root, "export")

    dir.create(file.path(output_root, "uveal_full"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(output_root, "uveal_full", "cache"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(output_root, "merged_tables"), recursive = TRUE, showWarnings = FALSE)

    writeLines("report", file.path(output_root, "uveal_full", "summary.xlsx"))
    writeLines("model", file.path(output_root, "uveal_full", "model.rds"))
    writeLines("cache", file.path(output_root, "uveal_full", "cache", "cache.csv"))
    writeLines("merged", file.path(output_root, "merged_tables", "merged_baseline.csv"))

    old_output <- OUTPUT_DIR
    old_merged <- MERGED_TABLES_DIR
    old_export_analysis <- EXPORT_ANALYSIS_DIR
    old_slug <- PROJECT_SLUG

    on.exit({
        assign("OUTPUT_DIR", old_output, envir = .GlobalEnv)
        assign("MERGED_TABLES_DIR", old_merged, envir = .GlobalEnv)
        assign("EXPORT_ANALYSIS_DIR", old_export_analysis, envir = .GlobalEnv)
        assign("PROJECT_SLUG", old_slug, envir = .GlobalEnv)
        unlink(tmp_root, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    assign("OUTPUT_DIR", output_root, envir = .GlobalEnv)
    assign("MERGED_TABLES_DIR", file.path(output_root, "merged_tables"), envir = .GlobalEnv)
    assign("EXPORT_ANALYSIS_DIR", file.path(export_root, "Analysis"), envir = .GlobalEnv)
    assign("PROJECT_SLUG", "om_workspace", envir = .GlobalEnv)

    result <- publish_outputs(snapshot_id = "2026-03-25-unit", dry_run = TRUE)

    expect_true(result$dry_run)
    expect_equal(result$summary$would_copy, 2)
    expect_equal(result$summary$copied, 0)
    expect_true(any(result$manifest$status == "skipped_not_publishable"))
    expect_false(dir.exists(result$snapshot_dir))
})

test_that("publish_outputs creates a new snapshot and rejects existing snapshot overwrite", {
    tmp_root <- tempfile("runtime-publish-copy-")
    runtime_root <- file.path(tmp_root, "runtime")
    output_root <- file.path(runtime_root, "Analysis")
    export_root <- file.path(tmp_root, "export")

    dir.create(file.path(output_root, "uveal_full"), recursive = TRUE, showWarnings = FALSE)
    writeLines("report", file.path(output_root, "uveal_full", "summary.xlsx"))

    old_output <- OUTPUT_DIR
    old_merged <- MERGED_TABLES_DIR
    old_export_analysis <- EXPORT_ANALYSIS_DIR
    old_slug <- PROJECT_SLUG

    on.exit({
        assign("OUTPUT_DIR", old_output, envir = .GlobalEnv)
        assign("MERGED_TABLES_DIR", old_merged, envir = .GlobalEnv)
        assign("EXPORT_ANALYSIS_DIR", old_export_analysis, envir = .GlobalEnv)
        assign("PROJECT_SLUG", old_slug, envir = .GlobalEnv)
        unlink(tmp_root, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    assign("OUTPUT_DIR", output_root, envir = .GlobalEnv)
    assign("MERGED_TABLES_DIR", file.path(output_root, "merged_tables"), envir = .GlobalEnv)
    assign("EXPORT_ANALYSIS_DIR", file.path(export_root, "Analysis"), envir = .GlobalEnv)
    assign("PROJECT_SLUG", "om_workspace", envir = .GlobalEnv)

    first_publish <- publish_outputs(snapshot_id = "2026-03-25-publish", dry_run = FALSE)
    expect_true(dir.exists(first_publish$snapshot_dir))
    expect_true(file.exists(file.path(first_publish$snapshot_dir, "uveal_full", "summary.xlsx")))
    expect_true(file.exists(file.path(first_publish$snapshot_dir, "publish_manifest.csv")))

    expect_error(
        publish_outputs(snapshot_id = "2026-03-25-publish", dry_run = FALSE),
        regexp = "Snapshot target already exists"
    )
})
