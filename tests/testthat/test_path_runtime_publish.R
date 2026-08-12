test_that("project paths use distinct analysis and repository slugs", {
    expect_equal(PROJECT_SLUG, "uveal_melanoma")
    expect_equal(REPOSITORY_SLUG, "uveal-melanoma")
})

test_that("project paths follow the canonical workspace and Project Vault contract", {
    expect_equal(
        normalizePath(PROJECT_ROOT, winslash = "/", mustWork = FALSE),
        normalizePath(here::here(), winslash = "/", mustWork = FALSE)
    )
    expect_equal(
        normalizePath(DEFAULT_RUNTIME_ROOT, winslash = "/", mustWork = FALSE),
        file.path(path.expand("~/Workspaces"), "uveal-melanoma", "runtime")
    )
    expect_equal(
        normalizePath(DEFAULT_RAW_DATA_DIR, winslash = "/", mustWork = FALSE),
        file.path(
            path.expand("~/Library/CloudStorage/OneDrive-Personal"),
            "Project Vault",
            "Research",
            "uveal-melanoma",
            "Original Files"
        )
    )
    expect_equal(
        normalizePath(DEFAULT_PUBLISH_ROOT, winslash = "/", mustWork = FALSE),
        file.path(
            path.expand("~/Library/CloudStorage/OneDrive-Personal"),
            "Project Vault",
            "Research",
            "uveal-melanoma",
            "outputs"
        )
    )
})

test_that("standalone tools use the canonical workspace runtime", {
    configured_runtime_root <- RUNTIME_ROOT
    withr::local_envvar(c(
        OUTPUT_DIR = NA,
        OCULAR_RUNTIME_ROOT = NA,
        OCULAR_RUNTIME_PARENT_DIR = NA
    ))

    browse_env <- new.env(parent = baseenv())
    sys.source(
        here::here("scripts", "tools", "browse_diagnostics.R"),
        envir = browse_env
    )
    expect_equal(
        browse_env$get_default_output_dir(),
        file.path(path.expand("~/Workspaces"), "uveal-melanoma", "runtime", "Analysis")
    )
    expect_identical(RUNTIME_ROOT, configured_runtime_root)

    export_env <- new.env(parent = baseenv())
    sys.source(
        here::here("scripts", "tools", "export_gep_objective4_to_downloads.R"),
        envir = export_env
    )
    expect_equal(
        export_env$resolve_runtime_analysis_root("uveal_melanoma"),
        file.path(path.expand("~/Workspaces"), "uveal-melanoma", "runtime", "Analysis")
    )
})

test_that("portable path tools do not embed a maintainer home directory", {
    tool_paths <- c(
        here::here("scripts", "tools", "browse_diagnostics.R"),
        here::here("scripts", "tools", "export_gep_objective4_to_downloads.R")
    )

    expect_true(all(file.exists(tool_paths)))
    tool_text <- unlist(lapply(tool_paths, readLines, warn = FALSE), use.names = FALSE)
    expect_false(any(grepl("/Users/ncamarda", tool_text, fixed = TRUE)))
})

test_that("portable test runner propagates testthat failures", {
    runner <- here::here("scripts", "tools", "run_testthat.R")
    expect_true(file.exists(runner))

    passing_dir <- tempfile("intentional-testthat-pass-")
    failing_dir <- tempfile("intentional-testthat-failure-")
    dir.create(passing_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(failing_dir, recursive = TRUE, showWarnings = FALSE)
    withr::defer(unlink(passing_dir, recursive = TRUE, force = TRUE), envir = parent.frame())
    withr::defer(unlink(failing_dir, recursive = TRUE, force = TRUE), envir = parent.frame())
    writeLines(
        "testthat::test_that('intentional pass', testthat::expect_true(TRUE))",
        file.path(passing_dir, "test-intentional-pass.R")
    )
    writeLines(
        "testthat::test_that('intentional failure', testthat::expect_true(FALSE))",
        file.path(failing_dir, "test-intentional-failure.R")
    )

    passing_status <- system2(
        file.path(R.home("bin"), "Rscript"),
        c(shQuote(runner), shQuote(passing_dir)),
        stdout = FALSE,
        stderr = FALSE
    )
    failing_status <- suppressWarnings(
        system2(
            file.path(R.home("bin"), "Rscript"),
            c(shQuote(runner), shQuote(failing_dir)),
            stdout = FALSE,
            stderr = FALSE
        )
    )
    expect_identical(passing_status, 0L)
    expect_true(failing_status != 0L)
})

test_that("clean-checkout production and CI files are not excluded", {
    ignore_text <- readLines(here::here(".gitignore"), warn = FALSE)

    expect_true(file.exists(here::here("scripts", "tools", "export_gep_objective4_to_downloads.R")))
    expect_true(file.exists(here::here(".github", "workflows", "portable-tests.yml")))
    expect_true(file.exists(here::here(".lintr")))
    expect_false(any(trimws(ignore_text) == "scripts/tools/export_gep_objective4_to_downloads.R"))
    expect_false(any(trimws(ignore_text) == ".github/"))
    expect_false(any(trimws(ignore_text) == ".lintr"))
})

test_that("configured raw, runtime, and publish paths retain their storage roles", {
    expect_equal(RAW_DATA_DIR, file.path(EXPORT_ROOT, "Original Files"))
    expect_equal(DATA_DIR, EXPORT_ROOT)
    expect_equal(EXPORT_ANALYSIS_DIR, file.path(EXPORT_ROOT, "outputs"))
    expect_equal(PROCESSED_DATA_DIR, file.path(RUNTIME_ROOT, "Analytic Dataset"))
    expect_equal(OUTPUT_DIR, file.path(RUNTIME_ROOT, "Analysis"))
    expect_equal(TOOLS_OUTPUT_DIR, file.path(RUNTIME_ROOT, "tools_output"))
})

test_that("propensity sensitivity output is registered only for the restricted cohort", {
    output_root <- tempfile("cohort-output-routing-")
    old_output <- OUTPUT_DIR
    on.exit({
        assign("OUTPUT_DIR", old_output, envir = .GlobalEnv)
        unlink(output_root, recursive = TRUE, force = TRUE)
    }, add = TRUE)
    assign("OUTPUT_DIR", output_root, envir = .GlobalEnv)

    full <- setup_cohort_outputs("uveal_melanoma_full_cohort", "uveal_full")
    restricted <- setup_cohort_outputs(
        OBJECTIVE1_PROPENSITY_DATASET,
        "uveal_restricted"
    )
    gksrs <- setup_cohort_outputs("uveal_melanoma_gksrs_only_cohort", "gksrs")

    expect_false("obj1_propensity_sensitivity" %in% names(full$output_dirs))
    expect_true("obj1_propensity_sensitivity" %in% names(restricted$output_dirs))
    expect_false("obj1_propensity_sensitivity" %in% names(gksrs$output_dirs))
    expect_false(dir.exists(file.path(
        full$cohort_base_dir,
        "01_Efficacy",
        "h_propensity_score_sensitivity"
    )))
    expect_true(dir.exists(restricted$output_dirs$obj1_propensity_sensitivity))
    expect_false(dir.exists(file.path(
        gksrs$cohort_base_dir,
        "01_Efficacy",
        "h_propensity_score_sensitivity"
    )))
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
    expect_true(is_publishable_relative_artifact("merged_tables/objective4_poster_figures/three_panel.png", "merged_tables"))
    expect_true(is_publishable_relative_artifact("merged_tables/objective4_poster_figures/three_panel_report.md", "merged_tables"))
    expect_true(is_publishable_relative_artifact("04_GEP_Validation/unified_summary/full_cohort_simple_gep_validation_report.md", "cohort"))
    expect_false(is_publishable_relative_artifact("01_Efficacy/model.rds", "cohort"))
    expect_false(is_publishable_relative_artifact("01_Efficacy/summary_diagnostics.xlsx", "cohort"))
    expect_false(is_publishable_relative_artifact("cache/intermediate.csv", "cohort"))
    expect_false(is_publishable_relative_artifact("04_GEP_Validation/unified_summary/full_cohort_simple_gep_validation_report.txt", "cohort"))
    expect_false(is_publishable_relative_artifact("04_GEP_Validation/a_metastasis_free_survival/05_summary_tables/full_cohort_mfs_validation_narrative_summary.txt", "cohort"))
    expect_true(is_publishable_relative_artifact("04_GEP_Validation/c_proportional_hazards_diagnostics/full_cohort_mfs_proportional_hazards_summary.txt", "cohort"))
    expect_true(is_publishable_relative_artifact(
        "01_Efficacy/h_propensity_score_sensitivity/restricted_cohort_propensity_overlap_summary.md",
        "cohort"
    ))
    expect_false(is_publishable_relative_artifact(
        "01_Efficacy/h_propensity_score_sensitivity/restricted_cohort_propensity_design_audit.rds",
        "cohort"
    ))
    expect_false(is_publishable_relative_artifact(
        "01_Efficacy/h_propensity_score_sensitivity/patient_weights.csv",
        "cohort"
    ))
})

test_that("propensity publication dry run includes six reader artifacts and excludes audit RDS", {
    tmp_root <- tempfile("propensity-publish-dryrun-")
    output_root <- file.path(tmp_root, "runtime", "Analysis")
    export_root <- file.path(tmp_root, "export")
    propensity_dir <- file.path(
        output_root,
        "uveal_restricted",
        "01_Efficacy",
        "h_propensity_score_sensitivity"
    )
    dir.create(propensity_dir, recursive = TRUE)
    reader_basenames <- paste0(
        "restricted_cohort_",
        unname(OBJECTIVE1_PROPENSITY_ARTIFACT_BASENAMES[names(
            OBJECTIVE1_PROPENSITY_ARTIFACT_BASENAMES
        ) != "audit"])
    )
    audit_basename <- paste0(
        "restricted_cohort_",
        OBJECTIVE1_PROPENSITY_ARTIFACT_BASENAMES[["audit"]]
    )
    for (basename in c(reader_basenames, audit_basename)) {
        writeLines("unit artifact", file.path(propensity_dir, basename))
    }

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
    assign("EXPORT_ANALYSIS_DIR", file.path(export_root, "outputs"), envir = .GlobalEnv)

    result <- publish_outputs(
        cohorts = OBJECTIVE1_PROPENSITY_DATASET,
        snapshot_id = "2026-08-04-propensity-unit",
        include_merged_tables = FALSE,
        dry_run = TRUE
    )
    would_copy <- result$manifest$source_path[result$manifest$status == "would_copy"]

    expect_equal(result$summary$would_copy, 6L)
    expect_setequal(basename(would_copy), reader_basenames)
    expect_false(audit_basename %in% basename(would_copy))
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

test_that("publish CLI report gives dry-run scope and exact execute command", {
    report <- format_publish_outputs_cli_report(
        result = list(
            snapshot_dir = "/tmp/export/2026-07-28",
            dry_run = TRUE,
            summary = list(
                publishable_files = 2,
                copied = 0,
                would_copy = 2,
                skipped = 1,
                missing = 0,
                failed = 0,
                snapshot_exists = FALSE
            )
        ),
        opts = list(
            snapshot_id = "2026-07-28",
            snapshot_id_supplied = TRUE,
            cohorts = c("uveal_melanoma_full_cohort", "gksrs"),
            include_merged_tables = FALSE
        )
    )

    expect_match(report, "DRY RUN: no files were copied", fixed = TRUE)
    expect_match(report, "Would copy: 2", fixed = TRUE)
    expect_match(report, "Excluded by registry: 1", fixed = TRUE)
    expect_match(
        report,
        "Rscript scripts/workflow/publish_outputs.R --execute --snapshot-id 2026-07-28 --cohorts uveal_melanoma_full_cohort,gksrs --no-merged-tables",
        fixed = TRUE
    )

    default_snapshot_command <- publish_execute_command(list(
        snapshot_id_supplied = FALSE,
        cohorts = NULL,
        include_merged_tables = TRUE
    ))
    expect_identical(
        default_snapshot_command,
        "Rscript scripts/workflow/publish_outputs.R --execute"
    )
})

test_that("default publish snapshots add letter suffixes on the same day", {
    tmp_root <- tempfile("runtime-publish-suffix-")
    export_root <- file.path(tmp_root, "export")
    export_analysis_root <- file.path(export_root, "Analysis")
    dir.create(file.path(export_analysis_root, "2026-08-04"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(export_analysis_root, "2026-08-04-a"), recursive = TRUE, showWarnings = FALSE)

    old_export_root <- EXPORT_ROOT
    old_export_analysis <- EXPORT_ANALYSIS_DIR
    on.exit({
        assign("EXPORT_ROOT", old_export_root, envir = .GlobalEnv)
        assign("EXPORT_ANALYSIS_DIR", old_export_analysis, envir = .GlobalEnv)
        unlink(tmp_root, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    assign("EXPORT_ROOT", export_root, envir = .GlobalEnv)
    assign("EXPORT_ANALYSIS_DIR", export_analysis_root, envir = .GlobalEnv)

    expect_identical(
        next_available_publish_snapshot_id(default_snapshot_id = "2026-08-04"),
        "2026-08-04-b"
    )
})

test_that("publish CLI main prints the concise report without manifest rows", {
    tmp_root <- tempfile("runtime-publish-cli-")
    output_root <- file.path(tmp_root, "runtime", "Analysis")
    export_root <- file.path(tmp_root, "export")
    export_analysis_root <- file.path(export_root, "Analysis")
    dir.create(file.path(output_root, "uveal_full", "01_Efficacy"), recursive = TRUE, showWarnings = FALSE)
    writeLines("report", file.path(output_root, "uveal_full", "01_Efficacy", "summary.xlsx"))

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

    output <- capture_output(main(list(
        snapshot_id = "2026-07-28-cli",
        dry_run = TRUE,
        include_merged_tables = FALSE,
        cohorts = "uveal_full",
        help = FALSE
    )))

    expect_match(output, "DRY RUN: no files were copied", fixed = TRUE)
    expect_match(output, "Next step (performs the copy):", fixed = TRUE)
    expect_false(grepl("source_path", output, fixed = TRUE))
})

test_that("publish_outputs prefers Objective 4 markdown summaries over legacy text duplicates", {
    tmp_root <- tempfile("runtime-publish-obj4-md-")
    runtime_root <- file.path(tmp_root, "runtime")
    output_root <- file.path(runtime_root, "Analysis")
    export_root <- file.path(tmp_root, "export")
    export_analysis_root <- file.path(export_root, "Analysis")
    obj4_summary_dir <- file.path(output_root, "uveal_full", "04_GEP_Validation", "unified_summary")
    obj4_mfs_summary_dir <- file.path(output_root, "uveal_full", "04_GEP_Validation", "a_metastasis_free_survival", "05_summary_tables")
    obj4_diag_dir <- file.path(output_root, "uveal_full", "04_GEP_Validation", "c_proportional_hazards_diagnostics")

    dir.create(obj4_summary_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(obj4_mfs_summary_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(obj4_diag_dir, recursive = TRUE, showWarnings = FALSE)

    writeLines("md summary", file.path(obj4_summary_dir, "full_cohort_simple_gep_validation_report.md"))
    writeLines("legacy txt summary", file.path(obj4_summary_dir, "full_cohort_simple_gep_validation_report.txt"))
    writeLines("md narrative", file.path(obj4_mfs_summary_dir, "full_cohort_mfs_validation_narrative_summary.md"))
    writeLines("legacy txt narrative", file.path(obj4_mfs_summary_dir, "full_cohort_mfs_validation_narrative_summary.txt"))
    writeLines("diagnostic txt", file.path(obj4_diag_dir, "full_cohort_mfs_proportional_hazards_summary.txt"))

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

    result <- publish_outputs(snapshot_id = "2026-04-02-obj4-md", dry_run = TRUE)

    would_copy_sources <- result$manifest$source_path[result$manifest$status == "would_copy"]
    skipped_sources <- result$manifest$source_path[result$manifest$status == "skipped_not_publishable"]

    expect_true(any(grepl("full_cohort_simple_gep_validation_report\\.md$", would_copy_sources)))
    expect_true(any(grepl("full_cohort_mfs_validation_narrative_summary\\.md$", would_copy_sources)))
    expect_true(any(grepl("full_cohort_mfs_proportional_hazards_summary\\.txt$", would_copy_sources)))
    expect_true(any(grepl("full_cohort_simple_gep_validation_report\\.txt$", skipped_sources)))
    expect_true(any(grepl("full_cohort_mfs_validation_narrative_summary\\.txt$", skipped_sources)))
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
    old_logs <- LOGS_DIR
    old_merged <- MERGED_TABLES_DIR
    old_export_root <- EXPORT_ROOT
    old_export_analysis <- EXPORT_ANALYSIS_DIR

    on.exit({
        assign("OUTPUT_DIR", old_output, envir = .GlobalEnv)
        assign("LOGS_DIR", old_logs, envir = .GlobalEnv)
        assign("MERGED_TABLES_DIR", old_merged, envir = .GlobalEnv)
        assign("EXPORT_ROOT", old_export_root, envir = .GlobalEnv)
        assign("EXPORT_ANALYSIS_DIR", old_export_analysis, envir = .GlobalEnv)
        unlink(tmp_root, recursive = TRUE, force = TRUE)
    }, add = TRUE)

    assign("OUTPUT_DIR", output_root, envir = .GlobalEnv)
    assign("LOGS_DIR", file.path(runtime_root, "logs"), envir = .GlobalEnv)
    assign("MERGED_TABLES_DIR", file.path(output_root, "merged_tables"), envir = .GlobalEnv)
    assign("EXPORT_ROOT", export_root, envir = .GlobalEnv)
    assign("EXPORT_ANALYSIS_DIR", export_analysis_root, envir = .GlobalEnv)

    clean_log <- file.path(LOGS_DIR, "txt", "run_log_20260806_000000.txt")
    dir.create(dirname(clean_log), recursive = TRUE, showWarnings = FALSE)
    writeLines("[INFO] analysis completed", clean_log)

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
test_that("configured raw input uses the versioned NDC workbook", {
    expect_identical(
        INPUT_FILENAME,
        "Ocular Melanoma Master Spreadsheet REVISED FOR STATS (08-11-26 NDC).xlsx"
    )
})
