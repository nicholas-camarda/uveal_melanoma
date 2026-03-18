# Test file for Objective 3: Repeat Radiation
# Tests the actual content and statistical results of the repeat radiation pipeline
# Run with: testthat::test_dir('tests/testthat')

# CRITICAL: Set test environment variables BEFORE sourcing load_all.R
# This prevents directory creation in the wrong location

# Override project constants to prevent test interference

# Load the project environment with ALL of the variables and functions
# You do not need to load libraries separately

#' Run the Objective 3 pipeline in the test harness
#'
#' @param data Data frame used as pipeline input.
#'
#' @return List containing pipeline results plus the temporary output paths, or
#'   an error wrapper.
run_objective3_pipeline_test <- function(data) {
  # Run Objective 3 (Repeat Radiation) pipeline for testing
  tryCatch({
    # Create proper output directory structure
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective3_test")
    output_dirs <- list(
      obj3_pfs2 = file.path(test_output_dir, "03_Repeat_Radiation", "a_pfs2"),
      obj3_ph_diagnostics = file.path(test_output_dir, "03_Repeat_Radiation", "b_proportional_hazards_diagnostics")
    )
    
    # Create directories
    for (dir_path in output_dirs) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Set up test parameters
    dataset_name <- "test_cohort"
    prefix <- "test_"
    confounders <- c("age_at_diagnosis", "sex")

    # Run the ACTUAL Objective 3 pipeline function
    results <- run_objective_3(data, dataset_name, output_dirs, prefix, confounders)

    return(list(
      results = results,
      output_dirs = output_dirs,
      test_output_dir = test_output_dir
    ))
  }, error = function(e) {
    list(error = e$message)
  })
}

test_that("Objective 3 pipeline returns the current PFS-2 analysis contract", {
  test_data <- tibble::tibble(
    id = seq_len(12),
    tt_pfs2_months = c(8, 10, 12, 14, 16, 18, 9, 11, 13, 15, 17, 19),
    recurrence1_treatment_clean = factor(rep(c("GKSRS", "Plaque"), each = 6)),
    recurrence1_treatment = rep(c("GKSRS", "Plaque"), each = 6),
    treatment_group = factor(rep(c("PBT", "GKSRS"), each = 6)),
    pfs2_event = c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0),
    age_at_diagnosis = rep(c(60, 68, 72), length.out = 12),
    sex = factor(rep(c("Male", "Female"), length.out = 12))
  )

  pipeline_run <- run_objective3_pipeline_test(test_data)
  withr::defer(unlink(pipeline_run$test_output_dir, recursive = TRUE), envir = parent.frame())
  results <- pipeline_run$results

  expect_named(results, "pfs2_analysis")
  expect_false(is.null(results$pfs2_analysis$pfs2_data))
  expect_equal(nrow(results$pfs2_analysis$pfs2_data), 12)
  expect_null(results$pfs2_analysis$summary_table)
  expect_null(results$pfs2_analysis$survival_analysis$cox_model)

  expect_true(file.exists(file.path(pipeline_run$output_dirs$obj3_pfs2, "test_pfs2_treatment_summary.xlsx")))
  expect_true(file.exists(file.path(pipeline_run$output_dirs$obj3_pfs2, "test_pfs2_analysis_SKIPPED.html")))
  expect_true(file.exists(file.path(pipeline_run$output_dirs$obj3_pfs2, "test_pfs2_analysis_diagnostics.xlsx")))
  expect_true(file.exists(file.path(pipeline_run$output_dirs$obj3_ph_diagnostics, "test_pfs2_analysis_SKIPPED.html")))
  expect_true(file.exists(file.path(pipeline_run$output_dirs$obj3_ph_diagnostics, "test_pfs2_analysis_diagnostics.xlsx")))
})

test_that("PFS-2 insufficient-event skips retain txt notes and add structured skip artifacts", {
  test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective3_pfs2_skip_test")
  output_dirs <- list(
    obj3_pfs2 = file.path(test_output_dir, "03_Repeat_Radiation", "a_pfs2"),
    obj3_ph_diagnostics = file.path(test_output_dir, "03_Repeat_Radiation", "b_proportional_hazards_diagnostics")
  )
  dir.create(output_dirs$obj3_pfs2, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dirs$obj3_ph_diagnostics, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

  pfs2_test_data <- tibble::tibble(
    id = seq_len(12),
    tt_pfs2_months = c(8, 10, 12, 14, 16, 18, 9, 11, 13, 15, 17, 19),
    recurrence1_treatment_clean = factor(rep(c("GKSRS", "Plaque"), each = 6)),
    recurrence1_treatment = rep(c("GKSRS", "Plaque"), each = 6),
    treatment_group = factor(rep(c("PBT", "GKSRS"), each = 6)),
    pfs2_event = c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  result <- analyze_pfs2(
    data = pfs2_test_data,
    confounders = character(),
    dataset_name = "test_cohort",
    output_dirs = output_dirs,
    prefix = "test_"
  )

  expect_false(is.null(result$pfs2_data))
  expect_null(result$survival_analysis$cox_model)

  expect_true(file.exists(file.path(output_dirs$obj3_pfs2, "test_pfs2_analysis_skipped_explanation.txt")))
  expect_true(file.exists(file.path(output_dirs$obj3_ph_diagnostics, "test_pfs2_analysis_skipped_explanation.txt")))
  expect_true(file.exists(file.path(output_dirs$obj3_pfs2, "test_pfs2_analysis_SKIPPED.html")))
  expect_true(file.exists(file.path(output_dirs$obj3_pfs2, "test_pfs2_analysis_diagnostics.xlsx")))
  expect_true(file.exists(file.path(output_dirs$obj3_ph_diagnostics, "test_pfs2_analysis_SKIPPED.html")))
  expect_true(file.exists(file.path(output_dirs$obj3_ph_diagnostics, "test_pfs2_analysis_diagnostics.xlsx")))

  skip_sheets <- readxl::excel_sheets(file.path(output_dirs$obj3_pfs2, "test_pfs2_analysis_diagnostics.xlsx"))
  expect_true(all(c("Skip_summary", "Narrative_summary", "Event_support") %in% skip_sheets))

  skip_html <- paste(
    readLines(file.path(output_dirs$obj3_pfs2, "test_pfs2_analysis_SKIPPED.html"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    skip_html,
    sprintf("at least %d are required", MINIMUM_SURVIVAL_EVENTS),
    fixed = TRUE
  )
  expect_match(skip_html, "Modeled Outcome Counts By Covariate Level", fixed = TRUE)
})

test_that("PFS-2 precheck requires the configured minimum analyzable patient count", {
  test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective3_pfs2_patient_threshold")
  output_dirs <- list(
    obj3_pfs2 = file.path(test_output_dir, "03_Repeat_Radiation", "a_pfs2"),
    obj3_ph_diagnostics = file.path(test_output_dir, "03_Repeat_Radiation", "b_proportional_hazards_diagnostics")
  )
  dir.create(output_dirs$obj3_pfs2, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dirs$obj3_ph_diagnostics, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

  pfs2_small_n <- tibble::tibble(
    id = seq_len(9),
    tt_pfs2_months = c(8, 10, 12, 14, 16, 18, 9, 11, 13),
    recurrence1_treatment_clean = factor(rep("GKSRS", 9), levels = c("GKSRS", "Plaque")),
    recurrence1_treatment = rep("GKSRS", 9),
    treatment_group = factor(rep("PBT", 9), levels = c("PBT", "GKSRS")),
    pfs2_event = c(1, 1, 1, 1, 1, 0, 0, 0, 0)
  )

  result <- analyze_pfs2(
    data = pfs2_small_n,
    confounders = character(),
    dataset_name = "test_cohort",
    output_dirs = output_dirs,
    prefix = "test_"
  )

  expect_equal(nrow(result$pfs2_data), MINIMUM_PFS2_PATIENTS - 1L)
  expect_null(result$survival_analysis)
  expect_null(result$summary_table)
  expect_null(result$ph_diagnostics)
  expect_false(file.exists(file.path(output_dirs$obj3_pfs2, "test_pfs2_analysis_SKIPPED.html")))
  expect_false(file.exists(file.path(output_dirs$obj3_pfs2, "test_pfs2_analysis_diagnostics.xlsx")))
})

test_that("PH diagnostics are skipped below the configured event floor", {
  test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective3_ph_event_floor")
  dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

  ph_test_data <- tibble::tibble(
    time_months = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
    status = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0),
    treatment_group = factor(rep(c("PBT", "GKSRS"), length.out = 10))
  )

  ph_model <- survival::coxph(Surv(time_months, status) ~ treatment_group, data = ph_test_data)
  expect_equal(ph_model$nevent, MINIMUM_PH_TEST_EVENTS - 1L)

  ph_result <- test_proportional_hazards_assumption(
    cox_model = ph_model,
    outcome_name = "Unit Test Survival",
    output_dir = test_output_dir,
    file_prefix = "unit_",
    dataset_name = "test_cohort"
  )

  expect_null(ph_result)
  expect_false(file.exists(file.path(test_output_dir, "unit_proportional_hazards_tests.xlsx")))
})
