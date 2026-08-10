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
    output_dirs <- build_subdivided_output_dirs(test_output_dir, "^obj3_")
    
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

  expect_true(file.exists(file.path(pipeline_run$output_dirs$obj3_pfs2_cohort_support, "test_pfs2_treatment_summary.xlsx")))
  expect_true(file.exists(file.path(pipeline_run$output_dirs$obj3_pfs2_cox, "test_pfs2_analysis_SKIPPED.html")))
  expect_true(file.exists(file.path(pipeline_run$output_dirs$obj3_pfs2_cox, "test_pfs2_analysis_diagnostics.xlsx")))
  ph_skipped_files <- list.files(
    pipeline_run$output_dirs$obj3_pfs2_ph,
    pattern = "proportional_hazards_diagnostics_SKIPPED\\.html$",
    full.names = TRUE
  )
  expect_true(length(ph_skipped_files) > 0)
})

test_that("PFS-2 insufficient-event skips retain txt notes and add structured skip artifacts", {
  test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective3_pfs2_skip_test")
  output_dirs <- build_subdivided_output_dirs(test_output_dir, "^obj3_")
  for (dir_path in output_dirs) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
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

  expect_true(file.exists(file.path(output_dirs$obj3_pfs2_cohort_support, "test_pfs2_analysis_skipped_explanation.txt")))
  expect_true(file.exists(file.path(output_dirs$obj3_pfs2_cox, "test_pfs2_analysis_SKIPPED.html")))
  expect_true(file.exists(file.path(output_dirs$obj3_pfs2_cox, "test_pfs2_analysis_diagnostics.xlsx")))
  ph_skipped_files <- list.files(
    output_dirs$obj3_pfs2_ph,
    pattern = "proportional_hazards_diagnostics_SKIPPED\\.html$",
    full.names = TRUE
  )
  expect_true(length(ph_skipped_files) > 0)

  skip_sheets <- readxl::excel_sheets(file.path(output_dirs$obj3_pfs2_cox, "test_pfs2_analysis_diagnostics.xlsx"))
  expect_true(all(c("Skip_summary", "Narrative_summary", "Event_support") %in% skip_sheets))

  skip_html <- paste(
    readLines(file.path(output_dirs$obj3_pfs2_cox, "test_pfs2_analysis_SKIPPED.html"), warn = FALSE),
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
  output_dirs <- build_subdivided_output_dirs(test_output_dir, "^obj3_")
  for (dir_path in output_dirs) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
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
  expect_false(is.null(result$survival_analysis))
  expect_null(result$survival_analysis$cox_model)
  expect_null(result$summary_table)
  expect_null(result$ph_diagnostics)
  expect_true(file.exists(file.path(output_dirs$obj3_pfs2_cohort_support, "test_pfs2_analysis_skipped_explanation.txt")))
  expect_true(file.exists(file.path(output_dirs$obj3_pfs2_cox, "test_pfs2_analysis_SKIPPED.html")))
  expect_true(file.exists(file.path(output_dirs$obj3_pfs2_cox, "test_pfs2_analysis_diagnostics.xlsx")))

  skip_sheets <- readxl::excel_sheets(file.path(output_dirs$obj3_pfs2_cox, "test_pfs2_analysis_diagnostics.xlsx"))
  expect_true(all(c("Skip_summary", "Narrative_summary", "Event_support", "Model_context") %in% skip_sheets))
})

test_that("PFS-2 derivation censors death before second recurrence", {
  base_date <- as.Date("2020-01-01")
  test_data <- create_test_dataset()
  test_data$recurrence1[[1]] <- "Y"
  test_data$recurrence1_date[[1]] <- base_date + 30
  test_data$recurrence1_treatment_date[[1]] <- base_date + 60
  test_data$recurrence1_treatment[[1]] <- "GKSRS"
  test_data$recurrence2[[1]] <- "Y"
  test_data$recurrence2_date[[1]] <- base_date + 240
  test_data$dod[[1]] <- base_date + 120
  test_data$last_known_alive_date[[1]] <- base_date + 300

  derived <- create_derived_variables(test_data)

  expect_equal(derived$pfs2_event[[1]], 0)
  expect_equal(derived$tt_pfs2[[1]], 60)
  expect_equal(round(derived$tt_pfs2_months[[1]], 1), round(lubridate::time_length(lubridate::interval(base_date + 60, base_date + 120), "months"), 1))
})

test_that("PFS-2 derivation is invariant to raw and display recurrence coding", {
  raw_data <- create_test_dataset()
  display_data <- raw_data

  display_data$recurrence1 <- dplyr::case_when(
    raw_data$recurrence1 == "Y" ~ "Yes",
    raw_data$recurrence1 == "N" ~ "No",
    TRUE ~ raw_data$recurrence1
  )
  display_data$recurrence2 <- dplyr::case_when(
    raw_data$recurrence2 == "Y" ~ "Yes",
    raw_data$recurrence2 == "N" ~ "No",
    TRUE ~ raw_data$recurrence2
  )

  raw_derived <- create_derived_variables(raw_data)
  display_derived <- create_derived_variables(display_data)

  expect_equal(display_derived$tt_pfs2, raw_derived$tt_pfs2)
  expect_equal(display_derived$tt_pfs2_months, raw_derived$tt_pfs2_months)
  expect_equal(display_derived$tt_pfs2_years, raw_derived$tt_pfs2_years)
  expect_equal(display_derived$pfs2_event, raw_derived$pfs2_event)
  expect_equal(display_derived$recurrence1_treatment_clean, raw_derived$recurrence1_treatment_clean)
})

test_that("PFS-2 summaries include censoring support and downgrade notes", {
  test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective3_pfs2_censoring_support")
  output_dirs <- build_subdivided_output_dirs(test_output_dir, "^obj3_")
  for (dir_path in output_dirs) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
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

  summary_path <- file.path(output_dirs$obj3_pfs2_cohort_support, "test_pfs2_treatment_summary.xlsx")
  expect_true(file.exists(summary_path))
  expect_true(all(c("censoring_support", "interpretation_guardrails") %in% readxl::excel_sheets(summary_path)))
  guardrails <- readxl::read_xlsx(summary_path, sheet = "interpretation_guardrails")
  expect_true(any(guardrails$guardrail == "short_follow_up" & guardrails$status == "downgrade"))
  expect_identical(result$interpretation_guardrails$status, "downgraded")
})

test_that("PFS-2 zero-event reference arm suppresses Cox treatment output", {
  test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective3_pfs2_zero_reference")
  output_dirs <- build_subdivided_output_dirs(test_output_dir, "^obj3_")
  for (dir_path in output_dirs) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

  pfs2_test_data <- tibble::tibble(
    id = seq_len(12),
    tt_pfs2_months = c(8, 10, 12, 14, 16, 18, 9, 11, 13, 15, 17, 19),
    recurrence1_treatment_clean = factor(rep(c("GKSRS", "Plaque"), each = 6), levels = c("GKSRS", "Plaque")),
    recurrence1_treatment = rep(c("GKSRS", "Plaque"), each = 6),
    treatment_group = factor(rep(c("PBT", "GKSRS"), each = 6)),
    pfs2_event = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0)
  )

  result <- analyze_pfs2(
    data = pfs2_test_data,
    confounders = character(),
    dataset_name = "test_cohort",
    output_dirs = output_dirs,
    prefix = "test_"
  )

  expect_false(isTRUE(result$treatment_estimability$reportable))
  expect_match(result$treatment_estimability$reason, "reference salvage-treatment arm `GKSRS` had zero second-recurrence events")
  expect_null(result$survival_analysis$cox_model)
  expect_null(result$summary_table)

  skipped_files <- list.files(output_dirs$obj3_pfs2_cox, pattern = "cox_SKIPPED\\.html$", full.names = TRUE)
  expect_length(skipped_files, 1)
  skipped_html <- paste(readLines(skipped_files[[1]], warn = FALSE), collapse = "\n")
  expect_match(skipped_html, "zero second-recurrence events", fixed = TRUE)

  ph_skipped_files <- list.files(output_dirs$obj3_pfs2_ph, pattern = "proportional_hazards_diagnostics_SKIPPED\\.html$", full.names = TRUE)
  expect_length(ph_skipped_files, 1)
  ph_skipped_html <- paste(readLines(ph_skipped_files[[1]], warn = FALSE), collapse = "\n")
  expect_match(ph_skipped_html, "no Cox model was fit", fixed = TRUE)
  expect_match(ph_skipped_html, "zero second-recurrence events", fixed = TRUE)
  expect_true(any(grepl("proportional_hazards_diagnostics_diagnostics\\.xlsx$", list.files(output_dirs$obj3_pfs2_ph))))
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

  expect_identical(ph_result$status, "skipped")
  expect_true(file.exists(file.path(test_output_dir, "unit_proportional_hazards_diagnostics_SKIPPED.html")))
  expect_true(file.exists(file.path(test_output_dir, "unit_proportional_hazards_diagnostics_diagnostics.xlsx")))
  expect_false(file.exists(file.path(test_output_dir, "unit_proportional_hazards_tests.xlsx")))

  skip_summary <- readxl::read_xlsx(
    file.path(test_output_dir, "unit_proportional_hazards_diagnostics_diagnostics.xlsx"),
    sheet = "Skip_summary"
  )
  expect_true(all(c("input_n", "fitted_n") %in% skip_summary$metric))
  expect_equal(skip_summary$value[skip_summary$metric == "input_n"], "10")
  expect_equal(skip_summary$value[skip_summary$metric == "fitted_n"], "10")
})

test_that("successful PH diagnostics report input and fitted N", {
  test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective3_ph_sample_size")
  dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

  ph_test_data <- tibble::tibble(
    time_months = seq(5, 24),
    status = rep(c(1, 0), 10),
    treatment_group = factor(rep(c("PBT", "GKSRS"), 10))
  )
  ph_model <- survival::coxph(
    survival::Surv(time_months, status) ~ treatment_group,
    data = ph_test_data,
    model = TRUE
  )

  ph_result <- test_proportional_hazards_assumption(
    cox_model = ph_model,
    outcome_name = "Unit Test Survival",
    output_dir = test_output_dir,
    file_prefix = "unit_",
    dataset_name = "test_cohort",
    input_n = nrow(ph_test_data),
    fitted_n = get_model_fitted_n(ph_model)
  )

  expect_true(is.data.frame(ph_result$ph_summary))
  expect_true(all(ph_result$ph_summary$input_n == nrow(ph_test_data)))
  expect_true(all(ph_result$ph_summary$fitted_n == nrow(ph_test_data)))
  ph_workbook <- readxl::read_xlsx(
    file.path(test_output_dir, "unit_proportional_hazards_tests.xlsx")
  )
  expect_true(all(c("input_n", "fitted_n") %in% names(ph_workbook)))
  expect_true(all(ph_workbook$input_n == ph_workbook$fitted_n))
})

test_that("generic PH diagnostics helper writes skip artifacts when Cox is unavailable", {
  test_output_dir <- file.path(TEST_OUTPUT_DIR, "generic_ph_skip_artifacts")
  dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

  ph_skip_data <- tibble::tibble(
    time_months = c(6, 9, 12, 15),
    status = c(1, 0, 0, 1),
    treatment_group = factor(c("PBT", "PBT", "GKSRS", "GKSRS"))
  )

  ph_result <- run_or_skip_proportional_hazards_diagnostics(
    cox_model = NULL,
    outcome_name = "Unit Test Survival",
    output_dir = test_output_dir,
    file_prefix = "unit_survival_",
    dataset_name = "test_cohort",
    data = ph_skip_data,
    time_var = "time_months",
    event_var = "status",
    variables = "treatment_group",
    reason = "Unit Test Survival proportional hazards diagnostics were not run because no Cox model was fit."
  )

  expect_identical(ph_result$status, "skipped")
  expect_true(file.exists(file.path(test_output_dir, "unit_survival_proportional_hazards_diagnostics_SKIPPED.html")))
  expect_true(file.exists(file.path(test_output_dir, "unit_survival_proportional_hazards_diagnostics_diagnostics.xlsx")))

  ph_skip_html <- paste(
    readLines(file.path(test_output_dir, "unit_survival_proportional_hazards_diagnostics_SKIPPED.html"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(ph_skip_html, "no Cox model was fit", fixed = TRUE)
  expect_match(ph_skip_html, "Schoenfeld residual proportional hazards tests require a fitted Cox model", fixed = TRUE)
})
