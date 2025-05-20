# # Source the data processing script
# source("scripts/data_processing.R")

# Minimal mock data for testing
# Includes all variables referenced in the data processing pipeline
data_mock <- tibble(
  id = 1:3,
  treatment_group = c("Plaque", "GKSRS", "Plaque"),
  sex = c("Male", "Female", "Male"),
  location = c("Choroidal", "Ciliary_Body", "Choroidal"),
  eye = c("OD", "OS", "OD"),
  race = c("White", "Black", "Asian"),
  initial_vision = c(20, 40, 60),
  initial_gk = c("Y", "N", "Y"),
  initial_plaque = c("N", "Y", "N"),
  initial_tumor_diameter = c(15, 25, 18),
  initial_tumor_height = c(8, 12, 9),
  optic_nerve = c("N", "Y", "N"),
  internal_reflectivity = c("Low", "High", "Medium"),
  srf = c("Y", "N", "Y"),
  op = c("Y", "N", "Y"),
  symptoms = c("Y", "N", "Y"),
  vision_loss_blurred_vision = c("Y", "N", "Y"),
  visual_field_defect = c("N", "Y", "N"),
  flashes_photopsia = c("Y", "N", "Y"),
  floaters = c("N", "Y", "N"),
  pain = c("N", "Y", "N"),
  initial_overall_stage = c("2A", "3A", "1"),
  initial_t_stage = c("T2", "T3", "T1"),
  initial_n_stage = c("N0", "N1", "N0"),
  initial_m_stage = c("M0", "M1", "M0"),
  initial_mets = c("N", "Y", "N"),
  biopsy1_gep = c("Class_1A_PRAME_negative", "Class_2_PRAME_negative", "Class_1B_PRAME_negative"),
  recurrence1 = c("N", "Y", NA),
  recurrence1_date = as.Date(c(NA, "2020-01-01", NA)),
  mets_progression = c("N", "Y", "N"),
  mets_progression_date = as.Date(c(NA, "2021-01-01", NA)),
  dob = as.Date(c("1960-01-01", "1970-01-01", "1980-01-01")),
  date_diagnosis = as.Date(c("2010-01-01", "2015-01-01", "2020-01-01")),
  initial_gk_date = as.Date(c("2010-02-01", NA, "2020-02-01")),
  initial_plaque_date = as.Date(c(NA, "2015-02-01", NA)),
  dod = as.Date(c(NA, "2023-01-01", NA)),
  consort_group = c("eligible_both", "gksrs_only", "eligible_both"),
  last_known_alive_date = as.Date(c("2022-01-01", "2023-01-01", "2024-01-01")),
  treatment_date = c(as.Date("2010-02-01"), as.Date("2015-02-01"), as.Date("2020-02-01"))
)

test_that("fix_event_date_consistency updates event indicators correctly", {
  data <- tibble(
    event = c("N", NA, "Y"),
    date = as.Date(c("2020-01-01", "2020-01-01", NA))
  )
  fixed <- fix_event_date_consistency(data, "event", "date")
  expect_equal(fixed$event, c("Y", "Y", "N"))
})

test_that("create_derived_variables adds all relevant columns and values are sane", {
  derived <- create_derived_variables(data_mock)
  expected_vars <- c(
    # Original variables from mock data
    "id", "sex", "location", "eye", "race", "initial_vision", "initial_gk", "initial_plaque",
    "initial_tumor_diameter", "initial_tumor_height", "optic_nerve", "internal_reflectivity",
    "srf", "op", "symptoms", "vision_loss_blurred_vision", "visual_field_defect",
    "flashes_photopsia", "floaters", "pain", "initial_overall_stage", "initial_t_stage",
    "initial_n_stage", "initial_m_stage", "initial_mets", "biopsy1_gep", "recurrence1",
    "recurrence1_date", "mets_progression", "mets_progression_date", "dob", "date_diagnosis",
    "initial_gk_date", "initial_plaque_date", "dod",
    # Derived variables
    "consort_group", "treatment_group", "age_at_diagnosis", "follow_up_days", "follow_up_years",
    "treatment_date", "tt_recurrence", "tt_mets", "tt_death",
    "recurrence_event", "mets_event", "death_event",  "last_known_alive_date"
  )
  missing_vars <- setdiff(expected_vars, colnames(derived))
  expect(
    length(missing_vars) == 0,
    sprintf("Missing variables: %s", paste(missing_vars, collapse = ", "))
  )

  # Check that no column is all NA
  all_na_cols <- names(which(sapply(derived[, expected_vars, drop=FALSE], function(x) all(is.na(x)))))
  expect(
    length(all_na_cols) == 0,
    sprintf("Columns with all NA: %s", paste(all_na_cols, collapse = ", "))
  )

  # Check types for a few key variables
  expect_type(derived$id, "integer")
  expect_true(is.numeric(derived$age_at_diagnosis))
  expect_true(is.factor(derived$recurrence1) || is.character(derived$recurrence1))
  expect_true(is.numeric(derived$follow_up_days))
  # check all the date columns
  expect_true(is.Date(derived$dob))
  expect_true(is.Date(derived$last_known_alive_date))

  # Check value ranges for numeric columns
  expect_true(all(derived$age_at_diagnosis > 0 & derived$age_at_diagnosis < 120, na.rm = TRUE))
  expect_true(all(derived$follow_up_days >= 0 & derived$follow_up_days < 365*20, na.rm = TRUE))
})

test_that("apply_criteria splits data into correct cohorts", {
  derived <- create_derived_variables(data_mock)
  cohorts <- apply_criteria(derived)
  expect_true(all(c("full_cohort", "restricted_cohort", "gksrs_only_cohort") %in% names(cohorts)))
  expect_true(is.data.frame(cohorts$full_cohort))
})

test_that("prepare_factor_levels converts columns to factors", {
  derived <- create_derived_variables(data_mock)
  factored <- prepare_factor_levels(derived)
  expect_true(is.factor(factored$recurrence1))
  expect_true(is.factor(factored$treatment_group))
})

test_that("calculate_treatment_duration_metrics returns correct metrics", {
  derived <- create_derived_variables(data_mock)
  metrics <- calculate_treatment_duration_metrics(derived)
  
  # Check structure
  expect_true(is.data.frame(metrics$interval_metrics))
  expect_true(is.data.frame(metrics$summary_stats))
  expect_true(all(c("interval_end", "interval_label") %in% colnames(metrics$interval_metrics)))
  expect_true(all(c("treatment_group", "n_total", "mean_followup_years") %in% colnames(metrics$summary_stats)))
  
  # Check that patient counts are correct for the mock data
  expect_equal(
    metrics$summary_stats$n_total[metrics$summary_stats$treatment_group == "Plaque"],
    1
  )
  expect_equal(
    metrics$summary_stats$n_total[metrics$summary_stats$treatment_group == "GKSRS"],
    2
  )
  
  # Check that follow-up years are calculated as expected
  # For example, for the first patient:
  expected_followup_years <- as.numeric(difftime(
    data_mock$last_known_alive_date[2],
    data_mock$treatment_date[2],
    units = "days"
  )) / 365.25
  actual_mean <- metrics$summary_stats$mean_followup_years[metrics$summary_stats$treatment_group == "Plaque"]
  expect_true(abs(actual_mean - expected_followup_years) < 1e-6)
})

test_that("get_cohort_info returns correct structure", {
  info <- get_cohort_info("full_cohort")
  expect_true(all(c("dir", "prefix") %in% names(info)))
})

# To run the tests:
# source("scripts/data_processing.R"); testthat::test_file("scripts/unit_tests.R")