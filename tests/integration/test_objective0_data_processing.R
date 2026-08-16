  # Load test data

  
  test_that("Data processing pipeline completes without segmentation fault", {
  # Load actual data and create a small test sample
  logger::log_info("Loading and cleaning raw data")
  raw_data <- load_and_clean_data(INPUT_FILENAME)
  logger::log_info(sprintf("Loaded %d rows of raw data", nrow(raw_data)))

  expect_true(nrow(raw_data) == 264)
  id247 <- raw_data %>% dplyr::filter(.data$id == 247)
  expect_equal(nrow(id247), 1)
  expect_equal(as.character(id247$consort_group), CONSORT_GROUP_FULL_ONLY_SPECIAL_CASE)
  expect_equal(as.character(id247$cohort_assignment_special_case), IRIS_OPTIC_NERVE_SPECIAL_CASE)
  expect_equal(as.character(id247$optic_nerve), "N")

  logger::log_info("Creating derived variables")
  derived_data <- create_derived_variables(raw_data)

  new_variables <- str_split(
      string = "treatment_group, age_at_diagnosis, follow_up_days, follow_up_years, follow_up_months, treatment_date, tt_recurrence, tt_pfs2, tt_mets, tt_death, tt_recurrence_months, tt_mets_months, tt_death_months, tt_pfs_months, tt_pfs2_months, tt_recurrence_years, tt_mets_years, tt_death_years, tt_pfs2_years, mets_before_treatment, recurrence_before_treatment, death_before_treatment, tt_mets_months_analysis, tt_recurrence_months_analysis, tt_death_months_analysis, tt_pfs_months_analysis, height_change, recurrence_event, mets_event, death_event, melanoma_death_event, competing_death_event, pfs_event, pfs2_event, recurrence1_treatment_clean, mets_free_at_baseline, gep_class_simple, expected_mfs_5yr, expected_mfs_7yr, expected_mfs_10yr, expected_mss_5yr, expected_mss_7yr, expected_mss_10yr, prame_status, gep_validation_set, mfs_event_5yr, mfs_event_7yr, mfs_event_10yr, mss_event_5yr, mss_event_7yr, mss_event_10yr, predicted_mfs_risk_5yr, predicted_mfs_risk_7yr, predicted_mfs_risk_10yr, predicted_mss_risk_5yr, predicted_mss_risk_7yr, predicted_mss_risk_10yr, event_type_mfs_5yr, event_type_mfs_7yr, event_type_mfs_10yr, event_type_mss_5yr, event_type_mss_7yr, event_type_mss_10yr, tt_mfs_5yr, tt_mfs_7yr, tt_mfs_10yr, tt_mss_5yr, tt_mss_7yr, tt_mss_10yr, mfs_analysis_eligible, mss_analysis_eligible",
      pattern = ", ",
      simplify = TRUE
  )[1, ]
  expect_true(all(new_variables %in% colnames(derived_data)))

  logger::log_info("Preparing factor levels")
  factored_result <- prepare_factor_levels(derived_data)
  factored_data <- factored_result$data
  expect_false("other_map" %in% names(factored_result))

  expect_true(identical(levels(factored_data$initial_t_stage_simple), c("T1", "T2", "T3", "T4")))
  expect_true(identical(levels(factored_data$initial_overall_stage), c("1", "2A", "2B", "3A", "3B", "3C", "4")))
  expect_true(identical(levels(factored_data$initial_stage_binary), c("Stage I-III", "Stage IV")))
  expect_true(identical(levels(factored_data$sex), c("Female", "Male")))
  expect_true(identical(levels(factored_data$location), c("Choroidal", "Ciliary Body", "Cilio-Choroidal", "Conjunctival", "Irido-Ciliary", "Iris")))
  expect_true(identical(levels(factored_data$internal_reflectivity), c("Very Low", "Low", "Low-Medium", "Medium", "Medium-High", "High", "Unknown")))
  expect_true(identical(levels(factored_data$srf), c("No", "Yes")))
  expect_false("Other" %in% levels(factored_data$biopsy1_gep_raw))
  expect_true(identical(levels(factored_data$gep_class_simple), c("Class 1", "Class 2", "GEP Failed/Indeterminate", "GEP Not Tested")))
  expect_false(identical(levels(factored_data$initial_tumor_height_binned), c("< 10 mm", "≥ 10 mm")))
  expect_true(identical(levels(factored_data$initial_tumor_height_binned), c("≤ 3 mm", "3.1-6 mm", "6.1-9 mm", "9.1-12 mm", "12.1-15 mm", "> 15 mm")))

  logger::log_info("Applying inclusion/exclusion criteria")
  factored_filtered_data <- apply_criteria(factored_data)
  cohorts <- factored_filtered_data$cohorts
  expect_false(any(as.character(cohorts$uveal_melanoma_full_cohort$consort_group) == "other"))
  expect_false(247 %in% cohorts$uveal_melanoma_restricted_cohort$id)
  expect_false(247 %in% cohorts$uveal_melanoma_gksrs_only_cohort$id)
  expect_true(247 %in% cohorts$uveal_melanoma_full_cohort$id)

      
    
})
