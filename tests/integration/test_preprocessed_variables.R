skip_if_integration_disabled()
skip_if_local_data_unavailable()

# Test file for pre-processed variables functionality
# This ensures that Task 2.8 changes work correctly and don't break the pipeline

# Load the project environment

test_that("Pre-processed variables are created correctly in data processing", {
  # Load test data
  data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
  
  # Create derived variables (this is what we're testing)
  data_derived <- create_derived_variables(data)
  
  # Test that all expected pre-processed variables exist
  expected_vars <- c(
    # Time-specific event indicators
    "mfs_event_5yr", "mfs_event_7yr", "mfs_event_10yr",
    "mss_event_5yr", "mss_event_7yr", "mss_event_10yr",
    
    # Pre-calculated risk variables
    "predicted_mfs_risk_5yr", "predicted_mfs_risk_7yr", "predicted_mfs_risk_10yr",
    "predicted_mss_risk_5yr", "predicted_mss_risk_7yr", "predicted_mss_risk_10yr",
    
    # Competing risk classifications
    "event_type_mfs_5yr", "event_type_mfs_7yr", "event_type_mfs_10yr",
    "event_type_mss_5yr", "event_type_mss_7yr", "event_type_mss_10yr",
    
    # Time-to-event variables
    "tt_mfs_5yr", "tt_mfs_7yr", "tt_mfs_10yr",
    "tt_mss_5yr", "tt_mss_7yr", "tt_mss_10yr",
    
    # Analysis eligibility flags
    "mfs_analysis_eligible", "mss_analysis_eligible"
  )
  
  # Check that all expected variables exist
  missing_vars <- setdiff(expected_vars, names(data_derived))
  expect_equal(length(missing_vars), 0, 
               info = paste("Missing pre-processed variables:", paste(missing_vars, collapse = ", ")))
  
  # Test that data dimensions are preserved
  expect_equal(nrow(data_derived), nrow(data), 
               info = "Data processing should not change number of rows")
  
  # Test that original variables are preserved
  original_vars <- setdiff(names(data), expected_vars)
  for (var in original_vars) {
    expect_true(var %in% names(data_derived), 
                info = paste("Original variable", var, "should be preserved"))
  }
})

test_that("Analysis eligibility filters work correctly", {
  # Load and process data
  data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
  data_derived <- create_derived_variables(data)
  
  # Test MFS analysis eligibility
  mfs_eligible <- data_derived %>% filter(mfs_analysis_eligible)
  expect_true(nrow(mfs_eligible) > 0, 
              info = "Should have some MFS eligible patients")
  expect_true(nrow(mfs_eligible) <= nrow(data_derived), 
              info = "MFS eligible should be subset of total data")
  
  # Test MSS analysis eligibility
  mss_eligible <- data_derived %>% filter(mss_analysis_eligible)
  expect_true(nrow(mss_eligible) > 0, 
              info = "Should have some MSS eligible patients")
  expect_true(nrow(mss_eligible) <= nrow(data_derived), 
              info = "MSS eligible should be subset of total data")
  
  # Test that eligibility is logical
  expect_true(all(mfs_eligible$mfs_analysis_eligible == TRUE), 
              info = "All MFS eligible patients should have mfs_analysis_eligible = TRUE")
  expect_true(all(mss_eligible$mss_analysis_eligible == TRUE), 
              info = "All MSS eligible patients should have mss_analysis_eligible = TRUE")
})

test_that("Time-specific event indicators are calculated correctly", {
  # Load and process data
  data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
  data_derived <- create_derived_variables(data)
  
  # Test that event indicators are binary (0 or 1)
  event_vars <- c("mfs_event_5yr", "mfs_event_7yr", "mfs_event_10yr",
                  "mss_event_5yr", "mss_event_7yr", "mss_event_10yr")
  
  for (var in event_vars) {
    values <- data_derived[[var]]
    expect_true(all(values %in% c(0, 1, NA)), 
                info = paste("Event indicator", var, "should be binary (0, 1, or NA)"))
  }
  
  # Test that 5yr events <= 7yr events <= 10yr events (monotonicity)
  mfs_eligible <- data_derived %>% filter(mfs_analysis_eligible)
  if (nrow(mfs_eligible) > 0) {
    expect_true(all(mfs_eligible$mfs_event_5yr <= mfs_eligible$mfs_event_7yr, na.rm = TRUE),
                info = "MFS 5yr events should be <= 7yr events")
    expect_true(all(mfs_eligible$mfs_event_7yr <= mfs_eligible$mfs_event_10yr, na.rm = TRUE),
                info = "MFS 7yr events should be <= 10yr events")
  }
  
  mss_eligible <- data_derived %>% filter(mss_analysis_eligible)
  if (nrow(mss_eligible) > 0) {
    expect_true(all(mss_eligible$mss_event_5yr <= mss_eligible$mss_event_7yr, na.rm = TRUE),
                info = "MSS 5yr events should be <= 7yr events")
    expect_true(all(mss_eligible$mss_event_7yr <= mss_eligible$mss_event_10yr, na.rm = TRUE),
                info = "MSS 7yr events should be <= 10yr events")
  }
})

test_that("Pre-calculated risk variables are valid", {
  # Load and process data
  data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
  data_derived <- create_derived_variables(data)
  
  # Test that risk variables are between 0 and 1
  risk_vars <- c("predicted_mfs_risk_5yr", "predicted_mfs_risk_7yr", "predicted_mfs_risk_10yr",
                 "predicted_mss_risk_5yr", "predicted_mss_risk_7yr", "predicted_mss_risk_10yr")
  
  for (var in risk_vars) {
    values <- data_derived[[var]]
    expect_true(all(values >= 0 & values <= 1, na.rm = TRUE), 
                info = paste("Risk variable", var, "should be between 0 and 1"))
  }
  
  # Test that risk = 1 - survival probability
  mfs_eligible <- data_derived %>% filter(mfs_analysis_eligible)
  if (nrow(mfs_eligible) > 0) {
    expect_equal(mfs_eligible$predicted_mfs_risk_5yr, 1 - mfs_eligible$expected_mfs_5yr,
                 tolerance = 1e-10, 
                 info = "MFS risk should equal 1 - survival probability")
  }
  
  mss_eligible <- data_derived %>% filter(mss_analysis_eligible)
  if (nrow(mss_eligible) > 0) {
    expect_equal(mss_eligible$predicted_mss_risk_5yr, 1 - mss_eligible$expected_mss_5yr,
                 tolerance = 1e-10, 
                 info = "MSS risk should equal 1 - survival probability")
  }
})

test_that("Competing risk variables are valid", {
  # Load and process data
  data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
  data_derived <- create_derived_variables(data)
  
  # Test that event type variables are valid (0, 1, 2, or NA)
  event_type_vars <- c("event_type_mfs_5yr", "event_type_mfs_7yr", "event_type_mfs_10yr",
                       "event_type_mss_5yr", "event_type_mss_7yr", "event_type_mss_10yr")
  
  for (var in event_type_vars) {
    values <- data_derived[[var]]
    expect_true(all(values %in% c(0, 1, 2, NA)), 
                info = paste("Event type variable", var, "should be 0, 1, 2, or NA"))
  }
  
  # Test that time-to-event variables are non-negative
  time_vars <- c("tt_mfs_5yr", "tt_mfs_7yr", "tt_mfs_10yr",
                 "tt_mss_5yr", "tt_mss_7yr", "tt_mss_10yr")
  
  for (var in time_vars) {
    values <- data_derived[[var]]
    expect_true(all(values >= 0, na.rm = TRUE), 
                info = paste("Time variable", var, "should be non-negative"))
  }
})

test_that("GEP analysis functions can use pre-processed variables", {
  # Load and process data
  data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
  data_derived <- create_derived_variables(data)
  
  # Test that MFS analysis functions can access pre-processed variables
  mfs_eligible <- data_derived %>% filter(mfs_analysis_eligible)
  if (nrow(mfs_eligible) > 0) {
    # Test that we can access the pre-processed variables
    expect_true(all(c("mfs_event_5yr", "predicted_mfs_risk_5yr", "tt_mfs_5yr") %in% names(mfs_eligible)),
                info = "MFS analysis should have access to pre-processed variables")
    
    # Test that the variables have expected values
    expect_true(sum(mfs_eligible$mfs_event_5yr, na.rm = TRUE) >= 0,
                info = "MFS 5yr events should be non-negative")
    expect_true(mean(mfs_eligible$predicted_mfs_risk_5yr, na.rm = TRUE) >= 0,
                info = "MFS 5yr risk should be non-negative")
  }
  
  # Test that MSS analysis functions can access pre-processed variables
  mss_eligible <- data_derived %>% filter(mss_analysis_eligible)
  if (nrow(mss_eligible) > 0) {
    # Test that we can access the pre-processed variables
    expect_true(all(c("mss_event_5yr", "predicted_mss_risk_5yr", "tt_mss_5yr") %in% names(mss_eligible)),
                info = "MSS analysis should have access to pre-processed variables")
    
    # Test that the variables have expected values
    expect_true(sum(mss_eligible$mss_event_5yr, na.rm = TRUE) >= 0,
                info = "MSS 5yr events should be non-negative")
    expect_true(mean(mss_eligible$predicted_mss_risk_5yr, na.rm = TRUE) >= 0,
                info = "MSS 5yr risk should be non-negative")
  }
})
