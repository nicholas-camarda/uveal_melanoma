# Test that Uno's C and AUC are displayed as numeric values (not NA)

# Follow existing test setup conventions

# Ensure environment and functions are loaded

test_that("Comprehensive GEP summary displays Uno's C and AUC as numbers", {
  # Mock validation results with known discrimination metrics
  mock_validation_results <- list(
    "5yr" = list(
      calibration = list(
        n = 100,
        nam_dagostino_p = 0.05,
        ici = 0.10,
        slope = 0.95
      ),
      discrimination = list(
        n = 100,
        events = 25,
        harrell_c = 0.75,
        uno_c = 0.73,
        auc_timepoint = 0.78
      )
    )
  )

  # Generate summary text
  summary_text <- create_comprehensive_gep_summary(
    validation_results = mock_validation_results,
    outcome_type = "MFS",
    prame_analysis = NULL,
    missing_data_analysis = NULL,
    dataset_name = "unit_test_dataset"
  )

  # Expect properly formatted numeric values in the detailed metrics section
  expect_true(grepl("Uno's C=0.730", summary_text),
              info = "Uno's C should be displayed as a numeric value with 3 decimals")
  expect_true(grepl("AUC=0.780", summary_text),
              info = "AUC should be displayed as a numeric value with 3 decimals")
  expect_true(grepl("Harrell's C=0.750", summary_text),
              info = "Harrell's C should be displayed as a numeric value with 3 decimals")
})

