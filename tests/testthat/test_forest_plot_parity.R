# Forest Plot Parity Tests
# Tests that functions migrated from forest_plot.R produce identical outputs

test_that("create_forest_plot_data parity", {
  # Create test subgroup results
  test_subgroup_results <- list(
    age_at_diagnosis = list(
      interaction_p = 0.045,
      subgroup_effects = data.frame(
        subgroup_level = c("≤65", ">65"),
        n_total = c(50, 30),
        n_plaque = c(25, 15),
        n_gksrs = c(25, 15),
        treatment_effect = c(0.85, 1.2),
        ci_lower = c(0.6, 0.8),
        ci_upper = c(1.2, 1.8),
        p_value = c(0.12, 0.08),
        stringsAsFactors = FALSE
      )
    ),
    sex = list(
      interaction_p = 0.23,
      subgroup_effects = data.frame(
        subgroup_level = c("Male", "Female"),
        n_total = c(45, 35),
        n_plaque = c(22, 18),
        n_gksrs = c(23, 17),
        treatment_effect = c(0.9, 1.1),
        ci_lower = c(0.7, 0.8),
        ci_upper = c(1.1, 1.5),
        p_value = c(0.15, 0.09),
        stringsAsFactors = FALSE
      )
    )
  )
  
  variable_order <- c("age_at_diagnosis", "sex")
  treatment_labels <- c("GKSRS", "Plaque")
  effect_measure <- "HR"
  
  # Call both original and new versions
  result_orig <- quiet_eval(fp_orig_env$create_forest_plot_data(
    subgroup_results = test_subgroup_results,
    variable_order = variable_order,
    treatment_labels = treatment_labels,
    effect_measure = effect_measure
  ))
  
  result_new <- quiet_eval(fp_new_env$create_forest_plot_data(
    subgroup_results = test_subgroup_results,
    variable_order = variable_order,
    treatment_labels = treatment_labels,
    effect_measure = effect_measure
  ))
  
  # Compare results
  expect_equal(result_orig$data_frame, result_new$data_frame)
  expect_equal(result_orig$est_values, result_new$est_values)
  expect_equal(result_orig$lower_values, result_new$lower_values)
  expect_equal(result_orig$upper_values, result_new$upper_values)
  expect_equal(result_orig$is_summary, result_new$is_summary)
  expect_equal(result_orig$font_face, result_new$font_face)
  expect_equal(result_orig$text_size, result_new$text_size)
})

test_that("format_variable_name parity", {
  test_vars <- c("age_at_diagnosis", "sex", "location", "unknown_var")
  
  for (var in test_vars) {
    result_orig <- quiet_eval(fp_orig_env$format_variable_name(var))
    result_new <- quiet_eval(fp_new_env$format_variable_name(var))
    expect_equal(result_orig, result_new)
  }
})

test_that("format_sample_size parity", {
  test_cases <- list(
    list(n_group = 25, n_total = 50),
    list(n_group = 30, n_total = NULL),
    list(n_group = NA, n_total = 100),
    list(n_group = NULL, n_total = 75)
  )
  
  for (case in test_cases) {
    result_orig <- quiet_eval(do.call(fp_orig_env$format_sample_size, case))
    result_new <- quiet_eval(do.call(fp_new_env$format_sample_size, case))
    expect_equal(result_orig, result_new)
  }
})

test_that("format_p_value parity", {
  test_p_values <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, NA, NULL)
  
  for (p_val in test_p_values) {
    result_orig <- quiet_eval(fp_orig_env$format_p_value(p_val))
    result_new <- quiet_eval(fp_new_env$format_p_value(p_val))
    expect_equal(result_orig, result_new)
  }
})

test_that("symmetric_log_clip parity", {
  test_lower <- c(0.5, 0.8, 1.2, 2.0)
  test_upper <- c(1.5, 1.2, 1.8, 3.0)
  
  result_orig <- quiet_eval(fp_orig_env$symmetric_log_clip(test_lower, test_upper))
  result_new <- quiet_eval(fp_new_env$symmetric_log_clip(test_lower, test_upper))
  
  expect_equal(result_orig, result_new)
})

test_that("symmetric_linear_clip parity", {
  test_lower <- c(-2.0, -1.5, 0.5, 1.0)
  test_upper <- c(2.0, 1.5, 1.5, 2.0)
  
  result_orig <- quiet_eval(fp_orig_env$symmetric_linear_clip(test_lower, test_upper))
  result_new <- quiet_eval(fp_new_env$symmetric_linear_clip(test_lower, test_upper))
  
  expect_equal(result_orig, result_new)
})

test_that("get_forest_plot_diagnostics parity", {
  # Create a mock forest plot object with diagnostics attribute
  mock_fp <- list()
  mock_diagnostics <- data.frame(
    variable = c("age_at_diagnosis", "sex"),
    subgroup_level = c("≤65", "Male"),
    treatment_effect = c(0.85, 0.9),
    stringsAsFactors = FALSE
  )
  attr(mock_fp, "diagnostics") <- mock_diagnostics
  
  result_orig <- quiet_eval(fp_orig_env$get_forest_plot_diagnostics(mock_fp))
  result_new <- quiet_eval(fp_new_env$get_forest_plot_diagnostics(mock_fp))
  
  expect_equal(result_orig, result_new)
})

test_that("write_diagnostics_excel parity", {
  # Create test diagnostics list
  test_diagnostics <- list(
    plot1 = data.frame(
      variable = c("age_at_diagnosis", "sex"),
      subgroup_level = c("≤65", "Male"),
      treatment_effect = c(0.85, 0.9),
      stringsAsFactors = FALSE
    ),
    plot2 = data.frame(
      variable = c("location"),
      subgroup_level = c("Posterior"),
      treatment_effect = c(1.1),
      stringsAsFactors = FALSE
    )
  )
  
  test_file <- file.path(getwd(), "test_output/test_diagnostics.xlsx")
  
  # Call both original and new versions
  result_orig <- quiet_eval(fp_orig_env$write_diagnostics_excel(test_diagnostics, test_file))
  result_new <- quiet_eval(fp_new_env$write_diagnostics_excel(test_diagnostics, test_file))
  
  # Compare results (both should return the same value, likely the file path)
  expect_equal(result_orig, result_new)
  
  # Clean up
  if (file.exists(test_file)) unlink(test_file)
  
  # Also test with empty diagnostics list
  empty_result_orig <- quiet_eval(fp_orig_env$write_diagnostics_excel(list(), test_file))
  empty_result_new <- quiet_eval(fp_new_env$write_diagnostics_excel(list(), test_file))
  
  expect_equal(empty_result_orig, empty_result_new)
})
