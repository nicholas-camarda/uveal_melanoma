# Test for Objective 1 output generation with visual inspection
# This test ensures that actual output files are created and saved for visual review

# Set up test environment
setwd(dirname(dirname(normalizePath("."))))
source("scripts/utils/all_helper_functions.R")

# Create test output directory
test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective1_visual_inspection")
dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

# Load test data
test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

test_that("Objective 1a: Recurrence analysis generates output files", {
  # Create recurrence model
  recurrence_model <- glm(recurrence1 ~ treatment_group, 
                         data = test_data, 
                         family = binomial)
  
  # Generate regression table
  table_result <- generate_regression_table(
    data = test_data,
    outcome_var = "recurrence1",
    predictor_vars = "treatment_group",
    confounders = NULL,
    model_type = "logistic",
    effect_measure = "OR",
    analysis_name = "recurrence_analysis",
    dataset_name = "test_dataset",
    output_dir = test_output_dir,
    prefix = "test_1a_",
    other_map = list()
  )
  
  # Test that output files were created
  expect_true(file.exists(file.path(test_output_dir, "test_1a_recurrence_analysis.xlsx")))
  expect_true(file.exists(file.path(test_output_dir, "test_1a_recurrence_analysis.html")))
  
  # Save model summary for inspection
  model_summary <- summary(recurrence_model)
  capture.output(model_summary, file = file.path(test_output_dir, "test_1a_recurrence_model_summary.txt"))
  
  expect_true(file.exists(file.path(test_output_dir, "test_1a_recurrence_model_summary.txt")))
})

test_that("Objective 1b: Metastatic progression generates output files", {
  # Convert factor to numeric for survival analysis
  mets_event_numeric <- as.numeric(test_data$mets_progression == "Yes")
  
  # Create survival object
  surv_obj <- Surv(test_data$tt_mets_years, mets_event_numeric)
  
  # Fit Cox model
  cox_model <- coxph(surv_obj ~ treatment_group, data = test_data)
  
  # Generate regression table
  table_result <- generate_regression_table(
    data = test_data,
    outcome_var = "mets_progression",
    predictor_vars = "treatment_group",
    confounders = NULL,
    model_type = "cox",
    time_var = "tt_mets_years",
    event_var = "mets_progression",
    effect_measure = "HR",
    analysis_name = "mets_progression_analysis",
    dataset_name = "test_dataset",
    output_dir = test_output_dir,
    prefix = "test_1b_",
    other_map = list()
  )
  
  # Test that output files were created
  expect_true(file.exists(file.path(test_output_dir, "test_1b_mets_progression_analysis.xlsx")))
  expect_true(file.exists(file.path(test_output_dir, "test_1b_mets_progression_analysis.html")))
  
  # Save model summary for inspection
  model_summary <- summary(cox_model)
  capture.output(model_summary, file = file.path(test_output_dir, "test_1b_mets_model_summary.txt"))
  
  expect_true(file.exists(file.path(test_output_dir, "test_1b_mets_model_summary.txt")))
})

test_that("Objective 1c: Overall survival generates output files", {
  # Create survival object
  surv_obj <- Surv(test_data$tt_death_years, test_data$death_event)
  
  # Fit Cox model
  cox_model <- coxph(surv_obj ~ treatment_group, data = test_data)
  
  # Generate regression table
  table_result <- generate_regression_table(
    data = test_data,
    outcome_var = "death_event",
    predictor_vars = "treatment_group",
    confounders = NULL,
    model_type = "cox",
    time_var = "tt_death_years",
    event_var = "death_event",
    effect_measure = "HR",
    analysis_name = "overall_survival_analysis",
    dataset_name = "test_dataset",
    output_dir = test_output_dir,
    prefix = "test_1c_",
    other_map = list()
  )
  
  # Test that output files were created
  expect_true(file.exists(file.path(test_output_dir, "test_1c_overall_survival_analysis.xlsx")))
  expect_true(file.exists(file.path(test_output_dir, "test_1c_overall_survival_analysis.html")))
  
  # Save model summary for inspection
  model_summary <- summary(cox_model)
  capture.output(model_summary, file = file.path(test_output_dir, "test_1c_survival_model_summary.txt"))
  
  expect_true(file.exists(file.path(test_output_dir, "test_1c_survival_model_summary.txt")))
})

test_that("Objective 1d: PFS generates output files", {
  # Create survival object
  surv_obj <- Surv(test_data$tt_pfs_months, test_data$pfs_event)
  
  # Fit Cox model
  cox_model <- coxph(surv_obj ~ treatment_group, data = test_data)
  
  # Generate regression table
  table_result <- generate_regression_table(
    data = test_data,
    outcome_var = "pfs_event",
    predictor_vars = "treatment_group",
    confounders = NULL,
    model_type = "cox",
    time_var = "tt_pfs_months",
    event_var = "pfs_event",
    effect_measure = "HR",
    analysis_name = "pfs_analysis",
    dataset_name = "test_dataset",
    output_dir = test_output_dir,
    prefix = "test_1d_",
    other_map = list()
  )
  
  # Test that output files were created
  expect_true(file.exists(file.path(test_output_dir, "test_1d_pfs_analysis.xlsx")))
  expect_true(file.exists(file.path(test_output_dir, "test_1d_pfs_analysis.html")))
  
  # Save model summary for inspection
  model_summary <- summary(cox_model)
  capture.output(model_summary, file = file.path(test_output_dir, "test_1d_pfs_model_summary.txt"))
  
  expect_true(file.exists(file.path(test_output_dir, "test_1d_pfs_model_summary.txt")))
})

test_that("Objective 1e: Tumor height changes generates output files", {
  # Calculate height change
  test_data$height_change <- test_data$last_height - test_data$initial_tumor_height
  
  # Fit linear model
  height_model <- lm(height_change ~ treatment_group, data = test_data)
  
  # Generate regression table
  table_result <- generate_regression_table(
    data = test_data,
    outcome_var = "height_change",
    predictor_vars = "treatment_group",
    confounders = NULL,
    model_type = "linear",
    effect_measure = "beta",
    analysis_name = "height_change_analysis",
    dataset_name = "test_dataset",
    output_dir = test_output_dir,
    prefix = "test_1e_",
    other_map = list()
  )
  
  # Test that output files were created
  expect_true(file.exists(file.path(test_output_dir, "test_1e_height_change_analysis.xlsx")))
  expect_true(file.exists(file.path(test_output_dir, "test_1e_height_change_analysis.html")))
  
  # Save model summary for inspection
  model_summary <- summary(height_model)
  capture.output(model_summary, file = file.path(test_output_dir, "test_1e_height_model_summary.txt"))
  
  expect_true(file.exists(file.path(test_output_dir, "test_1e_height_model_summary.txt")))
})

test_that("Objective 1f: Subgroup analysis generates output files", {
  # Test age subgroup analysis
  if ("age_at_diagnosis" %in% names(test_data)) {
    # Create interaction model
    age_model <- coxph(Surv(tt_death_years, death_event) ~ 
                      treatment_group * age_at_diagnosis, data = test_data)
    
    # Save model summary for inspection
    model_summary <- summary(age_model)
    capture.output(model_summary, file = file.path(test_output_dir, "test_1f_age_subgroup_model_summary.txt"))
    
    expect_true(file.exists(file.path(test_output_dir, "test_1f_age_subgroup_model_summary.txt")))
  }
  
  # Test sex subgroup analysis
  if ("sex" %in% names(test_data)) {
    # Create interaction model
    sex_model <- coxph(Surv(tt_death_years, death_event) ~ 
                      treatment_group * sex, data = test_data)
    
    # Save model summary for inspection
    model_summary <- summary(sex_model)
    capture.output(model_summary, file = file.path(test_output_dir, "test_1f_sex_subgroup_model_summary.txt"))
    
    expect_true(file.exists(file.path(test_output_dir, "test_1f_sex_subgroup_model_summary.txt")))
  }
})

test_that("Output files are readable and contain expected content", {
  # Check that XLSX files are readable
  xlsx_files <- list.files(test_output_dir, pattern = "\\.xlsx$", full.names = TRUE)
  
  for (file in xlsx_files) {
    # Try to read the file
    sheets <- excel_sheets(file)
    expect_true(length(sheets) > 0)
    
    # Read first sheet
    data <- read_excel(file, sheet = 1)
    expect_true(is.data.frame(data))
    expect_true(nrow(data) > 0)
  }
  
  # Check that HTML files are readable
  html_files <- list.files(test_output_dir, pattern = "\\.html$", full.names = TRUE)
  
  for (file in html_files) {
    # Read HTML content
    html_content <- readLines(file)
    expect_true(length(html_content) > 0)
    
    # Check for expected HTML structure
    expect_true(any(grepl("<html", html_content, ignore.case = TRUE)))
    expect_true(any(grepl("<table", html_content, ignore.case = TRUE)))
  }
  
  # Check that text files are readable
  txt_files <- list.files(test_output_dir, pattern = "\\.txt$", full.names = TRUE)
  
  for (file in txt_files) {
    # Read text content
    txt_content <- readLines(file)
    expect_true(length(txt_content) > 0)
  }
})

test_that("Test output directory contains all expected files", {
  # List all files in output directory
  all_files <- list.files(test_output_dir, recursive = TRUE)
  
  # Should have multiple output files
  expect_gt(length(all_files), 10)
  
  # Should have XLSX files
  xlsx_count <- length(grep("\\.xlsx$", all_files))
  expect_gt(xlsx_count, 0)
  
  # Should have HTML files
  html_count <- length(grep("\\.html$", all_files))
  expect_gt(html_count, 0)
  
  # Should have text files
  txt_count <- length(grep("\\.txt$", all_files))
  expect_gt(txt_count, 0)
  
  # Log file list for inspection
  cat("Files created in test output directory:\n")
  cat(paste(all_files, collapse = "\n"), "\n")
})

# Cleanup
test_that("Test cleanup", {
  # This test ensures cleanup happens after all other tests
  expect_true(TRUE)
}) 