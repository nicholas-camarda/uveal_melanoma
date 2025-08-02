# Objective 1 Efficacy Analysis Testing Suite
# Tests for components 1a-f: recurrence, progression, survival, PFS, tumor height, subgroups

# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

# Load test data
test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

# Create test output directory
test_output_dir <- "test_output/objective_1_testing"
dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

test_that("Objective 1a: Local recurrence rates analysis", {
  # Test data requirements
  expect_true("recurrence1" %in% names(test_data))
  expect_true("treatment_group" %in% names(test_data))
  expect_true(all(test_data$recurrence1 %in% c("Y", "N", NA)))
  
  # Test that we can create a basic recurrence model
  recurrence_model <- glm(recurrence1 ~ treatment_group, 
                         data = test_data, 
                         family = binomial)
  
  expect_s3_class(recurrence_model, "glm")
  expect_true("treatment_group" %in% names(coef(recurrence_model)))
  
  # Test that model produces reasonable coefficients
  coefs <- coef(recurrence_model)
  expect_true(is.numeric(coefs))
  expect_true(length(coefs) > 0)
  
  # Test that we can extract odds ratios
  or_ci <- exp(confint(recurrence_model))
  expect_true(is.matrix(or_ci))
  expect_equal(nrow(or_ci), length(coefs))
})

test_that("Objective 1b: Metastatic progression analysis", {
  # Test data requirements
  expect_true("mets_progression" %in% names(test_data))
  expect_true("mets_progression_time" %in% names(test_data))
  expect_true("treatment_group" %in% names(test_data))
  
  # Test that we can create a survival object
  surv_obj <- Surv(test_data$mets_progression_time, test_data$mets_progression)
  expect_s3_class(surv_obj, "Surv")
  
  # Test that we can fit a Cox model
  cox_model <- coxph(surv_obj ~ treatment_group, data = test_data)
  expect_s3_class(cox_model, "coxph")
  
  # Test that model produces reasonable results
  coefs <- coef(cox_model)
  expect_true(is.numeric(coefs))
  expect_true(length(coefs) > 0)
  
  # Test that we can extract hazard ratios
  hr_ci <- exp(confint(cox_model))
  expect_true(is.matrix(hr_ci))
})

test_that("Objective 1c: Overall survival analysis", {
  # Test data requirements
  expect_true("overall_survival_time" %in% names(test_data))
  expect_true("overall_survival_status" %in% names(test_data))
  expect_true("treatment_group" %in% names(test_data))
  
  # Test Kaplan-Meier analysis
  km_fit <- survfit(Surv(overall_survival_time, overall_survival_status) ~ treatment_group, 
                   data = test_data)
  expect_s3_class(km_fit, "survfit")
  
  # Test that we get survival curves for both treatment groups
  expect_true(length(km_fit$strata) >= 1)
  
  # Test Cox regression
  cox_model <- coxph(Surv(overall_survival_time, overall_survival_status) ~ treatment_group, 
                    data = test_data)
  expect_s3_class(cox_model, "coxph")
  
  # Test RMST analysis (if survRM2 is available)
  if (requireNamespace("survRM2", quietly = TRUE)) {
    rmst_result <- survRM2::rmst2(time = test_data$overall_survival_time,
                                 status = test_data$overall_survival_status,
                                 arm = test_data$treatment_group)
    expect_s3_class(rmst_result, "rmst2")
  }
})

test_that("Objective 1d: Progression-free survival analysis", {
  # Test data requirements
  expect_true("pfs_time" %in% names(test_data))
  expect_true("pfs_status" %in% names(test_data))
  expect_true("treatment_group" %in% names(test_data))
  
  # Test PFS survival analysis
  pfs_fit <- survfit(Surv(pfs_time, pfs_status) ~ treatment_group, 
                    data = test_data)
  expect_s3_class(pfs_fit, "survfit")
  
  # Test PFS Cox regression
  pfs_cox <- coxph(Surv(pfs_time, pfs_status) ~ treatment_group, 
                  data = test_data)
  expect_s3_class(pfs_cox, "coxph")
  
  # Test that PFS times are reasonable
  expect_true(all(test_data$pfs_time >= 0, na.rm = TRUE))
  expect_true(all(test_data$pfs_status %in% c(0, 1), na.rm = TRUE))
})

test_that("Objective 1e: Tumor height changes analysis", {
  # Test data requirements
  expect_true("initial_tumor_height" %in% names(test_data))
  expect_true("last_height" %in% names(test_data))
  expect_true("treatment_group" %in% names(test_data))
  
  # Test that we can calculate height changes
  test_data$height_change <- test_data$last_height - test_data$initial_tumor_height
  
  # Test that height changes are numeric
  expect_true(is.numeric(test_data$height_change))
  
  # Test linear regression for height changes
  height_model <- lm(height_change ~ treatment_group, data = test_data)
  expect_s3_class(height_model, "lm")
  
  # Test that model produces reasonable results
  coefs <- coef(height_model)
  expect_true(is.numeric(coefs))
  expect_true(length(coefs) > 0)
  
  # Test that we can get confidence intervals
  ci <- confint(height_model)
  expect_true(is.matrix(ci))
})

test_that("Objective 1f: Subgroup analysis framework", {
  # Test that subgroup variables exist
  subgroup_vars <- c("age_at_diagnosis", "sex", "location", "initial_t_stage", 
                    "initial_tumor_height", "initial_tumor_diameter", 
                    "biopsy1_gep", "optic_nerve")
  
  for (var in subgroup_vars) {
    if (var %in% names(test_data)) {
      expect_true(length(unique(test_data[[var]])) > 1)
    }
  }
  
  # Test that we can create interaction models
  if ("age_at_diagnosis" %in% names(test_data)) {
    # Test age subgroup analysis
    age_model <- coxph(Surv(overall_survival_time, overall_survival_status) ~ 
                      treatment_group * age_at_diagnosis, data = test_data)
    expect_s3_class(age_model, "coxph")
    
    # Test that interaction term exists
    interaction_term <- "treatment_group:age_at_diagnosis"
    expect_true(interaction_term %in% names(coef(age_model)))
  }
  
  if ("sex" %in% names(test_data)) {
    # Test sex subgroup analysis
    sex_model <- coxph(Surv(overall_survival_time, overall_survival_status) ~ 
                      treatment_group * sex, data = test_data)
    expect_s3_class(sex_model, "coxph")
  }
})

test_that("Objective 1: Output generation", {
  # Test that we can create output directories
  output_dirs <- list(
    "recurrence" = file.path(test_output_dir, "1a_recurrence"),
    "progression" = file.path(test_output_dir, "1b_progression"),
    "survival" = file.path(test_output_dir, "1c_survival"),
    "pfs" = file.path(test_output_dir, "1d_pfs"),
    "height" = file.path(test_output_dir, "1e_height"),
    "subgroups" = file.path(test_output_dir, "1f_subgroups")
  )
  
  for (dir_name in output_dirs) {
    dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
    expect_true(dir.exists(dir_name))
  }
  
  # Test that we can save test results
  test_results <- list(
    test_data = test_data,
    timestamp = Sys.time(),
    test_version = "1.0"
  )
  
  saveRDS(test_results, file.path(test_output_dir, "test_results.rds"))
  expect_true(file.exists(file.path(test_output_dir, "test_results.rds")))
})

test_that("Objective 1: Data quality checks", {
  # Test that required variables have appropriate data types
  expect_true(is.factor(test_data$treatment_group))
  expect_true(is.factor(test_data$recurrence1))
  
  # Test that survival times are positive
  expect_true(all(test_data$overall_survival_time > 0, na.rm = TRUE))
  expect_true(all(test_data$pfs_time > 0, na.rm = TRUE))
  
  # Test that status variables are binary
  expect_true(all(test_data$overall_survival_status %in% c(0, 1), na.rm = TRUE))
  expect_true(all(test_data$pfs_status %in% c(0, 1), na.rm = TRUE))
  
  # Test that we have sufficient data for analysis
  expect_gt(nrow(test_data), 50)  # Minimum sample size
  expect_gt(sum(!is.na(test_data$treatment_group)), 0)
})

# Cleanup
test_that("Test cleanup", {
  # This test ensures cleanup happens after all other tests
  expect_true(TRUE)
}) 