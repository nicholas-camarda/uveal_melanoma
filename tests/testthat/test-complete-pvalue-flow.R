# tests/testthat/test-complete-pvalue-flow.R
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("Complete p-value flow works for Cox models", {
  # Load real data
  data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Test complete flow for Cox model
  surv_obj <- Surv(data$tt_death_years, data$death_event)
  cox_model <- coxph(surv_obj ~ treatment_group + age_at_diagnosis + sex, data = data, model = TRUE)
  
  # Step 1: Calculate factor label p-value
  pval <- calculate_factor_label_pvalue(cox_model, 'treatment_group', data, 'death_event', c('age_at_diagnosis', 'sex'))
  expect_true(!is.na(pval))
  expect_true(pval >= 0 && pval <= 1)
  expect_type(pval, "double")
  
  # Step 2: Create gtsummary table
  table <- cox_model %>% tbl_regression(exponentiate = TRUE)
  expect_s3_class(table, "tbl_regression")
  
  # Step 3: Modify table p-values
  modified_table <- modify_gt_table_pvalues(table %>% as_gt(), table, data, 'death_event', c('age_at_diagnosis', 'sex'), cox_model)
  
  # Step 4: Verify HTML output
  table_data <- modified_table$table_body
  treatment_rows <- which(table_data$variable == 'treatment_group')
  expect_true(!is.na(table_data$p.value[treatment_rows[1]]))
})

test_that("Factor label p-values are calculated for all variables", {
  # Load real data
  data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Test Cox model with multiple variables
  surv_obj <- Surv(data$tt_death_years, data$death_event)
  cox_model <- coxph(surv_obj ~ treatment_group + age_at_diagnosis + sex, data = data, model = TRUE)
  
  # Test each variable
  variables <- c('treatment_group', 'age_at_diagnosis', 'sex')
  confounders <- c('age_at_diagnosis', 'sex')
  
  for (var in variables) {
    pval <- calculate_factor_label_pvalue(cox_model, var, data, 'death_event', confounders[confounders != var])
    expect_true(!is.na(pval))
    expect_true(pval >= 0 && pval <= 1)
    expect_type(pval, "double")
  }
})

test_that("HTML table shows factor label p-values only", {
  # Load real data
  data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Create Cox model and table
  surv_obj <- Surv(data$tt_death_years, data$death_event)
  cox_model <- coxph(surv_obj ~ treatment_group + age_at_diagnosis + sex, data = data, model = TRUE)
  table <- cox_model %>% tbl_regression(exponentiate = TRUE)
  
  # Modify table p-values
  modified_table <- modify_gt_table_pvalues(table %>% as_gt(), table, data, 'death_event', c('age_at_diagnosis', 'sex'), cox_model)
  
  # Check that only factor label p-values are shown (first row of each variable)
  table_data <- modified_table$table_body
  
  # For each variable, check that only the first row has a p-value
  for (var in unique(table_data$variable)) {
    var_rows <- which(table_data$variable == var)
    if (length(var_rows) > 1) {
      # First row should have p-value
      expect_true(!is.na(table_data$p.value[var_rows[1]]))
      # Other rows should be NA
      expect_true(all(is.na(table_data$p.value[var_rows[-1]])))
    } else {
      # Single row should have p-value
      expect_true(!is.na(table_data$p.value[var_rows[1]]))
    }
  }
}) 