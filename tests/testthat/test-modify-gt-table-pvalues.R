# tests/testthat/test-modify-gt-table-pvalues.R
# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

# Source helper functions (loads all necessary functions and constants)
source("scripts/utils/all_helper_functions.R")

test_that("modify_gt_table_pvalues works for Cox models", {
  # Load real data
  data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
  
  # Test Cox model table modification
  surv_obj <- Surv(data$tt_death_years, data$death_event)
  cox_model <- coxph(surv_obj ~ treatment_group + age_at_diagnosis + sex, data = data, model = TRUE)
  table <- cox_model %>% tbl_regression(exponentiate = TRUE)
  
  cat("DEBUG: Before modify_gt_table_pvalues\n")
  cat("  Table class:", class(table), "\n")
  cat("  Table structure:", str(table), "\n")
  
  # Call with correct parameters: (gt_table, table_result, data, outcome_var, confounders, model_fit)
  modified_table <- modify_gt_table_pvalues(table %>% as_gt(), table, data, 'death_event', c('age_at_diagnosis', 'sex'), cox_model)
  
  cat("DEBUG: After modify_gt_table_pvalues\n")
  cat("  Modified table class:", class(modified_table), "\n")
  
  # Check that factor label p-values are applied
  table_data <- modified_table$table_body
  treatment_rows <- which(table_data$variable == 'treatment_group')
  
  expect_true(length(treatment_rows) > 0)
  
  # First row should have p-value, others should be NA
  expect_true(!is.na(table_data$p.value[treatment_rows[1]]))
  if (length(treatment_rows) > 1) {
    expect_true(all(is.na(table_data$p.value[treatment_rows[-1]])))
  }
}) 