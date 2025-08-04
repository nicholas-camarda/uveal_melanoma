# Debug script for Cox model p-values
library(survival)
library(gtsummary)

# Load test data
test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

# Create survival object
surv_obj <- Surv(test_data$tt_death_years, test_data$death_event)
test_data$surv_obj <- surv_obj

# Fit Cox model with model=TRUE
cox_model <- coxph(surv_obj ~ treatment_group, data = test_data, model = TRUE)

# Check model summary
summary_result <- summary(cox_model)
cat("Model summary structure:\n")
print(str(summary_result))

cat("\nCoefficients:\n")
print(summary_result$coefficients)

cat("\nNumber of columns in coefficients:\n")
print(ncol(summary_result$coefficients))

cat("\nColumn names:\n")
print(colnames(summary_result$coefficients))

# Check if p-values are in column 5 (Pr(>|z|))
if (ncol(summary_result$coefficients) >= 5) {
    cat("\nP-values from column 5:\n")
    print(summary_result$coefficients[, 5])
} else {
    cat("\nNo column 5 found\n")
}

# Check if p-values are in column 4
if (ncol(summary_result$coefficients) >= 4) {
    cat("\nP-values from column 4:\n")
    print(summary_result$coefficients[, 4])
} else {
    cat("\nNo column 4 found\n")
}

# Create gtsummary table
table <- cox_model %>% tbl_regression(exponentiate = TRUE)

cat("\nTable body structure:\n")
print(str(table$table_body))

cat("\nP-values in table:\n")
print(table$table_body$p.value)

# Check if model frame is present
cat("\nModel frame present:", !is.null(cox_model$model), "\n") 