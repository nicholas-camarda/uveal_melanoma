# Debug Regression Table Structure
# This script examines the actual structure to fix the indentation logic

# Load required libraries
library(tidyverse)
library(gt)
library(gtsummary)

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/utils/output_utilities.R")

cat("=== DEBUGGING REGRESSION TABLE STRUCTURE ===\n\n")

# Create sample data
set.seed(123)
n <- 200
test_data <- data.frame(
    recurrence1 = sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2)),
    treatment_group = factor(sample(c("Plaque", "GKSRS"), n, replace = TRUE)),
    age_at_diagnosis = round(rnorm(n, 65, 10), 1),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    location = factor(sample(c("Choroidal", "Cilio-Choroidal", "Other"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2))),
    optic_nerve = factor(sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.3, 0.7)))
)

# Fit regression model
logit_model <- glm(
    recurrence1 ~ treatment_group + age_at_diagnosis + sex + location + optic_nerve,
    family = binomial(link = "logit"),
    data = test_data
)

# Create regression table
regression_table <- tbl_regression(
    logit_model,
    intercept = FALSE,
    exponentiate = TRUE,
    show_single_row = "treatment_group",
    quiet = TRUE,
    label = list(
        treatment_group ~ "Treatment Group",
        age_at_diagnosis ~ "Age at Diagnosis (years)",
        sex ~ "Sex",
        location ~ "Tumor Location", 
        optic_nerve ~ "Optic Nerve Involvement"
    )
) %>%
    modify_header(
        label = "Variable",  # Remove ** markdown
        estimate = "OR",
        p.value = "p-value"
    )

cat("1. EXAMINING TABLE STRUCTURE:\n")

# Convert to tibble to see the actual data
table_data <- regression_table %>% as_tibble()

cat("Column names:", paste(names(table_data), collapse = ", "), "\n")
cat("Number of rows:", nrow(table_data), "\n")
cat("First column content:\n")
for (i in 1:nrow(table_data)) {
    cat(sprintf("Row %d: '%s'\n", i, table_data[[1]][i]))
}

cat("\n2. TESTING DETECTION LOGIC:\n")

# Test our detection function
table_type <- detect_gtsummary_table_type(table_data)
cat("Detected table type:", table_type, "\n")

# Test our main variable detection
if (table_type == "tbl_regression") {
    cat("\nTesting main variable detection:\n")
    for (i in 1:nrow(table_data)) {
        var_name <- trimws(table_data[[1]][i])
        is_main <- detect_regression_main_variable(var_name, table_data, i)
        cat(sprintf("Row %d: '%s' -> %s\n", i, var_name, ifelse(is_main, "MAIN VARIABLE", "factor level")))
    }
}

cat("\n3. WHAT THE DATA ACTUALLY LOOKS like:\n")
print(table_data)

cat("\n=== END DEBUG ===\n") 