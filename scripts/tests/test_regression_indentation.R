# Test Regression Table Factor Indentation
# This test verifies that factor indentation works on tbl_regression tables

# Load required libraries
library(tidyverse)
library(gt)
library(gtsummary)

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/utils/output_utilities.R")

# Create test output directory
test_output_dir <- "test_output/regression_indentation_test"
if (!dir.exists(test_output_dir)) {
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
}

cat("=== REGRESSION TABLE FACTOR INDENTATION TEST ===\n")
cat("Test output directory:", test_output_dir, "\n\n")

cat("1. Creating realistic test data for regression...\n")

# Create sample data that matches your actual data structure
set.seed(123)
n <- 200
test_data <- data.frame(
    # Outcome variable
    recurrence1 = sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2)),
    
    # Predictor variables (match your actual variable names)
    treatment_group = factor(sample(c("Plaque", "GKSRS"), n, replace = TRUE)),
    age_at_diagnosis = round(rnorm(n, 65, 10), 1),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    location = factor(sample(c("Choroidal", "Cilio-Choroidal", "Other"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2))),
    optic_nerve = factor(sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.3, 0.7)))
)

cat("✓ Created realistic test dataset with", n, "patients\n")

cat("2. Creating logistic regression model...\n")

# Fit logistic regression model (similar to your analyze_binary_outcome_rates function)
logit_model <- glm(
    recurrence1 ~ treatment_group + age_at_diagnosis + sex + location + optic_nerve,
    family = binomial(link = "logit"),
    data = test_data
)

cat("✓ Logistic regression model fitted successfully\n")

cat("3. Creating tbl_regression table...\n")

# Create regression table using gtsummary (matches your actual code)
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
        label = "Variable",
        estimate = "OR",
        ci = "95% CI",  # Fix the CI column header
        p.value = "p-value"
    ) %>%
    modify_caption(
        "Adjusted Odds Ratios for recurrence1 by Treatment Group and Covariates"
    ) %>%
    modify_footnote(
        update = all_stat_cols() ~ "Reference level: Plaque"
    )

cat("✓ tbl_regression table created successfully\n")

cat("4. Testing factor indentation on regression table...\n")

# Test our new regression table indentation logic
# Apply indentation to the gtsummary object first, then add headers
styled_regression_table <- regression_table %>%
    apply_factor_level_indentation() %>%  # This converts to gt and applies indentation
    tab_header(
        title = "Table: Test Regression Results with Factor Indentation",
        subtitle = "Testing New Regression Table Logic"
    ) %>%
    # Add proper header styling
    tab_style(
        style = list(
            cell_text(weight = "bold", color = "black", size = px(16))
        ),
        locations = cells_title(groups = "title")
    ) %>%
    tab_style(
        style = list(
            cell_text(weight = "normal", color = "black", size = px(14))
        ),
        locations = cells_title(groups = "subtitle")
    ) %>%
    # Style column headers
    tab_style(
        style = list(
            cell_text(weight = "bold", color = "black"),
            cell_fill(color = "white")
        ),
        locations = cells_column_labels()
    ) %>%
    # Add nice styling
    tab_options(
        row.striping.include_table_body = TRUE,
        row.striping.background_color = "#f8f9fa",
        table.font.size = px(12)
    )

cat("✓ Applied factor indentation to regression table\n")

cat("5. Saving comparison tables...\n")

# Save original regression table (no indentation)
regression_table %>%
    as_gt() %>%
    gtsave(filename = file.path(test_output_dir, "regression_original.html"))

# Save styled regression table (with indentation) - it's already a gt object with indentation applied
styled_regression_table %>%
    gtsave(filename = file.path(test_output_dir, "regression_with_indentation.html"))

cat("✓ Both regression tables saved successfully!\n\n")

cat("=== REGRESSION TEST SUMMARY ===\n")
cat("Test output saved to:", test_output_dir, "\n")
cat("Files created:\n")
cat("  - regression_original.html (no indentation)\n")
cat("  - regression_with_indentation.html (WITH factor indentation)\n\n")

cat("VERIFICATION INSTRUCTIONS:\n")
cat("1. Open BOTH HTML files in a browser\n")
cat("2. In the indented version, you should see:\n")
cat("   ✓ Main variables (Treatment Group, Age, Sex, Tumor Location, Optic Nerve) in BOLD\n")
cat("   ✓ Factor levels (Male, Female, Choroidal, Cilio-Choroidal, Other, Yes, No) INDENTED\n")
cat("   ✓ Clean header formatting without markdown asterisks\n\n")
cat("This tests the new regression table indentation logic!\n") 