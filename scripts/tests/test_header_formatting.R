# Test Header Formatting Fix
# This test verifies that table headers are properly styled without markdown asterisks
# AND that factor indentation works on proper gtsummary tables

# Load required libraries
library(tidyverse)
library(gt)
library(gtsummary)

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/utils/output_utilities.R")

# Create test output directory
test_output_dir <- "test_output/header_test"
if (!dir.exists(test_output_dir)) {
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
}

cat("=== HEADER FORMATTING & FACTOR INDENTATION TEST ===\n")
cat("Test output directory:", test_output_dir, "\n\n")

cat("1. Creating realistic test data with factor levels...\n")

# Create sample data that mimics real baseline characteristics
set.seed(123)
n <- 100
test_data <- data.frame(
    age_at_diagnosis = round(rnorm(n, 65, 10), 1),
    sex = sample(c("Male", "Female"), n, replace = TRUE),
    treatment_group = sample(c("Plaque", "GKSRS"), n, replace = TRUE),
    location = sample(c("Anterior", "Posterior", "Equatorial"), n, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
    initial_tumor_height = round(rnorm(n, 5.2, 1.8), 1),
    biopsy1_gep = sample(c("Class 1A", "Class 1B", "Class 2"), n, replace = TRUE, prob = c(0.4, 0.3, 0.3))
)

cat("✓ Created realistic test dataset with", n, "patients\n")

cat("2. Creating gtsummary baseline table...\n")

# Create proper gtsummary table (this is what our function expects)
baseline_table <- test_data %>%
    tbl_summary(
        by = treatment_group,
        missing = "no",
        label = STANDARD_TABLE_LABELS  # Use our centralized labels
    ) %>%
    add_overall() %>%
    add_p() %>%
    modify_header(
        label = "Characteristic",
        stat_0 = "Overall, N = {N}",
        stat_1 = "Plaque, N = {n}",
        stat_2 = "GKSRS, N = {n}",
        p.value = "p-value"
    )

cat("✓ gtsummary baseline table created successfully\n")

cat("3. Testing header formatting and factor indentation...\n")

# Test the complete workflow: gtsummary -> factor indentation -> gt -> styled headers
final_table <- baseline_table %>%
    apply_factor_level_indentation() %>%  # Apply factor indentation first
    tab_header(
        title = "Table 1: Test Baseline Characteristics",
        subtitle = "Testing Header Formatting AND Factor Indentation"
    ) %>%
    # Add proper header styling (our fix)
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

cat("✓ Applied factor indentation and header styling\n")

cat("4. Saving both versions for comparison...\n")

# Save original gtsummary table (no indentation)
baseline_table %>%
    as_gt() %>%
    gtsave(filename = file.path(test_output_dir, "baseline_original.html"))

# Save final styled table (with indentation + headers)
save_gt_html(
    final_table,
    filename = file.path(test_output_dir, "baseline_with_indentation_and_headers.html")
)

cat("✓ Both tables saved successfully!\n\n")

cat("=== COMPLETE TEST SUMMARY ===\n")
cat("Test output saved to:", test_output_dir, "\n")
cat("Files created:\n")
cat("  - baseline_original.html (no indentation)\n")
cat("  - baseline_with_indentation_and_headers.html (WITH indentation + styled headers)\n\n")

cat("VERIFICATION INSTRUCTIONS:\n")
cat("1. Open BOTH HTML files in a browser\n")
cat("2. Compare the difference:\n")
cat("   Original: Plain table with no indentation\n")
cat("   Final: Should have:\n")
cat("     ✓ Bold title without '**' asterisks\n")
cat("     ✓ Main variables (Age, Sex, Location, etc.) in BOLD\n")
cat("     ✓ Factor levels (Male, Female, Anterior, etc.) INDENTED\n")
cat("     ✓ Clean professional styling\n\n")
cat("This shows the complete workflow working together!\n") 