# Test Subgroup Analysis Table Formatting
# Tests the format_subgroup_analysis_results function to ensure both Excel and HTML outputs work correctly

# Load required libraries
library(tidyverse)
library(gt)
library(gtsummary)
library(writexl)

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/utils/output_utilities.R")
source("scripts/analysis/subgroup_analysis.R")
source("scripts/visualization/forest_plot.R")

# Create test output directory
test_output_dir <- "test_output/subgroup_table_formatting"
if (!dir.exists(test_output_dir)) {
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
}

cat("=== SUBGROUP TABLE FORMATTING TEST ===\n")
cat("Test output directory:", test_output_dir, "\n\n")

# Test 1: Create mock subgroup results data
cat("1. Creating mock subgroup analysis results...\n")

# Create realistic subgroup results that mimic what analyze_treatment_effect_subgroups_* functions return
create_mock_subgroup_results <- function(outcome_type = "binary") {
    
    # Mock subgroup effects for different variables
    age_effects <- data.frame(
        subgroup_level = c("< 65", ">= 65"),
        n_total = c(45, 55),
        n_plaque = c(22, 28),
        n_gksrs = c(23, 27),
        treatment_effect = c(1.25, 0.85),
        ci_lower = c(0.75, 0.45),
        ci_upper = c(2.10, 1.60),
        p_value = c(0.395, 0.628),
        stringsAsFactors = FALSE
    )
    
    sex_effects <- data.frame(
        subgroup_level = c("Male", "Female"),
        n_total = c(55, 45),
        n_plaque = c(28, 22),
        n_gksrs = c(27, 23),
        treatment_effect = c(0.92, 1.15),
        ci_lower = c(0.52, 0.68),
        ci_upper = c(1.65, 1.95),
        p_value = c(0.792, 0.601),
        stringsAsFactors = FALSE
    )
    
    location_effects <- data.frame(
        subgroup_level = c("Anterior", "Posterior"),
        n_total = c(35, 65),
        n_plaque = c(18, 32),
        n_gksrs = c(17, 33),
        treatment_effect = c(1.42, 0.88),
        ci_lower = c(0.65, 0.48),
        ci_upper = c(3.10, 1.61),
        p_value = c(0.378, 0.668),
        stringsAsFactors = FALSE
    )
    
    # Adjust effect measure for different outcome types
    if (outcome_type == "survival") {
        # For survival outcomes, make effects look like hazard ratios
        age_effects$treatment_effect <- c(0.75, 1.25)
        sex_effects$treatment_effect <- c(1.08, 0.92)
        location_effects$treatment_effect <- c(0.68, 1.15)
    } else if (outcome_type == "continuous") {
        # For continuous outcomes, make effects look like mean differences
        age_effects$treatment_effect <- c(-1.2, 0.8)
        age_effects$ci_lower <- c(-2.5, -0.9)
        age_effects$ci_upper <- c(0.1, 2.5)
        
        sex_effects$treatment_effect <- c(0.5, -0.9)
        sex_effects$ci_lower <- c(-1.1, -2.2)
        sex_effects$ci_upper <- c(2.1, 0.4)
        
        location_effects$treatment_effect <- c(-0.7, 0.3)
        location_effects$ci_lower <- c(-2.8, -1.2)
        location_effects$ci_upper <- c(1.4, 1.8)
    }
    
    # Create the full subgroup results structure
    subgroup_results <- list(
        age_at_diagnosis = list(
            subgroup_effects = age_effects,
            interaction_p = 0.1234
        ),
        sex = list(
            subgroup_effects = sex_effects,
            interaction_p = 0.5678
        ),
        location = list(
            subgroup_effects = location_effects,
            interaction_p = 0.0123
        )
    )
    
    return(subgroup_results)
}

cat("✓ Mock subgroup results creation function defined\n")

# Test 2: Test Binary Outcome Formatting (OR)
cat("\n2. Testing binary outcome formatting (Odds Ratios)...\n")

binary_results <- create_mock_subgroup_results("binary")
binary_output_path <- file.path(test_output_dir, "binary_outcome_subgroup_test.xlsx")

tryCatch({
    binary_formatted <- format_subgroup_analysis_results(
        subgroup_results = binary_results,
        outcome_name = "Local Recurrence - Full Cohort",
        effect_measure = "OR",
        output_path = binary_output_path
    )
    
    cat("✓ Binary outcome table formatting completed\n")
    cat(sprintf("  - Excel file: %s\n", binary_output_path))
    cat(sprintf("  - HTML file: %s\n", gsub("\\.xlsx$", ".html", binary_output_path)))
    cat(sprintf("  - Table dimensions: %d rows x %d columns\n", nrow(binary_formatted), ncol(binary_formatted)))
    
}, error = function(e) {
    cat("✗ Binary outcome formatting failed:", e$message, "\n")
})

# Test 3: Test Survival Outcome Formatting (HR)
cat("\n3. Testing survival outcome formatting (Hazard Ratios)...\n")

survival_results <- create_mock_subgroup_results("survival")
survival_output_path <- file.path(test_output_dir, "survival_outcome_subgroup_test.xlsx")

tryCatch({
    survival_formatted <- format_subgroup_analysis_results(
        subgroup_results = survival_results,
        outcome_name = "Overall Survival - Restricted Cohort",
        effect_measure = "HR",
        output_path = survival_output_path
    )
    
    cat("✓ Survival outcome table formatting completed\n")
    cat(sprintf("  - Excel file: %s\n", survival_output_path))
    cat(sprintf("  - HTML file: %s\n", gsub("\\.xlsx$", ".html", survival_output_path)))
    cat(sprintf("  - Table dimensions: %d rows x %d columns\n", nrow(survival_formatted), ncol(survival_formatted)))
    
}, error = function(e) {
    cat("✗ Survival outcome formatting failed:", e$message, "\n")
})

# Test 4: Test Continuous Outcome Formatting (MD)
cat("\n4. Testing continuous outcome formatting (Mean Differences)...\n")

continuous_results <- create_mock_subgroup_results("continuous")
continuous_output_path <- file.path(test_output_dir, "continuous_outcome_subgroup_test.xlsx")

tryCatch({
    continuous_formatted <- format_subgroup_analysis_results(
        subgroup_results = continuous_results,
        outcome_name = "Tumor Height Change - PRIMARY - GKSRS Only",
        effect_measure = "MD",
        output_path = continuous_output_path
    )
    
    cat("✓ Continuous outcome table formatting completed\n")
    cat(sprintf("  - Excel file: %s\n", continuous_output_path))
    cat(sprintf("  - HTML file: %s\n", gsub("\\.xlsx$", ".html", continuous_output_path)))
    cat(sprintf("  - Table dimensions: %d rows x %d columns\n", nrow(continuous_formatted), ncol(continuous_formatted)))
    
}, error = function(e) {
    cat("✗ Continuous outcome formatting failed:", e$message, "\n")
})

# Test 5: Test format_subgroup_analysis_tables wrapper function
cat("\n5. Testing wrapper function format_subgroup_analysis_tables...\n")

wrapper_output_dir <- file.path(test_output_dir, "wrapper_test")
if (!dir.exists(wrapper_output_dir)) {
    dir.create(wrapper_output_dir, recursive = TRUE, showWarnings = FALSE)
}

tryCatch({
    format_subgroup_analysis_tables(
        subgroup_results = continuous_results,
        dataset_name = "Test Dataset - Full Cohort",
        subgroup_dir = wrapper_output_dir,
        prefix = "test_"
    )
    
    # Count generated files
    generated_files <- list.files(wrapper_output_dir, pattern = "test_.*\\.(xlsx|html)$")
    cat(sprintf("✓ Wrapper function completed - generated %d files:\n", length(generated_files)))
    for (file in generated_files) {
        cat(sprintf("  - %s\n", file))
    }
    
}, error = function(e) {
    cat("✗ Wrapper function failed:", e$message, "\n")
})

# Test 6: Verify file contents
cat("\n6. Verifying file contents and structure...\n")

# Check that Excel files exist and are readable
excel_files <- list.files(test_output_dir, pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE)
html_files <- list.files(test_output_dir, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)

cat(sprintf("Found %d Excel files and %d HTML files\n", length(excel_files), length(html_files)))

# Test reading Excel files
for (excel_file in excel_files[1:3]) {  # Test first 3 to avoid too much output
    tryCatch({
        test_data <- readxl::read_excel(excel_file)
        cat(sprintf("✓ %s: %d rows x %d columns\n", basename(excel_file), nrow(test_data), ncol(test_data)))
        
        # Check for required columns
        required_cols <- c("Subgroup", "Level", "p-value", "GKSRS/Plaque")
        has_required <- all(required_cols %in% names(test_data))
        cat(sprintf("  Required columns present: %s\n", ifelse(has_required, "✓", "✗")))
        
    }, error = function(e) {
        cat(sprintf("✗ Failed to read %s: %s\n", basename(excel_file), e$message))
    })
}

# Test 7: Display sample of formatted output
cat("\n7. Sample of formatted output:\n")
if (exists("binary_formatted") && !is.null(binary_formatted)) {
    cat("Binary outcome table (first 5 rows):\n")
    print(head(binary_formatted, 5))
}

cat("\n=== TEST SUMMARY ===\n")
cat(sprintf("Test output directory: %s\n", test_output_dir))
cat("Files generated:\n")

all_files <- list.files(test_output_dir, recursive = TRUE)
for (file in all_files) {
    cat(sprintf("  - %s\n", file))
}

cat("\n✓ SUBGROUP TABLE FORMATTING TEST COMPLETED!\n")
cat("\nTo verify the styling, open the HTML files in a web browser.\n")
cat("The tables should have:\n")
cat("  - Clean titles with cohort names\n")
cat("  - Interaction p-values in subtitles\n")
cat("  - Bold headers\n")
cat("  - Professional formatting matching other analysis tables\n")
cat("  - No duplicate information\n") 