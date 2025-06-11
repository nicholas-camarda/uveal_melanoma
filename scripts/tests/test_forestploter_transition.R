# Test script for forestploter transition
# Verifies that all forest plot functions work with the new forestploter library

library(forestploter)
library(grid)

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/visualization/forest_plot.R")

cat("Testing forestploter transition...\n")

# Create minimal test data
minimal_test_data <- list(
    age_at_diagnosis = list(
        subgroup_effects = data.frame(
            subgroup_level = c("< 65", ">= 65"),
            treatment_effect = c(0.8, 1.2),
            ci_lower = c(0.5, 0.9),
            ci_upper = c(1.3, 1.6),
            p_value = c(0.04, 0.15),
            n_gksrs = c(20, 30),
            n_plaque = c(25, 28),
            n_total = c(45, 58),
            stringsAsFactors = FALSE
        ),
        interaction_p = 0.08
    )
)

test_variable_order <- c("age_at_diagnosis")
test_treatment_labels <- c("GKSRS", "Plaque")
test_favours_labels <- c("Favours GKSRS", "Favours Plaque")

# Test 1: Basic forest plot creation
cat("Test 1: Creating basic forest plot...\n")
tryCatch({
    test_plot <- create_single_cohort_forest_plot(
        subgroup_results = minimal_test_data,
        outcome_name = "Test Outcome",
        cohort_name = "Test Cohort",
        treatment_labels = test_treatment_labels,
        variable_order = test_variable_order,
        effect_measure = "HR",
        favours_labels = test_favours_labels,
        clip = c(0.1, 3),
        title = "Forestploter Test Plot"
    )
    
    if (!is.null(test_plot)) {
        cat("✓ Basic forest plot creation successful\n")
    } else {
        cat("✗ Basic forest plot creation failed\n")
    }
}, error = function(e) {
    cat("✗ Error in basic forest plot creation:", e$message, "\n")
})

# Test 2: Data formatting function
cat("Test 2: Testing data formatting function...\n")
tryCatch({
    formatted_data <- create_forest_plot_data(
        minimal_test_data, 
        test_variable_order, 
        test_treatment_labels, 
        "HR"
    )
    
    if (!is.null(formatted_data) && !is.null(formatted_data$data_frame)) {
        cat("✓ Data formatting successful\n")
        cat("  - Data frame rows:", nrow(formatted_data$data_frame), "\n")
        cat("  - Data frame columns:", ncol(formatted_data$data_frame), "\n")
    } else {
        cat("✗ Data formatting failed\n")
    }
}, error = function(e) {
    cat("✗ Error in data formatting:", e$message, "\n")
})

# Test 3: Save plot to file
cat("Test 3: Testing plot saving...\n")
tryCatch({
    output_dir <- "test_output"
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    
    output_file <- file.path(output_dir, "test_forestploter_transition.png")
    
    # Use the create_forest_plot wrapper function
    success <- create_forest_plot(
        subgroup_results = minimal_test_data,
        outcome_name = "Test Outcome", 
        effect_measure = "HR",
        dataset_name = "Test Dataset",
        output_path = output_file
    )
    
    if (file.exists(output_file)) {
        file_size <- file.info(output_file)$size
        cat("✓ Plot saving successful\n")
        cat("  - File size:", file_size, "bytes\n")
    } else {
        cat("✗ Plot saving failed - file not created\n")
    }
}, error = function(e) {
    cat("✗ Error in plot saving:", e$message, "\n")
})

# Test 4: Check required columns in data frame
cat("Test 4: Checking data frame structure...\n")
tryCatch({
    formatted_data <- create_forest_plot_data(
        minimal_test_data, 
        test_variable_order, 
        test_treatment_labels, 
        "HR"
    )
    
    df <- formatted_data$data_frame
    required_cols <- c("Subgroup", "GKSRS_n", "Plaque_n", "CI_space", "Effect_CI", "p_value")
    
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) == 0) {
        cat("✓ All required columns present\n")
        cat("  - Columns:", paste(names(df), collapse = ", "), "\n")
    } else {
        cat("✗ Missing columns:", paste(missing_cols, collapse = ", "), "\n")
    }
}, error = function(e) {
    cat("✗ Error checking data frame structure:", e$message, "\n")
})

cat("\nForestploter transition testing complete!\n")
cat("All functions should now use the forestploter library instead of forestplot.\n")
cat("Check test output files in test_output/ directory for visual verification.\n") 