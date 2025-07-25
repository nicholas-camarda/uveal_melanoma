# Test New Subgroup Formatting with T-stage Cutoffs
# Tests the updated subgroup analysis with clinical bins and factor-grouped tables

# Define VERBOSE for data utilities functions
VERBOSE <- TRUE

# Load required libraries
library(tidyverse)
library(gt)
library(writexl)

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/utils/subgroup_config.R") 
source("scripts/data_helper/data_utilities.R")
source("scripts/analysis/subgroup_analysis.R")
source("scripts/visualization/forest_plot.R")

# Create test output directory
test_output_dir <- "test_output/new_subgroup_formatting"
if (!dir.exists(test_output_dir)) {
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
}

cat("=== TESTING NEW SUBGROUP FORMATTING ===\n")
cat("Test output directory:", test_output_dir, "\n\n")

# Load test data
cat("1. Loading test data...\n")
tryCatch({
    test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
    cat(sprintf("✓ Loaded %d patients for testing\n", nrow(test_data)))
}, error = function(e) {
    cat("✗ Error loading test data:", e$message, "\n")
    stop("Cannot proceed without test data")
})

# Test 2: Check T-stage clinical bins  
cat("\n2. Testing T-stage clinical bins...\n")
tryCatch({
    # Test height bins
    if ("initial_tumor_height" %in% names(test_data)) {
        height_values <- test_data$initial_tumor_height[!is.na(test_data$initial_tumor_height)]
        cutoffs_height <- STANDARDIZED_CUTOFFS$initial_tumor_height
        height_bins <- create_clinical_bins(height_values, cutoffs_height, "initial_tumor_height")
        
        cat(sprintf("  Height: %d values -> %d bins\n", 
                    length(height_values), 
                    length(levels(height_bins))))
        cat("  Height bin levels:", paste(levels(height_bins), collapse=", "), "\n")
        
        # Show distribution
        height_table <- table(height_bins)
        cat("  Height distribution:\n")
        for (i in seq_along(height_table)) {
            cat(sprintf("    %s: %d patients\n", names(height_table)[i], height_table[i]))
        }
    }
    
    # Test diameter bins
    if ("initial_tumor_diameter" %in% names(test_data)) {
        diameter_values <- test_data$initial_tumor_diameter[!is.na(test_data$initial_tumor_diameter)]
        cutoffs_diameter <- STANDARDIZED_CUTOFFS$initial_tumor_diameter
        diameter_bins <- create_clinical_bins(diameter_values, cutoffs_diameter, "initial_tumor_diameter")
        
        cat(sprintf("  Diameter: %d values -> %d bins\n", 
                    length(diameter_values), 
                    length(levels(diameter_bins))))
        cat("  Diameter bin levels:", paste(levels(diameter_bins), collapse=", "), "\n")
        
        # Show distribution
        diameter_table <- table(diameter_bins)
        cat("  Diameter distribution:\n")
        for (i in seq_along(diameter_table)) {
            cat(sprintf("    %s: %d patients\n", names(diameter_table)[i], diameter_table[i]))
        }
    }
    
    cat("✓ T-stage clinical bins working correctly\n")
    
}, error = function(e) {
    cat("✗ T-stage bins test failed:", e$message, "\n")
})

# Test 3: Run subgroup analysis with new cutoffs
cat("\n3. Testing subgroup analysis with T-stage cutoffs...\n")
tryCatch({
    # Test with height and diameter variables
    test_vars <- c("initial_tumor_height", "initial_tumor_diameter")
    
    # Run tumor height subgroup analysis
    subgroup_results <- list()
    
    for (var in test_vars) {
        cat(sprintf("  Testing %s...\n", var))
        
        result <- analyze_treatment_effect_subgroups_height(
            data = test_data,
            subgroup_var = var,
            percentile_cut = 0.5,
            confounders = c("age_at_diagnosis", "sex", "location"),
            include_baseline_height = FALSE
        )
        
        subgroup_results[[var]] <- result
        
        # Check interaction p-value
        p_val <- result$interaction_p
        sig_status <- ifelse(!is.na(p_val) && p_val < 0.05, "SIGNIFICANT", "non-significant")
        cat(sprintf("    Interaction p-value: %.4f (%s)\n", 
                    ifelse(is.na(p_val), 999, p_val), sig_status))
        
        # Check number of subgroup levels
        if (!is.null(result$subgroup_effects)) {
            n_levels <- nrow(result$subgroup_effects)
            cat(sprintf("    Subgroup levels: %d\n", n_levels))
        }
    }
    
    cat("✓ Subgroup analysis with T-stage cutoffs completed\n")
    
}, error = function(e) {
    cat("✗ Subgroup analysis test failed:", e$message, "\n")
    subgroup_results <- list()
})

# Test 4: Test new table formatting
cat("\n4. Testing new factor-grouped table formatting...\n")
if (length(subgroup_results) > 0) {
    tryCatch({
        # Test the new formatting function
        output_path <- file.path(test_output_dir, "test_factor_grouped_table.xlsx")
        
        formatted_table <- format_subgroup_analysis_results(
            subgroup_results = subgroup_results,
            outcome_name = "Tumor Height Change - Test",
            effect_measure = "MD",
            output_path = output_path
        )
        
        if (!is.null(formatted_table)) {
            cat("✓ Factor-grouped table formatting completed\n")
            cat(sprintf("  Table dimensions: %d rows x %d columns\n", 
                        nrow(formatted_table), ncol(formatted_table)))
            cat(sprintf("  Excel file: %s\n", output_path))
            cat(sprintf("  HTML file: %s\n", gsub("\\.xlsx$", ".html", output_path)))
            
            # Show first few rows
            cat("  First few rows:\n")
            print(head(formatted_table, 10))
        } else {
            cat("⚠ Table formatting returned NULL\n")
        }
        
    }, error = function(e) {
        cat("✗ Table formatting test failed:", e$message, "\n")
    })
} else {
    cat("⚠ No subgroup results to format\n")
}

cat("\n=== TEST COMPLETE ===\n")
cat("Check the test output directory for generated files:\n")
cat(sprintf("  %s\n", test_output_dir)) 