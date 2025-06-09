# Test Location Problem for Metastatic and Recurrence Outcomes
# Focused test to verify if location issues persist

# Define VERBOSE for data utilities functions
VERBOSE <- TRUE

# Load required libraries
suppressMessages({
    library(tidyverse)
    library(survival)
    library(gtsummary)
    library(gt)
})

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/data_helper/data_processing.R") # Contains log_message function
source("scripts/data_helper/data_utilities.R") 
source("scripts/analysis/subgroup_analysis.R")
source("scripts/visualization/forest_plot.R")

# Create test output directory
test_output_dir <- file.path("test_output", format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== TESTING LOCATION PROBLEM FOR METASTATIC AND RECURRENCE OUTCOMES ===\n")
cat(sprintf("Test output directory: %s\n", test_output_dir))

# Load test data
cat("\n1. Loading test data...\n")
test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
cat(sprintf("✓ Loaded %d patients for testing\n", nrow(test_data)))

# Check location variable specifically
cat("\n2. Examining location variable...\n")
if ("location" %in% names(test_data)) {
    location_summary <- test_data %>%
        count(location, sort = TRUE) %>%
        mutate(percent = round(n/sum(n)*100, 1))
    
    cat("Location distribution:\n")
    print(location_summary)
    
    # Check for rare categories
    rare_categories <- location_summary %>%
        filter(n < 5) %>%
        pull(location)
    
    if (length(rare_categories) > 0) {
        cat(sprintf("\nRare location categories (n<5): %s\n", paste(rare_categories, collapse = ", ")))
    } else {
        cat("\nNo rare location categories found (all n>=5)\n")
    }
} else {
    cat("✗ Location variable not found in data\n")
    stop("Cannot proceed without location variable")
}

# Test 3: Location subgroup analysis for LOCAL RECURRENCE
cat("\n3. Testing location subgroup analysis for LOCAL RECURRENCE...\n")
tryCatch({
    recurrence_location_result <- analyze_treatment_effect_subgroups_binary(
        data = test_data,
        outcome_var = "recurrence1",
        subgroup_vars = "location",
        confounders = setdiff(confounders, "location"), # Remove location from confounders
        outcome_name = "Local Recurrence (Location Test)"
    )
    
    # Extract and display results
    if ("location" %in% names(recurrence_location_result)) {
        result <- recurrence_location_result[["location"]]
        
        if (!is.null(result$interaction_p) && !is.na(result$interaction_p)) {
            sig_status <- ifelse(result$interaction_p < 0.05, "SIGNIFICANT", "non-significant")
            cat(sprintf("✓ Local recurrence × location interaction: p = %.4f (%s)\n", 
                       result$interaction_p, sig_status))
            
            # Show subgroup effects if available
            if (!is.null(result$subgroup_effects) && nrow(result$subgroup_effects) > 0) {
                cat("\nSubgroup effects by location:\n")
                subgroup_summary <- result$subgroup_effects %>%
                    select(subgroup_level, or, or_ci_lower, or_ci_upper, p_value) %>%
                    mutate(
                        or_ci = sprintf("%.2f (%.2f-%.2f)", or, or_ci_lower, or_ci_upper),
                        p_formatted = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
                    ) %>%
                    select(subgroup_level, or_ci, p_formatted)
                
                print(subgroup_summary)
            }
        } else {
            cat("✗ Local recurrence × location analysis failed or returned NA\n")
            if (!is.null(result$error)) {
                cat(sprintf("Error: %s\n", result$error))
            }
        }
    } else {
        cat("✗ Location results not found in recurrence analysis\n")
    }
    
    # Save results
    saveRDS(recurrence_location_result, 
            file.path(test_output_dir, "recurrence_location_subgroup.rds"))
    
}, error = function(e) {
    cat(sprintf("✗ Local recurrence × location test failed: %s\n", e$message))
})

# Test 4: Location subgroup analysis for METASTATIC PROGRESSION
cat("\n4. Testing location subgroup analysis for METASTATIC PROGRESSION...\n")
tryCatch({
    mets_location_result <- analyze_treatment_effect_subgroups_binary(
        data = test_data,
        outcome_var = "mets_progression",
        subgroup_vars = "location",
        confounders = setdiff(confounders, "location"), # Remove location from confounders
        outcome_name = "Metastatic Progression (Location Test)"
    )
    
    # Extract and display results
    if ("location" %in% names(mets_location_result)) {
        result <- mets_location_result[["location"]]
        
        if (!is.null(result$interaction_p) && !is.na(result$interaction_p)) {
            sig_status <- ifelse(result$interaction_p < 0.05, "SIGNIFICANT", "non-significant")
            cat(sprintf("✓ Metastatic progression × location interaction: p = %.4f (%s)\n", 
                       result$interaction_p, sig_status))
            
            # Show subgroup effects if available
            if (!is.null(result$subgroup_effects) && nrow(result$subgroup_effects) > 0) {
                cat("\nSubgroup effects by location:\n")
                subgroup_summary <- result$subgroup_effects %>%
                    select(subgroup_level, or, or_ci_lower, or_ci_upper, p_value) %>%
                    mutate(
                        or_ci = sprintf("%.2f (%.2f-%.2f)", or, or_ci_lower, or_ci_upper),
                        p_formatted = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
                    ) %>%
                    select(subgroup_level, or_ci, p_formatted)
                
                print(subgroup_summary)
            }
        } else {
            cat("✗ Metastatic progression × location analysis failed or returned NA\n")
            if (!is.null(result$error)) {
                cat(sprintf("Error: %s\n", result$error))
            }
        }
    } else {
        cat("✗ Location results not found in metastatic progression analysis\n")
    }
    
    # Save results
    saveRDS(mets_location_result, 
            file.path(test_output_dir, "mets_location_subgroup.rds"))
    
}, error = function(e) {
    cat(sprintf("✗ Metastatic progression × location test failed: %s\n", e$message))
})

# Test 5: Forest plot creation for location subgroups
cat("\n5. Testing forest plot creation for location subgroups...\n")
tryCatch({
    # Test with recurrence results if available
    if (exists("recurrence_location_result") && !is.null(recurrence_location_result)) {
        recurrence_forest_plot <- create_forest_plot(
            subgroup_results = recurrence_location_result,
            outcome_name = "Local Recurrence (Location)",
            effect_measure = "OR",
            dataset_name = "Test Cohort",
            output_path = file.path(test_output_dir, "recurrence_location_forest_plot.png")
        )
        
        if (!is.null(recurrence_forest_plot)) {
            cat("✓ Local recurrence location forest plot created\n")
        } else {
            cat("✗ Local recurrence location forest plot creation failed\n")
        }
    }
    
    # Test with metastatic progression results if available
    if (exists("mets_location_result") && !is.null(mets_location_result)) {
        mets_forest_plot <- create_forest_plot(
            subgroup_results = mets_location_result,
            outcome_name = "Metastatic Progression (Location)",
            effect_measure = "OR",
            dataset_name = "Test Cohort",
            output_path = file.path(test_output_dir, "mets_location_forest_plot.png")
        )
        
        if (!is.null(mets_forest_plot)) {
            cat("✓ Metastatic progression location forest plot created\n")
        } else {
            cat("✗ Metastatic progression location forest plot creation failed\n")
        }
    }
    
}, error = function(e) {
    cat(sprintf("✗ Forest plot creation failed: %s\n", e$message))
})

# Test 6: Check data processing for location specifically
cat("\n6. Testing location data processing...\n")
tryCatch({
    processed_location_data <- process_subgroup_data(
        test_data, 
        "location", 
        setdiff(confounders, "location")
    )
    
    cat("✓ Location data processing completed\n")
    cat(sprintf("  Original variable: location\n"))
    cat(sprintf("  Processed variable: %s\n", processed_location_data$subgroup_var_to_use))
    cat(sprintf("  Data rows: %d\n", nrow(processed_location_data$data)))
    
    # Check the processed location levels
    if (is.factor(processed_location_data$data[[processed_location_data$subgroup_var_to_use]])) {
        levels_info <- table(processed_location_data$data[[processed_location_data$subgroup_var_to_use]])
        cat("  Processed location levels:\n")
        for (level in names(levels_info)) {
            cat(sprintf("    %s: %d patients\n", level, levels_info[level]))
        }
    }
    
}, error = function(e) {
    cat(sprintf("✗ Location data processing failed: %s\n", e$message))
})

# Summary
cat("\n=== LOCATION PROBLEM TEST SUMMARY ===\n")
cat(sprintf("Test results saved to: %s\n", test_output_dir))
cat("\nTests performed:\n")
cat("  - Location variable examination and rare category detection\n")
cat("  - Local recurrence × location interaction analysis\n") 
cat("  - Metastatic progression × location interaction analysis\n")
cat("  - Forest plot creation for location subgroups\n")
cat("  - Location-specific data processing verification\n")

cat("\nTo check for remaining issues:\n")
cat("  - Review interaction p-values above\n")
cat("  - Check forest plot files in test output directory\n")
cat("  - Examine subgroup effect estimates by location\n") 