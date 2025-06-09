# Test Tumor Height Analysis Components
# Independent test of tumor height change analyses

# Load required libraries
suppressMessages({
    library(tidyverse)
    library(gtsummary)
    library(gt)
})

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/data_helper/data_utilities.R")
source("scripts/analysis/tumor_height_analysis.R")

# Create test output directory
test_output_dir <- file.path("test_output", format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== TESTING TUMOR HEIGHT ANALYSIS COMPONENTS ===\n")
cat(sprintf("Test output directory: %s\n", test_output_dir))

# Load test data
cat("\n1. Loading test data...\n")
tryCatch({
    test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
    cat(sprintf("✓ Loaded %d patients for testing\n", nrow(test_data)))
    
    # Check for tumor height variables
    height_vars <- c("tumor_height_baseline", "tumor_height_followup", "tumor_height_change")
    cat("Tumor height variables:\n")
    for (var in height_vars) {
        if (var %in% names(test_data)) {
            non_na_count <- sum(!is.na(test_data[[var]]))
            cat(sprintf("  %s: %d non-missing values\n", var, non_na_count))
        } else {
            cat(sprintf("  %s: MISSING\n", var))
        }
    }
    
}, error = function(e) {
    cat("✗ Error loading test data:", e$message, "\n")
    stop("Cannot proceed without test data")
})

# Test 2: Main tumor height analysis function
cat("\n2. Testing main tumor height analysis...\n")
tryCatch({
    height_results <- analyze_tumor_height_changes(test_data)
    cat("✓ Tumor height analysis completed\n")
    
    # Check results structure
    if (is.list(height_results)) {
        cat("Results components:\n")
        for (component in names(height_results)) {
            cat(sprintf("  %s: %s\n", component, class(height_results[[component]])[1]))
        }
        
        # Show key statistics if available
        if ("primary_analysis" %in% names(height_results) && 
            !is.null(height_results$primary_analysis) &&
            "height_change_summary" %in% names(height_results$primary_analysis)) {
            
            summary_stats <- height_results$primary_analysis$height_change_summary
            cat("\nHeight change summary:\n")
            print(summary_stats)
        }
    }
    
    # Save results for inspection
    saveRDS(height_results, file.path(test_output_dir, "test_height_analysis_results.rds"))
    
}, error = function(e) {
    cat("✗ Tumor height analysis failed:", e$message, "\n")
})

# Test 3: Subgroup analysis for tumor height
cat("\n3. Testing tumor height subgroup analysis...\n")

# Test if the tumor height subgroup function exists
if (exists("analyze_treatment_effect_subgroups_height")) {
    tryCatch({
        # Test with a few subgroup variables
        test_subgroup_vars <- subgroup_vars[1:3]
        
        for (i in seq_along(test_subgroup_vars)) {
            subgroup_var <- test_subgroup_vars[i]
            cat(sprintf("\nTesting subgroup: %s\n", subgroup_var))
            
            # Primary analysis (without baseline height)
            primary_result <- analyze_treatment_effect_subgroups_height(
                data = test_data,
                subgroup_var = subgroup_var,
                percentile_cut = 0.5,
                confounders = confounders,
                include_baseline_height = FALSE
            )
            
            # Sensitivity analysis (with baseline height)
            sensitivity_result <- analyze_treatment_effect_subgroups_height(
                data = test_data,
                subgroup_var = subgroup_var,
                percentile_cut = 0.5,
                confounders = confounders,
                include_baseline_height = TRUE
            )
            
            # Show interaction p-values
            cat(sprintf("  Primary p-value: %.4f\n", 
                        ifelse(is.na(primary_result$interaction_p), 999, primary_result$interaction_p)))
            cat(sprintf("  Sensitivity p-value: %.4f\n", 
                        ifelse(is.na(sensitivity_result$interaction_p), 999, sensitivity_result$interaction_p)))
        }
        
        cat("✓ Tumor height subgroup analysis completed\n")
        
    }, error = function(e) {
        cat("✗ Tumor height subgroup analysis failed:", e$message, "\n")
    })
} else {
    cat("⚠ analyze_treatment_effect_subgroups_height function not found\n")
}

# Test 4: Data quality checks for tumor height
cat("\n4. Testing tumor height data quality...\n")
tryCatch({
    if ("tumor_height_change" %in% names(test_data)) {
        height_change <- test_data$tumor_height_change
        
        # Basic statistics
        cat(sprintf("  Total observations: %d\n", length(height_change)))
        cat(sprintf("  Non-missing: %d (%.1f%%)\n", 
                    sum(!is.na(height_change)), 
                    100 * sum(!is.na(height_change)) / length(height_change)))
        
        if (sum(!is.na(height_change)) > 0) {
            cat(sprintf("  Mean change: %.2f mm\n", mean(height_change, na.rm = TRUE)))
            cat(sprintf("  Median change: %.2f mm\n", median(height_change, na.rm = TRUE)))
            cat(sprintf("  Range: %.2f to %.2f mm\n", 
                        min(height_change, na.rm = TRUE),
                        max(height_change, na.rm = TRUE)))
            
            # Check by treatment group
            if ("treatment_group" %in% names(test_data)) {
                by_treatment <- test_data %>%
                    filter(!is.na(tumor_height_change)) %>%
                    group_by(treatment_group) %>%
                    summarise(
                        n = n(),
                        mean_change = mean(tumor_height_change, na.rm = TRUE),
                        sd_change = sd(tumor_height_change, na.rm = TRUE),
                        .groups = "drop"
                    )
                
                cat("\nBy treatment group:\n")
                print(by_treatment)
            }
        }
    } else {
        cat("⚠ tumor_height_change variable not found\n")
    }
    
    cat("✓ Data quality check completed\n")
    
}, error = function(e) {
    cat("✗ Data quality check failed:", e$message, "\n")
})

# Test 5: Comparison methods (t-test, Mann-Whitney)
cat("\n5. Testing comparison methods...\n")
tryCatch({
    if ("tumor_height_change" %in% names(test_data) && "treatment_group" %in% names(test_data)) {
        analysis_data <- test_data %>%
            filter(!is.na(tumor_height_change), !is.na(treatment_group))
        
        if (nrow(analysis_data) > 10) {
            plaque_data <- analysis_data$tumor_height_change[analysis_data$treatment_group == "Plaque"]
            gksrs_data <- analysis_data$tumor_height_change[analysis_data$treatment_group == "GKSRS"]
            
            # T-test
            if (length(plaque_data) > 5 && length(gksrs_data) > 5) {
                t_test_result <- t.test(gksrs_data, plaque_data)
                cat(sprintf("  T-test p-value: %.4f\n", t_test_result$p.value))
                cat(sprintf("  Mean difference: %.2f mm\n", 
                            t_test_result$estimate[1] - t_test_result$estimate[2]))
                
                # Mann-Whitney U test
                wilcox_result <- wilcox.test(gksrs_data, plaque_data)
                cat(sprintf("  Mann-Whitney p-value: %.4f\n", wilcox_result$p.value))
                
                cat("✓ Comparison methods tested\n")
            } else {
                cat("⚠ Insufficient data for comparison tests\n")
            }
        } else {
            cat("⚠ Insufficient data for analysis\n")
        }
    } else {
        cat("⚠ Required variables not available\n")
    }
    
}, error = function(e) {
    cat("✗ Comparison methods test failed:", e$message, "\n")
})

# Test 6: Regression modeling for tumor height
cat("\n6. Testing regression modeling...\n")
tryCatch({
    if ("tumor_height_change" %in% names(test_data)) {
        model_data <- test_data %>%
            filter(!is.na(tumor_height_change), !is.na(treatment_group)) %>%
            ensure_consistent_contrasts()
        
        if (nrow(model_data) > 20) {
            # Simple model
            simple_model <- lm(tumor_height_change ~ treatment_group, data = model_data)
            cat(sprintf("  Simple model treatment effect: %.2f (p = %.4f)\n",
                        coef(simple_model)["treatment_groupGKSRS"],
                        summary(simple_model)$coefficients["treatment_groupGKSRS", "Pr(>|t|)"]))
            
            # Model with confounders (if available)
            valid_confounders <- generate_valid_confounders(
                model_data, confounders, threshold = THRESHOLD_RARITY
            )
            
            if (length(valid_confounders) > 0) {
                formula_str <- paste("tumor_height_change ~ treatment_group +", 
                                   paste(valid_confounders, collapse = " + "))
                adjusted_model <- lm(as.formula(formula_str), data = model_data)
                
                cat(sprintf("  Adjusted model treatment effect: %.2f (p = %.4f)\n",
                            coef(adjusted_model)["treatment_groupGKSRS"],
                            summary(adjusted_model)$coefficients["treatment_groupGKSRS", "Pr(>|t|)"]))
                
                cat(sprintf("  R-squared: %.3f\n", summary(adjusted_model)$r.squared))
            }
            
            cat("✓ Regression modeling tested\n")
        } else {
            cat("⚠ Insufficient data for regression modeling\n")
        }
    }
    
}, error = function(e) {
    cat("✗ Regression modeling test failed:", e$message, "\n")
})

# Summary
cat("\n=== TUMOR HEIGHT ANALYSIS TEST SUMMARY ===\n")
cat(sprintf("All test outputs saved to: %s\n", test_output_dir))
cat("\nTest components checked:\n")
cat("  - Data loading and tumor height variable availability\n")
cat("  - Main tumor height analysis function\n")
cat("  - Subgroup analysis for tumor height changes\n")
cat("  - Data quality and descriptive statistics\n")
cat("  - Statistical comparison methods (t-test, Mann-Whitney)\n")
cat("  - Regression modeling (simple and adjusted)\n")

cat("\nTo test individual functions:\n")
cat("  analyze_tumor_height_changes(data)\n")
cat("  analyze_treatment_effect_subgroups_height(data, subgroup_var, ...)\n")

# Check if results files exist from previous analyses
cat("\nExisting tumor height analysis results:\n")
height_results_pattern <- "final_data/Analysis/*/tables/primary_outcomes/tumor_height_change/**/*.rds"
existing_results <- Sys.glob(height_results_pattern)
if (length(existing_results) > 0) {
    for (result_file in existing_results) {
        cat(sprintf("  %s\n", result_file))
    }
} else {
    cat("  No existing results found\n")
} 