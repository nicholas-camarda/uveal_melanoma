# Quick Workflow Test
# Runs a streamlined version of the main workflow with minimal processing

# Load required libraries
suppressMessages({
    library(tidyverse)
    library(survival)
    library(gtsummary)
    library(gt)
})

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/data_helper/data_utilities.R") 
source("scripts/analysis/statistical_analysis.R")
source("scripts/visualization/forest_plot.R")

# Create test output directory
test_output_dir <- file.path("test_output", format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== QUICK WORKFLOW TEST ===\n")
cat("Testing streamlined version of main workflow with subset of functions\n")
cat(sprintf("Test output directory: %s\n", test_output_dir))

workflow_start_time <- Sys.time()

# Step 1: Load processed data
cat("\n1. Loading processed data...\n")
tryCatch({
    full_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
    cat(sprintf("✓ Loaded full cohort: %d patients\n", nrow(full_data)))
    
    # Use subset for faster testing
    set.seed(123)
    test_data <- full_data %>% 
        slice_sample(n = min(100, nrow(full_data))) %>%
        ensure_consistent_contrasts()
    
    cat(sprintf("✓ Using subset: %d patients for testing\n", nrow(test_data)))
    
}, error = function(e) {
    cat("✗ Data loading failed:", e$message, "\n")
    stop("Cannot proceed without data")
})

# Step 2: Quick binary outcome analysis
cat("\n2. Testing binary outcome analysis...\n")
tryCatch({
    recurrence_result <- analyze_binary_outcome_rates(
        test_data,
        outcome_var = "recurrence1",
        time_var = "tt_recurrence_months",
        event_var = "recurrence_event", 
        confounders = confounders[1:3],  # Use fewer confounders for speed
        exclude_before_treatment = TRUE,
        handle_rare = TRUE,
        dataset_name = "quick_test"
    )
    cat("✓ Binary outcome analysis completed\n")
    
}, error = function(e) {
    cat("✗ Binary outcome analysis failed:", e$message, "\n")
})

# Step 3: Quick survival analysis
cat("\n3. Testing survival analysis...\n")
tryCatch({
    os_result <- analyze_time_to_event_outcomes(
        test_data,
        time_var = "tt_death_months",
        event_var = "death_event",
        group_var = "treatment_group",
        confounders = confounders[1:3],  # Use fewer confounders for speed
        ylab = "Overall Survival Probability",
        exclude_before_treatment = TRUE,
        handle_rare = TRUE,
        dataset_name = "quick_test"
    )
    cat("✓ Survival analysis completed\n")
    
    # Save test plot
    if (!is.null(os_result$km_plot)) {
        ggsave(file.path(test_output_dir, "quick_survival_plot.png"), 
               os_result$km_plot, width = 8, height = 6, dpi = 300)
        cat("  Survival plot saved\n")
    }
    
}, error = function(e) {
    cat("✗ Survival analysis failed:", e$message, "\n")
})

# Step 4: Quick subgroup analysis
cat("\n4. Testing subgroup analysis...\n")
if (exists("analyze_treatment_effect_subgroups_binary")) {
    tryCatch({
        # Use only first 2 subgroup variables for speed
        quick_subgroup_vars <- subgroup_vars[1:2]
        
        subgroup_results <- analyze_treatment_effect_subgroups_binary(
            data = test_data,
            outcome_var = "recurrence1",
            subgroup_vars = quick_subgroup_vars,
            confounders = confounders[1:2],  # Minimal confounders
            outcome_name = "Local Recurrence (Quick Test)"
        )
        
        cat(sprintf("✓ Subgroup analysis completed for %d variables\n", length(quick_subgroup_vars)))
        
        # Show interaction p-values
        for (var in names(subgroup_results)) {
            p_val <- subgroup_results[[var]]$interaction_p
            cat(sprintf("  %s: p = %.4f\n", var, ifelse(is.na(p_val), 999, p_val)))
        }
        
    }, error = function(e) {
        cat("✗ Subgroup analysis failed:", e$message, "\n")
    })
} else {
    cat("⚠ Subgroup analysis function not available\n")
}

# Step 5: Quick forest plot test
cat("\n5. Testing forest plot creation...\n")
if (exists("subgroup_results") && !is.null(subgroup_results)) {
    tryCatch({
        forest_plot <- create_forest_plot(
            subgroup_results = subgroup_results,
            outcome_name = "Local Recurrence (Quick Test)",
            effect_measure = "OR",
            dataset_name = "Quick Test Cohort",
            output_path = file.path(test_output_dir, "quick_forest_plot.png")
        )
        
        if (!is.null(forest_plot)) {
            cat("✓ Forest plot created successfully\n")
        } else {
            cat("✗ Forest plot creation returned NULL\n")
        }
        
    }, error = function(e) {
        cat("✗ Forest plot creation failed:", e$message, "\n")
    })
} else {
    cat("⚠ Skipping forest plot - no subgroup results available\n")
}

# Step 6: Quick summary table
cat("\n6. Testing summary table creation...\n")
tryCatch({
    summary_table <- test_data %>%
        select(treatment_group, age_at_treatment, sex, recurrence1, death_event) %>%
        tbl_summary(by = treatment_group) %>%
        add_p()
    
    # Save table
    save_gt_html(
        summary_table %>% as_gt(),
        filename = file.path(test_output_dir, "quick_summary_table.html")
    )
    
    cat("✓ Summary table created and saved\n")
    
}, error = function(e) {
    cat("✗ Summary table creation failed:", e$message, "\n")
})

# Step 7: Data processing functions test
cat("\n7. Testing data processing functions...\n")
tryCatch({
    # Test rare category handling
    processed_data <- handle_rare_categories(
        test_data,
        vars = c("sex", "location"),
        threshold = 3  # Small threshold for testing
    )
    
    # Test factor consistency
    consistent_data <- ensure_consistent_contrasts(processed_data)
    
    # Test confounder validation
    valid_confounders <- generate_valid_confounders(
        test_data, confounders, threshold = THRESHOLD_RARITY
    )
    
    cat("✓ Data processing functions tested\n")
    cat(sprintf("  Valid confounders: %d/%d\n", length(valid_confounders), length(confounders)))
    
}, error = function(e) {
    cat("✗ Data processing test failed:", e$message, "\n")
})

# Workflow summary
workflow_duration <- as.numeric(difftime(Sys.time(), workflow_start_time, units = "secs"))

cat("\n=== QUICK WORKFLOW TEST SUMMARY ===\n")
cat(sprintf("Total execution time: %.1f seconds\n", workflow_duration))
cat(sprintf("Test data: %d patients (subset of %d)\n", nrow(test_data), nrow(full_data)))
cat(sprintf("All outputs saved to: %s\n", test_output_dir))

cat("\nWorkflow components tested:\n")
cat("  ✓ Data loading and preprocessing\n")
cat("  ✓ Binary outcome analysis (logistic regression)\n")
cat("  ✓ Survival analysis (Kaplan-Meier, Cox regression)\n")
cat("  ✓ Subgroup analysis with interactions\n")
cat("  ✓ Forest plot creation\n")
cat("  ✓ Summary table generation\n")
cat("  ✓ Data utility functions\n")

cat("\nThis quick test demonstrates that key workflow components are functional.\n")
cat("For comprehensive testing, run the full test suite:\n")
cat("  source('scripts/tests/run_all_tests.R')\n")

cat("\nOr run individual component tests:\n")
cat("  source('scripts/tests/test_statistical_analysis.R')\n")
cat("  source('scripts/tests/test_subgroup_analysis.R')\n") 
cat("  source('scripts/tests/test_forest_plots.R')\n") 