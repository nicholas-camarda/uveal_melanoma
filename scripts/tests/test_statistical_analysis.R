# Test Statistical Analysis Components
# Independent test of statistical analysis functions

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

# Create test output directory
test_output_dir <- file.path("test_output", format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== TESTING STATISTICAL ANALYSIS COMPONENTS ===\n")
cat(sprintf("Test output directory: %s\n", test_output_dir))

# Load test data
cat("\n1. Loading test data...\n")
tryCatch({
    test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
    cat(sprintf("✓ Loaded %d patients for testing\n", nrow(test_data)))
    
    # Quick data check
    cat(sprintf("  Treatment groups: %s\n", paste(names(table(test_data$treatment_group)), collapse = ", ")))
    cat(sprintf("  Follow-up range: %.1f - %.1f months\n", 
                min(test_data$tt_death_months, na.rm = TRUE),
                max(test_data$tt_death_months, na.rm = TRUE)))
    
}, error = function(e) {
    cat("✗ Error loading test data:", e$message, "\n")
    stop("Cannot proceed without test data")
})

# Test 2: Binary outcome analysis
cat("\n2. Testing binary outcome analysis...\n")
tryCatch({
    recurrence_result <- analyze_binary_outcome_rates(
        test_data,
        outcome_var = "recurrence1",
        time_var = "tt_recurrence_months", 
        event_var = "recurrence_event",
        confounders = confounders,
        exclude_before_treatment = TRUE,
        handle_rare = TRUE,
        dataset_name = "test_full"
    )
    
    cat("✓ Local recurrence analysis completed\n")
    cat(sprintf("  Crude rates: Plaque %.1f%%, GKSRS %.1f%%\n",
                recurrence_result$crude_rates$rate_plaque * 100,
                recurrence_result$crude_rates$rate_gksrs * 100))
    
}, error = function(e) {
    cat("✗ Binary outcome test failed:", e$message, "\n")
})

# Test 3: Survival analysis
cat("\n3. Testing survival analysis...\n")
tryCatch({
    os_result <- analyze_time_to_event_outcomes(
        test_data,
        time_var = "tt_death_months",
        event_var = "death_event", 
        group_var = "treatment_group",
        confounders = confounders,
        ylab = "Overall Survival Probability",
        exclude_before_treatment = TRUE,
        handle_rare = TRUE,
        dataset_name = "test_full"
    )
    
    cat("✓ Overall survival analysis completed\n")
    cat(sprintf("  Events: %d deaths out of %d patients\n",
                sum(test_data$death_event, na.rm = TRUE),
                nrow(test_data)))
    
    # Save test survival plot
    if (!is.null(os_result$km_plot)) {
        ggsave(file.path(test_output_dir, "test_survival_plot.png"), 
               os_result$km_plot, width = 10, height = 6, dpi = 300)
        cat("  Plot saved to test output\n")
    }
    
}, error = function(e) {
    cat("✗ Survival analysis test failed:", e$message, "\n")
})

# Test 4: Model fitting with confounders
cat("\n4. Testing confounder handling...\n")
tryCatch({
    # Test a simple logistic regression
    test_model_data <- test_data %>%
        filter(!is.na(recurrence1), !is.na(treatment_group)) %>%
        ensure_consistent_contrasts()
    
    # Test with confounders
    if (length(confounders) > 0) {
        valid_confounders <- generate_valid_confounders(
            test_model_data, 
            confounders, 
            threshold = THRESHOLD_RARITY
        )
        
        cat(sprintf("  Valid confounders: %d out of %d\n", 
                    length(valid_confounders), length(confounders)))
        
        if (length(valid_confounders) > 0) {
            formula_str <- paste("recurrence1 ~ treatment_group +", 
                                paste(valid_confounders, collapse = " + "))
            test_model <- glm(as.formula(formula_str), 
                             data = test_model_data, 
                             family = binomial())
            
            cat(sprintf("✓ Model fitted with %d confounders\n", length(valid_confounders)))
            cat(sprintf("  Treatment OR: %.2f (p = %.3f)\n",
                        exp(coef(test_model)["treatment_groupGKSRS"]),
                        summary(test_model)$coefficients["treatment_groupGKSRS", "Pr(>|z|)"]))
        }
    }
    
}, error = function(e) {
    cat("✗ Confounder test failed:", e$message, "\n")
})

# Test 5: Data preprocessing functions
cat("\n5. Testing data preprocessing functions...\n")
tryCatch({
    # Test rare category handling
    test_subset <- test_data[1:100, ]  # Small subset for testing
    processed_data <- handle_rare_categories(
        test_subset, 
        vars = c("sex", "location", "optic_nerve"),
        threshold = 5  # Small threshold for testing
    )
    
    cat("✓ Rare category handling completed\n")
    
    # Test factor level consistency
    consistent_data <- ensure_consistent_contrasts(processed_data)
    cat("✓ Factor level consistency ensured\n")
    
}, error = function(e) {
    cat("✗ Data preprocessing test failed:", e$message, "\n")
})

# Test 6: Table generation
cat("\n6. Testing table generation...\n")
tryCatch({
    # Create a simple summary table
    summary_table <- test_data %>%
        select(treatment_group, age_at_treatment, sex, recurrence1) %>%
        slice_head(n = 200) %>%  # Subset for faster testing
        tbl_summary(by = treatment_group) %>%
        add_p()
    
    # Save table
    save_gt_html(
        summary_table %>% as_gt(),
        filename = file.path(test_output_dir, "test_summary_table.html")
    )
    
    cat("✓ Summary table created and saved\n")
    
}, error = function(e) {
    cat("✗ Table generation test failed:", e$message, "\n")
})

# Summary
cat("\n=== STATISTICAL ANALYSIS TEST SUMMARY ===\n")
cat(sprintf("All test outputs saved to: %s\n", test_output_dir))
cat("\nTest components checked:\n")
cat("  - Data loading and basic checks\n")
cat("  - Binary outcome analysis (logistic regression)\n") 
cat("  - Survival analysis (Kaplan-Meier, Cox regression)\n")
cat("  - Confounder handling and model fitting\n")
cat("  - Data preprocessing functions\n")
cat("  - Table generation and export\n")

cat("\nTo test individual functions:\n")
cat("  analyze_binary_outcome_rates(data, ...)\n")
cat("  analyze_time_to_event_outcomes(data, ...)\n")
cat("  handle_rare_categories(data, ...)\n") 