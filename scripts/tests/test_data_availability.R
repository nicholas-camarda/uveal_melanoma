# Test Data Availability
# Quick check to verify all required data files exist for independent testing

cat("=== CHECKING DATA AVAILABILITY FOR TESTING ===\n")

# Required directories and files
required_data <- list(
    processed_datasets = c(
        "final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds",
        "final_data/Analytic Dataset/uveal_melanoma_restricted_cohort.rds"
    ),
    
    config_files = c(
        "scripts/utils/analysis_config.R",
        "scripts/utils/output_utilities.R"
    ),
    
    analysis_scripts = c(
        "scripts/data_helper/data_processing.R",
        "scripts/data_helper/data_utilities.R",
        "scripts/analysis/statistical_analysis.R",
        "scripts/analysis/tumor_height_analysis.R",
        "scripts/analysis/vision_safety_analysis.R",
        "scripts/analysis/subgroup_analysis.R",
        # "scripts/analysis/primary_outcomes_subgroup_analysis.R", # Consolidated into subgroup_analysis.R
        "scripts/visualization/forest_plot.R"
    )
)

all_available <- TRUE

# Check each category
for (category in names(required_data)) {
    cat(sprintf("\n%s:\n", toupper(gsub("_", " ", category))))
    
    for (file_path in required_data[[category]]) {
        if (file.exists(file_path)) {
            cat(sprintf("  ✓ %s\n", file_path))
        } else {
            cat(sprintf("  ✗ MISSING: %s\n", file_path))
            all_available <- FALSE
        }
    }
}

# Check for results data that might be needed for some tests
cat("\nOPTIONAL RESULTS DATA (for advanced tests):\n")
results_patterns <- c(
    "final_data/Analysis/*/tables/primary_outcomes/subgroup_analysis/*subgroup_results.rds",
    "final_data/Analysis/*/tables/primary_outcomes/tumor_height_change/subgroup_interactions/*/*subgroup_interactions.rds"
)

for (pattern in results_patterns) {
    found_files <- Sys.glob(pattern)
    if (length(found_files) > 0) {
        cat(sprintf("  ✓ Found %d files matching: %s\n", length(found_files), pattern))
    } else {
        cat(sprintf("  ✗ No files found for: %s\n", pattern))
    }
}

# Summary
cat("\n=== SUMMARY ===\n")
if (all_available) {
    cat("✓ ALL REQUIRED FILES AVAILABLE - Ready for testing!\n")
} else {
    cat("✗ SOME REQUIRED FILES MISSING - Run main analysis first or check file paths\n")
}

cat("\nTo generate missing data files, run:")
cat("\n  source('scripts/main.R')  # Full analysis")
cat("\n  # OR set RECREATE_ANALYTIC_DATASETS = TRUE in main.R\n") 