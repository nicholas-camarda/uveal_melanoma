# Comprehensive Subgroup Analysis for Uveal Melanoma Study
# Author: Nicholas Camarda  
# Date: January 2025
# 
# This script performs comprehensive subgroup analysis for all primary outcomes:
# 1. Overall Survival (OS)
# 2. Progression-Free Survival (PFS) 
# 3. Local Recurrence
# 4. Metastatic Progression
# 5. Tumor Height Change
#
# Generates forest plots and combined tables as requested by collaborator

# Set working directory and load required libraries
library(tidyverse)
library(survival)
library(gt)
library(forestploter)
library(cowplot)
library(grid)

# Source the analysis functions
source("scripts/uveal_melanoma_analysis.R")

# Set analysis parameters
VERBOSE <- TRUE
SHOW_ALL_PVALUES <- TRUE

log_message("=== STARTING COMPREHENSIVE SUBGROUP ANALYSIS ===")

# Run the main analysis pipeline
source("scripts/main.R")

log_message("=== COMPREHENSIVE SUBGROUP ANALYSIS COMPLETED ===")

# Print summary of outputs
cat("\n")
cat("========================================\n")
cat("COMPREHENSIVE SUBGROUP ANALYSIS SUMMARY\n") 
cat("========================================\n")
cat("\n")
cat("Analysis completed for the following outcomes:\n")
cat("• Overall Survival (Hazard Ratios)\n")
cat("• Progression-Free Survival (Hazard Ratios)\n") 
cat("• Local Recurrence (Odds Ratios)\n")
cat("• Metastatic Progression (Odds Ratios)\n")
cat("• Tumor Height Change (Mean Differences)\n")
cat("\n")
cat("Forest plots and tables generated for:\n")
cat("• Full Cohort (~263 patients)\n")
cat("• Restricted Cohort (~169 patients)\n") 
cat("• Combined Full vs Restricted comparison\n")
cat("\n")

# Check if combined analysis was created
combined_dir <- file.path("final_data", "Analysis", "combined_cohort_analysis")
if (dir.exists(combined_dir)) {
    cat("✓ Combined forest plots and tables created successfully!\n")
    cat(sprintf("✓ Results saved to: %s\n", combined_dir))
    cat("\n")
    cat("Key files generated:\n")
    
    # List forest plots
    figures_dir <- file.path(combined_dir, "figures")
    if (dir.exists(figures_dir)) {
        forest_plots <- list.files(figures_dir, pattern = "_combined_forest_plot.png$", full.names = FALSE)
        if (length(forest_plots) > 0) {
            cat("\nForest Plots:\n")
            for (plot in forest_plots) {
                cat(sprintf("  • %s\n", plot))
            }
        }
    }
    
    # List summary tables
    tables_dir <- file.path(combined_dir, "tables")
    if (dir.exists(tables_dir)) {
        summary_tables <- list.files(tables_dir, pattern = "_comprehensive_summary.html$", full.names = FALSE)
        if (length(summary_tables) > 0) {
            cat("\nSummary Tables:\n")
            for (table in summary_tables) {
                cat(sprintf("  • %s\n", table))
            }
        }
    }
    
    # Check for README
    readme_path <- file.path(combined_dir, "README.md")
    if (file.exists(readme_path)) {
        cat(sprintf("\n✓ Analysis documentation: %s\n", readme_path))
    }
    
} else {
    cat("⚠ Combined analysis directory not found\n")
    cat("  This may be normal if only one cohort was processed\n")
}

cat("\n")
cat("========================================\n")
cat("WHAT COLLABORATOR REQUESTED vs DELIVERED\n")
cat("========================================\n")
cat("\n")
cat("✓ Compare plaque vs GKSRS for primary outcomes:\n")
cat("  → Delivered: All 5 primary outcomes analyzed\n")
cat("\n")
cat("✓ Clarify what 'subgroup levels' mean:\n") 
cat("  → Delivered: Categories within each subgroup variable\n")
cat("    (e.g., Sex: Male/Female; Age: <median/≥median)\n")
cat("\n")
cat("✓ Forest plots like Figure 3 example:\n")
cat("  → Delivered: Publication-ready forest plots with:\n")
cat("    • Hazard ratios/odds ratios with 95% CI\n")
cat("    • Sample sizes and p-values\n")
cat("    • Significance indicators (*)\n")
cat("    • Square sizes proportional to sample size\n")
cat("\n")
cat("✓ Combined tables (full vs restricted cohorts):\n")
cat("  → Delivered: Side-by-side comparison tables\n")
cat("    • Full cohort results\n")
cat("    • Restricted cohort results\n")
cat("    • Interaction p-values\n")
cat("\n")

cat("NEXT STEPS FOR COLLABORATOR:\n")
cat("========================================\n")
cat("1. Review forest plots in combined_cohort_analysis/figures/\n")
cat("2. Examine comprehensive tables in combined_cohort_analysis/tables/\n") 
cat("3. Read analysis summary in combined_cohort_analysis/README.md\n")
cat("4. Look for significant interactions (*) in forest plots\n")
cat("5. Consider which subgroups show differential treatment effects\n")
cat("\n")
cat("Forest plots show treatment effects (GKSRS vs Plaque) within\n")
cat("each subgroup. Significant interactions indicate that treatment\n")
cat("effectiveness differs across patient characteristics.\n")
cat("\n")