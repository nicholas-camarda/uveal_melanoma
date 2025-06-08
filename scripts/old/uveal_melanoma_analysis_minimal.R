# Uveal Melanoma Analysis - Minimal Version
# Author: Nicholas Camarda
# Date: 5/10/2025
# Description: Essential analysis functions that haven't been moved to other files
#
# NOTE: This file now contains only essential functions. Most analysis functions
# have been moved to separate, focused script files:
# - analysis_config.R: Configuration and setup functions
# - data_utilities.R: Data processing and validation functions  
# - statistical_analysis.R: Core statistical analysis functions
# - tumor_height_analysis.R: Tumor height specific analysis functions
# - vision_safety_analysis.R: Vision and safety outcome analysis functions
# - output_utilities.R: Output directory creation and table merging functions
# - subgroup_analysis.R: Subgroup interaction testing functions

# The main analysis code is now in main.R which sources all these scripts.

# This file is kept for backward compatibility but will be deprecated.
# Please use the individual focused scripts instead.

# Essential constants (duplicated in analysis_config.R)
if (!exists("THRESHOLD_RARITY")) {
    THRESHOLD_RARITY <- 5
}

if (!exists("confounders")) {
    confounders <- c(
        "age_at_diagnosis", "sex", "location",
        "optic_nerve"
    )
}

if (!exists("subgroup_vars")) {
    subgroup_vars <- c(
        "age_at_diagnosis", "sex", "location", "initial_overall_stage", "initial_t_stage",
        "initial_tumor_height", "initial_tumor_diameter", "biopsy1_gep", "optic_nerve"
    )
}

# Display message about script reorganization
log_message("NOTE: uveal_melanoma_analysis.R has been reorganized into focused scripts.")
log_message("Functions are now located in:")
log_message("  - analysis_config.R: Configuration and setup")
log_message("  - data_utilities.R: Data processing utilities")
log_message("  - statistical_analysis.R: Core statistical functions")
log_message("  - tumor_height_analysis.R: Tumor height analysis")
log_message("  - vision_safety_analysis.R: Vision and safety analysis")
log_message("  - output_utilities.R: Output and directory management")
log_message("  - subgroup_analysis.R: Subgroup interaction testing")
log_message("See main.R for the updated sourcing structure.") 