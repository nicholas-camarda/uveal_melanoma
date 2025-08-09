# Uveal Melanoma Treatment Outcomes Analysis
# Author: Nicholas Camarda
# Description: Analysis comparing outcomes between Gamma Knife and plaque brachytherapy
#              for uveal melanoma treatment, including both full and restricted cohort analyses
# Main script to run the analysis

# Source the analysis configuration first (all global variables), required libraries, and helper functions
source("scripts/utils/all_helper_functions.R")

# Set up logging if enabled
if (USE_LOGS) {
    # Create logs directory if it doesn't exist
    if (!dir.exists(LOGS_DIR)) {
        dir.create(LOGS_DIR, showWarnings = FALSE)
    }

    # Create timestamp for log file
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_file <- file.path(LOGS_DIR, paste0("run_log_", timestamp, ".txt"))
    log_con <- file(log_file, open = "wt")
    sink(log_con)
    sink(log_con, type = "message")
}

# Uncomment the appropriate line below to run:
# Run full analysis (all objectives, all datasets)
# main_execution()

# DEBUG: We are running specific objectives for all cohorts to debug each objective in isolation
# Run specific objective for specific dataset and objective number,
# 0 for data processing
run_specific_objective("uveal_melanoma_full_cohort", 0)
# then, eg. 1 for primary outcomes, 2 for safety/toxicity, 3 for repeat radiation efficacy, 4 for GEP validation
run_specific_objective("uveal_melanoma_full_cohort", 1)
# run_specific_objective("uveal_melanoma_restricted_cohort", 1)
# run_specific_objective("uveal_melanoma_gksrs_only_cohort", 1)
run_specific_objective("uveal_melanoma_full_cohort", 2)
# run_specific_objective("uveal_melanoma_restricted_cohort", 2)
# run_specific_objective("uveal_melanoma_gksrs_only_cohort", 2)
run_specific_objective("uveal_melanoma_full_cohort", 3)
# run_specific_objective("uveal_melanoma_restricted_cohort", 3)
# run_specific_objective("uveal_melanoma_gksrs_only_cohort", 3)
run_specific_objective("uveal_melanoma_full_cohort", 4)
# run_specific_objective("uveal_melanoma_restricted_cohort", 4)
# run_specific_objective("uveal_melanoma_gksrs_only_cohort", 4)

# Close logging if enabled
if (USE_LOGS) {
    sink(type = "message")
    sink()
    close(log_con)
    log_enhanced("Log file closed successfully")
}
