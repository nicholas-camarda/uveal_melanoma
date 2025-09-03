# Uveal Melanoma Treatment Outcomes Analysis
# Author: Nicholas Camarda
# Description: Analysis comparing outcomes between Gamma Knife and PBT brachytherapy
#              for uveal melanoma treatment, including both full and restricted cohort analyses
# Main script to run the analysis

# Clear the environment
rm(list = ls())

# Source the analysis configuration first (all global variables), required libraries, and helper functions
source(here::here("scripts", "load_all.R"))

# Initialize logging
if (USE_LOGS) {
    # Create logs directory if it doesn't exist
    if (!dir.exists(LOGS_DIR)) {
        dir.create(LOGS_DIR, showWarnings = FALSE)
    }
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_file <- file.path(LOGS_DIR, paste0("run_log_", timestamp, ".txt"))
    setup_logging(log_path = log_file, level = "INFO", progress = interactive(), quiet_html = TRUE)
} else {
    setup_logging(log_path = NULL, level = "INFO", progress = interactive(), quiet_html = TRUE)
}

# Uncomment the appropriate line below to run:
# Run full analysis (all objectives, all datasets, merged tables)
main_execution()

# DEBUG: We are running specific objectives for all cohorts to debug each objective in isolation
# Run specific objective for specific dataset and objective number
# my_cohort <- "uveal_melanoma_full_cohort"
# # my_cohort <- uveal_melanoma_restricted_cohort
# # my_cohort <- uveal_melanoma_gksrs_only_cohort

# # 0 for data processing
# invisible(with_log_context(cohort = my_cohort, objective = "objective_0_data_processing", subobjective = NULL, expr = {
#     run_specific_objective(my_cohort, 0)
# }))
# then, eg. 1 for primary outcomes, 2 for safety/toxicity, 3 for repeat radiation efficacy, 4 for GEP validation
# invisible(with_log_context(cohort = my_cohort, objective = "objective_1_primary_outcomes", subobjective = NULL, expr = {
#     run_specific_objective(my_cohort, 1)
# }))

# invisible(with_log_context(cohort = my_cohort, objective = "objective_2_safety_toxicity", subobjective = NULL, expr = {
#     run_specific_objective(my_cohort, 2)
# }))

# invisible(with_log_context(cohort = my_cohort, objective = "objective_3_repeat_radiation", subobjective = NULL, expr = {
#     run_specific_objective(my_cohort, 3)
# }))

# invisible(with_log_context(cohort = my_cohort, objective = "objective_4_gep_analysis", subobjective = NULL, expr = {
#     run_specific_objective(my_cohort, 4)
# }))
