# Uveal Melanoma Treatment Outcomes Analysis
# Author: Nicholas Camarda
# Description: Analysis comparing outcomes between Gamma Knife and PBT brachytherapy
#              for uveal melanoma treatment, including both full and restricted cohort analyses
# Main script to run the analysis

# Clear the environment
rm(list = ls())

# Source the analysis configuration first (all global variables), required libraries, and helper functions
source(here::here("scripts", "load_all.R"))

# All cohorts, all objectives
main_execution()

# Set these vectors to choose which cohorts and objectives to run.
# Examples:
# cohorts_to_run <- c("uveal_melanoma_full_cohort", "uveal_melanoma_restricted_cohort", "uveal_melanoma_gksrs_only_cohort") # Run all cohorts
# objectives_to_run <- c(1) # Run only objectives 1 and 4
# cohorts_to_run <- c("uveal_melanoma_full_cohort", "uveal_melanoma_gksrs_only_cohort")
# objectives_to_run <- c(4)

# invisible(run_selected_objectives(cohorts_to_run, objectives_to_run))

# invisible(with_log_context(cohort = "all_cohorts", objective = "merged_tables", subobjective = NULL, expr = {
#     merge_baseline_tables()
# }))