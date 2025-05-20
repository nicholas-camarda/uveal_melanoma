# Uveal Melanoma Treatment Outcomes Analysis
# Author: Nicholas Camarda
# Date: 5/10/2025
# Description: Analysis comparing outcomes between Gamma Knife and plaque brachytherapy
#              for uveal melanoma treatment, including both full and restricted cohort analyses
# Main script to run the analysis

########################################################
############### DATA PROCESSING #######################
########################################################

# Set the filename
fn <- "Ocular Melanoma Master Spreadsheet REVISED FOR STATS (5-10-25, TJM).xlsx"

# Source the data processing script
source("scripts/data_processing.R")

# Load and clean raw data
cleaned_data <- load_and_clean_data(filename = fn)

# Create derived variables BEFORE splitting into cohorts
derived_data <- create_derived_variables(cleaned_data)

# Prepare factor levels
factored_data <- prepare_factor_levels(derived_data)

# Apply inclusion/exclusion criteria (split into cohorts)
final_analytic_datasets_lst <- apply_criteria(factored_data)

# Save each cohort separately
save_cohorts(final_analytic_datasets_lst)

# Create summary tables
summary_tables <- create_summary_tables(final_analytic_datasets_lst)

# Create CONSORT diagram
# TODO: Add CONSORT diagram
# log_message("Creating CONSORT diagram")
# create_consort_diagram(final_analytic_datasets_lst)

########################################################
############### ANALYSIS RUN ##########################
########################################################

# Source the analysis script
source("scripts/uveal_melanoma_analysis.R")

# TODO: Run analysis for each dataset

run_my_analysis <- function(dataset_name) {

    # dataset_name <- "uveal_melanoma_full_cohort"
    # dataset_name <- "uveal_melanoma_restricted_cohort"
    # dataset_name <- "uveal_melanoma_gksrs_only_cohort"

    # Get cohort info for file paths
    cohort_info <- get_cohort_info(dataset_name)
    tables_dir <- file.path(cohort_info$dir, "tables")
    figures_dir <- file.path(cohort_info$dir, "figures")
    prefix <- cohort_info$prefix

    # Load analytic dataset
    log_message("Loading analytic dataset")
    data <- readRDS(file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds")))
    log_message(sprintf("Loaded %d patients", nrow(data)))

    # # Summarize key variables before analysis
    # summarize_data(data)

    # Show confounders
    log_message(sprintf("Using %d confounders for adjustment", length(confounders)))
    print(confounders)

    # 1a. Rates of recurrence
    log_message("Calculating recurrence rates")
    recurrence_rates <- calculate_rates(
        data,
        outcome_var = "recurrence1",
        time_var = "tt_recurrence",
        event_var = "recurrence_event",
        confounders = confounders,
        exclude_before_treatment = TRUE,
        handle_rare = TRUE,
        dataset_name = dataset_name
    )
    recurrence_rates

    # 1b. Rates of metastatic progression
    log_message("Calculating metastatic progression rates")
    mets_rates <- calculate_rates(
        data,
        outcome_var = "mets_progression",
        time_var = "tt_mets",
        event_var = "mets_event",
        confounders = confounders,
        exclude_before_treatment = TRUE,
        handle_rare = TRUE,
        dataset_name = dataset_name
    )
    mets_rates

    # 1c. Overall Survival
    log_message("Analyzing overall survival")
    os_analysis <- analyze_survival(
        data,
        time_var = "tt_death",
        event_var = "death_event",
        confounders = confounders,
        ylab = "Overall Survival Probability",
        exclude_before_treatment = TRUE,
        handle_rare = TRUE,
        dataset_name = dataset_name
    )
    os_analysis

    # 1d. Progression Free Survival
    log_message("Analyzing progression-free survival")
    pfs_analysis <- analyze_survival(
        data,
        time_var = "tt_recurrence",
        event_var = "recurrence_event",
        confounders = confounders,
        ylab = "Progression-Free Survival Probability",
        exclude_before_treatment = TRUE,
        handle_rare = TRUE,
        dataset_name = dataset_name
    )
    pfs_analysis

    # 1e. Tumor height changes
    log_message("Analyzing tumor height changes")
    height_changes <- analyze_tumor_height_changes(data)
    height_changes

    # 1f. Subgroup analysis
    # DEBUG
    # make_subgroup_tbl(data, subgroup_var = c("age_at_diagnosis", "sex", "location", "initial_t_stage"), efficacy_var = "recurrence1", group_var = "treatment_group")
}

# # Run analysis for each dataset
available_datasets <- list_available_datasets()
results <- list()
for (i in seq_along(available_datasets)) {
    dataset <- available_datasets[i]
    log_progress(i, length(available_datasets), message = sprintf("Analyzing dataset: %s", dataset))
    results[[dataset]] <- run_my_analysis(dataset)
}
