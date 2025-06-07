# Uveal Melanoma Treatment Outcomes Analysis
# Author: Nicholas Camarda
# Date: 5/10/2025
# Description: Analysis comparing outcomes between Gamma Knife and plaque brachytherapy
#              for uveal melanoma treatment, including both full and restricted cohort analyses
# Main script to run the analysis

########################################################
############### INPUT FILE ############################
########################################################

#' Set the filename, this will be loaded from the *data* directory
fn <- "Ocular Melanoma Master Spreadsheet REVISED FOR STATS (5-10-25, TJM).xlsx"

# Toggle logging functionality
USE_LOGS <- FALSE

########################################################
############### DATA PROCESSING #######################
########################################################

# Source the data processing script first
source("scripts/data_processing.R")

# Source the analysis script
source("scripts/uveal_melanoma_analysis.R")

# Create logs directory if it doesn't exist
if (USE_LOGS) {
    if (!dir.exists("logs")) {
        dir.create("logs", showWarnings = FALSE)
    }
}

# Set up logging if enabled
if (USE_LOGS) {
    # Create timestamp for log file
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_file <- file.path("logs", paste0("run_log_", timestamp, ".txt"))
    log_con <- file(log_file, open = "wt")
    sink(log_con)
    sink(log_con, type = "message")
}

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

# Create summary tables with organized output structure
summary_tables <- create_summary_tables(final_analytic_datasets_lst)

# Create CONSORT diagram
# TODO: Add CONSORT diagram
# log_message("Creating CONSORT diagram")
# create_consort_diagram(final_analytic_datasets_lst)

# TODO: Run analysis for each dataset

run_my_analysis <- function(dataset_name) {

    # dataset_name <- "uveal_melanoma_full_cohort"
    # dataset_name <- "uveal_melanoma_restricted_cohort"
    # dataset_name <- "uveal_melanoma_gksrs_only_cohort"

    # Get cohort info for file paths
    message(dataset_name)
    cohort_info <- get_cohort_info(dataset_name)
    tables_dir <<- file.path(cohort_info$dir, "tables")
    figures_dir <<- file.path(cohort_info$dir, "figures")
    prefix <<- cohort_info$prefix
    
    # Create organized output directory structure for tables (complex structure needed)
    output_dirs <<- create_output_structure(tables_dir)
    
    # Create simple figures directory (no complex subdirs needed)
    if (!dir.exists(figures_dir)) {
        dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
        log_message(sprintf("Created figures directory: %s", figures_dir))
    }

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
    # print(tables_dir)
    message(dataset_name)
    log_message("Calculating recurrence rates")
    recurrence_rates <- calculate_rates(
        data,
        outcome_var = "recurrence1",
        time_var = "tt_recurrence_months",
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
        time_var = "tt_mets_months",
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
        time_var = "tt_death_months",
        event_var = "death_event",
        confounders = confounders,
        ylab = "Overall Survival Probability",
        exclude_before_treatment = TRUE,
        handle_rare = TRUE,
        dataset_name = dataset_name
    )
    os_analysis

    # 1d. Progression Free Survival (includes both progression AND death)
    log_message("Analyzing progression-free survival (progression OR death)")
    pfs_analysis <- analyze_survival(
        data,
        time_var = "tt_pfs_months",
        event_var = "pfs_event",
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

    # 1f. Subgroup analysis with interaction terms
    log_message("Performing subgroup analysis with interaction terms for tumor height change")
    
    # Test treatment × subgroup interactions for tumor height change
    # Run both PRIMARY (without baseline height) and SENSITIVITY (with baseline height) analyses
    
    # PRIMARY ANALYSIS: Without baseline height adjustment
    log_message("=== PRIMARY SUBGROUP ANALYSIS (without baseline height adjustment) ===")
    primary_subgroup_results <- list()
    
    for (subgroup_var in subgroup_vars) {
        log_message(sprintf("Testing PRIMARY interaction for: %s", subgroup_var))
        
        # Test the interaction with confounders but without baseline height
        result <- test_subgroup_interaction(
            data = data,
            subgroup_var = subgroup_var,
            percentile_cut = 0.5,  # Use median split
            confounders = confounders,  # Pass confounders (will auto-exclude subgroup var)
            include_baseline_height = FALSE  # PRIMARY: no baseline height adjustment
        )
        
        # Store results
        primary_subgroup_results[[subgroup_var]] <- result
        
        # Log the interaction p-value
        if (!is.na(result$interaction_p)) {
            log_message(sprintf("  PRIMARY Interaction p-value: %.4f", result$interaction_p))
        } else {
            log_message("  PRIMARY Interaction p-value: NA (model issue)")
        }
        
        # Print subgroup effects
        print(result$subgroup_effects)
    }
    
    # SENSITIVITY ANALYSIS: With baseline height adjustment
    log_message("=== SENSITIVITY SUBGROUP ANALYSIS (with baseline height adjustment) ===")
    sensitivity_subgroup_results <- list()
    
    for (subgroup_var in subgroup_vars) {
        log_message(sprintf("Testing SENSITIVITY interaction for: %s", subgroup_var))
        
        # Test the interaction with confounders including baseline height
        result <- test_subgroup_interaction(
            data = data,
            subgroup_var = subgroup_var,
            percentile_cut = 0.5,  # Use median split
            confounders = confounders,  # Pass confounders (will auto-exclude subgroup var)
            include_baseline_height = TRUE  # SENSITIVITY: include baseline height adjustment
        )
        
        # Store results
        sensitivity_subgroup_results[[subgroup_var]] <- result
        
        # Log the interaction p-value
        if (!is.na(result$interaction_p)) {
            log_message(sprintf("  SENSITIVITY Interaction p-value: %.4f", result$interaction_p))
        } else {
            log_message("  SENSITIVITY Interaction p-value: NA (model issue)")
        }
        
        # Print subgroup effects
        print(result$subgroup_effects)
    }
    
    # Create formatted HTML tables for PRIMARY subgroup analysis
    log_message("Creating formatted PRIMARY subgroup analysis tables")
    create_subgroup_tables(
        subgroup_results = primary_subgroup_results,
        dataset_name = paste("PRIMARY -", tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))),
        subgroup_dir = output_dirs$subgroup_primary,
        prefix = paste0(prefix, "primary_")
    )
    
    # Create formatted HTML tables for SENSITIVITY subgroup analysis
    log_message("Creating formatted SENSITIVITY subgroup analysis tables")
    create_subgroup_tables(
        subgroup_results = sensitivity_subgroup_results,
        dataset_name = paste("SENSITIVITY -", tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))),
        subgroup_dir = output_dirs$subgroup_sensitivity,
        prefix = paste0(prefix, "sensitivity_")
    )
    
    # Save both sets of subgroup analysis results for this dataset
    saveRDS(primary_subgroup_results, 
            file.path(output_dirs$subgroup_primary, paste0(prefix, "primary_subgroup_interactions.rds")))
    
    saveRDS(sensitivity_subgroup_results, 
            file.path(output_dirs$subgroup_sensitivity, paste0(prefix, "sensitivity_subgroup_interactions.rds")))
    
    log_message(sprintf("Completed PRIMARY and SENSITIVITY subgroup analysis for %d variables", length(subgroup_vars)))
    
}

# Run analysis for each dataset
available_datasets <- list_available_datasets()
results <- list()

# Wrap main execution in tryCatch to ensure proper cleanup
tryCatch({
    for (i in seq_along(available_datasets)) {
        dataset <- available_datasets[i]
        log_progress(i, length(available_datasets), message = sprintf("Analyzing dataset: %s", dataset))
        results[[dataset]] <- run_my_analysis(dataset)
    }
}, finally = {
    # Clean up logging if it was enabled
    if (USE_LOGS) {
        sink(type = "message")
        sink()
        close(log_con)
        log_message("Log file closed successfully")
    }
})
