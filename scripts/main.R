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

########################################################
############### ANALYSIS SETTINGS #####################
########################################################

# Toggle logging functionality
USE_LOGS <- TRUE

# Toggle to control whether to recreate analytic datasets (default: FALSE)
# Set to TRUE if you need to reprocess raw data or if data has changed
RECREATE_ANALYTIC_DATASETS <- FALSE

# Set to FALSE to suppress detailed logging in analysis functions
VERBOSE <- TRUE 

# Set to TRUE to show all individual p-values in regression tables
# Set to FALSE to show only grouped p-values (one per variable group)
SHOW_ALL_PVALUES <- TRUE

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

# Check if we need to recreate analytic datasets
if (RECREATE_ANALYTIC_DATASETS) {
    log_message("RECREATE_ANALYTIC_DATASETS = TRUE: Creating new analytic datasets")
    
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
    
} else {
    log_message("RECREATE_ANALYTIC_DATASETS = FALSE: Skipping analytic dataset creation")
    log_message("Using existing datasets from final_data/Analytic Dataset/")
    log_message("Set RECREATE_ANALYTIC_DATASETS = TRUE if you need to reprocess raw data")
}

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

    ########################################################
    ############### STEP 1: PRIMARY OUTCOMES ###############
    ########################################################

    message("=== STARTING STEP 1: PRIMARY OUTCOMES ANALYSIS ===")

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

    # 1f. Comprehensive Subgroup analysis for all primary outcomes
    log_message("Performing comprehensive subgroup analysis for all primary outcomes")
    
    # Create directories for subgroup analysis outputs
    subgroup_base_dir <- file.path(tables_dir, "comprehensive_subgroup_analysis")
    subgroup_figures_dir <- file.path(figures_dir, "comprehensive_subgroup_analysis")
    
    if (!dir.exists(subgroup_base_dir)) {
        dir.create(subgroup_base_dir, recursive = TRUE)
    }
    if (!dir.exists(subgroup_figures_dir)) {
        dir.create(subgroup_figures_dir, recursive = TRUE)
    }
    
    # Perform subgroup analysis for survival outcomes
    log_message("=== SUBGROUP ANALYSIS FOR SURVIVAL OUTCOMES ===")
    
    # Overall Survival
    log_message("Performing subgroup analysis for Overall Survival")
    os_subgroup_results <- perform_survival_subgroup_analysis(
        data = data,
        time_var = "tt_death",
        event_var = "death_event", 
        subgroup_vars = subgroup_vars,
        confounders = confounders,
        outcome_name = "Overall Survival"
    )
    
    # Create forest plot for OS
    os_forest_plot <- create_forest_plot(
        subgroup_results = os_subgroup_results,
        outcome_name = "Overall Survival",
        effect_measure = "HR",
        dataset_name = dataset_name,
        output_path = file.path(subgroup_figures_dir, paste0(prefix, "overall_survival_forest_plot.png"))
    )
    
    # Progression-Free Survival  
    log_message("Performing subgroup analysis for Progression-Free Survival")
    pfs_subgroup_results <- perform_survival_subgroup_analysis(
        data = data,
        time_var = "tt_progression_death",
        event_var = "progression_death_event",
        subgroup_vars = subgroup_vars,
        confounders = confounders,
        outcome_name = "Progression-Free Survival"
    )
    
    # Create forest plot for PFS
    pfs_forest_plot <- create_forest_plot(
        subgroup_results = pfs_subgroup_results,
        outcome_name = "Progression-Free Survival",
        effect_measure = "HR",
        dataset_name = dataset_name,
        output_path = file.path(subgroup_figures_dir, paste0(prefix, "progression_free_survival_forest_plot.png"))
    )
    
    # Perform subgroup analysis for binary outcomes
    log_message("=== SUBGROUP ANALYSIS FOR BINARY OUTCOMES ===")
    
    # Local Recurrence
    log_message("Performing subgroup analysis for Local Recurrence")
    recurrence_subgroup_results <- perform_binary_subgroup_analysis(
        data = data,
        outcome_var = "recurrence1",
        subgroup_vars = subgroup_vars,
        confounders = confounders,
        outcome_name = "Local Recurrence"
    )
    
    # Create forest plot for Recurrence
    recurrence_forest_plot <- create_forest_plot(
        subgroup_results = recurrence_subgroup_results,
        outcome_name = "Local Recurrence",
        effect_measure = "OR",
        dataset_name = dataset_name,
        output_path = file.path(subgroup_figures_dir, paste0(prefix, "local_recurrence_forest_plot.png"))
    )
    
    # Metastatic Progression
    log_message("Performing subgroup analysis for Metastatic Progression")
    mets_subgroup_results <- perform_binary_subgroup_analysis(
        data = data,
        outcome_var = "mets_progression",
        subgroup_vars = subgroup_vars,
        confounders = confounders,
        outcome_name = "Metastatic Progression"
    )
    
    # Create forest plot for Metastatic Progression
    mets_forest_plot <- create_forest_plot(
        subgroup_results = mets_subgroup_results,
        outcome_name = "Metastatic Progression",
        effect_measure = "OR",
        dataset_name = dataset_name,
        output_path = file.path(subgroup_figures_dir, paste0(prefix, "metastatic_progression_forest_plot.png"))
    )
    
    # Tumor Height Change - use existing function but create forest plot
    log_message("=== SUBGROUP ANALYSIS FOR TUMOR HEIGHT CHANGE ===")
    
    # Test treatment × subgroup interactions for tumor height change
    output_dirs <- create_output_structure(tables_dir)
    
    log_message("=== PRIMARY SUBGROUP ANALYSIS (without baseline height adjustment) ===")
    primary_subgroup_results <- list()
    
    for (subgroup_var in subgroup_vars) {
        log_message(sprintf("Testing PRIMARY interaction for: %s", subgroup_var))
        
        # Test the interaction with confounders but without baseline height
        result <- test_subgroup_interaction(
            data = data,
            subgroup_var = subgroup_var,
            confounders = confounders,  # Pass confounders (will auto-exclude subgroup var)
            include_baseline_height = FALSE  # PRIMARY analysis
        )
        
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
    
    # Create forest plot for tumor height change
    height_forest_plot <- create_forest_plot(
        subgroup_results = primary_subgroup_results,
        outcome_name = "Tumor Height Change",
        effect_measure = "MD",
        dataset_name = dataset_name,
        output_path = file.path(subgroup_figures_dir, paste0(prefix, "tumor_height_change_forest_plot.png"))
    )
    
    # Create formatted HTML tables for PRIMARY subgroup analysis
    log_message("Creating formatted PRIMARY subgroup analysis tables")
    create_subgroup_tables(
        subgroup_results = primary_subgroup_results,
        dataset_name = dataset_name,
        subgroup_dir = output_dirs$subgroup_primary,
        prefix = prefix
    )
    
    # Save subgroup analysis results for this dataset
    saveRDS(os_subgroup_results,
            file.path(subgroup_base_dir, paste0(prefix, "overall_survival_subgroup_results.rds")))
    saveRDS(pfs_subgroup_results,
            file.path(subgroup_base_dir, paste0(prefix, "progression_free_survival_subgroup_results.rds")))
    saveRDS(recurrence_subgroup_results,
            file.path(subgroup_base_dir, paste0(prefix, "local_recurrence_subgroup_results.rds")))
    saveRDS(mets_subgroup_results,
            file.path(subgroup_base_dir, paste0(prefix, "metastatic_progression_subgroup_results.rds")))
    saveRDS(primary_subgroup_results,
            file.path(output_dirs$subgroup_primary, paste0(prefix, "primary_subgroup_interactions.rds")))
    
    log_message(sprintf("Completed comprehensive subgroup analysis for %s", dataset_name))
    
    ########################################################
    ############### STEP 2: SAFETY/TOXICITY ###############
    ########################################################
    
    message("=== STARTING STEP 2: SAFETY/TOXICITY ANALYSIS ===")
    
    # 2a. Vision changes analysis (similar to tumor height changes)
    log_message("Analyzing vision changes")
    vision_changes <- analyze_vision_changes(data)
    vision_changes
    
    # 2b. Rates of radiation sequelae
    log_message("Analyzing radiation sequelae rates")
    
    # 2b1. Retinopathy
    log_message("Analyzing retinopathy rates")
    retinopathy_rates <- analyze_radiation_sequelae(
        data = data,
        sequela_type = "retinopathy",
        confounders = confounders,
        dataset_name = dataset_name
    )
    retinopathy_rates
    
    # 2b2. Neovascular glaucoma (NVG)
    log_message("Analyzing neovascular glaucoma (NVG) rates")
    nvg_rates <- analyze_radiation_sequelae(
        data = data,
        sequela_type = "nvg",
        confounders = confounders,
        dataset_name = dataset_name
    )
    nvg_rates
    
    # 2b3. Serous retinal detachment (SRD) - only radiation-induced
    log_message("Analyzing serous retinal detachment (SRD) rates - radiation-induced only")
    srd_rates <- analyze_radiation_sequelae(
        data = data,
        sequela_type = "srd",
        confounders = confounders,
        dataset_name = dataset_name
    )
    srd_rates
    
    log_message("=== COMPLETED STEP 2: SAFETY/TOXICITY ANALYSIS ===")
    
    ########################################################
    ############### STEP 3: REPEAT RADIATION ##############
    ########################################################
    
    message("=== STARTING STEP 3: REPEAT RADIATION EFFICACY ===")
    
    # 3a. Progression-Free Survival-2 (PFS-2) for recurrent patients
    log_message("Analyzing PFS-2 for recurrent patients")
    pfs2_results <- analyze_pfs2(data, confounders = confounders, dataset_name = dataset_name)
    pfs2_results
    
    log_message("=== COMPLETED STEP 3: REPEAT RADIATION EFFICACY ===")
    
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
    
    # After all individual analyses are complete, create merged tables
    log_message("=== CREATING MERGED TABLES FOR COLLABORATOR ===")
    
    # Load the full and restricted cohort data
    full_cohort_data <- readRDS(file.path("final_data", "Analytic Dataset", "uveal_melanoma_full_cohort.rds"))
    restricted_cohort_data <- readRDS(file.path("final_data", "Analytic Dataset", "uveal_melanoma_restricted_cohort.rds"))
    
    # Create merged tables showing both cohorts side by side
    merge_cohort_tables(
        full_cohort_data = full_cohort_data,
        restricted_cohort_data = restricted_cohort_data,
        output_path = file.path("final_data", "Analysis", "merged_tables")
    )
    
    log_message("=== COMPLETED ALL ANALYSES INCLUDING MERGED TABLES ===")
    
    log_message("=== ANALYSIS PIPELINE COMPLETED SUCCESSFULLY ===")

    #########################################################################
    ################ COMPREHENSIVE SUBGROUP ANALYSIS SUMMARY ###############
    #########################################################################

    log_message("=== CREATING COMPREHENSIVE SUBGROUP ANALYSIS SUMMARY ===")

    # Create combined analysis directory
    combined_analysis_dir <- file.path(OUTPUT_DIR, "combined_cohort_analysis")
    combined_figures_dir <- file.path(combined_analysis_dir, "figures")
    combined_tables_dir <- file.path(combined_analysis_dir, "tables")

    if (!dir.exists(combined_analysis_dir)) {
        dir.create(combined_analysis_dir, recursive = TRUE)
    }
    if (!dir.exists(combined_figures_dir)) {
        dir.create(combined_figures_dir, recursive = TRUE)
    }
    if (!dir.exists(combined_tables_dir)) {
        dir.create(combined_tables_dir, recursive = TRUE)
    }

    # Load results from both cohorts if they exist
    full_cohort_dir <- file.path(OUTPUT_DIR, "uveal_full", "tables", "comprehensive_subgroup_analysis")
    restricted_cohort_dir <- file.path(OUTPUT_DIR, "uveal_restricted", "tables", "comprehensive_subgroup_analysis")

    if (dir.exists(full_cohort_dir) && dir.exists(restricted_cohort_dir)) {
        
        log_message("Creating combined forest plots and tables for full vs restricted cohorts")
        
        # Define outcomes to process
        outcomes_to_process <- list(
            list(
                name = "Overall Survival",
                filename = "overall_survival_subgroup_results.rds",
                effect_measure = "HR"
            ),
            list(
                name = "Progression-Free Survival",
                filename = "progression_free_survival_subgroup_results.rds",
                effect_measure = "HR"
            ),
            list(
                name = "Local Recurrence",
                filename = "local_recurrence_subgroup_results.rds",
                effect_measure = "OR"
            ),
            list(
                name = "Metastatic Progression",
                filename = "metastatic_progression_subgroup_results.rds",
                effect_measure = "OR"
            ),
            list(
                name = "Tumor Height Change",
                filename = "primary_subgroup_interactions.rds",
                effect_measure = "MD",
                subdir = "primary_outcomes/tumor_height_change/subgroup_interactions/without_baseline_height"
            )
        )
        
        # Process each outcome
        for (outcome in outcomes_to_process) {
            
            log_message(sprintf("Processing combined analysis for: %s", outcome$name))
            
            tryCatch({
                # Load results from both cohorts
                if (!is.null(outcome$subdir)) {
                    # Special case for tumor height change
                    full_results_path <- file.path(OUTPUT_DIR, "uveal_full", "tables", outcome$subdir, paste0("uveal_full_", outcome$filename))
                    restricted_results_path <- file.path(OUTPUT_DIR, "uveal_restricted", "tables", outcome$subdir, paste0("uveal_restricted_", outcome$filename))
                } else {
                    full_results_path <- file.path(full_cohort_dir, paste0("uveal_full_", outcome$filename))
                    restricted_results_path <- file.path(restricted_cohort_dir, paste0("uveal_restricted_", outcome$filename))
                }
                
                if (file.exists(full_results_path) && file.exists(restricted_results_path)) {
                    
                    full_results <- readRDS(full_results_path)
                    restricted_results <- readRDS(restricted_results_path)
                    
                    # Create combined forest plot
                    combined_plot_path <- file.path(combined_figures_dir, paste0(gsub(" ", "_", tolower(outcome$name)), "_combined_forest_plot.png"))
                    combined_forest_plot <- create_combined_forest_plot(
                        full_results = full_results,
                        restricted_results = restricted_results,
                        outcome_name = outcome$name,
                        effect_measure = outcome$effect_measure,
                        output_path = combined_plot_path
                    )
                    
                    # Create comprehensive summary table
                    table_path <- file.path(combined_tables_dir, paste0(gsub(" ", "_", tolower(outcome$name)), "_comprehensive_summary.html"))
                    comprehensive_table <- create_comprehensive_subgroup_table(
                        full_results = full_results,
                        restricted_results = restricted_results,
                        outcome_name = outcome$name,
                        effect_measure = outcome$effect_measure,
                        output_path = table_path
                    )
                    
                    log_message(sprintf("Created combined analysis for %s", outcome$name))
                    
                } else {
                    log_message(sprintf("Skipping %s - results files not found", outcome$name))
                    log_message(sprintf("  Full path: %s (exists: %s)", full_results_path, file.exists(full_results_path)))
                    log_message(sprintf("  Restricted path: %s (exists: %s)", restricted_results_path, file.exists(restricted_results_path)))
                }
                
            }, error = function(e) {
                warning(sprintf("Error creating combined analysis for %s: %s", outcome$name, e$message))
            })
        }
        
        log_message("Comprehensive subgroup analysis summary completed successfully!")
        log_message(sprintf("Results saved to: %s", combined_analysis_dir))
        
    } else {
        log_message("Skipping combined analysis - both full and restricted cohort results not available")
        log_message(sprintf("Full cohort dir exists: %s", dir.exists(full_cohort_dir)))
        log_message(sprintf("Restricted cohort dir exists: %s", dir.exists(restricted_cohort_dir)))
    }

    log_message("=== ALL ANALYSES COMPLETED ===")
    
}, finally = {
    # Clean up logging if it was enabled
    if (USE_LOGS) {
        sink(type = "message")
        sink()
        close(log_con)
        log_message("Log file closed successfully")
    }
})
