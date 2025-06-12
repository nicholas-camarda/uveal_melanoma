# Uveal Melanoma Treatment Outcomes Analysis
# Author: Nicholas Camarda
# Date: 5/10/2025
# Description: Analysis comparing outcomes between Gamma Knife and plaque brachytherapy
#              for uveal melanoma treatment, including both full and restricted cohort analyses
# Main script to run the analysis

########################################################
############### REQUIRED LIBRARIES ####################
########################################################

# Load required libraries
library(tidyverse) # For data manipulation and visualization
library(readxl) # For reading Excel files
library(writexl) # For writing Excel files
library(lubridate) # Date handling
library(gtsummary) # For creating publication-ready tables
library(janitor) # Data cleaning
library(survival) # For survival analysis
library(survminer) # For survival visualization
library(gt) # For table formatting
library(forestploter) # For forest plots
library(grid) # for unit()
library(cowplot) # for combining plots
library(DiagrammeR) # Creating CONSORT diagram
library(DiagrammeRsvg) # SVG export for CONSORT diagram
library(rsvg) # PNG conversion for CONSORT diagram
library(survRM2, quietly = TRUE)

# Source the analysis configuration first (contains all global variables)
source("scripts/utils/analysis_config.R")

# Create necessary directories now that libraries are loaded
dir.create(PROCESSED_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

########################################################
############### ANALYSIS SETTINGS #####################
########################################################

# Toggle logging functionality
USE_LOGS <- TRUE

# Toggle to control whether to recreate analytic datasets (default: FALSE)
# Set to TRUE if you need to reprocess raw data or if data has changed
RECREATE_ANALYTIC_DATASETS <- TRUE

# Set to FALSE to suppress detailed logging in analysis functions
VERBOSE <- TRUE 

# Set to TRUE to show all individual p-values in regression tables
# Set to FALSE to show only grouped p-values (one per variable group)
SHOW_ALL_PVALUES <- TRUE

########################################################
############### DATA PROCESSING #######################
########################################################

# Source the data processing script
source("scripts/data_helper/data_processing.R")

# Source the utility and helper scripts
source("scripts/data_helper/data_utilities.R")
source("scripts/utils/output_utilities.R")

# Source the main analysis function scripts
source("scripts/analysis/statistical_analysis.R")
source("scripts/analysis/tumor_height_analysis.R") 
source("scripts/analysis/vision_safety_analysis.R")
source("scripts/analysis/subgroup_analysis.R")
# Primary outcomes subgroup analysis functions now in subgroup_analysis.R

# Source the forest plot script
source("scripts/visualization/forest_plot.R")

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
    log_section_start("DATA PREPROCESSING PHASE")
    data_start_time <- Sys.time()
    
    log_enhanced("RECREATE_ANALYTIC_DATASETS = TRUE: Creating new analytic datasets", level = "INFO")
    
    # Load and clean raw data
    log_function("load_and_clean_data", paste("Input file:", INPUT_FILENAME))
    cleaned_data <- load_and_clean_data(filename = INPUT_FILENAME)

    # Create derived variables BEFORE splitting into cohorts
    log_function("create_derived_variables", "Creating PFS-2 variables and other derived measures")
    derived_data <- create_derived_variables(cleaned_data)

    # Prepare factor levels
    log_function("prepare_factor_levels", "Setting up factor levels for analysis")
    factored_data <- prepare_factor_levels(derived_data)

    # Apply inclusion/exclusion criteria (split into cohorts)
    log_function("apply_criteria", "Applying inclusion/exclusion criteria and creating cohorts")
    final_analytic_datasets_lst <- apply_criteria(factored_data)

    # Save each cohort separately
    log_function("save_cohorts", "Saving processed cohorts to RDS files")
    save_cohorts(final_analytic_datasets_lst)

    # Create summary tables with organized output structure
    log_function("create_summary_tables", "Creating baseline characteristics tables")
    
    # Create cohort-specific output structures for baseline characteristics
    temp_output_dirs_by_cohort <- list()
    for (cohort_name in names(final_analytic_datasets_lst)) {
        # Determine cohort directory name
        cohort_dir_name <- case_when(
            grepl("full", cohort_name) ~ "uveal_full",
            grepl("restricted", cohort_name) ~ "uveal_restricted", 
            grepl("gksrs", cohort_name) ~ "gksrs",
            TRUE ~ cohort_name
        )
        
        # Create cohort-specific directory structure
        cohort_base_dir <- file.path("final_data/Analysis", cohort_dir_name)
        temp_output_dirs_by_cohort[[cohort_name]] <- create_output_structure(cohort_base_dir)
    }
    
    summary_tables <- create_summary_tables(final_analytic_datasets_lst, temp_output_dirs_by_cohort)

    # Create CONSORT diagram
    # TODO: Add CONSORT diagram
    # log_function("create_consort_diagram", "Creating patient flow diagram")
    # create_consort_diagram(final_analytic_datasets_lst)
    
    log_section_complete("DATA PREPROCESSING PHASE", data_start_time)
    
} else {
    log_section_start("DATA LOADING PHASE")
    log_enhanced("RECREATE_ANALYTIC_DATASETS = FALSE: Skipping analytic dataset creation", level = "INFO")
    log_enhanced("Using existing datasets from final_data/Analytic Dataset/", level = "INFO")
    log_enhanced("Set RECREATE_ANALYTIC_DATASETS = TRUE if you need to reprocess raw data", level = "INFO")
}

# TODO: Run analysis for each dataset

run_my_analysis <- function(dataset_name) {
    analysis_start_time <- Sys.time()
    
    # dataset_name <- "uveal_melanoma_full_cohort"

    # Clean dataset name for display
    display_name <- tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))
    log_section_start("STATISTICAL ANALYSIS", display_name)

    # Set up cohort information
    cohort_info <- case_when(
        grepl("full", dataset_name) ~ list(prefix = "full_cohort_", dir_name = "uveal_full"),
        grepl("restricted", dataset_name) ~ list(prefix = "restricted_cohort_", dir_name = "uveal_restricted"), 
        grepl("gksrs", dataset_name) ~ list(prefix = "gksrs_only_cohort_", dir_name = "gksrs"),
        TRUE ~ list(prefix = paste0(dataset_name, "_"), dir_name = dataset_name)
    )
    
    prefix <<- cohort_info$prefix
    cohort_dir_name <- cohort_info$dir_name
    
    # CRITICAL: Validate naming consistency to prevent bugs
    if (!validate_naming_consistency(dataset_name, prefix, cohort_dir_name)) {
        stop(sprintf("NAMING VALIDATION FAILED for dataset: %s", dataset_name))
    }
    
    # Create cohort-specific directory structure
    cohort_base_dir <<- file.path("final_data/Analysis", cohort_dir_name)
    log_function("create_output_structure", sprintf("Creating cohort-specific directory structure for %s", cohort_dir_name))
    output_dirs <<- create_output_structure(cohort_base_dir)
    
    # Forest plots go directly into the objective 1 folder
    forest_plots_dir <- output_dirs$obj1_forest_plots
    log_enhanced(sprintf("All outputs organized by objectives under: %s", cohort_base_dir), level = "INFO", indent = 1)

    # Load analytic dataset
    log_function("readRDS", paste("Loading analytic dataset:", dataset_name))
    data <- readRDS(file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds")))
    log_enhanced(sprintf("Successfully loaded %d patients for analysis", nrow(data)), level = "INFO", indent = 1)

    ########################################################
    ############### STEP 1: PRIMARY OUTCOMES ###############
    ########################################################

    step1_start_time <- Sys.time()
    log_section_start("STEP 1: PRIMARY OUTCOMES ANALYSIS", display_name)

    # Show confounders being used
    log_enhanced(sprintf("Using %d confounders for adjustment: %s", 
                        length(confounders), paste(confounders, collapse = ", ")), 
                level = "INFO", indent = 1)

    # 1a. Rates of recurrence
    log_function("analyze_binary_outcome_rates", "Local recurrence rates analysis")
    recurrence_rates <- analyze_binary_outcome_rates(
        data,
        outcome_var = "recurrence1",
        time_var = "tt_recurrence_months",
        event_var = "recurrence_event",
        confounders = confounders,
        exclude_before_treatment = TRUE,
        handle_rare = TRUE,
        dataset_name = dataset_name
    )
    log_enhanced("Local recurrence analysis completed", level = "INFO", indent = 1)

    # 1b. Rates of metastatic progression
    log_function("analyze_binary_outcome_rates", "Metastatic progression rates analysis")
    mets_rates <- analyze_binary_outcome_rates(
        data,
        outcome_var = "mets_progression",
        time_var = "tt_mets_months",
        event_var = "mets_event",
        confounders = confounders,
        exclude_before_treatment = TRUE,
        handle_rare = TRUE,
        dataset_name = dataset_name
    )
    log_enhanced("Metastatic progression analysis completed", level = "INFO", indent = 1)

    # 1c. Overall Survival
    log_function("analyze_time_to_event_outcomes", "Overall survival analysis (Kaplan-Meier & Cox regression)")
    os_analysis <- analyze_time_to_event_outcomes(
        data,
        time_var = "tt_death_months",
        event_var = "death_event",
        group_var = "treatment_group",
        confounders = confounders,
        ylab = "Overall Survival Probability",
        exclude_before_treatment = TRUE,
        handle_rare = TRUE,
        dataset_name = dataset_name
    )
    log_enhanced("Overall survival analysis completed", level = "INFO", indent = 1)

    # 1d. Progression Free Survival (includes both progression AND death)
    log_function("analyze_time_to_event_outcomes", "Progression-free survival analysis (progression OR death)")
    pfs_analysis <- analyze_time_to_event_outcomes(
        data,
        time_var = "tt_pfs_months",
        event_var = "pfs_event",
        group_var = "treatment_group",
        confounders = confounders,
        ylab = "Progression-Free Survival Probability",
        exclude_before_treatment = TRUE,
        handle_rare = TRUE,
        dataset_name = dataset_name
    )
    log_enhanced("Progression-free survival analysis completed", level = "INFO", indent = 1)

    # 1e. Tumor height changes
    log_function("analyze_tumor_height_changes", "Primary and sensitivity tumor height analysis")
    height_changes <- analyze_tumor_height_changes(data)
    log_enhanced("Tumor height changes analysis completed", level = "INFO", indent = 1)

    # 1f. Subgroup analysis with interaction terms
    log_function("analyze_treatment_effect_subgroups_height", "Subgroup analysis with interaction terms for tumor height change")
    
    # Test treatment × subgroup interactions for tumor height change
    # Run both PRIMARY (without baseline height) and SENSITIVITY (with baseline height) analyses
    
    # PRIMARY ANALYSIS: Without baseline height adjustment
    primary_start_time <- Sys.time()
    log_enhanced("PRIMARY SUBGROUP ANALYSIS (without baseline height adjustment)", level = "PROGRESS", indent = 1)
    primary_subgroup_results <- list()
    
    for (i in seq_along(subgroup_vars)) {
        subgroup_var <- subgroup_vars[i]
        log_progress(i, length(subgroup_vars), subgroup_var, "Testing PRIMARY interaction")
        
        # Test the interaction with confounders but without baseline height
        result <- analyze_treatment_effect_subgroups_height(
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
            p_status <- if (result$interaction_p < 0.05) "SIGNIFICANT" else "non-significant"
            log_enhanced(sprintf("PRIMARY Interaction p-value: %.4f (%s)", result$interaction_p, p_status), 
                        level = "INFO", indent = 2)
        } else {
            log_enhanced("PRIMARY Interaction p-value: NA (model issue)", level = "WARN", indent = 2)
        }
    }
    log_section_complete("PRIMARY SUBGROUP ANALYSIS", primary_start_time)
    
    # SENSITIVITY ANALYSIS: With baseline height adjustment
    sensitivity_start_time <- Sys.time()
    log_enhanced("SENSITIVITY SUBGROUP ANALYSIS (with baseline height adjustment)", level = "PROGRESS", indent = 1)
    sensitivity_subgroup_results <- list()
    
    for (i in seq_along(subgroup_vars)) {
        subgroup_var <- subgroup_vars[i]
        log_progress(i, length(subgroup_vars), subgroup_var, "Testing SENSITIVITY interaction")
        
        # Test the interaction with confounders including baseline height
        result <- analyze_treatment_effect_subgroups_height(
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
            p_status <- if (result$interaction_p < 0.05) "SIGNIFICANT" else "non-significant"
            log_enhanced(sprintf("SENSITIVITY Interaction p-value: %.4f (%s)", result$interaction_p, p_status), 
                        level = "INFO", indent = 2)
        } else {
            log_enhanced("SENSITIVITY Interaction p-value: NA (model issue)", level = "WARN", indent = 2)
        }
    }
    log_section_complete("SENSITIVITY SUBGROUP ANALYSIS", sensitivity_start_time)
    
    # Create formatted HTML tables for subgroup analyses
    log_function("format_subgroup_analysis_tables", "Creating formatted PRIMARY subgroup analysis tables")
    format_subgroup_analysis_tables(
        subgroup_results = primary_subgroup_results,
        dataset_name = paste("PRIMARY -", display_name),
        subgroup_dir = output_dirs$obj1_subgroup_primary,
        prefix = paste0(prefix, "primary_")
    )
    
    log_function("format_subgroup_analysis_tables", "Creating formatted SENSITIVITY subgroup analysis tables")
    format_subgroup_analysis_tables(
        subgroup_results = sensitivity_subgroup_results,
        dataset_name = paste("SENSITIVITY -", display_name),
        subgroup_dir = output_dirs$obj1_subgroup_sensitivity,
        prefix = paste0(prefix, "sensitivity_")
    )
    
    # Create forest plots for tumor height subgroup analyses
    log_function("create_forest_plots_height", "Creating forest plots for tumor height subgroup analyses")
    
    # Forest plot for PRIMARY tumor height subgroup analysis (without baseline height)
    primary_height_forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = primary_subgroup_results,
        outcome_name = "Tumor Height Change (Primary Analysis)",
        cohort_name = display_name,
        treatment_labels = TREATMENT_LABELS,
        variable_order = FOREST_PLOT_VARIABLE_ORDER,
        effect_measure = "MD",  # Mean Difference for continuous outcome
        favours_labels = FAVOURS_LABELS,
        title = sprintf("Subgroup Analysis: Tumor Height Change - Primary (%s)", display_name)
    )
    
    # Save the PRIMARY forest plot
    png(file.path(forest_plots_dir, paste0(prefix, "tumor_height_primary_subgroup_forest_plot.png")), 
        width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)
    print(primary_height_forest_plot)
    dev.off()
    log_enhanced("PRIMARY tumor height forest plot created", level = "INFO", indent = 1)
    
    # Forest plot for SENSITIVITY tumor height subgroup analysis (with baseline height)
    sensitivity_height_forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = sensitivity_subgroup_results,
        outcome_name = "Tumor Height Change (Sensitivity Analysis)",
        cohort_name = display_name,
        treatment_labels = TREATMENT_LABELS,
        variable_order = FOREST_PLOT_VARIABLE_ORDER,
        effect_measure = "MD",  # Mean Difference for continuous outcome
        favours_labels = FAVOURS_LABELS,
        title = sprintf("Subgroup Analysis: Tumor Height Change - Sensitivity (%s)", display_name)
    )
    
    # Save the SENSITIVITY forest plot
    png(file.path(forest_plots_dir, paste0(prefix, "tumor_height_sensitivity_subgroup_forest_plot.png")), 
        width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)
    print(sensitivity_height_forest_plot)
    dev.off()
    log_enhanced("SENSITIVITY tumor height forest plot created", level = "INFO", indent = 1)
    
    # Save both sets of subgroup analysis results for this dataset
    saveRDS(primary_subgroup_results, 
            file.path(output_dirs$obj1_subgroup_primary, paste0(prefix, "primary_subgroup_interactions.rds")))
    
    saveRDS(sensitivity_subgroup_results, 
            file.path(output_dirs$obj1_subgroup_sensitivity, paste0(prefix, "sensitivity_subgroup_interactions.rds")))
    
    # 1g. PRIMARY OUTCOMES SUBGROUP ANALYSIS
    log_function("primary_outcomes_subgroup_analysis", "Subgroup analysis for primary clinical outcomes")
    
    # Perform subgroup analysis for each primary outcome
    primary_outcomes_start_time <- Sys.time()
    log_enhanced("PRIMARY OUTCOMES SUBGROUP ANALYSIS", level = "PROGRESS", indent = 1)
    
    # 1g1. Local Recurrence Subgroup Analysis
    log_enhanced("Analyzing subgroup effects for Local Recurrence", level = "INFO", indent = 1)
    recurrence_subgroup_results <- analyze_treatment_effect_subgroups_binary(
        data = data,
        outcome_var = "recurrence1",
        subgroup_vars = subgroup_vars,
        confounders = confounders,
        outcome_name = "Local Recurrence"
    )
    
    # Create forest plot for local recurrence
    recurrence_forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = recurrence_subgroup_results,
        outcome_name = "Local Recurrence",
        cohort_name = display_name,
        treatment_labels = TREATMENT_LABELS,
        variable_order = FOREST_PLOT_VARIABLE_ORDER,
        effect_measure = "OR",
        favours_labels = FAVOURS_LABELS,
        title = sprintf("Subgroup Analysis: Local Recurrence (%s)", display_name)
    )
    
    # Save the forest plot
    png(file.path(forest_plots_dir, paste0(prefix, "local_recurrence_subgroup_forest_plot.png")), 
        width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)
    print(recurrence_forest_plot)
    dev.off()
    log_enhanced("Local recurrence subgroup analysis completed", level = "INFO", indent = 1)
    
    # 1g2. Metastatic Progression Subgroup Analysis
    log_enhanced("Analyzing subgroup effects for Metastatic Progression", level = "INFO", indent = 1)
    mets_subgroup_results <- analyze_treatment_effect_subgroups_binary(
        data = data,
        outcome_var = "mets_progression",
        subgroup_vars = subgroup_vars,
        confounders = confounders,
        outcome_name = "Metastatic Progression"
    )
    
    # Create forest plot for metastatic progression
    mets_forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = mets_subgroup_results,
        outcome_name = "Metastatic Progression",
        cohort_name = display_name,
        treatment_labels = TREATMENT_LABELS,
        variable_order = FOREST_PLOT_VARIABLE_ORDER,
        effect_measure = "OR",
        favours_labels = FAVOURS_LABELS,
        title = sprintf("Subgroup Analysis: Metastatic Progression (%s)", display_name)
    )
    
    # Save the forest plot
    png(file.path(forest_plots_dir, paste0(prefix, "metastatic_progression_subgroup_forest_plot.png")), 
        width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)
    print(mets_forest_plot)
    dev.off()
    log_enhanced("Metastatic progression subgroup analysis completed", level = "INFO", indent = 1)
    
    # 1g3. Overall Survival Subgroup Analysis
    log_enhanced("Analyzing subgroup effects for Overall Survival", level = "INFO", indent = 1)
    os_subgroup_results <- analyze_treatment_effect_subgroups_survival(
        data = data,
        time_var = "tt_death_months",
        event_var = "death_event",
        subgroup_vars = subgroup_vars,
        confounders = confounders,
        outcome_name = "Overall Survival"
    )
    
    # Create forest plot for overall survival
    os_forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = os_subgroup_results,
        outcome_name = "Overall Survival",
        cohort_name = display_name,
        treatment_labels = TREATMENT_LABELS,
        variable_order = FOREST_PLOT_VARIABLE_ORDER,
        effect_measure = "HR",
        favours_labels = FAVOURS_LABELS,
        title = sprintf("Subgroup Analysis: Overall Survival (%s)", display_name)
    )
    
    # Save the forest plot
    png(file.path(forest_plots_dir, paste0(prefix, "overall_survival_subgroup_forest_plot.png")), 
        width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)
    print(os_forest_plot)
    dev.off()
    log_enhanced("Overall survival subgroup analysis completed", level = "INFO", indent = 1)
    
    # 1g4. Progression-Free Survival Subgroup Analysis
    log_enhanced("Analyzing subgroup effects for Progression-Free Survival", level = "INFO", indent = 1)
    pfs_subgroup_results <- analyze_treatment_effect_subgroups_survival(
        data = data,
        time_var = "tt_pfs_months",
        event_var = "pfs_event",
        subgroup_vars = subgroup_vars,
        confounders = confounders,
        outcome_name = "Progression-Free Survival"
    )
    
    # Create forest plot for progression-free survival
    pfs_forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = pfs_subgroup_results,
        outcome_name = "Progression-Free Survival",
        cohort_name = display_name,
        treatment_labels = TREATMENT_LABELS,
        variable_order = FOREST_PLOT_VARIABLE_ORDER,
        effect_measure = "HR",
        favours_labels = FAVOURS_LABELS,
        title = sprintf("Subgroup Analysis: Progression-Free Survival (%s)", display_name)
    )
    
    # Save the forest plot
    png(file.path(forest_plots_dir, paste0(prefix, "progression_free_survival_subgroup_forest_plot.png")), 
        width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)
    print(pfs_forest_plot)
    dev.off()
    log_enhanced("Progression-free survival subgroup analysis completed", level = "INFO", indent = 1)
    
    # Save primary outcomes subgroup results
    primary_outcomes_subgroup_results <- list(
        local_recurrence = recurrence_subgroup_results,
        metastatic_progression = mets_subgroup_results,
        overall_survival = os_subgroup_results,
        progression_free_survival = pfs_subgroup_results
    )
    
    # Create organized directory for primary outcomes subgroup results
    primary_outcomes_subgroup_dir <- output_dirs$obj1_subgroup_clinical
    if (!dir.exists(primary_outcomes_subgroup_dir)) {
        dir.create(primary_outcomes_subgroup_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Save formatted tables for each primary outcome
    for (outcome_name in names(primary_outcomes_subgroup_results)) {
        outcome_results <- primary_outcomes_subgroup_results[[outcome_name]]
        effect_measure <- ifelse(outcome_name %in% c("overall_survival", "progression_free_survival"), "HR", "OR")
        
        formatted_table <- format_subgroup_analysis_results(
            subgroup_results = outcome_results,
            outcome_name = tools::toTitleCase(gsub("_", " ", outcome_name)),
            effect_measure = effect_measure,
            output_path = file.path(primary_outcomes_subgroup_dir, paste0(prefix, outcome_name, "_subgroup_analysis.xlsx"))
        )
    }
    
    # Save all primary outcomes subgroup results as RDS
    saveRDS(primary_outcomes_subgroup_results, 
            file.path(primary_outcomes_subgroup_dir, paste0(prefix, "primary_outcomes_subgroup_results.rds")))
    
    log_section_complete("PRIMARY OUTCOMES SUBGROUP ANALYSIS", primary_outcomes_start_time)
    
    log_section_complete("STEP 1: PRIMARY OUTCOMES ANALYSIS", step1_start_time)
    
    ########################################################
    ############### STEP 2: SAFETY/TOXICITY ###############
    ########################################################
    
    step2_start_time <- Sys.time()
    log_section_start("STEP 2: SAFETY/TOXICITY ANALYSIS", display_name)
    
    # 2a. Vision changes analysis (similar to tumor height changes)
    log_function("analyze_visual_acuity_changes", "Vision changes analysis")
    vision_changes <- analyze_visual_acuity_changes(data)
    log_enhanced("Vision changes analysis completed", level = "INFO", indent = 1)
    
    # 2b. Rates of radiation sequelae
    log_enhanced("Analyzing radiation sequelae rates", level = "INFO", indent = 1)
    
    # 2b1. Retinopathy
    log_function("analyze_radiation_complications", "Radiation retinopathy analysis")
    retinopathy_rates <- analyze_radiation_complications(
        data = data,
        sequela_type = "retinopathy",
        confounders = confounders,
        dataset_name = dataset_name
    )
    log_enhanced("Retinopathy analysis completed", level = "INFO", indent = 1)
    
    # 2b2. Neovascular glaucoma (NVG)
    log_function("analyze_radiation_complications", "Neovascular glaucoma (NVG) analysis")
    nvg_rates <- analyze_radiation_complications(
        data = data,
        sequela_type = "nvg",
        confounders = confounders,
        dataset_name = dataset_name
    )
    log_enhanced("Neovascular glaucoma analysis completed", level = "INFO", indent = 1)
    
    # 2b3. Serous retinal detachment (SRD) - only radiation-induced
    log_function("analyze_radiation_complications", "Serous retinal detachment (radiation-induced only)")
    srd_rates <- analyze_radiation_complications(
        data = data,
        sequela_type = "srd",
        confounders = confounders,
        dataset_name = dataset_name
    )
    log_enhanced("Serous retinal detachment analysis completed", level = "INFO", indent = 1)
    
    log_section_complete("STEP 2: SAFETY/TOXICITY ANALYSIS", step2_start_time)
    
    ########################################################
    ############### STEP 3: REPEAT RADIATION ##############
    ########################################################
    
    step3_start_time <- Sys.time()
    log_section_start("STEP 3: REPEAT RADIATION EFFICACY", display_name)
    
    # 3a. Progression-Free Survival-2 (PFS-2) for recurrent patients
    log_function("analyze_pfs2", "PFS-2 analysis for patients with local recurrence")
    pfs2_results <- analyze_pfs2(data, confounders = confounders, dataset_name = dataset_name)
    log_enhanced("PFS-2 analysis completed", level = "INFO", indent = 1)
    
    log_section_complete("STEP 3: REPEAT RADIATION EFFICACY", step3_start_time)
    
    # Complete analysis for this dataset
    log_section_complete(paste("STATISTICAL ANALYSIS -", display_name), analysis_start_time)
    
}

# Run analysis for each dataset
log_section_start("MAIN EXECUTION PHASE")
main_start_time <- Sys.time()

available_datasets <- list_available_datasets()
results <- list()

# Wrap main execution in tryCatch to ensure proper cleanup
tryCatch({
    log_enhanced(sprintf("Beginning analysis of %d datasets", length(available_datasets)), level = "INFO")
    
    for (i in seq_along(available_datasets)) {
        dataset <- available_datasets[i]
        dataset_display <- tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset)))
        log_progress(i, length(available_datasets), dataset_display, "Analyzing dataset")
        
        results[[dataset]] <- run_my_analysis(dataset)
        
        log_enhanced(sprintf("Dataset %d/%d completed: %s", i, length(available_datasets), dataset_display), 
                    level = "PROGRESS")
    }
    
    # NOTE: Forest plot combining functionality is available in scripts/utils/output_utilities.R
    # but not being run automatically. Collaborator can decide whether to use it later.
    log_section_complete("MAIN EXECUTION PHASE", main_start_time)
    
    # Merge baseline tables from all cohorts
    log_enhanced("Merging baseline tables from all cohorts", level = "INFO")
    merge_cohort_tables(
        full_cohort_data = readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds")),
        restricted_cohort_data = readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_restricted_cohort.rds")),
        output_path = file.path("final_data", "Analysis", "merged_tables")
    )
    
    # Final summary
    log_enhanced("", level = "SECTION")
    log_enhanced("ALL ANALYSES COMPLETED SUCCESSFULLY!", level = "PROGRESS")
    log_enhanced(sprintf("Total execution time: %.1f minutes", 
                        as.numeric(difftime(Sys.time(), main_start_time, units = "mins"))), 
                level = "PROGRESS")
    log_enhanced(sprintf("Datasets analyzed: %d", length(available_datasets)), level = "PROGRESS")
    log_enhanced("Check the logs above for detailed progress and any warnings.", level = "INFO")
    log_enhanced("Each cohort has its own complete set of analyses for easy comparison!", level = "INFO")
    
}, finally = {
    # Clean up logging if it was enabled
    if (USE_LOGS) {
        sink(type = "message")
        sink()
        close(log_con)
        log_enhanced("Log file closed successfully", level = "INFO")
    }
})
