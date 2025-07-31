# Uveal Melanoma Treatment Outcomes Analysis
# Author: Nicholas Camarda 
# Description: Analysis comparing outcomes between Gamma Knife and plaque brachytherapy
#              for uveal melanoma treatment, including both full and restricted cohort analyses
# Main script to run the analysis

# Source the analysis configuration first (all global variables), required libraries, and helper functions
source("scripts/utils/all_helper_functions.R")

########################################################
############### DATA PROCESSING ########################
########################################################

# Set up logging if enabled
if (USE_LOGS) {
    # Create logs directory if it doesn't exist
    if (!dir.exists("logs")) {
        dir.create("logs", showWarnings = FALSE)
    }

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
    factored_result <- prepare_factor_levels(derived_data)
    factored_data <- factored_result$data
    other_map <- factored_result$other_map

    # Apply inclusion/exclusion criteria (split into cohorts)
    log_function("apply_criteria", "Applying inclusion/exclusion criteria and creating cohorts")
    final_analytic_datasets_lst <- apply_criteria(factored_data)

    # Save each cohort separately
    log_function("save_cohorts", "Saving processed cohorts to RDS files")
    save_cohorts(final_analytic_datasets_lst)
    
    # Save the other_map information for use in analysis
    log_function("saveRDS", "Saving other_map information for tracking collapsed categories")
    saveRDS(other_map, file.path(PROCESSED_DATA_DIR, "other_map.rds"))

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
    
    # Create summary tables
    summary_tables <- create_summary_tables(final_analytic_datasets_lst, temp_output_dirs_by_cohort)

    log_section_complete("DATA PREPROCESSING PHASE", data_start_time)
    
} else {
    log_section_start("DATA LOADING PHASE")
    log_enhanced("RECREATE_ANALYTIC_DATASETS = FALSE: Skipping analytic dataset creation", level = "INFO")
    log_enhanced("Using existing datasets from final_data/Analytic Dataset/", level = "INFO")
    log_enhanced("Set RECREATE_ANALYTIC_DATASETS = TRUE if you need to reprocess raw data", level = "INFO")
}

########################################################
############### MODULAR ANALYSIS FUNCTIONS #############
########################################################

#' Run Objective 1: Primary Outcomes Analysis
#' 
#' @param data Data frame with analytic dataset
#' @param dataset_name Character string for dataset name
#' @param output_dirs List of output directories
#' @param prefix Character string for file prefix
#' @return List of analysis results
run_objective_1 <- function(data, dataset_name, output_dirs, prefix, other_map = list()) {
    step1_start_time <- Sys.time()
    display_name <- tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))
    log_section_start("STEP 1: PRIMARY OUTCOMES ANALYSIS", display_name)

    # Show confounders being used
    log_enhanced(sprintf("Using %d confounders for adjustment: %s", 
                        length(confounders), paste(confounders, collapse = ", ")), 
                level = "INFO", indent = 1)

    # 1a. Rates of recurrence (post-treatment only)
    log_function("analyze_binary_outcome_rates", "Local recurrence rates analysis (post-treatment only)")
    recurrence_rates <- analyze_binary_outcome_rates(
        data,
        outcome_var = "recurrence1",
        time_var = "tt_recurrence_months",
        event_var = "recurrence_event",
        confounders = confounders,
        analysis_type = "post_treatment_only",
        dataset_name = dataset_name,
        other_map = other_map
    )
    log_enhanced("Local recurrence analysis completed", level = "INFO", indent = 1)

    # 1b. Rates of metastatic progression (post-treatment only)
    log_function("analyze_binary_outcome_rates", "Metastatic progression rates analysis (post-treatment only)")
    mets_rates <- analyze_binary_outcome_rates(
        data,
        outcome_var = "mets_progression",
        time_var = "tt_mets_months",
        event_var = "mets_event",
        confounders = confounders,
        analysis_type = "post_treatment_only",
        dataset_name = dataset_name,
        other_map = other_map
    )
    log_enhanced("Metastatic progression analysis completed", level = "INFO", indent = 1)

    # 1c. Overall Survival (post-treatment only)
    log_function("analyze_time_to_event_outcomes", "Overall survival analysis (Kaplan-Meier & Cox regression)")
    os_analysis <- analyze_time_to_event_outcomes(
        data,
        time_var = "tt_death_months",
        event_var = "death_event",
        group_var = "treatment_group",
        confounders = confounders,
        ylab = "Overall Survival Probability",
        analysis_type = "post_treatment_only",
        dataset_name = dataset_name,
        other_map = other_map
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
        analysis_type = "post_treatment_only",
        dataset_name = dataset_name,
        other_map = other_map
    )
    log_enhanced("Progression-free survival analysis completed", level = "INFO", indent = 1)

    # 1e. Tumor height changes
    log_function("analyze_tumor_height_changes", "Primary and sensitivity tumor height analysis")
    height_changes <- analyze_tumor_height_changes(data, other_map)
    log_enhanced("Tumor height changes analysis completed", level = "INFO", indent = 1)

    # 1f. Subgroup analysis with interaction terms
    log_function("analyze_treatment_effect_subgroups_height", "Subgroup analysis with interaction terms for tumor height change")
    
    # Test treatment × subgroup interactions for tumor height change
    # Run both PRIMARY (without baseline height) and SENSITIVITY (with baseline height) analyses
    
    # PRIMARY ANALYSIS: Without baseline height adjustment
    primary_start_time <- Sys.time()
    log_enhanced("PRIMARY SUBGROUP ANALYSIS (without baseline height adjustment)", level = "PROGRESS", indent = 1)
    primary_subgroup_results <- list()
    primary_other_maps <- list()  # Collect other_map from all variables
    
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
        
        # Collect other_map if available (only if result is not NULL)
        if (!is.null(result) && !is.null(result$other_map) && length(result$other_map) > 0) {
            # Use a more robust way to combine lists
            for (var_name in names(result$other_map)) {
                if (!is.null(result$other_map[[var_name]]) && length(result$other_map[[var_name]]) > 0) {
                    primary_other_maps[[var_name]] <- result$other_map[[var_name]]
                }
            }
        }
        
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
    sensitivity_other_maps <- list()  # Collect other_map from all variables
    
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
        
        # Collect other_map if available (only if result is not NULL)
        if (!is.null(result) && !is.null(result$other_map) && length(result$other_map) > 0) {
            # Use a more robust way to combine lists
            for (var_name in names(result$other_map)) {
                if (!is.null(result$other_map[[var_name]]) && length(result$other_map[[var_name]]) > 0) {
                    sensitivity_other_maps[[var_name]] <- result$other_map[[var_name]]
                }
            }
        }
        
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
        subgroup_results = list(
            subgroup_results = primary_subgroup_results,
            other_map = primary_other_maps
        ),
        dataset_name = paste("PRIMARY -", display_name),
        subgroup_dir = output_dirs$obj1_subgroup_primary,
        prefix = paste0(prefix, "primary_")
    )
    
    log_function("format_subgroup_analysis_tables", "Creating formatted SENSITIVITY subgroup analysis tables")
    format_subgroup_analysis_tables(
        subgroup_results = list(
            subgroup_results = sensitivity_subgroup_results,
            other_map = sensitivity_other_maps
        ),
        dataset_name = paste("SENSITIVITY -", display_name),
        subgroup_dir = output_dirs$obj1_subgroup_sensitivity,
        prefix = paste0(prefix, "sensitivity_")
    )
    
    # Create forest plots for tumor height subgroup analyses
    log_function("create_forest_plots_height", "Creating forest plots for tumor height subgroup analyses")
    
    # Initialize forest plot diagnostics collector
    diagnostics_list <- list()
    
    # Forest plot for PRIMARY tumor height subgroup analysis (without baseline height)
    primary_height_forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = primary_subgroup_results,
        outcome_name = "Tumor Height Change (Primary Analysis)",
        cohort_name = display_name,
        treatment_labels = TREATMENT_LABELS,
        variable_order = FOREST_PLOT_VARIABLE_ORDER,
        effect_measure = "MD",  # Mean Difference for continuous outcome
        favours_labels = FAVOURS_LABELS,
        title = sprintf("Subgroup Analysis: Tumor Height Change - Primary (%s)", display_name),
        other_map = primary_other_maps # Pass the collected other_maps for diagnostics
    )
    
    # Collect diagnostics
    diagnostics_list[["tumor_height_primary"]] <- get_forest_plot_diagnostics(primary_height_forest_plot)
    
    # Save the PRIMARY forest plot
    png(file.path(output_dirs$obj1_forest_plots, paste0(prefix, "tumor_height_primary_subgroup_forest_plot.png")), 
        width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)
    plot(primary_height_forest_plot)
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
        title = sprintf("Subgroup Analysis: Tumor Height Change - Sensitivity (%s)", display_name),
        other_map = sensitivity_other_maps # Pass the collected other_maps for diagnostics
    )
    
    # Collect diagnostics
    diagnostics_list[["tumor_height_sensitivity"]] <- get_forest_plot_diagnostics(sensitivity_height_forest_plot)
    
    # Save the SENSITIVITY forest plot
    png(file.path(output_dirs$obj1_forest_plots, paste0(prefix, "tumor_height_sensitivity_subgroup_forest_plot.png")), 
        width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)
    plot(sensitivity_height_forest_plot)
    dev.off()
    log_enhanced("SENSITIVITY tumor height forest plot created", level = "INFO", indent = 1)
    
    # Save both sets of subgroup analysis results for this dataset
    saveRDS(primary_subgroup_results, 
            file.path(output_dirs$obj1_subgroup_primary, paste0(prefix, "primary_subgroup_interactions.rds")))
    
    saveRDS(sensitivity_subgroup_results, 
            file.path(output_dirs$obj1_subgroup_sensitivity, paste0(prefix, "sensitivity_subgroup_interactions.rds")))
    
    # PRIMARY TUMOR HEIGHT SUBGROUP ANALYSIS CONSOLIDATION
    primary_diagnostics_list <- list()
    for (i in seq_along(subgroup_vars)) {
        subgroup_var <- subgroup_vars[i]
        log_progress(i, length(subgroup_vars), subgroup_var, "Testing PRIMARY interaction")
        result <- analyze_treatment_effect_subgroups_height(
            data = data,
            subgroup_var = subgroup_var,
            percentile_cut = 0.5,
            confounders = confounders,
            include_baseline_height = FALSE
        )
        if (!is.null(result$subgroup_effects) && nrow(result$subgroup_effects) > 0) {
            tab_name <- tools::toTitleCase(gsub("_", " ", subgroup_var))
            tab_name <- gsub("[^A-Za-z0-9_]", "_", tab_name)
            tab_name <- substr(tab_name, 1, 31)
            primary_diagnostics_list[[tab_name]] <- result$subgroup_effects
        }
    }
    consolidated_primary_path <- file.path(output_dirs$obj1_subgroup_primary, paste0(prefix, "primary_tumor_height_diagnostics.xlsx"))
    writexl::write_xlsx(primary_diagnostics_list, consolidated_primary_path)
    log_enhanced(sprintf("Primary tumor height diagnostics written to %s with %d tabs", consolidated_primary_path, length(primary_diagnostics_list)), level = "INFO", indent = 1)
    
    # SENSITIVITY TUMOR HEIGHT SUBGROUP ANALYSIS CONSOLIDATION
    sensitivity_diagnostics_list <- list()
    for (i in seq_along(subgroup_vars)) {
        subgroup_var <- subgroup_vars[i]
        log_progress(i, length(subgroup_vars), subgroup_var, "Testing SENSITIVITY interaction")
        result <- analyze_treatment_effect_subgroups_height(
            data = data,
            subgroup_var = subgroup_var,
            percentile_cut = 0.5,
            confounders = confounders,
            include_baseline_height = TRUE
        )
        if (!is.null(result$subgroup_effects) && nrow(result$subgroup_effects) > 0) {
            tab_name <- tools::toTitleCase(gsub("_", " ", subgroup_var))
            tab_name <- gsub("[^A-Za-z0-9_]", "_", tab_name)
            tab_name <- substr(tab_name, 1, 31)
            sensitivity_diagnostics_list[[tab_name]] <- result$subgroup_effects
        }
    }
    consolidated_sensitivity_path <- file.path(output_dirs$obj1_subgroup_sensitivity, paste0(prefix, "sensitivity_tumor_height_diagnostics.xlsx"))
    writexl::write_xlsx(sensitivity_diagnostics_list, consolidated_sensitivity_path)
    log_enhanced(sprintf("Sensitivity tumor height diagnostics written to %s with %d tabs", consolidated_sensitivity_path, length(sensitivity_diagnostics_list)), level = "INFO", indent = 1)
    
    # 1g. PRIMARY OUTCOMES SUBGROUP ANALYSIS
    log_function("primary_outcomes_subgroup_analysis", "Subgroup analysis for primary clinical outcomes")
    
    # Perform subgroup analysis for each primary outcome
    primary_outcomes_start_time <- Sys.time()
    log_enhanced("PRIMARY OUTCOMES SUBGROUP ANALYSIS", level = "PROGRESS", indent = 1)

    # Create organized directory for primary outcomes subgroup results
    primary_outcomes_subgroup_dir <- output_dirs$obj1_subgroup_clinical
    if (!dir.exists(primary_outcomes_subgroup_dir)) {
        dir.create(primary_outcomes_subgroup_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # 1g1. Local Recurrence Subgroup Analysis
    log_enhanced("Analyzing subgroup effects for Local Recurrence", level = "INFO", indent = 1)
    recurrence_subgroup_analysis <- analyze_treatment_effect_subgroups_binary(
        data = data,
        outcome_var = "recurrence1",
        subgroup_vars = subgroup_vars,
        confounders = confounders,
        outcome_name = "Local Recurrence"
    )
    recurrence_subgroup_results <- recurrence_subgroup_analysis$subgroup_results
    recurrence_other_map <- recurrence_subgroup_analysis$other_map
    
    format_subgroup_analysis_results(
        subgroup_results = recurrence_subgroup_results,
        outcome_name = "Local Recurrence",
        effect_measure = "OR",
        output_path = file.path(primary_outcomes_subgroup_dir, paste0(prefix, "local_recurrence_subgroup_analysis.xlsx")),
        create_tables = TRUE,
        other_map = recurrence_other_map
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
        title = sprintf("Subgroup Analysis: Local Recurrence (%s)", display_name),
        other_map = recurrence_other_map # Pass for diagnostics
    )
    
    # Save the forest plot
    png(file.path(output_dirs$obj1_forest_plots, paste0(prefix, "local_recurrence_subgroup_forest_plot.png")), 
        width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)
    plot(recurrence_forest_plot)
    dev.off()
    log_enhanced("Local recurrence subgroup analysis completed", level = "INFO", indent = 1)
    
    # 1g2. Metastatic Progression Subgroup Analysis
    log_enhanced("Analyzing subgroup effects for Metastatic Progression", level = "INFO", indent = 1)
    mets_subgroup_analysis <- analyze_treatment_effect_subgroups_binary(
        data = data,
        outcome_var = "mets_progression",
        subgroup_vars = subgroup_vars,
        confounders = confounders,
        outcome_name = "Metastatic Progression"
    )
    mets_subgroup_results <- mets_subgroup_analysis$subgroup_results
    mets_other_map <- mets_subgroup_analysis$other_map
    
    format_subgroup_analysis_results(
        subgroup_results = mets_subgroup_results,
        outcome_name = "Metastatic Progression",
        effect_measure = "OR",
        output_path = file.path(primary_outcomes_subgroup_dir, paste0(prefix, "metastatic_progression_subgroup_analysis.xlsx")),
        create_tables = TRUE,
        other_map = mets_other_map
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
        title = sprintf("Subgroup Analysis: Metastatic Progression (%s)", display_name),
        other_map = mets_other_map # Pass for diagnostics
    )
    
    # Save the forest plot
    png(file.path(output_dirs$obj1_forest_plots, paste0(prefix, "metastatic_progression_subgroup_forest_plot.png")), 
        width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)
    plot(mets_forest_plot)
    dev.off()
    log_enhanced("Metastatic progression subgroup analysis completed", level = "INFO", indent = 1)
    
    # 1g3. Overall Survival Subgroup Analysis
    log_enhanced("Analyzing subgroup effects for Overall Survival", level = "INFO", indent = 1)
    os_subgroup_analysis <- analyze_treatment_effect_subgroups_survival(
        data = data,
        time_var = "tt_death_months",
        event_var = "death_event",
        subgroup_vars = subgroup_vars,
        confounders = confounders,
        outcome_name = "Overall Survival"
    )
    os_subgroup_results <- os_subgroup_analysis$subgroup_results
    os_other_map <- os_subgroup_analysis$other_map
    
    format_subgroup_analysis_results(
        subgroup_results = os_subgroup_results,
        outcome_name = "Overall Survival",
        effect_measure = "HR",
        output_path = file.path(primary_outcomes_subgroup_dir, paste0(prefix, "overall_survival_subgroup_analysis.xlsx")),
        create_tables = TRUE,
        other_map = os_other_map
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
        title = sprintf("Subgroup Analysis: Overall Survival (%s)", display_name),
        other_map = os_other_map # Pass for diagnostics
    )
    
    # Save the forest plot
    png(file.path(output_dirs$obj1_forest_plots, paste0(prefix, "overall_survival_subgroup_forest_plot.png")), 
        width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)
    plot(os_forest_plot)
    dev.off()
    log_enhanced("Overall survival subgroup analysis completed", level = "INFO", indent = 1)
    
    # 1g4. Progression-Free Survival Subgroup Analysis
    log_enhanced("Analyzing subgroup effects for Progression-Free Survival", level = "INFO", indent = 1)
    pfs_subgroup_analysis <- analyze_treatment_effect_subgroups_survival(
        data = data,
        time_var = "tt_pfs_months",
        event_var = "pfs_event",
        subgroup_vars = subgroup_vars,
        confounders = confounders,
        outcome_name = "Progression-Free Survival"
    )
    pfs_subgroup_results <- pfs_subgroup_analysis$subgroup_results
    pfs_other_map <- pfs_subgroup_analysis$other_map
    
    format_subgroup_analysis_results(
        subgroup_results = pfs_subgroup_results,
        outcome_name = "Progression-Free Survival",
        effect_measure = "HR",
        output_path = file.path(primary_outcomes_subgroup_dir, paste0(prefix, "progression_free_survival_subgroup_analysis.xlsx")),
        create_tables = TRUE,
        other_map = pfs_other_map
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
        title = sprintf("Subgroup Analysis: Progression-Free Survival (%s)", display_name),
        other_map = pfs_other_map # Pass for diagnostics
    )
    
    # Save the forest plot
    png(file.path(output_dirs$obj1_forest_plots, paste0(prefix, "progression_free_survival_subgroup_forest_plot.png")), 
        width = FOREST_PLOT_WIDTH, height = FOREST_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)
    plot(pfs_forest_plot)
    dev.off()
    log_enhanced("Progression-free survival subgroup analysis completed", level = "INFO", indent = 1)
    
    # Save primary outcomes subgroup results
    primary_outcomes_subgroup_results <- list(
        local_recurrence = recurrence_subgroup_results,
        metastatic_progression = mets_subgroup_results,
        overall_survival = os_subgroup_results,
        progression_free_survival = pfs_subgroup_results
    )
    
    # CLINICAL OUTCOMES SUBGROUP ANALYSIS CONSOLIDATION
    clinical_outcomes_diagnostics <- list()
    for (outcome_name in names(primary_outcomes_subgroup_results)) {
        outcome_results <- primary_outcomes_subgroup_results[[outcome_name]]
        for (var_name in names(outcome_results)) {
            var_result <- outcome_results[[var_name]]
            if (!is.null(var_result$subgroup_effects) && nrow(var_result$subgroup_effects) > 0) {
                tab_name <- paste0(tools::toTitleCase(gsub("_", " ", outcome_name)), "_", tools::toTitleCase(gsub("_", " ", var_name)))
                tab_name <- gsub("[^A-Za-z0-9_]", "_", tab_name)
                tab_name <- substr(tab_name, 1, 31)
                clinical_outcomes_diagnostics[[tab_name]] <- var_result$subgroup_effects
            }
        }
    }
    consolidated_clinical_path <- file.path(primary_outcomes_subgroup_dir, paste0(prefix, "clinical_outcomes_diagnostics.xlsx"))
    writexl::write_xlsx(clinical_outcomes_diagnostics, consolidated_clinical_path)
    log_enhanced(sprintf("Clinical outcomes diagnostics written to %s with %d tabs", consolidated_clinical_path, length(clinical_outcomes_diagnostics)), level = "INFO", indent = 1)
    
    # Save all primary outcomes subgroup results as RDS
    saveRDS(primary_outcomes_subgroup_results, 
            file.path(primary_outcomes_subgroup_dir, paste0(prefix, "primary_outcomes_subgroup_results.rds")))
    
    log_section_complete("PRIMARY OUTCOMES SUBGROUP ANALYSIS", primary_outcomes_start_time)
    
    log_section_complete("STEP 1: PRIMARY OUTCOMES ANALYSIS", step1_start_time)
    
    return(list(
        recurrence_rates = recurrence_rates,
        mets_rates = mets_rates,
        os_analysis = os_analysis,
        pfs_analysis = pfs_analysis,
        height_changes = height_changes,
        primary_subgroup_results = primary_subgroup_results,
        sensitivity_subgroup_results = sensitivity_subgroup_results
    ))
}

#' Run Objective 2: Safety/Toxicity Analysis
#' 
#' @param data Data frame with analytic dataset
#' @param dataset_name Character string for dataset name
#' @param output_dirs List of output directories
#' @param prefix Character string for file prefix
#' @return List of analysis results
run_objective_2 <- function(data, dataset_name, output_dirs, prefix, other_map = list()) {
    step2_start_time <- Sys.time()
    display_name <- tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))
    log_section_start("STEP 2: SAFETY/TOXICITY ANALYSIS", display_name)

    # 2a. Vision changes
    log_function("analyze_visual_acuity_changes", "Vision changes analysis")
    vision_changes <- analyze_visual_acuity_changes(data, other_map)
    log_enhanced("Vision changes analysis completed", level = "INFO", indent = 1)

    # 2b. Radiation complications
    log_function("analyze_radiation_complications", "Radiation complications analysis")
    
    # Retinopathy
    retinopathy_analysis <- analyze_radiation_complications(data, "retinopathy", confounders, dataset_name, other_map)
    log_enhanced("Retinopathy analysis completed", level = "INFO", indent = 1)
    
    # Neovascular glaucoma
    nvg_analysis <- analyze_radiation_complications(data, "nvg", confounders, dataset_name, other_map)
    log_enhanced("Neovascular glaucoma analysis completed", level = "INFO", indent = 1)
    
    # Serous retinal detachment
    srd_analysis <- analyze_radiation_complications(data, "srd", confounders, dataset_name, other_map)
    log_enhanced("Serous retinal detachment analysis completed", level = "INFO", indent = 1)

    log_section_complete("STEP 2: SAFETY/TOXICITY ANALYSIS", step2_start_time)
    
    return(list(
        vision_changes = vision_changes,
        retinopathy_analysis = retinopathy_analysis,
        nvg_analysis = nvg_analysis,
        srd_analysis = srd_analysis
    ))
}

#' Run Objective 3: Repeat Radiation Efficacy
#' 
#' @param data Data frame with analytic dataset
#' @param dataset_name Character string for dataset name
#' @param output_dirs List of output directories
#' @param prefix Character string for file prefix
#' @return List of analysis results
run_objective_3 <- function(data, dataset_name, output_dirs, prefix, other_map = list()) {
    step3_start_time <- Sys.time()
    display_name <- tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))
    log_section_start("STEP 3: REPEAT RADIATION EFFICACY", display_name)

    # PFS-2 analysis (freedom from second recurrence)
    log_function("analyze_pfs2", "PFS-2 analysis (freedom from second recurrence)")
    pfs2_analysis <- analyze_pfs2(data, dataset_name, other_map)
    log_enhanced("PFS-2 analysis completed", level = "INFO", indent = 1)

    log_section_complete("STEP 3: REPEAT RADIATION EFFICACY", step3_start_time)
    
    return(list(
        pfs2_analysis = pfs2_analysis
    ))
}

#' Run Objective 4: GEP Validation
#' 
#' @param data Data frame with analytic dataset
#' @param dataset_name Character string for dataset name
#' @param output_dirs List of output directories
#' @param prefix Character string for file prefix
#' @return List of analysis results
run_objective_4 <- function(data, dataset_name, output_dirs, prefix, other_map = list()) {
    step4_start_time <- Sys.time()
    display_name <- tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))
    log_section_start("STEP 4: GEP PREDICTIVE ACCURACY VALIDATION", display_name)

    # MFS GEP validation
    log_function("analyze_gep_mfs_validation", "MFS GEP validation analysis")
    mfs_gep_results <- analyze_gep_mfs_validation(data, dataset_name)
    log_enhanced("MFS GEP validation completed", level = "INFO", indent = 1)

    # MSS GEP validation
    log_function("analyze_gep_mss_validation", "MSS GEP validation analysis")
    mss_gep_results <- analyze_gep_mss_validation(data, dataset_name)
    log_enhanced("MSS GEP validation completed", level = "INFO", indent = 1)

    # Simple GEP validation
    log_function("simple_gep_validation", "Simple GEP validation - Actual vs Expected rates")
    simple_gep_results <- simple_gep_validation(data, output_dirs$obj4_simple, prefix)
    log_enhanced("Simple GEP validation completed", level = "INFO", indent = 1)

    log_section_complete("STEP 4: GEP PREDICTIVE ACCURACY VALIDATION", step4_start_time)
    
    return(list(
        mfs_gep_results = mfs_gep_results,
        mss_gep_results = mss_gep_results,
        simple_gep_results = simple_gep_results
    ))
}

########################################################
############### MAIN ANALYSIS FUNCTION #################
########################################################

# Run analysis for each dataset
run_my_analysis <- function(dataset_name, objectives_to_run = c(1, 2, 3, 4)) {
    analysis_start_time <- Sys.time()
    
    # Clean dataset name for display
    display_name <- tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))
    log_section_start("STATISTICAL ANALYSIS", display_name)

    # Set up cohort outputs using centralized function
    cohort_outputs <- setup_cohort_outputs(dataset_name)
    
    prefix <<- cohort_outputs$prefix
    cohort_base_dir <<- cohort_outputs$cohort_base_dir
    output_dirs <<- cohort_outputs$output_dirs
    
    # CRITICAL: Validate naming consistency to prevent bugs
    if (!validate_naming_consistency(dataset_name, prefix, basename(cohort_base_dir))) {
        stop(sprintf("NAMING VALIDATION FAILED for dataset: %s", dataset_name))
    }
    
    log_enhanced(sprintf("All outputs organized by objectives under: %s", cohort_base_dir), level = "INFO", indent = 1)

    # Load analytic dataset
    log_function("readRDS", paste("Loading analytic dataset:", dataset_name))
    data <- readRDS(file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds")))
    log_enhanced(sprintf("Successfully loaded %d patients for analysis", nrow(data)), level = "INFO", indent = 1)

    # Load other_map information for tracking collapsed categories
    other_map_file <- file.path(PROCESSED_DATA_DIR, "other_map.rds")
    if (file.exists(other_map_file)) {
        log_function("readRDS", "Loading other_map information for collapsed categories")
        all_other_maps <- readRDS(other_map_file)
        other_map <- all_other_maps[[dataset_name]]
        if (is.null(other_map)) {
            other_map <- list()
            log_enhanced("No other_map found for this dataset, using empty list", level = "INFO", indent = 1)
        } else {
            log_enhanced(sprintf("Loaded other_map with %d variables having collapsed categories", length(other_map)), level = "INFO", indent = 1)
            # Log what categories were collapsed
            for (var_name in names(other_map)) {
                collapsed_cats <- other_map[[var_name]]
                log_enhanced(sprintf("  %s: %s collapsed into 'Other'", var_name, paste(collapsed_cats, collapse = ", ")), level = "INFO", indent = 2)
            }
        }
    } else {
        other_map <- list()
        log_enhanced("No other_map.rds file found, using empty list", level = "INFO", indent = 1)
    }

    # Run selected objectives
    results <- list()
    
    if (1 %in% objectives_to_run) {
        log_enhanced("Running Objective 1: Primary Outcomes", level = "INFO")
        results$objective_1 <- run_objective_1(data, dataset_name, output_dirs, prefix, other_map)
    }
    
    if (2 %in% objectives_to_run) {
        log_enhanced("Running Objective 2: Safety/Toxicity", level = "INFO")
        results$objective_2 <- run_objective_2(data, dataset_name, output_dirs, prefix, other_map)
    }
    
    if (3 %in% objectives_to_run) {
        log_enhanced("Running Objective 3: Repeat Radiation Efficacy", level = "INFO")
        results$objective_3 <- run_objective_3(data, dataset_name, output_dirs, prefix, other_map)
    }
    
    if (4 %in% objectives_to_run) {
        log_enhanced("Running Objective 4: GEP Validation", level = "INFO")
        results$objective_4 <- run_objective_4(data, dataset_name, output_dirs, prefix, other_map)
    }

    log_section_complete("STATISTICAL ANALYSIS", analysis_start_time)
    
    return(results)
}

########################################################
############### MAIN EXECUTION #########################
########################################################

# Main execution
main_execution <- function() {
    main_start_time <- Sys.time()
    log_section_start("MAIN EXECUTION PHASE")
    
    # Define datasets to analyze
    # this should be generated from the list_available_datasets function and named appropriately so that run_my_analysis can be called with the correct dataset name
    datasets_to_analyze <- c(
        "uveal_melanoma_full_cohort",
        "uveal_melanoma_restricted_cohort", 
        "uveal_melanoma_gksrs_only_cohort"
    )
    
    # Run analysis for each dataset
    for (i in seq_along(datasets_to_analyze)) {
        dataset_name <- datasets_to_analyze[i]
        log_enhanced(sprintf(">>> Dataset %d/%d: %s", i, length(datasets_to_analyze), dataset_name), level = "PROGRESS")
        
        tryCatch({
            results <- run_my_analysis(dataset_name)
            log_enhanced(sprintf(">>> Dataset %d/%d completed: %s", i, length(datasets_to_analyze), dataset_name), level = "PROGRESS")
        }, error = function(e) {
            log_enhanced(sprintf("ERROR in dataset %s: %s", dataset_name, e$message), level = "ERROR")
        })
    }
    
    # Merge baseline tables from all cohorts
    log_enhanced("Merging baseline tables from all cohorts", level = "INFO")
    log_enhanced("=== STARTING TABLE MERGING: Full and Restricted Cohorts ===", level = "INFO")
    
    # Create merged tables directory
    merged_dir <- file.path(OUTPUT_DIR, "merged_tables")
    if (!dir.exists(merged_dir)) {
        dir.create(merged_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    log_enhanced(sprintf("Merging tables will be saved to: %s", merged_dir), level = "INFO")
    
    # Load both datasets for merging
    full_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
    restricted_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_restricted_cohort.rds"))
    
    # Create merged baseline characteristics table using the correct function
    merge_cohort_tables(full_data, restricted_data, merged_dir)
    log_enhanced("=== COMPLETED TABLE MERGING ===", level = "INFO")
    log_enhanced(sprintf("Merged baseline characteristics table saved to: %s", merged_dir), level = "INFO")
    log_enhanced("Files created: merged_baseline_characteristics.xlsx and merged_baseline_characteristics.html", level = "INFO")
    
    log_enhanced("===  ===", level = "INFO")
    log_enhanced(">>> ALL ANALYSES COMPLETED SUCCESSFULLY!", level = "SUCCESS")
    log_enhanced(sprintf(">>> Total execution time: %.1f minutes", as.numeric(difftime(Sys.time(), main_start_time, units = "mins"))), level = "SUCCESS")
    log_enhanced(sprintf(">>> Datasets analyzed: %d", length(datasets_to_analyze)), level = "SUCCESS")
    log_enhanced("Check the logs above for detailed progress and any warnings.", level = "INFO")
    log_enhanced("Each cohort has its own complete set of analyses for easy comparison!", level = "INFO")
    
    log_section_complete("MAIN EXECUTION PHASE", main_start_time)
}

# Run specific objective for testing
run_specific_objective <- function(dataset_name, objective_number) {
    log_enhanced(sprintf("Running only Objective %d for dataset: %s", objective_number, dataset_name), level = "INFO")
    results <- run_my_analysis(dataset_name, objectives_to_run = objective_number)
    return(results)
}

# Uncomment the appropriate line below to run:

# Run full analysis (all objectives, all datasets)
main_execution()

# Run specific objective for specific dataset and objective number, 
# e.g. 1 for primary outcomes, 2 for safety/toxicity, 3 for repeat radiation efficacy, 4 for GEP validation
# run_specific_objective("uveal_melanoma_full_cohort", 1)

# Close logging if enabled
if (USE_LOGS) {
    sink(type = "message")
    sink()
    close(log_con)
    log_enhanced("Log file closed successfully")
}

