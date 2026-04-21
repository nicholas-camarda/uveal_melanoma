#' Run Objective 1: Primary Outcomes Analysis
#'
#' Performs comprehensive analysis of primary outcomes for uveal melanoma patients:
#' - 1a: Local recurrence rates (binary outcome, post-treatment only)
#' - 1b: Metastatic progression rates (binary outcome, post-treatment only)
#' - 1c: Overall survival analysis (time-to-event, Kaplan-Meier + Cox regression)
#' - 1d: Progression-free survival (time-to-event, Kaplan-Meier + Cox regression)
#' - 1e: Tumor height changes (continuous outcome, linear regression)
#' - 1f: Subgroup analyses by age (<=65 vs >65) and sex (Female vs Male)
#'
#' All analyses adjust for confounders: age_at_diagnosis, sex, location, initial_t_stage,
#' initial_tumor_height, initial_tumor_diameter, biopsy1_gep, optic_nerve
#'
#' @param data Data frame containing the analytic dataset for one cohort (full_cohort, restricted_cohort, or gksrs_only_cohort)
#' @param dataset_name Character string identifying the cohort for file naming and logging
#' @param output_dirs List of output directories organized by analysis type (recurrence, mets, os, pfs, height, subgroups)
#' @param prefix Character string prefix for cohort identification in output files (e.g., "full_cohort_", "restricted_cohort_", "gksrs_only_cohort_")
#' @param confounders Character vector of confounder variables to use for statistical adjustment
#' @return List containing all analysis results, model objects, and output file paths for each analysis type
run_objective_1 <- function(data, dataset_name, output_dirs, prefix, confounders = confounders) {
    step1_start_time <- Sys.time()
    display_name <- tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))
    log_phase(paste("STEP 1: PRIMARY OUTCOMES ANALYSIS", display_name, sep = " - "))

    # Determine cohort-specific subgroup variables and forest plot variables
    all_subgroup_vars <- subgroup_vars
    cohort_constants <- COHORT_CONSTANT_VARIABLES[[dataset_name]]
    if (is.null(cohort_constants)) cohort_constants <- character(0)
    cohort_subgroup_vars <- setdiff(all_subgroup_vars, cohort_constants)
    if (!identical(all_subgroup_vars, cohort_subgroup_vars)) {
        logger::log_info(formatted(sprintf(
            "Removing non-varying subgroup variables for %s: %s",
            dataset_name,
            paste(setdiff(all_subgroup_vars, cohort_subgroup_vars), collapse = ", ")
        ), indent = 1))
    }

    cohort_forest_variable_order <- setdiff(FOREST_PLOT_VARIABLE_ORDER, cohort_constants)
    if (length(cohort_forest_variable_order) == 0) {
        cohort_forest_variable_order <- FOREST_PLOT_VARIABLE_ORDER
    }

    # Display the confounders that will be used for statistical adjustment
    logger::log_info(formatted(
        sprintf(
            "Using %d confounders for statistical adjustment: %s",
            length(confounders), paste(confounders, collapse = ", ")
        ),
        indent = 1
    ))

    # 1a. Rates of recurrence (post-treatment only)
    logger::log_info(formatted("Executing analyze_binary_outcome_rates: Local recurrence rates analysis (post-treatment only)", indent = 1))
    recurrence_rates <- analyze_binary_outcome_rates(
        data,
        outcome_var = "recurrence1",
        time_var = "tt_recurrence_months",
        event_var = "recurrence_event",
        confounders = confounders,
        analysis_type = "post_treatment_only",
        dataset_name = dataset_name,
        output_dirs = output_dirs,
        prefix = prefix
    )
    logger::log_info(formatted("Local recurrence analysis completed", indent = 1))

    # 1a1. Overall survival stratified by local recurrence status
    logger::log_warn(formatted(
        "Legacy exploratory one-off analysis: recurrence-stratified OS/PFS uses post-baseline recurrence status, sits outside the original formal objectives, and must not be interpreted as a baseline treatment comparison.",
        indent = 1
    ))
    logger::log_info(formatted("1a1: Recurrence-stratified overall survival (KM)", indent = 1))
    recurrence_os <- analyze_os_by_local_recurrence(
        data = data,
        dataset_name = dataset_name,
        output_dirs = output_dirs,
        prefix = prefix,
        confounders = confounders
    )
    logger::log_info(formatted("Recurrence-stratified overall survival completed", indent = 1))

    # 1a2. Progression-free survival stratified by local recurrence status
    logger::log_info(formatted("1a2: Recurrence-stratified progression-free survival (KM)", indent = 1))
    recurrence_pfs <- analyze_pfs_by_local_recurrence(
        data = data,
        dataset_name = dataset_name,
        output_dirs = output_dirs,
        prefix = prefix,
        confounders = confounders
    )
    logger::log_info(formatted("Recurrence-stratified progression-free survival completed", indent = 1))

    # 1b. Rates of metastatic progression (post-treatment only)
    logger::log_info(formatted("Executing analyze_binary_outcome_rates: Metastatic progression rates analysis (post-treatment only)", indent = 1))
    mets_rates <- analyze_binary_outcome_rates(
        data,
        outcome_var = "mets_progression",
        time_var = "tt_mets_months",
        event_var = "mets_event",
        confounders = confounders,
        analysis_type = "post_treatment_only",
        dataset_name = dataset_name,
        output_dirs = output_dirs,
        prefix = prefix
    )
    logger::log_info(formatted("Metastatic progression analysis completed", indent = 1))

    # 2a1. Overall survival stratified by metastatic progression status
    logger::log_warn(formatted(
        "Legacy exploratory one-off analysis: metastasis-stratified OS/PFS uses post-baseline metastatic progression status, sits outside the original formal objectives, and must not be interpreted as a baseline treatment comparison.",
        indent = 1
    ))
    logger::log_info(formatted("2a1: Metastasis-stratified overall survival (KM)", indent = 1))
    metastasis_os <- analyze_os_by_metastatic_progression(
        data = data,
        dataset_name = dataset_name,
        output_dirs = output_dirs,
        prefix = prefix,
        confounders = confounders
    )
    logger::log_info(formatted("Metastasis-stratified overall survival completed", indent = 1))

    # 2a2. Progression-free survival stratified by metastatic progression status
    logger::log_info(formatted("2a2: Metastasis-stratified progression-free survival (KM)", indent = 1))
    metastasis_pfs <- analyze_pfs_by_metastatic_progression(
        data = data,
        dataset_name = dataset_name,
        output_dirs = output_dirs,
        prefix = prefix,
        confounders = confounders
    )
    logger::log_info(formatted("Metastasis-stratified progression-free survival completed", indent = 1))

    # 1c. Overall Survival (post-treatment only)
    logger::log_info(formatted("Executing analyze_time_to_event_outcomes: Overall survival analysis (Kaplan-Meier & Cox regression)", indent = 1))
    os_analysis <- analyze_time_to_event_outcomes(
        data,
        time_var = "tt_death_months",
        event_var = "death_event",
        group_var = "treatment_group",
        confounders = confounders,
        ylab = "Overall Survival Probability",
        analysis_type = "post_treatment_only",
        dataset_name = dataset_name,
        output_dirs = output_dirs,
        prefix = prefix
    )
    logger::log_info(formatted("Overall survival analysis completed", indent = 1))

    # Proportional hazards diagnostics (OS)
    try(
        {
            os_analysis$ph_diagnostics <- run_or_skip_proportional_hazards_diagnostics(
                cox_model = os_analysis$cox_model,
                outcome_name = "Overall Survival Probability",
                output_dir = output_dirs$obj1_ph_diagnostics,
                file_prefix = paste0(prefix, "overall_survival_probability_"),
                dataset_name = dataset_name,
                data = data,
                time_var = "tt_death_months",
                event_var = "death_event",
                variables = unique(c("treatment_group", confounders)),
                reason = "Overall Survival Probability proportional hazards diagnostics were not run because no Cox model was fit."
            )
        },
        silent = TRUE
    )

    # 1d. Progression Free Survival (includes both progression AND death)
    logger::log_info(formatted("Executing analyze_time_to_event_outcomes: Progression-free survival analysis (progression OR death)", indent = 1))
    pfs_analysis <- analyze_time_to_event_outcomes(
        data,
        time_var = "tt_pfs_months",
        event_var = "pfs_event",
        group_var = "treatment_group",
        confounders = confounders,
        ylab = "Progression-Free Survival Probability",
        analysis_type = "post_treatment_only",
        dataset_name = dataset_name,
        output_dirs = output_dirs,
        prefix = prefix
    )
    logger::log_info(formatted("Progression-free survival analysis completed", indent = 1))

    # Proportional hazards diagnostics (PFS)
    try(
        {
            pfs_analysis$ph_diagnostics <- run_or_skip_proportional_hazards_diagnostics(
                cox_model = pfs_analysis$cox_model,
                outcome_name = "Progression-Free Survival Probability",
                output_dir = output_dirs$obj1_ph_diagnostics,
                file_prefix = paste0(prefix, "progression_free_survival_probability_"),
                dataset_name = dataset_name,
                data = data,
                time_var = "tt_pfs_months",
                event_var = "pfs_event",
                variables = unique(c("treatment_group", confounders)),
                reason = "Progression-Free Survival Probability proportional hazards diagnostics were not run because no Cox model was fit."
            )
        },
        silent = TRUE
    )

    # 1e. Tumor height changes
    logger::log_info(formatted("Executing analyze_tumor_height_changes: Primary and sensitivity tumor height analysis", indent = 1))
    height_changes <- analyze_tumor_height_changes(data, output_dirs, prefix, confounders)
    logger::log_info(formatted("Tumor height changes analysis completed", indent = 1))
    logger::log_info(formatted("Creating tumor size by treatment group summary and plot", indent = 1))
    tumor_size_summary <- summarize_tumor_size_by_treatment(
        data = data,
        size_var = "initial_tumor_height",
        output_dir = output_dirs$obj1_height_primary,
        prefix = prefix
    )
    baseline_diameter_summary <- summarize_tumor_size_by_treatment(
        data = data,
        size_var = "initial_tumor_diameter",
        output_dir = output_dirs$obj1_height_primary,
        prefix = prefix
    )
    logger::log_info(formatted("Tumor size by treatment group outputs completed", indent = 1))

    # 1f. Subgroup analysis with interaction terms
    logger::log_info(formatted("Executing analyze_treatment_effect_subgroups_height: Subgroup analysis with interaction terms for tumor height change", indent = 1))

    # PRIMARY ANALYSIS: Without baseline height adjustment
    primary_start_time <- Sys.time()
    logger::log_info(formatted("PRIMARY SUBGROUP ANALYSIS (without baseline height adjustment)", indent = 1))
    primary_subgroup_results <- list()
    for (i in seq_along(cohort_subgroup_vars)) {
        subgroup_var <- cohort_subgroup_vars[i]
        logger::log_info(formatted(sprintf(">>> Testing PRIMARY interaction (%d/%d): %s", i, length(cohort_subgroup_vars), subgroup_var)))

        # Test the interaction with confounders but without baseline height
        result <- analyze_treatment_effect_subgroups_height(
            data = data,
            subgroup_var = subgroup_var,
            confounders = confounders, # Pass confounders (will auto-exclude subgroup var)
            include_baseline_height = FALSE, # PRIMARY: no baseline height adjustment
            dataset_name = dataset_name
        )

        # Store results
        primary_subgroup_results[[subgroup_var]] <- result

        # Log the interaction p-value
        if (!is.na(result$interaction_p)) {
            p_status <- if (result$interaction_p < 0.05) "SIGNIFICANT" else "non-significant"
            logger::log_info(formatted(sprintf("PRIMARY Interaction p-value: %.4f (%s)", result$interaction_p, p_status), indent = 2))
        } else {
            logger::log_warn(formatted("PRIMARY Interaction p-value: NA (model issue)", indent = 2))
        }
    }
    logger::log_info(formatted(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)", "PRIMARY SUBGROUP ANALYSIS", as.numeric(difftime(Sys.time(), primary_start_time, units = "secs"))), indent = 1))

    # SENSITIVITY ANALYSIS: With baseline height adjustment
    sensitivity_start_time <- Sys.time()
    logger::log_info(formatted("SENSITIVITY SUBGROUP ANALYSIS (with baseline height adjustment)", indent = 1))
    sensitivity_subgroup_results <- list()
    for (i in seq_along(cohort_subgroup_vars)) {
        subgroup_var <- cohort_subgroup_vars[i]
        logger::log_info(formatted(sprintf(">>> Testing SENSITIVITY interaction (%d/%d): %s", i, length(cohort_subgroup_vars), subgroup_var)))

        # Test the interaction with confounders including baseline height
        result <- analyze_treatment_effect_subgroups_height(
            data = data,
            subgroup_var = subgroup_var,
            confounders = confounders, # Pass confounders (will auto-exclude subgroup var)
            include_baseline_height = TRUE, # SENSITIVITY: include baseline height adjustment
            dataset_name = dataset_name
        )

        # Store results
        sensitivity_subgroup_results[[subgroup_var]] <- result

        # Log the interaction p-value
        if (!is.na(result$interaction_p)) {
            p_status <- if (result$interaction_p < 0.05) "SIGNIFICANT" else "non-significant"
            logger::log_info(formatted(sprintf("SENSITIVITY Interaction p-value: %.4f (%s)", result$interaction_p, p_status), indent = 2))
        } else {
            logger::log_warn(formatted("SENSITIVITY Interaction p-value: NA (model issue)", indent = 2))
        }
    }
    logger::log_info(formatted(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)", "SENSITIVITY SUBGROUP ANALYSIS", as.numeric(difftime(Sys.time(), sensitivity_start_time, units = "secs"))), indent = 1))

    # Create formatted HTML tables for subgroup analyses
    logger::log_info(formatted("Executing format_subgroup_analysis_tables: Creating formatted PRIMARY subgroup analysis tables", indent = 1))
    format_subgroup_analysis_tables(
        subgroup_results = primary_subgroup_results,
        dataset_name = paste("PRIMARY -", display_name),
        subgroup_dir = output_dirs$obj1_subgroup_primary,
        prefix = paste0(prefix, "primary_")
    )

    logger::log_info(formatted("Executing format_subgroup_analysis_tables: Creating formatted SENSITIVITY subgroup analysis tables", indent = 1))
    format_subgroup_analysis_tables(
        subgroup_results = sensitivity_subgroup_results,
        dataset_name = paste("SENSITIVITY -", display_name),
        subgroup_dir = output_dirs$obj1_subgroup_sensitivity,
        prefix = paste0(prefix, "sensitivity_")
    )

    # Create forest plots for tumor height subgroup analyses
    logger::log_info(formatted("Executing create_forest_plots_height: Creating forest plots for tumor height subgroup analyses", indent = 1))

    # Initialize forest plot diagnostics collector
    diagnostics_list <- list()

    # Forest plot for PRIMARY tumor height subgroup analysis (without baseline height)
    primary_height_forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = primary_subgroup_results,
        outcome_name = "Tumor Height Change (Primary Analysis)",
        cohort_name = display_name,
        treatment_labels = TREATMENT_LABELS,
        variable_order = cohort_forest_variable_order,
        effect_measure = "MD", # Mean Difference for continuous outcome
        favours_labels = FAVOURS_LABELS,
        title = sprintf("Subgroup Analysis: Tumor Height Change - Primary (%s)", display_name)
    )

    # Collect diagnostics using dedicated function with raw data
    diagnostics_list[["tumor_height_primary"]] <- create_forest_plot_diagnostics(
        subgroup_results = primary_subgroup_results,
        effect_measure = "MD",
        variable_order = cohort_forest_variable_order
    )

    # Save the PRIMARY forest plot
    png(file.path(output_dirs$obj1_forest_plots, paste0(prefix, "tumor_height_primary_subgroup_forest_plot.png")),
        width = FOREST_PLOT_WIDTH, height = compute_forest_plot_height(primary_height_forest_plot), units = PLOT_UNITS, res = PLOT_DPI
    )
    plot(primary_height_forest_plot)
    dev.off()
    logger::log_info(formatted("PRIMARY tumor height forest plot created", indent = 1))

    # Forest plot for SENSITIVITY tumor height subgroup analysis (with baseline height)
    sensitivity_height_forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = sensitivity_subgroup_results,
        outcome_name = "Tumor Height Change (Sensitivity Analysis)",
        cohort_name = display_name,
        treatment_labels = TREATMENT_LABELS,
        variable_order = cohort_forest_variable_order,
        effect_measure = "MD", # Mean Difference for continuous outcome
        favours_labels = FAVOURS_LABELS,
        title = sprintf("Subgroup Analysis: Tumor Height Change - Sensitivity (%s)", display_name)
    )

    # Collect diagnostics using dedicated function with raw data
    diagnostics_list[["tumor_height_sensitivity"]] <- create_forest_plot_diagnostics(
        subgroup_results = sensitivity_subgroup_results,
        effect_measure = "MD",
        variable_order = cohort_forest_variable_order
    )

    # Save the SENSITIVITY forest plot
    png(file.path(output_dirs$obj1_forest_plots, paste0(prefix, "tumor_height_sensitivity_subgroup_forest_plot.png")),
        width = FOREST_PLOT_WIDTH, height = compute_forest_plot_height(sensitivity_height_forest_plot), units = PLOT_UNITS, res = PLOT_DPI
    )
    plot(sensitivity_height_forest_plot)
    dev.off()
    logger::log_info(formatted("SENSITIVITY tumor height forest plot created", indent = 1))

    # Save both sets of subgroup analysis results for this dataset
    saveRDS(
        primary_subgroup_results,
        file.path(output_dirs$obj1_subgroup_primary, paste0(prefix, "primary_subgroup_interactions.rds"))
    )

    saveRDS(
        sensitivity_subgroup_results,
        file.path(output_dirs$obj1_subgroup_sensitivity, paste0(prefix, "sensitivity_subgroup_interactions.rds"))
    )

    # PRIMARY TUMOR HEIGHT SUBGROUP ANALYSIS CONSOLIDATION
    primary_diagnostics_list <- list()
    for (i in seq_along(cohort_subgroup_vars)) {
        subgroup_var <- cohort_subgroup_vars[i]
        logger::log_info(formatted(sprintf(">>> Testing PRIMARY interaction (%d/%d): %s", i, length(cohort_subgroup_vars), subgroup_var)))
        result <- analyze_treatment_effect_subgroups_height(
            data = data,
            subgroup_var = subgroup_var,
            confounders = confounders,
            include_baseline_height = FALSE
        )
        if (!is.null(result$subgroup_effects) && nrow(result$subgroup_effects) > 0) {
            tab_name <- tools::toTitleCase(gsub("_", " ", subgroup_var))
            tab_name <- gsub("[^A-Za-z0-9_]", "_", tab_name)
            tab_name <- substr(tab_name, 1, 31)
            header_row <- data.frame(
                subgroup_variable = subgroup_var,
                subgroup_level = "__HEADER__",
                n_total = NA, n_plaque = NA, n_gksrs = NA,
                events_plaque = NA, events_gksrs = NA,
                treatment_effect = NA, ci_lower = NA, ci_upper = NA,
                p_value = result$interaction_p,
                other_variable_contents = "",
                stringsAsFactors = FALSE
            )
            # Ensure detail rows have the column present
            result$subgroup_effects$other_variable_contents <- ""
            # Bind header row before the detailed subgroup rows
            df_out <- rbind(header_row, result$subgroup_effects)
            primary_diagnostics_list[[tab_name]] <- df_out
        }
    }
    consolidated_primary_path <- file.path(output_dirs$obj1_subgroup_primary, paste0(prefix, "primary_tumor_height_diagnostics.xlsx"))
    writexl::write_xlsx(primary_diagnostics_list, consolidated_primary_path)
    logger::log_info(formatted(sprintf("Primary tumor height diagnostics written to %s with %d tabs", consolidated_primary_path, length(primary_diagnostics_list)), indent = 1))

    # SENSITIVITY TUMOR HEIGHT SUBGROUP ANALYSIS CONSOLIDATION
    sensitivity_diagnostics_list <- list()
    for (i in seq_along(cohort_subgroup_vars)) {
        subgroup_var <- cohort_subgroup_vars[i]
        logger::log_info(formatted(sprintf(">>> Testing SENSITIVITY interaction (%d/%d): %s", i, length(cohort_subgroup_vars), subgroup_var)))
        result <- analyze_treatment_effect_subgroups_height(
            data = data,
            subgroup_var = subgroup_var,
            confounders = confounders,
            include_baseline_height = TRUE
        )
        if (!is.null(result$subgroup_effects) && nrow(result$subgroup_effects) > 0) {
            tab_name <- tools::toTitleCase(gsub("_", " ", subgroup_var))
            tab_name <- gsub("[^A-Za-z0-9_]", "_", tab_name)
            tab_name <- substr(tab_name, 1, 31)
            header_row <- data.frame(
                subgroup_variable = subgroup_var,
                subgroup_level = "__HEADER__",
                n_total = NA, n_plaque = NA, n_gksrs = NA,
                events_plaque = NA, events_gksrs = NA,
                treatment_effect = NA, ci_lower = NA, ci_upper = NA,
                p_value = result$interaction_p,
                other_variable_contents = "",
                stringsAsFactors = FALSE
            )
            result$subgroup_effects$other_variable_contents <- ""
            df_out <- rbind(header_row, result$subgroup_effects)
            sensitivity_diagnostics_list[[tab_name]] <- df_out
        }
    }
    consolidated_sensitivity_path <- file.path(output_dirs$obj1_subgroup_sensitivity, paste0(prefix, "sensitivity_tumor_height_diagnostics.xlsx"))
    writexl::write_xlsx(sensitivity_diagnostics_list, consolidated_sensitivity_path)
    logger::log_info(formatted(sprintf("Sensitivity tumor height diagnostics written to %s with %d tabs", consolidated_sensitivity_path, length(sensitivity_diagnostics_list)), indent = 1))

    # 1g. PRIMARY OUTCOMES SUBGROUP ANALYSIS
    logger::log_info(formatted("Executing primary_outcomes_subgroup_analysis: Subgroup analysis for primary clinical outcomes", indent = 1))

    # Perform subgroup analysis for each primary outcome
    primary_outcomes_start_time <- Sys.time()
    logger::log_info(formatted("PRIMARY OUTCOMES SUBGROUP ANALYSIS", indent = 1))

    # 1g1. Local Recurrence Subgroup Analysis
    logger::log_info(formatted("Analyzing subgroup effects for Local Recurrence", indent = 1))
    recurrence_subgroup_analysis <- analyze_treatment_effect_subgroups_binary(
        data = data,
        outcome_var = "recurrence1",
        subgroup_vars = cohort_subgroup_vars,
        confounders = confounders,
        outcome_name = "Local Recurrence",
        dataset_name = dataset_name
    )
    recurrence_subgroup_results <- recurrence_subgroup_analysis$subgroup_results

    # Create forest plot for local recurrence
    recurrence_forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = recurrence_subgroup_results,
        outcome_name = "Local Recurrence",
        cohort_name = display_name,
        treatment_labels = TREATMENT_LABELS,
        variable_order = cohort_forest_variable_order,
        effect_measure = "OR",
        favours_labels = FAVOURS_LABELS,
        title = sprintf("Subgroup Analysis: Local Recurrence (%s)", display_name)
    )

    # Save the forest plot
    png(file.path(output_dirs$obj1_forest_plots, paste0(prefix, "local_recurrence_subgroup_forest_plot.png")),
        width = FOREST_PLOT_WIDTH, height = compute_forest_plot_height(recurrence_forest_plot), units = PLOT_UNITS, res = PLOT_DPI
    )
    plot(recurrence_forest_plot)
    dev.off()
    logger::log_info(formatted("Local recurrence subgroup analysis completed", indent = 1))

    # 1g2. Metastatic Progression Subgroup Analysis
    logger::log_info(formatted("Analyzing subgroup effects for Metastatic Progression", indent = 1))
    mets_subgroup_analysis <- analyze_treatment_effect_subgroups_binary(
        data = data,
        outcome_var = "mets_progression",
        subgroup_vars = cohort_subgroup_vars,
        confounders = confounders,
        outcome_name = "Metastatic Progression",
        dataset_name = dataset_name
    )
    mets_subgroup_results <- mets_subgroup_analysis$subgroup_results

    # Create forest plot for metastatic progression
    mets_forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = mets_subgroup_results,
        outcome_name = "Metastatic Progression",
        cohort_name = display_name,
        treatment_labels = TREATMENT_LABELS,
        variable_order = cohort_forest_variable_order,
        effect_measure = "OR",
        favours_labels = FAVOURS_LABELS,
        title = sprintf("Subgroup Analysis: Metastatic Progression (%s)", display_name)
    )

    # Save the forest plot
    png(file.path(output_dirs$obj1_forest_plots, paste0(prefix, "metastatic_progression_subgroup_forest_plot.png")),
        width = FOREST_PLOT_WIDTH, height = compute_forest_plot_height(mets_forest_plot), units = PLOT_UNITS, res = PLOT_DPI
    )
    plot(mets_forest_plot)
    dev.off()
    logger::log_info(formatted("Metastatic progression subgroup analysis completed", indent = 1))

    # 1g3. Overall Survival Subgroup Analysis
    logger::log_info(formatted("Analyzing subgroup effects for Overall Survival", indent = 1))
    os_subgroup_analysis <- analyze_treatment_effect_subgroups_survival(
        data = data,
        time_var = "tt_death_months",
        event_var = "death_event",
        subgroup_vars = cohort_subgroup_vars,
        confounders = confounders,
        outcome_name = "Overall Survival",
        dataset_name = dataset_name
    )
    os_subgroup_results <- os_subgroup_analysis$subgroup_results

    # Create forest plot for overall survival
    os_forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = os_subgroup_results,
        outcome_name = "Overall Survival",
        cohort_name = display_name,
        treatment_labels = TREATMENT_LABELS,
        variable_order = cohort_forest_variable_order,
        effect_measure = "HR",
        favours_labels = FAVOURS_LABELS,
        title = sprintf("Subgroup Analysis: Overall Survival (%s)", display_name)
    )

    # Save the forest plot
    png(file.path(output_dirs$obj1_forest_plots, paste0(prefix, "overall_survival_subgroup_forest_plot.png")),
        width = FOREST_PLOT_WIDTH, height = compute_forest_plot_height(os_forest_plot), units = PLOT_UNITS, res = PLOT_DPI
    )
    plot(os_forest_plot)
    dev.off()
    logger::log_info(formatted("Overall survival subgroup analysis completed", indent = 1))

    # 1g4. Progression-Free Survival Subgroup Analysis
    logger::log_info(formatted("Analyzing subgroup effects for Progression-Free Survival", indent = 1))
    pfs_subgroup_analysis <- analyze_treatment_effect_subgroups_survival(
        data = data,
        time_var = "tt_pfs_months",
        event_var = "pfs_event",
        subgroup_vars = cohort_subgroup_vars,
        confounders = confounders,
        outcome_name = "Progression-Free Survival",
        dataset_name = dataset_name
    )
    pfs_subgroup_results <- pfs_subgroup_analysis$subgroup_results

    # Create forest plot for progression-free survival
    pfs_forest_plot <- create_single_cohort_forest_plot(
        subgroup_results = pfs_subgroup_results,
        outcome_name = "Progression-Free Survival",
        cohort_name = display_name,
        treatment_labels = TREATMENT_LABELS,
        variable_order = cohort_forest_variable_order,
        effect_measure = "HR",
        favours_labels = FAVOURS_LABELS,
        title = sprintf("Subgroup Analysis: Progression-Free Survival (%s)", display_name)
    )

    # Save the forest plot
    png(file.path(output_dirs$obj1_forest_plots, paste0(prefix, "progression_free_survival_subgroup_forest_plot.png")),
        width = FOREST_PLOT_WIDTH, height = compute_forest_plot_height(pfs_forest_plot), units = PLOT_UNITS, res = PLOT_DPI
    )
    plot(pfs_forest_plot)
    dev.off()
    logger::log_info(formatted("Progression-free survival subgroup analysis completed", indent = 1))

    # Combined primary-outcome forest plot (2x2 layout)
    combined_input_grobs <- list(
        recurrence_forest_plot,
        mets_forest_plot,
        os_forest_plot,
        pfs_forest_plot
    )
    combined_panel <- tryCatch(
        combine_forest_plot_panels(
            grobs = combined_input_grobs,
            panel_labels = c(
                "a. Local Recurrence",
                "b. Metastatic Progression",
                "c. Overall Survival",
                "d. Progression-Free Survival"
            ),
            ncol = 2
        ),
        error = function(e) {
            logger::log_warn(formatted(sprintf("Unable to assemble composite forest plot: %s", e$message), indent = 1))
            NULL
        }
    )

    if (!is.null(combined_panel)) {
        combined_png_path <- file.path(output_dirs$obj1_forest_plots, paste0(prefix, "primary_outcomes_composite_forest_plot.png"))
        panel_count <- sum(vapply(combined_input_grobs, function(x) !is.null(x), logical(1)))
        col_count <- attr(combined_panel, "column_count")
        if (is.null(col_count) || !is.finite(col_count) || col_count <= 0) {
            col_count <- min(2, max(1, panel_count))
        }
        row_height_inches <- attr(combined_panel, "row_height_inches")
        if (is.null(row_height_inches) || !all(is.finite(row_height_inches))) {
            fallback_rows <- ceiling(panel_count / col_count)
            row_height_inches <- rep(FOREST_PLOT_HEIGHT, fallback_rows)
        }
        # Add margins: top (0.1) + bottom (0.5) = 0.6
        total_height_inches <- sum(row_height_inches) + 0.6
        png(
            combined_png_path,
            width = FOREST_PLOT_WIDTH * col_count,
            height = total_height_inches,
            units = PLOT_UNITS,
            res = PLOT_DPI
        )
        grid::grid.draw(combined_panel)
        dev.off()
        logger::log_info(formatted(sprintf("Composite primary-outcomes forest plot saved to %s", combined_png_path), indent = 1))
        
    } else {
        logger::log_warn(formatted("Composite primary-outcomes forest plot skipped (missing inputs)", indent = 1))
    }

    # FOREST PLOT DIAGNOSTICS COLLECTION
    diagnostics_list[["local_recurrence"]] <- create_forest_plot_diagnostics(
        subgroup_results = recurrence_subgroup_results,
        effect_measure = "OR",
        variable_order = cohort_forest_variable_order
    )
    diagnostics_list[["metastatic_progression"]] <- create_forest_plot_diagnostics(
        subgroup_results = mets_subgroup_results,
        effect_measure = "OR",
        variable_order = cohort_forest_variable_order
    )
    diagnostics_list[["overall_survival"]] <- create_forest_plot_diagnostics(
        subgroup_results = os_subgroup_results,
        effect_measure = "HR",
        variable_order = cohort_forest_variable_order
    )
    diagnostics_list[["progression_free_survival"]] <- create_forest_plot_diagnostics(
        subgroup_results = pfs_subgroup_results,
        effect_measure = "HR",
        variable_order = cohort_forest_variable_order
    )

    # Save forest plot diagnostics
    consolidated_forest_path <- file.path(output_dirs$obj1_forest_plots, paste0(prefix, "forest_plot_diagnostics.xlsx"))
    diagnostics_list_no_interaction <- lapply(diagnostics_list, function(df) {
        if (is.data.frame(df) && ("interaction_p" %in% names(df))) {
            df <- df[, setdiff(names(df), "interaction_p"), drop = FALSE]
        }
        df
    })
    writexl::write_xlsx(diagnostics_list_no_interaction, consolidated_forest_path)
    logger::log_info(formatted(sprintf("Forest plot diagnostics written to %s with %d tabs", consolidated_forest_path, length(diagnostics_list)), indent = 1))

    logger::log_info(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)",
        "STEP 1: PRIMARY OUTCOMES ANALYSIS",
        as.numeric(difftime(Sys.time(), step1_start_time, units = "secs"))
    ))

    return(list(
        recurrence_rates = recurrence_rates,
        recurrence_os = recurrence_os,
        recurrence_pfs = recurrence_pfs,
        mets_rates = mets_rates,
        metastasis_os = metastasis_os,
        metastasis_pfs = metastasis_pfs,
        os_analysis = os_analysis,
        pfs_analysis = pfs_analysis,
        height_changes = height_changes,
        primary_subgroup_results = primary_subgroup_results,
        sensitivity_subgroup_results = sensitivity_subgroup_results
    ))
}
