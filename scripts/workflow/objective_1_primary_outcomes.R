#' Determine the Objective 1 cohort interpretation role
#'
#' @param dataset_name Character dataset/cohort identifier.
#' @return A single-row data frame describing the comparative interpretation role.
build_objective1_cohort_interpretation <- function(dataset_name) {
    role <- dplyr::case_when(
        grepl("restricted", dataset_name %||% "", ignore.case = TRUE) ~ "primary_dual_eligible_comparative",
        grepl("full", dataset_name %||% "", ignore.case = TRUE) ~ "real_world_associational_context",
        grepl("gksrs", dataset_name %||% "", ignore.case = TRUE) ~ "gksrs_only_characterization",
        TRUE ~ "cohort_context_not_preclassified"
    )
    interpretation <- switch(role,
        primary_dual_eligible_comparative = "Restricted cohort is the primary dual-eligible treatment-comparison surface.",
        real_world_associational_context = "Full cohort is real-world associational context; treatment comparisons require confounding caution.",
        gksrs_only_characterization = "GKSRS-only cohort is characterization or exploratory support, not a primary treatment-comparison surface.",
        cohort_context_not_preclassified = "Cohort role was not preclassified; interpret treatment contrasts according to cohort construction."
    )

    data.frame(
        dataset = dataset_name %||% "unspecified_dataset",
        cohort_interpretation_role = role,
        interpretation = interpretation,
        stringsAsFactors = FALSE
    )
}

#' Write the centralized Objective 1 interpretation note
#'
#' @param output_dirs Named output directory list for one cohort.
#' @param dataset_name Character dataset/cohort identifier.
#' @param prefix Character file prefix for the cohort.
#' @return Path to the note file, invisibly.
write_objective1_interpretation_note <- function(output_dirs, dataset_name, prefix) {
    obj1_dir <- dirname(output_dirs$obj1_recurrence %||% output_dirs$obj1_os %||% getwd())
    if (!dir.exists(obj1_dir)) {
        dir.create(obj1_dir, recursive = TRUE, showWarnings = FALSE)
    }
    cohort_note <- build_objective1_cohort_interpretation(dataset_name)
    note_path <- file.path(obj1_dir, paste0(prefix, "objective1_interpretation_notes.txt"))
    note_lines <- c(
        "OBJECTIVE 1 INTERPRETATION NOTES",
        "",
        paste0("Dataset: ", cohort_note$dataset),
        paste0("Cohort role: ", cohort_note$cohort_interpretation_role),
        paste0("Interpretation: ", cohort_note$interpretation),
        "",
        "Recurrence and metastatic-progression endpoints use Cox-led time-to-event inference:",
        "- Adjusted Cox models are the lead reviewer-response treatment-effect summaries.",
        "- Ever-observed event counts and competing-risk cumulative incidence are descriptive support.",
        "",
        "Cox survival summaries use graded PH interpretation:",
        "- Cox-forward when PH diagnostics do not show concern.",
        "- Cox-with-PH-caution for mild or isolated PH concerns.",
        "- RMST/KM-forward when treatment and global PH concerns are material and RMST/KM context is available.",
        "- Cox-limited when PH diagnostics are not supportable."
    )
    writeLines(note_lines, note_path)
    invisible(note_path)
}

#' Write an artifact-level note for legacy post-baseline Objective 1 outputs
#'
#' @param output_dir Directory containing a legacy post-baseline output bundle.
#' @param analysis_label Character label for the legacy analysis.
#' @param stratifier_label Character label for the post-baseline stratifier.
#' @return Path to the note file, invisibly.
write_objective1_legacy_exploratory_note <- function(output_dir, analysis_label, stratifier_label) {
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    note_path <- file.path(output_dir, "post_baseline_exploratory_note.txt")
    writeLines(c(
        "POST-BASELINE EXPLORATORY ANALYSIS",
        "",
        paste0("Analysis: ", analysis_label),
        paste0("Post-baseline stratifier: ", stratifier_label),
        "",
        "This output stratifies OS/PFS by a post-baseline event status.",
        "It is exploratory and non-causal, and it must not be interpreted as a baseline treatment comparison."
    ), note_path)
    invisible(note_path)
}

#' Classify Objective 1 proportional-hazards interpretation severity
#'
#' @param ph_diagnostics Result from proportional-hazards diagnostics.
#' @param rmst_results RMST result table for the same endpoint.
#' @return Single-row data frame with PH interpretation metadata.
classify_objective1_ph_interpretation <- function(ph_diagnostics, rmst_results = NULL) {
    if (is.null(ph_diagnostics) || is.null(ph_diagnostics$ph_summary)) {
        return(data.frame(
            PH_Interpretation = "cox_limited_ph_untestable",
            PH_Interpretation_Reason = "PH diagnostics unavailable due to low event support, missing Cox model, or diagnostic failure.",
            Interpretation_Priority = "Cox HR limited; interpret with event support and RMST/KM context where available.",
            stringsAsFactors = FALSE
        ))
    }

    ph_summary <- ph_diagnostics$ph_summary
    global_p <- ph_summary$P_Value[ph_summary$Variable == "GLOBAL"][1] %||% NA_real_
    treatment_p <- ph_summary$P_Value[grepl("treatment_group", ph_summary$Variable)][1] %||% NA_real_
    has_completed_rmst <- !is.null(rmst_results) &&
        is.data.frame(rmst_results) &&
        nrow(rmst_results) > 0 &&
        any(rmst_results$Analysis_Status == "completed", na.rm = TRUE)

    global_flag <- !is.na(global_p) && global_p < 0.05
    treatment_flag <- !is.na(treatment_p) && treatment_p < 0.05
    material_flag <- has_completed_rmst && (
        (!is.na(global_p) && !is.na(treatment_p) && global_p < 0.05 && treatment_p < 0.05) ||
            (!is.na(treatment_p) && treatment_p < 0.01)
    )

    if (material_flag) {
        return(data.frame(
            PH_Interpretation = "rmst_km_forward",
            PH_Interpretation_Reason = "Treatment-term and global PH diagnostics indicate material non-proportionality with RMST/KM context available.",
            Interpretation_Priority = "RMST/KM lead; Cox HR is secondary or time-compressed.",
            stringsAsFactors = FALSE
        ))
    }
    if (global_flag || treatment_flag) {
        reason <- if (global_flag && !treatment_flag) {
            "Global PH diagnostic concern without treatment-term PH concern."
        } else if (treatment_flag && !global_flag) {
            "Treatment-term PH diagnostic concern without global PH concern."
        } else {
            "PH diagnostics show some evidence against proportional hazards."
        }
        return(data.frame(
            PH_Interpretation = "cox_with_ph_caution",
            PH_Interpretation_Reason = reason,
            Interpretation_Priority = "Cox HR remains reportable with PH caution and RMST/KM triangulation.",
            stringsAsFactors = FALSE
        ))
    }

    data.frame(
        PH_Interpretation = "cox_forward",
        PH_Interpretation_Reason = "PH diagnostics did not show evidence against proportional hazards.",
        Interpretation_Priority = "Cox HR remains the lead model-based summary, with RMST/KM retained as absolute-time context.",
        stringsAsFactors = FALSE
    )
}

#' Add graded PH interpretation to an existing Objective 1 effect summary
#'
#' @param output_dir Directory containing the survival effect summary workbook.
#' @param prefix Character file prefix.
#' @param outcome_label Character survival outcome label used in filenames.
#' @param ph_diagnostics Result from proportional-hazards diagnostics.
#' @param rmst_results RMST result table for the same endpoint.
#' @return PH interpretation data frame, invisibly.
annotate_objective1_survival_effect_summary <- function(output_dir, prefix, outcome_label, ph_diagnostics, rmst_results = NULL) {
    interpretation <- classify_objective1_ph_interpretation(ph_diagnostics, rmst_results)
    summary_path <- file.path(output_dir, paste0(prefix, make_filename_safe(outcome_label), "_effect_summary.xlsx"))
    if (file.exists(summary_path)) {
        summary_rows <- readxl::read_xlsx(summary_path)
        summary_rows$PH_Interpretation <- interpretation$PH_Interpretation
        summary_rows$PH_Interpretation_Reason <- interpretation$PH_Interpretation_Reason
        summary_rows$Interpretation_Priority <- interpretation$Interpretation_Priority
        write_readable_xlsx(summary_rows, summary_path)
    }
    invisible(interpretation)
}

#' Write the Objective 1 subgroup artifact contract note
#'
#' @param output_dirs Named output directory list for one cohort.
#' @param dataset_name Character dataset/cohort identifier.
#' @param prefix Character file prefix for the cohort.
#' @return Path to the note file, invisibly.
write_objective1_subgroup_contract_note <- function(output_dirs, dataset_name, prefix) {
    subgroup_dir <- dirname(output_dirs$obj1_subgroup_primary %||% output_dirs$obj1_forest_plots %||% getwd())
    if (!dir.exists(subgroup_dir)) {
        dir.create(subgroup_dir, recursive = TRUE, showWarnings = FALSE)
    }
    note_path <- file.path(subgroup_dir, paste0(prefix, "subgroup_analysis_contract_note.txt"))
    writeLines(c(
        "OBJECTIVE 1 SUBGROUP ARTIFACT CONTRACT",
        "",
        paste0("Dataset: ", dataset_name %||% "unspecified_dataset"),
        "",
        "Primary tabular subgroup outputs are consolidated multi-sheet Excel diagnostics workbooks:",
        "- forest_plots/*_forest_plot_diagnostics.xlsx",
        "- tumor_height_primary/*_primary_tumor_height_diagnostics.xlsx",
        "- tumor_height_sensitivity/*_sensitivity_tumor_height_diagnostics.xlsx",
        "",
        "Companion artifacts include subgroup forest plots and subgroup interaction RDS objects.",
        "Per-subgroup HTML files are ancillary previews when emitted by the existing formatter.",
        "Subgroup analyses are exploratory support analyses and should not be interpreted as confirmatory interaction evidence."
    ), note_path)
    invisible(note_path)
}

#' Add exploratory interpretation metadata to subgroup diagnostic rows
#'
#' @param diagnostics Data frame of subgroup diagnostics.
#' @param dataset_name Character dataset/cohort identifier.
#' @param subgroup_surface Character subgroup output family.
#' @return Diagnostics data frame with exploratory interpretation columns.
annotate_objective1_subgroup_diagnostics <- function(diagnostics, dataset_name, subgroup_surface, reviewer_pruning = NULL) {
    if (is.null(diagnostics) || !is.data.frame(diagnostics)) {
        return(diagnostics)
    }
    sparse_note <- if (grepl("gksrs", dataset_name %||% "", ignore.case = TRUE)) {
        "GKSRS-only subgroup surface is sparse-support characterization; avoid confirmatory interaction language."
    } else {
        "Subgroup surface is exploratory support; interaction findings require cautious interpretation."
    }
    diagnostics$analysis_role <- "exploratory_support"
    diagnostics$subgroup_surface <- subgroup_surface
    diagnostics$interpretation_note <- sparse_note
    diagnostics$reviewer_exclusion_note <- NA_character_
    diagnostics$reviewer_excluded_level <- NA_character_
    diagnostics$reviewer_excluded_n <- NA_integer_

    if (!is.null(reviewer_pruning)) {
        variable_col <- if ("variable" %in% names(diagnostics)) {
            "variable"
        } else if ("subgroup_variable" %in% names(diagnostics)) {
            "subgroup_variable"
        } else {
            NULL
        }
        if (!is.null(variable_col)) {
            t4_mask <- diagnostics[[variable_col]] == "initial_t_stage_simple"
            if (any(t4_mask, na.rm = TRUE)) {
                diagnostics$reviewer_exclusion_note[t4_mask] <- "T4 excluded from reviewer-facing subgroup displays due to sparse and inconsistent support."
                diagnostics$reviewer_excluded_level[t4_mask] <- "T4"
                diagnostics$reviewer_excluded_n[t4_mask] <- reviewer_pruning$t4_excluded_n
            }
        }
    }
    diagnostics
}

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
    write_objective1_interpretation_note(output_dirs, dataset_name, prefix)
    write_objective1_subgroup_contract_note(output_dirs, dataset_name, prefix)

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
    reviewer_subgroup_pruning <- build_reviewer_subgroup_pruning_audit(data)

    # Display the confounders that will be used for statistical adjustment
    logger::log_info(formatted(
        sprintf(
            "Using %d confounders for statistical adjustment: %s",
            length(confounders), paste(confounders, collapse = ", ")
        ),
        indent = 1
    ))

    # 1a. Local recurrence: descriptive support plus Cox-led time-to-event analysis
    logger::log_info(formatted("Executing recurrence event-support summary and Cox time-to-local-recurrence analysis", indent = 1))
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
    recurrence_time_to_event <- analyze_time_to_event_outcomes(
        data,
        time_var = "tt_recurrence_months",
        event_var = "recurrence_event",
        group_var = "treatment_group",
        confounders = confounders,
        ylab = "Local Recurrence-Free Probability",
        analysis_type = "post_treatment_only",
        dataset_name = dataset_name,
        output_dirs = output_dirs,
        prefix = prefix,
        route_key = "obj1_recurrence"
    )
    recurrence_time_to_event$ph_diagnostics <- run_or_skip_proportional_hazards_diagnostics(
        cox_model = recurrence_time_to_event$cox_model,
        outcome_name = "Local Recurrence-Free Probability",
        output_dir = output_dirs$obj1_recurrence,
        file_prefix = paste0(prefix, "local_recurrence_free_probability_"),
        dataset_name = dataset_name,
        data = data,
        time_var = "tt_recurrence_months",
        event_var = "recurrence_event",
        variables = unique(c("treatment_group", confounders)),
        reason = "Local recurrence proportional hazards diagnostics were not run because no Cox model was fit."
    )
    annotate_objective1_survival_effect_summary(
        output_dir = output_dirs$obj1_recurrence,
        prefix = prefix,
        outcome_label = "Local Recurrence-Free Probability",
        ph_diagnostics = recurrence_time_to_event$ph_diagnostics,
        rmst_results = recurrence_time_to_event$rmst_analysis
    )
    logger::log_info(formatted("Local recurrence Cox time-to-event analysis completed", indent = 1))

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
    write_objective1_legacy_exploratory_note(
        output_dir = output_dirs$obj1_recurrence_1a1 %||% file.path(output_dirs$obj1_recurrence, "1a1_recurrence_stratified_os"),
        analysis_label = "Recurrence-stratified overall survival",
        stratifier_label = "Local recurrence status"
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
    write_objective1_legacy_exploratory_note(
        output_dir = output_dirs$obj1_recurrence_1a2 %||% file.path(output_dirs$obj1_recurrence, "1a2_recurrence_stratified_pfs"),
        analysis_label = "Recurrence-stratified progression-free survival",
        stratifier_label = "Local recurrence status"
    )
    logger::log_info(formatted("Recurrence-stratified progression-free survival completed", indent = 1))

    # 1b. Metastatic progression: descriptive support plus Cox-led time-to-event analysis
    logger::log_info(formatted("Executing metastasis event-support summary and Cox time-to-metastasis analysis", indent = 1))
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
    mets_time_to_event <- analyze_time_to_event_outcomes(
        data,
        time_var = "tt_mets_months",
        event_var = "mets_event",
        group_var = "treatment_group",
        confounders = confounders,
        ylab = "Metastasis-Free Survival Probability",
        analysis_type = "post_treatment_only",
        dataset_name = dataset_name,
        output_dirs = output_dirs,
        prefix = prefix,
        route_key = "obj1_mets"
    )
    mets_time_to_event$ph_diagnostics <- run_or_skip_proportional_hazards_diagnostics(
        cox_model = mets_time_to_event$cox_model,
        outcome_name = "Metastasis-Free Survival Probability",
        output_dir = output_dirs$obj1_mets,
        file_prefix = paste0(prefix, "metastasis_free_survival_probability_"),
        dataset_name = dataset_name,
        data = data,
        time_var = "tt_mets_months",
        event_var = "mets_event",
        variables = unique(c("treatment_group", confounders)),
        reason = "Metastasis proportional hazards diagnostics were not run because no Cox model was fit."
    )
    annotate_objective1_survival_effect_summary(
        output_dir = output_dirs$obj1_mets,
        prefix = prefix,
        outcome_label = "Metastasis-Free Survival Probability",
        ph_diagnostics = mets_time_to_event$ph_diagnostics,
        rmst_results = mets_time_to_event$rmst_analysis
    )
    logger::log_info(formatted("Metastasis Cox time-to-event analysis completed", indent = 1))

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
    write_objective1_legacy_exploratory_note(
        output_dir = output_dirs$obj1_mets_2a1 %||% file.path(output_dirs$obj1_mets, "2a1_metastasis_stratified_os"),
        analysis_label = "Metastasis-stratified overall survival",
        stratifier_label = "Metastatic progression status"
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
    write_objective1_legacy_exploratory_note(
        output_dir = output_dirs$obj1_mets_2a2 %||% file.path(output_dirs$obj1_mets, "2a2_metastasis_stratified_pfs"),
        analysis_label = "Metastasis-stratified progression-free survival",
        stratifier_label = "Metastatic progression status"
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
        prefix = prefix,
        route_key = "obj1_os"
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
    annotate_objective1_survival_effect_summary(
        output_dir = output_dirs$obj1_os,
        prefix = prefix,
        outcome_label = "Overall Survival Probability",
        ph_diagnostics = os_analysis$ph_diagnostics,
        rmst_results = os_analysis$rmst_analysis
    )
    os_5yr_capped <- fit_capped_cox_sensitivity(
        data = data,
        time_var = "tt_death_months",
        event_var = "death_event",
        horizon_months = 60,
        group_var = "treatment_group",
        confounders = confounders,
        output_dir = output_dirs$obj1_os,
        prefix = prefix,
        analysis_label = "Overall Survival Probability",
        dataset_name = dataset_name
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
        prefix = prefix,
        route_key = "obj1_pfs"
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
    annotate_objective1_survival_effect_summary(
        output_dir = output_dirs$obj1_pfs,
        prefix = prefix,
        outcome_label = "Progression-Free Survival Probability",
        ph_diagnostics = pfs_analysis$ph_diagnostics,
        rmst_results = pfs_analysis$rmst_analysis
    )
    pfs_5yr_capped <- fit_capped_cox_sensitivity(
        data = data,
        time_var = "tt_pfs_months",
        event_var = "pfs_event",
        horizon_months = 60,
        group_var = "treatment_group",
        confounders = confounders,
        output_dir = output_dirs$obj1_pfs,
        prefix = prefix,
        analysis_label = "Progression-Free Survival Probability",
        dataset_name = dataset_name
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
            df_out <- annotate_objective1_subgroup_diagnostics(
                diagnostics = df_out,
                dataset_name = dataset_name,
                subgroup_surface = "tumor_height_primary",
                reviewer_pruning = reviewer_subgroup_pruning
            )
            primary_diagnostics_list[[tab_name]] <- df_out
        }
    }
    consolidated_primary_path <- file.path(output_dirs$obj1_subgroup_primary, paste0(prefix, "primary_tumor_height_diagnostics.xlsx"))
    write_readable_xlsx(primary_diagnostics_list, consolidated_primary_path)
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
            df_out <- annotate_objective1_subgroup_diagnostics(
                diagnostics = df_out,
                dataset_name = dataset_name,
                subgroup_surface = "tumor_height_sensitivity",
                reviewer_pruning = reviewer_subgroup_pruning
            )
            sensitivity_diagnostics_list[[tab_name]] <- df_out
        }
    }
    consolidated_sensitivity_path <- file.path(output_dirs$obj1_subgroup_sensitivity, paste0(prefix, "sensitivity_tumor_height_diagnostics.xlsx"))
    write_readable_xlsx(sensitivity_diagnostics_list, consolidated_sensitivity_path)
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
        annotate_objective1_subgroup_diagnostics(
            diagnostics = df,
            dataset_name = dataset_name,
            subgroup_surface = "primary_outcomes_forest_plots",
            reviewer_pruning = reviewer_subgroup_pruning
        )
    })
    diagnostics_list_no_interaction[["reviewer_pruning_audit"]] <- reviewer_subgroup_pruning$audit
    write_readable_xlsx(diagnostics_list_no_interaction, consolidated_forest_path)
    logger::log_info(formatted(sprintf("Forest plot diagnostics written to %s with %d tabs", consolidated_forest_path, length(diagnostics_list)), indent = 1))

    logger::log_info(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)",
        "STEP 1: PRIMARY OUTCOMES ANALYSIS",
        as.numeric(difftime(Sys.time(), step1_start_time, units = "secs"))
    ))

    return(list(
        recurrence_rates = recurrence_rates,
        recurrence_time_to_event = recurrence_time_to_event,
        recurrence_os = recurrence_os,
        recurrence_pfs = recurrence_pfs,
        mets_rates = mets_rates,
        mets_time_to_event = mets_time_to_event,
        metastasis_os = metastasis_os,
        metastasis_pfs = metastasis_pfs,
        os_analysis = os_analysis,
        os_5yr_capped = os_5yr_capped,
        pfs_analysis = pfs_analysis,
        pfs_5yr_capped = pfs_5yr_capped,
        height_changes = height_changes,
        primary_subgroup_results = primary_subgroup_results,
        sensitivity_subgroup_results = sensitivity_subgroup_results
    ))
}
