# Subgroup Binary Outcome Analysis

#' Subgroup analysis for binary outcomes (logistic with interaction)
#' @param data Data frame
#' @param outcome_var Binary outcome
#' @param subgroup_vars Subgroup variables
#' @param confounders Confounders
#' @param outcome_name Label
#' @param dataset_name Optional dataset name for other_map
#' @return List of results per subgroup
analyze_treatment_effect_subgroups_binary <- function(data, outcome_var, subgroup_vars, confounders = NULL, outcome_name = "Binary Outcome", dataset_name = NULL) {
    log_enhanced(sprintf("Performing subgroup analysis for %s", outcome_name), level = "INFO")
    subgroup_results <- list()
    cohort_other_map <- list()
    if (!is.null(dataset_name)) cohort_other_map <- tryCatch(get_cohort_specific_other_map(dataset_name), error = function(e) list())
    for (subgroup_var in subgroup_vars) {
        log_enhanced(sprintf("Testing interaction for: %s", subgroup_var), level = "INFO")
        res <- tryCatch(
            {
                processed <- process_subgroup_data(data, subgroup_var, confounders, FALSE)
                if (!is.null(processed$error) && processed$error == "insufficient_levels") {
                    list(interaction_p = NA, subgroup_effects = data.frame(), error = "insufficient_levels")
                } else {
                    outcome_config <- list(type = "binary", outcome_var = outcome_var)
                    model_results <- fit_subgroup_model(processed$data, outcome_config, processed$subgroup_var_to_use, processed$confounders_to_use)
                    data_for_effects <- if (!is.null(model_results$filtered_data)) model_results$filtered_data else processed$data
                    subgroup_effects <- calculate_subgroup_effects(model_results$model, data_for_effects, processed$subgroup_var_to_use, outcome_config$type, subgroup_var)
                    list(
                        interaction_p = model_results$interaction_p, subgroup_effects = subgroup_effects, model = model_results$model,
                        subgroup_var_used = processed$subgroup_var_to_use, formula_used = model_results$formula_used,
                        confounders_used = processed$confounders_to_use, was_continuous = processed$was_continuous,
                        cutoff_value = processed$cutoff_value, interaction_diagnostics = model_results$interaction_diagnostics,
                        other_map = processed$other_map
                    )
                }
            },
            error = function(e) list(interaction_p = NA, subgroup_effects = data.frame(), error = e$message)
        )
        subgroup_results[[subgroup_var]] <- res
    }
    other_map <- if (length(cohort_other_map) > 0) {
        cohort_other_map
    } else {
        var_other_map <- list()
        for (var_name in names(subgroup_results)) if (!is.null(subgroup_results[[var_name]]$other_map)) var_other_map[[var_name]] <- subgroup_results[[var_name]]$other_map
        var_other_map
    }
    list(subgroup_results = subgroup_results, other_map = other_map)
}
