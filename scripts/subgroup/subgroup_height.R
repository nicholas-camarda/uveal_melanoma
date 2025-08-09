# Subgroup Analysis for Tumor Height Change

#' Subgroup analysis for continuous outcome (height change)
#' @param data Data frame
#' @param subgroup_var Subgroup variable
#' @param confounders Confounders
#' @param include_baseline_height Logical; include baseline height
#' @param dataset_name Optional dataset name
#' @return List with interaction p-value, effects, and metadata
analyze_treatment_effect_subgroups_height <- function(data, subgroup_var, confounders = NULL, include_baseline_height = FALSE, dataset_name = NULL) {
    if (!subgroup_var %in% names(data)) {
        warning(sprintf("Variable '%s' not found in data", subgroup_var))
        return(list(interaction_p = NA, subgroup_effects = data.frame(), model = NULL, subgroup_var_used = NA, formula_used = NA, confounders_used = NA))
    }
    data <- data %>% dplyr::filter(!is.na(.data[[subgroup_var]]))
    if (nrow(data) == 0) {
        warning(sprintf("No data remaining after removing missing values for '%s'", subgroup_var))
        return(list(interaction_p = NA, subgroup_effects = data.frame(), model = NULL, subgroup_var_used = NA, formula_used = NA, confounders_used = NA))
    }
    cohort_other_map <- list()
    if (!is.null(dataset_name)) cohort_other_map <- tryCatch(get_cohort_specific_other_map(dataset_name), error = function(e) list())
    processed <- process_subgroup_data(data, subgroup_var, confounders, include_baseline_height)
    if (!is.null(processed$error) && processed$error == "insufficient_levels") {
        return(list(interaction_p = NA, subgroup_effects = data.frame(), model = NULL, subgroup_var_used = NA, formula_used = NA, confounders_used = NA, error = "insufficient_levels"))
    }
    outcome_config <- list(type = "continuous", outcome_var = "height_change")
    model_results <- fit_subgroup_model(processed$data, outcome_config, processed$subgroup_var_to_use, processed$confounders_to_use)
    data_for_effects <- if (!is.null(model_results$filtered_data)) model_results$filtered_data else processed$data
    subgroup_effects <- calculate_subgroup_effects(model_results$model, data_for_effects, processed$subgroup_var_to_use, outcome_config$type, subgroup_var)
    list(
        interaction_p = model_results$interaction_p, subgroup_effects = subgroup_effects, model = model_results$model,
        subgroup_var_used = processed$subgroup_var_to_use, formula_used = model_results$formula_used,
        confounders_used = processed$confounders_to_use, interaction_diagnostics = model_results$interaction_diagnostics,
        other_map = cohort_other_map
    )
}
