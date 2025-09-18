# Subgroup Survival Analysis

#' Subgroup analysis for survival outcomes (Cox with interaction)
#' @param data Data frame
#' @param time_var Time variable
#' @param event_var Event indicator
#' @param subgroup_vars Character vector of subgroup variables
#' @param confounders Character vector of confounders
#' @param outcome_name Label for output
#' @param dataset_name Optional dataset name for other_map
#' @return List of results per subgroup
analyze_treatment_effect_subgroups_survival <- function(data, time_var, event_var, subgroup_vars, confounders = NULL, outcome_name = "Survival", dataset_name = NULL) {
    logger::log_info(sprintf("Performing subgroup analysis for %s", outcome_name))
    subgroup_results <- list()
    cohort_other_map <- list()
    if (!is.null(dataset_name)) {
        cohort_other_map <- tryCatch(get_cohort_specific_other_map(dataset_name), error = function(e) list())
    }
    for (subgroup_var in subgroup_vars) {
        logger::log_info(sprintf("Testing interaction for: %s", subgroup_var))
        res <- tryCatch(
            {
                processed <- process_subgroup_data(data, subgroup_var, confounders, FALSE)
                if (!is.null(processed$error) && processed$error == "insufficient_levels") {
                    list(interaction_p = NA, subgroup_effects = data.frame(), error = "insufficient_levels")
                } else {
                    outcome_config <- list(type = "survival", time_var = time_var, event_var = event_var)
                    other_vars <- unique(c("treatment_group", processed$subgroup_var_to_use, processed$confounders_to_use))
                    exclusion_result <- exclude_other_categories(
                        data = processed$data,
                        variables = other_vars[other_vars %in% names(processed$data)],
                        other_map = if (!is.null(cohort_other_map)) cohort_other_map else list()
                    )

                    if (exclusion_result$removed_row_count > 0) {
                        logger::log_info(sprintf(
                            "Removed %d rows labelled 'Other' prior to survival subgroup modeling for %s",
                            exclusion_result$removed_row_count,
                            subgroup_var
                        ))
                    }

                    if (nrow(exclusion_result$data) == 0) {
                        interaction_diagnostics <- list(failure_reason = "No data after removing 'Other' levels",
                            other_level_details = exclusion_result$other_level_details)
                        list(
                            interaction_p = NA,
                            subgroup_effects = data.frame(),
                            model = NULL,
                            subgroup_var_used = processed$subgroup_var_to_use,
                            formula_used = NA,
                            confounders_used = processed$confounders_to_use,
                            was_continuous = processed$was_continuous,
                            cutoff_value = processed$cutoff_value,
                            interaction_diagnostics = interaction_diagnostics,
                            other_map = processed$other_map,
                            other_level_details = exclusion_result$other_level_details,
                            error = "no_data_after_other_removal"
                        )
                    } else {
                        model_results <- fit_subgroup_model(exclusion_result$data, outcome_config, processed$subgroup_var_to_use, processed$confounders_to_use)
                        data_for_effects <- if (!is.null(model_results$filtered_data)) model_results$filtered_data else exclusion_result$data
                        if (!is.null(model_results$interaction_diagnostics)) {
                            model_results$interaction_diagnostics$other_level_details <- exclusion_result$other_level_details
                        }
                        subgroup_effects <- calculate_subgroup_effects(model_results$model, data_for_effects, processed$subgroup_var_to_use, outcome_config$type, subgroup_var)
                        list(
                            interaction_p = model_results$interaction_p, subgroup_effects = subgroup_effects, model = model_results$model,
                            subgroup_var_used = processed$subgroup_var_to_use, formula_used = model_results$formula_used,
                            confounders_used = processed$confounders_to_use, was_continuous = processed$was_continuous,
                            cutoff_value = processed$cutoff_value, interaction_diagnostics = model_results$interaction_diagnostics,
                            other_map = processed$other_map,
                            other_level_details = exclusion_result$other_level_details
                        )
                    }
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
