# Subgroup Binary Outcome Analysis

#' Subgroup analysis for binary outcomes (logistic with interaction)
#' @param data Data frame
#' @param outcome_var Binary outcome
#' @param subgroup_vars Subgroup variables
#' @param confounders Confounders
#' @param outcome_name Label
#' @param dataset_name Optional dataset name
#' @return List of results per subgroup
analyze_treatment_effect_subgroups_binary <- function(data, outcome_var, subgroup_vars, confounders = NULL, outcome_name = "Binary Outcome", dataset_name = NULL) {
    logger::log_info(sprintf("Performing subgroup analysis for %s", outcome_name))
    subgroup_results <- list()
    for (subgroup_var in subgroup_vars) {
        logger::log_info(sprintf("Testing interaction for: %s", subgroup_var))
        res <- tryCatch(
            {
                processed <- process_subgroup_data(data, subgroup_var, confounders, FALSE)
                if (!is.null(processed$error) && processed$error == "insufficient_levels") {
                    list(interaction_p = NA, subgroup_effects = data.frame(), error = "insufficient_levels")
                } else {
                    outcome_config <- list(type = "binary", outcome_var = outcome_var)
                    other_vars <- unique(c("treatment_group", processed$subgroup_var_to_use, processed$confounders_to_use))
                    exclusion_result <- apply_sparse_level_exclusions(
                        data = processed$data,
                        variables = other_vars[other_vars %in% names(processed$data)],
                        analysis_name = paste0("subgroup_binary_", subgroup_var),
                        id_col = pick_sparse_level_id_col(processed$data),
                        level_exclusions = MODELING_LEVEL_EXCLUSIONS
                    )

                    if (exclusion_result$removed_row_count > 0) {
                        logger::log_info(sprintf(
                            "Excluded %d rows with sparse categorical levels prior to binary subgroup modeling for %s",
                            exclusion_result$removed_row_count,
                            subgroup_var
                        ))
                    }

                    if (nrow(exclusion_result$data) == 0) {
                        interaction_diagnostics <- list(
                            failure_reason = "No data after sparse-level exclusions",
                            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics
                        )
                        list(
                            interaction_p = NA,
                            subgroup_effects = data.frame(),
                            model = NULL,
                            subgroup_var_used = processed$subgroup_var_to_use,
                            formula_used = NA,
                            confounders_used = processed$confounders_to_use,
                            was_continuous = processed$was_continuous,
                            modeled_continuously = processed$modeled_continuously,
                            cutoff_value = processed$cutoff_value,
                            interaction_diagnostics = interaction_diagnostics,
                            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
                            error = "no_data_after_sparse_exclusions"
                        )
                    } else {
                        model_results <- fit_subgroup_model(exclusion_result$data, outcome_config, processed$subgroup_var_to_use, processed$confounders_to_use)
                        data_for_effects <- if (!is.null(model_results$filtered_data)) model_results$filtered_data else exclusion_result$data
                        if (!is.null(model_results$interaction_diagnostics)) {
                            model_results$interaction_diagnostics$sparse_level_diagnostics <- exclusion_result$sparse_level_diagnostics
                        }
                        subgroup_effects <- calculate_subgroup_effects(model_results$model, data_for_effects, processed$subgroup_var_to_use, outcome_config$type, subgroup_var)
                        list(
                            interaction_p = model_results$interaction_p, subgroup_effects = subgroup_effects, model = model_results$model,
                            subgroup_var_used = processed$subgroup_var_to_use, formula_used = model_results$formula_used,
                            confounders_used = processed$confounders_to_use, was_continuous = processed$was_continuous,
                            modeled_continuously = processed$modeled_continuously,
                            cutoff_value = processed$cutoff_value, interaction_diagnostics = model_results$interaction_diagnostics,
                            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics
                        )
                    }
                }
            },
            error = function(e) list(interaction_p = NA, subgroup_effects = data.frame(), error = e$message)
        )
        subgroup_results[[subgroup_var]] <- res
    }
    list(subgroup_results = subgroup_results)
}
