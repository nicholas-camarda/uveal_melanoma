# Table Generation Core Helpers

#' Build model formula for regression
#'
#' @param outcome_var Character string name of outcome variable
#' @param predictor_vars Character vector of predictor variables
#' @param confounders Character vector of confounder variables
#' @param model_type Character string for model type
#' @return Model formula
build_model_formula <- function(outcome_var, predictor_vars, confounders, model_type) {
    # Combine predictor and confounder variables
    all_vars <- c(predictor_vars, confounders)

    # Remove any NULL or empty variables
    all_vars <- all_vars[!is.null(all_vars) & all_vars != ""]

    if (length(all_vars) == 0) {
        # No variables to include
        formula_str <- paste(outcome_var, "~ 1")
    } else {
        # Create formula with all variables
        formula_str <- paste(outcome_var, "~", paste(all_vars, collapse = " + "))
    }

    return(as.formula(formula_str))
}

#' Get descriptive model type string
#'
#' @param model_fit Fitted model object
#' @return Character string with descriptive model type
get_descriptive_model_type <- function(model_fit) {
    # Get the class of the model
    model_class <- class(model_fit)[1]

    # Determine model type based on class
    if (model_class == "glm") {
        family <- model_fit$family$family
        if (family == "binomial") {
            return("Logistic Regression")
        } else if (family == "gaussian") {
            return("Linear Regression")
        } else if (family == "poisson") {
            return("Poisson Regression")
        } else {
            return("Generalized Linear Model")
        }
    } else if (model_class == "coxph") {
        return("Cox Proportional Hazards")
    } else if (model_class == "lm") {
        return("Linear Regression")
    } else if (model_class == "polr") {
        return("Ordinal Logistic Regression")
    } else {
        # Fallback for unknown model types
        return("Regression")
    }
}

#' Convert model type to outcome type
#'
#' @param model_type Character string indicating model type
#' @return Character string indicating outcome type
model_type_to_outcome_type <- function(model_type) {
    switch(model_type,
        "logistic" = "binary",
        "linear" = "continuous",
        "cox" = "survival",
        "ordinal" = "ordinal",
        "other_glm" = "binary", # Default for other GLMs
        "unknown" = "binary", # Default fallback
        "binary" # If already an outcome type, return as is
    )
}

#' Detect the type of regression model
#'
#' @param model_fit Fitted model object
#' @return Character string indicating model type: "linear", "logistic", "cox", "ordinal", "other_glm", or "unknown"
detect_model_type <- function(model_fit) {
    if (is.null(model_fit)) {
        return("unknown")
    }

    # Check for GLM models
    if ("glm" %in% class(model_fit)) {
        if (model_fit$family$family == "binomial") {
            return("logistic")
        } else if (model_fit$family$family == "gaussian") {
            return("linear")
        } else {
            return("other_glm")
        }
    }

    # Check for Cox proportional hazards models
    if ("coxph" %in% class(model_fit)) {
        return("cox")
    }

    # Check for linear models
    if ("lm" %in% class(model_fit)) {
        return("linear")
    }

    if ("polr" %in% class(model_fit)) {
        return("ordinal")
    }

    return("unknown")
}

#' Generate regression table with comprehensive diagnostics
#'
#' @param data Data frame
#' @param outcome_var Character string name of outcome variable
#' @param predictor_vars Character vector of predictor variables
#' @param confounders Character vector of confounder variables
#' @param model_type Character string for model type ("logistic", "cox", "linear", "ordinal")
#' @param effect_measure Character string for effect measure ("OR", "HR", "beta")
#' @param analysis_name Character string for analysis name
#' @param dataset_name Character string for dataset name
#' @param output_dir Character string for output directory
#' @param prefix Character string for file prefix
#' @param analysis_type Character string for analysis type ("post_treatment_only" or "all_patients")
#' @param time_var Character string for time variable (Cox models)
#' @param event_var Character string for event variable (Cox models)
#' @param treatment_var Name of the treatment variable in the model (default: "treatment_group")
#' @param sparse_level_diagnostics Data frame with details about excluded sparse levels (optional)
#' @param filter_stats List summarizing pre- vs post-filter sample sizes (optional)
#' @return List containing table result and diagnostics
generate_regression_table <- function(data, outcome_var, predictor_vars, confounders, model_type, effect_measure, analysis_name, dataset_name, output_dir, prefix, time_var = NULL, event_var = NULL, treatment_var = "treatment_group", sparse_level_diagnostics = NULL, filter_stats = NULL) {
    logger::log_info(sprintf("Generating regression table for %s", analysis_name))

    # Build model formula
    formula <- build_model_formula(outcome_var, predictor_vars, confounders, model_type)
    formula_text <- paste(deparse(formula, width.cutoff = 500L), collapse = " ")
    support_variables <- unique(c(predictor_vars, confounders))

    # Fit regression model
    model_fit <- fit_regression_model(data, formula, model_type, time_var, event_var)

    if (is.null(model_fit)) {
        logger::log_error("Model fitting failed - returning NULL result")
        sample_size_summary <- build_sample_size_summary_tab(
            filter_stats = filter_stats,
            dataset_name = dataset_name,
            analysis_name = analysis_name,
            modeled_n = nrow(data)
        )
        diagnostics <- build_skip_report_diagnostics(
            status = "skipped",
            analysis_name = analysis_name,
            dataset_name = dataset_name,
            reason = "Model fitting failed due to insufficient usable data, no outcome variation, or numerical issues.",
            narrative_lines = c(
                sprintf("Model fitting was attempted using `%s`, but the model object could not be created.", formula_text),
                "This usually indicates insufficient usable data, no outcome variation, or a numerical fitting failure."
            ),
            sample_size_summary = sample_size_summary,
            skip_summary = build_skip_summary_tab(list(
                status = "skipped",
                model_type = model_type,
                modeled_n = nrow(data)
            )),
            sparse_level_diagnostics = create_sparse_level_diagnostics_tab(sparse_level_diagnostics),
            event_support = if (identical(model_type, "logistic") || identical(model_type, "cox")) {
                build_level_support_tab(data, support_variables, outcome_var = outcome_var)
            } else {
                NULL
            },
            level_support = if (identical(model_type, "linear") || identical(model_type, "ordinal")) {
                build_level_support_tab(data, support_variables)
            } else {
                NULL
            },
            model_context = build_model_context_tab(list(
                model_type = model_type,
                formula = formula_text,
                predictors = paste(as.character(predictor_vars), collapse = ", "),
                confounders = format_effect_summary_covariates(confounders),
                time_var = time_var %||% "",
                event_var = event_var %||% ""
            )),
            raw_model_output = "Model fitting failed - no diagnostics available"
        )
        output_files <- save_skipped_model_outputs(
            analysis_name = analysis_name,
            dataset_name = dataset_name,
            output_dir = output_dir,
            prefix = prefix,
            reason = diagnostics$reason,
            diagnostics = diagnostics
        )
        return(list(
            table = NULL,
            diagnostics = diagnostics,
            model = NULL,
            output_files = output_files
        ))
    }

    # Check for perfect separation and handle gracefully
    if (!is.null(model_fit$perfect_separation_vars) && length(model_fit$perfect_separation_vars) > 0) {
        logger::log_warn(sprintf(
            "Perfect separation detected in variables: %s. Model fitted but these variables may have unreliable estimates.",
            paste(model_fit$perfect_separation_vars, collapse = ", ")
        ))
    }

    # Create gtsummary table only if model fitting succeeded
    if (!is.null(model_fit)) {
        outcome_type <- model_type_to_outcome_type(detect_model_type(model_fit))
        table_result <- create_gtsummary_table(
            model_fit, effect_measure, analysis_name,
            data, outcome_var, confounders, outcome_type,
            sparse_level_diagnostics = sparse_level_diagnostics
        )
        
        # DEBUG: Check table creation result
        if (!is.null(table_result)) {
            logger::log_info(sprintf("DEBUG: create_gtsummary_table for %s created table with %d rows", analysis_name, nrow(table_result$table_body)))
        } else {
            logger::log_info(sprintf("DEBUG: create_gtsummary_table for %s returned NULL", analysis_name))
        }

        # Get list of variables that were completely removed from the table
        filtered_variables <- get_filtered_variables_from_table(table_result, model_fit)
    } else {
        table_result <- NULL
        filtered_variables <- NULL
    }

    # Apply extreme estimate filtering and create diagnostics only if model fitting succeeded
    if (!is.null(model_fit) && !is.null(table_result)) {
        # Apply extreme estimate filtering to get detailed diagnostics
        extreme_filtering_result <- process_extreme_estimates(table_result, model_fit, effect_measure,
            variables_to_check = unique(c(predictor_vars, confounders)),
            analysis_name
        )

        # Use the filtered table instead of the original
        filtered_table_result <- extreme_filtering_result$tbl_filtered

        # Create comprehensive diagnostics with all required tabs
        # Extract the actual filtered variables from the extreme filtering result
        filtered_variables <- extreme_filtering_result$diagnostics$extreme_terms
        
        # Add completely removed variables to the diagnostics
        completely_removed_vars <- extreme_filtering_result$diagnostics$completely_removed_variables
        if (!is.null(completely_removed_vars) && length(completely_removed_vars) > 0) {
            logger::log_info(sprintf("DEBUG: Completely removed variables: %s", paste(completely_removed_vars, collapse = ", ")))
        }

        diagnostics <- create_comprehensive_diagnostics(model_fit, data, outcome_var,
            predictor_vars, confounders, analysis_name,
            dataset_name, filtered_variables,
            extreme_filtering_result$diagnostics,
            treatment_var = treatment_var,
            effect_measure = effect_measure,
            table_result = table_result,
            sparse_level_diagnostics = sparse_level_diagnostics,
            filter_stats = filter_stats
        )

        # Create raw_output from diagnostics for save_table_outputs
        raw_output <- diagnostics$raw_model_output

        # Save outputs using the filtered table
        output_files <- save_table_outputs(
            table_result = filtered_table_result, 
            raw_output = raw_output, 
            model_fit = model_fit,
            analysis_name = analysis_name, 
            dataset_name = dataset_name, 
            output_dir = output_dir, 
            prefix = prefix,
            diagnostics = diagnostics, 
            data = data, 
            outcome_var = outcome_var, 
            confounders = confounders,
            treatment_var = treatment_var
        )
    } else {
        # Handle case where model fitting failed - still create diagnostics file
        filtered_table_result <- NULL

        sample_size_summary <- build_sample_size_summary_tab(
            filter_stats = filter_stats,
            dataset_name = dataset_name,
            analysis_name = analysis_name,
            modeled_n = nrow(data)
        )
        diagnostics <- build_skip_report_diagnostics(
            status = "unavailable",
            analysis_name = analysis_name,
            dataset_name = dataset_name,
            reason = "The model fit completed, but no interpretable regression table could be generated.",
            narrative_lines = c(
                sprintf("Model fitting completed for `%s`, but the table-generation step returned no reportable output.", formula_text),
                "This usually means the fitted model could not be converted into a stable summary table."
            ),
            sample_size_summary = sample_size_summary,
            skip_summary = build_skip_summary_tab(list(
                status = "unavailable",
                model_type = model_type,
                modeled_n = nrow(data)
            )),
            sparse_level_diagnostics = sparse_level_diagnostics,
            event_support = if (identical(model_type, "logistic") || identical(model_type, "cox")) {
                build_level_support_tab(data, support_variables, outcome_var = outcome_var)
            } else {
                NULL
            },
            level_support = if (identical(model_type, "linear") || identical(model_type, "ordinal")) {
                build_level_support_tab(data, support_variables)
            } else {
                NULL
            },
            model_context = build_model_context_tab(list(
                model_type = model_type,
                formula = formula_text,
                predictors = paste(as.character(predictor_vars), collapse = ", "),
                confounders = format_effect_summary_covariates(confounders),
                time_var = time_var %||% "",
                event_var = event_var %||% ""
            )),
            raw_model_output = "Model fit completed, but no interpretable regression table could be generated."
        )
        output_files <- save_skipped_model_outputs(
            analysis_name = analysis_name,
            dataset_name = dataset_name,
            output_dir = output_dir,
            prefix = prefix,
            reason = diagnostics$reason,
            diagnostics = diagnostics
        )
    }

    logger::log_info(sprintf("Regression table generation completed for %s", analysis_name))

    return(list(
        table = filtered_table_result,
        diagnostics = diagnostics,
        model = model_fit,
        output_files = output_files
    ))
}
