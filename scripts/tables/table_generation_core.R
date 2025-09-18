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
        "other_glm" = "binary", # Default for other GLMs
        "unknown" = "binary", # Default fallback
        "binary" # If already an outcome type, return as is
    )
}

#' Detect the type of regression model
#'
#' @param model_fit Fitted model object
#' @return Character string indicating model type: "linear", "logistic", "cox", "other_glm", or "unknown"
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

    return("unknown")
}

#' Generate regression table with comprehensive diagnostics
#'
#' @param data Data frame
#' @param outcome_var Character string name of outcome variable
#' @param predictor_vars Character vector of predictor variables
#' @param confounders Character vector of confounder variables
#' @param model_type Character string for model type ("logistic", "cox", "linear")
#' @param effect_measure Character string for effect measure ("OR", "HR", "beta")
#' @param analysis_name Character string for analysis name
#' @param dataset_name Character string for dataset name
#' @param output_dir Character string for output directory
#' @param prefix Character string for file prefix
#' @param analysis_type Character string for analysis type ("post_treatment_only" or "all_patients")
#' @param time_var Character string for time variable (Cox models)
#' @param event_var Character string for event variable (Cox models)
#' @param other_map List containing mapping of what categories were collapsed into "Other"
#' @param treatment_var Name of the treatment variable in the model (default: "treatment_group")
#' @param other_level_details Data frame with details about "Other" levels (optional)
#' @return List containing table result and diagnostics
generate_regression_table <- function(data, outcome_var, predictor_vars, confounders, model_type, effect_measure, analysis_name, dataset_name, output_dir, prefix, time_var = NULL, event_var = NULL, other_map = NULL, treatment_var = "treatment_group", other_level_details = NULL) {
    logger::log_info(sprintf("Generating regression table for %s", analysis_name))

    # Build model formula
    formula <- build_model_formula(outcome_var, predictor_vars, confounders, model_type)

    # Fit regression model
    model_fit <- fit_regression_model(data, formula, model_type, time_var, event_var)

    if (is.null(model_fit)) {
        logger::log_error("Model fitting failed - returning NULL result")
        return(list(
            table = NULL,
            diagnostics = NULL,
            model = NULL,
            output_files = NULL
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
            model_fit, effect_measure, analysis_name, other_map,
            data, outcome_var, confounders, outcome_type,
            other_level_details = other_level_details
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
            dataset_name, filtered_variables, other_map,
            extreme_filtering_result$diagnostics,
            treatment_var = treatment_var,
            effect_measure = effect_measure,
            table_result = table_result,
            other_level_details = other_level_details
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

        # Create minimal diagnostics documenting the failure
        diagnostics <- list(
            raw_model_output = "Model fitting failed - no diagnostics available",
            extreme_estimates = data.frame(
                variable = "Model Failure",
                estimate = NA,
                conf.low = NA,
                conf.high = NA,
                p.value = NA,
                status = "Model fitting failed",
                stringsAsFactors = FALSE
            ),
            perfect_separation = data.frame(
                variable = "Model Failure",
                status = "Model fitting failed",
                details = "Unable to fit model due to data or parameter issues",
                stringsAsFactors = FALSE
            ),
            other_details = data.frame(
                issue = "Model Fitting Failure",
                details = "The regression model could not be fitted. Check data quality and model parameters.",
                timestamp = Sys.time(),
                stringsAsFactors = FALSE
            )
        )

        if (!is.null(other_level_details)) {
            diagnostics$other_level_details <- other_level_details
        }

        # Still save diagnostics file even when model fails
        output_files <- tryCatch(
            {
                save_table_outputs(NULL, diagnostics$raw_model_output, NULL,
                    analysis_name, dataset_name, output_dir, prefix,
                    diagnostics, data, outcome_var, confounders,
                    treatment_var = treatment_var
                )
            },
            error = function(e) {
                logger::log_error(sprintf("Failed to save diagnostics file: %s", e$message))
                NULL
            }
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
