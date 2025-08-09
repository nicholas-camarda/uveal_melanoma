# Table Model Fitting

#' Fit regression model
#'
#' @param data Data frame
#' @param formula Model formula
#' @param model_type Character string for model type
#' @param time_var Character string for time variable (Cox models)
#' @param event_var Character string for event variable (Cox models)
#' @return Fitted model object
fit_regression_model <- function(data, formula, model_type, time_var = NULL, event_var = NULL) {
    
    tryCatch({
        if (model_type == "logistic") {
            # Check for perfect separation before fitting
            formula_vars <- all.vars(formula)
            outcome_var <- formula_vars[1]
            predictor_vars <- formula_vars[-1]
            
            # Check each predictor for perfect separation
            perfect_separation_vars <- c()
            for (var in predictor_vars) {
                if (var %in% names(data)) {
                    # Check if this variable perfectly predicts the outcome
                    if (is.factor(data[[var]]) || is.character(data[[var]])) {
                        # For categorical variables, check each level
                        for (level in unique(data[[var]])) {
                            level_data <- data[data[[var]] == level, ]
                            if (nrow(level_data) > 0) {
                                outcome_counts <- table(level_data[[outcome_var]])
                                if (length(outcome_counts) == 1 || any(outcome_counts == 0)) {
                                    # Perfect separation detected
                                    perfect_separation_vars <- c(perfect_separation_vars, var)
                                    break
                                }
                            }
                        }
                    }
                }
            }
            
            if (length(perfect_separation_vars) > 0) {
                log_enhanced(sprintf("Perfect separation detected in variables: %s. Fitting model with warnings.", 
                                   paste(perfect_separation_vars, collapse = ", ")), level = "WARN")
            }
            
            # Use more robust fitting for logistic regression
            model <- glm(formula, data = data, family = binomial(), 
                        control = list(maxit = 100, epsilon = 1e-8))
            
            # Check for convergence issues
            if (!model$converged) {
                log_enhanced("Warning: Logistic regression did not converge, but proceeding with results", level = "WARN")
            }
            
            # Add perfect separation info to model
            model$perfect_separation_vars <- perfect_separation_vars
            
            return(model)
        } else if (model_type == "cox") {
            if (is.null(time_var) || is.null(event_var)) {
                stop("Cox models require time_var and event_var")
            }
            
            # Check for perfect separation in Cox models (moved earlier in pipeline)
            formula_vars <- all.vars(formula)
            predictor_vars <- formula_vars[-1]  # Remove outcome variable
            
            # Check each predictor for perfect separation
            perfect_separation_vars <- c()
            for (var in predictor_vars) {
                if (var %in% names(data)) {
                    # Check if this variable perfectly predicts the outcome
                    if (is.factor(data[[var]]) || is.character(data[[var]])) {
                        # For categorical variables, check each level
                        for (level in unique(data[[var]])) {
                            level_data <- data[data[[var]] == level, ]
                            if (nrow(level_data) > 0) {
                                event_counts <- table(level_data[[event_var]])
                                if (length(event_counts) == 1 || any(event_counts == 0)) {
                                    # Perfect separation detected
                                    perfect_separation_vars <- c(perfect_separation_vars, var)
                                    break
                                }
                            }
                        }
                    }
                }
            }
            
            if (length(perfect_separation_vars) > 0) {
                log_enhanced(sprintf("Perfect separation detected in Cox model variables: %s. Fitting model with warnings.", 
                                   paste(perfect_separation_vars, collapse = ", ")), level = "WARN")
            }
            
            # Validate time and event variables before creating survival object
            if (is.null(time_var) || is.null(event_var)) {
                log_enhanced("ERROR: time_var or event_var is NULL for Cox model", level = "ERROR")
                return(NULL)
            }
            
            if (!time_var %in% names(data)) {
                log_enhanced(sprintf("ERROR: time_var '%s' not found in data", time_var), level = "ERROR")
                return(NULL)
            }
            
            if (!event_var %in% names(data)) {
                log_enhanced(sprintf("ERROR: event_var '%s' not found in data", event_var), level = "ERROR")
                return(NULL)
            }
            
            # Check for valid data in time and event variables
            if (all(is.na(data[[time_var]])) || length(data[[time_var]]) == 0) {
                log_enhanced(sprintf("ERROR: time_var '%s' contains no valid data", time_var), level = "ERROR")
                return(NULL)
            }
            
            if (all(is.na(data[[event_var]])) || length(data[[event_var]]) == 0) {
                log_enhanced(sprintf("ERROR: event_var '%s' contains no valid data", event_var), level = "ERROR")
                return(NULL)
            }
            
            # Create survival object with error handling
            surv_obj <- tryCatch({
                Surv(data[[time_var]], data[[event_var]])
            }, error = function(e) {
                log_enhanced(sprintf("ERROR: Failed to create survival object: %s", e$message), level = "ERROR")
                return(NULL)
            })
            
            if (is.null(surv_obj)) {
                return(NULL)
            }
            
            # Update formula to use survival object
            log_enhanced(sprintf("Creating survival formula with surv_obj of class: %s", class(surv_obj)[1]), level = "INFO")
            surv_formula <- update(formula, surv_obj ~ .)
            log_enhanced(sprintf("Survival formula created: %s", deparse(surv_formula)), level = "INFO")
            
            # Fit Cox model with error handling
            cox_model <- tryCatch({
                log_enhanced("Attempting to fit Cox model...", level = "INFO")
                # Add surv_obj to the data frame so coxph can find it
                data_with_surv <- data
                data_with_surv$surv_obj <- surv_obj
                result <- coxph(surv_formula, data = data_with_surv, model = TRUE)
                log_enhanced("Cox model fitted successfully", level = "INFO")
                result
            }, error = function(e) {
                log_enhanced(sprintf("Cox model fitting error: %s", e$message), level = "ERROR")
                log_enhanced(sprintf("Error occurred at: %s", e$call), level = "ERROR")
                return(NULL)
            })
            
            # Add perfect separation info to model if it exists
            if (!is.null(cox_model)) {
                cox_model$perfect_separation_vars <- perfect_separation_vars
            }
            
            return(cox_model)
        } else if (model_type == "linear") {
            lm(formula, data = data)
        } else {
            stop("Unsupported model type: ", model_type)
        }
    }, error = function(e) {
        log_enhanced(sprintf("Model fitting error: %s", e$message), level = "ERROR")
        return(NULL)
    })
}
