# Table Model Fitting

#' Format a small set of unique values for logging/documentation
format_unique_values <- function(values, max_display = 5) {
    if (length(values) == 0) {
        return("none")
    }

    unique_vals <- unique(as.character(values))
    if (length(unique_vals) > max_display) {
        unique_vals <- c(unique_vals[seq_len(max_display)], "...")
    }

    paste(unique_vals, collapse = ", ")
}

#' Remove predictors that lack post-filter variability
prune_low_variability_terms <- function(formula, data, model_label = "Regression") {
    if (is.null(formula) || is.null(data)) {
        return(list(formula = formula, removed = list()))
    }

    terms_obj <- terms(formula)
    predictor_terms <- attr(terms_obj, "term.labels")
    if (length(predictor_terms) == 0) {
        return(list(formula = formula, removed = list()))
    }

    kept_terms <- predictor_terms
    removed_details <- list()

    for (term in predictor_terms) {
        if (!term %in% names(data)) {
            next
        }

        term_values <- data[[term]]
        non_missing <- term_values[!is.na(term_values)]
        unique_values <- unique(non_missing)

        insufficient_variation <- length(unique_values) <= 1
        if (insufficient_variation) {
            kept_terms <- setdiff(kept_terms, term)
            reason <- if (length(unique_values) == 0) {
                "all observations missing after filtering"
            } else {
                sprintf("only one unique value remains (%s)", format_unique_values(unique_values))
            }
            removed_details[[term]] <- list(
                reason = reason,
                unique_values = if (length(unique_values) == 0) "none" else format_unique_values(unique_values),
                non_missing_n = length(non_missing)
            )

            logger::log_warn(sprintf(
                "%s model: dropping covariate '%s' due to insufficient variation (%s).",
                model_label,
                term,
                reason
            ))
        }
    }

    if (length(removed_details) > 0) {
        outcome <- as.character(formula[[2]])
        if (length(kept_terms) == 0) {
            updated_formula <- as.formula(paste(outcome, "~ 1"))
        } else {
            updated_formula <- as.formula(paste(outcome, "~", paste(kept_terms, collapse = " + ")))
        }
        environment(updated_formula) <- environment(formula)
        formula <- updated_formula
    }

    list(formula = formula, removed = removed_details)
}

#' Fit regression model
#'
#' @param data Data frame
#' @param formula Model formula
#' @param model_type Character string for model type
#' @param time_var Character string for time variable (Cox models)
#' @param event_var Character string for event variable (Cox models)
#' @return Fitted model object
fit_regression_model <- function(data, formula, model_type, time_var = NULL, event_var = NULL) {
    tryCatch(
        {
            if (model_type == "logistic") {
                pruning <- prune_low_variability_terms(formula, data, "Logistic")
                formula <- pruning$formula
                removed_covariates <- pruning$removed

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
                    logger::log_warn(sprintf(
                        "Perfect separation detected in variables: %s. Fitting model with warnings.",
                        paste(perfect_separation_vars, collapse = ", ")
                    ))
                }

                # Use more robust fitting for logistic regression
                model <- glm(formula,
                    data = data, family = binomial(),
                    control = list(maxit = 100, epsilon = 1e-8)
                )

                # Check for convergence issues
                if (!model$converged) {
                    logger::log_warn("Warning: Logistic regression did not converge, but proceeding with results")
                }

                # Add perfect separation info to model
                model$perfect_separation_vars <- perfect_separation_vars
                model$removed_covariates <- removed_covariates

                return(model)
            } else if (model_type == "cox") {
                pruning <- prune_low_variability_terms(formula, data, "Cox")
                formula <- pruning$formula
                removed_covariates <- pruning$removed

                if (is.null(time_var) || is.null(event_var)) {
                    stop("Cox models require time_var and event_var")
                }

                # Check for perfect separation in Cox models (moved earlier in pipeline)
                formula_vars <- all.vars(formula)
                predictor_vars <- formula_vars[-1] # Remove outcome variable

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
                    logger::log_warn(sprintf(
                        "Perfect separation detected in Cox model variables: %s. Fitting model with warnings.",
                        paste(perfect_separation_vars, collapse = ", ")
                    ))
                }

                # Validate time and event variables before creating survival object
                if (is.null(time_var) || is.null(event_var)) {
                    logger::log_error("ERROR: time_var or event_var is NULL for Cox model")
                    return(NULL)
                }

                if (!time_var %in% names(data)) {
                    logger::log_error(sprintf("ERROR: time_var '%s' not found in data", time_var))
                    return(NULL)
                }

                if (!event_var %in% names(data)) {
                    logger::log_error(sprintf("ERROR: event_var '%s' not found in data", event_var))
                    return(NULL)
                }

                # Check for valid data in time and event variables
                if (all(is.na(data[[time_var]])) || length(data[[time_var]]) == 0) {
                    logger::log_error(sprintf("ERROR: time_var '%s' contains no valid data", time_var))
                    return(NULL)
                }

                if (all(is.na(data[[event_var]])) || length(data[[event_var]]) == 0) {
                    logger::log_error(sprintf("ERROR: event_var '%s' contains no valid data", event_var))
                    return(NULL)
                }

                # Create survival object with error handling
                surv_obj <- tryCatch(
                    {
                        Surv(data[[time_var]], data[[event_var]])
                    },
                    error = function(e) {
                        logger::log_error(sprintf("ERROR: Failed to create survival object: %s", e$message))
                        return(NULL)
                    }
                )

                if (is.null(surv_obj)) {
                    return(NULL)
                }

                # Update formula to use survival object
                logger::log_info(sprintf("Creating survival formula with surv_obj of class: %s", class(surv_obj)[1]))
                surv_formula <- update(formula, surv_obj ~ .)
                logger::log_info(sprintf("Survival formula created: %s", paste(deparse(surv_formula, width.cutoff = 500L), collapse = " ")))

                # Fit Cox model with error handling
                cox_model <- tryCatch(
                    {
                        logger::log_info("Attempting to fit Cox model...")
                        # Add surv_obj to the data frame so coxph can find it
                        data_with_surv <- data
                        data_with_surv$surv_obj <- surv_obj
                        result <- coxph(surv_formula, data = data_with_surv, model = TRUE)
                        logger::log_info("Cox model fitted successfully")
                        result
                    },
                    error = function(e) {
                        logger::log_error(sprintf("Cox model fitting error: %s", e$message))
                        logger::log_error(sprintf("Error occurred at: %s", e$call))
                        return(NULL)
                    }
                )

                # Add perfect separation info to model if it exists
                if (!is.null(cox_model)) {
                    cox_model$perfect_separation_vars <- perfect_separation_vars
                    cox_model$removed_covariates <- removed_covariates
                }

                return(cox_model)
            } else if (model_type == "linear") {
                pruning <- prune_low_variability_terms(formula, data, "Linear")
                formula <- pruning$formula
                removed_covariates <- pruning$removed

                lm_model <- lm(formula, data = data)
                lm_model$removed_covariates <- removed_covariates
                lm_model
            } else if (model_type == "ordinal") {
                pruning <- prune_low_variability_terms(formula, data, "Ordinal")
                formula <- pruning$formula
                removed_covariates <- pruning$removed

                outcome_var <- all.vars(formula)[1]
                ordinal_data <- data
                if (outcome_var %in% names(ordinal_data)) {
                    ordinal_data[[outcome_var]] <- droplevels(ordinal_data[[outcome_var]])
                    if (!is.ordered(ordinal_data[[outcome_var]])) {
                        ordinal_data[[outcome_var]] <- ordered(ordinal_data[[outcome_var]], levels = levels(ordinal_data[[outcome_var]]))
                    }
                }

                ordinal_model <- MASS::polr(
                    formula,
                    data = ordinal_data,
                    Hess = TRUE,
                    model = TRUE
                )
                ordinal_model$removed_covariates <- removed_covariates
                ordinal_model
            } else {
                stop("Unsupported model type: ", model_type)
            }
        },
        error = function(e) {
            logger::log_error(sprintf("Model fitting error: %s", e$message))
            return(NULL)
        }
    )
}
