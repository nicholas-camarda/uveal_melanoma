# Primary Outcomes Subgroup Analysis
# Functions for analyzing treatment effects across subgroups for primary outcomes

#' Analyze treatment effects across subgroups for survival outcomes
#'
#' Performs subgroup analysis for time-to-event outcomes (OS, PFS, recurrence, metastatic progression)
#' using Cox regression with interaction terms
#'
#' @param data Data frame containing the analysis variables
#' @param time_var Name of the time variable
#' @param event_var Name of the event indicator variable
#' @param subgroup_vars Character vector of subgroup variables to test
#' @param confounders Character vector of confounders
#' @param outcome_name Name of the outcome for labeling
#' @return List of subgroup analysis results
analyze_treatment_effect_subgroups_survival <- function(data, time_var, event_var, subgroup_vars, confounders = NULL, outcome_name = "Survival") {
    log_message(sprintf("Performing subgroup analysis for %s", outcome_name))

    subgroup_results <- list()

    for (subgroup_var in subgroup_vars) {
        log_message(sprintf("Testing interaction for: %s", subgroup_var))

        tryCatch(
            {
                # Filter confounders to exclude the subgroup variable
                confounders_to_use <- if (!is.null(confounders)) {
                    confounders[confounders != subgroup_var]
                } else {
                    NULL
                }

                # Validate confounders
                if (length(confounders_to_use) > 0) {
                    confounders_to_use <- generate_valid_confounders(data, confounders_to_use, threshold = THRESHOLD_RARITY)
                }

                # Process subgroup variable (bin if continuous)
                processed_data <- data
                was_continuous <- is.numeric(data[[subgroup_var]]) || is.integer(data[[subgroup_var]])

                if (was_continuous) {
                    cutoff_val <- quantile(data[[subgroup_var]], probs = 0.5, na.rm = TRUE)
                    subgroup_var_binned <- paste0(subgroup_var, "_binned")
                    processed_data[[subgroup_var_binned]] <- factor(
                        ifelse(data[[subgroup_var]] < cutoff_val,
                            paste0("< ", round(cutoff_val, 1)),
                            paste0("≥ ", round(cutoff_val, 1))
                        ),
                        levels = c(
                            paste0("< ", round(cutoff_val, 1)),
                            paste0("≥ ", round(cutoff_val, 1))
                        )
                    )
                    subgroup_var_to_use <- subgroup_var_binned
                } else {
                    if (!is.factor(processed_data[[subgroup_var]])) {
                        processed_data[[subgroup_var]] <- as.factor(processed_data[[subgroup_var]])
                    }
                    processed_data <- handle_rare_categories(processed_data, vars = subgroup_var, threshold = THRESHOLD_RARITY)
                    subgroup_var_to_use <- subgroup_var
                }

                # Ensure consistent contrasts
                processed_data <- ensure_consistent_contrasts(processed_data)

                # Build formula with interaction
                if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
                    formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ treatment_group * ", subgroup_var_to_use)
                } else {
                    formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ treatment_group * ", subgroup_var_to_use, " + ", paste(confounders_to_use, collapse = " + "))
                }

                # Fit Cox model with interaction
                cox_model <- coxph(as.formula(formula_str), data = processed_data)

                # Test interaction significance
                subgroup_levels <- levels(processed_data[[subgroup_var_to_use]])

                if (length(subgroup_levels) == 2) {
                    # Simple interaction test
                    interaction_term <- get_interaction_coefficient_name(
                        cox_model, "treatment_group",
                        subgroup_var_to_use, subgroup_levels[2], processed_data
                    )
                    if (!is.null(interaction_term)) {
                        interaction_p <- summary(cox_model)$coefficients[interaction_term, "Pr(>|z|)"]
                    } else {
                        interaction_p <- NA
                    }
                } else {
                    # Multiple levels - use likelihood ratio test
                    if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
                        formula_no_int <- paste0("Surv(", time_var, ", ", event_var, ") ~ treatment_group + ", subgroup_var_to_use)
                    } else {
                        formula_no_int <- paste0("Surv(", time_var, ", ", event_var, ") ~ treatment_group + ", subgroup_var_to_use, " + ", paste(confounders_to_use, collapse = " + "))
                    }
                    cox_no_interaction <- coxph(as.formula(formula_no_int), data = processed_data)
                    interaction_test <- anova(cox_no_interaction, cox_model)
                    interaction_p <- interaction_test$`Pr(>Chi)`[2]
                }

                # Calculate hazard ratios for each subgroup
                subgroup_effects <- data.frame()
                for (level in subgroup_levels) {
                    # Create subset for this subgroup
                    subset_data <- processed_data[processed_data[[subgroup_var_to_use]] == level, ]

                    # Fit model for this subgroup
                    if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
                        subset_formula <- paste0("Surv(", time_var, ", ", event_var, ") ~ treatment_group")
                    } else {
                        subset_formula <- paste0("Surv(", time_var, ", ", event_var, ") ~ treatment_group + ", paste(confounders_to_use, collapse = " + "))
                    }

                    subset_model <- coxph(as.formula(subset_formula), data = subset_data)
                    subset_summary <- summary(subset_model)

                    # Extract hazard ratio and confidence intervals
                    hr <- subset_summary$conf.int[1, 1]
                    ci_lower <- subset_summary$conf.int[1, 3]
                    ci_upper <- subset_summary$conf.int[1, 4]
                    p_value <- subset_summary$coefficients[1, 5]

                    # Get sample sizes
                    n_gksrs <- sum(subset_data$treatment_group == "GKSRS")
                    n_plaque <- sum(subset_data$treatment_group == "Plaque")
                    n_total <- n_gksrs + n_plaque

                    # Add to results
                    subgroup_effects <- rbind(subgroup_effects, data.frame(
                        subgroup_level = level,
                        treatment_effect = hr,
                        ci_lower = ci_lower,
                        ci_upper = ci_upper,
                        p_value = p_value,
                        n_gksrs = n_gksrs,
                        n_plaque = n_plaque,
                        n_total = n_total
                    ))
                }

                # Store results
                subgroup_results[[subgroup_var]] <- list(
                    interaction_p = interaction_p,
                    subgroup_effects = subgroup_effects,
                    was_continuous = was_continuous,
                    cutoff_value = if (was_continuous) cutoff_val else NULL
                )

            },
            error = function(e) {
                log_message(sprintf("Error in subgroup analysis for %s: %s", subgroup_var, e$message))
                subgroup_results[[subgroup_var]] <- list(
                    interaction_p = NA,
                    subgroup_effects = data.frame(),
                    error = e$message
                )
            }
        )
    }

    return(subgroup_results)
}

#' Analyze treatment effects across subgroups for binary outcomes
#'
#' Performs subgroup analysis for binary outcomes (local recurrence, metastatic progression)
#' using logistic regression with interaction terms
#'
#' @param data Data frame containing the analysis variables
#' @param outcome_var Name of the binary outcome variable
#' @param subgroup_vars Character vector of subgroup variables to test
#' @param confounders Character vector of confounders
#' @param outcome_name Name of the outcome for labeling
#' @return List of subgroup analysis results
analyze_treatment_effect_subgroups_binary <- function(data, outcome_var, subgroup_vars, confounders = NULL, outcome_name = "Binary Outcome") {
    log_message(sprintf("Performing subgroup analysis for %s", outcome_name))

    subgroup_results <- list()

    for (subgroup_var in subgroup_vars) {
        log_message(sprintf("Testing interaction for: %s", subgroup_var))

        tryCatch(
            {
                # Filter confounders to exclude the subgroup variable
                confounders_to_use <- if (!is.null(confounders)) {
                    confounders[confounders != subgroup_var]
                } else {
                    NULL
                }

                # Validate confounders
                if (length(confounders_to_use) > 0) {
                    confounders_to_use <- generate_valid_confounders(data, confounders_to_use, threshold = THRESHOLD_RARITY)
                }

                # Process subgroup variable (bin if continuous)
                processed_data <- data
                was_continuous <- is.numeric(data[[subgroup_var]]) || is.integer(data[[subgroup_var]])

                if (was_continuous) {
                    cutoff_val <- quantile(data[[subgroup_var]], probs = 0.5, na.rm = TRUE)
                    subgroup_var_binned <- paste0(subgroup_var, "_binned")
                    processed_data[[subgroup_var_binned]] <- factor(
                        ifelse(data[[subgroup_var]] < cutoff_val,
                            paste0("< ", round(cutoff_val, 1)),
                            paste0("≥ ", round(cutoff_val, 1))
                        ),
                        levels = c(
                            paste0("< ", round(cutoff_val, 1)),
                            paste0("≥ ", round(cutoff_val, 1))
                        )
                    )
                    subgroup_var_to_use <- subgroup_var_binned
                } else {
                    if (!is.factor(processed_data[[subgroup_var]])) {
                        processed_data[[subgroup_var]] <- as.factor(processed_data[[subgroup_var]])
                    }
                    processed_data <- handle_rare_categories(processed_data, vars = subgroup_var, threshold = THRESHOLD_RARITY)
                    subgroup_var_to_use <- subgroup_var
                }

                # Ensure consistent contrasts
                processed_data <- ensure_consistent_contrasts(processed_data)

                # Build formula with interaction
                if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
                    formula_str <- paste0(outcome_var, " ~ treatment_group * ", subgroup_var_to_use)
                } else {
                    formula_str <- paste0(outcome_var, " ~ treatment_group * ", subgroup_var_to_use, " + ", paste(confounders_to_use, collapse = " + "))
                }

                # Fit logistic model with interaction
                logit_model <- glm(as.formula(formula_str), family = binomial(), data = processed_data)

                # Test interaction significance
                subgroup_levels <- levels(processed_data[[subgroup_var_to_use]])

                if (length(subgroup_levels) == 2) {
                    # Simple interaction test
                    interaction_term <- get_interaction_coefficient_name(
                        logit_model, "treatment_group",
                        subgroup_var_to_use, subgroup_levels[2], processed_data
                    )
                    if (!is.null(interaction_term)) {
                        interaction_p <- summary(logit_model)$coefficients[interaction_term, "Pr(>|z|)"]
                    } else {
                        interaction_p <- NA
                    }
                } else {
                    # Multiple levels - use likelihood ratio test
                    if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
                        formula_no_int <- paste0(outcome_var, " ~ treatment_group + ", subgroup_var_to_use)
                    } else {
                        formula_no_int <- paste0(outcome_var, " ~ treatment_group + ", subgroup_var_to_use, " + ", paste(confounders_to_use, collapse = " + "))
                    }
                    logit_no_interaction <- glm(as.formula(formula_no_int), family = binomial(), data = processed_data)
                    interaction_test <- anova(logit_no_interaction, logit_model, test = "Chisq")
                    interaction_p <- interaction_test$`Pr(>Chi)`[2]
                }

                # Calculate odds ratios for each subgroup
                subgroup_effects <- data.frame()
                for (level in subgroup_levels) {
                    # Create subset for this subgroup
                    subset_data <- processed_data[processed_data[[subgroup_var_to_use]] == level, ]

                    # Fit model for this subgroup
                    if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
                        subset_formula <- paste0(outcome_var, " ~ treatment_group")
                    } else {
                        subset_formula <- paste0(outcome_var, " ~ treatment_group + ", paste(confounders_to_use, collapse = " + "))
                    }

                    subset_model <- glm(as.formula(subset_formula), family = binomial(), data = subset_data)
                    subset_summary <- summary(subset_model)

                    # Extract odds ratio and confidence intervals
                    or <- exp(subset_summary$coefficients[2, 1])
                    ci_lower <- exp(subset_summary$coefficients[2, 1] - 1.96 * subset_summary$coefficients[2, 2])
                    ci_upper <- exp(subset_summary$coefficients[2, 1] + 1.96 * subset_summary$coefficients[2, 2])
                    p_value <- subset_summary$coefficients[2, 4]

                    # Get sample sizes
                    n_gksrs <- sum(subset_data$treatment_group == "GKSRS")
                    n_plaque <- sum(subset_data$treatment_group == "Plaque")
                    n_total <- n_gksrs + n_plaque

                    # Add to results
                    subgroup_effects <- rbind(subgroup_effects, data.frame(
                        subgroup_level = level,
                        treatment_effect = or,
                        ci_lower = ci_lower,
                        ci_upper = ci_upper,
                        p_value = p_value,
                        n_gksrs = n_gksrs,
                        n_plaque = n_plaque,
                        n_total = n_total
                    ))
                }

                # Store results
                subgroup_results[[subgroup_var]] <- list(
                    interaction_p = interaction_p,
                    subgroup_effects = subgroup_effects,
                    was_continuous = was_continuous,
                    cutoff_value = if (was_continuous) cutoff_val else NULL
                )

            },
            error = function(e) {
                log_message(sprintf("Error in subgroup analysis for %s: %s", subgroup_var, e$message))
                subgroup_results[[subgroup_var]] <- list(
                    interaction_p = NA,
                    subgroup_effects = data.frame(),
                    error = e$message
                )
            }
        )
    }

    return(subgroup_results)
}

#' Format subgroup analysis results into publication-ready table
#'
#' Creates a formatted table of subgroup analysis results for publication
#'
#' @param subgroup_results List of subgroup analysis results
#' @param outcome_name Name of the outcome being analyzed
#' @param effect_measure Type of effect measure ("HR" for hazard ratio, "OR" for odds ratio, "MD" for mean difference)
#' @param output_path Full path for saving the table
#' @return Formatted data frame
format_subgroup_analysis_results <- function(subgroup_results, outcome_name, effect_measure = "HR", output_path = NULL) {
    # Combine all subgroup results into a single data frame
    table_data <- data.frame()

    for (var_name in names(subgroup_results)) {
        result <- subgroup_results[[var_name]]

        if (!is.null(result$subgroup_effects) && nrow(result$subgroup_effects) > 0) {
            temp_data <- result$subgroup_effects %>%
                mutate(
                    subgroup_variable = var_name,
                    variable_label = tools::toTitleCase(gsub("_", " ", var_name)),
                    level_label = case_when(
                        # Clean up common variable names
                        var_name == "treatment_group" ~ as.character(subgroup_level),
                        var_name == "sex" ~ as.character(subgroup_level),
                        var_name == "location" ~ as.character(subgroup_level),
                        var_name == "optic_nerve" ~ ifelse(as.character(subgroup_level) == "Y", "Yes", "No"),
                        var_name == "biopsy1_gep" ~ as.character(subgroup_level),
                        TRUE ~ as.character(subgroup_level)
                    ),
                    interaction_p = result$interaction_p
                )
            table_data <- rbind(table_data, temp_data)
        }
    }

    if (nrow(table_data) == 0) {
        warning("No data available for table")
        return(NULL)
    }

    # Format the table
    formatted_table <- table_data %>%
        mutate(
            # Format effect estimates and confidence intervals
            effect_ci = sprintf(
                "%.2f (%.2f-%.2f)",
                treatment_effect,
                ci_lower,
                ci_upper
            ),
            # Format p-values
            p_value_formatted = sprintf("%.3f", p_value),
            interaction_p_formatted = sprintf("%.3f", interaction_p),
            # Format sample sizes
            sample_size = sprintf("%d/%d", n_gksrs, n_plaque)
        ) %>%
        select(
            variable_label,
            level_label,
            effect_ci,
            p_value_formatted,
            interaction_p_formatted,
            sample_size
        ) %>%
        arrange(variable_label, level_label)

    # Add column headers
    colnames(formatted_table) <- c(
        "Subgroup",
        "Level",
        paste0(effect_measure, " (95% CI)"),
        "p-value",
        "Interaction p-value",
        "GKSRS/Plaque"
    )

    # Save table if path provided
    if (!is.null(output_path)) {
        write.xlsx(formatted_table, output_path)
        log_message(sprintf("Subgroup analysis table saved to: %s", output_path))
    }

    return(formatted_table)
} 