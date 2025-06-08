# Forest plot functions and comprehensive subgroup analysis

#' Create forest plot for subgroup analysis
#'
#' Creates a forest plot showing treatment effects across different subgroups
#' similar to the example in Figure 3 provided by the collaborator
#'
#' @param subgroup_results List of subgroup analysis results
#' @param outcome_name Name of the outcome being analyzed
#' @param effect_measure Type of effect measure ("HR" for hazard ratio, "OR" for odds ratio, "MD" for mean difference)
#' @param dataset_name Name of the dataset
#' @param output_path Full path for saving the plot
#' @return ggplot object
create_forest_plot <- function(subgroup_results, outcome_name, effect_measure = "HR", dataset_name = "", output_path = NULL) {
    # Combine all subgroup results into a single data frame
    forest_data <- data.frame()

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
            forest_data <- rbind(forest_data, temp_data)
        }
    }

    if (nrow(forest_data) == 0) {
        warning("No data available for forest plot")
        return(NULL)
    }

    # Determine x-axis label and reference line based on effect measure
    x_label <- case_when(
        effect_measure == "HR" ~ "Hazard Ratio (95% CI)",
        effect_measure == "OR" ~ "Odds Ratio (95% CI)",
        effect_measure == "MD" ~ "Mean Difference (95% CI)",
        TRUE ~ "Effect Estimate (95% CI)"
    )

    ref_line <- ifelse(effect_measure == "MD", 0, 1)
    favors_left <- ifelse(effect_measure == "MD", "Favours Plaque", "Favours Plaque")
    favors_right <- ifelse(effect_measure == "MD", "Favours GKSRS", "Favours GKSRS")

    # Create the plot data with proper ordering
    plot_data <- forest_data %>%
        filter(!is.na(treatment_effect)) %>%
        mutate(
            # Create combined label for display
            combined_label = paste0(variable_label, ": ", level_label),
            # Format sample sizes
            n_label = sprintf("%d/%d", n_gksrs, n_plaque),
            # Determine significance for interaction
            interaction_sig = ifelse(is.na(interaction_p), "",
                ifelse(interaction_p < 0.05, "*", "")
            ),
            # Add asterisk to variable name if interaction is significant
            variable_display = paste0(variable_label, interaction_sig)
        ) %>%
        arrange(variable_label, subgroup_level)

    # Create forest plot
    p <- ggplot(plot_data, aes(x = treatment_effect, y = reorder(combined_label, desc(row_number())))) +
        geom_vline(xintercept = ref_line, linetype = "dashed", color = "gray50", size = 0.8) +
        geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
            height = 0.3, size = 0.8, color = "black"
        ) +
        geom_point(aes(size = n_total), shape = 15, color = "black") +
        scale_size_continuous(name = "Sample Size", range = c(2, 5), guide = guide_legend(order = 1)) +
        labs(
            title = paste("Subgroup Analysis:", outcome_name),
            subtitle = paste("Dataset:", dataset_name),
            x = x_label,
            y = "Subgroup",
            caption = "* indicates significant interaction (p < 0.05)\nSquare size proportional to sample size"
        ) +
        theme_classic() +
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 11),
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 10),
            legend.position = "right",
            panel.grid.major.y = element_line(color = "gray90", size = 0.3),
            strip.text = element_text(size = 10, face = "bold"),
            plot.caption = element_text(size = 9, hjust = 0)
        ) +
        # Add sample size annotations
        geom_text(aes(label = n_label),
            x = max(plot_data$ci_upper, na.rm = TRUE) * 1.15,
            size = 3, hjust = 0
        ) +
        # Add p-value annotations
        geom_text(aes(label = sprintf("%.3f", p_value)),
            x = max(plot_data$ci_upper, na.rm = TRUE) * 1.35,
            size = 3, hjust = 0
        ) +
        # Extend x-axis to accommodate annotations
        expand_limits(x = max(plot_data$ci_upper, na.rm = TRUE) * 1.5) +
        # Add labels for direction of effect
        annotate("text",
            x = min(plot_data$ci_lower, na.rm = TRUE) * 0.8,
            y = 0.5, label = favors_left, hjust = 1, size = 3.5, color = "blue"
        ) +
        annotate("text",
            x = max(plot_data$ci_upper, na.rm = TRUE) * 0.8,
            y = 0.5, label = favors_right, hjust = 0, size = 3.5, color = "blue"
        )

    # Add column headers
    p <- p +
        annotation_custom(
            grob = textGrob("GKSRS/Plaque", gp = gpar(fontsize = 9, fontface = "bold")),
            xmin = max(plot_data$ci_upper, na.rm = TRUE) * 1.15,
            xmax = max(plot_data$ci_upper, na.rm = TRUE) * 1.15,
            ymin = nrow(plot_data) + 0.5,
            ymax = nrow(plot_data) + 0.5
        ) +
        annotation_custom(
            grob = textGrob("p-value", gp = gpar(fontsize = 9, fontface = "bold")),
            xmin = max(plot_data$ci_upper, na.rm = TRUE) * 1.35,
            xmax = max(plot_data$ci_upper, na.rm = TRUE) * 1.35,
            ymin = nrow(plot_data) + 0.5,
            ymax = nrow(plot_data) + 0.5
        )

    # Save plot if path provided
    if (!is.null(output_path)) {
        ggsave(output_path, p, width = 12, height = max(6, nrow(plot_data) * 0.4), dpi = 300)
        log_message(sprintf("Forest plot saved to: %s", output_path))
    }

    return(p)
}

#' Perform comprehensive subgroup analysis for survival outcomes
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
perform_survival_subgroup_analysis <- function(data, time_var, event_var, subgroup_vars, confounders = NULL, outcome_name = "Survival") {
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

                for (i in seq_along(subgroup_levels)) {
                    level <- subgroup_levels[i]
                    level_data <- processed_data[processed_data[[subgroup_var_to_use]] == level, ]

                    n_total <- nrow(level_data)
                    n_plaque <- sum(level_data$treatment_group == "Plaque", na.rm = TRUE)
                    n_gksrs <- sum(level_data$treatment_group == "GKSRS", na.rm = TRUE)

                    if (i == 1) {
                        # Reference subgroup: main treatment effect
                        coef_idx <- get_treatment_coefficient_name(cox_model, "treatment_group", processed_data)
                        if (!is.null(coef_idx)) {
                            hr <- exp(coef(cox_model)[coef_idx])
                            se_log_hr <- sqrt(vcov(cox_model)[coef_idx, coef_idx])
                            ci_lower <- exp(coef(cox_model)[coef_idx] - 1.96 * se_log_hr)
                            ci_upper <- exp(coef(cox_model)[coef_idx] + 1.96 * se_log_hr)
                            p_val <- summary(cox_model)$coefficients[coef_idx, "Pr(>|z|)"]
                        } else {
                            hr <- NA
                            ci_lower <- NA
                            ci_upper <- NA
                            p_val <- NA
                        }
                    } else {
                        # Non-reference subgroup: combined effect
                        main_coef_idx <- get_treatment_coefficient_name(cox_model, "treatment_group", processed_data)
                        interaction_coef_idx <- get_interaction_coefficient_name(
                            cox_model, "treatment_group",
                            subgroup_var_to_use, level, processed_data
                        )

                        if (!is.null(main_coef_idx) && !is.null(interaction_coef_idx)) {
                            combined_coef <- coef(cox_model)[main_coef_idx] + coef(cox_model)[interaction_coef_idx]
                            hr <- exp(combined_coef)

                            # Standard error for combined effect
                            var_main <- vcov(cox_model)[main_coef_idx, main_coef_idx]
                            var_int <- vcov(cox_model)[interaction_coef_idx, interaction_coef_idx]
                            cov_main_int <- vcov(cox_model)[main_coef_idx, interaction_coef_idx]
                            se_combined <- sqrt(var_main + var_int + 2 * cov_main_int)

                            ci_lower <- exp(combined_coef - 1.96 * se_combined)
                            ci_upper <- exp(combined_coef + 1.96 * se_combined)

                            # P-value for combined effect
                            z_stat <- combined_coef / se_combined
                            p_val <- 2 * (1 - pnorm(abs(z_stat)))
                        } else {
                            hr <- NA
                            ci_lower <- NA
                            ci_upper <- NA
                            p_val <- NA
                        }
                    }

                    subgroup_effects <- rbind(subgroup_effects, data.frame(
                        subgroup_variable = subgroup_var,
                        subgroup_level = level,
                        n_total = n_total,
                        n_plaque = n_plaque,
                        n_gksrs = n_gksrs,
                        treatment_effect = hr, # This will be HR for survival outcomes
                        ci_lower = ci_lower,
                        ci_upper = ci_upper,
                        p_value = p_val,
                        stringsAsFactors = FALSE
                    ))
                }

                subgroup_results[[subgroup_var]] <- list(
                    interaction_p = interaction_p,
                    subgroup_effects = subgroup_effects,
                    model = cox_model,
                    subgroup_var_used = subgroup_var_to_use,
                    formula_used = formula_str,
                    confounders_used = confounders_to_use
                )

                log_message(sprintf("  Interaction p-value: %.4f", ifelse(is.na(interaction_p), 999, interaction_p)))
            },
            error = function(e) {
                warning(sprintf("Error in subgroup analysis for %s: %s", subgroup_var, e$message))
                subgroup_results[[subgroup_var]] <- list(
                    interaction_p = NA,
                    subgroup_effects = data.frame(),
                    model = NULL,
                    subgroup_var_used = NA,
                    formula_used = NA,
                    confounders_used = NULL
                )
            }
        )
    }

    return(subgroup_results)
}

#' Perform comprehensive subgroup analysis for binary outcomes
#'
#' Performs subgroup analysis for binary outcomes (recurrence, metastatic progression)
#' using logistic regression with interaction terms
#'
#' @param data Data frame containing the analysis variables
#' @param outcome_var Name of the binary outcome variable
#' @param subgroup_vars Character vector of subgroup variables to test
#' @param confounders Character vector of confounders
#' @param outcome_name Name of the outcome for labeling
#' @return List of subgroup analysis results
perform_binary_subgroup_analysis <- function(data, outcome_var, subgroup_vars, confounders = NULL, outcome_name = "Binary Outcome") {
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

                # Fit logistic regression model with interaction
                logit_model <- glm(as.formula(formula_str), data = processed_data, family = binomial())

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
                    logit_no_interaction <- glm(as.formula(formula_no_int), data = processed_data, family = binomial())
                    interaction_test <- anova(logit_no_interaction, logit_model, test = "Chisq")
                    interaction_p <- interaction_test$`Pr(>Chi)`[2]
                }

                # Calculate odds ratios for each subgroup
                subgroup_effects <- data.frame()

                for (i in seq_along(subgroup_levels)) {
                    level <- subgroup_levels[i]
                    level_data <- processed_data[processed_data[[subgroup_var_to_use]] == level, ]

                    n_total <- nrow(level_data)
                    n_plaque <- sum(level_data$treatment_group == "Plaque", na.rm = TRUE)
                    n_gksrs <- sum(level_data$treatment_group == "GKSRS", na.rm = TRUE)

                    if (i == 1) {
                        # Reference subgroup: main treatment effect
                        coef_idx <- get_treatment_coefficient_name(logit_model, "treatment_group", processed_data)
                        if (!is.null(coef_idx)) {
                            or <- exp(coef(logit_model)[coef_idx])
                            se_log_or <- sqrt(vcov(logit_model)[coef_idx, coef_idx])
                            ci_lower <- exp(coef(logit_model)[coef_idx] - 1.96 * se_log_or)
                            ci_upper <- exp(coef(logit_model)[coef_idx] + 1.96 * se_log_or)
                            p_val <- summary(logit_model)$coefficients[coef_idx, "Pr(>|z|)"]
                        } else {
                            or <- NA
                            ci_lower <- NA
                            ci_upper <- NA
                            p_val <- NA
                        }
                    } else {
                        # Non-reference subgroup: combined effect
                        main_coef_idx <- get_treatment_coefficient_name(logit_model, "treatment_group", processed_data)
                        interaction_coef_idx <- get_interaction_coefficient_name(
                            logit_model, "treatment_group",
                            subgroup_var_to_use, level, processed_data
                        )

                        if (!is.null(main_coef_idx) && !is.null(interaction_coef_idx)) {
                            combined_coef <- coef(logit_model)[main_coef_idx] + coef(logit_model)[interaction_coef_idx]
                            or <- exp(combined_coef)

                            # Standard error for combined effect
                            var_main <- vcov(logit_model)[main_coef_idx, main_coef_idx]
                            var_int <- vcov(logit_model)[interaction_coef_idx, interaction_coef_idx]
                            cov_main_int <- vcov(logit_model)[main_coef_idx, interaction_coef_idx]
                            se_combined <- sqrt(var_main + var_int + 2 * cov_main_int)

                            ci_lower <- exp(combined_coef - 1.96 * se_combined)
                            ci_upper <- exp(combined_coef + 1.96 * se_combined)

                            # P-value for combined effect
                            z_stat <- combined_coef / se_combined
                            p_val <- 2 * (1 - pnorm(abs(z_stat)))
                        } else {
                            or <- NA
                            ci_lower <- NA
                            ci_upper <- NA
                            p_val <- NA
                        }
                    }

                    subgroup_effects <- rbind(subgroup_effects, data.frame(
                        subgroup_variable = subgroup_var,
                        subgroup_level = level,
                        n_total = n_total,
                        n_plaque = n_plaque,
                        n_gksrs = n_gksrs,
                        treatment_effect = or, # This will be OR for binary outcomes
                        ci_lower = ci_lower,
                        ci_upper = ci_upper,
                        p_value = p_val,
                        stringsAsFactors = FALSE
                    ))
                }

                subgroup_results[[subgroup_var]] <- list(
                    interaction_p = interaction_p,
                    subgroup_effects = subgroup_effects,
                    model = logit_model,
                    subgroup_var_used = subgroup_var_to_use,
                    formula_used = formula_str,
                    confounders_used = confounders_to_use
                )

                log_message(sprintf("  Interaction p-value: %.4f", ifelse(is.na(interaction_p), 999, interaction_p)))
            },
            error = function(e) {
                warning(sprintf("Error in subgroup analysis for %s: %s", subgroup_var, e$message))
                subgroup_results[[subgroup_var]] <- list(
                    interaction_p = NA,
                    subgroup_effects = data.frame(),
                    model = NULL,
                    subgroup_var_used = NA,
                    formula_used = NA,
                    confounders_used = NULL
                )
            }
        )
    }

    return(subgroup_results)
}

#' Create combined forest plots for full vs restricted cohorts
#'
#' Creates side-by-side forest plots comparing results from full and restricted cohorts
#'
#' @param full_results Subgroup results from full cohort
#' @param restricted_results Subgroup results from restricted cohort
#' @param outcome_name Name of the outcome
#' @param effect_measure Type of effect measure
#' @param output_path Path for saving the combined plot
#' @return Combined ggplot object
create_combined_forest_plot <- function(full_results, restricted_results, outcome_name, effect_measure = "HR", output_path = NULL) {
    # Create individual plots
    plot_full <- create_forest_plot(full_results, outcome_name, effect_measure, "Full Cohort")
    plot_restricted <- create_forest_plot(restricted_results, outcome_name, effect_measure, "Restricted Cohort")

    if (is.null(plot_full) || is.null(plot_restricted)) {
        warning("Cannot create combined plot - one or both individual plots failed")
        return(NULL)
    }

    # Combine plots side by side
    combined_plot <- plot_grid(plot_full, plot_restricted,
        ncol = 2,
        labels = c("A. Full Cohort", "B. Restricted Cohort"),
        label_size = 12
    )

    # Save combined plot if path provided
    if (!is.null(output_path)) {
        ggsave(output_path, combined_plot, width = 20, height = max(8, nrow(plot_full$data) * 0.4), dpi = 300)
        log_message(sprintf("Combined forest plot saved to: %s", output_path))
    }

    return(combined_plot)
}

#' Create comprehensive summary table for subgroup analysis
#'
#' Creates a comprehensive table summarizing subgroup analysis results across cohorts
#'
#' @param full_results List of subgroup results from full cohort
#' @param restricted_results List of subgroup results from restricted cohort
#' @param outcome_name Name of the outcome
#' @param effect_measure Type of effect measure
#' @param output_path Path for saving the table
#' @return gt table object
create_comprehensive_subgroup_table <- function(full_results, restricted_results, outcome_name, effect_measure = "HR", output_path = NULL) {
    # Helper function to extract summary data
    extract_summary_data <- function(results, cohort_name) {
        summary_data <- data.frame()

        for (var_name in names(results)) {
            result <- results[[var_name]]
            if (!is.null(result$subgroup_effects) && nrow(result$subgroup_effects) > 0) {
                for (i in 1:nrow(result$subgroup_effects)) {
                    row <- result$subgroup_effects[i, ]
                    summary_data <- rbind(summary_data, data.frame(
                        cohort = cohort_name,
                        subgroup_variable = tools::toTitleCase(gsub("_", " ", var_name)),
                        subgroup_level = as.character(row$subgroup_level),
                        n_total = row$n_total,
                        n_gksrs = row$n_gksrs,
                        n_plaque = row$n_plaque,
                        treatment_effect = row$treatment_effect,
                        ci_lower = row$ci_lower,
                        ci_upper = row$ci_upper,
                        p_value = row$p_value,
                        interaction_p = result$interaction_p,
                        stringsAsFactors = FALSE
                    ))
                }
            }
        }
        return(summary_data)
    }

    # Extract data from both cohorts
    full_data <- extract_summary_data(full_results, "Full")
    restricted_data <- extract_summary_data(restricted_results, "Restricted")

    # Combine data
    combined_data <- rbind(full_data, restricted_data)

    if (nrow(combined_data) == 0) {
        warning("No data available for comprehensive table")
        return(NULL)
    }

    # Format the data for display
    display_data <- combined_data %>%
        mutate(
            effect_ci = ifelse(
                is.na(treatment_effect),
                "Insufficient data",
                sprintf("%.2f (%.2f-%.2f)", treatment_effect, ci_lower, ci_upper)
            ),
            p_value_fmt = ifelse(is.na(p_value), "—", sprintf("%.3f", p_value)),
            sample_size = sprintf("%d (%d/%d)", n_total, n_gksrs, n_plaque),
            interaction_p_fmt = ifelse(is.na(interaction_p), "—", sprintf("%.3f", interaction_p))
        ) %>%
        select(cohort, subgroup_variable, subgroup_level, sample_size, effect_ci, p_value_fmt, interaction_p_fmt) %>%
        pivot_wider(
            names_from = cohort,
            values_from = c(sample_size, effect_ci, p_value_fmt),
            names_sep = "_"
        )

    # Create gt table
    effect_label <- case_when(
        effect_measure == "HR" ~ "Hazard Ratio (95% CI)",
        effect_measure == "OR" ~ "Odds Ratio (95% CI)",
        effect_measure == "MD" ~ "Mean Difference (95% CI)",
        TRUE ~ "Effect (95% CI)"
    )

    table_gt <- display_data %>%
        gt() %>%
        tab_header(
            title = md(sprintf("**Comprehensive Subgroup Analysis: %s**", outcome_name)),
            subtitle = md(sprintf("Comparison of %s across Full and Restricted Cohorts", effect_label))
        ) %>%
        tab_spanner(
            label = "Full Cohort",
            columns = c(sample_size_Full, effect_ci_Full, p_value_fmt_Full)
        ) %>%
        tab_spanner(
            label = "Restricted Cohort",
            columns = c(sample_size_Restricted, effect_ci_Restricted, p_value_fmt_Restricted)
        ) %>%
        cols_label(
            subgroup_variable = "Variable",
            subgroup_level = "Level",
            sample_size_Full = "N (GKSRS/Plaque)",
            effect_ci_Full = effect_label,
            p_value_fmt_Full = "p-value",
            sample_size_Restricted = "N (GKSRS/Plaque)",
            effect_ci_Restricted = effect_label,
            p_value_fmt_Restricted = "p-value",
            interaction_p_fmt = "Interaction p"
        ) %>%
        tab_source_note(
            source_note = md(sprintf(
                "**%s:** GKSRS vs Plaque (reference). %s\n\n**Interaction p-value** tests whether treatment effects differ across subgroup levels within each cohort.",
                effect_label,
                case_when(
                    effect_measure == "HR" ~ "Values > 1 indicate higher hazard with GKSRS.",
                    effect_measure == "OR" ~ "Values > 1 indicate higher odds with GKSRS.",
                    effect_measure == "MD" ~ "Positive values indicate greater reduction with GKSRS.",
                    TRUE ~ ""
                )
            ))
        ) %>%
        tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_column_labels()
        ) %>%
        tab_style(
            style = cell_text(style = "italic"),
            locations = cells_body(
                columns = contains("effect_ci"),
                rows = grepl("Insufficient", effect_ci_Full) | grepl("Insufficient", effect_ci_Restricted)
            )
        )

    # Save table if path provided
    if (!is.null(output_path)) {
        table_gt %>% gtsave(output_path)
        log_message(sprintf("Comprehensive subgroup table saved to: %s", output_path))
    }

    return(table_gt)
}
