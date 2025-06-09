# Subgroup Analysis Functions
# Author: Nicholas Camarda
# Description: Functions for testing treatment interactions and creating subgroup tables

#' Analyze treatment effects across subgroups for tumor height change
#'
#' Tests whether treatment effect on tumor height change differs across subgroups
#' using interaction terms. Bins continuous variables at specified percentile.
#' Now handles confounders while automatically excluding the subgroup variable to avoid collinearity.
#'
#' @param data Data frame containing the analysis variables
#' @param subgroup_var Name of the subgroup variable (character)
#' @param percentile_cut Percentile for binning continuous variables (default: 0.5 for median split)
#' @param confounders Character vector of confounders to adjust for (subgroup variable will be automatically excluded)
#' @param include_baseline_height Logical, whether to include initial_tumor_height as a confounder (default: FALSE for primary analysis)
#' @param create_tables Logical, whether to create formatted HTML tables (default: FALSE for individual calls)
#'
#' @return List containing:
#'   - interaction_p: P-value for the interaction term
#'   - subgroup_effects: Data frame with treatment effects in each subgroup
#'   - model: The fitted linear model object
#'   - subgroup_var_used: Name of the binned variable created
#'   - formula_used: The formula used for the model
#'   - confounders_used: Character vector of confounders actually used in the model
#'
#' @examples
#' analyze_treatment_effect_subgroups_height(data, "age_at_diagnosis", confounders = c("sex", "location"))
#' analyze_treatment_effect_subgroups_height(data, "initial_tumor_height", percentile_cut = 0.75, include_baseline_height = TRUE)
analyze_treatment_effect_subgroups_height <- function(data, subgroup_var, percentile_cut = 0.5, confounders = NULL, include_baseline_height = FALSE, create_tables = FALSE) {
    
    # Calculate tumor height change if not already present
    if (!("height_change" %in% names(data))) {
        data <- data %>%
            mutate(
                height_change = case_when(
                    recurrence1 == "Y" ~ initial_tumor_height - recurrence1_pretreatment_height,
                    TRUE ~ initial_tumor_height - last_height
                )
            )
    }
    
    # Check if subgroup variable exists and has variation
    if (!subgroup_var %in% names(data)) {
        warning(sprintf("Variable '%s' not found in data", subgroup_var))
        return(list(interaction_p = NA, subgroup_effects = data.frame(), 
                   model = NULL, subgroup_var_used = NA, formula_used = NA, confounders_used = NA))
    }
    
    # Remove rows with missing subgroup variable
    data <- data %>% filter(!is.na(.data[[subgroup_var]]))
    
    if (nrow(data) == 0) {
        warning(sprintf("No data remaining after removing missing values for '%s'", subgroup_var))
        return(list(interaction_p = NA, subgroup_effects = data.frame(), 
                   model = NULL, subgroup_var_used = NA, formula_used = NA, confounders_used = NA))
    }
    
    # Handle confounders: exclude the subgroup variable to avoid collinearity
    confounders_to_use <- NULL
    if (!is.null(confounders)) {
        # Remove the subgroup variable from confounders if present
        confounders_to_use <- confounders[confounders != subgroup_var]
        
        # Add initial_tumor_height if requested and not already present
        if (include_baseline_height && !"initial_tumor_height" %in% confounders_to_use) {
            confounders_to_use <- c(confounders_to_use, "initial_tumor_height")
        }
        
        # Validate confounders using existing function
        if (length(confounders_to_use) > 0) {
            confounders_to_use <- generate_valid_confounders(data, confounders_to_use, threshold = THRESHOLD_RARITY)
        }
        
        # Log which confounders are being used
        if (VERBOSE) {
            if (subgroup_var %in% confounders) {
                log_message(sprintf("Excluded '%s' from confounders to avoid collinearity with subgroup variable", subgroup_var))
            }
            if (length(confounders_to_use) > 0) {
                log_message(sprintf("Using confounders for %s interaction: %s", subgroup_var, paste(confounders_to_use, collapse = ", ")))
            } else {
                log_message(sprintf("No confounders used for %s interaction", subgroup_var))
            }
        }
    }
    
    # Track if variable was originally continuous for coefficient naming
    was_continuous <- is.numeric(data[[subgroup_var]]) || is.integer(data[[subgroup_var]])
    
    # Bin continuous subgroup variables at specified percentile
    if (was_continuous) {
        # Check for sufficient variation
        unique_vals <- unique(data[[subgroup_var]])
        if (length(unique_vals) < 2) {
            warning(sprintf("Variable '%s' has insufficient variation (only %d unique values)", 
                           subgroup_var, length(unique_vals)))
            return(list(interaction_p = NA, subgroup_effects = data.frame(), 
                       model = NULL, subgroup_var_used = NA, formula_used = NA, confounders_used = confounders_to_use))
        }
        
        # Calculate the percentile cutoff
        cutoff_val <- quantile(data[[subgroup_var]], probs = percentile_cut, na.rm = TRUE)
        
        # Create binned variable with descriptive labels
        subgroup_var_binned <- paste0(subgroup_var, "_binned")
        data[[subgroup_var_binned]] <- factor(
            ifelse(data[[subgroup_var]] < cutoff_val,
                   paste0("< ", round(cutoff_val, 1)),
                   paste0("≥ ", round(cutoff_val, 1))),
            levels = c(paste0("< ", round(cutoff_val, 1)), 
                      paste0("≥ ", round(cutoff_val, 1)))
        )
        subgroup_var_to_use <- subgroup_var_binned
    } else {
        # For categorical variables, use your existing function to handle rare categories
        if (!is.factor(data[[subgroup_var]])) {
            data[[subgroup_var]] <- as.factor(data[[subgroup_var]])
        }
        
        # Convert ordered factors to regular factors to get proper dummy variable contrasts
        if (is.ordered(data[[subgroup_var]])) {
            data[[subgroup_var]] <- factor(data[[subgroup_var]], ordered = FALSE)
        }
        
        # Use your existing handle_rare_categories function
        data <- handle_rare_categories(data, vars = subgroup_var, threshold = THRESHOLD_RARITY)
        
        # Ensure treatment contrasts for clearer coefficient names
        contrasts(data[[subgroup_var]]) <- contr.treatment(nlevels(data[[subgroup_var]]))
        
        subgroup_var_to_use <- subgroup_var
    }
    
    # Check if the binned/categorical variable has at least 2 levels
    subgroup_levels <- levels(data[[subgroup_var_to_use]])
    if (length(subgroup_levels) < 2) {
        warning(sprintf("Subgroup variable '%s' has only %d level(s) after processing", 
                       subgroup_var_to_use, length(subgroup_levels)))
        return(list(interaction_p = NA, subgroup_effects = data.frame(), 
                   model = NULL, subgroup_var_used = subgroup_var_to_use, formula_used = NA, confounders_used = confounders_to_use))
    }
    
    # Check that each level has both treatment groups
    for (level in subgroup_levels) {
        level_data <- data[data[[subgroup_var_to_use]] == level, ]
        treatment_groups <- unique(level_data$treatment_group)
        if (length(treatment_groups) < 2) {
            warning(sprintf("Subgroup level '%s' has only %d treatment group(s): %s", 
                           level, length(treatment_groups), paste(treatment_groups, collapse = ", ")))
        }
    }
    
    # Try to fit the linear model
    tryCatch({
        # Ensure consistent factor contrasts for modeling
        data <- ensure_consistent_contrasts(data)
        
        # Build model formula with interaction term and confounders
        if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
            formula_str <- paste0("height_change ~ treatment_group * ", subgroup_var_to_use)
        } else {
            formula_str <- paste0("height_change ~ treatment_group * ", subgroup_var_to_use, " + ", paste(confounders_to_use, collapse = " + "))
        }
        
        model_formula <- as.formula(formula_str)
        model <- lm(model_formula, data = data)
        
        # DEBUGGING: Print model summary and coefficient names
        if (VERBOSE) {
            log_message(sprintf("Model formula: %s", formula_str))
            log_message("Available coefficients:")
            print(names(coef(model)))
            log_message("Factor levels:")
            log_message(sprintf("  treatment_group: %s", paste(levels(data$treatment_group), collapse = ", ")))
            log_message(sprintf("  %s: %s", subgroup_var_to_use, paste(levels(data[[subgroup_var_to_use]]), collapse = ", ")))
            log_message("Model summary:")
            print(summary(model))
        }
        
        # Extract interaction p-value (test overall interaction significance)
        model_summary <- summary(model)
        
        # For multiple levels, we need to test the overall interaction significance
        if (length(subgroup_levels) == 2) {
            # Simple case: just one interaction term
            # Use the helper function to find the interaction term robustly
            interaction_term <- get_interaction_coefficient_name(model, "treatment_group", 
                                                               subgroup_var_to_use, subgroup_levels[2], data)
            
            if (VERBOSE) {
                log_message(sprintf("Looking for interaction terms: treatment_group:%s", subgroup_var_to_use))
                if (!is.null(interaction_term)) {
                    log_message(sprintf("Found interaction term: %s", interaction_term))
                } else {
                    log_message("No interaction term found in model coefficients")
                }
            }
            
            if (!is.null(interaction_term)) {
                interaction_p <- model_summary$coefficients[interaction_term, "Pr(>|t|)"]
            } else {
                interaction_p <- NA
            }
        } else {
            # Multiple levels: use F-test for overall interaction significance
            # This requires comparing models with and without interaction
            if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
                formula_no_int <- paste0("height_change ~ treatment_group + ", subgroup_var_to_use)
            } else {
                formula_no_int <- paste0("height_change ~ treatment_group + ", subgroup_var_to_use, " + ", paste(confounders_to_use, collapse = " + "))
            }
            model_no_interaction <- lm(as.formula(formula_no_int), data = data)
            interaction_test <- anova(model_no_interaction, model)
            interaction_p <- interaction_test$`Pr(>F)`[2]
        }
        
        # Calculate treatment effects in each subgroup more clearly
        subgroup_effects <- data.frame()
        
        for (i in seq_along(subgroup_levels)) {
            level <- subgroup_levels[i]
            
            if (VERBOSE) {
                log_message(sprintf("Processing subgroup level %d: %s", i, level))
            }
            
            # Get detailed sample sizes for this subgroup level
            level_data <- data[data[[subgroup_var_to_use]] == level, ]
            n_total <- nrow(level_data)
            n_plaque <- sum(level_data$treatment_group == "Plaque", na.rm = TRUE)
            n_gksrs <- sum(level_data$treatment_group == "GKSRS", na.rm = TRUE)
            
            if (VERBOSE) {
                log_message(sprintf("  Sample sizes - Total: %d, Plaque: %d, GKSRS: %d", n_total, n_plaque, n_gksrs))
            }
            
            if (i == 1) {
                # Reference subgroup: treatment effect = β1 (main effect of treatment)
                
                # Use the new helper function for robust coefficient extraction
                coef_idx <- get_treatment_coefficient_name(model, "treatment_group", data)
                
                if (VERBOSE) {
                    log_message(sprintf("  Reference level - Looking for coefficient: treatment_group coefficient"))
                    if (!is.null(coef_idx)) {
                        log_message(sprintf("  Found coefficient: %s", coef_idx))
                    } else {
                        log_message(sprintf("  Coefficient 'treatment_group%s' not found", levels(data$treatment_group)[2]))
                    }
                }
                
                if (!is.null(coef_idx)) {
                    treatment_effect <- coef(model)[coef_idx]
                    se_effect <- sqrt(vcov(model)[coef_idx, coef_idx])
                    ci_lower <- treatment_effect - 1.96 * se_effect
                    ci_upper <- treatment_effect + 1.96 * se_effect
                    p_val <- model_summary$coefficients[coef_idx, "Pr(>|t|)"]
                    if (VERBOSE) {
                        log_message(sprintf("  Found coefficient - Effect: %.4f, SE: %.4f, p: %.4f", treatment_effect, se_effect, p_val))
                    }
                } else {
                    if (VERBOSE) {
                        log_message("  No main treatment coefficient found")
                    }
                    treatment_effect <- NA; ci_lower <- NA; ci_upper <- NA; p_val <- NA
                }
            } else {
                # Non-reference subgroup: treatment effect = β1 + β3 (main + interaction)
                
                # Use helper functions for robust coefficient extraction
                main_coef_idx <- get_treatment_coefficient_name(model, "treatment_group", data)
                interaction_coef_idx <- get_interaction_coefficient_name(model, "treatment_group", 
                                                                        subgroup_var_to_use, level, data)
                
                if (VERBOSE) {
                    log_message(sprintf("  Non-reference level - Looking for coefficients:"))
                    log_message(sprintf("    Main: treatment_group"))
                    log_message(sprintf("    Interaction candidates: treatment_group:%s%s, treatment_group:%s2, treatment_group:%sYes, treatment_group:%sNo", 
                                      subgroup_var_to_use, level, subgroup_var_to_use, subgroup_var_to_use, subgroup_var_to_use))
                    if (!is.null(interaction_coef_idx)) {
                        log_message(sprintf("    Found interaction: %s", interaction_coef_idx))
                    } else {
                        log_message("    No interaction coefficient found")
                    }
                }
                
                if (!is.null(main_coef_idx) && !is.null(interaction_coef_idx)) {
                    # Combined effect
                    treatment_effect <- coef(model)[main_coef_idx] + coef(model)[interaction_coef_idx]
                    
                    # Standard error using variance-covariance matrix
                    var_main <- vcov(model)[main_coef_idx, main_coef_idx]
                    var_int <- vcov(model)[interaction_coef_idx, interaction_coef_idx]
                    cov_main_int <- vcov(model)[main_coef_idx, interaction_coef_idx]
                    se_effect <- sqrt(var_main + var_int + 2 * cov_main_int)
                    
                    ci_lower <- treatment_effect - 1.96 * se_effect
                    ci_upper <- treatment_effect + 1.96 * se_effect
                    
                    # P-value for combined effect (Wald test)
                    t_stat <- treatment_effect / se_effect
                    df <- model$df.residual
                    p_val <- 2 * (1 - pt(abs(t_stat), df))
                    
                    if (VERBOSE) {
                        log_message(sprintf("  Found both coefficients - Combined effect: %.4f, SE: %.4f, p: %.4f", treatment_effect, se_effect, p_val))
                    }
                } else {
                    if (VERBOSE) {
                        log_message(sprintf("  Missing coefficients - Main found: %s, Interaction found: %s", 
                                          !is.null(main_coef_idx),
                                          !is.null(interaction_coef_idx)))
                        log_message(sprintf("  Available coefficients: %s", paste(names(coef(model)), collapse = ", ")))
                    }
                    treatment_effect <- NA; ci_lower <- NA; ci_upper <- NA; p_val <- NA
                }
            }
            
            # Add to results with clear naming and detailed sample sizes
            subgroup_effects <- rbind(subgroup_effects, data.frame(
                subgroup_variable = subgroup_var,
                subgroup_level = level,
                n_total = n_total,
                n_plaque = n_plaque,
                n_gksrs = n_gksrs,
                treatment_effect = treatment_effect,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                p_value = p_val,
                stringsAsFactors = FALSE
            ))
        }
        
        # Return organized results
        return(list(
            interaction_p = interaction_p,
            subgroup_effects = subgroup_effects,
            model = model,
            subgroup_var_used = subgroup_var_to_use,
            formula_used = formula_str,
            confounders_used = confounders_to_use
        ))
        
    }, error = function(e) {
        warning(sprintf("Error fitting model for '%s': %s", subgroup_var, e$message))
        return(list(interaction_p = NA, subgroup_effects = data.frame(), 
                   model = NULL, subgroup_var_used = subgroup_var_to_use, 
                   formula_used = formula_str, confounders_used = confounders_to_use))
    })
}

#' Format subgroup analysis results into publication-ready tables
#'
#' Creates publication-ready HTML tables showing treatment effects within each subgroup level
#' with interaction p-values and detailed sample size information.
#'
#' @param subgroup_results List of results from analyze_treatment_effect_subgroups_height
#' @param dataset_name Name of the dataset for table caption
#' @param output_dir Directory to save the HTML tables
#' @param prefix File prefix for naming
#'
#' @return None. Saves HTML tables to specified directory.
#' @examples
#' format_subgroup_analysis_tables(subgroup_results, "Full Cohort", tables_dir, prefix)
format_subgroup_analysis_tables <- function(subgroup_results, dataset_name, subgroup_dir, prefix) {
    
    # subgroup_dir is now passed directly from calling function
    if (!dir.exists(subgroup_dir)) {
        dir.create(subgroup_dir, recursive = TRUE)
    }
    
    # Process each subgroup variable
    for (var_name in names(subgroup_results)) {
        result <- subgroup_results[[var_name]]
        
        if (!is.null(result$subgroup_effects) && nrow(result$subgroup_effects) > 0) {
            
            # Create individual table for this variable
            effects_data <- result$subgroup_effects %>%
                mutate(
                    # Format treatment effect with CI
                    Treatment_Effect_CI = ifelse(
                        is.na(treatment_effect),
                        "Insufficient data",
                        sprintf("%.2f (%.2f, %.2f)", treatment_effect, ci_lower, ci_upper)
                    ),
                    # Format p-value
                    P_Value = ifelse(is.na(p_value), "—", sprintf("%.4f", p_value)),
                    # Format sample sizes
                    Sample_Size = sprintf("%d (%d Plaque + %d GKSRS)", n_total, n_plaque, n_gksrs),
                    # Clean subgroup level names
                    Subgroup_Level = tools::toTitleCase(as.character(subgroup_level))
                ) %>%
                select(
                    `Subgroup Level` = Subgroup_Level,
                    `Sample Size` = Sample_Size,
                    `Treatment Effect (95% CI)` = Treatment_Effect_CI,
                    `P-value` = P_Value
                )
            
            # Format interaction p-value for display
            interaction_text <- ifelse(
                is.na(result$interaction_p), 
                "Model convergence issue", 
                sprintf("%.4f", result$interaction_p)
            )
            
            # Determine significance and interpretation
            interaction_significant <- !is.na(result$interaction_p) && result$interaction_p < 0.05
            significance_text <- ifelse(
                is.na(result$interaction_p),
                "Cannot determine significance due to model issues",
                ifelse(interaction_significant,
                       "**Significant interaction (p < 0.05):** Treatment effect differs across subgroups",
                       "**Non-significant interaction (p ≥ 0.05):** Treatment effect is similar across subgroups")
            )
            
            # Create gt table with interaction info prominently displayed
            var_table <- effects_data %>%
                gt::gt() %>%
                gt::tab_header(
                    title = md(sprintf("**Subgroup Analysis: %s**", tools::toTitleCase(gsub("_", " ", var_name)))),
                    subtitle = md(sprintf("Treatment Effect on Tumor Height Change | **Interaction P-value: %s**", interaction_text))
                ) %>%
                gt::tab_source_note(
                    source_note = md(sprintf(
                        "**Treatment Effect:** GKSRS vs Plaque (reference). Positive values indicate greater height reduction with GKSRS.\n\n%s\n\n**Model:** %s\n\n**Confounders:** %s\n\n**Dataset:** %s",
                        significance_text,
                        ifelse(is.null(result$formula_used), "Model formula not available", result$formula_used),
                        ifelse(is.null(result$confounders_used) || length(result$confounders_used) == 0, 
                               "None", 
                               paste(result$confounders_used, collapse = ", ")),
                        dataset_name
                    ))
                ) %>%
                gt::tab_style(
                    style = list(
                        gt::cell_text(weight = "bold")
                    ),
                    locations = gt::cells_column_labels()
                ) %>%
                gt::tab_style(
                    style = list(
                        gt::cell_text(style = "italic")
                    ),
                    locations = gt::cells_body(
                        columns = "Treatment Effect (95% CI)",
                        rows = grepl("Insufficient", `Treatment Effect (95% CI)`)
                    )
                ) %>%
                # Highlight significant interactions in the subtitle
                gt::tab_style(
                    style = list(
                        gt::cell_fill(color = "#e8f4fd"),
                        gt::cell_text(weight = "bold")
                    ),
                    locations = gt::cells_title(groups = "subtitle")
                ) %>%
                gt::fmt_markdown(columns = everything())
            
            # Apply special styling for significant interactions
            if (interaction_significant) {
                var_table <- var_table %>%
                    gt::tab_style(
                        style = list(
                            gt::cell_fill(color = "#fff3cd"),
                            gt::cell_text(weight = "bold")
                        ),
                        locations = gt::cells_title(groups = "subtitle")
                    )
            }
            
            # Save individual table
            var_table %>%
                gt::gtsave(
                    filename = file.path(subgroup_dir, paste0(prefix, "subgroup_", var_name, ".html"))
                )
        }
    }
    
    log_message(sprintf("Saved subgroup analysis tables to: %s", subgroup_dir))
} 