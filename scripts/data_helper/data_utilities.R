# Data Utilities and Processing Functions
# Author: Nicholas Camarda
# Description: Data processing, validation, and utility functions for analysis

#' List available datasets
#'
#' Lists all datasets in the processed data directory that have an .rds extension.
#'
#' @return A character vector of dataset names.
#'
#' @examples
#' list_available_datasets()
list_available_datasets <- function() {
    datasets <- list.files(PROCESSED_DATA_DIR, pattern = "\\.rds$")
    log_enhanced(sprintf("Found %d datasets to analyze", length(datasets)))
    print(datasets)
    return(gsub("\\.rds$", "", datasets))
}

#' Handle rare categories in factor variables
#'
#' Collapses rare categories in specified factor variables into 'Other' if their count is below a threshold.
#' Only creates 'Other' category if at least 2 rare categories are being collapsed.
#'
#' @param data Data frame containing the variables.
#' @param vars Character vector of variable names to check.
#' @param threshold Minimum number of observations required to keep a category (default: 5).
#'
#' @return Data frame with rare categories collapsed to 'Other'.
#' @examples
#' handle_rare_categories(data, vars = c("sex", "location"), threshold = 5)
handle_rare_categories <- function(data, vars, threshold = 5) {
    other_map <- list()
    if (VERBOSE) {
        log_enhanced(sprintf("\nChecking for rare categories (threshold: %d):", threshold))
    }

    for (var in vars) {
        if (var %in% names(data) && is.factor(data[[var]])) {
            log_enhanced(sprintf("Checking for rare categories in %s", var))

            # 1) Forced collapsing based on centralized config (always to 'Other')
            if (exists("FORCED_OTHER_BY_VARIABLE") && var %in% names(FORCED_OTHER_BY_VARIABLE)) {
                forced_levels <- FORCED_OTHER_BY_VARIABLE[[var]]
                # Collapse only levels that actually exist
                forced_levels_present <- intersect(forced_levels, levels(data[[var]]))
                if (length(forced_levels_present) > 0) {
                    if (VERBOSE) {
                        log_enhanced(sprintf("Forcing collapse of specified levels in %s into 'Other': %s", var, paste(forced_levels_present, collapse = ", ")))
                    }
                    # Ensure 'Other' exists in levels to avoid unknown-level warnings when refactoring
                    collapsed <- fct_collapse(data[[var]], Other = forced_levels_present) %>%
                        fct_drop()
                    if ("Other" %in% levels(collapsed)) {
                        collapsed <- fct_relevel(collapsed, "Other", after = Inf)
                    }
                    data[[var]] <- factor(collapsed)
                    # Track forced-collapsed categories
                    other_map[[var]] <- unique(c(other_map[[var]], forced_levels_present))
                }
            }

            # 2) Rarity-based collapsing (post-forced)
            # Get category counts after forced collapse
            cat_counts <- table(data[[var]])
            rare_cats <- names(cat_counts)[cat_counts < threshold & names(cat_counts) != "Other"]
            valid_cats <- names(cat_counts)[cat_counts >= threshold | names(cat_counts) == "Other"]

            if (length(rare_cats) > 0) {
                # Only create/inflate "Other" if at least 2 rare categories to collapse
                if (length(rare_cats) >= 2) {
                    total_rare_count <- sum(cat_counts[rare_cats])
                    would_have_valid_other <- total_rare_count >= threshold
                    final_valid_levels <- length(valid_cats) + (if (would_have_valid_other && !("Other" %in% names(cat_counts))) 1 else 0)

                    if (final_valid_levels >= 2) {
                        if (VERBOSE) {
                            log_enhanced(sprintf("\nCollapsing %d rare categories in %s into 'Other':", length(rare_cats), var))
                            for (cat in rare_cats) {
                                log_enhanced(sprintf("- %s (n=%d)", cat, cat_counts[cat]))
                            }
                        }

                        collapsed <- fct_collapse(data[[var]], Other = rare_cats) %>%
                            fct_drop()
                        if ("Other" %in% levels(collapsed)) {
                            collapsed <- fct_relevel(collapsed, "Other", after = Inf)
                        }
                        data[[var]] <- factor(collapsed)

                        # Track which categories were collapsed into Other (append to any forced ones)
                        other_map[[var]] <- unique(c(other_map[[var]], rare_cats))

                        if (VERBOSE) {
                            log_enhanced(sprintf("After collapse - %s levels: %s", var, paste(levels(data[[var]]), collapse = ", ")))
                            log_enhanced(sprintf("After collapse - %s counts: %s", var, paste(names(table(data[[var]])), "=", table(data[[var]]), collapse = ", ")))
                        }
                    } else {
                        if (VERBOSE) {
                            log_enhanced(sprintf("\nSkipping collapse for %s: would result in insufficient valid levels", var))
                            log_enhanced(sprintf(
                                "Valid categories: %d, Rare total: %d (threshold: %d)",
                                length(valid_cats), total_rare_count, threshold
                            ))
                        }
                    }
                } else {
                    if (VERBOSE) {
                        log_enhanced(sprintf(
                            "\nSkipping collapse for %s: only 1 rare category (%s, n=%d) - not creating 'Other'",
                            var, rare_cats[1], cat_counts[rare_cats[1]]
                        ))
                    }
                }
            }
        }
    }

    # Return both the modified data and the mapping of 'Other' categories
    return(list(data = data, other_map = other_map))
}

#' Generate valid confounders
#'
#' Generates a list of valid confounders that have more than 1 level and at least THRESHOLD_RARITY counts per level.
#'
#' @param data Data frame.
#' @param confounders Character vector of confounder variable names.
#' @param threshold Minimum number of observations required to keep a category (default: THRESHOLD_RARITY).
#' @param verbose Logical indicating whether to log the removal of confounders (default: TRUE).
#' @return Character vector of valid confounders.
#' @examples
#' generate_valid_confounders(data, confounders)
generate_valid_confounders <- function(data, confounders, threshold = THRESHOLD_RARITY, verbose = TRUE) {
    # Before fitting the model, filter confounders to those with >1 level and at least THRESHOLD_RARITY counts per level
    keep_cfs <- sapply(confounders, function(var) {
        var_data <- data[[var]]
        if (is.factor(var_data)) {
            tab <- table(var_data)
            return(sum(tab >= THRESHOLD_RARITY) >= 2)
        } else {
            # For non-factors, require >1 unique value and at least THRESHOLD_RARITY non-NA values
            return(length(unique(na.omit(var_data))) > 1 && sum(!is.na(var_data)) >= THRESHOLD_RARITY)
        }
    })
    valid_confounders <- confounders[keep_cfs]
    # Check if any confounders were removed
    if (verbose && length(confounders) != length(valid_confounders)) {
        log_enhanced("Removed confounders with only 1 level or <THRESHOLD_RARITY counts:")
        log_enhanced(paste(setdiff(confounders, valid_confounders), collapse = ", "))
    }
    return(valid_confounders)
}

#' Bin continuous variables
#'
#' Bins a continuous variable using quantiles or custom breaks.
#'
#' @param vec Numeric vector to bin.
#' @param bins Number of bins (default: 2).
#' @param custom_breaks Optional custom breakpoints.
#' @param varname Optional variable name (for labeling).
#' @param digits_lab Number of digits to round the labels (default: 2).
#'
#' @return Factor with binned values.
#' @examples
#' bin_continuous(1:10, bins = 3)
bin_continuous <- function(vec, bins = 2, custom_breaks = NULL, varname = NULL, digits_lab = 2) {
    if (!is.null(custom_breaks)) {
        cut(vec, breaks = custom_breaks, include.lowest = TRUE, right = FALSE)
    } else {
        # Use quantiles (e.g., median split for bins=2, tertiles for bins=3, etc.)
        q <- quantile(vec, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
        # Ensure unique breaks (if not, fallback to pretty)
        if (length(unique(q)) < length(q)) {
            q <- pretty(vec, n = bins)
        }
        cut(vec, breaks = q, include.lowest = TRUE, right = FALSE, dig.lab = digits_lab)
    }
}

#' Summarize key variables in the dataset
#'
#' Prints summary statistics and distributions for key variables in the data.
#'
#' @param data Data frame.
#'
#' @return None. Side effect: prints summary to console.
#' @param verbose Logical indicating whether to log the summary (default: TRUE).
#' @examples
#' summarize_data(data)
summarize_data <- function(data, verbose = TRUE) {
    if (verbose) {
        log_enhanced("\nData Summary:")
        log_enhanced(sprintf("Total patients: %d", nrow(data)))

        log_enhanced("\nTreatment Groups:")
        print(table(data$treatment_group))

        log_enhanced("\nCohort Distribution:")
        print(table(data$cohort))

        log_enhanced("\nTumor Characteristics:")
        log_enhanced(sprintf("Location: %s", paste(unique(data$location), collapse = ", ")))
        log_enhanced(sprintf("Optic Nerve Involvement: %s", paste(unique(data$optic_nerve), collapse = ", ")))
        log_enhanced(sprintf("Initial Stage: %s", paste(unique(data$initial_overall_stage), collapse = ", ")))

        log_enhanced("\nGene Expression Profile:")
        print(table(data$biopsy1_gep))

        log_enhanced("\nOutcomes:")
        log_enhanced(sprintf("Recurrence: %d patients", sum(data$recurrence_event)))
        log_enhanced(sprintf("Metastasis: %d patients", sum(data$mets_event)))
        log_enhanced(sprintf("Death: %d patients", sum(data$death_event)))

        log_enhanced("\nFollow-up:")
        log_enhanced(sprintf("Median follow-up: %.1f years", median(data$follow_up_years, na.rm = TRUE)))
    }
}

#' Calculate interaction p-value for a single variable
#'
#' This function calculates the interaction p-value between a treatment group variable
#' and another variable for various outcome types. It's designed to be used consistently
#' across the entire analytical pipeline.
#'
#' @param data Data frame containing the analysis variables
#' @param variable_name Name of the variable to test for interaction with treatment_group
#' @param outcome_var Name of the outcome variable
#' @param treatment_var Name of the treatment variable (default: "treatment_group")
#' @param confounders Character vector of confounders to include in models
#' @param outcome_type Type of outcome ("binary", "survival", "continuous")
#' @param time_var Name of time variable for survival outcomes (required if outcome_type = "survival")
#' @param event_var Name of event variable for survival outcomes (required if outcome_type = "survival")
#' @return Numeric p-value or NA if calculation fails
#'
#' @examples
#' # Binary outcome
#' calculate_variable_interaction_pvalue(data, "age_group", "recurrence", confounders = c("sex", "location"))
#'
#' # Survival outcome
#' calculate_variable_interaction_pvalue(data, "age_group", "survival_time",
#'     outcome_type = "survival",
#'     time_var = "follow_up_time", event_var = "death_event"
#' )
#'
#' # Continuous outcome
#' calculate_variable_interaction_pvalue(data, "age_group", "tumor_size", outcome_type = "continuous")
calculate_variable_interaction_pvalue <- function(data, variable_name, outcome_var,
                                                  treatment_var = "treatment_group",
                                                  confounders = NULL,
                                                  outcome_type = "binary",
                                                  time_var = NULL,
                                                  event_var = NULL) {
    # Input validation
    if (length(variable_name) != 1 || !variable_name %in% names(data)) {
        warning(sprintf("Variable '%s' not found in data", variable_name))
        return(NA)
    }

    if (length(treatment_var) != 1 || !treatment_var %in% names(data)) {
        warning(sprintf("Treatment variable '%s' not found in data", treatment_var))
        return(NA)
    }

    if (length(outcome_var) != 1 || !outcome_var %in% names(data)) {
        warning(sprintf("Outcome variable '%s' not found in data", outcome_var))
        return(NA)
    }

    # For survival outcomes, check required variables
    if (outcome_type == "survival") {
        if (is.null(time_var) || length(time_var) != 1 || !time_var %in% names(data)) {
            warning("time_var is required and must exist in data for survival outcomes")
            return(NA)
        }
        if (is.null(event_var) || length(event_var) != 1 || !event_var %in% names(data)) {
            warning("event_var is required and must exist in data for survival outcomes")
            return(NA)
        }
    }

    # Remove rows with missing values for key variables
    required_vars <- c(variable_name, treatment_var, outcome_var)
    if (outcome_type == "survival") {
        required_vars <- c(required_vars, time_var, event_var)
    }
    if (!is.null(confounders)) {
        required_vars <- c(required_vars, confounders)
    }

    data_clean <- data %>%
        filter(if_all(all_of(required_vars), ~ !is.na(.x)))

    if (nrow(data_clean) == 0) {
        warning("No complete cases available for analysis")
        return(NA)
    }

    # Check if variable has sufficient levels/variation
    if (is.factor(data_clean[[variable_name]])) {
        level_counts <- table(data_clean[[variable_name]])
        if (length(level_counts) < 2 || any(level_counts == 0)) {
            warning(sprintf("Variable '%s' has insufficient levels for interaction testing", variable_name))
            return(NA)
        }
    }

    # Check treatment variable has sufficient levels
    if (is.factor(data_clean[[treatment_var]])) {
        treatment_counts <- table(data_clean[[treatment_var]])
        if (length(treatment_counts) < 2 || any(treatment_counts == 0)) {
            warning(sprintf("Treatment variable '%s' has insufficient levels for interaction testing", treatment_var))
            return(NA)
        }
    }

    # Build confounder string
    confounders_str <- if (is.null(confounders) || length(confounders) == 0) {
        ""
    } else {
        paste(" + ", paste(confounders, collapse = " + "))
    }

    # Build interaction term
    interaction_term <- paste0(treatment_var, " * ", variable_name)

    # Build formulas and fit models based on outcome type
    tryCatch(
        {
            if (outcome_type == "binary") {
                # Model with interaction
                formula_str <- paste0(outcome_var, " ~ ", interaction_term, confounders_str)
                model <- glm(as.formula(formula_str), data = data_clean, family = binomial())

                # Model without interaction
                no_interaction_formula <- paste0(
                    outcome_var, " ~ ",
                    treatment_var, " + ", variable_name, confounders_str
                )
                no_interaction_model <- glm(as.formula(no_interaction_formula), data = data_clean, family = binomial())

                # Calculate interaction p-value
                interaction_test <- anova(no_interaction_model, model, test = "Chisq")
                if (nrow(interaction_test) >= 2 && "Pr(>Chi)" %in% names(interaction_test)) {
                    return(interaction_test$`Pr(>Chi)`[2])
                } else {
                    return(NA)
                }
            } else if (outcome_type == "survival") {
                # Model with interaction
                formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", interaction_term, confounders_str)
                model <- coxph(as.formula(formula_str), data = data_clean, model = TRUE)

                # Model without interaction
                no_interaction_formula <- paste0(
                    "Surv(", time_var, ", ", event_var, ") ~ ",
                    treatment_var, " + ", variable_name, confounders_str
                )
                no_interaction_model <- coxph(as.formula(no_interaction_formula), data = data_clean, model = TRUE)

                # Calculate interaction p-value
                interaction_test <- anova(no_interaction_model, model)
                if (length(interaction_test$`Pr(>|Chi|)`) >= 2) {
                    return(interaction_test$`Pr(>|Chi|)`[2])
                } else {
                    return(NA)
                }
            } else if (outcome_type == "continuous") {
                # Model with interaction
                formula_str <- paste0(outcome_var, " ~ ", interaction_term, confounders_str)
                model <- lm(as.formula(formula_str), data = data_clean)

                # Model without interaction
                no_interaction_formula <- paste0(
                    outcome_var, " ~ ",
                    treatment_var, " + ", variable_name, confounders_str
                )
                no_interaction_model <- lm(as.formula(no_interaction_formula), data = data_clean)

                # Calculate interaction p-value
                interaction_test <- anova(no_interaction_model, model)
                if (nrow(interaction_test) >= 2 && "Pr(>F)" %in% names(interaction_test)) {
                    return(interaction_test$`Pr(>F)`[2])
                } else {
                    return(NA)
                }
            } else {
                warning(sprintf("Unsupported outcome type: %s", outcome_type))
                return(NA)
            }
        },
        error = function(e) {
            warning(sprintf("Error calculating interaction p-value for %s: %s", variable_name, e$message))
            return(NA)
        }
    )
}

#' Calculate overall variable significance using likelihood ratio test
#'
#' This function tests whether a variable (as a whole) is significantly associated with the outcome
#' using likelihood ratio tests comparing models with and without the variable.
#'
#' @param data Data frame containing the variables
#' @param variable_name Name of the variable to test
#' @param outcome_var Name of the outcome variable
#' @param treatment_var Name of the treatment variable (default: "treatment_group")
#' @param confounders Character vector of confounders to include in models
#' @param outcome_type Type of outcome ("binary", "survival", or "continuous")
#' @param time_var Name of time variable (required for survival outcomes)
#' @param event_var Name of event variable (required for survival outcomes)
#' @return Numeric p-value from likelihood ratio test, or NA if test fails
#'
#' @examples
#' calculate_variable_overall_significance(data, "age_group", "recurrence", confounders = c("sex", "location"))
#' calculate_variable_overall_significance(data, "age_group", "survival_time",
#'     outcome_type = "survival",
#'     time_var = "time", event_var = "event"
#' )
calculate_variable_overall_significance <- function(data, variable_name, outcome_var,
                                                    treatment_var = "treatment_group",
                                                    confounders = NULL,
                                                    outcome_type = "binary",
                                                    time_var = NULL,
                                                    event_var = NULL) {
    # Input validation
    if (length(variable_name) != 1 || !variable_name %in% names(data)) {
        warning(sprintf("Variable '%s' not found in data", variable_name))
        return(NA)
    }

    if (length(treatment_var) != 1 || !treatment_var %in% names(data)) {
        warning(sprintf("Treatment variable '%s' not found in data", treatment_var))
        return(NA)
    }

    if (length(outcome_var) != 1 || !outcome_var %in% names(data)) {
        warning(sprintf("Outcome variable '%s' not found in data", outcome_var))
        return(NA)
    }

    # For survival outcomes, check required variables
    if (outcome_type == "survival") {
        if (is.null(time_var) || length(time_var) != 1 || !time_var %in% names(data)) {
            warning("time_var is required and must exist in data for survival outcomes")
            return(NA)
        }
        if (is.null(event_var) || length(event_var) != 1 || !event_var %in% names(data)) {
            warning("event_var is required and must exist in data for survival outcomes")
            return(NA)
        }
    }

    # Remove rows with missing values for key variables
    required_vars <- c(variable_name, treatment_var, outcome_var)
    if (outcome_type == "survival") {
        required_vars <- c(required_vars, time_var, event_var)
    }
    if (!is.null(confounders)) {
        required_vars <- c(required_vars, confounders)
    }

    data_clean <- data %>%
        filter(if_all(all_of(required_vars), ~ !is.na(.x)))

    if (nrow(data_clean) == 0) {
        warning("No complete cases available for analysis")
        return(NA)
    }

    # Check if variable has sufficient levels/variation
    if (is.factor(data_clean[[variable_name]])) {
        level_counts <- table(data_clean[[variable_name]])
        if (length(level_counts) < 2 || any(level_counts == 0)) {
            warning(sprintf("Variable '%s' has insufficient levels for significance testing", variable_name))
            return(NA)
        }
    }

    # Build confounder string
    confounders_str <- if (is.null(confounders) || length(confounders) == 0) {
        ""
    } else {
        paste(" + ", paste(confounders, collapse = " + "))
    }

    # Build formulas and fit models based on outcome type
    tryCatch(
        {
            if (outcome_type == "binary") {
                if (variable_name == treatment_var) {
                    # Test treatment_group: reduced model excludes treatment_var entirely
                    formula_with_var <- paste0(outcome_var, " ~ ", treatment_var, confounders_str)
                    formula_without_var <- paste0(outcome_var, " ~ 1", confounders_str)
                } else {
                    # Test other variables: reduced model keeps treatment_var, excludes variable_name
                    formula_with_var <- paste0(outcome_var, " ~ ", treatment_var, " + ", variable_name, confounders_str)
                    formula_without_var <- paste0(outcome_var, " ~ ", treatment_var, confounders_str)
                }
                model_with_var <- glm(as.formula(formula_with_var), data = data_clean, family = binomial())
                model_without_var <- glm(as.formula(formula_without_var), data = data_clean, family = binomial())

                # Check if both models converged
                if (!model_with_var$converged || !model_without_var$converged) {
                    warning(sprintf("Models did not converge for variable '%s'. Likelihood ratio test may be unreliable.", variable_name))
                    # Try the likelihood ratio test anyway, but handle errors gracefully
                    lrt_test <- tryCatch(
                        {
                            anova(model_without_var, model_with_var, test = "Chisq")
                        },
                        error = function(e) {
                            warning(sprintf("Likelihood ratio test failed for variable '%s': %s", variable_name, e$message))
                            return(NULL)
                        }
                    )
                } else {
                    # Calculate likelihood ratio test
                    lrt_test <- tryCatch(
                        {
                            anova(model_without_var, model_with_var, test = "Chisq")
                        },
                        error = function(e) {
                            warning(sprintf("Likelihood ratio test failed for variable '%s': %s", variable_name, e$message))
                            return(NULL)
                        }
                    )
                }

                if (!is.null(lrt_test) && nrow(lrt_test) >= 2 && "Pr(>Chi)" %in% names(lrt_test)) {
                    return(lrt_test$`Pr(>Chi)`[2])
                } else {
                    # If likelihood ratio test fails, return NA
                    # The individual coefficient p-values will be available in the diagnostic output
                    # and can be used to assess the significance of individual levels
                    warning(sprintf("Likelihood ratio test failed for variable '%s'. Factor label p-value will be NA. Check individual coefficient p-values for significance.", variable_name))
                    return(NA)
                }
            } else if (outcome_type == "survival") {
                if (variable_name == treatment_var) {
                    formula_with_var <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", treatment_var, confounders_str)
                    formula_without_var <- paste0("Surv(", time_var, ", ", event_var, ") ~ 1", confounders_str)
                } else {
                    formula_with_var <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", treatment_var, " + ", variable_name, confounders_str)
                    formula_without_var <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", treatment_var, confounders_str)
                }

                # Fit models with error handling
                model_with_var <- tryCatch(
                    {
                        coxph(as.formula(formula_with_var), data = data_clean, model = TRUE)
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )

                model_without_var <- tryCatch(
                    {
                        coxph(as.formula(formula_without_var), data = data_clean, model = TRUE)
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )

                if (is.null(model_with_var) || is.null(model_without_var)) {
                    return(NA)
                }

                # Calculate likelihood ratio test
                lrt_test <- tryCatch(
                    {
                        anova(model_without_var, model_with_var, test = "Chisq")
                    },
                    error = function(e) {
                        return(NULL)
                    }
                )

                if (is.null(lrt_test)) {
                    return(NA)
                }

                if (nrow(lrt_test) >= 2 && "Pr(>|Chi|)" %in% names(lrt_test)) {
                    return(lrt_test$`Pr(>|Chi|)`[2])
                } else {
                    return(NA)
                }
            } else {
                return(NA)
            }
        },
        error = function(e) {
            warning(sprintf("Error in calculate_variable_overall_significance: %s", e$message))
            return(NA)
        }
    )
}

#' Load cohort-specific other_map.rds file
#'
#' Unified function to load cohort-specific other_map files for consistent handling
#' across all analysis functions.
#'
#' @param dataset_name Character string for dataset name (e.g., "uveal_melanoma_full_cohort")
#' @param processed_data_dir Character string for processed data directory
#' @return List containing other_map information for the specific cohort
#' @examples
#' other_map <- get_cohort_specific_other_map("uveal_melanoma_full_cohort")
get_cohort_specific_other_map <- function(dataset_name, processed_data_dir = "final_data/Analytic Dataset") {
    other_map_file <- file.path(processed_data_dir, "other_map.rds")
    if (file.exists(other_map_file)) {
        combined_other_map <- readRDS(other_map_file)
        if (dataset_name %in% names(combined_other_map)) {
            cohort_other_map <- combined_other_map[[dataset_name]]
            log_enhanced(sprintf("Loaded cohort-specific other_map for %s with %d variables", dataset_name, length(cohort_other_map)), level = "INFO")
            return(cohort_other_map)
        } else {
            log_enhanced(sprintf("Dataset %s not found in combined other_map, using empty list", dataset_name), level = "INFO")
            return(list())
        }
    } else {
        log_enhanced(sprintf("No combined other_map.rds found at %s, using empty list", other_map_file), level = "INFO")
        return(list())
    }
}
