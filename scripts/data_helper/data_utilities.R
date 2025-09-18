# Data Utilities and Processing Functions
# Author: Nicholas Camarda
# Description: Data processing, validation, and utility functions for analysis

#' List available datasets
#'
#' Lists all cohort datasets in the processed data directory that follow the
#' naming convention "uveal_melanoma_*_cohort.rds". Ignores non-cohort artifacts
#' like other_map.rds or tool outputs.
#'
#' @return A character vector of dataset names.
#'
#' @examples
#' list_available_datasets()
list_available_datasets <- function() {
    # Only include cohort RDS files
    all_rds <- list.files(PROCESSED_DATA_DIR, pattern = "\\.rds$")
    cohort_rds <- grep("^uveal_melanoma_.*_cohort\\.rds$", all_rds, value = TRUE)

    # Optionally log ignored non-cohort artifacts when verbose
    ignored <- setdiff(all_rds, cohort_rds)
    logger::log_info(sprintf("Found %d datasets to analyze", length(cohort_rds)))
    if (length(ignored) > 0) {
        logger::log_debug(formatted(sprintf("Ignoring non-cohort RDS files: %s", paste(ignored, collapse = ", "))))
    }

    print(cohort_rds)
    return(gsub("\\.rds$", "", cohort_rds))
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
        logger::log_info(sprintf("\nChecking for rare categories (threshold: %d):", threshold))
    }

    for (var in vars) {
        if (var %in% names(data) && is.factor(data[[var]])) {
            logger::log_info(sprintf("Checking for rare categories in %s", var))

            # 1) Forced collapsing based on centralized config (always to 'Other')
            if (exists("FORCED_OTHER_BY_VARIABLE") && var %in% names(FORCED_OTHER_BY_VARIABLE)) {
                forced_levels <- FORCED_OTHER_BY_VARIABLE[[var]]
                # Collapse only levels that actually exist
                forced_levels_present <- intersect(forced_levels, levels(data[[var]]))
                if (length(forced_levels_present) > 0) {
                    if (VERBOSE) {
                        logger::log_info(sprintf("Forcing collapse of specified levels in %s into 'Other': %s", var, paste(forced_levels_present, collapse = ", ")))
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
                    collapsed <- fct_collapse(data[[var]], Other = rare_cats) %>%
                        fct_drop()
                    if ("Other" %in% levels(collapsed)) {
                        collapsed <- fct_relevel(collapsed, "Other", after = Inf)
                    }
                    data[[var]] <- factor(collapsed)
                    other_map[[var]] <- unique(c(other_map[[var]], rare_cats))
                    if (VERBOSE) {
                        logger::log_info(sprintf("Collapsed rare categories into 'Other' for %s: %s", var, paste(rare_cats, collapse = ", ")))
                    }
                } else {
                    if (VERBOSE) {
                        logger::log_info(sprintf("Skipping 'Other' creation for %s (only 1 rare category)", var))
                    }
                    # Keep categories as-is when only one rare category (no subsetting to avoid length mismatch)
                    data[[var]] <- factor(data[[var]])
                }
            }
        }
    }

    return(list(data = data, other_map = other_map))
}

#' Exclude "Other" categories prior to modeling
#'
#' Removes observations where specified variables take the value "Other" so that
#' downstream models are not fitted on aggregated convenience categories. A
#' summary of the exclusions is returned so diagnostics can describe what was
#' removed alongside the collapsed level mappings in `other_map`.
#'
#' @param data Data frame that will be supplied to the model fitting routine.
#' @param variables Character vector of column names to inspect. Defaults to all
#'   factor or character columns in `data` when NULL.
#' @param other_label Character string denoting the placeholder level (default:
#'   "Other").
#' @param other_map Optional named list describing which original levels were
#'   collapsed into "Other" for each variable.
#'
#' @return List with the filtered data frame (`data`), a diagnostics data frame
#'   (`other_level_details`), the indices of removed rows (`removed_row_indices`),
#'   and the number of unique rows excluded (`removed_row_count`).
exclude_other_categories <- function(data, variables = NULL, other_label = "Other", other_map = list()) {
    if (is.null(variables)) {
        variables <- names(data)[sapply(data, function(col) is.factor(col) || is.character(col))]
    }

    if (length(variables) == 0 || nrow(data) == 0) {
        return(list(
            data = data,
            other_level_details = NULL,
            removed_row_indices = integer(0),
            removed_row_count = 0L
        ))
    }

    removal_mask <- rep(FALSE, nrow(data))
    details_list <- list()

    for (var in variables) {
        if (!var %in% names(data)) {
            next
        }

        column <- data[[var]]
        if (!(is.factor(column) || is.character(column))) {
            next
        }

        is_other <- !is.na(column) & column == other_label
        if (!any(is_other)) {
            next
        }

        removal_mask <- removal_mask | is_other

        mapped_levels <- if (!is.null(other_map) && length(other_map) > 0 && var %in% names(other_map) && length(other_map[[var]]) > 0) {
            paste(other_map[[var]], collapse = ", ")
        } else {
            "Collapsed level details unavailable"
        }

        details_list[[length(details_list) + 1L]] <- data.frame(
            variable = var,
            has_other_level = TRUE,
            other_categories = mapped_levels,
            other_count = sum(is_other),
            other_pct = round(sum(is_other) / nrow(data) * 100, 1),
            stringsAsFactors = FALSE
        )
    }

    if (!any(removal_mask)) {
        return(list(
            data = data,
            other_level_details = NULL,
            removed_row_indices = integer(0),
            removed_row_count = 0L
        ))
    }

    filtered_data <- data[!removal_mask, , drop = FALSE]
    # Drop unused factor levels so removed categories do not persist in modeling outputs
    factor_cols <- names(filtered_data)[sapply(filtered_data, is.factor)]
    if (length(factor_cols) > 0) {
        filtered_data[factor_cols] <- lapply(filtered_data[factor_cols], droplevels)
    }
    removed_indices <- which(removal_mask)
    removed_count <- length(removed_indices)

    other_level_details <- if (length(details_list) > 0) {
        details_df <- do.call(rbind, details_list)
        details_df$unique_rows_removed <- removed_count
        details_df
    } else {
        NULL
    }

    list(
        data = filtered_data,
        other_level_details = other_level_details,
        removed_row_indices = removed_indices,
        removed_row_count = removed_count
    )
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
        logger::log_info("Removed confounders with only 1 level or <THRESHOLD_RARITY counts:")
        logger::log_info(paste(setdiff(confounders, valid_confounders), collapse = ", "))
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
        logger::log_info("\nData Summary:")
        logger::log_info(sprintf("Total patients: %d", nrow(data)))

        logger::log_info("\nTreatment Groups:")
        print(table(data$treatment_group))

        logger::log_info("\nCohort Distribution:")
        print(table(data$cohort))

        logger::log_info("\nTumor Characteristics:")
        logger::log_info(sprintf("Location: %s", paste(unique(data$location), collapse = ", ")))
        logger::log_info(sprintf("Optic Nerve Involvement: %s", paste(unique(data$optic_nerve), collapse = ", ")))
        logger::log_info(sprintf("Initial Stage: %s", paste(unique(data$initial_overall_stage), collapse = ", ")))

        logger::log_info("\nGene Expression Profile:")
        print(table(data$biopsy1_gep))

        logger::log_info("\nOutcomes:")
        logger::log_info(sprintf("Recurrence: %d patients", sum(data$recurrence_event)))
        logger::log_info(sprintf("Metastasis: %d patients", sum(data$mets_event)))
        logger::log_info(sprintf("Death: %d patients", sum(data$death_event)))

        logger::log_info("\nFollow-up:")
        logger::log_info(sprintf("Median follow-up: %.1f years", median(data$follow_up_years, na.rm = TRUE)))
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
get_cohort_specific_other_map <- function(dataset_name, processed_data_dir = PROCESSED_DATA_DIR) {
    other_map_file <- file.path(processed_data_dir, "other_map.rds")
    if (!file.exists(other_map_file)) {
        logger::log_info(sprintf("No combined other_map.rds found at %s, using empty list", other_map_file))
        return(list())
    }

    fi <- tryCatch(file.info(other_map_file), error = function(e) NULL)
    if (is.null(fi) || is.na(fi$size) || fi$size <= 0) {
        logger::log_warn(sprintf("Combined other_map.rds at %s is empty or unreadable; using empty list", other_map_file))
        return(list())
    }

    combined_other_map <- tryCatch(readRDS(other_map_file), error = function(e) NULL)
    if (is.null(combined_other_map) || (!is.list(combined_other_map))) {
        logger::log_warn(sprintf("Combined other_map.rds at %s could not be parsed as a named list; using empty list", other_map_file))
        return(list())
    }

    if (dataset_name %in% names(combined_other_map)) {
        cohort_other_map <- combined_other_map[[dataset_name]]
        logger::log_info(sprintf("Loaded cohort-specific other_map for %s with %d variables", dataset_name, length(cohort_other_map)))
        # Explicit per-cohort log of collapsed levels, if available
        if (length(cohort_other_map) > 0) {
            vars_logged <- utils::head(names(cohort_other_map), 10)
            logger::log_info(sprintf(
                "Collapsed categories recorded for variables (first %d): %s",
                length(vars_logged), paste(vars_logged, collapse = ", ")
            ))
        } else {
            logger::log_info(sprintf("No collapsed categories recorded for %s", dataset_name))
        }
        return(cohort_other_map)
    } else {
        logger::log_info(sprintf("Dataset %s not found in combined other_map, using empty list", dataset_name))
        return(list())
    }
}
