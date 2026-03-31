# Data Utilities and Processing Functions
# Author: Nicholas Camarda
# Description: Data processing, validation, and utility functions for analysis

#' List available datasets
#'
#' Lists all cohort datasets in the processed data directory that follow the
#' naming convention "uveal_melanoma_*_cohort.rds". Ignores non-cohort artifacts
#' like legacy pipeline artifacts or tool outputs.
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

#' Pick a stable patient identifier column for sparse-level diagnostics
pick_sparse_level_id_col <- function(data) {
    key_candidates <- c("id", "patient_id", "record_id", "case_id", "study_id")
    present <- key_candidates[key_candidates %in% names(data)]
    if (length(present) == 0) {
        return(NULL)
    }
    present[[1]]
}

#' Return observed counts for a factor/character vector after exclusions
get_observed_level_counts <- function(values, excluded_levels = character()) {
    if (is.factor(values)) {
        values <- droplevels(values)
    } else {
        values <- coerce_to_factor_preserving_levels(values)
    }

    keep_mask <- !is.na(values)
    if (length(excluded_levels) > 0) {
        keep_mask <- keep_mask & !(as.character(values) %in% excluded_levels)
    }

    values <- droplevels(values[keep_mask])
    counts <- table(values, useNA = "no")

    tibble::tibble(
        level = if (length(counts) == 0) character() else names(counts),
        observed_n = as.integer(counts)
    )
}

#' Evaluate sparse categorical levels for one analysis variable
summarize_sparse_factor_levels <- function(values,
                                           min_level_count = THRESHOLD_RARITY,
                                           explicit_exclusions = character()) {
    counts <- get_observed_level_counts(values)

    explicit_exclusions <- unique(explicit_exclusions[explicit_exclusions %in% counts$level])
    post_explicit_counts <- get_observed_level_counts(values, excluded_levels = explicit_exclusions)
    sparse_levels <- post_explicit_counts$level[post_explicit_counts$observed_n < min_level_count]
    excluded_levels <- unique(c(explicit_exclusions, sparse_levels))
    retained_counts <- get_observed_level_counts(values, excluded_levels = excluded_levels)

    drop_reason <- NA_character_
    if (nrow(retained_counts) < 2) {
        drop_reason <- sprintf(
            "fewer than 2 observed levels remain after exclusions (threshold=%d)",
            min_level_count
        )
    }

    list(
        counts = counts,
        retained_counts = retained_counts,
        explicit_exclusions = explicit_exclusions,
        sparse_levels = sparse_levels,
        excluded_levels = excluded_levels,
        retained_levels = retained_counts$level,
        reference_level = retained_counts$level[[1]] %||% NA_character_,
        drop_reason = drop_reason
    )
}

#' Apply sparse-level exclusions to a model-specific analysis copy
apply_sparse_level_exclusions <- function(data,
                                          variables = NULL,
                                          analysis_name = "analysis",
                                          min_level_count = THRESHOLD_RARITY,
                                          id_col = NULL,
                                          level_exclusions = NULL) {
    initial_row_count <- nrow(data)

    if (is.null(variables)) {
        variables <- names(data)[vapply(data, function(col) is.factor(col) || is.character(col), logical(1))]
    }
    variables <- variables[variables %in% names(data)]

    if (is.null(id_col)) {
        id_col <- pick_sparse_level_id_col(data)
    }

    if (length(variables) == 0 || nrow(data) == 0) {
        return(list(
            data = data,
            sparse_level_diagnostics = NULL,
            variable_screening = tibble::tibble(),
            removed_row_indices = integer(0),
            removed_row_ids = character(0),
            removed_row_count = 0L,
            filter_stats = list(
                initial_n = initial_row_count,
                model_n = initial_row_count,
                removed_n = 0L,
                removed_pct = 0,
                removal_reason = "No sparse-level exclusions applied"
            )
        ))
    }

    removal_mask <- rep(FALSE, nrow(data))
    diagnostics_rows <- list()
    screening_rows <- list()
    exclusions_by_variable <- level_exclusions %||% list()

    for (var in variables) {
        column <- data[[var]]
        if (!(is.factor(column) || is.character(column))) {
            next
        }

        explicit_exclusions <- exclusions_by_variable[[var]] %||% character()
        sparse_summary <- summarize_sparse_factor_levels(
            column,
            min_level_count = min_level_count,
            explicit_exclusions = explicit_exclusions
        )

        screening_rows[[length(screening_rows) + 1L]] <- tibble::tibble(
            analysis_name = analysis_name,
            variable = var,
            status = ifelse(is.na(sparse_summary$drop_reason), "retained", "dropped"),
            reason = ifelse(is.na(sparse_summary$drop_reason),
                "passes sparse-level screening",
                sparse_summary$drop_reason
            ),
            reference_level = sparse_summary$reference_level,
            retained_levels = paste(sparse_summary$retained_levels, collapse = ", "),
            excluded_levels = paste(sparse_summary$excluded_levels, collapse = ", ")
        )

        if (length(sparse_summary$excluded_levels) == 0) {
            next
        }

        level_values <- as.character(column)
        variable_mask <- !is.na(level_values) & level_values %in% sparse_summary$excluded_levels
        if (!any(variable_mask)) {
            next
        }

        removal_mask <- removal_mask | variable_mask

        for (excluded_level in sparse_summary$excluded_levels) {
            level_mask <- !is.na(level_values) & level_values == excluded_level
            if (!any(level_mask)) {
                next
            }

            level_count <- sparse_summary$counts$observed_n[
                match(excluded_level, sparse_summary$counts$level)
            ]
            row_ids <- if (!is.null(id_col) && id_col %in% names(data)) {
                as.character(data[[id_col]][level_mask])
            } else {
                character(0)
            }

            n_plaque <- if ("treatment_group" %in% names(data)) {
                sum(data$treatment_group[level_mask] == "PBT", na.rm = TRUE)
            } else {
                NA_integer_
            }
            n_gksrs <- if ("treatment_group" %in% names(data)) {
                sum(data$treatment_group[level_mask] == "GKSRS", na.rm = TRUE)
            } else {
                NA_integer_
            }

            diagnostics_rows[[length(diagnostics_rows) + 1L]] <- tibble::tibble(
                analysis_name = analysis_name,
                variable = var,
                level = excluded_level,
                observed_n = as.integer(level_count %||% 0L),
                n_plaque = as.integer(n_plaque),
                n_gksrs = as.integer(n_gksrs),
                action = "excluded_rows",
                reason = ifelse(
                    excluded_level %in% sparse_summary$explicit_exclusions,
                    "explicit level exclusion",
                    sprintf("observed count below threshold (%d)", min_level_count)
                ),
                threshold = min_level_count,
                reference_level = sparse_summary$reference_level,
                rows_removed = sum(level_mask),
                row_ids = paste(row_ids, collapse = ", "),
                source = ifelse(
                    excluded_level %in% sparse_summary$explicit_exclusions,
                    "explicit_exclusion",
                    "sparse_level"
                )
            )
        }
    }

    filtered_data <- data[!removal_mask, , drop = FALSE]
    factor_cols <- names(filtered_data)[vapply(filtered_data, is.factor, logical(1))]
    if (length(factor_cols) > 0) {
        filtered_data[factor_cols] <- lapply(filtered_data[factor_cols], droplevels)
    }

    removed_indices <- which(removal_mask)
    removed_ids <- if (!is.null(id_col) && id_col %in% names(data) && length(removed_indices) > 0) {
        as.character(data[[id_col]][removed_indices])
    } else {
        character(0)
    }

    filter_stats <- list(
        initial_n = initial_row_count,
        model_n = nrow(filtered_data),
        removed_n = length(removed_indices),
        removed_pct = if (initial_row_count > 0) round(length(removed_indices) / initial_row_count * 100, 1) else 0,
        removal_reason = "Excluded sparse categorical levels before modeling"
    )

    list(
        data = filtered_data,
        sparse_level_diagnostics = if (length(diagnostics_rows) > 0) dplyr::bind_rows(diagnostics_rows) else NULL,
        variable_screening = if (length(screening_rows) > 0) dplyr::bind_rows(screening_rows) else tibble::tibble(),
        removed_row_indices = removed_indices,
        removed_row_ids = removed_ids,
        removed_row_count = length(removed_indices),
        filter_stats = filter_stats
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
            counts <- get_observed_level_counts(var_data)
            return(sum(counts$observed_n >= threshold) >= 2)
        } else {
            # For non-factors, require >1 unique value and at least THRESHOLD_RARITY non-NA values
            return(length(unique(na.omit(var_data))) > 1 && sum(!is.na(var_data)) >= threshold)
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

    factor_cols <- names(data_clean)[vapply(data_clean, is.factor, logical(1))]
    if (length(factor_cols) > 0) {
        data_clean[factor_cols] <- lapply(data_clean[factor_cols], droplevels)
    }

    if (nrow(data_clean) == 0) {
        warning("No complete cases available for analysis")
        return(NA)
    }

    # Check if variable has sufficient levels/variation
    if (is.factor(data_clean[[variable_name]])) {
        level_counts <- get_observed_level_counts(data_clean[[variable_name]])
        if (nrow(level_counts) < 2) {
            warning(sprintf("Variable '%s' has insufficient levels for interaction testing", variable_name))
            return(NA)
        }
    }

    # Check treatment variable has sufficient levels
    if (is.factor(data_clean[[treatment_var]])) {
        treatment_counts <- get_observed_level_counts(data_clean[[treatment_var]])
        if (nrow(treatment_counts) < 2) {
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

    factor_cols <- names(data_clean)[vapply(data_clean, is.factor, logical(1))]
    if (length(factor_cols) > 0) {
        data_clean[factor_cols] <- lapply(data_clean[factor_cols], droplevels)
    }

    if (nrow(data_clean) == 0) {
        warning("No complete cases available for analysis")
        return(NA)
    }

    has_usable_variation <- function(x) {
        x_non_missing <- x[!is.na(x)]
        if (length(x_non_missing) == 0) {
            return(FALSE)
        }

        if (is.factor(x_non_missing)) {
            return(nlevels(droplevels(x_non_missing)) >= 2)
        }

        length(unique(x_non_missing)) >= 2
    }

    # Check if primary variables have sufficient levels/variation
    if (!has_usable_variation(data_clean[[variable_name]])) {
        warning(sprintf("Variable '%s' has insufficient levels for significance testing", variable_name))
        return(NA)
    }

    if (!has_usable_variation(data_clean[[treatment_var]])) {
        warning(sprintf("Treatment variable '%s' has insufficient variation for significance testing", treatment_var))
        return(NA)
    }

    # Drop confounders that become constant after complete-case filtering to avoid
    # nested-model failures when rendering adjusted tables.
    if (!is.null(confounders) && length(confounders) > 0) {
        confounders <- confounders[vapply(confounders, function(confounder) {
            confounder %in% names(data_clean) && has_usable_variation(data_clean[[confounder]])
        }, logical(1))]
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
