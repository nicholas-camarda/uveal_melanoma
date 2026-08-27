# Subgroup Data Preparation and Common Helpers

#' Standard methods note for estimability-aware subgroup outputs
#'
#' This text is shared by subgroup tables, forest-plot diagnostics, and the
#' Objective 1 artifact contract so the display rule is documented once.
get_subgroup_estimability_method_note <- function() {
    paste(
        "Unsupported subgroup levels remain displayed as not estimable.",
        "Treatment effects are reported only when a supported model produces finite estimates and confidence limits;",
        "interaction p-values are omitted when fewer than two levels support interaction modeling.",
        "A single-level estimate is a stratum-specific treatment effect and does not establish treatment-effect modification."
    )
}

#' Get cutoff value for a variable (clinical or legacy)
#' @param var_name Variable name
#' @param data Data frame (for median calc)
#' @param percentile_cut Percentile for legacy split (default 0.5)
#' @return Numeric cutoff or vector of cutoffs
get_cutoff_value <- function(var_name, data, percentile_cut = 0.5) {
    if (USE_CLINICAL_BINNING_CONTINUOUS && var_name %in% c("initial_tumor_height", "initial_tumor_diameter")) {
        if (var_name == "initial_tumor_height") {
            return(T_STAGE_HEIGHT_CUTOFFS)
        }
        if (var_name == "initial_tumor_diameter") {
            return(T_STAGE_DIAMETER_CUTOFFS)
        }
    } else if (var_name %in% names(LEGACY_CUTOFFS)) {
        return(LEGACY_CUTOFFS[[var_name]])
    } else {
        return(quantile(data[[var_name]], probs = percentile_cut, na.rm = TRUE))
    }
}

#' Create T-stage clinical bins
#' @param values Numeric vector (height/diameter)
#' @param cutoffs Vector of cutoffs
#' @param var_name Variable name
#' @return Factor of bin labels
create_clinical_bins <- function(values, cutoffs, var_name) {
    if (length(cutoffs) == 1) {
        bin_labels <- c(paste0("< ", cutoffs), paste0("≥ ", cutoffs))
        bins <- ifelse(values < cutoffs, bin_labels[1], bin_labels[2])
    } else {
        bin_labels <- character(length(cutoffs) + 1)
        bin_labels[1] <- paste0("≤ ", cutoffs[1])
        for (i in 2:length(cutoffs)) bin_labels[i] <- paste0(cutoffs[i - 1] + 0.1, "-", cutoffs[i])
        bin_labels[length(cutoffs) + 1] <- paste0("> ", cutoffs[length(cutoffs)])
        bins <- cut(values, breaks = c(-Inf, cutoffs, Inf), labels = bin_labels, include.lowest = TRUE)
    }
    factor(bins, levels = bin_labels)
}

#' Get fixed subgroup levels for clinical bins or legacy split
#' @param var_name Variable name
#' @return Character vector of levels or NULL
get_subgroup_levels <- function(var_name) {
    if (USE_CLINICAL_BINNING_CONTINUOUS && var_name %in% c("initial_tumor_height", "initial_tumor_diameter")) {
        if (var_name == "initial_tumor_height") {
            return(c(
                paste0("≤ ", T_STAGE_HEIGHT_CUTOFFS[1], " mm"),
                paste0(T_STAGE_HEIGHT_CUTOFFS[1] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[2], " mm"),
                paste0(T_STAGE_HEIGHT_CUTOFFS[2] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[3], " mm"),
                paste0(T_STAGE_HEIGHT_CUTOFFS[3] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[4], " mm"),
                paste0(T_STAGE_HEIGHT_CUTOFFS[4] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[5], " mm"),
                paste0("> ", T_STAGE_HEIGHT_CUTOFFS[5], " mm")
            ))
        } else {
            return(c(
                paste0("≤ ", T_STAGE_DIAMETER_CUTOFFS[1], " mm"),
                paste0(T_STAGE_DIAMETER_CUTOFFS[1] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[2], " mm"),
                paste0(T_STAGE_DIAMETER_CUTOFFS[2] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[3], " mm"),
                paste0(T_STAGE_DIAMETER_CUTOFFS[3] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[4], " mm"),
                paste0(T_STAGE_DIAMETER_CUTOFFS[4] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[5], " mm"),
                paste0(T_STAGE_DIAMETER_CUTOFFS[5] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[6], " mm"),
                paste0("> ", T_STAGE_DIAMETER_CUTOFFS[6], " mm")
            ))
        }
    } else if (var_name == "age_at_diagnosis") {
        return(c(paste0("< ", LEGACY_CUTOFFS$age_at_diagnosis), paste0("≥ ", LEGACY_CUTOFFS$age_at_diagnosis)))
    } else {
        return(NULL)
    }
}

#' Build reviewer-facing subgroup support audit counts
#'
#' @param data Cohort analytic data frame.
#' @return List with T4 counts and audit data frame.
build_reviewer_subgroup_support_audit <- function(data) {
    t4_n <- if ("initial_t_stage_simple" %in% names(data)) {
        sum(!is.na(data$initial_t_stage_simple) & data$initial_t_stage_simple == "T4", na.rm = TRUE)
    } else {
        0L
    }
    list(
        t4_n = as.integer(t4_n),
        audit = data.frame(
            subgroup_var = "initial_t_stage_simple",
            level = "T4",
            observed_n = t4_n,
            reason = c(
                "T4 is retained in every reviewer-facing subgroup display; each outcome-specific treatment effect is shown when estimable and otherwise labeled not estimable."
            ),
            stringsAsFactors = FALSE
        )
    )
}

#' Process subgroup data (binning & confounders)
#' @param data Data frame
#' @param subgroup_var Variable name
#' @param confounders Confounders to include
#' @param include_baseline_height Logical for height analysis
#' @return List with processed data and metadata
process_subgroup_data <- function(data, subgroup_var, confounders, include_baseline_height = FALSE) {
    if (!subgroup_var %in% names(data)) stop(sprintf("Variable '%s' not found in data", subgroup_var))
    data <- normalize_treatment_group_data(data)
    observed_treatments <- unique(as.character(stats::na.omit(data$treatment_group)))
    unsupported_treatments <- setdiff(observed_treatments, TREATMENT_FACTOR_LEVELS)
    if (length(unsupported_treatments) > 0) {
        stop(sprintf(
            "Unsupported treatment_group values in subgroup analysis: %s",
            paste(unsupported_treatments, collapse = ", ")
        ))
    }
    data$treatment_group <- factor(
        as.character(data$treatment_group),
        levels = TREATMENT_FACTOR_LEVELS
    )
    data <- data %>% dplyr::filter(!is.na(.data[[subgroup_var]]))
    if (nrow(data) == 0) stop(sprintf("No data remaining after removing missing values for '%s'", subgroup_var))
    confounders_to_exclude <- subgroup_var
    if (
        exists("OBJECTIVE1_AGE_SUBGROUP_OPTIONS", inherits = TRUE) &&
            subgroup_var %in% get("OBJECTIVE1_AGE_SUBGROUP_OPTIONS", inherits = TRUE)
    ) {
        confounders_to_exclude <- get("OBJECTIVE1_AGE_SUBGROUP_OPTIONS", inherits = TRUE)
    }
    confounders_to_use <- if (!is.null(confounders)) {
        confounders[!confounders %in% confounders_to_exclude]
    } else {
        NULL
    }
    if (include_baseline_height && !"initial_tumor_height" %in% confounders_to_use) confounders_to_use <- c(confounders_to_use, "initial_tumor_height")
    processed_data <- data
    was_continuous <- is.numeric(data[[subgroup_var]]) || is.integer(data[[subgroup_var]])
    is_categorical_factor <- is.factor(data[[subgroup_var]])
    cutoff_value <- NULL
    continuous_reference_value <- NULL
    modeled_continuously <- was_continuous &&
        exists("CONTINUOUS_INTERACTION_SUBGROUP_VARS", inherits = TRUE) &&
        subgroup_var %in% get("CONTINUOUS_INTERACTION_SUBGROUP_VARS", inherits = TRUE)
    if (modeled_continuously) {
        continuous_reference_value <- if (
            identical(subgroup_var, "age_at_diagnosis") &&
                exists("OBJECTIVE1_AGE_REFERENCE_VALUE", inherits = TRUE)
        ) {
            get("OBJECTIVE1_AGE_REFERENCE_VALUE", inherits = TRUE)
        } else {
            stats::median(data[[subgroup_var]], na.rm = TRUE)
        }
        subgroup_var_to_use <- paste0(subgroup_var, "_centered")
        processed_data[[subgroup_var_to_use]] <- data[[subgroup_var]] - continuous_reference_value
        attr(processed_data[[subgroup_var_to_use]], "continuous_reference_value") <- continuous_reference_value
        attr(processed_data[[subgroup_var_to_use]], "continuous_reference_unit") <- "years"
        attr(processed_data[[subgroup_var_to_use]], "continuous_reference_label") <- if (
            identical(subgroup_var, "age_at_diagnosis") &&
                identical(continuous_reference_value, OBJECTIVE1_GENERAL_POP_MEDIAN_AGE_CUTOFF)
        ) {
            "general-population median"
        } else {
            "cohort median"
        }
    } else if (was_continuous) {
        # Check if a binned version already exists (e.g., age_at_diagnosis_binned)
        subgroup_var_binned <- paste0(subgroup_var, "_binned")
        if (subgroup_var_binned %in% names(data)) {
            # Use existing binned variable instead of creating a new one
            subgroup_var_to_use <- subgroup_var_binned
            cutoff_value <- NULL
        } else {
            # Create new binned variable as before
            cutoff_val <- get_cutoff_value(subgroup_var, data, percentile_cut = 0.5)
            if (subgroup_var %in% c("initial_tumor_height", "initial_tumor_diameter") && USE_CLINICAL_BINNING_CONTINUOUS && length(cutoff_val) > 1) {
                processed_data[[subgroup_var_binned]] <- create_clinical_bins(data[[subgroup_var]], cutoff_val, subgroup_var)
                subgroup_var_to_use <- subgroup_var_binned
                cutoff_value <- cutoff_val
            } else {
                processed_data[[subgroup_var_binned]] <- factor(
                    ifelse(data[[subgroup_var]] < cutoff_val, paste0("< ", round(cutoff_val, 1)), paste0("≥ ", round(cutoff_val, 1))),
                    levels = c(paste0("< ", round(cutoff_val, 1)), paste0("≥ ", round(cutoff_val, 1)))
                )
                subgroup_var_to_use <- subgroup_var_binned
                cutoff_value <- cutoff_val
            }
        }
    } else if (is_categorical_factor) {
        subgroup_var_to_use <- subgroup_var
    } else {
        processed_data[[subgroup_var]] <- coerce_to_factor_preserving_levels(processed_data[[subgroup_var]])
        subgroup_var_to_use <- subgroup_var
    }
    if (is.factor(processed_data[[subgroup_var_to_use]])) {
        level_counts <- get_observed_level_counts(processed_data[[subgroup_var_to_use]])
        if (nrow(level_counts) < 2) {
            warning(sprintf("Variable %s has insufficient valid levels (%d)", subgroup_var, nrow(level_counts)))
            return(list(data = NULL, subgroup_var_to_use = NULL, confounders_to_use = NULL, was_continuous = FALSE, cutoff_value = NA, error = "insufficient_levels"))
        }
    }
    processed_data <- enforce_unordered_factors(processed_data)
    list(
        data = processed_data,
        subgroup_var_to_use = subgroup_var_to_use,
        confounders_to_use = confounders_to_use,
        was_continuous = was_continuous,
        modeled_continuously = modeled_continuously,
        continuous_reference_value = continuous_reference_value,
        cutoff_value = cutoff_value
    )
}

#' Resolve the outcome column used for subgroup event counts
#'
#' @param data Data frame used for the subgroup analysis.
#' @param outcome_config List describing the modeled outcome.
#' @return Character column name, or `NULL` for continuous outcomes or missing columns.
resolve_subgroup_event_count_variable <- function(data, outcome_config) {
    if (identical(outcome_config$type, "binary")) {
        outcome_var <- outcome_config$outcome_var %||% NULL
    } else if (identical(outcome_config$type, "survival")) {
        outcome_var <- outcome_config$event_var %||% NULL
    } else {
        return(NULL)
    }

    if (!is.null(outcome_var) && outcome_var %in% names(data)) {
        return(outcome_var)
    }

    NULL
}

#' Count modeled outcome events by treatment arm within a subgroup level
#'
#' @param level_data Data frame restricted to one subgroup level.
#' @param outcome_config List describing the modeled outcome.
#' @return List with `plaque_events`, `gksrs_events`, and `event_var`.
count_subgroup_events_by_arm <- function(level_data, outcome_config) {
    event_var <- resolve_subgroup_event_count_variable(level_data, outcome_config)
    if (is.null(event_var)) {
        return(list(plaque_events = NA_real_, gksrs_events = NA_real_, event_var = NULL))
    }

    event_indicator <- coerce_binary_outcome_vector(level_data[[event_var]])
    list(
        plaque_events = sum(level_data$treatment_group == "PBT" & event_indicator == 1, na.rm = TRUE),
        gksrs_events = sum(level_data$treatment_group == "GKSRS" & event_indicator == 1, na.rm = TRUE),
        event_var = event_var
    )
}

#' Fit model with interaction for a given outcome type
#' @param data Processed data
#' @param outcome_config List with type & vars
#' @param subgroup_var_to_use Binned subgroup variable name
#' @param confounders_to_use Confounders
#' @return List with model, p-value, and diagnostics
fit_subgroup_model <- function(data, outcome_config, subgroup_var_to_use, confounders_to_use) {
    interaction_diagnostics <- list()
    interaction_p <- NA
    if (is.numeric(data[[subgroup_var_to_use]]) || is.integer(data[[subgroup_var_to_use]])) {
        continuous_reference_value <- attr(data[[subgroup_var_to_use]], "continuous_reference_value")
        continuous_reference_unit <- attr(data[[subgroup_var_to_use]], "continuous_reference_unit") %||% "units"
        continuous_reference_label <- attr(data[[subgroup_var_to_use]], "continuous_reference_label") %||% "reference value"
        n_plaque <- sum(data$treatment_group == "PBT", na.rm = TRUE)
        n_gksrs <- sum(data$treatment_group == "GKSRS", na.rm = TRUE)
        interaction_diagnostics$modeled_continuously <- TRUE
        interaction_diagnostics$scale <- paste("per", sub("s$", "", continuous_reference_unit))
        interaction_diagnostics$reference_value <- continuous_reference_value
        interaction_diagnostics$reference_label <- continuous_reference_label
        interaction_diagnostics$level_statistics <- list()

        event_counts <- count_subgroup_events_by_arm(data, outcome_config)
        sample_ok <- n_plaque >= 2 && n_gksrs >= 2
        events_ok <- outcome_config$type == "continuous" ||
            (!is.null(event_counts$event_var) &&
                !is.na(event_counts$plaque_events) &&
                !is.na(event_counts$gksrs_events) &&
                event_counts$plaque_events >= 1 &&
                event_counts$gksrs_events >= 1)
        if (!sample_ok || !events_ok) {
            interaction_diagnostics$failure_reason <- paste(
                c(
                    if (!sample_ok) sprintf("Requires at least 2 patients per arm; observed PBT=%d, GKSRS=%d", n_plaque, n_gksrs),
                    if (!events_ok) sprintf(
                        "Requires at least 1 event per arm; observed PBT events=%s, GKSRS events=%s",
                        ifelse(is.na(event_counts$plaque_events), "NA", event_counts$plaque_events),
                        ifelse(is.na(event_counts$gksrs_events), "NA", event_counts$gksrs_events)
                    )
                ),
                collapse = "; "
            )
            return(list(
                model = NULL,
                interaction_p = NA,
                formula_used = NA,
                interaction_diagnostics = interaction_diagnostics,
                filtered_data = data,
                modeled_continuously = TRUE
            ))
        }

        confounders_str <- if (is.null(confounders_to_use) || length(confounders_to_use) == 0) "" else paste0(" + ", paste(confounders_to_use, collapse = " + "))
        interaction_term <- paste0("treatment_group * ", subgroup_var_to_use)
        if (outcome_config$type == "survival") {
            formula_str <- paste0("Surv(", outcome_config$time_var, ", ", outcome_config$event_var, ") ~ ", interaction_term, confounders_str)
            model <- tryCatch(coxph(as.formula(formula_str), data = data, model = TRUE), error = function(e) NULL)
        } else if (outcome_config$type == "binary") {
            formula_str <- paste0(outcome_config$outcome_var, " ~ ", interaction_term, confounders_str)
            model <- tryCatch(glm(as.formula(formula_str), data = data, family = binomial()), error = function(e) NULL)
        } else {
            formula_str <- paste0(outcome_config$outcome_var, " ~ ", interaction_term, confounders_str)
            model <- tryCatch(lm(as.formula(formula_str), data = data), error = function(e) NULL)
        }
        if (is.null(model)) {
            interaction_diagnostics$failure_reason <- "Continuous interaction model fitting failed"
            return(list(
                model = NULL,
                interaction_p = NA,
                formula_used = formula_str,
                interaction_diagnostics = interaction_diagnostics,
                filtered_data = data,
                modeled_continuously = TRUE
            ))
        }

        attr(model, "subgroup_event_var") <- resolve_subgroup_event_count_variable(data, outcome_config)
        attr(model, "continuous_reference_value") <- continuous_reference_value
        attr(model, "continuous_reference_unit") <- continuous_reference_unit
        attr(model, "continuous_reference_label") <- continuous_reference_label
        interaction_coef_name <- get_interaction_coefficient_name(
            model,
            "treatment_group",
            subgroup_var_to_use,
            subgroup_level = NULL,
            data
        )
        coefficient_table <- summary(model)$coefficients
        if (!is.null(interaction_coef_name) && interaction_coef_name %in% rownames(coefficient_table)) {
            p_column <- if (outcome_config$type == "continuous") "Pr(>|t|)" else "Pr(>|z|)"
            interaction_p <- coefficient_table[interaction_coef_name, p_column]
        } else {
            interaction_diagnostics$failure_reason <- "Continuous treatment-by-age interaction coefficient was not found"
        }
        return(list(
            model = model,
            interaction_p = interaction_p,
            formula_used = formula_str,
            interaction_diagnostics = interaction_diagnostics,
            filtered_data = data,
            modeled_continuously = TRUE
        ))
    }

    valid_levels <- c()
    level_statistics <- list()
    subgroup_levels <- levels(data[[subgroup_var_to_use]])
    interaction_diagnostics$original_level_order <- as.character(subgroup_levels)
    for (level in subgroup_levels) {
        level_data <- data[data[[subgroup_var_to_use]] == level, ]
        n_plaque <- sum(level_data$treatment_group == "PBT", na.rm = TRUE)
        n_gksrs <- sum(level_data$treatment_group == "GKSRS", na.rm = TRUE)

        level_label <- as.character(level)
        plaque_events <- NA
        gksrs_events <- NA
        reason_parts <- c()
        if (outcome_config$type == "survival") {
            event_counts <- count_subgroup_events_by_arm(level_data, outcome_config)
            event_var <- event_counts$event_var
            plaque_events <- event_counts$plaque_events
            gksrs_events <- event_counts$gksrs_events
            sample_ok <- n_plaque >= 2 && n_gksrs >= 2
            events_ok <- !is.null(event_var) && !is.na(plaque_events) && !is.na(gksrs_events) && plaque_events >= 1 && gksrs_events >= 1
            if (!sample_ok) {
                reason_parts <- c(reason_parts, sprintf("Sample size: Requires ≥2 patients per arm; observed PBT=%d, GKSRS=%d", n_plaque, n_gksrs))
            }
            if (!events_ok) {
                reason_parts <- c(reason_parts, sprintf("Event count: Requires ≥1 event per arm; observed PBT events=%s, GKSRS events=%s",
                    ifelse(is.na(plaque_events), "NA", plaque_events), ifelse(is.na(gksrs_events), "NA", gksrs_events)))
            }
            if (sample_ok && events_ok) {
                valid_levels <- c(valid_levels, level)
            }
        } else {
            if (outcome_config$type == "binary") {
                event_counts <- count_subgroup_events_by_arm(level_data, outcome_config)
                plaque_events <- event_counts$plaque_events
                gksrs_events <- event_counts$gksrs_events
            }
            sample_ok <- n_plaque >= 2 && n_gksrs >= 2
            events_ok <- TRUE
            if (!is.na(plaque_events) || !is.na(gksrs_events)) {
                events_ok <- (is.na(plaque_events) || plaque_events >= 1) && (is.na(gksrs_events) || gksrs_events >= 1)
                if (!events_ok) {
                    reason_parts <- c(reason_parts, sprintf("Event count: Low events; PBT events=%s, GKSRS events=%s",
                        ifelse(is.na(plaque_events), "NA", plaque_events), ifelse(is.na(gksrs_events), "NA", gksrs_events)))
                }
            }
            if (!sample_ok) {
                reason_parts <- c(reason_parts, sprintf("Sample size: Requires ≥2 patients per arm; observed PBT=%d, GKSRS=%d", n_plaque, n_gksrs))
            }
            if (sample_ok && events_ok) {
                valid_levels <- c(valid_levels, level)
            }
        }

        level_statistics[[level_label]] <- list(
            n_total = nrow(level_data),
            n_plaque = n_plaque,
            n_gksrs = n_gksrs,
            events_plaque = plaque_events,
            events_gksrs = gksrs_events,
            exclusion_reason = if (length(reason_parts) > 0) paste(reason_parts, collapse = "; ") else ""
        )
    }
    interaction_diagnostics$level_statistics <- level_statistics
    interaction_diagnostics$supported_levels <- as.character(valid_levels)
    interaction_diagnostics$supported_level_count <- length(valid_levels)
    excluded_levels <- setdiff(subgroup_levels, valid_levels)
    if (length(excluded_levels) > 0) {
        interaction_diagnostics$excluded_level_names <- paste(excluded_levels, collapse = ", ")
    }
    for (level_name in names(level_statistics)) {
        level_statistics[[level_name]]$supported_for_interaction <- level_name %in% valid_levels
    }
    interaction_diagnostics$level_statistics <- level_statistics

    if (length(valid_levels) == 0) {
        interaction_diagnostics$model_status <- "no_supported_levels"
        interaction_diagnostics$interaction_test_status <- "not_testable_no_supported_levels"
        interaction_diagnostics$failure_reason <- "No subgroup levels met minimum sample/event requirements; no treatment model was fit"
        return(list(
            model = NULL,
            interaction_p = NA,
            formula_used = NA,
            interaction_diagnostics = interaction_diagnostics,
            filtered_data = NULL
        ))
    }
    filtered_data <- data[data[[subgroup_var_to_use]] %in% valid_levels, ]
    filtered_data[[subgroup_var_to_use]] <- factor(filtered_data[[subgroup_var_to_use]], levels = valid_levels)
    confounders_str <- if (is.null(confounders_to_use) || length(confounders_to_use) == 0) "" else paste(" + ", paste(confounders_to_use, collapse = " + "))

    # A single supported level has a valid within-level treatment contrast, but
    # there is no subgroup contrast left with which to test heterogeneity.
    if (length(valid_levels) == 1) {
        treatment_formula_str <- if (outcome_config$type == "survival") {
            paste0("Surv(", outcome_config$time_var, ", ", outcome_config$event_var, ") ~ treatment_group", confounders_str)
        } else {
            paste0(outcome_config$outcome_var, " ~ treatment_group", confounders_str)
        }
        model_error <- NULL
        model <- tryCatch(
            {
                if (outcome_config$type == "survival") {
                    coxph(as.formula(treatment_formula_str), data = filtered_data, model = TRUE)
                } else if (outcome_config$type == "binary") {
                    glm(as.formula(treatment_formula_str), data = filtered_data, family = binomial())
                } else {
                    lm(as.formula(treatment_formula_str), data = filtered_data)
                }
            },
            error = function(e) {
                model_error <<- conditionMessage(e)
                NULL
            }
        )
        if (is.null(model)) {
            interaction_diagnostics$model_status <- "model_failure"
            interaction_diagnostics$interaction_test_status <- "not_testable_single_supported_level"
            interaction_diagnostics$failure_reason <- "Single-supported-level treatment model fitting failed"
            interaction_diagnostics$model_error <- model_error %||% "Unknown model fitting error"
            return(list(
                model = NULL,
                interaction_p = NA,
                formula_used = treatment_formula_str,
                interaction_diagnostics = interaction_diagnostics,
                filtered_data = filtered_data
            ))
        }
        attr(model, "subgroup_event_var") <- resolve_subgroup_event_count_variable(filtered_data, outcome_config)
        interaction_diagnostics$model_status <- "single_supported_level_treatment_model"
        interaction_diagnostics$interaction_test_status <- "not_testable_single_supported_level"
        interaction_diagnostics$failure_reason <- "Interaction testing not possible: only one subgroup level met minimum sample/event requirements"
        return(list(
            model = model,
            interaction_p = NA,
            formula_used = treatment_formula_str,
            interaction_diagnostics = interaction_diagnostics,
            filtered_data = filtered_data
        ))
    }

    interaction_term <- paste0("treatment_group * ", subgroup_var_to_use)
    model_error <- NULL
    no_int_error <- NULL
    if (outcome_config$type == "survival") {
        formula_str <- paste0("Surv(", outcome_config$time_var, ", ", outcome_config$event_var, ") ~ ", interaction_term, confounders_str)
        model <- tryCatch(coxph(as.formula(formula_str), data = filtered_data, model = TRUE), error = function(e) { model_error <<- conditionMessage(e); NULL })
        no_int <- tryCatch(coxph(as.formula(paste0("Surv(", outcome_config$time_var, ", ", outcome_config$event_var, ") ~ ", "treatment_group + ", subgroup_var_to_use, confounders_str)), data = filtered_data, model = TRUE), error = function(e) { no_int_error <<- conditionMessage(e); NULL })
    } else if (outcome_config$type == "binary") {
        formula_str <- paste0(outcome_config$outcome_var, " ~ ", interaction_term, confounders_str)
        model <- tryCatch(glm(as.formula(formula_str), data = filtered_data, family = binomial()), error = function(e) { model_error <<- conditionMessage(e); NULL })
        no_int <- tryCatch(glm(as.formula(paste0(outcome_config$outcome_var, " ~ treatment_group + ", subgroup_var_to_use, confounders_str)), data = filtered_data, family = binomial()), error = function(e) { no_int_error <<- conditionMessage(e); NULL })
    } else {
        formula_str <- paste0(outcome_config$outcome_var, " ~ ", interaction_term, confounders_str)
        model <- tryCatch(lm(as.formula(formula_str), data = filtered_data), error = function(e) { model_error <<- conditionMessage(e); NULL })
        no_int <- tryCatch(lm(as.formula(paste0(outcome_config$outcome_var, " ~ treatment_group + ", subgroup_var_to_use, confounders_str)), data = filtered_data), error = function(e) { no_int_error <<- conditionMessage(e); NULL })
    }
    if (is.null(model)) {
        interaction_diagnostics$model_status <- "model_failure"
        interaction_diagnostics$interaction_test_status <- "model_failure"
        interaction_diagnostics$failure_reason <- "Interaction model fitting failed"
        interaction_diagnostics$model_error <- model_error %||% "Unknown model fitting error"
        return(list(model = NULL, interaction_p = NA, formula_used = formula_str, interaction_diagnostics = interaction_diagnostics, filtered_data = filtered_data))
    }
    interaction_diagnostics$model_status <- "interaction_model_fitted"
    attr(model, "subgroup_event_var") <- resolve_subgroup_event_count_variable(filtered_data, outcome_config)
    subgroup_levels <- levels(filtered_data[[subgroup_var_to_use]])
    if (length(subgroup_levels) == 2) {
        interaction_coef_name <- get_interaction_coefficient_name(model, "treatment_group", subgroup_var_to_use, subgroup_levels[2], filtered_data)
        if (!is.null(interaction_coef_name) && interaction_coef_name %in% rownames(summary(model)$coefficients)) {
            interaction_p <- if (outcome_config$type == "continuous") summary(model)$coefficients[interaction_coef_name, "Pr(>|t|)"] else summary(model)$coefficients[interaction_coef_name, "Pr(>|z|)"]
            interaction_diagnostics$interaction_test_status <- "tested"
        } else {
            interaction_p <- NA
            interaction_diagnostics$interaction_test_status <- "interaction_coefficient_not_found"
            interaction_diagnostics$failure_reason <- "Interaction coefficient was not found"
        }
    } else {
        if (is.null(no_int)) {
            interaction_diagnostics$interaction_test_status <- "reduced_model_failure"
            interaction_diagnostics$failure_reason <- "Interaction testing failed: reduced no-interaction model fitting failed"
            interaction_diagnostics$reduced_model_error <- no_int_error %||% "Unknown reduced-model fitting error"
        }
        interaction_test <- tryCatch(
            {
                if (outcome_config$type == "survival") anova(no_int, model) else if (outcome_config$type == "binary") anova(no_int, model, test = "Chisq") else anova(no_int, model)
            },
            error = function(e) NULL
        )
        if (!is.null(interaction_test) && nrow(interaction_test) >= 2) {
            if (outcome_config$type == "survival") {
                interaction_p <- interaction_test[[grep("Pr.*Chi", names(interaction_test), value = TRUE)[1]]][2]
            } else if (outcome_config$type == "binary") {
                interaction_p <- interaction_test$`Pr(>Chi)`[2]
            } else {
                interaction_p <- interaction_test$`Pr(>F)`[2]
            }
            interaction_diagnostics$interaction_test_status <- "tested"
        } else {
            interaction_p <- NA
            if (is.null(interaction_diagnostics$failure_reason)) {
                interaction_diagnostics$interaction_test_status <- "interaction_test_failure"
                interaction_diagnostics$failure_reason <- "Interaction testing failed"
            }
        }
    }
    list(model = model, interaction_p = interaction_p, formula_used = formula_str, interaction_diagnostics = interaction_diagnostics, filtered_data = filtered_data)
}

#' Calculate subgroup effects by level
#' @param model Fitted model
#' @param data Data used
#' @param subgroup_var_to_use Binned subgroup var name
#' @param outcome_type "survival" | "binary" | "continuous"
#' @param original_var_name Original variable name
#' @return Data frame of subgroup effects
calculate_subgroup_effects <- function(model, data, subgroup_var_to_use, outcome_type, original_var_name) {
    if (is.numeric(data[[subgroup_var_to_use]]) || is.integer(data[[subgroup_var_to_use]])) {
        treatment_coef <- get_treatment_coefficient_name(model, "treatment_group", data)
        if (is.null(treatment_coef)) {
            return(data.frame())
        }

        coefficient <- stats::coef(model)[treatment_coef]
        standard_error <- sqrt(stats::vcov(model)[treatment_coef, treatment_coef])
        if (outcome_type == "continuous") {
            treatment_effect <- coefficient
            ci_lower <- coefficient - 1.96 * standard_error
            ci_upper <- coefficient + 1.96 * standard_error
            p_value <- summary(model)$coefficients[treatment_coef, "Pr(>|t|)"]
        } else {
            treatment_effect <- exp(coefficient)
            ci_lower <- exp(coefficient - 1.96 * standard_error)
            ci_upper <- exp(coefficient + 1.96 * standard_error)
            p_value <- summary(model)$coefficients[treatment_coef, "Pr(>|z|)"]
        }

        outcome_config <- list(type = outcome_type)
        if (outcome_type == "survival") {
            outcome_config$event_var <- attr(model, "subgroup_event_var") %||% NULL
        } else if (outcome_type == "binary") {
            outcome_config$outcome_var <- attr(model, "subgroup_event_var") %||% NULL
        }
        event_counts <- count_subgroup_events_by_arm(data, outcome_config)
        reference_value <- attr(model, "continuous_reference_value")
        reference_unit <- attr(model, "continuous_reference_unit") %||% "units"
        reference_name <- attr(model, "continuous_reference_label") %||% "reference value"
        reference_label <- if (!is.null(reference_value) && is.finite(reference_value)) {
            formatted_reference <- if (abs(reference_value - round(reference_value)) < .Machine$double.eps^0.5) {
                sprintf("%.0f", reference_value)
            } else {
                sprintf("%.1f", reference_value)
            }
            if (identical(original_var_name, "age_at_diagnosis")) {
                sprintf("At age %s %s (%s)", formatted_reference, reference_unit, reference_name)
            } else {
                sprintf("At %s %s (%s)", formatted_reference, reference_unit, reference_name)
            }
        } else {
            "At the reference value"
        }

        return(data.frame(
            subgroup_variable = original_var_name,
            subgroup_level = reference_label,
            n_total = nrow(data),
            n_plaque = sum(data$treatment_group == "PBT", na.rm = TRUE),
            n_gksrs = sum(data$treatment_group == "GKSRS", na.rm = TRUE),
            events_plaque = event_counts$plaque_events,
            events_gksrs = event_counts$gksrs_events,
            treatment_effect = treatment_effect,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            p_value = p_value,
            stringsAsFactors = FALSE
        ))
    }

    subgroup_levels <- levels(data[[subgroup_var_to_use]])
    actual_levels <- unique(data[[subgroup_var_to_use]][!is.na(data[[subgroup_var_to_use]])])
    levels_to_process <- intersect(subgroup_levels, actual_levels)
    subgroup_effects <- data.frame()
    for (i in seq_along(levels_to_process)) {
        level <- levels_to_process[i]
        level_data <- data[data[[subgroup_var_to_use]] == level, ]
        n_total <- nrow(level_data)
        n_plaque <- sum(level_data$treatment_group == "PBT", na.rm = TRUE)
        n_gksrs <- sum(level_data$treatment_group == "GKSRS", na.rm = TRUE)
        events_plaque <- NA
        events_gksrs <- NA
        outcome_config <- list(type = outcome_type)
        if (outcome_type == "survival") {
            outcome_config$event_var <- attr(model, "subgroup_event_var") %||% NULL
            event_counts <- count_subgroup_events_by_arm(level_data, outcome_config)
            events_plaque <- event_counts$plaque_events
            events_gksrs <- event_counts$gksrs_events
        } else if (outcome_type == "binary") {
            outcome_config$outcome_var <- attr(model, "subgroup_event_var") %||% NULL
            event_counts <- count_subgroup_events_by_arm(level_data, outcome_config)
            events_plaque <- event_counts$plaque_events
            events_gksrs <- event_counts$gksrs_events
        }
        if (i == 1) {
            coef_idx <- get_treatment_coefficient_name(model, "treatment_group", data)
            if (!is.null(coef_idx)) {
                if (outcome_type == "continuous") {
                    effect_est <- coef(model)[coef_idx]
                    se_effect <- sqrt(vcov(model)[coef_idx, coef_idx])
                    ci_lower <- effect_est - 1.96 * se_effect
                    ci_upper <- effect_est + 1.96 * se_effect
                    p_val <- summary(model)$coefficients[coef_idx, "Pr(>|t|)"]
                } else {
                    effect_est <- exp(coef(model)[coef_idx])
                    se_effect <- sqrt(vcov(model)[coef_idx, coef_idx])
                    ci_lower <- exp(coef(model)[coef_idx] - 1.96 * se_effect)
                    ci_upper <- exp(coef(model)[coef_idx] + 1.96 * se_effect)
                    p_val <- summary(model)$coefficients[coef_idx, "Pr(>|z|)"]
                }
            } else {
                effect_est <- ci_lower <- ci_upper <- p_val <- NA
            }
        } else {
            main_coef_idx <- get_treatment_coefficient_name(model, "treatment_group", data)
            interaction_coef_idx <- get_interaction_coefficient_name(model, "treatment_group", subgroup_var_to_use, level, data)
            if (!is.null(main_coef_idx) && !is.null(interaction_coef_idx)) {
                combined_coef <- coef(model)[main_coef_idx] + coef(model)[interaction_coef_idx]
                var_main <- vcov(model)[main_coef_idx, main_coef_idx]
                var_int <- vcov(model)[interaction_coef_idx, interaction_coef_idx]
                cov_main_int <- vcov(model)[main_coef_idx, interaction_coef_idx]
                se_combined <- sqrt(var_main + var_int + 2 * cov_main_int)
                if (outcome_type == "continuous") {
                    effect_est <- combined_coef
                    ci_lower <- combined_coef - 1.96 * se_combined
                    ci_upper <- combined_coef + 1.96 * se_combined
                } else {
                    effect_est <- exp(combined_coef)
                    ci_lower <- exp(combined_coef - 1.96 * se_combined)
                    ci_upper <- exp(combined_coef + 1.96 * se_combined)
                }
                z_stat <- combined_coef / se_combined
                p_val <- 2 * (1 - pnorm(abs(z_stat)))
            } else {
                effect_est <- ci_lower <- ci_upper <- p_val <- NA
            }
        }
        subgroup_effects <- rbind(subgroup_effects, data.frame(
            subgroup_variable = original_var_name,
            subgroup_level = level,
            n_total = n_total,
            n_plaque = n_plaque,
            n_gksrs = n_gksrs,
            events_plaque = events_plaque,
            events_gksrs = events_gksrs,
            treatment_effect = effect_est,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            p_value = p_val,
            stringsAsFactors = FALSE
        ))
    }
    rownames(subgroup_effects) <- NULL # Normalize row names
    subgroup_effects
}
