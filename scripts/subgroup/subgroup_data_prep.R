# Subgroup Data Preparation and Common Helpers

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
        bin_labels <- c(paste0("< ", cutoffs), paste0("\u2265 ", cutoffs))
        bins <- ifelse(values < cutoffs, bin_labels[1], bin_labels[2])
    } else {
        bin_labels <- character(length(cutoffs) + 1)
        bin_labels[1] <- paste0("\u2264 ", cutoffs[1])
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
                paste0("\u2264 ", T_STAGE_HEIGHT_CUTOFFS[1], " mm"),
                paste0(T_STAGE_HEIGHT_CUTOFFS[1] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[2], " mm"),
                paste0(T_STAGE_HEIGHT_CUTOFFS[2] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[3], " mm"),
                paste0(T_STAGE_HEIGHT_CUTOFFS[3] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[4], " mm"),
                paste0(T_STAGE_HEIGHT_CUTOFFS[4] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[5], " mm"),
                paste0("> ", T_STAGE_HEIGHT_CUTOFFS[5], " mm")
            ))
        } else {
            return(c(
                paste0("\u2264 ", T_STAGE_DIAMETER_CUTOFFS[1], " mm"),
                paste0(T_STAGE_DIAMETER_CUTOFFS[1] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[2], " mm"),
                paste0(T_STAGE_DIAMETER_CUTOFFS[2] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[3], " mm"),
                paste0(T_STAGE_DIAMETER_CUTOFFS[3] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[4], " mm"),
                paste0(T_STAGE_DIAMETER_CUTOFFS[4] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[5], " mm"),
                paste0(T_STAGE_DIAMETER_CUTOFFS[5] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[6], " mm"),
                paste0("> ", T_STAGE_DIAMETER_CUTOFFS[6], " mm")
            ))
        }
    } else if (var_name == "age_at_diagnosis") {
        return(c(paste0("< ", LEGACY_CUTOFFS$age_at_diagnosis), paste0("\u2265 ", LEGACY_CUTOFFS$age_at_diagnosis)))
    } else {
        return(NULL)
    }
}

#' Process subgroup data (binning & confounders)
#' @param data Data frame
#' @param subgroup_var Variable name
#' @param confounders Confounders to include
#' @param include_baseline_height Logical for height analysis
#' @return List with processed data and metadata
process_subgroup_data <- function(data, subgroup_var, confounders, include_baseline_height = FALSE) {
    other_map <- list()
    if (!subgroup_var %in% names(data)) stop(sprintf("Variable '%s' not found in data", subgroup_var))
    data <- data %>% dplyr::filter(!is.na(.data[[subgroup_var]]))
    if (nrow(data) == 0) stop(sprintf("No data remaining after removing missing values for '%s'", subgroup_var))
    confounders_to_use <- if (!is.null(confounders)) confounders[confounders != subgroup_var] else NULL
    if (include_baseline_height && !"initial_tumor_height" %in% confounders_to_use) confounders_to_use <- c(confounders_to_use, "initial_tumor_height")
    processed_data <- data
    was_continuous <- is.numeric(data[[subgroup_var]]) || is.integer(data[[subgroup_var]])
    is_categorical_factor <- is.factor(data[[subgroup_var]])
    cutoff_value <- NULL
    if (was_continuous) {
        # Check if a binned version already exists (e.g., age_at_diagnosis_binned)
        subgroup_var_binned <- paste0(subgroup_var, "_binned")
        if (subgroup_var_binned %in% names(data)) {
            # Use existing binned variable instead of creating a new one
            subgroup_var_to_use <- subgroup_var_binned
            cutoff_value <- NULL
            other_map <- list()
        } else {
            # Create new binned variable as before
            cutoff_val <- get_cutoff_value(subgroup_var, data, percentile_cut = 0.5)
            if (subgroup_var %in% c("initial_tumor_height", "initial_tumor_diameter") && USE_CLINICAL_BINNING_CONTINUOUS && length(cutoff_val) > 1) {
                processed_data[[subgroup_var_binned]] <- create_clinical_bins(data[[subgroup_var]], cutoff_val, subgroup_var)
                subgroup_var_to_use <- subgroup_var_binned
                cutoff_value <- cutoff_val
                other_map <- list()
            } else {
                processed_data[[subgroup_var_binned]] <- factor(
                    ifelse(data[[subgroup_var]] < cutoff_val, paste0("< ", round(cutoff_val, 1)), paste0("\u2265 ", round(cutoff_val, 1))),
                    levels = c(paste0("< ", round(cutoff_val, 1)), paste0("\u2265 ", round(cutoff_val, 1)))
                )
                subgroup_var_to_use <- subgroup_var_binned
                cutoff_value <- cutoff_val
            }
        }
    } else if (is_categorical_factor) {
        other_map <- list()
        subgroup_var_to_use <- subgroup_var
    } else {
        if (!is.factor(processed_data[[subgroup_var]])) processed_data[[subgroup_var]] <- as.factor(processed_data[[subgroup_var]])
        other_map <- list()
        subgroup_var_to_use <- subgroup_var
    }
    if (is.factor(processed_data[[subgroup_var_to_use]])) {
        level_counts <- table(processed_data[[subgroup_var_to_use]])
        if (sum(level_counts > 0) < 2) {
            warning(sprintf("Variable %s has insufficient valid levels (%d)", subgroup_var, sum(level_counts > 0)))
            return(list(data = NULL, subgroup_var_to_use = NULL, confounders_to_use = NULL, was_continuous = FALSE, cutoff_value = NA, error = "insufficient_levels"))
        }
    }
    processed_data <- enforce_unordered_factors(processed_data)
    list(data = processed_data, subgroup_var_to_use = subgroup_var_to_use, confounders_to_use = confounders_to_use, was_continuous = was_continuous, cutoff_value = cutoff_value, other_map = other_map)
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
    valid_levels <- c()
    level_statistics <- list()
    subgroup_levels <- levels(data[[subgroup_var_to_use]])
    for (level in subgroup_levels) {
        level_data <- data[data[[subgroup_var_to_use]] == level, ]
        n_plaque <- sum(level_data$treatment_group == "PBT", na.rm = TRUE)
        n_gksrs <- sum(level_data$treatment_group == "GKSRS", na.rm = TRUE)

        level_label <- as.character(level)
        plaque_events <- NA
        gksrs_events <- NA
        reason_parts <- c()
        if (outcome_config$type == "survival") {
            event_vars <- c("death_event", "mets_event", "pfs_event", "event")
            event_var <- NULL
            for (ev in event_vars) {
                if (ev %in% names(level_data)) {
                    event_var <- ev
                    break
                }
            }
            if (!is.null(event_var)) {
                plaque_events <- sum(level_data$treatment_group == "PBT" & level_data[[event_var]] == 1, na.rm = TRUE)
                gksrs_events <- sum(level_data$treatment_group == "GKSRS" & level_data[[event_var]] == 1, na.rm = TRUE)
            }
            sample_ok <- n_plaque >= 2 && n_gksrs >= 2
            events_ok <- !is.null(event_var) && !is.na(plaque_events) && !is.na(gksrs_events) && plaque_events >= 1 && gksrs_events >= 1
            if (!sample_ok) {
                reason_parts <- c(reason_parts, sprintf("Requires ≥2 patients per arm; observed PBT=%d, GKSRS=%d", n_plaque, n_gksrs))
            }
            if (!events_ok) {
                reason_parts <- c(reason_parts, sprintf("Requires ≥1 event per arm; observed PBT events=%s, GKSRS events=%s",
                    ifelse(is.na(plaque_events), "NA", plaque_events), ifelse(is.na(gksrs_events), "NA", gksrs_events)))
            }
            if (sample_ok && events_ok) {
                valid_levels <- c(valid_levels, level)
            }
        } else {
            if (outcome_config$type == "binary") {
                outcome_vars <- c("recurrence1", "mets_progression", "event", "outcome")
                found_outcome_var <- NULL
                for (ov in outcome_vars) {
                    if (ov %in% names(level_data)) {
                        found_outcome_var <- ov
                        break
                    }
                }
                if (!is.null(found_outcome_var)) {
                    outcome_var <- level_data[[found_outcome_var]]
                    if (is.factor(outcome_var)) {
                        ref_level <- levels(outcome_var)[1]
                        plaque_events <- sum(level_data$treatment_group == "PBT" & level_data[[found_outcome_var]] != ref_level, na.rm = TRUE)
                        gksrs_events <- sum(level_data$treatment_group == "GKSRS" & level_data[[found_outcome_var]] != ref_level, na.rm = TRUE)
                    } else {
                        plaque_events <- sum(level_data$treatment_group == "PBT" & (level_data[[found_outcome_var]] == 1 | level_data[[found_outcome_var]] == TRUE), na.rm = TRUE)
                        gksrs_events <- sum(level_data$treatment_group == "GKSRS" & (level_data[[found_outcome_var]] == 1 | level_data[[found_outcome_var]] == TRUE), na.rm = TRUE)
                    }
                }
            }
            sample_ok <- n_plaque >= 2 && n_gksrs >= 2
            events_ok <- TRUE
            if (!is.na(plaque_events) || !is.na(gksrs_events)) {
                events_ok <- (is.na(plaque_events) || plaque_events >= 1) && (is.na(gksrs_events) || gksrs_events >= 1)
                if (!events_ok) {
                    reason_parts <- c(reason_parts, sprintf("Low events; PBT events=%s, GKSRS events=%s",
                        ifelse(is.na(plaque_events), "NA", plaque_events), ifelse(is.na(gksrs_events), "NA", gksrs_events)))
                }
            }
            if (!sample_ok) {
                reason_parts <- c(reason_parts, sprintf("Requires ≥2 patients per arm; observed PBT=%d, GKSRS=%d", n_plaque, n_gksrs))
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
    excluded_levels <- setdiff(subgroup_levels, valid_levels)
    if (length(excluded_levels) > 0) {
        interaction_diagnostics$excluded_level_names <- paste(excluded_levels, collapse = ", ")
    }

    if (length(valid_levels) == 0) {
        interaction_diagnostics$failure_reason <- "No subgroup levels met minimum sample/event requirements"
        return(list(model = NULL, interaction_p = NA, formula_used = NA, interaction_diagnostics = interaction_diagnostics, filtered_data = NULL))
    }
    filtered_data <- data[data[[subgroup_var_to_use]] %in% valid_levels, ]
    filtered_data[[subgroup_var_to_use]] <- factor(filtered_data[[subgroup_var_to_use]], levels = valid_levels)
    confounders_str <- if (is.null(confounders_to_use) || length(confounders_to_use) == 0) "" else paste(" + ", paste(confounders_to_use, collapse = " + "))
    interaction_term <- paste0("treatment_group * ", subgroup_var_to_use)
    if (outcome_config$type == "survival") {
        formula_str <- paste0("Surv(", outcome_config$time_var, ", ", outcome_config$event_var, ") ~ ", interaction_term, confounders_str)
        model <- tryCatch(coxph(as.formula(formula_str), data = filtered_data, model = TRUE), error = function(e) NULL)
        no_int <- tryCatch(coxph(as.formula(paste0("Surv(", outcome_config$time_var, ", ", outcome_config$event_var, ") ~ ", "treatment_group + ", subgroup_var_to_use, confounders_str)), data = filtered_data, model = TRUE), error = function(e) NULL)
    } else if (outcome_config$type == "binary") {
        formula_str <- paste0(outcome_config$outcome_var, " ~ ", interaction_term, confounders_str)
        model <- tryCatch(glm(as.formula(formula_str), data = filtered_data, family = binomial()), error = function(e) NULL)
        no_int <- tryCatch(glm(as.formula(paste0(outcome_config$outcome_var, " ~ treatment_group + ", subgroup_var_to_use, confounders_str)), data = filtered_data, family = binomial()), error = function(e) NULL)
    } else {
        formula_str <- paste0(outcome_config$outcome_var, " ~ ", interaction_term, confounders_str)
        model <- tryCatch(lm(as.formula(formula_str), data = filtered_data), error = function(e) NULL)
        no_int <- tryCatch(lm(as.formula(paste0(outcome_config$outcome_var, " ~ treatment_group + ", subgroup_var_to_use, confounders_str)), data = filtered_data), error = function(e) NULL)
    }
    if (is.null(model)) {
        interaction_diagnostics$failure_reason <- "Model fitting failed"
        return(list(model = NULL, interaction_p = NA, formula_used = NA, interaction_diagnostics = interaction_diagnostics, filtered_data = filtered_data))
    }
    subgroup_levels <- levels(filtered_data[[subgroup_var_to_use]])
    if (length(subgroup_levels) == 2) {
        interaction_coef_name <- get_interaction_coefficient_name(model, "treatment_group", subgroup_var_to_use, subgroup_levels[2], filtered_data)
        if (!is.null(interaction_coef_name) && interaction_coef_name %in% rownames(summary(model)$coefficients)) {
            interaction_p <- if (outcome_config$type == "continuous") summary(model)$coefficients[interaction_coef_name, "Pr(>|t|)"] else summary(model)$coefficients[interaction_coef_name, "Pr(>|z|)"]
        } else {
            interaction_p <- NA
        }
    } else {
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
        } else {
            interaction_p <- NA
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
        if (outcome_type == "survival") {
            plaque_data <- level_data %>% dplyr::filter(treatment_group == "PBT")
            gksrs_data <- level_data %>% dplyr::filter(treatment_group == "GKSRS")
            event_vars <- c("death_event", "mets_event", "pfs_event", "event")
            found_event_var <- NULL
            for (ev in event_vars) {
                if (ev %in% names(level_data)) {
                    found_event_var <- ev
                    break
                }
            }
            if (!is.null(found_event_var)) {
                events_plaque <- sum(plaque_data[[found_event_var]] == 1, na.rm = TRUE)
                events_gksrs <- sum(gksrs_data[[found_event_var]] == 1, na.rm = TRUE)
            }
        } else if (outcome_type == "binary") {
            plaque_data <- level_data %>% dplyr::filter(treatment_group == "PBT")
            gksrs_data <- level_data %>% dplyr::filter(treatment_group == "GKSRS")
            outcome_vars <- c("recurrence1", "mets_progression", "outcome")
            found_outcome_var <- NULL
            for (ov in outcome_vars) {
                if (ov %in% names(level_data)) {
                    found_outcome_var <- ov
                    break
                }
            }
            if (!is.null(found_outcome_var)) {
                outcome_var <- level_data[[found_outcome_var]]
                if (is.factor(outcome_var)) {
                    events_plaque <- sum(plaque_data[[found_outcome_var]] != levels(outcome_var)[1], na.rm = TRUE)
                    events_gksrs <- sum(gksrs_data[[found_outcome_var]] != levels(outcome_var)[1], na.rm = TRUE)
                } else {
                    events_plaque <- sum(plaque_data[[found_outcome_var]] == 1 | plaque_data[[found_outcome_var]] == TRUE, na.rm = TRUE)
                    events_gksrs <- sum(gksrs_data[[found_outcome_var]] == 1 | gksrs_data[[found_outcome_var]] == TRUE, na.rm = TRUE)
                }
            }
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
