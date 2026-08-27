# Model Utilities
# Author: Nicholas Camarda
# Description: Model-related utility functions for the analysis pipeline

# =============================================================================
# MODEL UTILITY FUNCTIONS
# =============================================================================

#' Enforce unordered factors for modeling
#'
#' Converts any ordered factors to regular factors while preserving their level order.
#' This ensures that all factors use treatment contrasts (not polynomial contrasts) in models.
#' The function preserves the display order of levels but removes the 'ordered' attribute.
#'
#' @param data Data frame containing factor variables
#' @param verbose Whether to log which factors were converted (default: FALSE)
#' @return Data frame with all factors converted to unordered
enforce_unordered_factors <- function(data, verbose = FALSE) {
    if (verbose) {
        logger::log_info("Enforcing unordered factors for modeling")
    }

    if (!is.data.frame(data) || ncol(data) == 0) {
        if (verbose) {
            logger::log_info("✓ No columns available - skipping factor normalization")
        }
        return(data)
    }

    factor_vars <- names(data)[sapply(data, is.factor)]
    converted_count <- 0

    for (var in factor_vars) {
        if (is.ordered(data[[var]])) {
            # Get current levels to preserve order
            current_levels <- levels(data[[var]])

            # Convert to unordered factor while preserving level order
            data[[var]] <- factor(data[[var]],
                levels = current_levels,
                ordered = FALSE
            )

            converted_count <- converted_count + 1
            if (verbose) {
                logger::log_info(sprintf(
                    "  Converted ordered factor '%s' to unordered (levels: %s)",
                    var, paste(current_levels, collapse = ", ")
                ))
            }
        }
    }

    if (verbose && converted_count > 0) {
        logger::log_info(sprintf("✓ Converted %d ordered factors to unordered for modeling", converted_count))
    } else if (verbose) {
        logger::log_info("✓ No ordered factors found - all factors already unordered")
    }

    return(data)
}

#' Normalize Treatment Group Labels to Canonical Values
#'
#' Preserves the existing level semantics while remapping legacy aliases (for
#' example `Plaque`) onto the project-standard treatment labels used in models,
#' plots, and tables.
#'
#' @param values Vector of treatment labels.
#'
#' @return Vector with legacy aliases normalized to canonical labels.
normalize_treatment_group_values <- function(values) {
    normalized_values <- as.character(values)
    alias_map <- c(
        "Plaque" = "PBT",
        "Plaque Brachytherapy" = "PBT"
    )

    alias_hits <- normalized_values %in% names(alias_map)
    normalized_values[alias_hits] <- alias_map[normalized_values[alias_hits]]
    normalized_values
}

#' Normalize Treatment Group Columns Within a Data Frame
#'
#' Applies canonical treatment-group label normalization to the requested
#' columns when they are present, preserving factor order when the input is a
#' factor.
#'
#' @param data Data frame to normalize.
#' @param columns Character vector of candidate treatment columns.
#'
#' @return Data frame with normalized treatment labels.
normalize_treatment_group_data <- function(data, columns = "treatment_group") {
    if (!is.data.frame(data) || length(columns) == 0) {
        return(data)
    }

    for (column_name in columns[columns %in% names(data)]) {
        original_values <- data[[column_name]]
        normalized_values <- normalize_treatment_group_values(original_values)

        if (is.factor(original_values)) {
            normalized_levels <- unique(normalize_treatment_group_values(levels(original_values)))
            data[[column_name]] <- factor(
                normalized_values,
                levels = normalized_levels,
                ordered = is.ordered(original_values)
            )
        } else {
            data[[column_name]] <- normalized_values
        }
    }

    data
}

#' Get Stable Factor Levels Without Reordering Existing Factors
#'
#' Returns the current level order for factor inputs. For non-factor inputs,
#' returns a deterministic sorted set of distinct non-missing values so the
#' caller can coerce explicitly instead of relying on `as.factor()`.
#'
#' @param values Vector to inspect.
#'
#' @return Character vector of stable factor levels.
get_stable_factor_levels <- function(values) {
    if (is.factor(values)) {
        return(levels(values))
    }

    unique_values <- unique(stats::na.omit(as.character(values)))
    sort(unique_values)
}

#' Coerce a Vector to a Factor While Preserving Existing Level Order
#'
#' Reuses the current factor levels when `values` is already a factor. For
#' non-factor inputs, levels are created deterministically from the distinct
#' observed values.
#'
#' @param values Vector to coerce.
#' @param ordered Logical flag for ordered factors.
#'
#' @return A factor with stable level ordering.
coerce_to_factor_preserving_levels <- function(values, ordered = FALSE) {
    factor(
        as.character(values),
        levels = get_stable_factor_levels(values),
        ordered = ordered
    )
}



#' Get Variable Labels for Display
#'
#' Returns a named vector of human-readable labels for variables.
#' Uses centralized STANDARD_TABLE_LABELS from scripts/config/labels_display.R for consistency.
#'
#' @return Named character vector of variable labels
get_variable_labels <- function() {
    return(STANDARD_TABLE_LABELS)
}

#' Get Treatment Coefficient Name
#'
#' Finds the coefficient name for the treatment group variable in a regression model.
#' This function searches through the model coefficients to find the treatment group coefficient.
#'
#' @param model Fitted regression model (lm, glm, coxph, etc.)
#' @param treatment_var Character string of the treatment variable name
#' @param data Data frame used to fit the model
#' @return Character string of the coefficient name, or NULL if not found
get_treatment_coefficient_name <- function(model, treatment_var, data) {
    if (is.null(model) || inherits(model, "try-error")) {
        return(NULL)
    }

    # Get coefficient names from the model
    coef_names <- names(coef(model))
    if (is.null(coef_names)) {
        return(NULL)
    }

    # Look for treatment coefficient
    # Pattern: treatment_var + level (e.g., "treatment_groupGKSRS")
    treatment_pattern <- paste0("^", treatment_var, "[A-Z]")
    treatment_coef <- coef_names[grepl(treatment_pattern, coef_names)]

    if (length(treatment_coef) > 0) {
        return(treatment_coef[1]) # Return first match
    }

    # If not found with pattern, try exact match
    if (treatment_var %in% coef_names) {
        return(treatment_var)
    }

    return(NULL)
}

#' Get Interaction Coefficient Name
#'
#' Finds the coefficient name for an interaction term between treatment and subgroup variable.
#' This function searches through the model coefficients to find the interaction coefficient.
#'
#' @param model Fitted regression model (lm, glm, coxph, etc.)
#' @param treatment_var Character string of the treatment variable name
#' @param subgroup_var Character string of the subgroup variable name
#' @param subgroup_level Character string of the subgroup level
#' @param data Data frame used to fit the model
#' @return Character string of the coefficient name, or NULL if not found
get_interaction_coefficient_name <- function(model, treatment_var, subgroup_var, subgroup_level, data) {
    if (is.null(model) || inherits(model, "try-error")) {
        return(NULL)
    }

    # Get coefficient names from the model
    coef_names <- names(coef(model))
    if (is.null(coef_names)) {
        return(NULL)
    }

    # Look for interaction coefficient
    # Pattern: treatment_var + level:subgroup_var + level (e.g., "treatment_groupGKSRS:age_at_diagnosis")
    # or treatment_var + level:subgroup_var + level (e.g., "treatment_groupGKSRS:age_at_diagnosis_binned>=65")

    # First, try to find the treatment level
    treatment_pattern <- paste0("^", treatment_var, "[A-Z]")
    treatment_coef <- coef_names[grepl(treatment_pattern, coef_names)]

    if (length(treatment_coef) == 0) {
        return(NULL)
    }

    treatment_level <- treatment_coef[1]

    # CRITICAL FIX: Look for the specific interaction coefficient for this subgroup level
    # The pattern should be: treatment_level:subgroup_var + subgroup_level
    specific_interaction_pattern <- paste0(treatment_level, ":", subgroup_var, subgroup_level)
    specific_interaction_coef <- coef_names[coef_names == specific_interaction_pattern]

    if (length(specific_interaction_coef) > 0) {
        return(specific_interaction_coef[1])
    }

    # Fallback: Look for any interaction with the subgroup variable
    interaction_pattern <- paste0(treatment_level, ":", subgroup_var)
    interaction_coef <- coef_names[grepl(interaction_pattern, coef_names)]

    if (length(interaction_coef) > 0) {
        return(interaction_coef[1]) # Return first match
    }

    # If not found, try with subgroup level included
    if (!is.null(subgroup_level)) {
        # Try different patterns for the interaction
        patterns <- c(
            paste0(treatment_level, ":", subgroup_var, subgroup_level),
            paste0(treatment_level, ":", subgroup_var, ".*", subgroup_level),
            paste0(treatment_level, ":", subgroup_var, ".*", gsub("[^a-zA-Z0-9]", "", subgroup_level))
        )

        for (pattern in patterns) {
            interaction_coef <- coef_names[grepl(pattern, coef_names)]
            if (length(interaction_coef) > 0) {
                return(interaction_coef[1])
            }
        }
    }

    return(NULL)
}
