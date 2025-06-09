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
    log_message(sprintf("Found %d datasets to analyze", length(datasets)))
    print(datasets)
    return(gsub("\\.rds$", "", datasets))
}

#' Handle rare categories in factor variables
#'
#' Collapses rare categories in specified factor variables into 'Other' if their count is below a threshold.
#'
#' @param data Data frame containing the variables.
#' @param vars Character vector of variable names to check.
#' @param threshold Minimum number of observations required to keep a category (default: 5).
#'
#' @return Data frame with rare categories collapsed to 'Other'.
#' @examples
#' handle_rare_categories(data, vars = c("sex", "location"), threshold = 5)
handle_rare_categories <- function(data, vars, threshold = 5) {
    if (VERBOSE) {
        log_message(sprintf("\nChecking for rare categories (threshold: %d):", threshold))
    }

    for (var in vars) {
        if (is.factor(data[[var]])) {
            log_message(sprintf("Checking for rare categories in %s", var))
            # Get category counts
            cat_counts <- table(data[[var]])
            rare_cats <- names(cat_counts)[cat_counts < threshold]
            valid_cats <- names(cat_counts)[cat_counts >= threshold]

            if (length(rare_cats) > 0) {
                # Check if collapsing would leave at least 2 valid levels
                # (1 from valid_cats + 1 from combined rare_cats)
                total_rare_count <- sum(cat_counts[rare_cats])
                would_have_valid_other <- total_rare_count >= threshold
                final_valid_levels <- length(valid_cats) + (if (would_have_valid_other) 1 else 0)
                
                if (final_valid_levels >= 2) {
                    if (VERBOSE) {
                        log_message(sprintf("\nCollapsing rare categories in %s:", var))
                        for (cat in rare_cats) {
                            log_message(sprintf("- %s (n=%d)", cat, cat_counts[cat]))
                        }
                    }

                    # Collapse rare categories into "Other"
                    data[[var]] <- fct_collapse(data[[var]],
                        Other = rare_cats
                    ) %>%
                        fct_relevel("Other", after = Inf)
                } else {
                    if (VERBOSE) {
                        log_message(sprintf("\nSkipping collapse for %s: would result in insufficient valid levels", var))
                        log_message(sprintf("Valid categories: %d, Rare total: %d (threshold: %d)", 
                                          length(valid_cats), total_rare_count, threshold))
                    }
                }
            }
        }
    }

    return(data)
}

#' Generate valid confounders
#'
#' Generates a list of valid confounders that have more than 1 level and at least THRESHOLD_RARITY counts per level.
#'
#' @param data Data frame.
#' @param confounders Character vector of confounder variable names.
#' @param threshold Minimum number of observations required to keep a category (default: THRESHOLD_RARITY).
#' @return Character vector of valid confounders.
#' @examples
#' generate_valid_confounders(data, confounders)
generate_valid_confounders <- function(data, confounders, threshold = THRESHOLD_RARITY) {
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
    if (VERBOSE && length(confounders) != length(valid_confounders)) {
        log_message("Removed confounders with only 1 level or <THRESHOLD_RARITY counts:")
        log_message(paste(setdiff(confounders, valid_confounders), collapse = ", "))
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
#' @examples
#' summarize_data(data)
summarize_data <- function(data) {
    if (VERBOSE) {
        log_message("\nData Summary:")
        log_message(sprintf("Total patients: %d", nrow(data)))
        
        log_message("\nTreatment Groups:")
        print(table(data$treatment_group))
        
        log_message("\nCohort Distribution:")
        print(table(data$cohort))
        
        log_message("\nTumor Characteristics:")
        log_message(sprintf("Location: %s", paste(unique(data$location), collapse=", ")))
        log_message(sprintf("Optic Nerve Involvement: %s", paste(unique(data$optic_nerve), collapse=", ")))
        log_message(sprintf("Initial Stage: %s", paste(unique(data$initial_overall_stage), collapse=", ")))
        
        log_message("\nGene Expression Profile:")
        print(table(data$biopsy1_gep))
        
        log_message("\nOutcomes:")
        log_message(sprintf("Recurrence: %d patients", sum(data$recurrence_event)))
        log_message(sprintf("Metastasis: %d patients", sum(data$mets_event)))
        log_message(sprintf("Death: %d patients", sum(data$death_event)))
        
        log_message("\nFollow-up:")
        log_message(sprintf("Median follow-up: %.1f years", median(data$follow_up_years, na.rm=TRUE)))
    }
} 