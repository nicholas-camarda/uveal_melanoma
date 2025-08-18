#!/usr/bin/env Rscript

# Perfect Separation Investigation Tool
# Purpose: Analyze event counts and perfect separation issues in real analysis data

source("scripts/utils/load_all.R")

# Load the real analysis data
cat("Loading analysis data...\n")
full_cohort <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
restricted_cohort <- readRDS("final_data/Analytic Dataset/uveal_melanoma_restricted_cohort.rds")
gksrs_only_cohort <- readRDS("final_data/Analytic Dataset/uveal_melanoma_gksrs_only_cohort.rds")

#' Analyze event counts for small-sample separation risks
#' @param data Data frame with outcome and group columns
#' @param cohort_name Character label for cohort (for messages)
#' @return Invisible NULL (prints a diagnostic summary)
analyze_event_counts <- function(data, cohort_name) {
    cat("\n=== ", cohort_name, " Cohort Analysis ===\n")

    # Overall sample size
    cat("Total sample size:", nrow(data), "\n")

    # Treatment group distribution
    treatment_counts <- table(data$treatment_group)
    cat("Treatment group distribution:\n")
    print(treatment_counts)

    # Event counts for different outcomes
    outcomes <- c("death_event", "pfs_event", "recurrence1")

    for (outcome in outcomes) {
        if (outcome %in% names(data)) {
            cat("\n--- ", outcome, " Analysis ---\n")

            # Overall event count
            event_count <- sum(data[[outcome]] == 1, na.rm = TRUE)
            total_valid <- sum(!is.na(data[[outcome]]))
            cat(
                "Total events:", event_count, "out of", total_valid, "(",
                round(100 * event_count / total_valid, 1), "%)\n"
            )

            # Events by treatment group
            event_by_treatment <- table(data$treatment_group, data[[outcome]], useNA = "ifany")
            cat("Events by treatment group:\n")
            print(event_by_treatment)

            # Check for perfect separation
            if (ncol(event_by_treatment) >= 2) {
                # Handle both numeric (0/1) and factor (Yes/No) outcomes
                if (is.numeric(data[[outcome]])) {
                    event_col <- "1"
                } else {
                    # For factors, assume "Yes" is the event
                    event_col <- "Yes"
                }

                if (event_col %in% colnames(event_by_treatment)) {
                    events_per_group <- event_by_treatment[, event_col, drop = FALSE]
                    zero_event_groups <- names(events_per_group[events_per_group == 0])

                    if (length(zero_event_groups) > 0) {
                        cat("⚠️  PERFECT SEPARATION DETECTED!\n")
                        cat("Groups with zero events:", paste(zero_event_groups, collapse = ", "), "\n")
                        cat("This will cause statistical problems in regression analysis.\n")
                    } else {
                        cat("✅ No perfect separation detected\n")
                    }
                } else {
                    cat("⚠️  Cannot determine event column for perfect separation check\n")
                }
            }
        }
    }

    # Survival time analysis
    if ("tt_death_years" %in% names(data)) {
        cat("\n--- Survival Time Analysis ---\n")
        death_times <- data$tt_death_years[data$death_event == 1]
        if (length(death_times) > 0) {
            cat("Death times range:", range(death_times), "\n")
            cat("Median survival time:", median(death_times), "years\n")
        }
    }
}

# Analyze each cohort
cohorts <- list(
    "Full" = full_cohort,
    "Restricted" = restricted_cohort,
    "GKSRS-Only" = gksrs_only_cohort
)

for (cohort_name in names(cohorts)) {
    analyze_event_counts(cohorts[[cohort_name]], cohort_name)
}

# Summary recommendations
cat("\n=== RECOMMENDATIONS ===\n")
cat("1. If perfect separation is detected in real data:\n")
cat("   - Consider exact logistic regression for small samples\n")
cat("   - Use Bayesian approaches\n")
cat("   - Report limitations transparently\n")
cat("   - Consider different time horizons\n")
cat("   - Stratified analysis by important covariates\n")
cat("\n2. If no perfect separation:\n")
cat("   - The test issue was likely due to small test subsets\n")
cat("   - Main analysis should be reliable\n")
cat("   - Continue with standard methods\n")

cat("\nAnalysis complete.\n")
