# Validation Utilities
# Author: Nicholas Camarda
# Description: Canonical Objective 0 validation helpers

#' Return the canonical set of analytic cohort object names expected from
#' Objective 0 processing.
#'
#' @return Character vector of expected analytic cohort names.
get_expected_analytic_cohort_names <- function() {
    c(
        "uveal_melanoma_full_cohort",
        "uveal_melanoma_restricted_cohort",
        "uveal_melanoma_gksrs_only_cohort"
    )
}
