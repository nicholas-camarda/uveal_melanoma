#' Main processing function to create analytic dataset
#'
#' Orchestrates the full data processing pipeline: loads, cleans, applies criteria, creates derived variables, summary tables, and saves outputs.
#'
#' @param output_dirs Optional list of output directories for each cohort.
#' @return A list with analytic_data, summary_tables, other_map, validated_confounders_by_cohort
create_analytic_dataset <- function(output_dirs = NULL) {
    log_enhanced("Starting data processing pipeline", level = "INFO")

    log_enhanced("Loading and cleaning raw data", level = "INFO")
    raw_data <- load_and_clean_data(INPUT_FILENAME)
    log_enhanced(sprintf("Loaded %d rows of raw data", nrow(raw_data)), level = "INFO")

    log_enhanced("Creating derived variables", level = "INFO")
    derived_data <- create_derived_variables(raw_data)

    log_enhanced("Preparing factor levels", level = "INFO")
    factored_result <- prepare_factor_levels(derived_data)
    factored_data <- factored_result$data

    log_enhanced("Applying inclusion/exclusion criteria", level = "INFO")
    factored_filtered_data <- apply_criteria(factored_data)
    log_enhanced(sprintf("Created %d cohorts", length(factored_filtered_data)), level = "INFO")
    for (cohort in names(factored_filtered_data)) {
        log_enhanced(sprintf("Cohort '%s': %d patients", cohort, nrow(factored_filtered_data[[cohort]])), level = "INFO")
    }

    log_enhanced("Collapsing rare categories", level = "INFO")
    other_map <- list()
    for (cohort_name in names(factored_filtered_data)) {
        log_enhanced(sprintf("Processing rare categories for cohort: %s", cohort_name), level = "INFO")
        all_vars_to_process <- names(factored_filtered_data[[cohort_name]])
        factor_vars <- all_vars_to_process[sapply(factored_filtered_data[[cohort_name]][all_vars_to_process], is.factor)]
        if (length(factor_vars) > 0) {
            collapse_result <- handle_rare_categories(factored_filtered_data[[cohort_name]], factor_vars)
            factored_filtered_data[[cohort_name]] <- collapse_result$data
            other_map[[cohort_name]] <- collapse_result$other_map
            if (length(other_map[[cohort_name]]) > 0) {
                log_enhanced(sprintf("Categories collapsed into 'Other' for cohort %s:", cohort_name), level = "INFO")
                for (var_name in names(other_map[[cohort_name]])) {
                    collapsed_cats <- other_map[[cohort_name]][[var_name]]
                    log_enhanced(sprintf("  %s: %s", var_name, paste(collapsed_cats, collapse = ", ")), level = "INFO")
                }
            } else {
                log_enhanced(sprintf("No categories collapsed for cohort %s", cohort_name), level = "INFO")
            }
        } else {
            other_map[[cohort_name]] <- list()
            log_enhanced(sprintf("No factor variables to process for cohort %s", cohort_name), level = "INFO")
        }
    }

    log_enhanced("Creating summary tables", level = "INFO")
    summary_tables <- create_summary_tables(factored_filtered_data, output_dirs)

    log_enhanced("Saving processed data", level = "INFO")
    for (cohort_name in names(factored_filtered_data)) {
        log_enhanced(sprintf("Saving cohort: %s", cohort_name), level = "INFO")
        write_xlsx(factored_filtered_data[[cohort_name]], file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".xlsx")))
        saveRDS(factored_filtered_data[[cohort_name]], file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".rds")))
    }

    log_enhanced("Validating confounders for each cohort", level = "INFO")
    validated_confounders_by_cohort <- list()
    for (cohort_name in names(factored_filtered_data)) {
        log_enhanced(sprintf("Validating confounders for cohort: %s", cohort_name), level = "INFO")
        if (exists("confounders") && !is.null(confounders) && length(confounders) > 0) {
            valid_confounders <- generate_valid_confounders(factored_filtered_data[[cohort_name]], confounders)
            if (length(valid_confounders) != length(confounders)) {
                log_enhanced(sprintf("Removed %d invalid confounders for cohort %s: %s", 
                                   length(confounders) - length(valid_confounders), cohort_name,
                                   paste(setdiff(confounders, valid_confounders), collapse = ", ")), level = "WARN")
            }
            validated_confounders_by_cohort[[cohort_name]] <- valid_confounders
            log_enhanced(sprintf("Validated confounders for cohort %s: %s", cohort_name, paste(valid_confounders, collapse = ", ")), level = "INFO")
        } else {
            validated_confounders_by_cohort[[cohort_name]] <- character(0)
            log_enhanced(sprintf("No confounders to validate for cohort %s", cohort_name), level = "INFO")
        }
    }

    saveRDS(validated_confounders_by_cohort, file.path(PROCESSED_DATA_DIR, "validated_confounders_by_cohort.rds"))
    log_enhanced("Saved validated confounders for all cohorts", level = "INFO")

    saveRDS(other_map, file.path(PROCESSED_DATA_DIR, "other_map.rds"))
    log_enhanced("Saved combined other_map information for all cohorts", level = "INFO")

    generate_validation_report(factored_filtered_data)

    return(list(
        analytic_data = factored_filtered_data,
        summary_tables = summary_tables,
        other_map = other_map,
        validated_confounders_by_cohort = validated_confounders_by_cohort
    ))
}
