#' Main processing function to create analytic dataset
#'
#' Orchestrates the full data processing pipeline: loads, cleans, applies criteria, creates derived variables, summary tables, and saves outputs.
#'
#' @param output_dirs Optional list of output directories for each cohort.
#' @return A list with analytic_data, summary_tables, other_map, validated_confounders_by_cohort
create_analytic_dataset <- function(output_dirs = NULL) {
    logger::log_info("Starting data processing pipeline")

    logger::log_info("Loading and cleaning raw data")
    raw_data <- load_and_clean_data(INPUT_FILENAME)
    logger::log_info(sprintf("Loaded %d rows of raw data", nrow(raw_data)))

    logger::log_info("Creating derived variables")
    derived_data <- create_derived_variables(raw_data)

    logger::log_info("Preparing factor levels")
    factored_result <- prepare_factor_levels(derived_data)
    factored_data <- factored_result$data

    logger::log_info("Applying inclusion/exclusion criteria")
    factored_filtered_data <- apply_criteria(factored_data)
    logger::log_info(sprintf("Created %d cohorts", length(factored_filtered_data)))
    for (cohort in names(factored_filtered_data)) {
        logger::log_info(formatted(sprintf("Cohort '%s': %d patients", cohort, nrow(factored_filtered_data[[cohort]]))))
    }

    logger::log_info("Collapsing rare categories")
    other_map <- list()
    for (cohort_name in names(factored_filtered_data)) {
        logger::log_info(formatted(sprintf("Processing rare categories for cohort: %s", cohort_name)))
        all_vars_to_process <- names(factored_filtered_data[[cohort_name]])
        factor_vars <- all_vars_to_process[sapply(factored_filtered_data[[cohort_name]][all_vars_to_process], is.factor)]
        if (length(factor_vars) > 0) {
            collapse_result <- handle_rare_categories(factored_filtered_data[[cohort_name]], factor_vars)
            factored_filtered_data[[cohort_name]] <- collapse_result$data
            other_map[[cohort_name]] <- collapse_result$other_map
            if (length(other_map[[cohort_name]]) > 0) {
                logger::log_info(formatted(sprintf("Categories collapsed into 'Other' for cohort %s:", cohort_name)))
                for (var_name in names(other_map[[cohort_name]])) {
                    collapsed_cats <- other_map[[cohort_name]][[var_name]]
                    logger::log_info(formatted(sprintf("  %s: %s", var_name, paste(collapsed_cats, collapse = ", ")), indent = 1))
                }
            } else {
                logger::log_info(formatted(sprintf("No categories collapsed for cohort %s", cohort_name)))
            }
        } else {
            other_map[[cohort_name]] <- list()
            logger::log_info(formatted(sprintf("No factor variables to process for cohort %s", cohort_name)))
        }
    }

    logger::log_info("Creating summary tables")
    summary_tables <- create_summary_tables(factored_filtered_data, output_dirs)

    logger::log_info("Saving processed data")
    for (cohort_name in names(factored_filtered_data)) {
        logger::log_info(formatted(sprintf("Saving cohort: %s", cohort_name)))
        write_xlsx(factored_filtered_data[[cohort_name]], file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".xlsx")))
        saveRDS(factored_filtered_data[[cohort_name]], file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".rds")))
    }

    logger::log_info("Validating confounders for each cohort")
    validated_confounders_by_cohort <- list()
    for (cohort_name in names(factored_filtered_data)) {
        logger::log_info(formatted(sprintf("Validating confounders for cohort: %s", cohort_name)))
        if (exists("confounders") && !is.null(confounders) && length(confounders) > 0) {
            valid_confounders <- generate_valid_confounders(factored_filtered_data[[cohort_name]], confounders)
            if (length(valid_confounders) != length(confounders)) {
                logger::log_warn(sprintf(
                    "Removed %d invalid confounders for cohort %s: %s",
                    length(confounders) - length(valid_confounders), cohort_name,
                    paste(setdiff(confounders, valid_confounders), collapse = ", ")
                ))
            }
            validated_confounders_by_cohort[[cohort_name]] <- valid_confounders
            logger::log_info(formatted(sprintf("Validated confounders for cohort %s: %s", cohort_name, paste(valid_confounders, collapse = ", "))))
        } else {
            validated_confounders_by_cohort[[cohort_name]] <- character(0)
            logger::log_info(formatted(sprintf("No confounders to validate for cohort %s", cohort_name)))
        }
    }

    saveRDS(validated_confounders_by_cohort, file.path(PROCESSED_DATA_DIR, "validated_confounders_by_cohort.rds"))
    logger::log_info("Saved validated confounders for all cohorts")

    saveRDS(other_map, file.path(PROCESSED_DATA_DIR, "other_map.rds"))
    logger::log_info("Saved combined other_map information for all cohorts")

    generate_validation_report(factored_filtered_data)

    return(list(
        analytic_data = factored_filtered_data,
        summary_tables = summary_tables,
        other_map = other_map,
        validated_confounders_by_cohort = validated_confounders_by_cohort
    ))
}
