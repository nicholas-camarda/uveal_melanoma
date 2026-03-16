#' Main processing function to create analytic dataset
#'
#' Orchestrates the full data processing pipeline: loads, cleans, applies criteria, creates derived variables, summary tables, and saves outputs.
#'
#' @param output_dirs Optional list of output directories for each cohort.
#' @return A list with analytic_data, summary_tables, and removal_log
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
    criteria_result <- apply_criteria(factored_data)
    factored_filtered_data <- criteria_result$cohorts
    removal_log <- criteria_result$removal_log

    logger::log_info(sprintf("Created %d cohorts", length(factored_filtered_data)))
    for (cohort in names(factored_filtered_data)) {
        logger::log_info(formatted(sprintf("Cohort '%s': %d patients", cohort, nrow(factored_filtered_data[[cohort]]))))
    }

    # Save pre-collapsed snapshots for baseline characteristics (raw factor levels)
    logger::log_info("Saving pre-collapsed cohort snapshots for baseline characteristics")
    for (cohort_name in names(factored_filtered_data)) {
        saveRDS(factored_filtered_data[[cohort_name]], file.path(PROCESSED_DATA_DIR, paste0(cohort_name, "_derived_precollapse.rds")))
    }

    if (!is.null(output_dirs)) {
        logger::log_info("Documenting removed patients for each cohort (00_General)")

        cohort_filters <- list(
            full_cohort = function(df) df,
            restricted_cohort = function(df) df %>% dplyr::filter(consort_group == "eligible_both"),
            gksrs_only_cohort = function(df) df %>% dplyr::filter(consort_group == "gksrs_only")
        )

        for (cohort_key in names(output_dirs)) {
            cohort_dirs <- output_dirs[[cohort_key]]

            if (!"baseline_characteristics" %in% names(cohort_dirs)) {
                next
            }

            general_dir <- dirname(cohort_dirs$baseline_characteristics)
            if (!dir.exists(general_dir)) {
                dir.create(general_dir, recursive = TRUE, showWarnings = FALSE)
            }

            cohort_removal <- if (!is.null(removal_log) && nrow(removal_log) > 0) {
                filter_fn <- cohort_filters[[cohort_key]]
                matching_ids <- if (is.null(filter_fn)) {
                    removal_log$id
                } else {
                    filter_fn(removal_log)$id
                }

                removal_log %>%
                    dplyr::mutate(
                        id = as.character(id),
                        consort_group = as.character(consort_group),
                        treatment_group = as.character(treatment_group),
                        initial_overall_stage = as.character(initial_overall_stage),
                        would_have_entered_cohort = dplyr::case_when(
                            is.na(id) ~ NA,
                            TRUE ~ id %in% as.character(matching_ids)
                        )
                    ) %>%
                    dplyr::arrange(dplyr::desc(dplyr::coalesce(would_have_entered_cohort, FALSE)), id, removal_step)
            } else {
                dplyr::tibble()
            }

            if (nrow(cohort_removal) == 0) {
                cohort_removal <- dplyr::tibble(
                    id = NA_character_,
                    removal_reason = "No patients removed prior to analytic dataset creation for this cohort.",
                    removal_step = NA_character_,
                    consort_group = NA_character_,
                    treatment_group = NA_character_,
                    initial_overall_stage = NA_character_,
                    would_have_entered_cohort = NA
                )
            }

            output_path <- file.path(general_dir, "removed_patients_summary.tsv")
            readr::write_tsv(cohort_removal, output_path, na = "")

            logger::log_info(formatted(sprintf("Documented removals for %s in %s", cohort_key, output_path), indent = 1))
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

    logger::log_info("Exporting cohort summary statistics to JSON")
    export_cohort_summary(
        cohort_list = factored_filtered_data,
        removal_log = removal_log,
        output_path = file.path(PROCESSED_DATA_DIR, "cohort_summary_statistics.json"),
        output_dirs = output_dirs
    )

    logger::log_info("Validating cohorts after saving")
    # Validate after files are saved and that they meet all the criteria for analytic dataset
    generate_validation_report(factored_filtered_data)

    return(list(
        analytic_data = factored_filtered_data,
        summary_tables = summary_tables,
        removal_log = removal_log
    ))
}
