#' Objective 0: Data Processing
#'
#' Processes raw data to generate analytic datasets for downstream analysis.
#' - Creates analytic datasets for each cohort (full, restricted, GKSRS-only)
#' - Generates summary tables for each analytic dataset
#' - Establishes cohort-specific output directory structures
#'
#' This script is intended to be run as the first step in the analysis pipeline.
#' All subsequent objectives depend on the outputs generated here.
#'
#' @note Requires global variables RECREATE_ANALYTIC_DATASETS, OUTPUT_DIR, and confounders
#'
#' @return List containing all analysis results, model objects, and output file paths for each analysis type
run_objective_0 <- function() {
    if (RECREATE_ANALYTIC_DATASETS) {
        log_phase("DATA PREPROCESSING PHASE")
        data_start_time <- Sys.time()

        logger::log_info("RECREATE_ANALYTIC_DATASETS = TRUE: Creating new analytic datasets")

        # Create cohort-specific output structures BEFORE creating analytic datasets
        # This ensures summary tables are saved to the correct cohort-specific directories
        temp_output_dirs_by_cohort <- list()

        # Create the output directory structures for each expected cohort
        # We need to create these before calling create_analytic_dataset() so the directories are available
        expected_cohorts <- c("uveal_melanoma_full_cohort", "uveal_melanoma_restricted_cohort", "uveal_melanoma_gksrs_only_cohort")
        for (cohort_name in expected_cohorts) {
            # Determine cohort directory name
            cohort_dir_name <- case_when(
                grepl("full", cohort_name) ~ "uveal_full", # Full cohort: all patients regardless of eligibility
                grepl("restricted", cohort_name) ~ "uveal_restricted", # Restricted cohort: eligible for both treatments
                grepl("gksrs", cohort_name) ~ "gksrs", # GKSRS-only cohort: ineligible for plaque treatment
                TRUE ~ cohort_name
            )

            # Create the complete directory structure with subdirectories for each analysis type
            cohort_base_dir <- file.path(OUTPUT_DIR, cohort_dir_name)

            # Use simplified names as keys in temp_output_dirs_by_cohort
            simplified_cohort_name <- case_when(
                grepl("full", cohort_name) ~ "full_cohort",
                grepl("restricted", cohort_name) ~ "restricted_cohort",
                grepl("gksrs", cohort_name) ~ "gksrs_only_cohort",
                TRUE ~ cohort_name
            )

            temp_output_dirs_by_cohort[[simplified_cohort_name]] <- create_output_structure(cohort_base_dir)
        }

        # Create analytic datasets using the comprehensive function
        logger::log_info(formatted("Executing create_analytic_dataset: Creating analytic datasets with full processing pipeline", indent = 1))
        analytic_result <- create_analytic_dataset(output_dirs = temp_output_dirs_by_cohort)

        # Extract the results
        final_analytic_datasets <- analytic_result$analytic_data
        summary_tables <- analytic_result$summary_tables

        logger::log_info(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)",
            "DATA PREPROCESSING PHASE",
            as.numeric(difftime(Sys.time(), data_start_time, units = "secs"))
        ))
    } else {
        log_phase("DATA LOADING PHASE")
        logger::log_info("RECREATE_ANALYTIC_DATASETS = FALSE: Skipping analytic dataset creation")
        logger::log_info("Using existing datasets from final_data/Analytic Dataset/")
        logger::log_info("Set RECREATE_ANALYTIC_DATASETS = TRUE if you need to reprocess raw data")
    }
}
