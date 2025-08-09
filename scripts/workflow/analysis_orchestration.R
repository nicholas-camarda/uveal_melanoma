########################################################
############### MAIN EXECUTION FUNCTIONS ###############
########################################################

#' Run analysis for a single dataset and selected objectives
#'
#' This function orchestrates the statistical analysis for a given dataset.
#' It checks for required dependencies, sets up output directories, loads the analytic dataset,
#' and runs the specified analysis objectives (0: Data Processing, 1: Primary Outcomes,
#' 2: Safety/Toxicity, 3: Repeat Radiation Efficacy, 4: GEP Validation).
#'
#' @param dataset_name Character. The name of the dataset to analyze (e.g., "uveal_melanoma_full_cohort").
#' @param objectives_to_run Integer vector. Objectives to run (default: c(0, 1, 2, 3, 4)).
#'
#' @return A named list containing the results of each objective that was run.
#' @export
run_my_analysis <- function(dataset_name, objectives_to_run = c(0, 1, 2, 3, 4)) {
    analysis_start_time <- Sys.time()

    # Clean dataset name for display
    display_name <- tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))
    log_section_start("STATISTICAL ANALYSIS", display_name)

    # Check dependencies before running analysis objectives
    if (any(objectives_to_run %in% c(1, 2, 3, 4))) {
        # Check if required files exist for analysis objectives
        required_files <- c(
            file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds")),
            file.path(PROCESSED_DATA_DIR, "other_map.rds")
        )

        missing_files <- required_files[!file.exists(required_files)]

        if (length(missing_files) > 0 && !(0 %in% objectives_to_run)) {
            stop(sprintf(
                "DEPENDENCY ERROR: Required files missing for dataset '%s': %s\nRun Objective 0 first or include it in objectives_to_run.",
                dataset_name, paste(basename(missing_files), collapse = ", ")
            ))
        }

        if (length(missing_files) > 0 && (0 %in% objectives_to_run)) {
            log_enhanced(sprintf(
                "WARNING: Required files missing for dataset '%s': %s\nObjective 0 will create these files.",
                dataset_name, paste(basename(missing_files), collapse = ", ")
            ), level = "WARN")
        }
    }

    # Set up cohort outputs using centralized function
    cohort_outputs <- setup_cohort_outputs(dataset_name)

    prefix <<- cohort_outputs$prefix
    cohort_base_dir <<- cohort_outputs$cohort_base_dir
    output_dirs <<- cohort_outputs$output_dirs

    # CRITICAL: Validate naming consistency to prevent bugs
    if (!validate_naming_consistency(dataset_name, prefix, basename(cohort_base_dir))) {
        stop(sprintf("NAMING VALIDATION FAILED for dataset: %s", dataset_name))
    }

    log_enhanced(sprintf("All outputs organized by objectives under: %s", cohort_base_dir), level = "INFO", indent = 1)

    # Load analytic dataset
    log_function("readRDS", paste("Loading analytic dataset:", dataset_name))
    data <- readRDS(file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds")))
    log_enhanced(sprintf("Successfully loaded %d patients for analysis", nrow(data)), level = "INFO", indent = 1)

    # Load cohort-specific other_map information using unified function
    other_map <- get_cohort_specific_other_map(dataset_name, PROCESSED_DATA_DIR)

    # Load pre-validated confounders for this cohort
    validated_confounders_file <- file.path(PROCESSED_DATA_DIR, "validated_confounders_by_cohort.rds")
    if (file.exists(validated_confounders_file)) {
        validated_confounders_by_cohort <- readRDS(validated_confounders_file)
        cohort_confounders <- validated_confounders_by_cohort[[dataset_name]]
        if (is.null(cohort_confounders)) {
            log_enhanced(sprintf("No validated confounders found for cohort %s, using original confounders", dataset_name), level = "WARN")
            cohort_confounders <- confounders
        } else {
            log_enhanced(sprintf(
                "Loaded %d validated confounders for cohort %s: %s",
                length(cohort_confounders), dataset_name,
                paste(cohort_confounders, collapse = ", ")
            ), level = "INFO")
        }
    } else {
        log_enhanced("No validated confounders file found, using original confounders", level = "WARN")
        cohort_confounders <- confounders
    }

    # Run selected objectives
    results <- list()

    if (0 %in% objectives_to_run) {
        log_enhanced("Running Objective 0: Data Processing", level = "INFO")
        # Objective 0 uses global variables, so we don't need to pass any arguments
        results$objective_0 <- run_objective_0()
    }

    if (1 %in% objectives_to_run) {
        log_enhanced("Running Objective 1: Primary Outcomes", level = "INFO")
        results$objective_1 <- run_objective_1(data, dataset_name, output_dirs, prefix, other_map, confounders = cohort_confounders)
    }

    if (2 %in% objectives_to_run) {
        log_enhanced("Running Objective 2: Safety/Toxicity", level = "INFO")
        results$objective_2 <- run_objective_2(data, dataset_name, output_dirs, prefix, other_map, confounders = cohort_confounders)
    }

    if (3 %in% objectives_to_run) {
        log_enhanced("Running Objective 3: Repeat Radiation Efficacy", level = "INFO")
        results$objective_3 <- run_objective_3(data, dataset_name, output_dirs, prefix, other_map, confounders = cohort_confounders)
    }

    if (4 %in% objectives_to_run) {
        log_enhanced("Running Objective 4: GEP Validation", level = "INFO")
        results$objective_4 <- run_objective_4(data, dataset_name, output_dirs, prefix, other_map, confounders = cohort_confounders)
    }

    log_section_complete("STATISTICAL ANALYSIS", analysis_start_time)

    return(results)
}

#' Run a Specific Objective for a Given Dataset
#'
#' This function executes a single specified objective for a given dataset.
#' It is primarily intended for testing or targeted analysis runs.
#'
#' @param dataset_name Character string specifying the dataset to analyze.
#' @param objective_number Integer or character indicating which objective to run.
#'
#' @return The results object returned by \code{run_my_analysis()} for the specified objective.
#' @export
run_specific_objective <- function(dataset_name, objective_number) {
    log_enhanced(sprintf("Running only Objective %d for dataset: %s", objective_number, dataset_name), level = "INFO")

    # Check dependencies for analysis objectives (1-4)
    if (objective_number %in% c(1, 2, 3, 4)) {
        required_files <- c(
            file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds")),
            file.path(PROCESSED_DATA_DIR, "other_map.rds")
        )

        missing_files <- required_files[!file.exists(required_files)]

        if (length(missing_files) > 0) {
            stop(sprintf(
                "DEPENDENCY ERROR: Required files missing for dataset '%s': %s\nRun Objective 0 first to create these files.",
                dataset_name, paste(basename(missing_files), collapse = ", ")
            ))
        }
    }

    results <- run_my_analysis(dataset_name, objectives_to_run = objective_number)
    return(results)
}

#' Merge baseline tables from all cohorts
#' This function merges the baseline tables from all cohorts.
#'
#' @return None
#' @export
merge_baseline_tables <- function() {
    # Merge baseline tables from all cohorts
    log_enhanced("Merging baseline tables from all cohorts", level = "INFO")
    log_enhanced("=== STARTING TABLE MERGING: Full and Restricted Cohorts ===", level = "INFO")

    # Create merged tables directory
    if (!dir.exists(MERGED_TABLES_DIR)) {
        dir.create(MERGED_TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
    }

    log_enhanced(sprintf("Merging tables will be saved to: %s", MERGED_TABLES_DIR), level = "INFO")

    # Load both datasets for merging
    full_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
    restricted_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_restricted_cohort.rds"))

    # Create merged baseline characteristics table using the correct function
    merge_cohort_tables(full_data, restricted_data, MERGED_TABLES_DIR)
    log_enhanced("=== COMPLETED TABLE MERGING ===", level = "INFO")
    log_enhanced(sprintf("Merged baseline characteristics table saved to: %s", MERGED_TABLES_DIR), level = "INFO")
    log_enhanced("Files created: merged_baseline_characteristics.xlsx and merged_baseline_characteristics.html", level = "INFO")
}

#' Main execution and merging of baseline tables
#' This function orchestrates the analysis for all datasets and all objectives
#' It runs the analysis for each dataset and merges the baseline tables from all cohorts.
#'
#' @return None
#' @export
main_execution <- function() {
    main_start_time <- Sys.time()
    log_section_start("MAIN EXECUTION PHASE")

    # Define datasets to analyze
    # this should be generated from the list_available_datasets function and named appropriately so that run_my_analysis can be called with the correct dataset name
    datasets_to_analyze_temp <- tools::file_path_sans_ext(list_available_datasets())
    datasets_to_analyze <- datasets_to_analyze_temp[!str_detect(datasets_to_analyze_temp, "other_map")]

    # Run analysis for each dataset
    for (i in seq_along(datasets_to_analyze)) {
        dataset_name <- datasets_to_analyze[i]
        log_enhanced(sprintf(">>> Dataset %d/%d: %s", i, length(datasets_to_analyze), dataset_name), level = "PROGRESS")

        tryCatch(
            {
                results <- run_my_analysis(dataset_name)
                log_enhanced(sprintf(">>> Dataset %d/%d completed: %s", i, length(datasets_to_analyze), dataset_name), level = "PROGRESS")
            },
            error = function(e) {
                log_enhanced(sprintf("ERROR in dataset %s: %s", dataset_name, e$message), level = "ERROR")
            }
        )
    }

    # Merge baseline tables from all cohorts
    merge_baseline_tables()

    log_enhanced("===  ===", level = "INFO")
    log_enhanced(">>> ALL ANALYSES COMPLETED SUCCESSFULLY!", level = "SUCCESS")
    log_enhanced(sprintf(">>> Total execution time: %.1f minutes", as.numeric(difftime(Sys.time(), main_start_time, units = "mins"))), level = "SUCCESS")
    log_enhanced(sprintf(">>> Datasets analyzed: %d", length(datasets_to_analyze)), level = "SUCCESS")
    log_enhanced("Check the logs above for detailed progress and any warnings.", level = "INFO")
    log_enhanced("Each cohort has its own complete set of analyses for easy comparison!", level = "INFO")

    log_section_complete("MAIN EXECUTION PHASE", main_start_time)
}
