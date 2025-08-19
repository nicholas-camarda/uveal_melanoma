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
    log_phase(paste("STATISTICAL ANALYSIS", display_name, sep = " - "))

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
            logger::log_warn(formatted(sprintf(
                "WARNING: Required files missing for dataset '%s': %s\nObjective 0 will create these files.",
                dataset_name, paste(basename(missing_files), collapse = ", ")
            )))
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

    logger::log_info(formatted(sprintf("All outputs organized by objectives under: %s", cohort_base_dir), indent = 1))

    # Load analytic dataset
    logger::log_info(formatted(paste("Executing readRDS:", paste("Loading analytic dataset:", dataset_name)), indent = 1))
    data <- readRDS(file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds")))
    logger::log_info(formatted(sprintf("Successfully loaded %d patients for analysis", nrow(data)), indent = 1))

    # Set initial log context
    set_log_context(cohort = dataset_name, objective = NULL, subobjective = NULL)

    # Track errors for this dataset
    errors_this_dataset <- FALSE

    # If Objective 0 is included, run it first so that dependent artifacts (e.g., other_map.rds) exist
    results <- list()
    if (0 %in% objectives_to_run) {
        with_log_context(cohort = dataset_name, objective = "objective_0_data_processing", subobjective = NULL, expr = {
            logger::log_info("Running Objective 0: Data Processing")
            results$objective_0 <- tryCatch(run_objective_0(), error = function(e) {
                errors_this_dataset <<- TRUE
                logger::log_error(formatted(sprintf("ERROR in Objective 0: %s", e$message)))
                NULL
            })
        })
        # Reload analytic dataset and other_map after processing
        data <- readRDS(file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds")))
        logger::log_info(formatted(sprintf("Successfully reloaded %d patients after Objective 0", nrow(data)), indent = 1))
        other_map <- get_cohort_specific_other_map(dataset_name, PROCESSED_DATA_DIR)
    } else {
        # Load cohort-specific other_map information using unified function
        other_map <- get_cohort_specific_other_map(dataset_name, PROCESSED_DATA_DIR)
    }

    # Use configured confounders directly (do not load/save validated_confounders_by_cohort)
    cohort_confounders <- confounders

    # Run selected objectives (excluding 0 which may have been run above) with error tracking
    if (1 %in% objectives_to_run) {
        with_log_context(cohort = dataset_name, objective = "objective_1_primary_outcomes", subobjective = NULL, expr = {
            logger::log_info("Running Objective 1: Primary Outcomes")
            results$objective_1 <- tryCatch(run_objective_1(data, dataset_name, output_dirs, prefix, other_map, confounders = cohort_confounders), error = function(e) {
                errors_this_dataset <<- TRUE
                logger::log_error(formatted(sprintf("ERROR in Objective 1: %s", e$message)))
                NULL
            })
        })
    }

    if (2 %in% objectives_to_run) {
        with_log_context(cohort = dataset_name, objective = "objective_2_safety_toxicity", subobjective = NULL, expr = {
            logger::log_info("Running Objective 2: Safety/Toxicity")
            results$objective_2 <- tryCatch(run_objective_2(data, dataset_name, output_dirs, prefix, other_map, confounders = cohort_confounders), error = function(e) {
                errors_this_dataset <<- TRUE
                logger::log_error(formatted(sprintf("ERROR in Objective 2: %s", e$message)))
                NULL
            })
        })
    }

    if (3 %in% objectives_to_run) {
        with_log_context(cohort = dataset_name, objective = "objective_3_repeat_radiation", subobjective = NULL, expr = {
            logger::log_info("Running Objective 3: Repeat Radiation Efficacy")
            results$objective_3 <- tryCatch(run_objective_3(data, dataset_name, output_dirs, prefix, other_map, confounders = cohort_confounders), error = function(e) {
                errors_this_dataset <<- TRUE
                logger::log_error(formatted(sprintf("ERROR in Objective 3: %s", e$message)))
                NULL
            })
        })
    }

    if (4 %in% objectives_to_run) {
        with_log_context(cohort = dataset_name, objective = "objective_4_gep_analysis", subobjective = NULL, expr = {
            logger::log_info("Running Objective 4: GEP Validation")
            results$objective_4 <- tryCatch(run_objective_4(data, dataset_name, output_dirs, prefix, other_map, confounders = cohort_confounders), error = function(e) {
                errors_this_dataset <<- TRUE
                logger::log_error(formatted(sprintf("ERROR in Objective 4: %s", e$message)))
                NULL
            })
        })
    }

    logger::log_info(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)",
        "STATISTICAL ANALYSIS",
        as.numeric(difftime(Sys.time(), analysis_start_time, units = "secs"))
    ))

    results$had_errors <- errors_this_dataset
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
    logger::log_info(formatted(sprintf("Running only Objective %d for dataset: %s", objective_number, dataset_name)))

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

#' Merge baseline tables from all cohorts using provided data
#' This function merges baseline tables using data that's already loaded in memory
#'
#' @param full_data Data frame for the full cohort
#' @param restricted_data Data frame for the restricted cohort
#' @return None
#' @export
merge_baseline_tables_with_data <- function(full_data, restricted_data) {
    # Merge baseline tables from all cohorts
    logger::log_info("Merging baseline tables from all cohorts")
    log_phase("STARTING TABLE MERGING: Full and Restricted Cohorts")

    # Create merged tables directory
    if (!dir.exists(MERGED_TABLES_DIR)) {
        dir.create(MERGED_TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
    }

    logger::log_info(formatted(sprintf("Merging tables will be saved to: %s", MERGED_TABLES_DIR)))

    # Create merged baseline characteristics table using the correct function
    merge_cohort_tables(full_data, restricted_data, MERGED_TABLES_DIR)
    logger::log_info("=== COMPLETED BASELINE TABLE MERGING ===")
    logger::log_info(formatted(sprintf("Merged baseline characteristics table saved to: %s", MERGED_TABLES_DIR)))
    logger::log_info("Files created: merged_baseline_characteristics.xlsx and merged_baseline_characteristics.html")
    
    # Create merged recurrence and metastatic progression tables
    merge_recurrence_metastatic_progression_tables(full_data, restricted_data, MERGED_TABLES_DIR)
    logger::log_info("=== COMPLETED RECURRENCE/METASTATIC TABLE MERGING ===")
    logger::log_info("Files created: merged_recurrence_metastatic_progression.xlsx and merged_recurrence_metastatic_progression.html")
    
    # Create merged adverse events tables
    merge_adverse_events_tables(full_data, restricted_data, MERGED_TABLES_DIR)
    logger::log_info("=== COMPLETED ADVERSE EVENTS TABLE MERGING ===")
    logger::log_info("Files created: merged_adverse_events.xlsx and merged_adverse_events.html")
}

#' Merge baseline tables from all cohorts
#' This function merges baseline tables by reading the .rds files
#' @deprecated Use merge_baseline_tables_with_data instead when data is already loaded
#'
#' @return None
#' @export
merge_baseline_tables <- function() {
    # Merge baseline tables from all cohorts
    logger::log_info("Merging baseline tables from all cohorts")
    log_phase("STARTING TABLE MERGING: Full and Restricted Cohorts")

    # Create merged tables directory
    if (!dir.exists(MERGED_TABLES_DIR)) {
        dir.create(MERGED_TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
    }

    logger::log_info(formatted(sprintf("Merging tables will be saved to: %s", MERGED_TABLES_DIR)))

    # Load both datasets for merging
    full_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
    restricted_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_restricted_cohort.rds"))

    # Create merged baseline characteristics table using the correct function
    merge_cohort_tables(full_data, restricted_data, MERGED_TABLES_DIR)
    logger::log_info("=== COMPLETED BASELINE TABLE MERGING ===")
    logger::log_info(formatted(sprintf("Merged baseline characteristics table saved to: %s", MERGED_TABLES_DIR)))
    logger::log_info("Files created: merged_baseline_characteristics.xlsx and merged_baseline_characteristics.html")
    
    # Create merged recurrence and metastatic progression tables
    merge_recurrence_metastatic_progression_tables(full_data, restricted_data, MERGED_TABLES_DIR)
    logger::log_info("=== COMPLETED RECURRENCE/METASTATIC TABLE MERGING ===")
    logger::log_info("Files created: merged_recurrence_metastatic_progression.xlsx and merged_recurrence_metastatic_progression.html")
    
    # Create merged adverse events tables
    merge_adverse_events_tables(full_data, restricted_data, MERGED_TABLES_DIR)
    logger::log_info("=== COMPLETED ADVERSE EVENTS TABLE MERGING ===")
    logger::log_info("Files created: merged_adverse_events.xlsx and merged_adverse_events.html")
}

#' Main execution and merging of baseline tables
#' This function orchestrates the analysis for all datasets and all objectives
#' It runs the analysis for each dataset and merges the baseline tables from all cohorts.
#'
#' @return None
#' @export
main_execution <- function() {
    main_start_time <- Sys.time()
    log_phase("MAIN EXECUTION PHASE")

    # Define datasets to analyze
    # this should be generated from the list_available_datasets function and named appropriately so that run_my_analysis can be called with the correct dataset name
    datasets_to_analyze_temp <- tools::file_path_sans_ext(list_available_datasets())
    # Keep only true cohort datasets (already filtered by list_available_datasets, but double-guard here)
    datasets_to_analyze <- grep("^uveal_melanoma_.*_cohort$", datasets_to_analyze_temp, value = TRUE)

    had_errors <- FALSE
    
    # Store data for merging at the end
    cohort_data <- list()

    # Run analysis for each dataset with progress tracking
    progressr::with_progress({
        p <- progressr::progressor(steps = length(datasets_to_analyze))
        for (i in seq_along(datasets_to_analyze)) {
            dataset_name <- datasets_to_analyze[i]
            logger::log_info(formatted(sprintf(">>> Dataset %d/%d: %s", i, length(datasets_to_analyze), dataset_name)))

            tryCatch(
                {
                    results <- run_my_analysis(dataset_name)
                    if (results$had_errors) had_errors <- TRUE
                    
                    # Store the data for merging (if available)
                    if (exists("data") && !is.null(data)) {
                        cohort_data[[dataset_name]] <- data
                    }
                    
                    logger::log_info(formatted(sprintf(">>> Dataset %d/%d completed: %s", i, length(datasets_to_analyze), dataset_name)))
                },
                error = function(e) {
                    had_errors <<- TRUE
                    logger::log_error(formatted(sprintf("ERROR in dataset %s: %s", dataset_name, e$message)))
                }
            )

            p(message = sprintf("Completed %s", dataset_name))
        }
    })

    # Merge baseline tables from all cohorts using stored data
    tryCatch({
        if (length(cohort_data) >= 2) {
            # Get the two main cohorts for merging
            full_data <- cohort_data[["uveal_melanoma_full_cohort"]]
            restricted_data <- cohort_data[["uveal_melanoma_restricted_cohort"]]
            
            if (!is.null(full_data) && !is.null(restricted_data)) {
                merge_baseline_tables_with_data(full_data, restricted_data)
            } else {
                logger::log_warn("Cannot merge tables: required cohort data not available")
            }
        } else {
            logger::log_warn("Cannot merge tables: insufficient cohort data available")
        }
    }, error = function(e) {
        had_errors <<- TRUE
        logger::log_error(formatted(sprintf("Error merging baseline tables: %s", e$message)))
    })

    # Summary banner
    if (had_errors) {
        logger::log_error(">>> ANALYSES COMPLETED WITH ERRORS. Review logs for details.")
    } else {
        logger::log_info(">>> ALL ANALYSES COMPLETED SUCCESSFULLY!")
    }
    logger::log_info(formatted(sprintf(">>> Total execution time: %.1f minutes", as.numeric(difftime(Sys.time(), main_start_time, units = "mins")))))
    logger::log_info(formatted(sprintf(">>> Datasets analyzed: %d", length(datasets_to_analyze))))
    logger::log_info("Check the logs above for detailed progress and any warnings.")
    logger::log_info("Each cohort has its own complete set of analyses for easy comparison!")

    logger::log_info(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)",
        "MAIN EXECUTION PHASE",
        as.numeric(difftime(Sys.time(), main_start_time, units = "secs"))
    ))
}
