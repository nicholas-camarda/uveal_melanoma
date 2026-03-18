########################################################
############### MAIN EXECUTION FUNCTIONS ###############
########################################################

#' Append a distinct issue code to an issue collection
#'
#' @param existing Character vector of previously recorded issue codes.
#' @param new_issue Character scalar describing the new issue.
#' @return Character vector with the new issue added once.
append_issue <- function(existing, new_issue) {
    unique(c(existing, new_issue))
}

#' Determine the overall workflow run state
#'
#' @param fatal_issues Character vector of fatal issue codes.
#' @param warning_issues Character vector of warning-only issue codes.
#' @return Character scalar equal to `success`, `completed_with_warnings`, or
#'   `failed`.
determine_run_state <- function(fatal_issues = character(), warning_issues = character()) {
    if (length(fatal_issues) > 0) {
        return("failed")
    }
    if (length(warning_issues) > 0) {
        return("completed_with_warnings")
    }
    "success"
}

#' Collect expected warning signals from nested workflow results
#'
#' @param x Arbitrary workflow result object.
#' @param path Character scalar describing the current traversal path.
#' @return Character vector of warning issue codes.
collect_expected_warning_signals <- function(x, path = "root") {
    warning_issues <- character()

    if (is.null(x)) {
        return(warning_issues)
    }

    if (is.data.frame(x)) {
        if ("Analysis_Status" %in% names(x) && any(x$Analysis_Status == "skipped", na.rm = TRUE)) {
            warning_issues <- append_issue(warning_issues, paste0(path, ":rmst_skipped"))
        }
        if ("status" %in% names(x) && any(grepl("skipped|insufficient|no_event_of_interest", x$status), na.rm = TRUE)) {
            warning_issues <- append_issue(warning_issues, paste0(path, ":status_skip"))
        }
        if (all(c("variable", "reason", "retained_values", "non_missing_n") %in% names(x)) && nrow(x) > 0) {
            warning_issues <- append_issue(warning_issues, paste0(path, ":covariates_dropped"))
        }
        return(warning_issues)
    }

    if (is.list(x)) {
        if (!is.null(x$feasibility)) {
            model_statuses <- x$feasibility$models %||% list()
            skipped_models <- vapply(model_statuses, function(model_status) {
                identical(model_status$status %||% NA_character_, "skipped")
            }, logical(1))
            if (any(skipped_models)) {
                warning_issues <- append_issue(warning_issues, paste0(path, ":competing_risk_feasibility"))
            }
        }
        if (!is.null(x$warning_issues) && length(x$warning_issues) > 0) {
            warning_issues <- c(warning_issues, unlist(x$warning_issues, use.names = FALSE))
        }

        child_names <- names(x)
        if (is.null(child_names)) {
            child_names <- seq_along(x)
        }

        for (i in seq_along(x)) {
            warning_issues <- c(
                warning_issues,
                collect_expected_warning_signals(x[[i]], path = paste0(path, "$", child_names[[i]]))
            )
        }
    }

    unique(warning_issues)
}

#' Collect unexpected failure signals from nested workflow results
#'
#' @param x Arbitrary workflow result object.
#' @param path Character scalar describing the current traversal path.
#' @return Character vector of fatal issue codes.
collect_unexpected_failure_signals <- function(x, path = "root") {
    fatal_issues <- character()

    if (is.null(x)) {
        return(fatal_issues)
    }

    if (is.data.frame(x)) {
        if ("Analysis_Status" %in% names(x) && any(x$Analysis_Status == "failed", na.rm = TRUE)) {
            fatal_issues <- append_issue(fatal_issues, paste0(path, ":analysis_failed"))
        }
        if ("status" %in% names(x) && any(x$status == "failed", na.rm = TRUE)) {
            fatal_issues <- append_issue(fatal_issues, paste0(path, ":status_failed"))
        }
        return(fatal_issues)
    }

    if (is.list(x)) {
        if (!is.null(x$unexpected_failures) && length(x$unexpected_failures) > 0) {
            fatal_issues <- c(
                fatal_issues,
                paste0(path, ":unexpected:", unlist(x$unexpected_failures, use.names = FALSE))
            )
        }
        if (!is.null(x$status) && identical(x$status, "failed")) {
            fatal_issues <- append_issue(fatal_issues, paste0(path, ":status_failed"))
        }

        child_names <- names(x)
        if (is.null(child_names)) {
            child_names <- seq_along(x)
        }

        for (i in seq_along(x)) {
            fatal_issues <- c(
                fatal_issues,
                collect_unexpected_failure_signals(x[[i]], path = paste0(path, "$", child_names[[i]]))
            )
        }
    }

    unique(fatal_issues)
}

#' Run Objective 0 with a global log context
#'
#' Objective 0 is a workflow-wide preflight gate, so its logs should carry the
#' objective tag but never inherit a cohort-specific tag from prior or surrounding
#' analyses.
#'
#' @return The list returned by `run_objective_0()`.
run_objective_0_with_global_context <- function() {
    with_log_context(
        cohort = NULL,
        objective = "objective_0_data_processing",
        subobjective = NULL,
        replace = TRUE,
        expr = {
            logger::log_info("Running Objective 0: Data Processing (global preflight)")
            run_objective_0()
        }
    )
}

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
    objectives_to_run <- as.integer(objectives_to_run)
    results <- list()
    fatal_issues <- character()
    warning_issues <- character()

    # Clean dataset name for display
    display_name <- tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))
    log_phase(paste("STATISTICAL ANALYSIS", display_name, sep = " - "))

    # Objective 0 is a global preflight gate and does not depend on dataset loading
    if (0 %in% objectives_to_run) {
        results$objective_0 <- run_objective_0_with_global_context()

        if (!isTRUE(results$objective_0$success)) {
            fatal_issues <- append_issue(
                fatal_issues,
                sprintf(
                    "objective_0_preflight_failed:%s",
                    paste(results$objective_0$validation_errors %||% "unknown", collapse = ",")
                )
            )
        }
    }

    analysis_objectives <- intersect(objectives_to_run, c(1, 2, 3, 4))
    if (length(analysis_objectives) == 0) {
        results$run_state <- determine_run_state(fatal_issues, warning_issues)
        results$had_errors <- identical(results$run_state, "failed")
        results$had_warnings <- identical(results$run_state, "completed_with_warnings")
        results$fatal_issues <- fatal_issues
        results$warning_issues <- unique(warning_issues)
        return(invisible(results))
    }

    if (length(fatal_issues) > 0) {
        results$run_state <- determine_run_state(fatal_issues, warning_issues)
        results$had_errors <- TRUE
        results$had_warnings <- FALSE
        results$fatal_issues <- fatal_issues
        results$warning_issues <- unique(warning_issues)
        return(invisible(results))
    }

    # Check dependencies before running analysis objectives
    if (length(analysis_objectives) > 0) {
        # Check if required files exist for analysis objectives
        required_files <- c(file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds")))

        missing_files <- required_files[!file.exists(required_files)]

        if (length(missing_files) > 0) {
            stop(sprintf(
                "DEPENDENCY ERROR: Required files missing for dataset '%s': %s\nRun Objective 0 first to create these files.",
                dataset_name, paste(basename(missing_files), collapse = ", ")
            ))
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

    # Use configured confounders directly (do not load/save validated_confounders_by_cohort)
    cohort_confounders <- confounders

    # Run selected objectives (excluding 0 which may have been run above) with error tracking
    if (1 %in% analysis_objectives) {
        with_log_context(cohort = dataset_name, objective = "objective_1_primary_outcomes", subobjective = NULL, expr = {
            logger::log_info("Running Objective 1: Primary Outcomes")
            results$objective_1 <- tryCatch(run_objective_1(data, dataset_name, output_dirs, prefix, confounders = cohort_confounders), error = function(e) {
                fatal_issues <<- append_issue(fatal_issues, sprintf("objective_1:%s", e$message))
                logger::log_error(formatted(sprintf("ERROR in Objective 1: %s", e$message)))
                NULL
            })
        })
        warning_issues <- c(warning_issues, collect_expected_warning_signals(results$objective_1, "objective_1"))
        fatal_issues <- c(fatal_issues, collect_unexpected_failure_signals(results$objective_1, "objective_1"))
    }

    if (2 %in% analysis_objectives) {
        with_log_context(cohort = dataset_name, objective = "objective_2_safety_toxicity", subobjective = NULL, expr = {
            logger::log_info("Running Objective 2: Safety/Toxicity")
            results$objective_2 <- tryCatch(run_objective_2(data, dataset_name, output_dirs, prefix, confounders = cohort_confounders), error = function(e) {
                fatal_issues <<- append_issue(fatal_issues, sprintf("objective_2:%s", e$message))
                logger::log_error(formatted(sprintf("ERROR in Objective 2: %s", e$message)))
                NULL
            })
        })
        warning_issues <- c(warning_issues, collect_expected_warning_signals(results$objective_2, "objective_2"))
        fatal_issues <- c(fatal_issues, collect_unexpected_failure_signals(results$objective_2, "objective_2"))
    }

    if (3 %in% analysis_objectives) {
        with_log_context(cohort = dataset_name, objective = "objective_3_repeat_radiation", subobjective = NULL, expr = {
            logger::log_info("Running Objective 3: Repeat Radiation Efficacy")
            results$objective_3 <- tryCatch(run_objective_3(data, dataset_name, output_dirs, prefix, confounders = cohort_confounders), error = function(e) {
                fatal_issues <<- append_issue(fatal_issues, sprintf("objective_3:%s", e$message))
                logger::log_error(formatted(sprintf("ERROR in Objective 3: %s", e$message)))
                NULL
            })
        })
        warning_issues <- c(warning_issues, collect_expected_warning_signals(results$objective_3, "objective_3"))
        fatal_issues <- c(fatal_issues, collect_unexpected_failure_signals(results$objective_3, "objective_3"))
    }

    if (4 %in% analysis_objectives) {
        with_log_context(cohort = dataset_name, objective = "objective_4_gep_analysis", subobjective = NULL, expr = {
            logger::log_info("Running Objective 4: GEP Validation")
            results$objective_4 <- tryCatch(run_objective_4(data, dataset_name, output_dirs, prefix, confounders = cohort_confounders), error = function(e) {
                fatal_issues <<- append_issue(fatal_issues, sprintf("objective_4:%s", e$message))
                logger::log_error(formatted(sprintf("ERROR in Objective 4: %s", e$message)))
                NULL
            })
        })
        warning_issues <- c(warning_issues, collect_expected_warning_signals(results$objective_4, "objective_4"))
        fatal_issues <- c(fatal_issues, collect_unexpected_failure_signals(results$objective_4, "objective_4"))
    }

    logger::log_info(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)",
        "STATISTICAL ANALYSIS",
        as.numeric(difftime(Sys.time(), analysis_start_time, units = "secs"))
    ))

    results$warning_issues <- unique(warning_issues)
    results$fatal_issues <- unique(fatal_issues)
    results$run_state <- determine_run_state(results$fatal_issues, results$warning_issues)
    results$had_errors <- identical(results$run_state, "failed")
    results$had_warnings <- identical(results$run_state, "completed_with_warnings")
    return(invisible(results))
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

    if (identical(as.integer(objective_number), 0L)) {
        return(invisible(run_objective_0_with_global_context()))
    }

    # Check dependencies for analysis objectives (1-4)
    if (objective_number %in% c(1, 2, 3, 4)) {
        required_files <- c(file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds")))

        missing_files <- required_files[!file.exists(required_files)]

        if (length(missing_files) > 0) {
            stop(sprintf(
                "DEPENDENCY ERROR: Required files missing for dataset '%s': %s\nRun Objective 0 first to create these files.",
                dataset_name, paste(basename(missing_files), collapse = ", ")
            ))
        }
    }

    results <- run_my_analysis(dataset_name, objectives_to_run = objective_number)
    return(invisible(results))
}

#' Merge baseline tables from all cohorts using provided data
#' This function merges baseline tables using data that's already loaded in memory
#'
#' @param full_data Data frame for the full cohort
#' @param restricted_data Data frame for the restricted cohort
#' @param gksrs_only_data Optional data frame for the GKSRS-only cohort
#' @return None
#' @export
merge_baseline_tables_with_data <- function(full_data, restricted_data, gksrs_only_data = NULL) {
    # Merge baseline tables from all cohorts
    logger::log_info("Merging baseline tables from all cohorts")
    log_phase("STARTING TABLE MERGING: Full and Restricted Cohorts")

    # Create merged tables directory
    if (!dir.exists(MERGED_TABLES_DIR)) {
        dir.create(MERGED_TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
    }

    logger::log_info(formatted(sprintf("Merging tables will be saved to: %s", MERGED_TABLES_DIR)))

    # Create merged baseline characteristics table using the correct function
    merge_cohort_tables(full_data, restricted_data, MERGED_TABLES_DIR,
        dataset_names = list(
            full = "uveal_melanoma_full_cohort",
            restricted = "uveal_melanoma_restricted_cohort"
        )
    )
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

    if (!is.null(gksrs_only_data)) {
        merge_full_vs_gksrs_baseline_tables(
            full_cohort_data = full_data,
            gksrs_only_cohort_data = gksrs_only_data,
            output_path = MERGED_TABLES_DIR,
            dataset_names = list(
                full = "uveal_melanoma_full_cohort",
                gksrs_only = "uveal_melanoma_gksrs_only_cohort"
            )
        )

        merge_all_cohort_baseline_tables(
            full_cohort_data = full_data,
            restricted_cohort_data = restricted_data,
            gksrs_only_cohort_data = gksrs_only_data,
            output_path = MERGED_TABLES_DIR,
            dataset_names = list(
                full = "uveal_melanoma_full_cohort",
                restricted = "uveal_melanoma_restricted_cohort",
                gksrs_only = "uveal_melanoma_gksrs_only_cohort"
            )
        )
    } else {
        logger::log_warn("GKSRS-only cohort data not available; skipping three-cohort baseline merge")
    }
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

    # Load cohort datasets for merging
    full_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
    restricted_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_restricted_cohort.rds"))
    gksrs_only_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_gksrs_only_cohort.rds"))

    # Create merged baseline characteristics table using the correct function
    merge_cohort_tables(full_data, restricted_data, MERGED_TABLES_DIR,
        dataset_names = list(
            full = "uveal_melanoma_full_cohort",
            restricted = "uveal_melanoma_restricted_cohort"
        )
    )
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

    merge_full_vs_gksrs_baseline_tables(
        full_cohort_data = full_data,
        gksrs_only_cohort_data = gksrs_only_data,
        output_path = MERGED_TABLES_DIR,
        dataset_names = list(
            full = "uveal_melanoma_full_cohort",
            gksrs_only = "uveal_melanoma_gksrs_only_cohort"
        )
    )

    merge_all_cohort_baseline_tables(
        full_cohort_data = full_data,
        restricted_cohort_data = restricted_data,
        gksrs_only_cohort_data = gksrs_only_data,
        output_path = MERGED_TABLES_DIR,
        dataset_names = list(
            full = "uveal_melanoma_full_cohort",
            restricted = "uveal_melanoma_restricted_cohort",
            gksrs_only = "uveal_melanoma_gksrs_only_cohort"
        )
    )
}

#' Main execution and merging of baseline tables
#' This function orchestrates the analysis for all datasets and all objectives
#' It runs the analysis for each dataset and merges the baseline tables from all cohorts.
#'
#' @return None
#' @export
main_execution <- function() {
    main_start_time <- Sys.time()
    set_log_context(replace = TRUE)
    log_phase("MAIN EXECUTION PHASE")

    # Define datasets to analyze
    # this should be generated from the list_available_datasets function and named appropriately so that run_my_analysis can be called with the correct dataset name
    datasets_to_analyze_temp <- tools::file_path_sans_ext(list_available_datasets())
    # Keep only true cohort datasets (already filtered by list_available_datasets, but double-guard here)
    datasets_to_analyze <- grep("^uveal_melanoma_.*_cohort$", datasets_to_analyze_temp, value = TRUE)

    fatal_issues <- character()
    warning_issues <- character()
    
    # Store data for merging at the end
    cohort_data <- list()

    logger::log_info(sprintf("Found %d datasets to analyze", length(datasets_to_analyze)))
    logger::log_info("Starting global Objective 0 preflight")

    preflight_result <- tryCatch(
        run_objective_0_with_global_context(),
        error = function(e) {
            list(
                success = FALSE,
                validated_cohorts = character(),
                validation_errors = sprintf("objective_0_exception:%s", e$message),
                created_datasets = character()
            )
        }
    )

    if (!isTRUE(preflight_result$success)) {
        fatal_issues <- append_issue(
            fatal_issues,
            sprintf(
                "objective_0_preflight_failed:%s",
                paste(preflight_result$validation_errors %||% "unknown", collapse = ",")
            )
        )
    }

    if (isTRUE(preflight_result$success)) {
        logger::log_info("Objective 0 preflight completed successfully")
    }

    if (length(fatal_issues) > 0) {
        logger::log_error(">>> ANALYSES COMPLETED WITH ERRORS. Objective 0 preflight failed.")
        logger::log_info(formatted(sprintf(">>> Total execution time: %.1f minutes", as.numeric(difftime(Sys.time(), main_start_time, units = "mins")))))
        logger::log_info(formatted(sprintf(">>> Datasets analyzed: %d", 0)))
        logger::log_info("Check the logs above for detailed validation failures.")
        logger::log_info(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)",
            "MAIN EXECUTION PHASE",
            as.numeric(difftime(Sys.time(), main_start_time, units = "secs"))
        ))
        return(invisible(list(
            run_state = "failed",
            fatal_issues = fatal_issues,
            warning_issues = warning_issues,
            objective_0 = preflight_result
        )))
    }

    # Run analysis for each dataset with explicit logger milestones
    for (i in seq_along(datasets_to_analyze)) {
        dataset_name <- datasets_to_analyze[i]
        logger::log_info(formatted(sprintf(">>> Dataset %d/%d: %s", i, length(datasets_to_analyze), dataset_name)))

        tryCatch(
            {
                results <- run_my_analysis(dataset_name, objectives_to_run = c(1, 2, 3, 4))
                fatal_issues <- c(fatal_issues, results$fatal_issues %||% character())
                warning_issues <- c(warning_issues, results$warning_issues %||% character())

                # Load the data directly for merging
                tryCatch({
                    data_path <- file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds"))
                    if (file.exists(data_path)) {
                        cohort_data[[dataset_name]] <- readRDS(data_path)
                        logger::log_info(sprintf("Loaded data for merging: %s (%d patients)", dataset_name, nrow(cohort_data[[dataset_name]])))
                    } else {
                        fatal_issues <<- append_issue(fatal_issues, sprintf("merge_input_missing:%s", data_path))
                        logger::log_error(sprintf("Data file not found for merging: %s", data_path))
                    }
                }, error = function(e) {
                    fatal_issues <<- append_issue(fatal_issues, sprintf("merge_input_load:%s:%s", dataset_name, e$message))
                    logger::log_error(sprintf("Error loading data for merging (%s): %s", dataset_name, e$message))
                })

                logger::log_info(formatted(sprintf(">>> Dataset %d/%d completed: %s", i, length(datasets_to_analyze), dataset_name)))
            },
            error = function(e) {
                fatal_issues <<- append_issue(fatal_issues, sprintf("dataset:%s:%s", dataset_name, e$message))
                logger::log_error(formatted(sprintf("ERROR in dataset %s: %s", dataset_name, e$message)))
            }
        )
    }

    # Merge baseline tables from all cohorts using stored data
    set_log_context(replace = TRUE)
    tryCatch({
        if (length(cohort_data) >= 2) {
            # Get the two main cohorts for merging
            full_data <- cohort_data[["uveal_melanoma_full_cohort"]]
            restricted_data <- cohort_data[["uveal_melanoma_restricted_cohort"]]
            gksrs_only_data <- cohort_data[["uveal_melanoma_gksrs_only_cohort"]]
            
            if (!is.null(full_data) && !is.null(restricted_data)) {
                merge_baseline_tables_with_data(full_data, restricted_data, gksrs_only_data)
            } else {
                fatal_issues <<- append_issue(fatal_issues, "merge_required_cohort_missing")
                logger::log_error("Cannot merge tables: required cohort data not available")
            }
        } else {
            fatal_issues <<- append_issue(fatal_issues, "merge_insufficient_cohort_data")
            logger::log_error("Cannot merge tables: insufficient cohort data available")
        }
    }, error = function(e) {
        fatal_issues <<- append_issue(fatal_issues, sprintf("merge_failure:%s", e$message))
        logger::log_error(formatted(sprintf("Error merging baseline tables: %s", e$message)))
    })

    # Summary banner
    set_log_context(replace = TRUE)
    run_state <- determine_run_state(unique(fatal_issues), unique(warning_issues))
    if (identical(run_state, "failed")) {
        logger::log_error(">>> ANALYSES COMPLETED WITH ERRORS. Review logs for details.")
    } else if (identical(run_state, "completed_with_warnings")) {
        logger::log_warn(">>> ANALYSES COMPLETED WITH WARNINGS. Review feasibility notes and diagnostics.")
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

    invisible(list(
        run_state = run_state,
        fatal_issues = unique(fatal_issues),
        warning_issues = unique(warning_issues),
        objective_0 = preflight_result
    ))
}
