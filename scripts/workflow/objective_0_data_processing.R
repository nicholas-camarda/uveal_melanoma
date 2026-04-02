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
#' @return Named list of cohort-specific output directories for Objective 0.
build_objective_0_output_dirs <- function() {
    temp_output_dirs_by_cohort <- list()

    for (cohort_name in get_expected_analytic_cohort_names()) {
        cohort_dir_name <- case_when(
            grepl("full", cohort_name) ~ "uveal_full",
            grepl("restricted", cohort_name) ~ "uveal_restricted",
            grepl("gksrs", cohort_name) ~ "gksrs",
            TRUE ~ cohort_name
        )

        cohort_base_dir <- file.path(OUTPUT_DIR, cohort_dir_name)
        simplified_cohort_name <- case_when(
            grepl("full", cohort_name) ~ "full_cohort",
            grepl("restricted", cohort_name) ~ "restricted_cohort",
            grepl("gksrs", cohort_name) ~ "gksrs_only_cohort",
            TRUE ~ cohort_name
        )

        temp_output_dirs_by_cohort[[simplified_cohort_name]] <- create_output_structure(cohort_base_dir)
    }

    temp_output_dirs_by_cohort
}

#' Load previously processed analytic cohorts from disk
#'
#' @param expected_cohorts Character vector of cohort dataset names to load.
#' @return Named list with `cohort_data` and `missing_files`.
load_existing_analytic_cohorts <- function(expected_cohorts = get_expected_analytic_cohort_names()) {
    cohort_data <- list()
    missing_files <- character()

    for (cohort_name in expected_cohorts) {
        data_path <- file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".rds"))
        if (!file.exists(data_path)) {
            missing_files <- c(missing_files, basename(data_path))
            next
        }
        cohort_data[[cohort_name]] <- readRDS(data_path)
    }

    list(
        cohort_data = cohort_data,
        missing_files = unique(missing_files)
    )
}

load_existing_objective0_removal_log <- function(output_dirs) {
    if (is.null(output_dirs) || is.null(output_dirs$full_cohort$baseline_characteristics)) {
        return(NULL)
    }

    removal_path <- file.path(dirname(output_dirs$full_cohort$baseline_characteristics), "removed_patients_summary.tsv")
    if (!file.exists(removal_path)) {
        return(NULL)
    }

    removal_log <- tryCatch(
        readr::read_tsv(removal_path, show_col_types = FALSE),
        error = function(e) NULL
    )

    if (is.null(removal_log) || nrow(removal_log) == 0) {
        return(NULL)
    }

    removal_log %>%
        dplyr::filter(!grepl("^No patients removed prior", .data$removal_reason))
}

refresh_generated_study_docs <- function(validation_result) {
    if (isTRUE(validation_result$has_hard_errors)) {
        logger::log_warn("Objective 0 validation found hard errors; skipping committed study doc refresh.")
        return(list(status = "skipped_hard_errors"))
    }

    tryCatch(
        refresh_study_docs(),
        error = function(e) {
            logger::log_warn(sprintf("Objective 0 study doc refresh failed: %s", conditionMessage(e)))
            list(
                status = "warning_failed",
                error_message = conditionMessage(e)
            )
        }
    )
}

#' Run Objective 0 as a global analytic-data preflight gate
#'
#' Create or validate analytic datasets once for the entire workflow and return a
#' structured status object that downstream orchestration can use as a hard gate.
#'
#' @return List containing `success`, `validated_cohorts`,
#'   `validation_errors`, and `created_datasets`.
run_objective_0 <- function() {
    expected_cohorts <- get_expected_analytic_cohort_names()
    temp_output_dirs_by_cohort <- build_objective_0_output_dirs()

    if (RECREATE_ANALYTIC_DATASETS) {
        log_phase("DATA PREPROCESSING PHASE")
        data_start_time <- Sys.time()

        logger::log_info("RECREATE_ANALYTIC_DATASETS = TRUE: Creating new analytic datasets")
        assert_required_input_paths(input_filename = INPUT_FILENAME, require_data_dictionary = FALSE)
        logger::log_info(sprintf("Validated raw inputs at export-backed path: %s", RAW_DATA_DIR))

        # Create analytic datasets using the comprehensive function
        logger::log_info(formatted("Executing create_analytic_dataset: Creating analytic datasets with full processing pipeline", indent = 1))
        analytic_result <- create_analytic_dataset(
            output_dirs = temp_output_dirs_by_cohort,
            validate_after_saving = TRUE,
            stop_on_validation_failure = FALSE
        )
        validation_result <- analytic_result$validation %||% list(
            success = TRUE,
            validated_cohorts = names(analytic_result$analytic_data %||% list()),
            validation_errors = character()
        )

        write_objective0_validation_artifacts(
            validation_result = validation_result,
            output_dirs = temp_output_dirs_by_cohort,
            reconciliation_audit = analytic_result$reconciliation_audit
        )
        documentation_refresh <- refresh_generated_study_docs(validation_result)

        logger::log_info(sprintf(">>> COMPLETED %s (Duration: %.1f seconds)",
            "DATA PREPROCESSING PHASE",
            as.numeric(difftime(Sys.time(), data_start_time, units = "secs"))
        ))

        return(list(
            success = isTRUE(validation_result$success),
            validated_cohorts = validation_result$validated_cohorts %||% character(),
            validation_errors = validation_result$validation_errors %||% character(),
            created_datasets = names(analytic_result$analytic_data %||% list()),
            analytic_result = analytic_result,
            documentation_refresh = documentation_refresh
        ))
    } else {
        log_phase("DATA LOADING PHASE")
        logger::log_info("RECREATE_ANALYTIC_DATASETS = FALSE: Skipping analytic dataset creation")
        logger::log_info(sprintf("Using existing runtime datasets from: %s", PROCESSED_DATA_DIR))
        logger::log_info("Set RECREATE_ANALYTIC_DATASETS = TRUE if you need to reprocess raw data")

        existing_data <- load_existing_analytic_cohorts(expected_cohorts)
        existing_removal_log <- load_existing_objective0_removal_log(temp_output_dirs_by_cohort)
        validation_errors <- character()

        if (length(existing_data$missing_files) > 0) {
            logger::log_error(sprintf(
                "Processed analytic datasets missing: %s",
                paste(existing_data$missing_files, collapse = ", ")
            ))
            validation_errors <- c(
                validation_errors,
                paste0("processed_files_missing:", existing_data$missing_files)
            )
        }

        if (length(existing_data$cohort_data) == length(expected_cohorts)) {
            logger::log_info("Refreshing cohort summary artifacts from existing runtime datasets")
            export_cohort_summary(
                cohort_list = existing_data$cohort_data,
                removal_log = existing_removal_log,
                output_path = file.path(PROCESSED_DATA_DIR, "cohort_summary_statistics.json"),
                output_dirs = temp_output_dirs_by_cohort
            )
        }

        validation_result <- if (length(existing_data$cohort_data) == length(expected_cohorts)) {
            validate_processing_pipeline(
                existing_data$cohort_data,
                stop_on_failure = FALSE,
                removal_log = existing_removal_log
            )
        } else {
            list(
                success = FALSE,
                validated_cohorts = names(existing_data$cohort_data),
                validation_errors = character(),
                has_hard_errors = TRUE,
                validation_findings = empty_validation_findings(),
                detail_tables = empty_validation_detail_table()
            )
        }

        write_objective0_validation_artifacts(
            validation_result = validation_result,
            output_dirs = temp_output_dirs_by_cohort,
            reconciliation_audit = NULL
        )
        documentation_refresh <- refresh_generated_study_docs(validation_result)

        validation_errors <- unique(c(
            validation_errors,
            validation_result$validation_errors %||% character()
        ))

        return(list(
            success = length(validation_errors) == 0 && isTRUE(validation_result$success),
            validated_cohorts = validation_result$validated_cohorts %||% names(existing_data$cohort_data),
            validation_errors = validation_errors,
            created_datasets = character(),
            documentation_refresh = documentation_refresh
        ))
    }
}
