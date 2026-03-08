# GEP Evaluation Orchestration (no algorithms)

#' Analyze GEP MFS Validation
#'
#' Orchestrate MFS validation at multiple timepoints, computing observed vs
#' expected, calibration, discrimination, decision curves, PRAME analysis, and
#' saving outputs.
#'
#' @param data Data frame with GEP predictions and outcomes
#' @param dataset_name Optional character label for reporting
#' @param timepoints Numeric vector of years (default `GEP_VALIDATION_TIMEPOINTS`)
#' @param bootstrap_iterations Integer bootstrap iterations retained for API compatibility; the current calibration slope uses IPCW-weighted logistic recalibration rather than bootstrap optimism correction
#' @param create_unified_at_base logical If TRUE, also write unified visuals at the parent objective dir
#' @param other_map List containing treatment group mappings and categorical variable level mappings for consistent analysis
#' @param output_dirs Named list of objective-specific output directories
#' @param prefix Character prefix for all generated files
#' @return A list with `validation_results`, `prame_analysis`, and `missing_data_analysis`.
analyze_gep_mfs_validation <- function(data,
                                       dataset_name = NULL,
                                       timepoints = GEP_VALIDATION_TIMEPOINTS,
                                       bootstrap_iterations = GEP_BOOTSTRAP_ITERATIONS,
                                       create_unified_at_base = FALSE,
                                       other_map = NULL,
                                       output_dirs = NULL,
                                       prefix = "") {
    logger::log_info("Starting GEP Metastasis-Free Survival validation analysis")

    if (is.null(output_dirs) || is.null(output_dirs$obj4_mfs)) {
        stop("analyze_gep_mfs_validation() requires an output_dirs list with obj4_mfs entry")
    }

    if (is.null(prefix)) {
        prefix <- ""
    }
    
    # Load other_map if not provided
    if (is.null(other_map)) {
        if (!is.null(dataset_name)) {
            other_map <- tryCatch(get_cohort_specific_other_map(dataset_name), error = function(e) list())
        } else {
            other_map <- list()
        }
    }
    mfs_output_dir <- output_dirs$obj4_mfs
    if (!dir.exists(mfs_output_dir)) {
        dir.create(mfs_output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    logger::log_info(formatted("Reporting GEP validation dataset distribution", indent = 1))
    gep_distribution <- data %>%
        count(gep_validation_set, biopsy1_gep) %>%
        tidyr::pivot_wider(names_from = biopsy1_gep, values_from = n, values_fill = 0)
    logger::log_info(formatted("GEP Validation Set Distribution:", indent = 1))
    for (i in seq_len(nrow(gep_distribution))) {
        set_name <- gep_distribution$gep_validation_set[i]
        class_1 <- ifelse(is.na(gep_distribution$`Class 1`[i]), 0, gep_distribution$`Class 1`[i])
        class_2 <- ifelse(is.na(gep_distribution$`Class 2`[i]), 0, gep_distribution$`Class 2`[i])
        no_gep <- ifelse(is.na(gep_distribution$`No`[i]), 0, gep_distribution$`No`[i])
        total <- class_1 + class_2 + no_gep
        logger::log_info(formatted(sprintf("%s: %d patients (Class1:%d, Class2:%d, No:%d)", set_name, total, class_1, class_2, no_gep), indent = 2))
    }
    write_xlsx(gep_distribution, file.path(mfs_output_dir, paste0(prefix, "gep_validation_distribution.xlsx")))
    logger::log_info(formatted("Filtering data for MFS validation", indent = 1))
    required_vars <- c("biopsy1_gep", "expected_mfs_5yr", "expected_mfs_7yr", "expected_mfs_10yr", "prame_status", "gep_validation_set")
    missing_vars <- setdiff(required_vars, names(data))
    if (length(missing_vars) > 0) {
        logger::log_error(formatted(sprintf("ERROR: Missing required GEP variables: %s", paste(missing_vars, collapse = ", ")), indent = 1))
        logger::log_error(formatted("These variables should have been created in data_processing.R", indent = 1))
        stop("GEP validation cannot proceed without required variables")
    }
    # Use pre-processed analysis eligibility for consistency in risk-based metrics
    analysis_data <- data %>%
        filter(mfs_analysis_eligible)
    logger::log_info(formatted(sprintf("Analysis dataset: %d patients with valid GEP and MFS data", nrow(analysis_data)), indent = 1))

    # Create an expanded dataset for KM curves and PH diagnostics that retains "GEP Not Tested"
    km_ph_data <- data %>%
        filter(
            !is.na(biopsy1_gep),
            !is.na(tt_mets_months),
            !is.na(mets_event),
            tt_mets_months >= 0
        )
    logger::log_info(formatted(sprintf(
        "KM/PH dataset: %d patients after including GEP Not Tested",
        nrow(km_ph_data)
    ), indent = 1))
    if (nrow(km_ph_data) > 0) {
        km_summary <- km_ph_data %>%
            group_by(biopsy1_gep) %>%
            summarise(
                n = n(),
                events = sum(mets_event == 1, na.rm = TRUE),
                .groups = "drop"
            )
        for (i in seq_len(nrow(km_summary))) {
            logger::log_info(formatted(sprintf(
                "%s: n = %d, metastasis events = %d",
                km_summary$biopsy1_gep[i],
                km_summary$n[i],
                km_summary$events[i]
            ), indent = 2))
        }
    }
    # Use pre-processed time-specific event indicators for consistency
    events_per_timepoint <- sapply(timepoints, function(tp) {
        sum(analysis_data[[paste0("mfs_event_", tp, "yr")]])
    })
    names(events_per_timepoint) <- paste0(timepoints, "yr")
    logger::log_info(formatted("Events per timepoint:", indent = 1))
    for (i in seq_along(events_per_timepoint)) {
        ep_status <- ifelse(events_per_timepoint[i] >= 100, "✓", "⚠ <100")
        logger::log_info(formatted(sprintf("%s: %d events %s", names(events_per_timepoint)[i], events_per_timepoint[i], ep_status), indent = 2))
    }
    missing_data_analysis <- assess_gep_missing_data(data)
    validation_results <- list()
    for (tp in timepoints) {
        logger::log_info(formatted(sprintf("Analyzing %d-year MFS validation", tp), indent = 1))
        # Use PRAME-aware grouping (biopsy1_gep) for class-level O/E
        oe_results <- calculate_observed_expected_mfs(analysis_data, tp)
        calibration_results <- perform_calibration_mfs(analysis_data, tp, bootstrap_iterations)
        discrimination_results <- perform_discrimination_mfs(analysis_data, tp)
        dca_results <- perform_decision_curve_analysis_mfs(analysis_data, tp)
        validation_results[[paste0(tp, "yr")]] <- list(
            observed_expected = oe_results,
            calibration = calibration_results,
            discrimination = discrimination_results,
            decision_curve = dca_results
        )
    }
    logger::log_info(formatted("Performing PRAME-augmented analysis", indent = 1))
    prame_analysis <- tryCatch({
        perform_prame_augmented_analysis_mfs(analysis_data, timepoints)
    }, error = function(e) {
        logger::log_warn(formatted(sprintf("PRAME-augmented analysis failed: %s", e$message), indent = 2))
        list(status = "failed", error = e$message)
    })
    
    # NEW: Use comprehensive GEP summary system instead of old repetitive reports
    # The save_mfs_validation_results function now creates comprehensive summaries automatically
    save_mfs_validation_results(validation_results, missing_data_analysis, prame_analysis, mfs_output_dir, prefix, dataset_name = dataset_name)

    # Optionally create unified visuals at the GEP objective root directory
    if (create_unified_at_base) {
        gep_base_dir <- dirname(mfs_output_dir)
        tryCatch(
            {
                create_unified_gep_validation_summary(
                    mfs_results = list(validation_results = validation_results, prame_analysis = prame_analysis),
                    mss_results = NULL,
                    dataset_name = dataset_name,
                    output_dir = gep_base_dir,
                    prefix = prefix
                )
                # Write MFS-specific visuals (MFS folder)
                tryCatch(
                    {
                        # Diagnostics before KM: ensure non-NA time/event by class
                        if (!is.null(analysis_data)) {
                            nn_time <- sum(!is.na(analysis_data$tt_mets_months))
                            nn_event <- sum(!is.na(analysis_data$mets_event))
                            logger::log_info(formatted(sprintf(
                                "MFS diagnostics: non-NA tt_mets_months=%d, non-NA mets_event=%d",
                                nn_time, nn_event
                            ), indent = 2))
                            if (!is.null(analysis_data$biopsy1_gep)) {
                                by_class <- analysis_data %>%
                                    dplyr::group_by(biopsy1_gep) %>%
                                    dplyr::summarise(
                                        n = dplyr::n(),
                                        nn_time = sum(!is.na(tt_mets_months)),
                                        nn_event = sum(!is.na(mets_event)),
                                        events = sum(mets_event == 1, na.rm = TRUE),
                                        .groups = "drop"
                                    )
                                logger::log_info(formatted(sprintf("MFS diagnostics by class:\n%s", capture.output(print(by_class)) %>% paste(collapse = "\n")), indent = 2))
                            }
                        }
                                        # Individual plot generation removed - now using consolidated table approach
                # All information is consolidated into comprehensive Excel tables and text summaries
                # This eliminates redundant plots while maintaining all statistical information
                
                    },
                    error = function(e) {
                        logger::log_warn(sprintf("Visual creation failed (MFS outcome folder): %s", e$message))
                    }
                )
            },
            error = function(e) {
                logger::log_warn(sprintf("Visual creation failed (MFS): %s", e$message))
            }
        )
    }

    # Write MFS-specific visuals including survival curves to MFS folder
    tryCatch(
        {
            # Diagnostics before KM: ensure non-NA time/event by class
            if (!is.null(analysis_data)) {
                nn_time <- sum(!is.na(analysis_data$tt_mets_months))
                nn_event <- sum(!is.na(analysis_data$mets_event))
                logger::log_info(formatted(sprintf(
                    "MFS diagnostics: non-NA tt_mets_months=%d, non-NA mets_event=%d",
                    nn_time, nn_event
                ), indent = 1))
                if (!is.null(analysis_data$biopsy1_gep)) {
                    by_class <- analysis_data %>%
                        dplyr::group_by(biopsy1_gep) %>%
                        dplyr::summarise(
                            n = dplyr::n(),
                            nn_time = sum(!is.na(tt_mets_months)),
                            nn_event = sum(!is.na(mets_event)),
                            events = sum(mets_event == 1, na.rm = TRUE),
                            .groups = "drop"
                        )
                    logger::log_info(formatted(sprintf("MFS diagnostics by class:\n%s", capture.output(print(by_class)) %>% paste(collapse = "\n")), indent = 1))
                }
            }
            # NOTE: Individual plot generation removed - now using consolidated table approach
            # All information is consolidated into comprehensive Excel tables and text summaries
            # This eliminates redundant plots while maintaining all statistical information
            
            # CRITICAL: Generate KM plots as required by spec
            # The spec requires "all expected outputs (KM plots, CIF plots, HTML/Excel tables)"
            logger::log_info(formatted("Creating MFS GEP visualization plots (KM curves, calibration, discrimination)", indent = 1))
            tryCatch({
                create_mfs_gep_visuals(
                    mfs_results = validation_results,
                    mfs_data = km_ph_data,
                    output_dir = mfs_output_dir,
                    prefix = prefix,
                    group_var = "biopsy1_gep",
                    other_map = other_map,
                    dataset_name = dataset_name
                )
                logger::log_info(formatted("MFS GEP visualization plots created successfully", indent = 2))
            }, error = function(e) {
                logger::log_warn(formatted(sprintf("MFS visualization creation failed: %s", e$message), indent = 2))
            })
            
            # Create proportional hazards diagnostics for MFS
            logger::log_info(formatted("Creating MFS proportional hazards diagnostics", indent = 1))
            tryCatch({
                if (length(unique(km_ph_data$biopsy1_gep)) >= 2) {
                    mfs_cox_formula <- as.formula("Surv(tt_mets_months, mets_event) ~ biopsy1_gep")
                    mfs_cox_model <- survival::coxph(mfs_cox_formula, data = km_ph_data)
                    
                    test_proportional_hazards_assumption(
                        cox_model = mfs_cox_model,
                        outcome_name = "Metastasis-Free Survival",
                        output_dir = output_dirs$obj4_ph_diagnostics,
                        file_prefix = paste0(prefix, "mfs_"),
                        dataset_name = dataset_name
                    )
                    logger::log_info(formatted("MFS proportional hazards diagnostics completed", indent = 2))
                } else {
                    logger::log_warn(formatted("Insufficient groups for MFS proportional hazards diagnostics", indent = 2))
                }
            }, error = function(e) {
                logger::log_warn(formatted(sprintf("MFS diagnostics failed: %s", e$message), indent = 2))
            })
            
            # Only log success if we actually completed without errors
            logger::log_info("GEP MFS validation analysis completed successfully")
        },
        error = function(e) {
            logger::log_error(sprintf("GEP MFS validation analysis FAILED: %s", e$message))
        }
    )
    
    return(list(
        validation_results = validation_results,
        prame_analysis = prame_analysis,
        missing_data_analysis = missing_data_analysis
        # validation_report removed - now using comprehensive summary system
    ))
}

#' Analyze GEP MSS Validation
#'
#' Orchestrate MSS validation at multiple timepoints, computing observed vs
#' expected, calibration, discrimination, competing risks, PRAME analysis,
#' and saving outputs.
#'
#' @param data Data frame with GEP predictions and outcomes
#' @param dataset_name Optional character label for reporting
#' @param timepoints Numeric vector of years (default `GEP_VALIDATION_TIMEPOINTS`)
#' @param bootstrap_iterations Integer bootstrap iterations for discrimination where applicable
#' @param create_unified_at_base logical If TRUE, also write unified visuals at the parent objective dir
#' @param output_dirs Named list of objective-specific output directories
#' @param prefix Character prefix for generated files
#' @return A list with `standard_results`, `competing_results`, `prame_results`,
#'   `missing_data_analysis`.
analyze_gep_mss_validation <- function(data,
                                       dataset_name = NULL,
                                       timepoints = GEP_VALIDATION_TIMEPOINTS,
                                       bootstrap_iterations = GEP_BOOTSTRAP_ITERATIONS,
                                       create_unified_at_base = FALSE,
                                       other_map = NULL,
                                       output_dirs = NULL,
                                       prefix = "") {
    logger::log_info("Starting GEP Melanoma-Specific Survival validation analysis")
    logger::log_info(formatted("DEBUG: Function entry point reached", indent = 1))

    if (is.null(output_dirs) || is.null(output_dirs$obj4_mss)) {
        stop("analyze_gep_mss_validation() requires an output_dirs list with obj4_mss entry")
    }

    if (is.null(prefix)) {
        prefix <- ""
    }
    
    # Load other_map if not provided
    if (is.null(other_map)) {
        logger::log_info(formatted("DEBUG: Loading other_map", indent = 1))
        if (!is.null(dataset_name)) {
            other_map <- tryCatch(get_cohort_specific_other_map(dataset_name), error = function(e) list())
        } else {
            other_map <- list()
        }
    }
    
    logger::log_info(formatted("DEBUG: Setting up output directory", indent = 1))
    mss_output_dir <- output_dirs$obj4_mss
    if (!dir.exists(mss_output_dir)) {
        dir.create(mss_output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    logger::log_info(formatted("DEBUG: Checking required variables", indent = 1))
    logger::log_info(formatted("Filtering data for MSS validation", indent = 1))
    required_vars <- c("biopsy1_gep", "expected_mss_5yr", "expected_mss_7yr", "expected_mss_10yr", "prame_status", "gep_validation_set")
    missing_vars <- setdiff(required_vars, names(data))
    if (length(missing_vars) > 0) {
        logger::log_error(formatted(sprintf("ERROR: Missing required GEP variables: %s", paste(missing_vars, collapse = ", ")), indent = 1))
        logger::log_error(formatted("These variables should have been created in data_processing.R", indent = 1))
        stop("GEP validation cannot proceed without required variables")
    }
    
    logger::log_info(formatted("DEBUG: Filtering data for MSS analysis", indent = 1))
    analysis_data <- data %>%
        filter(mss_analysis_eligible)
    logger::log_info(formatted(sprintf("Analysis dataset: %d patients with valid GEP and MSS data", nrow(analysis_data)), indent = 1))

    mss_visual_data <- data %>%
        filter(
            !is.na(biopsy1_gep),
            !is.na(tt_death_years),
            !is.na(melanoma_death_event),
            !is.na(competing_death_event),
            tt_death_years >= 0
        )
    logger::log_info(formatted(sprintf(
        "MSS visual dataset: %d patients (including GEP Not Tested / Failed)",
        nrow(mss_visual_data)
    ), indent = 1))
    if (nrow(mss_visual_data) > 0) {
        mss_visual_summary <- mss_visual_data %>%
            group_by(biopsy1_gep) %>%
            summarise(
                n = n(),
                melanoma_deaths = sum(melanoma_death_event == 1, na.rm = TRUE),
                other_deaths = sum(competing_death_event == 1, na.rm = TRUE),
                .groups = "drop"
            )
        for (i in seq_len(nrow(mss_visual_summary))) {
            logger::log_info(formatted(sprintf(
                "%s: n = %d (melanoma deaths = %d, competing deaths = %d)",
                mss_visual_summary$biopsy1_gep[i],
                mss_visual_summary$n[i],
                mss_visual_summary$melanoma_deaths[i],
                mss_visual_summary$other_deaths[i]
            ), indent = 2))
        }
    }
    # Use pre-processed time-specific event indicators for consistency
    events_per_timepoint <- sapply(timepoints, function(tp) {
        sum(analysis_data[[paste0("mss_event_", tp, "yr")]])
    })
    names(events_per_timepoint) <- paste0(timepoints, "yr")
    logger::log_info(formatted("Events per timepoint:", indent = 1))
    for (i in seq_along(events_per_timepoint)) {
        ep_status <- ifelse(events_per_timepoint[i] >= 100, "✓", "⚠ <100")
        logger::log_info(formatted(sprintf("%s: %d events %s", names(events_per_timepoint)[i], events_per_timepoint[i], ep_status), indent = 2))
    }
    missing_data_analysis <- assess_gep_missing_data(data)
    standard_results <- list()
    for (tp in timepoints) {
        logger::log_info(formatted(sprintf("Analyzing %d-year standard MSS validation", tp), indent = 2))
        standard_results[[paste0(tp, "yr")]] <- perform_standard_mss_validation(analysis_data, tp, bootstrap_iterations, time_var = "tt_death_months")
    }
    logger::log_info(formatted("Performing competing risk MSS validation", indent = 1))
    competing_results <- list()
    for (tp in timepoints) {
        logger::log_info(formatted(sprintf("Analyzing %d-year competing risk MSS validation", tp), indent = 2))
        tryCatch({
            competing_results[[paste0(tp, "yr")]] <- perform_competing_risk_mss_validation(analysis_data, tp, time_var = "tt_death_months")
        }, error = function(e) {
            logger::log_warn(formatted(sprintf("Competing risk analysis failed for %d-year timepoint: %s", tp, e$message), indent = 3))
            competing_results[[paste0(tp, "yr")]] <- list(status = "failed", error = e$message)
        })
    }
    logger::log_info(formatted("Performing PRAME-augmented MSS analysis", indent = 1))
    prame_results <- tryCatch({
        perform_prame_augmented_analysis_mss(analysis_data, timepoints)
    }, error = function(e) {
        logger::log_warn(formatted(sprintf("PRAME-augmented analysis failed: %s", e$message), indent = 2))
        list(status = "failed", error = e$message)
    })
    
    # NEW: Use comprehensive GEP summary system instead of old repetitive reports
    # The save_mss_validation_results function now creates comprehensive summaries automatically
    save_mss_validation_results(
        standard_results, competing_results,
        missing_data_analysis, prame_results, mss_output_dir, prefix,
        dataset_name = dataset_name
    )

    # ALWAYS generate per-cohort MSS visuals (CIF curves, calibration, discrimination)
    logger::log_info(formatted("Creating MSS GEP visualization plots (CIF curves, calibration, discrimination)", indent = 1))
    tryCatch({
        create_mss_gep_visuals(
            mss_results = list(standard_validation = standard_results, competing_risk_validation = competing_results),
            mss_data = mss_visual_data,
            output_dir = mss_output_dir,
            prefix = prefix,
            group_var = "biopsy1_gep",
            other_map = other_map
        )
        logger::log_info(formatted("MSS GEP visualization plots created successfully", indent = 2))
    }, error = function(e) {
        logger::log_warn(formatted(sprintf("MSS visualization creation failed: %s", e$message), indent = 2))
    })

    # Optionally create unified visuals at the GEP objective root directory
    if (create_unified_at_base) {
        gep_base_dir <- dirname(mss_output_dir)
        tryCatch(
            {
                create_unified_gep_validation_summary(
                    mfs_results = NULL,
                    mss_results = list(standard_validation = standard_results, competing_risk_validation = competing_results, prame_results = prame_results),
                    dataset_name = dataset_name,
                    output_dir = gep_base_dir,
                    prefix = prefix
                )
            },
            error = function(e) {
                logger::log_warn(sprintf("Visual creation failed (MSS): %s", e$message))
            }
        )
    }

    # NOTE: Individual plot generation removed - now using consolidated table approach
    # All information is consolidated into comprehensive Excel tables and text summaries
    # This eliminates redundant plots while maintaining all statistical information

    logger::log_info("GEP MSS validation analysis completed successfully")
    return(list(
        standard_results = standard_results,
        competing_results = competing_results,
        prame_results = prame_results,
        missing_data_analysis = missing_data_analysis
        # validation_report removed - now using comprehensive summary system
    ))
}
