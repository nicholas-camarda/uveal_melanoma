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
#' @param bootstrap_iterations Integer bootstrap iterations for calibration slope
#' @param create_unified_at_base logical If TRUE, also write unified visuals at the parent objective dir
#' @return A list with `validation_results`, `prame_analysis`, `missing_data_analysis`, and `validation_report`.
analyze_gep_mfs_validation <- function(data, dataset_name = NULL, timepoints = GEP_VALIDATION_TIMEPOINTS, bootstrap_iterations = GEP_BOOTSTRAP_ITERATIONS, create_unified_at_base = FALSE) {
    logger::log_info("Starting GEP Metastasis-Free Survival validation analysis")
    mfs_output_dir <- output_dirs$obj4_mfs
    if (!dir.exists(mfs_output_dir)) {
        dir.create(mfs_output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    logger::log_info(formatted("Reporting GEP validation dataset distribution", indent = 1))
    gep_distribution <- data %>%
        count(gep_validation_set, gep_class_simple) %>%
        tidyr::pivot_wider(names_from = gep_class_simple, values_from = n, values_fill = 0)
    logger::log_info(formatted("GEP Validation Set Distribution:", indent = 1))
    for (i in seq_len(nrow(gep_distribution))) {
        set_name <- gep_distribution$gep_validation_set[i]
        class_1a <- ifelse(is.na(gep_distribution$`Class 1A`[i]), 0, gep_distribution$`Class 1A`[i])
        class_1b <- ifelse(is.na(gep_distribution$`Class 1B`[i]), 0, gep_distribution$`Class 1B`[i])
        class_2 <- ifelse(is.na(gep_distribution$`Class 2`[i]), 0, gep_distribution$`Class 2`[i])
        total <- class_1a + class_1b + class_2
        logger::log_info(formatted(sprintf("%s: %d patients (1A:%d, 1B:%d, 2:%d)", set_name, total, class_1a, class_1b, class_2), indent = 2))
    }
    write_xlsx(gep_distribution, file.path(mfs_output_dir, paste0(prefix, "gep_validation_distribution.xlsx")))
    logger::log_info(formatted("Filtering data for MFS validation", indent = 1))
    required_vars <- c("gep_class_simple", "expected_mfs_5yr", "expected_mfs_7yr", "expected_mfs_10yr", "prame_status", "gep_validation_set")
    missing_vars <- setdiff(required_vars, names(data))
    if (length(missing_vars) > 0) {
        logger::log_error(formatted(sprintf("ERROR: Missing required GEP variables: %s", paste(missing_vars, collapse = ", ")), indent = 1))
        logger::log_error(formatted("These variables should have been created in data_processing.R", indent = 1))
        stop("GEP validation cannot proceed without required variables")
    }
    analysis_data <- data %>%
        filter(
            !is.na(biopsy1_gep),
            !is.na(biopsy1_gep_mfs),
            biopsy1_gep != "Failed",
            biopsy1_gep != "Unknown",
            !is.na(tt_mets_months),
            !is.na(mets_event),
            tt_mets_months >= 0,
            biopsy1_gep_mfs >= 0 & biopsy1_gep_mfs <= 1,
            gep_class_simple %in% c("Class 1A", "Class 1B", "Class 2")
        )
    logger::log_info(formatted(sprintf("Analysis dataset: %d patients with valid GEP and MFS data", nrow(analysis_data)), indent = 1))
    events_per_timepoint <- sapply(timepoints, function(tp) {
        tp_months <- tp * 12
        sum(analysis_data$mets_event == 1 & analysis_data$tt_mets_months <= tp_months)
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
    prame_analysis <- perform_prame_augmented_analysis_mfs(analysis_data, timepoints)
    validation_report <- create_mfs_validation_report(validation_results, prame_analysis, missing_data_analysis, dataset_name)
    save_mfs_validation_results(validation_results, validation_report, missing_data_analysis, prame_analysis, mfs_output_dir, prefix)

    # Optionally create unified visuals at the GEP objective root directory
    if (isTRUE(create_unified_at_base)) {
        gep_base_dir <- dirname(mfs_output_dir)
        tryCatch({
            create_unified_gep_validation_summary(
                mfs_results = list(validation_results = validation_results, prame_analysis = prame_analysis),
                mss_results = NULL,
                dataset_name = dataset_name,
                output_dir = gep_base_dir,
                prefix = prefix
            )
            create_gep_validation_visuals(
                mfs_results = list(validation_results = validation_results, prame_analysis = prame_analysis),
                mss_results = NULL,
                output_dir = mfs_output_dir,
                prefix = prefix
            )
        }, error = function(e) {
            logger::log_warn(sprintf("Visual creation failed (MFS): %s", e$message))
        })
    }

    logger::log_info("GEP MFS validation analysis completed successfully")
    return(list(
        validation_results = validation_results,
        prame_analysis = prame_analysis,
        missing_data_analysis = missing_data_analysis,
        validation_report = validation_report
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
#' @return A list with `standard_results`, `competing_results`, `prame_results`,
#'   `missing_data_analysis`, and `validation_report`.
analyze_gep_mss_validation <- function(data, dataset_name = NULL, timepoints = GEP_VALIDATION_TIMEPOINTS, bootstrap_iterations = GEP_BOOTSTRAP_ITERATIONS, create_unified_at_base = FALSE) {
    logger::log_info("Starting GEP Melanoma-Specific Survival validation analysis")
    mss_output_dir <- output_dirs$obj4_mss
    if (!dir.exists(mss_output_dir)) {
        dir.create(mss_output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    logger::log_info(formatted("Filtering data for MSS validation", indent = 1))
    required_vars <- c("gep_class_simple", "expected_mss_5yr", "expected_mss_7yr", "expected_mss_10yr", "prame_status", "gep_validation_set")
    missing_vars <- setdiff(required_vars, names(data))
    if (length(missing_vars) > 0) {
        logger::log_error(formatted(sprintf("ERROR: Missing required GEP variables: %s", paste(missing_vars, collapse = ", ")), indent = 1))
        logger::log_error(formatted("These variables should have been created in data_processing.R", indent = 1))
        stop("GEP validation cannot proceed without required variables")
    }
    analysis_data <- prepare_mss_competing_risk_data(data)
    logger::log_info(formatted(sprintf("Analysis dataset: %d patients with valid GEP and MSS data", nrow(analysis_data)), indent = 1))
    events_per_timepoint <- sapply(timepoints, function(tp) {
        tp_months <- tp * 12
        sum(analysis_data$death_event == 1 & analysis_data$tt_death_months <= tp_months)
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
        standard_results[[paste0(tp, "yr")]] <- perform_standard_mss_validation(analysis_data, tp, bootstrap_iterations)
    }
    logger::log_info(formatted("Performing competing risk MSS validation", indent = 1))
    competing_results <- list()
    for (tp in timepoints) {
        logger::log_info(formatted(sprintf("Analyzing %d-year competing risk MSS validation", tp), indent = 2))
        competing_results[[paste0(tp, "yr")]] <- perform_competing_risk_mss_validation(analysis_data, tp)
    }
    logger::log_info(formatted("Performing PRAME-augmented MSS analysis", indent = 1))
    prame_results <- perform_prame_augmented_analysis_mss(analysis_data, timepoints)
    validation_report <- create_mss_validation_report(standard_results, competing_results, prame_results, missing_data_analysis, dataset_name)
    save_mss_validation_results(
        standard_results, competing_results, validation_report,
        missing_data_analysis, prame_results, mss_output_dir, prefix
    )

    # Optionally create unified visuals at the GEP objective root directory
    if (isTRUE(create_unified_at_base)) {
        gep_base_dir <- dirname(mss_output_dir)
        tryCatch({
            create_unified_gep_validation_summary(
                mfs_results = NULL,
                mss_results = list(standard_validation = standard_results, competing_risk_validation = competing_results, prame_results = prame_results),
                dataset_name = dataset_name,
                output_dir = gep_base_dir,
                prefix = prefix
            )
            create_gep_validation_visuals(
                mfs_results = NULL,
                mss_results = list(standard_validation = standard_results, competing_risk_validation = competing_results, prame_results = prame_results),
                output_dir = mss_output_dir,
                prefix = prefix
            )
        }, error = function(e) {
            logger::log_warn(sprintf("Visual creation failed (MSS): %s", e$message))
        })
    }

    logger::log_info("GEP MSS validation analysis completed successfully")
    return(list(
        standard_results = standard_results,
        competing_results = competing_results,
        prame_results = prame_results,
        missing_data_analysis = missing_data_analysis,
        validation_report = validation_report
    ))
}
