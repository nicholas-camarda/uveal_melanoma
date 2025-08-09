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
#' @return A list with `validation_results`, `prame_analysis`, `missing_data_analysis`, and `validation_report`.
analyze_gep_mfs_validation <- function(data, dataset_name = NULL, timepoints = GEP_VALIDATION_TIMEPOINTS, bootstrap_iterations = GEP_BOOTSTRAP_ITERATIONS) {
    log_enhanced("Starting GEP Metastasis-Free Survival validation analysis", level = "INFO")
    mfs_output_dir <- output_dirs$obj4_mfs
    if (!dir.exists(mfs_output_dir)) {
        dir.create(mfs_output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    log_enhanced("Reporting GEP validation dataset distribution", level = "INFO", indent = 1)
    gep_distribution <- data %>%
        count(gep_validation_set, gep_class_simple) %>%
        tidyr::pivot_wider(names_from = gep_class_simple, values_from = n, values_fill = 0)
    log_enhanced("GEP Validation Set Distribution:", level = "INFO", indent = 1)
    for (i in seq_len(nrow(gep_distribution))) {
        set_name <- gep_distribution$gep_validation_set[i]
        class_1a <- ifelse(is.na(gep_distribution$`Class 1A`[i]), 0, gep_distribution$`Class 1A`[i])
        class_1b <- ifelse(is.na(gep_distribution$`Class 1B`[i]), 0, gep_distribution$`Class 1B`[i])
        class_2 <- ifelse(is.na(gep_distribution$`Class 2`[i]), 0, gep_distribution$`Class 2`[i])
        total <- class_1a + class_1b + class_2
        log_enhanced(sprintf("%s: %d patients (1A:%d, 1B:%d, 2:%d)", set_name, total, class_1a, class_1b, class_2), level = "INFO", indent = 2)
    }
    write_xlsx(gep_distribution, file.path(mfs_output_dir, paste0(prefix, "gep_validation_distribution.xlsx")))
    log_enhanced("Filtering data for MFS validation", level = "INFO", indent = 1)
    required_vars <- c("gep_class_simple", "expected_mfs_5yr", "expected_mfs_7yr", "expected_mfs_10yr", "prame_status", "gep_validation_set")
    missing_vars <- setdiff(required_vars, names(data))
    if (length(missing_vars) > 0) {
        log_enhanced(sprintf("ERROR: Missing required GEP variables: %s", paste(missing_vars, collapse = ", ")), level = "ERROR", indent = 1)
        log_enhanced("These variables should have been created in data_processing.R", level = "ERROR", indent = 1)
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
    log_enhanced(sprintf("Analysis dataset: %d patients with valid GEP and MFS data", nrow(analysis_data)), level = "INFO", indent = 1)
    events_per_timepoint <- sapply(timepoints, function(tp) {
        tp_months <- tp * 12
        sum(analysis_data$mets_event == 1 & analysis_data$tt_mets_months <= tp_months)
    })
    names(events_per_timepoint) <- paste0(timepoints, "yr")
    log_enhanced("Events per timepoint:", level = "INFO", indent = 1)
    for (i in seq_along(events_per_timepoint)) {
        ep_status <- ifelse(events_per_timepoint[i] >= 100, "✓", "⚠ <100")
        log_enhanced(sprintf("%s: %d events %s", names(events_per_timepoint)[i], events_per_timepoint[i], ep_status), level = "INFO", indent = 2)
    }
    missing_data_analysis <- assess_gep_missing_data(data)
    validation_results <- list()
    for (tp in timepoints) {
        log_enhanced(sprintf("Analyzing %d-year MFS validation", tp), level = "INFO", indent = 1)
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
    log_enhanced("Performing PRAME-augmented analysis", level = "INFO", indent = 1)
    prame_analysis <- perform_prame_augmented_analysis_mfs(analysis_data, timepoints)
    validation_report <- create_mfs_validation_report(validation_results, prame_analysis, missing_data_analysis, dataset_name)
    save_mfs_validation_results(validation_results, validation_report, missing_data_analysis, prame_analysis, mfs_output_dir, prefix)
    log_enhanced("GEP MFS validation analysis completed successfully", level = "INFO")
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
#' @return A list with `standard_results`, `competing_results`, `prame_results`,
#'   `missing_data_analysis`, and `validation_report`.
analyze_gep_mss_validation <- function(data, dataset_name = NULL, timepoints = GEP_VALIDATION_TIMEPOINTS, bootstrap_iterations = GEP_BOOTSTRAP_ITERATIONS) {
    log_enhanced("Starting GEP Melanoma-Specific Survival validation analysis", level = "INFO")
    mss_output_dir <- output_dirs$obj4_mss
    if (!dir.exists(mss_output_dir)) {
        dir.create(mss_output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    log_enhanced("Filtering data for MSS validation", level = "INFO", indent = 1)
    required_vars <- c("gep_class_simple", "expected_mss_5yr", "expected_mss_7yr", "expected_mss_10yr", "prame_status", "gep_validation_set")
    missing_vars <- setdiff(required_vars, names(data))
    if (length(missing_vars) > 0) {
        log_enhanced(sprintf("ERROR: Missing required GEP variables: %s", paste(missing_vars, collapse = ", ")), level = "ERROR", indent = 1)
        log_enhanced("These variables should have been created in data_processing.R", level = "ERROR", indent = 1)
        stop("GEP validation cannot proceed without required variables")
    }
    analysis_data <- prepare_mss_competing_risk_data(data)
    log_enhanced(sprintf("Analysis dataset: %d patients with valid GEP and MSS data", nrow(analysis_data)), level = "INFO", indent = 1)
    events_per_timepoint <- sapply(timepoints, function(tp) {
        tp_months <- tp * 12
        sum(analysis_data$death_event == 1 & analysis_data$tt_death_months <= tp_months)
    })
    names(events_per_timepoint) <- paste0(timepoints, "yr")
    log_enhanced("Events per timepoint:", level = "INFO", indent = 1)
    for (i in seq_along(events_per_timepoint)) {
        ep_status <- ifelse(events_per_timepoint[i] >= 100, "✓", "⚠ <100")
        log_enhanced(sprintf("%s: %d events %s", names(events_per_timepoint)[i], events_per_timepoint[i], ep_status), level = "INFO", indent = 2)
    }
    missing_data_analysis <- assess_gep_missing_data(data)
    standard_results <- list()
    for (tp in timepoints) {
        log_enhanced(sprintf("Analyzing %d-year standard MSS validation", tp), level = "INFO", indent = 2)
        standard_results[[paste0(tp, "yr")]] <- perform_standard_mss_validation(analysis_data, tp, bootstrap_iterations)
    }
    log_enhanced("Performing competing risk MSS validation", level = "INFO", indent = 1)
    competing_results <- list()
    for (tp in timepoints) {
        log_enhanced(sprintf("Analyzing %d-year competing risk MSS validation", tp), level = "INFO", indent = 2)
        competing_results[[paste0(tp, "yr")]] <- perform_competing_risk_mss_validation(analysis_data, tp)
    }
    log_enhanced("Performing PRAME-augmented MSS analysis", level = "INFO", indent = 1)
    prame_results <- perform_prame_augmented_analysis_mss(analysis_data, timepoints)
    validation_report <- create_mss_validation_report(standard_results, competing_results, prame_results, missing_data_analysis, dataset_name)
    save_mss_validation_results(
        standard_results, competing_results, validation_report,
        missing_data_analysis, prame_results, mss_output_dir, prefix
    )
    log_enhanced("GEP MSS validation analysis completed successfully", level = "INFO")
    return(list(
        standard_results = standard_results,
        competing_results = competing_results,
        prame_results = prame_results,
        missing_data_analysis = missing_data_analysis,
        validation_report = validation_report
    ))
}
