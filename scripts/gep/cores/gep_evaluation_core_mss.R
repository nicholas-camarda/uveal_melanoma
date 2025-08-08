# GEP MSS Evaluation Core
# Contains MSS model evaluation algorithms (no plotting or I/O)

#' Perform standard MSS validation
#'
#' @param data Prepared MSS data
#' @param timepoint Timepoint in years
#' @param bootstrap_iterations Number of bootstrap iterations
#' @return List with validation results
perform_standard_mss_validation <- function(data, timepoint, bootstrap_iterations) {
    
    log_enhanced(sprintf("Performing standard MSS validation for %d-year timepoint", timepoint), level = "DEBUG")
    
    # Create time-to-event outcome for the specific timepoint
    analysis_data <- data %>%
        mutate(
            time_to_event = pmin(tt_death_years, timepoint),
            event_occurred = melanoma_death_event & (tt_death_years <= timepoint)
        )
    
    # Calculate observed vs expected rates
    observed_expected <- calculate_observed_expected_rates(
        data = analysis_data,
        expected_var = paste0("expected_mss_", timepoint, "yr"),
        event_var = "event_occurred",
        time_var = "time_to_event"
    )
    
    # Calculate calibration metrics
    calibration_metrics <- calculate_calibration_metrics(
        data = analysis_data,
        expected_var = paste0("expected_mss_", timepoint, "yr"),
        event_var = "event_occurred",
        time_var = "time_to_event"
    )
    
    # Calculate discrimination metrics
    discrimination_metrics <- calculate_discrimination_metrics(
        data = analysis_data,
        expected_var = paste0("expected_mss_", timepoint, "yr"),
        event_var = "event_occurred",
        time_var = "time_to_event",
        bootstrap_iterations = bootstrap_iterations
    )
    
    return(list(
        observed_expected = observed_expected,
        calibration = calibration_metrics,
        discrimination = discrimination_metrics,
        timepoint = timepoint
    ))
}

#' Perform competing risk MSS validation
#'
#' @param data Prepared MSS data
#' @param timepoint Timepoint in years
#' @return List with competing risk results
perform_competing_risk_mss_validation <- function(data, timepoint) {
    
    log_enhanced(sprintf("Performing competing risk MSS validation for %d-year timepoint", timepoint), level = "DEBUG")
    
    # Create competing risk outcome
    analysis_data <- data %>%
        mutate(
            time_to_event = pmin(tt_death_years, timepoint),
            event_type = case_when(
                melanoma_death_event == 1 & tt_death_years <= timepoint ~ 1,  # Melanoma death
                competing_death_event == 1 & tt_death_years <= timepoint ~ 2,  # Competing death
                TRUE ~ 0  # Censored
            )
        )
    
    # Calculate cumulative incidence functions
    cumulative_incidence <- calculate_cumulative_incidence(
        data = analysis_data,
        time_var = "time_to_event",
        event_var = "event_type",
        group_var = "gep_class_simple"
    )
    
    # Calculate cause-specific hazard ratios
    cause_specific_hazards <- calculate_cause_specific_hazards(
        data = analysis_data,
        time_var = "time_to_event",
        event_var = "event_type",
        group_var = "gep_class_simple"
    )
    
    return(list(
        cumulative_incidence = cumulative_incidence,
        cause_specific_hazards = cause_specific_hazards,
        timepoint = timepoint
    ))
}

#' Perform PRAME-augmented analysis for MSS
#'
#' @param data Prepared MSS data
#' @param timepoints Vector of timepoints
#' @return List with PRAME analysis results
perform_prame_augmented_analysis_mss <- function(data, timepoints) {
    
    log_enhanced("Performing PRAME-augmented MSS analysis", level = "DEBUG")
    
    # Check if PRAME data is available
    if (!"prame_status" %in% names(data)) {
        log_enhanced("PRAME status not available, skipping PRAME-augmented analysis", level = "WARN")
        return(NULL)
    }
    
    # Calculate net reclassification index for each timepoint
    nri_results <- list()
    
    for (timepoint in timepoints) {
        analysis_data <- data %>%
            mutate(
                time_to_event = pmin(tt_death_years, timepoint),
                event_occurred = melanoma_death_event & (tt_death_years <= timepoint)
            )
        
        # Calculate NRI comparing GEP-only vs GEP+PRAME
        nri_result <- calculate_net_reclassification_index(
            data = analysis_data,
            base_pred = paste0("expected_mss_", timepoint, "yr"),
            enhanced_pred = paste0("expected_mss_", timepoint, "yr"),  # Placeholder - would need PRAME-augmented predictions
            event_var = "event_occurred"
        )
        
        nri_results[[paste0("yr", timepoint)]] <- nri_result
    }
    
    return(nri_results)
}
