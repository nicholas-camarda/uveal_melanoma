# GEP Validation Analysis
# Author: Nicholas Camarda
# Description: Advanced validation of Gene Expression Profile predictions
#              incorporating state-of-the-art survival model validation methods

# Note: All required libraries loaded in main.R
# Additional libraries may be needed: pec, survcomp, riskRegression, cmprsk, rms, pROC

#' Advanced GEP Metastasis-Free Survival Validation
#'
#' Performs comprehensive validation of GEP predictions for metastasis-free survival
#' using multiple timepoints and advanced survival validation metrics.
#' Uses centralized constants from analysis_config.R for timepoints and bootstrap iterations.
#'
#' @param data Data frame with GEP and survival data
#' @param dataset_name Character string identifying the cohort
#' @param timepoints Numeric vector of timepoints in years (uses GEP_VALIDATION_TIMEPOINTS from config)
#' @param bootstrap_iterations Number of bootstrap samples (uses GEP_BOOTSTRAP_ITERATIONS from config)
#' @return List containing validation results, plots, and tables
analyze_gep_mfs_validation <- function(data, dataset_name = NULL, timepoints = GEP_VALIDATION_TIMEPOINTS, bootstrap_iterations = GEP_BOOTSTRAP_ITERATIONS) {
    
    log_enhanced("Starting GEP Metastasis-Free Survival validation analysis", level = "INFO")
    
    # Create output directories
    mfs_output_dir <- output_dirs$obj4_mfs
    if (!dir.exists(mfs_output_dir)) {
        dir.create(mfs_output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Report training/testing distribution first
    log_enhanced("Reporting GEP validation dataset distribution", level = "INFO", indent = 1)
    
    gep_distribution <- data %>%
        count(gep_validation_set, gep_class_simple) %>%
        pivot_wider(names_from = gep_class_simple, values_from = n, values_fill = 0)
    
    log_enhanced("GEP Validation Set Distribution:", level = "INFO", indent = 1)
    for (i in 1:nrow(gep_distribution)) {
        set_name <- gep_distribution$gep_validation_set[i]
        class_1a <- ifelse(is.na(gep_distribution$`Class 1A`[i]), 0, gep_distribution$`Class 1A`[i])
        class_1b <- ifelse(is.na(gep_distribution$`Class 1B`[i]), 0, gep_distribution$`Class 1B`[i])
        class_2 <- ifelse(is.na(gep_distribution$`Class 2`[i]), 0, gep_distribution$`Class 2`[i])
        total <- class_1a + class_1b + class_2
        
        log_enhanced(sprintf("%s: %d patients (1A:%d, 1B:%d, 2:%d)", 
                            set_name, total, class_1a, class_1b, class_2), 
                    level = "INFO", indent = 2)
    }
    
    # Save distribution table
    write_xlsx(gep_distribution, 
               file.path(mfs_output_dir, paste0(prefix, "gep_validation_distribution.xlsx")))
    
    # Data preparation and quality checks
    log_enhanced("Filtering data for MFS validation", level = "INFO", indent = 1)
    
    # Verify required GEP variables exist (should have been created in data_processing.R)
    required_vars <- c("gep_class_simple", "expected_mfs_5yr", "expected_mfs_7yr", "expected_mfs_10yr", "prame_status", "gep_validation_set")
    missing_vars <- setdiff(required_vars, names(data))
    
    if (length(missing_vars) > 0) {
        log_enhanced(sprintf("ERROR: Missing required GEP variables: %s", paste(missing_vars, collapse = ", ")), level = "ERROR", indent = 1)
        log_enhanced("These variables should have been created in data_processing.R", level = "ERROR", indent = 1)
        stop("GEP validation cannot proceed without required variables")
    }
    
    # Filter patients with valid GEP and survival data (filter only, no mutations)
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
            # Use the pre-created gep_class_simple to filter valid classes
            gep_class_simple %in% c("Class 1A", "Class 1B", "Class 2")
        )
    
    log_enhanced(sprintf("Analysis dataset: %d patients with valid GEP and MFS data", nrow(analysis_data)), level = "INFO", indent = 1)
    
    # Events-per-endpoint analysis (convert timepoints from years to months for comparison)
    events_per_timepoint <- sapply(timepoints, function(tp) {
        tp_months <- tp * 12  # Convert years to months
        sum(analysis_data$mets_event == 1 & analysis_data$tt_mets_months <= tp_months)
    })
    names(events_per_timepoint) <- paste0(timepoints, "yr")
    
    log_enhanced("Events per timepoint:", level = "INFO", indent = 1)
    for(i in seq_along(events_per_timepoint)) {
        ep_status <- ifelse(events_per_timepoint[i] >= 100, "✓", "⚠ <100")
        log_enhanced(sprintf("%s: %d events %s", names(events_per_timepoint)[i], 
                           events_per_timepoint[i], ep_status), level = "INFO", indent = 2)
    }
    
    # Missing data assessment
    missing_data_analysis <- assess_gep_missing_data(data)
    
    # Primary validation analyses for each timepoint
    validation_results <- list()
    
    for(timepoint in timepoints) {
        log_enhanced(sprintf("Analyzing %d-year MFS validation", timepoint), level = "PROGRESS", indent = 1)
        
        tp_key <- paste0("yr", timepoint)
        
        # Calculate observed vs expected rates
        obs_exp_results <- calculate_observed_expected_mfs(analysis_data, timepoint)
        
        # Advanced calibration assessment
        calibration_results <- perform_advanced_calibration_mfs(analysis_data, timepoint, bootstrap_iterations)
        
        # Enhanced discrimination analysis
        discrimination_results <- perform_enhanced_discrimination_mfs(analysis_data, timepoint)
        
        # Decision curve analysis
        dca_results <- perform_decision_curve_analysis_mfs(analysis_data, timepoint)
        
        # Store results
        validation_results[[tp_key]] <- list(
            timepoint = timepoint,
            observed_expected = obs_exp_results,
            calibration = calibration_results,
            discrimination = discrimination_results,
            decision_curve = dca_results,
            events = events_per_timepoint[paste0(timepoint, "yr")]
        )
    }
    
    # PRAME-augmented analysis (secondary)
    prame_analysis <- perform_prame_augmented_analysis_mfs(analysis_data, timepoints)
    
    # Create comprehensive validation report
    validation_report <- create_mfs_validation_report(validation_results, prame_analysis, missing_data_analysis, dataset_name)
    
    # Save all results
    save_mfs_validation_results(validation_results, validation_report, missing_data_analysis, prame_analysis, mfs_output_dir, prefix)
    
    log_enhanced("GEP MFS validation analysis completed", level = "INFO")
    
    return(list(
        validation_results = validation_results,
        prame_analysis = prame_analysis,
        missing_data_analysis = missing_data_analysis,
        validation_report = validation_report
    ))
}

#' Advanced GEP Melanoma-Specific Survival Validation
#'
#' Performs comprehensive validation including competing risk analysis
#' for melanoma-specific survival predictions.
#' Uses centralized constants from analysis_config.R for timepoints and bootstrap iterations.
#'
#' @param data Data frame with GEP and survival data (pre-processed, no modifications allowed)
#' @param dataset_name Character string identifying the cohort
#' @param timepoints Numeric vector of timepoints in years (uses GEP_VALIDATION_TIMEPOINTS from config)
#' @param bootstrap_iterations Number of bootstrap samples (uses GEP_BOOTSTRAP_ITERATIONS from config)
#' @return List containing validation results including competing risk models
analyze_gep_mss_validation <- function(data, dataset_name = NULL, timepoints = GEP_VALIDATION_TIMEPOINTS, bootstrap_iterations = GEP_BOOTSTRAP_ITERATIONS) {
    
    log_enhanced("Starting GEP Melanoma-Specific Survival validation analysis", level = "INFO")
    
    # Create output directories
    mss_output_dir <- output_dirs$obj4_mss
    if (!dir.exists(mss_output_dir)) {
        dir.create(mss_output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Data preparation with competing risk considerations
    log_enhanced("Preparing data for MSS validation with competing risk analysis", level = "INFO", indent = 1)
    
    # Verify required GEP variables exist (should have been created in data_processing.R)
    required_vars <- c("gep_class_simple", "expected_mss_5yr", "expected_mss_7yr", "expected_mss_10yr", "prame_status")
    missing_vars <- setdiff(required_vars, names(data))
    
    if (length(missing_vars) > 0) {
        log_enhanced(sprintf("ERROR: Missing required GEP variables: %s", paste(missing_vars, collapse = ", ")), level = "ERROR", indent = 1)
        log_enhanced("These variables should have been created in data_processing.R", level = "ERROR", indent = 1)
        stop("GEP validation cannot proceed without required variables")
    }
    
    # Need to determine melanoma-specific death vs other causes
    # This will require examination of cause of death variables
    analysis_data <- prepare_mss_competing_risk_data(data)
    
    if (nrow(analysis_data) == 0) {
        log_enhanced("No patients with valid MSS data for competing risk analysis", level = "WARN")
        return(NULL)
    }
    
    # Events-per-endpoint analysis for melanoma-specific deaths
    melanoma_events_per_timepoint <- sapply(timepoints, function(tp) {
        sum(analysis_data$melanoma_death_event == 1 & analysis_data$tt_death_years <= tp)
    })
    names(melanoma_events_per_timepoint) <- paste0(timepoints, "yr_melanoma")
    
    # Standard survival analysis (ignoring competing risks)
    standard_validation_results <- list()
    
    # Competing risk analysis (Fine-Gray models)
    competing_risk_results <- list()
    
    for(timepoint in timepoints) {
        log_enhanced(sprintf("Analyzing %d-year MSS validation", timepoint), level = "PROGRESS", indent = 1)
        
        tp_key <- paste0("yr", timepoint)
        
        # Standard survival analysis
        standard_results <- perform_standard_mss_validation(analysis_data, timepoint, bootstrap_iterations)
        standard_validation_results[[tp_key]] <- standard_results
        
        # Competing risk analysis
        competing_results <- perform_competing_risk_mss_validation(analysis_data, timepoint)
        competing_risk_results[[tp_key]] <- competing_results
    }
    
    # PRAME-augmented analysis for MSS
    prame_mss_analysis <- perform_prame_augmented_analysis_mss(analysis_data, timepoints)
    
    # Missing data assessment (same as MFS)
    missing_data_analysis <- assess_gep_missing_data(data)
    
    # Create comprehensive MSS validation report
    mss_validation_report <- create_mss_validation_report(
        standard_validation_results, 
        competing_risk_results, 
        prame_mss_analysis, 
        missing_data_analysis, 
        dataset_name
    )
    
    # Save all MSS results
    save_mss_validation_results(
        standard_validation_results, 
        competing_risk_results, 
        mss_validation_report, 
        missing_data_analysis, 
        prame_mss_analysis, 
        mss_output_dir, 
        prefix
    )
    
    log_enhanced("GEP MSS validation analysis completed", level = "INFO")
    
    return(list(
        standard_validation = standard_validation_results,
        competing_risk_validation = competing_risk_results,
        prame_analysis = prame_mss_analysis,
        missing_data_analysis = missing_data_analysis,
        validation_report = mss_validation_report
    ))
}

# =============================================================================
# HELPER FUNCTIONS FOR ADVANCED VALIDATION METHODS
# =============================================================================

#' Calculate Observed vs Expected Rates with Poisson Confidence Intervals
calculate_observed_expected_mfs <- function(data, timepoint) {
    # Implementation for exact Poisson CIs and chi-square tests
    # This function will calculate O/E ratios by GEP class with proper statistical testing
}

#' Perform Advanced Calibration Assessment
perform_advanced_calibration_mfs <- function(data, timepoint, bootstrap_iterations) {
    # Implementation for:
    # - Nam-D'Agostino χ² test
    # - Integrated Calibration Index
    # - Loess-smoothed calibration plot with 95% confidence bands
    # - Bootstrap-shrunk calibration slope and intercept
}

#' Enhanced Discrimination Analysis
perform_enhanced_discrimination_mfs <- function(data, timepoint) {
    # Implementation for:
    # - Harrell's C-index
    # - Uno's censoring-adjusted C-index
    # - Cumulative/dynamic ROC curves
}

#' Decision Curve Analysis
perform_decision_curve_analysis_mfs <- function(data, timepoint) {
    # Implementation for net clinical benefit across risk thresholds
}

#' PRAME-Augmented Analysis with Net Reclassification Index
perform_prame_augmented_analysis_mfs <- function(data, timepoints) {
    # Implementation for PRAME-enhanced models and NRI calculation
}

#' Missing Data Assessment and Multiple Imputation
assess_gep_missing_data <- function(data) {
    # Implementation for:
    # - Baseline comparison of GEP-tested vs missing
    # - Multiple imputation sensitivity analysis
    # - Assessment of informative missingness
}

#' Prepare MSS Data with Competing Risk Variables
prepare_mss_competing_risk_data <- function(data) {
    # Implementation for creating competing risk variables
    # Need to identify melanoma-specific deaths vs other causes
    
    # For now, create a basic implementation that returns the data
    # This should be expanded to properly handle competing risks
    
    # Filter for patients with valid GEP MSS data
    analysis_data <- data %>%
        filter(
            !is.na(expected_mss_5yr),
            !is.na(tt_death_months),
            !is.na(death_event)
        ) %>%
        mutate(
            # Convert months to years
            tt_death_years = tt_death_months / 12,
            # For basic implementation, treat all deaths as melanoma-specific
            # This should be refined based on cause of death variables
            melanoma_death_event = death_event,
            other_death_event = 0  # Placeholder for other causes
        )
    
    return(analysis_data)
}

#' Standard MSS Validation (Kaplan-Meier approach)
perform_standard_mss_validation <- function(data, timepoint, bootstrap_iterations) {
    # Same methods as MFS but for melanoma-specific survival
    
    # Basic implementation - return placeholder results
    log_enhanced(sprintf("Basic MSS validation for %d years", timepoint), level = "INFO", indent = 2)
    
    return(list(
        timepoint = timepoint,
        n_patients = nrow(data),
        events = sum(data$melanoma_death_event),
        status = "basic_implementation"
    ))
}

#' Competing Risk MSS Validation (Fine-Gray approach)
perform_competing_risk_mss_validation <- function(data, timepoint) {
    # Implementation for Fine-Gray subdistribution hazards models
    # and cumulative incidence functions
    
    # Basic implementation - return placeholder results
    return(list(
        timepoint = timepoint,
        method = "competing_risk",
        status = "basic_implementation"
    ))
}

#' PRAME-Augmented MSS Analysis
perform_prame_augmented_analysis_mss <- function(data, timepoints) {
    # PRAME analysis for MSS outcomes
    
    # Basic implementation - return placeholder results
    return(list(
        prame_analysis = "basic_implementation",
        timepoints = timepoints
    ))
}

# =============================================================================
# REPORTING AND OUTPUT FUNCTIONS
# =============================================================================

#' Create Comprehensive MFS Validation Report
create_mfs_validation_report <- function(validation_results, prame_analysis, missing_data_analysis, dataset_name) {
    # Create summary tables and interpretation text
}

#' Create Comprehensive MSS Validation Report  
create_mss_validation_report <- function(standard_results, competing_results, prame_analysis, missing_data_analysis, dataset_name) {
    # Create summary tables for both standard and competing risk analyses
    
    # Basic implementation - return placeholder report
    return(list(
        dataset_name = dataset_name,
        report_type = "MSS_validation",
        status = "basic_implementation"
    ))
}

#' Save All MFS Validation Results
save_mfs_validation_results <- function(validation_results, validation_report, missing_data_analysis, prame_analysis, output_dir, prefix) {
    # Save all tables, plots, and reports for MFS validation
}

#' Save All MSS Validation Results
save_mss_validation_results <- function(standard_results, competing_results, validation_report, missing_data_analysis, prame_analysis, output_dir, prefix) {
    # Save all tables, plots, and reports for MSS validation
    
    # Basic implementation - create placeholder files
    log_enhanced("Saving MSS validation results (basic implementation)", level = "INFO", indent = 1)
    
    # Create summary file
    summary_file <- file.path(output_dir, paste0(prefix, "mss_validation_summary.txt"))
    writeLines(
        c("MSS Validation Analysis - Basic Implementation",
          paste("Analysis completed at:", Sys.time()),
          paste("Number of standard validation results:", length(standard_results)),
          paste("Number of competing risk results:", length(competing_results))),
        summary_file
    )
    
    return(invisible(NULL))
} 