# GEP Validation Core Functions
# Author: Nicholas Camarda
# Description: Core GEP validation orchestration functions
#              Main validation functions for MFS and MSS analysis

#' GEP Metastasis-Free Survival Validation
#'
#' Performs comprehensive validation of GEP predictions for metastasis-free survival
#' using multiple timepoints and advanced survival validation metrics.
#' Uses centralized constants from config_constants.R for timepoints and bootstrap iterations.
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
    
    for (tp in timepoints) {
        log_enhanced(sprintf("Analyzing %d-year MFS validation", tp), level = "INFO", indent = 1)
        
        # Observed vs Expected analysis
        oe_results <- calculate_observed_expected_mfs(analysis_data, tp)
        
        # Calibration analysis
        calibration_results <- perform_calibration_mfs(analysis_data, tp, bootstrap_iterations)
        
        # Discrimination analysis
        discrimination_results <- perform_discrimination_mfs(analysis_data, tp)
        
        # Decision curve analysis
        dca_results <- perform_decision_curve_analysis_mfs(analysis_data, tp)
        
        # Store results
        validation_results[[paste0(tp, "yr")]] <- list(
            observed_expected = oe_results,
            calibration = calibration_results,
            discrimination = discrimination_results,
            decision_curve = dca_results
        )
    }
    
    # PRAME-augmented analysis
    log_enhanced("Performing PRAME-augmented analysis", level = "INFO", indent = 1)
    prame_analysis <- perform_prame_augmented_analysis_mfs(analysis_data, timepoints)
    
    # Create validation report
    validation_report <- create_mfs_validation_report(validation_results, prame_analysis, missing_data_analysis, dataset_name)
    
    # Save results
    save_mfs_validation_results(validation_results, validation_report, missing_data_analysis, prame_analysis, mfs_output_dir, prefix)
    
    log_enhanced("GEP MFS validation analysis completed successfully", level = "INFO")
    
    return(list(
        validation_results = validation_results,
        prame_analysis = prame_analysis,
        missing_data_analysis = missing_data_analysis,
        validation_report = validation_report
    ))
}

#' GEP Melanoma-Specific Survival Validation
#'
#' Performs comprehensive validation including competing risk analysis
#' for melanoma-specific survival predictions.
#' Uses centralized constants from config_constants.R for timepoints and bootstrap iterations.
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
    
    # Data preparation and quality checks
    log_enhanced("Filtering data for MSS validation", level = "INFO", indent = 1)
    
    # Verify required GEP variables exist
    required_vars <- c("gep_class_simple", "expected_mss_5yr", "expected_mss_7yr", "expected_mss_10yr", "prame_status", "gep_validation_set")
    missing_vars <- setdiff(required_vars, names(data))
    
    if (length(missing_vars) > 0) {
        log_enhanced(sprintf("ERROR: Missing required GEP variables: %s", paste(missing_vars, collapse = ", ")), level = "ERROR", indent = 1)
        log_enhanced("These variables should have been created in data_processing.R", level = "ERROR", indent = 1)
        stop("GEP validation cannot proceed without required variables")
    }
    
    # Filter patients with valid GEP and survival data
    analysis_data <- data %>%
        filter(
            !is.na(biopsy1_gep),
            !is.na(biopsy1_gep_mss),
            biopsy1_gep != "Failed",
            biopsy1_gep != "Unknown",
            !is.na(tt_mss_months),
            !is.na(mss_event),
            tt_mss_months >= 0,
            biopsy1_gep_mss >= 0 & biopsy1_gep_mss <= 1,
            gep_class_simple %in% c("Class 1A", "Class 1B", "Class 2")
        )
    
    log_enhanced(sprintf("Analysis dataset: %d patients with valid GEP and MSS data", nrow(analysis_data)), level = "INFO", indent = 1)
    
    # Events-per-endpoint analysis
    events_per_timepoint <- sapply(timepoints, function(tp) {
        tp_months <- tp * 12
        sum(analysis_data$mss_event == 1 & analysis_data$tt_mss_months <= tp_months)
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
    
    # Standard MSS validation (treating death from other causes as censoring)
    log_enhanced("Performing standard MSS validation (treating other deaths as censoring)", level = "INFO", indent = 1)
    standard_results <- list()
    
    for (tp in timepoints) {
        log_enhanced(sprintf("Analyzing %d-year standard MSS validation", tp), level = "INFO", indent = 2)
        standard_results[[paste0(tp, "yr")]] <- perform_standard_mss_validation(analysis_data, tp, bootstrap_iterations)
    }
    
    # Competing risk MSS validation
    log_enhanced("Performing competing risk MSS validation", level = "INFO", indent = 1)
    competing_results <- list()
    
    for (tp in timepoints) {
        log_enhanced(sprintf("Analyzing %d-year competing risk MSS validation", tp), level = "INFO", indent = 2)
        competing_results[[paste0(tp, "yr")]] <- perform_competing_risk_mss_validation(analysis_data, tp)
    }
    
    # PRAME-augmented analysis
    log_enhanced("Performing PRAME-augmented MSS analysis", level = "INFO", indent = 1)
    prame_results <- perform_prame_augmented_analysis_mss(analysis_data, timepoints)
    
    # Create validation report
    validation_report <- create_mss_validation_report(standard_results, competing_results, prame_results, missing_data_analysis, dataset_name)
    
    # Save results
    save_mss_validation_results(standard_results, competing_results, validation_report, 
                               missing_data_analysis, prame_results, mss_output_dir, prefix)
    
    log_enhanced("GEP MSS validation analysis completed successfully", level = "INFO")
    
    return(list(
        standard_results = standard_results,
        competing_results = competing_results,
        prame_results = prame_results,
        missing_data_analysis = missing_data_analysis,
        validation_report = validation_report
    ))
}

#' Missing Data Assessment and Multiple Imputation
#'
#' Assesses patterns of missing GEP data and evaluates for informative missingness
#' using baseline comparisons and simplified multiple imputation sensitivity analysis.
#'
#' @param data Data frame with complete patient cohort including missing GEP data
#' @return List with missing_patterns (summary of missing data patterns),
#'   baseline_comparisons (comparison of patients with/without GEP data),
#'   and imputation_sensitivity (sensitivity analysis results)
assess_gep_missing_data <- function(data) {
    
    log_enhanced("Assessing patterns of missing GEP data", level = "INFO", indent = 1)
    
    # Overall missing data summary
    total_patients <- nrow(data)
    gep_available <- sum(!is.na(data$biopsy1_gep) & data$biopsy1_gep != "Failed" & data$biopsy1_gep != "Unknown")
    gep_missing <- total_patients - gep_available
    
    missing_summary <- data.frame(
        Variable = c("Total Patients", "GEP Available", "GEP Missing", "Missing Rate (%)"),
        Count = c(total_patients, gep_available, gep_missing, round(gep_missing/total_patients*100, 1))
    )
    
    log_enhanced(sprintf("Missing data summary: %d/%d patients missing GEP data (%.1f%%)", 
                        gep_missing, total_patients, gep_missing/total_patients*100), 
                level = "INFO", indent = 2)
    
    # Baseline comparisons between patients with and without GEP data
    baseline_vars <- c("age_at_diagnosis", "sex", "location", "initial_t_stage", 
                      "initial_tumor_height", "initial_tumor_diameter", "treatment_group")
    
    baseline_comparisons <- list()
    
    for (var in baseline_vars) {
        if (var %in% names(data)) {
            # Create comparison groups
            with_gep <- data[!is.na(data$biopsy1_gep) & data$biopsy1_gep != "Failed" & data$biopsy1_gep != "Unknown", ]
            without_gep <- data[is.na(data$biopsy1_gep) | data$biopsy1_gep == "Failed" | data$biopsy1_gep == "Unknown", ]
            
            if (is.numeric(data[[var]])) {
                # Numeric variable - t-test
                test_result <- t.test(with_gep[[var]], without_gep[[var]])
                baseline_comparisons[[var]] <- list(
                    type = "numeric",
                    with_gep_mean = mean(with_gep[[var]], na.rm = TRUE),
                    without_gep_mean = mean(without_gep[[var]], na.rm = TRUE),
                    p_value = test_result$p.value
                )
            } else {
                # Categorical variable - chi-square test
                contingency_table <- table(data[[var]], 
                                         is.na(data$biopsy1_gep) | data$biopsy1_gep == "Failed" | data$biopsy1_gep == "Unknown")
                test_result <- chisq.test(contingency_table)
                baseline_comparisons[[var]] <- list(
                    type = "categorical",
                    contingency_table = contingency_table,
                    p_value = test_result$p.value
                )
            }
        }
    }
    
    # Simplified multiple imputation sensitivity analysis
    log_enhanced("Performing simplified multiple imputation sensitivity analysis", level = "INFO", indent = 2)
    
    # For this analysis, we'll use a simplified approach with 5 imputations
    # focusing on key variables that might be associated with missingness
    imputation_vars <- c("age_at_diagnosis", "sex", "location", "initial_t_stage", 
                        "initial_tumor_height", "initial_tumor_diameter", "treatment_group")
    
    # Only proceed if we have enough complete cases
    complete_cases <- data[complete.cases(data[imputation_vars]), ]
    
    if (nrow(complete_cases) >= 50) {  # Minimum threshold for imputation
        # Perform simple multiple imputation using mice package
        tryCatch({
            # Create imputation dataset
            imp_data <- complete_cases[imputation_vars]
            
            # Add missing indicator for GEP
            imp_data$gep_missing <- is.na(complete_cases$biopsy1_gep) | 
                                   complete_cases$biopsy1_gep == "Failed" | 
                                   complete_cases$biopsy1_gep == "Unknown"
            
            # Simple logistic regression to predict missingness
            missing_model <- glm(gep_missing ~ ., data = imp_data, family = binomial)
            
            imputation_sensitivity <- list(
                n_complete_cases = nrow(complete_cases),
                missing_model_summary = summary(missing_model),
                significant_predictors = names(which(summary(missing_model)$coefficients[, 4] < 0.05))
            )
            
            log_enhanced(sprintf("Imputation sensitivity analysis completed with %d complete cases", nrow(complete_cases)), 
                        level = "INFO", indent = 3)
            
        }, error = function(e) {
            log_enhanced("Imputation sensitivity analysis failed - insufficient data or computational issues", 
                        level = "WARN", indent = 3)
            imputation_sensitivity <- list(
                error = e$message,
                n_complete_cases = nrow(complete_cases)
            )
        })
    } else {
        log_enhanced("Insufficient complete cases for imputation sensitivity analysis", level = "WARN", indent = 3)
        imputation_sensitivity <- list(
            n_complete_cases = nrow(complete_cases),
            error = "Insufficient complete cases (< 50)"
        )
    }
    
    return(list(
        missing_patterns = missing_summary,
        baseline_comparisons = baseline_comparisons,
        imputation_sensitivity = imputation_sensitivity
    ))
}

#' Simple GEP Validation Wrapper
#'
#' Performs simplified GEP validation analysis for quick assessment.
#' Uses basic validation metrics without advanced statistical methods.
#'
#' @param data Data frame with GEP and survival data
#' @param output_dir Output directory for results
#' @param prefix File prefix for output files
#' @return List containing simplified validation results
simple_gep_validation <- function(data, output_dir, prefix) {
    
    log_enhanced("Starting simplified GEP validation analysis", level = "INFO")
    
    # Create output directory
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Filter for valid GEP data
    analysis_data <- data %>%
        filter(
            !is.na(biopsy1_gep),
            biopsy1_gep != "Failed",
            biopsy1_gep != "Unknown",
            gep_class_simple %in% c("Class 1A", "Class 1B", "Class 2")
        )
    
    log_enhanced(sprintf("Analysis dataset: %d patients with valid GEP data", nrow(analysis_data)), level = "INFO", indent = 1)
    
    # Simple observed vs expected analysis for 5-year outcomes
    mfs_results <- NULL
    mss_results <- NULL
    
    # MFS analysis if data available
    if (all(c("tt_mets_months", "mets_event", "expected_mfs_5yr") %in% names(analysis_data))) {
        log_enhanced("Performing simple 5-year MFS validation", level = "INFO", indent = 1)
        
        mfs_data <- analysis_data %>%
            filter(!is.na(tt_mets_months), !is.na(mets_event), !is.na(expected_mfs_5yr))
        
        if (nrow(mfs_data) > 0) {
            mfs_results <- calculate_observed_expected_rates(mfs_data, "expected_mfs_5yr", "mets_event", "tt_mets_months")
        }
    }
    
    # MSS analysis if data available
    if (all(c("tt_mss_months", "mss_event", "expected_mss_5yr") %in% names(analysis_data))) {
        log_enhanced("Performing simple 5-year MSS validation", level = "INFO", indent = 1)
        
        mss_data <- analysis_data %>%
            filter(!is.na(tt_mss_months), !is.na(mss_event), !is.na(expected_mss_5yr))
        
        if (nrow(mss_data) > 0) {
            mss_results <- calculate_observed_expected_rates(mss_data, "expected_mss_5yr", "mss_event", "tt_mss_months")
        }
    }
    
    # Create simple summary
    overall_summary <- data.frame(
        Metric = c("Total Patients", "MFS Analysis", "MSS Analysis"),
        Value = c(
            nrow(analysis_data),
            ifelse(!is.null(mfs_results), "Available", "Not Available"),
            ifelse(!is.null(mss_results), "Available", "Not Available")
        )
    )
    
    # Create simple plots if results available
    if (!is.null(mfs_results) || !is.null(mss_results)) {
        create_simple_gep_plots(mfs_results, mss_results, output_dir, prefix)
    }
    
    # Create simple report
    create_simple_gep_report(mfs_results, mss_results, overall_summary, output_dir, prefix)
    
    log_enhanced("Simplified GEP validation analysis completed", level = "INFO")
    
    return(list(
        mfs_results = mfs_results,
        mss_results = mss_results,
        overall_summary = overall_summary
    ))
} 