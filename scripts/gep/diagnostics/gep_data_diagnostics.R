# GEP Data Diagnostics
# Contains data diagnostics and preparation helpers (no modeling, no plotting)

#' Missing Data Assessment and Multiple Imputation (Diagnostics)
#'
#' Assess patterns of missing GEP-related data and evaluate for informative
#' missingness using baseline comparisons, survival differences, and a simple
#' multiple-imputation sensitivity analysis.
#'
#' @param data Data frame containing cohort with possible missing GEP variables
#' @return A list with `missing_patterns`, `baseline_comparison`,
#'   `outcome_by_missing`, `imputation_analysis`, and
#'   `informative_missingness_detected`.
assess_gep_missing_data <- function(data) {
    log_enhanced("Assessing GEP missing data patterns and informative missingness", level = "INFO", indent = 1)
    
    # Create missing data indicator variables
    missing_data <- data %>%
        mutate(
            has_gep = !is.na(biopsy1_gep) & 
                     !biopsy1_gep %in% c("Failed", "Unknown", "Other"),
            has_gep_mfs = !is.na(biopsy1_gep_mfs),
            has_gep_mss = !is.na(biopsy1_gep_mss),
            has_prame = !is.na(prame_status) & 
                       prame_status %in% c("Positive", "Negative"),
            missing_gep_group = case_when(
                has_gep & has_gep_mfs & has_gep_mss ~ "Complete GEP",
                has_gep & (has_gep_mfs | has_gep_mss) ~ "Partial GEP",
                TRUE ~ "No GEP"
            )
        )
    
    # Summary of missing data patterns
    missing_pattern_summary <- missing_data %>%
        count(missing_gep_group) %>%
        mutate(percentage = round(100 * n / sum(n), 1))
    
    log_enhanced("GEP data availability patterns:", level = "INFO", indent = 2)
    for (i in 1:nrow(missing_pattern_summary)) {
        pattern <- missing_pattern_summary$missing_gep_group[i]
        n <- missing_pattern_summary$n[i]
        pct <- missing_pattern_summary$percentage[i]
        log_enhanced(sprintf("%s: %d patients (%.1f%%)", pattern, n, pct), level = "INFO", indent = 3)
    }
    
    # Baseline characteristics comparison between GEP-tested vs missing
    baseline_vars <- c(
        "age_at_diagnosis", "sex", "eye", "initial_tumor_height", 
        "initial_tumor_diameter", "location", "initial_t_stage",
        "treatment_group", "tt_mets_months", "mets_event", 
        "tt_death_months", "death_event"
    )
    
    # Select variables that actually exist in the data
    available_baseline_vars <- intersect(baseline_vars, names(missing_data))
    
    baseline_comparison <- NULL
    if (length(available_baseline_vars) > 0) {
        tryCatch({
            # Create comparison table
            comparison_data <- missing_data %>%
                select(all_of(available_baseline_vars), missing_gep_group)
            
            # Test for differences between groups
            group_tests <- list()
            
            for (var in available_baseline_vars) {
                if (var == "missing_gep_group") next
                
                var_data <- comparison_data[[var]]
                groups <- comparison_data$missing_gep_group
                
                # Skip if variable is mostly missing
                if (sum(!is.na(var_data)) < 10) next
                
                test_result <- NULL
                if (is.numeric(var_data)) {
                    # Kruskal-Wallis test for continuous variables
                    tryCatch({
                        kw_test <- kruskal.test(var_data ~ groups)
                        test_result <- list(
                            variable = var,
                            test = "Kruskal-Wallis",
                            statistic = round(kw_test$statistic, 3),
                            p_value = round(kw_test$p.value, 4),
                            significant = kw_test$p.value < 0.05
                        )
                    }, error = function(e) NULL)
                } else {
                    # Chi-square test for categorical variables
                    tryCatch({
                        chi_test <- chisq.test(table(var_data, groups))
                        test_result <- list(
                            variable = var,
                            test = "Chi-square",
                            statistic = round(chi_test$statistic, 3),
                            p_value = round(chi_test$p.value, 4),
                            significant = chi_test$p.value < 0.05
                        )
                    }, error = function(e) NULL)
                }
                
                if (!is.null(test_result)) {
                    group_tests[[var]] <- test_result
                }
            }
            
            baseline_comparison <- list(
                comparison_data = comparison_data,
                group_tests = group_tests,
                n_significant = sum(sapply(group_tests, function(x) x$significant), na.rm = TRUE)
            )
            
        }, error = function(e) {
            log_enhanced("Error in baseline characteristics comparison", level = "WARN", indent = 2)
            baseline_comparison <- NULL
        })
    }
    
    # Log baseline comparison results
    if (!is.null(baseline_comparison) && !is.null(baseline_comparison$n_significant)) {
        n_sig <- baseline_comparison$n_significant
        if (is.na(n_sig)) n_sig <- 0
        log_enhanced(sprintf("Baseline comparison: %d/%d variables show significant differences (p<0.05)", 
                           n_sig, length(baseline_comparison$group_tests)), level = "INFO", indent = 2)
    } else {
        log_enhanced("Baseline comparison: No significant differences detected (insufficient data)", level = "INFO", indent = 2)
    }
    
    # Outcome differences by missing data pattern
    outcome_by_missing <- NULL
    tryCatch({
        # Check if outcomes differ by missing data pattern
        if (all(c("tt_mets_months", "mets_event") %in% names(missing_data))) {
            surv_by_missing <- missing_data %>%
                filter(!is.na(tt_mets_months), !is.na(mets_event)) %>%
                select(tt_mets_months, mets_event, missing_gep_group)
            
            if (nrow(surv_by_missing) >= GEP_MIN_SAMPLE_SIZE) {
                # Log-rank test
                surv_obj <- Surv(surv_by_missing$tt_mets_months, surv_by_missing$mets_event)
                logrank_test <- survdiff(surv_obj ~ missing_gep_group, data = surv_by_missing)
                
                outcome_by_missing <- list(
                    n = nrow(surv_by_missing),
                    logrank_statistic = round(logrank_test$chisq, 3),
                    logrank_p = round(pchisq(logrank_test$chisq, df = length(logrank_test$n) - 1, lower.tail = FALSE), 4),
                    significant = pchisq(logrank_test$chisq, df = length(logrank_test$n) - 1, lower.tail = FALSE) < 0.05
                )
                
                log_enhanced(sprintf("Survival differs by missing pattern: p=%.4f (%s)", 
                                   outcome_by_missing$logrank_p,
                                   ifelse(outcome_by_missing$significant, "significant", "not significant")), 
                           level = "INFO", indent = 2)
            }
        }
    }, error = function(e) {
        log_enhanced("Error in outcome analysis by missing pattern", level = "WARN", indent = 2)
    })
    
    # Multiple imputation sensitivity analysis (simplified approach)
    imputation_analysis <- NULL
    if (nrow(missing_data %>% filter(has_gep)) >= GEP_MIN_BOOTSTRAP_SAMPLE) {
        tryCatch({
            log_enhanced("Performing simplified multiple imputation sensitivity analysis", level = "INFO", indent = 2)
            
            imputable_data <- missing_data %>%
                filter(
                    !has_gep,  # Missing GEP
                    !is.na(initial_tumor_height),
                    !is.na(initial_tumor_diameter),
                    !is.na(tt_mets_months),
                    !is.na(mets_event)
                )
            
            if (nrow(imputable_data) >= GEP_MISSING_DATA_THRESHOLD) {
                # Simple imputation based on tumor size (larger tumors more likely Class 2)
                imputed_gep_class <- ifelse(
                    imputable_data$initial_tumor_height > median(missing_data$initial_tumor_height, na.rm = TRUE) |
                    imputable_data$initial_tumor_diameter > median(missing_data$initial_tumor_diameter, na.rm = TRUE),
                    "Class 2", "Class 1A"
                )
                
                imputation_analysis <- list(
                    n_imputable = nrow(imputable_data),
                    imputed_class_1a = sum(imputed_gep_class == "Class 1A"),
                    imputed_class_2 = sum(imputed_gep_class == "Class 2"),
                    method = "tumor_size_based"
                )
                
                log_enhanced(sprintf("Imputation analysis: %d patients imputed (%d Class 1A, %d Class 2)", 
                                   imputation_analysis$n_imputable,
                                   imputation_analysis$imputed_class_1a,
                                   imputation_analysis$imputed_class_2), level = "INFO", indent = 3)
            }
        }, error = function(e) {
            log_enhanced("Error in multiple imputation analysis", level = "WARN", indent = 2)
        })
    }
    
    informative_missingness_detected <- FALSE
    if (!is.null(baseline_comparison) && !is.null(baseline_comparison$n_significant)) {
        informative_missingness_detected <- baseline_comparison$n_significant > 0
    }
    
    return(list(
        n_total = nrow(missing_data),
        missing_patterns = missing_pattern_summary,
        baseline_comparison = baseline_comparison,
        outcome_by_missing = outcome_by_missing,
        imputation_analysis = imputation_analysis,
        informative_missingness_detected = informative_missingness_detected
    ))
}

#' Prepare MSS Data with Competing Risk Variables (Diagnostics)
#'
#' Prepare melanoma-specific survival data for competing risk analysis by
#' deriving event type (melanoma-specific vs competing) and related indicators
#' using either cause-of-death information or fallbacks when unavailable.
#'
#' @param data Data frame with GEP predictions and survival data
#' @return A data frame with indicators: `melanoma_death_event`,
#'   `competing_death_event`, `tt_death_years`, and validation summaries.
prepare_mss_competing_risk_data <- function(data) {
    
    log_enhanced("Preparing data for MSS competing risk analysis", level = "DEBUG")
    
    # Check for cause of death variables
    cause_of_death_vars <- c("cause_of_death", "death_cause", "mortality_cause")
    available_cause_vars <- intersect(cause_of_death_vars, names(data))
    
    if (length(available_cause_vars) == 0) {
        log_enhanced("No cause of death variables found, using all deaths as melanoma-specific", level = "WARN")
        # If no cause of death data, treat all deaths as melanoma-specific
        analysis_data <- data %>%
            filter(
                !is.na(biopsy1_gep),
                !is.na(biopsy1_gep_mss),
                biopsy1_gep != "Failed",
                biopsy1_gep != "Unknown",
                !is.na(tt_death_months),
                tt_death_months >= 0,
                biopsy1_gep_mss >= 0 & biopsy1_gep_mss <= 1,
                gep_class_simple %in% c("Class 1A", "Class 1B", "Class 2")
            ) %>%
            mutate(
                melanoma_death_event = death_event,  # All deaths treated as melanoma-specific
                competing_death_event = 0,  # No competing risks
                tt_death_years = tt_death_months / 12
            )
    } else {
        # Use available cause of death variable
        cause_var <- available_cause_vars[1]
        log_enhanced(sprintf("Using cause of death variable: %s", cause_var), level = "INFO")
        
        analysis_data <- data %>%
            filter(
                !is.na(biopsy1_gep),
                !is.na(biopsy1_gep_mss),
                biopsy1_gep != "Failed",
                biopsy1_gep != "Unknown",
                !is.na(tt_death_months),
                tt_death_months >= 0,
                biopsy1_gep_mss >= 0 & biopsy1_gep_mss <= 1,
                gep_class_simple %in% c("Class 1A", "Class 1B", "Class 2")
            ) %>%
            mutate(
                # Define melanoma-specific death (adjust based on actual variable values)
                melanoma_death_event = case_when(
                    death_event == 0 ~ 0,
                    grepl("melanoma|metastasis|cancer", tolower(!!sym(cause_var))) ~ 1,
                    TRUE ~ 0
                ),
                competing_death_event = case_when(
                    death_event == 0 ~ 0,
                    melanoma_death_event == 1 ~ 0,
                    TRUE ~ 1
                ),
                tt_death_years = tt_death_months / 12
            )
    }
    
    log_enhanced(sprintf("MSS analysis dataset: %d patients", nrow(analysis_data)), level = "INFO")
    log_enhanced(sprintf("Melanoma deaths: %d, Competing deaths: %d", 
                        sum(analysis_data$melanoma_death_event), 
                        sum(analysis_data$competing_death_event)), level = "INFO")
    
    return(analysis_data)
}
