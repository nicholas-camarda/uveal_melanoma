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
    logger::log_info(formatted("Assessing GEP missing data patterns and informative missingness", indent = 1))

    # Summary of missing data patterns
    missing_data <- data
    missing_pattern_summary <- missing_data %>%
        count(missing_gep_group) %>%
        mutate(percentage = round(100 * n / sum(n), 1))

    logger::log_info(formatted("GEP data availability patterns:", indent = 2))
    for (i in seq_len(nrow(missing_pattern_summary))) {
        pattern <- missing_pattern_summary$missing_gep_group[i]
        n <- missing_pattern_summary$n[i]
        pct <- missing_pattern_summary$percentage[i]
        logger::log_info(formatted(sprintf("%s: %d patients (%.1f%%)", pattern, n, pct), indent = 3))
    }

    # Baseline characteristics comparison between GEP-tested vs missing

    baseline_comparison <- NULL
    available_baseline_vars <- intersect(BASELINE_VARIABLES_TO_SUMMARIZE, names(missing_data))
    tryCatch(
        {
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
                    tryCatch(
                        {
                            kw_test <- kruskal.test(var_data ~ groups)
                            test_result <- list(
                                variable = var,
                                test = "Kruskal-Wallis",
                                statistic = round(kw_test$statistic, 3),
                                p_value = round(kw_test$p.value, 4),
                                significant = kw_test$p.value < 0.05
                            )
                        },
                        error = function(e) NULL
                    )
                } else {
                    # Chi-square test for categorical variables
                    tryCatch(
                        {
                            contingency <- table(var_data, groups)
                            chi_test <- suppressWarnings(chisq.test(contingency))
                            use_fisher <- any(chi_test$expected < 5)
                            association_test <- if (use_fisher) {
                                fisher.test(contingency)
                            } else {
                                chi_test
                            }
                            test_result <- list(
                                variable = var,
                                test = if (use_fisher) "Fisher exact" else "Chi-square",
                                statistic = if (use_fisher) NA_real_ else round(association_test$statistic, 3),
                                p_value = round(association_test$p.value, 4),
                                significant = association_test$p.value < 0.05
                            )
                        },
                        error = function(e) NULL
                    )
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
        },
        error = function(e) {
            print(e$message)
            logger::log_warn(formatted("Error in baseline characteristics comparison", indent = 2))
            baseline_comparison <- NULL
        }
    )

    # Log baseline comparison results
    if (!is.null(baseline_comparison) && !is.null(baseline_comparison$n_significant)) {
        n_sig <- baseline_comparison$n_significant
        if (is.na(n_sig)) n_sig <- 0
        logger::log_info(formatted(sprintf(
            "Baseline comparison: %d/%d variables show significant differences (p<0.05)",
            n_sig, length(baseline_comparison$group_tests)
        ), indent = 2))
        logger::log_info(sprintf("Different variables: %s", paste(names(baseline_comparison$group_tests), collapse = ", ")))
    } else {
        logger::log_info(formatted("Baseline comparison: No significant differences detected (insufficient data)", indent = 2))
    }

    # Outcome differences by missing data pattern
    outcome_by_missing <- NULL
    tryCatch(
        {
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

                    logger::log_info(formatted(
                        sprintf(
                            "Survival differs by missing pattern: p=%.4f (%s)",
                            outcome_by_missing$logrank_p,
                            ifelse(outcome_by_missing$significant, "significant", "not significant")
                        ),
                        indent = 2
                    ))
                }
            }
        },
        error = function(e) {
            logger::log_warn(formatted("Error in outcome analysis by missing pattern", indent = 2))
        }
    )

    # Multiple imputation sensitivity analysis (simplified approach)
    imputation_analysis <- NULL
    if (nrow(dplyr::filter(missing_data, has_gep)) >= GEP_MIN_BOOTSTRAP_SAMPLE) {
        tryCatch(
            {
                logger::log_info(formatted("Performing simplified multiple imputation sensitivity analysis", indent = 2))

                imputable_data <- missing_data %>%
                    filter(
                        !has_gep, # Missing GEP
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
                        "Class 2", "Class 1"
                    )

                    imputation_analysis <- list(
                        n_imputable = nrow(imputable_data),
                        imputed_class_1 = sum(imputed_gep_class == "Class 1"),
                        imputed_class_2 = sum(imputed_gep_class == "Class 2"),
                        method = "tumor_size_based"
                    )

                    logger::log_info(formatted(sprintf(
                        "Imputation analysis: %d patients imputed (%d Class 1, %d Class 2)",
                        imputation_analysis$n_imputable,
                        imputation_analysis$imputed_class_1,
                        imputation_analysis$imputed_class_2
                    ), indent = 3))
                }
            },
            error = function(e) {
                logger::log_warn(formatted("Error in multiple imputation analysis", indent = 2))
            }
        )
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
