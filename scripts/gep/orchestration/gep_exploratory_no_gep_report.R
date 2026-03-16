# Exploratory No-GEP reporting

#' Pick a Stable Patient Identifier Column
#'
#' Chooses the first available patient-level identifier column so downstream
#' report outputs can carry a stable key when one exists.
#'
#' @param data A data frame containing the cohort.
#'
#' @return A single column name or `NULL` if no supported identifier exists.
pick_exploratory_patient_id_col <- function(data) {
    key_candidates <- c("id", "patient_id", "record_id", "case_id", "study_id")
    key_col <- key_candidates[key_candidates %in% names(data)][1]

    if (length(key_col) == 0 || is.na(key_col)) {
        return(NULL)
    }

    key_col
}

#' Bound Binary Probabilities Away From 0 and 1
#'
#' Clips predicted probabilities to avoid infinite logits or unstable
#' calibration summaries when predictions are exactly 0 or 1.
#'
#' @param probabilities Numeric vector of predicted probabilities.
#' @param eps Small numeric boundary used for clipping.
#'
#' @return A numeric vector with values constrained to `[eps, 1 - eps]`.
clip_binary_probabilities <- function(probabilities, eps = 1e-06) {
    pmin(pmax(probabilities, eps), 1 - eps)
}

#' Calculate a Rank-Based AUC
#'
#' Computes the area under the ROC curve for a binary outcome using the
#' Mann-Whitney rank formulation.
#'
#' @param outcome Binary observed outcome vector coded as 0/1.
#' @param score Numeric prediction score.
#'
#' @return Numeric AUC value or `NA_real_` when the outcome lacks both classes.
calculate_binary_auc <- function(outcome, score) {
    valid <- !is.na(outcome) & !is.na(score)
    outcome <- outcome[valid]
    score <- score[valid]

    n_positive <- sum(outcome == 1)
    n_negative <- sum(outcome == 0)

    if (n_positive == 0 || n_negative == 0) {
        return(NA_real_)
    }

    ranks <- rank(score, ties.method = "average")
    (sum(ranks[outcome == 1]) - n_positive * (n_positive + 1) / 2) / (n_positive * n_negative)
}

#' Calculate a Binary Brier Score
#'
#' Computes mean squared prediction error for a binary endpoint after removing
#' incomplete rows.
#'
#' @param outcome Binary observed outcome vector coded as 0/1.
#' @param score Numeric predicted probability vector.
#'
#' @return Numeric Brier score or `NA_real_` when no complete rows remain.
calculate_binary_brier <- function(outcome, score) {
    valid <- !is.na(outcome) & !is.na(score)
    if (!any(valid)) {
        return(NA_real_)
    }

    mean((outcome[valid] - score[valid])^2)
}

#' Create Risk Quantile Bins
#'
#' Splits a numeric score into low/intermediate/high style bins while handling
#' sparse or tied distributions gracefully.
#'
#' @param values Numeric vector to bin.
#' @param n_bins Number of bins to create.
#' @param labels Labels to assign to the ordered bins.
#'
#' @return An ordered factor of quantile-based bins.
create_quantile_bins <- function(values, n_bins = 3, labels = c("Low", "Intermediate", "High")) {
    valid <- !is.na(values)
    bins <- rep(NA_character_, length(values))

    if (sum(valid) == 0) {
        return(factor(bins, levels = labels[seq_len(n_bins)]))
    }

    if (sum(valid) < n_bins) {
        ranked <- rank(values[valid], ties.method = "first")
        bins[valid] <- labels[pmin(ranked, length(labels))]
        return(factor(bins, levels = labels[seq_len(min(length(labels), n_bins))]))
    }

    cuts <- unique(stats::quantile(values[valid], probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE))

    if (length(cuts) <= 2) {
        ranked <- dplyr::ntile(rank(values[valid], ties.method = "average"), n = n_bins)
        bins[valid] <- labels[ranked]
        return(factor(bins, levels = labels[seq_len(n_bins)]))
    }

    bins[valid] <- as.character(cut(
        values[valid],
        breaks = cuts,
        include.lowest = TRUE,
        labels = labels[seq_len(length(cuts) - 1)]
    ))

    factor(bins, levels = labels[seq_len(length(cuts) - 1)])
}

#' Derive a Binary Event-by-Horizon Endpoint
#'
#' Uses an existing event indicator when present, otherwise derives a binary
#' horizon-specific endpoint from event status and follow-up time.
#'
#' @param data A data frame containing event and follow-up variables.
#' @param event_col Name of the binary event column to use when available.
#' @param time_col Name of the follow-up time column in months.
#' @param horizon_months Time horizon used for the derived endpoint.
#'
#' @return An integer vector coded as 0/1/`NA`.
derive_binary_endpoint <- function(data, event_col, time_col, horizon_months) {
    if (event_col %in% names(data)) {
        return(as.integer(data[[event_col]]))
    }

    if (!time_col %in% names(data)) {
        return(rep(NA_integer_, nrow(data)))
    }

    outcome_event <- if (grepl("mss", event_col, ignore.case = TRUE) && "melanoma_death_event" %in% names(data)) {
        data$melanoma_death_event
    } else if ("mets_event" %in% names(data)) {
        data$mets_event
    } else {
        rep(NA_integer_, nrow(data))
    }

    as.integer(!is.na(outcome_event) & outcome_event == 1 & !is.na(data[[time_col]]) & data[[time_col]] <= horizon_months)
}

#' Prepare Data for Exploratory No-GEP Modeling
#'
#' Restores GEP display labels, derives the exploratory grouping variables,
#' standardizes the candidate baseline predictors, collapses sparse factor
#' levels using the existing rare-category helper, and prepares screened model
#' datasets for the surrogate and direct-risk models.
#'
#' @param data The analytic cohort.
#' @param dataset_name Dataset identifier used by existing restoration helpers.
#'
#' @return A list containing the prepared full data, screened modeling datasets,
#'   predictor metadata, screening diagnostics, and an optional ID column.
prepare_exploratory_no_gep_data <- function(data, dataset_name = "uveal_melanoma_full_cohort") {
    prepared <- data %>%
        refresh_gep_analysis_flags() %>%
        restore_gep_display_variables(dataset_name = dataset_name)

    prepared <- prepared %>%
        dplyr::mutate(
            exploratory_gep_group = factor(
                as.character(.data$gep_class_simple),
                levels = c("Class 1", "Class 2", "GEP Failed/Indeterminate", "GEP Not Tested")
            ),
            no_gep_group = dplyr::case_when(
                .data$exploratory_gep_group == "GEP Failed/Indeterminate" ~ "GEP Failed/Indeterminate",
                .data$exploratory_gep_group == "GEP Not Tested" ~ "GEP Not Tested",
                TRUE ~ NA_character_
            ),
            ciliary_involvement = as.integer(grepl("cilio|ciliary", as.character(.data$location), ignore.case = TRUE)),
            sex = factor(as.character(.data$sex)),
            location = factor(as.character(.data$location)),
            initial_t_stage_simple = factor(as.character(.data$initial_t_stage_simple)),
            internal_reflectivity = factor(as.character(.data$internal_reflectivity)),
            srf = factor(as.character(.data$srf)),
            optic_nerve_involvement = dplyr::case_when(
                as.character(.data$optic_nerve) %in% c("Yes", "Y", "Involved") ~ 1L,
                as.character(.data$optic_nerve) %in% c("No", "N", "Not Involved") ~ 0L,
                TRUE ~ NA_integer_
            ),
            mfs_event_5yr = derive_binary_endpoint(prepared, "mfs_event_5yr", "tt_mets_months", 60),
            mss_event_5yr = derive_binary_endpoint(prepared, "mss_event_5yr", "tt_death_months", 60)
        ) %>%
        enforce_unordered_factors()

    candidate_predictors <- c(
        "age_at_diagnosis",
        "sex",
        "initial_tumor_height",
        "initial_tumor_diameter",
        "location",
        "initial_t_stage_simple",
        "internal_reflectivity",
        "srf",
        "initial_vision",
        "optic_nerve_involvement"
    )
    factor_predictors <- c("sex", "location", "initial_t_stage_simple", "internal_reflectivity", "srf")

    screening <- screen_exploratory_predictors(
        prepared,
        candidate_predictors = candidate_predictors,
        factor_predictors = factor_predictors,
        completeness_threshold = 0.9,
        min_level_count = 5
    )
    retained_predictors <- screening %>%
        dplyr::filter(.data$status == "retained") %>%
        dplyr::pull(.data$predictor)

    if (length(retained_predictors) == 0) {
        stop("Exploratory no-GEP report could not retain any baseline predictors after screening.")
    }

    definitive_training <- build_exploratory_model_dataset(
        prepared,
        predictors = retained_predictors,
        factor_predictors = intersect(factor_predictors, retained_predictors),
        outcome_var = "class2_outcome",
        group_levels = c("Class 1", "Class 2")
    )
    no_gep_prediction <- build_exploratory_model_dataset(
        prepared,
        predictors = retained_predictors,
        factor_predictors = intersect(factor_predictors, retained_predictors),
        outcome_var = NULL,
        group_levels = c("GEP Failed/Indeterminate", "GEP Not Tested")
    )
    mfs_model_data <- build_exploratory_model_dataset(
        prepared,
        predictors = retained_predictors,
        factor_predictors = intersect(factor_predictors, retained_predictors),
        outcome_var = "mfs_event_5yr"
    )
    mss_model_data <- build_exploratory_model_dataset(
        prepared,
        predictors = retained_predictors,
        factor_predictors = intersect(factor_predictors, retained_predictors),
        outcome_var = "mss_event_5yr"
    )

    list(
        full_data = prepared,
        definitive_training = definitive_training,
        no_gep_prediction = no_gep_prediction,
        mfs_model_data = mfs_model_data,
        mss_model_data = mss_model_data,
        candidate_predictors = candidate_predictors,
        factor_predictors = factor_predictors,
        predictors = retained_predictors,
        predictor_screening = screening,
        patient_id_col = pick_exploratory_patient_id_col(prepared)
    )
}

#' Screen Candidate Predictors for the Exploratory Models
#'
#' Applies shared completeness and sparse-level rules across the surrogate and
#' direct-risk modeling datasets.
#'
#' @param data Prepared exploratory cohort.
#' @param candidate_predictors Character vector of candidate predictor names.
#' @param factor_predictors Character vector of factor predictors.
#' @param completeness_threshold Minimum required non-missing proportion.
#' @param min_level_count Minimum required count for retained levels.
#'
#' @return A data frame describing retained and dropped predictors.
screen_exploratory_predictors <- function(data,
                                          candidate_predictors,
                                          factor_predictors,
                                          completeness_threshold = 0.9,
                                          min_level_count = 5) {
    dataset_map <- list(
        surrogate = data %>% dplyr::filter(.data$exploratory_gep_group %in% c("Class 1", "Class 2")),
        direct_mfs = data %>% dplyr::filter(!is.na(.data$mfs_event_5yr)),
        direct_mss = data %>% dplyr::filter(!is.na(.data$mss_event_5yr))
    )

    purrr::map_dfr(candidate_predictors, function(predictor) {
        completeness_values <- purrr::map_dbl(dataset_map, function(dataset) {
            mean(!is.na(dataset[[predictor]]))
        })
        completeness_ok <- all(completeness_values >= completeness_threshold)

        level_check_reason <- NA_character_
        if (completeness_ok && predictor %in% factor_predictors) {
            for (dataset_name in names(dataset_map)) {
                dataset <- dataset_map[[dataset_name]]
                sparse_summary <- summarize_sparse_factor_levels(
                    dataset[[predictor]],
                    min_level_count = min_level_count,
                    explicit_exclusions = MODELING_LEVEL_EXCLUSIONS[[predictor]] %||% character()
                )

                if (!is.na(sparse_summary$drop_reason)) {
                    level_check_reason <- sprintf(
                        "%s %s",
                        dataset_name,
                        sparse_summary$drop_reason
                    )
                    break
                }

            }
        }

        status <- if (!completeness_ok) {
            "dropped"
        } else if (!is.na(level_check_reason)) {
            "dropped"
        } else {
            "retained"
        }

        reason <- if (!completeness_ok) {
            paste(
                sprintf("%s completeness %.1f%%", names(completeness_values), 100 * completeness_values),
                collapse = "; "
            )
        } else if (!is.na(level_check_reason)) {
            level_check_reason
        } else {
            "passes completeness and sparse-level screening"
        }

        tibble::tibble(
            predictor = predictor,
            predictor_type = ifelse(predictor %in% factor_predictors, "factor", "numeric"),
            surrogate_completeness = completeness_values[["surrogate"]],
            direct_mfs_completeness = completeness_values[["direct_mfs"]],
            direct_mss_completeness = completeness_values[["direct_mss"]],
            status = status,
            reason = reason
        )
    })
}

#' Build a Screened Modeling Dataset for the Exploratory Workflow
#'
#' Applies sparse-level exclusions for retained factor predictors, then filters
#' to complete rows for the requested outcome.
#'
#' @param data Prepared exploratory cohort.
#' @param predictors Retained predictor names.
#' @param factor_predictors Retained factor predictor names.
#' @param outcome_var Optional outcome variable name.
#' @param group_levels Optional character vector of `exploratory_gep_group`
#'   levels to keep.
#'
#' @return A filtered modeling data frame.
build_exploratory_model_dataset <- function(data,
                                            predictors,
                                            factor_predictors,
                                            outcome_var = NULL,
                                            group_levels = NULL) {
    model_data <- data

    if (!is.null(group_levels)) {
        model_data <- model_data %>%
            dplyr::filter(.data$exploratory_gep_group %in% group_levels)
    }

    if (!is.null(outcome_var) && identical(outcome_var, "class2_outcome")) {
        model_data <- model_data %>%
            dplyr::mutate(class2_outcome = as.integer(.data$exploratory_gep_group == "Class 2"))
    }

    if (length(factor_predictors) > 0) {
        exclusion_result <- apply_sparse_level_exclusions(
            model_data,
            variables = factor_predictors,
            analysis_name = paste0(outcome_var %||% "no_gep", "_exploratory"),
            id_col = pick_sparse_level_id_col(model_data),
            level_exclusions = MODELING_LEVEL_EXCLUSIONS
        )
        model_data <- exclusion_result$data
    }

    required_vars <- predictors
    if (!is.null(outcome_var)) {
        required_vars <- c(required_vars, outcome_var)
    }

    model_data %>%
        dplyr::filter(stats::complete.cases(dplyr::across(all_of(required_vars)))) %>%
        enforce_unordered_factors()
}

#' Verify the Simplified KM Risk-Table Fix for No-GEP Reporting
#'
#' Confirms the expected cohort counts and checks that the simplified KM helper
#' now renders the visible risk-table rows with the correct row/count pairing.
#'
#' @param data The analytic cohort.
#' @param dataset_name Dataset identifier used in display restoration.
#' @param visual_file Retained for compatibility with earlier verification
#'   signatures.
#'
#' @return A tibble summarizing cohort counts and simplified KM verification.
verify_exploratory_no_gep_km_fix <- function(data,
                                             dataset_name = "uveal_melanoma_full_cohort",
                                             visual_file = here("scripts", "gep", "visualization", "gep_visuals.R")) {
    prepared <- prepare_exploratory_no_gep_data(data, dataset_name = dataset_name)$full_data

    observed_counts <- prepared %>%
        dplyr::filter(!is.na(.data$exploratory_gep_group)) %>%
        dplyr::count(.data$exploratory_gep_group, name = "observed_n")

    expected_counts <- tibble::tibble(
        exploratory_gep_group = factor(
            c("Class 1", "Class 2", "GEP Failed/Indeterminate", "GEP Not Tested"),
            levels = levels(prepared$exploratory_gep_group)
        ),
        expected_n = c(58L, 27L, 13L, 162L)
    )

    verification <- expected_counts %>%
        dplyr::left_join(observed_counts, by = "exploratory_gep_group") %>%
        dplyr::mutate(
            observed_n = dplyr::coalesce(.data$observed_n, 0L),
            status = dplyr::if_else(.data$expected_n == .data$observed_n, "matched", "mismatch")
        )

    if (any(verification$status != "matched")) {
        mismatch_text <- verification %>%
            dplyr::filter(.data$status != "matched") %>%
            dplyr::transmute(text = sprintf("%s expected %d observed %d", .data$exploratory_gep_group, .data$expected_n, .data$observed_n)) %>%
            dplyr::pull(.data$text)
        stop(sprintf(
            "Exploratory no-GEP KM verification failed: group counts do not match the expected fixed values (%s).",
            paste(mismatch_text, collapse = "; ")
        ))
    }

    simplified_plot <- create_mfs_simplified_survival_curves(
        data = prepared,
        output_dir = tempdir(),
        prefix = "verification_",
        dataset_name = dataset_name,
        km_output_dir = tempdir(),
        return_plot = TRUE,
        save_plot = FALSE
    )

    baseline_counts <- simplified_plot$plot_data %>%
        dplyr::count(.data$gep_km_simple, name = "n") %>%
        tibble::deframe()

    y_scales <- Filter(function(scale) "y" %in% scale$aesthetics, simplified_plot$plot$table$scales$scales)
    displayed_limits <- y_scales[[length(y_scales)]]$limits
    displayed_order <- rev(displayed_limits)
    displayed_counts <- suppressWarnings(ggplot2::ggplot_build(simplified_plot$plot$table)$data[[1]]) %>%
        dplyr::filter(.data$x == 0) %>%
        dplyr::arrange(dplyr::desc(.data$y)) %>%
        dplyr::pull(.data$label) %>%
        as.integer()
    expected_displayed_counts <- unname(as.integer(baseline_counts[displayed_order]))

    if (!identical(displayed_counts, expected_displayed_counts)) {
        stop(sprintf(
            "Exploratory no-GEP KM verification failed: simplified KM displayed counts (%s) do not match expected counts in displayed order (%s).",
            paste(displayed_counts, collapse = ", "),
            paste(expected_displayed_counts, collapse = ", ")
        ))
    }

    verification %>%
        dplyr::mutate(
            simple_km_display_order = c(displayed_order, rep(NA_character_, n() - length(displayed_order))),
            simple_km_scale_limits = c(displayed_limits, rep(NA_character_, n() - length(displayed_limits))),
            simple_km_displayed_n = c(displayed_counts, rep(NA_integer_, n() - length(displayed_counts)))
        )
}

#' Summarize Exploratory Data Audit Results
#'
#' Combines fixed cohort counts, endpoint counts, predictor completeness, and
#' KM verification details into a workbook-ready table.
#'
#' @param prepared_data Output from `prepare_exploratory_no_gep_data()`.
#' @param km_verification Output from `verify_exploratory_no_gep_km_fix()`.
#'
#' @return A data frame for the `Data_Audit` workbook tab.
summarize_exploratory_data_audit <- function(prepared_data, km_verification) {
    group_summary <- prepared_data$full_data %>%
        dplyr::filter(!is.na(.data$exploratory_gep_group)) %>%
        dplyr::group_by(.data$exploratory_gep_group) %>%
        dplyr::summarise(
            n = dplyr::n(),
            metastasis_events = sum(.data$mets_event == 1, na.rm = TRUE),
            melanoma_deaths = sum(.data$melanoma_death_event == 1, na.rm = TRUE),
            mfs_5yr_events = sum(.data$mfs_event_5yr == 1, na.rm = TRUE),
            mss_5yr_events = sum(.data$mss_event_5yr == 1, na.rm = TRUE),
            complete_predictors = sum(stats::complete.cases(dplyr::across(all_of(prepared_data$predictors)))),
            .groups = "drop"
        ) %>%
        dplyr::mutate(section = "Group Counts")

    km_summary <- km_verification %>%
        dplyr::mutate(section = "KM Verification")

    dplyr::bind_rows(
        group_summary %>%
            dplyr::rename(group = exploratory_gep_group),
        km_summary %>%
            dplyr::rename(group = exploratory_gep_group)
    )
}

#' Format a Baseline Comparison Cell
#'
#' Summarizes a variable within one group using median/IQR for numeric fields and
#' yes-count/percent formatting for binary indicators.
#'
#' @param data Grouped data frame.
#' @param variable Variable name to summarize.
#'
#' @return A character summary for one group-variable combination.
format_baseline_value <- function(data, variable) {
    values <- data[[variable]]
    non_missing <- values[!is.na(values)]
    is_binary_numeric <- is.numeric(values) && length(non_missing) > 0 && all(non_missing %in% c(0, 1))

    if (is_binary_numeric) {
        percent_yes <- mean(values == 1, na.rm = TRUE) * 100
        return(sprintf("%d/%d (%.1f%%)", sum(values == 1, na.rm = TRUE), sum(!is.na(values)), percent_yes))
    }

    if (is.numeric(values)) {
        median_value <- stats::median(values, na.rm = TRUE)
        q1 <- stats::quantile(values, probs = 0.25, na.rm = TRUE)
        q3 <- stats::quantile(values, probs = 0.75, na.rm = TRUE)
        return(sprintf("%.1f (%.1f, %.1f)", median_value, q1, q3))
    }

    level_counts <- sort(table(as.character(values), useNA = "no"), decreasing = TRUE)
    if (length(level_counts) == 0) {
        return("No data")
    }

    total_n <- sum(level_counts)
    paste(
        sprintf(
            "%s=%d/%d (%.1f%%)",
            names(level_counts),
            as.integer(level_counts),
            total_n,
            100 * as.integer(level_counts) / total_n
        ),
        collapse = "; "
    )
}

#' Calculate a Baseline Comparison P-Value
#'
#' Uses a Wilcoxon test for numeric variables and Fisher's exact test for binary
#' indicators when enough data are available.
#'
#' @param data Data frame containing the comparison variables.
#' @param variable Variable name to test.
#' @param group_var Grouping variable name.
#'
#' @return A list containing the test name and p-value.
calculate_baseline_p_value <- function(data, variable, group_var) {
    values <- data[[variable]]
    groups <- data[[group_var]]
    valid <- !is.na(values) & !is.na(groups)
    non_missing <- values[valid]
    is_binary_numeric <- is.numeric(values) && length(non_missing) > 0 && all(non_missing %in% c(0, 1))

    if (sum(valid) < 5 || length(unique(groups[valid])) < 2) {
        return(list(test = "insufficient_data", p_value = NA_real_))
    }

    if (is_binary_numeric) {
        contingency <- table(values[valid], groups[valid])
        use_fisher <- any(contingency < 5)
        result <- tryCatch(
            if (use_fisher) stats::fisher.test(contingency) else suppressWarnings(stats::chisq.test(contingency)),
            error = function(e) NULL
        )
        return(list(
            test = if (use_fisher) "Fisher's exact" else "Chi-square",
            p_value = if (is.null(result)) NA_real_ else result$p.value
        ))
    }

    if (is.numeric(values) && !is_binary_numeric) {
        result <- tryCatch(
            stats::kruskal.test(values[valid] ~ groups[valid]),
            error = function(e) NULL
        )
        return(list(
            test = "Kruskal-Wallis",
            p_value = if (is.null(result)) NA_real_ else result$p.value
        ))
    }

    contingency <- table(values[valid], groups[valid])
    use_fisher <- any(contingency < 5)
    result <- tryCatch(
        if (use_fisher) stats::fisher.test(contingency) else suppressWarnings(stats::chisq.test(contingency)),
        error = function(e) NULL
    )
    list(
        test = if (use_fisher) "Fisher's exact" else "Chi-square",
        p_value = if (is.null(result)) NA_real_ else result$p.value
    )
}

#' Summarize Baseline Comparisons Across Exploratory GEP Groups
#'
#' Produces workbook-ready baseline summaries across Class 1, Class 2,
#' Failed/Indeterminate, and Not Tested groups using the fixed predictor set.
#'
#' @param prepared_data Output from `prepare_exploratory_no_gep_data()`.
#'
#' @return A data frame for the `Baseline_Comparisons` workbook tab.
summarize_exploratory_baseline_comparisons <- function(prepared_data) {
    baseline_data <- prepared_data$full_data %>%
        dplyr::filter(!is.na(.data$exploratory_gep_group))

    baseline_vars <- unique(c(prepared_data$candidate_predictors, "ciliary_involvement"))

    group_levels <- levels(baseline_data$exploratory_gep_group)

    purrr::map_dfr(baseline_vars, function(variable) {
        p_value_info <- calculate_baseline_p_value(
            baseline_data,
            variable = variable,
            group_var = "exploratory_gep_group"
        )

        summary_values <- purrr::map_chr(group_levels, function(level_name) {
            format_baseline_value(
                baseline_data %>% dplyr::filter(.data$exploratory_gep_group == level_name),
                variable
            )
        })

        tibble::tibble(
            variable = variable,
            class_1 = summary_values[[1]],
            class_2 = summary_values[[2]],
            gep_failed_indeterminate = summary_values[[3]],
            gep_not_tested = summary_values[[4]],
            test = p_value_info$test,
            p_value = p_value_info$p_value
        )
    })
}

#' Summarize Kaplan-Meier Survival at Fixed Timepoints
#'
#' Extracts KM survival estimates by group at pre-specified month horizons.
#'
#' @param data Data frame used for KM fitting.
#' @param group_var Grouping variable name.
#' @param time_var Follow-up time variable name.
#' @param event_var Event indicator variable name.
#' @param times Numeric vector of time horizons in months.
#'
#' @return A tidy data frame of group-by-time survival estimates.
summarize_km_timepoints <- function(data, group_var, time_var, event_var, times) {
    fit <- survival::survfit(
        stats::as.formula(sprintf("Surv(%s, %s) ~ %s", time_var, event_var, group_var)),
        data = data
    )
    fit_summary <- summary(fit, times = times, extend = TRUE)

    tibble::tibble(
        group = sub(sprintf("%s=", group_var), "", fit_summary$strata),
        time_months = fit_summary$time,
        time_years = round(fit_summary$time / 12, 1),
        n_risk = fit_summary$n.risk,
        survival_probability = fit_summary$surv,
        event_risk = 1 - fit_summary$surv
    )
}

#' Summarize MSS Cumulative Incidence at Fixed Timepoints
#'
#' Computes group-level melanoma-specific death cumulative incidence estimates at
#' fixed horizons using the existing competing-risks stack.
#'
#' @param data Data frame used for the MSS competing-risks summaries.
#' @param group_var Grouping variable name.
#' @param times Numeric vector of time horizons in months.
#'
#' @return A tidy data frame of CIF summaries.
summarize_mss_cif_timepoints <- function(data, group_var, times) {
    status <- dplyr::case_when(
        data$melanoma_death_event == 1 ~ 1L,
        data$competing_death_event == 1 ~ 2L,
        TRUE ~ 0L
    )

    cif_fit <- cmprsk::cuminc(
        ftime = data$tt_death_months,
        fstatus = status,
        group = data[[group_var]],
        cencode = 0
    )

    cif_summary <- cmprsk::timepoints(cif_fit, times = times)
    event_rows <- grep(" 1$", rownames(cif_summary$est), value = TRUE)

    purrr::map_dfr(event_rows, function(row_name) {
        tibble::tibble(
            group = sub(" 1$", "", row_name),
            time_months = times,
            time_years = round(times / 12, 1),
            cumulative_incidence = as.numeric(cif_summary$est[row_name, ]),
            mss_probability = 1 - as.numeric(cif_summary$est[row_name, ])
        )
    })
}

#' Build a Design Matrix for the Exploratory Ridge Models
#'
#' Uses `model.matrix()` so factor expansion follows standard R contrasts, then
#' aligns prediction matrices back to the training design columns.
#'
#' @param data Modeling data frame.
#' @param predictors Retained predictor names.
#' @param reference_columns Optional training design column names used to align
#'   prediction matrices.
#'
#' @return A numeric matrix ready for `glmnet`.
build_exploratory_design_matrix <- function(data, predictors, reference_columns = NULL) {
    design_formula <- stats::as.formula(sprintf("~ %s", paste(predictors, collapse = " + ")))
    design_matrix <- stats::model.matrix(design_formula, data = data)
    design_matrix <- design_matrix[, colnames(design_matrix) != "(Intercept)", drop = FALSE]

    if (!is.null(reference_columns)) {
        missing_columns <- setdiff(reference_columns, colnames(design_matrix))
        extra_columns <- setdiff(colnames(design_matrix), reference_columns)

        if (length(missing_columns) > 0) {
            zero_block <- matrix(
                0,
                nrow = nrow(design_matrix),
                ncol = length(missing_columns),
                dimnames = list(NULL, missing_columns)
            )
            design_matrix <- cbind(design_matrix, zero_block)
        }

        if (length(extra_columns) > 0) {
            design_matrix <- design_matrix[, setdiff(colnames(design_matrix), extra_columns), drop = FALSE]
        }

        design_matrix <- design_matrix[, reference_columns, drop = FALSE]
    }

    design_matrix
}

#' Create Stratified Cross-Validation Fold IDs
#'
#' Assigns observations to folds while roughly preserving event balance.
#'
#' @param outcome Binary outcome vector coded as 0/1.
#' @param folds Number of folds.
#' @param seed Random seed.
#'
#' @return Integer vector of fold IDs.
create_stratified_fold_ids <- function(outcome, folds = 5, seed = 123) {
    set.seed(seed)
    fold_id <- integer(length(outcome))

    for (class_value in unique(outcome)) {
        class_indices <- which(outcome == class_value)
        fold_id[class_indices] <- sample(rep(seq_len(folds), length.out = length(class_indices)))
    }

    fold_id
}

#' Choose a Safe Number of CV Folds for Binary Ridge Models
#'
#' Caps the fold count so each outcome class can contribute at least one
#' observation per fold, avoiding `cv.glmnet()` failures in sparse datasets.
#'
#' @param outcome Binary outcome vector coded as 0/1.
#' @param preferred_folds Requested fold count.
#'
#' @return Integer fold count.
choose_binary_cv_folds <- function(outcome, preferred_folds = 5) {
    class_counts <- table(outcome)

    if (length(class_counts) < 2) {
        return(2L)
    }

    max_supported <- min(as.integer(class_counts), length(outcome), preferred_folds)
    as.integer(max(2, max_supported))
}

#' Cross-Validate Binary Model Predictions
#'
#' Performs simple K-fold cross-validation for a ridge-penalized logistic model.
#'
#' @param data Modeling data frame.
#' @param outcome_var Name of the binary outcome column.
#' @param predictors Character vector of predictor names.
#' @param folds Number of folds.
#' @param seed Random seed for fold assignment.
#'
#' @return A numeric vector of out-of-fold predicted probabilities aligned to
#'   the input rows.
cross_validate_binary_predictions <- function(data, outcome_var, predictors, folds = 5, seed = 123) {
    n_rows <- nrow(data)
    outcome <- data[[outcome_var]]
    folds <- choose_binary_cv_folds(outcome, preferred_folds = folds)

    if (n_rows < folds || length(unique(outcome)) < 2) {
        return(rep(NA_real_, n_rows))
    }

    fold_id <- create_stratified_fold_ids(outcome, folds = folds, seed = seed)
    predictions <- rep(NA_real_, n_rows)

    for (fold in seq_len(folds)) {
        train_data <- data[fold_id != fold, , drop = FALSE]
        test_data <- data[fold_id == fold, , drop = FALSE]

        if (nrow(train_data) == 0 || length(unique(train_data[[outcome_var]])) < 2) {
            next
        }

        x_train <- build_exploratory_design_matrix(train_data, predictors = predictors)
        x_test <- build_exploratory_design_matrix(
            test_data,
            predictors = predictors,
            reference_columns = colnames(x_train)
        )

        fold_fit <- tryCatch(
            suppressWarnings(glmnet::cv.glmnet(
                x = x_train,
                y = train_data[[outcome_var]],
                family = "binomial",
                alpha = 0,
                nfolds = choose_binary_cv_folds(train_data[[outcome_var]], preferred_folds = 5),
                standardize = TRUE,
                type.measure = "deviance"
            )),
            error = function(e) NULL
        )

        if (is.null(fold_fit)) {
            next
        }

        predictions[fold_id == fold] <- as.numeric(
            stats::predict(fold_fit, newx = x_test, s = "lambda.min", type = "response")
        )
    }

    predictions
}

#' Summarize Binary Calibration
#'
#' Produces a lightweight calibration summary suitable for exploratory
#' workbook and text reporting.
#'
#' @param outcome Binary observed outcome vector.
#' @param predicted Numeric predicted probabilities.
#'
#' @return A named list containing calibration status, intercept, slope, and
#'   grouped calibration curve data.
summarize_binary_calibration <- function(outcome, predicted) {
    valid <- !is.na(outcome) & !is.na(predicted)
    outcome <- outcome[valid]
    predicted <- clip_binary_probabilities(predicted[valid])

    if (length(outcome) < 20 || length(unique(outcome)) < 2 || length(unique(round(predicted, 6))) < 5) {
        return(list(
            status = "unsupported_sparse_data",
            intercept = NA_real_,
            slope = NA_real_,
            curve = tibble::tibble()
        ))
    }

    intercept_fit <- tryCatch(
        stats::glm(outcome ~ offset(stats::qlogis(predicted)), family = stats::binomial()),
        error = function(e) NULL
    )
    slope_fit <- tryCatch(
        stats::glm(outcome ~ stats::qlogis(predicted), family = stats::binomial()),
        error = function(e) NULL
    )

    curve_data <- tibble::tibble(
        observed = outcome,
        predicted = predicted,
        calibration_bin = create_quantile_bins(predicted, n_bins = 5, labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
    ) %>%
        dplyr::filter(!is.na(.data$calibration_bin)) %>%
        dplyr::group_by(.data$calibration_bin) %>%
        dplyr::summarise(
            n = dplyr::n(),
            mean_predicted = mean(.data$predicted, na.rm = TRUE),
            observed_rate = mean(.data$observed, na.rm = TRUE),
            .groups = "drop"
        )

    list(
        status = "ok",
        intercept = if (is.null(intercept_fit)) NA_real_ else unname(stats::coef(intercept_fit)[1]),
        slope = if (is.null(slope_fit)) NA_real_ else unname(stats::coef(slope_fit)[2]),
        curve = curve_data
    )
}

#' Map Ridge Design Terms Back to Source Predictors
#'
#' @param term Design-matrix term name.
#' @param predictors Retained predictor names.
#'
#' @return A predictor name.
map_design_term_to_predictor <- function(term, predictors) {
    matched <- predictors[startsWith(term, predictors)]
    if (length(matched) == 0) {
        return(term)
    }

    matched[which.max(nchar(matched))][1]
}

#' Extract Penalized Coefficients for Reporting
#'
#' Returns the ridge coefficients at `lambda.min`. These are standardized,
#' shrunken coefficients for ranking and directionality, not classical
#' inferential estimates.
#'
#' @param model_fit A fitted `cv.glmnet` object.
#' @param predictors Retained predictor names.
#'
#' @return A data frame of coefficient summaries.
extract_binary_model_coefficients <- function(model_fit, predictors) {
    coefficient_matrix <- as.matrix(stats::coef(model_fit, s = "lambda.min"))
    tibble::tibble(
        term = rownames(coefficient_matrix),
        estimate = as.numeric(coefficient_matrix[, 1]),
        conf_low = NA_real_,
        conf_high = NA_real_,
        std_error = NA_real_,
        z_value = NA_real_,
        p_value = NA_real_,
        predictor = vapply(rownames(coefficient_matrix), map_design_term_to_predictor, character(1), predictors = predictors),
        coefficient_type = "ridge_penalized"
    )
}

#' Summarize Predictor Contributions for a Ridge Model
#'
#' Ranks predictors by the largest absolute standardized coefficient assigned to
#' any of their design-matrix terms.
#'
#' @param coefficient_data Output from `extract_binary_model_coefficients()`.
#' @param model_name Display name for the model.
#'
#' @return A ranked predictor-contribution table.
summarize_predictor_contributions <- function(coefficient_data, model_name) {
    coefficient_data %>%
        dplyr::filter(.data$term != "(Intercept)") %>%
        dplyr::mutate(abs_estimate = abs(.data$estimate)) %>%
        dplyr::group_by(.data$predictor) %>%
        dplyr::slice_max(order_by = .data$abs_estimate, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dplyr::desc(.data$abs_estimate)) %>%
        dplyr::mutate(
            model = model_name,
            rank = dplyr::row_number(),
            direction = dplyr::if_else(.data$estimate >= 0, "higher predicted risk", "lower predicted risk")
        ) %>%
        dplyr::transmute(
            model = .data$model,
            rank = .data$rank,
            predictor = .data$predictor,
            dominant_term = .data$term,
            standardized_coefficient = .data$estimate,
            standardized_abs_coefficient = .data$abs_estimate,
            direction = .data$direction
        )
}

#' Fit an Exploratory Binary Model
#'
#' Fits a ridge-penalized logistic regression, derives apparent and
#' cross-validated performance, and packages model outputs for workbook
#' reporting.
#'
#' @param data Modeling data frame.
#' @param outcome_var Name of the binary outcome column.
#' @param predictors Character vector of predictor names.
#' @param model_name Display label for the model.
#' @param seed Random seed for cross-validation.
#'
#' @return A list containing the fit object, coefficients, contributions,
#'   metrics, and calibration data.
fit_exploratory_binary_model <- function(data, outcome_var, predictors, model_name, seed = 123) {
    design_matrix <- build_exploratory_design_matrix(data, predictors = predictors)
    outcome <- data[[outcome_var]]
    cv_folds <- choose_binary_cv_folds(outcome, preferred_folds = 5)

    fitted_model <- suppressWarnings(glmnet::cv.glmnet(
        x = design_matrix,
        y = outcome,
        family = "binomial",
        alpha = 0,
        nfolds = cv_folds,
        standardize = TRUE,
        type.measure = "deviance"
    ))

    apparent_predictions <- as.numeric(
        stats::predict(fitted_model, newx = design_matrix, s = "lambda.min", type = "response")
    )
    cv_predictions <- cross_validate_binary_predictions(
        data,
        outcome_var = outcome_var,
        predictors = predictors,
        seed = seed
    )
    calibration <- summarize_binary_calibration(outcome, apparent_predictions)
    coefficient_data <- extract_binary_model_coefficients(fitted_model, predictors = predictors)
    predictor_contributions <- summarize_predictor_contributions(coefficient_data, model_name = model_name)

    metrics <- tibble::tibble(
        model = model_name,
        n = nrow(data),
        events = sum(outcome == 1, na.rm = TRUE),
        apparent_auc = calculate_binary_auc(outcome, apparent_predictions),
        cv_auc = calculate_binary_auc(outcome, cv_predictions),
        apparent_brier = calculate_binary_brier(outcome, apparent_predictions),
        cv_brier = calculate_binary_brier(outcome, cv_predictions),
        calibration_status = calibration$status,
        calibration_intercept = calibration$intercept,
        calibration_slope = calibration$slope,
        cv_folds = cv_folds,
        lambda_min = fitted_model$lambda.min,
        lambda_1se = fitted_model$lambda.1se
    )

    list(
        model = fitted_model,
        metrics = metrics,
        coefficients = coefficient_data,
        predictor_contributions = predictor_contributions,
        calibration_curve = calibration$curve,
        predictors = predictors,
        design_columns = colnames(design_matrix)
    )
}

#' Flatten Model Outputs for Workbook Export
#'
#' Creates a single workbook-ready data frame containing model metrics,
#' coefficients, and calibration-curve rows.
#'
#' @param model_results Output from `fit_exploratory_binary_model()`.
#'
#' @return A data frame for one workbook tab.
create_model_workbook_tab <- function(model_results) {
    metrics_section <- model_results$metrics %>%
        dplyr::mutate(section = "metrics")
    coefficient_section <- model_results$coefficients %>%
        dplyr::mutate(section = "coefficients")
    calibration_section <- model_results$calibration_curve %>%
        dplyr::mutate(section = "calibration_curve")

    dplyr::bind_rows(metrics_section, coefficient_section, calibration_section)
}

#' Predict from an Exploratory Ridge Model
#'
#' Aligns new-data design columns to the model's training matrix and returns
#' predicted probabilities at `lambda.min`.
#'
#' @param model_results Output from `fit_exploratory_binary_model()`.
#' @param newdata New data frame for prediction.
#'
#' @return Numeric vector of predicted probabilities.
predict_exploratory_binary_model <- function(model_results, newdata) {
    new_design <- build_exploratory_design_matrix(
        newdata,
        predictors = model_results$predictors,
        reference_columns = model_results$design_columns
    )

    as.numeric(
        stats::predict(model_results$model, newx = new_design, s = "lambda.min", type = "response")
    )
}

#' Summarize No-GEP Patient-Level Predictions
#'
#' Reduces row-level predictions into grouped descriptive summaries for the main
#' no-GEP reporting tab.
#'
#' @param prediction_data Row-level prediction output.
#'
#' @return A grouped summary data frame.
summarize_no_gep_predictions <- function(prediction_data) {
    prediction_data %>%
        dplyr::group_by(.data$no_gep_group) %>%
        dplyr::summarise(
            n = dplyr::n(),
            median_surrogate_class2_probability = stats::median(.data$surrogate_class2_probability, na.rm = TRUE),
            median_predicted_mfs_5yr_risk = stats::median(.data$predicted_mfs_5yr_risk, na.rm = TRUE),
            median_predicted_mss_5yr_risk = stats::median(.data$predicted_mss_5yr_risk, na.rm = TRUE),
            observed_mfs_5yr_event_rate = mean(.data$mfs_event_5yr == 1, na.rm = TRUE),
            observed_mss_5yr_event_rate = mean(.data$mss_event_5yr == 1, na.rm = TRUE),
            .groups = "drop"
    )
}

#' Summarize Pooled No-GEP Sensitivity Analysis
#'
#' Produces pooled low/intermediate/high summaries for the sensitivity tab while
#' keeping the main report separated by no-GEP subgroup.
#'
#' @param prediction_data Row-level prediction output.
#'
#' @return A pooled summary data frame.
summarize_pooled_no_gep_sensitivity <- function(prediction_data) {
    dplyr::bind_rows(
        prediction_data %>%
            dplyr::group_by(.data$surrogate_probability_bin) %>%
            dplyr::summarise(
                analysis = "Surrogate_Class2_Probability",
                n = dplyr::n(),
                failed_indeterminate_n = sum(.data$no_gep_group == "GEP Failed/Indeterminate"),
                not_tested_n = sum(.data$no_gep_group == "GEP Not Tested"),
                mean_predicted = mean(.data$surrogate_class2_probability, na.rm = TRUE),
                observed_mfs_5yr_event_rate = mean(.data$mfs_event_5yr == 1, na.rm = TRUE),
                observed_mss_5yr_event_rate = mean(.data$mss_event_5yr == 1, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            dplyr::rename(bin = surrogate_probability_bin),
        prediction_data %>%
            dplyr::group_by(.data$mfs_risk_bin) %>%
            dplyr::summarise(
                analysis = "Direct_MFS_5yr_Risk",
                n = dplyr::n(),
                failed_indeterminate_n = sum(.data$no_gep_group == "GEP Failed/Indeterminate"),
                not_tested_n = sum(.data$no_gep_group == "GEP Not Tested"),
                mean_predicted = mean(.data$predicted_mfs_5yr_risk, na.rm = TRUE),
                observed_mfs_5yr_event_rate = mean(.data$mfs_event_5yr == 1, na.rm = TRUE),
                observed_mss_5yr_event_rate = mean(.data$mss_event_5yr == 1, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            dplyr::rename(bin = mfs_risk_bin),
        prediction_data %>%
            dplyr::group_by(.data$mss_risk_bin) %>%
            dplyr::summarise(
                analysis = "Direct_MSS_5yr_Risk",
                n = dplyr::n(),
                failed_indeterminate_n = sum(.data$no_gep_group == "GEP Failed/Indeterminate"),
                not_tested_n = sum(.data$no_gep_group == "GEP Not Tested"),
                mean_predicted = mean(.data$predicted_mss_5yr_risk, na.rm = TRUE),
                observed_mfs_5yr_event_rate = mean(.data$mfs_event_5yr == 1, na.rm = TRUE),
                observed_mss_5yr_event_rate = mean(.data$mss_event_5yr == 1, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            dplyr::rename(bin = mss_risk_bin)
    )
}

#' Summarize No-GEP Risk Strata by Subgroup
#'
#' Produces tidy subgroup-by-bin summaries for the surrogate and direct-risk
#' outputs so the same summaries can feed both the appendix workbook and the
#' compact Objective 4 unified workbook.
#'
#' @param prediction_data Row-level prediction output.
#'
#' @return A tidy data frame with one row per subgroup, analysis, and bin.
summarize_no_gep_risk_strata <- function(prediction_data) {
    dplyr::bind_rows(
        prediction_data %>%
            dplyr::group_by(.data$no_gep_group, .data$surrogate_probability_bin) %>%
            dplyr::summarise(
                Analysis = "Surrogate_Class2_Probability",
                N = dplyr::n(),
                Mean_Predicted = mean(.data$surrogate_class2_probability, na.rm = TRUE),
                Observed_MFS_5yr_Event_Rate = mean(.data$mfs_event_5yr == 1, na.rm = TRUE),
                Observed_MSS_5yr_Event_Rate = mean(.data$mss_event_5yr == 1, na.rm = TRUE),
                Events_MFS_5yr = sum(.data$mfs_event_5yr == 1, na.rm = TRUE),
                Events_MSS_5yr = sum(.data$mss_event_5yr == 1, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            dplyr::rename(
                No_GEP_Group = no_gep_group,
                Bin = surrogate_probability_bin
            ),
        prediction_data %>%
            dplyr::group_by(.data$no_gep_group, .data$mfs_risk_bin) %>%
            dplyr::summarise(
                Analysis = "Direct_MFS_5yr_Risk",
                N = dplyr::n(),
                Mean_Predicted = mean(.data$predicted_mfs_5yr_risk, na.rm = TRUE),
                Observed_MFS_5yr_Event_Rate = mean(.data$mfs_event_5yr == 1, na.rm = TRUE),
                Observed_MSS_5yr_Event_Rate = mean(.data$mss_event_5yr == 1, na.rm = TRUE),
                Events_MFS_5yr = sum(.data$mfs_event_5yr == 1, na.rm = TRUE),
                Events_MSS_5yr = sum(.data$mss_event_5yr == 1, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            dplyr::rename(
                No_GEP_Group = no_gep_group,
                Bin = mfs_risk_bin
            ),
        prediction_data %>%
            dplyr::group_by(.data$no_gep_group, .data$mss_risk_bin) %>%
            dplyr::summarise(
                Analysis = "Direct_MSS_5yr_Risk",
                N = dplyr::n(),
                Mean_Predicted = mean(.data$predicted_mss_5yr_risk, na.rm = TRUE),
                Observed_MFS_5yr_Event_Rate = mean(.data$mfs_event_5yr == 1, na.rm = TRUE),
                Observed_MSS_5yr_Event_Rate = mean(.data$mss_event_5yr == 1, na.rm = TRUE),
                Events_MFS_5yr = sum(.data$mfs_event_5yr == 1, na.rm = TRUE),
                Events_MSS_5yr = sum(.data$mss_event_5yr == 1, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            dplyr::rename(
                No_GEP_Group = no_gep_group,
                Bin = mss_risk_bin
            )
    ) %>%
        dplyr::filter(!is.na(.data$Bin))
}

#' Create the Patient-Level No-GEP Prediction Sheet
#'
#' @param prepared_data Prepared exploratory data bundle.
#' @param no_gep_predictions Row-level no-GEP prediction data.
#'
#' @return Workbook-ready patient-level prediction table.
create_no_gep_predictions_sheet <- function(prepared_data, no_gep_predictions) {
    patient_id_col <- prepared_data$patient_id_col
    patient_id_values <- if (!is.null(patient_id_col)) {
        no_gep_predictions[[patient_id_col]]
    } else {
        seq_len(nrow(no_gep_predictions))
    }

    dplyr::bind_cols(
        tibble::tibble(patient_id = patient_id_values),
        no_gep_predictions %>%
            dplyr::select(
                no_gep_group,
                dplyr::any_of(prepared_data$predictors),
                ciliary_involvement,
                mfs_event_5yr,
                mss_event_5yr,
                surrogate_class2_probability,
                surrogate_probability_bin,
                predicted_mfs_5yr_risk,
                mfs_risk_bin,
                predicted_mss_5yr_risk,
                mss_risk_bin
            )
    )
}

#' Build Unified No-GEP Overview Rows
#'
#' @param analysis_results Output from `collect_exploratory_no_gep_analysis()`.
#'
#' @return Compact overview table for the Objective 4 unified workbook.
create_no_gep_unified_overview <- function(analysis_results) {
    group_rows <- analysis_results$data_audit %>%
        dplyr::filter(.data$section == "Group Counts") %>%
        dplyr::transmute(
            Group = .data$group,
            N = .data$n,
            Metastasis_Events_Any = .data$metastasis_events,
            Melanoma_Deaths_Any = .data$melanoma_deaths,
            MFS_5yr_Events = .data$mfs_5yr_events,
            MSS_5yr_Events = .data$mss_5yr_events,
            Complete_Predictors_N = .data$complete_predictors
        ) %>%
        dplyr::left_join(
            analysis_results$no_gep_summary %>%
                dplyr::transmute(
                    Group = .data$no_gep_group,
                    Median_Surrogate_Class2_Probability = .data$median_surrogate_class2_probability,
                    Median_Predicted_MFS_5yr_Risk = .data$median_predicted_mfs_5yr_risk,
                    Median_Predicted_MSS_5yr_Risk = .data$median_predicted_mss_5yr_risk
                ),
            by = "Group"
        ) %>%
        dplyr::mutate(
            Interpretation_Note = dplyr::case_when(
                .data$Group == "Class 1" ~ "Reference low-risk definitive GEP group.",
                .data$Group == "Class 2" ~ "Reference high-risk definitive GEP group.",
                .data$Group == "GEP Failed/Indeterminate" ~ "No-GEP subgroup with higher median baseline-only predicted risk than GEP Not Tested.",
                .data$Group == "GEP Not Tested" ~ "Larger no-GEP subgroup with intermediate observed risk between definitive Class 1 and Class 2.",
                TRUE ~ NA_character_
            )
        )

    best_baseline_row <- analysis_results$baseline_comparisons %>%
        dplyr::filter(is.finite(.data$p_value)) %>%
        dplyr::slice_min(.data$p_value, n = 1, with_ties = FALSE)

    baseline_note <- tibble::tibble(
        Group = "Baseline_Separation_Note",
        N = NA_real_,
        Metastasis_Events_Any = NA_real_,
        Melanoma_Deaths_Any = NA_real_,
        MFS_5yr_Events = NA_real_,
        MSS_5yr_Events = NA_real_,
        Complete_Predictors_N = NA_real_,
        Median_Surrogate_Class2_Probability = NA_real_,
        Median_Predicted_MFS_5yr_Risk = NA_real_,
        Median_Predicted_MSS_5yr_Risk = NA_real_,
        Interpretation_Note = if (nrow(best_baseline_row) == 1) {
            sprintf(
                "Strongest 4-group baseline separator: %s (p=%s).",
                best_baseline_row$variable[[1]],
                format_gep_p_value(best_baseline_row$p_value[[1]])
            )
        } else {
            "Strongest 4-group baseline separator unavailable."
        }
    )

    dplyr::bind_rows(group_rows, baseline_note)
}

#' Build Unified No-GEP Model Comparison Rows
#'
#' @param analysis_results Output from `collect_exploratory_no_gep_analysis()`.
#'
#' @return Compact model-comparison table for the unified workbook.
create_no_gep_unified_model_comparison <- function(analysis_results) {
    model_specs <- list(
        list(
            key = "surrogate",
            label = "Surrogate Class 2 Probability",
            training_set = "Definitive Class 1 vs Class 2 only",
            use_case = "Descriptive Class 2-like clinical resemblance only; do not use for molecular reassignment."
        ),
        list(
            key = "mfs",
            label = "Direct 5-Year MFS Risk",
            training_set = "Full eligible cohort with 5-year metastasis endpoint",
            use_case = "Primary baseline-only clinical risk estimate when GEP is unusable."
        ),
        list(
            key = "mss",
            label = "Direct 5-Year MSS Risk",
            training_set = "Full eligible cohort with 5-year melanoma-specific death endpoint",
            use_case = "Primary baseline-only melanoma-specific risk estimate when GEP is unusable."
        )
    )

    purrr::map_dfr(model_specs, function(spec) {
        model_results <- if (identical(spec$key, "surrogate")) analysis_results$surrogate_model else analysis_results$direct_models[[spec$key]]
        top_predictors <- model_results$predictor_contributions %>%
            dplyr::slice_head(n = 3) %>%
            dplyr::mutate(
                formatted = sprintf(
                    "%s (%s; %s=%.3f)",
                    .data$predictor,
                    .data$dominant_term,
                    "coef",
                    .data$standardized_coefficient
                )
            ) %>%
            dplyr::pull(.data$formatted)

        tibble::tibble(
            Model = spec$label,
            Training_Set = spec$training_set,
            N = model_results$metrics$n[[1]],
            Events = model_results$metrics$events[[1]],
            Apparent_AUC = model_results$metrics$apparent_auc[[1]],
            CV_AUC = model_results$metrics$cv_auc[[1]],
            Apparent_Brier = model_results$metrics$apparent_brier[[1]],
            CV_Brier = model_results$metrics$cv_brier[[1]],
            Calibration_Status = model_results$metrics$calibration_status[[1]],
            Calibration_Intercept = model_results$metrics$calibration_intercept[[1]],
            Calibration_Slope = model_results$metrics$calibration_slope[[1]],
            Top_Predictor_1 = top_predictors[[1]] %||% NA_character_,
            Top_Predictor_2 = top_predictors[[2]] %||% NA_character_,
            Top_Predictor_3 = top_predictors[[3]] %||% NA_character_,
            Use_Case = spec$use_case
        )
    })
}

#' Collect Exploratory No-GEP Analysis Results
#'
#' Computes the reusable no-GEP summary objects shared by the standalone
#' appendix workbook and the compact Objective 4 unified workbook.
#'
#' @param data Cohort data frame.
#' @param dataset_name Dataset identifier.
#' @param verify_km_fix Whether to run the KM verification helper.
#' @param visual_file Retained for compatibility with the verification helper.
#'
#' @return A list of reusable no-GEP analysis objects and summary tables.
collect_exploratory_no_gep_analysis <- function(data,
                                                dataset_name = "uveal_melanoma_full_cohort",
                                                verify_km_fix = TRUE,
                                                visual_file = here("scripts", "gep", "visualization", "gep_visuals.R")) {
    if (!identical(dataset_name, "uveal_melanoma_full_cohort")) {
        stop("collect_exploratory_no_gep_analysis() currently supports only uveal_melanoma_full_cohort.")
    }

    km_verification <- if (isTRUE(verify_km_fix)) {
        verify_exploratory_no_gep_km_fix(data, dataset_name = dataset_name, visual_file = visual_file)
    } else {
        prepare_exploratory_no_gep_data(data, dataset_name = dataset_name)$full_data %>%
            dplyr::filter(!is.na(.data$exploratory_gep_group)) %>%
            dplyr::count(.data$exploratory_gep_group, name = "observed_n") %>%
            dplyr::mutate(
                expected_n = c(58L, 27L, 13L, 162L),
                status = "verification_skipped"
            )
    }

    prepared_data <- prepare_exploratory_no_gep_data(data, dataset_name = dataset_name)
    full_data <- prepared_data$full_data
    data_audit <- summarize_exploratory_data_audit(prepared_data, km_verification = km_verification)
    baseline_summary <- summarize_exploratory_baseline_comparisons(prepared_data)

    km_times <- c(60, 84, 120)
    km_corrected_mfs <- summarize_km_timepoints(
        full_data %>% dplyr::filter(!is.na(.data$exploratory_gep_group)),
        group_var = "exploratory_gep_group",
        time_var = "tt_mets_months",
        event_var = "mets_event",
        times = km_times
    )
    km_corrected_mss <- summarize_mss_cif_timepoints(
        full_data %>% dplyr::filter(!is.na(.data$exploratory_gep_group)),
        group_var = "exploratory_gep_group",
        times = km_times
    )

    surrogate_model <- fit_exploratory_binary_model(
        prepared_data$definitive_training,
        outcome_var = "class2_outcome",
        predictors = prepared_data$predictors,
        model_name = "Surrogate Class 2 Probability"
    )
    direct_mfs_model <- fit_exploratory_binary_model(
        prepared_data$mfs_model_data,
        outcome_var = "mfs_event_5yr",
        predictors = prepared_data$predictors,
        model_name = "Direct 5-Year MFS Risk"
    )
    direct_mss_model <- fit_exploratory_binary_model(
        prepared_data$mss_model_data,
        outcome_var = "mss_event_5yr",
        predictors = prepared_data$predictors,
        model_name = "Direct 5-Year MSS Risk"
    )

    no_gep_predictions <- prepared_data$no_gep_prediction %>%
        dplyr::mutate(
            surrogate_class2_probability = predict_exploratory_binary_model(surrogate_model, .),
            predicted_mfs_5yr_risk = predict_exploratory_binary_model(direct_mfs_model, .),
            predicted_mss_5yr_risk = predict_exploratory_binary_model(direct_mss_model, .)
        ) %>%
        dplyr::mutate(
            surrogate_probability_bin = create_quantile_bins(.data$surrogate_class2_probability),
            mfs_risk_bin = create_quantile_bins(.data$predicted_mfs_5yr_risk),
            mss_risk_bin = create_quantile_bins(.data$predicted_mss_5yr_risk)
        )

    no_gep_predictions_sheet <- create_no_gep_predictions_sheet(prepared_data, no_gep_predictions)
    no_gep_summary <- summarize_no_gep_predictions(no_gep_predictions)
    sensitivity_summary <- summarize_pooled_no_gep_sensitivity(no_gep_predictions)
    risk_strata_summary <- summarize_no_gep_risk_strata(no_gep_predictions)
    predictor_contribution <- create_predictor_contribution_tab(
        prepared_data = prepared_data,
        surrogate_model = surrogate_model,
        mfs_model = direct_mfs_model,
        mss_model = direct_mss_model
    )
    summary_guide <- create_exploratory_summary_guide_tab(
        prepared_data = prepared_data,
        surrogate_model = surrogate_model,
        mfs_model = direct_mfs_model,
        mss_model = direct_mss_model,
        no_gep_summary = no_gep_summary
    )

    analysis_results <- list(
        prepared_data = prepared_data,
        km_verification = km_verification,
        data_audit = data_audit,
        baseline_comparisons = baseline_summary,
        km_corrected_mfs = km_corrected_mfs,
        km_corrected_mss = km_corrected_mss,
        surrogate_model = surrogate_model,
        direct_models = list(
            mfs = direct_mfs_model,
            mss = direct_mss_model
        ),
        predictor_contribution = predictor_contribution,
        summary_and_guide = summary_guide,
        no_gep_summary = no_gep_summary,
        no_gep_predictions = no_gep_predictions,
        no_gep_predictions_sheet = no_gep_predictions_sheet,
        sensitivity_summary = sensitivity_summary,
        risk_strata_summary = risk_strata_summary
    )

    analysis_results$unified_no_gep_overview <- create_no_gep_unified_overview(analysis_results)
    analysis_results$unified_no_gep_model_comparison <- create_no_gep_unified_model_comparison(analysis_results)
    analysis_results$unified_no_gep_risk_strata <- risk_strata_summary

    analysis_results
}

#' Create a Corrected Exploratory MFS KM Plot
#'
#' Builds a lightweight KM figure using the four exploratory GEP groupings for
#' the report output folder.
#'
#' @param data Prepared exploratory cohort.
#' @param output_path File path for the saved PNG.
#'
#' @return Invisibly returns the saved plot path.
create_exploratory_mfs_km_plot <- function(data, output_path) {
    fit <- survival::survfit(
        survival::Surv(tt_mets_months, mets_event) ~ exploratory_gep_group,
        data = data
    )
    fit_summary <- summary(fit)

    plot_data <- tibble::tibble(
        group = sub("exploratory_gep_group=", "", fit_summary$strata),
        time_months = fit_summary$time,
        survival_probability = fit_summary$surv
    ) %>%
        dplyr::bind_rows(
            tibble::tibble(
                group = levels(droplevels(data$exploratory_gep_group)),
                time_months = 0,
                survival_probability = 1
            )
        )

    palette <- get_palette_by_variable("biopsy1_gep", levels(droplevels(data$exploratory_gep_group)))
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$time_months, y = .data$survival_probability, color = .data$group)) +
        ggplot2::geom_step(linewidth = 1.1) +
        ggplot2::scale_color_manual(values = palette) +
        ggplot2::scale_y_continuous(labels = function(x) sprintf("%.0f", 100 * x), limits = c(0, 1)) +
        ggplot2::labs(
            title = "Corrected Exploratory MFS Curves",
            subtitle = "Full cohort: Class 1, Class 2, GEP Failed/Indeterminate, GEP Not Tested",
            x = "Time (months)",
            y = "Metastasis-Free Survival Probability (%)",
            color = "Group"
        ) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(legend.position = "bottom")

    ggplot2::ggsave(output_path, plot, width = 12, height = 8, dpi = PLOT_DPI, bg = "white")
    invisible(output_path)
}

#' Tidy MSS CIF Curves for Plotting
#'
#' Converts the project competing-risks fit into a long plotting data frame for
#' exploratory MSS visualization.
#'
#' @param data Prepared exploratory cohort.
#'
#' @return A long data frame of cumulative incidence curves.
tidy_mss_cif_curves <- function(data) {
    status <- dplyr::case_when(
        data$melanoma_death_event == 1 ~ 1L,
        data$competing_death_event == 1 ~ 2L,
        TRUE ~ 0L
    )

    cif_fit <- cmprsk::cuminc(
        ftime = data$tt_death_months,
        fstatus = status,
        group = data$exploratory_gep_group,
        cencode = 0
    )

    event_names <- names(cif_fit)[grepl(" 1$", names(cif_fit))]

    purrr::map_dfr(event_names, function(event_name) {
        tibble::tibble(
            group = sub(" 1$", "", event_name),
            time_months = cif_fit[[event_name]]$time,
            cumulative_incidence = cif_fit[[event_name]]$est
        )
    }) %>%
        dplyr::bind_rows(
            tibble::tibble(
                group = levels(droplevels(data$exploratory_gep_group)),
                time_months = 0,
                cumulative_incidence = 0
            )
        )
}

#' Create a Corrected Exploratory MSS CIF Plot
#'
#' Builds a lightweight cumulative-incidence figure for melanoma-specific death
#' across the four exploratory GEP groups.
#'
#' @param data Prepared exploratory cohort.
#' @param output_path File path for the saved PNG.
#'
#' @return Invisibly returns the saved plot path.
create_exploratory_mss_cif_plot <- function(data, output_path) {
    plot_data <- tidy_mss_cif_curves(data)
    palette <- get_palette_by_variable("biopsy1_gep", levels(droplevels(data$exploratory_gep_group)))

    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$time_months, y = .data$cumulative_incidence, color = .data$group)) +
        ggplot2::geom_step(linewidth = 1.1) +
        ggplot2::scale_color_manual(values = palette) +
        ggplot2::scale_y_continuous(labels = function(x) sprintf("%.0f", 100 * x), limits = c(0, 1)) +
        ggplot2::labs(
            title = "Corrected Exploratory MSS CIF Curves",
            subtitle = "Melanoma-specific death cumulative incidence with competing death retained",
            x = "Time (months)",
            y = "Cumulative Incidence of Melanoma-Specific Death (%)",
            color = "Group"
        ) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(legend.position = "bottom")

    ggplot2::ggsave(output_path, plot, width = 12, height = 8, dpi = PLOT_DPI, bg = "white")
    invisible(output_path)
}

#' Create a Probability Density Plot
#'
#' Draws overlaid density curves comparing the two no-GEP groups for one
#' predicted probability measure.
#'
#' @param data Row-level prediction data.
#' @param probability_col Name of the probability column to plot.
#' @param plot_title Plot title.
#' @param output_path File path for the saved PNG.
#'
#' @return Invisibly returns the saved plot path.
create_probability_density_plot <- function(data, probability_col, plot_title, output_path) {
    plot <- ggplot2::ggplot(
        data,
        ggplot2::aes(
            x = .data[[probability_col]],
            color = .data$no_gep_group,
            fill = .data$no_gep_group
        )
    ) +
        ggplot2::geom_density(alpha = 0.2, adjust = 1.1) +
        ggplot2::scale_x_continuous(labels = function(x) sprintf("%.0f%%", 100 * x), limits = c(0, 1)) +
        ggplot2::scale_color_manual(values = get_palette_by_variable("biopsy1_gep", unique(data$no_gep_group))) +
        ggplot2::scale_fill_manual(values = get_palette_by_variable("biopsy1_gep", unique(data$no_gep_group))) +
        ggplot2::labs(
            title = plot_title,
            x = "Predicted probability",
            y = "Density",
            color = "No-GEP group",
            fill = "No-GEP group"
        ) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(legend.position = "bottom")

    ggplot2::ggsave(output_path, plot, width = 11, height = 7, dpi = PLOT_DPI, bg = "white")
    invisible(output_path)
}

#' Create an Observed Event-Rate by Bin Plot
#'
#' Visualizes observed outcome rates across predicted-risk bins for one of the
#' exploratory surrogate or direct-risk analyses.
#'
#' @param summary_data Summary data produced for the sensitivity tab.
#' @param analysis_name Analysis label to filter.
#' @param event_col Column containing the observed event rate.
#' @param plot_title Plot title.
#' @param output_path File path for the saved PNG.
#'
#' @return Invisibly returns the saved plot path.
create_event_rate_bin_plot <- function(summary_data, analysis_name, event_col, plot_title, output_path) {
    plot_data <- summary_data %>%
        dplyr::filter(.data$analysis == analysis_name, !is.na(.data$bin))

    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$bin, y = .data[[event_col]])) +
        ggplot2::geom_col(fill = "#0072B5FF", width = 0.7) +
        ggplot2::geom_text(
            ggplot2::aes(label = sprintf("n=%d", .data$n)),
            vjust = -0.4,
            size = 4
        ) +
        ggplot2::scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100 * x), limits = c(0, max(plot_data[[event_col]], na.rm = TRUE) * 1.2 + 0.01)) +
        ggplot2::labs(
            title = plot_title,
            x = "Predicted-risk bin",
            y = "Observed event rate"
        ) +
        ggplot2::theme_minimal(base_size = 14)

    ggplot2::ggsave(output_path, plot, width = 10, height = 6, dpi = PLOT_DPI, bg = "white")
    invisible(output_path)
}

#' Prepend Short Guide Text to a Workbook Sheet
#'
#' @param sheet_data Data frame to be written to the workbook.
#' @param guide_lines Character vector of helper text lines.
#'
#' @return A data frame with guide rows prepended.
prepend_sheet_guide <- function(sheet_data, guide_lines) {
    guide_frame <- tibble::tibble(
        section = "guide",
        guide_text = guide_lines
    )

    dplyr::bind_rows(guide_frame, sheet_data)
}

#' Create the Workbook Summary and Guide Tab
#'
#' @param prepared_data Prepared exploratory data bundle.
#' @param surrogate_model Surrogate model bundle.
#' @param mfs_model Direct MFS model bundle.
#' @param mss_model Direct MSS model bundle.
#' @param no_gep_summary No-GEP group summary table.
#'
#' @return A summary-and-guide data frame.
create_exploratory_summary_guide_tab <- function(prepared_data,
                                                 surrogate_model,
                                                 mfs_model,
                                                 mss_model,
                                                 no_gep_summary) {
    top_surrogate <- surrogate_model$predictor_contributions %>% dplyr::slice_head(n = 3)
    top_mfs <- mfs_model$predictor_contributions %>% dplyr::slice_head(n = 3)
    top_mss <- mss_model$predictor_contributions %>% dplyr::slice_head(n = 3)

    summary_rows <- tibble::tribble(
        ~section, ~item, ~detail,
        "What was computed", "Surrogate model", "Ridge-penalized logistic model trained only on patients with definitive Class 1 or Class 2 GEP results. It learns which baseline clinical features are more typical of the observed Class 1 and Class 2 groups, then gives each no-GEP patient a clinical resemblance probability showing how much their baseline profile looks like the observed Class 2 pattern rather than the observed Class 1 pattern in this cohort.",
        "What was computed", "Direct MFS model", "Ridge-penalized logistic model estimating 5-year metastasis risk in the full eligible cohort.",
        "What was computed", "Direct MSS model", "Ridge-penalized logistic model estimating 5-year melanoma-specific death risk in the full eligible cohort.",
        "Primary findings", "Molecular reassignment", "The surrogate output is a clinical resemblance score anchored to definitive Class 1 and Class 2 patients. It is descriptive only and should not be treated as a recovered molecular class label.",
        "Primary findings", "Direct risk use", "Direct MFS/MSS outputs are the primary clinically usable outputs for no-GEP patients.",
        "How to interpret", "AUC", "AUC near 0.5 means poor discrimination; 0.6 to 0.7 means modest but usable ranking; >0.7 is stronger separation.",
        "How to interpret", "Calibration", "Calibration slope/intercept summarize whether predicted risks are systematically too high or too low; sparse data can limit interpretation.",
        "How to interpret", "Density plots", "A right-shifted density indicates a subgroup receiving higher predicted risk overall.",
        "How to interpret", "Bin plots", "Observed event rates should generally increase from low to high predicted-risk bins if ranking is clinically useful.",
        "How to interpret", "Individual use", "Treat patient-level predictions as exploratory risk support. The surrogate score answers which known class pattern a patient looks more like clinically, not what their true assay result must have been."
    )

    metric_rows <- tibble::tribble(
        ~section, ~item, ~detail,
        "Key metrics", "Surrogate CV AUC", sprintf("%.3f", surrogate_model$metrics$cv_auc[[1]]),
        "Key metrics", "Direct MFS CV AUC", sprintf("%.3f", mfs_model$metrics$cv_auc[[1]]),
        "Key metrics", "Direct MSS CV AUC", sprintf("%.3f", mss_model$metrics$cv_auc[[1]]),
        "Key metrics", "Failed/Indeterminate median 5-year MFS risk", sprintf("%.3f", no_gep_summary$median_predicted_mfs_5yr_risk[no_gep_summary$no_gep_group == "GEP Failed/Indeterminate"]),
        "Key metrics", "Not Tested median 5-year MFS risk", sprintf("%.3f", no_gep_summary$median_predicted_mfs_5yr_risk[no_gep_summary$no_gep_group == "GEP Not Tested"])
    )

    top_rows <- dplyr::bind_rows(
        top_surrogate %>% dplyr::transmute(section = "Top predictors", item = paste0("Surrogate rank ", .data$rank), detail = sprintf("%s (%s)", .data$predictor, .data$direction)),
        top_mfs %>% dplyr::transmute(section = "Top predictors", item = paste0("Direct MFS rank ", .data$rank), detail = sprintf("%s (%s)", .data$predictor, .data$direction)),
        top_mss %>% dplyr::transmute(section = "Top predictors", item = paste0("Direct MSS rank ", .data$rank), detail = sprintf("%s (%s)", .data$predictor, .data$direction))
    )

    predictor_rows <- dplyr::bind_rows(
        prepared_data$predictor_screening %>%
            dplyr::filter(.data$status == "retained") %>%
            dplyr::transmute(section = "Predictor screening", item = paste0("Retained: ", .data$predictor), detail = .data$reason),
        prepared_data$predictor_screening %>%
            dplyr::filter(.data$status == "dropped") %>%
            dplyr::transmute(section = "Predictor screening", item = paste0("Dropped: ", .data$predictor), detail = .data$reason)
    )

    dplyr::bind_rows(summary_rows, metric_rows, top_rows, predictor_rows)
}

#' Create the Predictor Contribution Workbook Tab
#'
#' @param prepared_data Prepared exploratory data bundle.
#' @param surrogate_model Surrogate model bundle.
#' @param mfs_model Direct MFS model bundle.
#' @param mss_model Direct MSS model bundle.
#'
#' @return A workbook-ready contribution table.
create_predictor_contribution_tab <- function(prepared_data,
                                              surrogate_model,
                                              mfs_model,
                                              mss_model) {
    screening_section <- prepared_data$predictor_screening %>%
        dplyr::mutate(section = "predictor_screening")

    contribution_section <- dplyr::bind_rows(
        surrogate_model$predictor_contributions,
        mfs_model$predictor_contributions,
        mss_model$predictor_contributions
    ) %>%
        dplyr::mutate(section = "model_contribution")

    dplyr::bind_rows(screening_section, contribution_section)
}

format_exploratory_group_percentages <- function(values) {
    paste(
        sprintf("%s=%.1f%%", names(values), 100 * values),
        collapse = ", "
    )
}

format_exploratory_group_medians <- function(values) {
    paste(
        sprintf("%s=%.1f", names(values), values),
        collapse = ", "
    )
}

trim_exploratory_dominant_term <- function(predictor, dominant_term) {
    if (is.na(dominant_term) || !nzchar(dominant_term)) {
        return("term not recorded")
    }

    trimmed <- sub(sprintf("^%s", predictor), "", dominant_term)
    if (!nzchar(trimmed)) {
        dominant_term
    } else {
        trimmed
    }
}

summarize_exploratory_predictor_context <- function(prepared_data, predictor) {
    data <- prepared_data$full_data %>%
        dplyr::filter(!is.na(.data$exploratory_gep_group))

    if (identical(predictor, "initial_t_stage_simple")) {
        advanced_stage <- data %>%
            dplyr::group_by(.data$exploratory_gep_group) %>%
            dplyr::summarise(
                value = mean(.data$initial_t_stage_simple %in% c("T3", "T4"), na.rm = TRUE),
                .groups = "drop"
            )

        return(sprintf(
            "Advanced stage (T3/T4) was %s across the four exploratory groups.",
            format_exploratory_group_percentages(stats::setNames(advanced_stage$value, advanced_stage$exploratory_gep_group))
        ))
    }

    if (predictor %in% c("age_at_diagnosis", "initial_tumor_height", "initial_tumor_diameter", "initial_vision")) {
        medians <- data %>%
            dplyr::group_by(.data$exploratory_gep_group) %>%
            dplyr::summarise(
                value = stats::median(.data[[predictor]], na.rm = TRUE),
                .groups = "drop"
            )

        label <- c(
            age_at_diagnosis = "Group medians were",
            initial_tumor_height = "Tumor-height medians were",
            initial_tumor_diameter = "Tumor-diameter medians were",
            initial_vision = "Baseline-vision medians were"
        )[[predictor]]

        return(sprintf(
            "%s %s.",
            label,
            format_exploratory_group_medians(stats::setNames(medians$value, medians$exploratory_gep_group))
        ))
    }

    if (identical(predictor, "sex")) {
        male_share <- data %>%
            dplyr::group_by(.data$exploratory_gep_group) %>%
            dplyr::summarise(
                value = mean(as.character(.data$sex) == "Male", na.rm = TRUE),
                .groups = "drop"
            )

        return(sprintf(
            "Male share was %s; sex distributions were broadly similar across groups, so this should be read as model-specific weighting rather than strong stand-alone separation.",
            format_exploratory_group_percentages(stats::setNames(male_share$value, male_share$exploratory_gep_group))
        ))
    }

    if (identical(predictor, "srf")) {
        srf_share <- data %>%
            dplyr::group_by(.data$exploratory_gep_group) %>%
            dplyr::summarise(
                value = mean(as.character(.data$srf) == "Yes", na.rm = TRUE),
                .groups = "drop"
            )

        return(sprintf(
            "SRF prevalence was %s, so the coefficient direction is more supportable than a strong four-group separation claim.",
            format_exploratory_group_percentages(stats::setNames(srf_share$value, srf_share$exploratory_gep_group))
        ))
    }

    if (identical(predictor, "optic_nerve_involvement")) {
        optic_share <- data %>%
            dplyr::group_by(.data$exploratory_gep_group) %>%
            dplyr::summarise(
                value = mean(.data$optic_nerve_involvement == 1, na.rm = TRUE),
                .groups = "drop"
            )

        return(sprintf(
            "Optic-nerve involvement was %s, so this term appears to be a model-level discriminator rather than a single dominant baseline separator.",
            format_exploratory_group_percentages(stats::setNames(optic_share$value, optic_share$exploratory_gep_group))
        ))
    }

    sprintf("%s ranked highly in the penalized model, but only coefficient direction is directly supportable from the current descriptive summaries.", predictor)
}

summarize_exploratory_bin_pattern <- function(sensitivity_summary, analysis_name, event_col) {
    rows <- sensitivity_summary %>%
        dplyr::filter(.data$analysis == analysis_name, .data$bin %in% c("Low", "Intermediate", "High")) %>%
        dplyr::mutate(bin = factor(.data$bin, levels = c("Low", "Intermediate", "High"))) %>%
        dplyr::arrange(.data$bin)

    if (nrow(rows) != 3) {
        return("Observed risk-bin separation was unavailable from the current summary table.")
    }

    sprintf(
        "%s-event rates across pooled %s bins were Low=%.1f%%, Intermediate=%.1f%%, and High=%.1f%%.",
        if (identical(event_col, "observed_mfs_5yr_event_rate")) "5-year MFS " else "5-year MSS ",
        analysis_name,
        100 * rows[[event_col]][[1]],
        100 * rows[[event_col]][[2]],
        100 * rows[[event_col]][[3]]
    )
}

create_exploratory_top_predictor_block <- function(model_label,
                                                   model_results,
                                                   prepared_data,
                                                   sensitivity_summary,
                                                   analysis_name,
                                                   event_col,
                                                   model_context) {
    top_rows <- model_results$predictor_contributions %>%
        dplyr::slice_head(n = 3)

    predictor_lines <- purrr::map_chr(seq_len(nrow(top_rows)), function(i) {
        row <- top_rows[i, , drop = FALSE]
        sprintf(
            "  * %s ranked %s (dominant term %s, standardized coefficient %.3f; %s). %s",
            row$predictor[[1]],
            c("first", "second", "third")[[i]],
            trim_exploratory_dominant_term(row$predictor[[1]], row$dominant_term[[1]]),
            row$standardized_coefficient[[1]],
            row$direction[[1]],
            summarize_exploratory_predictor_context(prepared_data, row$predictor[[1]])
        )
    })

    c(
        sprintf("- %s: %s %s", model_label, model_context, summarize_exploratory_bin_pattern(sensitivity_summary, analysis_name, event_col)),
        predictor_lines
    )
}

#' Write the Exploratory No-GEP Narrative Summary
#'
#' Generates the short reader-facing text summary that accompanies the workbook.
#'
#' @param dataset_name Dataset identifier used in the summary header.
#' @param data_audit Data audit summary table.
#' @param baseline_summary Baseline comparison summary table.
#' @param surrogate_model Surrogate Class 2-like model result bundle.
#' @param mfs_model Direct 5-year MFS model result bundle.
#' @param mss_model Direct 5-year MSS model result bundle.
#' @param no_gep_summary Grouped no-GEP summary table.
#' @param output_path File path for the written text summary.
#'
#' @return Invisibly returns the saved summary path.
create_exploratory_no_gep_summary_text <- function(dataset_name,
                                                   data_audit,
                                                   baseline_summary,
                                                   prepared_data,
                                                   predictor_screening,
                                                   surrogate_model,
                                                   mfs_model,
                                                   mss_model,
                                                   no_gep_summary,
                                                   sensitivity_summary,
                                                   output_path) {
    failed_row <- no_gep_summary %>% dplyr::filter(.data$no_gep_group == "GEP Failed/Indeterminate")
    not_tested_row <- no_gep_summary %>% dplyr::filter(.data$no_gep_group == "GEP Not Tested")
    retained_predictors <- predictor_screening %>%
        dplyr::filter(.data$status == "retained") %>%
        dplyr::pull(.data$predictor)
    dropped_predictors <- predictor_screening %>%
        dplyr::filter(.data$status == "dropped") %>%
        dplyr::transmute(text = sprintf("%s (%s)", .data$predictor, .data$reason)) %>%
        dplyr::pull(.data$text)
    best_baseline_row <- baseline_summary %>%
        dplyr::filter(is.finite(.data$p_value)) %>%
        dplyr::slice_min(.data$p_value, n = 1, with_ties = FALSE)

    top_predictor_block <- c(
        "Top predictors with data-backed context:",
        create_exploratory_top_predictor_block(
            model_label = "Surrogate Class 2-like",
            model_results = surrogate_model,
            prepared_data = prepared_data,
            sensitivity_summary = sensitivity_summary,
            analysis_name = "Surrogate_Class2_Probability",
            event_col = "observed_mfs_5yr_event_rate",
            model_context = sprintf(
                "This is a descriptive Class 2-like clinical resemblance model rather than a molecular classifier. CV AUC was %.3f.",
                surrogate_model$metrics$cv_auc[[1]]
            )
        ),
        create_exploratory_top_predictor_block(
            model_label = "Direct 5-year MFS",
            model_results = mfs_model,
            prepared_data = prepared_data,
            sensitivity_summary = sensitivity_summary,
            analysis_name = "Direct_MFS_5yr_Risk",
            event_col = "observed_mfs_5yr_event_rate",
            model_context = sprintf(
                "This is the main baseline-only metastasis-risk model for no-GEP patients. CV AUC was %.3f.",
                mfs_model$metrics$cv_auc[[1]]
            )
        ),
        create_exploratory_top_predictor_block(
            model_label = "Direct 5-year MSS",
            model_results = mss_model,
            prepared_data = prepared_data,
            sensitivity_summary = sensitivity_summary,
            analysis_name = "Direct_MSS_5yr_Risk",
            event_col = "observed_mss_5yr_event_rate",
            model_context = sprintf(
                "This is the main baseline-only melanoma-specific risk model for no-GEP patients. CV AUC was %.3f.",
                mss_model$metrics$cv_auc[[1]]
            )
        )
    )

    summary_lines <- c(
        "EXPLORATORY NO-GEP RISK REPORT",
        "==============================",
        "",
        sprintf("Dataset: %s", dataset_name),
        "",
        "What was computed:",
        "- A ridge-penalized surrogate model was trained only on patients with definitive Class 1 or Class 2 GEP results.",
        "- That model learned what the baseline clinical patterns of the observed Class 1 and Class 2 patients looked like in this cohort, then assigned each no-GEP patient a Class 2-like clinical resemblance probability from those baseline features.",
        "- Two ridge-penalized direct clinical models estimated 5-year MFS and 5-year MSS risk using baseline variables available without a usable GEP result.",
        "",
        "Interpretation guardrails:",
        "- This report improves clinical risk assessment; it does not claim to recover the true molecular GEP class.",
        "- GEP Failed/Indeterminate and GEP Not Tested are reported separately because they behave like distinct clinical populations.",
        "- The surrogate output should be read as: among patients with known Class 1 and Class 2 results, which known clinical pattern does this no-GEP patient look more like?",
        "- The surrogate stores P(Class 2-like | baseline features); its complement, 1 - P(Class 2-like | baseline features), is only Class 1-like resemblance within this surrogate and is not a true molecular Class 1 probability.",
        "",
        "Data audit highlights:",
        sprintf(
            "- Group counts: %s",
            paste(sprintf("%s=%d", data_audit$group[seq_len(4)], data_audit$n[seq_len(4)]), collapse = ", ")
        ),
        sprintf(
            "- Surrogate Class 2 model: apparent AUC %.3f, 5-fold CV AUC %.3f",
            surrogate_model$metrics$apparent_auc[1],
            surrogate_model$metrics$cv_auc[1]
        ),
        sprintf(
            "- Direct 5-year MFS risk model: apparent AUC %.3f, 5-fold CV AUC %.3f",
            mfs_model$metrics$apparent_auc[1],
            mfs_model$metrics$cv_auc[1]
        ),
        sprintf(
            "- Direct 5-year MSS risk model: apparent AUC %.3f, 5-fold CV AUC %.3f",
            mss_model$metrics$apparent_auc[1],
            mss_model$metrics$cv_auc[1]
        ),
        "",
        "Result interpretation:",
        "- The surrogate Class 2-like model is a descriptive only clinical resemblance score and shows only modest discrimination, so it should not be used to relabel patients as true Class 1 or Class 2.",
        "- The direct MFS/MSS models provide moderate risk stratification and are the preferred outputs when a patient has no usable GEP.",
        "- Higher predicted-risk bins should be interpreted as clinically higher-risk groups, not as precise deterministic individual forecasts.",
        "",
        "No-GEP prediction summary:",
        sprintf(
            "- Failed/Indeterminate: median Class 2-like probability %.3f, median predicted 5-year MFS risk %.3f, median predicted 5-year MSS risk %.3f",
            failed_row$median_surrogate_class2_probability[[1]],
            failed_row$median_predicted_mfs_5yr_risk[[1]],
            failed_row$median_predicted_mss_5yr_risk[[1]]
        ),
        sprintf(
            "- Not Tested: median Class 2-like probability %.3f, median predicted 5-year MFS risk %.3f, median predicted 5-year MSS risk %.3f",
            not_tested_row$median_surrogate_class2_probability[[1]],
            not_tested_row$median_predicted_mfs_5yr_risk[[1]],
            not_tested_row$median_predicted_mss_5yr_risk[[1]]
        ),
        "",
        "Retained baseline predictors used in all exploratory models:",
        paste0("- ", retained_predictors),
        "",
        "Dropped candidate predictors:",
        if (length(dropped_predictors) > 0) paste0("- ", dropped_predictors) else "- None",
        "",
        top_predictor_block,
        "",
        "Baseline separation note:",
        sprintf(
            "- The strongest 4-group separation among candidate baseline predictors was %s (p=%s).",
            best_baseline_row$variable[[1]],
            format_gep_p_value(best_baseline_row$p_value[[1]])
        )
    )

    summary_lines <- c(
        summary_lines,
        "",
        "Ciliary coding note:",
        "- Ciliary involvement is now derived from location values containing 'Cilio' or 'Ciliary'. The earlier all-zero derivation was incorrect because the source data encode this location as 'Cilio-Choroidal'."
    )

    writeLines(summary_lines, output_path)
    invisible(output_path)
}

#' Run the Exploratory No-GEP Report Workflow
#'
#' Generates the standalone workbook, summary text, and plots requested for
#' patients without usable GEP, while keeping the analysis separate from the
#' routine Objective 4 production pipeline.
#'
#' @param dataset_name Dataset identifier. Currently only the full uveal cohort
#'   is supported.
#' @param output_dir Optional output directory. Defaults to the agreed
#'   `d_exploratory_no_gep` location under the cohort's Objective 4 folder.
#' @param verify_km_fix Whether to verify the simplified KM risk-table fix
#'   before running the report.
#' @param data Optional in-memory cohort data. When `NULL`, the function reads
#'   the processed cohort from disk.
#' @param visual_file Retained for compatibility with the verification helper.
#' @param collected_results Optional output from
#'   `collect_exploratory_no_gep_analysis()`. When supplied, the report reuses
#'   those results instead of recomputing them.
#'
#' @return A list containing the major report tables, model objects, and output
#'   file paths.
run_exploratory_no_gep_report <- function(dataset_name = "uveal_melanoma_full_cohort",
                                          output_dir = NULL,
                                          verify_km_fix = TRUE,
                                          data = NULL,
                                          visual_file = here("scripts", "gep", "visualization", "gep_visuals.R"),
                                          collected_results = NULL) {
    if (!identical(dataset_name, "uveal_melanoma_full_cohort")) {
        stop("run_exploratory_no_gep_report() currently supports only uveal_melanoma_full_cohort.")
    }

    logger::log_info("Starting exploratory no-GEP risk report")

    if (is.null(data)) {
        data <- readRDS(file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds")))
    }

    if (is.null(output_dir)) {
        cohort_outputs <- setup_cohort_outputs(dataset_name)
        output_dir <- file.path(cohort_outputs$cohort_base_dir, "04_GEP_Validation", "d_exploratory_no_gep")
    }

    output_dir <- ensure_output_dir(output_dir)
    plots_dir <- ensure_output_dir(file.path(output_dir, "plots"))
    analysis_results <- collected_results %||% collect_exploratory_no_gep_analysis(
        data = data,
        dataset_name = dataset_name,
        verify_km_fix = verify_km_fix,
        visual_file = visual_file
    )

    prepared_data <- analysis_results$prepared_data
    full_data <- prepared_data$full_data
    data_audit <- analysis_results$data_audit
    baseline_summary <- analysis_results$baseline_comparisons
    km_corrected_mfs <- analysis_results$km_corrected_mfs
    km_corrected_mss <- analysis_results$km_corrected_mss
    surrogate_model <- analysis_results$surrogate_model
    direct_mfs_model <- analysis_results$direct_models$mfs
    direct_mss_model <- analysis_results$direct_models$mss
    predictor_contribution <- analysis_results$predictor_contribution
    summary_guide <- analysis_results$summary_and_guide
    no_gep_summary <- analysis_results$no_gep_summary
    no_gep_predictions <- analysis_results$no_gep_predictions
    no_gep_predictions_sheet <- analysis_results$no_gep_predictions_sheet
    sensitivity_summary <- analysis_results$sensitivity_summary

    workbook_data <- list(
        Summary_and_Guide = summary_guide,
        Predictor_Contribution = predictor_contribution,
        Data_Audit = data_audit,
        Baseline_Comparisons = baseline_summary,
        KM_Corrected_MFS = km_corrected_mfs,
        KM_Corrected_MSS = km_corrected_mss,
        Surrogate_Class2_Model = prepend_sheet_guide(
            create_model_workbook_tab(surrogate_model),
            c(
                "Surrogate Class 2-like probability is a clinical resemblance score based on known Class 1 versus Class 2 baseline patterns; it is not equivalent to true molecular class reassignment.",
                "Metrics summarize how well baseline features distinguish the known classes in this cohort; coefficients are ridge-shrunken terms for directionality and ranking, not p-value-based inference.",
                sprintf("Retained predictors: %s", paste(prepared_data$predictors, collapse = ", "))
            )
        ),
        Direct_MFS_Risk_Model = prepend_sheet_guide(
            create_model_workbook_tab(direct_mfs_model),
            c(
                "This is the primary direct-risk model for 5-year metastasis-free survival in patients with no usable GEP.",
                "Use CV AUC/Brier and calibration rows to judge overall support; use predictor contributions for ranked driver summaries.",
                sprintf("Retained predictors: %s", paste(prepared_data$predictors, collapse = ", "))
            )
        ),
        Direct_MSS_Risk_Model = prepend_sheet_guide(
            create_model_workbook_tab(direct_mss_model),
            c(
                "This is the primary direct-risk model for 5-year melanoma-specific survival risk in patients with no usable GEP.",
                "Sparse event support can weaken MSS calibration and make interpretation more cautious than MFS.",
                sprintf("Retained predictors: %s", paste(prepared_data$predictors, collapse = ", "))
            )
        ),
        No_GEP_Predictions = prepend_sheet_guide(
            no_gep_predictions_sheet,
            c(
                "Each row is a patient with failed/indeterminate or not-tested GEP and baseline-only exploratory predictions.",
                "Surrogate Class 2-like probability answers which known definitive-GEP clinical pattern the patient resembles more; direct MFS/MSS risks are the main clinically usable outputs.",
                "Risk bins are pooled low/intermediate/high quantile groupings used for descriptive calibration checks."
            )
        ),
        Sensitivity_Pooled_No_GEP = prepend_sheet_guide(
            sensitivity_summary,
            c(
                "This sheet pools failed/indeterminate and not-tested patients only for sensitivity summaries.",
                "Higher observed event rates across bins support useful risk ordering, but this is still exploratory internal validation.",
                "Primary interpretation should stay with the separate subgroup summaries rather than the pooled rows alone."
            )
        )
    )

    workbook_path <- file.path(output_dir, "full_cohort_exploratory_no_gep_report.xlsx")
    summary_path <- file.path(output_dir, "full_cohort_exploratory_no_gep_summary.txt")

    write_gep_workbook(workbook_data, workbook_path)
    create_exploratory_no_gep_summary_text(
        dataset_name = dataset_name,
        data_audit = data_audit,
        baseline_summary = baseline_summary,
        prepared_data = prepared_data,
        predictor_screening = prepared_data$predictor_screening,
        surrogate_model = surrogate_model,
        mfs_model = direct_mfs_model,
        mss_model = direct_mss_model,
        no_gep_summary = no_gep_summary,
        sensitivity_summary = sensitivity_summary,
        output_path = summary_path
    )

    plot_paths <- list(
        mfs_km = file.path(plots_dir, "full_cohort_exploratory_no_gep_mfs_km_corrected.png"),
        mss_cif = file.path(plots_dir, "full_cohort_exploratory_no_gep_mss_cif_corrected.png"),
        surrogate_density = file.path(plots_dir, "full_cohort_exploratory_no_gep_surrogate_probability_density.png"),
        mfs_density = file.path(plots_dir, "full_cohort_exploratory_no_gep_mfs_risk_density.png"),
        mss_density = file.path(plots_dir, "full_cohort_exploratory_no_gep_mss_risk_density.png"),
        surrogate_bins = file.path(plots_dir, "full_cohort_exploratory_no_gep_surrogate_bin_event_rates.png"),
        mfs_bins = file.path(plots_dir, "full_cohort_exploratory_no_gep_mfs_bin_event_rates.png"),
        mss_bins = file.path(plots_dir, "full_cohort_exploratory_no_gep_mss_bin_event_rates.png")
    )

    create_exploratory_mfs_km_plot(
        full_data %>% dplyr::filter(!is.na(.data$exploratory_gep_group)),
        plot_paths$mfs_km
    )
    create_exploratory_mss_cif_plot(
        full_data %>% dplyr::filter(!is.na(.data$exploratory_gep_group)),
        plot_paths$mss_cif
    )
    create_probability_density_plot(
        no_gep_predictions,
        probability_col = "surrogate_class2_probability",
        plot_title = "Surrogate Class 2-Like Probability by No-GEP Group",
        output_path = plot_paths$surrogate_density
    )
    create_probability_density_plot(
        no_gep_predictions,
        probability_col = "predicted_mfs_5yr_risk",
        plot_title = "Predicted 5-Year MFS Risk by No-GEP Group",
        output_path = plot_paths$mfs_density
    )
    create_probability_density_plot(
        no_gep_predictions,
        probability_col = "predicted_mss_5yr_risk",
        plot_title = "Predicted 5-Year MSS Risk by No-GEP Group",
        output_path = plot_paths$mss_density
    )
    create_event_rate_bin_plot(
        sensitivity_summary,
        analysis_name = "Surrogate_Class2_Probability",
        event_col = "observed_mfs_5yr_event_rate",
        plot_title = "Observed 5-Year MFS Event Rate by Surrogate Probability Bin",
        output_path = plot_paths$surrogate_bins
    )
    create_event_rate_bin_plot(
        sensitivity_summary,
        analysis_name = "Direct_MFS_5yr_Risk",
        event_col = "observed_mfs_5yr_event_rate",
        plot_title = "Observed 5-Year MFS Event Rate by Predicted MFS Risk Bin",
        output_path = plot_paths$mfs_bins
    )
    create_event_rate_bin_plot(
        sensitivity_summary,
        analysis_name = "Direct_MSS_5yr_Risk",
        event_col = "observed_mss_5yr_event_rate",
        plot_title = "Observed 5-Year MSS Event Rate by Predicted MSS Risk Bin",
        output_path = plot_paths$mss_bins
    )

    logger::log_info("Exploratory no-GEP risk report completed")

    list(
        data_audit = data_audit,
        baseline_comparisons = baseline_summary,
        km_corrected_mfs = km_corrected_mfs,
        km_corrected_mss = km_corrected_mss,
        surrogate_model = surrogate_model,
        direct_models = list(
            mfs = direct_mfs_model,
            mss = direct_mss_model
        ),
        predictor_contribution = predictor_contribution,
        summary_and_guide = summary_guide,
        no_gep_summary = no_gep_summary,
        no_gep_predictions = no_gep_predictions_sheet,
        sensitivity_summary = sensitivity_summary,
        risk_strata_summary = analysis_results$risk_strata_summary,
        unified_no_gep_overview = analysis_results$unified_no_gep_overview,
        unified_no_gep_model_comparison = analysis_results$unified_no_gep_model_comparison,
        unified_no_gep_risk_strata = analysis_results$unified_no_gep_risk_strata,
        output_paths = c(
            list(workbook = workbook_path, summary = summary_path),
            plot_paths
        )
    )
}
