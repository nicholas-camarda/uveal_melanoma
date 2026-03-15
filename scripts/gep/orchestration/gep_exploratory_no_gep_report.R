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
#' Restores GEP display labels, derives the no-GEP groupings, creates the fixed
#' baseline predictors, and splits the cohort into training and prediction sets.
#'
#' @param data The analytic cohort.
#' @param dataset_name Dataset identifier used by existing restoration helpers.
#'
#' @return A list containing the full prepared data, definitive-GEP training
#'   data, no-GEP prediction data, predictor names, and an optional ID column.
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
            optic_nerve_involvement = dplyr::case_when(
                as.character(.data$optic_nerve) %in% c("Yes", "Y", "Involved") ~ 1L,
                as.character(.data$optic_nerve) %in% c("No", "N", "Not Involved") ~ 0L,
                TRUE ~ NA_integer_
            ),
            mfs_event_5yr = derive_binary_endpoint(prepared, "mfs_event_5yr", "tt_mets_months", 60),
            mss_event_5yr = derive_binary_endpoint(prepared, "mss_event_5yr", "tt_death_months", 60)
        )

    predictors <- c(
        "age_at_diagnosis",
        "initial_tumor_height",
        "initial_tumor_diameter",
        "ciliary_involvement",
        "optic_nerve_involvement"
    )

    complete_predictors <- stats::complete.cases(prepared[, predictors])

    definitive_training <- prepared %>%
        dplyr::filter(
            .data$exploratory_gep_group %in% GEP_DEFINITIVE_SIMPLE_LEVELS,
            complete_predictors
        ) %>%
        dplyr::mutate(class2_outcome = as.integer(.data$exploratory_gep_group == "Class 2"))

    no_gep_prediction <- prepared %>%
        dplyr::filter(
            .data$exploratory_gep_group %in% c("GEP Failed/Indeterminate", "GEP Not Tested"),
            complete_predictors
        )

    list(
        full_data = prepared,
        definitive_training = definitive_training,
        no_gep_prediction = no_gep_prediction,
        predictors = predictors,
        patient_id_col = pick_exploratory_patient_id_col(prepared)
    )
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

    percent_yes <- mean(values == 1, na.rm = TRUE) * 100
    sprintf("%d/%d (%.1f%%)", sum(values == 1, na.rm = TRUE), sum(!is.na(values)), percent_yes)
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
        result <- tryCatch(
            stats::chisq.test(contingency),
            error = function(e) NULL
        )
        return(list(
            test = "Chi-square",
            p_value = if (is.null(result)) NA_real_ else result$p.value
        ))
    }

    if (is.numeric(values)) {
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
    result <- tryCatch(
        stats::chisq.test(contingency),
        error = function(e) NULL
    )
    list(
        test = "Chi-square",
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

    baseline_vars <- c(
        "age_at_diagnosis",
        "initial_tumor_height",
        "initial_tumor_diameter",
        "ciliary_involvement",
        "optic_nerve_involvement"
    )

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

#' Cross-Validate Binary Model Predictions
#'
#' Performs simple K-fold cross-validation for a logistic regression using the
#' fixed exploratory predictor set.
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
    if (n_rows < folds || length(unique(data[[outcome_var]])) < 2) {
        return(rep(NA_real_, n_rows))
    }

    set.seed(seed)
    fold_id <- sample(rep(seq_len(folds), length.out = n_rows))
    predictions <- rep(NA_real_, n_rows)
    model_formula <- stats::as.formula(sprintf("%s ~ %s", outcome_var, paste(predictors, collapse = " + ")))

    for (fold in seq_len(folds)) {
        train_data <- data[fold_id != fold, , drop = FALSE]
        test_data <- data[fold_id == fold, , drop = FALSE]

        if (nrow(train_data) == 0 || length(unique(train_data[[outcome_var]])) < 2) {
            next
        }

        fold_model <- stats::glm(
            formula = model_formula,
            data = train_data,
            family = stats::binomial()
        )

        predictions[fold_id == fold] <- stats::predict(fold_model, newdata = test_data, type = "response")
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

#' Extract Logistic Model Coefficients for Reporting
#'
#' Converts a fitted generalized linear model into a tidy coefficient table with
#' optional exponentiation of odds ratios and confidence intervals.
#'
#' @param model_fit A fitted logistic regression model.
#' @param exponentiate Whether to exponentiate estimates and confidence bounds.
#'
#' @return A data frame of coefficient summaries.
extract_binary_model_coefficients <- function(model_fit, exponentiate = TRUE) {
    coefficient_summary <- stats::coef(summary(model_fit))
    coefficient_names <- rownames(coefficient_summary)
    confidence_intervals <- tryCatch(
        stats::confint.default(model_fit),
        error = function(e) {
            fallback <- matrix(NA_real_, nrow = length(coefficient_names), ncol = 2)
            rownames(fallback) <- coefficient_names
            fallback
        }
    )
    confidence_intervals <- confidence_intervals[coefficient_names, , drop = FALSE]

    estimates <- coefficient_summary[, "Estimate"]
    std_errors <- coefficient_summary[, "Std. Error"]
    z_values <- coefficient_summary[, "z value"]
    p_values <- coefficient_summary[, "Pr(>|z|)"]

    if (exponentiate) {
        estimate_display <- exp(estimates)
        lower_display <- exp(confidence_intervals[, 1])
        upper_display <- exp(confidence_intervals[, 2])
    } else {
        estimate_display <- estimates
        lower_display <- confidence_intervals[, 1]
        upper_display <- confidence_intervals[, 2]
    }

    tibble::tibble(
        term = rownames(coefficient_summary),
        estimate = estimate_display,
        conf_low = lower_display,
        conf_high = upper_display,
        std_error = std_errors,
        z_value = z_values,
        p_value = p_values
    )
}

#' Fit an Exploratory Binary Model
#'
#' Fits a logistic regression, derives apparent and cross-validated performance,
#' and packages model outputs for workbook reporting.
#'
#' @param data Modeling data frame.
#' @param outcome_var Name of the binary outcome column.
#' @param predictors Character vector of predictor names.
#' @param model_name Display label for the model.
#' @param seed Random seed for cross-validation.
#'
#' @return A list containing the fit object, coefficients, metrics, and
#'   calibration data.
fit_exploratory_binary_model <- function(data, outcome_var, predictors, model_name, seed = 123) {
    model_formula <- stats::as.formula(sprintf("%s ~ %s", outcome_var, paste(predictors, collapse = " + ")))
    fitted_model <- stats::glm(
        formula = model_formula,
        data = data,
        family = stats::binomial()
    )

    apparent_predictions <- stats::predict(fitted_model, type = "response")
    cv_predictions <- cross_validate_binary_predictions(data, outcome_var = outcome_var, predictors = predictors, seed = seed)
    calibration <- summarize_binary_calibration(data[[outcome_var]], apparent_predictions)

    metrics <- tibble::tibble(
        model = model_name,
        n = nrow(data),
        events = sum(data[[outcome_var]] == 1, na.rm = TRUE),
        apparent_auc = calculate_binary_auc(data[[outcome_var]], apparent_predictions),
        cv_auc = calculate_binary_auc(data[[outcome_var]], cv_predictions),
        apparent_brier = calculate_binary_brier(data[[outcome_var]], apparent_predictions),
        cv_brier = calculate_binary_brier(data[[outcome_var]], cv_predictions),
        calibration_status = calibration$status,
        calibration_intercept = calibration$intercept,
        calibration_slope = calibration$slope
    )

    list(
        model = fitted_model,
        metrics = metrics,
        coefficients = extract_binary_model_coefficients(fitted_model),
        calibration_curve = calibration$curve
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
                                                   surrogate_model,
                                                   mfs_model,
                                                   mss_model,
                                                   no_gep_summary,
                                                   output_path) {
    failed_row <- no_gep_summary %>% dplyr::filter(.data$no_gep_group == "GEP Failed/Indeterminate")
    not_tested_row <- no_gep_summary %>% dplyr::filter(.data$no_gep_group == "GEP Not Tested")

    summary_lines <- c(
        "EXPLORATORY NO-GEP RISK REPORT",
        "==============================",
        "",
        sprintf("Dataset: %s", dataset_name),
        "",
        "Purpose:",
        "Estimate direct MFS/MSS risk for patients without usable GEP while reporting surrogate Class 2-like probability as a secondary descriptive measure.",
        "",
        "Interpretation guardrails:",
        "- This report improves clinical risk assessment; it does not claim to recover the true molecular GEP class.",
        "- GEP Failed/Indeterminate and GEP Not Tested are reported separately because they behave like distinct clinical populations.",
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
        "Predictor set used in all exploratory models:",
        "- age at diagnosis",
        "- initial tumor height",
        "- initial tumor diameter",
        "- ciliary involvement/location",
        "- optic nerve involvement",
        "",
        "Baseline comparison note:",
        sprintf(
            "- The strongest 4-group separation in the fixed predictor set was %s (p=%s).",
            baseline_summary$variable[[which.min(dplyr::coalesce(baseline_summary$p_value, Inf))]],
            format_gep_p_value(min(baseline_summary$p_value, na.rm = TRUE))
        )
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
#'
#' @return A list containing the major report tables, model objects, and output
#'   file paths.
run_exploratory_no_gep_report <- function(dataset_name = "uveal_melanoma_full_cohort",
                                          output_dir = NULL,
                                          verify_km_fix = TRUE,
                                          data = NULL,
                                          visual_file = here("scripts", "gep", "visualization", "gep_visuals.R")) {
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

    mfs_model_data <- full_data %>%
        dplyr::filter(stats::complete.cases(dplyr::across(all_of(c(prepared_data$predictors, "mfs_event_5yr")))))
    mss_model_data <- full_data %>%
        dplyr::filter(stats::complete.cases(dplyr::across(all_of(c(prepared_data$predictors, "mss_event_5yr")))))

    direct_mfs_model <- fit_exploratory_binary_model(
        mfs_model_data,
        outcome_var = "mfs_event_5yr",
        predictors = prepared_data$predictors,
        model_name = "Direct 5-Year MFS Risk"
    )
    direct_mss_model <- fit_exploratory_binary_model(
        mss_model_data,
        outcome_var = "mss_event_5yr",
        predictors = prepared_data$predictors,
        model_name = "Direct 5-Year MSS Risk"
    )

    no_gep_predictions <- prepared_data$no_gep_prediction %>%
        dplyr::mutate(
            surrogate_class2_probability = stats::predict(surrogate_model$model, newdata = ., type = "response"),
            predicted_mfs_5yr_risk = stats::predict(direct_mfs_model$model, newdata = ., type = "response"),
            predicted_mss_5yr_risk = stats::predict(direct_mss_model$model, newdata = ., type = "response")
        ) %>%
        dplyr::mutate(
            surrogate_probability_bin = create_quantile_bins(.data$surrogate_class2_probability),
            mfs_risk_bin = create_quantile_bins(.data$predicted_mfs_5yr_risk),
            mss_risk_bin = create_quantile_bins(.data$predicted_mss_5yr_risk)
        )

    patient_id_col <- prepared_data$patient_id_col
    no_gep_predictions_sheet <- no_gep_predictions %>%
        dplyr::transmute(
            patient_id = if (!is.null(patient_id_col)) .data[[patient_id_col]] else seq_len(dplyr::n()),
            no_gep_group = .data$no_gep_group,
            age_at_diagnosis = .data$age_at_diagnosis,
            initial_tumor_height = .data$initial_tumor_height,
            initial_tumor_diameter = .data$initial_tumor_diameter,
            ciliary_involvement = .data$ciliary_involvement,
            optic_nerve_involvement = .data$optic_nerve_involvement,
            mfs_event_5yr = .data$mfs_event_5yr,
            mss_event_5yr = .data$mss_event_5yr,
            surrogate_class2_probability = .data$surrogate_class2_probability,
            surrogate_probability_bin = .data$surrogate_probability_bin,
            predicted_mfs_5yr_risk = .data$predicted_mfs_5yr_risk,
            mfs_risk_bin = .data$mfs_risk_bin,
            predicted_mss_5yr_risk = .data$predicted_mss_5yr_risk,
            mss_risk_bin = .data$mss_risk_bin
        )

    no_gep_summary <- summarize_no_gep_predictions(no_gep_predictions)
    sensitivity_summary <- summarize_pooled_no_gep_sensitivity(no_gep_predictions)

    workbook_data <- list(
        Data_Audit = data_audit,
        Baseline_Comparisons = baseline_summary,
        KM_Corrected_MFS = km_corrected_mfs,
        KM_Corrected_MSS = km_corrected_mss,
        Surrogate_Class2_Model = create_model_workbook_tab(surrogate_model),
        Direct_MFS_Risk_Model = create_model_workbook_tab(direct_mfs_model),
        Direct_MSS_Risk_Model = create_model_workbook_tab(direct_mss_model),
        No_GEP_Predictions = no_gep_predictions_sheet,
        Sensitivity_Pooled_No_GEP = sensitivity_summary
    )

    workbook_path <- file.path(output_dir, "full_cohort_exploratory_no_gep_report.xlsx")
    summary_path <- file.path(output_dir, "full_cohort_exploratory_no_gep_summary.txt")

    write_gep_workbook(workbook_data, workbook_path)
    create_exploratory_no_gep_summary_text(
        dataset_name = dataset_name,
        data_audit = data_audit,
        baseline_summary = baseline_summary,
        surrogate_model = surrogate_model,
        mfs_model = direct_mfs_model,
        mss_model = direct_mss_model,
        no_gep_summary = no_gep_summary,
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
        no_gep_summary = no_gep_summary,
        no_gep_predictions = no_gep_predictions_sheet,
        sensitivity_summary = sensitivity_summary,
        output_paths = c(
            list(workbook = workbook_path, summary = summary_path),
            plot_paths
        )
    )
}
