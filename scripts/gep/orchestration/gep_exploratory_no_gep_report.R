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

#' Preserve Validated Factor Levels for Exploratory Modeling
#'
#' Recasts a factor as an unordered factor without changing the existing level
#' order. This avoids accidental releveling when validated objective 0 cohort
#' columns are temporarily normalized inside the exploratory workflow.
#'
#' @param values A factor or character vector.
#'
#' @return An unordered factor with preserved level order when available.
preserve_exploratory_factor_levels <- function(values) {
    if (is.factor(values)) {
        return(coerce_to_factor_preserving_levels(values))
    }

    coerce_to_factor_preserving_levels(values)
}

#' Build a Follow-Up Context Block for Exploratory No-GEP Narratives
#'
#' Summarizes the no-GEP prediction subset used by the exploratory baseline-only
#' models so readers can see the follow-up duration and operational censoring
#' context before the model-performance sections.
#'
#' @param prepared_data List returned by `prepare_exploratory_no_gep_data()`.
#' @param dataset_name Optional dataset label for contextual wording.
#'
#' @return Character vector of narrative lines.
build_exploratory_no_gep_followup_block <- function(prepared_data, dataset_name = NULL) {
    if (is.null(prepared_data)) {
        return(character())
    }

    analysis_data <- if (is.data.frame(prepared_data)) {
        prepared_data
    } else {
        prepared_data$no_gep_prediction %||% prepared_data$full_data
    }

    if (is.null(analysis_data) || !is.data.frame(analysis_data) || nrow(analysis_data) == 0) {
        return(character())
    }

    if (!"follow_up_years" %in% names(analysis_data)) {
        if ("follow_up_days" %in% names(analysis_data)) {
            analysis_data$follow_up_years <- analysis_data$follow_up_days / DAYS_IN_YEAR
        } else if (all(c("date_diagnosis", "last_known_alive_date") %in% names(analysis_data))) {
            analysis_data$follow_up_years <- as.numeric(difftime(
                analysis_data$last_known_alive_date,
                analysis_data$date_diagnosis,
                units = "days"
            )) / DAYS_IN_YEAR
        } else {
            analysis_data$follow_up_years <- NA_real_
        }
    }

    if (!"no_gep_group" %in% names(analysis_data) && "exploratory_gep_group" %in% names(analysis_data)) {
        analysis_data$no_gep_group <- as.character(analysis_data$exploratory_gep_group)
    }
    if (!"no_gep_group" %in% names(analysis_data)) {
        analysis_data$no_gep_group <- NA_character_
    }

    analysis_data <- add_objective4_operational_followup_status(analysis_data)

    valid_followup <- !is.na(analysis_data$follow_up_years) & analysis_data$follow_up_years >= 0
    followup_values <- analysis_data$follow_up_years[valid_followup]
    total_n <- nrow(analysis_data)
    followup_ge_5yr_n <- sum(valid_followup & analysis_data$follow_up_years >= 5, na.rm = TRUE)
    followup_ge_5yr_prop <- if (total_n > 0) followup_ge_5yr_n / total_n else NA_real_

    followup_mean <- if (length(followup_values) > 0) mean(followup_values) else NA_real_
    followup_median <- if (length(followup_values) > 0) stats::median(followup_values) else NA_real_
    followup_max <- if (length(followup_values) > 0) max(followup_values) else NA_real_

    operational_counts <- table(analysis_data$operational_followup_status, useNA = "no")
    operational_alive <- as.integer(if ("alive" %in% names(operational_counts)) operational_counts[["alive"]] else 0L)
    operational_dead <- as.integer(if ("dead" %in% names(operational_counts)) operational_counts[["dead"]] else 0L)
    operational_lost <- as.integer(if ("lost_to_followup" %in% names(operational_counts)) operational_counts[["lost_to_followup"]] else 0L)

    group_summary <- analysis_data %>%
        dplyr::filter(!is.na(.data$no_gep_group)) %>%
        dplyr::group_by(.data$no_gep_group) %>%
        dplyr::summarise(
            n = dplyr::n(),
            median_followup_years = if (sum(!is.na(.data$follow_up_years) & .data$follow_up_years >= 0) > 0) {
                stats::median(.data$follow_up_years[!is.na(.data$follow_up_years) & .data$follow_up_years >= 0])
            } else {
                NA_real_
            },
            alive = sum(.data$operational_followup_status == "alive", na.rm = TRUE),
            dead = sum(.data$operational_followup_status == "dead", na.rm = TRUE),
            lost_to_followup = sum(.data$operational_followup_status == "lost_to_followup", na.rm = TRUE),
            .groups = "drop"
        ) %>%
        dplyr::arrange(factor(.data$no_gep_group, levels = c("GEP Failed/Indeterminate", "GEP Not Tested")))

    group_line <- if (nrow(group_summary) > 0) {
        paste(
            vapply(seq_len(nrow(group_summary)), function(i) {
                sprintf(
                    "%s median follow-up %.1f years (n=%d)",
                    group_summary$no_gep_group[i],
                    group_summary$median_followup_years[i],
                    group_summary$n[i]
                )
            }, character(1)),
            collapse = "; "
        )
    } else {
        NULL
    }

    lines <- c(
        md_heading("Follow-Up Context", 2L),
        sprintf(
            "The follow-up summary below uses the no-GEP prediction subset for %s, so the denominator matches the rows entering the direct no-GEP risk outputs.",
            dataset_name %||% "this cohort"
        ),
        md_bullet(sprintf(
            "Overall follow-up: median %.1f years; mean %.1f years; max %.1f years; %d/%d (%.1f%%) reached at least 5 years.",
            followup_median,
            followup_mean,
            followup_max,
            followup_ge_5yr_n,
            total_n,
            100 * followup_ge_5yr_prop
        )),
        md_bullet(sprintf(
            "Operational view: alive %d/%d (%.1f%%); dead %d/%d (%.1f%%); lost_to_followup %d/%d (%.1f%%).",
            operational_alive,
            total_n,
            100 * (operational_alive / total_n),
            operational_dead,
            total_n,
            100 * (operational_dead / total_n),
            operational_lost,
            total_n,
            100 * (operational_lost / total_n)
        ))
    )

    if (!is.null(group_line) && nzchar(group_line)) {
        lines <- c(lines, md_bullet(paste0("By no-GEP group: ", group_line)))
    }

    lines
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
            exploratory_gep_group = preserve_exploratory_factor_levels(.data$gep_class_simple),
            no_gep_group = dplyr::case_when(
                .data$exploratory_gep_group == "GEP Failed/Indeterminate" ~ "GEP Failed/Indeterminate",
                .data$exploratory_gep_group == "GEP Not Tested" ~ "GEP Not Tested",
                TRUE ~ NA_character_
            ),
            ciliary_involvement = as.integer(grepl("cilio|ciliary", as.character(.data$location), ignore.case = TRUE)),
            sex = preserve_exploratory_factor_levels(.data$sex),
            location = preserve_exploratory_factor_levels(.data$location),
            initial_t_stage_simple = preserve_exploratory_factor_levels(.data$initial_t_stage_simple),
            internal_reflectivity = preserve_exploratory_factor_levels(.data$internal_reflectivity),
            srf = preserve_exploratory_factor_levels(.data$srf),
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

#' Choose a Parsimonious Predictor Set for Sensitivity Modeling
#'
#' Returns a pre-specified smaller baseline predictor set composed of broadly
#' standard clinical variables so the direct-risk conclusions can be checked
#' against a lower-complexity specification.
#'
#' @param prepared_data Output from `prepare_exploratory_no_gep_data()`.
#'
#' @return Character vector of retained parsimonious predictors.
choose_exploratory_parsimonious_predictors <- function(prepared_data) {
    preferred_predictors <- c(
        "age_at_diagnosis",
        "initial_tumor_diameter",
        "location",
        "initial_t_stage_simple"
    )
    parsimonious_predictors <- intersect(prepared_data$predictors, preferred_predictors)

    if (length(parsimonious_predictors) < 3) {
        stop("Parsimonious exploratory sensitivity model retained fewer than 3 baseline predictors.")
    }

    parsimonious_predictors
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

#' Summarize an Empirical Uncertainty Interval
#'
#' Calculates a median and central percentile interval for a numeric metric
#' vector after dropping missing values.
#'
#' @param values Numeric vector of metric values.
#' @param conf_level Confidence level for the central interval.
#'
#' @return A named list with `median`, `lower`, `upper`, and `n`.
summarize_numeric_interval <- function(values, conf_level = 0.95) {
    valid_values <- values[is.finite(values)]

    if (length(valid_values) == 0) {
        return(list(
            median = NA_real_,
            lower = NA_real_,
            upper = NA_real_,
            n = 0L
        ))
    }

    alpha <- (1 - conf_level) / 2
    quantiles <- stats::quantile(
        valid_values,
        probs = c(alpha, 0.5, 1 - alpha),
        na.rm = TRUE,
        names = FALSE
    )

    list(
        median = unname(quantiles[[2]]),
        lower = unname(quantiles[[1]]),
        upper = unname(quantiles[[3]]),
        n = length(valid_values)
    )
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

#' Repeat Cross-Validated Binary Metrics Across Multiple Random Partitions
#'
#' Repeats the internal cross-validation procedure across several seeds to
#' quantify how sensitive the out-of-fold discrimination and calibration
#' summaries are to fold assignment.
#'
#' @param data Modeling data frame.
#' @param outcome_var Name of the binary outcome column.
#' @param predictors Character vector of predictor names.
#' @param repeats Number of repeated cross-validation runs.
#' @param seed Base random seed.
#'
#' @return A tibble with one row per repeat and repeated CV metrics.
repeat_cross_validated_binary_metrics <- function(data,
                                                  outcome_var,
                                                  predictors,
                                                  repeats = 20,
                                                  seed = 123) {
    if (repeats < 1) {
        return(tibble::tibble())
    }

    outcome <- data[[outcome_var]]

    purrr::map_dfr(seq_len(repeats), function(repeat_id) {
        cv_predictions <- cross_validate_binary_predictions(
            data = data,
            outcome_var = outcome_var,
            predictors = predictors,
            seed = seed + repeat_id - 1
        )
        cv_calibration <- summarize_binary_calibration(outcome, cv_predictions)

        tibble::tibble(
            repeat_id = repeat_id,
            cv_auc = calculate_binary_auc(outcome, cv_predictions),
            cv_brier = calculate_binary_brier(outcome, cv_predictions),
            cv_calibration_intercept = cv_calibration$intercept,
            cv_calibration_slope = cv_calibration$slope
        )
    })
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
    cv_calibration <- summarize_binary_calibration(outcome, cv_predictions)
    repeated_cv_metrics <- repeat_cross_validated_binary_metrics(
        data = data,
        outcome_var = outcome_var,
        predictors = predictors,
        repeats = 20,
        seed = seed + 1000
    )
    cv_auc_interval <- summarize_numeric_interval(repeated_cv_metrics$cv_auc)
    cv_brier_interval <- summarize_numeric_interval(repeated_cv_metrics$cv_brier)
    cv_slope_interval <- summarize_numeric_interval(repeated_cv_metrics$cv_calibration_slope)
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
        cv_calibration_status = cv_calibration$status,
        cv_calibration_intercept = cv_calibration$intercept,
        cv_calibration_slope = cv_calibration$slope,
        cv_folds = cv_folds,
        cv_repeats = nrow(repeated_cv_metrics),
        cv_auc_ci_lower = cv_auc_interval$lower,
        cv_auc_ci_upper = cv_auc_interval$upper,
        cv_brier_ci_lower = cv_brier_interval$lower,
        cv_brier_ci_upper = cv_brier_interval$upper,
        cv_calibration_slope_ci_lower = cv_slope_interval$lower,
        cv_calibration_slope_ci_upper = cv_slope_interval$upper,
        uncertainty_method = sprintf("Repeated %d-fold CV percentile interval", cv_folds),
        lambda_min = fitted_model$lambda.min,
        lambda_1se = fitted_model$lambda.1se
    )

    list(
        model = fitted_model,
        metrics = metrics,
        coefficients = coefficient_data,
        predictor_contributions = predictor_contributions,
        calibration_curve = calibration$curve,
        repeated_cv_metrics = repeated_cv_metrics,
        predictors = predictors,
        design_columns = colnames(design_matrix)
    )
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

#' Format a Metric Interval as Text
#'
#' Combines a point estimate and percentile interval into a compact string for
#' reader-facing workbook tabs.
#'
#' @param estimate Numeric point estimate.
#' @param lower Numeric lower interval bound.
#' @param upper Numeric upper interval bound.
#' @param digits Number of digits to retain.
#'
#' @return Character scalar such as `"0.665 (0.590 to 0.728)"`.
format_exploratory_metric_interval <- function(estimate, lower, upper, digits = 3) {
    if (any(!is.finite(c(estimate, lower, upper)))) {
        return(NA_character_)
    }

    sprintf(
        paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)"),
        estimate,
        lower,
        upper
    )
}

#' Build a Four-Group 5-Year Risk Ladder
#'
#' Scores all four exploratory GEP groups with the direct 5-year MFS and MSS
#' models so the no-GEP groups can be compared against definitive Class 1 and
#' Class 2 on the same descriptive scale.
#'
#' @param full_data Prepared exploratory full cohort.
#' @param direct_mfs_model Direct 5-year MFS model bundle.
#' @param direct_mss_model Direct 5-year MSS model bundle.
#'
#' @return A grouped descriptive risk-ladder table.
create_exploratory_risk_ladder <- function(full_data,
                                           direct_mfs_model,
                                           direct_mss_model) {
    ladder_data <- full_data %>%
        dplyr::filter(!is.na(.data$exploratory_gep_group)) %>%
        dplyr::mutate(
            exploratory_gep_group = factor(
                as.character(.data$exploratory_gep_group),
                levels = c("Class 1", "GEP Not Tested", "GEP Failed/Indeterminate", "Class 2")
            ),
            predicted_mfs_5yr_risk = NA_real_,
            predicted_mss_5yr_risk = NA_real_
        )

    mfs_complete <- stats::complete.cases(ladder_data[, direct_mfs_model$predictors, drop = FALSE])
    mss_complete <- stats::complete.cases(ladder_data[, direct_mss_model$predictors, drop = FALSE])

    if (any(mfs_complete)) {
        ladder_data$predicted_mfs_5yr_risk[mfs_complete] <- predict_exploratory_binary_model(
            direct_mfs_model,
            ladder_data[mfs_complete, , drop = FALSE]
        )
    }

    if (any(mss_complete)) {
        ladder_data$predicted_mss_5yr_risk[mss_complete] <- predict_exploratory_binary_model(
            direct_mss_model,
            ladder_data[mss_complete, , drop = FALSE]
        )
    }

    ladder_data %>%
        dplyr::group_by(.data$exploratory_gep_group) %>%
        dplyr::summarise(
            group = dplyr::first(.data$exploratory_gep_group),
            n = dplyr::n(),
            predictable_mfs_n = sum(!is.na(.data$predicted_mfs_5yr_risk)),
            predictable_mss_n = sum(!is.na(.data$predicted_mss_5yr_risk)),
            mfs_5yr_events = sum(.data$mfs_event_5yr == 1, na.rm = TRUE),
            observed_5yr_mfs_event_rate = mean(.data$mfs_event_5yr == 1, na.rm = TRUE),
            median_predicted_5yr_mfs_risk = stats::median(.data$predicted_mfs_5yr_risk, na.rm = TRUE),
            mss_5yr_events = sum(.data$mss_event_5yr == 1, na.rm = TRUE),
            observed_5yr_mss_event_rate = mean(.data$mss_event_5yr == 1, na.rm = TRUE),
            median_predicted_5yr_mss_risk = stats::median(.data$predicted_mss_5yr_risk, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        dplyr::select(-exploratory_gep_group) %>%
        dplyr::mutate(
            interpretation = dplyr::case_when(
                .data$group == "Class 1" ~ "Reference low-risk definitive GEP group.",
                .data$group == "GEP Not Tested" ~ "Lower-risk no-GEP subgroup overall, but still above definitive Class 1 on direct baseline-only risk.",
                .data$group == "GEP Failed/Indeterminate" ~ "Higher-risk no-GEP subgroup overall, sitting above GEP Not Tested and below definitive Class 2 on direct baseline-only risk.",
                .data$group == "Class 2" ~ "Reference high-risk definitive GEP group.",
                TRUE ~ NA_character_
            )
        )
}

#' Create the Minimal Start-Here Workbook Tab
#'
#' Builds a compact orientation sheet that tells readers what to conclude and
#' which tabs to open next.
#'
#' @param surrogate_model Surrogate Class 2-like model result bundle.
#' @param mfs_model Direct 5-year MFS model result bundle.
#' @param mss_model Direct 5-year MSS model result bundle.
#'
#' @return A compact start-here table.
create_exploratory_start_here_tab <- function(surrogate_model,
                                              mfs_model,
                                              mss_model) {
    tibble::tibble(
        row_order = seq_len(10),
        section = c(
            "study_question",
            rep("bottom_line", 4),
            rep("read_this_next", 4),
            "guardrail"
        ),
        label = c(
            "question",
            "takeaway_1",
            "takeaway_2",
            "takeaway_3",
            "takeaway_4",
            "tab_1",
            "tab_2",
            "tab_3",
            "tab_4",
            "interpretation"
        ),
        value = c(
            "What do baseline clinical features tell us about 5-year risk when GEP is unavailable or unusable?",
            sprintf(
                "Baseline clinical features provided moderate prognostic support for 5-year MFS and MSS (cross-validated AUC %.3f and %.3f).",
                mfs_model$metrics$cv_auc[[1]],
                mss_model$metrics$cv_auc[[1]]
            ),
            sprintf(
                "Clinical features only weakly approximated definitive molecular class (surrogate cross-validated AUC %.3f).",
                surrogate_model$metrics$cv_auc[[1]]
            ),
            "Do not relabel no-GEP patients into molecular classes based on the surrogate output.",
            "Do not present no-GEP patients as one homogeneous intermediate-risk group; the failed/indeterminate subgroup is higher risk than the larger not-tested subgroup.",
            "Open Key_Findings_5yr first for the main answer.",
            "Open Risk_Ladder_5yr next to compare Class 1, Not Tested, Failed/Indeterminate, and Class 2 on one 5-year scale.",
            "Open No_GEP_Subgroups to see the clinically relevant split within no-GEP patients.",
            "Open Model_Performance for compact discrimination and calibration results.",
            "Use direct MFS/MSS models as baseline-only prognostic support when GEP is unavailable; treat the surrogate as descriptive resemblance only."
        )
    )
}

#' Create the Main Key-Findings Workbook Table
#'
#' Reduces the four-group risk ladder into the reader-facing summary table that
#' answers the main question first.
#'
#' @param risk_ladder Four-group descriptive risk-ladder table.
#'
#' @return A compact key-findings table.
create_exploratory_key_findings_table <- function(risk_ladder) {
    risk_ladder %>%
        dplyr::transmute(
            group = .data$group,
            n = .data$n,
            observed_5yr_mfs_event_rate = .data$observed_5yr_mfs_event_rate,
            observed_5yr_mss_event_rate = .data$observed_5yr_mss_event_rate,
            median_predicted_5yr_mfs_risk = .data$median_predicted_5yr_mfs_risk,
            median_predicted_5yr_mss_risk = .data$median_predicted_5yr_mss_risk,
            interpretation = .data$interpretation
        )
}

#' Create a No-GEP Subgroup Comparison Table
#'
#' Focuses the workbook on the clinically important split between patients with
#' failed/indeterminate GEP and those who were not tested.
#'
#' @param no_gep_summary Grouped no-GEP summary table.
#'
#' @return A subgroup-comparison table.
create_exploratory_no_gep_subgroups_table <- function(no_gep_summary) {
    no_gep_summary %>%
        dplyr::transmute(
            no_gep_group = .data$no_gep_group,
            n = .data$n,
            observed_5yr_mfs_event_rate = .data$observed_mfs_5yr_event_rate,
            observed_5yr_mss_event_rate = .data$observed_mss_5yr_event_rate,
            median_surrogate_class2_probability = .data$median_surrogate_class2_probability,
            median_predicted_5yr_mfs_risk = .data$median_predicted_mfs_5yr_risk,
            median_predicted_5yr_mss_risk = .data$median_predicted_mss_5yr_risk,
            interpretation = dplyr::case_when(
                .data$no_gep_group == "GEP Failed/Indeterminate" ~ "Higher-risk no-GEP subgroup overall; avoid interpretive pooling with GEP Not Tested.",
                .data$no_gep_group == "GEP Not Tested" ~ "Lower-risk no-GEP subgroup overall, but still above definitive Class 1 on baseline-only risk.",
                TRUE ~ NA_character_
            )
        ) %>%
        dplyr::arrange(factor(.data$no_gep_group, levels = c("GEP Not Tested", "GEP Failed/Indeterminate")))
}

#' Create a Compact Model-Performance Table
#'
#' Produces one row per exploratory model with the discrimination,
#' calibration, and practical interpretation needed for front-of-workbook
#' review.
#'
#' @param surrogate_model Surrogate Class 2-like model result bundle.
#' @param mfs_model Direct 5-year MFS model result bundle.
#' @param mss_model Direct 5-year MSS model result bundle.
#' @param parsimonious_mfs_model Parsimonious direct 5-year MFS model result bundle.
#' @param parsimonious_mss_model Parsimonious direct 5-year MSS model result bundle.
#'
#' @return A compact model-performance table.
create_exploratory_model_performance_table <- function(surrogate_model,
                                                       mfs_model,
                                                       mss_model,
                                                       parsimonious_mfs_model,
                                                       parsimonious_mss_model) {
    model_specs <- list(
        list(
            label = "Surrogate Class 2-like",
            outcome = "definitive_class_2",
            population = "Definitive Class 1 vs Class 2 only",
            practical_read = "Weak molecular approximation; descriptive only and not suitable for molecular reassignment.",
            model_results = surrogate_model
        ),
        list(
            label = "Direct 5-year MFS",
            outcome = "5_year_mfs",
            population = "Eligible full cohort",
            practical_read = "Primary baseline-only clinical risk estimate when GEP is unavailable.",
            model_results = mfs_model
        ),
        list(
            label = "Direct 5-year MSS",
            outcome = "5_year_mss",
            population = "Eligible full cohort",
            practical_read = "Primary baseline-only melanoma-specific risk estimate when GEP is unavailable.",
            model_results = mss_model
        ),
        list(
            label = "Parsimonious Direct 5-year MFS",
            outcome = "5_year_mfs",
            population = "Eligible full cohort",
            practical_read = "Sensitivity check using a smaller pre-specified baseline feature set.",
            model_results = parsimonious_mfs_model
        ),
        list(
            label = "Parsimonious Direct 5-year MSS",
            outcome = "5_year_mss",
            population = "Eligible full cohort",
            practical_read = "Sensitivity check using a smaller pre-specified baseline feature set.",
            model_results = parsimonious_mss_model
        )
    )

    purrr::map_dfr(model_specs, function(model_spec) {
        metrics <- model_spec$model_results$metrics
        tibble::tibble(
            model = model_spec$label,
            outcome = model_spec$outcome,
            population = model_spec$population,
            n = metrics$n[[1]],
            events = metrics$events[[1]],
            cv_auc = metrics$cv_auc[[1]],
            cv_auc_ci = format_exploratory_metric_interval(
                metrics$cv_auc[[1]],
                metrics$cv_auc_ci_lower[[1]],
                metrics$cv_auc_ci_upper[[1]]
            ),
            cv_brier = metrics$cv_brier[[1]],
            cv_brier_ci = format_exploratory_metric_interval(
                metrics$cv_brier[[1]],
                metrics$cv_brier_ci_lower[[1]],
                metrics$cv_brier_ci_upper[[1]]
            ),
            calibration_slope = metrics$cv_calibration_slope[[1]],
            calibration_slope_ci = format_exploratory_metric_interval(
                metrics$cv_calibration_slope[[1]],
                metrics$cv_calibration_slope_ci_lower[[1]],
                metrics$cv_calibration_slope_ci_upper[[1]]
            ),
            practical_read = model_spec$practical_read
        )
    })
}

#' Create a Coefficient Table for One Exploratory Model
#'
#' Converts the ridge coefficient output into a standard rectangular table with
#' one row per design term and predictor-level ranking columns attached.
#'
#' @param model_results Exploratory model bundle.
#'
#' @return A coefficient table.
create_exploratory_model_coefficients_table <- function(model_results) {
    top_predictors <- model_results$predictor_contributions %>%
        dplyr::transmute(
            predictor = .data$predictor,
            predictor_rank = .data$rank,
            predictor_direction = .data$direction,
            dominant_term_for_predictor = .data$dominant_term,
            standardized_abs_coefficient = .data$standardized_abs_coefficient
        )

    model_results$coefficients %>%
        dplyr::filter(.data$term != "(Intercept)") %>%
        dplyr::left_join(top_predictors, by = "predictor") %>%
        dplyr::mutate(
            model = model_results$metrics$model[[1]],
            odds_ratio_or_hazard_ratio = exp(.data$estimate),
            penalty_context = "ridge_penalized_standardized"
        ) %>%
        dplyr::transmute(
            model = .data$model,
            predictor = .data$predictor,
            term = .data$term,
            estimate = .data$estimate,
            odds_ratio_or_hazard_ratio = .data$odds_ratio_or_hazard_ratio,
            coefficient_type = .data$coefficient_type,
            penalty_context = .data$penalty_context,
            predictor_rank = .data$predictor_rank,
            predictor_direction = .data$predictor_direction,
            dominant_term_for_predictor = .data$dominant_term_for_predictor,
            standardized_abs_coefficient = .data$standardized_abs_coefficient
        ) %>%
        dplyr::arrange(.data$predictor_rank, dplyr::desc(abs(.data$estimate)), .data$term)
}

#' Create a Calibration Summary Table Across Exploratory Models
#'
#' Produces a one-row-per-model table containing apparent and cross-validated
#' discrimination plus calibration summaries.
#'
#' @param surrogate_model Surrogate Class 2-like model result bundle.
#' @param mfs_model Direct 5-year MFS model result bundle.
#' @param mss_model Direct 5-year MSS model result bundle.
#' @param parsimonious_mfs_model Parsimonious direct 5-year MFS model result bundle.
#' @param parsimonious_mss_model Parsimonious direct 5-year MSS model result bundle.
#'
#' @return A calibration summary table.
create_exploratory_model_calibration_table <- function(surrogate_model,
                                                       mfs_model,
                                                       mss_model,
                                                       parsimonious_mfs_model,
                                                       parsimonious_mss_model) {
    model_results_list <- list(
        surrogate_model,
        mfs_model,
        mss_model,
        parsimonious_mfs_model,
        parsimonious_mss_model
    )

    purrr::map_dfr(model_results_list, function(model_results) {
        metrics <- model_results$metrics
        tibble::tibble(
            model = metrics$model[[1]],
            n = metrics$n[[1]],
            events = metrics$events[[1]],
            apparent_auc = metrics$apparent_auc[[1]],
            cv_auc = metrics$cv_auc[[1]],
            apparent_brier = metrics$apparent_brier[[1]],
            cv_brier = metrics$cv_brier[[1]],
            calibration_status = metrics$calibration_status[[1]],
            calibration_intercept = metrics$calibration_intercept[[1]],
            calibration_slope = metrics$calibration_slope[[1]],
            cv_calibration_status = metrics$cv_calibration_status[[1]],
            cv_calibration_intercept = metrics$cv_calibration_intercept[[1]],
            cv_calibration_slope = metrics$cv_calibration_slope[[1]],
            cv_auc_ci_lower = metrics$cv_auc_ci_lower[[1]],
            cv_auc_ci_upper = metrics$cv_auc_ci_upper[[1]],
            cv_brier_ci_lower = metrics$cv_brier_ci_lower[[1]],
            cv_brier_ci_upper = metrics$cv_brier_ci_upper[[1]],
            cv_calibration_slope_ci_lower = metrics$cv_calibration_slope_ci_lower[[1]],
            cv_calibration_slope_ci_upper = metrics$cv_calibration_slope_ci_upper[[1]],
            cv_folds = metrics$cv_folds[[1]],
            cv_repeats = metrics$cv_repeats[[1]],
            uncertainty_method = metrics$uncertainty_method[[1]]
        )
    })
}

#' Create a Parsimonious Direct-Model Sensitivity Summary
#'
#' Compares the full direct-risk models against a smaller pre-specified direct
#' model to check whether the no-GEP subgroup ordering materially changes under
#' a lower-complexity specification.
#'
#' @param full_no_gep_predictions Row-level full-model no-GEP predictions.
#' @param parsimonious_no_gep_predictions Row-level parsimonious-model no-GEP predictions.
#' @param full_mfs_model Full direct 5-year MFS model bundle.
#' @param full_mss_model Full direct 5-year MSS model bundle.
#' @param parsimonious_mfs_model Parsimonious direct 5-year MFS model bundle.
#' @param parsimonious_mss_model Parsimonious direct 5-year MSS model bundle.
#' @param parsimonious_predictors Character vector of parsimonious predictors.
#'
#' @return A workbook-ready comparison table.
create_exploratory_parsimonious_sensitivity <- function(full_no_gep_predictions,
                                                        parsimonious_no_gep_predictions,
                                                        full_mfs_model,
                                                        full_mss_model,
                                                        parsimonious_mfs_model,
                                                        parsimonious_mss_model,
                                                        parsimonious_predictors) {
    summarize_prediction_medians <- function(data, prediction_col) {
        data %>%
            dplyr::group_by(.data$no_gep_group) %>%
            dplyr::summarise(
                median_prediction = stats::median(.data[[prediction_col]], na.rm = TRUE),
                .groups = "drop"
            )
    }

    full_mfs_medians <- summarize_prediction_medians(full_no_gep_predictions, "predicted_mfs_5yr_risk")
    full_mss_medians <- summarize_prediction_medians(full_no_gep_predictions, "predicted_mss_5yr_risk")
    parsimonious_mfs_medians <- summarize_prediction_medians(parsimonious_no_gep_predictions, "predicted_mfs_5yr_risk")
    parsimonious_mss_medians <- summarize_prediction_medians(parsimonious_no_gep_predictions, "predicted_mss_5yr_risk")

    dplyr::bind_rows(
        tibble::tibble(
            outcome = "MFS",
            model_specification = c("Full direct model", "Parsimonious direct model"),
            predictors = c(
                paste(full_mfs_model$predictors, collapse = ", "),
                paste(parsimonious_predictors, collapse = ", ")
            ),
            n = c(full_mfs_model$metrics$n[[1]], parsimonious_mfs_model$metrics$n[[1]]),
            events = c(full_mfs_model$metrics$events[[1]], parsimonious_mfs_model$metrics$events[[1]]),
            cv_auc = c(full_mfs_model$metrics$cv_auc[[1]], parsimonious_mfs_model$metrics$cv_auc[[1]]),
            cv_auc_ci_lower = c(full_mfs_model$metrics$cv_auc_ci_lower[[1]], parsimonious_mfs_model$metrics$cv_auc_ci_lower[[1]]),
            cv_auc_ci_upper = c(full_mfs_model$metrics$cv_auc_ci_upper[[1]], parsimonious_mfs_model$metrics$cv_auc_ci_upper[[1]]),
            cv_brier = c(full_mfs_model$metrics$cv_brier[[1]], parsimonious_mfs_model$metrics$cv_brier[[1]]),
            cv_brier_ci_lower = c(full_mfs_model$metrics$cv_brier_ci_lower[[1]], parsimonious_mfs_model$metrics$cv_brier_ci_lower[[1]]),
            cv_brier_ci_upper = c(full_mfs_model$metrics$cv_brier_ci_upper[[1]], parsimonious_mfs_model$metrics$cv_brier_ci_upper[[1]]),
            cv_calibration_slope = c(full_mfs_model$metrics$cv_calibration_slope[[1]], parsimonious_mfs_model$metrics$cv_calibration_slope[[1]]),
            cv_calibration_slope_ci_lower = c(full_mfs_model$metrics$cv_calibration_slope_ci_lower[[1]], parsimonious_mfs_model$metrics$cv_calibration_slope_ci_lower[[1]]),
            cv_calibration_slope_ci_upper = c(full_mfs_model$metrics$cv_calibration_slope_ci_upper[[1]], parsimonious_mfs_model$metrics$cv_calibration_slope_ci_upper[[1]]),
            median_not_tested_predicted_risk = c(
                full_mfs_medians$median_prediction[full_mfs_medians$no_gep_group == "GEP Not Tested"],
                parsimonious_mfs_medians$median_prediction[parsimonious_mfs_medians$no_gep_group == "GEP Not Tested"]
            ),
            median_failed_indeterminate_predicted_risk = c(
                full_mfs_medians$median_prediction[full_mfs_medians$no_gep_group == "GEP Failed/Indeterminate"],
                parsimonious_mfs_medians$median_prediction[parsimonious_mfs_medians$no_gep_group == "GEP Failed/Indeterminate"]
            )
        ),
        tibble::tibble(
            outcome = "MSS",
            model_specification = c("Full direct model", "Parsimonious direct model"),
            predictors = c(
                paste(full_mss_model$predictors, collapse = ", "),
                paste(parsimonious_predictors, collapse = ", ")
            ),
            n = c(full_mss_model$metrics$n[[1]], parsimonious_mss_model$metrics$n[[1]]),
            events = c(full_mss_model$metrics$events[[1]], parsimonious_mss_model$metrics$events[[1]]),
            cv_auc = c(full_mss_model$metrics$cv_auc[[1]], parsimonious_mss_model$metrics$cv_auc[[1]]),
            cv_auc_ci_lower = c(full_mss_model$metrics$cv_auc_ci_lower[[1]], parsimonious_mss_model$metrics$cv_auc_ci_lower[[1]]),
            cv_auc_ci_upper = c(full_mss_model$metrics$cv_auc_ci_upper[[1]], parsimonious_mss_model$metrics$cv_auc_ci_upper[[1]]),
            cv_brier = c(full_mss_model$metrics$cv_brier[[1]], parsimonious_mss_model$metrics$cv_brier[[1]]),
            cv_brier_ci_lower = c(full_mss_model$metrics$cv_brier_ci_lower[[1]], parsimonious_mss_model$metrics$cv_brier_ci_lower[[1]]),
            cv_brier_ci_upper = c(full_mss_model$metrics$cv_brier_ci_upper[[1]], parsimonious_mss_model$metrics$cv_brier_ci_upper[[1]]),
            cv_calibration_slope = c(full_mss_model$metrics$cv_calibration_slope[[1]], parsimonious_mss_model$metrics$cv_calibration_slope[[1]]),
            cv_calibration_slope_ci_lower = c(full_mss_model$metrics$cv_calibration_slope_ci_lower[[1]], parsimonious_mss_model$metrics$cv_calibration_slope_ci_lower[[1]]),
            cv_calibration_slope_ci_upper = c(full_mss_model$metrics$cv_calibration_slope_ci_upper[[1]], parsimonious_mss_model$metrics$cv_calibration_slope_ci_upper[[1]]),
            median_not_tested_predicted_risk = c(
                full_mss_medians$median_prediction[full_mss_medians$no_gep_group == "GEP Not Tested"],
                parsimonious_mss_medians$median_prediction[parsimonious_mss_medians$no_gep_group == "GEP Not Tested"]
            ),
            median_failed_indeterminate_predicted_risk = c(
                full_mss_medians$median_prediction[full_mss_medians$no_gep_group == "GEP Failed/Indeterminate"],
                parsimonious_mss_medians$median_prediction[parsimonious_mss_medians$no_gep_group == "GEP Failed/Indeterminate"]
            )
        )
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
                .data$Group == "GEP Failed/Indeterminate" ~ "Higher-risk no-GEP subgroup overall; do not pool interpretively with GEP Not Tested without showing the subgroup split.",
                .data$Group == "GEP Not Tested" ~ "Larger no-GEP subgroup with lower baseline-only risk than GEP Failed/Indeterminate, but still between definitive Class 1 and Class 2 overall.",
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
        ),
        list(
            key = "parsimonious_mfs",
            label = "Parsimonious Direct 5-Year MFS Risk",
            training_set = "Full eligible cohort with 4 pre-specified baseline predictors",
            use_case = "Sensitivity check showing whether the no-GEP MFS ordering persists under a lower-complexity clinical model."
        ),
        list(
            key = "parsimonious_mss",
            label = "Parsimonious Direct 5-Year MSS Risk",
            training_set = "Full eligible cohort with 4 pre-specified baseline predictors",
            use_case = "Sensitivity check showing whether the no-GEP MSS ordering persists under a lower-complexity clinical model."
        )
    )

    purrr::map_dfr(model_specs, function(spec) {
        model_results <- if (identical(spec$key, "surrogate")) {
            analysis_results$surrogate_model
        } else if (identical(spec$key, "mfs")) {
            analysis_results$direct_models$mfs
        } else if (identical(spec$key, "mss")) {
            analysis_results$direct_models$mss
        } else if (identical(spec$key, "parsimonious_mfs")) {
            analysis_results$parsimonious_models$mfs
        } else if (identical(spec$key, "parsimonious_mss")) {
            analysis_results$parsimonious_models$mss
        } else {
            stop(sprintf("Unknown unified no-GEP model key: %s", spec$key))
        }
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
            CV_AUC_CI_Lower = model_results$metrics$cv_auc_ci_lower[[1]],
            CV_AUC_CI_Upper = model_results$metrics$cv_auc_ci_upper[[1]],
            CV_Brier_CI_Lower = model_results$metrics$cv_brier_ci_lower[[1]],
            CV_Brier_CI_Upper = model_results$metrics$cv_brier_ci_upper[[1]],
            Calibration_Status = model_results$metrics$calibration_status[[1]],
            Calibration_Intercept = model_results$metrics$calibration_intercept[[1]],
            Calibration_Slope = model_results$metrics$calibration_slope[[1]],
            CV_Calibration_Slope = model_results$metrics$cv_calibration_slope[[1]],
            CV_Calibration_Slope_CI_Lower = model_results$metrics$cv_calibration_slope_ci_lower[[1]],
            CV_Calibration_Slope_CI_Upper = model_results$metrics$cv_calibration_slope_ci_upper[[1]],
            CV_Repeats = model_results$metrics$cv_repeats[[1]],
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
    parsimonious_predictors <- choose_exploratory_parsimonious_predictors(prepared_data)
    parsimonious_mfs_model <- fit_exploratory_binary_model(
        prepared_data$mfs_model_data,
        outcome_var = "mfs_event_5yr",
        predictors = parsimonious_predictors,
        model_name = "Parsimonious Direct 5-Year MFS Risk"
    )
    parsimonious_mss_model <- fit_exploratory_binary_model(
        prepared_data$mss_model_data,
        outcome_var = "mss_event_5yr",
        predictors = parsimonious_predictors,
        model_name = "Parsimonious Direct 5-Year MSS Risk"
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
    parsimonious_no_gep_predictions <- prepared_data$no_gep_prediction %>%
        dplyr::mutate(
            predicted_mfs_5yr_risk = predict_exploratory_binary_model(parsimonious_mfs_model, .),
            predicted_mss_5yr_risk = predict_exploratory_binary_model(parsimonious_mss_model, .)
        )

    no_gep_predictions_sheet <- create_no_gep_predictions_sheet(prepared_data, no_gep_predictions)
    no_gep_summary <- summarize_no_gep_predictions(no_gep_predictions)
    sensitivity_summary <- summarize_pooled_no_gep_sensitivity(no_gep_predictions)
    risk_strata_summary <- summarize_no_gep_risk_strata(no_gep_predictions)
    risk_ladder <- create_exploratory_risk_ladder(
        full_data = full_data,
        direct_mfs_model = direct_mfs_model,
        direct_mss_model = direct_mss_model
    )
    parsimonious_sensitivity <- create_exploratory_parsimonious_sensitivity(
        full_no_gep_predictions = no_gep_predictions,
        parsimonious_no_gep_predictions = parsimonious_no_gep_predictions,
        full_mfs_model = direct_mfs_model,
        full_mss_model = direct_mss_model,
        parsimonious_mfs_model = parsimonious_mfs_model,
        parsimonious_mss_model = parsimonious_mss_model,
        parsimonious_predictors = parsimonious_predictors
    )
    predictor_contribution <- create_predictor_contribution_tab(
        prepared_data = prepared_data,
        surrogate_model = surrogate_model,
        mfs_model = direct_mfs_model,
        mss_model = direct_mss_model
    )
    start_here <- create_exploratory_start_here_tab(
        surrogate_model = surrogate_model,
        mfs_model = direct_mfs_model,
        mss_model = direct_mss_model
    )
    key_findings_5yr <- create_exploratory_key_findings_table(risk_ladder)
    no_gep_subgroups <- create_exploratory_no_gep_subgroups_table(no_gep_summary)
    model_performance <- create_exploratory_model_performance_table(
        surrogate_model = surrogate_model,
        mfs_model = direct_mfs_model,
        mss_model = direct_mss_model,
        parsimonious_mfs_model = parsimonious_mfs_model,
        parsimonious_mss_model = parsimonious_mss_model
    )
    surrogate_coefficients <- create_exploratory_model_coefficients_table(surrogate_model)
    direct_mfs_coefficients <- create_exploratory_model_coefficients_table(direct_mfs_model)
    direct_mss_coefficients <- create_exploratory_model_coefficients_table(direct_mss_model)
    model_calibration <- create_exploratory_model_calibration_table(
        surrogate_model = surrogate_model,
        mfs_model = direct_mfs_model,
        mss_model = direct_mss_model,
        parsimonious_mfs_model = parsimonious_mfs_model,
        parsimonious_mss_model = parsimonious_mss_model
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
        parsimonious_models = list(
            mfs = parsimonious_mfs_model,
            mss = parsimonious_mss_model
        ),
        start_here = start_here,
        key_findings_5yr = key_findings_5yr,
        no_gep_subgroups = no_gep_subgroups,
        model_performance = model_performance,
        surrogate_model_coefficients = surrogate_coefficients,
        direct_mfs_coefficients = direct_mfs_coefficients,
        direct_mss_coefficients = direct_mss_coefficients,
        model_calibration = model_calibration,
        predictor_contribution = predictor_contribution,
        no_gep_summary = no_gep_summary,
        no_gep_predictions = no_gep_predictions,
        no_gep_predictions_sheet = no_gep_predictions_sheet,
        sensitivity_summary = sensitivity_summary,
        risk_strata_summary = risk_strata_summary,
        risk_ladder = risk_ladder,
        parsimonious_sensitivity = parsimonious_sensitivity
    )

    analysis_results$unified_no_gep_overview <- create_no_gep_unified_overview(analysis_results)
    analysis_results$unified_no_gep_model_comparison <- create_no_gep_unified_model_comparison(analysis_results)
    analysis_results$unified_no_gep_risk_strata <- risk_strata_summary
    analysis_results$unified_no_gep_risk_ladder <- risk_ladder %>%
        dplyr::transmute(
            Group = .data$group,
            N = .data$n,
            Predictable_MFS_N = .data$predictable_mfs_n,
            Predictable_MSS_N = .data$predictable_mss_n,
            MFS_5yr_Events = .data$mfs_5yr_events,
            Observed_MFS_5yr_Event_Rate = .data$observed_5yr_mfs_event_rate,
            Median_Predicted_MFS_5yr_Risk = .data$median_predicted_5yr_mfs_risk,
            MSS_5yr_Events = .data$mss_5yr_events,
            Observed_MSS_5yr_Event_Rate = .data$observed_5yr_mss_event_rate,
            Median_Predicted_MSS_5yr_Risk = .data$median_predicted_5yr_mss_risk,
            Interpretation = .data$interpretation
        )

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
#' @param x_label X-axis label for the bin names.
#' @param output_path File path for the saved PNG.
#'
#' @return Invisibly returns the saved plot path.
create_event_rate_bin_plot <- function(summary_data, analysis_name, event_col, plot_title, x_label = "Predicted-risk bin", output_path) {
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
            x = x_label,
            y = "Observed event rate"
        ) +
        ggplot2::theme_minimal(base_size = 14)

    ggplot2::ggsave(output_path, plot, width = 10, height = 6, dpi = PLOT_DPI, bg = "white")
    invisible(output_path)
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

#' Format Group Percentages for Exploratory Text Output
#'
#' @param values Named numeric vector of proportions.
#'
#' @return A single comma-separated character string.
format_exploratory_group_percentages <- function(values) {
    paste(
        sprintf("%s=%.1f%%", names(values), 100 * values),
        collapse = ", "
    )
}

#' Format Group Medians for Exploratory Text Output
#'
#' @param values Named numeric vector of medians.
#'
#' @return A single comma-separated character string.
format_exploratory_group_medians <- function(values) {
    paste(
        sprintf("%s=%.1f", names(values), values),
        collapse = ", "
    )
}

#' Remove the Predictor Prefix From a Dominant Design Term
#'
#' @param predictor Base predictor name.
#' @param dominant_term Expanded design-matrix term.
#'
#' @return A shortened dominant-term label for narrative text.
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

#' Summarize Descriptive Context for a Top Exploratory Predictor
#'
#' @param prepared_data Prepared exploratory data bundle.
#' @param predictor Predictor name to summarize.
#'
#' @return A short narrative sentence.
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

#' Summarize Observed Event Patterns Across Exploratory Risk Bins
#'
#' @param sensitivity_summary Pooled sensitivity summary table.
#' @param analysis_name Analysis label to filter.
#' @param event_col Event-rate column name.
#'
#' @return A short narrative sentence.
summarize_exploratory_bin_pattern <- function(sensitivity_summary, analysis_name, event_col) {
    rows <- sensitivity_summary %>%
        dplyr::filter(.data$analysis == analysis_name, .data$bin %in% c("Low", "Intermediate", "High")) %>%
        dplyr::mutate(bin = factor(.data$bin, levels = c("Low", "Intermediate", "High"))) %>%
        dplyr::arrange(.data$bin)

    if (nrow(rows) != 3) {
        return("Observed risk-bin separation was unavailable from the current summary table.")
    }

    sprintf(
        "Observed %s event rates across pooled %s bins were Low=%.1f%%, Intermediate=%.1f%%, and High=%.1f%%.",
        if (identical(event_col, "observed_mfs_5yr_event_rate")) "5-year MFS" else "5-year MSS",
        analysis_name,
        100 * rows[[event_col]][[1]],
        100 * rows[[event_col]][[2]],
        100 * rows[[event_col]][[3]]
    )
}

#' Extract Exploratory Risk-Bin Rates
#'
#' @param sensitivity_summary Pooled sensitivity summary table.
#' @param analysis_name Analysis label to filter.
#' @param event_col Event-rate column name.
#'
#' @return Named numeric vector with Low, Intermediate, and High rates, or
#'   `NULL` when the summary is incomplete.
extract_exploratory_bin_rates <- function(sensitivity_summary,
                                          analysis_name,
                                          event_col) {
    rows <- sensitivity_summary %>%
        dplyr::filter(.data$analysis == analysis_name, .data$bin %in% c("Low", "Intermediate", "High")) %>%
        dplyr::mutate(bin = factor(.data$bin, levels = c("Low", "Intermediate", "High"))) %>%
        dplyr::arrange(.data$bin)

    if (nrow(rows) != 3) {
        return(NULL)
    }

    stats::setNames(rows[[event_col]], rows$bin)
}

#' Create an Exploratory Model Overview Table Row
#'
#' @param model_label Display label for the model.
#' @param model_context Short description of the model's role.
#' @param model_results Exploratory model bundle.
#' @param sensitivity_summary Pooled sensitivity summary table.
#' @param analysis_name Analysis label for pooled bin summaries.
#' @param event_col Event-rate column name for the pooled summary.
#'
#' @return A single fixed-width table row as character.
create_exploratory_model_overview_row <- function(model_label,
                                                  model_context,
                                                  model_results,
                                                  sensitivity_summary,
                                                  analysis_name,
                                                  event_col) {
    bin_rates <- extract_exploratory_bin_rates(
        sensitivity_summary = sensitivity_summary,
        analysis_name = analysis_name,
        event_col = event_col
    )
    event_label <- if (identical(event_col, "observed_mfs_5yr_event_rate")) {
        "5-year MFS"
    } else {
        "5-year MSS"
    }
    bin_summary <- if (is.null(bin_rates)) {
        "Unavailable"
    } else {
        sprintf(
            "%s bins L/I/H %.1f%% / %.1f%% / %.1f%%",
            event_label,
            100 * bin_rates[["Low"]],
            100 * bin_rates[["Intermediate"]],
            100 * bin_rates[["High"]]
        )
    }

    sprintf(
        "| %-24s | %-32s | %13.3f | %10.3f | %-33s |",
        model_label,
        model_context,
        model_results$metrics$apparent_auc[[1]],
        model_results$metrics$cv_auc[[1]],
        bin_summary
    )
}

#' Create a Top-Predictor Table Block for One Exploratory Model
#'
#' @param model_label Display label for the model.
#' @param model_results Exploratory model bundle.
#' @param prepared_data Prepared exploratory data bundle.
#' @param model_context Short description of the model's role.
#'
#' @return Character vector of fixed-width table lines.
create_exploratory_top_predictor_table <- function(model_label,
                                                   model_results,
                                                   prepared_data,
                                                   model_context) {
    top_rows <- model_results$predictor_contributions %>%
        dplyr::slice_head(n = 3)

    predictor_rows <- purrr::map_chr(seq_len(nrow(top_rows)), function(i) {
        row <- top_rows[i, , drop = FALSE]
        sprintf(
            "| %-4s | %-24s | %-18s | %10.3f | %-18s | %-s |",
            paste0("#", i),
            row$predictor[[1]],
            trim_exploratory_dominant_term(row$predictor[[1]], row$dominant_term[[1]]),
            row$standardized_coefficient[[1]],
            row$direction[[1]],
            summarize_exploratory_predictor_context(prepared_data, row$predictor[[1]])
        )
    })

    c(
        sprintf("%s", model_label),
        sprintf("Role: %s", model_context),
        "| Rank | Predictor                | Dominant term      | Std. coef. | Direction          | Data-backed context |",
        "| ---- | ------------------------ | ------------------ | ---------- | ------------------ | ------------------- |",
        predictor_rows
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
#' @param risk_ladder Four-group descriptive risk ladder table.
#' @param parsimonious_sensitivity Parsimonious direct-model comparison table.
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
                                                   risk_ladder,
                                                   parsimonious_sensitivity,
                                                   sensitivity_summary,
                                                   output_path) {
    failed_row <- no_gep_summary %>% dplyr::filter(.data$no_gep_group == "GEP Failed/Indeterminate")
    not_tested_row <- no_gep_summary %>% dplyr::filter(.data$no_gep_group == "GEP Not Tested")
    class1_ladder <- risk_ladder %>% dplyr::filter(.data$group == "Class 1")
    not_tested_ladder <- risk_ladder %>% dplyr::filter(.data$group == "GEP Not Tested")
    failed_ladder <- risk_ladder %>% dplyr::filter(.data$group == "GEP Failed/Indeterminate")
    class2_ladder <- risk_ladder %>% dplyr::filter(.data$group == "Class 2")
    parsimonious_mfs <- parsimonious_sensitivity %>%
        dplyr::filter(.data$outcome == "MFS", .data$model_specification == "Parsimonious direct model")
    parsimonious_mss <- parsimonious_sensitivity %>%
        dplyr::filter(.data$outcome == "MSS", .data$model_specification == "Parsimonious direct model")
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
        md_heading("Model Overview", 3L),
        "| Model                    | Purpose                          | Apparent AUC | 5-fold CV AUC | Observed pooled risk-bin rates      |",
        "| ------------------------ | -------------------------------- | ------------ | ------------- | ----------------------------------- |",
        create_exploratory_model_overview_row(
            model_label = "Surrogate Class 2-like",
            model_context = "Clinical resemblance, not molecular class",
            model_results = surrogate_model,
            sensitivity_summary = sensitivity_summary,
            analysis_name = "Surrogate_Class2_Probability",
            event_col = "observed_mfs_5yr_event_rate"
        ),
        create_exploratory_model_overview_row(
            model_label = "Direct 5-year MFS",
            model_context = "Main no-GEP metastasis-risk model",
            model_results = mfs_model,
            sensitivity_summary = sensitivity_summary,
            analysis_name = "Direct_MFS_5yr_Risk",
            event_col = "observed_mfs_5yr_event_rate"
        ),
        create_exploratory_model_overview_row(
            model_label = "Direct 5-year MSS",
            model_context = "Main no-GEP melanoma-specific model",
            model_results = mss_model,
            sensitivity_summary = sensitivity_summary,
            analysis_name = "Direct_MSS_5yr_Risk",
            event_col = "observed_mss_5yr_event_rate"
        ),
        "",
        md_heading("Top Predictors with Data-Backed Context", 3L),
        create_exploratory_top_predictor_table(
            model_label = "Surrogate Class 2-like",
            model_results = surrogate_model,
            prepared_data = prepared_data,
            model_context = "Descriptive Class 2-like clinical resemblance score; do not treat this as a recovered molecular label."
        ),
        "",
        create_exploratory_top_predictor_table(
            model_label = "Direct 5-year MFS",
            model_results = mfs_model,
            prepared_data = prepared_data,
            model_context = "Preferred baseline-only metastasis-risk output when GEP is unavailable or unusable."
        ),
        "",
        create_exploratory_top_predictor_table(
            model_label = "Direct 5-year MSS",
            model_results = mss_model,
            prepared_data = prepared_data,
            model_context = "Preferred baseline-only melanoma-specific risk output when GEP is unavailable or unusable."
        )
    )

    summary_lines <- c(
        md_heading("Exploratory No-GEP Risk Report", 1L),
        "",
        sprintf("Dataset: %s", dataset_name),
        "",
        md_heading("Bottom Line", 2L),
        md_bullet("Baseline clinical features provided moderate prognostic discrimination for 5-year MFS/MSS when GEP was unusable, but the same baseline features only weakly approximated definitive molecular class."),
        md_bullet("The surrogate Class 2-like model is descriptive only and should not be used to relabel patients as true Class 1 or Class 2."),
        md_bullet("The direct MFS/MSS models are the preferred outputs when a patient has no usable GEP, but they should be described as exploratory prognostic support rather than precise patient-level forecasts."),
        md_bullet("The no-GEP population should not be presented as one homogeneous intermediate-risk group: overall it sits between definitive Class 1 and Class 2, but the failed/indeterminate subgroup is higher risk than the larger not-tested subgroup."),
        "",
        build_exploratory_no_gep_followup_block(prepared_data = prepared_data, dataset_name = dataset_name),
        "",
        md_heading("Key Findings at 5 Years", 2L),
        md_bullet(sprintf(
            "Group counts: %s",
            paste(sprintf("%s=%d", data_audit$group[seq_len(4)], data_audit$n[seq_len(4)]), collapse = ", ")
        )),
        md_bullet(sprintf(
            "Observed 5-year MFS events: Class 1 %.1f%%, GEP Not Tested %.1f%%, GEP Failed/Indeterminate %.1f%%, Class 2 %.1f%%.",
            100 * class1_ladder$observed_5yr_mfs_event_rate[[1]],
            100 * not_tested_ladder$observed_5yr_mfs_event_rate[[1]],
            100 * failed_ladder$observed_5yr_mfs_event_rate[[1]],
            100 * class2_ladder$observed_5yr_mfs_event_rate[[1]]
        )),
        md_bullet(sprintf(
            "Median predicted 5-year MFS risk from the direct clinical model: Class 1 %.3f, GEP Not Tested %.3f, GEP Failed/Indeterminate %.3f, Class 2 %.3f.",
            class1_ladder$median_predicted_5yr_mfs_risk[[1]],
            not_tested_ladder$median_predicted_5yr_mfs_risk[[1]],
            failed_ladder$median_predicted_5yr_mfs_risk[[1]],
            class2_ladder$median_predicted_5yr_mfs_risk[[1]]
        )),
        md_bullet(sprintf(
            "Median predicted 5-year MSS risk from the direct clinical model: Class 1 %.3f, GEP Not Tested %.3f, GEP Failed/Indeterminate %.3f, Class 2 %.3f.",
            class1_ladder$median_predicted_5yr_mss_risk[[1]],
            not_tested_ladder$median_predicted_5yr_mss_risk[[1]],
            failed_ladder$median_predicted_5yr_mss_risk[[1]],
            class2_ladder$median_predicted_5yr_mss_risk[[1]]
        )),
        "",
        md_heading("No-GEP Subgroup Summary", 2L),
        md_bullet(sprintf(
            "Failed/Indeterminate: median Class 2-like probability %.3f, median predicted 5-year MFS risk %.3f, median predicted 5-year MSS risk %.3f",
            failed_row$median_surrogate_class2_probability[[1]],
            failed_row$median_predicted_mfs_5yr_risk[[1]],
            failed_row$median_predicted_mss_5yr_risk[[1]]
        )),
        md_bullet(sprintf(
            "Not Tested: median Class 2-like probability %.3f, median predicted 5-year MFS risk %.3f, median predicted 5-year MSS risk %.3f",
            not_tested_row$median_surrogate_class2_probability[[1]],
            not_tested_row$median_predicted_mfs_5yr_risk[[1]],
            not_tested_row$median_predicted_mss_5yr_risk[[1]]
        )),
        "",
        md_heading("Technical Notes", 2L),
        md_bullet("A ridge-penalized surrogate model was trained only on patients with definitive Class 1 or Class 2 GEP results."),
        md_bullet("That surrogate stores P(Class 2-like | baseline features); it is a clinical resemblance score, not a recovered molecular class label."),
        md_bullet("Direct MFS and MSS models estimate baseline-only 5-year risk when GEP is unavailable or unusable."),
        md_bullet("Apparent AUC is the in-sample fit; cross-validated AUC is the better estimate of expected performance on new patients."),
        md_bullet("95% repeated-CV intervals show how much AUC, Brier score, and calibration slope change across different fold assignments."),
        "",
        md_heading("Parsimonious Sensitivity Check", 2L),
        md_bullet(sprintf(
            "Parsimonious direct MFS model using %s retained a CV AUC of %.3f (95%% repeated-CV interval %.3f to %.3f).",
            paste(unique(strsplit(parsimonious_mfs$predictors[[1]], ", ")[[1]]), collapse = ", "),
            parsimonious_mfs$cv_auc[[1]],
            parsimonious_mfs$cv_auc_ci_lower[[1]],
            parsimonious_mfs$cv_auc_ci_upper[[1]]
        )),
        md_bullet(sprintf(
            "Parsimonious direct MSS model retained a CV AUC of %.3f (95%% repeated-CV interval %.3f to %.3f).",
            parsimonious_mss$cv_auc[[1]],
            parsimonious_mss$cv_auc_ci_lower[[1]],
            parsimonious_mss$cv_auc_ci_upper[[1]]
        )),
        md_bullet("Similar performance under the parsimonious specification supports the subgroup ordering without requiring a larger baseline feature set."),
        "",
        md_heading("Retained Baseline Predictors", 2L),
        if (length(retained_predictors) > 0) {
            vapply(retained_predictors, md_bullet, character(1))
        } else {
            md_bullet("None")
        },
        "",
        md_heading("Dropped Candidate Predictors", 2L),
        if (length(dropped_predictors) > 0) {
            vapply(dropped_predictors, md_bullet, character(1))
        } else {
            md_bullet("None")
        },
        "",
        top_predictor_block,
        "",
        md_heading("Baseline Separation Note", 2L),
        md_bullet(sprintf(
            "The strongest 4-group separation among candidate baseline predictors was %s (p=%s).",
            best_baseline_row$variable[[1]],
            format_gep_p_value(best_baseline_row$p_value[[1]])
        )),
        "",
        md_heading("Ciliary Coding Note", 2L),
        md_bullet("Ciliary involvement is now derived from location values containing 'Cilio' or 'Ciliary'. The earlier all-zero derivation was incorrect because the source data encode this location as 'Cilio-Choroidal'.")
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
    parsimonious_mfs_model <- analysis_results$parsimonious_models$mfs
    parsimonious_mss_model <- analysis_results$parsimonious_models$mss
    start_here <- analysis_results$start_here
    key_findings_5yr <- analysis_results$key_findings_5yr
    no_gep_subgroups <- analysis_results$no_gep_subgroups
    model_performance <- analysis_results$model_performance
    surrogate_model_coefficients <- analysis_results$surrogate_model_coefficients
    direct_mfs_coefficients <- analysis_results$direct_mfs_coefficients
    direct_mss_coefficients <- analysis_results$direct_mss_coefficients
    model_calibration <- analysis_results$model_calibration
    predictor_contribution <- analysis_results$predictor_contribution
    no_gep_summary <- analysis_results$no_gep_summary
    no_gep_predictions <- analysis_results$no_gep_predictions
    no_gep_predictions_sheet <- analysis_results$no_gep_predictions_sheet
    sensitivity_summary <- analysis_results$sensitivity_summary
    risk_ladder <- analysis_results$risk_ladder
    parsimonious_sensitivity <- analysis_results$parsimonious_sensitivity

    workbook_data <- list(
        Start_Here = start_here,
        Key_Findings_5yr = key_findings_5yr,
        Risk_Ladder_5yr = risk_ladder,
        No_GEP_Subgroups = no_gep_subgroups,
        Model_Performance = model_performance,
        Parsimonious_Sensitivity = parsimonious_sensitivity,
        Surrogate_Model_Coefficients = surrogate_model_coefficients,
        Direct_MFS_Coefficients = direct_mfs_coefficients,
        Direct_MSS_Coefficients = direct_mss_coefficients,
        Model_Calibration = model_calibration,
        Predictor_Contribution = predictor_contribution,
        Baseline_Comparisons = baseline_summary,
        Data_Audit = data_audit,
        No_GEP_Predictions = no_gep_predictions_sheet,
        Sensitivity_Pooled_No_GEP = sensitivity_summary,
        KM_Corrected_MFS = km_corrected_mfs,
        KM_Corrected_MSS = km_corrected_mss
    )

    workbook_path <- file.path(output_dir, "full_cohort_exploratory_no_gep_report.xlsx")
    summary_path <- file.path(output_dir, "full_cohort_exploratory_no_gep_summary.md")

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
        risk_ladder = risk_ladder,
        sensitivity_summary = sensitivity_summary,
        parsimonious_sensitivity = parsimonious_sensitivity,
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
        plot_title = "Observed 5-Year MFS Event Rate by Surrogate Class 2-Like Score Bin",
        x_label = "Surrogate Class 2-like score bin",
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
        parsimonious_models = list(
            mfs = parsimonious_mfs_model,
            mss = parsimonious_mss_model
        ),
        start_here = start_here,
        key_findings_5yr = key_findings_5yr,
        no_gep_subgroups = no_gep_subgroups,
        model_performance = model_performance,
        surrogate_model_coefficients = surrogate_model_coefficients,
        direct_mfs_coefficients = direct_mfs_coefficients,
        direct_mss_coefficients = direct_mss_coefficients,
        model_calibration = model_calibration,
        predictor_contribution = predictor_contribution,
        no_gep_summary = no_gep_summary,
        no_gep_predictions = no_gep_predictions_sheet,
        sensitivity_summary = sensitivity_summary,
        risk_strata_summary = analysis_results$risk_strata_summary,
        risk_ladder = risk_ladder,
        parsimonious_sensitivity = parsimonious_sensitivity,
        unified_no_gep_overview = analysis_results$unified_no_gep_overview,
        unified_no_gep_model_comparison = analysis_results$unified_no_gep_model_comparison,
        unified_no_gep_risk_strata = analysis_results$unified_no_gep_risk_strata,
        unified_no_gep_risk_ladder = analysis_results$unified_no_gep_risk_ladder,
        output_paths = c(
            list(workbook = workbook_path, summary = summary_path),
            plot_paths
        )
    )
}
