# Vision and Safety Analysis Functions
# Author: Nicholas Camarda
# Description: Functions for vision change and radiation sequelae analysis

get_ordered_treatment_groups <- function(data, group_var = "treatment_group") {
    if (!group_var %in% names(data)) {
        return(character())
    }

    group_values <- data[[group_var]]
    group_values <- group_values[!is.na(group_values)]
    if (length(group_values) == 0) {
        return(character())
    }

    if (identical(group_var, "treatment_group")) {
        group_values <- normalize_treatment_group_values(group_values)
    }

    if (is.factor(group_values)) {
        return(levels(droplevels(group_values)))
    }

    unique(as.character(group_values))
}

format_effect_summary_pvalue <- function(p_value) {
    if (is.null(p_value) || length(p_value) == 0 || is.na(p_value)) {
        return("p = NA")
    }
    if (p_value < 0.001) {
        return("p < 0.001")
    }
    sprintf("p = %.3f", p_value)
}

format_continuous_summary_string <- function(values, digits = 1) {
    values <- values[!is.na(values)]
    if (length(values) == 0) {
        return(NA_character_)
    }

    sprintf(
        paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f); mean %.", digits, "f"),
        stats::median(values),
        min(values),
        max(values),
        mean(values)
    )
}

#' Evaluate an expression with a local RNG seed and restore prior RNG state
#'
#' Runs seeded calculations without changing the caller's `.Random.seed`. This
#' is used for Objective 2 simulated Fisher p-values so reported descriptive
#' p-values are reproducible without perturbing unrelated analyses.
#'
#' @param seed Integer random seed.
#' @param expr Expression to evaluate under the local seed.
#' @return The value produced by `expr`.
with_preserved_rng_seed <- function(seed, expr) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    previous_seed <- if (had_seed) {
        get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
        NULL
    }

    on.exit({
        if (had_seed) {
            assign(".Random.seed", previous_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
            rm(".Random.seed", envir = .GlobalEnv)
        }
    }, add = TRUE)

    set.seed(seed)
    force(expr)
}

#' Compute an Objective 2 simulated Fisher p-value with the configured seed
#'
#' Uses the central Objective 2 seed for sparse categorical descriptive tests.
#' Degenerate tables return `NA` instead of stopping the analysis.
#'
#' @param data Data frame containing grouping and outcome columns.
#' @param group_var Character scalar grouping column name.
#' @param outcome_var Character scalar categorical outcome column name.
#' @return Numeric p-value or `NA_real_` when the table is not testable.
calculate_objective2_fisher_p_value <- function(data, group_var, outcome_var) {
    test_data <- data %>%
        dplyr::filter(!is.na(.data[[group_var]]), !is.na(.data[[outcome_var]]))

    if (
        nrow(test_data) == 0 ||
            dplyr::n_distinct(test_data[[group_var]]) < 2 ||
            dplyr::n_distinct(test_data[[outcome_var]]) < 2
    ) {
        return(NA_real_)
    }

    contingency_table <- table(test_data[[group_var]], test_data[[outcome_var]])
    if (any(dim(contingency_table) < 2)) {
        return(NA_real_)
    }

    with_preserved_rng_seed(
        OBJECTIVE2_SIMULATED_FISHER_SEED,
        tryCatch(
            stats::fisher.test(contingency_table, simulate.p.value = TRUE)$p.value,
            error = function(e) NA_real_
        )
    )
}

#' Safely compute a Wilcoxon treatment-group p-value
#'
#' Returns `NA` for one-group, all-missing, or otherwise untestable vision
#' summaries so Objective 2 can still write descriptive artifacts.
#'
#' @param data Data frame containing the grouping and continuous outcome columns.
#' @param value_var Character scalar continuous outcome column name.
#' @param group_var Character scalar grouping column name.
#' @return Numeric p-value or `NA_real_` when the comparison is not testable.
safe_wilcox_p_value <- function(data, value_var, group_var = "treatment_group") {
    test_data <- data %>%
        dplyr::filter(!is.na(.data[[group_var]]), !is.na(.data[[value_var]]))

    if (nrow(test_data) == 0 || dplyr::n_distinct(test_data[[group_var]]) < 2) {
        return(NA_real_)
    }

    tryCatch(
        stats::wilcox.test(stats::as.formula(paste(value_var, "~", group_var)), data = test_data)$p.value,
        error = function(e) NA_real_
    )
}

#' Add p-values to a gtsummary table without stopping Objective 2 outputs
#'
#' Keeps descriptive tables publishable when sparse or degenerate data prevent a
#' p-value calculation. The returned table includes a blank p-value column when
#' `gtsummary::add_p()` cannot compute one.
#'
#' @param summary_table A gtsummary object.
#' @param context_label Character scalar used in warning messages.
#' @param test Optional `gtsummary::add_p()` test specification.
#' @param test.args Optional `gtsummary::add_p()` test arguments.
#' @return A gtsummary object with p-value support where available.
safe_add_p_to_summary <- function(summary_table, context_label, test = NULL, test.args = NULL) {
    tryCatch(
        {
            if (is.null(test.args)) {
                summary_table %>% add_p(test = test)
            } else {
                summary_table %>% add_p(test = test, test.args = test.args)
            }
        },
        error = function(e) {
            logger::log_warn(sprintf("%s p-value not computed: %s", context_label, e$message))
            summary_table %>%
                modify_table_body(function(body) {
                    if (!"p.value" %in% names(body)) {
                        body$p.value <- NA_real_
                    }
                    body
                })
        }
    )
}

#' Resolve the Objective 2 burden field for a toxicity endpoint
#'
#' Looks up the Objective 0-prepared burden field that Objective 2 must consume
#' for the requested toxicity endpoint.
#'
#' @param sequela_type Character scalar toxicity endpoint source field.
#' @return Character scalar prepared burden field name.
resolve_objective2_toxicity_burden_field <- function(sequela_type) {
    endpoint <- OBJECTIVE2_TOXICITY_ENDPOINTS %>%
        dplyr::filter(.data$source_field == sequela_type)

    if (nrow(endpoint) != 1) {
        stop(sprintf("No Objective 2 toxicity endpoint contract found for '%s'.", sequela_type), call. = FALSE)
    }

    endpoint$analysis_field[[1]]
}

#' Assert that an Objective 2 toxicity burden field is analysis-ready
#'
#' Objective 2 does not recode raw toxicity source values. This assertion
#' requires the Objective 0-prepared burden field to be present, numeric, and
#' complete binary 0/1 before descriptive or model outputs are generated.
#'
#' @param data Data frame used by Objective 2.
#' @param analysis_field Character scalar prepared burden field name.
#' @param sequela_type Character scalar toxicity endpoint source field.
#' @return Invisibly returns `TRUE` when the field is valid.
assert_valid_objective2_toxicity_burden_field <- function(data, analysis_field, sequela_type) {
    if (!analysis_field %in% names(data)) {
        stop(
            sprintf(
                "Objective 2 requires Objective 0-validated toxicity burden field '%s' for %s; re-run Objective 0 data derivation/validation.",
                analysis_field,
                sequela_type
            ),
            call. = FALSE
        )
    }

    if (!is.numeric(data[[analysis_field]]) && !is.integer(data[[analysis_field]])) {
        stop(
            sprintf(
                "Objective 2 requires numeric binary 0/1 toxicity burden field '%s'; found class %s.",
                analysis_field,
                paste(class(data[[analysis_field]]), collapse = ", ")
            ),
            call. = FALSE
        )
    }

    invalid_rows <- is.na(data[[analysis_field]]) | !data[[analysis_field]] %in% c(0, 1)
    if (any(invalid_rows)) {
        stop(
            sprintf(
                "Objective 2 requires Objective 0-validated toxicity burden field '%s'; found %d missing/non-binary row(s).",
                analysis_field,
                sum(invalid_rows)
            ),
            call. = FALSE
        )
    }

    invisible(TRUE)
}

build_grouped_continuous_summary <- function(data, value_var, digits = 1) {
    group_var <- "treatment_group"
    overall_stat <- format_continuous_summary_string(data[[value_var]], digits = digits)
    overall_median <- suppressWarnings(round(stats::median(data[[value_var]], na.rm = TRUE), digits))
    grouped_stats <- c(Overall = overall_stat)

    for (group_name in get_ordered_treatment_groups(data, group_var = group_var)) {
        group_values <- data %>%
            filter(as.character(.data[[group_var]]) == group_name) %>%
            pull(.data[[value_var]])
        grouped_stats[[group_name]] <- format_continuous_summary_string(group_values, digits = digits)
    }

    list(
        display_stats = grouped_stats,
        overall_estimate = ifelse(is.finite(overall_median), overall_median, NA_real_),
        n_outcome_non_missing = sum(!is.na(data[[value_var]]))
    )
}

build_summary_note <- function(display_stats, p_value = NA_real_, suffix = NULL) {
    parts <- vapply(
        names(display_stats),
        FUN.VALUE = character(1),
        FUN = function(name) sprintf("%s: %s", name, display_stats[[name]])
    )
    parts <- c(parts, format_effect_summary_pvalue(p_value))
    if (!is.null(suffix) && nzchar(suffix)) {
        parts <- c(parts, suffix)
    }
    paste(parts, collapse = "; ")
}

build_distribution_note <- function(data, category_var, detail_file_label, suffix = NULL) {
    non_missing_data <- data %>%
        filter(!is.na(.data[[category_var]]))

    p_value <- calculate_objective2_fisher_p_value(non_missing_data, "treatment_group", category_var)
    category_count <- dplyr::n_distinct(stats::na.omit(non_missing_data[[category_var]]))
    parts <- c(
        sprintf("Observed %d non-missing ordered categories.", category_count),
        format_effect_summary_pvalue(p_value),
        sprintf("Detailed counts are saved in %s.", detail_file_label)
    )
    if (!is.null(suffix) && nzchar(suffix)) {
        parts <- c(parts, suffix)
    }
    paste(parts, collapse = " ")
}

build_binary_rate_note <- function(data, outcome_var, suffix = NULL) {
    group_var <- "treatment_group"
    overall_n <- sum(!is.na(data[[outcome_var]]))
    overall_events <- sum(data[[outcome_var]] == 1, na.rm = TRUE)
    overall_rate <- if (overall_n > 0) round(100 * overall_events / overall_n, 1) else NA_real_

    parts <- sprintf("Overall: %d/%d (%.1f%%)", overall_events, overall_n, overall_rate)

    for (group_name in get_ordered_treatment_groups(data, group_var = group_var)) {
        group_data <- data %>%
            filter(as.character(.data[[group_var]]) == group_name)
        group_n <- sum(!is.na(group_data[[outcome_var]]))
        group_events <- sum(group_data[[outcome_var]] == 1, na.rm = TRUE)
        group_rate <- if (group_n > 0) round(100 * group_events / group_n, 1) else NA_real_
        parts <- c(parts, sprintf("%s: %d/%d (%.1f%%)", group_name, group_events, group_n, group_rate))
    }

    p_value <- calculate_objective2_fisher_p_value(data, group_var, outcome_var)
    parts <- c(parts, format_effect_summary_pvalue(p_value))
    if (!is.null(suffix) && nzchar(suffix)) {
        parts <- c(parts, suffix)
    }

    paste(parts, collapse = "; ")
}

#' Safely summarize numeric timing values
#'
#' @param values Numeric vector.
#' @return Named numeric vector with mean/median/min/max, using NA when no values exist.
safe_numeric_range_summary <- function(values) {
    values <- values[!is.na(values)]
    if (length(values) == 0) {
        return(c(mean = NA_real_, median = NA_real_, min = NA_real_, max = NA_real_))
    }
    c(
        mean = mean(values),
        median = stats::median(values),
        min = min(values),
        max = max(values)
    )
}

#' Add latest visual-acuity follow-up timing for reviewer-response sensitivity
#'
#' @param data Data frame.
#' @return Data frame with explicit and proxy latest-VA timing fields.
add_last_vision_followup_months <- function(data) {
    explicit_months <- rep(NA_real_, nrow(data))
    if (all(c("treatment_date", "last_followup") %in% names(data))) {
        explicit_months <- suppressWarnings(lubridate::time_length(
            lubridate::interval(data$treatment_date, data$last_followup),
            unit = "months"
        ))
    }

    proxy_months <- explicit_months
    if ("follow_up_months" %in% names(data)) {
        follow_up_months <- suppressWarnings(as.numeric(data$follow_up_months))
        proxy_months <- dplyr::if_else(is.na(proxy_months), follow_up_months, proxy_months)
    }

    data$last_vision_followup_months_explicit <- explicit_months
    data$last_vision_followup_months_proxy <- proxy_months
    data$last_vision_followup_timing_source <- dplyr::case_when(
        !is.na(explicit_months) ~ "explicit_last_followup",
        is.na(explicit_months) & !is.na(proxy_months) ~ "proxy_general_recorded_followup",
        TRUE ~ "missing_timing"
    )
    data$last_vision_followup_months <- data$last_vision_followup_months_explicit
    data
}

#' Summarize latest visual-acuity timing-source availability
#'
#' @param data Data frame.
#' @return Tibble with explicit and proxy timing availability counts.
summarize_vision_followup_timing_sources <- function(data) {
    data <- add_last_vision_followup_months(data)
    last_vision_present <- if ("last_vision" %in% names(data)) !is.na(data$last_vision) else rep(FALSE, nrow(data))
    explicit_present <- !is.na(data$last_vision_followup_months_explicit)
    proxy_present <- !is.na(data$last_vision_followup_months_proxy)
    recovered_by_proxy <- !explicit_present & proxy_present

    height_months <- if (all(c("treatment_date", "last_height_date") %in% names(data))) {
        suppressWarnings(lubridate::time_length(
            lubridate::interval(data$treatment_date, data$last_height_date),
            unit = "months"
        ))
    } else {
        rep(NA_real_, nrow(data))
    }

    tibble::tibble(
        timing_definition = c(
            "explicit_last_followup",
            "proxy_general_recorded_followup",
            "recovered_by_proxy_when_explicit_missing",
            "last_height_date_comparison"
        ),
        n_patients = c(
            sum(explicit_present),
            sum(proxy_present),
            sum(recovered_by_proxy),
            sum(!is.na(height_months))
        ),
        n_with_last_vision = c(
            sum(explicit_present & last_vision_present),
            sum(proxy_present & last_vision_present),
            sum(recovered_by_proxy & last_vision_present),
            sum(!is.na(height_months) & last_vision_present)
        ),
        note = c(
            "Treatment-to-last_followup timing; primary conservative timing field for latest-VA minimum-follow-up sensitivity.",
            "Uses explicit last_followup timing when available; otherwise uses the derived general follow_up_months field as a proxy for recorded follow-up duration.",
            "Rows that have latest VA and general follow-up duration but no explicit last_followup date.",
            "Tumor-height timing is summarized separately and is not used as the latest-VA timing proxy."
        )
    )
}

#' Summarize visual-acuity follow-up timing by treatment group
#'
#' @param data Data frame.
#' @param value_var Character scalar timing variable.
#' @return Tibble with treatment-group timing summary.
summarize_vision_followup_by_group <- function(data, value_var = "last_vision_followup_months") {
    if (!all(c("treatment_group", value_var) %in% names(data))) {
        return(tibble::tibble())
    }
    data %>%
        dplyr::group_by(.data$treatment_group) %>%
        dplyr::summarise(
            variable = value_var,
            n_rows = dplyr::n(),
            n_nonmissing = sum(!is.na(.data[[value_var]])),
            mean_months = safe_numeric_range_summary(.data[[value_var]])[["mean"]],
            median_months = safe_numeric_range_summary(.data[[value_var]])[["median"]],
            min_months = safe_numeric_range_summary(.data[[value_var]])[["min"]],
            max_months = safe_numeric_range_summary(.data[[value_var]])[["max"]],
            .groups = "drop"
        )
}

#' Build minimum-follow-up visual-acuity sensitivity summary
#'
#' @param data Data frame with visual-acuity change and latest-VA follow-up timing.
#' @param min_followup_months Numeric minimum follow-up threshold.
#' @param timing_var Character scalar timing variable.
#' @return List with filtered data and summary table.
build_visual_acuity_min_followup_sensitivity <- function(data,
                                                         min_followup_months = 36,
                                                         timing_var = "last_vision_followup_months_explicit") {
    followup_data <- add_last_vision_followup_months(data)
    filtered <- followup_data %>%
        dplyr::filter(
            !is.na(.data$vision_change),
            !is.na(.data[[timing_var]]),
            .data[[timing_var]] >= min_followup_months
        )
    summary <- filtered %>%
        dplyr::group_by(.data$treatment_group) %>%
        dplyr::summarise(
            min_followup_months = min_followup_months,
            timing_definition = timing_var,
            n = dplyr::n(),
            mean_last_vision_followup_months = safe_numeric_range_summary(.data[[timing_var]])[["mean"]],
            median_last_vision_followup_months = safe_numeric_range_summary(.data[[timing_var]])[["median"]],
            mean_vision_change = safe_numeric_range_summary(.data$vision_change)[["mean"]],
            median_vision_change = safe_numeric_range_summary(.data$vision_change)[["median"]],
            min_vision_change = safe_numeric_range_summary(.data$vision_change)[["min"]],
            max_vision_change = safe_numeric_range_summary(.data$vision_change)[["max"]],
            .groups = "drop"
        )
    list(data = filtered, summary = summary)
}

#' Declare reviewer-facing toxicity scope for Objective 2 outputs
#'
#' @return Tibble describing the supported toxicity endpoint scope.
objective2_toxicity_scope_note <- function() {
    tibble::tibble(
        endpoint_family = "retinal_toxicity",
        scope = "recorded_burden_by_available_follow_up",
        reviewer_label = "Retinopathy, neovascular glaucoma, and serous retinal detachment were analyzed as recorded burden by available follow-up, not as standardized graded or time-to-toxicity incidence endpoints.",
        limitation = "The checked source and derived fields do not provide CTCAE-style grades or dated onset fields for each toxicity. SRD cause is available for records coded as SRD, but the current Objective 2 burden endpoint is not a radiation-induced-only SRD incidence analysis."
    )
}

#' Reviewer-requested visual outcome predictor candidates
#'
#' @return Tibble mapping reviewer-requested predictors to analytic fields.
visual_reviewer_predictor_candidates <- function() {
    tibble::tribble(
        ~reviewer_predictor, ~field, ~candidate_term, ~candidate_role, ~note,
        "Baseline visual acuity", "initial_vision", "initial_vision", "baseline_adjustment", "Available baseline logMAR visual acuity.",
        "Latest visual-acuity follow-up duration", "last_vision_followup_months_explicit", "last_vision_followup_months_explicit", "followup_adjustment", "Available treatment-to-last_followup timing for latest VA.",
        "Tumor height", "initial_tumor_height", "initial_tumor_height", "baseline_predictor", "Available baseline tumor height.",
        "Basal diameter", "initial_tumor_diameter", "initial_tumor_diameter", "baseline_predictor", "Available baseline largest basal diameter.",
        "T stage", "initial_t_stage_simple", "initial_t_stage_simple", "baseline_predictor", "Available baseline T-stage grouping used elsewhere for model support.",
        "Subretinal fluid", "srf", "srf", "baseline_predictor", "Available baseline subretinal-fluid field.",
        "Optic-nerve proximity/involvement", "optic_nerve", "optic_nerve", "baseline_predictor", "Available optic-nerve involvement/abutment field; this is not a quantitative distance.",
        "Treatment year", "treatment_year", "treatment_year", "baseline_predictor", "Derived centrally from treatment_date during Objective 0.",
        "Macular or foveal proximity", NA_character_, NA_character_, "not_available", "No structured macular/foveal proximity or distance field is present in the checked analytic/source columns.",
        "Baseline retinal detachment", NA_character_, NA_character_, "not_available", "No baseline retinal-detachment predictor is present. The available srd/srd_cause fields are recorded Objective 2 toxicity-burden fields, not baseline adjustment variables.",
        "Radiation dose to optic disc/nerve", NA_character_, NA_character_, "not_available", "No structured cross-modality optic-disc/optic-nerve dose field is present in the checked analytic/source columns.",
        "Radiation dose to macula", NA_character_, NA_character_, "not_available", "No structured cross-modality macular dose field is present in the checked analytic/source columns."
    )
}

#' Screen reviewer-requested visual outcome predictors for model viability
#'
#' @param data Data frame prepared for visual acuity modeling.
#' @param min_level_n Minimum non-missing observations required per categorical level.
#' @return List with selected terms and an availability/decision tibble.
screen_visual_reviewer_predictors <- function(data, min_level_n = 5L) {
    candidate_table <- visual_reviewer_predictor_candidates()
    selected_terms <- character()

    availability <- candidate_table %>%
        rowwise() %>%
        mutate(
            present = !is.na(field) && field %in% names(data),
            non_missing_n = if (present) sum(!is.na(data[[field]])) else 0L,
            unique_non_missing_n = if (present) length(unique(data[[field]][!is.na(data[[field]])])) else 0L,
            min_level_n_observed = if (present && (is.factor(data[[field]]) || is.character(data[[field]]))) {
                counts <- table(data[[field]], useNA = "no")
                if (length(counts) == 0) 0L else as.integer(min(counts))
            } else {
                NA_integer_
            },
            denominator_n = nrow(data),
            included_in_latest_va_model = present &&
                !is.na(candidate_term) &&
                unique_non_missing_n >= 2L &&
                (is.na(min_level_n_observed) || min_level_n_observed >= min_level_n),
            exclusion_reason = dplyr::case_when(
                candidate_role == "not_available" ~ note,
                !present ~ "Field not present in prepared analytic dataset.",
                non_missing_n == 0L ~ "Field has no non-missing values.",
                unique_non_missing_n < 2L ~ "Field has insufficient variation in this cohort.",
                !is.na(min_level_n_observed) && min_level_n_observed < min_level_n ~ sprintf(
                    "Categorical field has a level with fewer than %d observations.",
                    min_level_n
                ),
                included_in_latest_va_model ~ "Included.",
                TRUE ~ "Not included."
            )
        ) %>%
        ungroup()

    selected_terms <- availability %>%
        filter(.data$included_in_latest_va_model) %>%
        pull(.data$candidate_term) %>%
        unique()

    list(
        selected_terms = selected_terms,
        availability = availability
    )
}

#' Select terms that do not make a linear model rank-deficient
#'
#' @param data Complete-case model data.
#' @param outcome_var Outcome column name.
#' @param predictor_terms Terms forced before candidate terms.
#' @param candidate_terms Candidate covariate terms to add in order.
#' @return List with supported terms and dropped-term diagnostics.
select_rank_supported_linear_terms <- function(data, outcome_var, predictor_terms, candidate_terms) {
    supported_terms <- character()
    dropped_rows <- list()

    for (candidate_term in unique(candidate_terms)) {
        trial_terms <- unique(c(predictor_terms, supported_terms, candidate_term))
        trial_formula <- stats::as.formula(paste(outcome_var, "~", paste(trial_terms, collapse = " + ")))
        rank_ok <- tryCatch(
            {
                model_frame <- stats::model.frame(trial_formula, data = data, na.action = stats::na.omit)
                model_matrix <- stats::model.matrix(trial_formula, data = model_frame)
                qr(model_matrix)$rank == ncol(model_matrix)
            },
            error = function(e) {
                FALSE
            }
        )

        if (rank_ok) {
            supported_terms <- c(supported_terms, candidate_term)
        } else {
            dropped_rows[[length(dropped_rows) + 1L]] <- tibble::tibble(
                candidate_term = candidate_term,
                exclusion_reason = "Excluded because adding this term made the latest-VA sensitivity model rank-deficient."
            )
        }
    }

    list(
        supported_terms = supported_terms,
        dropped_terms = if (length(dropped_rows) > 0) dplyr::bind_rows(dropped_rows) else tibble::tibble()
    )
}

#' Analyze visual acuity changes by treatment group
#'
#' Calculates and summarizes changes in visual acuity by treatment group.
#' This function is used for objective 2a and does not include subgroup interactions.
#'
#' @param data A data frame containing vision-related variables.
#' @param output_dirs A named list of output directories organized by analysis type (e.g., recurrence, mets, os, pfs, height, subgroups).
#' @param prefix A character string used as a file prefix for output files (e.g., "full_cohort_") to identify cohort or analysis context in filenames.
#' @param confounders Character vector of confounders to adjust for in the analysis.
#' @param dataset_name Character string dataset identifier for diagnostics and effect summaries.
#'
#' @return A list with the following elements:
#'   - changes: summary data frame of vision changes by treatment group
#'   - table: gtsummary object with formatted summary statistics
#'   - regression_model: linear model (lm) object for vision change by treatment group
#'   - regression_table: gtsummary object summarizing the regression results
#'
#' @examples
#' analyze_visual_acuity_changes(data, output_dirs, prefix)
analyze_visual_acuity_changes <- function(data, output_dirs, prefix, confounders = NULL, dataset_name = NULL) {
    data <- normalize_treatment_group_data(data)
    data <- add_last_vision_followup_months(data)
    vision_descriptive_dir <- resolve_route_output_dir(output_dirs, "obj2_vision", "descriptive")
    vision_adjusted_dir <- resolve_route_output_dir(output_dirs, "obj2_vision", "adjusted_models")
    vision_effect_summary_dir <- resolve_route_output_dir(output_dirs, "obj2_vision", "effect_summary")
    vision_sensitivity_dir <- resolve_route_output_dir(output_dirs, "obj2_vision", "sensitivity")
    # Calculate vision changes (row-level)
    # Vision change is already calculated in data derivation (Objective 0)
    # Positive values = improvement (lower logMAR), negative = worsening
    
    # Ensure consistent factor contrasts for modeling
    data_with_vision_change <- enforce_unordered_factors(data)

    # Preserve the full analytic set for descriptive summaries/tests (no location filtering)
    summary_data <- data_with_vision_change %>%
        mutate(
            vision_line_change = compute_line_change_lines(vision_change),
            vision_line_change_label = categorize_line_change(vision_change),
            vision_line_change_bucket = assign_line_change_bucket(vision_line_change)
        )
    vision_change_contract_note <- paste(
        "Vision endpoint is visual-acuity change score",
        "(initial vision minus final or recurrence-pre-treatment vision);",
        "baseline visual acuity and latest-VA follow-up time are reviewer-response sensitivity considerations;",
        "minimum-follow-up sensitivity uses explicit last_followup timing as primary and a separately labeled general-follow-up proxy as sensitivity context."
    )
    ordinal_assumption_note <- paste(
        "Proportional-odds assumption was not formally tested;",
        "ordinal odds ratios are assumption-dependent descriptive model summaries."
    )

    line_levels <- line_change_label_levels(summary_data$vision_line_change)
    line_values <- line_change_ordered_values(summary_data$vision_line_change)

    if (length(line_levels) > 0) {
        summary_data <- summary_data %>%
            mutate(
                vision_line_change_label = factor(vision_line_change_label, levels = line_levels, ordered = TRUE)
            )
    }

    if (!is.null(summary_data$vision_line_change_bucket)) {
        summary_data <- summary_data %>%
            mutate(
                vision_line_change_bucket = factor(
                    vision_line_change_bucket,
                    levels = VISION_LINE_CHANGE_CATEGORY_LEVELS,
                    ordered = TRUE
                )
            )
    }

    confounders_for_model <- confounders %||% character()
    confounders_for_model <- confounders_for_model[confounders_for_model %in% names(data_with_vision_change)]
    exclusion_vars <- unique(c("treatment_group", confounders_for_model))
    exclusion_result <- apply_sparse_level_exclusions(
        data_with_vision_change,
        variables = exclusion_vars[exclusion_vars %in% names(data_with_vision_change)],
        analysis_name = "vision_change_linear",
        id_col = pick_sparse_level_id_col(data_with_vision_change),
        level_exclusions = MODELING_LEVEL_EXCLUSIONS
    )

    if (exclusion_result$removed_row_count > 0) {
        logger::log_info(sprintf(
            "Excluded %d rows with sparse categorical levels prior to vision change analysis",
            exclusion_result$removed_row_count
        ))
    }

    vision_model_data <- exclusion_result$data
    line_change_model_data <- vision_model_data %>%
        mutate(
            vision_line_change = compute_line_change_lines(vision_change)
        ) %>%
        filter(!is.na(vision_line_change))

    line_change_filter_stats <- exclusion_result$filter_stats
    if (!is.null(line_change_filter_stats)) {
        line_change_removed_n <- nrow(vision_model_data) - nrow(line_change_model_data)
        line_change_filter_stats$model_n <- nrow(line_change_model_data)
        line_change_filter_stats$removed_n <- line_change_filter_stats$removed_n + line_change_removed_n
        line_change_filter_stats$removed_pct <- if (line_change_filter_stats$initial_n > 0) {
            round(100 * line_change_filter_stats$removed_n / line_change_filter_stats$initial_n, 1)
        } else {
            0
        }
        if (line_change_removed_n > 0) {
            line_change_filter_stats$removal_reason <- paste(
                exclusion_result$filter_stats$removal_reason,
                sprintf("Excluded %d additional rows with missing Snellen line-change outcome.", line_change_removed_n)
            )
        }
    }

    ordinal_model_data <- vision_model_data %>%
        mutate(
            vision_line_change = compute_line_change_lines(vision_change),
            vision_line_change_bucket = assign_line_change_bucket(vision_line_change)
        ) %>%
        filter(!is.na(vision_line_change_bucket)) %>%
        mutate(
            vision_line_change_bucket = factor(
                vision_line_change_bucket,
                levels = VISION_LINE_CHANGE_CATEGORY_LEVELS,
                ordered = TRUE
            )
        )

    ordinal_filter_stats <- exclusion_result$filter_stats
    if (!is.null(ordinal_filter_stats)) {
        ordinal_removed_n <- nrow(vision_model_data) - nrow(ordinal_model_data)
        ordinal_filter_stats$model_n <- nrow(ordinal_model_data)
        ordinal_filter_stats$removed_n <- ordinal_filter_stats$removed_n + ordinal_removed_n
        ordinal_filter_stats$removed_pct <- if (ordinal_filter_stats$initial_n > 0) {
            round(100 * ordinal_filter_stats$removed_n / ordinal_filter_stats$initial_n, 1)
        } else {
            0
        }
        if (ordinal_removed_n > 0) {
            ordinal_filter_stats$removal_reason <- paste(
                exclusion_result$filter_stats$removal_reason,
                sprintf("Excluded %d additional rows with missing Snellen line-change distribution outcome.", ordinal_removed_n)
            )
        }
    }

    # Summary statistics (grouped)
    vision_changes <- summary_data %>%
        group_by(treatment_group) %>%
        summarise(
            n = n(),
            mean_change = mean(vision_change, na.rm = TRUE),
            sd_change = sd(vision_change, na.rm = TRUE),
            median_change = median(vision_change, na.rm = TRUE),
            iqr_change = IQR(vision_change, na.rm = TRUE),
            .groups = "drop"
        )

    line_change_distribution <- tibble()
    line_change_bucket_distribution <- tibble()

    if (length(line_levels) > 0) {
        level_lookup <- tibble(
            vision_line_change_label = factor(line_levels, levels = line_levels, ordered = TRUE),
            line_change_lines = line_values
        )

        by_group <- summary_data %>%
            filter(!is.na(vision_line_change_label)) %>%
            count(treatment_group, vision_line_change_label, name = "count") %>%
            tidyr::complete(
                treatment_group,
                vision_line_change_label = level_lookup$vision_line_change_label,
                fill = list(count = 0)
            ) %>%
            group_by(treatment_group) %>%
            mutate(
                total = sum(count),
                percent = dplyr::if_else(total > 0, round(100 * count / total, 1), NA_real_)
            ) %>%
            ungroup()

        overall_distribution <- summary_data %>%
            filter(!is.na(vision_line_change_label)) %>%
            mutate(treatment_group = factor("Overall", levels = "Overall")) %>%
            count(treatment_group, vision_line_change_label, name = "count") %>%
            tidyr::complete(
                treatment_group,
                vision_line_change_label = level_lookup$vision_line_change_label,
                fill = list(count = 0)
            ) %>%
            group_by(treatment_group) %>%
            mutate(
                total = sum(count),
                percent = dplyr::if_else(total > 0, round(100 * count / total, 1), NA_real_)
            ) %>%
            ungroup()

        line_change_distribution <- bind_rows(by_group, overall_distribution) %>%
            left_join(level_lookup, by = "vision_line_change_label") %>%
            arrange(line_change_lines)
    }

    if (!all(is.na(summary_data$vision_line_change_bucket))) {
        bucket_counts <- summary_data %>%
            filter(!is.na(vision_line_change_bucket)) %>%
            count(treatment_group, vision_line_change_bucket, name = "count") %>%
            tidyr::complete(
                treatment_group,
                vision_line_change_bucket = factor(VISION_LINE_CHANGE_CATEGORY_LEVELS, levels = VISION_LINE_CHANGE_CATEGORY_LEVELS, ordered = TRUE),
                fill = list(count = 0)
            ) %>%
            group_by(treatment_group) %>%
            mutate(
                total = sum(count),
                percent = dplyr::if_else(total > 0, round(100 * count / total, 1), NA_real_)
            ) %>%
            ungroup()

        overall_bucket_counts <- summary_data %>%
            filter(!is.na(vision_line_change_bucket)) %>%
            mutate(treatment_group = factor("Overall", levels = "Overall")) %>%
            count(treatment_group, vision_line_change_bucket, name = "count") %>%
            tidyr::complete(
                treatment_group,
                vision_line_change_bucket = factor(VISION_LINE_CHANGE_CATEGORY_LEVELS, levels = VISION_LINE_CHANGE_CATEGORY_LEVELS, ordered = TRUE),
                fill = list(count = 0)
            ) %>%
            group_by(treatment_group) %>%
            mutate(
                total = sum(count),
                percent = dplyr::if_else(total > 0, round(100 * count / total, 1), NA_real_)
            ) %>%
            ungroup()

        line_change_bucket_distribution <- bind_rows(bucket_counts, overall_bucket_counts)
    }

    logmar_p_value <- safe_wilcox_p_value(summary_data, "vision_change")

    # Table for publication (row-level input)
    tbl_summary_obj <- summary_data %>%
        select(treatment_group, vision_change) %>%
        tbl_summary(
            missing = "no",
            by = treatment_group,
            type = list(vision_change ~ "continuous"),
            statistic = list(vision_change ~ "{median} ({min}, {max}); mean {mean}"),
            digits = list(all_continuous() ~ 1, all_categorical() ~ 0),
            label = list(vision_change ~ "Vision Change (logMAR)")
        ) %>%
        safe_add_p_to_summary(
            context_label = "LogMAR vision summary",
            test = list(
                all_continuous() ~ "wilcox.test"
            )
        ) %>%
        add_overall() %>%
        bold_labels() %>% # Built-in gtsummary function for bold variable labels!
        modify_header(
            label = "**Characteristic**",
            stat_0 = "**Overall**\nN = {N}"
        ) %>%
        modify_caption("Vision change (logMAR)")


    line_change_bucket_tbl <- NULL
    line_change_tbl <- NULL
    if (length(line_levels) > 0) {
        line_change_tbl <- with_preserved_rng_seed(
            OBJECTIVE2_SIMULATED_FISHER_SEED,
            summary_data %>%
                filter(!is.na(vision_line_change_label)) %>%
                select(treatment_group, vision_line_change_label) %>%
                tbl_summary(
                    missing = "no",
                    by = treatment_group,
                    type = list(vision_line_change_label ~ "categorical"),
                    statistic = list(all_categorical() ~ "{n} ({p}%)"),
                    digits = list(all_categorical() ~ 1),
                    label = list(vision_line_change_label ~ "Snellen Line Change Integer Distribution")
                ) %>%
                safe_add_p_to_summary(
                    context_label = "Snellen integer line-change summary",
                    test = list(
                        all_categorical() ~ "fisher.test"
                    ),
                    test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))
                )
            ) %>%
            add_overall() %>%
            format_count_percent_columns() %>%
            bold_labels() %>%
            modify_header(
                label = "**Snellen Line Change**",
                stat_0 = "**Overall**\nN = {N}",
                stat_1 = "**PBT**\nN = {n}",
                stat_2 = "**GKSRS**\nN = {n}",
                p.value = "**p-value**"
            ) %>%
            modify_caption("Snellen Line Change Integer Distribution")
    }

    if (!all(is.na(summary_data$vision_line_change_bucket))) {
        line_change_bucket_tbl <- with_preserved_rng_seed(
            OBJECTIVE2_SIMULATED_FISHER_SEED,
            summary_data %>%
                filter(!is.na(vision_line_change_bucket)) %>%
                select(treatment_group, vision_line_change_bucket) %>%
                tbl_summary(
                    missing = "no",
                    by = treatment_group,
                    type = list(vision_line_change_bucket ~ "categorical"),
                    statistic = list(all_categorical() ~ "{n} ({p}%)"),
                    digits = list(all_categorical() ~ 1),
                    label = list(vision_line_change_bucket ~ "Snellen Line Change Distribution")
                ) %>%
                safe_add_p_to_summary(
                    context_label = "Snellen bucket line-change summary",
                    test = list(
                        all_categorical() ~ "fisher.test"
                    ),
                    test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))
                )
            ) %>%
            add_overall() %>%
            format_count_percent_columns() %>%
            bold_labels() %>%
            modify_header(
                label = "**Snellen Line Change Distribution**",
                stat_0 = "**Overall**\nN = {N}",
                stat_1 = "**PBT**\nN = {n}",
                stat_2 = "**GKSRS**\nN = {n}",
                p.value = "**p-value**"
            ) %>%
            modify_caption("Snellen Line Change Distribution")
    }

    line_change_summary_tbl <- summary_data %>%
        select(treatment_group, vision_change) %>%
        tbl_summary(
            missing = "no",
            by = treatment_group,
            type = list(vision_change ~ "continuous"),
            statistic = list(vision_change ~ "{median} ({min}, {max}); mean {mean}"),
            digits = list(vision_change ~ 1),
            label = list(vision_change ~ "Vision Change (logMAR)")
        ) %>%
        safe_add_p_to_summary(
            context_label = "Snellen line-change continuous summary",
            test = list(
                all_continuous() ~ "wilcox.test"
            )
        ) %>%
        add_overall() %>%
        bold_labels() %>%
        modify_header(
            label = "**Characteristic**",
            stat_0 = "**Overall**\nN = {N}"
        ) %>%
        convert_logmar_summary_table_to_line_summary(
            label = "Snellen Line Change",
            caption = "Snellen Line Change Summary"
        )

    # Save tables
    stacked_tbls <- Filter(
        Negate(is.null),
        list(tbl_summary_obj, line_change_summary_tbl, line_change_bucket_tbl)
    )

    combined_tbl <- quiet_tbl_stack(tbls = stacked_tbls) %>%
        modify_caption("Vision changes overview") %>%
        modify_table_styling(
            columns = "p.value",
            rows = .data$row_type == "label",
            footnote = NA_character_
        ) %>%
        modify_table_styling(
            columns = "p.value",
            footnote = "Wilcoxon rank-sum test for continuous rows; Fisher's exact test (simulated p-value) for categorical rows."
        )

    save_gt_html(
        combined_tbl,
        filename = file.path(vision_descriptive_dir, paste0(prefix, "vision_changes.html"))
    )

    if (nrow(line_change_distribution) > 0) {
        write_readable_xlsx(
            line_change_distribution,
            path = file.path(vision_descriptive_dir, paste0(prefix, "snellen_line_change_integer_distribution.xlsx"))
        )
    }

    if (nrow(line_change_bucket_distribution) > 0) {
        write_readable_xlsx(
            line_change_bucket_distribution,
            path = file.path(vision_descriptive_dir, paste0(prefix, "snellen_line_change_distribution_summary.xlsx"))
        )
    }

    snellen_section_tbls <- Filter(
        Negate(is.null),
        list(line_change_summary_tbl, line_change_bucket_tbl, line_change_tbl)
    )

    if (length(snellen_section_tbls) > 0) {
        snellen_combo_tbl <- quiet_tbl_stack(snellen_section_tbls) %>%
            modify_caption("Snellen Line Change Descriptive Summary")

        save_gt_html(
            snellen_combo_tbl,
            filename = file.path(vision_descriptive_dir, paste0(prefix, "snellen_line_change_descriptive_summary.html"))
        )
    }

    # Linear regression model
    logger::log_info("Fitting linear regression model for vision changes")

    # Use the unified table generation system for linear regression
    # Use the same standardized confounders as all other analyses
    vision_result <- generate_regression_table(
        data = vision_model_data,
        outcome_var = "vision_change",
        predictor_vars = "treatment_group",
        confounders = confounders_for_model,
        model_type = "linear",
        effect_measure = "MD", # Mean Difference for continuous outcome
        analysis_name = "logmar_vision_change_adjusted",
        dataset_name = dataset_name %||% "vision_safety",
        output_dir = vision_adjusted_dir,
        prefix = prefix,
        sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
        filter_stats = exclusion_result$filter_stats
    )

    vision_lm <- vision_result$model
    vision_lm_tbl <- vision_result$table

    visual_predictor_screen <- screen_visual_reviewer_predictors(data_with_vision_change)
    latest_va_sensitivity_covariates <- unique(c(
        visual_predictor_screen$selected_terms,
        confounders_for_model
    ))
    latest_va_sensitivity_covariates <- latest_va_sensitivity_covariates[
        latest_va_sensitivity_covariates %in% names(data_with_vision_change)
    ]
    latest_va_sensitivity_vars <- unique(c("treatment_group", latest_va_sensitivity_covariates))
    latest_va_required_vars <- unique(c("last_vision", latest_va_sensitivity_vars))
    latest_va_initial_n <- nrow(data_with_vision_change)
    latest_va_complete_data <- data_with_vision_change %>%
        filter(if_all(all_of(latest_va_required_vars), ~ !is.na(.x)))
    latest_va_missing_removed_n <- latest_va_initial_n - nrow(latest_va_complete_data)
    latest_va_exclusion_result <- apply_sparse_level_exclusions(
        latest_va_complete_data,
        variables = latest_va_sensitivity_vars[latest_va_sensitivity_vars %in% names(data_with_vision_change)],
        analysis_name = "latest_vision_reviewer_predictor_sensitivity",
        id_col = pick_sparse_level_id_col(data_with_vision_change),
        level_exclusions = MODELING_LEVEL_EXCLUSIONS
    )
    latest_va_model_data <- latest_va_exclusion_result$data
    latest_va_rank_screen <- select_rank_supported_linear_terms(
        data = latest_va_model_data,
        outcome_var = "last_vision",
        predictor_terms = "treatment_group",
        candidate_terms = latest_va_sensitivity_covariates
    )
    if (nrow(latest_va_rank_screen$dropped_terms) > 0) {
        dropped_terms <- latest_va_rank_screen$dropped_terms
        visual_predictor_screen$availability <- visual_predictor_screen$availability %>%
            left_join(dropped_terms, by = "candidate_term", suffix = c("", ".rank")) %>%
            mutate(
                included_in_latest_va_model = if_else(
                    !is.na(.data$exclusion_reason.rank),
                    FALSE,
                    .data$included_in_latest_va_model
                ),
                exclusion_reason = coalesce(.data$exclusion_reason.rank, .data$exclusion_reason)
            ) %>%
            select(-"exclusion_reason.rank")
    }
    latest_va_sensitivity_covariates <- latest_va_rank_screen$supported_terms
    latest_va_sensitivity_vars <- unique(c("treatment_group", latest_va_sensitivity_covariates))
    latest_va_filter_stats <- latest_va_exclusion_result$filter_stats
    latest_va_filter_stats$initial_n <- latest_va_initial_n
    latest_va_filter_stats$model_n <- nrow(latest_va_model_data)
    latest_va_filter_stats$removed_n <- latest_va_initial_n - nrow(latest_va_model_data)
    latest_va_filter_stats$removed_pct <- if (latest_va_initial_n > 0) {
        round(100 * latest_va_filter_stats$removed_n / latest_va_initial_n, 1)
    } else {
        0
    }
    latest_va_filter_stats$removal_reason <- paste(
        sprintf(
            "Removed rows missing latest-VA sensitivity outcome or covariates before modeling (n=%d)",
            latest_va_missing_removed_n
        ),
        sprintf(
            "excluded sparse categorical levels after complete-case filtering (n=%d)",
            latest_va_exclusion_result$removed_row_count
        ),
        sep = "; "
    )
    latest_va_sensitivity_note <- paste(
        "ANCOVA-style reviewer-response sensitivity: latest logMAR visual acuity is modeled with treatment group, viable reviewer-requested baseline predictors, explicit latest-VA follow-up duration, and the standard confounder set.",
        "This avoids adding baseline visual acuity as an ordinary covariate to a change-score outcome that already contains baseline visual acuity.",
        "The accompanying reviewer_predictor_availability sheet documents unavailable and non-viable requested fields."
    )
    latest_va_sensitivity_result <- generate_regression_table(
        data = latest_va_model_data,
        outcome_var = "last_vision",
        predictor_vars = "treatment_group",
        confounders = latest_va_sensitivity_covariates,
        model_type = "linear",
        effect_measure = "MD",
        analysis_name = "va_latest_reviewer_sens",
        dataset_name = dataset_name %||% "vision_safety",
        output_dir = vision_sensitivity_dir,
        prefix = prefix,
        sparse_level_diagnostics = latest_va_exclusion_result$sparse_level_diagnostics,
        filter_stats = latest_va_filter_stats
    )
    latest_va_sensitivity_model <- latest_va_sensitivity_result$model

    line_change_result <- generate_regression_table(
        data = line_change_model_data,
        outcome_var = "vision_line_change",
        predictor_vars = "treatment_group",
        confounders = confounders_for_model,
        model_type = "linear",
        effect_measure = "MD",
        analysis_name = "snellen_line_change_adjusted",
        dataset_name = dataset_name %||% "vision_safety",
        output_dir = vision_adjusted_dir,
        prefix = prefix,
        sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
        filter_stats = line_change_filter_stats
    )

    line_change_lm <- line_change_result$model
    line_change_lm_tbl <- line_change_result$table

    line_change_ordinal_result <- generate_regression_table(
        data = ordinal_model_data,
        outcome_var = "vision_line_change_bucket",
        predictor_vars = "treatment_group",
        confounders = confounders_for_model,
        model_type = "ordinal",
        effect_measure = "OR",
        analysis_name = "snellen_line_change_distribution_adjusted",
        dataset_name = dataset_name %||% "vision_safety",
        output_dir = vision_adjusted_dir,
        prefix = prefix,
        sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
        filter_stats = ordinal_filter_stats
    )

    line_change_ordinal_model <- line_change_ordinal_result$model
    line_change_ordinal_tbl <- line_change_ordinal_result$table

    logmar_summary <- build_grouped_continuous_summary(summary_data, "vision_change", digits = 1)

    snellen_summary_strings <- convert_logmar_summary_stat_to_line_summary(unname(logmar_summary$display_stats))
    names(snellen_summary_strings) <- names(logmar_summary$display_stats)
    snellen_overall_estimate <- if (is.na(logmar_summary$overall_estimate)) {
        NA_real_
    } else {
        compute_line_change_lines(logmar_summary$overall_estimate)
    }

    logmar_unadjusted_model <- fit_regression_model(
        data = vision_model_data,
        formula = build_model_formula("vision_change", "treatment_group", character(), "linear"),
        model_type = "linear"
    )
    snellen_line_unadjusted_model <- fit_regression_model(
        data = line_change_model_data,
        formula = build_model_formula("vision_line_change", "treatment_group", character(), "linear"),
        model_type = "linear"
    )
    snellen_distribution_unadjusted_model <- fit_regression_model(
        data = ordinal_model_data,
        formula = build_model_formula("vision_line_change_bucket", "treatment_group", character(), "ordinal"),
        model_type = "ordinal"
    )

    vision_effect_summary <- bind_effect_summary_rows(
        create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "LogMAR Vision Change",
            model_label = "Descriptive",
            term = "summary",
            model_formula = "Descriptive summary",
            covariates_used = "None",
            effect_measure = "Median (Min, Max); Mean",
            estimate = logmar_summary$overall_estimate,
            n_patients = nrow(summary_data),
            n_outcome_non_missing = logmar_summary$n_outcome_non_missing,
            data_source = "Displayed descriptive summary",
            model_status = "DESCRIPTIVE",
            notes = build_summary_note(logmar_summary$display_stats, logmar_p_value, suffix = vision_change_contract_note)
        ),
        summarize_effect_model(
            model = logmar_unadjusted_model,
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "LogMAR Vision Change",
            model_label = "Unadjusted linear",
            group_var = "treatment_group",
            data_source_label = "Filtered vision-change dataset without covariates",
            effect_measure = "MD",
            outcome_var = "vision_change",
            notes = vision_change_contract_note
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "LogMAR Vision Change",
            model_label = "Unadjusted linear",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula("vision_change", "treatment_group"),
            covariates_used = "None",
            effect_measure = "MD",
            n_patients = nrow(vision_model_data),
            n_outcome_non_missing = sum(!is.na(vision_model_data$vision_change)),
            data_source = "Filtered vision-change dataset without covariates",
            model_status = "SKIPPED",
            notes = paste("Unadjusted linear model could not be fit.", vision_change_contract_note)
        ),
        summarize_effect_model(
            model = vision_lm,
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "LogMAR Vision Change",
            model_label = "Adjusted linear",
            group_var = "treatment_group",
            data_source_label = "Filtered vision-change dataset with confounders",
            effect_measure = "MD",
            outcome_var = "vision_change",
            notes = vision_change_contract_note
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "LogMAR Vision Change",
            model_label = "Adjusted linear",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula("vision_change", "treatment_group", confounders_for_model),
            covariates_used = format_effect_summary_covariates(confounders_for_model),
            effect_measure = "MD",
            n_patients = nrow(vision_model_data),
            n_outcome_non_missing = sum(!is.na(vision_model_data$vision_change)),
            data_source = "Filtered vision-change dataset with confounders",
            model_status = "SKIPPED",
            notes = paste(as.character(vision_result$diagnostics$raw_model_output %||% "Adjusted linear model could not be fit."), vision_change_contract_note)
        ),
        summarize_effect_model(
            model = latest_va_sensitivity_model,
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Latest LogMAR Vision",
            model_label = "Reviewer-predictor adjusted linear sensitivity",
            group_var = "treatment_group",
            data_source_label = "Filtered latest-VA dataset with viable reviewer-requested predictors and standard confounders",
            effect_measure = "MD",
            outcome_var = "last_vision",
            notes = latest_va_sensitivity_note
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Latest LogMAR Vision",
            model_label = "Reviewer-predictor adjusted linear sensitivity",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula("last_vision", "treatment_group", latest_va_sensitivity_covariates),
            covariates_used = format_effect_summary_covariates(latest_va_sensitivity_covariates),
            effect_measure = "MD",
            n_patients = nrow(latest_va_model_data),
            n_outcome_non_missing = sum(!is.na(latest_va_model_data$last_vision)),
            data_source = "Filtered latest-VA dataset with viable reviewer-requested predictors and standard confounders",
            model_status = "SKIPPED",
            notes = paste(as.character(latest_va_sensitivity_result$diagnostics$raw_model_output %||% "Latest-VA reviewer-predictor sensitivity model could not be fit."), latest_va_sensitivity_note)
        ),
        create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change",
            model_label = "Descriptive",
            term = "summary",
            model_formula = "Descriptive summary",
            covariates_used = "None",
            effect_measure = "Median (Min, Max); Mean",
            estimate = snellen_overall_estimate,
            n_patients = nrow(summary_data),
            n_outcome_non_missing = logmar_summary$n_outcome_non_missing,
            data_source = "Displayed descriptive summary converted from logMAR",
            model_status = "DESCRIPTIVE",
            notes = build_summary_note(snellen_summary_strings, logmar_p_value, suffix = vision_change_contract_note)
        ),
        summarize_effect_model(
            model = snellen_line_unadjusted_model,
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change",
            model_label = "Unadjusted linear",
            group_var = "treatment_group",
            data_source_label = "Filtered Snellen line-change dataset without covariates",
            effect_measure = "MD",
            outcome_var = "vision_line_change",
            notes = vision_change_contract_note
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change",
            model_label = "Unadjusted linear",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula("vision_line_change", "treatment_group"),
            covariates_used = "None",
            effect_measure = "MD",
            n_patients = nrow(line_change_model_data),
            n_outcome_non_missing = sum(!is.na(line_change_model_data$vision_line_change)),
            data_source = "Filtered Snellen line-change dataset without covariates",
            model_status = "SKIPPED",
            notes = paste("Unadjusted Snellen line-change model could not be fit.", vision_change_contract_note)
        ),
        summarize_effect_model(
            model = line_change_lm,
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change",
            model_label = "Adjusted linear",
            group_var = "treatment_group",
            data_source_label = "Filtered Snellen line-change dataset with confounders",
            effect_measure = "MD",
            outcome_var = "vision_line_change",
            notes = vision_change_contract_note
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change",
            model_label = "Adjusted linear",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula("vision_line_change", "treatment_group", confounders_for_model),
            covariates_used = format_effect_summary_covariates(confounders_for_model),
            effect_measure = "MD",
            n_patients = nrow(line_change_model_data),
            n_outcome_non_missing = sum(!is.na(line_change_model_data$vision_line_change)),
            data_source = "Filtered Snellen line-change dataset with confounders",
            model_status = "SKIPPED",
            notes = paste(as.character(line_change_result$diagnostics$raw_model_output %||% "Adjusted Snellen line-change model could not be fit."), vision_change_contract_note)
        ),
        create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change Distribution",
            model_label = "Descriptive",
            term = "summary",
            model_formula = "Descriptive summary",
            covariates_used = "None",
            effect_measure = "Distribution",
            n_patients = nrow(summary_data),
            n_outcome_non_missing = sum(!is.na(summary_data$vision_line_change_bucket)),
            data_source = "Displayed categorical distribution summary",
            model_status = "DESCRIPTIVE",
            notes = build_distribution_note(
                summary_data,
                category_var = "vision_line_change_bucket",
                detail_file_label = paste0(prefix, "snellen_line_change_distribution_summary.xlsx"),
                suffix = ordinal_assumption_note
            )
        ),
        summarize_effect_model(
            model = snellen_distribution_unadjusted_model,
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change Distribution",
            model_label = "Unadjusted ordinal logistic",
            group_var = "treatment_group",
            data_source_label = "Filtered Snellen line-change distribution dataset without covariates",
            effect_measure = "OR",
            outcome_var = "vision_line_change_bucket",
            notes = ordinal_assumption_note
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change Distribution",
            model_label = "Unadjusted ordinal logistic",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula("vision_line_change_bucket", "treatment_group"),
            covariates_used = "None",
            effect_measure = "OR",
            n_patients = nrow(ordinal_model_data),
            n_outcome_non_missing = sum(!is.na(ordinal_model_data$vision_line_change_bucket)),
            data_source = "Filtered Snellen line-change distribution dataset without covariates",
            model_status = "SKIPPED",
            notes = paste("Unadjusted ordinal Snellen distribution model could not be fit.", ordinal_assumption_note)
        ),
        summarize_effect_model(
            model = line_change_ordinal_model,
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change Distribution",
            model_label = "Adjusted ordinal logistic",
            group_var = "treatment_group",
            data_source_label = "Filtered Snellen line-change distribution dataset with confounders",
            effect_measure = "OR",
            outcome_var = "vision_line_change_bucket",
            notes = ordinal_assumption_note
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "vision_safety",
            analysis_label = "Snellen Line Change Distribution",
            model_label = "Adjusted ordinal logistic",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula("vision_line_change_bucket", "treatment_group", confounders_for_model),
            covariates_used = format_effect_summary_covariates(confounders_for_model),
            effect_measure = "OR",
            n_patients = nrow(ordinal_model_data),
            n_outcome_non_missing = sum(!is.na(ordinal_model_data$vision_line_change_bucket)),
            data_source = "Filtered Snellen line-change distribution dataset with confounders",
            model_status = "SKIPPED",
            notes = paste(as.character(line_change_ordinal_result$diagnostics$raw_model_output %||% "Adjusted ordinal Snellen distribution model could not be fit."), ordinal_assumption_note)
        )
    )

    write_effect_summary_workbook(
        effect_summary_rows = vision_effect_summary,
        output_dir = vision_effect_summary_dir,
        prefix = prefix,
        analysis_name = "vision"
    )

    minimum_followup_thresholds <- c(12, 36, 60)
    followup_timing_definitions <- list(
        explicit = list(
            timing_var = "last_vision_followup_months_explicit",
            sheet_prefix = "explicit_min_followup",
            file_prefix = "explicit",
            label = "Explicit treatment-to-last_followup timing"
        ),
        proxy = list(
            timing_var = "last_vision_followup_months_proxy",
            sheet_prefix = "proxy_min_followup",
            file_prefix = "proxy",
            label = "Proxy timing using explicit last_followup when available, otherwise general follow_up_months"
        )
    )
    visual_followup_sensitivities <- list()
    visual_followup_models <- list()
    visual_followup_model_status <- list()

    for (timing_name in names(followup_timing_definitions)) {
        timing_definition <- followup_timing_definitions[[timing_name]]
        for (threshold_months in minimum_followup_thresholds) {
            threshold_label <- paste0(threshold_months, "mo")
            sensitivity_key <- paste(timing_name, threshold_label, sep = "_")
            threshold_slug <- sprintf("va_minfu_%s_%s", timing_definition$file_prefix, threshold_label)
            visual_followup_sensitivities[[sensitivity_key]] <- build_visual_acuity_min_followup_sensitivity(
                data,
                min_followup_months = threshold_months,
                timing_var = timing_definition$timing_var
            )
            threshold_model_data <- visual_followup_sensitivities[[sensitivity_key]]$data %>%
                enforce_unordered_factors()
            visual_followup_models[[sensitivity_key]] <- if (
                nrow(threshold_model_data) > 0 &&
                    dplyr::n_distinct(stats::na.omit(threshold_model_data$treatment_group)) >= 2
            ) {
                generate_regression_table(
                    data = threshold_model_data,
                    outcome_var = "vision_change",
                    predictor_vars = "treatment_group",
                    confounders = confounders_for_model,
                    model_type = "linear",
                    effect_measure = "MD",
                    analysis_name = threshold_slug,
                    dataset_name = dataset_name %||% "vision_followup_sensitivity",
                    output_dir = vision_sensitivity_dir,
                    prefix = prefix
                )
            } else {
                list(
                    table = NULL,
                    model = NULL,
                    diagnostics = tibble::tibble(
                        status = "skipped",
                        reason = sprintf(
                            "Minimum-follow-up visual-acuity treatment-effect model skipped for the %s %d-month threshold because the subset did not retain enough treatment-group support.",
                            timing_name,
                            threshold_months
                        )
                    )
                )
            }
            visual_followup_model_status[[sensitivity_key]] <- tibble::tibble(
                timing_surface = timing_name,
                min_followup_months = threshold_months,
                model_status = ifelse(is.null(visual_followup_models[[sensitivity_key]]$model), "skipped", "completed"),
                model = "vision_change ~ treatment_group + confounders",
                subset = paste0(timing_definition$timing_var, " >= ", threshold_months),
                timing_definition = timing_definition$label,
                threshold_rationale = "Minimum-follow-up latest-visual-acuity sensitivity using 1-, 3-, and 5-year durations as cutoff options; these are not fixed-landmark VA measurements."
            )
        }
    }

    visual_followup_summary_sheets <- purrr::imap(
        visual_followup_sensitivities,
        function(sensitivity, sensitivity_key) sensitivity$summary
    )
    names(visual_followup_summary_sheets) <- vapply(names(visual_followup_summary_sheets), function(sensitivity_key) {
        key_parts <- strsplit(sensitivity_key, "_", fixed = TRUE)[[1]]
        timing_definition <- followup_timing_definitions[[key_parts[[1]]]]
        paste(timing_definition$sheet_prefix, key_parts[[2]], sep = "_")
    }, character(1))

    visual_followup_workbook <- c(
        visual_followup_summary_sheets,
        list(
            available_explicit_va_timing = summarize_vision_followup_by_group(data, "last_vision_followup_months_explicit"),
            available_proxy_va_timing = summarize_vision_followup_by_group(data, "last_vision_followup_months_proxy"),
            timing_source_counts = summarize_vision_followup_timing_sources(data),
            treatment_effect_model = dplyr::bind_rows(visual_followup_model_status),
            latest_va_reviewer_model = tibble::tibble(
                model_status = ifelse(is.null(latest_va_sensitivity_model), "skipped", "completed"),
                model = build_effect_summary_model_formula("last_vision", "treatment_group", latest_va_sensitivity_covariates),
                covariates_used = format_effect_summary_covariates(latest_va_sensitivity_covariates),
                modeled_n = if (is.null(latest_va_sensitivity_model)) nrow(latest_va_model_data) else stats::nobs(latest_va_sensitivity_model),
                sensitivity_rationale = latest_va_sensitivity_note
            ),
            reviewer_predictor_availability = visual_predictor_screen$availability,
            toxicity_scope = objective2_toxicity_scope_note(),
            limitation = tibble::tibble(
                note = "The primary latest-VA minimum-follow-up sensitivity uses treatment-to-last_followup timing when that date is recorded. A separate proxy surface uses the derived general follow_up_months field when last_followup is missing. Tumor-height timing is not used as the VA timing proxy. The 12-, 36-, and 60-month sensitivity analyses are minimum-follow-up restrictions on latest VA, not standardized 1-, 3-, or 5-year landmark VA analyses."
            )
        )
    )

    write_readable_xlsx(
        visual_followup_workbook,
        file.path(vision_sensitivity_dir, paste0(prefix, "vision_followup_sensitivity.xlsx"))
    )

    # Note: Table formatting and saving are now handled by the unified table generation system

    return(list(
        changes = vision_changes,
        table = tbl_summary_obj,
        line_change_distribution = line_change_distribution,
        line_change_bucket_distribution = line_change_bucket_distribution,
        line_change_table = line_change_tbl,
        line_change_bucket_table = line_change_bucket_tbl,
        line_change_summary_table = line_change_summary_tbl,
        regression_model = vision_lm,
        regression_table = vision_lm_tbl,
        latest_va_sensitivity_model = latest_va_sensitivity_model,
        latest_va_predictor_screen = visual_predictor_screen,
        line_change_regression_model = line_change_lm,
        line_change_regression_table = line_change_lm_tbl,
        line_change_regression_diagnostics = line_change_result$diagnostics,
        line_change_bucket_regression_model = line_change_ordinal_model,
        line_change_bucket_regression_table = line_change_ordinal_tbl,
        line_change_bucket_regression_diagnostics = line_change_ordinal_result$diagnostics,
        visual_followup_sensitivity = visual_followup_sensitivities,
        visual_followup_model = visual_followup_models,
        effect_summary = vision_effect_summary
    ))
}

#' Build skip diagnostics for sparse binary outcome models
#'
#' Creates a compact summary of why an adjusted binary outcome model was not fit,
#' including the total number of modeled events, sparse-level exclusions, and
#' outcome counts within each modeled covariate level. Levels with zero events or
#' all events are flagged because they indicate separation risk if the model were
#' forced.
#'
#' @param data Data frame used for the adjusted model after exclusions.
#' @param outcome_var Character scalar naming the binary outcome column encoded as 0/1.
#' @param variables Character vector of modeled variables to summarize.
#' @param minimum_events Integer minimum number of events required to attempt fitting
#'   (defaults to `MINIMUM_ADJUSTED_LOGISTIC_EVENTS`).
#' @param sparse_level_diagnostics Optional data frame of rows removed before modeling.
#' @param analysis_name Character scalar analysis identifier.
#' @param dataset_name Character scalar dataset identifier.
#'
#' @return Named list compatible with the shared skip-report renderer.
build_binary_skip_diagnostics <- function(data,
                                          outcome_var,
                                          variables,
                                          minimum_events = MINIMUM_ADJUSTED_LOGISTIC_EVENTS,
                                          sparse_level_diagnostics = NULL,
                                          analysis_name = "analysis",
                                          dataset_name = "unspecified_dataset") {
    modeled_n <- nrow(data)
    modeled_events <- sum(data[[outcome_var]] == 1, na.rm = TRUE)
    modeled_nonevents <- sum(data[[outcome_var]] == 0, na.rm = TRUE)
    event_support <- build_level_support_tab(data, variables, outcome_var = outcome_var)

    sparse_exclusion_summary <- if (is.null(sparse_level_diagnostics) || nrow(sparse_level_diagnostics) == 0) {
        "None"
    } else {
        sparse_level_diagnostics %>%
            dplyr::mutate(level_label = paste0(variable, "=", level, " (n=", observed_n, ")")) %>%
            dplyr::pull(level_label) %>%
            paste(collapse = "; ")
    }

    flagged_levels <- if (is.null(event_support) || nrow(event_support) == 0) {
        character()
    } else {
        event_support %>%
            dplyr::filter(support_flag != "usable") %>%
            dplyr::mutate(level_label = paste0(variable, "=", level, " [", support_flag, "]")) %>%
            dplyr::pull(level_label)
    }

    narrative_lines <- c(
        sprintf(
            "Adjusted model not attempted because only %d outcome events remained in %d modeled patients after exclusions; the pipeline requires at least %d events for adjusted logistic regression.",
            modeled_events,
            modeled_n,
            minimum_events
        ),
        sprintf(
            "Modeled data contained %d non-events and %d events.",
            modeled_nonevents,
            modeled_events
        ),
        sprintf(
            "Sparse-level exclusions before modeling: %s.",
            sparse_exclusion_summary
        )
    )

    if (length(flagged_levels) > 0) {
        narrative_lines <- c(
            narrative_lines,
            sprintf(
                "If the model were forced, these covariate levels show separation risk because all observed outcomes fall in one category: %s.",
                paste(flagged_levels, collapse = "; ")
            )
        )
    }

    build_skip_report_diagnostics(
        status = "skipped",
        analysis_name = analysis_name,
        dataset_name = dataset_name,
        reason = paste(narrative_lines, collapse = " "),
        narrative_lines = narrative_lines,
        skip_summary = build_skip_summary_tab(list(
            modeled_n = modeled_n,
            modeled_events = modeled_events,
            modeled_non_events = modeled_nonevents,
            minimum_events_required = minimum_events,
            events_shortfall = max(minimum_events - modeled_events, 0),
            sparse_exclusions = sparse_exclusion_summary,
            separation_risk_levels = if (length(flagged_levels) > 0) {
                paste(flagged_levels, collapse = "; ")
            } else {
                "None detected"
            }
        )),
        sparse_level_diagnostics = sparse_level_diagnostics,
        event_support = event_support,
        raw_model_output = sprintf(
            "Model skipped: only %d events available after sparse-level exclusions.",
            modeled_events
        )
    )
}

#' Analyze radiation complications
#'
#' Analyze rates of radiation complications (retinopathy, nvg, srd) by treatment group.
#' This function reuses the existing analyze_binary_outcome_rates function for consistency.
#'
#' @param data A data frame containing radiation sequelae variables.
#' @param sequela_type Character. The type of sequela to analyze. Must be one of "retinopathy", "nvg", or "srd".
#' @param confounders Character vector of confounders to adjust for in the analysis. Default is NULL.
#' @param dataset_name Character. Name of the dataset for output files. Default is NULL.
#' @param output_dirs List of output directories organized by analysis type (recurrence, mets, os, pfs, height, subgroups, etc.).
#' @param prefix Character string used as a file prefix for output files (e.g., "full_cohort_"). Used to identify cohort or analysis context in filenames.
#'
#' @return A list of results from analyze_binary_outcome_rates, including model output and summary tables.
#' @examples
#' analyze_radiation_complications(data, "retinopathy", confounders, "uveal_full", output_dirs, prefix)
analyze_radiation_complications <- function(data, sequela_type, confounders = NULL, dataset_name = NULL, output_dirs = NULL, prefix = NULL) {
    data <- normalize_treatment_group_data(data)
    # Validate sequela type
    valid_sequelae <- c("retinopathy", "nvg", "srd")
    if (!sequela_type %in% valid_sequelae) {
        stop(sprintf(
            "Invalid sequela_type '%s'. Must be one of: %s",
            sequela_type, paste(valid_sequelae, collapse = ", ")
        ))
    }
    sequela_label <- switch(sequela_type,
        retinopathy = "Retinopathy",
        nvg = "Neovascular Glaucoma",
        srd = "Serous Retinal Detachment"
    )
    outcome_var <- resolve_objective2_toxicity_burden_field(sequela_type)
    burden_estimand_note <- sprintf(
        "Recorded toxicity burden by available follow-up from Objective 0-validated field '%s'; not time-to-toxicity incidence.",
        outcome_var
    )

    collapse_binary_summary_to_cases <- function(tbl) {
        tbl %>%
            modify_table_body(function(body) {
                case_rows <- body %>%
                    filter(row_type == "level", label %in% c("Y", "Yes")) %>%
                    select(variable, dplyr::starts_with("stat_"), dplyr::any_of("p.value"))

                label_rows <- body %>%
                    filter(row_type == "label") %>%
                    left_join(case_rows, by = "variable", suffix = c("", "_cases")) %>%
                    mutate(
                        stat_0 = coalesce(stat_0_cases, stat_0),
                        stat_1 = coalesce(stat_1_cases, stat_1),
                        stat_2 = coalesce(stat_2_cases, stat_2),
                        p.value = coalesce(p.value_cases, p.value)
                    ) %>%
                    select(names(body))

                label_rows
            })
    }

    # Ensure consistent factor contrasts for modeling
    data <- enforce_unordered_factors(data)
    assert_valid_objective2_toxicity_burden_field(data, outcome_var, sequela_type)

    # Retain a copy without additional filtering for descriptive outputs
    summary_data <- data

    confounders_for_model <- if (is.null(confounders)) character() else confounders
    exclusion_vars <- unique(c("treatment_group", confounders_for_model))
    exclusion_result <- apply_sparse_level_exclusions(
        data,
        variables = exclusion_vars[exclusion_vars %in% names(data)],
        analysis_name = paste0(sequela_type, "_logistic"),
        id_col = pick_sparse_level_id_col(data),
        level_exclusions = MODELING_LEVEL_EXCLUSIONS
    )

    if (exclusion_result$removed_row_count > 0) {
        logger::log_info(sprintf(
            "Excluded %d rows with sparse categorical levels prior to %s analysis",
            exclusion_result$removed_row_count,
            sequela_type
        ))
    }

    model_data <- exclusion_result$data
    assert_valid_objective2_toxicity_burden_field(model_data, outcome_var, sequela_type)

    logger::log_info(sprintf("Analyzing %s rates (binary outcome)", toupper(sequela_type)))

    summary_rates_data <- summary_data

    # Calculate rates by treatment group
    sequela_rates <- summary_rates_data %>%
        group_by(treatment_group) %>%
        summarise(
            n_total = n(),
            n_events = sum(.data[[outcome_var]] == 1, na.rm = TRUE),
            rate_percent = round(100 * n_events / n_total, 1),
            .groups = "drop"
        ) %>%
        mutate(
            endpoint = sequela_label,
            analysis_field = outcome_var,
            estimand = "Recorded toxicity burden by available follow-up",
            notes = burden_estimand_note,
            .before = treatment_group
        )

    # Determine output directory
    output_dir <- switch(sequela_type,
        "retinopathy" = output_dirs$obj2_retinopathy,
        "nvg" = output_dirs$obj2_nvg,
        "srd" = output_dirs$obj2_srd
    )
    route_prefix <- switch(sequela_type,
        "retinopathy" = "obj2_retinopathy",
        "nvg" = "obj2_nvg",
        "srd" = "obj2_srd"
    )
    descriptive_dir <- resolve_route_output_dir(output_dirs, route_prefix, "descriptive")
    adjusted_dir <- resolve_route_output_dir(output_dirs, route_prefix, "adjusted_models")
    effect_summary_dir <- resolve_route_output_dir(output_dirs, route_prefix, "effect_summary")

    # Save rates summary
    write_readable_xlsx(
        sequela_rates,
        file.path(descriptive_dir, paste0(prefix, sequela_type, "_rates_summary.xlsx"))
    )

    display_outcome_var <- paste0(outcome_var, "_display")
    summary_table_data <- summary_data %>%
        mutate(
            !!display_outcome_var := factor(
                dplyr::if_else(.data[[outcome_var]] == 1, "Yes", "No"),
                levels = c("No", "Yes")
            )
        )
    summary_labels <- get_variable_labels()
    summary_labels[[display_outcome_var]] <- paste(sequela_label, "Recorded Burden")

    # Create summary table
    tbl_summary_obj <- with_preserved_rng_seed(
        OBJECTIVE2_SIMULATED_FISHER_SEED,
        summary_table_data %>%
            select(treatment_group, all_of(display_outcome_var)) %>%
            tbl_summary(
                by = treatment_group,
                missing = "no",
                label = summary_labels,
                statistic = list(
                    all_continuous() ~ "{median} ({min}, {max})",
                    all_categorical() ~ "{n} ({p}%)"
                ),
                digits = list(all_continuous() ~ 1, all_categorical() ~ 0)
            ) %>%
            add_overall() %>%
            safe_add_p_to_summary(
                context_label = paste(sequela_label, "recorded burden summary"),
                test = list(
                    all_categorical() ~ "fisher.test",
                    all_continuous() ~ "wilcox.test"
                ),
                test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))
            )
    ) %>%
        bold_labels() %>%
        modify_header(
            label = "**Characteristic**",
            stat_0 = "**Overall**\nN = {N}",
            stat_1 = "**PBT**\nN = {n}",
            stat_2 = "**GKSRS**\nN = {n}",
            p.value = "**p-value**"
        ) %>%
        modify_caption(paste("Recorded burden of", tools::toTitleCase(sequela_label), "by Treatment Group")) %>%
        collapse_binary_summary_to_cases()

    # Convert to gt table and save
    tbl <- tbl_summary_obj %>%
        as_gt() %>%
        tab_source_note(
            source_note = md(burden_estimand_note)
        )

    # Save summary table
    save_gt_html(
        tbl,
        filename = file.path(descriptive_dir, paste0(prefix, sequela_type, "_summary_table.html"))
    )

    # Fit logistic regression if there are enough events and confounders
    model_result <- NULL
    safety_diagnostics <- NULL
    regression_table <- NULL
    logistic_analysis_name <- paste0(sequela_type, "_logistic")
    if (sum(model_data[[outcome_var]] == 1, na.rm = TRUE) >= MINIMUM_ADJUSTED_LOGISTIC_EVENTS) {

        # Use the unified table generation system and centralized confounders.
        sequela_confounders <- confounders_for_model

        regression_result <- generate_regression_table(
            data = model_data,
            outcome_var = outcome_var,
            predictor_vars = "treatment_group",
            confounders = sequela_confounders,
            model_type = "logistic",
            effect_measure = "OR",
            analysis_name = logistic_analysis_name,
            dataset_name = dataset_name,
            output_dir = adjusted_dir,
            prefix = prefix,
            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
            filter_stats = exclusion_result$filter_stats
        )

        # Extract the model and table from the result
        model_result <- regression_result$model
        safety_diagnostics <- regression_result$diagnostics
        regression_table <- regression_result$table # Get the regression table
    } else {
        modeled_events <- sum(model_data[[outcome_var]] == 1, na.rm = TRUE)
        skip_diagnostics <- build_binary_skip_diagnostics(
            data = model_data,
            outcome_var = outcome_var,
            variables = unique(c("treatment_group", confounders_for_model)),
            minimum_events = MINIMUM_ADJUSTED_LOGISTIC_EVENTS,
            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
            analysis_name = logistic_analysis_name,
            dataset_name = dataset_name
        )
        logger::log_warn(sprintf("Insufficient events for regression modeling (%d events)", modeled_events))
        safety_diagnostics <- skip_diagnostics
        safety_diagnostics$sample_size_summary <- build_sample_size_summary_tab(
            filter_stats = exclusion_result$filter_stats,
            dataset_name = dataset_name,
            analysis_name = logistic_analysis_name,
            modeled_n = nrow(model_data)
        )

        save_skipped_model_outputs(
            analysis_name = logistic_analysis_name,
            dataset_name = dataset_name,
            output_dir = adjusted_dir,
            prefix = prefix,
            reason = skip_diagnostics$reason,
            diagnostics = safety_diagnostics
        )
    }

    # Note: Diagnostics are now handled by the unified table generation system

    unadjusted_model <- fit_regression_model(
        data = model_data,
        formula = build_model_formula(outcome_var, "treatment_group", character(), "logistic"),
        model_type = "logistic"
    )

    effect_summary_rows <- bind_effect_summary_rows(
        create_effect_summary_rows(
            dataset_name = dataset_name %||% "unspecified_dataset",
            analysis_label = sequela_label,
            model_label = "Descriptive",
            term = "summary",
            model_formula = "Descriptive summary",
            covariates_used = "None",
            effect_measure = "Rate (%)",
            estimate = if (nrow(sequela_rates) > 0) round(100 * sum(summary_rates_data[[outcome_var]] == 1, na.rm = TRUE) / nrow(summary_rates_data), 1) else NA_real_,
            n_patients = nrow(summary_rates_data),
            n_events = sum(summary_rates_data[[outcome_var]] == 1, na.rm = TRUE),
            n_outcome_non_missing = sum(!is.na(summary_rates_data[[outcome_var]])),
            data_source = "Displayed Objective 0-validated recorded-burden summary",
            model_status = "DESCRIPTIVE",
            notes = build_binary_rate_note(summary_rates_data, outcome_var, suffix = burden_estimand_note)
        ),
        summarize_effect_model(
            model = unadjusted_model,
            dataset_name = dataset_name %||% "unspecified_dataset",
            analysis_label = sequela_label,
            model_label = "Unadjusted logistic",
            group_var = "treatment_group",
            data_source_label = "Filtered Objective 0-validated recorded-burden dataset without covariates",
            effect_measure = "OR",
            outcome_var = outcome_var,
            notes = burden_estimand_note
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "unspecified_dataset",
            analysis_label = sequela_label,
            model_label = "Unadjusted logistic",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula(outcome_var, "treatment_group"),
            covariates_used = "None",
            effect_measure = "OR",
            n_patients = nrow(model_data),
            n_events = sum(model_data[[outcome_var]] == 1, na.rm = TRUE),
            n_outcome_non_missing = sum(!is.na(model_data[[outcome_var]])),
            data_source = "Filtered Objective 0-validated recorded-burden dataset without covariates",
            model_status = "SKIPPED",
            notes = paste("Unadjusted logistic model could not be fit.", burden_estimand_note)
        ),
        summarize_effect_model(
            model = model_result,
            dataset_name = dataset_name %||% "unspecified_dataset",
            analysis_label = sequela_label,
            model_label = "Adjusted logistic",
            group_var = "treatment_group",
            data_source_label = "Filtered Objective 0-validated recorded-burden dataset with confounders",
            effect_measure = "OR",
            outcome_var = outcome_var,
            notes = burden_estimand_note
        ) %||% create_effect_summary_rows(
            dataset_name = dataset_name %||% "unspecified_dataset",
            analysis_label = sequela_label,
            model_label = "Adjusted logistic",
            term = "treatment_group",
            model_formula = build_effect_summary_model_formula(outcome_var, "treatment_group", confounders_for_model),
            covariates_used = format_effect_summary_covariates(confounders_for_model),
            effect_measure = "OR",
            n_patients = nrow(model_data),
            n_events = sum(model_data[[outcome_var]] == 1, na.rm = TRUE),
            n_outcome_non_missing = sum(!is.na(model_data[[outcome_var]])),
            data_source = "Filtered Objective 0-validated recorded-burden dataset with confounders",
            model_status = "SKIPPED",
            notes = if (is.list(safety_diagnostics) && !is.null(safety_diagnostics$raw_model_output)) {
                paste(paste(as.character(safety_diagnostics$raw_model_output), collapse = " "), burden_estimand_note)
            } else {
                paste("Adjusted logistic model could not be fit.", burden_estimand_note)
            }
        )
    )

    write_effect_summary_workbook(
        effect_summary_rows = effect_summary_rows,
        output_dir = effect_summary_dir,
        prefix = prefix,
        analysis_name = sequela_label
    )

    return(list(
        rates = sequela_rates,
        table = if (!is.null(regression_table)) regression_table else tbl, # Return regression table if available, otherwise summary table
        model = model_result,
        diagnostics = safety_diagnostics, # Add diagnostics for consolidation
        effect_summary = effect_summary_rows
    ))
}
