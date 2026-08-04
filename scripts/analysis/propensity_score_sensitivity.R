# Restricted-cohort propensity-overlap sensitivity analysis

OBJECTIVE1_PROPENSITY_WORKBOOK_SHEETS <- c(
    "analysis_specification", "analysis_population", "propensity_diagnostics",
    "covariate_balance", "weighted_cox_results", "ph_diagnostics"
)

OBJECTIVE1_PROPENSITY_ARTIFACT_BASENAMES <- c(
    workbook = "propensity_overlap_sensitivity.xlsx",
    overlap_plot = "propensity_score_overlap.png",
    balance_plot = "propensity_covariate_balance.png",
    forest_plot = "propensity_overlap_forest_plot.png",
    schoenfeld_plot = "propensity_weighted_schoenfeld.png",
    summary = "propensity_overlap_summary.md",
    audit = "propensity_design_audit.rds"
)

#' Compute a stable fingerprint for propensity-model membership
#'
#' @param data Selected propensity-model rows.
#' @return SHA-256 digest of sorted patient identifier and treatment records.
compute_propensity_membership_fingerprint <- function(data) {
    id_candidates <- intersect(c("id", "study_id", "patient_id"), names(data))
    if (length(id_candidates) == 0L) {
        stop(
            "A stable patient identifier is required to verify the propensity population.",
            call. = FALSE
        )
    }
    id_col <- id_candidates[[1]]
    records <- data %>%
        dplyr::transmute(
            patient_id = as.character(.data[[id_col]]),
            treatment_group = as.character(.data$treatment_group)
        ) %>%
        dplyr::arrange(.data$patient_id, .data$treatment_group)

    digest::digest(
        paste(
            paste(records$patient_id, records$treatment_group, sep = "|"),
            collapse = "\n"
        ),
        algo = "sha256",
        serialize = FALSE
    )
}

#' Prepare the prespecified restricted propensity-model population
#'
#' Validates Objective 0 fields without deriving or recoding analytic variables.
#' Sparse location levels select model rows only; the input dataset is retained.
#'
#' @param data Objective 0 analytic cohort.
#' @param dataset_name Dataset identifier used for production drift checks.
#' @return List containing the unchanged data, selected row indices, and audits.
prepare_objective1_propensity_population <- function(data, dataset_name) {
    required_columns <- c("treatment_group", OBJECTIVE1_PROPENSITY_COVARIATES)
    missing_columns <- setdiff(required_columns, names(data))
    if (length(missing_columns) > 0L) {
        stop(
            sprintf(
                "Propensity analysis is missing required propensity columns: %s.",
                paste(missing_columns, collapse = ", ")
            ),
            call. = FALSE
        )
    }

    missing_value_columns <- required_columns[vapply(
        data[required_columns],
        function(values) any(is.na(values)),
        logical(1)
    )]
    if (length(missing_value_columns) > 0L) {
        stop(
            sprintf(
                "Propensity analysis has missing required propensity values in: %s.",
                paste(missing_value_columns, collapse = ", ")
            ),
            call. = FALSE
        )
    }

    if (!is.factor(data$treatment_group) ||
        !identical(levels(data$treatment_group), TREATMENT_FACTOR_LEVELS) ||
        !identical(
            sort(unique(as.character(data$treatment_group))),
            sort(TREATMENT_FACTOR_LEVELS)
        )) {
        stop(
            sprintf(
                "Propensity treatment factor levels must be exactly %s in that order, with both arms observed.",
                paste(TREATMENT_FACTOR_LEVELS, collapse = ", ")
            ),
            call. = FALSE
        )
    }

    sparse_summary <- summarize_sparse_factor_levels(
        data$location,
        min_level_count = THRESHOLD_RARITY
    )
    if (!is.na(sparse_summary$drop_reason)) {
        stop(
            sprintf("Propensity location support is invalid: %s.", sparse_summary$drop_reason),
            call. = FALSE
        )
    }

    excluded <- as.character(data$location) %in% sparse_summary$excluded_levels
    selection_index <- which(!excluded)
    selected_data <- data[selection_index, , drop = FALSE]
    population_fingerprint <- compute_propensity_membership_fingerprint(selected_data)

    if (identical(dataset_name, OBJECTIVE1_PROPENSITY_DATASET)) {
        expected <- OBJECTIVE1_PROPENSITY_EXPECTED_POPULATIONS %>%
            dplyr::filter(.data$surface == "propensity_membership")
        if (nrow(expected) != 1L ||
            nrow(selected_data) != expected$n_patients[[1]] ||
            !identical(population_fingerprint, expected$population_fingerprint[[1]])) {
            stop(
                sprintf(
                    paste(
                        "Restricted propensity population drift detected:",
                        "observed n=%d and fingerprint=%s. Review row-level membership before approval."
                    ),
                    nrow(selected_data),
                    population_fingerprint
                ),
                call. = FALSE
            )
        }
    }

    sparse_audit <- tibble::tibble(
        variable = "location",
        threshold = THRESHOLD_RARITY,
        excluded_levels = paste(sparse_summary$excluded_levels, collapse = ", "),
        retained_levels = paste(sparse_summary$retained_levels, collapse = ", "),
        excluded_n = as.integer(sum(excluded))
    )
    population_audit <- tibble::tibble(
        dataset_name = dataset_name,
        input_n = nrow(data),
        model_n = nrow(selected_data),
        excluded_n = as.integer(sum(excluded)),
        population_fingerprint = population_fingerprint
    )

    list(
        data = data,
        selection_index = selection_index,
        sparse_audit = sparse_audit,
        population_audit = population_audit
    )
}

#' Compute an effective sample size from analysis weights
#'
#' @param weights Positive numeric weights.
#' @return Effective sample size.
compute_propensity_effective_sample_size <- function(weights) {
    sum(weights)^2 / sum(weights^2)
}

#' Compute standardized mean differences for the propensity model matrix
#'
#' @param model_matrix Model matrix without its intercept column.
#' @param treatment Treatment factor with approved reference/comparison levels.
#' @param weights Overlap weights.
#' @return Covariate-balance tibble.
compute_propensity_balance <- function(model_matrix, treatment, weights) {
    reference <- treatment == TREATMENT_REFERENCE_LEVEL
    comparison <- treatment == TREATMENT_COMPARISON_LEVEL

    purrr::map_dfr(colnames(model_matrix), function(term) {
        values <- model_matrix[, term]
        pooled_sd <- sqrt(
            (stats::var(values[reference]) + stats::var(values[comparison])) / 2
        )
        unweighted_difference <- mean(values[comparison]) - mean(values[reference])
        weighted_difference <- stats::weighted.mean(values[comparison], weights[comparison]) -
            stats::weighted.mean(values[reference], weights[reference])
        unweighted_smd <- if (pooled_sd == 0) 0 else unweighted_difference / pooled_sd
        weighted_smd <- if (pooled_sd == 0) 0 else weighted_difference / pooled_sd

        tibble::tibble(
            term = term,
            unweighted_smd = unweighted_smd,
            unweighted_abs_smd = abs(unweighted_smd),
            weighted_smd = weighted_smd,
            weighted_abs_smd = abs(weighted_smd),
            weighted_exceeds_threshold = abs(weighted_smd) >
                OBJECTIVE1_PROPENSITY_BALANCE_THRESHOLD
        )
    })
}

#' Compute a stable overlap-weight fingerprint
#'
#' @param data Weighted propensity-model data.
#' @return SHA-256 digest of sorted patient identifiers and weights.
compute_propensity_weight_fingerprint <- function(data) {
    id_candidates <- intersect(c("id", "study_id", "patient_id"), names(data))
    if (length(id_candidates) == 0L) {
        stop("A stable patient identifier is required to verify propensity weights.", call. = FALSE)
    }
    id_col <- id_candidates[[1]]
    records <- data %>%
        dplyr::transmute(
            patient_id = as.character(.data[[id_col]]),
            weight = sprintf("%.12f", .data$.overlap_weight)
        ) %>%
        dplyr::arrange(.data$patient_id)

    digest::digest(
        paste(paste(records$patient_id, records$weight, sep = "|"), collapse = "\n"),
        algo = "sha256",
        serialize = FALSE
    )
}

#' Fit the prespecified propensity model and overlap weights
#'
#' @param prepared_population Output from
#'   `prepare_objective1_propensity_population()`.
#' @return Weighted design and its diagnostics.
fit_objective1_propensity_weights <- function(prepared_population) {
    formula <- stats::reformulate(
        OBJECTIVE1_PROPENSITY_COVARIATES,
        response = "treatment_group"
    )
    analysis_data <- prepared_population$data[
        prepared_population$selection_index,
        ,
        drop = FALSE
    ]
    model <- suppressWarnings(stats::glm(
        formula,
        data = analysis_data,
        family = stats::binomial()
    ))

    coefficients <- stats::coef(model)
    if (!isTRUE(model$converged) || any(!is.finite(coefficients))) {
        stop(
            "Propensity model has aliased or non-finite coefficients and cannot be used.",
            call. = FALSE
        )
    }

    propensity_score <- stats::predict(model, type = "response")
    if (any(!is.finite(propensity_score)) ||
        any(propensity_score <= 0 | propensity_score >= 1)) {
        stop("Propensity scores must be finite and strictly inside (0, 1).", call. = FALSE)
    }

    analysis_data <- analysis_data %>%
        dplyr::mutate(
            .propensity_score = propensity_score,
            .overlap_weight = dplyr::case_when(
                .data$treatment_group == TREATMENT_COMPARISON_LEVEL ~
                    1 - .data$.propensity_score,
                .data$treatment_group == TREATMENT_REFERENCE_LEVEL ~
                    .data$.propensity_score,
                TRUE ~ NA_real_
            )
        )
    if (any(!is.finite(analysis_data$.overlap_weight)) ||
        any(analysis_data$.overlap_weight <= 0 | analysis_data$.overlap_weight >= 1)) {
        stop("Overlap weights must be finite and strictly inside (0, 1).", call. = FALSE)
    }

    model_matrix <- stats::model.matrix(model)
    model_matrix <- model_matrix[, colnames(model_matrix) != "(Intercept)", drop = FALSE]
    balance_table <- compute_propensity_balance(
        model_matrix,
        analysis_data$treatment_group,
        analysis_data$.overlap_weight
    )
    weight_fingerprint <- compute_propensity_weight_fingerprint(analysis_data)

    score_weight_summary <- analysis_data %>%
        dplyr::group_by(.data$treatment_group) %>%
        dplyr::summarise(
            n = dplyr::n(),
            ps_min = min(.data$.propensity_score),
            ps_q1 = as.numeric(stats::quantile(.data$.propensity_score, 0.25)),
            ps_median = stats::median(.data$.propensity_score),
            ps_q3 = as.numeric(stats::quantile(.data$.propensity_score, 0.75)),
            ps_max = max(.data$.propensity_score),
            weight_min = min(.data$.overlap_weight),
            weight_q1 = as.numeric(stats::quantile(.data$.overlap_weight, 0.25)),
            weight_median = stats::median(.data$.overlap_weight),
            weight_q3 = as.numeric(stats::quantile(.data$.overlap_weight, 0.75)),
            weight_max = max(.data$.overlap_weight),
            .groups = "drop"
        )
    ess_summary <- analysis_data %>%
        dplyr::group_by(.data$treatment_group) %>%
        dplyr::summarise(
            n = dplyr::n(),
            effective_sample_size = compute_propensity_effective_sample_size(
                .data$.overlap_weight
            ),
            .groups = "drop"
        ) %>%
        dplyr::mutate(treatment_group = as.character(.data$treatment_group)) %>%
        dplyr::bind_rows(tibble::tibble(
            treatment_group = "Total",
            n = nrow(analysis_data),
            effective_sample_size = compute_propensity_effective_sample_size(
                analysis_data$.overlap_weight
            )
        ))

    arm_ranges <- score_weight_summary %>%
        dplyr::select("treatment_group", "ps_min", "ps_max")
    common_min <- max(arm_ranges$ps_min)
    common_max <- min(arm_ranges$ps_max)
    common_support_fraction <- mean(
        analysis_data$.propensity_score >= common_min &
            analysis_data$.propensity_score <= common_max
    )
    formula_text <- paste(
        "treatment_group ~",
        paste(OBJECTIVE1_PROPENSITY_COVARIATES, collapse = " + ")
    )
    diagnostics <- tibble::tibble(
        estimand = OBJECTIVE1_PROPENSITY_ESTIMAND,
        formula = formula_text,
        population_fingerprint = prepared_population$population_audit$population_fingerprint,
        weight_fingerprint = weight_fingerprint,
        n = nrow(analysis_data),
        propensity_min = min(analysis_data$.propensity_score),
        propensity_max = max(analysis_data$.propensity_score),
        common_support_min = common_min,
        common_support_max = common_max,
        common_support_fraction = common_support_fraction,
        maximum_unweighted_abs_smd = max(balance_table$unweighted_abs_smd),
        maximum_weighted_abs_smd = max(balance_table$weighted_abs_smd)
    )
    coefficient_table <- broom::tidy(model)

    list(
        data = analysis_data,
        model = model,
        formula = formula,
        formula_text = formula_text,
        model_matrix = model_matrix,
        coefficient_table = coefficient_table,
        score_weight_summary = score_weight_summary,
        ess_summary = ess_summary,
        balance_table = balance_table,
        diagnostics = diagnostics,
        weight_fingerprint = weight_fingerprint,
        population = prepared_population
    )
}

#' Validate one weighted endpoint population
#'
#' @param data Weighted propensity design data.
#' @param dataset_name Dataset identifier.
#' @param outcome_key Named Objective 1 endpoint key.
#' @param spec Objective 1 endpoint specification.
#' @return Endpoint support values invisibly.
validate_objective1_weighted_endpoint <- function(data, dataset_name, outcome_key, spec) {
    required_columns <- c(
        "treatment_group", ".overlap_weight", spec$time_var, spec$event_var
    )
    missing_columns <- setdiff(required_columns, names(data))
    if (length(missing_columns) > 0L) {
        stop(
            sprintf(
                "Weighted endpoint %s is missing columns: %s.",
                outcome_key,
                paste(missing_columns, collapse = ", ")
            ),
            call. = FALSE
        )
    }
    time <- suppressWarnings(as.numeric(data[[spec$time_var]]))
    event <- suppressWarnings(as.integer(as.character(data[[spec$event_var]])))
    if (any(is.na(time)) || any(!is.finite(time)) || any(time < 0)) {
        stop(sprintf("Weighted endpoint %s has invalid follow-up times.", outcome_key), call. = FALSE)
    }
    if (any(is.na(event)) || any(!event %in% c(0L, 1L))) {
        stop(sprintf("Weighted endpoint %s must have complete binary events.", outcome_key), call. = FALSE)
    }
    if (sum(event) < MINIMUM_SURVIVAL_EVENTS) {
        stop(
            sprintf(
                "Weighted endpoint %s has %d events; at least %d are required.",
                outcome_key,
                sum(event),
                MINIMUM_SURVIVAL_EVENTS
            ),
            call. = FALSE
        )
    }

    fingerprint <- compute_survival_population_fingerprint(
        data,
        time_var = spec$time_var,
        event_var = spec$event_var,
        group_var = "treatment_group"
    )
    if (identical(dataset_name, OBJECTIVE1_PROPENSITY_DATASET)) {
        expected <- OBJECTIVE1_PROPENSITY_EXPECTED_POPULATIONS %>%
            dplyr::filter(.data$surface == .env$outcome_key)
        if (nrow(expected) != 1L ||
            nrow(data) != expected$n_patients[[1]] ||
            sum(event) != expected$n_events[[1]] ||
            !identical(fingerprint, expected$population_fingerprint[[1]])) {
            stop(
                sprintf(
                    paste(
                        "Weighted endpoint population drift for %s.",
                        "Expected n=%s, events=%s, fingerprint=%s;",
                        "observed n=%d, events=%d, fingerprint=%s.",
                        "Explain row-level drift before changing expected values."
                    ),
                    outcome_key,
                    expected$n_patients[[1]] %||% NA_integer_,
                    expected$n_events[[1]] %||% NA_integer_,
                    expected$population_fingerprint[[1]] %||% NA_character_,
                    nrow(data),
                    sum(event),
                    fingerprint
                ),
                call. = FALSE
            )
        }
    }

    invisible(list(events = sum(event), fingerprint = fingerprint))
}

#' Derive named treatment and competing-event support
#'
#' @param data Weighted propensity design data.
#' @param outcome_key Named endpoint key.
#' @param spec Objective 1 endpoint specification.
#' @return One-row support tibble.
build_objective1_weighted_endpoint_support <- function(data, outcome_key, spec) {
    event <- as.integer(as.character(data[[spec$event_var]]))
    reference <- data$treatment_group == TREATMENT_REFERENCE_LEVEL
    comparison <- data$treatment_group == TREATMENT_COMPARISON_LEVEL
    support <- tibble::tibble(
        outcome_key = outcome_key,
        n = nrow(data),
        events = sum(event),
        pbt_n = sum(reference),
        pbt_events = sum(event[reference]),
        gksrs_n = sum(comparison),
        gksrs_events = sum(event[comparison]),
        competing_deaths = NA_integer_,
        pbt_competing_deaths = NA_integer_,
        gksrs_competing_deaths = NA_integer_
    )

    if (outcome_key %in% c("local_recurrence", "metastatic_progression")) {
        competing <- prepare_competing_risk_data(
            data,
            time_var = spec$time_var,
            event_var = spec$event_var,
            group_var = "treatment_group"
        )
        competing_death <- competing$.cr_status == 2L
        support$competing_deaths <- sum(competing_death)
        support$pbt_competing_deaths <- sum(
            competing_death & competing$group == TREATMENT_REFERENCE_LEVEL
        )
        support$gksrs_competing_deaths <- sum(
            competing_death & competing$group == TREATMENT_COMPARISON_LEVEL
        )
    }

    support
}

#' Fit all four overlap-weighted Objective 1 endpoint models
#'
#' @param weighted_design Output from `fit_objective1_propensity_weights()`.
#' @return Endpoint models, canonical results, support, and PH diagnostics.
fit_objective1_weighted_endpoints <- function(weighted_design) {
    data <- weighted_design$data
    dataset_name <- weighted_design$population$population_audit$dataset_name[[1]]
    ess <- stats::setNames(
        weighted_design$ess_summary$effective_sample_size,
        weighted_design$ess_summary$treatment_group
    )
    fits <- list()
    result_rows <- list()
    support_rows <- list()
    ph_rows <- list()
    zph_objects <- list()

    for (outcome_key in names(OBJECTIVE1_SUBGROUP_OUTCOME_SPECS)) {
        spec <- OBJECTIVE1_SUBGROUP_OUTCOME_SPECS[[outcome_key]]
        validation <- validate_objective1_weighted_endpoint(
            data,
            dataset_name,
            outcome_key,
            spec
        )
        support <- build_objective1_weighted_endpoint_support(data, outcome_key, spec)
        formula <- stats::as.formula(sprintf(
            "survival::Surv(%s, %s) ~ treatment_group",
            spec$time_var,
            spec$event_var
        ))
        model <- survival::coxph(
            formula,
            data = data,
            weights = .overlap_weight,
            robust = TRUE,
            x = TRUE,
            model = TRUE
        )
        coefficient_name <- paste0("treatment_group", TREATMENT_COMPARISON_LEVEL)
        if (!identical(names(stats::coef(model)), coefficient_name)) {
            stop(
                sprintf(
                    "Weighted endpoint %s did not estimate the required %s coefficient.",
                    outcome_key,
                    coefficient_name
                ),
                call. = FALSE
            )
        }

        log_hr <- unname(stats::coef(model)[[1]])
        robust_se <- sqrt(model$var[[1]])
        z_value <- log_hr / robust_se
        estimate <- exp(log_hr)
        conf_low <- exp(log_hr - stats::qnorm(0.975) * robust_se)
        conf_high <- exp(log_hr + stats::qnorm(0.975) * robust_se)
        p_value <- 2 * stats::pnorm(abs(z_value), lower.tail = FALSE)

        if (validation$events >= MINIMUM_PH_TEST_EVENTS) {
            zph <- survival::cox.zph(model)
            zph_table <- as.data.frame(zph$table) %>%
                tibble::rownames_to_column("term")
            names(zph_table)[names(zph_table) == "p"] <- "p_value"
            ph <- zph_table %>%
                dplyr::transmute(
                    outcome_key = outcome_key,
                    term = .data$term,
                    chisq = .data$chisq,
                    df = .data$df,
                    p_value = .data$p_value,
                    status = "tested"
                )
            ph_global_p <- ph$p_value[ph$term == "GLOBAL"][[1]]
        } else {
            zph <- NULL
            ph <- tibble::tibble(
                outcome_key = outcome_key,
                term = c("treatment_group", "GLOBAL"),
                chisq = NA_real_,
                df = NA_real_,
                p_value = NA_real_,
                status = "not_tested_insufficient_events"
            )
            ph_global_p <- NA_real_
        }

        interpretation <- sprintf(
            paste(
                "Overlap-weighted GKSRS-versus-PBT hazard ratio in the measured",
                "overlap population; %s."
            ),
            spec$estimand
        )
        result_rows[[outcome_key]] <- tibble::tibble(
            outcome_key = outcome_key,
            outcome = spec$outcome,
            time_var = spec$time_var,
            event_var = spec$event_var,
            endpoint_estimand = spec$estimand,
            model_family = "Overlap-weighted Cox proportional hazards",
            effect_measure = spec$effect_measure,
            comparison = sprintf(
                "%s vs %s",
                TREATMENT_COMPARISON_LEVEL,
                TREATMENT_REFERENCE_LEVEL
            ),
            estimand = OBJECTIVE1_PROPENSITY_ESTIMAND,
            weight_method = sprintf("Overlap weighting (%s)", OBJECTIVE1_PROPENSITY_ESTIMAND),
            n = support$n,
            events = support$events,
            pbt_n = support$pbt_n,
            pbt_events = support$pbt_events,
            gksrs_n = support$gksrs_n,
            gksrs_events = support$gksrs_events,
            weighted_ess_total = unname(ess[["Total"]]),
            weighted_ess_pbt = unname(ess[[TREATMENT_REFERENCE_LEVEL]]),
            weighted_ess_gksrs = unname(ess[[TREATMENT_COMPARISON_LEVEL]]),
            estimate = estimate,
            conf_low = conf_low,
            conf_high = conf_high,
            p_value = p_value,
            ph_global_p = ph_global_p,
            weight_fingerprint = weighted_design$weight_fingerprint,
            status = "estimated",
            interpretation = interpretation
        )
        support_rows[[outcome_key]] <- support
        ph_rows[[outcome_key]] <- ph
        zph_objects[[outcome_key]] <- zph
        fits[[outcome_key]] <- list(model = model, zph = zph)
    }

    list(
        fits = fits,
        weighted_cox_results = dplyr::bind_rows(result_rows),
        endpoint_support = dplyr::bind_rows(support_rows),
        ph_diagnostics = dplyr::bind_rows(ph_rows),
        zph_objects = zph_objects
    )
}
