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

OBJECTIVE1_PROPENSITY_AUDIT_SCHEMA_VERSION <- 1L

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

#' Compute a complete design-input fingerprint
#'
#' @param data Original analytic cohort.
#' @return SHA-256 fingerprint over sorted model and endpoint inputs.
compute_propensity_design_input_fingerprint <- function(data) {
    id_candidates <- intersect(c("id", "study_id", "patient_id"), names(data))
    if (length(id_candidates) == 0L) {
        stop("A stable patient identifier is required for the propensity audit.", call. = FALSE)
    }
    endpoint_fields <- unique(unlist(lapply(
        OBJECTIVE1_SUBGROUP_OUTCOME_SPECS,
        function(spec) c(spec$time_var, spec$event_var)
    )))
    fields <- unique(c(
        id_candidates[[1]], "treatment_group", OBJECTIVE1_PROPENSITY_COVARIATES,
        endpoint_fields, "tt_death_months", "death_event"
    ))
    records <- data %>%
        dplyr::select(dplyr::all_of(fields)) %>%
        dplyr::arrange(as.character(.data[[id_candidates[[1]]]]))
    canonical <- lapply(records, function(values) {
        if (is.numeric(values)) {
            ifelse(is.na(values), "NA", sprintf("%.12f", values))
        } else {
            ifelse(is.na(values), "NA", as.character(values))
        }
    })
    digest::digest(canonical, algo = "sha256", serialize = TRUE)
}

#' Build aggregate propensity population checks
#'
#' @param weighted_design Weighted propensity design.
#' @param endpoint_results Weighted endpoint results.
#' @return Population-check tibble.
build_propensity_population_checks <- function(weighted_design, endpoint_results) {
    dataset_name <- weighted_design$population$population_audit$dataset_name[[1]]
    observed <- dplyr::bind_rows(
        tibble::tibble(
            surface = "propensity_membership",
            observed_n = nrow(weighted_design$data),
            observed_events = NA_integer_,
            observed_fingerprint = weighted_design$population$population_audit$population_fingerprint
        ),
        purrr::imap_dfr(OBJECTIVE1_SUBGROUP_OUTCOME_SPECS, function(spec, outcome_key) {
            tibble::tibble(
                surface = outcome_key,
                observed_n = nrow(weighted_design$data),
                observed_events = sum(as.integer(as.character(
                    weighted_design$data[[spec$event_var]]
                ))),
                observed_fingerprint = compute_survival_population_fingerprint(
                    weighted_design$data,
                    spec$time_var,
                    spec$event_var,
                    "treatment_group"
                )
            )
        })
    )
    expected <- if (identical(dataset_name, OBJECTIVE1_PROPENSITY_DATASET)) {
        OBJECTIVE1_PROPENSITY_EXPECTED_POPULATIONS %>%
            dplyr::rename(
                expected_n = "n_patients",
                expected_events = "n_events",
                expected_fingerprint = "population_fingerprint"
            )
    } else {
        observed %>%
            dplyr::transmute(
                surface = .data$surface,
                expected_n = NA_integer_,
                expected_events = NA_integer_,
                expected_fingerprint = NA_character_
            )
    }

    observed %>%
        dplyr::left_join(expected, by = "surface") %>%
        dplyr::mutate(
            status = dplyr::if_else(
                is.na(.data$expected_n),
                "structural_validation_only",
                dplyr::if_else(
                    .data$observed_n == .data$expected_n &
                        (is.na(.data$expected_events) |
                            .data$observed_events == .data$expected_events) &
                        .data$observed_fingerprint == .data$expected_fingerprint,
                    "matched",
                    "drift"
                )
            )
        )
}

#' Build the technical patient and endpoint design audit
#'
#' @param weighted_design Weighted propensity design.
#' @param endpoint_results Weighted endpoint results.
#' @param analysis_specification Analysis specification list.
#' @param cohort_flow Aggregate cohort-flow table.
#' @param population_checks Aggregate population-check table.
#' @return Local-only technical audit list.
build_propensity_technical_audit <- function(weighted_design,
                                             endpoint_results,
                                             analysis_specification,
                                             cohort_flow,
                                             population_checks) {
    original <- weighted_design$population$data
    selection_index <- weighted_design$population$selection_index
    id_candidates <- intersect(c("id", "study_id", "patient_id"), names(original))
    id_col <- id_candidates[[1]]
    included <- seq_len(nrow(original)) %in% selection_index
    linear_predictor <- rep(NA_real_, nrow(original))
    propensity_score <- rep(NA_real_, nrow(original))
    overlap_weight <- rep(NA_real_, nrow(original))
    linear_predictor[selection_index] <- stats::predict(
        weighted_design$model,
        type = "link"
    )
    propensity_score[selection_index] <- weighted_design$data$.propensity_score
    overlap_weight[selection_index] <- weighted_design$data$.overlap_weight
    excluded_location <- as.character(original$location)
    exclusion_reason <- ifelse(
        included,
        NA_character_,
        sprintf(
            "Sparse location level: %s (observed n below %d)",
            excluded_location,
            THRESHOLD_RARITY
        )
    )
    patient_design <- original %>%
        dplyr::transmute(
            study_id = as.character(.data[[id_col]]),
            treatment_group = .data$treatment_group,
            dplyr::across(dplyr::all_of(OBJECTIVE1_PROPENSITY_COVARIATES)),
            analysis_included = included,
            exclusion_reason = exclusion_reason,
            propensity_linear_predictor = linear_predictor,
            propensity_score = propensity_score,
            overlap_weight = overlap_weight
        )

    full_model_matrix <- stats::model.matrix(weighted_design$model)
    propensity_model_matrix <- dplyr::bind_cols(
        tibble::tibble(
            study_id = as.character(weighted_design$data[[id_col]])
        ),
        as.data.frame(full_model_matrix, check.names = FALSE)
    )
    endpoint_fields <- unique(unlist(lapply(
        OBJECTIVE1_SUBGROUP_OUTCOME_SPECS,
        function(spec) c(spec$time_var, spec$event_var)
    )))
    recurrence_cr <- prepare_competing_risk_data(
        weighted_design$data,
        OBJECTIVE1_SUBGROUP_OUTCOME_SPECS$local_recurrence$time_var,
        OBJECTIVE1_SUBGROUP_OUTCOME_SPECS$local_recurrence$event_var
    )
    metastasis_cr <- prepare_competing_risk_data(
        weighted_design$data,
        OBJECTIVE1_SUBGROUP_OUTCOME_SPECS$metastatic_progression$time_var,
        OBJECTIVE1_SUBGROUP_OUTCOME_SPECS$metastatic_progression$event_var
    )
    endpoint_design <- weighted_design$data %>%
        dplyr::transmute(
            study_id = as.character(.data[[id_col]]),
            treatment_group = .data$treatment_group,
            dplyr::across(dplyr::all_of(endpoint_fields)),
            tt_death_months = .data$tt_death_months,
            death_event = .data$death_event,
            recurrence_competing_death = recurrence_cr$.cr_status == 2L,
            metastasis_competing_death = metastasis_cr$.cr_status == 2L,
            overlap_weight = .data$.overlap_weight
        )
    endpoint_fingerprints <- stats::setNames(
        population_checks$observed_fingerprint[
            population_checks$surface != "propensity_membership"
        ],
        population_checks$surface[
            population_checks$surface != "propensity_membership"
        ]
    )

    list(
        schema_version = OBJECTIVE1_PROPENSITY_AUDIT_SCHEMA_VERSION,
        analysis_specification = analysis_specification,
        provenance = list(
            generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
            session_info = utils::sessionInfo(),
            fingerprints = list(
                design_input = compute_propensity_design_input_fingerprint(original),
                membership = weighted_design$population$population_audit$population_fingerprint,
                endpoints = endpoint_fingerprints,
                weight = weighted_design$weight_fingerprint
            )
        ),
        cohort_flow = cohort_flow,
        population_checks = population_checks,
        patient_design = patient_design,
        propensity_model_matrix = propensity_model_matrix,
        propensity_model = weighted_design$model,
        propensity_diagnostics = list(
            coefficients = weighted_design$coefficient_table,
            score_weight_summary = weighted_design$score_weight_summary,
            effective_sample_size = weighted_design$ess_summary,
            diagnostics = weighted_design$diagnostics,
            balance = weighted_design$balance_table
        ),
        endpoint_design = endpoint_design,
        weighted_cox_models = lapply(endpoint_results$fits, `[[`, "model"),
        ph_objects = endpoint_results$zph_objects,
        weighted_cox_results = endpoint_results$weighted_cox_results,
        endpoint_support = endpoint_results$endpoint_support,
        ph_diagnostics = endpoint_results$ph_diagnostics
    )
}

#' Build all reader-facing and technical propensity artifacts
#'
#' @param weighted_design Weighted propensity design.
#' @param endpoint_results Weighted endpoint results.
#' @return Structured artifact object used by every writer.
build_objective1_propensity_artifacts <- function(weighted_design, endpoint_results) {
    dataset_name <- weighted_design$population$population_audit$dataset_name[[1]]
    specification_table <- tibble::tribble(
        ~item, ~value,
        "Analysis role", "Supplemental sensitivity; primary adjusted Cox models remain primary",
        "Cohort", dataset_name,
        "Treatment comparison", sprintf("%s vs %s", TREATMENT_COMPARISON_LEVEL, TREATMENT_REFERENCE_LEVEL),
        "Propensity model", weighted_design$formula_text,
        "Propensity estimand", OBJECTIVE1_PROPENSITY_ESTIMAND,
        "Weighting", "Overlap weights; no trimming, capping, stabilization, or support filtering",
        "Outcome models", "Treatment-only overlap-weighted Cox models with robust sandwich variance",
        "Recurrence/metastasis", "Cause-specific hazards; death before the endpoint ends risk time",
        "Competing-risk effect", "No propensity-weighted Fine-Gray or cumulative-incidence effect was estimated",
        "Balance threshold", sprintf("Absolute standardized mean difference <= %.2f", OBJECTIVE1_PROPENSITY_BALANCE_THRESHOLD)
    )
    covariate_rationale <- tibble::tribble(
        ~variable, ~decision, ~rationale,
        "age_at_diagnosis", "Included", "Pretreatment demographic factor; modeled continuously",
        "sex", "Included", "Pretreatment demographic factor",
        "location", "Included", "Pretreatment anatomy; sparse levels excluded from this model population only",
        "initial_tumor_height", "Included", "Pretreatment continuous tumor burden",
        "initial_tumor_diameter", "Included", "Pretreatment continuous tumor burden",
        "srf", "Included", "Pretreatment subretinal-fluid feature",
        "treatment_year", "Included", "Index-date treatment-era factor",
        "T/overall stage", "Excluded", "Redundant with continuous tumor dimensions",
        "optic_nerve", "Excluded", "No restricted-cohort variation",
        "dose/proximity/planning fields", "Excluded", "Unavailable comparably or treatment-defined"
    )
    cohort_flow <- tibble::tibble(
        stage = c("Restricted analytic cohort", "Sparse-location exclusions", "Propensity model population"),
        n = c(
            nrow(weighted_design$population$data),
            weighted_design$population$sparse_audit$excluded_n,
            nrow(weighted_design$data)
        ),
        detail = c(
            "Objective 0 analytic dataset",
            weighted_design$population$sparse_audit$excluded_levels,
            "One shared population and weight vector for all four endpoints"
        )
    )
    population_checks <- build_propensity_population_checks(weighted_design, endpoint_results)

    palette <- get_treatment_palette(TREATMENT_FACTOR_LEVELS)
    overlap_plot <- ggplot2::ggplot(
        weighted_design$data,
        ggplot2::aes(
            x = .data$.propensity_score,
            color = .data$treatment_group,
            fill = .data$treatment_group
        )
    ) +
        ggplot2::geom_density(alpha = 0.18, linewidth = 1) +
        ggplot2::scale_color_manual(values = palette, drop = FALSE) +
        ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
        ggplot2::labs(
            title = "Propensity-score overlap in the restricted cohort",
            subtitle = "Unweighted score distributions; overlap weights are used for analysis",
            x = sprintf("Probability of %s treatment", TREATMENT_COMPARISON_LEVEL),
            y = "Density",
            color = "Treatment",
            fill = "Treatment"
        ) +
        ggplot2::theme_minimal(base_size = 12)

    balance_plot_data <- weighted_design$balance_table %>%
        dplyr::select("term", "unweighted_abs_smd", "weighted_abs_smd") %>%
        tidyr::pivot_longer(
            cols = c("unweighted_abs_smd", "weighted_abs_smd"),
            names_to = "weighting",
            values_to = "absolute_smd"
        ) %>%
        dplyr::mutate(
            weighting = dplyr::recode(
                .data$weighting,
                unweighted_abs_smd = "Unweighted",
                weighted_abs_smd = "Overlap weighted"
            ),
            term = stats::reorder(.data$term, .data$absolute_smd, FUN = max)
        )
    balance_plot <- ggplot2::ggplot(
        balance_plot_data,
        ggplot2::aes(x = .data$absolute_smd, y = .data$term, color = .data$weighting, shape = .data$weighting)
    ) +
        ggplot2::geom_vline(
            xintercept = OBJECTIVE1_PROPENSITY_BALANCE_THRESHOLD,
            linetype = "dashed",
            color = "grey45"
        ) +
        ggplot2::geom_point(size = 2.6) +
        ggplot2::labs(
            title = "Covariate balance before and after overlap weighting",
            subtitle = sprintf("Restricted-cohort %s target population", OBJECTIVE1_PROPENSITY_ESTIMAND),
            x = "Absolute standardized mean difference",
            y = NULL,
            color = NULL,
            shape = NULL
        ) +
        ggplot2::theme_minimal(base_size = 12)

    forest_data <- endpoint_results$weighted_cox_results %>%
        dplyr::mutate(
            outcome = factor(.data$outcome, levels = rev(.data$outcome)),
            result_label = sprintf(
                "%.2f (%.2f, %.2f); p=%s",
                .data$estimate,
                .data$conf_low,
                .data$conf_high,
                format.pval(.data$p_value, digits = 2, eps = 0.001)
            )
        )
    forest_plot <- ggplot2::ggplot(
        forest_data,
        ggplot2::aes(y = .data$outcome, x = .data$estimate)
    ) +
        ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "grey45") +
        ggplot2::geom_errorbarh(
            ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
            height = 0.18
        ) +
        ggplot2::geom_point(size = 2.8) +
        ggplot2::scale_x_log10() +
        ggplot2::labs(
            title = "Restricted-cohort propensity-overlap sensitivity",
            subtitle = "GKSRS versus PBT; restricted-cohort average treatment effect in the overlap population",
            x = "Overlap-weighted HR (95% CI)",
            y = NULL
        ) +
        ggplot2::theme_minimal(base_size = 12)

    analysis_specification <- list(
        specification = specification_table,
        covariate_rationale = covariate_rationale,
        dataset_name = dataset_name,
        formula = weighted_design$formula_text,
        covariates = OBJECTIVE1_PROPENSITY_COVARIATES,
        treatment_reference = TREATMENT_REFERENCE_LEVEL,
        treatment_comparison = TREATMENT_COMPARISON_LEVEL,
        estimand = OBJECTIVE1_PROPENSITY_ESTIMAND,
        weighting_method = "Overlap weighting",
        balance_threshold = OBJECTIVE1_PROPENSITY_BALANCE_THRESHOLD,
        sparse_threshold = THRESHOLD_RARITY,
        endpoint_specifications = OBJECTIVE1_SUBGROUP_OUTCOME_SPECS,
        treatment_factor_levels = TREATMENT_FACTOR_LEVELS,
        contrasts = getOption("contrasts")
    )
    technical_audit <- build_propensity_technical_audit(
        weighted_design,
        endpoint_results,
        analysis_specification,
        cohort_flow,
        population_checks
    )
    layout <- list(
        analysis_specification = list(
            specification = 2L,
            covariate_rationale = 2L + nrow(specification_table) + 3L
        ),
        analysis_population = list(
            cohort_flow = 2L,
            population_checks = 2L + nrow(cohort_flow) + 3L,
            endpoint_support = 2L + nrow(cohort_flow) + 3L + nrow(population_checks) + 3L
        ),
        propensity_diagnostics = list(
            coefficients = 2L,
            score_weight_summary = 2L + nrow(weighted_design$coefficient_table) + 3L,
            effective_sample_size = 2L + nrow(weighted_design$coefficient_table) + 3L +
                nrow(weighted_design$score_weight_summary) + 3L
        )
    )

    list(
        analysis_specification = analysis_specification,
        cohort_flow = cohort_flow,
        population_checks = population_checks,
        propensity_diagnostics = technical_audit$propensity_diagnostics,
        covariate_balance = weighted_design$balance_table,
        weighted_cox_results = endpoint_results$weighted_cox_results,
        endpoint_support = endpoint_results$endpoint_support,
        ph_diagnostics = endpoint_results$ph_diagnostics,
        plots = list(
            overlap = overlap_plot,
            balance = balance_plot,
            forest = forest_plot
        ),
        workbook_layout = layout,
        technical_audit = technical_audit,
        weighted_design = weighted_design,
        endpoint_results = endpoint_results
    )
}

#' Write a labeled table into an openxlsx worksheet
#'
#' @param workbook Open workbook.
#' @param sheet Worksheet name.
#' @param label Table label.
#' @param data Table data.
#' @param start_row Header row for the table.
#' @return Invisibly returns the next available row.
write_propensity_labeled_table <- function(workbook, sheet, label, data, start_row) {
    openxlsx::writeData(workbook, sheet, label, startRow = start_row - 1L, colNames = FALSE)
    openxlsx::writeData(workbook, sheet, data, startRow = start_row, rowNames = FALSE)
    invisible(start_row + nrow(data) + 2L)
}

#' Build the generated propensity summary Markdown
#'
#' @param artifacts Structured propensity artifacts.
#' @return Character vector of Markdown lines.
render_objective1_propensity_summary <- function(artifacts) {
    diagnostics <- artifacts$propensity_diagnostics$diagnostics
    ess <- artifacts$propensity_diagnostics$effective_sample_size
    results <- artifacts$weighted_cox_results
    support <- artifacts$endpoint_support
    excluded <- artifacts$cohort_flow$n[artifacts$cohort_flow$stage == "Sparse-location exclusions"]
    result_lines <- purrr::pmap_chr(
        results[c("outcome", "estimate", "conf_low", "conf_high", "p_value")],
        function(outcome, estimate, conf_low, conf_high, p_value) {
            sprintf(
                "| %s | %.2f (%.2f, %.2f) | %s |",
                outcome,
                estimate,
                conf_low,
                conf_high,
                format.pval(p_value, digits = 3, eps = 0.001)
            )
        }
    )
    recurrence_support <- support[support$outcome_key == "local_recurrence", ]
    metastasis_support <- support[support$outcome_key == "metastatic_progression", ]

    c(
        "# Restricted-cohort propensity-overlap sensitivity",
        "",
        "This supplemental sensitivity addresses measured pretreatment treatment-selection differences. The prespecified adjusted Cox models remain the primary analyses.",
        "",
        sprintf(
            "The restricted analytic cohort contained %d patients. %d patients in sparse location levels were excluded from this model-specific population, leaving %d patients for one shared propensity model and weight vector.",
            artifacts$cohort_flow$n[[1]], excluded, diagnostics$n[[1]]
        ),
        sprintf("Propensity model: `%s`.", artifacts$analysis_specification$formula),
        sprintf(
            "Overlap weighting targets the average treatment effect in the overlap population (%s). No trimming or support-based filtering was applied.",
            OBJECTIVE1_PROPENSITY_ESTIMAND
        ),
        sprintf(
            "Propensity scores ranged from %.3f to %.3f; common-support fraction was %.3f. Total weighted ESS was %.1f (PBT %.1f; GKSRS %.1f), and the maximum weighted absolute standardized mean difference was %.4f.",
            diagnostics$propensity_min,
            diagnostics$propensity_max,
            diagnostics$common_support_fraction,
            ess$effective_sample_size[ess$treatment_group == "Total"],
            ess$effective_sample_size[ess$treatment_group == TREATMENT_REFERENCE_LEVEL],
            ess$effective_sample_size[ess$treatment_group == TREATMENT_COMPARISON_LEVEL],
            diagnostics$maximum_weighted_abs_smd
        ),
        "",
        "| Outcome | Overlap-weighted HR (95% CI) | p-value |",
        "|---|---:|---:|",
        result_lines,
        "",
        sprintf(
            "For local recurrence, %d deaths occurred first (PBT %d; GKSRS %d); for metastasis, %d deaths occurred first (PBT %d; GKSRS %d). These Cox estimates are cause-specific hazards among patients still alive and endpoint-free.",
            recurrence_support$competing_deaths,
            recurrence_support$pbt_competing_deaths,
            recurrence_support$gksrs_competing_deaths,
            metastasis_support$competing_deaths,
            metastasis_support$pbt_competing_deaths,
            metastasis_support$gksrs_competing_deaths
        ),
        "Weighted treatment-term and global Schoenfeld tests are reported in the workbook; the combined Schoenfeld figure should be reviewed for time trends.",
        "",
        "Limitations: weighting addresses only measured covariates; comparable dose and quantitative proximity variables were unavailable; the data remain observational; hazard ratios are non-collapsible; and event support remains limited for some outcomes. No propensity-weighted Fine-Gray or cumulative-incidence treatment effect was estimated."
    )
}

#' Format a propensity-weighted Schoenfeld panel title
#'
#' @param outcome Outcome label.
#' @param p_value Schoenfeld proportional-hazards test p-value.
#' @return Two-line panel title matching the project Schoenfeld plots.
format_propensity_schoenfeld_title <- function(outcome, p_value) {
    p_text <- if (p_value < 0.001) {
        "p < 0.001"
    } else {
        sprintf("p = %.3f", p_value)
    }
    sprintf("%s\n%s", outcome, p_text)
}

#' Write all propensity sensitivity artifacts
#'
#' @param artifacts Structured output from
#'   `build_objective1_propensity_artifacts()`.
#' @param output_dir Existing Objective 1 propensity output directory.
#' @param prefix Cohort filename prefix.
#' @return Named character vector of seven output paths.
write_objective1_propensity_artifacts <- function(artifacts, output_dir, prefix) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    output_paths <- c(
        workbook = file.path(output_dir, paste0(prefix, OBJECTIVE1_PROPENSITY_ARTIFACT_BASENAMES[["workbook"]])),
        propensity_overlap_plot = file.path(output_dir, paste0(prefix, OBJECTIVE1_PROPENSITY_ARTIFACT_BASENAMES[["overlap_plot"]])),
        covariate_balance_plot = file.path(output_dir, paste0(prefix, OBJECTIVE1_PROPENSITY_ARTIFACT_BASENAMES[["balance_plot"]])),
        results_forest_plot = file.path(output_dir, paste0(prefix, OBJECTIVE1_PROPENSITY_ARTIFACT_BASENAMES[["forest_plot"]])),
        weighted_schoenfeld_plot = file.path(output_dir, paste0(prefix, OBJECTIVE1_PROPENSITY_ARTIFACT_BASENAMES[["schoenfeld_plot"]])),
        markdown_summary = file.path(output_dir, paste0(prefix, OBJECTIVE1_PROPENSITY_ARTIFACT_BASENAMES[["summary"]])),
        technical_audit_rds = file.path(output_dir, paste0(prefix, OBJECTIVE1_PROPENSITY_ARTIFACT_BASENAMES[["audit"]]))
    )

    workbook <- openxlsx::createWorkbook()
    for (sheet in OBJECTIVE1_PROPENSITY_WORKBOOK_SHEETS) {
        openxlsx::addWorksheet(workbook, sheet)
    }
    write_propensity_labeled_table(
        workbook, "analysis_specification", "Analysis specification",
        artifacts$analysis_specification$specification,
        artifacts$workbook_layout$analysis_specification$specification
    )
    write_propensity_labeled_table(
        workbook, "analysis_specification", "Covariate rationale",
        artifacts$analysis_specification$covariate_rationale,
        artifacts$workbook_layout$analysis_specification$covariate_rationale
    )
    write_propensity_labeled_table(
        workbook, "analysis_population", "Cohort flow",
        artifacts$cohort_flow,
        artifacts$workbook_layout$analysis_population$cohort_flow
    )
    write_propensity_labeled_table(
        workbook, "analysis_population", "Population checks",
        artifacts$population_checks,
        artifacts$workbook_layout$analysis_population$population_checks
    )
    write_propensity_labeled_table(
        workbook, "analysis_population", "Endpoint support",
        artifacts$endpoint_support,
        artifacts$workbook_layout$analysis_population$endpoint_support
    )
    write_propensity_labeled_table(
        workbook, "propensity_diagnostics", "Propensity coefficients",
        artifacts$propensity_diagnostics$coefficients,
        artifacts$workbook_layout$propensity_diagnostics$coefficients
    )
    write_propensity_labeled_table(
        workbook, "propensity_diagnostics", "Scores and weights by treatment",
        artifacts$propensity_diagnostics$score_weight_summary,
        artifacts$workbook_layout$propensity_diagnostics$score_weight_summary
    )
    write_propensity_labeled_table(
        workbook, "propensity_diagnostics", "Effective sample size",
        artifacts$propensity_diagnostics$effective_sample_size,
        artifacts$workbook_layout$propensity_diagnostics$effective_sample_size
    )
    openxlsx::writeData(
        workbook, "covariate_balance", artifacts$covariate_balance,
        startRow = 1L, rowNames = FALSE
    )
    openxlsx::writeData(
        workbook, "weighted_cox_results", artifacts$weighted_cox_results,
        startRow = 1L, rowNames = FALSE
    )
    openxlsx::writeData(
        workbook, "ph_diagnostics", artifacts$ph_diagnostics,
        startRow = 1L, rowNames = FALSE
    )
    openxlsx::saveWorkbook(workbook, output_paths[["workbook"]], overwrite = TRUE)

    ggplot2::ggsave(
        output_paths[["propensity_overlap_plot"]], artifacts$plots$overlap,
        width = DEFAULT_PLOT_WIDTH, height = SMALL_PLOT_HEIGHT,
        dpi = PLOT_DPI, bg = "white"
    )
    ggplot2::ggsave(
        output_paths[["covariate_balance_plot"]], artifacts$plots$balance,
        width = DEFAULT_PLOT_WIDTH, height = DEFAULT_PLOT_HEIGHT,
        dpi = PLOT_DPI, bg = "white"
    )
    ggplot2::ggsave(
        output_paths[["results_forest_plot"]], artifacts$plots$forest,
        width = DEFAULT_PLOT_WIDTH, height = SMALL_PLOT_HEIGHT,
        dpi = PLOT_DPI, bg = "white"
    )

    grDevices::png(
        output_paths[["weighted_schoenfeld_plot"]],
        width = DEFAULT_PLOT_WIDTH,
        height = DEFAULT_PLOT_HEIGHT,
        units = PLOT_UNITS,
        res = PLOT_DPI
    )
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit({
        graphics::par(old_par)
        grDevices::dev.off()
    }, add = TRUE)
    graphics::par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
    for (outcome_key in names(OBJECTIVE1_SUBGROUP_OUTCOME_SPECS)) {
        zph <- artifacts$endpoint_results$zph_objects[[outcome_key]]
        if (is.null(zph)) {
            graphics::plot.new()
            graphics::title(main = OBJECTIVE1_SUBGROUP_OUTCOME_SPECS[[outcome_key]]$outcome)
            graphics::text(0.5, 0.5, "Not tested: insufficient events")
        } else {
            p_value <- zph$table["treatment_group", "p"]
            panel_title <- format_propensity_schoenfeld_title(
                OBJECTIVE1_SUBGROUP_OUTCOME_SPECS[[outcome_key]]$outcome,
                p_value
            )
            plot(
                zph,
                var = "treatment_group",
                main = panel_title
            )
            graphics::title(
                main = panel_title,
                col.main = ifelse(p_value < 0.05, "red", "darkgreen")
            )
        }
    }
    graphics::mtext(
        "Overlap-weighted treatment-term Schoenfeld diagnostics",
        outer = TRUE,
        cex = 1.2
    )
    grDevices::dev.off()
    on.exit(NULL, add = FALSE)

    writeLines(
        render_objective1_propensity_summary(artifacts),
        output_paths[["markdown_summary"]]
    )
    saveRDS(
        artifacts$technical_audit,
        output_paths[["technical_audit_rds"]],
        version = 3,
        compress = "xz"
    )

    output_paths
}

#' Run the restricted propensity-overlap sensitivity analysis
#'
#' @param data Objective 0 analytic cohort.
#' @param dataset_name Dataset identifier.
#' @param output_dir Objective 1 propensity output directory.
#' @param prefix Cohort filename prefix.
#' @return Complete analysis result and output paths.
run_objective1_propensity_sensitivity <- function(data, dataset_name, output_dir, prefix) {
    population <- prepare_objective1_propensity_population(data, dataset_name)
    weighted_design <- fit_objective1_propensity_weights(population)
    endpoint_results <- fit_objective1_weighted_endpoints(weighted_design)
    artifacts <- build_objective1_propensity_artifacts(weighted_design, endpoint_results)
    output_paths <- write_objective1_propensity_artifacts(
        artifacts,
        output_dir,
        prefix
    )

    list(
        population = population,
        weighted_design = weighted_design,
        endpoint_results = endpoint_results,
        artifacts = artifacts,
        output_paths = output_paths
    )
}
