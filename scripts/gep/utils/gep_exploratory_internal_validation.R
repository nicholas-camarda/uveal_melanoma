# Objective 4 Exploratory Internal Validation Primitives

#' Validate fixed-horizon outcome inputs
#'
#' @param time Numeric observed follow-up in months.
#' @param event_type Integer event types: 0 for censoring/event-free observation,
#'   1 for the target event, and 2 for a competing event.
#' @param horizon_months Positive numeric evaluation horizon in months.
#' @return Invisibly returns `NULL` after validating the shared contract.
validate_horizon_inputs <- function(time, event_type, horizon_months) {
    if (length(time) != length(event_type)) {
        stop("time and event_type must have the same length.", call. = FALSE)
    }
    if (length(time) == 0L) {
        stop("time and event_type must not be empty.", call. = FALSE)
    }
    if (!is.numeric(time) || any(!is.finite(time)) || any(time < 0)) {
        stop("time must contain finite non-negative values.", call. = FALSE)
    }
    if (!is.numeric(event_type) || anyNA(event_type) ||
        any(event_type != as.integer(event_type)) ||
        any(!as.integer(event_type) %in% c(0L, 1L, 2L))) {
        stop("event_type must contain only 0L, 1L, or 2L.", call. = FALSE)
    }
    if (length(horizon_months) != 1L || !is.numeric(horizon_months) ||
        !is.finite(horizon_months) || horizon_months <= 0) {
        stop("horizon_months must be one positive finite number.", call. = FALSE)
    }

    invisible(NULL)
}

#' Derive known fixed-horizon outcome status
#'
#' Target and competing events observed by the horizon have known status. Early
#' loss to observation does not. Event-free observation through the horizon has
#' known non-event status. `weight_time` is the censoring-survival evaluation
#' time; `use_left_limit` marks target and competing events that require the
#' exact Kaplan-Meier left limit at that time.
#'
#' @param time Numeric observed follow-up in months.
#' @param event_type Integer event types: 0 censoring/event-free observation, 1
#'   target event, 2 competing event.
#' @param horizon_months Positive numeric evaluation horizon in months.
#' @return A data frame with `horizon_event`, `known_status`, `weight_time`, and
#'   `use_left_limit`.
derive_horizon_status <- function(time, event_type, horizon_months) {
    validate_horizon_inputs(time, event_type, horizon_months)
    event_type <- as.integer(event_type)
    event_before_or_at_horizon <- event_type %in% c(1L, 2L) & time <= horizon_months
    known_status <- event_before_or_at_horizon | time >= horizon_months
    horizon_event <- rep(NA_integer_, length(time))
    horizon_event[known_status] <- 0L
    horizon_event[event_type == 1L & time <= horizon_months] <- 1L
    weight_time <- pmin(time, horizon_months)

    data.frame(
        horizon_event = horizon_event,
        known_status = known_status,
        weight_time = as.numeric(weight_time),
        use_left_limit = event_before_or_at_horizon
    )
}

#' Fit a censoring distribution on outer-training rows
#'
#' Target and competing outcomes are retained as observed outcomes rather than
#' censoring events. Only type-0 observations before the fixed horizon are
#' censoring events in the Kaplan-Meier fit.
#'
#' @param time Numeric observed follow-up in months from the outer-training set.
#' @param event_type Integer event types: 0 censoring/event-free observation, 1
#'   target event, 2 competing event.
#' @param horizon_months Positive numeric evaluation horizon in months.
#' @return A `survival::survfit` censoring Kaplan-Meier fit.
fit_training_censoring_distribution <- function(time, event_type, horizon_months) {
    validate_horizon_inputs(time, event_type, horizon_months)
    censoring_event <- as.integer(event_type == 0L & time < horizon_months)
    censoring_time <- pmin(time, horizon_months)
    fit <- survival::survfit(survival::Surv(censoring_time, censoring_event) ~ 1)
    attr(fit, "horizon_months") <- horizon_months
    fit
}

#' Predict censoring survival from a fitted outer-training distribution
#'
#' @param censoring_fit A `survival::survfit` object produced by
#'   `fit_training_censoring_distribution()`.
#' @param times Numeric non-negative times at which to evaluate survival.
#' @param left_limit Logical scalar or vector aligned to `times`. When `TRUE`,
#'   evaluates the exact Kaplan-Meier survival immediately before that time.
#' @return Numeric censoring-survival probabilities aligned to `times`.
predict_censoring_survival <- function(censoring_fit, times, left_limit = FALSE) {
    if (!inherits(censoring_fit, "survfit")) {
        stop("censoring_fit must be a survival::survfit object.", call. = FALSE)
    }
    if (!is.numeric(times) || any(!is.finite(times)) || any(times < 0)) {
        stop("times must contain finite non-negative values.", call. = FALSE)
    }
    if (!is.logical(left_limit) || anyNA(left_limit) ||
        !(length(left_limit) %in% c(1L, length(times)))) {
        stop("left_limit must be a non-missing logical scalar or align to times.", call. = FALSE)
    }
    if (length(left_limit) == 1L) {
        left_limit <- rep(left_limit, length(times))
    }

    censoring_grid <- censoring_fit$n.event > 0
    censoring_times <- censoring_fit$time[censoring_grid]
    censoring_survival <- censoring_fit$surv[censoring_grid]
    predicted <- vapply(seq_along(times), function(index) {
        eligible <- if (left_limit[[index]]) {
            which(censoring_times < times[[index]])
        } else {
            which(censoring_times <= times[[index]])
        }
        if (length(eligible) == 0L) {
            return(1)
        }
        censoring_survival[[max(eligible)]]
    }, numeric(1))

    predicted
}

#' Apply a censoring fit to one split without refitting
#'
#' @param data Data frame containing time and event-type variables.
#' @param time_var Character name of observed-time column.
#' @param event_type_var Character name of event-type column.
#' @param horizon_months Positive numeric evaluation horizon in months.
#' @param censoring_fit Training-only censoring fit.
#' @param weight_cap Positive cap derived from the outer-training weights.
#' @param normalization_factor Positive multiplier derived from outer-training
#'   weights.
#' @return `data` augmented with horizon and IPCW fields.
apply_training_censoring_weights <- function(
    data,
    time_var,
    event_type_var,
    horizon_months,
    censoring_fit,
    weight_cap,
    normalization_factor
) {
    if (!is.data.frame(data) || !all(c(time_var, event_type_var) %in% names(data))) {
        stop("data must contain the declared time and event-type columns.", call. = FALSE)
    }
    if (length(weight_cap) != 1L || !is.finite(weight_cap) || weight_cap <= 0 ||
        length(normalization_factor) != 1L || !is.finite(normalization_factor) || normalization_factor <= 0) {
        stop("weight_cap and normalization_factor must be positive finite numbers.", call. = FALSE)
    }

    status <- derive_horizon_status(
        time = data[[time_var]],
        event_type = data[[event_type_var]],
        horizon_months = horizon_months
    )
    censoring_survival <- predict_censoring_survival(
        censoring_fit,
        status$weight_time,
        left_limit = status$use_left_limit
    )
    known_survival <- censoring_survival[status$known_status]
    if (any(known_survival <= 0)) {
        stop("Observed horizon outcomes require positive training-derived censoring survival.", call. = FALSE)
    }

    raw_ipcw_weight <- numeric(nrow(data))
    raw_ipcw_weight[status$known_status] <- 1 / known_survival
    ipcw_weight <- pmin(raw_ipcw_weight, weight_cap)
    ipcw_weight[ipcw_weight > 0] <-
        ipcw_weight[ipcw_weight > 0] * normalization_factor

    data$horizon_event <- status$horizon_event
    data$known_status <- status$known_status
    data$weight_time <- status$weight_time
    data$censoring_survival <- censoring_survival
    data$raw_ipcw_weight <- raw_ipcw_weight
    data$ipcw_weight <- ipcw_weight
    data
}

#' Derive fold-local IPCW payloads from outer-training censoring information
#'
#' @param training Outer-training data frame.
#' @param assessment Outer-assessment data frame.
#' @param time_var Character name of observed-time column.
#' @param event_type_var Character name of event-type column.
#' @param horizon_months Positive numeric evaluation horizon in months.
#' @return A list containing a training-only censoring fit, its cap and scaling
#'   factor, and augmented training and assessment data frames.
derive_fold_ipcw_payload <- function(
    training,
    assessment,
    time_var,
    event_type_var,
    horizon_months
) {
    if (!is.data.frame(training) || !is.data.frame(assessment) ||
        !all(c(time_var, event_type_var) %in% names(training)) ||
        !all(c(time_var, event_type_var) %in% names(assessment))) {
        stop("training and assessment must contain the declared time and event-type columns.", call. = FALSE)
    }
    validate_horizon_inputs(training[[time_var]], training[[event_type_var]], horizon_months)
    validate_horizon_inputs(assessment[[time_var]], assessment[[event_type_var]], horizon_months)
    if (!exists("GEP_IPCW_WEIGHT_CAP_PROB", inherits = TRUE) ||
        !is.numeric(GEP_IPCW_WEIGHT_CAP_PROB) || length(GEP_IPCW_WEIGHT_CAP_PROB) != 1L ||
        !is.finite(GEP_IPCW_WEIGHT_CAP_PROB) || GEP_IPCW_WEIGHT_CAP_PROB <= 0 ||
        GEP_IPCW_WEIGHT_CAP_PROB > 1) {
        stop("GEP_IPCW_WEIGHT_CAP_PROB must be a probability in (0, 1].", call. = FALSE)
    }

    censoring_fit <- fit_training_censoring_distribution(
        time = training[[time_var]],
        event_type = training[[event_type_var]],
        horizon_months = horizon_months
    )
    training_status <- derive_horizon_status(
        time = training[[time_var]],
        event_type = training[[event_type_var]],
        horizon_months = horizon_months
    )
    training_survival <- predict_censoring_survival(
        censoring_fit,
        training_status$weight_time,
        left_limit = training_status$use_left_limit
    )
    known_survival <- training_survival[training_status$known_status]
    if (length(known_survival) == 0L || any(known_survival <= 0)) {
        stop("Outer-training rows do not support finite IPCW weights at this horizon.", call. = FALSE)
    }

    training_raw_weights <- 1 / known_survival
    weight_cap <- as.numeric(stats::quantile(
        training_raw_weights,
        probs = GEP_IPCW_WEIGHT_CAP_PROB,
        names = FALSE,
        type = 8
    ))
    capped_training_weights <- pmin(training_raw_weights, weight_cap)
    normalization_factor <- length(training_raw_weights) / sum(capped_training_weights)

    training_payload <- apply_training_censoring_weights(
        data = training,
        time_var = time_var,
        event_type_var = event_type_var,
        horizon_months = horizon_months,
        censoring_fit = censoring_fit,
        weight_cap = weight_cap,
        normalization_factor = normalization_factor
    )
    assessment_payload <- apply_training_censoring_weights(
        data = assessment,
        time_var = time_var,
        event_type_var = event_type_var,
        horizon_months = horizon_months,
        censoring_fit = censoring_fit,
        weight_cap = weight_cap,
        normalization_factor = normalization_factor
    )

    list(
        censoring_fit = censoring_fit,
        weight_cap = weight_cap,
        normalization_factor = normalization_factor,
        training = training_payload,
        assessment = assessment_payload
    )
}

#' Create stable deterministic cross-validation fold identifiers
#'
#' @param strata Vector used to balance fold assignments.
#' @param folds Integer number of folds.
#' @param seed Integer seed for the deterministic assignment.
#' @param stable_id Unique non-missing row keys.
#' @return Integer fold IDs aligned to the input row order.
create_deterministic_fold_ids <- function(strata, folds, seed, stable_id) {
    if (length(strata) != length(stable_id) || length(strata) == 0L) {
        stop("strata and stable_id must have the same non-zero length.", call. = FALSE)
    }
    if (anyNA(strata) || anyNA(stable_id) || anyDuplicated(stable_id)) {
        stop("strata must be observed and stable_id must be unique and observed.", call. = FALSE)
    }
    if (length(folds) != 1L || !is.finite(folds) || folds < 2L || folds != as.integer(folds)) {
        stop("folds must be an integer of at least 2.", call. = FALSE)
    }
    if (length(seed) != 1L || !is.finite(seed) || seed != as.integer(seed)) {
        stop("seed must be one finite integer.", call. = FALSE)
    }

    strata <- as.character(strata)
    stable_id <- as.character(stable_id)
    strata_sizes <- table(strata)
    if (any(strata_sizes < folds)) {
        stop("Every stratum must contain at least folds rows.", call. = FALSE)
    }

    random_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (random_seed_exists) {
        previous_random_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
    on.exit({
        if (random_seed_exists) {
            assign(".Random.seed", previous_random_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
            rm(".Random.seed", envir = .GlobalEnv)
        }
    }, add = TRUE)
    set.seed(as.integer(seed))

    fold_id <- integer(length(strata))
    for (stratum in sort(unique(strata))) {
        stratum_rows <- which(strata == stratum)
        keyed_rows <- stratum_rows[order(stable_id[stratum_rows], method = "radix")]
        fold_id[keyed_rows] <- sample(rep(seq_len(as.integer(folds)), length.out = length(keyed_rows)))
    }

    fold_id
}

#' Validate and retain rows with positive assessment IPCW weight
#'
#' Rows with zero weight have unknown horizon status and must have no influence
#' on weighted out-of-fold performance summaries.
#'
#' @param outcome Binary fixed-horizon outcome vector.
#' @param score Numeric score or predicted risk vector.
#' @param weight Non-negative assessment IPCW weight vector.
#' @return A data frame containing only rows with positive weight.
prepare_positive_ipcw_rows <- function(outcome, score, weight) {
    if (length(outcome) != length(score) || length(outcome) != length(weight)) {
        stop("outcome, score, and weight must have the same length.", call. = FALSE)
    }
    if (!is.numeric(weight) || any(!is.finite(weight)) || any(weight < 0)) {
        stop("weight must contain finite non-negative values.", call. = FALSE)
    }

    positive_weight <- weight > 0
    outcome <- outcome[positive_weight]
    score <- score[positive_weight]
    weight <- weight[positive_weight]

    if (length(outcome) > 0L &&
        (!is.numeric(outcome) || anyNA(outcome) ||
            any(outcome != as.integer(outcome)) ||
            any(!as.integer(outcome) %in% c(0L, 1L)))) {
        stop("Positive-weight outcomes must contain only 0L or 1L.", call. = FALSE)
    }
    if (length(score) > 0L && (!is.numeric(score) || any(!is.finite(score)))) {
        stop("Positive-weight scores must contain finite numeric values.", call. = FALSE)
    }

    data.frame(
        outcome = as.integer(outcome),
        score = as.numeric(score),
        weight = as.numeric(weight)
    )
}

#' Describe weighted case-control support
#'
#' @param data Positive-weight binary horizon data.
#' @return A named list with support status and weighted case-control mass.
summarize_ipcw_case_control_support <- function(data) {
    positive_weight_mass <- sum(data$weight)
    weighted_cases <- sum(data$weight[data$outcome == 1L])
    weighted_controls <- sum(data$weight[data$outcome == 0L])
    status <- if (positive_weight_mass == 0) {
        "unsupported_no_positive_weight"
    } else if (weighted_cases == 0) {
        "unsupported_no_weighted_cases"
    } else if (weighted_controls == 0) {
        "unsupported_no_weighted_controls"
    } else {
        "ok"
    }

    list(
        status = status,
        positive_weight_mass = positive_weight_mass,
        weighted_cases = weighted_cases,
        weighted_controls = weighted_controls,
        n_positive_weight = nrow(data)
    )
}

#' Calculate a censoring-aware weighted case-control AUC
#'
#' Every positive-weight case-control pair contributes its product weight to the
#' denominator. Concordant pairs receive full credit and tied scores receive
#' half credit. Rows with zero IPCW weight are excluded before input validation.
#'
#' @param outcome Binary fixed-horizon outcome vector.
#' @param score Numeric prediction scores, where larger values rank higher risk.
#' @param weight Non-negative assessment IPCW weight vector.
#' @return Named list with `status`, `auc`, and weighted support fields.
calculate_ipcw_auc <- function(outcome, score, weight) {
    data <- prepare_positive_ipcw_rows(outcome, score, weight)
    support <- summarize_ipcw_case_control_support(data)
    if (support$status != "ok") {
        return(c(support, list(auc = NA_real_)))
    }

    cases <- data[data$outcome == 1L, , drop = FALSE]
    controls <- data[data$outcome == 0L, , drop = FALSE]
    comparison <- outer(cases$score, controls$score, FUN = "-")
    pair_weight <- outer(cases$weight, controls$weight, FUN = "*")
    concordance_credit <- (comparison > 0) + 0.5 * (comparison == 0)

    c(
        support,
        list(auc = sum(pair_weight * concordance_credit) / (support$weighted_cases * support$weighted_controls))
    )
}

#' Calculate a censoring-aware weighted Brier score
#'
#' The score is the weighted squared error divided by positive assessment IPCW
#' mass. Case-control support is required so an unsupported validation scope is
#' never represented by a substitute scalar estimate.
#'
#' @param outcome Binary fixed-horizon outcome vector.
#' @param score Predicted probabilities in the closed unit interval.
#' @param weight Non-negative assessment IPCW weight vector.
#' @return Named list with `status`, `brier`, and weighted support fields.
calculate_ipcw_brier <- function(outcome, score, weight) {
    data <- prepare_positive_ipcw_rows(outcome, score, weight)
    if (any(data$score < 0 | data$score > 1)) {
        stop("Positive-weight Brier scores must be probabilities from 0 to 1.", call. = FALSE)
    }
    support <- summarize_ipcw_case_control_support(data)
    if (support$status != "ok") {
        return(c(support, list(brier = NA_real_)))
    }

    c(
        support,
        list(brier = sum(data$weight * (data$outcome - data$score)^2) / support$positive_weight_mass)
    )
}

#' Extract one stable coefficient from a weighted binomial calibration fit
#'
#' @param fit A fitted `stats::glm` object or `NULL` after a failed fit.
#' @param coefficient_index One-based coefficient index.
#' @return Estimate, standard error, convergence, and stability fields.
extract_stable_calibration_coefficient <- function(fit, coefficient_index) {
    if (is.null(fit) || !isTRUE(fit$converged)) {
        return(list(
            estimate = NA_real_,
            standard_error = NA_real_,
            converged = FALSE,
            stable = FALSE
        ))
    }

    coefficients <- tryCatch(stats::coef(fit), error = function(e) NULL)
    coefficient_summary <- tryCatch(summary(fit)$coefficients, error = function(e) NULL)
    has_standard_error <- !is.null(coefficient_summary) &&
        nrow(coefficient_summary) >= coefficient_index &&
        "Std. Error" %in% colnames(coefficient_summary)
    estimate <- if (!is.null(coefficients) && length(coefficients) >= coefficient_index) {
        unname(coefficients[[coefficient_index]])
    } else {
        NA_real_
    }
    standard_error <- if (has_standard_error) {
        unname(coefficient_summary[coefficient_index, "Std. Error"])
    } else {
        NA_real_
    }
    stable <- is.finite(estimate) &&
        is.finite(standard_error) &&
        abs(estimate) <= GEP_MAX_CALIBRATION_COEF_ABS &&
        standard_error <= GEP_MAX_CALIBRATION_COEF_SE

    list(
        estimate = estimate,
        standard_error = standard_error,
        converged = TRUE,
        stable = stable
    )
}

#' Summarize IPCW-weighted fixed-horizon calibration
#'
#' The intercept is estimated from a binomial logistic model with the prediction
#' logit as offset; the slope is estimated from a binomial logistic model using
#' that logit as predictor. Both models use outer-assessment IPCW weights.
#'
#' @param outcome Binary fixed-horizon outcome vector.
#' @param predicted Predicted probabilities strictly between zero and one.
#' @param weight Non-negative assessment IPCW weight vector.
#' @return Named list with status, intercept, slope, and weighted support.
summarize_ipcw_calibration <- function(outcome, predicted, weight) {
    data <- prepare_positive_ipcw_rows(outcome, predicted, weight)
    if (any(data$score <= 0 | data$score >= 1)) {
        stop("Positive-weight calibration predictions must be strictly between 0 and 1.", call. = FALSE)
    }
    support <- summarize_ipcw_case_control_support(data)
    event_rows <- sum(data$outcome == 1L)
    control_rows <- sum(data$outcome == 0L)
    unique_prediction_count <- length(unique(data$score))

    sparse_support <- support$status != "ok" ||
        nrow(data) < GEP_MIN_SAMPLE_SIZE ||
        event_rows < GEP_MIN_CALIBRATION_EVENTS ||
        control_rows < GEP_MIN_CALIBRATION_EVENTS ||
        unique_prediction_count < 2L
    if (sparse_support) {
        support$status <- "unsupported_sparse_support"
        return(c(
            support,
            list(
                intercept = NA_real_,
                slope = NA_real_,
                event_rows = event_rows,
                control_rows = control_rows,
                unique_prediction_count = unique_prediction_count
            )
        ))
    }

    calibration_data <- data.frame(
        outcome = data$outcome,
        logit_predicted = stats::qlogis(data$score),
        weight = data$weight
    )
    intercept_fit <- tryCatch(
        suppressWarnings(stats::glm(
            outcome ~ offset(logit_predicted),
            data = calibration_data,
            weights = weight,
            family = stats::binomial()
        )),
        error = function(e) NULL
    )
    slope_fit <- tryCatch(
        suppressWarnings(stats::glm(
            outcome ~ logit_predicted,
            data = calibration_data,
            weights = weight,
            family = stats::binomial()
        )),
        error = function(e) NULL
    )
    intercept_details <- extract_stable_calibration_coefficient(intercept_fit, 1L)
    slope_details <- extract_stable_calibration_coefficient(slope_fit, 2L)
    all_converged <- intercept_details$converged && slope_details$converged
    all_stable <- intercept_details$stable && slope_details$stable
    status <- if (!all_converged) {
        "unsupported_calibration_nonconverged"
    } else if (!all_stable) {
        "recalibration_fit_unstable"
    } else {
        "ok"
    }
    support$status <- status

    c(
        support,
        list(
            intercept = if (status == "ok") intercept_details$estimate else NA_real_,
            slope = if (status == "ok") slope_details$estimate else NA_real_,
            intercept_se = intercept_details$standard_error,
            slope_se = slope_details$standard_error,
            event_rows = event_rows,
            control_rows = control_rows,
            unique_prediction_count = unique_prediction_count
        )
    )
}

#' Build and align a design matrix for nested ridge validation
#'
#' @param data Predictor data.
#' @param predictors Character predictor names.
#' @param reference_columns Optional design columns from the training split.
#' @return A numeric, intercept-free design matrix.
build_horizon_ridge_design_matrix <- function(data, predictors, reference_columns = NULL) {
    formula <- stats::reformulate(predictors)
    design <- stats::model.matrix(formula, data = data)
    design <- design[, colnames(design) != "(Intercept)", drop = FALSE]

    if (!is.null(reference_columns)) {
        missing_columns <- setdiff(reference_columns, colnames(design))
        if (length(missing_columns) > 0L) {
            missing_block <- matrix(
                0,
                nrow = nrow(design),
                ncol = length(missing_columns),
                dimnames = list(NULL, missing_columns)
            )
            design <- cbind(design, missing_block)
        }
        design <- design[, reference_columns, drop = FALSE]
    }

    if (ncol(design) == 0L || any(!is.finite(design))) {
        stop("The retained predictors must produce a finite non-empty design matrix.", call. = FALSE)
    }
    design
}

#' Stop with deterministic nested-CV split context
#'
#' @param message Failure message.
#' @param repeat_id Repeat number, or `NULL` for the final fit.
#' @param outer_fold Outer-fold number, or `NULL` for the final fit.
#' @return Does not return.
stop_nested_cv_context <- function(message, repeat_id = NULL, outer_fold = NULL) {
    context <- if (is.null(repeat_id)) {
        "final full-data fit"
    } else {
        sprintf("repeat %d, outer fold %d", repeat_id, outer_fold)
    }
    stop(sprintf("Nested horizon ridge failed at %s: %s", context, message), call. = FALSE)
}

#' Fit one deterministic weighted ridge model
#'
#' @param training_payload Outer-training data with horizon status and IPCW.
#' @param predictors Character predictor names.
#' @param stable_id_var Stable key column.
#' @param inner_folds Requested deterministic tuning folds.
#' @param seed Split-specific integer seed.
#' @param repeat_id Optional repeat context.
#' @param outer_fold Optional outer-fold context.
#' @return A list containing the fitted model, design columns, fold IDs, and
#'   positive-weight training rows.
fit_weighted_horizon_ridge <- function(
    training_payload,
    predictors,
    stable_id_var,
    inner_folds,
    seed,
    repeat_id = NULL,
    outer_fold = NULL
) {
    fit_rows <- training_payload$known_status &
        training_payload$ipcw_weight > 0 &
        !is.na(training_payload$horizon_event) &
        stats::complete.cases(training_payload[, predictors, drop = FALSE])
    fit_data <- training_payload[fit_rows, , drop = FALSE]
    outcome <- fit_data$horizon_event

    if (nrow(fit_data) < GEP_MIN_SAMPLE_SIZE) {
        stop_nested_cv_context(
            sprintf("only %d positive-weight complete training rows were available", nrow(fit_data)),
            repeat_id,
            outer_fold
        )
    }
    if (length(unique(outcome)) != 2L) {
        stop_nested_cv_context("outer training did not contain both horizon outcome classes", repeat_id, outer_fold)
    }

    inner_foldid <- tryCatch(
        create_deterministic_fold_ids(
            strata = outcome,
            folds = inner_folds,
            seed = seed,
            stable_id = fit_data[[stable_id_var]]
        ),
        error = function(error) {
            stop_nested_cv_context(
                paste("inner-fold assignment was infeasible:", conditionMessage(error)),
                repeat_id,
                outer_fold
            )
        }
    )
    design <- build_horizon_ridge_design_matrix(fit_data, predictors)
    fitted <- tryCatch(
        suppressWarnings(glmnet::cv.glmnet(
            x = design,
            y = outcome,
            family = "binomial",
            alpha = 0,
            foldid = inner_foldid,
            weights = fit_data$ipcw_weight,
            standardize = TRUE,
            type.measure = "deviance"
        )),
        error = function(error) {
            stop_nested_cv_context(conditionMessage(error), repeat_id, outer_fold)
        }
    )

    list(
        model = fitted,
        design_columns = colnames(design),
        foldid = inner_foldid,
        fit_data = fit_data
    )
}

#' Repeated deterministic nested cross-validation for a horizon ridge model
#'
#' Censoring and IPCW estimation are confined to each outer-training split.
#' Ridge tuning uses explicitly supplied deterministic inner fold IDs. Every
#' complete-predictor assessment row receives an out-of-fold prediction; its
#' evaluation outcome and weight are derived from the corresponding
#' outer-training censoring model.
#'
#' @param data Analysis data.
#' @param predictors Character predictor names.
#' @param time_var Observed-time column.
#' @param event_type_var Event-type column using 0/1/2 coding.
#' @param horizon_months Fixed prediction horizon.
#' @param stable_id_var Unique stable key column.
#' @param seed Configured base seed.
#' @param repeats Number of outer partition repeats.
#' @param outer_folds Number of outer folds.
#' @param inner_folds Number of inner tuning folds.
#' @return A list with keyed OOF predictions, split metadata, row-level training
#'   IPCW records, repeated OOF metrics, and a deterministic full-data fit for
#'   coefficients and future scoring only.
cross_validate_horizon_ridge <- function(
    data,
    predictors,
    time_var,
    event_type_var,
    horizon_months,
    stable_id_var,
    seed = GEP_EXPLORATORY_CV_SEED,
    repeats = GEP_EXPLORATORY_CV_REPEATS,
    outer_folds = GEP_EXPLORATORY_OUTER_FOLDS,
    inner_folds = GEP_EXPLORATORY_INNER_FOLDS
) {
    required <- unique(c(predictors, time_var, event_type_var, stable_id_var))
    if (!is.data.frame(data) || length(predictors) == 0L || any(!required %in% names(data))) {
        stop("data must contain the declared predictors, time, event type, and stable ID.", call. = FALSE)
    }
    if (anyNA(data[[stable_id_var]]) || anyDuplicated(data[[stable_id_var]])) {
        stop("stable_id_var must be unique and observed.", call. = FALSE)
    }
    validate_horizon_inputs(data[[time_var]], data[[event_type_var]], horizon_months)
    for (value in list(seed = seed, repeats = repeats, outer_folds = outer_folds, inner_folds = inner_folds)) {
        if (length(value) != 1L || !is.finite(value) || value != as.integer(value)) {
            stop("seed, repeats, outer_folds, and inner_folds must be finite integers.", call. = FALSE)
        }
    }
    if (repeats < 1L || outer_folds < 2L || inner_folds < 2L) {
        stop("repeats must be positive and both fold counts must be at least 2.", call. = FALSE)
    }

    complete_predictors <- stats::complete.cases(data[, predictors, drop = FALSE])
    model_data <- data[complete_predictors, , drop = FALSE]
    if (nrow(model_data) < GEP_MIN_SAMPLE_SIZE) {
        stop("Too few complete-predictor rows for nested horizon validation.", call. = FALSE)
    }
    stable_id <- as.character(model_data[[stable_id_var]])
    outer_strata <- ifelse(model_data[[event_type_var]] == 1L, "target", "other")

    predictions <- list()
    metadata <- list()
    training_records <- list()
    prediction_index <- 0L
    metadata_index <- 0L
    training_index <- 0L

    for (repeat_id in seq_len(repeats)) {
        outer_seed <- as.integer(seed + repeat_id - 1L)
        outer_foldid <- tryCatch(
            create_deterministic_fold_ids(
                strata = outer_strata,
                folds = outer_folds,
                seed = outer_seed,
                stable_id = stable_id
            ),
            error = function(error) {
                stop(sprintf(
                    "Nested horizon ridge failed at repeat %d, outer fold assignment: %s",
                    repeat_id,
                    conditionMessage(error)
                ), call. = FALSE)
            }
        )

        for (outer_fold in seq_len(outer_folds)) {
            training <- model_data[outer_foldid != outer_fold, , drop = FALSE]
            assessment <- model_data[outer_foldid == outer_fold, , drop = FALSE]
            payload <- tryCatch(
                derive_fold_ipcw_payload(
                    training = training,
                    assessment = assessment,
                    time_var = time_var,
                    event_type_var = event_type_var,
                    horizon_months = horizon_months
                ),
                error = function(error) {
                    stop_nested_cv_context(conditionMessage(error), repeat_id, outer_fold)
                }
            )
            inner_seed <- as.integer(seed + repeat_id * 1000L + outer_fold)
            ridge <- fit_weighted_horizon_ridge(
                training_payload = payload$training,
                predictors = predictors,
                stable_id_var = stable_id_var,
                inner_folds = inner_folds,
                seed = inner_seed,
                repeat_id = repeat_id,
                outer_fold = outer_fold
            )
            assessment_design <- build_horizon_ridge_design_matrix(
                payload$assessment,
                predictors,
                reference_columns = ridge$design_columns
            )
            assessment_prediction <- tryCatch(
                as.numeric(stats::predict(
                    ridge$model,
                    newx = assessment_design,
                    s = "lambda.min",
                    type = "response"
                )),
                error = function(error) {
                    stop_nested_cv_context(conditionMessage(error), repeat_id, outer_fold)
                }
            )
            if (length(assessment_prediction) != nrow(assessment) || any(!is.finite(assessment_prediction))) {
                stop_nested_cv_context("assessment predictions were incomplete", repeat_id, outer_fold)
            }

            prediction_index <- prediction_index + 1L
            predictions[[prediction_index]] <- data.frame(
                stable_id = as.character(assessment[[stable_id_var]]),
                repeat_id = as.integer(repeat_id),
                outer_fold = as.integer(outer_fold),
                prediction = assessment_prediction,
                horizon_event = payload$assessment$horizon_event,
                known_status = payload$assessment$known_status,
                ipcw_weight = payload$assessment$ipcw_weight,
                stringsAsFactors = FALSE
            )
            metadata_index <- metadata_index + 1L
            metadata[[metadata_index]] <- data.frame(
                repeat_id = as.integer(repeat_id),
                outer_fold = as.integer(outer_fold),
                outer_seed = outer_seed,
                inner_seed = inner_seed,
                training_n = nrow(training),
                training_known_n = sum(payload$training$known_status),
                assessment_n = nrow(assessment),
                lambda_min = ridge$model$lambda.min,
                lambda_1se = ridge$model$lambda.1se,
                training_weight_sum = sum(payload$training$ipcw_weight),
                training_weight_cap = payload$weight_cap,
                training_normalization_factor = payload$normalization_factor
            )
            training_index <- training_index + 1L
            training_records[[training_index]] <- data.frame(
                stable_id = as.character(payload$training[[stable_id_var]]),
                repeat_id = as.integer(repeat_id),
                outer_fold = as.integer(outer_fold),
                horizon_event = payload$training$horizon_event,
                known_status = payload$training$known_status,
                ipcw_weight = payload$training$ipcw_weight,
                stringsAsFactors = FALSE
            )
        }
    }

    oof_predictions <- do.call(rbind, predictions)
    fold_metadata <- do.call(rbind, metadata)
    training_ipcw <- do.call(rbind, training_records)
    oof_predictions <- oof_predictions[
        order(oof_predictions$repeat_id, oof_predictions$outer_fold, oof_predictions$stable_id, method = "radix"),
        ,
        drop = FALSE
    ]
    fold_metadata <- fold_metadata[
        order(fold_metadata$repeat_id, fold_metadata$outer_fold, method = "radix"),
        ,
        drop = FALSE
    ]
    training_ipcw <- training_ipcw[
        order(training_ipcw$repeat_id, training_ipcw$outer_fold, training_ipcw$stable_id, method = "radix"),
        ,
        drop = FALSE
    ]
    rownames(oof_predictions) <- NULL
    rownames(fold_metadata) <- NULL
    rownames(training_ipcw) <- NULL
    repeated_metrics <- do.call(rbind, lapply(seq_len(repeats), function(repeat_id) {
        repeat_data <- oof_predictions[oof_predictions$repeat_id == repeat_id, , drop = FALSE]
        auc <- calculate_ipcw_auc(repeat_data$horizon_event, repeat_data$prediction, repeat_data$ipcw_weight)
        brier <- calculate_ipcw_brier(repeat_data$horizon_event, repeat_data$prediction, repeat_data$ipcw_weight)
        calibration <- summarize_ipcw_calibration(
            repeat_data$horizon_event,
            pmin(pmax(repeat_data$prediction, 1e-6), 1 - 1e-6),
            repeat_data$ipcw_weight
        )
        data.frame(
            repeat_id = as.integer(repeat_id),
            performance_scope = "Overall",
            auc_status = auc$status,
            cv_auc = auc$auc,
            brier_status = brier$status,
            cv_brier = brier$brier,
            calibration_status = calibration$status,
            cv_calibration_intercept = calibration$intercept,
            cv_calibration_slope = calibration$slope,
            n_positive_weight = auc$n_positive_weight,
            weighted_cases = auc$weighted_cases,
            weighted_controls = auc$weighted_controls,
            stringsAsFactors = FALSE
        )
    }))

    full_payload <- derive_fold_ipcw_payload(
        training = model_data,
        assessment = model_data,
        time_var = time_var,
        event_type_var = event_type_var,
        horizon_months = horizon_months
    )
    final_ridge <- fit_weighted_horizon_ridge(
        training_payload = full_payload$training,
        predictors = predictors,
        stable_id_var = stable_id_var,
        inner_folds = inner_folds,
        seed = as.integer(seed + repeats * 1000L + outer_folds + 1L)
    )

    list(
        oof_predictions = oof_predictions,
        fold_metadata = fold_metadata,
        training_ipcw = training_ipcw,
        repeated_metrics = repeated_metrics,
        final_model = final_ridge$model,
        final_design_columns = final_ridge$design_columns,
        final_foldid = final_ridge$foldid,
        final_stable_id = as.character(final_ridge$fit_data[[stable_id_var]]),
        seed = as.integer(seed),
        repeats = as.integer(repeats),
        outer_folds = as.integer(outer_folds),
        inner_folds = as.integer(inner_folds)
    )
}
