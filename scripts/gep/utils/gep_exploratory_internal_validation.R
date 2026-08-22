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
