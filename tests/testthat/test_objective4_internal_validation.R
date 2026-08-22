test_that("horizon status distinguishes known competing outcomes from early censoring", {
    status <- derive_horizon_status(
        time = c(12, 18, 24, 60),
        event_type = c(1L, 2L, 0L, 0L),
        horizon_months = 60
    )

    expect_identical(status$known_status, c(TRUE, TRUE, FALSE, TRUE))
    expect_identical(status$horizon_event, c(1L, 0L, NA_integer_, 0L))
    expect_equal(status$weight_time, c(12, 18, 24, 60))
    expect_identical(status$use_left_limit, c(TRUE, TRUE, FALSE, FALSE))
})

test_that("fold IPCW payload uses only training rows for censoring estimation", {
    training <- tibble::tibble(
        followup = c(10, 20, 70, 80),
        outcome_type = c(0L, 1L, 0L, 2L)
    )
    assessment <- tibble::tibble(
        followup = c(15, 75),
        outcome_type = c(0L, 1L)
    )
    perturbed_assessment <- assessment
    perturbed_assessment$followup[[1]] <- 45

    payload <- derive_fold_ipcw_payload(
        training = training,
        assessment = assessment,
        time_var = "followup",
        event_type_var = "outcome_type",
        horizon_months = 60
    )
    perturbed_payload <- derive_fold_ipcw_payload(
        training = training,
        assessment = perturbed_assessment,
        time_var = "followup",
        event_type_var = "outcome_type",
        horizon_months = 60
    )

    expect_equal(
        predict_censoring_survival(payload$censoring_fit, c(9.9, 10, 60)),
        c(1, 0.75, 0.75)
    )
    expect_equal(
        predict_censoring_survival(payload$censoring_fit, c(9.9, 10, 60)),
        predict_censoring_survival(perturbed_payload$censoring_fit, c(9.9, 10, 60))
    )
    expect_equal(payload$weight_cap, perturbed_payload$weight_cap)
    expect_equal(payload$training$ipcw_weight, perturbed_payload$training$ipcw_weight)
})

test_that("deterministic folds are keyed to stable IDs rather than input row order", {
    first <- create_deterministic_fold_ids(
        strata = c("event", "event", "event", "nonevent", "nonevent", "nonevent"),
        folds = 3L,
        seed = 20260820L,
        stable_id = c("p03", "p01", "p02", "p06", "p04", "p05")
    )
    reordered <- create_deterministic_fold_ids(
        strata = c("nonevent", "event", "nonevent", "event", "nonevent", "event"),
        folds = 3L,
        seed = 20260820L,
        stable_id = c("p05", "p02", "p06", "p03", "p04", "p01")
    )

    expect_identical(
        first[order(c("p03", "p01", "p02", "p06", "p04", "p05"))],
        reordered[order(c("p05", "p02", "p06", "p03", "p04", "p01"))]
    )
    expect_identical(sort(unique(first)), 1:3)
})

test_that("event-time IPCW uses exact censoring survival immediately before tied time-zero outcomes", {
    payload <- derive_fold_ipcw_payload(
        training = tibble::tibble(
            followup = c(0, 0, 0, 60),
            outcome_type = c(0L, 1L, 2L, 0L)
        ),
        assessment = tibble::tibble(
            followup = c(0, 0),
            outcome_type = c(1L, 2L)
        ),
        time_var = "followup",
        event_type_var = "outcome_type",
        horizon_months = 60
    )

    expect_equal(payload$training$censoring_survival[2:3], c(1, 1))
    expect_equal(payload$assessment$censoring_survival, c(1, 1))
})

test_that("event-time IPCW handles target and competing events closer than the former epsilon", {
    payload <- derive_fold_ipcw_payload(
        training = tibble::tibble(
            followup = c(1e-10, 2e-10, 3e-10, 60),
            outcome_type = c(0L, 1L, 2L, 0L)
        ),
        assessment = tibble::tibble(
            followup = c(2e-10, 3e-10),
            outcome_type = c(1L, 2L)
        ),
        time_var = "followup",
        event_type_var = "outcome_type",
        horizon_months = 60
    )

    expect_equal(payload$training$censoring_survival[2:3], c(0.75, 0.75))
    expect_equal(payload$assessment$censoring_survival, c(0.75, 0.75))
})

test_that("fold IPCW payload normalizes training weights and reuses training scaling for assessment", {
    payload <- derive_fold_ipcw_payload(
        training = tibble::tibble(
            followup = c(rep(5, 67), 10, 20),
            outcome_type = c(rep(1L, 67), 0L, 1L)
        ),
        assessment = tibble::tibble(
            followup = 20,
            outcome_type = 1L
        ),
        time_var = "followup",
        event_type_var = "outcome_type",
        horizon_months = 60
    )

    expect_equal(
        sum(payload$training$ipcw_weight[payload$training$known_status]),
        sum(payload$training$known_status)
    )
    expect_equal(payload$weight_cap, 119 / 60)
    expect_equal(payload$normalization_factor, 4080 / 4139)
    expect_gt(payload$assessment$raw_ipcw_weight[[1]], payload$weight_cap)
    expect_equal(payload$assessment$ipcw_weight[[1]], 8092 / 4139)
})

test_that("fold IPCW payload fails closed for unsupported event types and zero censoring survival", {
    valid_assessment <- tibble::tibble(
        followup = c(20, 60),
        outcome_type = c(1L, 0L)
    )

    expect_error(
        derive_fold_ipcw_payload(
            training = tibble::tibble(
                followup = c(10, 20, 60),
                outcome_type = c(0L, 1.5, 0L)
            ),
            assessment = valid_assessment,
            time_var = "followup",
            event_type_var = "outcome_type",
            horizon_months = 60
        ),
        "event_type must contain only 0L, 1L, or 2L"
    )
    zero_survival_fit <- fit_training_censoring_distribution(
        time = 10,
        event_type = 0L,
        horizon_months = 60
    )
    expect_error(
        apply_training_censoring_weights(
            data = valid_assessment,
            time_var = "followup",
            event_type_var = "outcome_type",
            horizon_months = 60,
            censoring_fit = zero_survival_fit,
            weight_cap = 1,
            normalization_factor = 1
        ),
        "require positive training-derived censoring survival"
    )
})

test_that("IPCW AUC and Brier use literal weighted horizon outcomes", {
    # Weighted case-control concordance is 23.5 / 25 = 0.94:
    # 2 * (1 + 4) + 3 * (0.5 * 1 + 4) = 23.5, with 5 * 5 pair mass.
    # Weighted squared error is 1.48 / 10 = 0.148:
    # (2 * 0.2^2 + 1 * 0.4^2 + 3 * 0.6^2 + 4 * 0.2^2) / (2 + 1 + 3 + 4).
    outcome <- c(1L, 0L, 1L, 0L)
    score <- c(0.8, 0.4, 0.4, 0.2)
    weight <- c(2, 1, 3, 4)

    auc <- calculate_ipcw_auc(outcome, score, weight)
    brier <- calculate_ipcw_brier(outcome, score, weight)

    expect_identical(auc$status, "ok")
    expect_equal(auc$auc, 0.94)
    expect_equal(auc$weighted_cases, 5)
    expect_equal(auc$weighted_controls, 5)
    expect_identical(brier$status, "ok")
    expect_equal(brier$brier, 0.148)
    expect_equal(brier$positive_weight_mass, 10)
})

test_that("zero-weight unknown rows cannot alter IPCW metrics", {
    outcome <- c(1L, 0L, 1L, 0L, NA_integer_)
    score <- c(0.8, 0.4, 0.4, 0.2, NA_real_)
    weight <- c(2, 1, 3, 4, 0)

    auc <- calculate_ipcw_auc(outcome, score, weight)
    brier <- calculate_ipcw_brier(outcome, score, weight)

    expect_identical(auc$status, "ok")
    expect_equal(auc$auc, 0.94)
    expect_identical(brier$status, "ok")
    expect_equal(brier$brier, 0.148)
})

test_that("IPCW metrics explicitly report missing weighted case or control support", {
    no_controls <- calculate_ipcw_auc(
        outcome = c(1L, NA_integer_),
        score = c(0.8, NA_real_),
        weight = c(2, 0)
    )
    no_cases <- calculate_ipcw_brier(
        outcome = c(0L, NA_integer_),
        score = c(0.2, NA_real_),
        weight = c(3, 0)
    )

    expect_identical(no_controls$status, "unsupported_no_weighted_controls")
    expect_true(is.na(no_controls$auc))
    expect_identical(no_cases$status, "unsupported_no_weighted_cases")
    expect_true(is.na(no_cases$brier))
})

test_that("IPCW calibration uses assessment weights and rejects sparse support", {
    # At each prediction value, the weighted event rate equals the prediction:
    # 1 / (4 + 1) = 0.2 and 4 / (1 + 4) = 0.8.  The weighted offset
    # intercept and logit slope are therefore both 0 and 1, respectively.
    outcome <- rep(c(0L, 1L, 0L, 1L), 5)
    predicted <- rep(c(0.2, 0.2, 0.8, 0.8), 5)
    weight <- rep(c(4, 1, 1, 4), 5)

    calibration <- summarize_ipcw_calibration(outcome, predicted, weight)
    sparse <- summarize_ipcw_calibration(
        outcome = c(0L, 1L, NA_integer_),
        predicted = c(0.2, 0.8, NA_real_),
        weight = c(1, 1, 0)
    )

    expect_identical(calibration$status, "ok")
    expect_equal(calibration$intercept, 0, tolerance = 1e-7)
    expect_equal(calibration$slope, 1, tolerance = 1e-7)
    expect_equal(calibration$weighted_cases, 25)
    expect_equal(calibration$weighted_controls, 25)
    expect_identical(sparse$status, "unsupported_sparse_support")
    expect_true(is.na(sparse$intercept))
    expect_true(is.na(sparse$slope))
})
