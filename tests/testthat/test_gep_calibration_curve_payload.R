# Validate that the survival calibration summary exposes plot-ready curve data.

test_that("Survival calibration summary includes plot-ready curve payload", {
    set.seed(20260323)

    n <- 220
    predicted_risk <- stats::runif(n, min = 0.02, max = 0.60)

    # Simulate event and censoring times in months with higher hazards at higher predicted risk.
    hazard_rate <- 0.006 + 0.045 * predicted_risk
    event_time <- stats::rexp(n, rate = hazard_rate)
    censor_time <- stats::runif(n, min = 12, max = 120)

    observed_time <- pmin(event_time, censor_time)
    observed_event <- as.integer(event_time <= censor_time)

    data <- data.frame(
        predicted_risk = predicted_risk,
        observed_time = observed_time,
        observed_event = observed_event,
        stringsAsFactors = FALSE
    )

    res <- calculate_survival_calibration_summary(
        data = data,
        predicted_risk_var = "predicted_risk",
        time_var = "observed_time",
        event_var = "observed_event",
        eval_time_months = 60
    )

    expect_true(is.list(res$curve))
    expect_true(all(c("bins", "smooth", "curve_method") %in% names(res$curve)))

    bins <- res$curve$bins
    expect_s3_class(bins, "data.frame")
    expect_true(all(c(
        "timepoint_months",
        "risk_bin",
        "n",
        "mean_predicted_risk",
        "observed_risk_km",
        "km_survival_se"
    ) %in% names(bins)))

    expect_true(all(bins$mean_predicted_risk >= 0 & bins$mean_predicted_risk <= 1, na.rm = TRUE))
    expect_true(all(bins$observed_risk_km >= 0 & bins$observed_risk_km <= 1, na.rm = TRUE))

    smooth <- res$curve$smooth
    if (!is.null(smooth)) {
        expect_s3_class(smooth, "data.frame")
        expect_true(all(c("predicted_risk", "observed_risk_ipcw_spline") %in% names(smooth)))
        expect_true(all(smooth$predicted_risk >= 0 & smooth$predicted_risk <= 1, na.rm = TRUE))
        expect_true(all(smooth$observed_risk_ipcw_spline >= 0 & smooth$observed_risk_ipcw_spline <= 1, na.rm = TRUE))
    }

    expect_true(res$curve$curve_method %in% c("ipcw_logistic_spline", "bins_only_fallback"))
})
