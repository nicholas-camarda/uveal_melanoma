test_that("general treatment-duration summary exports quartile endpoints and IQR", {
    data <- tibble::tibble(
        id = seq_len(8),
        treatment_group = factor(
            rep(c("PBT", "GKSRS"), each = 4),
            levels = c("PBT", "GKSRS")
        ),
        treatment_date = as.Date("2020-01-01"),
        last_known_alive_date = as.Date("2020-01-01") +
            c(365, 730, 1095, 1460, 3650, 7300, 10950, 14600)
    )

    summary <- calculate_treatment_duration_metrics(data)$summary_stats

    expect_true(all(c(
        "q1_followup_years",
        "q3_followup_years",
        "iqr_followup_years"
    ) %in% names(summary)))
    expect_equal(summary$q1_followup_years, c(547.5, 5475) / DAYS_IN_YEAR)
    expect_equal(summary$q3_followup_years, c(1277.5, 12775) / DAYS_IN_YEAR)
    expect_equal(summary$iqr_followup_years, c(730, 7300) / DAYS_IN_YEAR)
})
