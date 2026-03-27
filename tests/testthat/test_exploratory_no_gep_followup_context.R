test_that("Exploratory no-GEP follow-up block summarizes follow-up and operational status", {
    test_data <- create_test_dataset() %>%
        dplyr::slice(1:8) %>%
        dplyr::mutate(
            exploratory_gep_group = factor(
                c(
                    rep("GEP Failed/Indeterminate", 4),
                    rep("GEP Not Tested", 4)
                ),
                levels = c("GEP Failed/Indeterminate", "GEP Not Tested")
            ),
            no_gep_group = as.character(.data$exploratory_gep_group),
            follow_up_years = c(1.0, 2.0, 3.5, 4.0, 5.0, 6.5, 7.0, 8.5),
            last_known_alive_date = as.Date(c(
                "2025-02-20",
                "2025-01-10",
                "2023-01-01",
                "2024-12-01",
                "2025-02-15",
                "2023-06-01",
                "2025-01-30",
                "2024-04-01"
            )),
            death_event = c(0, 0, 1, 0, 0, 1, 0, 0)
        )

    block <- build_exploratory_no_gep_followup_block(
        prepared_data = list(no_gep_prediction = test_data),
        dataset_name = "uveal_melanoma_full_cohort"
    )

    expect_true(any(grepl("FOLLOW-UP CONTEXT", block, fixed = TRUE)))
    expect_true(any(grepl("no-GEP prediction subset", block, fixed = TRUE)))
    expect_true(any(grepl("reached at least 5 years", block, fixed = TRUE)))
    expect_true(any(grepl("Operational view:", block, fixed = TRUE)))
    expect_true(any(grepl("By no-GEP group:", block, fixed = TRUE)))
})
