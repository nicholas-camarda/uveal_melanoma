test_that("RMST publication table uses native month estimates", {
    rmst_results <- tibble::tibble(
        Time_Point_Years = c(1, 3),
        RMST_Group1_Months = c(11.93, 34.32),
        RMST_Group1_Years = RMST_Group1_Months / 12,
        RMST_Group2_Months = c(11.97, 33.65),
        RMST_Group2_Years = RMST_Group2_Months / 12,
        RMST_Difference_Months = c(0.04, -0.67),
        RMST_P_Value = c(0.58, 0.52)
    )

    table_months <- build_rmst_timepoint_table(
        rmst_results,
        group1_label = "PBT",
        group2_label = "GKSRS",
        display_unit = "months",
        digits_rmst = 2,
        digits_diff = 2,
        digits_p = 2
    )

    pull_value <- function(tbl, row_label, col_label) {
        as.numeric(tbl[tbl$`Treatment Group` == row_label, col_label])
    }

    pbt_month <- pull_value(table_months, "PBT (months)", "1-year")
    gksrs_month <- pull_value(table_months, "GKSRS (months)", "1-year")
    diff_month <- pull_value(table_months, "RMST Difference (months)", "1-year")

    expect_equal(pbt_month, 11.93)
    expect_equal(gksrs_month, 11.97)
    expect_equal(diff_month, round(gksrs_month - pbt_month, 2))

    table_years <- build_rmst_timepoint_table(
        rmst_results,
        group1_label = "PBT",
        group2_label = "GKSRS",
        display_unit = "years",
        digits_rmst = 3,
        digits_diff = 3,
        digits_p = 2
    )

    pbt_year <- pull_value(table_years, "PBT (years)", "1-year")
    expect_equal(pbt_year, round(rmst_results$RMST_Group1_Years[1], 3))
})


test_that("RMST, KM, and Cox models share filtered cohort", {
    old_threshold_rarity <- THRESHOLD_RARITY
    assign("THRESHOLD_RARITY", 2L, envir = .GlobalEnv)
    on.exit(
        assign("THRESHOLD_RARITY", old_threshold_rarity, envir = .GlobalEnv),
        add = TRUE
    )

    sample_data <- tibble::tibble(
        time_months = c(24, 22, 20, 18, 16, 14, 12),
        status = c(1, 1, 0, 1, 0, 0, 1),
        treatment_group = factor(
            c("PBT", "GKSRS", "PBT", "PBT", "GKSRS", "GKSRS", "PBT"),
            levels = c("PBT", "GKSRS")
        ),
        grade_clean = factor(
            c("A", "A", "A", "B", "B", "B", "Other"),
            levels = c("A", "B", "Other")
        )
    )

    total_n <- nrow(sample_data)
    cox_n_expected <- sample_data %>% dplyr::filter(grade_clean != "Other") %>% nrow()

    result <- suppressWarnings(analyze_time_to_event_outcomes(
        data = sample_data,
        time_var = "time_months",
        event_var = "status",
        group_var = "treatment_group",
        confounders = "grade_clean",
        analysis_type = "all_patients",
        dataset_name = "unit_test",
        ylab = "Unit Test Survival",
        legend_labels = c("PBT", "GKSRS"),
        output_dirs = NULL,
        prefix = "unit_"
    ))

    skip_if(is.null(result$fit))
    skip_if(is.null(result$rmst_analysis) || nrow(result$rmst_analysis) == 0)

    expect_equal(sum(result$fit$n), total_n)
    expect_true(all(result$rmst_analysis$Group1_Name == "PBT"))
    expect_true(all(result$rmst_analysis$Group2_Name == "GKSRS"))

    modeled_n <- tryCatch({
        result$diagnostics$sample_size_summary$modeled_n[1]
    }, error = function(e) NA_real_)
    expect_equal(as.numeric(modeled_n), cox_n_expected)
})

test_that("RMST skip rows retain explicit feasibility metadata", {
    rmst_results <- tibble::tibble(
        Time_Point_Years = c(1, 3),
        Analysis_Status = c("completed", "skipped"),
        Skip_Reason = c(NA_character_, "tau_exceeds_followup_minimum(36.0>24.0)"),
        RMST_Group1_Months = c(10.25, NA_real_),
        RMST_Group1_Years = RMST_Group1_Months / 12,
        RMST_Group2_Months = c(11.10, NA_real_),
        RMST_Group2_Years = RMST_Group2_Months / 12,
        RMST_Difference_Months = c(0.85, NA_real_),
        RMST_Difference_Years = RMST_Difference_Months / 12,
        RMST_P_Value = c(0.42, NA_real_)
    )

    table_months <- build_rmst_timepoint_table(
        rmst_results,
        group1_label = "PBT",
        group2_label = "GKSRS",
        display_unit = "months",
        digits_rmst = 2,
        digits_diff = 2,
        digits_p = 2
    )

    expect_equal(
        table_months[table_months$`Treatment Group` == "RMST P-Value", "3-year"][[1]],
        "Skipped: tau_exceeds_followup_minimum(36.0>24.0)"
    )
    expect_equal(
        table_months[table_months$`Treatment Group` == "RMST Difference (months)", "3-year"][[1]],
        "Skipped"
    )
})
