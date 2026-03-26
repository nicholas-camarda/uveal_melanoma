build_objective2_output_dirs <- function(test_output_dir) {
    list(
        obj2_vision = file.path(test_output_dir, "02_Safety", "a_vision_changes"),
        obj2_retinopathy = file.path(test_output_dir, "02_Safety", "b_retinopathy"),
        obj2_nvg = file.path(test_output_dir, "02_Safety", "c_neovascular_glaucoma"),
        obj2_srd = file.path(test_output_dir, "02_Safety", "d_serous_retinal_detachment")
    )
}

test_that("Snellen conversion rounds to the nearest line away from zero", {
    expect_equal(
        compute_line_change_lines(c(-0.2, -0.3, 0.2, -0.25, 0.25)),
        c(-2, -3, 2, -3, 3)
    )
    expect_equal(
        compute_line_change_lines(c(-0.04, 0.04, -0.05, 0.05)),
        c(0, 0, -1, 1)
    )
    expect_equal(
        convert_logmar_summary_stat_to_line_summary("-0.2 (-3.0, 1.0)"),
        "-2 (-30, 10)"
    )
})

test_that("Snellen line-change buckets keep sub-half-line deltas in the stable 0-line category", {
    bucket_counts <- tibble::tibble(
        treatment_group = factor(
            c(rep("PBT", 5), rep("GKSRS", 5)),
            levels = c("PBT", "GKSRS")
        ),
        vision_change = c(
            0.00, 0.04, -0.04, 0.06, -0.06,
            0.00, 0.03, -0.03, 0.24, -0.24
        )
    ) %>%
        dplyr::mutate(
            vision_line_change_bucket = assign_line_change_bucket(
                compute_line_change_lines(vision_change)
            )
        ) %>%
        dplyr::count(treatment_group, vision_line_change_bucket, name = "count") %>%
        dplyr::arrange(treatment_group, vision_line_change_bucket)

    expect_equal(
        bucket_counts,
        tibble::tibble(
            treatment_group = factor(
                c("PBT", "PBT", "PBT", "GKSRS", "GKSRS", "GKSRS"),
                levels = c("PBT", "GKSRS")
            ),
            vision_line_change_bucket = factor(
                c(
                    "1-line improvement",
                    "Stable (0-line change)",
                    "1-line loss",
                    "2-line improvement",
                    "Stable (0-line change)",
                    "2-line loss"
                ),
                levels = VISION_LINE_CHANGE_CATEGORY_LEVELS,
                ordered = TRUE
            ),
            count = c(1L, 3L, 1L, 1L, 3L, 1L)
        )
    )
})

test_that("Objective 2 writes adjusted outputs in each side-effect subfolder", {
    test_data <- create_test_dataset()
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective2_test")
    output_dirs <- build_objective2_output_dirs(test_output_dir)
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    withr::defer(unlink(test_output_dir, recursive = TRUE), envir = parent.frame())

    results <- run_objective_2(
        data = test_data,
        dataset_name = "test_cohort",
        output_dirs = output_dirs,
        prefix = "test_",
        confounders = c("age_at_diagnosis")
    )

    expect_s3_class(results$vision_changes$regression_model, "lm")
    expect_s3_class(results$vision_changes$line_change_regression_model, "lm")
    expect_s3_class(results$vision_changes$line_change_bucket_regression_model, "polr")
    expect_s3_class(results$retinopathy_analysis$model, "glm")
    expect_s3_class(results$srd_analysis$model, "glm")
    expect_null(results$nvg_analysis$model)

    expect_true(file.exists(file.path(output_dirs$obj2_vision, "test_logmar_vision_change_adjusted_lm.html")))
    expect_true(file.exists(file.path(output_dirs$obj2_vision, "test_logmar_vision_change_adjusted_diagnostics.xlsx")))
    expect_true(file.exists(file.path(output_dirs$obj2_vision, "test_snellen_line_change_adjusted_lm.html")))
    expect_true(file.exists(file.path(output_dirs$obj2_vision, "test_snellen_line_change_adjusted_diagnostics.xlsx")))
    expect_true(file.exists(file.path(output_dirs$obj2_vision, "test_snellen_line_change_distribution_adjusted_polr.html")))
    expect_true(file.exists(file.path(output_dirs$obj2_vision, "test_snellen_line_change_distribution_adjusted_diagnostics.xlsx")))
    expect_true(file.exists(file.path(output_dirs$obj2_vision, "test_snellen_line_change_descriptive_summary.html")))
    expect_true(file.exists(file.path(output_dirs$obj2_vision, "test_snellen_line_change_integer_distribution.xlsx")))
    expect_true(file.exists(file.path(output_dirs$obj2_vision, "test_snellen_line_change_distribution_summary.xlsx")))
    expect_true(file.exists(file.path(output_dirs$obj2_vision, "test_vision_effect_summary.xlsx")))

    expect_true(file.exists(file.path(output_dirs$obj2_retinopathy, "test_retinopathy_logistic_glm.html")))
    expect_true(file.exists(file.path(output_dirs$obj2_retinopathy, "test_retinopathy_logistic_diagnostics.xlsx")))
    expect_true(file.exists(file.path(output_dirs$obj2_retinopathy, "test_retinopathy_effect_summary.xlsx")))

    expect_true(file.exists(file.path(output_dirs$obj2_srd, "test_srd_logistic_glm.html")))
    expect_true(file.exists(file.path(output_dirs$obj2_srd, "test_srd_logistic_diagnostics.xlsx")))
    expect_true(file.exists(file.path(output_dirs$obj2_srd, "test_serous_retinal_detachment_effect_summary.xlsx")))

    expect_true(file.exists(file.path(output_dirs$obj2_nvg, "test_nvg_logistic_SKIPPED.html")))
    expect_true(file.exists(file.path(output_dirs$obj2_nvg, "test_nvg_logistic_diagnostics.xlsx")))
    expect_true(file.exists(file.path(output_dirs$obj2_nvg, "test_neovascular_glaucoma_effect_summary.xlsx")))

    nvg_skip_sheets <- readxl::excel_sheets(file.path(output_dirs$obj2_nvg, "test_nvg_logistic_diagnostics.xlsx"))
    expect_true(all(c("Skip_summary", "Narrative_summary", "Event_support") %in% nvg_skip_sheets))

    nvg_skip_summary <- readxl::read_xlsx(
        file.path(output_dirs$obj2_nvg, "test_nvg_logistic_diagnostics.xlsx"),
        sheet = "Skip_summary"
    )
    expect_equal(
        as.integer(nvg_skip_summary$value[nvg_skip_summary$metric == "minimum_events_required"]),
        MINIMUM_ADJUSTED_LOGISTIC_EVENTS
    )

    nvg_skip_html <- paste(
        readLines(file.path(output_dirs$obj2_nvg, "test_nvg_logistic_SKIPPED.html"), warn = FALSE),
        collapse = "\n"
    )
    expect_match(
        nvg_skip_html,
        sprintf("requires at least %d events", MINIMUM_ADJUSTED_LOGISTIC_EVENTS),
        fixed = TRUE
    )
    expect_match(nvg_skip_html, "Modeled Outcome Counts By Covariate Level", fixed = TRUE)

    vision_effect_summary <- readxl::read_xlsx(file.path(output_dirs$obj2_vision, "test_vision_effect_summary.xlsx"))
    expect_true(all(c(
        "LogMAR Vision Change",
        "Snellen Line Change",
        "Snellen Line Change Distribution"
    ) %in% vision_effect_summary$analysis_label))
    expect_true(all(c(
        "Descriptive",
        "Unadjusted linear",
        "Adjusted linear",
        "Unadjusted ordinal logistic",
        "Adjusted ordinal logistic"
    ) %in% vision_effect_summary$model_label))
    expect_true(all(c("model_formula", "covariates_used") %in% names(vision_effect_summary)))

    ordinal_rows <- subset(
        vision_effect_summary,
        analysis_label == "Snellen Line Change Distribution" &
            model_label %in% c("Unadjusted ordinal logistic", "Adjusted ordinal logistic")
    )
    expect_true(all(ordinal_rows$dataset == "test_cohort"))
    expect_equal(nrow(ordinal_rows), 2)
    expect_true(all(!is.na(ordinal_rows$ci_lower)))
    expect_true(all(!is.na(ordinal_rows$ci_upper)))
    expect_true(all(!is.na(ordinal_rows$p_value)))
    expect_true(all(grepl("^vision_line_change_bucket ~ treatment_group", ordinal_rows$model_formula)))
    expect_equal(
        ordinal_rows$covariates_used[ordinal_rows$model_label == "Unadjusted ordinal logistic"],
        "None"
    )
    expect_equal(
        ordinal_rows$covariates_used[ordinal_rows$model_label == "Adjusted ordinal logistic"],
        "age_at_diagnosis"
    )

    ordinal_html <- readLines(
        file.path(output_dirs$obj2_vision, "test_snellen_line_change_distribution_adjusted_polr.html"),
        warn = FALSE
    )
    ordinal_html <- paste(ordinal_html, collapse = "\n")
    expect_match(
        ordinal_html,
        "GKSRS</td>\\s*<td headers=\"estimate\" class=\"gt_row gt_center\">—</td>",
        perl = TRUE
    )
    expect_match(
        ordinal_html,
        "PBT</td>\\s*<td headers=\"estimate\" class=\"gt_row gt_center\">[0-9.]+</td>",
        perl = TRUE
    )

    nvg_effect_summary <- readxl::read_xlsx(file.path(output_dirs$obj2_nvg, "test_neovascular_glaucoma_effect_summary.xlsx"))
    expect_true(any(nvg_effect_summary$model_status == "SKIPPED"))
})

test_that("Merged adverse events table converts the displayed logMAR summary row to Snellen lines", {
    test_data <- create_test_dataset()
    output_path <- file.path(TEST_OUTPUT_DIR, "merged_objective2_test")
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
    withr::defer(unlink(output_path, recursive = TRUE), envir = parent.frame())

    merge_adverse_events_tables(test_data, test_data, output_path)

    merged_xlsx <- file.path(output_path, "merged_adverse_events.xlsx")
    expect_true(file.exists(merged_xlsx))

    merged_tbl <- readxl::read_xlsx(merged_xlsx)
    label_col <- names(merged_tbl)[1]
    clean_labels <- gsub("_", "", merged_tbl[[label_col]])
    logmar_row <- merged_tbl[clean_labels == "Vision Change (logMAR)", ]
    snellen_row <- merged_tbl[clean_labels == "Snellen Line Change", ]

    expect_equal(nrow(logmar_row), 1)
    expect_equal(nrow(snellen_row), 1)

    stat_cols <- setdiff(names(logmar_row)[-1], grep("p-value", names(logmar_row), value = TRUE))
    expect_true(length(stat_cols) > 0)

    for (col_name in stat_cols) {
        expect_equal(
            snellen_row[[col_name]][[1]],
            convert_logmar_summary_stat_to_line_summary(logmar_row[[col_name]][[1]])
        )
    }
})
