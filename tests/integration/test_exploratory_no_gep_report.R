# Tests for exploratory no-GEP reporting
library(dplyr)

test_that("exploratory no-GEP dataset preparation isolates reference and scoring cohorts", {
    actual_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))

    prepared <- prepare_exploratory_no_gep_data(actual_data)

    expect_identical(levels(prepared$full_data$exploratory_gep_group), levels(actual_data$gep_class_simple))
    expect_identical(levels(prepared$full_data$sex), levels(actual_data$sex))
    expect_identical(levels(prepared$full_data$location), levels(actual_data$location))
    expect_identical(levels(prepared$full_data$initial_t_stage_simple), levels(actual_data$initial_t_stage_simple))
    expect_identical(levels(prepared$full_data$internal_reflectivity), levels(actual_data$internal_reflectivity))
    expect_identical(levels(prepared$full_data$srf), levels(actual_data$srf))

    expect_true(all(prepared$definitive_reference$exploratory_gep_group %in% c("Class 1", "Class 2")))
    expect_true(all(prepared$no_gep_scoring$exploratory_gep_group %in% c("GEP Failed/Indeterminate", "GEP Not Tested")))
    expect_true(all(unique(prepared$no_gep_scoring$no_gep_group) %in% c("GEP Failed/Indeterminate", "GEP Not Tested")))
    expect_true(all(prepared$predictors %in% names(prepared$definitive_reference)))
    expect_true(any(prepared$full_data$ciliary_involvement == 1, na.rm = TRUE))

    cilio_rows <- prepared$full_data %>%
        dplyr::filter(.data$location == "Cilio-Choroidal")
    choroidal_rows <- prepared$full_data %>%
        dplyr::filter(.data$location == "Choroidal")

    expect_true(nrow(cilio_rows) > 0)
    expect_true(all(cilio_rows$ciliary_involvement == 1))
    expect_true(all(choroidal_rows$ciliary_involvement == 0))

    screening_predictors <- prepared$predictor_screening %>%
        dplyr::filter(.data$status == "retained") %>%
        dplyr::pull(.data$predictor)
    expect_setequal(prepared$predictors, screening_predictors)
})

test_that("exploratory no-GEP KM verification checks displayed counts and cohort counts", {
    actual_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
    prepared <- prepare_exploratory_no_gep_data(actual_data)

    verification <- verify_exploratory_no_gep_km_fix(
        actual_data,
        prepared_data = prepared,
        expected_group_counts = prepared$group_snapshot
    )
    expect_equal(verification$observed_n, prepared$group_snapshot$expected_n)
    expect_equal(verification$expected_n, prepared$group_snapshot$expected_n)
    expect_true(all(verification$status == "matched"))
    expect_equal(as.character(stats::na.omit(verification$simple_km_display_order)), c("Class 1", "Class 2", "GEP Not Tested"))
    expect_equal(as.integer(stats::na.omit(verification$simple_km_displayed_n)), c(58L, 27L, 162L))

    modified_data <- actual_data
    class1_idx <- which(as.character(modified_data$exploratory_gep_group) == "Class 1")[1]
    modified_data$exploratory_gep_group[class1_idx] <- factor(
        "Class 2",
        levels = levels(modified_data$exploratory_gep_group)
    )

    expect_error(
        verify_exploratory_no_gep_km_fix(
            modified_data,
            expected_group_counts = prepared$group_snapshot
        ),
        "group counts do not match the prepared-dataset snapshot"
    )
})

test_that("exploratory no-GEP preparation fails fast when Objective 0 columns are absent", {
    broken_data <- tibble::tibble(
        gep_class_simple = factor(c("Class 1", "Class 2"))
    )

    expect_error(
        prepare_exploratory_no_gep_data(broken_data),
        "expects the Objective 0 prepared cohort"
    )
})

test_that("exploratory direct targets preserve MFS eligibility and MSS competing outcomes", {
    actual_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
    fixture <- actual_data
    initially_prepared <- prepare_exploratory_no_gep_data(actual_data)

    baseline_mets_id <- initially_prepared$mfs_model_data$id[[1]]
    competing_id <- initially_prepared$mss_model_data$id[[1]]
    early_censor_id <- initially_prepared$mss_model_data$id[[2]]
    baseline_row <- match(baseline_mets_id, fixture$id)
    mss_rows_index <- match(c(competing_id, early_censor_id), fixture$id)

    fixture$mets_free_at_baseline[[baseline_row]] <- FALSE
    fixture$tt_mets_months_analysis[[baseline_row]] <- NA_real_
    fixture$mfs_event_5yr[[baseline_row]] <- 1L

    fixture$tt_death_months[mss_rows_index] <- 12
    fixture$melanoma_death_event[mss_rows_index] <- 0L
    fixture$competing_death_event[mss_rows_index] <- c(1L, 0L)
    fixture$mss_event_5yr[mss_rows_index] <- 0L

    prepared <- prepare_exploratory_no_gep_data(fixture)

    expect_false(baseline_mets_id %in% prepared$mfs_model_data$id)
    expect_true(all(prepared$mfs_model_data$mets_free_at_baseline))
    expect_true(all(is.finite(prepared$mfs_model_data$tt_mets_months_analysis)))

    mss_rows <- prepared$mss_model_data %>%
        dplyr::filter(.data$id %in% c(competing_id, early_censor_id)) %>%
        dplyr::arrange(match(.data$id, c(competing_id, early_censor_id)))
    mss_status <- derive_horizon_status(
        mss_rows$tt_death_months,
        mss_rows$objective4_mss_event_type,
        60
    )

    expect_identical(mss_rows$objective4_mss_event_type, c(2L, 0L))
    expect_identical(mss_status$horizon_event, c(0L, NA_integer_))
    expect_identical(mss_status$known_status, c(TRUE, FALSE))

    mss_weight_check <- derive_fold_ipcw_payload(
        training = tibble::tibble(
            time = c(12, 18, 24, 36, 60, 72),
            event_type = c(1L, 2L, 0L, 1L, 0L, 0L)
        ),
        assessment = tibble::tibble(
            time = mss_rows$tt_death_months,
            event_type = mss_rows$objective4_mss_event_type
        ),
        time_var = "time",
        event_type_var = "event_type",
        horizon_months = 60
    )
    expect_gt(mss_weight_check$assessment$ipcw_weight[[1]], 0)
    expect_identical(mss_weight_check$assessment$ipcw_weight[[2]], 0)
})

test_that("scoped OOF performance uses one keyed prediction set and fails closed", {
    oof <- tibble::tibble(
        stable_id = rep(as.character(1:6), 2),
        repeat_id = rep(1:2, each = 6),
        prediction = rep(c(0.9, 0.7, 0.6, 0.4, 0.3, 0.1), 2),
        horizon_event = rep(c(1L, 1L, 0L, 0L, NA_integer_, 0L), 2),
        ipcw_weight = rep(c(2, 1, 1, 2, 0, 1), 2),
        exploratory_gep_group = rep(
            c("Class 2", "GEP Failed/Indeterminate", "Class 1", "GEP Not Tested", "GEP Not Tested", "Class 1"),
            2
        )
    )

    scoped <- summarize_scoped_ipcw_oof_performance(
        oof,
        group_var = "exploratory_gep_group"
    )

    expect_setequal(unique(scoped$performance_scope), c("Overall", "No GEP"))
    expect_equal(nrow(scoped), 4)
    expect_true(all(scoped$evaluation_method == "outer-training-fold IPCW weighted OOF AUC/Brier/calibration"))
    expect_true(all(scoped$not_tested_n == 2L))
    expect_true(all(scoped$failed_indeterminate_n == 1L))
    expect_true(all(scoped$auc_status[scoped$performance_scope == "Overall"] == "ok"))
    expect_true(all(scoped$auc_status[scoped$performance_scope == "No GEP"] == "ok"))

    no_gep_one_class <- oof %>%
        dplyr::mutate(
            horizon_event = dplyr::if_else(
                .data$exploratory_gep_group %in% c("GEP Failed/Indeterminate", "GEP Not Tested") & .data$ipcw_weight > 0,
                0L,
                .data$horizon_event
            )
        )
    unsupported <- summarize_scoped_ipcw_oof_performance(
        no_gep_one_class,
        group_var = "exploratory_gep_group"
    )
    expect_true(all(is.na(unsupported$cv_auc[unsupported$performance_scope == "No GEP"])))
    expect_true(all(unsupported$auc_status[unsupported$performance_scope == "No GEP"] == "unsupported_no_weighted_cases"))

    absent_target_population <- summarize_scoped_ipcw_oof_performance(
        oof %>% dplyr::filter(!.data$exploratory_gep_group %in% c("GEP Failed/Indeterminate", "GEP Not Tested")),
        group_var = "exploratory_gep_group"
    )
    absent_rows <- absent_target_population %>% dplyr::filter(.data$performance_scope == "No GEP")
    expect_equal(nrow(absent_rows), 2)
    expect_true(all(absent_rows$auc_status == "unsupported_no_positive_weight"))
    expect_true(all(is.na(absent_rows$cv_auc)))
})

test_that("exploratory horizon summaries use censoring-aware event estimates", {
    prediction_data <- tibble::tibble(
        no_gep_group = c("GEP Failed/Indeterminate", "GEP Failed/Indeterminate", "GEP Failed/Indeterminate"),
        tt_mets_months = c(48, 24, 24),
        tt_mets_months_analysis = c(48, 24, 24),
        mets_event = c(1, 0, 0),
        mets_free_at_baseline = TRUE,
        objective4_mfs_event_type = c(1L, 0L, 0L),
        tt_death_months = c(48, 24, 24),
        melanoma_death_event = c(1, 0, 0),
        competing_death_event = c(0, 0, 0),
        mfs_event_5yr = c(1L, 0L, 0L),
        mss_event_5yr = c(1L, 0L, 0L),
        surrogate_class2_probability = c(0.5, 0.4, 0.6),
        predicted_mfs_5yr_risk = c(0.5, 0.4, 0.6),
        predicted_mss_5yr_risk = c(0.5, 0.4, 0.6)
    )

    summary_tbl <- summarize_no_gep_predictions(prediction_data)

    expect_equal(summary_tbl$mfs_observed_method[[1]], "kaplan_meier_at_horizon")
    expect_equal(summary_tbl$mss_observed_method[[1]], "aalen_johansen_cif_at_horizon")
    expect_true(summary_tbl$observed_mfs_5yr_event_rate[[1]] > mean(prediction_data$mfs_event_5yr))
    expect_true(summary_tbl$observed_mss_5yr_event_rate[[1]] > mean(prediction_data$mss_event_5yr))
})

test_that("exploratory pooled summaries tolerate bins with no melanoma failures", {
    prediction_data <- tibble::tibble(
        no_gep_group = c("GEP Failed/Indeterminate", "GEP Failed/Indeterminate", "GEP Not Tested", "GEP Not Tested"),
        surrogate_probability_bin = c("Low", "High", "Low", "High"),
        mfs_risk_bin = c("Low", "High", "Low", "High"),
        mss_risk_bin = c("Low", "High", "Low", "High"),
        surrogate_class2_probability = c(0.2, 0.8, 0.3, 0.7),
        predicted_mfs_5yr_risk = c(0.1, 0.6, 0.2, 0.5),
        predicted_mss_5yr_risk = c(0.05, 0.4, 0.1, 0.3),
        tt_mets_months = c(24, 48, 36, 60),
        tt_mets_months_analysis = c(24, 48, 36, 60),
        mets_event = c(0, 1, 0, 1),
        mets_free_at_baseline = TRUE,
        objective4_mfs_event_type = c(0L, 1L, 0L, 1L),
        tt_death_months = c(24, 48, 36, 60),
        melanoma_death_event = c(0, 1, 0, 0),
        competing_death_event = c(0, 0, 0, 0),
        mfs_event_5yr = c(0L, 1L, 0L, 1L),
        mss_event_5yr = c(0L, 1L, 0L, 0L)
    )

    pooled_summary <- summarize_pooled_no_gep_sensitivity(prediction_data)
    risk_strata_summary <- summarize_no_gep_risk_strata(prediction_data)

    expect_true(nrow(pooled_summary) > 0)
    expect_true(nrow(risk_strata_summary) > 0)
    expect_true(all(stats::na.omit(pooled_summary$mss_observed_method) == "aalen_johansen_cif_at_horizon"))
    expect_true(all(stats::na.omit(risk_strata_summary$MSS_Observed_Method) == "aalen_johansen_cif_at_horizon"))
    expect_equal(
        risk_strata_summary$Observed_MSS_5yr_Event_Rate[
            risk_strata_summary$No_GEP_Group == "GEP Not Tested" &
                risk_strata_summary$Analysis == "Direct_60mo_Melanoma_Death_Cumulative_Incidence_Risk"
        ],
        c(0, 0)
    )
})

test_that("exploratory predictor screening ignores unused Other factor levels", {
    exploratory_data <- tibble::tibble(
        exploratory_gep_group = factor(
            c(rep("Class 1", 41), rep("Class 2", 40)),
            levels = c("Class 1", "Class 2", "GEP Failed/Indeterminate", "GEP Not Tested")
        ),
        mfs_event_5yr = c(rep(0L, 50), rep(1L, 31)),
        mss_event_5yr = c(rep(0L, 55), rep(1L, 26)),
        tt_mets_months_analysis = rep(72, 81),
        mets_free_at_baseline = TRUE,
        objective4_mfs_event_type = as.integer(c(rep(0L, 50), rep(1L, 31))),
        tt_death_months = rep(72, 81),
        objective4_mss_event_type = as.integer(c(rep(0L, 55), rep(1L, 26))),
        location = factor(
            c(rep("Choroidal", 74), rep("Cilio-Choroidal", 7)),
            levels = c("Choroidal", "Cilio-Choroidal", "Other")
        )
    )

    screening <- screen_exploratory_predictors(
        data = exploratory_data,
        candidate_predictors = "location",
        factor_predictors = "location",
        completeness_threshold = 0.9,
        min_level_count = 5
    )

    expect_equal(screening$status, "retained")
    expect_match(screening$reason, "passes completeness and sparse-level screening", fixed = TRUE)

    model_data <- build_exploratory_model_dataset(
        data = exploratory_data,
        predictors = "location",
        factor_predictors = "location",
        outcome_var = "class2_outcome",
        group_levels = c("Class 1", "Class 2")
    )

    expect_equal(levels(model_data$location), c("Choroidal", "Cilio-Choroidal"))
    expect_false("Other" %in% levels(model_data$location))
})

test_that("exploratory no-GEP report writes workbook, summary, and plots", {
    actual_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "exploratory_no_gep")
    withr::defer(unlink(test_output_dir, recursive = TRUE), teardown_env())

    results <- run_exploratory_no_gep_report(
        dataset_name = "uveal_melanoma_full_cohort",
        output_dir = test_output_dir,
        verify_km_fix = FALSE,
        data = actual_data
    )

    expect_true(file.exists(results$output_paths$workbook))
    expect_true(file.exists(results$output_paths$summary))
    expect_true(file.exists(results$output_paths$mfs_km))
    expect_true(file.exists(results$output_paths$mss_cif))
    expect_true(file.exists(results$output_paths$surrogate_density))
    expect_true(file.exists(results$output_paths$presentation_subgroup_comparison))
    expect_true(file.exists(results$output_paths$presentation_observed_risk_thirds))
    expect_true(file.exists(results$output_paths$presentation_direct_model_contributors))

    workbook_sheets <- openxlsx::getSheetNames(results$output_paths$workbook)
    expect_equal(
        workbook_sheets,
        c(
            "Start_Here",
            "Key_Findings_5yr",
            "Risk_Ladder_5yr",
            "No_GEP_Subgroups",
            "Model_Performance",
            "Parsimonious_Sensitivity",
            "Surrogate_Model_Coefficients",
            "Direct_MFS_Coefficients",
            "Direct_MSS_Coefficients",
            "Model_Calibration",
            "Predictor_Contribution",
            "Overlap_Diagnostics",
            "Baseline_Comparisons",
            "Data_Audit",
            "Presentation_Data",
            "No_GEP_Predictions",
            "Sensitivity_Pooled_No_GEP",
            "KM_Corrected_MFS",
            "KM_Corrected_MSS"
        )
    )

    start_here_sheet <- openxlsx::read.xlsx(results$output_paths$workbook, sheet = "Start_Here")
    key_findings_sheet <- openxlsx::read.xlsx(results$output_paths$workbook, sheet = "Key_Findings_5yr")
    risk_ladder_sheet <- openxlsx::read.xlsx(results$output_paths$workbook, sheet = "Risk_Ladder_5yr")
    model_performance_sheet <- openxlsx::read.xlsx(results$output_paths$workbook, sheet = "Model_Performance")
    presentation_data_sheet <- openxlsx::read.xlsx(results$output_paths$workbook, sheet = "Presentation_Data")

    expect_false(any(c("section", "item", "detail", "guide_text") %in% names(key_findings_sheet)))
    expect_false(any(c("section", "item", "detail", "guide_text") %in% names(risk_ladder_sheet)))
    expect_false(any(c("section", "item", "detail", "guide_text") %in% names(model_performance_sheet)))
    expect_equal(nrow(key_findings_sheet), 4)
    expect_equal(
        key_findings_sheet$group,
        c("Class 1", "GEP Not Tested", "GEP Failed/Indeterminate", "Class 2")
    )
    expect_true(
        "median_predicted_60mo_melanoma_death_cumulative_incidence_risk" %in%
            names(key_findings_sheet)
    )
    expect_false("median_predicted_5yr_mss_risk" %in% names(key_findings_sheet))
    expect_true(all(c("section", "label", "value") %in% names(start_here_sheet)))
    expect_true(all(c(
        "semantic_id", "value_numeric", "value_character", "reason_for_missing_gep_available"
    ) %in% names(presentation_data_sheet)))
    expect_identical(anyDuplicated(presentation_data_sheet$semantic_id), 0L)
    expect_true(all(is.finite(stats::na.omit(presentation_data_sheet$value_numeric))))
    presentation_probability_rows <- presentation_data_sheet %>%
        dplyr::filter(.data$unit == "probability_0_to_1")
    expect_true(all(
        presentation_probability_rows$value_numeric >= 0 &
            presentation_probability_rows$value_numeric <= 1
    ))
    expect_true(all(presentation_data_sheet$reason_for_missing_gep_available == FALSE))
    expect_equal(
        presentation_data_sheet$value_numeric[
            presentation_data_sheet$semantic_id == "no_gep_scoreable_count"
        ],
        164
    )
    expect_equal(
        presentation_data_sheet$value_numeric[
            presentation_data_sheet$semantic_id == "no_gep_without_usable_count"
        ],
        175
    )
    expect_false(any(grepl("_na_(count|observed_event_rate)$", presentation_data_sheet$semantic_id)))
    expect_false(any(grepl("patient|record|case|study", names(presentation_data_sheet), ignore.case = TRUE)))
    expect_true(all(c(
        "cohort_total_count",
        "gep_usable_count",
        "gep_not_tested_count",
        "gep_failed_indeterminate_count",
        "no_gep_without_usable_count",
        "no_gep_scoreable_count",
        "followup_gep_not_tested_median_years",
        "observed_gep_failed_indeterminate_mfs_5yr_event_rate",
        "observed_gep_not_tested_mss_60mo_event_rate",
        "direct_model_gep_not_tested_mfs_5yr_median_risk",
        "predictor_direct_mfs_1_standardized_coefficient",
        "baseline_contrast_initial_tumor_diameter_p_value",
        "overlap_initial_tumor_diameter_abs_smd",
        "reason_for_missing_gep_available"
    ) %in% presentation_data_sheet$semantic_id))
    expect_true(all(c("group", "n", "observed_5yr_mfs_event_rate", "median_predicted_5yr_mfs_risk") %in% names(risk_ladder_sheet)))
    expect_true(all(c(
        "model", "model_method", "reported_risk_scale", "cv_auc",
        "cv_auc_stability_interval", "calibration_slope_stability_interval",
        "practical_read"
    ) %in% names(model_performance_sheet)))

    summary_text <- paste(readLines(results$output_paths$summary), collapse = "\n")
    expect_match(summary_text, "descriptive only", fixed = TRUE)
    expect_match(summary_text, "homogeneous intermediate-risk group", fixed = TRUE)
    expect_match(summary_text, "## Follow-Up Context", fixed = TRUE)
    expect_match(summary_text, "no-GEP scoring cohort", fixed = TRUE)
    expect_match(summary_text, "## Key Findings at 5 Years", fixed = TRUE)
    expect_match(summary_text, "95% repeated-partition stability interval", fixed = TRUE)
    expect_match(summary_text, "censoring weights are estimated in each outer training fold", fixed = TRUE)
    expect_match(summary_text, "60-month melanoma-death cumulative-incidence risk", fixed = TRUE)
    expect_match(
        summary_text,
        "Median predicted 60-month melanoma-death cumulative-incidence risk",
        fixed = TRUE
    )
    reader_facing_model_text <- paste(
        summary_text,
        paste(start_here_sheet$value, collapse = "\n"),
        paste(model_performance_sheet$model, collapse = "\n"),
        paste(model_performance_sheet$prediction_target, collapse = "\n"),
        collapse = "\n"
    )
    expect_false(grepl("predicted 5-year MSS risk", reader_facing_model_text, fixed = TRUE))
    expect_false(grepl("Direct 5-year MSS", reader_facing_model_text, fixed = TRUE))
    expect_false(grepl("melanoma-specific model", reader_facing_model_text, fixed = TRUE))
    expect_match(summary_text, "## Parsimonious Sensitivity Check", fixed = TRUE)
    expect_match(summary_text, "## Retained Baseline Predictors", fixed = TRUE)
    expect_match(summary_text, "Std. coef.", fixed = TRUE)
    expect_match(summary_text, "ranked highly in the penalized model", fixed = TRUE)
    expect_match(summary_text, "P\\(Class 2-like \\| baseline features\\)")
    expect_match(summary_text, "Cilio-Choroidal", fixed = TRUE)
    expect_match(summary_text, "0-1 probability scale", fixed = TRUE)
    expect_match(summary_text, "overlap diagnostic", ignore.case = TRUE)
    expect_match(summary_text, "## Why the No-GEP Groups May Differ", fixed = TRUE)
    expect_match(summary_text, "## Presentation Narrative and Figure Priorities", fixed = TRUE)
    expect_match(summary_text, "Direct overlap absolute SMD", fixed = TRUE)
    expect_false(grepl("Selected baseline contrast", summary_text, fixed = TRUE))

    expect_s3_class(results$surrogate_model$model, "cv.glmnet")
    expect_s3_class(results$direct_models$mfs$model, "cv.glmnet")
    expect_s3_class(results$direct_models$mss$model, "cv.glmnet")
    expect_s3_class(results$parsimonious_models$mfs$model, "cv.glmnet")
    expect_s3_class(results$parsimonious_models$mss$model, "cv.glmnet")
    expect_true("calibration_status" %in% names(results$surrogate_model$metrics))
    expect_true("calibration_status" %in% names(results$direct_models$mfs$metrics))
    expect_true("calibration_status" %in% names(results$direct_models$mss$metrics))
    expect_equal(results$direct_models$mfs$metrics$model_mode_used[[1]], "ipcw_horizon_mfs")
    expect_equal(results$direct_models$mss$metrics$model_mode_used[[1]], "ipcw_horizon_competing_risk_mss")
    expect_equal(
        results$direct_models$mss$metrics$prediction_target[[1]],
        "60-month melanoma-death cumulative-incidence risk"
    )
    expect_setequal(
        unique(results$direct_models$mfs$scoped_oof_performance$performance_scope),
        c("Overall", "No GEP")
    )
    expect_setequal(
        unique(results$direct_models$mss$scoped_oof_performance$performance_scope),
        c("Overall", "No GEP")
    )
    expect_true(all(c(
        "performance_scope", "evaluation_method", "prediction_target",
        "metric_status", "positive_weight_n", "case_n", "control_n",
        "weighted_cases", "weighted_controls",
        "failed_indeterminate_n", "not_tested_n", "uncertainty_method"
    ) %in% names(model_performance_sheet)))
    expect_false("model_fallback_reason" %in% names(results$direct_models$mfs$metrics))
    expect_false("model_fallback_reason" %in% names(results$direct_models$mss$metrics))
    expect_false("raw_backtest" %in% names(results$direct_models$mfs))
    expect_false("raw_backtest" %in% names(results$direct_models$mss))
    expect_equal(
        nrow(results$direct_models$mfs$oof_predictions),
        nrow(prepare_exploratory_no_gep_data(actual_data)$mfs_model_data) * GEP_EXPLORATORY_CV_REPEATS
    )
    expect_equal(
        nrow(results$direct_models$mss$oof_predictions),
        nrow(prepare_exploratory_no_gep_data(actual_data)$mss_model_data) * GEP_EXPLORATORY_CV_REPEATS
    )
    prepared_for_oof <- prepare_exploratory_no_gep_data(actual_data)
    expect_setequal(
        unique(results$direct_models$mfs$oof_predictions$stable_id),
        as.character(prepared_for_oof$mfs_model_data$id)
    )
    expect_false(any(
        as.character(actual_data$id[!actual_data$mets_free_at_baseline]) %in%
            results$direct_models$mfs$oof_predictions$stable_id
    ))
    expect_identical(
        anyDuplicated(results$direct_models$mfs$oof_predictions[c("repeat_id", "stable_id")]),
        0L
    )
    expect_identical(
        anyDuplicated(results$direct_models$mss$oof_predictions[c("repeat_id", "stable_id")]),
        0L
    )
    expect_true(all(c("cv_auc_ci_lower", "cv_auc_ci_upper", "cv_repeats") %in% names(results$direct_models$mfs$metrics)))
    expect_true(all(c("cv_auc_ci_lower", "cv_auc_ci_upper", "cv_repeats") %in% names(results$direct_models$mss$metrics)))
    expect_true(all(unique(results$no_gep_predictions$no_gep_group) %in% c("GEP Failed/Indeterminate", "GEP Not Tested")))
    expect_true("start_here" %in% names(results))
    expect_true("key_findings_5yr" %in% names(results))
    expect_true("no_gep_subgroups" %in% names(results))
    expect_true("model_performance" %in% names(results))
    expect_true("surrogate_model_coefficients" %in% names(results))
    expect_true("model_calibration" %in% names(results))
    expect_true("predictor_contribution" %in% names(results))
    expect_true("risk_ladder" %in% names(results))
    expect_true("overlap_diagnostics" %in% names(results))
    expect_true("parsimonious_sensitivity" %in% names(results))
    expect_true("presentation_data" %in% names(results))
    expect_true("presentation_figures" %in% names(results))
    expect_setequal(
        names(results$presentation_figures),
        c("subgroup_comparison", "observed_risk_thirds", "direct_model_contributors")
    )
    subgroup_figure <- results$presentation_figures$subgroup_comparison
    expect_true(all(c(
        "no_gep_group", "n", "median_followup_years", "observed_5yr_mfs_event_rate",
        "observed_5yr_mss_event_rate", "descriptive_frame"
    ) %in% names(subgroup_figure)))
    expect_setequal(
        subgroup_figure$no_gep_group,
        c("GEP Failed/Indeterminate", "GEP Not Tested")
    )
    expect_true(all(subgroup_figure$n > 0))
    expect_true(all(is.finite(subgroup_figure$median_followup_years)))
    expect_true(all(subgroup_figure$descriptive_frame == "Descriptive, non-causal subgroup comparison"))

    risk_thirds_figure <- results$presentation_figures$observed_risk_thirds
    expect_true(all(c(
        "outcome", "risk_third", "n", "observed_event_rate", "estimator"
    ) %in% names(risk_thirds_figure)))
    expect_setequal(risk_thirds_figure$outcome, c("5-year MFS event risk (1-Kaplan-Meier)", "Melanoma-death cumulative incidence"))
    expect_false(any(is.na(risk_thirds_figure$risk_third)))
    expect_true(all(risk_thirds_figure$risk_third %in% c("Lower", "Middle", "Higher")))
    expect_true(all(risk_thirds_figure$n > 0))
    expect_true(all(risk_thirds_figure$observed_event_rate >= 0 & risk_thirds_figure$observed_event_rate <= 1))
    expect_setequal(
        unique(risk_thirds_figure$estimator),
        c("Kaplan-Meier at 60 months", "Aalen-Johansen at 60 months")
    )

    contributor_figure <- results$presentation_figures$direct_model_contributors
    expect_true(all(c(
        "model", "predictor", "signed_standardized_coefficient", "caption"
    ) %in% names(contributor_figure)))
    expect_true(all(is.finite(contributor_figure$signed_standardized_coefficient)))
    expect_true(all(contributor_figure$caption == "Model weights; not causal effects or conventional significance tests."))
    if (any(grepl("optic_nerve", contributor_figure$predictor, fixed = TRUE))) {
        expect_true(any(grepl("counterintuitive/model-dependent", contributor_figure$predictor_note, fixed = TRUE)))
        expect_true(any(grepl("counterintuitive/model-dependent", contributor_figure$rendered_caption, fixed = TRUE)))
        expect_true(any(grepl("interpret cautiously", contributor_figure$rendered_caption, fixed = TRUE)))
        expect_true(any(grepl("\n", contributor_figure$rendered_caption, fixed = TRUE)))
    }
    expect_true(any(results$predictor_contribution$section == "model_contribution"))
    expect_true(
        "predicted_60mo_melanoma_death_cumulative_incidence_risk" %in%
            names(results$no_gep_predictions)
    )
    expect_false("predicted_mss_5yr_risk" %in% names(results$no_gep_predictions))
    expect_true(all(c("Group", "Interpretation_Note") %in% names(results$unified_no_gep_overview)))
    expect_true(all(c(
        "Model", "Model_Method", "Reported_Risk_Scale", "Top_Predictor_1",
        "Use_Case", "CV_AUC_Stability_Lower", "CV_AUC_Stability_Upper"
    ) %in% names(results$unified_no_gep_model_comparison)))
    expect_true(all(c("No_GEP_Group", "Analysis", "Bin") %in% names(results$unified_no_gep_risk_strata)))
    expect_true(all(c("Group", "Observed_MFS_Method", "Observed_MFS_5yr_Event_Rate", "Median_Predicted_MFS_5yr_Risk", "Reported_Risk_Scale") %in% names(results$unified_no_gep_risk_ladder)))
    expect_true(
        "Median_Predicted_60mo_Melanoma_Death_Cumulative_Incidence_Risk" %in%
            names(results$unified_no_gep_risk_ladder)
    )
    expect_false("Median_Predicted_MSS_5yr_Risk" %in% names(results$unified_no_gep_risk_ladder))
    expect_equal(
        as.character(results$risk_ladder$group),
        c("Class 1", "GEP Not Tested", "GEP Failed/Indeterminate", "Class 2")
    )
})

test_that("presentation figure constructors fail closed on incomplete aggregates", {
    incomplete_risk_thirds <- tibble::tibble(
        analysis = rep(c("Direct_MFS_5yr_Risk", "Direct_60mo_Melanoma_Death_Cumulative_Incidence_Risk"), each = 3),
        bin = c("Low", "Intermediate", "High", "Low", "Intermediate", NA_character_),
        n = c(5L, 5L, 5L, 5L, 5L, 1L),
        observed_mfs_5yr_event_rate = rep(0.2, 6),
        observed_mss_5yr_event_rate = rep(0.1, 6)
    )
    expect_error(
        create_exploratory_observed_risk_thirds_figure(
            incomplete_risk_thirds,
            tempfile(fileext = ".png")
        ),
        "exactly Lower, Middle, and Higher"
    )

    empty_contributors <- tibble::tibble(
        predictor = character(),
        standardized_coefficient = numeric()
    )
    expect_error(
        create_exploratory_direct_model_contributors_figure(
            list(predictor_contributions = empty_contributors),
            list(predictor_contributions = empty_contributors),
            tempfile(fileext = ".png")
        ),
        "both direct-model contributor panels"
    )
})
