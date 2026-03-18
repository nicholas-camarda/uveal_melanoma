skip_if_integration_disabled()
skip_if_local_data_unavailable()

# Tests for exploratory no-GEP reporting
library(dplyr)

test_that("exploratory no-GEP dataset preparation isolates training and prediction sets", {
    actual_data <- readRDS(here("final_data", "Analytic Dataset", "uveal_melanoma_full_cohort.rds"))

    prepared <- prepare_exploratory_no_gep_data(actual_data)

    expect_true(all(prepared$definitive_training$exploratory_gep_group %in% c("Class 1", "Class 2")))
    expect_true(all(prepared$no_gep_prediction$exploratory_gep_group %in% c("GEP Failed/Indeterminate", "GEP Not Tested")))
    expect_true(all(unique(prepared$no_gep_prediction$no_gep_group) %in% c("GEP Failed/Indeterminate", "GEP Not Tested")))
    expect_true(all(prepared$predictors %in% names(prepared$definitive_training)))
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
    actual_data <- readRDS(here("final_data", "Analytic Dataset", "uveal_melanoma_full_cohort.rds"))

    verification <- verify_exploratory_no_gep_km_fix(actual_data)
    expect_equal(verification$observed_n, c(58L, 27L, 13L, 162L))
    expect_true(all(verification$status == "matched"))
    expect_equal(as.character(stats::na.omit(verification$simple_km_display_order)), c("Class 1", "Class 2", "GEP Not Tested"))
    expect_equal(as.integer(stats::na.omit(verification$simple_km_displayed_n)), c(58L, 27L, 162L))

    modified_data <- actual_data %>%
        dplyr::filter(as.character(.data$gep_class_simple) != "Class 1")

    expect_error(
        verify_exploratory_no_gep_km_fix(modified_data),
        "group counts do not match the expected fixed values"
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
    actual_data <- readRDS(here("final_data", "Analytic Dataset", "uveal_melanoma_full_cohort.rds"))
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

    workbook_sheets <- openxlsx::getSheetNames(results$output_paths$workbook)
    expect_true(all(c(
        "Summary_and_Guide",
        "Predictor_Contribution",
        "Data_Audit",
        "Baseline_Comparisons",
        "KM_Corrected_MFS",
        "KM_Corrected_MSS",
        "Surrogate_Class2_Model",
        "Direct_MFS_Risk_Model",
        "Direct_MSS_Risk_Model",
        "No_GEP_Predictions",
        "Sensitivity_Pooled_No_GEP"
    ) %in% workbook_sheets))

    summary_text <- paste(readLines(results$output_paths$summary), collapse = "\n")
    expect_match(summary_text, "descriptive only", fixed = TRUE)
    expect_match(summary_text, "Retained baseline predictors used in all exploratory models", fixed = TRUE)
    expect_match(summary_text, "standardized coefficient", fixed = TRUE)
    expect_match(summary_text, "ranked first", fixed = TRUE)
    expect_match(summary_text, "P\\(Class 2-like \\| baseline features\\)")
    expect_match(summary_text, "Cilio-Choroidal", fixed = TRUE)

    expect_s3_class(results$surrogate_model$model, "cv.glmnet")
    expect_s3_class(results$direct_models$mfs$model, "cv.glmnet")
    expect_s3_class(results$direct_models$mss$model, "cv.glmnet")
    expect_true("calibration_status" %in% names(results$surrogate_model$metrics))
    expect_true("calibration_status" %in% names(results$direct_models$mfs$metrics))
    expect_true("calibration_status" %in% names(results$direct_models$mss$metrics))
    expect_true(all(unique(results$no_gep_predictions$no_gep_group) %in% c("GEP Failed/Indeterminate", "GEP Not Tested")))
    expect_true("summary_and_guide" %in% names(results))
    expect_true("predictor_contribution" %in% names(results))
    expect_true(any(results$predictor_contribution$section == "model_contribution"))
    expect_true(all(c("Group", "Interpretation_Note") %in% names(results$unified_no_gep_overview)))
    expect_true(all(c("Model", "Top_Predictor_1", "Use_Case") %in% names(results$unified_no_gep_model_comparison)))
    expect_true(all(c("No_GEP_Group", "Analysis", "Bin") %in% names(results$unified_no_gep_risk_strata)))
})
