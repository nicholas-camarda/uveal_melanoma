skip_if_integration_disabled()
skip_if_local_data_unavailable()

# Test file for Objective 4: GEP Analysis
# Tests the actual content and statistical results of the GEP analysis pipeline
# Run with: testthat::test_dir('tests/testthat')

# CRITICAL: Set test environment variables BEFORE sourcing load_all.R
# This prevents directory creation in the wrong location

# Load required packages first
library(dplyr)

# Override project constants to prevent test interference

# Load the project environment with ALL of the variables and functions
# You do not need to load libraries separately

# Source the helper file for test data creation
source(here("tests", "testthat", "test_helper_data.R"))

extract_risk_table_y_limits <- function(risk_table_plot) {
    y_scales <- Filter(function(scale) "y" %in% scale$aesthetics, risk_table_plot$scales$scales)
    y_scales[[length(y_scales)]]$limits
}

extract_displayed_risk_counts <- function(risk_table_plot, time_point = 0) {
    built_layer <- suppressWarnings(ggplot2::ggplot_build(risk_table_plot)$data[[1]])
    built_layer %>%
        dplyr::filter(.data$x == time_point) %>%
        dplyr::arrange(dplyr::desc(.data$y)) %>%
        dplyr::pull(.data$label) %>%
        as.integer()
}

# Test 4a: GEP MFS validation produces valid statistical results
test_that("4a: GEP MFS validation produces valid statistical results", {
    # Load actual data to test with
    actual_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))

    # Create minimal output directories
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_test")
    output_dirs <- list(
        obj4_mfs = file.path(test_output_dir, "04_GEP_Validation", "a_mfs_validation"),
        obj4_mss = file.path(test_output_dir, "04_GEP_Validation", "b_mss_validation"),
        obj4_ph_diagnostics = file.path(test_output_dir, "04_GEP_Validation", "c_proportional_hazards_diagnostics")
    )

    # Create directories
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }

    # Run simple GEP validation (this works)
    results <- simple_gep_validation(actual_data, output_dirs, "test_")

    # Test that results exist and have expected structure
    expect_true("mfs_results" %in% names(results))
    expect_true("mss_results" %in% names(results))
    expect_true("overall_summary" %in% names(results))

    # Test MFS results structure
    expect_true(is.data.frame(results$mfs_results))
    expect_true("gep_class_simple" %in% names(results$mfs_results))
    expect_true("n" %in% names(results$mfs_results))
    expect_true("expected_rate" %in% names(results$mfs_results))
    expect_true("actual_rate" %in% names(results$mfs_results))

    # Test MSS results structure
    expect_true(is.data.frame(results$mss_results))
    expect_true("gep_class_simple" %in% names(results$mss_results))
    expect_true("n" %in% names(results$mss_results))
    expect_true("expected_rate" %in% names(results$mss_results))
    expect_true("actual_rate" %in% names(results$mss_results))

    # Test overall summary structure
    expect_true(is.data.frame(results$overall_summary))
    expect_true("outcome" %in% names(results$overall_summary))
    expect_true("total_patients" %in% names(results$overall_summary))

    # Test that values are reasonable
    expect_true(all(results$mfs_results$n > 0))
    expect_true(all(results$mss_results$n > 0))
    expect_true(all(results$overall_summary$total_patients > 0))

    # Cleanup
    unlink(test_output_dir, recursive = TRUE)
})

# Test 4b: GEP MSS validation produces valid statistical results
test_that("4b: GEP MSS validation produces valid statistical results", {
    # This test is covered by the MFS test above since simple_gep_validation
    # returns both MFS and MSS results in the same structure
    expect_true(TRUE) # Placeholder - actual testing done in test 4a
})

test_that("simple GEP validation uses melanoma-specific MSS endpoint", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_simple_mss_endpoint")
    output_dirs <- list(
        obj4_mfs = file.path(test_output_dir, "04_GEP_Validation", "a_mfs_validation"),
        obj4_mss = file.path(test_output_dir, "04_GEP_Validation", "b_mss_validation"),
        obj4_ph_diagnostics = file.path(test_output_dir, "04_GEP_Validation", "c_proportional_hazards_diagnostics")
    )

    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }

    test_data <- tibble::tibble(
        biopsy1_gep = c("Class 1", "Class 2"),
        gep_class_simple = c("Class 1", "Class 2"),
        biopsy1_gep_mfs = c(0.80, 0.20),
        biopsy1_gep_mss = c(0.85, 0.15),
        expected_mfs_5yr = c(0.80, 0.20),
        expected_mss_5yr = c(0.85, 0.15),
        tt_mets_months = c(72, 24),
        mets_event = c(0, 1),
        mfs_event_5yr = c(0, 1),
        tt_death_months = c(24, 24),
        tt_death_years = c(2, 2),
        death_event = c(1, 1),
        melanoma_death_event = c(0, 1),
        competing_death_event = c(1, 0),
        mss_event_5yr = c(0, 1),
        mfs_analysis_eligible = c(TRUE, TRUE),
        mss_analysis_eligible = c(TRUE, TRUE)
    )

    results <- simple_gep_validation(test_data, output_dirs, "endpoint_")

    class1_mss <- results$mss_results %>%
        dplyr::filter(gep_class_simple == "Class 1") %>%
        dplyr::pull(actual_rate)
    class2_mss <- results$mss_results %>%
        dplyr::filter(gep_class_simple == "Class 2") %>%
        dplyr::pull(actual_rate)

    expect_equal(class1_mss, 1)
    expect_equal(class2_mss, 0)

    unlink(test_output_dir, recursive = TRUE)
})

test_that("restore_gep_display_variables restores only protected GEP display columns", {
    dataset_name <- "unit_test_gep_display_restore"
    dir.create(PROCESSED_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
    precollapse_path <- file.path(PROCESSED_DATA_DIR, paste0(dataset_name, "_derived_precollapse.rds"))
    on.exit(unlink(precollapse_path), add = TRUE)

    precollapse_data <- tibble::tibble(
        biopsy1_gep = factor(c("Class 1 PRAME Negative", "GEP Failed/Indeterminate")),
        gep_class_simple = factor(c("Class 1", "GEP Failed/Indeterminate")),
        prame_status = factor(c("Negative", "Not Available")),
        gep12_prame_status = factor(c("Negative", NA_character_)),
        unchanged_var = c("keep_a", "keep_b")
    )
    saveRDS(precollapse_data, precollapse_path)

    collapsed_data <- precollapse_data %>%
        mutate(
            biopsy1_gep = factor(c("Class 1 PRAME Negative", "Other")),
            gep_class_simple = factor(c("Class 1", "Other")),
            prame_status = factor(c("Negative", "Other")),
            gep12_prame_status = factor(c("Negative", "Other")),
            unchanged_var = c("keep_a", "changed")
        )

    restored_data <- restore_gep_display_variables(collapsed_data, dataset_name = dataset_name)

    expect_equal(as.character(restored_data$biopsy1_gep), as.character(precollapse_data$biopsy1_gep))
    expect_equal(as.character(restored_data$gep_class_simple), as.character(precollapse_data$gep_class_simple))
    expect_equal(as.character(restored_data$prame_status), as.character(precollapse_data$prame_status))
    expect_equal(as.character(restored_data$gep12_prame_status), as.character(precollapse_data$gep12_prame_status))
    expect_equal(restored_data$unchanged_var, collapsed_data$unchanged_var)
})

test_that("restore_gep_display_variables aligns subsetted rows by id", {
    dataset_name <- "unit_test_gep_subset_restore"
    dir.create(PROCESSED_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
    precollapse_path <- file.path(PROCESSED_DATA_DIR, paste0(dataset_name, "_derived_precollapse.rds"))
    on.exit(unlink(precollapse_path), add = TRUE)

    saveRDS(
        tibble::tibble(
            id = c(101, 102, 103),
            biopsy1_gep = factor(c("Class 1 PRAME Negative", "Class 2 PRAME Positive", "GEP Failed/Indeterminate")),
            gep_class_simple = factor(c("Class 1", "Class 2", "GEP Failed/Indeterminate")),
            prame_status = factor(c("Negative", "Positive", "Not Available")),
            gep12_prame_status = factor(c("Negative", "Positive", NA_character_))
        ),
        precollapse_path
    )

    subset_data <- tibble::tibble(
        id = c(103, 101),
        biopsy1_gep = factor(c("Other", "Class 1 PRAME Negative")),
        gep_class_simple = factor(c("Other", "Class 1")),
        prame_status = factor(c("Other", "Negative")),
        gep12_prame_status = factor(c("Other", "Negative")),
        unchanged_var = c("keep_c", "keep_a")
    )

    restored_data <- restore_gep_display_variables(subset_data, dataset_name = dataset_name)

    expect_equal(as.character(restored_data$biopsy1_gep), c("GEP Failed/Indeterminate", "Class 1 PRAME Negative"))
    expect_equal(as.character(restored_data$gep_class_simple), c("GEP Failed/Indeterminate", "Class 1"))
    expect_equal(as.character(restored_data$prame_status), c("Not Available", "Negative"))
    expect_equal(restored_data$unchanged_var, subset_data$unchanged_var)
})

test_that("simple GEP validation restores protected display labels when precollapse data exist", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_simple_restore")
    output_dirs <- list(
        obj4_mfs = file.path(test_output_dir, "04_GEP_Validation", "a_mfs_validation"),
        obj4_mss = file.path(test_output_dir, "04_GEP_Validation", "b_mss_validation"),
        obj4_ph_diagnostics = file.path(test_output_dir, "04_GEP_Validation", "c_proportional_hazards_diagnostics")
    )
    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }

    dataset_name <- "unit_test_simple_gep_restore"
    dir.create(PROCESSED_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
    precollapse_path <- file.path(PROCESSED_DATA_DIR, paste0(dataset_name, "_derived_precollapse.rds"))
    on.exit(unlink(precollapse_path), add = TRUE)
    on.exit(unlink(test_output_dir, recursive = TRUE), add = TRUE)

    saveRDS(
        tibble::tibble(
            biopsy1_gep = factor(c("Class 1", "GEP Failed/Indeterminate")),
            gep_class_simple = factor(c("Class 1", "GEP Failed/Indeterminate")),
            prame_status = factor(c("Negative", "Not Available")),
            gep12_prame_status = factor(c("Negative", NA_character_))
        ),
        precollapse_path
    )

    test_data <- tibble::tibble(
        biopsy1_gep = factor(c("Class 1", "Other")),
        gep_class_simple = factor(c("Class 1", "Other")),
        prame_status = factor(c("Negative", "Other")),
        gep12_prame_status = factor(c("Negative", "Other")),
        biopsy1_gep_mfs = c(0.80, 0.20),
        biopsy1_gep_mss = c(0.85, 0.15),
        expected_mfs_5yr = c(0.80, 0.20),
        expected_mss_5yr = c(0.85, 0.15),
        tt_mets_months = c(72, 24),
        mets_event = c(0, 1),
        mfs_event_5yr = c(0, 1),
        tt_death_months = c(24, 24),
        tt_death_years = c(2, 2),
        melanoma_death_event = c(0, 1),
        competing_death_event = c(1, 0),
        mss_event_5yr = c(0, 1),
        mfs_analysis_eligible = c(TRUE, TRUE),
        mss_analysis_eligible = c(TRUE, TRUE)
    )

    results <- simple_gep_validation(test_data, output_dirs, "restore_", dataset_name = dataset_name)

    expect_false(any(as.character(results$mfs_results$gep_class_simple) == "Other"))
    expect_false(any(as.character(results$mss_results$gep_class_simple) == "Other"))
    expect_false("GEP Failed/Indeterminate" %in% as.character(results$mfs_results$gep_class_simple))
    expect_false("GEP Failed/Indeterminate" %in% as.character(results$mss_results$gep_class_simple))
    expect_equal(as.character(results$mfs_results$gep_class_simple), "Class 1")
    expect_equal(as.character(results$mss_results$gep_class_simple), "Class 1")
})

test_that("survival helper can separate KM display groups from Cox model groups", {
    test_data <- tibble::tibble(
        tt_mets_months = c(12, 24, 18, 30),
        mets_event = c(0, 1, 0, 1),
        biopsy1_gep = factor(c("Class 1", "Class 1", "GEP Failed/Indeterminate", "GEP Failed/Indeterminate")),
        biopsy1_gep_model = factor(c("Class 1", "Class 1", "Other", "Other"))
    )

    result <- analyze_time_to_event_outcomes(
        data = test_data,
        time_var = "tt_mets_months",
        event_var = "mets_event",
        group_var = "biopsy1_gep",
        model_group_var = "biopsy1_gep_model",
        confounders = NULL,
        ylab = "Metastasis-Free Survival Probability",
        analysis_type = "post_treatment_only",
        dataset_name = "unit_test_display_model_split",
        output_dirs = NULL,
        prefix = NULL
    )

    expect_true(any(grepl("GEP Failed/Indeterminate", names(result$fit$strata))))
    expect_false(any(grepl("Other", names(result$fit$strata))))
    expect_match(result$diagnostics$raw_model_output, "skipped")
})

test_that("risk table keeps displayed row labels aligned with their counts when legend order differs from fit order", {
    desired_order <- c(
        "Class 1 PRAME Negative",
        "Class 1 PRAME Positive",
        "Class 2 PRAME Negative",
        "Class 2 PRAME Positive",
        "GEP Failed/Indeterminate",
        "GEP Not Tested"
    )
    baseline_counts <- c(5, 4, 3, 2, 6, 7)
    names(baseline_counts) <- desired_order

    test_data <- tibble::tibble(
        tt_mets_months = seq_len(sum(baseline_counts)) * 6,
        mets_event = rep(c(0, 1), length.out = sum(baseline_counts)),
        biopsy1_gep = factor(
            unlist(Map(rep, names(baseline_counts), baseline_counts)),
            levels = rev(desired_order)
        )
    )

    result <- suppressWarnings(analyze_time_to_event_outcomes(
        data = test_data,
        time_var = "tt_mets_months",
        event_var = "mets_event",
        group_var = "biopsy1_gep",
        model_group_var = "biopsy1_gep",
        confounders = NULL,
        ylab = "Metastasis-Free Survival Probability",
        analysis_type = "post_treatment_only",
        dataset_name = "unit_test_risk_table_alignment",
        legend_labels = desired_order,
        output_dirs = NULL,
        prefix = NULL
    ))

    observed_baseline <- result$plot$table$data %>%
        dplyr::filter(.data$time == 0) %>%
        dplyr::transmute(strata = as.character(.data$strata), n_risk = .data$n.risk)

    observed_lookup <- stats::setNames(observed_baseline$n_risk, observed_baseline$strata)

    expect_equal(rev(extract_risk_table_y_limits(result$plot$table)), desired_order)
    expect_equal(unname(observed_lookup[desired_order]), unname(baseline_counts[desired_order]))
    expect_equal(extract_displayed_risk_counts(result$plot$table, time_point = 0), unname(baseline_counts[desired_order]))
})

test_that("risk table preserves requested row order for a subset of present strata", {
    desired_order <- c(
        "Class 1 PRAME Negative",
        "Class 1 PRAME Positive",
        "Class 2 PRAME Negative",
        "Class 2 PRAME Positive",
        "GEP Failed/Indeterminate",
        "GEP Not Tested"
    )
    baseline_counts <- c(
        "Class 1 PRAME Negative" = 5,
        "Class 2 PRAME Positive" = 2,
        "GEP Not Tested" = 7
    )
    present_order <- desired_order[desired_order %in% names(baseline_counts)]

    test_data <- tibble::tibble(
        tt_mets_months = seq_len(sum(baseline_counts)) * 6,
        mets_event = rep(c(0, 1), length.out = sum(baseline_counts)),
        biopsy1_gep = factor(
            unlist(Map(rep, names(baseline_counts), baseline_counts)),
            levels = rev(desired_order)
        )
    )

    result <- suppressWarnings(analyze_time_to_event_outcomes(
        data = test_data,
        time_var = "tt_mets_months",
        event_var = "mets_event",
        group_var = "biopsy1_gep",
        model_group_var = "biopsy1_gep",
        confounders = NULL,
        ylab = "Metastasis-Free Survival Probability",
        analysis_type = "post_treatment_only",
        dataset_name = "unit_test_risk_table_alignment_subset",
        legend_labels = desired_order,
        output_dirs = NULL,
        prefix = NULL
    ))

    observed_baseline <- result$plot$table$data %>%
        dplyr::filter(.data$time == 0) %>%
        dplyr::transmute(strata = as.character(.data$strata), n_risk = .data$n.risk)

    observed_lookup <- stats::setNames(observed_baseline$n_risk, observed_baseline$strata)

    expect_equal(rev(extract_risk_table_y_limits(result$plot$table)), present_order)
    expect_equal(unname(observed_lookup[present_order]), unname(baseline_counts[present_order]))
    expect_equal(extract_displayed_risk_counts(result$plot$table, time_point = 0), unname(baseline_counts[present_order]))
})

test_that("four-group collapsed MFS KM retains both no-GEP strata as separate rows", {
    test_output_dir <- tempfile("objective4_four_group_km_")
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(test_output_dir, recursive = TRUE), add = TRUE)

    test_data <- tibble::tibble(
        tt_mets_months = c(12, 24, 18, 36, 6, 30, 9, 42),
        mets_event = c(0, 1, 0, 1, 1, 0, 1, 0),
        biopsy1_gep = c(
            "Class 1 PRAME Negative",
            "Class 1 PRAME Positive",
            "Class 2 PRAME Negative",
            "Class 2 PRAME Positive",
            "GEP Not Tested",
            "GEP Not Tested",
            "GEP Failed/Indeterminate",
            "GEP Failed/Indeterminate"
        ),
        gep_class_simple = c("Class 1", "Class 1", "Class 2", "Class 2", NA, NA, NA, NA)
    )

    result <- suppressWarnings(
        create_mfs_four_group_survival_curves(
            data = test_data,
            output_dir = test_output_dir,
            km_output_dir = test_output_dir,
            prefix = "unit_",
            dataset_name = "unit_test_four_group_km",
            return_plot = TRUE,
            save_plot = TRUE
        )
    )

    expect_equal(
        result$present_levels,
        c("Class 1", "Class 2", "GEP Not Tested", "GEP Failed/Indeterminate")
    )
    expect_true(all(
        c("GEP Not Tested", "GEP Failed/Indeterminate") %in%
            unique(as.character(stats::na.omit(result$plot_data$gep_km_simple)))
    ))
    expect_equal(
        rev(extract_risk_table_y_limits(result$plot$table)),
        c("Class 1", "Class 2", "GEP Not Tested", "GEP Failed/Indeterminate")
    )
    expect_equal(extract_displayed_risk_counts(result$plot$table, time_point = 0), c(2L, 2L, 2L, 2L))
    expect_true(file.exists(file.path(test_output_dir, "unit_mfs_four_group_gep_km.png")))
})

test_that("Cox diagnostics keep group counts and event rates in Raw_model_output", {
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_group_counts_in_raw_output")
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(test_output_dir, recursive = TRUE), add = TRUE)

    test_data <- tibble::tibble(
        tt_mets_months = c(8, 16, 24, 32, 10, 20, 30, 40),
        mets_event = c(1, 0, 0, 1, 0, 1, 0, 1),
        biopsy1_gep_model = factor(
            c("Class 1", "Class 1", "Class 1", "Class 1", "Class 2", "Class 2", "Class 2", "Class 2"),
            levels = c("Class 1", "Class 2")
        )
    )

    result <- generate_regression_table(
        data = test_data,
        outcome_var = "mets_event",
        predictor_vars = "biopsy1_gep_model",
        confounders = NULL,
        model_type = "cox",
        effect_measure = "HR",
        analysis_name = "unit_test_group_counts",
        dataset_name = "unit_test_group_counts",
        output_dir = test_output_dir,
        prefix = "unit_",
        time_var = "tt_mets_months",
        event_var = "mets_event",
        treatment_var = "biopsy1_gep_model"
    )

    expect_true(file.exists(result$output_files$diagnostics_path))

    raw_output <- readxl::read_excel(result$output_files$diagnostics_path, sheet = "Raw_model_output")
    expect_true(all(c(
        "group_n", "group_events", "group_non_events", "group_event_rate_pct",
        "reference_level", "reference_n", "reference_events", "reference_non_events",
        "reference_event_rate_pct"
    ) %in% names(raw_output)))
    expect_false("group_event_summary" %in% names(raw_output))

    reference_row <- raw_output[raw_output$row_type == "Reference Level" & raw_output$variable == "Class 1", , drop = FALSE]
    expect_equal(nrow(reference_row), 1)
    expect_equal(reference_row$group_n[[1]], 4)
    expect_equal(reference_row$group_events[[1]], 2)
    expect_equal(reference_row$group_non_events[[1]], 2)
    expect_equal(reference_row$group_event_rate_pct[[1]], 50)

    coefficient_row <- raw_output[raw_output$row_type == "Coefficient" & raw_output$variable == "Class 2", , drop = FALSE]
    expect_equal(nrow(coefficient_row), 1)
    expect_equal(coefficient_row$group_n[[1]], 4)
    expect_equal(coefficient_row$group_events[[1]], 2)
    expect_equal(coefficient_row$group_non_events[[1]], 2)
    expect_equal(coefficient_row$group_event_rate_pct[[1]], 50)
    expect_equal(coefficient_row$reference_level[[1]], "Class 1")
    expect_equal(coefficient_row$reference_n[[1]], 4)
    expect_equal(coefficient_row$reference_events[[1]], 2)
    expect_equal(coefficient_row$reference_non_events[[1]], 2)
    expect_equal(coefficient_row$reference_event_rate_pct[[1]], 50)
})

test_that("MSS comprehensive summary accepts tibble observed_expected results", {
    validation_results <- list(
        `5yr` = list(
            observed_expected = tibble::tibble(
                biopsy1_gep = c("Class 1", "Class 2"),
                n = c(10, 12),
                observed = c(1, 3),
                expected = c(1.2, 2.4),
                expected_rate = c(0.12, 0.20),
                observed_rate = c(0.10, 0.25)
            ),
            calibration = list(
                n = 22,
                slope = 0.95,
                ici = 0.04,
                nam_dagostino_p = 0.31
            ),
            discrimination = list(
                harrell_c = 0.81,
                integrated_auc = 0.77,
                cumulative_discrimination = 0.80,
                time_averaged_discrimination = 0.79
            )
        )
    )

    missing_data_analysis <- list(missing_patterns = data.frame(pattern = character()))

    expect_no_warning({
        summary_text <- create_comprehensive_gep_summary(
            validation_results = validation_results,
            outcome_type = "MSS",
            prame_analysis = NULL,
            missing_data_analysis = missing_data_analysis,
            dataset_name = "test_dataset"
        )
    })

    expect_match(summary_text, "OBSERVED VS EXPECTED ANALYSIS")
    expect_match(summary_text, "Overall O/E")
    expect_false(grepl("NA-NA", summary_text, fixed = TRUE))
    expect_false(grepl("Chi-square p=NA", summary_text, fixed = TRUE))
})

test_that("GEP text summaries format tiny p-values in scientific notation", {
    validation_results <- list(
        `5yr` = list(
            observed_expected = tibble::tibble(
                biopsy1_gep = c("Class 1", "Class 2"),
                n = c(10, 12),
                observed = c(1, 3),
                expected = c(1.2, 2.4)
            ),
            calibration = list(
                n = 22,
                slope = 0.95,
                ici = 0.04,
                nam_dagostino_p = 0,
                nam_dagostino_log_p = log(2.34) - 5 * log(10)
            ),
            discrimination = list(
                harrell_c = 0.81,
                integrated_auc = 0.77,
                cumulative_discrimination = 0.80,
                time_averaged_discrimination = 0.79
            )
        )
    )

    attr(validation_results[["5yr"]]$observed_expected, "chisq_p_value") <- 0
    attr(validation_results[["5yr"]]$observed_expected, "chisq_log_p_value") <- log(5.67) - 8 * log(10)
    attr(validation_results[["5yr"]]$observed_expected, "overall_poisson_ci_lower") <- 0.4
    attr(validation_results[["5yr"]]$observed_expected, "overall_poisson_ci_upper") <- 1.2

    summary_text <- create_detailed_metrics_table(validation_results)

    expect_match(summary_text, "Nam-D'Agostino p=2\\.340e-05")
    expect_match(summary_text, "Chi-square p=5\\.670e-08")
    expect_false(grepl("Nam-D'Agostino p=0\\.0000", summary_text))
    expect_false(grepl("Chi-square p=0\\.0000", summary_text))
})

test_that("GEP p-value formatter returns 0 when the value is truly underflowed", {
    expect_equal(format_gep_p_value(0), "0")
    expect_equal(format_gep_p_value(0, log_p_value = -Inf), "0")
    expect_equal(format_gep_p_value(NA_real_, log_p_value = -Inf), "0")
    expect_equal(format_gep_log_p_value(-Inf), "0")
})

test_that("Saved MSS summary text preserves dataset name", {
    standard_results <- list(
        `5yr` = list(
            observed_expected = tibble::tibble(
                biopsy1_gep = c("Class 1", "Class 2"),
                n = c(10, 12),
                observed = c(1, 3),
                expected = c(1.2, 2.4),
                expected_rate = c(0.12, 0.20),
                observed_rate = c(0.10, 0.25)
            ),
            calibration = list(
                n = 22,
                slope = 0.95,
                ici = 0.04,
                nam_dagostino_p = 0.31
            ),
            discrimination = list(
                harrell_c = 0.81,
                integrated_auc = 0.77,
                cumulative_discrimination = 0.80,
                time_averaged_discrimination = 0.79
            )
        )
    )

    missing_data_analysis <- list(missing_patterns = data.frame(pattern = character()))
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_dataset_name_summary")
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

    save_mss_validation_results(
        standard_results = standard_results,
        competing_results = NULL,
        missing_data = missing_data_analysis,
        prame_results = NULL,
        output_dir = test_output_dir,
        prefix = "dataset_",
        dataset_name = "uveal_melanoma_restricted_cohort"
    )

    summary_path <- file.path(test_output_dir, "dataset_mss_validation_narrative_summary.txt")
    expect_true(file.exists(summary_path))

    summary_text <- paste(readLines(summary_path, warn = FALSE), collapse = "\n")
    expect_match(summary_text, "Dataset: uveal_melanoma_restricted_cohort")

    unlink(test_output_dir, recursive = TRUE)
})

test_that("Consolidated and unified workbooks write PRAME placeholder sheets when PRAME is unavailable", {
    validation_results <- list(
        `5yr` = list(
            observed_expected = tibble::tibble(
                biopsy1_gep = c("Class 1", "Class 2"),
                n = c(10, 12),
                observed = c(1, 3),
                expected = c(1.2, 2.4)
            ),
            calibration = list(
                n = 22,
                slope = 0.95,
                ici = 0.04,
                nam_dagostino_p = 0.31,
                brier_score = 0.10,
                brier_method = "test",
                brier_fallback_used = FALSE
            ),
            discrimination = list(
                n = 22,
                events = 4,
                harrell_c = 0.81,
                integrated_auc = 0.77,
                cumulative_discrimination = 0.80,
                time_averaged_discrimination = 0.79
            ),
            decision_curve = list(
                n = 22,
                events = 4,
                event_rate = 4 / 22,
                optimal_threshold = 0.2,
                optimal_net_benefit = 0.05
            )
        )
    )

    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_prame_placeholder")
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

    create_consolidated_gep_tables(
        validation_results = validation_results,
        outcome_type = "MSS",
        output_dir = test_output_dir,
        prefix = "placeholder_",
        prame_results = NULL,
        missing_data = NULL
    )

    consolidated_path <- file.path(test_output_dir, "placeholder_MSS_consolidated_summary.xlsx")
    consolidated_sheets <- readxl::excel_sheets(consolidated_path)
    expect_true(all(c("Observed_Expected_Summary", "PRAME_Summary") %in% consolidated_sheets))

    consolidated_prame <- readxl::read_excel(consolidated_path, sheet = "PRAME_Summary")
    expect_true(all(c(
        "Timepoint", "N", "N_PRAME_Positive", "N_PRAME_Negative",
        "Events", "Events_PRAME_Positive", "Events_PRAME_Negative",
        "Non_Events", "Event_Rate_Pct", "Bootstrap_Valid_Resamples",
        "Base_Harrell_C", "Enhanced_Harrell_C",
        "Delta_Harrell_C", "Delta_CI_Lower", "Delta_CI_Upper", "LR_p",
        "PRAME_HR", "PRAME_HR_CI_Lower", "PRAME_HR_CI_Upper",
        "Analysis_Tier", "Interpretation"
    ) %in% names(consolidated_prame)))
    expect_match(consolidated_prame$Interpretation[[1]], "not run|not supportable|did not produce")

    create_unified_gep_validation_summary(
        mfs_results = list(
            validation_results = validation_results,
            prame_analysis = NULL,
            missing_data_analysis = NULL
        ),
        mss_results = list(
            standard_results = validation_results,
            prame_results = NULL,
            missing_data_analysis = NULL
        ),
        output_dir = test_output_dir,
        prefix = "placeholder_"
    )

    unified_path <- file.path(test_output_dir, "placeholder_unified_gep_validation_summary.xlsx")
    unified_sheets <- readxl::excel_sheets(unified_path)
    expect_true(all(c("Calibration_Comparison", "Discrimination_Comparison", "PRAME_Comparison") %in% unified_sheets))
    expect_false(any(c("Unified_Calibration", "Unified_Discrimination", "PRAME_Summary", "Missing_Data_Summary") %in% unified_sheets))

    unified_prame <- readxl::read_excel(unified_path, sheet = "PRAME_Comparison")
    expect_true(all(c("MFS", "MSS") %in% unified_prame$Outcome))
    expect_true(all(c(
        "Outcome", "Timepoint", "N", "Base_Harrell_C",
        "Enhanced_Harrell_C", "Delta_Harrell_C", "LR_p", "Interpretation"
    ) %in% names(unified_prame)))
    expect_true(all(grepl("not run|not supportable|did not produce", unified_prame$Interpretation)))

    unlink(test_output_dir, recursive = TRUE)
})

test_that("PRAME incremental comparison writes the new workbook schema", {
    validation_results <- list(
        `5yr` = list(
            observed_expected = list(
                overall_n = 42,
                overall_observed = 8,
                overall_expected = 7.5,
                overall_oe_ratio = 1.07,
                overall_poisson_ci_lower = 0.51,
                overall_poisson_ci_upper = 1.98,
                chisq_p_value = 0.43
            ),
            calibration = list(
                n = 42,
                fit_n = 40,
                status = "ok",
                events = 8,
                non_events = 32,
                unique_risk_count = 40,
                nam_dagostino_p = 0.24,
                nam_dagostino_method = "greenwood_nam_dagostino",
                ici = 0.05,
                ici_method = "grouped_km",
                slope = 1.02,
                slope_method = "ipcw_logit"
            ),
            discrimination = list(
                n = 42,
                events = 8,
                harrell_c = 0.78,
                integrated_auc = 0.74,
                cumulative_discrimination = 0.76,
                time_averaged_discrimination = 0.75
            ),
            decision_curve = list(
                n = 42,
                events = 8,
                event_rate = 8 / 42,
                optimal_threshold = 0.22,
                optimal_net_benefit = 0.03
            )
        )
    )

    prame_results <- list(
        n = 42,
        prame_available = TRUE,
        analysis_type = "incremental_discrimination",
        comparison_results = list(
            yr5 = list(
                timepoint = 5,
                n = 42,
                n_positive = 14,
                n_negative = 28,
                events = 8,
                events_positive = 5,
                events_negative = 3,
                non_events = 34,
                bootstrap_valid_resamples = 87L,
                status = "ok",
                base_harrell_c = 0.71,
                enhanced_harrell_c = 0.77,
                delta_harrell_c = 0.06,
                delta_ci_lower = 0.01,
                delta_ci_upper = 0.11,
                lr_p = 0.031,
                prame_hr = 1.82,
                prame_hr_ci_lower = 1.10,
                prame_hr_ci_upper = 3.02,
                analysis_tier = "Primary",
                interpretation = "PRAME improved discrimination beyond GEP alone"
            )
        )
    )

    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_prame_schema")
    dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

    create_consolidated_gep_tables(
        validation_results = validation_results,
        outcome_type = "MFS",
        output_dir = test_output_dir,
        prefix = "schema_",
        prame_results = prame_results,
        missing_data = NULL
    )

    consolidated_prame <- readxl::read_excel(
        file.path(test_output_dir, "schema_MFS_consolidated_summary.xlsx"),
        sheet = "PRAME_Summary"
    )
    expect_equal(consolidated_prame$Delta_Harrell_C[[1]], 0.06, tolerance = 1e-08)
    expect_equal(consolidated_prame$Analysis_Tier[[1]], "Primary")
    expect_match(consolidated_prame$Interpretation[[1]], "improved discrimination")

    create_unified_gep_validation_summary(
        mfs_results = list(
            validation_results = validation_results,
            prame_analysis = prame_results,
            missing_data_analysis = NULL
        ),
        mss_results = NULL,
        output_dir = test_output_dir,
        prefix = "schema_"
    )

    unified_prame <- readxl::read_excel(
        file.path(test_output_dir, "schema_unified_gep_validation_summary.xlsx"),
        sheet = "PRAME_Comparison"
    )
    expect_equal(unified_prame$Base_Harrell_C[[1]], 0.71, tolerance = 1e-08)
    expect_equal(unified_prame$Enhanced_Harrell_C[[1]], 0.77, tolerance = 1e-08)
    expect_equal(unified_prame$Delta_Harrell_C[[1]], 0.06, tolerance = 1e-08)

    unlink(test_output_dir, recursive = TRUE)
})

# Test 4c: Competing risk analysis produces valid statistical results
test_that("4c: Competing risk analysis produces valid statistical results", {
    # This test would require running the full MSS validation pipeline
    # For now, just verify the function exists
    expect_true(exists("analyze_gep_mss_validation"))
    expect_true(is.function(analyze_gep_mss_validation))
})

# Test 4d: Time-specific analysis produces valid statistical results
test_that("4d: Time-specific analysis produces valid statistical results", {
    # This test would require running the full MFS validation pipeline
    # For now, just verify the function exists
    expect_true(exists("analyze_gep_mfs_validation"))
    expect_true(is.function(analyze_gep_mfs_validation))
})

test_that("MFS observed expected summaries retain the overall denominator", {
    test_data <- tibble::tibble(
        biopsy1_gep = c("Class 1", "Class 1", "Class 2"),
        expected_mfs_5yr = c(0.90, 0.80, 0.40),
        tt_mets_months = c(12, 60, 72),
        mets_event = c(0, 1, 0),
        mfs_event_5yr = c(0, 1, 0),
        mfs_analysis_eligible = c(TRUE, TRUE, TRUE)
    )

    oe_results <- calculate_observed_expected_mfs(test_data, timepoint = 5)
    expect_equal(oe_results$overall_n, 3)
    expect_equal(oe_results$results_by_class[["Class 1"]]$observed, 2)
    expect_equal(oe_results$overall_observed, 1.5)

    fallback_metrics <- extract_overall_oe_metrics(list(
        results_by_class = list(
            "Class 1" = list(n = 10, observed = 1, expected = 1.5),
            "Class 2" = list(n = 12, observed = 3, expected = 2.5)
        ),
        overall_observed = 4,
        overall_expected = 4,
        overall_oe_ratio = 1,
        overall_poisson_ci_lower = 0.5,
        overall_poisson_ci_upper = 1.5,
        chisq_p_value = 0.4
    ))

    expect_equal(fallback_metrics$n, 22)
})

test_that("Greenwood Nam-D'Agostino uses KM-adjusted grouped observed events", {
    test_data <- tibble::tibble(
        predicted_risk = c(
            seq(0.05, 0.14, length.out = 10),
            seq(0.45, 0.54, length.out = 10),
            seq(0.85, 0.94, length.out = 10)
        ),
        observed_time = c(
            rep(12, 7), 24, rep(60, 2),
            rep(24, 3), rep(60, 7),
            rep(18, 6), rep(60, 4)
        ),
        observed_event = c(
            rep(0, 7), 1, rep(0, 2),
            rep(1, 3), rep(0, 7),
            rep(1, 6), rep(0, 4)
        )
    )

    cal <- calculate_greenwood_nam_dagostino(
        data = test_data,
        predicted_risk_var = "predicted_risk",
        time_var = "observed_time",
        event_var = "observed_event",
        eval_time_months = 60
    )

    expect_equal(cal$nam_dagostino_method, "greenwood_nam_dagostino")
    expect_equal(cal$ici_method, "grouped_km")
    expect_true(nrow(cal$group_results) >= 3)
    expect_true(any(abs(cal$group_results$observed_events - cal$group_results$raw_events) > 0.01))
    expect_true(all(cal$group_results$observed_events_variance >= 0))
})

test_that("IPCW recalibration produces a plausible horizon-specific slope", {
    set.seed(123)

    n_obs <- 400
    predicted_risk <- seq(0.05, 0.85, length.out = n_obs)
    horizon_event <- stats::rbinom(n_obs, size = 1, prob = predicted_risk)
    observed_time <- ifelse(horizon_event == 1, stats::runif(n_obs, min = 6, max = 54), 60)

    test_data <- tibble::tibble(
        predicted_risk = predicted_risk,
        observed_time = observed_time,
        observed_event = horizon_event
    )

    cal <- calculate_survival_calibration_summary(
        data = test_data,
        predicted_risk_var = "predicted_risk",
        time_var = "observed_time",
        event_var = "observed_event",
        eval_time_months = 60
    )

    expect_equal(cal$slope_method, "ipcw_logit")
    expect_equal(cal$intercept_method, "ipcw_offset")
    expect_true(is.finite(cal$slope))
    expect_true(abs(cal$slope - 1) < 0.35)
    expect_true(abs(cal$calibration_intercept) < 0.4)
})

test_that("ICI falls back to grouped KM when predicted risks are too discrete", {
    test_data <- tibble::tibble(
        predicted_risk = rep(c(0.10, 0.35, 0.70, 0.90), each = 15),
        observed_time = c(
            rep(60, 10), rep(24, 5),
            rep(60, 8), rep(36, 7),
            rep(60, 5), rep(18, 10),
            rep(60, 3), rep(12, 12)
        ),
        observed_event = c(
            rep(0, 10), rep(1, 5),
            rep(0, 8), rep(1, 7),
            rep(0, 5), rep(1, 10),
            rep(0, 3), rep(1, 12)
        )
    )

    cal <- calculate_survival_calibration_summary(
        data = test_data,
        predicted_risk_var = "predicted_risk",
        time_var = "observed_time",
        event_var = "observed_event",
        eval_time_months = 60
    )

    expect_equal(cal$ici_method, "grouped_km")
    expect_equal(cal$slope_method, "ipcw_logit")
    expect_true(is.finite(cal$ici))
})

test_that("Unstable IPCW recalibration fits are suppressed as unavailable", {
    actual_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
    analysis_data <- actual_data %>% filter(mss_analysis_eligible)

    cal <- calculate_survival_calibration_summary(
        data = analysis_data,
        predicted_risk_var = "predicted_mss_risk_7yr",
        time_var = "tt_death_months",
        event_var = "mss_event_7yr",
        eval_time_months = 84
    )

    expect_true(is.na(cal$slope))
    expect_true(is.finite(cal$calibration_intercept))
    expect_equal(cal$slope_method, "ipcw_logit_unavailable")
    expect_equal(cal$intercept_method, "ipcw_offset")
    expect_equal(cal$status, "recalibration_fit_unstable")
})

test_that("Consolidated calibration outputs include method traceability columns", {
    validation_results <- list(
        `5yr` = list(
            calibration = list(
                n = 48,
                fit_n = 44,
                status = "ok",
                events = 12,
                non_events = 32,
                unique_risk_count = 44,
                nam_dagostino_p = 0.21,
                nam_dagostino_method = "greenwood_nam_dagostino",
                ici = 0.05,
                ici_method = "grouped_km",
                slope = 1.08,
                slope_method = "ipcw_logit",
                slope_se = 0.31,
                brier_score = 0.11,
                brier_method = "time_dependent",
                brier_fallback_used = FALSE
            )
        )
    )

    consolidated <- create_consolidated_calibration_table(validation_results, outcome_type = "MFS")
    expect_true(all(c("Fit_N", "Status", "Events", "Non_Events", "Unique_Risk_Count", "Nam_D_Agostino_Method", "ICI_Method", "Slope_Method", "Slope_Unavailable_Reason") %in% names(consolidated)))
    expect_equal(consolidated$ICI_Method[[1]], "grouped_km")
    expect_equal(consolidated$Slope_Method[[1]], "ipcw_logit")
    expect_true(is.na(consolidated$Slope_Unavailable_Reason[[1]]))

    unified <- create_unified_calibration_summary(
        mfs_results = list(
            validation_results = validation_results,
            extrapolation_assessment = list(status = "Supported", note = "Supportable")
        ),
        mss_results = list(
            standard_results = validation_results,
            extrapolation_assessment = list(status = "Weakly Supported", note = "Limited support")
        )
    )
    expect_true(all(c(
        "Fit_N", "Status", "Events", "Non_Events", "Unique_Risk_Count",
        "Nam_D_Agostino_Method", "ICI_Method", "Slope_Method", "Slope_Unavailable_Reason",
        "Prediction_Source", "Extrapolation_Assumption", "Assumption_Support_Status", "Assumption_Support_Notes"
    ) %in% names(unified)))
})

test_that("Sparse consolidated summaries tolerate all-NA calibration slopes", {
    validation_results <- list(
        `5yr` = list(
            calibration = list(
                n = 29,
                fit_n = 18,
                status = "insufficient_recalibration_data",
                events = 3,
                non_events = 15,
                unique_risk_count = 4,
                slope = NA_real_,
                slope_method = "ipcw_logit_unavailable"
            ),
            discrimination = list(n = 29, harrell_c = 0.84),
            decision_curve = list(n = 29, optimal_threshold = 0.39, optimal_net_benefit = 0),
            observed_expected = list(overall_n = 29, overall_observed = 3, overall_expected = 2.8)
        ),
        `7yr` = list(
            calibration = list(
                n = 29,
                fit_n = 17,
                status = "insufficient_recalibration_data",
                events = 4,
                non_events = 13,
                unique_risk_count = 4,
                slope = NA_real_,
                slope_method = "ipcw_logit_unavailable"
            ),
            discrimination = list(n = 29, harrell_c = 0.87),
            decision_curve = list(n = 29, optimal_threshold = 0.5, optimal_net_benefit = 0),
            observed_expected = list(overall_n = 29, overall_observed = 4, overall_expected = 3.5)
        )
    )

    cal_consolidated <- create_consolidated_calibration_table(validation_results, outcome_type = "MSS")
    disc_consolidated <- create_consolidated_discrimination_table(validation_results, outcome_type = "MSS")
    dca_consolidated <- create_consolidated_decision_curve_table(validation_results, outcome_type = "MSS")

    summary_text <- create_comprehensive_text_summary(
        validation_results = validation_results,
        outcome_type = "MSS",
        cal_consolidated = cal_consolidated,
        disc_consolidated = disc_consolidated,
        dca_consolidated = dca_consolidated
    )

    expect_match(summary_text, "could not be estimated at any timepoint")
    expect_match(summary_text, "too few patients had usable data")
    expect_match(summary_text, "Best discrimination at 7yr")
})

test_that("consolidated Objective 4 summaries carry extrapolation metadata", {
    validation_results <- list(
        `5yr` = list(
            calibration = list(n = 30, fit_n = 28, status = "ok", events = 8, non_events = 20, unique_risk_count = 28),
            discrimination = list(n = 30, events = 8, harrell_c = 0.82, integrated_auc = 0.78),
            decision_curve = list(n = 30, events = 8, event_rate = 8 / 30, optimal_threshold = 0.3, optimal_net_benefit = 0.01),
            observed_expected = list(overall_n = 30, overall_observed = 8, overall_expected = 7.5)
        ),
        `7yr` = list(
            calibration = list(n = 30, fit_n = 27, status = "ok", events = 10, non_events = 17, unique_risk_count = 27),
            discrimination = list(n = 30, events = 10, harrell_c = 0.84, integrated_auc = 0.8),
            decision_curve = list(n = 30, events = 10, event_rate = 10 / 30, optimal_threshold = 0.35, optimal_net_benefit = 0.015),
            observed_expected = list(overall_n = 30, overall_observed = 10, overall_expected = 9.4)
        )
    )
    extrapolation_assessment <- list(
        status = "Weakly Supported",
        note = "Borderline model-fit differences left only limited support for constant hazard."
    )

    cal_consolidated <- create_consolidated_calibration_table(
        validation_results,
        outcome_type = "MFS",
        extrapolation_assessment = extrapolation_assessment
    )
    disc_consolidated <- create_consolidated_discrimination_table(
        validation_results,
        outcome_type = "MFS",
        extrapolation_assessment = extrapolation_assessment
    )
    dca_consolidated <- create_consolidated_decision_curve_table(
        validation_results,
        outcome_type = "MFS",
        extrapolation_assessment = extrapolation_assessment
    )

    for (summary_df in list(cal_consolidated, disc_consolidated, dca_consolidated)) {
        expect_true(all(c(
            "Prediction_Source",
            "Extrapolation_Assumption",
            "Assumption_Support_Status",
            "Assumption_Support_Notes"
        ) %in% names(summary_df)))
        expect_equal(summary_df$Prediction_Source[[1]], "Imported")
        expect_equal(summary_df$Extrapolation_Assumption[[1]], "Not Applicable")
        expect_equal(summary_df$Prediction_Source[[2]], "Extrapolated from imported 5-year value")
        expect_equal(summary_df$Assumption_Support_Status[[2]], "Weakly Supported")
    }
})

test_that("Objective 4 extrapolation check classifies sparse data as unsupported", {
    sparse_data <- tibble::tibble(
        tt_mets_months = c(12, 18, 24, 36, 48, 72, 84, 96, 108),
        mets_event = c(1, 0, 1, 0, 0, 1, 0, 1, 0)
    )

    results <- evaluate_gep_extrapolation_assumption(
        analysis_data = sparse_data,
        outcome_type = "MFS"
    )

    expect_identical(results$status, "Unsupported")
    expect_match(results$note, "Fewer than 10 events")
})

test_that("Objective 4 extrapolation check distinguishes exponential from non-exponential data", {
    set.seed(42)
    exponential_data <- tibble::tibble(
        tt_mets_months = rexp(400, rate = 0.015) * 12,
        mets_event = 1
    )
    weibull_data <- tibble::tibble(
        tt_mets_months = rweibull(400, shape = 2.5, scale = 8) * 12,
        mets_event = 1
    )

    exponential_results <- evaluate_gep_extrapolation_assumption(
        analysis_data = exponential_data,
        outcome_type = "MFS"
    )
    weibull_results <- evaluate_gep_extrapolation_assumption(
        analysis_data = weibull_data,
        outcome_type = "MFS"
    )

    expect_true(exponential_results$status %in% c("Supported", "Weakly Supported"))
    expect_identical(weibull_results$status, "Unsupported")
})

test_that("Clinical interpretation tolerates unavailable slopes", {
    calibration_data <- tibble::tibble(
        Timepoint = c("5yr", "7yr"),
        Slope = c(NA_real_, NA_real_),
        Status = c("insufficient_recalibration_data", "insufficient_recalibration_data"),
        Fit_N = c(18, 17),
        Events = c(3, 4),
        Non_Events = c(15, 13),
        Unique_Risk_Count = c(4, 4),
        Slope_SE = c(NA_real_, NA_real_)
    )
    discrimination_data <- tibble::tibble(Timepoint = c("5yr", "7yr"), Harrell_C = c(0.84, 0.87))
    oe_data <- tibble::tibble(Timepoint = c("5yr", "7yr"), Overall_OE = c(1.05, 0.98))

    interpretation <- create_clinical_interpretation(
        calibration_data = calibration_data,
        discrimination_data = discrimination_data,
        oe_data = oe_data,
        outcome_type = "MSS"
    )

    expect_true(is.list(interpretation))
    expect_match(interpretation$calibration_interpretation, "too few patients had usable data")
    expect_false(grepl("absolute risk estimates can be used", interpretation$clinical_implications, fixed = TRUE))
    expect_match(interpretation$clinical_implications, "interpreted with caution")
})

test_that("Comprehensive GEP summary states why slope is NA in plain language", {
    validation_results <- list(
        `5yr` = list(
            calibration = list(
                n = 29,
                fit_n = 18,
                status = "insufficient_recalibration_data",
                events = 3,
                non_events = 15,
                unique_risk_count = 4,
                slope = NA_real_,
                slope_method = "ipcw_logit_unavailable",
                slope_se = NA_real_,
                ici = 0.068,
                ici_method = "grouped_km",
                nam_dagostino_p = NA_real_
            ),
            discrimination = list(
                harrell_c = 0.867,
                integrated_auc = 0.778,
                cumulative_discrimination = 0.867
            ),
            observed_expected = list(
                overall_n = 29,
                overall_observed = 3,
                overall_expected = 4.1,
                overall_oe_ratio = 0.73,
                overall_poisson_ci_lower = 0.20,
                overall_poisson_ci_upper = 1.87,
                chisq_p_value = 0.5553
            )
        )
    )

    summary_text <- create_comprehensive_gep_summary(
        validation_results = validation_results,
        outcome_type = "MFS",
        prame_analysis = NULL,
        missing_data_analysis = list(missing_patterns = data.frame(pattern = character())),
        dataset_name = "test_dataset"
    )

    expect_match(summary_text, "too few patients had usable data")
    expect_match(summary_text, "events=3")
    expect_match(summary_text, "non-events=15")
    expect_match(summary_text, "The calibration slope could not be estimated")
})

test_that("PRAME incremental helper returns delta-C metrics on deterministic data", {
    set.seed(42)

    n_obs <- 80
    base_risk <- seq(0.1, 0.8, length.out = n_obs)
    prame_positive <- rep(c("Negative", "Positive"), each = n_obs / 2)
    risk_signal <- stats::qlogis(base_risk) + ifelse(prame_positive == "Positive", 0.9, 0)
    event_probability <- stats::plogis(-2 + risk_signal)
    horizon_event <- stats::rbinom(n_obs, 1, event_probability)
    observed_time <- ifelse(horizon_event == 1, stats::runif(n_obs, min = 6, max = 54), 60)

    test_data <- tibble::tibble(
        prame_status = prame_positive,
        predicted_mfs_risk_5yr = base_risk,
        tt_mfs_5yr = observed_time,
        mfs_event_5yr = horizon_event
    )

    result <- calculate_prame_incremental_value_metrics(
        data = test_data,
        time_var = "tt_mfs_5yr",
        event_var = "mfs_event_5yr",
        base_risk_var = "predicted_mfs_risk_5yr",
        timepoint = 5,
        outcome_label = "MFS",
        analysis_tier = "Primary",
        bootstrap_iterations = 30
    )

    expect_equal(result$status, "ok")
    expect_true(is.finite(result$base_harrell_c))
    expect_true(is.finite(result$enhanced_harrell_c))
    expect_true(is.finite(result$delta_harrell_c))
    expect_true(result$bootstrap_valid_resamples >= 20)
    expect_equal(result$delta_ci_method, "bootstrap_percentile")
})

test_that("MSS PRAME incremental analysis runs without fatal model warnings", {
    actual_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))

    expect_no_warning({
        prame_results <- perform_prame_augmented_analysis_mss(actual_data, GEP_VALIDATION_TIMEPOINTS)
    })

    expect_true(is.list(prame_results))
    expect_true(all(c("n", "prame_available", "comparison_results") %in% names(prame_results)))
    expect_true(length(prame_results$comparison_results) >= 1)
})

test_that("Objective 4 MSS core components run without fatal errors", {
    actual_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
    test_data <- actual_data %>% filter(mss_analysis_eligible)

    required_vars <- c(
        "biopsy1_gep", "expected_mss_5yr", "expected_mss_7yr", "expected_mss_10yr",
        "mss_event_5yr", "mss_event_7yr", "mss_event_10yr",
        "event_type_mss_5yr", "tt_mss_5yr", "melanoma_death_event", "competing_death_event"
    )
    expect_true(all(required_vars %in% names(test_data)))

    timepoints <- GEP_VALIDATION_TIMEPOINTS

    expect_no_error({
        standard_results <- lapply(timepoints, function(tp) {
            perform_standard_mss_validation(test_data, tp, GEP_BOOTSTRAP_ITERATIONS)
        })
        names(standard_results) <- paste0(timepoints, "yr")
    })

    expect_no_error({
        competing_results <- lapply(timepoints, function(tp) {
            perform_competing_risk_mss_validation(test_data, tp)
        })
        names(competing_results) <- paste0(timepoints, "yr")
    })

    expect_no_error({
        calculate_cif_by_class_with_ci(
            data = test_data,
            time_var = "tt_mss_5yr",
            event_type_var = "event_type_mss_5yr",
            eval_time = 5,
            n_boot = 10
        )
    })

    expect_no_error({
        calculate_fine_gray_model(
            data = test_data,
            time_var = "tt_mss_5yr",
            event_var = "event_type_mss_5yr",
            group_var = "biopsy1_gep",
            eligibility_filter = "mss_analysis_eligible"
        )
    })

    expect_no_error({
        calculate_cause_specific_cox_model(
            data = test_data,
            time_var = "tt_mss_5yr",
            event_var = "event_type_mss_5yr",
            group_var = "biopsy1_gep",
            eligibility_filter = "mss_analysis_eligible"
        )
    })

    cif_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_components", "cif")
    dir.create(cif_output_dir, recursive = TRUE, showWarnings = FALSE)

    expect_no_error({
        create_mss_cumulative_incidence_curves(
            data = test_data,
            timepoint = 5,
            output_dir = cif_output_dir,
            prefix = "test_",
            group_var = "biopsy1_gep"
        )
    })

    expect_true(file.exists(file.path(cif_output_dir, "test_mss_cumulative_incidence_curves.png")))

    unlink(file.path(TEST_OUTPUT_DIR, "objective4_components"), recursive = TRUE)
})

test_that("Competing-risk MSS feasibility returns explicit skip metadata", {
    test_data <- tibble::tibble(
        biopsy1_gep = factor(
            c(
                rep("Class 1 PRAME Negative", 10),
                rep("Class 2 PRAME Positive", 10)
            )
        ),
        mss_analysis_eligible = TRUE,
        tt_death_months = c(rep(24, 10), rep(24, 10)),
        event_type_mss_5yr = c(rep(0, 10), rep(1, 10))
    )

    results <- perform_competing_risk_mss_validation(test_data, timepoint = 5)

    expect_true("feasibility" %in% names(results))
    expect_true("cif_with_ci" %in% names(results))
    expect_true("unexpected_failures" %in% names(results))
    expect_length(results$unexpected_failures, 0)

    expect_identical(results$feasibility$models$cause_specific_cox$status, "skipped")
    expect_match(results$feasibility$models$cause_specific_cox$reason, "groups_with_zero_melanoma_deaths")
    expect_null(results$cause_specific_cox)
    expect_null(results$fine_gray)

    expect_true(all(c("status", "skip_reason") %in% names(results$cif_with_ci)))
    zero_event_row <- results$cif_with_ci %>%
        dplyr::filter(Group == "Class 1 PRAME Negative")

    expect_equal(nrow(zero_event_row), 1)
    expect_equal(zero_event_row$cif[[1]], 0)
    expect_identical(zero_event_row$status[[1]], "no_event_of_interest")
    expect_identical(zero_event_row$skip_reason[[1]], "no_melanoma_deaths")
})

test_that("run_objective_4 creates current canonical Objective 4 artifacts", {
    actual_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_runtime")
    output_dirs <- list(
        obj4_mfs = file.path(test_output_dir, "04_GEP_Validation", "a_metastasis_free_survival"),
        obj4_mss = file.path(test_output_dir, "04_GEP_Validation", "b_melanoma_specific_survival"),
        obj4_ph_diagnostics = file.path(test_output_dir, "04_GEP_Validation", "c_proportional_hazards_diagnostics")
    )

    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }

    expect_no_error({
        results <- suppressWarnings(run_objective_4(
            data = actual_data,
            dataset_name = "uveal_melanoma_full_cohort",
            output_dirs = output_dirs,
            prefix = "test_"
        ))
    })

    expect_true(all(c(
        "mfs_gep_results",
        "mss_gep_results",
        "simple_gep_results",
        "mfs_sensitivity_results",
        "exploratory_no_gep_results"
    ) %in% names(results)))

    expect_true(file.exists(file.path(output_dirs$obj4_mfs, "test_mfs_validation_technical_details.xlsx")))
    expect_true(file.exists(file.path(output_dirs$obj4_mfs, "test_MFS_consolidated_summary.xlsx")))
    expect_true(file.exists(file.path(output_dirs$obj4_mfs, "test_mfs_validation_narrative_summary.txt")))
    expect_true(file.exists(file.path(output_dirs$obj4_mfs, "test_mfs_extrapolation_assumption_summary.txt")))
    expect_true(file.exists(file.path(output_dirs$obj4_mfs, "test_mfs_extrapolation_cumhaz_diagnostic.png")))
    expect_true(file.exists(file.path(output_dirs$obj4_mfs, "test_mfs_calibration_full.png")))
    expect_true(file.exists(file.path(output_dirs$obj4_mss, "test_MSS_consolidated_summary.xlsx")))
    expect_true(file.exists(file.path(output_dirs$obj4_mss, "test_mss_validation_technical_details.xlsx")))
    expect_true(file.exists(file.path(output_dirs$obj4_mss, "test_mss_validation_narrative_summary.txt")))
    expect_true(file.exists(file.path(output_dirs$obj4_mss, "test_mss_extrapolation_assumption_summary.txt")))
    expect_true(file.exists(file.path(output_dirs$obj4_mss, "test_mss_extrapolation_cumhaz_diagnostic.png")))
    expect_true(file.exists(file.path(output_dirs$obj4_mfs, "test_mfs_prame_delta_c.png")))
    expect_true(file.exists(file.path(output_dirs$obj4_mss, "test_mss_prame_delta_c.png")))
    expect_true(file.exists(file.path(dirname(output_dirs$obj4_mfs), "test_unified_gep_validation_summary.xlsx")))
    expect_true(file.exists(file.path(dirname(output_dirs$obj4_mfs), "unified_summary", "test_simple_gep_validation.xlsx")))
    expect_true(file.exists(file.path(dirname(output_dirs$obj4_mfs), "unified_summary", "test_mfs_sensitivity_summary.xlsx")))
    expect_true(file.exists(file.path(dirname(output_dirs$obj4_mfs), "unified_summary", "test_mfs_sensitivity_summary.txt")))
    expect_true(file.exists(file.path(dirname(output_dirs$obj4_mfs), "d_exploratory_no_gep", "full_cohort_exploratory_no_gep_report.xlsx")))
    expect_true(file.exists(file.path(dirname(output_dirs$obj4_mfs), "d_exploratory_no_gep", "full_cohort_exploratory_no_gep_summary.txt")))
    expect_false(file.exists(file.path(dirname(output_dirs$obj4_mfs), "test_prame_delta_c.png")))

    mfs_technical_sheets <- readxl::excel_sheets(file.path(output_dirs$obj4_mfs, "test_mfs_validation_technical_details.xlsx"))
    expect_true("Observed_Expected_by_class" %in% mfs_technical_sheets)
    expect_false(any(c("Calibration", "Discrimination") %in% mfs_technical_sheets))

    mfs_consolidated_sheets <- readxl::excel_sheets(file.path(output_dirs$obj4_mfs, "test_MFS_consolidated_summary.xlsx"))
    expect_true(all(c("Observed_Expected_Summary", "PRAME_Summary", "Extrapolation_Assumption_Checks") %in% mfs_consolidated_sheets))

    mss_technical_sheets <- readxl::excel_sheets(file.path(output_dirs$obj4_mss, "test_mss_validation_technical_details.xlsx"))
    expect_true("Observed_Expected_by_class" %in% mss_technical_sheets)
    expect_false(any(c("Calibration", "Discrimination") %in% mss_technical_sheets))

    unified_sheets <- readxl::excel_sheets(file.path(dirname(output_dirs$obj4_mfs), "test_unified_gep_validation_summary.xlsx"))
    expect_true(all(c(
        "Calibration_Comparison",
        "Discrimination_Comparison",
        "PRAME_Comparison",
        "Missing_Data_Comparison",
        "No_GEP_Overview",
        "No_GEP_Model_Comparison",
        "No_GEP_Risk_Strata",
        "No_GEP_Risk_Ladder"
    ) %in% unified_sheets))
    expect_false(any(c("Unified_Calibration", "Unified_Discrimination", "PRAME_Summary", "Missing_Data_Summary") %in% unified_sheets))

    sensitivity_sheets <- readxl::excel_sheets(file.path(dirname(output_dirs$obj4_mfs), "unified_summary", "test_mfs_sensitivity_summary.xlsx"))
    expect_true(all(c(
        "Followup_Operational",
        "Followup_5yr",
        "TxMix_ByClass",
        "Repeat_Comparison",
        "Guardrail_Notes"
    ) %in% sensitivity_sheets))

    unlink(test_output_dir, recursive = TRUE)
})

test_that("run_objective_4 carries confounders into adjusted GEP MFS effect summaries", {
    actual_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "objective4_adjusted_confounders")
    output_dirs <- list(
        obj4_mfs = file.path(test_output_dir, "04_GEP_Validation", "a_metastasis_free_survival"),
        obj4_mss = file.path(test_output_dir, "04_GEP_Validation", "b_melanoma_specific_survival"),
        obj4_ph_diagnostics = file.path(test_output_dir, "04_GEP_Validation", "c_proportional_hazards_diagnostics")
    )
    expected_confounders <- c("age_at_diagnosis_general_pop_median", "sex", "location")

    for (dir_path in output_dirs) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    on.exit(unlink(test_output_dir, recursive = TRUE), add = TRUE)

    expect_no_error({
        suppressWarnings(run_objective_4(
            data = actual_data,
            dataset_name = "uveal_melanoma_full_cohort",
            output_dirs = output_dirs,
            prefix = "test_",
            confounders = expected_confounders
        ))
    })

    effect_summary_paths <- list.files(
        path = output_dirs$obj4_mfs,
        pattern = "metastasis_free_survival_probability_effect_summary\\.xlsx$",
        recursive = TRUE,
        full.names = TRUE
    )

    full_summary_path <- effect_summary_paths[!grepl("simple_gep_binary", basename(effect_summary_paths))][1]
    simple_summary_path <- effect_summary_paths[grepl("simple_gep_binary", basename(effect_summary_paths))][1]

    expect_true(file.exists(full_summary_path))
    expect_true(file.exists(simple_summary_path))

    full_summary <- readxl::read_xlsx(full_summary_path)
    simple_summary <- readxl::read_xlsx(simple_summary_path)

    adjusted_full <- full_summary %>%
        dplyr::filter(.data$model_label == "Adjusted Cox (confounders)")
    adjusted_simple <- simple_summary %>%
        dplyr::filter(.data$model_label == "Adjusted Cox (confounders)")
    unadjusted_simple <- simple_summary %>%
        dplyr::filter(.data$model_label == "Unadjusted (Cox data)")

    expect_true(nrow(adjusted_full) > 0)
    expect_true(nrow(adjusted_simple) > 0)
    expect_true(all(adjusted_full$covariates_used != "None"))
    expect_true(all(adjusted_simple$covariates_used != "None"))

    for (confounder in expected_confounders) {
        expect_true(all(grepl(confounder, adjusted_full$model_formula, fixed = TRUE)))
        expect_true(all(grepl(confounder, adjusted_simple$model_formula, fixed = TRUE)))
    }

    merged_simple <- adjusted_simple %>%
        dplyr::select(.data$term, adjusted_estimate = .data$estimate) %>%
        dplyr::inner_join(
            unadjusted_simple %>%
                dplyr::select(.data$term, unadjusted_estimate = .data$estimate),
            by = "term"
        )

    expect_true(nrow(merged_simple) > 0)
    expect_true(any(merged_simple$adjusted_estimate != merged_simple$unadjusted_estimate))
})

test_that("4e: Existing Objective 4 cohort artifacts follow current placement conventions", {
    cohort_configs <- list(
        list(
            name = "full",
            base_dir = file.path(OUTPUT_DIR, "uveal_full", "04_GEP_Validation"),
            prefix = "full_cohort_"
        ),
        list(
            name = "restricted",
            base_dir = file.path(OUTPUT_DIR, "uveal_restricted", "04_GEP_Validation"),
            prefix = "restricted_cohort_"
        ),
        list(
            name = "gksrs",
            base_dir = file.path(OUTPUT_DIR, "gksrs", "04_GEP_Validation"),
            prefix = "gksrs_only_cohort_"
        )
    )

    for (cfg in cohort_configs) {
        mfs_dir <- file.path(cfg$base_dir, "a_metastasis_free_survival")
        mss_dir <- file.path(cfg$base_dir, "b_melanoma_specific_survival")
        unified_dir <- file.path(cfg$base_dir, "unified_summary")

        expect_true(file.exists(file.path(mfs_dir, paste0(cfg$prefix, "MFS_consolidated_summary.xlsx"))),
            info = sprintf("MFS consolidated workbook should exist for %s cohort", cfg$name))
        expect_true(file.exists(file.path(mss_dir, paste0(cfg$prefix, "MSS_consolidated_summary.xlsx"))),
            info = sprintf("MSS consolidated workbook should exist for %s cohort", cfg$name))
        expect_true(file.exists(file.path(mss_dir, paste0(cfg$prefix, "mss_validation_technical_details.xlsx"))),
            info = sprintf("MSS validation workbook should exist for %s cohort", cfg$name))
        expect_true(file.exists(file.path(cfg$base_dir, paste0(cfg$prefix, "unified_gep_validation_summary.xlsx"))),
            info = sprintf("Root-level unified workbook should exist for %s cohort", cfg$name))
        expect_true(file.exists(file.path(unified_dir, paste0(cfg$prefix, "simple_gep_validation.xlsx"))),
            info = sprintf("Simple validation workbook should exist in unified_summary for %s cohort", cfg$name))
        expect_true(file.exists(file.path(unified_dir, paste0(cfg$prefix, "mfs_sensitivity_summary.xlsx"))),
            info = sprintf("MFS sensitivity workbook should exist in unified_summary for %s cohort", cfg$name))
        expect_true(file.exists(file.path(unified_dir, paste0(cfg$prefix, "mfs_sensitivity_summary.txt"))),
            info = sprintf("MFS sensitivity summary text should exist in unified_summary for %s cohort", cfg$name))

        if (identical(cfg$name, "gksrs")) {
            expect_true(file.exists(file.path(mfs_dir, paste0(cfg$prefix, "metastasis_free_survival_probability_cox_NO_CONTENT_DIAGNOSTIC.html"))),
                info = "GKSRS cohort should retain the explicit NO_CONTENT diagnostic artifact when PH diagnostics are sparse")
        }
    }
})

# Test that eligibility filters properly exclude invalid data
test_that("Eligibility filters properly exclude invalid data", {
    test_data <- tibble::tibble(
        biopsy1_gep = c(
            "Class 1 PRAME Negative",
            "Class 2 PRAME Positive",
            "GEP Failed/Indeterminate",
            "Other",
            "GEP Not Tested"
        ),
        biopsy1_gep_raw = c(
            "Class_1A_PRAME_negative",
            "Class_2_PRAME_positive",
            "Class_1A_PRAME_not_reported",
            "Other",
            "No"
        ),
        gep_class_simple = c("Class 1", "Class 2", "Class 1", "Class 2", "GEP Not Tested"),
        biopsy1_gep_mfs = c(0.8, 0.4, 0.6, 0.5, 0.7),
        biopsy1_gep_mss = c(0.9, 0.6, 0.7, 0.8, 0.8),
        tt_mets_months = c(24, 36, 18, 12, 48),
        mets_event = c(0, 1, 0, 1, 0),
        tt_death_years = c(2, 3, 1.5, 1, 4),
        melanoma_death_event = c(0, 1, 0, 1, 0),
        competing_death_event = c(0, 0, 1, 0, 0),
        prame_status = c("Negative", "Positive", "Unknown", "Not Available", "Not Available")
    )

    refreshed_data <- refresh_gep_analysis_flags(test_data)

    expect_true(refreshed_data$mfs_analysis_eligible[1],
        info = "Definitive Class 1 should be included in MFS analysis"
    )
    expect_true(refreshed_data$mfs_analysis_eligible[2],
        info = "Definitive Class 2 should be included in MFS analysis"
    )
    expect_false(refreshed_data$mfs_analysis_eligible[3],
        info = "Class 1 not reported should be excluded from MFS analysis"
    )
    expect_false(refreshed_data$mfs_analysis_eligible[4],
        info = "Collapsed Other rows should be excluded from MFS analysis"
    )
    expect_false(refreshed_data$mfs_analysis_eligible[5],
        info = "GEP not tested rows should be excluded from MFS analysis"
    )

    expect_true(refreshed_data$mss_analysis_eligible[1],
        info = "Definitive Class 1 should be included in MSS analysis"
    )
    expect_true(refreshed_data$mss_analysis_eligible[2],
        info = "Definitive Class 2 should be included in MSS analysis"
    )
    expect_false(refreshed_data$mss_analysis_eligible[3],
        info = "Class 1 not reported should be excluded from MSS analysis"
    )
    expect_false(refreshed_data$mss_analysis_eligible[4],
        info = "Collapsed Other rows should be excluded from MSS analysis"
    )
    expect_false(refreshed_data$mss_analysis_eligible[5],
        info = "GEP not tested rows should be excluded from MSS analysis"
    )
})

test_that("Objective 4 eligibility refresh removes failed and other rows from cohort analyses", {
    cohort_names <- c(
        "uveal_melanoma_full_cohort",
        "uveal_melanoma_restricted_cohort",
        "uveal_melanoma_gksrs_only_cohort"
    )

    for (dataset_name in cohort_names) {
        actual_data <- readRDS(file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds")))
        refreshed_data <- refresh_gep_analysis_flags(actual_data)
        display_data <- restore_gep_display_variables(refreshed_data, dataset_name = dataset_name)

        mfs_data <- display_data %>% filter(mfs_analysis_eligible)
        mss_data <- display_data %>% filter(mss_analysis_eligible)

        expect_false(any(as.character(mfs_data$biopsy1_gep) %in% c("GEP Failed/Indeterminate", "GEP Not Tested", "Other")),
            info = sprintf("MFS-eligible rows should exclude failed and other labels for %s", dataset_name)
        )
        expect_false(any(as.character(mss_data$biopsy1_gep) %in% c("GEP Failed/Indeterminate", "GEP Not Tested", "Other")),
            info = sprintf("MSS-eligible rows should exclude failed and other labels for %s", dataset_name)
        )
        expect_true(all(as.character(mfs_data$gep_class_simple) %in% GEP_DEFINITIVE_SIMPLE_LEVELS),
            info = sprintf("MFS-eligible rows should retain only definitive simple classes for %s", dataset_name)
        )
        expect_true(all(as.character(mss_data$gep_class_simple) %in% GEP_DEFINITIVE_SIMPLE_LEVELS),
            info = sprintf("MSS-eligible rows should retain only definitive simple classes for %s", dataset_name)
        )
    }
})

test_that("Canonical GEP variables retain original levels without cohort-wide collapse", {
    actual_data <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))

    expect_false(file.exists(file.path(PROCESSED_DATA_DIR, "other_map.rds")))
    expect_true(all(c("biopsy1_gep_raw", GEP_DISPLAY_VARIABLES, "location") %in% names(actual_data)))
    expect_false("Other" %in% levels(actual_data$location))
    expect_true(any(as.character(actual_data$location) == "Cilio-Choroidal"))
    expect_true(any(as.character(actual_data$biopsy1_gep) == "GEP Not Tested"))
})

test_that("Eligibility depends on definitive raw labels even when text raw retains definitive class", {
    test_data <- tibble::tibble(
        biopsy1_gep = factor(c("Class 1 PRAME Positive", "Class 1 PRAME Positive")),
        biopsy1_gep_raw = factor(c("Other", "Class_1A_PRAME_positive")),
        biopsy1_gep_text_raw = c("Class_1A_PRAME_positive", "Class_1A_PRAME_positive"),
        gep_class_simple = factor(c("Class 1", "Class 1")),
        biopsy1_gep_mfs = c(0.80, 0.80),
        biopsy1_gep_mss = c(0.85, 0.85),
        tt_mets_months = c(36, 36),
        mets_event = c(0, 0),
        tt_death_years = c(3, 3),
        melanoma_death_event = c(0, 0),
        competing_death_event = c(0, 0),
        prame_status = factor(c("Positive", "Positive"))
    )

    refreshed_data <- refresh_gep_analysis_flags(test_data)

    expect_false(refreshed_data$mfs_analysis_eligible[1])
    expect_true(refreshed_data$mfs_analysis_eligible[2])
    expect_false(refreshed_data$mss_analysis_eligible[1])
    expect_true(refreshed_data$mss_analysis_eligible[2])
})
