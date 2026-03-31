# Test GEP Output Consolidation
# Tests the consolidation system that eliminates redundant outputs while maintaining essential visualizations

# CRITICAL: Set test environment variables BEFORE sourcing load_all.R
# This prevents directory creation in the wrong location

# Load required packages first
library(dplyr)

# Override project constants to prevent test interference

# Load the project environment with ALL of the variables and functions

test_that("GEP output consolidation creates comprehensive summary tables", {
    
    # Create mock validation results for testing
    mock_validation_results <- list(
        "5yr" = list(
            calibration = list(
                n = 100,
                nam_dagostino_p = 0.05,
                ici = 0.1,
                slope = 0.95
            ),
            discrimination = list(
                n = 100,
                events = 25,
                harrell_c = 0.75,
                uno_c = 0.73,
                auc_timepoint = 0.78
            ),
            decision_curve = list(
                n = 100,
                net_benefit_threshold = 0.3
            )
        ),
        "7yr" = list(
            calibration = list(
                n = 95,
                nam_dagostino_p = 0.08,
                ici = 0.12,
                slope = 0.92
            ),
            discrimination = list(
                n = 95,
                events = 30,
                harrell_c = 0.78,
                uno_c = 0.76,
                auc_timepoint = 0.81
            ),
            decision_curve = list(
                n = 95,
                net_benefit_threshold = 0.35
            )
        ),
        "10yr" = list(
            calibration = list(
                n = 90,
                nam_dagostino_p = 0.12,
                ici = 0.15,
                slope = 0.89
            ),
            discrimination = list(
                n = 90,
                events = 35,
                harrell_c = 0.80,
                uno_c = 0.78,
                auc_timepoint = 0.83
            ),
            decision_curve = list(
                n = 90,
                net_benefit_threshold = 0.40
            )
        )
    )
    
    # Test consolidated table creation
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "gep_consolidation_test")
    if (!dir.exists(test_output_dir)) {
        dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Test MFS consolidation
    mfs_consolidated <- create_consolidated_gep_tables(
        validation_results = mock_validation_results,
        outcome_type = "MFS",
        output_dir = test_output_dir,
        prefix = "test_mfs_"
    )
    
    # Verify all table types are created
    expect_true("calibration" %in% names(mfs_consolidated))
    expect_true("discrimination" %in% names(mfs_consolidated))
    # Performance table intentionally removed to eliminate redundancy with discrimination metrics
    expect_true("decision_curves" %in% names(mfs_consolidated))
    expect_true("text_summary" %in% names(mfs_consolidated))
    
    # Verify calibration table has expected rows and core columns
    cal_table <- mfs_consolidated$calibration
    expect_equal(nrow(cal_table), 3)  # 3 timepoints
    expect_true(all(c("Timepoint", "N", "Nam_D_Agostino_p", "ICI", "Slope") %in% names(cal_table)))
    expect_equal(cal_table$Timepoint, c("5yr", "7yr", "10yr"))
    expect_equal(cal_table$N, c(100, 95, 90))
    expect_true(all(c("Brier_Score", "Brier_Method", "Brier_Fallback_Used") %in% names(cal_table)))
    
    # Verify discrimination table has expected rows and enriched columns
    disc_table <- mfs_consolidated$discrimination
    expect_equal(nrow(disc_table), 3)  # 3 timepoints
    expect_true(all(c(
        "Timepoint", "N", "Events", "Harrell_C",
        "Integrated_AUC", "Integrated_AUC_Status", "Integrated_AUC_Method", "Integrated_AUC_Unavailable_Reason",
        "Cumulative_Discrimination",
        "Time_averaged_Discrimination", "IPA",
        "IPA_Method", "IPA_Fallback_Used"
    ) %in% names(disc_table)))
    expect_equal(disc_table$Timepoint, c("5yr", "7yr", "10yr"))
    expect_equal(disc_table$Harrell_C, c(0.75, 0.78, 0.80))
    expect_true(all(is.na(disc_table$Integrated_AUC)))  # mock inputs omit integrated values
    
    # Performance table intentionally removed to eliminate redundancy with discrimination metrics
    
    # Verify decision curve table has correct structure
    dca_table <- mfs_consolidated$decision_curves
    expect_equal(nrow(dca_table), 3)  # 3 timepoints
    expect_true(all(c(
        "Timepoint", "N", "Events", "Event_Rate",
        "Optimal_Threshold", "Optimal_Net_Benefit",
        "Threshold_Range_Min", "Threshold_Range_Max", "Threshold_Scale",
        "Area_Between_Curves"
    ) %in% names(dca_table)))
    expect_equal(dca_table$Timepoint, c("5yr", "7yr", "10yr"))
    expect_equal(dca_table$Optimal_Threshold, c(0.3, 0.35, 0.40))
    
    # Verify text summary contains all information
    text_summary <- mfs_consolidated$text_summary
    expect_match(text_summary, "^# MFS Validation - Consolidated Summary", perl = TRUE)
    expect_true(grepl("## Calibration Summary", text_summary, fixed = TRUE))
    expect_true(grepl("## Discrimination Summary", text_summary, fixed = TRUE))
    # Performance summary intentionally removed to eliminate redundancy
    expect_true(grepl("## Decision Curve Summary", text_summary, fixed = TRUE))
    expect_true(grepl("## Key Findings", text_summary, fixed = TRUE))
    
    # Test MSS consolidation
    mss_consolidated <- create_consolidated_gep_tables(
        validation_results = mock_validation_results,
        outcome_type = "MSS",
        output_dir = test_output_dir,
        prefix = "test_mss_"
    )
    
    # Verify MSS consolidation works the same way
    expect_true("calibration" %in% names(mss_consolidated))
    expect_true("discrimination" %in% names(mss_consolidated))
    # Performance table intentionally removed to eliminate redundancy with discrimination metrics
    expect_true("decision_curves" %in% names(mss_consolidated))
    expect_true("text_summary" %in% names(mss_consolidated))
    
    # Clean up test output
    unlink(test_output_dir, recursive = TRUE)
})

test_that("Unified GEP validation summary eliminates redundancy across outcomes", {
    # Load required functions
    
    # Create mock MFS and MSS results
    mock_mfs_results <- list(
        validation_results = list(
            "5yr" = list(
                calibration = list(n = 100, nam_dagostino_p = 0.05, ici = 0.1, slope = 0.95),
                discrimination = list(n = 100, events = 25, harrell_c = 0.75, uno_c = 0.73, auc_timepoint = 0.78)
            ),
            "7yr" = list(
                calibration = list(n = 95, nam_dagostino_p = 0.08, ici = 0.12, slope = 0.92),
                discrimination = list(n = 95, events = 30, harrell_c = 0.78, uno_c = 0.76, auc_timepoint = 0.81)
            )
        ),
        source_data = create_test_dataset(),
        dataset_name = "uveal_melanoma_full_cohort"
    )
    
    mock_mss_results <- list(
        standard_results = list(
            "5yr" = list(
                calibration = list(n = 98, nam_dagostino_p = 0.06, ici = 0.11, slope = 0.94),
                discrimination = list(n = 98, events = 26, harrell_c = 0.76, uno_c = 0.74, auc_timepoint = 0.79)
            ),
            "7yr" = list(
                calibration = list(n = 93, nam_dagostino_p = 0.09, ici = 0.13, slope = 0.91),
                discrimination = list(n = 93, events = 31, harrell_c = 0.79, uno_c = 0.77, auc_timepoint = 0.82)
            )
        ),
        source_data = create_test_dataset(),
        dataset_name = "uveal_melanoma_full_cohort"
    )
    
    # Test unified summary creation
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "gep_unified_test")
    if (!dir.exists(test_output_dir)) {
        dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    unified_summary <- create_unified_gep_validation_summary(
        mfs_results = mock_mfs_results,
        mss_results = mock_mss_results,
        output_dir = test_output_dir,
        prefix = "test_unified_"
    )
    
    # Verify unified summary contains all components
    expect_true("calibration" %in% names(unified_summary))
    expect_true("discrimination" %in% names(unified_summary))
    # Performance table intentionally removed to eliminate redundancy with discrimination metrics
    expect_true("text_summary" %in% names(unified_summary))
    
    # Verify calibration table combines both outcomes
    cal_table <- unified_summary$calibration
    expect_equal(nrow(cal_table), 4)  # 2 timepoints × 2 outcomes
    expect_true(all(c("Outcome", "Timepoint", "N", "Nam_D_Agostino_p", "ICI", "Slope") %in% names(cal_table)))
    expect_equal(cal_table$Outcome, c("MFS", "MFS", "MSS", "MSS"))
    expect_equal(cal_table$Timepoint, c("5yr", "7yr", "5yr", "7yr"))
    
    # Verify discrimination table combines both outcomes
    disc_table <- unified_summary$discrimination
    expect_equal(nrow(disc_table), 4)  # 2 timepoints × 2 outcomes
    expect_true(all(c(
        "Outcome", "Timepoint", "N", "Events",
        "Harrell_C", "Integrated_AUC", "Integrated_AUC_Status", "Integrated_AUC_Method", "Integrated_AUC_Unavailable_Reason",
        "Cumulative_Discrimination", "Time_averaged_Discrimination"
    ) %in% names(disc_table)))
    expect_equal(disc_table$Outcome, c("MFS", "MFS", "MSS", "MSS"))
    expect_equal(disc_table$Timepoint, c("5yr", "7yr", "5yr", "7yr"))
    
    # Performance table intentionally removed to eliminate redundancy with discrimination metrics
    
    # Verify text summary contains unified information
    text_summary <- unified_summary$text_summary
    expect_match(text_summary, "^# Unified GEP Validation Summary", perl = TRUE)
    expect_true(grepl("## Follow-Up Limitation (5-year)", text_summary, fixed = TRUE))
    expect_true(grepl("### MFS", text_summary, fixed = TRUE))
    expect_true(grepl("### MSS", text_summary, fixed = TRUE))
    expect_true(grepl("`followup_ge_5yr` means", text_summary, fixed = TRUE))
    expect_true(grepl("## Calibration Comparison (MFS vs MSS)", text_summary, fixed = TRUE))
    expect_true(grepl("## Discrimination Comparison (MFS vs MSS)", text_summary, fixed = TRUE))
    # Performance comparison intentionally removed to eliminate redundancy
    expect_true(grepl("## Key Findings", text_summary, fixed = TRUE))
    
    # Clean up test output
    unlink(test_output_dir, recursive = TRUE)
})

test_that("Consolidation maintains all statistical information while eliminating redundancy", {
    # Load required functions
    
    # Create comprehensive mock data
    mock_data <- list(
        "5yr" = list(
            calibration = list(n = 100, nam_dagostino_p = 0.05, ici = 0.1, slope = 0.95),
            discrimination = list(n = 100, events = 25, harrell_c = 0.75, uno_c = 0.73, auc_timepoint = 0.78),
            decision_curve = list(n = 100, net_benefit_threshold = 0.3)
        ),
        "7yr" = list(
            calibration = list(n = 95, nam_dagostino_p = 0.08, ici = 0.12, slope = 0.92),
            discrimination = list(n = 95, events = 30, harrell_c = 0.78, uno_c = 0.76, auc_timepoint = 0.81),
            decision_curve = list(n = 95, net_benefit_threshold = 0.35)
        ),
        "10yr" = list(
            calibration = list(n = 90, nam_dagostino_p = 0.12, ici = 0.15, slope = 0.89),
            discrimination = list(n = 90, events = 35, harrell_c = 0.80, uno_c = 0.78, auc_timepoint = 0.83),
            decision_curve = list(n = 90, net_benefit_threshold = 0.40)
        )
    )
    
    # Test that consolidation preserves all values
    test_output_dir <- file.path(TEST_OUTPUT_DIR, "gep_preservation_test")
    if (!dir.exists(test_output_dir)) {
        dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    consolidated <- create_consolidated_gep_tables(
        validation_results = mock_data,
        outcome_type = "Test",
        output_dir = test_output_dir,
        prefix = "test_preserve_"
    )
    
    # Verify calibration values are preserved
    cal_table <- consolidated$calibration
    expect_equal(cal_table$N[cal_table$Timepoint == "5yr"], 100)
    expect_equal(cal_table$Nam_D_Agostino_p[cal_table$Timepoint == "5yr"], 0.05)
    expect_equal(cal_table$ICI[cal_table$Timepoint == "5yr"], 0.1)
    expect_equal(cal_table$Slope[cal_table$Timepoint == "5yr"], 0.95)
    
    expect_equal(cal_table$N[cal_table$Timepoint == "7yr"], 95)
    expect_equal(cal_table$Nam_D_Agostino_p[cal_table$Timepoint == "7yr"], 0.08)
    expect_equal(cal_table$ICI[cal_table$Timepoint == "7yr"], 0.12)
    expect_equal(cal_table$Slope[cal_table$Timepoint == "7yr"], 0.92)
    
    expect_equal(cal_table$N[cal_table$Timepoint == "10yr"], 90)
    expect_equal(cal_table$Nam_D_Agostino_p[cal_table$Timepoint == "10yr"], 0.12)
    expect_equal(cal_table$ICI[cal_table$Timepoint == "10yr"], 0.15)
    expect_equal(cal_table$Slope[cal_table$Timepoint == "10yr"], 0.89)
    
    # Verify discrimination values are preserved
    disc_table <- consolidated$discrimination
    expect_equal(disc_table$N[disc_table$Timepoint == "5yr"], 100)
    expect_equal(disc_table$Events[disc_table$Timepoint == "5yr"], 25)
    expect_equal(disc_table$Harrell_C[disc_table$Timepoint == "5yr"], 0.75)
    expect_true("Cumulative_Discrimination" %in% names(disc_table))
    
    expect_equal(disc_table$N[disc_table$Timepoint == "7yr"], 95)
    expect_equal(disc_table$Events[disc_table$Timepoint == "7yr"], 30)
    expect_equal(disc_table$Harrell_C[disc_table$Timepoint == "7yr"], 0.78)
    
    expect_equal(disc_table$N[disc_table$Timepoint == "10yr"], 90)
    expect_equal(disc_table$Events[disc_table$Timepoint == "10yr"], 35)
    expect_equal(disc_table$Harrell_C[disc_table$Timepoint == "10yr"], 0.80)
    
    # Verify decision curve values are preserved
    dca_table <- consolidated$decision_curves
    expect_equal(dca_table$N[dca_table$Timepoint == "5yr"], 100)
    expect_equal(dca_table$Optimal_Threshold[dca_table$Timepoint == "5yr"], 0.3)
    
    expect_equal(dca_table$N[dca_table$Timepoint == "7yr"], 95)
    expect_equal(dca_table$Optimal_Threshold[dca_table$Timepoint == "7yr"], 0.35)
    
    expect_equal(dca_table$N[dca_table$Timepoint == "10yr"], 90)
    expect_equal(dca_table$Optimal_Threshold[dca_table$Timepoint == "10yr"], 0.40)
    
    # Clean up test output
    unlink(test_output_dir, recursive = TRUE)
})
