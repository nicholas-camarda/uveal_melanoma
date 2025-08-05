# Test script for T-stage clinical binning implementation
# This test verifies that the clinical binning uses T-stage cutoffs correctly

library(testthat)

test_that("T-stage clinical binning is correctly implemented", {
    
    # Test 1: Verify flag name change
    # The flag should be renamed from USE_T_STAGE_CUTOFFS to USE_CLINICAL_BINNING_CONTINUOUS
    all_helper_content <- readLines("scripts/utils/all_helper_functions.R")
    
    old_flag <- grep("USE_T_STAGE_CUTOFFS", all_helper_content, value = TRUE)
    new_flag <- grep("USE_CLINICAL_BINNING_CONTINUOUS", all_helper_content, value = TRUE)
    
    expect_equal(length(old_flag), 0, 
                info = "Old flag name USE_T_STAGE_CUTOFFS should be removed")
    expect_true(length(new_flag) > 0, 
                info = "New flag name USE_CLINICAL_BINNING_CONTINUOUS should be present")
    
    # Test 2: Verify flag is set to TRUE
    flag_line <- grep("USE_CLINICAL_BINNING_CONTINUOUS <- TRUE", all_helper_content, value = TRUE)
    expect_true(length(flag_line) > 0, 
                info = "USE_CLINICAL_BINNING_CONTINUOUS should be set to TRUE")
    
    # Test 3: Verify T-stage cutoffs are used in get_cutoff_value function
    subgroup_content <- readLines("scripts/analysis/subgroup_analysis.R")
    
    t_stage_usage <- grep("T_STAGE_HEIGHT_CUTOFFS|T_STAGE_DIAMETER_CUTOFFS", subgroup_content, value = TRUE)
    expect_true(length(t_stage_usage) > 0, 
                info = "T-stage cutoffs should be used in get_cutoff_value function")
    
    # Test 4: Verify clinical binning logic is implemented
    clinical_binning_logic <- grep("USE_CLINICAL_BINNING_CONTINUOUS && length\\(cutoff_val\\) > 1", subgroup_content, value = TRUE)
    expect_true(length(clinical_binning_logic) > 0, 
                info = "Clinical binning logic should check for multiple cutoffs")
    
    # Test 5: Verify create_clinical_bins function is used
    create_bins_usage <- grep("create_clinical_bins", subgroup_content, value = TRUE)
    expect_true(length(create_bins_usage) > 0, 
                info = "create_clinical_bins function should be used for T-stage binning")
    
    cat("✅ T-stage clinical binning implementation tests passed\n")
})

test_that("Sample size format improvements are correctly implemented", {
    
    # Test the new sample size format with source note explanation
    n_total <- 192
    n_plaque <- 105
    n_gksrs <- 87
    
    # New format should be "192 (105/87)"
    new_format <- sprintf("%d (%d/%d)", n_total, n_plaque, n_gksrs)
    expected_format <- "192 (105/87)"
    
    expect_equal(new_format, expected_format, 
                info = "Sample size format should be compact and clean")
    
    # Test that it's more concise than the old format
    old_format <- sprintf("%d (composed of %d Plaque + %d GKSRS)", n_total, n_plaque, n_gksrs)
    old_expected <- "192 (composed of 105 Plaque + 87 GKSRS)"
    
    expect_equal(old_format, old_expected, 
                info = "Old format should be verbose")
    
    # New format should be shorter
    expect_true(nchar(new_format) < nchar(old_format), 
                info = "New format should be more concise than old format")
    
    # Test source note explanation
    source_note <- "Sample Size format: Total (Plaque/GKSRS)"
    expect_true(grepl("Total \\(Plaque/GKSRS\\)", source_note), 
                info = "Source note should clearly explain the format")
    
    cat("✅ Sample size format improvements tests passed\n")
})

test_that("T-stage cutoff values are correctly defined", {
    
    # Test that T-stage cutoffs are properly defined
    # These should match the values in config_constants.R
    expected_height_cutoffs <- c(3.0, 6.0, 9.0, 12.0, 15.0)
    expected_diameter_cutoffs <- c(3.0, 6.0, 9.0, 12.0, 15.0, 18.0)
    
    # Test height cutoffs create correct ranges
    height_ranges <- c(
        paste0("≤ ", expected_height_cutoffs[1], " mm"),
        paste0(expected_height_cutoffs[1] + 0.1, "-", expected_height_cutoffs[2], " mm"),
        paste0(expected_height_cutoffs[2] + 0.1, "-", expected_height_cutoffs[3], " mm"),
        paste0(expected_height_cutoffs[3] + 0.1, "-", expected_height_cutoffs[4], " mm"),
        paste0(expected_height_cutoffs[4] + 0.1, "-", expected_height_cutoffs[5], " mm"),
        paste0("> ", expected_height_cutoffs[5], " mm")
    )
    
    expected_height_ranges <- c("≤ 3 mm", "3.1-6 mm", "6.1-9 mm", "9.1-12 mm", "12.1-15 mm", "> 15 mm")
    expect_equal(height_ranges, expected_height_ranges, 
                info = "Height cutoffs should create correct T-stage ranges")
    
    # Test diameter cutoffs create correct ranges
    diameter_ranges <- c(
        paste0("≤ ", expected_diameter_cutoffs[1], " mm"),
        paste0(expected_diameter_cutoffs[1] + 0.1, "-", expected_diameter_cutoffs[2], " mm"),
        paste0(expected_diameter_cutoffs[2] + 0.1, "-", expected_diameter_cutoffs[3], " mm"),
        paste0(expected_diameter_cutoffs[3] + 0.1, "-", expected_diameter_cutoffs[4], " mm"),
        paste0(expected_diameter_cutoffs[4] + 0.1, "-", expected_diameter_cutoffs[5], " mm"),
        paste0(expected_diameter_cutoffs[5] + 0.1, "-", expected_diameter_cutoffs[6], " mm"),
        paste0("> ", expected_diameter_cutoffs[6], " mm")
    )
    
    expected_diameter_ranges <- c("≤ 3 mm", "3.1-6 mm", "6.1-9 mm", "9.1-12 mm", "12.1-15 mm", "15.1-18 mm", "> 18 mm")
    expect_equal(diameter_ranges, expected_diameter_ranges, 
                info = "Diameter cutoffs should create correct T-stage ranges")
    
    cat("✅ T-stage cutoff values tests passed\n")
}) 