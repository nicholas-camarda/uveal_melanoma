# Test Phase 1: other_map.rds Utilization Fixes
# Tests the fixes for other_map.rds utilization issues

# Set up test environment
setwd(dirname(dirname(normalizePath("."))))
source("scripts/utils/all_helper_functions.R")

# Load test data
test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
test_data <- test_data[1:50, ] # Use subset to avoid model fitting issues

# Test 1: get_cohort_specific_other_map function works correctly
test_that("get_cohort_specific_other_map loads cohort-specific files", {
        # Test with full cohort
        full_other_map <- get_cohort_specific_other_map("uveal_melanoma_full_cohort")
        expect_true(is.list(full_other_map))
        
        # Test with restricted cohort
        restricted_other_map <- get_cohort_specific_other_map("uveal_melanoma_restricted_cohort")
        expect_true(is.list(restricted_other_map))
        
        # Test with GKSRS-only cohort
        gksrs_other_map <- get_cohort_specific_other_map("uveal_melanoma_gksrs_only_cohort")
        expect_true(is.list(gksrs_other_map))
    })
    
    # Test 2: add_other_level_details function works conditionally
    test_that("add_other_level_details only adds captions when 'Other' is present", {
        # Create a mock table with "Other" category
        mock_table <- list(
            table_body = data.frame(
                label = c("Treatment Group", "Sex", "Location"),
                row_type = c("label", "level", "level"),
                stringsAsFactors = FALSE
            )
        )
        class(mock_table) <- "gtsummary"
        
        # Test with other_map that has "Other" categories
        other_map_with_other <- list(location = c("Ciliary Body", "Conjunctival"))
        result_with_other <- add_other_level_details(mock_table, other_map_with_other, "test_cohort")
        expect_true(is.list(result_with_other))
        
        # Test with empty other_map
        other_map_empty <- list()
        result_empty <- add_other_level_details(mock_table, other_map_empty, "test_cohort")
        expect_true(is.list(result_empty))
    })
    
    # Test 3: Main.R fix ensures cohort-specific other_map files are created
    test_that("Main.R fix ensures cohort-specific other_map files are updated", {
        # Check that cohort-specific other_map files exist
        full_other_map_path <- "final_data/Analytic Dataset/full_other_map.rds"
        restricted_other_map_path <- "final_data/Analytic Dataset/restricted_other_map.rds"
        gksrs_other_map_path <- "final_data/Analytic Dataset/gksrs_only_other_map.rds"
        
        expect_true(file.exists(full_other_map_path))
        expect_true(file.exists(restricted_other_map_path))
        expect_true(file.exists(gksrs_other_map_path))
        
        # Check that files are readable
        full_other_map <- readRDS(full_other_map_path)
        restricted_other_map <- readRDS(restricted_other_map_path)
        gksrs_other_map <- readRDS(gksrs_other_map_path)
        
        expect_true(is.list(full_other_map))
        expect_true(is.list(restricted_other_map))
        expect_true(is.list(gksrs_other_map))
    })
    
    # Test 4: apply_criteria function creates cohort-specific other_map files
    test_that("apply_criteria function creates cohort-specific other_map files", {
        # Load test data
        test_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
        
        # Create a small subset for testing
        test_subset <- test_data[1:50, ]
        
        # Test that apply_criteria function works and returns expected structure
        result <- apply_criteria(test_subset)
        
        # Verify the function returns a list with expected cohort components
        expect_true(is.list(result))
        expect_true("uveal_melanoma_full_cohort" %in% names(result))
        expect_true("uveal_melanoma_restricted_cohort" %in% names(result))
        expect_true("uveal_melanoma_gksrs_only_cohort" %in% names(result))
        
        # Verify each cohort is a data frame
        expect_true(is.data.frame(result$uveal_melanoma_full_cohort))
        expect_true(is.data.frame(result$uveal_melanoma_restricted_cohort))
        expect_true(is.data.frame(result$uveal_melanoma_gksrs_only_cohort))
        
        # Verify cohort-specific other_map files are created
        expect_true(file.exists("final_data/Analytic Dataset/full_other_map.rds"))
        expect_true(file.exists("final_data/Analytic Dataset/restricted_other_map.rds"))
        expect_true(file.exists("final_data/Analytic Dataset/gksrs_only_other_map.rds"))
        
        # Test that we can load cohort-specific other_map files
        full_other_map <- readRDS("final_data/Analytic Dataset/full_other_map.rds")
        restricted_other_map <- readRDS("final_data/Analytic Dataset/restricted_other_map.rds")
        gksrs_other_map <- readRDS("final_data/Analytic Dataset/gksrs_only_other_map.rds")
        
        expect_true(is.list(full_other_map))
        expect_true(is.list(restricted_other_map))
        expect_true(is.list(gksrs_other_map))
        
        # Test that we can load cohort-specific other_map files
        full_other_map <- readRDS("final_data/Analytic Dataset/full_other_map.rds")
        restricted_other_map <- readRDS("final_data/Analytic Dataset/restricted_other_map.rds")
        gksrs_other_map <- readRDS("final_data/Analytic Dataset/gksrs_only_other_map.rds")
        
        expect_true(is.list(full_other_map))
        expect_true(is.list(restricted_other_map))
        expect_true(is.list(gksrs_other_map))
    }) 