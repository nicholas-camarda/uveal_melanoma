# Test Factor Level Indentation
# Standalone test to perfect the factor level indentation functionality

# Load required libraries
suppressMessages({
    library(tidyverse)
    library(gtsummary)
    library(gt)
})

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/utils/output_utilities.R")
source("scripts/data_helper/data_utilities.R")

# Create test output directory
test_output_dir <- file.path("test_output", "factor_indentation_test")
dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== TESTING FACTOR LEVEL INDENTATION ===\n")
cat(sprintf("Test output directory: %s\n", test_output_dir))

# Load a small subset of test data
cat("\n1. Loading test data...\n")
tryCatch({
    full_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")
    
    # Create a small test dataset with key variables
    test_data <- full_data %>%
        slice_head(n = 50) %>%  # Small subset for fast testing
        select(treatment_group, age_at_diagnosis, sex, location, initial_tumor_height, biopsy1_gep) %>%
        # Ensure we have some factor levels to test
        filter(!is.na(sex), !is.na(location))
    
    cat(sprintf("✓ Created test dataset with %d patients\n", nrow(test_data)))
    cat(sprintf("  Variables: %s\n", paste(names(test_data), collapse = ", ")))
    
}, error = function(e) {
    cat("✗ Error loading test data:", e$message, "\n")
    stop("Cannot proceed without test data")
})

# Test 2: Create a simple baseline table
cat("\n2. Creating test baseline table...\n")
tryCatch({
    # Create a simple baseline characteristics table
    baseline_table <- test_data %>%
        tbl_summary(
            by = treatment_group,
            missing = "no"
        ) %>%
        add_overall() %>%
        add_p()
    
    cat("✓ Baseline table created successfully\n")
    
    # Convert to tibble to examine structure
    baseline_tibble <- baseline_table %>% as_tibble()
    cat(sprintf("  Table dimensions: %d rows x %d columns\n", nrow(baseline_tibble), ncol(baseline_tibble)))
    cat("  Column names:", paste(names(baseline_tibble), collapse = ", "), "\n")
    
    # Show first few rows to understand structure
    cat("\nFirst few rows of table:\n")
    print(baseline_tibble %>% slice_head(n = 10))
    
}, error = function(e) {
    cat("✗ Baseline table creation failed:", e$message, "\n")
})

# Test 3: Create gt table without indentation first
cat("\n3. Creating gt table without indentation...\n")
tryCatch({
    gt_table_simple <- baseline_table %>%
        as_gt()
    
    # Save simple version
    save_gt_html(
        gt_table_simple,
        filename = file.path(test_output_dir, "baseline_simple.html")
    )
    
    cat("✓ Simple gt table saved successfully\n")
    
}, error = function(e) {
    cat("✗ Simple gt table creation failed:", e$message, "\n")
})

# Test 4: Debug the helper function step by step
cat("\n4. Debugging factor level detection...\n")
tryCatch({
    # Get the tibble version
    table_data <- baseline_table %>% as_tibble()
    
    # Debug the detection logic step by step
    cat("Analyzing table structure for factor level detection:\n")
    
    # Check data columns
    data_cols <- 2:min(4, ncol(table_data))
    cat(sprintf("  Data columns to check: %s\n", paste(data_cols, collapse = ", ")))
    
    # Check first column (variable names)
    var_names <- table_data[[1]]
    cat(sprintf("  Variable names in first column: %s\n", paste(head(var_names, 10), collapse = ", ")))
    
    # Look for patterns that indicate main variables vs factor levels
    for (i in 1:min(10, nrow(table_data))) {
        row_data <- table_data[i, data_cols, drop = FALSE]
        has_na <- any(grepl("^NA$|^N/A$", row_data, ignore.case = TRUE))
        cat(sprintf("  Row %d ('%s'): Has NA = %s, Data = %s\n", 
                    i, table_data[[1]][i], has_na, 
                    paste(as.character(row_data[1, ]), collapse = ", ")))
    }
    
    # Test the detection logic
    main_variables <- apply(table_data[, data_cols, drop = FALSE], 1, function(row) {
        all(grepl("^NA$|^N/A$", as.character(row), ignore.case = TRUE))
    })
    
    cat(sprintf("\nMain variables detected: %d out of %d rows\n", sum(main_variables), length(main_variables)))
    cat("Main variable rows:\n")
    main_rows <- which(main_variables)
    for (row_idx in main_rows) {
        cat(sprintf("  Row %d: %s\n", row_idx, table_data[[1]][row_idx]))
    }
    
    cat("\nFactor level rows:\n")
    factor_rows <- which(!main_variables)
    for (row_idx in head(factor_rows, 10)) {  # Show first 10
        cat(sprintf("  Row %d: %s\n", row_idx, table_data[[1]][row_idx]))
    }
    
}, error = function(e) {
    cat("✗ Factor level detection failed:", e$message, "\n")
})

# Test 5: Create a simple working version of the helper function
cat("\n5. Testing simplified helper function...\n")

# Create a simple, robust version of the helper function
apply_factor_indentation_simple <- function(gt_table, table_data) {
    tryCatch({
        # Better logic: main variables have "NA" in their data columns
        # Factor levels have actual percentages/values
        
        # Check data columns for NA patterns
        data_cols <- 2:min(4, ncol(table_data))
        var_names <- table_data[[1]]
        
        # A row is a main variable if the "Overall" column contains "NA"
        # The "Overall" column is usually column 2
        overall_column <- table_data[[2]]
        
        # Debug: print what we're actually looking for
        cat("Overall column values:\n")
        for (i in 1:min(10, length(overall_column))) {
            cat(sprintf("  Row %d: '%s' (class: %s)\n", i, overall_column[i], class(overall_column[i])))
        }
        
        # Try exact match, handling actual NA values safely
        main_variable_rows <- !is.na(overall_column) & (overall_column == "NA")
        
        # If that doesn't work, try better detection logic
        if (sum(main_variable_rows) < 3) {  # If we found very few main variables, logic is probably wrong
            cat("Exact NA matching failed, using improved detection...\n")
            var_names <- table_data[[1]]
            
            # Better approach: main variables are those where all data columns show "NA"
            # Check if ALL data columns have "NA" for this row
                         for (i in 1:nrow(table_data)) {
                 # Only check data columns (2-4), not p-value column (5)
                 data_row <- table_data[i, 2:4]  
                 
                 all_na <- all(trimws(as.character(data_row)) == "NA", na.rm = TRUE)
                 if (i <= 15) {  # Debug first 15 rows
                     cat(sprintf("  Row %d (%s): All NA = %s, Values = %s\n", 
                                i, var_names[i], all_na, 
                                paste(head(as.character(data_row), 3), collapse = ", ")))
                 }
             }
            
                         # Apply the logic to all rows - check ONLY data columns, NOT p-value column
            main_variable_rows <- sapply(1:nrow(table_data), function(i) {
                # Only check data columns (2-4), exclude p-value column (5)
                data_row <- table_data[i, 2:4]  # Fixed: only data columns
                # Check if all data values are "NA" (character strings)
                all(trimws(as.character(data_row)) == "NA", na.rm = TRUE)
            })
            
            cat(sprintf("Improved detection found %d main variables\n", sum(main_variable_rows)))
        }
        factor_level_rows <- !main_variable_rows
        
        cat(sprintf("Better detection: %d main variables, %d factor levels\n", 
                    sum(main_variable_rows), sum(factor_level_rows)))
        
        # Show what we detected
        cat("Main variables:\n")
        main_indices <- which(main_variable_rows)
        for (i in main_indices) {
            cat(sprintf("  Row %d: %s\n", i, var_names[i]))
        }
        
        cat("Factor levels:\n")
        factor_indices <- which(factor_level_rows)
        for (i in head(factor_indices, 8)) {  # Show first 8
            cat(sprintf("  Row %d: %s\n", i, var_names[i]))
        }
        
        # Only apply styling if we have a reasonable number of each
        if (sum(main_variable_rows) > 0 && sum(factor_level_rows) > 0) {
            # Get the actual column name instead of using column number
            first_col_name <- names(table_data)[1]
            cat(sprintf("First column name: '%s'\n", first_col_name))
            
            # Debug row indices
            cat(sprintf("Main indices: %s\n", paste(main_indices, collapse = ", ")))
            cat(sprintf("Factor indices: %s (showing first 10)\n", paste(head(factor_indices, 10), collapse = ", ")))
            cat(sprintf("Total table rows: %d\n", nrow(table_data)))
            
            # Check if indices are valid
            max_main <- if(length(main_indices) > 0) max(main_indices) else 0
            max_factor <- if(length(factor_indices) > 0) max(factor_indices) else 0
            cat(sprintf("Max main index: %d, Max factor index: %d\n", max_main, max_factor))
            
            if (max_main <= nrow(table_data) && max_factor <= nrow(table_data)) {
                # Apply styling with correct logic
                tryCatch({
                    # Apply bold to main variables - ONLY first column, not entire row
                    gt_table <- gt_table %>%
                        tab_style(
                            style = cell_text(weight = "bold"),
                            locations = cells_body(
                                columns = 1,  # Only first column
                                rows = main_indices
                            )
                        )
                    cat("✓ Bold styling applied to first column only\n")
                }, error = function(e) {
                    cat(sprintf("Bold styling failed: %s\n", e$message))
                })
                
                tryCatch({
                    # Apply indentation to factor levels
                    gt_table <- gt_table %>%
                        tab_style(
                            style = cell_text(indent = px(20)),
                            locations = cells_body(
                                columns = 1,
                                rows = factor_indices
                            )
                        )
                    cat("✓ Indentation styling applied\n")
                }, error = function(e) {
                    cat(sprintf("Indentation styling failed: %s\n", e$message))
                })
                
                cat("✓ Styling process completed\n")
            } else {
                cat("⚠ Row indices are out of bounds, skipping styling\n")
            }
        } else {
            cat("⚠ Skipping styling - detection results don't look right\n")
        }
        
        return(gt_table)
        
    }, error = function(e) {
        cat(sprintf("✗ Factor indentation failed: %s\n", e$message))
        return(gt_table)
    })
}

tryCatch({
    # Test the simple version
    gt_table_styled <- baseline_table %>%
        as_gt() %>%
        apply_factor_indentation_simple(baseline_table %>% as_tibble())
    
    # Save styled version
    save_gt_html(
        gt_table_styled,
        filename = file.path(test_output_dir, "baseline_styled.html")
    )
    
    cat("✓ Styled gt table saved successfully\n")
    
}, error = function(e) {
    cat("✗ Styled gt table creation failed:", e$message, "\n")
})

# Test 6: Save the gt table as a regular HTML file
cat("\n6. Saving gt table as a regular HTML file...\n")
tryCatch({
    # Save table
    save_gt_html(
        gt_table,
        filename = file.path(test_output_dir, "baseline.html")
    )
    
    cat("✓ gt table saved successfully\n")
    
}, error = function(e) {
    cat("✗ gt table creation failed:", e$message, "\n")
})

# Summary
cat("\n=== FACTOR INDENTATION TEST SUMMARY ===\n")
cat(sprintf("Test outputs saved to: %s\n", test_output_dir))
cat("\nFiles created:\n")
cat("  - baseline.html\n")
cat("\nCheck the file to verify formatting!\n") 