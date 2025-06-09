# Simple Working Factor Level Indentation Test
# This version modifies the data directly instead of using problematic gt styling

# Load required libraries
suppressMessages({
    library(tidyverse)
    library(gtsummary)
    library(gt)
})

# Source required scripts
source("scripts/utils/analysis_config.R")
source("scripts/utils/output_utilities.R")

# Create test output directory
test_output_dir <- file.path("test_output", "simple_indentation_test")
dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== SIMPLE FACTOR LEVEL INDENTATION TEST ===\n")
cat(sprintf("Test output directory: %s\n", test_output_dir))

# Load test data
cat("\n1. Loading test data...\n")
full_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

test_data <- full_data %>%
    slice_head(n = 50) %>%
    select(treatment_group, age_at_diagnosis, sex, location, initial_tumor_height, biopsy1_gep) %>%
    filter(!is.na(sex), !is.na(location))

cat(sprintf("✓ Created test dataset with %d patients\n", nrow(test_data)))

# Create baseline table
cat("\n2. Creating baseline table...\n")
baseline_table <- test_data %>%
    tbl_summary(
        by = treatment_group,
        missing = "no"
    ) %>%
    add_overall() %>%
    add_p()

cat("✓ Baseline table created successfully\n")

# Simple working helper function
apply_factor_indentation_working <- function(tbl_summary_obj) {
    tryCatch({
        # Convert to tibble to modify the data
        table_data <- tbl_summary_obj %>% as_tibble()
        
        # CORRECT SEQUENTIAL LOGIC: Track when we're in a categorical variable's factor levels
        main_variable_rows <- logical(nrow(table_data))
        in_factor_levels <- FALSE
        
        # Determine data columns dynamically by excluding p-value column
        col_names <- names(table_data)
        total_cols <- ncol(table_data)
        
        # Find p-value column (usually contains "p-value", "p.value", etc.)
        pvalue_col <- which(grepl("p.?value", col_names, ignore.case = TRUE))
        
        if (length(pvalue_col) > 0) {
            # Exclude first column (variable names) and p-value column
            data_cols <- setdiff(2:total_cols, pvalue_col)
            cat(sprintf("Table has %d columns. Found p-value in column %d ('%s'). Using data columns: %s\n", 
                        total_cols, pvalue_col[1], col_names[pvalue_col[1]], paste(data_cols, collapse = ", ")))
        } else {
            # No p-value column found, exclude first and last columns
            data_cols <- 2:(total_cols - 1)
            cat(sprintf("Table has %d columns. No p-value column detected. Using data columns: %s\n", 
                        total_cols, paste(data_cols, collapse = ", ")))
        }
        
        if (length(data_cols) == 0) {
            stop("No data columns found after excluding variable names and p-values")
        }
        
        for (i in 1:nrow(table_data)) {
            data_row <- table_data[i, data_cols]  # Use detected data columns
            all_na <- all(trimws(as.character(data_row)) == "NA", na.rm = TRUE)
            
            if (all_na) {
                # This is a categorical main variable
                main_variable_rows[i] <- TRUE
                in_factor_levels <- TRUE  # Next rows will be factor levels
            } else {
                if (in_factor_levels) {
                    # We're in factor levels from previous categorical variable
                    main_variable_rows[i] <- FALSE
                                         # Check if next row is also a factor level or if we're done with this variable
                     if (i < nrow(table_data)) {
                         next_data_row <- table_data[i+1, data_cols]
                         next_all_na <- all(trimws(as.character(next_data_row)) == "NA", na.rm = TRUE)
                        if (next_all_na) {
                            # Next row is a new main variable, so we're done with factor levels
                            in_factor_levels <- FALSE
                        }
                    } else {
                        # Last row, so we're done
                        in_factor_levels <- FALSE
                    }
                } else {
                    # This is a continuous main variable (has data but not in factor levels)
                    main_variable_rows[i] <- TRUE
                }
            }
        }
        
        cat(sprintf("Detected %d main variables out of %d rows\n", 
                    sum(main_variable_rows), nrow(table_data)))
        
        # Modify the variable names directly in the data
        var_names <- table_data[[1]]
        modified_names <- var_names
        
        # Add HTML formatting directly to variable names
        for (i in 1:length(var_names)) {
            if (main_variable_rows[i]) {
                # Main variables: make bold
                modified_names[i] <- paste0("<b>", var_names[i], "</b>")
                cat(sprintf("  Main variable: %s\n", var_names[i]))
            } else {
                # Factor levels: add indentation
                modified_names[i] <- paste0("&nbsp;&nbsp;&nbsp;&nbsp;", var_names[i])
                if (length(which(!main_variable_rows)) <= 8) {  # Show first few
                    cat(sprintf("  Factor level: %s\n", var_names[i]))
                }
            }
        }
        
        # Update the table data
        table_data[[1]] <- modified_names
        
        # Convert back to gt table
        gt_table <- table_data %>%
            gt() %>%
            fmt_markdown(columns = 1)  # Enable HTML formatting in first column
        
        cat("✓ Indentation applied successfully using direct HTML formatting\n")
        return(gt_table)
        
    }, error = function(e) {
        cat(sprintf("✗ Helper function failed: %s\n", e$message))
        # Fallback to simple gt table
        return(tbl_summary_obj %>% as_gt())
    })
}

# Test the working version
cat("\n3. Testing working indentation...\n")
tryCatch({
    # Apply our working indentation
    styled_table <- apply_factor_indentation_working(baseline_table)
    
    # Save both versions
    save_gt_html(
        baseline_table %>% as_gt(),
        filename = file.path(test_output_dir, "baseline_simple.html")
    )
    
    save_gt_html(
        styled_table,
        filename = file.path(test_output_dir, "baseline_styled_working.html")
    )
    
    cat("✓ Both tables saved successfully!\n")
    
}, error = function(e) {
    cat(sprintf("✗ Test failed: %s\n", e$message))
})

# Summary
cat("\n=== SIMPLE TEST SUMMARY ===\n")
cat(sprintf("Test outputs saved to: %s\n", test_output_dir))
cat("\nFiles created:\n")
cat("  - baseline_simple.html (original table)\n")
cat("  - baseline_styled_working.html (with factor indentation)\n")
cat("\nThis approach uses direct HTML formatting instead of gt styling!\n") 