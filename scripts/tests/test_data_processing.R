# Test Data Processing Components
# Independent test of data loading, cleaning, and preprocessing

# Load required libraries
suppressMessages({
    library(tidyverse)
    library(lubridate)  # Explicitly load for time_length and interval functions
    library(readxl)
    library(gtsummary)
    library(gt)
})

# Source required scripts
source("scripts/data_helper/data_processing.R")
source("scripts/data_helper/data_utilities.R")
source("scripts/utils/analysis_config.R")

# Create test output directory
test_output_dir <- file.path("test_output", format(Sys.time(), "%Y%m%d_%H%M%S"))
dir.create(test_output_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== TESTING DATA PROCESSING COMPONENTS ===\n")
cat(sprintf("Test output directory: %s\n", test_output_dir))

# Test 1: Check for raw data file
cat("\n1. Checking for raw data file...\n")
raw_data_file <- "Ocular Melanoma Master Spreadsheet REVISED FOR STATS (5-10-25, TJM).xlsx"
raw_data_path <- file.path("data", raw_data_file)

if (file.exists(raw_data_path)) {
    cat(sprintf("✓ Raw data file found: %s\n", raw_data_path))
    
    # Test loading raw data
    cat("\n2. Testing raw data loading...\n")
    tryCatch({
        raw_data <- load_and_clean_data(filename = raw_data_file)
        cat(sprintf("✓ Raw data loaded successfully: %d rows, %d columns\n", 
                    nrow(raw_data), ncol(raw_data)))
        
        # Show basic data structure
        cat("Key columns:\n")
        key_cols <- c("study_id", "treatment_group", "age_at_treatment", "sex", 
                     "recurrence1", "mets_progression", "death_event")
        for (col in key_cols) {
            if (col %in% names(raw_data)) {
                cat(sprintf("  %s: present\n", col))
            } else {
                cat(sprintf("  %s: MISSING\n", col))
            }
        }
        
    }, error = function(e) {
        cat("✗ Raw data loading failed:", e$message, "\n")
        raw_data <- NULL
    })
    
} else {
    cat(sprintf("✗ Raw data file not found: %s\n", raw_data_path))
    cat("Creating simulated test data for testing...\n")
    
    # Create simulated data for testing with column names that match what create_derived_variables expects
    set.seed(123)
    base_date <- as.Date("2020-01-01")
    raw_data <- data.frame(
        id = 1:200,
        initial_gk = sample(c("Y", "N"), 200, replace = TRUE, prob = c(0.3, 0.7)),
        initial_plaque = sample(c("Y", "N"), 200, replace = TRUE, prob = c(0.7, 0.3)),
        dob = base_date - runif(200, 365*40, 365*80),  # Age 40-80
        date_diagnosis = base_date + runif(200, -365*2, 0),  # Diagnosed up to 2 years ago
        last_known_alive_date = base_date + runif(200, 30, 365*3),  # Follow-up data
        initial_gk_date = base_date + runif(200, -365, 365),
        initial_plaque_date = base_date + runif(200, -365, 365),
        sex = sample(c("Male", "Female"), 200, replace = TRUE),
        location = sample(c("Temporal", "Nasal", "Superior", "Inferior"), 200, replace = TRUE),
        recurrence1 = sample(c("Y", "N"), 200, replace = TRUE, prob = c(0.2, 0.8)),
        recurrence1_date = base_date + runif(200, 180, 365*2),
        recurrence1_treatment = sample(c("GKSRS", "Enucleation", "TTT"), 200, replace = TRUE),
        recurrence1_treatment_date = base_date + runif(200, 200, 365*2),
        recurrence2 = sample(c("Y", "N"), 200, replace = TRUE, prob = c(0.1, 0.9)),
        recurrence2_date = base_date + runif(200, 365, 365*3),
        mets_progression = sample(c("Y", "N"), 200, replace = TRUE, prob = c(0.1, 0.9)),
        mets_progression_date = base_date + runif(200, 365, 365*4),
        dod = ifelse(runif(200) < 0.15, base_date + runif(200, 365, 365*5), NA),
        consort_group = sample(c("eligible_both", "gksrs_only"), 200, replace = TRUE),
        stringsAsFactors = FALSE
    )
    # Ensure logical consistency - if initial_gk="Y" then initial_plaque="N" and vice versa
    raw_data <- raw_data %>%
        mutate(
            initial_plaque = ifelse(initial_gk == "Y", "N", 
                                   ifelse(initial_gk == "N", "Y", initial_plaque))
        )
    cat("✓ Simulated test data created\n")
}

# Test 3: Derived variables creation
if (!is.null(raw_data)) {
    cat("\n3. Testing derived variables creation...\n")
    
    # Initialize derived_data first
    derived_data <- NULL
    
    tryCatch({
        derived_data <<- create_derived_variables(raw_data)  # Use <<- to assign to parent scope
        cat("✓ Derived variables created successfully\n")
        
        # Check for new variables
        derived_vars <- setdiff(names(derived_data), names(raw_data))
        if (length(derived_vars) > 0) {
            cat(sprintf("  New variables created: %s\n", paste(derived_vars, collapse = ", ")))
        }
        
        # Check tumor height change if available
        if ("tumor_height_change" %in% names(derived_data)) {
            height_change_summary <- summary(derived_data$tumor_height_change)
            cat(sprintf("  Tumor height change summary: %.2f to %.2f\n",
                        min(derived_data$tumor_height_change, na.rm = TRUE),
                        max(derived_data$tumor_height_change, na.rm = TRUE)))
        }
        
    }, error = function(e) {
        cat("✗ Derived variables creation failed:", e$message, "\n")
        derived_data <<- raw_data  # Use <<- to assign to parent scope
    })
} else {
    derived_data <- NULL
}

# Test 4: Factor level preparation
if (!is.null(derived_data)) {
    cat("\n4. Testing factor level preparation...\n")
    
    # Initialize factored_data first
    factored_data <- NULL
    
    tryCatch({
        factored_data <<- prepare_factor_levels(derived_data)  # Use <<- to assign to parent scope
        cat("✓ Factor levels prepared successfully\n")
        
        # Check factor variables
        factor_vars <- sapply(factored_data, is.factor)
        if (any(factor_vars)) {
            cat("Factor variables:\n")
            for (var in names(factored_data)[factor_vars]) {
                levels_str <- paste(levels(factored_data[[var]]), collapse = ", ")
                cat(sprintf("  %s: %s\n", var, levels_str))
            }
        }
        
    }, error = function(e) {
        cat("✗ Factor level preparation failed:", e$message, "\n")
        factored_data <<- derived_data  # Use <<- to assign to parent scope
    })
} else {
    factored_data <- NULL
}

# Test 5: Cohort creation (inclusion/exclusion criteria)
if (!is.null(factored_data)) {
    cat("\n5. Testing cohort creation...\n")
    
    # Initialize cohorts first
    cohorts <- NULL
    
    tryCatch({
        cohorts <<- apply_criteria(factored_data)  # Use <<- to assign to parent scope
        cat("✓ Cohort creation completed successfully\n")
        
        # Show cohort sizes
        for (cohort_name in names(cohorts)) {
            cohort_size <- nrow(cohorts[[cohort_name]])
            cat(sprintf("  %s: %d patients\n", cohort_name, cohort_size))
        }
        
        # Save test cohorts
        for (cohort_name in names(cohorts)) {
            saveRDS(cohorts[[cohort_name]], 
                    file.path(test_output_dir, paste0("test_", cohort_name, ".rds")))
        }
        cat("✓ Test cohorts saved\n")
        
    }, error = function(e) {
        cat("✗ Cohort creation failed:", e$message, "\n")
        cohorts <<- NULL  # Use <<- to assign to parent scope
    })
} else {
    cohorts <- NULL
}

# Test 6: Data utility functions
cat("\n6. Testing data utility functions...\n")

# Test with processed data if available, otherwise use existing
test_data_path <- "final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds"
if (file.exists(test_data_path)) {
    test_data <- readRDS(test_data_path)
    cat("✓ Using existing processed data for utility tests\n")
} else if (!is.null(cohorts) && length(cohorts) > 0) {
    test_data <- cohorts[[1]]
    cat("✓ Using newly created cohort for utility tests\n")
} else {
    cat("⚠ No data available for utility function tests\n")
    test_data <- NULL
}

if (!is.null(test_data)) {
    # Test rare category handling
    tryCatch({
        processed_data <- handle_rare_categories(
            test_data,
            vars = c("sex", "location"),
            threshold = 5
        )
        cat("✓ Rare category handling tested\n")
        
    }, error = function(e) {
        cat("✗ Rare category handling failed:", e$message, "\n")
    })
    
    # Test factor consistency
    tryCatch({
        consistent_data <- ensure_consistent_contrasts(test_data)
        cat("✓ Factor consistency ensured\n")
        
    }, error = function(e) {
        cat("✗ Factor consistency test failed:", e$message, "\n")
    })
    
    # Test confounders validation
    tryCatch({
        valid_confounders <- generate_valid_confounders(
            test_data,
            confounders,
            threshold = THRESHOLD_RARITY
        )
        cat(sprintf("✓ Confounder validation: %d out of %d valid\n",
                    length(valid_confounders), length(confounders)))
        
    }, error = function(e) {
        cat("✗ Confounder validation failed:", e$message, "\n")
    })
}

# Test 7: Summary table creation
if (!is.null(cohorts) && length(cohorts) > 0) {
    cat("\n7. Testing summary table creation...\n")
    tryCatch({
        summary_tables <- create_summary_tables(cohorts)
        cat("✓ Summary tables created successfully\n")
        
        # Save sample summary table
        if (length(summary_tables) > 0) {
            first_table <- summary_tables[[1]]
            first_table %>%
                as_gt() %>%
                gtsave(file.path(test_output_dir, "test_summary_table.html"))
            cat("✓ Sample summary table saved\n")
        }
        
    }, error = function(e) {
        cat("✗ Summary table creation failed:", e$message, "\n")
    })
}

# Test 8: Check existing processed data
cat("\n8. Checking existing processed datasets...\n")
processed_data_dir <- "final_data/Analytic Dataset"
if (dir.exists(processed_data_dir)) {
    processed_files <- list.files(processed_data_dir, pattern = "\\.rds$", full.names = TRUE)
    
    if (length(processed_files) > 0) {
        cat("Existing processed datasets:\n")
        for (file_path in processed_files) {
            tryCatch({
                data <- readRDS(file_path)
                cat(sprintf("  %s: %d patients, %d variables\n",
                            basename(file_path), nrow(data), ncol(data)))
            }, error = function(e) {
                cat(sprintf("  %s: ERROR loading\n", basename(file_path)))
            })
        }
    } else {
        cat("  No processed datasets found\n")
    }
} else {
    cat("  Processed data directory does not exist\n")
}

# Summary
cat("\n=== DATA PROCESSING TEST SUMMARY ===\n")
cat(sprintf("All test outputs saved to: %s\n", test_output_dir))
cat("\nTest components checked:\n")
cat("  - Raw data file availability and loading\n")
cat("  - Derived variables creation\n")
cat("  - Factor level preparation\n")
cat("  - Cohort creation (inclusion/exclusion criteria)\n")
cat("  - Data utility functions (rare categories, factor consistency)\n")
cat("  - Summary table creation\n")
cat("  - Existing processed dataset validation\n")

cat("\nTo test individual functions:\n")
cat("  load_and_clean_data(filename)\n")
cat("  create_derived_variables(data)\n")
cat("  prepare_factor_levels(data)\n")
cat("  apply_criteria(data)\n")
cat("  handle_rare_categories(data, vars, threshold)\n") 