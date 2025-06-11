# Test Factor Indentation in Production Code
# This script tests the apply_factor_level_indentation function
# with actual baseline table creation from the main data processing pipeline

# Load required libraries
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(gtsummary)
library(janitor)
library(survival)
library(survminer)
library(gt)
library(forestploter)
library(grid)
library(cowplot)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(survRM2, quietly = TRUE)

# Source configuration and utilities
source("scripts/utils/analysis_config.R")
source("scripts/data_helper/data_processing.R")
source("scripts/data_helper/data_utilities.R")
source("scripts/utils/output_utilities.R")

# Create directories
dir.create(PROCESSED_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Set settings
RECREATE_ANALYTIC_DATASETS <- TRUE
USE_LOGS <- TRUE
VERBOSE <- TRUE
SHOW_ALL_PVALUES <- TRUE

cat("=== TESTING FACTOR INDENTATION IN PRODUCTION ===\n")

# Load and process data
cat("Loading and processing data...\n")
cleaned_data <- load_and_clean_data(filename = INPUT_FILENAME)
derived_data <- create_derived_variables(cleaned_data)
factored_data <- prepare_factor_levels(derived_data)
final_analytic_datasets_lst <- apply_criteria(factored_data)

# Test with just the full cohort
cat("Testing with full cohort...\n")
cohort_name <- "uveal_melanoma_full_cohort"
data <- final_analytic_datasets_lst[[cohort_name]]

# Create directory structure for full cohort
cohort_base_dir <- file.path("test_output", "factor_indentation_production_test")
temp_output_dirs <- list()
temp_output_dirs[[cohort_name]] <- create_output_structure(cohort_base_dir)

# Create the baseline table with factor indentation
cat("Creating baseline table with factor indentation...\n")
summary_tables <- create_summary_tables(
    list(test_cohort = data), 
    list(test_cohort = temp_output_dirs[[cohort_name]])
)

# Output file location
output_file <- file.path(cohort_base_dir, "00_General/baseline_characteristics/full_cohort_baseline_characteristics.html")

cat("=== TEST COMPLETED ===\n")
cat("Baseline table with factor indentation created successfully!\n")
cat("Check the HTML file at:", output_file, "\n")

# Also create a simple version to inspect the output
cat("Creating simple inspection table...\n")
baseline_table <- summary_tables$test_cohort$baseline_table

# Test our function directly
cat("Testing apply_factor_level_indentation function directly...\n")
formatted_table <- baseline_table %>% apply_factor_level_indentation()

# Save the direct test result
test_output_file <- file.path("test_output", "factor_indentation_direct_test.html")
dir.create("test_output", showWarnings = FALSE, recursive = TRUE)

formatted_table %>% 
    save_gt_html(filename = test_output_file)

cat("Direct test table saved at:", test_output_file, "\n")
cat("=== ALL TESTS COMPLETED ===\n") 