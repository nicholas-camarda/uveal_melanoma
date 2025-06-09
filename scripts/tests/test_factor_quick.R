# Quick test of improved factor indentation logic
library(tidyverse)
library(gtsummary)
library(gt)

# Source our function
source("scripts/utils/output_utilities.R")

# Load test data
full_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

test_data <- full_data %>%
    slice_head(n = 50) %>%
    select(treatment_group, age_at_diagnosis, sex, location, initial_tumor_height, biopsy1_gep) %>%
    filter(!is.na(sex), !is.na(location))

# Create baseline table
baseline_table <- test_data %>%
    tbl_summary(
        by = treatment_group,
        missing = "no"
    ) %>%
    add_overall() %>%
    add_p()

# Test our improved function
cat("Testing improved factor indentation...\n")
styled_table <- baseline_table %>% apply_factor_level_indentation()

# Save to check result
test_output_file <- "test_output/factor_indentation_improved_test.html"
styled_table %>% 
    save_gt_html(filename = test_output_file)

cat("Improved test table saved at:", test_output_file, "\n")
cat("Check if the main variables are bold and factor levels are indented!\n") 