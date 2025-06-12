# Generate CONSORT Diagram Data for Collaborators
# Author: Nicholas Camarda
# Description: Creates Excel file with patient status counts (Dead, Lost to F/U, Alive) 
#              for each treatment arm in full and restricted cohorts

# Load required libraries
library(tidyverse)
library(readxl)
library(writexl)

# Source the logging utilities
source("scripts/utils/analysis_config.R")

# Define paths
PROCESSED_DATA_DIR <- "final_data/Analytic Dataset"
OUTPUT_DIR <- "final_data/Analysis"

#' Generate CONSORT data for collaborators
#' 
#' Creates a summary table with patient status counts for CONSORT diagram
#' 
#' @return Data frame with CONSORT counts
generate_consort_data <- function() {
    
    log_enhanced("Starting CONSORT data generation", level = "INFO")
    
    # Load both cohorts
    log_enhanced("Loading cohort data files", level = "INFO")
    
    full_cohort <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"))
    restricted_cohort <- readRDS(file.path(PROCESSED_DATA_DIR, "uveal_melanoma_restricted_cohort.rds"))
    
    log_enhanced(sprintf("Full cohort loaded: %d patients", nrow(full_cohort)), level = "INFO")
    log_enhanced(sprintf("Restricted cohort loaded: %d patients", nrow(restricted_cohort)), level = "INFO")
    
    # Function to calculate status counts for a cohort
    calculate_status_counts <- function(data, cohort_name) {
        
        # Determine patient status using proper survival analysis definitions
        # Lost to F/U = censored patients whose last contact was >1 year before study end (2024)
        study_cutoff_date <- as.Date("2024-01-01")  # Conservative cutoff date
        
        status_data <- data %>%
            mutate(
                patient_status = case_when(
                    # Dead: death_event = 1 (event occurred)
                    death_event == 1 ~ "Dead",
                    # Lost to F/U: death_event = 0 (censored) AND last contact >1 year before study cutoff
                    death_event == 0 & last_known_alive_date < study_cutoff_date ~ "Lost to F/U",
                    # Alive: death_event = 0 (censored) AND last contact within past year
                    death_event == 0 & last_known_alive_date >= study_cutoff_date ~ "Alive",
                    # This should cover all cases, but just in case:
                    TRUE ~ "Unknown"
                )
            )
        
        # Calculate counts by treatment group and status
        counts <- status_data %>%
            group_by(treatment_group, patient_status) %>%
            summarise(count = n(), .groups = "drop") %>%
            pivot_wider(names_from = patient_status, values_from = count, values_fill = 0) %>%
            mutate(cohort = cohort_name) %>%
            mutate(
                total = rowSums(select(., -treatment_group, -cohort), na.rm = TRUE)
            ) %>%
            select(cohort, treatment_group, everything(), total)
        
        return(counts)
    }
    
    # Calculate counts for both cohorts
    log_enhanced("Calculating status counts for full cohort", level = "INFO")
    full_counts <- calculate_status_counts(full_cohort, "Full Cohort")
    
    log_enhanced("Calculating status counts for restricted cohort", level = "INFO")
    restricted_counts <- calculate_status_counts(restricted_cohort, "Restricted Cohort")
    
    # Combine results
    consort_data <- bind_rows(full_counts, restricted_counts)
    
    # Add totals by cohort
    cohort_totals <- consort_data %>%
        group_by(cohort) %>%
        summarise(
            treatment_group = "Total",
            Alive = sum(Alive, na.rm = TRUE),
            Dead = sum(Dead, na.rm = TRUE),
            `Lost to F/U` = sum(`Lost to F/U`, na.rm = TRUE),
            total = sum(total, na.rm = TRUE),
            .groups = "drop"
        )
    
    # Combine with totals
    final_consort_data <- bind_rows(consort_data, cohort_totals) %>%
        arrange(cohort, treatment_group)
    
    log_enhanced("CONSORT data generation completed", level = "INFO")
    
    return(final_consort_data)
}

#' Main execution function
consort_main <- function() {
    
    # Create output directory if it doesn't exist
    if (!dir.exists(OUTPUT_DIR)) {
        dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Generate CONSORT data
    consort_data <- generate_consort_data()
    
    # Print results to console
    cat("\n=== CONSORT DIAGRAM DATA ===\n")
    print(consort_data)
    
    # Create output file without timestamp
    output_file <- file.path(OUTPUT_DIR, "consort_data.xlsx")
    
    # Save to Excel file
    log_enhanced(sprintf("Saving CONSORT data to: %s", output_file), level = "INFO")
    
    write_xlsx(consort_data, output_file)
    
    log_enhanced("CONSORT data Excel file created successfully", level = "SUCCESS")
    cat(sprintf("\nOutput saved to: %s\n", output_file))
    
    return(consort_data)
}

# Run the script if called directly
consort_main()
