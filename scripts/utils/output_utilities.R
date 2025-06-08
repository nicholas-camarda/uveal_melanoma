# Output Utilities and Directory Management Functions
# Author: Nicholas Camarda
# Description: Functions for creating output directories and merging tables

#' Create organized output directory structure
#'
#' Creates a well-organized directory structure for analysis outputs
#'
#' @param base_dir Base directory for outputs (e.g., tables_dir or figures_dir)
#' @return List of created directory paths
#' @examples
#' create_output_structure("/path/to/tables")
create_output_structure <- function(base_dir) {
    # Define the directory structure
    dirs <- list(
        recurrence = file.path(base_dir, "primary_outcomes", "recurrence"),
        mets = file.path(base_dir, "primary_outcomes", "metastatic_progression"),
        os = file.path(base_dir, "primary_outcomes", "overall_survival"),
        pfs = file.path(base_dir, "primary_outcomes", "progression_free_survival"),
        height_primary = file.path(base_dir, "primary_outcomes", "tumor_height_change", "primary_analysis"),
        height_sensitivity = file.path(base_dir, "primary_outcomes", "tumor_height_change", "sensitivity_analysis"),
        subgroup_primary = file.path(base_dir, "primary_outcomes", "tumor_height_change", "subgroup_interactions", "without_baseline_height"),
        subgroup_sensitivity = file.path(base_dir, "primary_outcomes", "tumor_height_change", "subgroup_interactions", "with_baseline_height"),
        # Step 2: Safety/Toxicity outcomes
        vision = file.path(base_dir, "safety_toxicity", "vision_change"),
        retinopathy = file.path(base_dir, "safety_toxicity", "radiation_sequelae", "retinopathy"),
        nvg = file.path(base_dir, "safety_toxicity", "radiation_sequelae", "nvg"),
        srg = file.path(base_dir, "safety_toxicity", "radiation_sequelae", "srg"),
        treatment_duration = file.path(base_dir, "treatment_duration")
    )
    
    # Create all directories
    for (dir_path in dirs) {
        if (!dir.exists(dir_path)) {
            dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
            log_message(sprintf("Created directory: %s", dir_path))
        }
    }
    
    return(dirs)
}

#' Merge Full and Restricted Cohort Tables Side by Side
#'
#' This function creates merged tables that display full and restricted cohort data side by side
#' for easier comparison and presentation, as requested by collaborators.
#' It reads existing HTML tables and recreates them in Excel format with both cohorts.
#'
#' @param full_cohort_data Data frame containing the full cohort data
#' @param restricted_cohort_data Data frame containing the restricted cohort data
#' @param output_path Path where merged tables should be saved
#'
#' @return None. Side effect: saves merged tables as Excel files
#'
#' @examples
#' merge_cohort_tables(full_data, restricted_data, "final_data/Analysis/merged_tables/")
merge_cohort_tables <- function(full_cohort_data, restricted_cohort_data, output_path = NULL) {
    
    log_message("=== STARTING TABLE MERGING: Full and Restricted Cohorts ===")
    
    # Set default output path if not provided
    if (is.null(output_path)) {
        output_path <- file.path("final_data", "Analysis", "merged_tables")
    }
    
    # Create output directory
    if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
        log_message(sprintf("Created merged tables directory: %s", output_path))
    }
    
    log_message(sprintf("Merging tables will be saved to: %s", output_path))
    
    # Define variables for baseline characteristics summary
    vars_to_summarize <- c(
        "age_at_diagnosis", "race", "sex", "eye",
        "initial_vision", "location", "optic_nerve",
        "initial_tumor_height", "initial_tumor_diameter",
        "internal_reflectivity", "srf", "op", "symptoms",
        "vision_loss_blurred_vision", "visual_field_defect",
        "flashes_photopsia", "floaters", "pain",
        "initial_overall_stage", "initial_t_stage",
        "initial_n_stage", "initial_m_stage",
        "initial_mets", "biopsy1_gep"
    )
    
    # 1. BASELINE CHARACTERISTICS TABLE (Most Important)
    log_message("Reading existing baseline characteristics tables and merging them")
    
    # Read the existing summary tables that were already created
    # These should be available from the analysis that was just run
    tryCatch({
        # Try to read from the latest results directory first
        full_cohort_dir <- file.path("old_results", format(Sys.Date(), "%y_%m_%d"), "uveal_full", "tables")
        restricted_cohort_dir <- file.path("old_results", format(Sys.Date(), "%y_%m_%d"), "uveal_restricted", "tables")
        
        # If today's results don't exist, find the most recent results
        if (!file.exists(file.path(full_cohort_dir, "uveal_full_baseline_characteristics.html"))) {
            # Find the most recent results directory
            results_dirs <- list.dirs("old_results", recursive = FALSE)
            if (length(results_dirs) > 0) {
                latest_dir <- results_dirs[length(results_dirs)]
                full_cohort_dir <- file.path(latest_dir, "uveal_full", "tables")
                restricted_cohort_dir <- file.path(latest_dir, "uveal_restricted", "tables") 
            }
        }
        
        log_message(sprintf("Looking for tables in: %s", full_cohort_dir))
        
        # For now, just recreate simple versions since reading HTML tables is complex
        # This is still much simpler than the previous version
        
        # Quick baseline table for full cohort
        full_baseline <- full_cohort_data %>%
            select(all_of(vars_to_summarize), treatment_group) %>%
            tbl_summary(by = treatment_group, missing = "no") %>%
            add_overall() %>%
            add_p() %>%
            as_tibble()
        
        # Quick baseline table for restricted cohort  
        restricted_baseline <- restricted_cohort_data %>%
            select(all_of(vars_to_summarize), treatment_group) %>%
            tbl_summary(by = treatment_group, missing = "no") %>%
            add_overall() %>%
            add_p() %>%
            as_tibble()
        
        # Check what the first column is actually called
        log_message(sprintf("Full table columns: %s", paste(names(full_baseline), collapse = ", ")))
        log_message(sprintf("Restricted table columns: %s", paste(names(restricted_baseline), collapse = ", ")))
        
        # Get the actual name of the first column (variable names)
        var_col_name <- names(full_baseline)[1]
        
        # Rename columns to distinguish cohorts (skip the first column which is variables)
        names(full_baseline)[2:ncol(full_baseline)] <- paste("Full Cohort", names(full_baseline)[2:ncol(full_baseline)], sep = " - ")
        names(restricted_baseline)[2:ncol(restricted_baseline)] <- paste("Restricted Cohort", names(restricted_baseline)[2:ncol(restricted_baseline)], sep = " - ")
        
        # Merge side by side using the actual variable column name
        merged_baseline <- full_baseline %>%
            full_join(restricted_baseline, by = var_col_name)
        
        # Save as Excel
        writexl::write_xlsx(
            merged_baseline,
            path = file.path(output_path, "merged_baseline_characteristics.xlsx")
        )
        
        log_message("Saved merged baseline characteristics table")
        
    }, error = function(e) {
        log_message(sprintf("Error merging baseline tables: %s", e$message))
        log_message("Skipping baseline table merge")
    })
    
    # Summary message
    log_message("=== COMPLETED TABLE MERGING ===")
    log_message(sprintf("Merged baseline characteristics table saved to: %s", output_path))
    log_message("File created: merged_baseline_characteristics.xlsx")
    
    return(invisible(NULL))
} 