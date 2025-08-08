########################################################
############### ANALYSIS SETTINGS #####################
########################################################

# Toggle logging functionality
USE_LOGS <- TRUE

# Toggle to control whether to recreate analytic datasets (default: FALSE)
# Set to TRUE if you need to reprocess raw data or if data has changed
RECREATE_ANALYTIC_DATASETS <- TRUE

# Set to FALSE to suppress detailed logging in analysis functions
VERBOSE <- TRUE

# Toggle between clinical binning for continuous variables and legacy median-based cutoffs
USE_CLINICAL_BINNING_CONTINUOUS <- TRUE  # DEFAULT: Use clinical thresholds for tumor height (10mm) and diameter (20mm)

######################################################################
############### LOAD / INSTALL REQUIRED LIBRARIES ####################
######################################################################
use <- function(pkg) {
    # Bootstrap pak if needed
    if (!requireNamespace("pak", quietly = TRUE)) {
        install.packages("pak")
    }

    # Install if missing using pak; try CRAN first, then Bioconductor via bioc::
    if (!requireNamespace(pkg, quietly = TRUE)) {
        tryCatch(
            {
                pak::pkg_install(pkg, ask = FALSE)
            },
            error = function(e1) {
                # Attempt Bioconductor namespace via pak
                tryCatch(
                    pak::pkg_install(paste0("bioc::", pkg), ask = FALSE),
                    error = function(e2) {
                        stop(sprintf("Failed to install package '%s' via pak (CRAN and Bioconductor attempts). Original errors: %s | %s", pkg, conditionMessage(e1), conditionMessage(e2)))
                    }
                )
            }
        )
    }

    suppressPackageStartupMessages(
        library(pkg, character.only = TRUE)
    )
}

######################################################################
############### LOAD / INSTALL REQUIRED LIBRARIES ####################
######################################################################

# Data wrangling & core utilities
use("tidyverse") # For data manipulation and visualization (dplyr, ggplot2, etc.)
use("readxl") # For reading Excel files
use("writexl") # For writing Excel files
use("openxlsx") # For creating Excel workbooks with multiple sheets
use("lubridate") # Date handling
use("janitor") # Data cleaning
use("broom.helpers") # For broom helpers
use("parameters") # For broom
use("gtsummary") # Creating publication-ready tables

# Core survival analysis
use("survival") # For survival analysis
use("survminer") # For survival visualization
use("survRM2") # Survival analysis at differ ent time points

# Tables and plots
use("gt") # Table formatting
use("cardx") # Extended statistical functions for gtsummary
use("forestploter") # Forest plots
use("grid") # grid::unit(), viewport helpers
use("cowplot") # Combining ggplots

# Testing
use("usethis") # For creating test files
use("testthat") # For testing
# usethis::use_testthat(3) # Set testthat edition to 3 for improved error messages
# local_edition(3) # Set testthat edition to 3 for improved error messages


# Advanced GEP validation (Objective 4)
use("rms") # Advanced regression modeling and validation
use("pec") # Prediction-error curves & validation metrics
use("survcomp") # Survival model comparison and validation (Bioconductor)
use("riskRegression") # Risk regression & competing risks
use("cmprsk") # Competing-risk analysis (Fine-Gray models)
use("pROC") # ROC analysis
use("rmda") # Risk-model decision analysis
use("VIM") # Visualization & imputation of missing values
use("mice") # Multiple imputation by chained equations

######################################################################
############### SOURCE ALL NECESSARY SCRIPTS #########################
######################################################################

# Source the split configuration and utility files
source("scripts/utils/config_constants.R")
source("scripts/utils/logging_utilities.R")
source("scripts/utils/validation_utilities.R")
source("scripts/utils/model_utilities.R")

# Source the extreme estimate handling utilities
source("scripts/utils/extreme_estimate_handling.R")

# Source the table generation utilities
source("scripts/utils/table_generation.R")

# Source the data processing script
source("scripts/data_helper/data_processing.R")

# Source the utility and helper scripts
source("scripts/data_helper/data_utilities.R")
source("scripts/utils/output_utilities.R")

# Source the main analysis function scripts
source("scripts/analysis/statistical_analysis.R")
source("scripts/analysis/tumor_height_analysis.R")
source("scripts/analysis/vision_safety_analysis.R")
source("scripts/analysis/subgroup_analysis.R")

# Source GEP evaluation modules
source("scripts/gep/utils/gep_model_evaluation_metrics.R")
source("scripts/gep/cores/gep_evaluation_core_mfs.R")
source("scripts/gep/cores/gep_evaluation_core_mss.R")
source("scripts/gep/diagnostics/gep_data_diagnostics.R")
source("scripts/gep/visualization/gep_visuals.R")
source("scripts/gep/reporting/gep_reporting.R")
source("scripts/gep/simple/gep_simple_validation.R")
source("scripts/gep/orchestration/gep_evaluation_orchestration.R")
source("scripts/gep/utils/gep_variable_checks.R")

# Source the forest plot script
source("scripts/visualization/forest_plot.R")

# Source the forest plot diagnostics script
source("scripts/utils/forest_plot_diagnostics.R")

# Workflow scripts
source("scripts/workflow/analysis_orchestration.R")

# Objective scripts
source("scripts/workflow/objective_0_data_processing.R")
source("scripts/workflow/objective_1_primary_outcomes.R")
source("scripts/workflow/objective_2_safety_toxicity.R")
source("scripts/workflow/objective_3_repeat_radiation.R")
source("scripts/workflow/objective_4_gep_analysis.R")

######################################################################
############### CREATE NECESSARY DIRECTORIES ##########################
######################################################################

# Create necessary directories now that libraries are loaded
dir.create(PROCESSED_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)


######################################################################
############### CENTRALIZED OUTPUT DIRECTORY MANAGEMENT ###############
######################################################################

#' Create standardized output directory structure and prefix for a cohort
#'
#' This function centralizes the creation of output directories and prefix generation
#' to ensure consistency across all analyses and prevent directory creation issues.
#'
#' @param dataset_name Character string of the dataset name (e.g., "uveal_melanoma_full_cohort")
#' @param cohort_dir_name Character string for the cohort directory name (e.g., "uveal_full")
#' @return List containing:
#'   - prefix: Character string prefix for file naming
#'   - cohort_base_dir: Character string path to the cohort base directory
#'   - output_dirs: Named list of all output directories for the cohort
#'
#' @examples
#' setup_cohort_outputs("uveal_melanoma_full_cohort", "uveal_full")
setup_cohort_outputs <- function(dataset_name, cohort_dir_name = NULL) {
    
    # Generate prefix based on dataset name
    prefix <- case_when(
        grepl("full", dataset_name) ~ "full_cohort_",
        grepl("restricted", dataset_name) ~ "restricted_cohort_", 
        grepl("gksrs", dataset_name) ~ "gksrs_only_cohort_",
        TRUE ~ paste0(dataset_name, "_")
    )
    
    # Determine cohort directory name if not provided
    if (is.null(cohort_dir_name)) {
        cohort_dir_name <- case_when(
            grepl("full", dataset_name) ~ "uveal_full",
            grepl("restricted", dataset_name) ~ "uveal_restricted", 
            grepl("gksrs", dataset_name) ~ "gksrs",
            TRUE ~ dataset_name
        )
    }
    
    # Create cohort base directory
    cohort_base_dir <- file.path("final_data/Analysis", cohort_dir_name)
    
    # Create the complete directory structure
    output_dirs <- create_output_structure(cohort_base_dir)
    
    # Log the setup
    log_enhanced(sprintf("Created output structure for %s", dataset_name), level = "INFO")
    log_enhanced(sprintf("Prefix: %s", prefix), level = "INFO", indent = 1)
    log_enhanced(sprintf("Base directory: %s", cohort_base_dir), level = "INFO", indent = 1)
    
    return(list(
        prefix = prefix,
        cohort_base_dir = cohort_base_dir,
        output_dirs = output_dirs
    ))
}

#' Validate naming consistency between dataset name, prefix, and cohort directory
#'
#' This function ensures that the naming conventions are consistent across
#' the analysis pipeline to prevent bugs and confusion.
#'
#' @param dataset_name Character string of the dataset name
#' @param prefix Character string prefix for file naming
#' @param cohort_dir_name Character string for the cohort directory name
#' @return Logical TRUE if consistent, FALSE otherwise
#'
#' @examples
#' validate_naming_consistency("uveal_melanoma_full_cohort", "full_cohort_", "uveal_full")
validate_naming_consistency <- function(dataset_name, prefix, cohort_dir_name) {
    
    # Check prefix consistency
    prefix_expected <- case_when(
        grepl("full", dataset_name) ~ "full_cohort_",
        grepl("restricted", dataset_name) ~ "restricted_cohort_", 
        grepl("gksrs", dataset_name) ~ "gksrs_only_cohort_",
        TRUE ~ paste0(dataset_name, "_")
    )
    
    # Check cohort directory name consistency
    cohort_dir_expected <- case_when(
        grepl("full", dataset_name) ~ "uveal_full",
        grepl("restricted", dataset_name) ~ "uveal_restricted", 
        grepl("gksrs", dataset_name) ~ "gksrs",
        TRUE ~ dataset_name
    )
    
    # Validate
    prefix_consistent <- prefix == prefix_expected
    cohort_dir_consistent <- cohort_dir_name == cohort_dir_expected
    
    if (!prefix_consistent) {
        log_enhanced(sprintf("PREFIX INCONSISTENCY: Expected '%s', got '%s'", prefix_expected, prefix), level = "ERROR")
    }
    
    if (!cohort_dir_consistent) {
        log_enhanced(sprintf("COHORT DIR INCONSISTENCY: Expected '%s', got '%s'", cohort_dir_expected, cohort_dir_name), level = "ERROR")
    }
    
    return(prefix_consistent && cohort_dir_consistent)
}

# Convert display labels to filename-safe strings
#' Convert display label to filename-safe string
#'
#' Converts display labels like "Overall Survival Probability" to filename-safe
#' strings like "overall_survival_probability" for use in file paths.
#'
#' @param label Character string with display label
#' @return Character string safe for use in filenames
#' @examples
#' make_filename_safe("Overall Survival Probability")  # "overall_survival_probability"
#' make_filename_safe("Progression-Free Survival")     # "progression_free_survival"
make_filename_safe <- function(label) {
    if (is.null(label) || is.na(label)) {
        return("unknown")
    }
    
    # Convert to lowercase, replace spaces and special characters with underscores
    safe_name <- label %>%
        tolower() %>%
        gsub("[^a-z0-9]", "_", .) %>%  # Replace non-alphanumeric with underscore
        gsub("_+", "_", .) %>%         # Replace multiple underscores with single
        gsub("^_|_$", "", .)           # Remove leading/trailing underscores
    
    return(safe_name)
}



# Set default prefix if not exists
if (!exists("prefix")) {
    prefix <- "test_"
}