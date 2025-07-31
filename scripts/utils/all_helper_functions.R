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

# Set to TRUE to show all individual p-values in regression tables
# Set to FALSE to show only grouped p-values (one per variable group)
SHOW_ALL_PVALUES <- TRUE

# TOGGLE: Switch between standardized vs median cutoffs
USE_STANDARDIZED_CUTOFFS <- TRUE

# Toggle to control whether to create subgroup tables
CREATE_SUBGROUP_TABLES <- TRUE

######################################################################
############### LOAD / INSTALL REQUIRED LIBRARIES ####################
######################################################################
use <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        # Try CRAN first
        tryCatch(
            install.packages(pkg),
            error = function(e) {
                message(sprintf("→ %s not on CRAN, trying Bioconductor…", pkg))
                if (!requireNamespace("BiocManager", quietly = TRUE)) {
                    install.packages("BiocManager")
                }
                BiocManager::install(pkg, ask = FALSE, update = FALSE)
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

# Source the analysis configuration first (contains all global variables)
source("scripts/utils/analysis_config.R")

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
source("scripts/analysis/gep_validation_analysis.R")
# Primary outcomes subgroup analysis functions now in subgroup_analysis.R

# Source the forest plot script
source("scripts/visualization/forest_plot.R")

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

# Set up global variables needed for analysis functions
# These are normally set in main.R but needed for individual testing

# Create default output directories structure
if (!exists("output_dirs")) {
    output_dirs <- list(
        baseline_characteristics = "test_output/baseline",
        obj1_recurrence = "test_output/recurrence", 
        obj1_mets = "test_output/mets",
        obj1_os = "test_output/os",
        obj1_ph_diagnostics = "test_output/ph_diagnostics",
        obj3_pfs2 = "test_output/pfs2",
        obj3_ph_diagnostics = "test_output/pfs2_ph_diagnostics"
    )
    
    # Create directories if they don't exist
    for (dir_path in output_dirs) {
        if (!dir.exists(dir_path)) {
            dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
        }
    }
}

# Set default prefix if not exists
if (!exists("prefix")) {
    prefix <- "test_"
}