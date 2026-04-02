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
USE_CLINICAL_BINNING_CONTINUOUS <- TRUE # DEFAULT: Use clinical thresholds for tumor height (10mm) and diameter (20mm)

######################################################################
############### LOAD / INSTALL REQUIRED LIBRARIES ####################
######################################################################
#' Require an Installed Package and Load It
#'
#' Fails fast with a clear bootstrap instruction when a required package is
#' missing, and otherwise loads it with startup messages suppressed.
#'
#' @param pkg Character package name
#' @return Invisibly loads the package into the session
use <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(
            sprintf(
                "Required package '%s' is not installed. Run `Rscript scripts/bootstrap_packages.R` before sourcing load_all.R.",
                pkg
            ),
            call. = FALSE
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
use("gridExtra") # Table grobs for inset RMST summaries
use("cowplot") # Combining ggplots
use("ggsurvfit") # For cumulative incidence plots
use("tidycmprsk") # For cumulative incidence plots

# Logging and progress
use("logger")
use("progressr")

# Testing
use("here") # For finding project root
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
use("timeROC") # Time-dependent ROC analysis
use("pROC") # ROC analysis
use("rmda") # Risk-model decision analysis
use("VIM") # Visualization & imputation of missing values
use("mice") # Multiple imputation by chained equations
use("glmnet") # Penalized generalized linear models for exploratory no-GEP models

######################################################################
############### SOURCE ALL NECESSARY SCRIPTS #########################
######################################################################

# Source the split configuration and utility files
source(here("scripts", "utils", "config_constants.R"))
source(here("scripts", "tools", "factor_level_audit.R"))
source(here("scripts", "utils", "output_utilities.R"))
source(here("scripts", "utils", "markdown_utilities.R"))
source(here("scripts", "utils", "logging_utilities.R"))
source(here("scripts", "utils", "validation_reporting.R"))
source(here("scripts", "utils", "validation_utilities.R"))
source(here("scripts", "utils", "objective0_validation_engine.R"))
source(here("scripts", "utils", "model_utilities.R"))
source(here("scripts", "utils", "color_palettes.R"))
source(here("scripts", "utils", "cohort_summary_export.R"))
source(here("scripts", "utils", "vision_helpers.R"))
source(here("scripts", "utils", "plot_utilities.R"))
 
 # Source the extreme estimate handling utilities
source(here("scripts", "utils", "extreme_estimate_handling.R"))

# Source the forest plot diagnostics script
source(here("scripts", "utils", "forest_plot_diagnostics.R"))

# Source the table generation utilities (modularized)
source(here("scripts", "tables", "table_generation_core.R"))
source(here("scripts", "tables", "table_model_fitting.R"))
source(here("scripts", "tables", "table_formatting.R"))
source(here("scripts", "tables", "table_diagnostics.R"))
source(here("scripts", "tables", "table_io.R"))
source(here("scripts", "tools", "tool_runtime_helpers.R"))
source(here("scripts", "tools", "effect_summary_audit.R"))
source(here("scripts", "tools", "study_doc_generators.R"))

# Source the data processing modules (modularized)
source(here("scripts", "data_helper", "data_loading.R"))
source(here("scripts", "data_helper", "data_derivation.R"))
source(here("scripts", "data_helper", "cohort_creation.R"))
source(here("scripts", "data_helper", "data_summaries.R"))
source(here("scripts", "data_helper", "cohort_orchestration.R"))

# Source the utility and helper scripts
source(here("scripts", "data_helper", "data_utilities.R"))

# Source the main analysis function scripts
source(here("scripts", "analysis", "tumor_height_analysis.R"))
source(here("scripts", "analysis", "vision_safety_analysis.R"))
# Source modular statistical analysis files
source(here("scripts", "analysis", "binary_outcomes.R"))
source(here("scripts", "analysis", "survival_outcomes.R"))
source(here("scripts", "analysis", "rmst_visualization.R"))

# Source modular subgroup analysis files
source(here("scripts", "subgroup", "subgroup_data_prep.R"))
source(here("scripts", "subgroup", "subgroup_survival.R"))
source(here("scripts", "subgroup", "subgroup_binary.R"))
source(here("scripts", "subgroup", "subgroup_height.R"))
source(here("scripts", "subgroup", "subgroup_formatting.R"))

# Source GEP evaluation modules
source(here("scripts", "gep", "utils", "gep_model_evaluation_metrics.R"))
source(here("scripts", "gep", "utils", "gep_excel_output.R"))
source(here("scripts", "gep", "utils", "gep_extrapolation_assumptions.R"))
source(here("scripts", "gep", "cores", "gep_evaluation_core_mfs.R"))
source(here("scripts", "gep", "cores", "gep_evaluation_core_mss.R"))
source(here("scripts", "gep", "reporting", "gep_mfs_sensitivity_reporting.R"))
source(here("scripts", "gep", "visualization", "gep_visuals.R"))

# Source the GEP reporting script
source(here("scripts", "gep", "reporting", "gep_reporting_core.R"))
source(here("scripts", "gep", "reporting", "gep_table_creation.R"))
source(here("scripts", "gep", "reporting", "gep_summary_generation.R"))
source(here("scripts", "gep", "reporting", "gep_clinical_interpretation.R"))
source(here("scripts", "gep", "reporting", "gep_output_consolidation.R"))
# Source the GEP simple validation script
source(here("scripts", "gep", "reporting", "gep_simple_validation.R"))
# Source the GEP evaluation orchestration script
source(here("scripts", "gep", "orchestration", "gep_evaluation_orchestration.R"))

# Source the GEP exploratory analysis script 
source(here("scripts", "gep", "orchestration", "gep_exploratory_no_gep_report.R"))

# Source the forest plot script (commented out to use modular version)
# source(here("scripts", "visualization", "forest_plot.R"))
# Source modular forest plot helpers
source(here("scripts", "visualization", "forest_plot_data.R"))
source(here("scripts", "visualization", "forest_plot_draw.R"))
source(here("scripts", "visualization", "forest_plot_formatting.R"))

# Workflow scripts
source(here("scripts", "workflow", "analysis_orchestration.R"))
source(here("scripts", "workflow", "publish_outputs.R"))

# Objective scripts
source(here("scripts", "workflow", "objective_0_data_processing.R"))
source(here("scripts", "workflow", "objective_1_primary_outcomes.R"))
source(here("scripts", "workflow", "objective_2_safety_toxicity.R"))
source(here("scripts", "workflow", "objective_3_repeat_radiation.R"))
source(here("scripts", "workflow", "objective_4_gep_analysis.R"))

# Set seed for reproducibility
set.seed(123)

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
    cohort_base_dir <- file.path(OUTPUT_DIR, cohort_dir_name)

    # Create the complete directory structure
    output_dirs <- create_output_structure(cohort_base_dir)

    # Log the setup
            logger::log_info(sprintf("Created output structure for %s", dataset_name))
            logger::log_info(formatted(sprintf("Prefix: %s", prefix), indent = 1))
            logger::log_info(formatted(sprintf("Base directory: %s", cohort_base_dir), indent = 1))

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
        logger::log_error(sprintf("PREFIX INCONSISTENCY: Expected '%s', got '%s'", prefix_expected, prefix))
    }

    if (!cohort_dir_consistent) {
        logger::log_error(sprintf("COHORT DIR INCONSISTENCY: Expected '%s', got '%s'", cohort_dir_expected, cohort_dir_name))
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
#' make_filename_safe("Overall Survival Probability") # "overall_survival_probability"
#' make_filename_safe("Progression-Free Survival") # "progression_free_survival"
make_filename_safe <- function(label) {
    if (is.null(label) || is.na(label)) {
        return("unknown")
    }

    # Convert to lowercase, replace spaces and special characters with underscores
    safe_name <- label %>%
        tolower() %>%
        gsub("[^a-z0-9]", "_", .) %>% # Replace non-alphanumeric with underscore
        gsub("_+", "_", .) %>% # Replace multiple underscores with single
        gsub("^_|_$", "", .) # Remove leading/trailing underscores

    return(safe_name)
}

# Required packages for GEP analysis
required_packages <- c(
    "survival", "survminer", "cmprsk", "riskRegression", "survcomp", "timeROC", "pROC",
    "dplyr", "ggplot2", "gridExtra", "writexl", "readxl", "knitr", "kableExtra",
    "gt", "gtsummary", "broom", "car", "lmtest", "sandwich", "MASS", "pscl",
    "logistf", "mice", "VIM", "naniar", "corrplot", "RColorBrewer", "scales",
    "stringr", "forcats", "tidyr", "purrr", "readr", "tibble", "magrittr",
    "rmarkdown", "DT", "plotly", "shiny", "shinydashboard", "flexdashboard"
)

# Source the GEP data diagnostics script
source(here("scripts", "data_helper", "gep_missing_data_analysis.R"))
