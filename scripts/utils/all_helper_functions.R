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

# Data wrangling & core utilities
use("tidyverse") # For data manipulation and visualization (dplyr, ggplot2, etc.)
use("readxl") # For reading Excel files
use("writexl") # For writing Excel files
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

# CONSORT diagram creation and export
use("DiagrammeR") # Create CONSORT diagram
use("DiagrammeRsvg") # SVG export for CONSORT diagram
use("rsvg") # PNG conversion for CONSORT diagram

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

# For installation of all required libraries, run the following command:
# Requires R version 4.4.0 or higher
# install.packages(c("tidyverse", "readxl", "writexl", "lubridate", "gtsummary", "janitor", "survival", "survminer", "gt", "cardx", "forestploter", "grid", "cowplot", "DiagrammeR", "DiagrammeRsvg", "rsvg", "survRM2", "rms", "pec", "survcomp", "riskRegression", "cmprsk", "pROC", "rmda", "VIM", "mice"))
# if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager"); BiocManager::install("survcomp")

# Source the analysis configuration first (contains all global variables)
source("scripts/utils/analysis_config.R")

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
