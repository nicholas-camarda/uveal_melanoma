# Configuration Constants
# Author: Nicholas Camarda
# Description: All configuration constants and settings for the analysis

# Note: All required libraries are loaded in main.R

# Set consistent contrast options for all modeling functions
# This ensures factor variables use consistent naming across all models
options(contrasts = c("contr.treatment", "contr.poly"))

# =============================================================================
# CORE DATA PATHS AND DIRECTORIES
# =============================================================================
DATA_DIR <- "final_data"
RAW_DATA_DIR <- file.path(DATA_DIR, "Original Files")
PROCESSED_DATA_DIR <- file.path(DATA_DIR, "Analytic Dataset")
OUTPUT_DIR <- file.path(DATA_DIR, "Analysis")
ANALYSIS_DIR <- OUTPUT_DIR  # Alias for consistency with legacy code

# =============================================================================
# DATA PROCESSING CONSTANTS
# =============================================================================
# Minimum number of observations required to keep a category
THRESHOLD_RARITY <- 5

# Verbose logging flag
VERBOSE <- TRUE

# Threshold for extreme regression estimates (odds ratios, hazard ratios, etc.)
# Estimates above this threshold will be excluded from tables and documented in diagnostics
EXTREME_ESTIMATE_THRESHOLD <- 100

# Threshold for CI width filtering - any CI wider than this will be filtered out
# This prevents infinite CIs and extremely wide intervals from appearing in tables
CI_WIDTH_THRESHOLD <- 10

# Threshold for near-perfect separation detection
# CI upper bound very close to 0 indicates near-perfect separation
NEAR_PERFECT_SEPARATION_THRESHOLD <- 0.001

# Threshold for extremely wide CI detection
# CI upper/lower ratio above this indicates unreliable estimates
EXTREMELY_WIDE_CI_THRESHOLD <- 1000

# Input file and exclusion settings
INPUT_FILENAME <- "Ocular Melanoma Master Spreadsheet REVISED FOR STATS (5-10-25, TJM).xlsx"
SPECIFIC_PATIENTS_TO_EXCLUDE <- c(271) # Patient 271: all supporting documentation was lost

# Tumor size thresholds for cohort eligibility
TUMOR_HEIGHT_THRESHOLD <- 10           # mm
TUMOR_DIAMETER_THRESHOLD <- 20         # mm

# Time conversion constants
DAYS_IN_YEAR <- 365.25
DAYS_IN_MONTH <- 30.44
FOLLOW_UP_YEARS <- 5                   # For 5-year outcomes
UNITS_OF_TIME <- "months"              # "days" or "months" or "years"

# =============================================================================
# ANALYSIS VARIABLES AND CONFOUNDERS
# =============================================================================

# Define confounders for adjustment in all models
confounders <- c(
    "age_at_diagnosis", "sex", "location"
    # "internal_reflectivity",
    # "srf", "flashes_photopsia", "floaters",
    # "initial_overall_stage_modified", "initial_t_stage",
    # "optic_nerve"
)

# NOTE: when initial_overall_stage_modified is in confounders, the factor label p-value calculation fails due to:
# 1. Perfect separation issues in the model
# 2. Likelihood ratio test failure: "models were not all fitted to the same size of dataset"
# 3. Even Firth's bias-reduced logistic regression fails due to perfect separation
# 4. Individual coefficient p-values (2A, 2B, 3A) are valid and may be used for significance assessment but unclear how to do this
# 5. Factor label p-value will show as NA in diagnostic files - this appears to be correct behavior

# Define subgroup variables for analysis
subgroup_vars <- c(
    "age_at_diagnosis", "sex", "location", "initial_t_stage",  
    "initial_tumor_height", "initial_tumor_diameter", 
    "initial_overall_stage_modified", "biopsy1_gep", "optic_nerve"
)

# Define which subgroup variables are continuous and need binning
continuous_subgroup_vars <- c("age_at_diagnosis", "initial_tumor_height", "initial_tumor_diameter")

# =============================================================================
# SUBGROUP ANALYSIS CONFIGURATION
# =============================================================================

# Standardized cutoffs (when USE_STANDARDIZED_CUTOFFS = TRUE)
# Updated to use T-stage clinical cutoffs from AJCC staging system
STANDARDIZED_CUTOFFS <- list(
    age_at_diagnosis = 65.0,  # Keep age cutoff as is
    # T-stage clinical cutoffs for tumor height (mm)
    initial_tumor_height = c(3.0, 6.0, 9.0, 12.0, 15.0),  # Creates bins: ≤3.0, 3.1-6.0, 6.1-9.0, 9.1-12.0, 12.1-15.0, >15.0
    # T-stage clinical cutoffs for tumor diameter (mm) 
    initial_tumor_diameter = c(3.0, 6.0, 9.0, 12.0, 15.0, 18.0)  # Creates bins: ≤3.0, 3.1-6.0, 6.1-9.0, 9.1-12.0, 12.1-15.0, 15.1-18.0, >18.0
)

# =============================================================================
# TREATMENT AND FACTOR LEVEL CONFIGURATION
# =============================================================================
# CRITICAL: These variables define factor levels and reference groups used throughout
# the entire analysis pipeline. Changing these affects all models, tables, and plots.

# Treatment group configuration
TREATMENT_LABELS <- c("GKSRS", "Plaque")                    # For display/plotting
FAVOURS_LABELS <- c("Favours GKSRS", "Favours Plaque")      # For forest plot labels

# Treatment factor levels (CRITICAL: Order determines reference group)
# Reference group = FIRST level (used in regression models)
# All models will compare TREATMENT_FACTOR_LEVELS[2] vs TREATMENT_FACTOR_LEVELS[1]
TREATMENT_FACTOR_LEVELS <- c("Plaque", "GKSRS")  # Plaque is reference group
TREATMENT_REFERENCE_LEVEL <- TREATMENT_FACTOR_LEVELS[1]  # Explicitly define reference
TREATMENT_COMPARISON_LEVEL <- TREATMENT_FACTOR_LEVELS[2]  # Explicitly define comparison

# Validation: Ensure consistency with TREATMENT_LABELS
if (!all(TREATMENT_LABELS %in% TREATMENT_FACTOR_LEVELS)) {
    stop(sprintf("CRITICAL ERROR: TREATMENT_LABELS (%s) must match TREATMENT_FACTOR_LEVELS (%s)", 
                 paste(TREATMENT_LABELS, collapse = ", "), 
                 paste(TREATMENT_FACTOR_LEVELS, collapse = ", ")))
}

# Binary factor configurations (used for ALL Y/N binary variables)
# N is ALWAYS the reference level (first), Y is comparison (second)
YN_RAW_LEVELS <- c("N", "Y")
YN_DISPLAY_LABELS <- c("No", "Yes")

# Other critical factor levels
SEX_FACTOR_LEVELS <- c("Female", "Male")

# =============================================================================
# PLOT AND VISUALIZATION SETTINGS
# =============================================================================
# Plot dimensions and settings for all output figures
FOREST_PLOT_WIDTH <- 10    # inches (reasonable width)
FOREST_PLOT_HEIGHT <- 12   # inches (increased height for all subgroup levels)
SURVIVAL_PLOT_WIDTH <- 10  # inches  
SURVIVAL_PLOT_HEIGHT <- 8  # inches
RMST_PLOT_WIDTH <- 10      # inches
RMST_PLOT_HEIGHT <- 6      # inches
PLOT_DPI <- 300           # resolution
PLOT_UNITS <- "in"        # units

# =============================================================================
# STAGE AND COHORT CONFIGURATION
# =============================================================================
# Stage exclusion configuration for modified overall stage variable
# Data-driven analysis showed Stage 3B (n=6, 2.3%), Stage 3C (n=1, 0.4%), and Stage 4 (n=3, 1.1%) 
# have insufficient patient numbers for reliable statistical analysis. Chi-square test confirmed 
# significant difference in stage distribution between treatment groups (p=0.0008), indicating stage 
# should be included as a confounder. Modified variable excludes problematic stages while 
# preserving the confounding adjustment for stages with adequate sample sizes.
STAGES_TO_EXCLUDE_FROM_MODIFIED <- c("3B", "3C", "4")

# =============================================================================
# GEP VALIDATION CONFIGURATION (OBJECTIVE 4)
# =============================================================================
# Core GEP validation settings
GEP_VALIDATION_TIMEPOINTS <- c(5, 7, 10)  # years for validation analysis
GEP_BOOTSTRAP_ITERATIONS <- 200           # bootstrap samples for optimism correction

# =============================================================================
# GEP VALIDATION CONFIGURATION (OBJECTIVE 4) - DETAILED SETTINGS
# =============================================================================
# These constants control various aspects of the GEP validation analysis
# Modify here to change validation behavior across all functions

# PRAME augmentation constants
GEP_PRAME_ADJUSTMENT_FACTOR <- 1.3    # 30% increase in risk for PRAME positive patients
GEP_PRAME_REDUCTION_FACTOR <- 0.9     # 10% decrease in risk for PRAME negative patients  
GEP_RISK_CAP_MAXIMUM <- 0.95          # Maximum allowed risk prediction (cap at 95%)

# Risk stratification cutoffs for NRI analysis
GEP_RISK_CUTOFFS <- c(0, 0.1, 0.3, 1.0)  # Risk categories: <10%, 10-30%, >30%
GEP_RISK_LABELS <- c("Low", "Intermediate", "High")

# Decision curve analysis thresholds
GEP_DCA_THRESHOLD_MIN <- 0.01          # Minimum risk threshold (1%)
GEP_DCA_THRESHOLD_MAX <- 0.50          # Maximum risk threshold (50%)
GEP_DCA_THRESHOLD_STEP <- 0.01         # Step size for threshold sequence

# Data cleaning and validation bounds
GEP_MAX_FOLLOWUP_YEARS <- 50           # Maximum reasonable follow-up time in years
GEP_MIN_FOLLOWUP_YEARS <- 0.01         # Minimum follow-up time in years
GEP_MIN_RISK_PREDICTION <- 0.001       # Minimum allowed risk prediction (avoid zero)
GEP_MAX_RISK_PREDICTION <- 0.999       # Maximum allowed risk prediction (avoid perfect)

# Calibration analysis constants
GEP_MIN_GROUP_SIZE <- 5                # Minimum patients per calibration group
GEP_DEFAULT_N_GROUPS <- 10             # Default number of calibration groups
GEP_MIN_N_GROUPS <- 3                  # Minimum number of calibration groups
GEP_LOESS_SPAN <- 0.3                  # Smoothing parameter for loess calibration curves

# Sample size requirements for analysis
GEP_MIN_SAMPLE_SIZE <- 20              # Minimum sample size for any analysis
GEP_MIN_EVENTS_COMPETING_RISK <- 5     # Minimum events for competing risk analysis
GEP_MIN_BOOTSTRAP_SAMPLE <- 30         # Minimum sample size for bootstrap analysis
GEP_MAX_BOOTSTRAP_ITERATIONS <- 100    # Maximum bootstrap iterations for speed

# Missing data analysis constants
GEP_MISSING_DATA_THRESHOLD <- 10       # Minimum patients needed for missing data analysis

# Validation metrics thresholds (for warnings/interpretation)
GEP_RECOMMENDED_VALIDATION_SAMPLE <- 100  # Recommended minimum for robust validation
GEP_RECOMMENDED_TESTING_SAMPLE <- 30      # Recommended minimum for testing set

# =============================================================================
# TABLE AND OUTPUT CONFIGURATION
# =============================================================================
# Define consistent variable order for forest plots and subgroup analysis
# This ensures all plots and tables show variables in the same order across cohorts
# Used by main.R, forest plot functions, and subgroup analysis to maintain consistency
# To change the order of variables in all outputs, modify this single variable
FOREST_PLOT_VARIABLE_ORDER <- c(
    "age_at_diagnosis", "sex", "location", "initial_t_stage",
    "initial_tumor_height", "initial_tumor_diameter", "biopsy1_gep", "optic_nerve"
)

# Define variables for baseline characteristics summary tables
# Used by create_summary_tables() and merge_cohort_tables() to ensure consistency
BASELINE_VARIABLES_TO_SUMMARIZE <- c(
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

# =============================================================================
# TABLE LABELS AND NAMING CONVENTIONS
# =============================================================================
# Centralized table labels to ensure consistency across all gtsummary tables
# These should match the labels used in data_processing.R baseline tables
STANDARD_TABLE_LABELS <- list(
    # Demographics
    age_at_diagnosis = "Age at Diagnosis (years)",
    race = "Race",
    sex = "Sex", 
    eye = "Eye",
    
    # Vision and measurements
    initial_vision = "Initial Visual Acuity (logMAR)",
    
    # Tumor characteristics
    location = "Tumor Location",
    optic_nerve = "Optic Nerve Involvement",
    initial_tumor_height = "Initial Tumor Height (mm)",
    initial_tumor_diameter = "Initial Tumor Diameter (mm)",
    internal_reflectivity = "Internal Reflectivity",
    srf = "Subretinal Fluid (SRF)",
    op = "Orange Pigment",
    
    # Symptoms
    symptoms = "Any Symptoms",
    vision_loss_blurred_vision = "Vision Loss/Blurred Vision",
    visual_field_defect = "Visual Field Defect",
    flashes_photopsia = "Flashes/Photopsia",
    floaters = "Floaters",
    pain = "Pain",
    
    # Staging
    initial_overall_stage = "Overall Stage",
    initial_overall_stage_modified = "Overall Stage (Modified)",
    initial_t_stage = "T Stage",
    initial_n_stage = "N Stage", 
    initial_m_stage = "M Stage",
    initial_mets = "Initial Metastases",
    biopsy1_gep = "Gene Expression Profile",
    gep_class_simple = "GEP Class",
    prame_status = "PRAME Status",
    expected_mfs_5yr = "Expected 5-Year MFS", # MFS = Metastasis-Free Survival
    expected_mfs_7yr = "Expected 7-Year MFS",
    expected_mfs_10yr = "Expected 10-Year MFS",
    expected_mss_5yr = "Expected 5-Year MSS", # MSS = Melanoma-Specific Survival
    expected_mss_7yr = "Expected 7-Year MSS",
    expected_mss_10yr = "Expected 10-Year MSS",
    
    # Treatment
    treatment_group = "Treatment Group",
    recurrence1_treatment_clean = "Recurrence Treatment",
    
    # Outcomes
    recurrence1 = "Local Recurrence",
    recurrence2 = "Second Recurrence",
    mets_progression = "Metastatic Progression",
    enucleation = "Enucleation",
    retinopathy = "Radiation Retinopathy",
    nvg = "Neovascular Glaucoma",
    srd = "Serous Retinal Detachment",
    
    # Changes/follow-up
    height_change = "Tumor Height Change (mm)",
    vision_change = "Visual Acuity Change (logMAR)",
    follow_up_years = "Follow-up Time (years)",
    follow_up_months = "Follow-up Time (months)",
    
    # PFS-2 specific
    tt_pfs2_months = "PFS-2 Time (months)", 
    pfs2_event = "Second Recurrence Events"
) 