# Configuration Constants - Central Repository for ALL Analysis Settings
# Author: Nicholas Camarda
# Description: This file contains ALL constants, thresholds, and configuration settings
#             used throughout the entire analysis pipeline. This is the SINGLE source
#             of truth for all analysis parameters. DO NOT add constants elsewhere.
#
# CONTENTS:
# - File paths and directory structures
# - Data processing thresholds and settings
# - Statistical analysis parameters (confounders, subgroups)
# - Treatment group factor levels and labels
# - Plot dimensions and visualization settings
# - GEP validation configuration (Objective 4)
# - Variable ordering and labeling conventions
# - Subgroup analysis cutoffs and settings
# - Data validation thresholds and requirements

# CRITICAL: Set consistent contrast options for ALL modeling functions
# This ensures factor variables use consistent naming across all models:
# - "contr.treatment" = treatment contrasts (first level is reference)
# - "contr.poly" = polynomial contrasts for ordered factors
# This prevents inconsistent coefficient names between models
options(contrasts = c("contr.treatment", "contr.poly"))

# =============================================================================
# CORE DATA PATHS AND DIRECTORIES
# =============================================================================
# CRITICAL: These paths define the entire data structure for the analysis
# - DATA_DIR: Root directory containing all data files
# - RAW_DATA_DIR: Original Excel files from clinical database
# - PROCESSED_DATA_DIR: Cleaned and processed analytic datasets
# - OUTPUT_DIR: All analysis results, tables, plots, and diagnostics
DATA_DIR <- here("final_data")
RAW_DATA_DIR <- here(DATA_DIR, "Original Files")
PROCESSED_DATA_DIR <- here(DATA_DIR, "Analytic Dataset")
OUTPUT_DIR <- here(DATA_DIR, "Analysis")

# =============================================================================
# TOOL PATHS AND CONFIGURATION
# =============================================================================
TOOLS_OUTPUT_DIR <- here(PROCESSED_DATA_DIR, "tools_output")
DATA_DICTIONARY_PATH <- here(RAW_DATA_DIR, "Data Dictionary.xlsx")

# =============================================================================
# LOGGING AND OUTPUT PATHS
# =============================================================================
LOGS_DIR <- here("logs")
MERGED_TABLES_DIR <- here(OUTPUT_DIR, "merged_tables")
TEST_OUTPUT_DIR <- here("test_output")

# =============================================================================
# DATA PROCESSING CONSTANTS
# =============================================================================
# CRITICAL: These thresholds control data quality and analysis decisions
# - THRESHOLD_RARITY: Minimum observations to keep a category (prevents sparse data)
# - EXTREME_ESTIMATE_THRESHOLD: Maximum allowed odds/hazard ratios (filters unreliable estimates)
# - CI_WIDTH_THRESHOLD: Maximum confidence interval width (filters extremely wide CIs)
# - TUMOR_SIZE_THRESHOLDS: Clinical criteria for treatment eligibility

# Input file and exclusion settings
INPUT_FILENAME <- "Ocular Melanoma Master Spreadsheet REVISED FOR STATS (5-10-25, TJM).xlsx"
SPECIFIC_PATIENTS_TO_EXCLUDE <- c(271) # Patient 271: all supporting documentation was lost

# Data quality thresholds
THRESHOLD_RARITY <- 5L # Minimum observations to keep a category
EXTREME_ESTIMATE_THRESHOLD <- 100 # Maximum allowed odds/hazard ratios
CI_WIDTH_THRESHOLD <- 1000 # Maximum confidence interval width (raw difference) - DEPRECATED
EXPONENTIATED_CI_THRESHOLD <- 100 # Maximum CI width for exponentiated values (ORs, HRs)
LOG_SCALE_CI_THRESHOLD <- 10 # Maximum CI width for log scale values (log-odds, log-hazards)
NEAR_PERFECT_SEPARATION_THRESHOLD <- 0.001 # Threshold for near-perfect separation detection (exponentiated scale)
LOG_SCALE_NEAR_PERFECT_SEPARATION_THRESHOLD <- 0.1 # Threshold for near-perfect separation detection (log scale)

# Model feasibility thresholds
# CRITICAL: These thresholds control whether model families are fit or only summarized.
# Keep them model-specific so analysis policy stays explicit and documents stay aligned.
MINIMUM_ADJUSTED_LOGISTIC_EVENTS <- 10L # Minimum outcome events for adjusted binary logistic regression
MINIMUM_SURVIVAL_EVENTS <- 5L # Minimum outcome events for survival analysis
MINIMUM_PFS2_PATIENTS <- 10L # Minimum analyzable patients before attempting PFS-2 modeling
MINIMUM_PH_TEST_EVENTS <- 10L # Minimum events for proportional-hazards diagnostics/reporting

# Tumor size thresholds for cohort eligibility
TUMOR_HEIGHT_THRESHOLD <- 10 # mm
TUMOR_DIAMETER_THRESHOLD <- 20 # mm

# Time conversion constants
DAYS_IN_YEAR <- 365.25
DAYS_IN_MONTH <- 30.44
FOLLOW_UP_YEARS <- 5 # For 5-year outcomes
UNITS_OF_TIME <- "months" # "days" or "months" or "years"
VITAL_STATUS_DATA_CUTOFF_DATE <- as.Date("2025-03-04")
LOST_TO_FOLLOWUP_CUTOFF_DAYS <- 450

# =============================================================================
# TREATMENT AND FACTOR LEVEL CONFIGURATION
# =============================================================================
# CRITICAL: These settings control ALL statistical models and factor levels
# - TREATMENT_FACTOR_LEVELS: Order determines reference group (PBT = reference, GKSRS = comparison)
# - TREATMENT_LABELS: Display labels for plots and tables
# - SEX_FACTOR_LEVELS: Female = reference, Male = comparison
# - YN_RAW_LEVELS: Binary variables (N = reference, Y = comparison)
# WARNING: Changing these affects ALL regression models, tables, and plots

# Treatment group configuration
TREATMENT_FACTOR_LEVELS <- c("PBT", "GKSRS") # PBT is reference group
TREATMENT_REFERENCE_LEVEL <- TREATMENT_FACTOR_LEVELS[1] # Explicitly define reference
TREATMENT_COMPARISON_LEVEL <- TREATMENT_FACTOR_LEVELS[2] # Explicitly define comparison
TREATMENT_LABELS <- c("PBT", "GKSRS") # For display/plotting (matches factor levels order)
FAVOURS_LABELS <- c("Favors PBT", "Favors GKSRS") # For forest plot labels (matches factor levels order)

# Validation: Ensure consistency with TREATMENT_LABELS
if (!all(TREATMENT_LABELS %in% TREATMENT_FACTOR_LEVELS)) {
    stop(sprintf(
        "CRITICAL ERROR: TREATMENT_LABELS (%s) must match TREATMENT_FACTOR_LEVELS (%s)",
        paste(TREATMENT_LABELS, collapse = ", "),
        paste(TREATMENT_FACTOR_LEVELS, collapse = ", ")
    ))
}

# Binary factor configurations (used for ALL Y/N binary variables)
# N is ALWAYS the reference level (first), Y is comparison (second)
YN_RAW_LEVELS <- c("N", "Y")
YN_DISPLAY_LABELS <- c("No", "Yes")

# Other critical factor levels
SEX_FACTOR_LEVELS <- c("Female", "Male")

# =============================================================================
# ANALYSIS VARIABLES AND CONFOUNDERS
# =============================================================================
# CRITICAL: These variables define the statistical models for ALL analyses
# - confounders: Variables adjusted for in ALL regression models (age, sex, location)
# - subgroup_vars: Variables used for subgroup analyses (age, sex, location, tumor features)
# - continuous_subgroup_vars: Variables that need binning for categorical analysis
# NOTE: Adding variables to confounders can cause perfect separation issues

# Define confounders for adjustment in all models
confounders <- c(
    "age_at_diagnosis_general_pop_median", "sex", "location"
    # "internal_reflectivity",
    # "srf", "flashes_photopsia", "floaters",
    # "initial_overall_stage", "initial_t_stage",
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
    "age_at_diagnosis_general_pop_median", "sex", "location", "initial_t_stage_simple",
    #"initial_t_stage",
    "initial_tumor_height", "initial_tumor_diameter",
    "initial_overall_stage", "biopsy1_gep", "gep_class_simple", "gep12_prame_status", "optic_nerve"
)

# Define which subgroup variables are continuous and need binning
continuous_subgroup_vars <- c("age_at_diagnosis", "initial_tumor_height", "initial_tumor_diameter")

# Define variables that are constant within specific cohorts and should be excluded from subgroup analysis
# These variables have no variation within the specified cohort and cannot be used for subgroup analysis
COHORT_CONSTANT_VARIABLES <- list(
    uveal_melanoma_restricted_cohort = c("optic_nerve"), # All restricted patients have optic_nerve == "N"
    uveal_melanoma_gksrs_only_cohort = c(), # No constant variables in GKSRS-only cohort
    uveal_melanoma_full_cohort = c() # No constant variables in full cohort
)

# =============================================================================
# STAGE AND COHORT CONFIGURATION
# =============================================================================
# CRITICAL: These settings control cohort eligibility and staging
# - STAGES_TO_EXCLUDE_FROM_MODIFIED: Stages with insufficient sample sizes (3B, 3C, 4)
# - TUMOR_SIZE_THRESHOLDS: Clinical criteria for treatment eligibility
# - SPECIFIC_PATIENTS_TO_EXCLUDE: Patients with missing documentation
# NOTE: Stage exclusions prevent perfect separation issues in statistical models
# Data-driven analysis showed Stage 3B (n=6, 2.3%), Stage 3C (n=1, 0.4%), and Stage 4 (n=3, 1.1%)
# have insufficient patient numbers for reliable statistical analysis. Chi-square test confirmed
# significant difference in stage distribution between treatment groups (p=0.0008), indicating stage
# should be included as a confounder. Modified variable excludes problematic stages while
# preserving the confounding adjustment for stages with adequate sample sizes.
STAGES_TO_EXCLUDE_FROM_MODIFIED <- c("3B", "3C", "4")

# Centralized model-layer level exclusions for sparse-factor handling
# These exclusions are applied only inside model-specific analysis copies.
MODELING_LEVEL_EXCLUSIONS <- list(
    initial_overall_stage = STAGES_TO_EXCLUDE_FROM_MODIFIED
)

# =============================================================================
# DATA VALIDATION THRESHOLDS AND REQUIREMENTS
# =============================================================================
# CRITICAL: These settings control data validation and quality checks
# - MINIMUM_COLUMNS_AFTER_PROCESSING: Minimum expected columns after data processing
# - CRITICAL_VARIABLES: Essential variables that must exist in the dataset
# - DERIVED_VARIABLES: Variables created during data processing
# - CRITICAL_FACTORS: Factor variables that must have proper levels
# - EXPECTED_COHORT_SIZES: Expected ranges for each cohort
# - MAXIMUM_MISSING_DATA_PERCENTAGE: Maximum allowed missing data for critical variables

# Data validation thresholds
MINIMUM_COLUMNS_AFTER_PROCESSING <- 150 # Minimum expected columns after data processing
MAXIMUM_MISSING_DATA_PERCENTAGE <- 50 # Maximum allowed missing data percentage for critical variables

# Critical variables that must exist in the dataset
CRITICAL_VARIABLES <- c(
    "id", "treatment_group", "age_at_diagnosis_binned", "age_at_diagnosis_general_pop_median",
    "sex", "location",
    "initial_tumor_height", "initial_tumor_diameter", "initial_t_stage_simple", # "initial_t_stage",
    "recurrence1", "mets_progression", "last_known_alive_date"
)

# Variables created during data processing
DERIVED_VARIABLES <- c(
    "age_at_diagnosis_binned", "age_at_diagnosis_general_pop_median",
    "initial_tumor_height_binned",
    "initial_tumor_diameter_binned", "initial_stage_binary",
    "gep_class_simple", "prame_status", "gep12_prame_status", "recurrence1_treatment_clean"
)

# Factor variables that must have proper levels
CRITICAL_FACTORS <- c(
    "treatment_group", "sex", "location",
    "biopsy1_gep", "gep_class_simple", "prame_status", "gep12_prame_status"
)

# Variables to check for missing data
MISSING_DATA_CHECK_VARIABLES <- c(
    "age_at_diagnosis_general_pop_median", "sex", "location", "initial_tumor_height",
    "initial_tumor_diameter", "treatment_group", "recurrence1",
    "mets_progression", "last_known_alive_date"
)

# Expected cohort sizes (ranges)
EXPECTED_COHORT_SIZES <- list(
    uveal_melanoma_full_cohort = c(250, 300), # Expected range for full cohort
    uveal_melanoma_restricted_cohort = c(150, 200), # Expected range for restricted cohort
    uveal_melanoma_gksrs_only_cohort = c(80, 120) # Expected range for GKSRS-only cohort
)

# =============================================================================
# VARIABLE LABELING AND TABLE CONFIGURATION
# =============================================================================
# CRITICAL: These settings control how variables are labeled in tables and plots
# - STANDARD_TABLE_LABELS: Human-readable labels for all variables
# - BASELINE_VARIABLES_TO_SUMMARIZE: Variables to include in baseline characteristics tables

# Human-readable labels for all variables
STANDARD_TABLE_LABELS <- list(
    # Demographics
    age_at_diagnosis = "Age at Diagnosis (years)",
    age_at_diagnosis_binned = "Age at Diagnosis (years)",
    age_at_diagnosis_general_pop_median = "Age at Diagnosis (years)",
    sex = "Sex",
    race = "Race",
    ethnicity = "Ethnicity",

    # Eye and tumor characteristics
    eye = "Eye",
    location = "Tumor Location",
    initial_tumor_height = "Initial Tumor Height (mm)",
    initial_tumor_diameter = "Initial Tumor Diameter (mm)",
    # GEP display cleanup driven in data; underscores removed via AUTO_CLEAN_LEVELS

    initial_overall_stage = "Overall Stage",
    # initial_overall_stage_modified = "Initial Overall Stage (Modified)",
    initial_t_stage = "Initial T-Stage",
    initial_t_stage_simple = "Initial T-Stage",
    initial_n_stage = "N Stage",
    initial_m_stage = "M Stage",
    unstaged = "Unstaged",

    # Treatment
    treatment_group = "Treatment Group",
    treatment_date = "Treatment Date",

    # Clinical features
    initial_vision = "Initial Visual Acuity (logMAR)",
    srf = "Subretinal Fluid (SRF)",
    op = "Orange Pigment",
    symptoms = "Symptomatic",
    vision_loss_blurred_vision = "Vision Loss/Blurred Vision",
    visual_field_defect = "Visual Field Defect",
    flashes_photopsia = "Flashes/Photopsia",
    floaters = "Floaters",
    pain = "Pain",

    # Tumor features
    internal_reflectivity = "Internal Reflectivity",
    optic_nerve = "Optic Nerve Abutment",

    # Tumor size binned variables
    initial_tumor_height_binned = "Initial Tumor Height (Binned)",
    initial_tumor_diameter_binned = "Initial Tumor Diameter (Binned)",
    initial_stage_binary = "Initial Stage (Binary)",

    # Staging
    n_stage = "N Stage",
    m_stage = "M Stage",
    initial_metastases = "Initial Metastases",
    initial_mets = "Initial Metastases",

    # Outcomes
    height_change = "Tumor Height Change (mm)",
    recurrence1 = "Local Recurrence",
    mets_progression = "Metastatic Progression",
    last_known_alive_date = "Last Known Alive Date",

    # Adverse Events
    vision_change = "Vision Change (logMAR)",
    retinopathy = "Radiation Retinopathy",
    nvg = "Neovascular Glaucoma",
    srd = "Serous Retinal Detachment",

    # GEP variables
    biopsy1_gep = "Gene Expression Profile",
    gep_class_simple = "GEP Class (Simple)",
    prame_status = "PRAME Status",

    # Follow-up
    total_followup_days = "Total Follow-up (Days)",
    total_years = "Total Follow-up (Years)",

    # Treatment outcomes
    recurrence1_treatment_clean = "Local Recurrence Treatment (Cleaned)"
)

# Display-level factor labels (raw -> display)
# Edit here to control how levels appear in tables/figures without changing the data
STANDARD_LEVEL_LABELS <- list(
    ethnicity = c(
        "Eastern_European" = "Eastern European",
        "Middle_Eastern" = "Middle Eastern"
    ),
    race = c(
        "African_American" = "African American",
        "Hispanic_Latino" = "Hispanic/Latino"
    ),
    unstaged = c(
        "Yes_Inappropriate_Scan" = "Yes Inappropriate Scan"
    ),
    gep_class_simple = c(
        "Class 1" = "Class 1",
        "Class 2" = "Class 2",
        "GEP Failed/Indeterminate" = "Failed or Indeterminate",
        "GEP Not Tested" = "Not Tested"
    ),
    gep12_prame_status = c(
        "Negative" = "Negative",
        "Positive" = "Positive"
    )
    # Add more mappings as needed, e.g.:
    # , eye = c("Left" = "OS", "Right" = "OD")
    # , prame_status = c("Not Available" = "Not Available", "Unknown" = "Unknown",
    #                   "Negative" = "PRAME Negative", "Positive" = "PRAME Positive")
)

# Automatically replace underscores with spaces for any levels not explicitly mapped above
# Set to FALSE to disable global cleanup
AUTO_CLEAN_LEVELS <- TRUE

# Evidence-based T-stage cutoffs for continuous variable binning
T_STAGE_HEIGHT_CUTOFFS <- c(3.0, 6.0, 9.0, 12.0, 15.0) # Creates ranges: <=3.0, 3.1-6.0, 6.1-9.0, 9.1-12.0, 12.1-15.0, >15.0
T_STAGE_DIAMETER_CUTOFFS <- c(3.0, 6.0, 9.0, 12.0, 15.0, 18.0) # Creates ranges: <=3.0, 3.1-6.0, 6.1-9.0, 9.1-12.0, 12.1-15.0, 15.1-18.0, >18.0

# Legacy median-based cutoffs (for backward compatibility)
LEGACY_CUTOFFS <- list(
    age_at_diagnosis_binned = 65, # Age cutoff for elderly vs young
    initial_tumor_height = 6.0, # Height cutoff for small vs large tumors (median-based)
    initial_tumor_diameter = 11.0 # Diameter cutoff for small vs large tumors (median-based)
)

# Dedicated cutoff for general population median age dichotomization (~63 years)
GENERAL_POP_MEDIAN_AGE_CUTOFF <- 63

# Variables to include in baseline characteristics tables
BASELINE_VARIABLES_TO_SUMMARIZE <- c(
    # Demographics
    "age_at_diagnosis",
    "age_at_diagnosis_binned", 
    "age_at_diagnosis_general_pop_median",
    "sex", "race", 
    # "ethnicity",

    # Eye and tumor characteristics
    "eye", "location", "initial_tumor_height", "initial_tumor_diameter",
    "initial_overall_stage", # this is the original variable
    # "initial_overall_stage_modified", # this is the modified variable
    "initial_t_stage_simple", # "initial_t_stage", 
    "initial_n_stage", "initial_m_stage", "unstaged",

    # Clinical features
    "initial_vision", "srf", "op", "symptoms", "vision_loss_blurred_vision",
    "visual_field_defect", "flashes_photopsia", "floaters", "pain",

    # Tumor features
    "internal_reflectivity", "optic_nerve",

    # Staging
    # "initial_mets",

    # GEP
    "biopsy1_gep",

    # Treatment
    "treatment_group"
)

# Variables treated as continuous in baseline summaries
BASELINE_CONTINUOUS_VARIABLES <- c(
    "age_at_diagnosis",
    "initial_tumor_height",
    "initial_tumor_diameter",
    "initial_vision"
)

# =============================================================================
# PLOT AND VISUALIZATION SETTINGS
# =============================================================================
# CRITICAL: These dimensions control ALL output figures and plots
# - FOREST_PLOT_WIDTH/HEIGHT: Dimensions for forest plots (inches)
# - SURVIVAL_PLOT_WIDTH/HEIGHT: Dimensions for survival curves
# - RMST_PLOT_WIDTH/HEIGHT: Dimensions for RMST plots
# - PLOT_DPI: Resolution for all saved figures (300 DPI for publication quality)

SURVIVAL_PLOT_SCALE <- 1.4 # Scale factor for survival plots

# Variable order for forest plots and subgroup analyses
FOREST_PLOT_VARIABLE_ORDER <- c(
    # "age_at_diagnosis_binned", 
    "age_at_diagnosis_general_pop_median",
    "sex", "location",
    "initial_t_stage_simple", # "initial_t_stage",
    "gep_class_simple", # gep_class_simple is just Class 1 vs Class 2
    "gep12_prame_status", # prame status within Class 1/2 (so this gets a value when gep_class_simple is known Class 1 or 2)
    # "initial_tumor_height", "initial_tumor_diameter", 
    # "biopsy1_gep", 
    "optic_nerve"
)

# This is used to map variable names to display names for forest plots
FORESTPLOT_NAME_MAPPING <- list(
    "age_at_diagnosis" = "Age at Diagnosis",
    "age_at_diagnosis_binned" = "Age at Diagnosis",
    "age_at_diagnosis_general_pop_median" = "Age at Diagnosis",
    "sex" = "Sex",
    "location" = "Location",
    "initial_overall_stage" = "Initial Overall Stage",
    "initial_t_stage" = "Initial T Stage",
    "initial_t_stage_simple" = "Initial T Stage",
    "initial_tumor_height" = "Initial Tumor Height",
    "initial_tumor_diameter" = "Initial Tumor Diameter",
    "biopsy1_gep" = "GEP Class",
    "gep_class_simple" = "GEP Class",
    "gep12_prame_status" = "PRAME Status",
    "optic_nerve" = "Optic Nerve Abutment"
)

# Canonical subgroup levels that should remain visible in forest plots even when
# a level is not estimable in the fitted model.
FOREST_PLOT_REQUIRED_LEVELS <- list(
    gep_class_simple = c(
        "Class 1",
        "Class 2",
        "GEP Failed/Indeterminate",
        "GEP Not Tested"
    ),
    gep12_prame_status = c(
        "Negative",
        "Positive"
    )
)

# Plot dimensions and settings for all output figures
FOREST_PLOT_WIDTH <- 10 # inches (reasonable width)
FOREST_PLOT_HEIGHT <- 12 # inches (increased height for all subgroup levels)
SURVIVAL_PLOT_WIDTH <- 16 # inches (prefer a wider KM layout so long legends/titles fit without making the figure overly tall)
SURVIVAL_PLOT_HEIGHT <- 9.5 # inches (base height lowered so standard KM figures save wider than tall)
PFS2_PLOT_HEIGHT <- 11.5 # inches (PFS-2 still needs extra vertical room for the risk table)
# KM x-axis cap (months) to match legacy visual range and avoid empty tails
SURVIVAL_XAXIS_MAX_MONTHS <- 216
# Dynamic sizing for KM plots based on number of strata (groups)
KM_BASE_HEIGHT <- SURVIVAL_PLOT_HEIGHT       # base height for ~2 strata
KM_HEIGHT_PER_STRATUM <- 0.4                 # extra inches per stratum beyond 2
KM_MAX_HEIGHT <- 14.5                        # upper bound to avoid overly tall figures
RMST_PLOT_WIDTH <- 11.5 # inches
RMST_PLOT_HEIGHT <- 6.25 # inches
CIF_PLOT_WIDTH <- 12 # inches (tighter than KM plots to reduce empty space for MSS cumulative incidence figures)
CIF_PLOT_HEIGHT <- 7 # inches
PLOT_DPI <- 300 # resolution
PLOT_UNITS <- "in" # units

# Default plot dimensions for generic figures
DEFAULT_PLOT_WIDTH <- 10 # inches
DEFAULT_PLOT_HEIGHT <- 8 # inches

# Small plot dimensions for compact figures (e.g., calibration mini-panels)
SMALL_PLOT_WIDTH <- 8  # inches
SMALL_PLOT_HEIGHT <- 6 # inches

# Centralized survival summary timepoints (years) used for KM summaries and RMST
SURVIVAL_SUMMARY_TIMEPOINTS_YEARS <- c(1, 3, 5, 10, 15)


#=============================================================================
# Vision helper utilities
# Provides shared functions for working with visual acuity changes.
#=============================================================================
# Vision change categorization settings
VISION_LINE_CHANGE_STEP <- 0.1
VISION_LINE_CHANGE_CATEGORY_LEVELS <- c(
    "≥3-line improvement",
    "2-line improvement",
    "1-line improvement",
    "Stable (0-line change)",
    "1-line loss",
    "2-line loss",
    "≥3-line loss"
)

# =============================================================================
# GEP VALIDATION CONFIGURATION (OBJECTIVE 4)
# =============================================================================
# CRITICAL: These settings control the GEP validation analysis (Objective 4)
# - GEP_VALIDATION_TIMEPOINTS: Years for validation (5, 7, 10 years)
# - GEP_BOOTSTRAP_ITERATIONS: Number of bootstrap samples for optimism correction
# - GEP_PRAME_BOOTSTRAP_ITERATIONS: Bootstrap resamples for PRAME delta-C intervals
# - GEP_PRAME_ADJUSTMENT_FACTOR / GEP_RISK_CUTOFFS: legacy heuristic PRAME constants pending removal
# - GEP_DCA_THRESHOLD_*: Decision curve analysis thresholds

# Save RDS files for reproducibility (disable by default to avoid unused outputs)
GEP_SAVE_RDS <- FALSE

# Core GEP validation settings
GEP_VALIDATION_TIMEPOINTS <- c(5, 7, 10) # years for validation analysis
GEP_BOOTSTRAP_ITERATIONS <- 1000
GEP_PRAME_BOOTSTRAP_ITERATIONS <- 100

# Legacy PRAME augmentation constants
GEP_PRAME_ADJUSTMENT_FACTOR <- 1.3 # 30% increase in risk for PRAME positive patients
GEP_PRAME_REDUCTION_FACTOR <- 0.9 # 10% decrease in risk for PRAME negative patients
GEP_RISK_CAP_MAXIMUM <- 0.95 # Maximum allowed risk prediction (cap at 95%)

# Risk stratification cutoffs for NRI analysis
GEP_RISK_CUTOFFS <- c(0, 0.1, 0.3, 1.0) # Risk categories: <10%, 10-30%, >30%
GEP_RISK_LABELS <- c("Low", "Intermediate", "High")

# Decision curve analysis thresholds
GEP_DCA_THRESHOLD_MIN <- 0.01 # Minimum risk threshold (1%)
GEP_DCA_THRESHOLD_MAX <- 0.50 # Maximum risk threshold (50%)
GEP_DCA_THRESHOLD_STEP <- 0.01 # Step size for threshold sequence

# Data cleaning and validation bounds
GEP_MAX_FOLLOWUP_YEARS <- 50 # Maximum reasonable follow-up time in years
GEP_MIN_FOLLOWUP_YEARS <- 0.01 # Minimum follow-up time in years
GEP_MIN_RISK_PREDICTION <- 0.001 # Minimum allowed risk prediction (avoid zero)
GEP_MAX_RISK_PREDICTION <- 0.999 # Maximum allowed risk prediction (avoid perfect)

# Calibration analysis constants
GEP_MIN_GROUP_SIZE <- 5 # Minimum patients per calibration group
GEP_DEFAULT_N_GROUPS <- 10 # Default number of calibration groups
GEP_MIN_N_GROUPS <- 3 # Minimum number of calibration groups
GEP_LOESS_SPAN <- 0.3 # Smoothing parameter for loess calibration curves
GEP_MIN_CALIBRATION_EVENTS <- 5 # Minimum events and non-events for stable recalibration fits
GEP_CALIBRATION_SPLINE_DF <- 3 # Degrees of freedom for IPCW-smoothed calibration curves
GEP_IPCW_WEIGHT_CAP_PROB <- 0.99 # Cap extreme inverse-censoring weights at the 99th percentile
GEP_MAX_CALIBRATION_COEF_ABS <- 10 # Treat larger recalibration coefficients as unstable/quasi-separated fits
GEP_MAX_CALIBRATION_COEF_SE <- 5 # Treat larger recalibration coefficient standard errors as unstable fits

# Sample size requirements for analysis
GEP_MIN_SAMPLE_SIZE <- 20 # Minimum sample size for any analysis
GEP_MIN_EVENTS_COMPETING_RISK <- 3 # Minimum events for competing risk analysis (lowered for exploratory cohorts)
GEP_MIN_BOOTSTRAP_SAMPLE <- 30 # Minimum sample size for bootstrap analysis
GEP_MAX_BOOTSTRAP_ITERATIONS <- 1000 # Maximum bootstrap iterations for production runs
GEP_MISSING_DATA_THRESHOLD <- 10 # Minimum patients needed for missing data analysis
GEP_RECOMMENDED_VALIDATION_SAMPLE <- 100 # Recommended minimum for robust validation
GEP_RECOMMENDED_TESTING_SAMPLE <- 30 # Recommended minimum for testing set

GEP_DEFINITIVE_SIMPLE_LEVELS <- c("Class 1", "Class 2")
GEP_CLASS_1_DEFINITIVE_RAW_LEVELS <- c(
    "Class_1A_PRAME_negative",
    "Class_1A_PRAME_positive",
    "Class_1B_PRAME_negative",
    "Class_1B_PRAME_positive"
)
GEP_CLASS_2_DEFINITIVE_RAW_LEVELS <- c(
    "Class_2_PRAME_negative",
    "Class_2_PRAME_positive"
)
GEP_DEFINITIVE_RAW_LEVELS <- c(
    GEP_CLASS_1_DEFINITIVE_RAW_LEVELS,
    GEP_CLASS_2_DEFINITIVE_RAW_LEVELS
)
GEP_FAILED_OR_INDETERMINATE_RAW_LEVELS <- c(
    "Failed",
    "Class_1A_PRAME_not_reported",
    "Class_2_PRAME_not_reported",
    "Class_2_PRAME_Unknown",
    "Class_1A_PRAME_discordant",
    "Unknown",
    "Other"
)
GEP_NOT_TESTED_RAW_LEVELS <- c("No", "N/A")
GEP_INVALID_ANALYSIS_LABELS <- c(
    GEP_FAILED_OR_INDETERMINATE_RAW_LEVELS,
    GEP_NOT_TESTED_RAW_LEVELS,
    "GEP Failed/Indeterminate",
    "GEP Not Tested"
)

# GEP-specific derived variables
GEP_DERIVED_VARIABLES <- c(
    "gep_class_simple", "prame_status", "expected_mfs_5yr", "expected_mfs_7yr",
    "expected_mfs_10yr", "expected_mss_5yr", "expected_mss_7yr", "expected_mss_10yr"
)

GEP_DISPLAY_VARIABLES <- c(
    "biopsy1_gep", "gep_class_simple", "prame_status", "gep12_prame_status"
)

# Objective 4 grouping specs and per-context defaults.
# Change these mappings when reader-facing or technical GEP groupings need to move together.
GEP_GROUPING_SPECS <- list(
    biopsy1_gep = list(
        key = "biopsy1_gep",
        var = "biopsy1_gep",
        label = "GEP Class",
        legend_title = "GEP Class",
        allowed_levels = NULL,
        reader_facing = FALSE
    ),
    biopsy1_gep_model = list(
        key = "biopsy1_gep_model",
        var = "biopsy1_gep_model",
        label = "GEP Class",
        legend_title = "GEP Class",
        allowed_levels = NULL,
        reader_facing = FALSE
    ),
    gep_class_simple = list(
        key = "gep_class_simple",
        var = "gep_class_simple",
        label = "Simplified GEP Class",
        legend_title = "GEP Class (Simple)",
        allowed_levels = c("Class 1", "Class 2"),
        reader_facing = TRUE
    )
)

GEP_OBJECTIVE4_GROUPING <- list(
    mfs = list(
        observed_expected = "biopsy1_gep",
        visuals = "biopsy1_gep",
        visuals_model = "biopsy1_gep_model"
    ),
    mss = list(
        observed_expected = "biopsy1_gep",
        competing_risk = "biopsy1_gep",
        reporting = "biopsy1_gep",
        visuals = "gep_class_simple"
    )
)

get_gep_grouping_spec <- function(grouping_key) {
    if (!grouping_key %in% names(GEP_GROUPING_SPECS)) {
        stop(sprintf("Unknown Objective 4 grouping key: %s", grouping_key))
    }

    GEP_GROUPING_SPECS[[grouping_key]]
}

get_gep_grouping_for_context <- function(outcome, context) {
    if (!outcome %in% names(GEP_OBJECTIVE4_GROUPING)) {
        stop(sprintf("Unknown Objective 4 outcome grouping scope: %s", outcome))
    }

    outcome_grouping <- GEP_OBJECTIVE4_GROUPING[[outcome]]
    if (!context %in% names(outcome_grouping)) {
        stop(sprintf("Unknown Objective 4 grouping context '%s' for outcome '%s'", context, outcome))
    }

    get_gep_grouping_spec(outcome_grouping[[context]])
}

# =============================================================================
# SUMMARY: This file contains ALL configuration constants for the analysis pipeline
# =============================================================================
# CRITICAL: This is the SINGLE source of truth for all analysis parameters
# - DO NOT add constants to other files
# - DO NOT duplicate constants across files
# - ALL analysis scripts source this file through all_helper_functions.R
# - Changes here affect the ENTIRE analysis pipeline
# =============================================================================
