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
    retinopathy_burden_event = "Radiation Retinopathy Recorded Burden",
    nvg_burden_event = "Neovascular Glaucoma Recorded Burden",
    srd_burden_event = "Serous Retinal Detachment Recorded Burden",

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
# KM x-axis cap (months) for display only; models and summaries still use available follow-up.
SURVIVAL_XAXIS_MAX_MONTHS <- 180
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
