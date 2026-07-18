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

# Reviewer-response adjusted models keep age continuous to avoid reviewer-flagged
# loss of information from dichotomization. Dichotomized age remains available for
# descriptive and exploratory subgroup displays only.
confounders <- c(
    "age_at_diagnosis", "sex", "location"
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

# These subgroup variables are exploratory display surfaces. They are not the
# default adjusted-model covariate set for the reviewer-response analyses.
subgroup_vars <- c(
    "age_at_diagnosis_general_pop_median", "sex", "location", "initial_t_stage_simple",
    #"initial_t_stage",
    "initial_tumor_height", "initial_tumor_diameter",
    "initial_overall_stage", "biopsy1_gep", "gep_class_simple", "optic_nerve"
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
