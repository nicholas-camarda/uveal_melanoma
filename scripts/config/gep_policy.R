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
GEP_RECOMMENDED_SCORING_SAMPLE <- 30 # Recommended minimum for the no-GEP scoring cohort

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
    "Unknown"
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
