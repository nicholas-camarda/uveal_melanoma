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

# Versioned raw-date corrections applied during Objective 0 loading.
# These are limited to records with clear source-level date typos and are
# published into each cohort's 00_General audit workbooks.
MANUAL_DATE_CORRECTIONS <- tibble::tribble(
    ~study_id, ~column_name,           ~corrected_value,       ~correction_reason,                                                                 ~confidence_tier, ~supporting_columns,
    "125",     "initial_gk_date",      as.Date("2017-06-13"),  "Initial GK treatment year aligned to the clustered 2017 consult, diagnosis, and liver-staging chronology.", "high",          "date_ophtho_consult,date_diagnosis,date_rad_onc_consult,date_initial_liver_staging",
    "211",     "date_diagnosis",       as.Date("2014-01-28"),  "Diagnosis year aligned to the 2014 plaque treatment and radiation-oncology consult chronology.",                  "high",          "date_ophtho_consult,date_rad_onc_consult,initial_plaque_date,last_followup",
    "211",     "date_ophtho_consult",  as.Date("2014-01-21"),  "Ophthalmology consult year aligned to the same 2014 treatment chronology as the corrected diagnosis date.",          "high",          "date_diagnosis,date_rad_onc_consult,initial_plaque_date,last_followup",
    "213",     "date_diagnosis",       as.Date("2012-12-16"),  "Diagnosis year aligned to the 2013 radiation-oncology consult and plaque treatment chronology.",                      "moderate",      "date_ophtho_consult,date_rad_onc_consult,initial_plaque_date,last_followup",
    "213",     "date_ophtho_consult",  as.Date("2012-12-16"),  "Ophthalmology consult year aligned to the same pre-treatment chronology as the corrected diagnosis date.",              "moderate",      "date_diagnosis,date_rad_onc_consult,initial_plaque_date,last_followup"
)

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
PFS2_REPORT_HORIZON_MONTHS <- 36 # PFS-2 support horizon used for censoring diagnostics
PFS2_HEAVY_CENSORING_THRESHOLD <- 0.70 # Downgrade fitted PFS-2 interpretation at or above this censored fraction
PFS2_CENSORING_IMBALANCE_THRESHOLD <- 0.30 # Downgrade if treatment-arm censoring differs by this fraction
MINIMUM_PH_TEST_EVENTS <- 10L # Minimum events for proportional-hazards diagnostics/reporting

# Tumor size thresholds for cohort eligibility
TUMOR_HEIGHT_THRESHOLD <- 10 # mm
TUMOR_DIAMETER_THRESHOLD <- 20 # mm

# Cohort assignment states written by Objective 0.
CONSORT_GROUP_ELIGIBLE_BOTH <- "eligible_both"
CONSORT_GROUP_GKSRS_ONLY <- "gksrs_only"
CONSORT_GROUP_FULL_ONLY_SPECIAL_CASE <- "full_cohort_only_special_case"
CONSORT_GROUP_UNCLASSIFIED_FIELDS <- "unclassified_cohort_fields"

# Audited Objective 0 special case: iris tumors cannot abut the optic nerve,
# but this record remains full-cohort-only rather than entering subcohorts.
IRIS_OPTIC_NERVE_SPECIAL_CASE <- "iris_optic_nerve_not_applicable_full_cohort_only"

# Time conversion constants
DAYS_IN_YEAR <- 365.25
DAYS_IN_MONTH <- 30.44
FOLLOW_UP_YEARS <- 5 # For 5-year outcomes
UNITS_OF_TIME <- "months" # "days" or "months" or "years"
VITAL_STATUS_DATA_CUTOFF_DATE <- as.Date("2025-03-04")
LOST_TO_FOLLOWUP_CUTOFF_DAYS <- 450
MINOR_TREATMENT_DIAGNOSIS_GAP_DAYS <- 7L # Reverse-order gaps at or below this threshold are warnings requiring manual review, not hard stops
