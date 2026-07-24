# =============================================================================
# OBJECTIVE 0 VALIDATION CONTRACTS
# =============================================================================
# These settings define the Objective 0 data-readiness boundary. Keep global
# structure checks, derived-output manifests, downstream objective inputs, and
# objective-specific derivation contracts distinct so the same variable is not
# maintained through overlapping lists with ambiguous ownership.

# Data validation thresholds
MINIMUM_COLUMNS_AFTER_PROCESSING <- 150 # Minimum expected columns after data processing
MAXIMUM_MISSING_DATA_PERCENTAGE <- 50 # Maximum allowed missing data percentage for critical variables

# Global structural fields that must exist before objective-specific checks.
OBJECTIVE0_GLOBAL_REQUIRED_VARIABLES <- c(
    "id", "treatment_group", "age_at_diagnosis_binned", "age_at_diagnosis_general_pop_median",
    "sex", "location",
    "initial_tumor_height", "initial_tumor_diameter", "initial_t_stage_simple", # "initial_t_stage",
    "recurrence1", "mets_progression", "last_known_alive_date"
)

# Derived fields Objective 0 is expected to create before downstream analysis.
OBJECTIVE0_DERIVED_OUTPUT_MANIFEST <- c(
    "age_at_diagnosis_binned", "age_at_diagnosis_general_pop_median",
    "initial_tumor_height_binned",
    "initial_tumor_diameter_binned", "initial_stage_binary",
    "gep_class_simple", "prame_status", "gep12_prame_status", "recurrence1_treatment_clean",
    "retinopathy_burden_event", "nvg_burden_event", "srd_burden_event"
)

# Compatibility aliases preserve existing callers while tests/docs use the
# explicit Objective 0 names above.
CRITICAL_VARIABLES <- OBJECTIVE0_GLOBAL_REQUIRED_VARIABLES
DERIVED_VARIABLES <- OBJECTIVE0_DERIVED_OUTPUT_MANIFEST

# Objective 2 toxicity burden endpoints prepared during Objective 0 data derivation.
OBJECTIVE2_TOXICITY_ENDPOINTS <- tibble::tribble(
    ~source_field, ~analysis_field, ~endpoint_label,
    "retinopathy", "retinopathy_burden_event", "Radiation Retinopathy",
    "nvg", "nvg_burden_event", "Neovascular Glaucoma",
    "srd", "srd_burden_event", "Serous Retinal Detachment"
)

# Objective 2 simulated Fisher p-values use a local seed so displayed
# descriptive p-values are reproducible without perturbing unrelated RNG state.
OBJECTIVE2_SIMULATED_FISHER_SEED <- 20260422L

# Downstream objective input contract enforced centrally during Objective 0.
# Domains are checked on non-missing values unless `missing_policy` is
# `complete`, which requires a value for every analytic row.
OBJECTIVE0_DOWNSTREAM_INPUT_CONTRACT <- tibble::tribble(
    ~objective_id, ~variable_name, ~variable_role, ~expected_domain, ~missing_policy, ~severity,
    "objective1", "treatment_group", "treatment exposure", "treatment_factor", "complete", "hard_error",
    "objective1", "age_at_diagnosis", "shared adjusted model covariate", "nonnegative_numeric", "complete", "hard_error",
    "objective1", "sex", "shared adjusted model covariate", "sex_factor", "complete", "hard_error",
    "objective1", "location", "shared adjusted model covariate", "present", "complete", "hard_error",
    "objective1", "recurrence1", "local recurrence source endpoint", "yn_display", "complete", "hard_error",
    "objective1", "recurrence_event", "local recurrence binary endpoint", "binary_01", "complete", "hard_error",
    "objective1", "tt_recurrence_months", "local recurrence follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective1", "tt_recurrence_months_analysis", "local recurrence analysis follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective1", "mets_progression", "metastasis source endpoint", "yn_display", "complete", "hard_error",
    "objective1", "mets_event", "metastasis binary endpoint", "binary_01", "complete", "hard_error",
    "objective1", "tt_mets_months", "metastasis follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective1", "tt_mets_months_analysis", "metastasis analysis follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective1", "death_event", "overall survival binary endpoint", "binary_01", "complete", "hard_error",
    "objective1", "tt_death_months", "overall survival follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective1", "tt_death_months_analysis", "overall survival analysis follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective1", "pfs_event", "PFS binary endpoint", "binary_01", "complete", "hard_error",
    "objective1", "tt_pfs_months", "PFS follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective1", "tt_pfs_months_analysis", "PFS analysis follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective1", "height_change", "tumor height response endpoint", "numeric", "optional", "warning",
    "objective1", "vision_change", "vision change endpoint", "numeric", "optional", "warning",
    "objective2", "retinopathy", "retinopathy source endpoint", "yn_raw", "complete", "hard_error",
    "objective2", "treatment_group", "treatment exposure", "treatment_factor", "complete", "hard_error",
    "objective2", "age_at_diagnosis", "shared adjusted model covariate", "nonnegative_numeric", "complete", "hard_error",
    "objective2", "age_at_diagnosis_general_pop_median", "shared adjusted model covariate", "present", "complete", "hard_error",
    "objective2", "sex", "shared adjusted model covariate", "sex_factor", "complete", "hard_error",
    "objective2", "location", "shared adjusted model covariate", "present", "complete", "hard_error",
    "objective2", "vision_change", "vision safety endpoint", "numeric", "optional", "warning",
    "objective2", "initial_vision", "vision change source baseline", "numeric", "optional", "warning",
    "objective2", "last_vision", "vision change source follow-up", "numeric", "optional", "warning",
    "objective2", "recurrence1_pretreatment_vision", "vision change recurrence source", "numeric", "optional", "warning",
    "objective2", "initial_tumor_height", "shared baseline tumor measure", "nonnegative_numeric", "optional", "warning",
    "objective2", "initial_tumor_diameter", "shared baseline tumor measure", "nonnegative_numeric", "optional", "warning",
    "objective2", "optic_nerve", "shared eligibility/subgroup descriptor", "yn_display", "optional", "hard_error",
    "objective2", "nvg", "neovascular glaucoma source endpoint", "yn_raw", "complete", "hard_error",
    "objective2", "srd", "serous retinal detachment source endpoint", "yn_raw", "complete", "hard_error",
    "objective2", "retinopathy_burden_event", "retinopathy burden endpoint", "binary_01", "complete", "hard_error",
    "objective2", "nvg_burden_event", "neovascular glaucoma burden endpoint", "binary_01", "complete", "hard_error",
    "objective2", "srd_burden_event", "serous retinal detachment burden endpoint", "binary_01", "complete", "hard_error",
    "objective3", "treatment_group", "primary treatment exposure", "treatment_factor", "complete", "hard_error",
    "objective3", "age_at_diagnosis", "shared adjusted model covariate", "nonnegative_numeric", "complete", "hard_error",
    "objective3", "age_at_diagnosis_general_pop_median", "shared adjusted model covariate", "present", "complete", "hard_error",
    "objective3", "sex", "shared adjusted model covariate", "sex_factor", "complete", "hard_error",
    "objective3", "location", "shared adjusted model covariate", "present", "complete", "hard_error",
    "objective3", "recurrence1", "first recurrence source endpoint", "yn_display", "complete", "hard_error",
    "objective3", "recurrence1_treatment", "first-recurrence treatment source", "present", "optional", "warning",
    "objective3", "recurrence1_treatment_date", "first-recurrence treatment date source", "present", "optional", "warning",
    "objective3", "recurrence2", "second recurrence source endpoint", "yn_raw_or_display", "optional", "hard_error",
    "objective3", "recurrence2_date", "second recurrence date source", "present", "optional", "warning",
    "objective3", "dod", "death date source for PFS-2 censoring/event logic", "present", "optional", "warning",
    "objective3", "last_known_alive_date", "follow-up source for PFS-2 censoring", "present", "optional", "warning",
    "objective3", "recurrence1_treatment_clean", "first-recurrence salvage treatment", "present", "optional", "hard_error",
    "objective3", "pfs2_event", "second recurrence binary endpoint", "binary_01", "optional", "hard_error",
    "objective3", "tt_pfs2_months", "second recurrence follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective3", "tt_pfs2_years", "second recurrence follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective4", "age_at_diagnosis", "shared prognostic/prediction field", "nonnegative_numeric", "complete", "hard_error",
    "objective4", "biopsy1_gep", "GEP class display endpoint", "present", "optional", "hard_error",
    "objective4", "biopsy1_gep_mfs", "GEP expected MFS source", "probability", "optional", "hard_error",
    "objective4", "biopsy1_gep_mss", "GEP expected MSS source", "probability", "optional", "hard_error",
    "objective4", "gep_class_simple", "GEP class endpoint", "gep_class_simple", "optional", "hard_error",
    "objective4", "prame_status", "PRAME endpoint", "prame_status", "optional", "hard_error",
    "objective4", "gep12_prame_status", "Class 1/2 PRAME endpoint", "gep12_prame_status", "optional", "warning",
    "objective4", "gep_validation_set", "GEP availability label", "gep_validation_set", "complete", "hard_error",
    "objective4", "mfs_analysis_eligible", "MFS analysis eligibility flag", "logical", "complete", "hard_error",
    "objective4", "mss_analysis_eligible", "MSS analysis eligibility flag", "logical", "complete", "hard_error",
    "objective4", "expected_mfs_5yr", "5-year expected MFS probability", "probability", "optional", "hard_error",
    "objective4", "expected_mfs_7yr", "7-year expected MFS probability", "probability", "optional", "hard_error",
    "objective4", "expected_mfs_10yr", "10-year expected MFS probability", "probability", "optional", "hard_error",
    "objective4", "expected_mss_5yr", "5-year expected MSS probability", "probability", "optional", "hard_error",
    "objective4", "expected_mss_7yr", "7-year expected MSS probability", "probability", "optional", "hard_error",
    "objective4", "expected_mss_10yr", "10-year expected MSS probability", "probability", "optional", "hard_error",
    "objective4", "mfs_event_5yr", "5-year MFS event endpoint", "binary_01", "complete", "hard_error",
    "objective4", "mfs_event_7yr", "7-year MFS event endpoint", "binary_01", "complete", "hard_error",
    "objective4", "mfs_event_10yr", "10-year MFS event endpoint", "binary_01", "complete", "hard_error",
    "objective4", "mss_event_5yr", "5-year MSS event endpoint", "binary_01", "complete", "hard_error",
    "objective4", "mss_event_7yr", "7-year MSS event endpoint", "binary_01", "complete", "hard_error",
    "objective4", "mss_event_10yr", "10-year MSS event endpoint", "binary_01", "complete", "hard_error",
    "objective4", "event_type_mfs_5yr", "5-year MFS competing-risk type", "event_type_012", "complete", "hard_error",
    "objective4", "event_type_mfs_7yr", "7-year MFS competing-risk type", "event_type_012", "complete", "hard_error",
    "objective4", "event_type_mfs_10yr", "10-year MFS competing-risk type", "event_type_012", "complete", "hard_error",
    "objective4", "event_type_mss_5yr", "5-year MSS competing-risk type", "event_type_012", "complete", "hard_error",
    "objective4", "event_type_mss_7yr", "7-year MSS competing-risk type", "event_type_012", "complete", "hard_error",
    "objective4", "event_type_mss_10yr", "10-year MSS competing-risk type", "event_type_012", "complete", "hard_error",
    "objective4", "tt_mfs_5yr", "5-year MFS follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective4", "tt_mfs_7yr", "7-year MFS follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective4", "tt_mfs_10yr", "10-year MFS follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective4", "tt_mss_5yr", "5-year MSS follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective4", "tt_mss_7yr", "7-year MSS follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective4", "tt_mss_10yr", "10-year MSS follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective4", "tt_death_months", "death follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective4", "tt_death_years", "death follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective4", "melanoma_death_event", "melanoma death endpoint", "binary_01", "complete", "hard_error",
    "objective4", "competing_death_event", "competing death endpoint", "binary_01", "complete", "hard_error",
    "objective4", "predicted_mfs_risk_5yr", "5-year predicted MFS risk", "probability", "optional", "hard_error",
    "objective4", "predicted_mfs_risk_7yr", "7-year predicted MFS risk", "probability", "optional", "hard_error",
    "objective4", "predicted_mfs_risk_10yr", "10-year predicted MFS risk", "probability", "optional", "hard_error",
    "objective4", "predicted_mss_risk_5yr", "5-year predicted MSS risk", "probability", "optional", "hard_error",
    "objective4", "predicted_mss_risk_7yr", "7-year predicted MSS risk", "probability", "optional", "hard_error",
    "objective4", "predicted_mss_risk_10yr", "10-year predicted MSS risk", "probability", "optional", "hard_error"
)

# Compact PFS-2 derivation contract. Death before second recurrence censors
# PFS-2 because the event of interest is second local recurrence.
OBJECTIVE3_PFS2_DERIVATION_CONTRACT <- list(
    source_fields = c(
        "recurrence1", "recurrence1_treatment", "recurrence1_treatment_date",
        "recurrence2", "recurrence2_date", "dod", "last_known_alive_date"
    ),
    derived_fields = c(
        "recurrence1_treatment_clean", "pfs2_event",
        "tt_pfs2_months", "tt_pfs2_years"
    ),
    time_origin = "recurrence1_treatment_date",
    event_date = "recurrence2_date",
    censor_dates = c("dod", "last_known_alive_date"),
    event_description = "second local recurrence after first-recurrence treatment",
    death_handling = "death before second local recurrence is censoring"
)

# Compact GEP derivation contract for imported probabilities and horizon fields.
# The time-unit convention is intentional: MFS horizons are in months, MSS
# horizons are in years.
OBJECTIVE4_GEP_DERIVATION_CONTRACT <- tibble::tribble(
    ~outcome, ~horizon_years, ~horizon_months, ~source_probability_field, ~expected_survival_field, ~predicted_risk_field, ~event_field, ~event_type_field, ~time_field, ~time_unit, ~eligibility_field,
    "mfs", 5, 60, "biopsy1_gep_mfs", "expected_mfs_5yr", "predicted_mfs_risk_5yr", "mfs_event_5yr", "event_type_mfs_5yr", "tt_mfs_5yr", "months", "mfs_analysis_eligible",
    "mfs", 7, 84, "biopsy1_gep_mfs", "expected_mfs_7yr", "predicted_mfs_risk_7yr", "mfs_event_7yr", "event_type_mfs_7yr", "tt_mfs_7yr", "months", "mfs_analysis_eligible",
    "mfs", 10, 120, "biopsy1_gep_mfs", "expected_mfs_10yr", "predicted_mfs_risk_10yr", "mfs_event_10yr", "event_type_mfs_10yr", "tt_mfs_10yr", "months", "mfs_analysis_eligible",
    "mss", 5, 60, "biopsy1_gep_mss", "expected_mss_5yr", "predicted_mss_risk_5yr", "mss_event_5yr", "event_type_mss_5yr", "tt_mss_5yr", "years", "mss_analysis_eligible",
    "mss", 7, 84, "biopsy1_gep_mss", "expected_mss_7yr", "predicted_mss_risk_7yr", "mss_event_7yr", "event_type_mss_7yr", "tt_mss_7yr", "years", "mss_analysis_eligible",
    "mss", 10, 120, "biopsy1_gep_mss", "expected_mss_10yr", "predicted_mss_risk_10yr", "mss_event_10yr", "event_type_mss_10yr", "tt_mss_10yr", "years", "mss_analysis_eligible"
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
