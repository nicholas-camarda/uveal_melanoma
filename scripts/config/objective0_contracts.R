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
    "treatment_year",
    "initial_tumor_height_binned",
    "initial_tumor_diameter_binned", "initial_stage_binary",
    "gep_class_simple", "prame_status", "gep12_prame_status", "recurrence1_treatment_clean",
    "retinopathy_burden_event", "nvg_burden_event", "srd_burden_event",
    "vision_change", "vision_line_change", "vision_line_change_bucket",
    "last_vision_followup_months_explicit", "last_vision_followup_months_proxy",
    "last_vision_followup_timing_source", "last_vision_followup_months",
    "exploratory_gep_group", "no_gep_group", "ciliary_involvement",
    "optic_nerve_involvement", "mets_at_or_before_treatment",
    "mets_free_at_baseline", "mets_event_analysis"
)

# Compatibility aliases preserve existing callers while tests/docs use the
# explicit Objective 0 names above.
CRITICAL_VARIABLES <- OBJECTIVE0_GLOBAL_REQUIRED_VARIABLES
DERIVED_VARIABLES <- OBJECTIVE0_DERIVED_OUTPUT_MANIFEST

# Downstream objective input contract enforced centrally during Objective 0.
# Domains are checked on non-missing values unless `missing_policy` is
# `complete`, which requires a value for every analytic row.
OBJECTIVE0_DOWNSTREAM_INPUT_CONTRACT <- tibble::tribble(
    ~objective_id, ~variable_name, ~variable_role, ~expected_domain, ~missing_policy, ~severity,
    "objective1", "treatment_group", "treatment exposure", "treatment_factor", "complete", "hard_error",
    "objective1", "age_at_diagnosis", "shared adjusted model covariate", "nonnegative_numeric", "complete", "hard_error",
    "objective1", "sex", "shared adjusted model covariate", "sex_factor", "complete", "hard_error",
    "objective1", "location", "shared adjusted model covariate", "present", "complete", "hard_error",
    "objective1", "treatment_year", "propensity baseline era covariate", "nonnegative_integer", "complete", "hard_error",
    "objective1", "recurrence1", "local recurrence source endpoint", "yn_display", "complete", "hard_error",
    "objective1", "recurrence_event", "local recurrence binary endpoint", "binary_01", "complete", "hard_error",
    "objective1", "tt_recurrence_months", "local recurrence follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective1", "tt_recurrence_months_analysis", "local recurrence analysis follow-up time", "nonnegative_numeric", "optional", "hard_error",
    "objective1", "mets_progression", "metastasis source endpoint", "yn_display", "complete", "hard_error",
    "objective1", "mets_event", "metastasis binary endpoint", "binary_01", "complete", "hard_error",
    "objective1", "mets_at_or_before_treatment", "baseline metastatic disease flag", "logical", "complete", "hard_error",
    "objective1", "mets_free_at_baseline", "incident MFS baseline eligibility flag", "logical", "complete", "hard_error",
    "objective1", "mets_event_analysis", "incident MFS event indicator", "binary_01", "optional", "hard_error",
    "objective1", "tt_mets_months", "metastasis follow-up time", "numeric", "optional", "hard_error",
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
    "objective2", "treatment_year", "visual sensitivity baseline era covariate", "nonnegative_integer", "complete", "hard_error",
    "objective2", "vision_change", "vision safety endpoint", "numeric", "optional", "warning",
    "objective2", "vision_line_change", "Snellen line-change numeric endpoint", "numeric", "optional", "warning",
    "objective2", "vision_line_change_bucket", "fixed Snellen line-change bucket", "vision_line_change_bucket", "optional", "warning",
    "objective2", "last_vision_followup_months_explicit", "explicit treatment-to-last-follow-up timing", "nonnegative_numeric", "optional", "warning",
    "objective2", "last_vision_followup_months_proxy", "proxy latest-VA timing", "nonnegative_numeric", "optional", "warning",
    "objective2", "last_vision_followup_timing_source", "latest-VA timing source", "vision_followup_timing_source", "optional", "warning",
    "objective2", "last_vision_followup_months", "primary latest-VA timing", "nonnegative_numeric", "optional", "warning",
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
    "objective4", "exploratory_gep_group", "exploratory GEP group", "gep_class_simple", "optional", "warning",
    "objective4", "no_gep_group", "no-GEP exploratory group", "no_gep_group", "optional", "warning",
    "objective4", "ciliary_involvement", "ciliary involvement exploratory predictor", "binary_01", "optional", "warning",
    "objective4", "optic_nerve_involvement", "optic-nerve involvement exploratory predictor", "binary_01", "optional", "warning",
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
    "objective4", "mfs_event_5yr", "5-year MFS event endpoint", "binary_01", "optional", "hard_error",
    "objective4", "mfs_event_7yr", "7-year MFS event endpoint", "binary_01", "optional", "hard_error",
    "objective4", "mfs_event_10yr", "10-year MFS event endpoint", "binary_01", "optional", "hard_error",
    "objective4", "mss_event_5yr", "5-year MSS event endpoint", "binary_01", "complete", "hard_error",
    "objective4", "mss_event_7yr", "7-year MSS event endpoint", "binary_01", "complete", "hard_error",
    "objective4", "mss_event_10yr", "10-year MSS event endpoint", "binary_01", "complete", "hard_error",
    "objective4", "event_type_mfs_5yr", "5-year MFS competing-risk type", "event_type_012", "optional", "hard_error",
    "objective4", "event_type_mfs_7yr", "7-year MFS competing-risk type", "event_type_012", "optional", "hard_error",
    "objective4", "event_type_mfs_10yr", "10-year MFS competing-risk type", "event_type_012", "optional", "hard_error",
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
