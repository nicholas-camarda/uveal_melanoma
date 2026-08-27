# =============================================================================
# OBJECTIVE 4 GEP DERIVATION CONTRACTS
# =============================================================================
# This contract protects row-wise consistency for imported probabilities and
# horizon fields. General presence/domain checks alone cannot prove these rules.
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
