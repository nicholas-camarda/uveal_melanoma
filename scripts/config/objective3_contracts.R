# =============================================================================
# OBJECTIVE 3 REPEAT-RADIATION CONTRACTS
# =============================================================================
# These settings define the PFS-2 derivation boundary and the feasibility and
# censoring thresholds used by the Objective 3 implementation.

OBJECTIVE3_MINIMUM_PFS2_PATIENTS <- 10L # Minimum analyzable patients before attempting PFS-2 modeling
OBJECTIVE3_PFS2_REPORT_HORIZON_MONTHS <- 36 # PFS-2 support horizon used for censoring diagnostics
OBJECTIVE3_PFS2_HEAVY_CENSORING_THRESHOLD <- 0.70 # Downgrade fitted PFS-2 interpretation at or above this censored fraction
OBJECTIVE3_PFS2_CENSORING_IMBALANCE_THRESHOLD <- 0.30 # Downgrade if treatment-arm censoring differs by this fraction

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
