# =============================================================================
# OBJECTIVE 1 ANALYSIS-POPULATION CONTRACTS
# =============================================================================
# These fingerprints make patient-level endpoint drift explicit. Updating a
# fingerprint is an approval action: first inspect the KM risk-set audit and
# document why the patient-level OS records changed.

OBJECTIVE1_OS_POPULATION_CONTRACTS <- tibble::tribble(
    ~dataset_name,                            ~time_var,          ~event_var,    ~group_var,          ~n_patients, ~n_events, ~population_fingerprint,                                           ~approval_note,
    "uveal_melanoma_restricted_cohort",      "tt_death_months", "death_event", "treatment_group", 167L,         39L,       "27c6c7e097e14ef6714ff4e1a2d9338f274b56fea4fb8ec5982a38c32cd695e4", "Approved after audited source-date corrections for IDs 125 and 211.",
    "uveal_melanoma_full_cohort",            "tt_death_months", "death_event", "treatment_group", 260L,         57L,       "111d11adbdf35be35d6c53764ba0e3d41c9929582f4b513ddc6c4a1ac6adbf6e", "Approved after audited source-date corrections for IDs 125 and 211."
)
