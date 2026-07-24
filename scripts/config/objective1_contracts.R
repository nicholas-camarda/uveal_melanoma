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

# Objective 1 treatment-by-subgroup comparisons use one event-time contract.
# The binary recurrence/metastasis outputs remain descriptive event support;
# inferential subgroup effects use Cox models and are reported as HRs.
OBJECTIVE1_SUBGROUP_OUTCOME_SPECS <- list(
    local_recurrence = list(
        outcome = "Local Recurrence",
        endpoint_type = "time_to_event",
        model_family = "Cox proportional hazards",
        effect_measure = "HR",
        time_var = "tt_recurrence_months",
        event_var = "recurrence_event",
        estimand = "Cause-specific hazard of local recurrence; death without recurrence is censored"
    ),
    metastatic_progression = list(
        outcome = "Metastatic Progression",
        endpoint_type = "time_to_event",
        model_family = "Cox proportional hazards",
        effect_measure = "HR",
        time_var = "tt_mets_months",
        event_var = "mets_event",
        estimand = "Cause-specific hazard of metastatic progression; death without metastasis is censored"
    ),
    overall_survival = list(
        outcome = "Overall Survival",
        endpoint_type = "time_to_event",
        model_family = "Cox proportional hazards",
        effect_measure = "HR",
        time_var = "tt_death_months",
        event_var = "death_event",
        estimand = "Hazard of death from any cause"
    ),
    progression_free_survival = list(
        outcome = "Progression-Free Survival",
        endpoint_type = "time_to_event",
        model_family = "Cox proportional hazards",
        effect_measure = "HR",
        time_var = "tt_pfs_months",
        event_var = "pfs_event",
        estimand = "Hazard of first local recurrence, metastatic progression, or death"
    )
)

get_objective1_subgroup_outcome_spec <- function(outcome_key) {
    spec <- OBJECTIVE1_SUBGROUP_OUTCOME_SPECS[[outcome_key]]
    if (is.null(spec)) {
        stop(sprintf("Unknown Objective 1 subgroup outcome key: %s", outcome_key), call. = FALSE)
    }
    spec
}
