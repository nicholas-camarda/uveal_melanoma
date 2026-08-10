# =============================================================================
# OBJECTIVE 1 ANALYSIS-POPULATION CONTRACTS
# =============================================================================
# These fingerprints make patient-level endpoint drift explicit. Updating a
# fingerprint is an approval action: first inspect the KM risk-set audit and
# document why the patient-level OS records changed.

OBJECTIVE1_OS_POPULATION_CONTRACTS <- tibble::tribble(
    ~dataset_name,                            ~time_var,          ~event_var,    ~group_var,          ~n_patients, ~n_events, ~population_fingerprint,                                           ~approval_note,
    "uveal_melanoma_restricted_cohort",      "tt_death_months", "death_event", "treatment_group", 167L,         39L,       "27c6c7e097e14ef6714ff4e1a2d9338f274b56fea4fb8ec5982a38c32cd695e4", "Approved after audited source-date corrections for IDs 125 and 211.",
    "uveal_melanoma_full_cohort",            "tt_death_months", "death_event", "treatment_group", 260L,         57L,       "baf54f9b3c6de607fddb44efa23860ab8368f33a8dc7ca39a8a0ca0379b82362", "Approved after the audited patient-45 initial-GK date correction and previously audited source-date corrections for IDs 125 and 211; only patient 45 changed in the full-cohort OS population, with cohort size and event count unchanged."
)

# Production propensity populations are approval-controlled. Any change in
# size or fingerprint requires row-level audit and scientific approval.
OBJECTIVE1_PROPENSITY_EXPECTED_POPULATIONS <- tibble::tribble(
    ~surface, ~n_patients, ~n_events, ~population_fingerprint,
    "propensity_membership", 164L, NA_integer_, "356d947d6528af641d7ec2c870314616a69f328977636d448d1ffe92546ae180",
    "local_recurrence", 164L, 18L, "bdd8cfb1610996e7d805dddca1a80f269f66fef4fd58faca1ac12cceecc33dd6",
    "metastatic_progression", 164L, 28L, "0796dd46c5740a05abc5c99d1534dfe75f12124d205e1a0f7712c0c2eda08b71",
    "overall_survival", 164L, 39L, "ed18b406e5e73752bdf98c7d9104e4c1024f0d31563d9200cdb748c8b0e6de7d",
    "progression_free_survival", 164L, 51L, "320fdfac66a6cacdb719935ee633b6146c36d793e9b4729261330ac532edb2ad"
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
