# objective0-config-contract-organization Specification

## Purpose
TBD - created by archiving change p5-harden-objective0-validation-contracts. Update Purpose after archive.
## Requirements
### Requirement: Objective 0 SHALL avoid duplicate hand-maintained validation lists
Objective 0 validation policy MUST NOT require maintainers to update overlapping required-variable lists for the same validation boundary. Global structural requirements, derived-output manifests, downstream objective-input contracts, and endpoint mapping tables MUST have distinct responsibilities.

`CRITICAL_VARIABLES` and `DERIVED_VARIABLES` MAY be renamed, replaced, or generated from a broader Objective 0 registry, but their existing validation intent MUST be preserved:

- Global structural requirements cover fields required for cohort identity, cohort construction, and baseline validation before objective-specific checks.
- Derived-output manifests cover fields that Objective 0 derivation is expected to create, including display, stratification, and reporting-support fields that are not necessarily downstream model inputs.
- Downstream objective-input contracts cover endpoint-definition, cohort-eligibility, core adjustment, and required prognostic/prediction fields consumed by Objectives 1-4.
- Endpoint mapping tables cover source-to-derived endpoint relationships used by derivation and analysis field resolution.

#### Scenario: Global and objective-specific contracts remain distinct
- **WHEN** Objective 0 validates a cohort
- **THEN** global structural requirements are evaluated separately from Objective 1-4 downstream input requirements
- **AND** validation findings preserve enough labeling to show whether a failure is global structural readiness, derived-output completeness, or downstream objective-input readiness

#### Scenario: Derived-output manifest is not treated as downstream model input
- **WHEN** Objective 0 verifies derived fields such as binned display variables or staging summaries
- **THEN** those fields may be checked as derived-output completeness
- **AND** they are not forced into the downstream objective-input contract unless a downstream objective requires them for endpoint definition, cohort eligibility, core adjustment, or required prognostic/prediction logic

### Requirement: Objective-specific source-derived contracts SHALL prevent endpoint drift
Objective 0 validation hardening MUST include source-derived consistency coverage where presence, domain, and missingness checks are insufficient to prove endpoint validity. The coverage MAY use invariant tests, mapping tables, or compact derivation manifests depending on the objective's endpoint structure.

These contracts MUST remain data-readiness checks. They MUST NOT create per-objective raw-data audit workbook families, replace downstream model feasibility diagnostics, or duplicate full derivation logic when a smaller invariant test is sufficient.

#### Scenario: Objective 1 endpoint invariants are locked without a full mapping table
- **WHEN** Objective 0 derivation creates Objective 1 recurrence, metastasis, death, and PFS fields
- **THEN** regression tests assert that `recurrence_event` follows `recurrence1`, `mets_event` follows `mets_progression`, `death_event` follows `dod`, and `pfs_event` follows local recurrence or death
- **AND** tests assert that `tt_pfs_months` is derived from recurrence/death timing and is not silently redefined to use metastatic progression
- **AND** Objective 1 does not require a standalone endpoint mapping table unless future derivation complexity makes that table clearer than invariant tests

### Requirement: Objective 2 toxicity endpoint mapping SHALL remain explicit
Objective 2 toxicity source-to-burden-field relationships MUST remain represented by an explicit mapping table because derivation, Objective 0 validation, and Objective 2 analysis field resolution all consume that mapping.

The mapping table MUST NOT be treated as a redundant copy of the downstream input contract. Instead, tests MUST assert that the mapping and downstream contract stay aligned for `retinopathy`, `nvg`, `srd`, `retinopathy_burden_event`, `nvg_burden_event`, and `srd_burden_event`.

#### Scenario: Toxicity mapping and validation contract cannot drift silently
- **WHEN** a toxicity endpoint source or burden field is added, removed, or renamed in the mapping
- **THEN** regression tests fail unless the Objective 0 downstream validation contract is updated consistently
- **AND** Objective 2 continues to resolve analysis fields from the central mapping rather than hard-coded endpoint names

### Requirement: Retired other-map plumbing SHALL stay retired
The current workflow MUST NOT depend on, generate, or document `other_map.rds` as an active runtime artifact. Sparse-factor handling and removed/filtered-level reporting MUST be carried by the existing diagnostics, validation bundles, and pre-collapse cohort artifacts rather than a separate `other_map.rds` sidecar.

Current workflow, analysis, table, visualization, and GEP public APIs MUST NOT reintroduce `other_map` parameters. Current documentation MUST describe the present artifact contract directly and MUST NOT list `other_map.rds` as a generated support file.

The current runtime audit establishes that the old collapsed-variable `Other` design is not active: current analytic and pre-collapse cohorts contain no observed `"Other"` values, and `other_map.rds` is absent from the processed runtime artifact directory. The current workbook also contains no raw `biopsy1_gep == "Other"` rows, so `biopsy1_gep_raw` MUST NOT retain `"Other"` as an unused factor level or configured failed/indeterminate raw GEP label.

#### Scenario: Other-map runtime artifact is absent
- **WHEN** Objective 0 rebuilds analytic datasets
- **THEN** `PROCESSED_DATA_DIR` does not contain `other_map.rds`
- **AND** downstream analyses run without loading or passing an `other_map` object

#### Scenario: Old collapsed Other data are absent
- **WHEN** Objective 0 writes analytic and pre-collapse cohort `.rds` files
- **THEN** no observed row values are produced by the retired collapsed-variable `Other` mechanism
- **AND** `biopsy1_gep_raw` does not include `"Other"` as an observed value or unused factor level
- **AND** GEP invalid-label constants do not list `"Other"` as a current raw GEP label

#### Scenario: Other-map documentation drift is rejected
- **WHEN** committed current documentation describes runtime support artifacts
- **THEN** it does not present `other_map.rds` as current behavior
- **AND** regression tests or documentation-contract checks fail if `other_map.rds` is reintroduced into current docs

### Requirement: Iris-tumor optic-nerve N/A SHALL be audited as a cohort special case
Objective 0 MUST audit raw `optic_nerve = "N/A"` for iris tumors as non-abutment because this raw state means the optic nerve abutment field is not applicable, not unknown. This special-case interpretation MUST occur during Objective 0 data preparation or raw-field normalization before cohort assignment, not inside downstream analysis scripts.

The special case MUST be explicit and auditable through existing Objective 0 validation or reconciliation surfaces. It MUST NOT create a broad rule that converts all missing `optic_nerve` values to non-abutment. It also MUST NOT automatically move iris-tumor special-case rows into the restricted or GKSRS-only cohorts.

#### Scenario: ID 247 is retained as an audited full-cohort-only special case
- **WHEN** Objective 0 processes the current raw row for `id = 247`
- **THEN** raw `optic_nerve = "N/A"` with `location = "Iris"` is recorded as non-abutment/not applicable in the Objective 0 audit trail
- **AND** the row is no longer assigned `consort_group == "other"`
- **AND** the row remains out of the restricted and GKSRS-only subcohorts unless a later explicit cohort-definition decision changes that rule
- **AND** the special-case handling is reported through existing Objective 0 audit/validation artifacts

#### Scenario: Ambiguous optic-nerve missingness is not silently recoded
- **WHEN** a treated row has missing `optic_nerve` without satisfying the explicit iris-tumor non-applicability rule
- **THEN** Objective 0 reports the row through existing removal or validation-failure mechanisms
- **AND** the row is not silently assigned to an analytic catch-all `other` cohort

### Requirement: Objective 3 PFS-2 SHALL have a compact derivation contract
Objective 0 MUST maintain a compact Objective 3 PFS-2 derivation contract linking `recurrence1`, `recurrence1_treatment`, `recurrence1_treatment_date`, `recurrence2`, `recurrence2_date`, `dod`, and `last_known_alive_date` to `pfs2_event`, `tt_pfs2_months`, `tt_pfs2_years`, and `recurrence1_treatment_clean`.

The contract MUST validate row-wise source-to-derived consistency before Objective 3 consumes PFS-2 fields. Death before second local recurrence MUST be treated as censoring, not a PFS-2 event.

#### Scenario: PFS-2 event and time fields match source dates
- **WHEN** a patient has first recurrence treatment and a second recurrence before death
- **THEN** `pfs2_event` is `1`
- **AND** PFS-2 time ends at `recurrence2_date`

#### Scenario: Death before second recurrence censors PFS-2
- **WHEN** a patient has first recurrence treatment and dies before second recurrence
- **THEN** `pfs2_event` is `0`
- **AND** PFS-2 time ends at `dod`

#### Scenario: No second recurrence censors at last known alive date
- **WHEN** a patient has first recurrence treatment and no second recurrence or prior death
- **THEN** `pfs2_event` is `0`
- **AND** PFS-2 time ends at `last_known_alive_date`

#### Scenario: PFS-2 contract catches valid-looking tampering
- **WHEN** `pfs2_event`, `tt_pfs2_months`, or `tt_pfs2_years` remain in-domain but no longer match the source-date contract
- **THEN** Objective 0 validation or regression tests fail with row-level detail

### Requirement: Objective 4 GEP SHALL have a compact derivation contract
Objective 0 MUST maintain a compact Objective 4 GEP derivation contract for imported GEP probabilities and horizon endpoints. General presence/domain registry checks are necessary but insufficient for Objective 4 endpoint validity.

The contract MUST cover 5-, 7-, and 10-year MFS and MSS expected survival probabilities, predicted risk complements, horizon event indicators, competing-risk event types, clipped horizon times, endpoint-specific eligibility flags, and `gep_validation_set`.

#### Scenario: GEP expected survival and predicted risks match source probabilities
- **WHEN** Objective 0 derives GEP expected survival and predicted risk fields
- **THEN** `expected_mfs_5yr` equals `biopsy1_gep_mfs`, 7- and 10-year expected MFS are derived from the 5-year MFS probability, and predicted MFS risk fields equal `1 - expected_mfs_*`
- **AND** `expected_mss_5yr` equals `biopsy1_gep_mss`, 7- and 10-year expected MSS are derived from the 5-year MSS probability, and predicted MSS risk fields equal `1 - expected_mss_*`

#### Scenario: GEP horizon event types and times match source event data
- **WHEN** Objective 0 derives MFS and MSS horizon fields
- **THEN** MFS event/type/time fields match metastasis and competing-death source data at 5, 7, and 10 years
- **AND** MSS event/type/time fields match melanoma-death and competing-death source data at 5, 7, and 10 years
- **AND** the contract explicitly preserves the existing unit convention: `tt_mfs_*` horizons are in months and `tt_mss_*` horizons are in years

#### Scenario: GEP eligibility and availability labels cannot drift
- **WHEN** Objective 0 derives GEP eligibility and availability fields
- **THEN** `mfs_analysis_eligible` and `mss_analysis_eligible` match the endpoint-specific source probability, definitive-GEP, observed endpoint, and nonnegative-time rules
- **AND** `gep_validation_set` uses only `Eligible` and `No GEP Data`
- **AND** retired Training/Testing split labels are not reintroduced

### Requirement: Config constants SHALL be modularized behind one public entry point
The oversized `scripts/utils/config_constants.R` file MUST be split into focused private config modules under `scripts/config/` while preserving `scripts/utils/config_constants.R` as the single public config entry point sourced by `scripts/load_all.R`.

Downstream workflow, analysis, table, visualization, GEP, and test code MUST NOT source individual config modules directly. After `config_constants.R` is sourced, existing public config objects MUST remain available under their established names unless a rename is explicitly part of the Objective 0 contract cleanup and tests/docs are updated accordingly.

The private module folder MUST NOT live under `scripts/utils/`. `scripts/utils/config_constants.R` may remain in `utils` only as the compatibility entry point because existing workflow, docs, and tests already treat it as the public config source.

#### Scenario: Workflow still sources one config entry point
- **WHEN** `scripts/load_all.R` loads project configuration
- **THEN** it sources `scripts/utils/config_constants.R`
- **AND** `load_all.R` does not need to know the internal config module layout

#### Scenario: Core source-file and exclusion settings are easy to locate
- **WHEN** a maintainer needs to review the raw input filename, raw/export paths, specific patient exclusions, or manual date corrections
- **THEN** those settings are placed in focused `scripts/config/` modules with names that make their ownership obvious
- **AND** `config_constants.R` documents the module order and public entry-point contract

#### Scenario: Config module load order is regression-tested
- **WHEN** the test suite sources `scripts/load_all.R` or `scripts/utils/config_constants.R`
- **THEN** required config objects for Objective 0 validation, Objective 2 toxicity derivation, Objective 4 GEP policy, labels, and path assertions are available
- **AND** no downstream script must source a private config module to run successfully
- **AND** no runtime code references `scripts/utils/config/` as the private module location

### Requirement: P5 implementation SHALL receive an organization and readability review before archive
Before P5 is archived, the final implementation MUST be reviewed for legibility, concision, and good organization in addition to functional correctness.

The review MUST verify that config modules have clear ownership, Objective 0 contracts and manifests are named according to their scope, validation messages are explicit without unnecessary repetition, tests prove meaningful contract drift without excessive fixture bloat, and documentation describes current behavior directly.

#### Scenario: Final P5 review blocks disorganized contract changes
- **WHEN** P5 implementation is complete and tests pass
- **THEN** a final review checks the config layout, Objective 0 contract names, validation output wording, test fixture size, and documentation alignment
- **AND** P5 is not archived until review findings are resolved or explicitly deferred into a new spec

