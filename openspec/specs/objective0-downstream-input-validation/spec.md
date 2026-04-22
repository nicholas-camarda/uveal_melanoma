# objective0-downstream-input-validation Specification

## Purpose
TBD - created by archiving change p5-harden-objective0-validation-contracts. Update Purpose after archive.
## Requirements
### Requirement: Objective 0 SHALL validate downstream objective input contracts
Objective 0 MUST maintain a compact validation contract for important downstream objective inputs. The contract MUST identify required source fields, required derived fields, expected value domains, missingness rules, and severity for Objectives 1, 2, 3, and 4 before downstream analysis code consumes those inputs.

The registered inputs MUST be limited to endpoint-definition variables, cohort-eligibility variables, core adjustment variables, and required prognostic/prediction fields. Objective 0 MUST NOT expand this contract into a full reporting-variable census or duplicate downstream model diagnostics.

The initial registry MUST include, or explicitly justify excluding after implementation review, the following starter fields:

- Shared/core: `id`, `treatment_group`, `age_at_diagnosis`, `sex`, and configured objective-specific confounders.
- Objective 1: `recurrence1`, `recurrence_event`, `tt_recurrence_months`, `tt_recurrence_months_analysis`, `mets_progression`, `mets_event`, `tt_mets_months`, `tt_mets_months_analysis`, `death_event`, `tt_death_months`, `tt_death_months_analysis`, `pfs_event`, `tt_pfs_months`, `tt_pfs_months_analysis`.
- Objective 2: `vision_change`, `initial_vision`, `last_vision`, `recurrence1_pretreatment_vision`, `retinopathy`, `nvg`, `srd`, `retinopathy_burden_event`, `nvg_burden_event`, `srd_burden_event`.
- Objective 3: `recurrence1`, `recurrence1_treatment_date`, `recurrence2`, `recurrence2_date`, `pfs2_event`, `tt_pfs2_months`, `tt_pfs2_years`, `recurrence1_treatment_clean`, `dod`, `last_known_alive_date`.
- Objective 4: `biopsy1_gep`, `gep_class_simple`, `prame_status`, `gep12_prame_status`, `biopsy1_gep_mfs`, `biopsy1_gep_mss`, expected and predicted MFS/MSS risk fields at 5/7/10 years, MFS/MSS eligibility flags, MFS/MSS event fields at 5/7/10 years, MFS/MSS competing-risk status fields at 5/7/10 years, and MFS/MSS time fields at 5/7/10 years.

Implementation MAY use any number of subagents to audit the live Objective 1-4 code paths, tests, and documentation before finalizing the registry.

#### Scenario: Objective input contract is evaluated during validation
- **WHEN** Objective 0 validates analytic cohorts
- **THEN** it evaluates the registered important inputs for each downstream objective
- **AND** validation findings identify the objective, variable, expected domain or missingness rule, severity, and affected row count when a check fails

#### Scenario: Registry fields are reviewed against live code paths
- **WHEN** P5 is implemented
- **THEN** the starter registry is reviewed against Objective 1-4 analysis entry points and helper functions
- **AND** any added or omitted registry field is justified in the implementation notes or tests
- **AND** distributed subagent review may be used without limiting the number of subagents assigned to this audit

#### Scenario: Objective 2 toxicity fields are covered by the Objective 0 contract
- **WHEN** Objective 0 validates analytic cohorts used by Objective 2
- **THEN** retinopathy, NVG, and SRD are checked for presence and allowed `Y`/`N` values among included analytic rows
- **AND** included missing, blank, or non-`Y`/`N` values are reported before Objective 2 runs

### Requirement: Objective 0 SHALL separate data-readiness validation from model diagnostics
Objective 0 MUST validate whether objective inputs are present, well-formed, and safe to consume. Downstream objectives MUST continue to own model feasibility, statistical assumption checks, censoring support, event support, and skip artifacts.

#### Scenario: Model feasibility remains downstream
- **WHEN** an objective has valid input variables but sparse event support or violated model assumptions
- **THEN** Objective 0 validation may still pass
- **AND** the downstream objective reports the sparse-support or assumption issue through its existing diagnostics and skip-artifact mechanisms

### Requirement: Objective 0 SHALL not carry catch-all cohort state into analytic datasets
Objective 0 MUST NOT preserve `consort_group == "other"` as a model-facing analytic cohort state. Cohort assignment MUST use explicit, named states for analyzable cohorts and handle unclassifiable rows through existing removal logs or validation findings.

Rows that fail cohort assignment because required cohort-defining source fields are missing, inconsistent, or outside the explicit eligibility contract MUST be reported explicitly. They MAY be excluded before analytic cohort creation with a clear removal reason, or they MAY trigger a hard validation failure when manual source-data review is required. They MUST NOT remain in the full analytic cohort under an ambiguous `other` label.

#### Scenario: Missing cohort-defining values do not create an analyzable other group
- **WHEN** a row has treatment information but cannot be assigned to `eligible_both` or `gksrs_only` because a cohort-defining field such as `optic_nerve` is missing
- **THEN** Objective 0 reports the row through existing removal or validation artifacts
- **AND** downstream analytic cohorts do not contain `consort_group == "other"`

#### Scenario: Full cohort remains analytically explicit
- **WHEN** Objective 0 writes `uveal_melanoma_full_cohort.rds`
- **THEN** every included row has an explicit analyzable `consort_group` value or the workflow fails closed before downstream analyses run
- **AND** no downstream objective has to weaken input contracts to accommodate catch-all cohort rows

### Requirement: Canonical factor coercion SHALL be centralized and audited
Objective-critical factor levels, display domains, and value coercions MUST be defined in data preparation, central constants/helpers, or Objective 0 validation. Downstream analysis scripts MUST NOT introduce ad hoc factor coercion or releveling that changes an objective-critical variable's analytic domain.

#### Scenario: Package-interface factor coercion is allowed only through central helpers
- **WHEN** a downstream model or estimator requires a temporary factor-typed input
- **THEN** the analysis code may use an approved central helper for non-mutating type adaptation
- **AND** the coercion does not recode values, change endpoint meaning, or write back to the prepared analytic cohort
- **AND** the site is covered by factor-coercion audit tests

#### Scenario: Display-only factor ordering is not treated as data derivation
- **WHEN** plotting, table display, or synthetic tests use factor construction only to order already-derived labels
- **THEN** the audit may permit the site when it is not redefining an objective-critical analytic variable
- **AND** objective-critical variables remain governed by the central data-preparation and validation contract

#### Scenario: Ad hoc factor construction is rejected
- **WHEN** downstream analysis code introduces `as.factor()` or unscoped `factor()` construction for an objective-critical variable
- **THEN** regression tests fail unless the site is moved upstream or replaced with an approved central helper and documented as model-interface-only

### Requirement: Objective 0 SHALL publish objective-input validation without workbook bloat
Objective 0 validation outputs MUST publish objective-input findings through the existing validation bundle/reporting surfaces. A single compact objective-input sheet or existing validation-findings rows MAY be used, but Objective 0 MUST NOT create per-objective raw-data audit workbooks for this contract.

#### Scenario: Objective-input findings are reviewable in existing artifacts
- **WHEN** Objective 0 writes the validation bundle
- **THEN** objective-input validation findings are visible in the bundle with the affected objective and variable
- **AND** no new per-objective raw-data audit workbook family is required

