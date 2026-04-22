# objective0-contract-regression-testing Specification

## Purpose
TBD - created by archiving change p5-harden-objective0-validation-contracts. Update Purpose after archive.
## Requirements
### Requirement: Objective 0 SHALL have regression tests for rebuild and reload contract parity
The test suite MUST verify that recreate and non-recreate execution paths publish the same required validation artifact classes and provenance fields.

#### Scenario: Reload path preserves required artifact classes
- **WHEN** the Objective 0 test harness exercises a non-recreate validation run
- **THEN** the resulting artifact bundle contains the same required audit sheets and outputs as the recreate-path contract, except where provenance intentionally differs

### Requirement: Objective 0 SHALL have regression tests for chronology enforcement
The test suite MUST lock the expected behavior for chronology hard failures without reintroducing the retired Objective 4 Training/Testing split as a fatal preprocessing contract.

#### Scenario: Chronology regression test fails closed
- **WHEN** a fixture contains impossible endpoint chronology
- **THEN** the test asserts that Objective 0 does not report validation success and does publish a hard-failure artifact

#### Scenario: Retired GEP split-shape contract is not reintroduced
- **WHEN** Objective 0 validates a full-cohort fixture with analyzable GEP rows
- **THEN** the test does not require Training/Testing split-shape enforcement as a fatal validation condition

### Requirement: Objective 0 SHALL have regression tests for downstream input contracts
The test suite MUST verify that Objective 0 evaluates registered objective-input contracts and reports invalid objective-critical fields before downstream analyses run.

#### Scenario: Invalid objective input is caught at Objective 0
- **WHEN** an analytic cohort fixture contains an included invalid value for a registered downstream objective input
- **THEN** Objective 0 validation reports an objective-input finding that identifies the affected objective and variable
- **AND** the downstream objective is not required to recode or silently repair that value

### Requirement: Objective 0 SHALL have regression tests for factor-coercion drift
The test suite MUST verify that canonical factor coercion and releveling do not drift back into ad hoc downstream analysis code.

#### Scenario: Ad hoc downstream factor coercion is detected
- **WHEN** the factor-level audit scans analysis scripts
- **THEN** it fails on risky `as.factor()` usage or unscoped factor construction for objective-critical variables
- **AND** it permits only central-helper, non-mutating package-interface coercion that preserves declared levels

