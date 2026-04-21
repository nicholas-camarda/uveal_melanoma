## ADDED Requirements

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
