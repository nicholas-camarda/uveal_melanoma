## Purpose
Retire Objective 4 Training/Testing split language from the active repo contract and prevent those labels from re-entering accepted runtime or documentation behavior.

## Requirements

### Requirement: Repo documentation SHALL retire the Objective 4 Training/Testing split contract
The repo MUST NOT present `gep_validation_set` or `Training` / `Testing` as a core active Objective 4 scientific contract.

#### Scenario: Retired split is removed from active docs
- **WHEN** repo docs describe Objective 4 validation inputs or output contracts
- **THEN** they do not describe Training/Testing as an active validation mechanism

#### Scenario: Residual field use is quarantined
- **WHEN** any residual output still exposes `gep_validation_set`
- **THEN** the docs label that field as retired or non-primary metadata and do not imply active analytical importance

### Requirement: Retired Training/Testing labels SHALL NOT re-enter accepted runtime behavior
The repo MUST NOT allow `Training` / `Testing` to silently re-enter active pipeline behavior, active docs, or accepted validation-contract values unless a future spec explicitly reintroduces them.

#### Scenario: Guardrails may reject retired labels without re-activating them
- **WHEN** tests or validation checks mention `Training` / `Testing`
- **THEN** those mentions exist only to reject retired labels or document retirement
- **AND** they do not treat those labels as accepted runtime values

#### Scenario: Accepted runtime contract blocks retired labels
- **WHEN** Objective 4 runtime or validation contracts define accepted `gep_validation_set` values
- **THEN** the accepted values exclude `Training` and `Testing`

#### Scenario: Exploratory no-GEP workflow uses neutral cohort-role language
- **WHEN** the exploratory no-GEP Objective 4 workflow defines cohort roles, workbook schema, or reader-facing summary labels
- **THEN** it uses neutral non-split terminology
- **AND** it does not emit fields such as `Training_Set` or analogous split-role labels

### Requirement: Repo contract checks SHALL catch stale split language
The repo MUST include a lightweight check that prevents stale Objective 4 Training/Testing split wording from being reintroduced after cleanup.

#### Scenario: Stale split claim fails the doc contract check
- **WHEN** a doc reintroduces language implying that `Training` / `Testing` is a core active Objective 4 contract
- **THEN** the contract check fails and points to the stale wording
