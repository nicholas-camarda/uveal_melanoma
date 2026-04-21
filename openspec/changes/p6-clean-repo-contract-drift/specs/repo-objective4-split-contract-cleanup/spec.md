## ADDED Requirements

### Requirement: Repo documentation SHALL retire the Objective 4 Training/Testing split contract
The repo MUST NOT present `gep_validation_set` or `Training` / `Testing` as a core active Objective 4 scientific contract.

#### Scenario: Retired split is removed from active docs
- **WHEN** repo docs describe Objective 4 validation inputs or output contracts
- **THEN** they do not describe Training/Testing as an active validation mechanism

#### Scenario: Residual field use is quarantined
- **WHEN** any residual output still exposes `gep_validation_set`
- **THEN** the docs label that field as retired or non-primary metadata and do not imply active analytical importance

### Requirement: Repo contract checks SHALL catch stale split language
The repo MUST include a lightweight check that prevents stale Objective 4 Training/Testing split wording from being reintroduced after cleanup.

#### Scenario: Stale split claim fails the doc contract check
- **WHEN** a doc reintroduces language implying that `Training` / `Testing` is a core active Objective 4 contract
- **THEN** the contract check fails and points to the stale wording
