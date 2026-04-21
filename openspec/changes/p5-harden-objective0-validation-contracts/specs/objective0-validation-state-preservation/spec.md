## ADDED Requirements

### Requirement: Objective 0 SHALL preserve full audit state on reload-path validation
When Objective 0 runs with analytic dataset recreation disabled, the published validation bundle MUST still include the same reconciliation-detail classes that are present on a recreate run, including manual date correction detail when it exists.

#### Scenario: Reload path republishes prior audit detail
- **WHEN** Objective 0 validates an existing runtime cohort without rebuilding it from raw input
- **THEN** the published validation outputs include reconciliation summary detail, manual correction detail, and any persisted audit tables required by the recreate-path contract

#### Scenario: Missing prior audit content is surfaced explicitly
- **WHEN** Objective 0 cannot recover an expected persisted audit component during a reload-path run
- **THEN** it publishes an explicit validation issue describing the missing component instead of silently omitting it

### Requirement: Objective 0 outputs SHALL declare cohort provenance
Objective 0 validation outputs MUST identify whether a cohort was rebuilt from raw source data or revalidated from an existing runtime artifact.

#### Scenario: Rebuilt cohort provenance is published
- **WHEN** Objective 0 recreates an analytic cohort from source inputs
- **THEN** the validation outputs include provenance text or fields stating that the cohort was rebuilt from raw data

#### Scenario: Reloaded cohort provenance is published
- **WHEN** Objective 0 validates a previously saved runtime cohort
- **THEN** the validation outputs include provenance text or fields stating that the cohort was revalidated from an existing runtime artifact
