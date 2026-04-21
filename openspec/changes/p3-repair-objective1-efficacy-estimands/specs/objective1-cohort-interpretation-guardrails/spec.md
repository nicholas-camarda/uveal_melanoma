## ADDED Requirements

### Requirement: Objective 1 SHALL distinguish comparative and associational cohort interpretations
Objective 1 summaries, tables, and narrative outputs MUST identify the restricted cohort as the primary comparative treatment cohort and MUST downgrade treatment language for the full and GKSRS-only cohorts.

#### Scenario: Restricted cohort carries primary comparative framing
- **WHEN** Objective 1 publishes treatment-effect summaries for the restricted cohort
- **THEN** the narrative may use comparative language consistent with the documented observational limits

#### Scenario: Full and GKSRS-only cohorts are labeled non-primary
- **WHEN** Objective 1 publishes treatment summaries for the full or GKSRS-only cohort
- **THEN** the narrative labels them as associational, characterization, or exploratory rather than primary comparative evidence

### Requirement: Objective 1 PFS summaries SHALL defer to RMST when PH fails
Objective 1 MUST NOT present the Cox hazard ratio as the lead interpretation for PFS when proportional-hazards diagnostics fail.

#### Scenario: PFS PH failure triggers RMST-first language
- **WHEN** Objective 1 PFS diagnostics indicate a proportional-hazards failure
- **THEN** the primary summary and narrative lead with RMST or other non-PH-sensitive estimates and explicitly downgrade the single HR interpretation

### Requirement: Legacy post-baseline outputs SHALL self-identify as exploratory
Objective 1 legacy recurrence-stratified and metastasis-stratified OS/PFS outputs MUST include artifact-level warnings that they are post-baseline exploratory analyses and not baseline treatment comparisons.

#### Scenario: Legacy folder contains explanation artifact
- **WHEN** Objective 1 writes a legacy post-baseline output bundle
- **THEN** the folder includes a stable note or summary artifact stating that the analysis is post-baseline and non-causal
