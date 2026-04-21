## ADDED Requirements

### Requirement: Objective 1 subgroup documentation and runtime artifacts SHALL match
Objective 1 MUST either emit the subgroup artifact types described in the docs or revise the docs so they accurately describe the produced artifacts.

#### Scenario: Documented subgroup tables are actually emitted
- **WHEN** the subgroup contract says table workbooks are produced
- **THEN** Objective 1 writes those table artifacts in the documented format

#### Scenario: Reduced subgroup contract is documented explicitly
- **WHEN** Objective 1 intentionally limits subgroup outputs to plots, diagnostics, or HTML previews
- **THEN** the docs and summary text state that reduced contract explicitly

### Requirement: Objective 1 subgroup outputs SHALL remain exploratory
Objective 1 subgroup outputs MUST include language or metadata indicating that they are exploratory support analyses, especially in sparse-support cohorts.

#### Scenario: Sparse cohort subgroup outputs are labeled exploratory
- **WHEN** Objective 1 writes subgroup outputs for the GKSRS-only cohort or another sparse-support setting
- **THEN** those outputs include exploratory labeling and avoid confirmatory interaction language
