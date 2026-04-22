## ADDED Requirements

### Requirement: Objective 1 subgroup documentation and runtime artifacts SHALL match the stable runtime contract
Objective 1 MUST document subgroup tabular outputs as consolidated multi-sheet Excel diagnostics workbooks emitted by the workflow, with subgroup forest plots and interaction RDS objects as companion artifacts. Objective 1 MUST NOT require per-subgroup workbook or HTML artifact proliferation to satisfy the subgroup contract.

#### Scenario: Stable subgroup contract is documented explicitly
- **WHEN** Objective 1 documents subgroup outputs
- **THEN** the docs and summary text identify consolidated Excel diagnostics workbooks, forest plots, and interaction RDS objects as the subgroup artifact contract

#### Scenario: HTML previews are ancillary if retained
- **WHEN** the existing subgroup formatter emits per-subgroup HTML previews
- **THEN** Objective 1 MAY mention them as ancillary previews
- **AND** those previews are not required as the primary tabular subgroup contract when the consolidated Excel diagnostics workbooks are present

### Requirement: Objective 1 subgroup outputs SHALL remain exploratory
Objective 1 subgroup outputs MUST include language or metadata indicating that they are exploratory support analyses, especially in sparse-support cohorts.

#### Scenario: Sparse cohort subgroup outputs are labeled exploratory
- **WHEN** Objective 1 writes subgroup outputs for the GKSRS-only cohort or another sparse-support setting
- **THEN** those outputs include exploratory labeling and avoid confirmatory interaction language
