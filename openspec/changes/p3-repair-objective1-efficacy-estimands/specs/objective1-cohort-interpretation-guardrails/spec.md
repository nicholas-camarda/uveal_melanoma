## ADDED Requirements

### Requirement: Objective 1 SHALL use a centralized cohort-interpretation note
Objective 1 high-level reader-facing summaries MUST include a centralized cohort-interpretation note that identifies the restricted cohort as the primary dual-eligible comparative cohort, the full cohort as real-world associational context, and the GKSRS-only cohort as characterization or exploratory support. This requirement does not require duplicating the note in every plot, model table, HTML regression output, or low-level diagnostic artifact.

#### Scenario: High-level summaries include cohort interpretation
- **WHEN** Objective 1 writes a high-level reader-facing summary or narrative artifact
- **THEN** the artifact includes or links to the centralized cohort-interpretation note

#### Scenario: Low-level artifacts are not burdened with repeated boilerplate
- **WHEN** Objective 1 writes individual plots, model tables, HTML regression outputs, or diagnostics
- **THEN** those artifacts are not required to repeat the full cohort-interpretation note unless they already have a centralized notes or metadata mechanism

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
