# objective1-cohort-interpretation-guardrails Specification

## Purpose
Define the active Objective 1 interpretation guardrails for cohort roles, proportional-hazards diagnostics, and valid survival grouping variables.

## Requirements
### Requirement: Objective 1 SHALL use a centralized cohort-interpretation note
Objective 1 high-level reader-facing summaries MUST include a centralized cohort-interpretation note that identifies the restricted cohort as the primary dual-eligible comparative cohort, the full cohort as real-world associational context, and the GKSRS-only cohort as characterization or exploratory support. This requirement does not require duplicating the note in every plot, model table, HTML regression output, or low-level diagnostic artifact.

#### Scenario: High-level summaries include cohort interpretation
- **WHEN** Objective 1 writes a high-level reader-facing summary or narrative artifact
- **THEN** the artifact includes or links to the centralized cohort-interpretation note

#### Scenario: Low-level artifacts are not burdened with repeated boilerplate
- **WHEN** Objective 1 writes individual plots, model tables, HTML regression outputs, or diagnostics
- **THEN** those artifacts are not required to repeat the full cohort-interpretation note unless they already have a centralized notes or metadata mechanism

### Requirement: Objective 1 Cox-based survival summaries SHALL use graded PH interpretation
Objective 1 MUST interpret Cox hazard ratios according to proportional-hazards diagnostic severity rather than treating every p-value below 0.05 as an automatic full demotion. When PH concerns are mild or borderline, Cox HRs may remain prominent with cautionary language and RMST/KM context. When PH violations are material and RMST/KM outputs are available, reader-facing summaries MUST lead with RMST/KM or other non-PH-sensitive estimates and downgrade the single Cox HR as secondary or time-compressed. This interpretation metadata MUST be incorporated into existing survival summaries, PH diagnostic summaries, effect-summary notes, or centralized Objective 1 summaries instead of creating a separate reporting surface.

#### Scenario: Mild PH concern triggers caution rather than full demotion
- **WHEN** Objective 1 PH diagnostics show only mild or borderline PH concern
- **THEN** the Cox HR may remain prominent
- **AND** the summary includes cautionary language and points to RMST/KM context

#### Scenario: Material PH violation triggers RMST-first language
- **WHEN** Objective 1 PH diagnostics indicate a material PH violation and RMST/KM outputs are available
- **THEN** the primary summary and narrative lead with RMST, KM, or other non-PH-sensitive estimates
- **AND** the single Cox HR is explicitly labeled as secondary, time-compressed, or PH-limited

#### Scenario: PH cannot be tested
- **WHEN** Objective 1 PH diagnostics cannot be run because model or event support is inadequate
- **THEN** the summary avoids claiming PH support and interprets any fitted Cox HR cautiously with event-support and RMST/KM context where available

#### Scenario: PH interpretation uses existing artifacts
- **WHEN** Objective 1 writes graded PH interpretation
- **THEN** it amends existing survival effect summaries, proportional-hazards summary text, RMST summaries, or centralized Objective 1 summaries where feasible
- **AND** it does not create additional PH/RMST interpretation workbooks, folders, or sidecar reports solely for this note

### Requirement: Objective 1 SHALL NOT group OS or PFS by eventual endpoint status
Objective 1 MUST NOT generate OS or PFS analyses grouped by eventual local-recurrence or metastatic-progression status. These downstream endpoint-component groups are circular and can introduce immortal-time bias; warning labels do not make the analyses valid.

#### Scenario: Objective 1 writes survival outputs
- **WHEN** Objective 1 writes OS or PFS curves, models, summaries, or diagnostics
- **THEN** the workflow does not use eventual local-recurrence or metastatic-progression status as the grouping variable
- **AND** it does not create dedicated output routes or artifacts for those groupings
