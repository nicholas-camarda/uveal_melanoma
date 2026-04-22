## ADDED Requirements

### Requirement: Objective 2 SHALL consume Objective 0-validated complication endpoints
Objective 0 MUST validate retinopathy, NVG, and SRD as Objective 2 toxicity endpoint inputs for included analytic rows. Objective 2 complication tables, models, and summaries MUST consume validated/prepared binary burden fields rather than recoding raw complication source fields inside model-specific analysis code.

The current source-data review found no missing or non-`Y`/`N` retinopathy, NVG, or SRD values in the included analytic cohorts. Future missing, blank, or non-`Y`/`N` values in included analytic rows MUST be surfaced by Objective 0 validation and MUST NOT be silently converted to non-events by Objective 2.

#### Scenario: Objective 0 validates toxicity endpoint inputs
- **WHEN** Objective 0 prepares analytic cohorts for Objective 2
- **THEN** retinopathy, NVG, and SRD are checked for presence and allowed `Y`/`N` values among included analytic rows
- **AND** any included missing, blank, or non-`Y`/`N` value is reported in the Objective 0 validation artifacts

#### Scenario: Objective 2 uses prepared toxicity burden fields
- **WHEN** Objective 2 computes retinopathy, NVG, or SRD rates or models
- **THEN** the analysis uses the Objective 0-validated/prepared burden field for that endpoint
- **AND** Objective 2 model and rate-generation code does not locally map raw `Y`, `N`, missing, or unexpected source values into events/non-events
- **AND** Objective 2 diagnostics keep denominator and model-status reporting concise without adding broad raw-data audit sheets

### Requirement: Objective 2 SHALL fail softly under sparse support
Objective 2 MUST publish explicit skip artifacts instead of hard-stopping when sparse support or degenerate data make a fitted model unreliable.

#### Scenario: Degenerate vision test produces a skip artifact
- **WHEN** the vision comparison has insufficient support for the configured inferential test
- **THEN** Objective 2 writes an explicit skip artifact instead of stopping execution with an unhandled error

### Requirement: Objective 2 ordinal outputs SHALL state whether the proportional-odds assumption was assessed
Objective 2 ordinal vision outputs MUST explicitly note that the proportional-odds assumption was not formally tested unless a future change adds a tested assumption diagnostic.

#### Scenario: Ordinal output includes assumption status
- **WHEN** Objective 2 publishes an ordinal Snellen model result
- **THEN** the diagnostic or summary output states that the proportional-odds assumption was not formally tested

### Requirement: Objective 2 simulated descriptive p-values SHALL be reproducible
Objective 2 categorical descriptive summaries that use simulated Fisher p-values MUST use a fixed, locally scoped random seed so rerunning the same unchanged data does not change reader-facing p-value notes.

#### Scenario: Simulated Fisher p-value is stable across reruns
- **WHEN** Objective 2 computes a simulated Fisher p-value for a categorical descriptive summary
- **THEN** repeated runs on the same data produce the same displayed p-value
- **AND** the seed does not perturb unrelated random-number state outside the scoped calculation
