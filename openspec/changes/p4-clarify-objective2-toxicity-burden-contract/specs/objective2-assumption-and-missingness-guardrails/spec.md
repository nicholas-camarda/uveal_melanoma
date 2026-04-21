## ADDED Requirements

### Requirement: Objective 2 SHALL apply one explicit missingness policy across complication outputs
Objective 2 complication tables, models, and summaries MUST use the same documented missingness policy for retinopathy, NVG, and SRD.

#### Scenario: Missing complication values do not drift across artifact types
- **WHEN** complication outcomes contain missing values
- **THEN** the HTML summaries, workbooks, and model denominators remain consistent with one declared missingness policy

### Requirement: Objective 2 SHALL fail softly under sparse support
Objective 2 MUST publish explicit skip artifacts instead of hard-stopping when sparse support or degenerate data make a fitted model unreliable.

#### Scenario: Degenerate vision test produces a skip artifact
- **WHEN** the vision comparison has insufficient support for the configured inferential test
- **THEN** Objective 2 writes an explicit skip artifact instead of stopping execution with an unhandled error

### Requirement: Objective 2 ordinal outputs SHALL state whether the proportional-odds assumption was assessed
Objective 2 ordinal vision outputs MUST either include a proportional-odds assessment or explicitly note that the assumption was not tested.

#### Scenario: Ordinal output includes assumption status
- **WHEN** Objective 2 publishes an ordinal Snellen model result
- **THEN** the diagnostic or summary output states whether the proportional-odds assumption was assessed and what the outcome was
