## ADDED Requirements

### Requirement: Objective 1 recurrence and metastatic progression SHALL use event-time analyses
Objective 1 MUST NOT present recurrence or metastatic progression treatment comparisons as primary ever-event logistic analyses when event-time variables are available.

#### Scenario: Recurrence comparison uses explicit time-to-event inputs
- **WHEN** Objective 1 computes recurrence treatment comparisons
- **THEN** it uses the derived recurrence time variable and a documented event-time estimand rather than a crude ever-event logistic model

#### Scenario: Metastatic progression comparison uses explicit time-to-event inputs
- **WHEN** Objective 1 computes metastatic progression treatment comparisons
- **THEN** it uses the derived metastasis time variable and a documented event-time estimand rather than a crude ever-event logistic model

### Requirement: Objective 1 SHALL handle death consistently with the selected event-time estimand
Objective 1 recurrence and metastatic progression reporting MUST state and implement whether death is treated as a competing event or as censoring, and that handling MUST match the selected estimand.

#### Scenario: Cumulative-incidence path documents competing death
- **WHEN** the selected estimand is cumulative incidence
- **THEN** Objective 1 treats death before the event of interest as a competing event and reports outputs consistent with that choice

#### Scenario: Cause-specific hazard path documents censoring and companion summaries
- **WHEN** the selected estimand is cause-specific hazard
- **THEN** Objective 1 states that non-index deaths are censored in the model and provides companion absolute-risk summaries so the interpretation remains anchored
