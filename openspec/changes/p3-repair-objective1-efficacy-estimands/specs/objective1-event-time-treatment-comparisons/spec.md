## ADDED Requirements

### Requirement: Objective 1 recurrence and metastatic progression SHALL use binary and cumulative-incidence co-primary estimands
Objective 1 MUST preserve collaborator-requested binary recurrence and metastatic-progression comparisons and MUST add competing-risk cumulative incidence as co-primary follow-up-aware evidence when event-time variables are available. Objective 1 MUST NOT present crude ever-event logistic analyses as the sole primary evidence for these endpoints when event-time variables are available.

#### Scenario: Recurrence comparison includes binary and cumulative-incidence outputs
- **WHEN** Objective 1 computes recurrence treatment comparisons
- **THEN** it reports the binary recurrence comparison requested by the project objective
- **AND** it reports cumulative incidence using the derived recurrence time variable and documented competing-risk event handling

#### Scenario: Metastatic progression comparison includes binary and cumulative-incidence outputs
- **WHEN** Objective 1 computes metastatic progression treatment comparisons
- **THEN** it reports the binary metastatic-progression comparison requested by the project objective
- **AND** it reports cumulative incidence using the derived metastasis time variable and documented competing-risk event handling

#### Scenario: Co-primary outputs are labeled by estimand
- **WHEN** Objective 1 writes recurrence or metastatic-progression summaries
- **THEN** binary outputs are labeled as ever-observed event/rate comparisons over available follow-up
- **AND** cumulative-incidence outputs are labeled as time-horizon event probabilities accounting for censoring and competing death

### Requirement: Objective 1 SHALL handle death consistently with each co-primary estimand
Objective 1 recurrence and metastatic progression reporting MUST state and implement death handling for both co-primary estimands. The cumulative-incidence lane MUST treat death before the event of interest as a competing event. The binary lane MUST state that it is an ever-observed event comparison over available follow-up and is not the censoring-aware probability of event by a horizon.

#### Scenario: Cumulative-incidence lane documents competing death
- **WHEN** Objective 1 reports cumulative incidence for recurrence or metastatic progression
- **THEN** Objective 1 treats death before the event of interest as a competing event and reports outputs consistent with that choice

#### Scenario: Binary lane does not claim censoring-aware probability
- **WHEN** Objective 1 reports binary recurrence or metastatic-progression comparisons
- **THEN** it avoids presenting that binary estimate as a censoring-aware event probability by a fixed horizon
- **AND** it points readers to the cumulative-incidence lane for horizon-specific probability interpretation

### Requirement: Objective 1 SHALL keep cause-specific or Fine-Gray models secondary when emitted
Objective 1 MAY emit cause-specific Cox or Fine-Gray regression outputs for recurrence or metastatic progression when model support is adequate, but those outputs MUST be labeled as model-based secondary or technical evidence rather than replacing the co-primary binary and cumulative-incidence summaries.

#### Scenario: Secondary competing-risk model is estimable
- **WHEN** Objective 1 emits a Fine-Gray or cause-specific model for recurrence or metastatic progression
- **THEN** the output states the model estimand and keeps the co-primary binary and cumulative-incidence summaries visible
