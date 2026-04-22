## ADDED Requirements

### Requirement: Objective 2 complication outputs SHALL declare recorded burden by follow-up
Objective 2 complication outputs MUST state that retinopathy, NVG, and SRD analyses estimate recorded toxicity burden by available follow-up, not time-to-toxicity incidence, unless a future source-data expansion adds reliable onset dates.

#### Scenario: Burden-style complication output is labeled explicitly
- **WHEN** Objective 2 uses ever-recorded complication indicators without onset-time modeling
- **THEN** the outputs label those results as burden or follow-up summaries rather than comparative incidence estimates

#### Scenario: Time-to-toxicity language is blocked without onset dates
- **WHEN** Objective 2 uses the current binary retinopathy, NVG, or SRD source fields
- **THEN** the outputs do not describe those results as time-to-toxicity incidence estimates

### Requirement: Objective 2 SHALL not use stronger causal language than the estimand supports
Objective 2 summaries MUST avoid treatment-effect or incidence language that exceeds the selected estimand.

#### Scenario: Burden output avoids incidence phrasing
- **WHEN** complication analyses are still burden-style summaries
- **THEN** the narrative avoids phrasing that implies equal risk time or causal toxicity incidence
