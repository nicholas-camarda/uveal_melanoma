## Purpose
Define the Objective 2 vision-model reporting contract so documentation and outputs match the implemented change-score analysis.

## Requirements

### Requirement: Objective 2 vision-model documentation SHALL match the implemented adjustment set
Objective 2 MUST NOT claim baseline-adjusted vision modeling unless baseline vision is included in the fitted model.

#### Scenario: Change-score path is labeled honestly
- **WHEN** Objective 2 fits vision change without baseline vision in the adjustment set
- **THEN** the docs and outputs describe it as a change-score model with the true covariates
- **AND** tests lock that baseline vision is not represented as an included adjustment covariate unless the fitted formula changes in a future spec

### Requirement: Objective 2 vision endpoint wording SHALL reflect measurement timing
Objective 2 MUST describe the vision endpoint as the actual implemented measurement construct, including pre-salvage measurement for recurrent eyes if that remains in place.

#### Scenario: Mixed timing endpoint is described explicitly
- **WHEN** the vision endpoint uses last-available vision for some eyes and pre-salvage vision for recurrent eyes
- **THEN** the output wording states that mixed timing explicitly
