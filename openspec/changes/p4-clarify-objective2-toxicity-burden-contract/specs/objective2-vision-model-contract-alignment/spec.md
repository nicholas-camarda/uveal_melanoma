## ADDED Requirements

### Requirement: Objective 2 vision-model documentation SHALL match the implemented adjustment set
Objective 2 MUST NOT claim baseline-adjusted vision modeling unless baseline vision is included in the fitted model.

#### Scenario: Baseline-adjusted path includes baseline vision
- **WHEN** Objective 2 documentation states that the vision model is baseline-adjusted
- **THEN** the fitted model includes baseline vision as a covariate and the tests lock that inclusion

#### Scenario: Change-score path is labeled honestly
- **WHEN** Objective 2 continues to fit vision change without baseline vision in the adjustment set
- **THEN** the docs and outputs describe it as a change-score model with the true covariates

### Requirement: Objective 2 vision endpoint wording SHALL reflect measurement timing
Objective 2 MUST describe the vision endpoint as the actual implemented measurement construct, including pre-salvage measurement for recurrent eyes if that remains in place.

#### Scenario: Mixed timing endpoint is described explicitly
- **WHEN** the vision endpoint uses last-available vision for some eyes and pre-salvage vision for recurrent eyes
- **THEN** the output wording states that mixed timing explicitly
