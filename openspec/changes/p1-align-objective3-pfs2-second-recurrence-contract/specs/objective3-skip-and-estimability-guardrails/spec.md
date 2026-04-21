## ADDED Requirements

### Requirement: Objective 3 SHALL publish skip artifacts for all sparse-data exits
Objective 3 MUST write explicit skip artifacts for both low-patient and low-event sparse-data branches.

#### Scenario: Low-patient branch publishes skip bundle
- **WHEN** Objective 3 has fewer than the configured minimum analyzable patients
- **THEN** it writes the same class of skip explanation, diagnostics, and status artifacts used by the low-event branch

#### Scenario: Low-event branch publishes skip bundle
- **WHEN** Objective 3 has too few events for fitted modeling
- **THEN** it writes the documented skip artifacts and does not leave the output folder ambiguous

### Requirement: Objective 3 SHALL block non-estimable treatment Cox outputs
Objective 3 MUST NOT present a fitted Cox treatment result as reportable when the treatment comparison is non-estimable.

#### Scenario: Zero-event reference arm downgrades fitted output
- **WHEN** the configured reference treatment arm has zero events
- **THEN** Objective 3 suppresses or explicitly downgrades the fitted treatment comparison instead of publishing it as a normal completed result

#### Scenario: Fully filtered treatment contrasts downgrade fitted output
- **WHEN** all primary treatment contrasts are removed as extreme or non-estimable
- **THEN** Objective 3 marks the fitted treatment analysis as non-reportable
