# objective3-skip-and-estimability-guardrails Specification

## Purpose
Define Objective 3 guardrails for sparse or non-estimable PFS-2 analyses, including explicit skip artifacts, censoring support, and downgrades when fitted treatment comparisons are not reportable.

## Requirements
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

### Requirement: Objective 3 SHALL report censoring support for PFS-2
Objective 3 PFS-2 outputs MUST report censoring and follow-up support so right-censoring-aware estimates are interpreted according to the observed data support.

#### Scenario: Fitted PFS-2 output includes censoring diagnostics
- **WHEN** Objective 3 publishes a fitted PFS-2 survival output
- **THEN** the output includes analyzable patient count, second-recurrence event count, censored count, and follow-up distribution overall and by treatment arm where feasible

#### Scenario: Heavy or imbalanced censoring downgrades interpretation
- **WHEN** censoring is heavy, follow-up is short relative to the reported horizon, or censoring is materially imbalanced across treatment groups
- **THEN** Objective 3 adds an explicit caution or downgrade to the fitted-output interpretation rather than treating convergence as sufficient support

#### Scenario: Skipped PFS-2 output includes censoring context
- **WHEN** Objective 3 skips PFS-2 modeling because of sparse data or non-estimability
- **THEN** the skip diagnostics include censoring and follow-up context so reviewers can distinguish low event incidence from inadequate observation time
