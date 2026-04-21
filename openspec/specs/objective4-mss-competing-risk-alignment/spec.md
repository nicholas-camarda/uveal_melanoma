# objective4-mss-competing-risk-alignment Specification

## Purpose
Define the active Objective 4 contract for melanoma-specific survival validation, with competing-risk cumulative incidence as the primary reader-facing estimand and legacy cause-specific metrics retained only as clearly labeled technical sidecars.

## Requirements
### Requirement: Objective 4 primary MSS validation metrics SHALL target competing-risk cumulative incidence
Objective 4 manuscript-facing MSS calibration, discrimination, and related validation summaries MUST align to a competing-risk cumulative-incidence target rather than a cause-specific binary horizon target.

#### Scenario: MSS primary calibration uses competing-risk-consistent methods
- **WHEN** Objective 4 reports primary MSS calibration summaries
- **THEN** those summaries are computed with methods that treat non-melanoma death as a competing event rather than simple censoring

#### Scenario: MSS primary discrimination uses competing-risk-consistent methods
- **WHEN** Objective 4 reports primary MSS discrimination summaries
- **THEN** those summaries evaluate the imported risk against a competing-risk-consistent target and document that estimand clearly

### Requirement: Objective 4 simple MSS outputs SHALL reuse the same corrected observed-risk logic as the primary MSS path
Objective 4 MUST align the reader-facing simple MSS summary to the corrected primary MSS observed-risk definition whenever feasible. Objective 4 MUST NOT publish a simple MSS summary that uses a different observed-risk definition from the corrected primary MSS validation path unless that output is explicitly labeled as QC-only.

#### Scenario: Simple MSS observed risk matches the primary MSS observed-risk definition
- **WHEN** Objective 4 publishes the simple 5-year MSS class summary
- **THEN** the observed 5-year MSS quantity is computed using the same competing-risk-aware observed-risk logic as the primary MSS validation path
- **AND** the summary remains eligible for reader-facing interpretation because it is a simplified view of the same estimand

#### Scenario: Divergent simple MSS logic is quarantined
- **WHEN** Objective 4 retains a simple MSS calculation that does not match the primary MSS observed-risk definition
- **THEN** that output is labeled QC-only and excluded from primary interpretation
- **AND** the output states why it could not be aligned to the primary competing-risk observed-risk logic

### Requirement: Objective 4 SHALL distinguish technical sidecars from primary MSS evidence
Objective 4 MUST label any cause-specific or legacy MSS metrics that remain available as technical or secondary outputs, not as the primary manuscript-facing validation result.

#### Scenario: Legacy MSS metric is demoted clearly
- **WHEN** Objective 4 publishes a non-primary MSS metric that does not align to the competing-risk primary estimand
- **THEN** the output labels it as technical or secondary and does not use it in the lead narrative
- **AND** the output preserves enough method metadata for readers to understand why it is a sidecar rather than primary evidence
