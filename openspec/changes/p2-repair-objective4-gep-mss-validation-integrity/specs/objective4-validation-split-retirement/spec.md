## ADDED Requirements

### Requirement: Objective 4 SHALL retire Training/Testing as an active validation contract
Objective 4 MUST NOT require or present `Training` / `Testing` split membership as a primary validation mechanism for imported GEP probabilities.

#### Scenario: Objective 4 refresh preserves eligibility without split claims
- **WHEN** Objective 4 refreshes GEP eligibility flags before analysis
- **THEN** it may classify rows as analyzable or non-analyzable for GEP validation
- **AND** it does not require a Training/Testing partition to run primary MFS or MSS validation

#### Scenario: Reader-facing outputs omit active split interpretation
- **WHEN** Objective 4 writes workbooks, summaries, or narrative interpretation
- **THEN** the output does not describe Training/Testing as driving the primary GEP validation
- **AND** any residual split-like field is labeled as retired or non-primary metadata

### Requirement: Objective 0 and Objective 4 tests SHALL not enforce retired split shape
Tests MUST stop treating the historical Objective 4 Training/Testing split as a fatal preprocessing or validation contract.

#### Scenario: Full-cohort GEP validation does not depend on split shape
- **WHEN** a full-cohort fixture has analyzable imported GEP probabilities and definitive GEP labels
- **THEN** primary Objective 4 validation can proceed without requiring both Training and Testing labels
