## ADDED Requirements

### Requirement: Objective 4 narrative strength SHALL degrade when support is weak
Objective 4 reader-facing clinical interpretation MUST be conditional on follow-up support, extrapolation support, and calibration support.

#### Scenario: Unsupported extrapolation suppresses strong clinical-use language
- **WHEN** an Objective 4 horizon is marked unsupported or weakly supported for extrapolation
- **THEN** the reader-facing summary avoids strong claims such as suitability for treatment planning or direct patient counseling at that horizon while still keeping that horizon in the main output set

#### Scenario: Unavailable calibration suppresses confident calibration language
- **WHEN** calibration slopes are unavailable or unstable across the relevant timepoints
- **THEN** the narrative states that limitation explicitly and avoids claiming good direct-use calibration

### Requirement: Objective 4 reporting SHALL enforce interval and estimand sanity checks
Objective 4 MUST validate that printed confidence intervals are internally consistent and that unified comparison outputs include estimand metadata where cross-outcome comparisons could mislead.

#### Scenario: Invalid interval blocks publish
- **WHEN** a point estimate falls outside its printed confidence interval in a reader-facing Objective 4 output
- **THEN** the output generation fails or downgrades with an explicit QC error instead of publishing the contradiction

#### Scenario: Unified cross-outcome discrimination output carries estimand metadata
- **WHEN** Objective 4 stacks MFS and MSS discrimination summaries into one comparison view
- **THEN** the comparison includes explicit estimand notes that distinguish the two outcomes
