## Why

Objective 1 currently mixes strong treatment-comparison language with methods that do not consistently target a defensible comparative estimand. The biggest gaps are recurrence/metastasis being modeled only as crude ever-event logistic outcomes and output/reporting surfaces that blur comparative, associational, and exploratory analyses.

## What Changes

- Preserve collaborator-requested binary recurrence and metastatic-progression comparisons while adding competing-risk cumulative incidence as a co-primary, follow-up-aware estimand.
- Make the restricted cohort the primary comparative cohort and downgrade full-cohort and GKSRS-only treatment language to associational or exploratory where appropriate.
- Apply graded proportional-hazards interpretation to Objective 1 Cox-based survival summaries, using RMST/KM-first language only when PH violations are material and RMST/KM support is available.
- Make legacy post-baseline outputs and subgroup outputs self-identifying and contract-aligned.

## Capabilities

### New Capabilities
- `objective1-event-time-treatment-comparisons`: Objective 1 recurrence and metastatic progression preserve binary rate comparisons and add competing-risk cumulative incidence as co-primary event-time evidence.
- `objective1-cohort-interpretation-guardrails`: Objective 1 output language and summaries distinguish comparative, associational, and exploratory cohorts and estimands.
- `objective1-subgroup-contract-alignment`: Objective 1 subgroup artifacts and documentation agree on what is produced and how it should be interpreted.

### Modified Capabilities

None.

## Impact

- Affected workflow: `scripts/workflow/objective_1_primary_outcomes.R`
- Affected analysis modules: `scripts/analysis/binary_outcomes.R`, `scripts/analysis/survival_outcomes.R`, `scripts/analysis/tumor_height_analysis.R`
- Affected runtime outputs, subgroup formatting, and efficacy documentation
