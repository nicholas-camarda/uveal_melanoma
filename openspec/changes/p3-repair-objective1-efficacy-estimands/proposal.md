## Why

Objective 1 currently mixes strong treatment-comparison language with methods that do not consistently target a defensible comparative estimand. The biggest gaps are recurrence/metastasis being modeled as crude ever-event logistic outcomes and output/reporting surfaces that blur comparative, associational, and exploratory analyses.

## What Changes

- Replace recurrence and metastatic-progression treatment comparisons with follow-up-aware event-time analyses that match the intended estimand.
- Make the restricted cohort the primary comparative cohort and downgrade full-cohort and GKSRS-only treatment language to associational or exploratory where appropriate.
- Enforce RMST-first interpretation when PFS proportional-hazards assumptions fail.
- Make legacy post-baseline outputs and subgroup outputs self-identifying and contract-aligned.

## Capabilities

### New Capabilities
- `objective1-event-time-treatment-comparisons`: Objective 1 recurrence and metastatic progression are reported on explicit event-time estimands instead of crude ever-event logits.
- `objective1-cohort-interpretation-guardrails`: Objective 1 output language and summaries distinguish comparative, associational, and exploratory cohorts and estimands.
- `objective1-subgroup-contract-alignment`: Objective 1 subgroup artifacts and documentation agree on what is produced and how it should be interpreted.

### Modified Capabilities

None.

## Impact

- Affected workflow: [objective_1_primary_outcomes.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/workflow/objective_1_primary_outcomes.R)
- Affected analysis modules: [binary_outcomes.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/analysis/binary_outcomes.R), [survival_outcomes.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/analysis/survival_outcomes.R), [tumor_height_analysis.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/analysis/tumor_height_analysis.R)
- Affected runtime outputs, subgroup formatting, and efficacy documentation
