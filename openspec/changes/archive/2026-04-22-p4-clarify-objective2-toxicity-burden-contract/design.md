## Context

Objective 2 contains vision-change modeling plus binary complication models for retinopathy, NVG, and SRD. The current implementation largely measures recorded burden by the available follow-up window, but the docs and some report language can be read as comparative toxicity incidence. Vision models also claim baseline-adjusted behavior in the docs without actually including baseline vision in the adjustment set.

## Goals / Non-Goals

**Goals:**
- Make the estimand for complications explicit and consistent in code, docs, and outputs.
- Align the vision-model documentation with the actual model or upgrade the model to match the documentation.
- Add clear handling for missingness, sparsity, untested ordinal assumptions, and reproducible simulated descriptive tests.

**Non-Goals:**
- Implement a complete new ophthalmic outcome ontology.
- Rebuild Objective 2 around external adjudication or new raw data not already available in the repo.
- Replace the existing Objective 2 diagnostics/effect-summary system with a new toxicity reporting framework.

## Implementation Constraint

Reuse the current Objective 2 model-fitting, diagnostics workbook, effect-summary, and skip-artifact mechanisms. Missingness policy, burden-language notes, proportional-odds assumption status, and sparse-support handling should be added as existing diagnostics tabs, notes/status fields, or concise summary text rather than as new standalone reporting layers.

Complication endpoint validation belongs at the Objective 0 data validation/prep boundary, not inside model-specific analysis code. Objective 2 analysis scripts should consume validated/prepared retinopathy, NVG, and SRD burden fields and should not independently recode raw `Y`/`N`/missing source values. Do not add broad raw-value audit sheets to every Objective 2 workbook; rely on Objective 0 validation artifacts for source-field validity and keep Objective 2 outputs focused on endpoint interpretation, denominators, sparse support, and model diagnostics.

## Decisions

### Decision: Standardize complication endpoints as burden-by-follow-up

The current source workbook and data dictionary expose retinopathy, NVG, and SRD as binary recorded outcomes without dedicated onset dates. Objective 2 will therefore standardize the complication analyses as recorded toxicity burden by available follow-up, not time-to-toxicity incidence. If reliable onset dates are later obtained, that should be a separate future change rather than an implicit expansion of this one.

### Decision: Keep vision wording tightly tied to the implemented endpoint

P4 will not add baseline vision to the Objective 2 vision models. The current change-score models will be preserved, and the docs/output language will be revised so they no longer promise baseline-adjusted modeling.

### Decision: Surface assumption and support limits in the outputs

Objective 2 will explicitly note missingness handling, sparse-event feasibility, and whether the proportional-odds assumption was assessed, rather than leaving those issues implicit.

### Decision: Make Objective 0 the toxicity endpoint validation boundary

Current raw source review found one missing retinopathy/NVG/SRD row, and that row is removed before Objective 2 because it is the configured no-chart manual exclusion. The analytic cohorts used by Objective 2 currently contain only valid `Y`/`N` values for retinopathy, NVG, and SRD.

P4 will therefore formalize Objective 0 as the place that validates these toxicity endpoint inputs for included analytic rows. Objective 2 should not preserve defensive `NA -> 0` or "anything else -> 0" analysis-local recoding. If future included rows contain missing, blank, or non-`Y`/`N` toxicity values, that should surface as an explicit Objective 0 validation finding and Objective 2 should not silently absorb those values as non-events.

### Decision: Report ordinal assumption status without adding a new test

P4 will not add a new proportional-odds assumption test. Ordinal Snellen outputs will state that the assumption was not formally tested, while preserving the existing ordinal logistic model outputs.

### Decision: Make simulated descriptive p-values reproducible

Objective 2 categorical descriptive summaries that require simulated Fisher p-values will use a fixed, documented seed scoped to the test call. This prevents run-to-run drift in reader-facing notes while preserving the existing descriptive testing approach.

## Risks / Trade-offs

- [Choosing burden wording may feel less ambitious] -> Mitigation: emphasize that it is the more honest interpretation if onset dates are not available.
- [Upgrading to time-to-toxicity may expand scope materially] -> Mitigation: defer that expansion to a future change that starts with new source-data verification.
- [Adding more output caveats may clutter reports] -> Mitigation: keep them concise and put technical detail in diagnostics sheets.
- [Setting a seed globally could perturb unrelated analyses] -> Mitigation: scope the seed locally around the simulated Fisher call and restore the prior RNG state.
- [Leaving recoding in analysis scripts could create silent drift] -> Mitigation: validate toxicity endpoint fields at Objective 0, create or require prepared binary fields before Objective 2 runs, and add tests that fail if Objective 2 reintroduces local raw-value mapping.
- [Adding endpoint validation to Objective 2 workbooks could create reporting bloat] -> Mitigation: keep source-field validity in Objective 0 validation artifacts and keep Objective 2 diagnostics limited to concise endpoint notes, denominators, sparse support, and model status.

## Migration Plan

1. Align complication outputs and language to recorded toxicity burden by available follow-up.
2. Remove or weaken incidence/time-to-toxicity language unless a specific output is based on valid onset-time data.
3. Preserve the implemented vision change-score models and align docs/output wording to the actual covariates and endpoint timing.
4. Add Objective 0 validation/prep coverage for retinopathy, NVG, and SRD endpoint inputs in included analytic rows.
5. Remove Objective 2 analysis-local raw-value recoding and consume only validated/prepared toxicity burden fields.
6. Keep Objective 2 diagnostics concise: endpoint interpretation, rates/model denominators, sparse support, and explicit skip/failure reasons.
7. State that proportional-odds assumptions were not formally tested in the current ordinal Snellen outputs.
8. Make simulated descriptive Fisher p-values deterministic without changing the underlying observed counts.
9. Add tests for Objective 0 toxicity endpoint validation, absence of Objective 2 local recoding, sparse support, deterministic descriptive p-values, and assumption/reporting behavior.

## Open Questions

None for this change. The current source-data review did not identify reliable retinopathy, NVG, or SRD onset-date fields, so Objective 2 remains burden-by-follow-up.
