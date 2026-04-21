## Context

Objective 2 contains vision-change modeling plus binary complication models for retinopathy, NVG, and SRD. The current implementation largely measures recorded burden by the available follow-up window, but the docs and some report language can be read as comparative toxicity incidence. Vision models also claim baseline-adjusted behavior in the docs without actually including baseline vision in the adjustment set.

## Goals / Non-Goals

**Goals:**
- Make the estimand for complications explicit and consistent in code, docs, and outputs.
- Align the vision-model documentation with the actual model or upgrade the model to match the documentation.
- Add clear handling for missingness, sparsity, and untested ordinal assumptions.

**Non-Goals:**
- Implement a complete new ophthalmic outcome ontology.
- Rebuild Objective 2 around external adjudication or new raw data not already available in the repo.
- Replace the existing Objective 2 diagnostics/effect-summary system with a new toxicity reporting framework.

## Implementation Constraint

Reuse the current Objective 2 model-fitting, diagnostics workbook, effect-summary, and skip-artifact mechanisms. Missingness policy, burden-language notes, proportional-odds assumption status, and sparse-support handling should be added as existing diagnostics tabs, notes/status fields, or concise summary text rather than as new standalone reporting layers.

## Decisions

### Decision: Standardize complication endpoints as burden-by-follow-up

The current source workbook and data dictionary expose retinopathy, NVG, and SRD as binary recorded outcomes without dedicated onset dates. Objective 2 will therefore standardize the complication analyses as recorded toxicity burden by available follow-up, not time-to-toxicity incidence. If reliable onset dates are later obtained, that should be a separate future change rather than an implicit expansion of this one.

### Decision: Keep vision wording tightly tied to the implemented endpoint

The repo will either add baseline vision to the model or immediately revise the docs and output language so they no longer promise baseline-adjusted change modeling.

### Decision: Surface assumption and support limits in the outputs

Objective 2 will explicitly note missingness handling, sparse-event feasibility, and whether the proportional-odds assumption was assessed, rather than leaving those issues implicit.

## Risks / Trade-offs

- [Choosing burden wording may feel less ambitious] -> Mitigation: emphasize that it is the more honest interpretation if onset dates are not available.
- [Upgrading to time-to-toxicity may expand scope materially] -> Mitigation: defer that expansion to a future change that starts with new source-data verification.
- [Adding more output caveats may clutter reports] -> Mitigation: keep them concise and put technical detail in diagnostics sheets.

## Migration Plan

1. Align complication outputs and language to recorded toxicity burden by available follow-up.
2. Remove or weaken incidence/time-to-toxicity language unless a specific output is based on valid onset-time data.
3. Align vision models and docs around baseline adjustment and endpoint wording.
4. Add tests for missingness, sparse support, and assumption/reporting behavior.

## Open Questions

None for this change. The current source-data review did not identify reliable retinopathy, NVG, or SRD onset-date fields, so Objective 2 remains burden-by-follow-up.
