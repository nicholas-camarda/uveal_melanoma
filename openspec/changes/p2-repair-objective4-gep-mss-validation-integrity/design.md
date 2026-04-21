## Context

Objective 4 validates imported GEP risk predictions against observed MFS and MSS outcomes. The current implementation already contains important censoring-aware and competing-risk-aware pieces, but they are not consistently used across all manuscript-facing layers. The largest integrity gaps are: MSS standard metrics that do not match a competing-risk absolute-risk estimand, a historical `Training` / `Testing` split that creates contract confusion without a clear active analytic role, reader-facing summaries that overstate support, and QC artifacts that can contradict themselves.

## Goals / Non-Goals

**Goals:**
- Make the primary MSS validation metrics align to competing-risk cumulative incidence.
- Retire `Training` / `Testing` as an Objective 4 validation contract and remove or quarantine related reader-facing language.
- Ensure narrative claims degrade automatically when support is weak.
- Remove or clearly quarantine naive or internally contradictory QC layers.

**Non-Goals:**
- Fit a brand-new prognostic model to replace imported assay risks.
- Eliminate all exploratory or QC outputs from Objective 4.
- Expand the no-GEP appendix into a separate major objective.
- Create a duplicate GEP reporting stack when existing consolidated workbooks, technical workbooks, simple validation outputs, and narrative generators can be extended.

## Implementation Constraint

Reuse the current Objective 4 orchestration, consolidated workbook, technical workbook, simple validation, no-GEP appendix, and narrative-summary mechanisms. Competing-risk MSS corrections, split retirement, QC checks, and narrative downgrades should be expressed through existing sheets, status columns, estimand metadata, and summary sections before adding any new artifact family.

## Decisions

### Decision: Rebuild manuscript-facing MSS validation around a competing-risk-consistent lane

The primary reported MSS calibration and discrimination outputs will target cumulative incidence with competing death handled explicitly. Legacy cause-specific style metrics may remain technical if clearly demoted.

### Decision: Retire the Objective 4 Training/Testing split

The imported GEP probabilities are fixed before this analysis, and the current Objective 4 workflow does not train a new model from the analytic cohort. The historical `Training` / `Testing` split therefore adds confusion and should not be preserved as a core validation contract. The pipeline may keep a simple analyzable/non-analyzable eligibility flag, but reader-facing Objective 4 docs and outputs should not imply that Training/Testing is driving the main validation.

### Decision: Separate core assay validation from later extrapolation claims

Five-year imported-risk validation will remain the core claim. Seven- and ten-year extrapolated horizons will remain in the main output set, but they will be reported with explicit support grading and weaker narrative language when support is limited.

### Decision: Add hard reporting sanity checks

Objective 4 will block or downgrade outputs when intervals are impossible, simple QC layers diverge from formal estimands without warning, or narratives would overstate unsupported clinical use.

## Risks / Trade-offs

- [Competing-risk-consistent MSS metrics may differ from current reported results] -> Mitigation: preserve technical comparison tables so the change is transparent.
- [Narrative downgrading may make the output feel less polished] -> Mitigation: prioritize scientific honesty and keep the language concise rather than alarmist.

## Migration Plan

1. Retire Objective 4 Training/Testing split language and remove split-shape expectations from active validation contracts.
2. Implement the competing-risk-consistent MSS validation lane.
3. Make the simple MSS layer reuse the same corrected observed-risk logic or clearly quarantine it.
4. Add narrative and QC guardrails.
5. Update unified outputs and no-GEP reporting sanity checks.
6. Expand tests from file existence to method and wording integrity.

## Open Questions

None. Seven-year and ten-year outputs remain in scope, with stronger evidence-based interpretation guardrails rather than demotion.
