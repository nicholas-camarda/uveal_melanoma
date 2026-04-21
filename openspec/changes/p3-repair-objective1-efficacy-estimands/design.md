## Context

Objective 1 spans recurrence, metastatic progression, OS, PFS, tumor-height change, and subgroup analyses across three cohorts with different causal credibility. The current implementation is strongest for OS, mixed for PFS, and weakest for recurrence/metastasis, where follow-up imbalance and competing death make logistic ever-event models poor proxies for treatment comparison. Reporting also blurs the restricted comparative cohort with the more confounded full and GKSRS-only cohorts.

## Goals / Non-Goals

**Goals:**
- Align recurrence and metastatic progression to event-time estimands.
- Make comparative versus associational interpretation explicit across cohorts.
- Force PFS summaries to privilege RMST when PH diagnostics fail.
- Bring subgroup and legacy-output contracts into line with actual runtime artifacts.

**Non-Goals:**
- Rebuild the entire Objective 1 output tree from scratch.
- Resolve every causal confounding limitation in observational treatment comparison.
- Redesign tumor-height methodology beyond labeling and interpretation guardrails in this change.

## Decisions

### Decision: Replace crude recurrence/metastasis logits with event-time analyses

Objective 1 will move recurrence and metastatic progression to event-time analyses using existing time variables, with explicit handling of death when the estimand is cumulative incidence.

Alternative considered:
- Keep logistic outputs as the primary analysis and relabel them. Rejected because the repo already derives event-time variables and the comparative claim is too strong for ever-event logits.

### Decision: Make the restricted cohort the primary comparative surface

The restricted cohort will carry the strongest comparative language. The full cohort will be reported as real-world associational, and the GKSRS-only cohort as characterization or exploratory support.

Alternative considered:
- Preserve equal treatment language across all cohorts. Rejected because the cohort construction itself encodes different causal credibility.

### Decision: Keep legacy post-baseline outputs only if they are unmistakably labeled

Legacy recurrence-stratified and metastasis-stratified OS/PFS outputs may remain available, but they must include artifact-level labeling that they are post-baseline, non-causal exploratory analyses.

### Decision: Align subgroup contract by either producing the promised tables or narrowing the docs

This change will not leave the current mismatch in place. Either the subgroup surface will emit the documented artifacts, or the docs will be reduced to the true runtime contract.

## Risks / Trade-offs

- [Choosing the wrong recurrence/metastasis estimand will create another contract drift] -> Mitigation: keep one explicit open question and do not hide it inside implementation tasks.
- [Event-time conversion will change historical effect summaries] -> Mitigation: preserve prior outputs as legacy artifacts and document the estimand change clearly.
- [Restricting strong treatment language may feel like a downgrade] -> Mitigation: explain that the new wording improves scientific defensibility rather than reducing useful output.

## Migration Plan

1. Decide the primary recurrence/metastasis estimand.
2. Implement event-time analysis and reporting updates for Objective 1a/1b.
3. Add cohort-specific interpretation guardrails and RMST-first PFS summaries.
4. Repair subgroup and legacy-output contracts in code, docs, and tests.

## Open Questions

- For Objective 1a local recurrence and Objective 1b metastatic progression, should the primary treatment comparison be cumulative incidence over time or cause-specific hazard? This question is about the main 1a/1b endpoints, not the legacy post-baseline `1a1`, `1a2`, `2a1`, or `2a2` exploratory folders.
- Cumulative incidence answers: by a given time horizon, how often does the event occur while handling death before the event as a competing event? This is usually easier to interpret clinically as absolute risk.
- Cause-specific hazard answers: among patients still event-free and alive/under observation, is the instantaneous event rate different by treatment? This can be useful etiologically but is less directly an absolute-risk comparison and usually needs cumulative-incidence companions for interpretation.
- Default recommendation for the next implementation pass: use cumulative incidence as the primary estimand for Objective 1a/1b, with cause-specific models as technical/secondary outputs only if model support is adequate.
