## Context

Objective 1 spans recurrence, metastatic progression, OS, PFS, tumor-height change, and subgroup analyses across three cohorts with different causal credibility. The current implementation is strongest for OS, mixed for PFS, and weakest for recurrence/metastasis, where follow-up imbalance and competing death make logistic ever-event models incomplete proxies for treatment comparison when used alone. Reporting also blurs the restricted comparative cohort with the more confounded full and GKSRS-only cohorts.

## Goals / Non-Goals

**Goals:**
- Preserve collaborator-requested binary recurrence/metastasis comparisons while adding competing-risk cumulative incidence as co-primary event-time evidence.
- Make comparative versus associational interpretation explicit across cohorts.
- Apply graded PH interpretation to Objective 1 Cox-based survival summaries so mild PH concerns trigger cautionary labeling while material PH violations trigger RMST/KM-first interpretation when supported.
- Bring subgroup and legacy-output contracts into line with actual runtime artifacts.

**Non-Goals:**
- Rebuild the entire Objective 1 output tree from scratch.
- Resolve every causal confounding limitation in observational treatment comparison.
- Redesign tumor-height methodology beyond labeling and interpretation guardrails in this change.
- Add repeated boilerplate or duplicate report generators across every Objective 1 plot, model table, and diagnostics artifact.

## Implementation Constraint

Reuse existing Objective 1 survival, RMST, binary-output, effect-summary, subgroup, forest-plot, and diagnostics mechanisms where possible. New cumulative-incidence outputs and interpretation notes should extend existing workbooks, diagnostics tabs, notes/status fields, or high-level summaries rather than creating parallel report paths.

## Decisions

### Decision: Treat binary rates and cumulative incidence as co-primary for recurrence/metastasis

Objective 1 will preserve the collaborator-requested binary recurrence and metastatic-progression comparisons while adding competing-risk cumulative incidence as co-primary evidence using existing time variables. Death before recurrence or metastasis will be handled as a competing event for the cumulative-incidence lane. Binary outputs answer whether an event was ever observed during available follow-up; cumulative-incidence outputs answer event probability by a time horizon while accounting for censoring and competing death.

Alternative considered:
- Replace binary/logistic outputs entirely. Rejected because the project objectives and collaborator-facing materials explicitly ask for binary rate comparisons.
- Keep logistic outputs as the only primary analysis and relabel them. Rejected because the repo already derives event-time variables and the comparative claim is too strong for ever-event logits alone.

### Decision: Use graded PH interpretation for Cox-based survival summaries

Objective 1 survival summaries will not treat every Schoenfeld p-value below 0.05 as a full Cox demotion. Mild or borderline PH concerns will keep the Cox HR visible with cautionary language and RMST/KM triangulation. Material PH violations will lead with RMST/KM when those outputs are available and label the single Cox HR as secondary or time-compressed.

Materiality should be judged from the existing PH diagnostics and companion outputs, including global PH strength, treatment-term PH strength, number of violating terms, diagnostic plot pattern, event support, and whether RMST/KM materially changes the treatment-effect story.

Alternative considered:
- Automatically make RMST primary whenever PH p < 0.05. Rejected because mild PH departures do not necessarily invalidate Cox summaries and prior statistical guidance supports proportional response to the severity of the violation.

### Decision: Add a centralized cohort-interpretation note

The restricted cohort will carry the strongest comparative language. The full cohort will be reported as real-world associational, and the GKSRS-only cohort as characterization or exploratory support. This will be implemented through a centralized note in high-level Objective 1 reader-facing summaries rather than by adding repeated boilerplate to every plot, model table, HTML regression output, or diagnostic artifact.

Alternative considered:
- Preserve equal treatment language across all cohorts. Rejected because the cohort construction itself encodes different causal credibility.

### Decision: Keep legacy post-baseline outputs only if they are unmistakably labeled

Legacy recurrence-stratified and metastasis-stratified OS/PFS outputs may remain available, but they must include artifact-level labeling that they are post-baseline, non-causal exploratory analyses.

### Decision: Align subgroup contract by either producing the promised tables or narrowing the docs

This change will not leave the current mismatch in place. Either the subgroup surface will emit the documented artifacts, or the docs will be reduced to the true runtime contract.

## Risks / Trade-offs

- [Co-primary recurrence/metastasis outputs could confuse readers if they disagree] -> Mitigation: label binary and cumulative-incidence outputs by estimand and explain discordance rather than forcing one result to override the other.
- [Cumulative-incidence outputs will change historical effect summaries] -> Mitigation: preserve binary outputs and document that cumulative incidence is a co-primary follow-up-aware companion, not a silent replacement.
- [Overreacting to mild PH departures could undercut useful Cox summaries] -> Mitigation: use graded PH interpretation and reserve RMST/KM-first language for material violations.
- [Restricting strong treatment language may feel like a downgrade] -> Mitigation: explain that the new wording improves scientific defensibility rather than reducing useful output.

## Migration Plan

1. Implement co-primary binary and competing-risk cumulative-incidence reporting for Objective 1a/1b.
2. Add graded PH interpretation for Objective 1 Cox-based survival summaries.
3. Add the centralized cohort-interpretation note.
4. Repair subgroup and legacy-output contracts in code, docs, and tests.

## Open Questions

- For Objective 1 subgroup outputs, should the implementation generate the documented subgroup table workbooks or narrow the documented contract to the currently stable runtime artifacts?
- Default recommendation for the next implementation pass: verify current subgroup artifacts first, then narrow documentation unless a missing workbook can be produced through the existing subgroup formatting path without adding a parallel reporting system.
