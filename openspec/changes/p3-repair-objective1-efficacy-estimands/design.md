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

Do not create new PH/RMST interpretation workbooks, folders, or one-off sidecar reports unless no existing artifact can carry the information. Prefer adding compact fields or notes to the current survival effect summaries, proportional-hazards summary text, RMST summary tables, and centralized Objective 1 high-level summary.

## Decisions

### Decision: Treat binary rates and cumulative incidence as co-primary for recurrence/metastasis

Objective 1 will preserve the collaborator-requested binary recurrence and metastatic-progression comparisons while adding competing-risk cumulative incidence as co-primary evidence using existing time variables. Death before recurrence or metastasis will be handled as a competing event for the cumulative-incidence lane. Binary outputs answer whether an event was ever observed during available follow-up; cumulative-incidence outputs answer event probability by a time horizon while accounting for censoring and competing death.

The cumulative-incidence workbook labels the Gray test as `gray_test_global_curve_p_value` because Gray's test is one global across-group comparison of cumulative-incidence curves. It is repeated across horizon rows only as contextual test metadata and must not be interpreted as a separate p-value for each time horizon or treatment group.

Alternative considered:
- Replace binary/logistic outputs entirely. Rejected because the project objectives and collaborator-facing materials explicitly ask for binary rate comparisons.
- Keep logistic outputs as the only primary analysis and relabel them. Rejected because the repo already derives event-time variables and the comparative claim is too strong for ever-event logits alone.

### Decision: Use graded PH interpretation for Cox-based survival summaries

Objective 1 survival summaries will not treat every Schoenfeld p-value below 0.05 as a full Cox demotion. Mild or borderline PH concerns will keep the Cox HR visible with cautionary language and RMST/KM triangulation. Material PH violations will lead with RMST/KM when those outputs are available and label the single Cox HR as secondary or time-compressed.

Materiality should be judged from the existing PH diagnostics and companion outputs, including global PH strength, treatment-term PH strength, number of violating terms, diagnostic plot pattern, event support, and whether RMST/KM materially changes the treatment-effect story.

Alternative considered:
- Automatically make RMST primary whenever PH p < 0.05. Rejected because mild PH departures do not necessarily invalidate Cox summaries and prior statistical guidance supports proportional response to the severity of the violation.

Implementation examples from the current Objective 1 results:

- Full-cohort OS: treatment PH p=0.151 and global PH p=0.128; adjusted Cox HR=1.12 (95% CI 0.65 to 1.93). Interpretation priority should remain Cox-forward, with RMST/KM as complementary absolute-time context.
- Full-cohort PFS: treatment PH p=0.0341 and global PH p=0.0340; adjusted Cox HR=1.22 (95% CI 0.77 to 1.94), while RMST favors PBT by about 5.0 months at 5 years (p=0.03). Interpretation priority should be RMST/KM-forward or at least Cox-with-material-PH-caution, with the Cox HR described as an average/time-compressed effect.
- Restricted-cohort PFS: global PH p=0.0241 but treatment PH p=0.212; adjusted Cox HR=1.31 (95% CI 0.72 to 2.37), with 5-year RMST difference about -5.5 months (p=0.07). Interpretation priority should be Cox-with-PH-caution rather than automatic Cox demotion because the treatment term itself does not show PH violation.
- GKSRS-only PFS: treatment PH p=0.0787 and global PH p=0.102; adjusted Cox HR=0.77 (95% CI 0.31 to 1.91). Interpretation priority can remain Cox-forward from a PH standpoint, while cohort-level language still treats this surface as exploratory characterization rather than the primary treatment-comparison surface.

Suggested compact fields for existing tables:

- `PH_Interpretation`: `cox_forward`, `cox_with_ph_caution`, `rmst_km_forward`, or `cox_limited_ph_untestable`.
- `PH_Interpretation_Reason`: short text such as `No PH diagnostic concern`, `Global PH flag without treatment-term PH flag`, `Treatment-term and global PH flags with RMST contrast`, or `PH diagnostics unavailable due to low event support`.

Suggested concise note patterns:

- Cox-forward: "PH diagnostics did not show evidence against proportional hazards; Cox HR remains the lead model-based summary, with RMST/KM retained as absolute-time context."
- Cox with PH caution: "PH diagnostics showed a global concern but not a treatment-term concern; Cox HR remains reportable, but interpretation should be triangulated with RMST/KM."
- RMST/KM-forward: "PH diagnostics flagged treatment and global non-proportionality; the Cox HR is reported as an average/time-compressed effect, while RMST/KM provide the lead interpretation."
- PH untestable: "PH diagnostics were not supportable; any Cox HR should be interpreted cautiously with event-support and RMST/KM context where available."

### Decision: Add a centralized cohort-interpretation note

The restricted cohort will carry the strongest comparative language. The full cohort will be reported as real-world associational, and the GKSRS-only cohort as characterization or exploratory support. This will be implemented through a centralized note in high-level Objective 1 reader-facing summaries rather than by adding repeated boilerplate to every plot, model table, HTML regression output, or diagnostic artifact.

Alternative considered:
- Preserve equal treatment language across all cohorts. Rejected because the cohort construction itself encodes different causal credibility.

### Decision: Keep legacy post-baseline outputs only if they are unmistakably labeled

Legacy recurrence-stratified and metastasis-stratified OS/PFS outputs may remain available, but they must include artifact-level labeling that they are post-baseline, non-causal exploratory analyses.

### Decision: Align subgroup contract to stable runtime artifacts

Objective 1 subgroup tabular outputs will be documented around the consolidated multi-sheet Excel workbooks that are emitted by the workflow: forest-plot diagnostics workbooks and primary/sensitivity tumor-height diagnostics workbooks. Subgroup forest plots and subgroup interaction RDS objects remain part of the stable runtime contract. Per-subgroup HTML files may remain as ancillary previews if the existing formatter emits them, but they should not be treated as the primary documented tabular surface or expanded into a new artifact family.

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

None for the current P3 implementation pass.
