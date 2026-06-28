# Peer Review Statistical Revision Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for verified completion tracking. Leave a step unchecked when draft text or code exists but the required runtime artifacts, exact evidence aliases, Markdown/path validation, or final interpretation review remain incomplete.

**Goal:** Refactor the uveal melanoma manuscript analysis pipeline and documentation into a reviewer-responsive, Cox-focused, time-aware, de-escalated post-submission revision while preserving the submitted pre-review work as a baseline.

**Architecture:** Execute in a separate peer-review branch/worktree from the current post-submission code state. Begin each task by tracing the submission-era artifact to its existing code and runtime implementation; reuse that infrastructure when it already answers the reviewer. Add only the narrow missing analysis or reporting surface, then remove redundant reviewer-facing inference rather than maintaining parallel old and new paths. Treat adjusted Cox models and transparent feasibility audits as the primary revision surface; demote binary/logistic, RMST/log-rank, subgroup, visual-acuity, tumor-height, and dosimetry claims where the data cannot support stronger inference.

**Reviewer-facing cohort scope:** This peer-review response and manuscript revision deal only with the full and restricted cohorts. Internal GKSRS-only runtime outputs may exist for other project purposes, but they are not part of the reviewer-facing response plan for this paper.

**Tech Stack:** R, tidyverse, survival, survminer, cmprsk, gtsummary, readxl/openxlsx, testthat, existing project helpers loaded through `<PEER_REVIEW_REPO>/scripts/load_all.R`.

## 2026-06-28 Completion Policy Correction

Nick clarified at final review that `docs/peer_review_revision_response.md` and `docs/PR_VS_ORIGINAL_RESULTS_AUDIT.md` are not deliverable commit artifacts for this branch. They may be used as working notes during review, but they must not be committed with this revision. Durable method, endpoint, limitation, and interpretation changes from those working notes belong in the existing baseline documentation that is already part of the repository, especially `README.md`, `docs/TECHNICAL.md`, `docs/CALCULATIONS.md`, `docs/STATISTICAL_METHODS.md`, `docs/INTERPRETATION_GUIDE.md`, and the relevant objective-specific docs/tests.

This superpowers plan remains commit-eligible as operator provenance for the large peer-review revision. Because it is operator-facing, it may retain absolute local paths under the explicit clickable-path exception below; that exception does not apply to reviewer-facing or baseline documentation.

---

## Source Material Reviewed For This Plan

This plan synthesizes the following source packet.

- Gmail thread `19ef47f8cf7efd88`, subject `RE: [EXT] Re: [External] Submitted 5/3/26: UM Manuscript to Advances in Radiation Oncology - 6/21/26 status update - major revisions`.
  - Tim's June 22, 2026 message says major revisions are required, both reviewers are concerned about missing radiation-treatment detail, both reviewers are concerned about statistics, some radiation-treatment detail may not be feasible inside the 60-day response window, and he wanted coauthor feedback before revising the manuscript.
  - The editorial decision requires resubmission by August 20, 2026 at 11:59:59 PM and asks for a clean manuscript, point-by-point response, and highlighted revised manuscript.
  - Angie replied on June 23, 2026 that most statistical comments make sense and that a meeting with Nick should be arranged to discuss how to address them.
  - Nick replied that he would review and could meet evenings after 6 PM or weekend mornings.
  - Angie suggested either meeting with Tim first or resolving more by email before meeting if needed.
- Gmail message `19ef497ceae3d686`, subject `Fwd: [External] Submitted 5/3/26: UM Manuscript to Advances in Radiation Oncology - 6/21/26 status update - major revisions`, including both readable `.docx` attachments.
  - Attachment `Advances in Rad Onc - UM Manuscript - Responses To Reviewer Comments (6-22-26, TJM).docx` is Tim's initial response draft. It confirms that several response items were already partly drafted as wording changes, but the statistical and data-availability fixes remained open or underdeveloped: radiation-treatment detail, PFS definition, tumor-height timing, visual-acuity conversion rationale, Cox-only recurrence/metastasis, PH diagnostics, continuous variables, dose limitations, adverse-event grading, and optic-nerve/proximity limitations.
  - Tim's initial response draft includes preliminary follow-up summaries for coauthor reference: full cohort PBT mean follow-up 6.5 years, full cohort GK mean follow-up 3.9 years, restricted cohort PBT mean follow-up 6.3 years, restricted cohort GK mean follow-up 4.1 years, with maxima 16.9 and 17.3 years. The implementation must recompute these values from runtime data and decide whether mean, median, range, and assessment-specific follow-up are all needed.
  - Attachment `ARO - UM - ANONYMIZED MANUSCRIPT (4-27-26)-FINAL.docx` is the submitted manuscript. It confirms the exact pre-review language and analyses that need revision: restricted cohort described as simulating randomization, recurrence/metastasis framed as binary event-rate primary outcomes, logistic regression adjusted for dichotomized age, baseline Fisher/Wilcoxon inferential tests, log-rank/RMST emphasis, broad vision-loss wording, conclusion claims of same efficacy, and preferred-treatment wording.
- Gmail message `19efb57e31f0d2a5`, subject `Re: [EXT] [External] Submitted 5/3/26: UM Manuscript to Advances in Radiation Oncology - 6/21/26 status update - major revisions`.
  - Tim says Angie and he met to discuss the statistical comments, their consensus recommendations were added to the updated Google Doc, and Nick should review the stats comments/questions plus Angie's suggestions in the comments section.
  - Chris Melhus's embedded comments support a pragmatic response strategy: do measured-conclusion changes plus limited feasible analyses, avoid attempting a full new dosimetry/visual-field project inside the response window, and separate visual-acuity preservation from broad visual-field preservation.
  - Chris provides technique details that can inform manuscript prose without overclaiming analyzable patient-level dosimetry: PBT primarily used Pd-103 with some I-125 and limited Cs-131; prescription generally benchmarked to 85 Gy to the tumor apex using I-125 with Pd-103 heterogeneity corrections; implant duration historically ranged from 4-7 days and later commonly 4-5 days; COMS plaque diameter was selected for margin; notched plaques were used; GKSRS planning used MR guidance with T1/T2 imaging and ophthalmology clinical notes; GK prescription/isodose/shot count varied by era, platform, and patient-specific planning.
- Gmail message `19ef4ac5db8541eb`, subject `Re: [EXT] [External] Submitted 5/3/26: UM Manuscript to Advances in Radiation Oncology - 6/21/26 status update - major revisions`.
  - Tim was available to meet during the week of June 23, 2026, would be unavailable July 8-17, 2026, and wanted to start addressing revisions before then because of the limited response timeframe.
- Gmail share email `19efb53054b26700`, subject `Document shared with you: "Advances in Rad Onc - UM Manuscript - Responses To Reviewer Comments (6-24-26, TJM) - SHARED.docx"`.
  - Tim's instruction: use the shared Google Doc to review/respond to reviewer comments.
  - Google Doc id: `1P2NEaDaXHekAm9mDFuZNYtIKTto1kIwFqMCsYIQqLJ8`.
- Shared Google Doc text and all visible comments for `Advances in Rad Onc - UM Manuscript - Responses To Reviewer Comments (6-24-26, TJM) - SHARED.docx`.
  - Tim comments explicitly directed to Nick include Cox-only recurrence/metastasis reanalysis, PH assumption confirmation, continuous-variable/dichotomization audit, PFS definition clarification, time-to-local-recurrence/time-to-metastasis Cox models, 5-year-capped OS/PFS HR sensitivity, propensity score assessment, minimum-follow-up visual-acuity sensitivity confirmation, PRAME/T4 subgroup removals, and no action needed for Table 1 p-values because Tim planned to remove them manually.
  - Tim comments also mark data feasibility constraints: PBT has radionuclide/plaque-size/notched-plaque fields; GK planning/dose data are not in the current dataset; visual-field/proximity/dose data require chart or plan re-review and may be infeasible in the 60-day window.
  - Melhus and Mignano comments support describing treatment technique where possible, softening conclusions, clarifying limitations, and not overexpanding into a new visual-acuity/dosimetry study.
- Local pasted statistical memo: `/Users/ncamarda/.codex/attachments/de3b6503-5999-4bb7-97bf-1015f1e097ce/pasted-text.txt`.
  - The memo frames the revision as de-escalation and refocusing: fewer stronger models, follow-up centrality, adjusted Cox as lead inference, no dichotomized age in models, reduced subgroup claims, and softened language.
- Current repository and runtime state.
  - Source repository: `/Users/ncamarda/Projects/uveal_melanoma`.
  - Runtime analytic datasets: `/Users/ncamarda/ProjectsRuntime/uveal_melanoma/Analytic Dataset/`.
  - Current branch: `master`, clean worktree, HEAD `7465447`.
  - Runtime cohort counts checked directly:
    - Full cohort: n=260, GKSRS=139, PBT=121.
    - Restricted cohort: n=167, GKSRS=64, PBT=103.
    - Restricted events: recurrence GKSRS=8/PBT=11; metastasis GKSRS=9/PBT=19; death GKSRS=12/PBT=27; PFS GKSRS=18/PBT=32.
    - Restricted median follow-up: GKSRS 37.96 months, PBT 75.20 months.
    - Current radiation/planning fields in runtime RDS: `optic_nerve`, `initial_plaque`, `initial_plaque_date`, `radionuclide`, `plaque_size`, `plaque_notch`; no GK dose/isodose/shot/isocenter/macula/fovea fields were found in the runtime columns.

## Scope Check

This revision spans multiple related surfaces, but they are not independent subsystems. The analysis, output files, and response text need to agree. Keep this as one coordinated plan with small commits because recurrence/metastasis model changes, confounder changes, PH reporting, follow-up summaries, and manuscript methods language must be internally consistent.

## Provenance And Existing-Work Corrections

- `ARO - UM - ANONYMIZED MANUSCRIPT (4-27-26)-FINAL.docx` is the manuscript submitted to *Advances in Radiation Oncology* on May 3, 2026. The submission-status email thread and the attachment supplied with the editorial decision identify it as the reviewed manuscript. It is not a JCO submission.
- Git commit `7465447` is a June 11, 2026 post-submission repository state. It must be preserved as the current pre-peer-review analysis baseline, but it must not be labelled as the submitted manuscript baseline. The submitted Word document and the dated `/Analysis/2026-04-27/` artifacts are the evidence for what reviewers saw.
- The submitted analysis already generated Schoenfeld-residual/`cox.zph()` diagnostics for OS and PFS. The April 27 restricted-cohort OS model had no global PH signal (`p = 0.1094`); the PFS model had a global PH warning (`p = 0.0241`) despite no individual treatment-term warning. The revision must report this rather than claim that all existing Cox models fulfilled PH. New recurrence/metastasis and 5-year capped Cox models still require their own diagnostics.
- The current analytic `last_vision` field is a numeric visual-acuity value. Latest-VA minimum-follow-up sensitivity now reports explicit treatment-to-`last_followup` timing as the primary conservative timing surface and a separately labeled proxy surface that uses derived general `follow_up_months` when explicit timing is missing. A separate latest-VA reviewer-predictor sensitivity models `last_vision` with baseline VA, explicit latest-VA follow-up timing, viable reviewer-requested baseline predictors, and the shared confounder set. Tumor-height timing is summarized separately and is not used as the VA timing proxy.

## Reuse And Reduction Rule

For every reviewer item, execution must follow this order:

1. Identify the reviewed-manuscript claim, the submission-era output, the producing code path, and the current equivalent implementation.
2. Reuse the existing helper, derivation, test fixture, output route, and diagnostic artifact when they already implement the requested method. For example, retain `run_or_skip_proportional_hazards_diagnostics()` and its `cox.zph()` output contract rather than introducing a second PH framework.
3. Add code only for the missing endpoint, derivation, diagnostic, or response-facing summary. The recurrence/metastasis Cox endpoints and any approved PFS rederivation are missing work; OS/PFS PH computation is not.
4. Remove or suppress the redundant reviewer-facing inference once its replacement is verified. Recurrence/metastasis logistic regression, inferential baseline p-values, unsupported PRAME/T4 subgroup displays, and unqualified RMST/log-rank claims must not remain as co-equal manuscript evidence.
5. Preserve provenance rather than preserve every legacy output: keep the submitted manuscript and dated April 27 artifacts as immutable reference material, but do not carry obsolete results into the revised manuscript, response tables, or reviewer packet.

No task should create a parallel implementation merely to protect an older analysis. If an existing component cannot be safely reused, replace it once, test it, and remove the obsolete reviewer-facing path.

Do not use this plan to reorganize the user's broader computer folders or residency workspace. That work should remain outside this repository and outside this branch/worktree.

## Source-Absence Claim Rule

Do not conclude that a reviewer-requested field is absent from the study merely because it is absent from the derived analytic RDS. For each claimed absence or infeasibility, check and document:

1. Derived runtime columns in the full/restricted analytic datasets.
2. Raw/source spreadsheet columns in the current master source file and relevant supplemental files.
3. Data-dictionary wording, when available.
4. Whether the source field exists but was intentionally not propagated into the analytic dataset.

Only after this check may the response say a field is unavailable. If a source field exists but is not currently derived, decide whether to add it to Objective 0 derivation or to explain why it is not usable for the reviewer response.

## Output-Note Bloat Guardrail

Do not add new scattered `.txt`, `.md`, or ad hoc explanatory note files under runtime output directories for this revision. The coauthor-facing narrative belongs in one document only: `docs/peer_review_revision_response.md`.

Runtime outputs should be limited to analysis artifacts that carry reusable tabular or visual evidence: Excel workbooks, plots, diagnostics, and explicitly skipped-analysis artifacts already produced by existing infrastructure. If an existing helper already writes a note file, do not add a second note layer around it. Prefer adding a compact sheet or metadata column to an existing workbook over creating a new prose artifact. If a note is only useful for Tim's response, summarize it in `docs/peer_review_revision_response.md` instead.

## Required Execution Posture

- Execute from a separate worktree created from the exact clean current post-submission/pre-peer-review code state. Use `superpowers:using-git-worktrees` at execution time.
- Use these source-path aliases throughout execution:
  - `<CANONICAL_REPO>` = `/Users/ncamarda/Projects/uveal_melanoma`
  - `<PEER_REVIEW_REPO>` = `/Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision`
  - `<INPUT_DATA_ROOT>` = `/Users/ncamarda/Library/CloudStorage/OneDrive-Personal/Research/uveal_melanoma/Original Files`
  - `<RUNTIME_ROOT>` = `/Users/ncamarda/ProjectsRuntime/uveal_melanoma-peer-review-statistical-revision`
  - `<OUTPUT_DIR>` = `/Users/ncamarda/ProjectsRuntime/uveal_melanoma-peer-review-statistical-revision/Analysis`
  - `<PROCESSED_DATA_DIR>` = `/Users/ncamarda/ProjectsRuntime/uveal_melanoma-peer-review-statistical-revision/Analytic Dataset`
  - `<TOOLS_OUTPUT_DIR>` = `/Users/ncamarda/ProjectsRuntime/uveal_melanoma-peer-review-statistical-revision/tools_output`
- All source, test, and documentation edits in Tasks 2-14 must be made under `<PEER_REVIEW_REPO>`, even when this plan names the canonical repository path for orientation. Do not edit or commit source files on `master` in `<CANONICAL_REPO>` during implementation. Runtime artifacts remain under `<RUNTIME_ROOT>`.
- Clickable-path exception: this internal implementation plan may include absolute local paths because it is an operator-facing execution document. On-demand audit tools, especially `scripts/tools/peer_review_followup_audit.R`, may also emit absolute `file://` Markdown links or absolute path columns in audit workbooks so Nick can click directly into runtime artifacts. Do not extend this exception to reviewer-facing or coauthor-facing narrative docs unless Nick explicitly asks for that document to become operator-facing.
- Preserve `master` as the current post-submission/pre-peer-review repository state, and preserve the submitted manuscript plus April 27 artifacts as external immutable evidence.
- Avoid compatibility rescue logic. Prefer one correct reviewer-response analysis path and remove or demote obsolete inferential paths.
- Do not treat successful code execution as enough. Verify data availability, estimand alignment, model output, manuscript methods wording, and reviewer-response wording.
- When any figure, plot, or image artifact is regenerated, inspect the regenerated file in an image viewer and make a reasonable visual verification that the intended reviewer-response changes landed. At minimum, confirm that the file is not blank/corrupt, labels and legends match the revised endpoint/method, obsolete reviewer-facing elements are absent, and the rendered figure is suitable for manuscript or response review.
- Do not claim equivalence, noninferiority, or causal visual-acuity benefit.
- Treat the propensity score request as a robustness strategy for treatment-selection adjustment and event-per-variable pressure, not as a solution for missing visual-field, proximity, or dose data.

## Execution Order For Bounded Goals

If new review feedback identifies additional minor plan defects before Goal Group 1 starts, run a **plan-amendment-only** turn first. That turn may edit only this plan, must run `git diff --check` on the plan, and must stop with the recommended Goal Group 1 text. Do not begin code implementation in the same turn as a final plan-amendment pass.

Do not execute this plan as one giant Codex goal. Execute it as bounded task-group goals. At the end of each goal, stop and provide a checkpoint with:

- Changed files.
- Exact commands run.
- Tests passed/failed.
- Runtime artifacts generated.
- Unresolved decisions or blockers.
- Next remaining task IDs.
- Recommended next Codex goal text for Nick to give.

Use these goal groups:

1. **Worktree/provenance/test helpers:** Tasks 1, 2, 2A, 2B.
2. **Objective 1 endpoint/modeling:** Tasks 3, 3A, 3B, 4, 5.
3. **Audit/propensity tools:** Tasks 6, 7.
4. **Objective 2/tumor-height/subgroups:** Tasks 8, 8A, 9, 10.
5. **Docs/response:** Tasks 11, 12, 14.
6. **Verification-only:** Task 13 and Final Verification Checklist.

If a goal uncovers a methodological decision that Nick/Tim/Angie must make, record the decision point in the checkpoint and do not silently choose the result-favorable path.

## File Structure

### Files To Modify

- `<PEER_REVIEW_REPO>/scripts/config/modeling_policy.R`
  - Change the global regression confounder set from dichotomized age to continuous `age_at_diagnosis`.
  - Keep dichotomized age available for descriptive/subgroup surfaces only where explicitly labeled exploratory.
- `<PEER_REVIEW_REPO>/scripts/analysis/survival_outcomes.R`
  - Route Objective 1 local-recurrence-free and metastasis-free survival outputs into Objective 1 folders.
  - Add optional Cox horizon truncation for 5-year HR sensitivity while preserving full KM plotting.
  - Add clearer PH diagnostic reporting metadata for reviewer response.
- `<PEER_REVIEW_REPO>/scripts/analysis/binary_outcomes.R`
  - Remove recurrence/metastasis logistic regression as a primary Objective 1 output.
  - Retain descriptive event counts and competing-risk cumulative incidence as descriptive/supportive artifacts, with language that they are not co-primary inferential estimands.
- `<PEER_REVIEW_REPO>/scripts/workflow/objective_1_primary_outcomes.R`
  - Replace `recurrence_rates` and `mets_rates` with Cox-led time-to-event outputs while retaining descriptive support tables.
  - Add recurrence and metastasis PH diagnostics.
  - Add 5-year-capped OS/PFS Cox sensitivity outputs.
  - Update interpretation notes to match the reviewer-response estimand hierarchy.
- `<PEER_REVIEW_REPO>/scripts/analysis/vision_safety_analysis.R`
  - Add a minimum-follow-up sensitivity for visual acuity with explicit treatment-to-`last_followup` timing and separately labeled proxy general-follow-up timing.
  - Add reviewer-facing feasibility notes explaining which suggested visual predictors are available, which are absent, and which were checked only in derived runtime data versus raw/source files.
  - Add a separate ANCOVA-style latest-VA sensitivity model if model support remains adequate after data checks; do not add baseline vision as an ordinary covariate to the change-score model.
  - Clarify SRD/SRG toxicity scope: derive radiation-induced endpoints if source fields support it, otherwise label current SRD/SRG outputs as all-cause recorded burden and do not present them as radiation-induced events.
- `<PEER_REVIEW_REPO>/scripts/analysis/tumor_height_analysis.R`
  - Add timing from treatment to height assessment.
  - Use the timing distribution to retain the existing baseline-height sensitivity model as a limited secondary association or demote the comparative output; do not condition the model on post-treatment measurement time.
- `<PEER_REVIEW_REPO>/scripts/subgroup/subgroup_data_prep.R`
  - Remove PRAME from local-recurrence subgroup output when no event support exists.
  - Exclude T4 from reviewer-facing subgroup tables/forest plots.
- `<PEER_REVIEW_REPO>/scripts/visualization/forest_plot_data.R`
  - Ensure subgroup diagnostics record exclusion reasons for PRAME/T4 removal.
- `<PEER_REVIEW_REPO>/docs/METHODS_SECTION_PAPER.md`
  - Rewrite methods to reflect Cox-led recurrence/metastasis, continuous age modeling, descriptive Table 1, PH diagnostics, limited subgroup interpretation, and updated visual/tumor-height limitations.
- `<PEER_REVIEW_REPO>/docs/CALCULATIONS.md`
  - Clarify current PFS definition and death handling.
  - Clarify dichotomized age is not used in reviewer-response adjusted models.
  - Clarify the visual-acuity endpoint, `last_vision`/`last_followup` timing contract, and tumor-height imaging timing.
- `<PEER_REVIEW_REPO>/docs/STATISTICAL_METHODS.md`
  - Align formal statistical methods with revised reviewer-response estimands.
- `<PEER_REVIEW_REPO>/docs/INTERPRETATION_GUIDE.md`
  - Add manuscript-language guardrails: no equivalence, no simulated randomization, no broad vision preservation.
- `<PEER_REVIEW_REPO>/tests/testthat/helper-peer-review-revision.R`
  - Shared test helper for peer-review artifact freshness, workbook validation, endpoint-contract checks, and Objective 1 test wrapper reuse.
- `<PEER_REVIEW_REPO>/tests/testthat/test_objective3_objective4_scope_protection.R`
  - Lightweight tests that Objective 1 endpoint/routing changes do not silently alter Objective 3 PFS-2 semantics or Objective 4 GEP MFS/MSS routing.
- `<PEER_REVIEW_REPO>/tests/testthat/test_peer_review_artifact_verification.R`
  - Tests/helpers for stale-artifact prevention and verification path correctness.

### Files To Create

- `<PEER_REVIEW_REPO>/scripts/tools/peer_review_followup_audit.R`
  - New explicit reviewer-response audit tool for follow-up distributions, candidate latest-VA timing fields, height-imaging timing, treatment-detail availability, restricted-cohort cutoff verification, and dose/proximity field availability/absence across derived runtime data and raw/source files. This is run on demand and is not sourced by `scripts/load_all.R`. Because this workbook is an internal inspection artifact, it may include a `clickable_paths` sheet with absolute paths and Markdown `file://` links to the source RDS, active curated workbook, and generated audit workbook.
- `<PEER_REVIEW_REPO>/scripts/tools/propensity_score_feasibility.R`
  - New explicit reviewer-response feasibility tool for propensity-score overlap diagnostics. This is run on demand and is not sourced by `scripts/load_all.R`; promote any reportable sensitivity model into the workflow only after feasibility is reviewed.
- `<PEER_REVIEW_REPO>/tests/testthat/test_peer_review_revision_contract.R`
  - New tests for reviewer-response model contracts and output artifacts.
- `<PEER_REVIEW_REPO>/docs/peer_review_revision_response.md`
  - The sole coauthor-facing response document. It contains one checklist item per reviewer/Tim request, each with the request, approved action, exact methods/endpoint, result, manuscript change, limitation, and runtime-artifact evidence.

### Artifact Boundary After Full Pipeline

The peer-review worktree is the source and response-document workspace. It contains changed R code, tests, and the single coauthor-facing document at `docs/peer_review_revision_response.md`. It does **not** become an output-data root.

Raw input files remain canonical and shared under `/Users/ncamarda/Library/CloudStorage/OneDrive-Personal/Research/uveal_melanoma/Original Files/`. Generated analytic datasets, workbooks, plots, diagnostics, logs, and tool outputs must be isolated under `/Users/ncamarda/ProjectsRuntime/uveal_melanoma-peer-review-statistical-revision/`. They are not committed to Git and are not copied into the worktree. Do not add new scattered prose note files to runtime output folders for this revision; the single response document records the reviewer-relevant interpretation and cites exact runtime artifacts with stable aliases. Internal audit workbooks may include absolute clickable path metadata for operator inspection. Synced/published output remains a deliberate later export step and must not be used as the default full-workflow output target during the revision.

- `01_Efficacy/a_recurrence/*local_recurrence_free_probability_effect_summary.xlsx`
- `01_Efficacy/a_recurrence/*local_recurrence_free_probability_km.png`
- `01_Efficacy/a_recurrence/*local_recurrence_free_probability_proportional_hazards_*`
- `01_Efficacy/b_metastatic_progression/*metastasis_free_survival_probability_effect_summary.xlsx`
- `01_Efficacy/b_metastatic_progression/*metastasis_free_survival_probability_km.png`
- `01_Efficacy/b_metastatic_progression/*metastasis_free_survival_probability_proportional_hazards_*`
- `01_Efficacy/c_overall_survival/*overall_survival_probability_5yr_capped_effect_summary.xlsx`
- `01_Efficacy/d_progression_free_survival/*progression_free_survival_probability_5yr_capped_effect_summary.xlsx`
- `01_Efficacy/*/*_km.png` figures should preserve observed follow-up; do not create a separate capped plotting dataset or administratively censor KM display data solely to improve appearance. If late tails are unstable, handle that in figure selection/captioning or with an explicitly approved sensitivity analysis, not by silently changing plotted event/censoring data.
- `peer_review_revision_audits/*followup_and_data_availability.xlsx`
- `02_Safety/a_vision_changes/*vision_followup_sensitivity.xlsx` (latest-VA follow-up thresholds with explicit and proxy timing surfaces)
- `01_Efficacy/e_tumor_height_primary/*tumor_height_timing_summary.xlsx`

## Reviewer/Tim To-Do Synthesis

### Nick-Owned Or Nick-Led

- Cox-only recurrence/metastasis revision.
- PH assumption confirmation and response wording.
- Continuous-variable/dichotomization audit.
- PFS definition/death handling clarification.
- 5-year-capped OS/PFS HR sensitivity.
- Propensity-score feasibility and, if defensible, sensitivity analysis.
- Minimum-follow-up visual-acuity sensitivity confirmation or implementation.
- Follow-up duration summaries for survival and visual-acuity sensitivity, plus tumor-height imaging timing.
- PRAME/T4 subgroup pruning.
- Data availability audit for dose/proximity/visual-field/treatment-detail variables.
- Methods/statistical response language for the above.

### Clinical Coauthor-Owned With Nick Support

- PBT prescription dose/depth/implant-duration technique prose.
- GK prescription/isodose/max dose/target/margin/setup/constraint technique prose.
- Whether adverse-event grading beyond visual acuity exists and how to describe it.
- References replacing UpToDate/EyeWiki.
- Final wording of clinical limitations around optic nerve, macula/fovea, visual field, and treatment planning.

### Team Decision Points

- Confirming, not debating, the PFS correction: the reviewer specifically asks for PFS definition/death handling, while the internal code audit shows a separate endpoint-alignment issue: the implemented PFS is local recurrence or death and omits metastatic progression. Because the manuscript label/wording and conventional PFS meaning require metastatic progression to be represented, the default implementation is to rederive PFS as time to the first of local recurrence, metastatic progression, or death. Only an explicit documented instruction from Tim/Angie should override this. If they do override it, the existing endpoint cannot be called PFS.
- PFS-2 endpoint decision: for this reviewer-response branch, protect Objective 3 from accidental contradiction or breakage caused by Objective 1 PFS changes. Do not perform a full Objective 3 PFS-2 reanalysis unless Tim/Angie explicitly request it. If PFS-2 remains visible in reviewer-facing materials, its label must match its implementation precisely: either true post-salvage second progression-free survival if rederived, or time from salvage treatment to second local recurrence if the current implementation is retained.
- Whether propensity score diagnostics are good enough to report as a sensitivity analysis or only as an attempted feasibility assessment.
- Whether tumor-height reduction remains an analysis endpoint or becomes descriptive/secondary due to timing confounding.
- Whether visual acuity should include baseline vision in the primary adjusted model or be presented as a sensitivity model if model support is thin. Resolved in implementation: baseline VA is included in a separate latest-VA sensitivity model, not in the change-score model.
- Whether OS/PFS plots keep log-rank p-values visibly printed. Reviewer 2 suggested demoting log-rank; the safer path is to keep KM curves and risk tables but remove visible log-rank p-values from manuscript figures.

---

## Goal Group 1: Worktree, Provenance, And Test Helpers

**Tasks:** 1, 2A, 2, 2B.

**Recommended Codex goal text for Nick:**

```text
Execute Goal Group 1 from docs/superpowers/plans/2026-06-26-peer-review-statistical-revision.md: create the peer-review worktree from the exact preserved baseline, commit the plan only on the peer-review branch, add shared peer-review test helpers, artifact freshness checks, reviewer revision contract tests, and Objective 3/4 scope-protection tests. Stop after Task 2B and report changed files, exact commands run, tests passed/failed, runtime artifacts generated, unresolved decisions/blockers, remaining task IDs, and the recommended next Codex goal text.
```

**Required end-of-goal checkpoint:** changed files, exact commands run, tests passed/failed, runtime artifacts generated, unresolved decisions/blockers, remaining task IDs, and recommended next goal text.

### Task 1: Create The Peer-Review Worktree And Preserve The Correct Baselines

**Files:**
- No source file edits.

- [ ] **Step 1: Confirm the current post-submission analysis baseline is clean**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma
git status --short --branch
git log --oneline --decorate -1
```

Expected:

```text
## master...origin/master
7465447 (HEAD -> master, origin/master, origin/HEAD) fix: Add outputs directory to .gitignore to prevent tracking of generated files
```

- [ ] **Step 2: Tag the current post-submission/pre-peer-review code state accurately**

Do not create a Git tag named for the May 3 submission: the submission was an external Word document, and `7465447` was created more than a month later. Preserve the current reproducible code state under a name that states what it is.

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma
if ! git rev-parse -q --verify refs/tags/post-submission-pre-peer-review-analysis-2026-06-11 >/dev/null; then
  git tag post-submission-pre-peer-review-analysis-2026-06-11 7465447
fi
git tag --list 'post-submission-pre-peer-review-analysis-*'
git rev-list -n 1 post-submission-pre-peer-review-analysis-2026-06-11
```

Expected:

```text
post-submission-pre-peer-review-analysis-2026-06-11
7465447...
```

- [ ] **Step 3: Record the submitted-manuscript evidence in the response workspace**

In the peer-review response packet, record these immutable references before changing any manuscript wording:

```text
Submitted manuscript: ARO - UM - ANONYMIZED MANUSCRIPT (4-27-26)-FINAL.docx
Journal / submission date: Advances in Radiation Oncology / May 3, 2026
Submission evidence: Gmail decision thread 19ef47f8cf7efd88
Submission-era generated outputs: /Users/ncamarda/Library/CloudStorage/OneDrive-Personal/Research/uveal_melanoma/Analysis/2026-04-27/
```

This is a provenance note, not a file copy. Do not add the manuscript or patient-level generated outputs to Git.

- [ ] **Step 4: Create a sibling worktree for the peer-review revision**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma
git worktree add -b peer-review-statistical-revision ../uveal_melanoma-peer-review-statistical-revision post-submission-pre-peer-review-analysis-2026-06-11
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git status --short --branch
git log --oneline -1
```

Expected:

```text
## peer-review-statistical-revision
7465447 fix: Add outputs directory to .gitignore to prevent tracking of generated files
```

- [ ] **Step 5: Commit the plan file only on the peer-review branch**

Copy or move the plan file into the peer-review worktree if needed, then commit it from `<PEER_REVIEW_REPO>`. Leave `<CANONICAL_REPO>`/`master` untouched.

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
mkdir -p docs/superpowers/plans
cp /Users/ncamarda/Projects/uveal_melanoma/docs/superpowers/plans/2026-06-26-peer-review-statistical-revision.md docs/superpowers/plans/2026-06-26-peer-review-statistical-revision.md
git add docs/superpowers/plans/2026-06-26-peer-review-statistical-revision.md
git commit -m "docs: add peer-review statistical revision plan"
```

Expected:

```text
[peer-review-statistical-revision ...] docs: add peer-review statistical revision plan
```

If the plan is already committed on `peer-review-statistical-revision`, `git status --short` should show no plan-file change and this step can be skipped. Do not commit this plan on `master`.

---

### Task 2A: Add Shared Peer-Review Test Helpers And Artifact Freshness Checks

**Files:**
- Create: `<PEER_REVIEW_REPO>/tests/testthat/helper-peer-review-revision.R`
- Create: `<PEER_REVIEW_REPO>/tests/testthat/test_peer_review_artifact_verification.R`

- [ ] **Step 1: Create shared test helpers**

Create `<PEER_REVIEW_REPO>/tests/testthat/helper-peer-review-revision.R` with:

```r
expect_workbook_has_sheets <- function(path, required_sheets) {
    expect_true(file.exists(path), info = paste("Missing workbook:", path))
    sheets <- readxl::excel_sheets(path)
    expect_true(
        all(required_sheets %in% sheets),
        info = paste("Workbook", path, "missing sheets:", paste(setdiff(required_sheets, sheets), collapse = ", "))
    )
    invisible(sheets)
}

expect_artifact_fresh_after <- function(path, started_at) {
    expect_true(file.exists(path), info = paste("Missing artifact:", path))
    artifact_time <- file.info(path)$mtime
    expect_true(
        !is.na(artifact_time) && artifact_time >= started_at,
        info = paste("Artifact is stale or has missing mtime:", path)
    )
}

expect_no_reviewer_facing_paths <- function(path) {
    text <- readLines(path, warn = FALSE)
    forbidden <- grep("/Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision|/Users/ncamarda/Projects/uveal_melanoma/docs|/Users/ncamarda/Projects/uveal_melanoma/scripts", text, value = TRUE)
    expect_length(forbidden, 0, info = paste("Committed reviewer-facing doc contains source-machine absolute paths:", paste(forbidden, collapse = "\n")))
}

run_objective1_test <- function(data, output_tag = "objective1_peer_review") {
    test_output_dir <- file.path(tempdir(), output_tag)
    output_dirs <- create_output_directories(test_output_dir)
    result <- run_objective_1(data, output_dirs = output_dirs, prefix = "test_", dataset_name = output_tag)
    list(results = result, output_dirs = output_dirs, test_output_dir = test_output_dir)
}

run_objective2_test <- function(data, output_tag = "objective2_peer_review") {
    test_output_dir <- file.path(tempdir(), output_tag)
    output_dirs <- create_output_directories(test_output_dir)
    result <- run_objective_2(data, output_dirs = output_dirs, prefix = "test_", dataset_name = output_tag)
    list(results = result, output_dirs = output_dirs, test_output_dir = test_output_dir)
}
```

- [ ] **Step 2: Add artifact verification tests**

Create `<PEER_REVIEW_REPO>/tests/testthat/test_peer_review_artifact_verification.R` with:

```r
test_that("peer-review response document avoids committed source-machine absolute paths", {
    response_path <- testthat::test_path("../../docs/peer_review_revision_response.md")
    if (!file.exists(response_path)) {
        skip("Response document is created later in the revision plan.")
    }
    expect_no_reviewer_facing_paths(response_path)
})

test_that("artifact freshness helper fails on missing files", {
    expect_error(
        expect_artifact_fresh_after(file.path(tempdir(), "missing_peer_review_artifact.xlsx"), Sys.time()),
        regexp = "Missing artifact"
    )
})
```

- [ ] **Step 3: Run helper tests**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_peer_review_artifact_verification.R')"
```

Expected:

```text
PASS
```

- [ ] **Step 4: Commit shared test helpers**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add tests/testthat/helper-peer-review-revision.R tests/testthat/test_peer_review_artifact_verification.R
git commit -m "test: add peer-review revision artifact helpers"
```

Expected:

```text
[peer-review-statistical-revision ...] test: add peer-review revision artifact helpers
```

---

### Task 2: Add Reviewer Revision Contract Tests

**Files:**
- Create: `<PEER_REVIEW_REPO>/tests/testthat/test_peer_review_revision_contract.R`
- Modify later tasks: `<PEER_REVIEW_REPO>/scripts/config/modeling_policy.R`
- Modify later tasks: `<PEER_REVIEW_REPO>/scripts/workflow/objective_1_primary_outcomes.R`
- Modify later tasks: `<PEER_REVIEW_REPO>/scripts/analysis/survival_outcomes.R`

- [ ] **Step 1: Write the failing reviewer-response contract tests**

Create `<PEER_REVIEW_REPO>/tests/testthat/test_peer_review_revision_contract.R` with this content:

```r
test_that("reviewer-response adjusted models use continuous age rather than dichotomized age", {
    expect_true("age_at_diagnosis" %in% confounders)
    expect_false("age_at_diagnosis_general_pop_median" %in% confounders)
})

test_that("Objective 1 returns Cox-led local recurrence and metastasis time-to-event analyses", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "peer_review_objective1_tte_contract")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    expect_true("recurrence_time_to_event" %in% names(pipeline$results))
    expect_true("mets_time_to_event" %in% names(pipeline$results))
    expect_s3_class(pipeline$results$recurrence_time_to_event$cox_model, "coxph")
    expect_s3_class(pipeline$results$mets_time_to_event$cox_model, "coxph")

    recurrence_summary <- file.path(
        pipeline$output_dirs$obj1_recurrence,
        "test_local_recurrence_free_probability_effect_summary.xlsx"
    )
    metastasis_summary <- file.path(
        pipeline$output_dirs$obj1_mets,
        "test_metastasis_free_survival_probability_effect_summary.xlsx"
    )
    expect_true(file.exists(recurrence_summary))
    expect_true(file.exists(metastasis_summary))

    recurrence_rows <- readxl::read_xlsx(recurrence_summary)
    metastasis_rows <- readxl::read_xlsx(metastasis_summary)
    expect_true(any(recurrence_rows$effect_measure == "HR"))
    expect_true(any(metastasis_rows$effect_measure == "HR"))
    expect_false(any(recurrence_rows$effect_measure == "OR"))
    expect_false(any(metastasis_rows$effect_measure == "OR"))
})

test_that("Objective 1 recurrence and metastasis descriptive summaries are not labeled co-primary", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "peer_review_descriptive_event_support")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    recurrence_summary_path <- file.path(pipeline$output_dirs$obj1_recurrence, "test_recurrence1_event_support_summary.xlsx")
    mets_summary_path <- file.path(pipeline$output_dirs$obj1_mets, "test_mets_progression_event_support_summary.xlsx")

    expect_true(file.exists(recurrence_summary_path))
    expect_true(file.exists(mets_summary_path))

    for (summary_path in c(recurrence_summary_path, mets_summary_path)) {
        expect_true(all(c(
            "descriptive_event_counts",
            "cumulative_incidence",
            "competing_risk_support",
            "estimand_notes"
        ) %in% readxl::excel_sheets(summary_path)))

        estimand_notes <- readxl::read_xlsx(summary_path, sheet = "estimand_notes")
        expect_true(all(estimand_notes$role %in% c("descriptive_support", "supportive_time_to_event_context")))
        expect_false(any(estimand_notes$role == "co-primary"))
    }
})

test_that("Objective 1 writes five-year capped OS and PFS Cox sensitivity summaries", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "peer_review_five_year_capped_contract")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    os_summary <- file.path(
        pipeline$output_dirs$obj1_os,
        "test_overall_survival_probability_5yr_capped_effect_summary.xlsx"
    )
    pfs_summary <- file.path(
        pipeline$output_dirs$obj1_pfs,
        "test_progression_free_survival_probability_5yr_capped_effect_summary.xlsx"
    )

    expect_true(file.exists(os_summary))
    expect_true(file.exists(pfs_summary))

    os_rows <- readxl::read_xlsx(os_summary)
    pfs_rows <- readxl::read_xlsx(pfs_summary)
    expect_true(any(grepl("5-year capped", os_rows$model_label, fixed = TRUE)))
    expect_true(any(grepl("5-year capped", pfs_rows$model_label, fixed = TRUE)))
    expect_true(all(os_rows$n_patients >= os_rows$n_events, na.rm = TRUE))
    expect_true(all(pfs_rows$n_patients >= pfs_rows$n_events, na.rm = TRUE))

    ph_or_skip_patterns <- c(
        "test_overall_survival_probability_5yr_capped_ph_diagnostics\\.xlsx$",
        "test_overall_survival_probability_5yr_capped_skipped.*\\.(xlsx|txt)$",
        "test_progression_free_survival_probability_5yr_capped_ph_diagnostics\\.xlsx$",
        "test_progression_free_survival_probability_5yr_capped_skipped.*\\.(xlsx|txt)$"
    )
    for (pattern in ph_or_skip_patterns[c(1, 2)]) {
        os_hits <- list.files(pipeline$output_dirs$obj1_os, pattern = pattern, full.names = TRUE)
        if (length(os_hits) > 0) break
    }
    for (pattern in ph_or_skip_patterns[c(3, 4)]) {
        pfs_hits <- list.files(pipeline$output_dirs$obj1_pfs, pattern = pattern, full.names = TRUE)
        if (length(pfs_hits) > 0) break
    }
    expect_true(length(os_hits) > 0, info = "Capped OS model must write PH diagnostics or an explicit skip artifact.")
    expect_true(length(pfs_hits) > 0, info = "Capped PFS model must write PH diagnostics or an explicit skip artifact.")
})

test_that("Objective 1 KM figures cap display at SURVIVAL_XAXIS_MAX_MONTHS while Cox models keep full follow-up", {
    data <- create_test_dataset()
    data$tt_recurrence_months[1] <- 187
    data$recurrence_event[1] <- 1
    data$tt_pfs_months[1] <- 187
    data$pfs_event[1] <- 1

    pipeline <- run_objective1_test(data, output_tag = "peer_review_km_export_padding")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    recurrence_plot <- pipeline$results$recurrence_time_to_event$plot$plot
    pfs_plot <- pipeline$results$pfs_analysis$plot$plot
    recurrence_x_range <- ggplot2::ggplot_build(recurrence_plot)$layout$panel_params[[1]]$x.range
    pfs_x_range <- ggplot2::ggplot_build(pfs_plot)$layout$panel_params[[1]]$x.range

    axis_cap_tolerance <- SURVIVAL_XAXIS_MAX_MONTHS * 0.05

    expect_lte(max(recurrence_x_range), SURVIVAL_XAXIS_MAX_MONTHS + axis_cap_tolerance)
    expect_lte(max(pfs_x_range), SURVIVAL_XAXIS_MAX_MONTHS + axis_cap_tolerance)
    expect_gt(max(data$tt_recurrence_months, na.rm = TRUE), SURVIVAL_XAXIS_MAX_MONTHS)
    expect_gt(max(data$tt_pfs_months, na.rm = TRUE), SURVIVAL_XAXIS_MAX_MONTHS)
})
```

- [ ] **Step 2: Run the new test file and confirm it fails for the expected reasons**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_peer_review_revision_contract.R')"
```

Expected failure patterns:

```text
Failure ... "age_at_diagnosis" %in% confounders is not TRUE
Failure ... "recurrence_time_to_event" %in% names(pipeline$results) is not TRUE
Failure ... "mets_time_to_event" %in% names(pipeline$results) is not TRUE
```

- [ ] **Step 3: Commit the failing tests**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add tests/testthat/test_peer_review_revision_contract.R
git commit -m "test: capture peer-review statistical revision contracts"
```

Expected:

```text
[peer-review-statistical-revision ...] test: capture peer-review statistical revision contracts
```

---

### Task 2B: Add Objective 3/4 Scope-Protection Checks

**Files:**
- Create: `<PEER_REVIEW_REPO>/tests/testthat/test_objective3_objective4_scope_protection.R`
- Modify only if tests reveal a real regression: Objective 3/4 source files.

- [ ] **Step 1: Add Objective 3/4 protection tests**

Create `<PEER_REVIEW_REPO>/tests/testthat/test_objective3_objective4_scope_protection.R` with:

```r
test_that("Objective 3 PFS-2 endpoint contract is explicit and isolated from Objective 1 PFS", {
    source_text <- paste(readLines(here::here("scripts", "data_helper", "data_derivation.R"), warn = FALSE), collapse = "\n")

    expect_true(grepl("pfs2_second_recurrence_observed", source_text, fixed = TRUE))
    expect_true(grepl("recurrence2", source_text, fixed = TRUE))
    expect_true(grepl("pfs2_event", source_text, fixed = TRUE))
    expect_true(grepl("tt_pfs2_months", source_text, fixed = TRUE))
    expect_false(grepl("pfs2_event\\s*=\\s*if_else\\([^\\n]*(mets_event|death_event)", source_text))
})

test_that("Objective 4 GEP MFS/MSS endpoints remain separate from Objective 1 PFS", {
    mfs_text <- paste(readLines(here::here("scripts", "gep", "cores", "gep_evaluation_core_mfs.R"), warn = FALSE), collapse = "\n")
    mss_text <- paste(readLines(here::here("scripts", "gep", "cores", "gep_evaluation_core_mss.R"), warn = FALSE), collapse = "\n")

    expect_true(grepl('event_var = "mets_event"', mfs_text, fixed = TRUE))
    expect_true(grepl('melanoma_event_var = "melanoma_death_event"', mss_text, fixed = TRUE))
    expect_true(grepl('competing_event_var = "competing_death_event"', mss_text, fixed = TRUE))
    expect_false(grepl("pfs_event|tt_pfs_months", mfs_text))
    expect_false(grepl("pfs_event|tt_pfs_months", mss_text))
})
```

These tests do not decide whether PFS-2 should be redefined. They prevent Objective 1 PFS edits from silently changing Objective 3/4 contracts before the team makes that endpoint decision.

- [ ] **Step 2: Run scope-protection tests**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_objective3_objective4_scope_protection.R')"
```

Expected:

```text
PASS
```

- [ ] **Step 3: Commit scope-protection tests**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add tests/testthat/test_objective3_objective4_scope_protection.R
git commit -m "test: protect objective 3 and 4 endpoint scope"
```

Expected:

```text
[peer-review-statistical-revision ...] test: protect objective 3 and 4 endpoint scope
```

---

## Goal Group 2: Objective 1 Endpoint And Modeling

**Tasks:** 3, 3A, 3B, 4, 5.

**Recommended Codex goal text for Nick:**

```text
Execute Goal Group 2 from docs/superpowers/plans/2026-06-26-peer-review-statistical-revision.md in the peer-review worktree: implement the Objective 1 endpoint/modeling changes, including continuous age, corrected PFS contract, endpoint-label audit, recurrence/metastasis Cox-led outputs, and 5-year-capped OS/PFS sensitivity. Stop after Task 5 and report changed files, exact commands run, tests passed/failed, runtime artifacts generated, unresolved decisions/blockers, remaining task IDs, and the recommended next Codex goal text.
```

**Required end-of-goal checkpoint:** changed files, exact commands run, tests passed/failed, runtime artifacts generated, unresolved decisions/blockers, remaining task IDs, and recommended next goal text.

### Task 3: Switch Adjusted Models To Continuous Age

**Files:**
- Modify: `<PEER_REVIEW_REPO>/scripts/config/modeling_policy.R`
- Modify: `<PEER_REVIEW_REPO>/docs/CALCULATIONS.md`
- Modify: `<PEER_REVIEW_REPO>/docs/METHODS_SECTION_PAPER.md`
- Test: `<PEER_REVIEW_REPO>/tests/testthat/test_peer_review_revision_contract.R`

- [ ] **Step 1: Replace the global confounder vector**

In `<PEER_REVIEW_REPO>/scripts/config/modeling_policy.R`, replace the `confounders <- c(...)` block with:

```r
# Reviewer-response adjusted models keep age continuous to avoid reviewer-flagged
# loss of information from dichotomization. Dichotomized age remains available for
# descriptive and exploratory subgroup displays only.
confounders <- c(
    "age_at_diagnosis", "sex", "location"
)
```

- [ ] **Step 2: Preserve dichotomized age only as exploratory subgroup context**

In the same file, keep `subgroup_vars` as currently configured unless Task 10 removes a sparse reviewer-facing subgroup variable. Add this comment immediately before `subgroup_vars <- c(`:

```r
# These subgroup variables are exploratory display surfaces. They are not the
# default adjusted-model covariate set for the reviewer-response analyses.
```

- [ ] **Step 3: Update age documentation**

In `<PEER_REVIEW_REPO>/docs/CALCULATIONS.md`, replace the "Key Details" bullet that says dichotomized age is used in model covariate adjustments with:

```markdown
- `GENERAL_POP_MEDIAN_AGE_CUTOFF` is defined in `config_constants.R` and currently equals **63**; changing it there automatically updates the derived descriptive/subgroup field.
- Output labels always render as “< 63 years” and “≥ 63 years” to match descriptive table and exploratory subgroup wording.
- In the peer-review revision, adjusted treatment-effect models use continuous `age_at_diagnosis`; `age_at_diagnosis_general_pop_median` is retained for descriptive and exploratory subgroup displays only.
```

- [ ] **Step 4: Run the targeted test for continuous age**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_peer_review_revision_contract.R', filter='continuous age')"
```

Expected:

```text
PASS
```

- [ ] **Step 5: Commit continuous-age policy**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add scripts/config/modeling_policy.R docs/CALCULATIONS.md
git commit -m "fix: use continuous age in adjusted reviewer-response models"
```

Expected:

```text
[peer-review-statistical-revision ...] fix: use continuous age in adjusted reviewer-response models
```

---

### Task 3A: Resolve The PFS Endpoint Mismatch Before Any PFS Reanalysis

**Files:**
- Modify: `<PEER_REVIEW_REPO>/scripts/data_helper/data_derivation.R`
- Modify: `<PEER_REVIEW_REPO>/scripts/utils/objective0_validation_engine.R`
- Modify: `<PEER_REVIEW_REPO>/docs/CALCULATIONS.md`
- Test: the Objective 0 derivation/validation tests that assert `pfs_event` and `tt_pfs_months`.

- [ ] **Step 1: Confirm and document the endpoint correction before rerunning results**

Separate the reviewer request from the internal implementation finding. The reviewer is asking whether deaths without documented progression were counted in PFS and wants the endpoint definition made explicit. The internal code audit shows that the current implementation sets `pfs_event = recurrence_event | death_event` and `tt_pfs_months = min(tt_recurrence_months, tt_death_months)`, so death is already included, but metastatic progression is omitted. Do not resolve that discrepancy by changing only a caption.

Reviewer-response default: define PFS as time from treatment to the first local recurrence, metastatic progression, or death from any cause. This answers the reviewer directly, retains death as an event, and matches the label "PFS".

Do not preserve the current local-recurrence-or-death implementation unless Tim/Angie explicitly instruct Nick to do so after seeing the endpoint mismatch. If that explicit override occurs, document it in `docs/peer_review_revision_response.md`, rename the endpoint throughout the manuscript, figures, response, documentation, and output paths as a local-recurrence-or-death-free endpoint, and do not call it PFS.

- [ ] **Step 2: Rederive and validate standard PFS**

In `scripts/data_helper/data_derivation.R`, make the endpoint contract explicit:

```r
tt_pfs_months = pmin(
    tt_recurrence_months,
    tt_mets_months,
    tt_death_months,
    na.rm = FALSE
),
pfs_event = if_else(
    recurrence_event == 1 | mets_event == 1 | death_event == 1,
    1L,
    0L
),
```

Update `objective0_validation_engine.R` so its expected PFS time/event derives from the same three component endpoints. The derived fields `pfs_event`, `tt_pfs_months`, and any analysis-time twin such as `tt_pfs_months_analysis` must all use the identical first-event contract. Add fixture rows where local recurrence is earliest, metastatic progression is earliest, and death occurs without recurrence/metastasis. Rerun the cohort derivation and all Objective 1 PFS analyses/figures after the contract changes.

- [ ] **Step 3: Make the response text match the approved endpoint**

The response must say exactly which components are events and whether death without documented progression is included. It must not state or imply that the April 27 PFS output used a different endpoint than the actual code.

---

### Task 3B: Run An Endpoint-Label Mismatch Audit Before Reviewer-Facing Output

**Files:**
- Modify/create: `<PEER_REVIEW_REPO>/docs/peer_review_revision_response.md`
- Modify, only if mismatches are found: endpoint-specific source/docs listed below.

- [ ] **Step 1: Audit every reviewer-facing outcome label against code and runtime fields**

Before revising tables, figures, or response wording, create an `Endpoint and claim audit` section in `docs/peer_review_revision_response.md`. For each reviewer-facing outcome, record:

```text
Outcome label:
Manuscript wording:
Code path:
Time variable:
Event variable:
Event definition:
Censoring/competing-event rule:
Runtime source fields:
Reviewer question addressed:
Action: keep / rederive / rename / demote / remove
```

This audit is a blocking gate. If the label, manuscript wording, and code implementation do not match, do not proceed by caption edit alone.

- [ ] **Step 2: Treat the following as high-risk mismatch surfaces**

Audit these explicitly because they have the same failure mode as the PFS issue:

- **PFS:** current code is local recurrence or death; reviewer asked about death handling; manuscript-facing PFS wording must be reconciled with metastatic progression handling.
- **PFS-2:** protect Objective 3 from accidental contradiction or breakage while correcting Objective 1 PFS. Do not blindly copy the Objective 1 composite into Objective 3. The current code treats the endpoint as second local recurrence after salvage treatment, with death before second recurrence censored. Unless Tim/Angie explicitly request Objective 3 reanalysis in this revision, do not rederive PFS-2; instead verify that any retained label defines the endpoint precisely as time from salvage treatment to second local recurrence, with death before second local recurrence censored, and does not imply metastatic progression or death are PFS-2 events. If the manuscript should report true post-salvage second progression-free survival, create a separate implementation task before changing Objective 3.
- **Local recurrence and metastatic progression:** submitted/reviewer-facing binary event-rate models ignore unequal follow-up; revised inference should use time-to-event Cox outputs, with binary rates retained only descriptively.
- **Post-baseline recurrence-stratified or metastasis-stratified OS/PFS:** these use post-treatment status as the stratifier and must not appear as baseline treatment-comparison evidence.
- **Vision change:** `last_vision` is a numeric acuity value. The endpoint audit reports explicit treatment-to-`last_followup` timing and a proxy general-follow-up timing surface separately.
- **Tumor-height change:** `last_height_date` exists, but treatment-to-height-assessment timing differs by treatment group and runtime data include some negative treatment-to-height-date intervals. Summarize timing and investigate negative intervals before deciding whether comparative tumor-height regression remains reviewer-facing.
- **Radiation adverse events:** current toxicity endpoints are recorded burden by available follow-up, not time-to-toxicity incidence and not CTCAE-style graded adverse events unless separate grading data are identified.
- **Cumulative-incidence summaries:** Gray test p-values are global across-curve comparisons, not horizon-specific p-values.
- **Dosimetry/proximity fields:** PBT has limited treatment-detail fields; GK plan dose/isodose/shot/isocenter and macula/fovea/proximity fields were not found in the current runtime dataset or checked master source columns. The audit must state which files were checked before calling a field absent.

- [ ] **Step 3: Add a regression check for the audit**

Add a test that fails if `docs/peer_review_revision_response.md` lacks completed audit rows for PFS, PFS-2 if still reported, local recurrence, metastatic progression, vision change, tumor-height change, adverse events, and dosimetry/proximity availability.

Expected response-document standard: every outcome has a single internally consistent endpoint definition, and every mismatch is either rederived, renamed, demoted, or removed.

---

### Task 4: Replace Recurrence And Metastasis Logistic Inference With Cox-Led Time-To-Event Outputs

**Files:**
- Modify: `<PEER_REVIEW_REPO>/scripts/analysis/survival_outcomes.R`
- Modify: `<PEER_REVIEW_REPO>/scripts/analysis/binary_outcomes.R`
- Modify: `<PEER_REVIEW_REPO>/scripts/workflow/objective_1_primary_outcomes.R`
- Test: `<PEER_REVIEW_REPO>/tests/testthat/test_peer_review_revision_contract.R`
- Test: `<PEER_REVIEW_REPO>/tests/testthat/test_objective1_primary_outcomes.R`

The shared PH diagnostic helper already existed at submission and produced OS/PFS Schoenfeld-residual outputs. Reuse that helper. This task adds diagnostics only for the two new Cox endpoint models; it does not rebuild or relitigate the existing OS/PFS diagnostic implementation.

- [ ] **Step 1: Add explicit survival output routing**

In `<PEER_REVIEW_REPO>/scripts/analysis/survival_outcomes.R`, update `determine_survival_output_dir()` to prefer an explicit `route_key` instead of inferring Objective 1 versus Objective 4 from the y-axis label. This prevents Objective 4 GEP metastasis-free survival from being misrouted into Objective 1 when both output directories exist.

```r
determine_survival_output_dir <- function(ylab, output_dirs, route_key = NULL) {
    if (is.null(output_dirs)) {
        return(getwd())
    }

    default_dir <- output_dirs$baseline_characteristics %||% getwd()
    route_map <- c(
        obj1_recurrence = "obj1_recurrence",
        obj1_mets = "obj1_mets",
        obj1_os = "obj1_os",
        obj1_pfs = "obj1_pfs",
        obj3_pfs2 = "obj3_pfs2",
        obj4_mfs = "obj4_mfs",
        obj4_mss = "obj4_mss"
    )

    if (!is.null(route_key)) {
        if (!route_key %in% names(route_map)) {
            stop(sprintf("Unknown survival output route_key `%s`.", route_key), call. = FALSE)
        }
        output_name <- route_map[[route_key]]
        if (!is.null(output_dirs[[output_name]])) {
            return(output_dirs[[output_name]])
        }
        logger::log_warn("Output directory for route_key {route_key} not provided; using baseline_characteristics as fallback")
        return(default_dir)
    }

    # Backward-compatible fallback for existing callers only. New Objective 1
    # and Objective 4 calls must pass route_key explicitly.
    if (grepl("Local Recurrence-Free", ylab) && !is.null(output_dirs$obj1_recurrence)) return(output_dirs$obj1_recurrence)
    if (grepl("Overall Survival", ylab) && !is.null(output_dirs$obj1_os)) return(output_dirs$obj1_os)
    if (grepl("Progression-Free Survival", ylab) && !is.null(output_dirs$obj1_pfs)) return(output_dirs$obj1_pfs)
    if (grepl("PFS-2", ylab) && !is.null(output_dirs$obj3_pfs2)) return(output_dirs$obj3_pfs2)
    if (grepl("Metastasis-Free Survival", ylab)) {
        if (!is.null(output_dirs$obj4_mfs)) {
            return(output_dirs$obj4_mfs)
        }
        if (!is.null(output_dirs$obj1_pfs)) {
            return(output_dirs$obj1_pfs)
        }
        logger::log_warn("Output directory for Metastasis-Free Survival not provided; using baseline_characteristics as fallback")
    }

    default_dir
}
```

Update all new Objective 1 survival calls to pass `route_key = "obj1_recurrence"`, `route_key = "obj1_mets"`, `route_key = "obj1_os"`, or `route_key = "obj1_pfs"` as appropriate. Update Objective 4 GEP MFS/MSS calls, if this helper is used there, to pass `route_key = "obj4_mfs"` or `route_key = "obj4_mss"`.

Add a scope-protection test where `output_dirs` contains both `obj1_mets` and `obj4_mfs`; a Metastasis-Free Survival call with `route_key = "obj4_mfs"` must return `obj4_mfs`, and a call with `route_key = "obj1_mets"` must return `obj1_mets`.

- [ ] **Step 2: Reclassify recurrence/metastasis event-support notes**

In `<PEER_REVIEW_REPO>/scripts/analysis/binary_outcomes.R`, replace `build_objective1_binary_estimand_notes()` with:

```r
#' Build Objective 1 reviewer-response event-support notes
#'
#' @param outcome_var Character outcome variable name.
#' @param time_var Character event-time variable name used for cumulative incidence.
#' @param event_var Character event indicator variable name.
#' @return Data frame describing descriptive and supportive event summaries.
build_objective1_binary_estimand_notes <- function(outcome_var, time_var, event_var) {
    outcome_label <- dplyr::case_when(
        identical(outcome_var, "recurrence1") ~ "local recurrence",
        identical(outcome_var, "mets_progression") ~ "metastatic progression",
        TRUE ~ outcome_var
    )

    data.frame(
        estimand = c("descriptive_ever_observed", "competing_risk_cumulative_incidence"),
        role = c("descriptive_support", "supportive_time_to_event_context"),
        endpoint = outcome_label,
        interpretation = c(
            "Ever-observed event counts over available follow-up; not a censoring-aware treatment-effect estimand.",
            "Time-horizon event probability accounting for censoring and death before the event as a competing event; used as supportive context for Cox-led inference."
        ),
        time_variable = c(NA_character_, time_var),
        event_variable = c(event_var, event_var),
        death_handling = c(
            "Deaths are reflected only through available follow-up for the descriptive ever-observed count.",
            "Death before the event of interest is coded as a competing event."
        ),
        stringsAsFactors = FALSE
    )
}
```

- [ ] **Step 3: Rename event-support workbook output**

In `analyze_binary_outcome_rates()`, change the rates table metadata and output filename:

```r
rates <- fix_event_data %>%
    dplyr::group_by(!!sym(group_var)) %>%
    dplyr::summarise(
        n = dplyr::n(),
        events = sum(!!sym(event_var), na.rm = TRUE),
        rate = events / n * 100,
        .groups = "drop"
    ) %>%
    dplyr::mutate(
        estimand = "descriptive_ever_observed",
        estimand_role = "descriptive_support",
        notes = "Ever-observed event counts over available follow-up; adjusted Cox models are the lead reviewer-response inference."
    )
```

Replace the Excel write block's sheet/file names with:

```r
write_readable_xlsx(
    list(
        descriptive_event_counts = rates,
        cumulative_incidence = cumulative_incidence$summary,
        competing_risk_support = cumulative_incidence$support,
        estimand_notes = cumulative_incidence$notes
    ),
    path = file.path(output_dir, paste0(prefix, outcome_var, "_event_support_summary.xlsx"))
)
```

- [ ] **Step 4: Stop fitting logistic regression for recurrence/metastasis**

In `analyze_binary_outcome_rates()`, after the event-support workbook write block and before `model_variables <- unique(...)`, return the descriptive result:

```r
return(list(
    rates = rates,
    cumulative_incidence = cumulative_incidence,
    table = NULL,
    model = NULL,
    diagnostics = list(
        status = "not_fit",
        reason = "Reviewer-response analysis treats recurrence and metastasis as time-dependent endpoints; logistic regression is intentionally not fit."
    )
))
```

Remove the now-unreachable logistic-modeling block from `model_variables <- unique(c(group_var, confounders_to_use))` through the previous final `list(...)` return.

- [ ] **Step 5: Add recurrence/metastasis Cox calls in Objective 1**

In `<PEER_REVIEW_REPO>/scripts/workflow/objective_1_primary_outcomes.R`, replace lines 270-283 with:

```r
# 1a. Local recurrence: descriptive support plus Cox-led time-to-event analysis
logger::log_info(formatted("Executing recurrence event-support summary and Cox time-to-local-recurrence analysis", indent = 1))
recurrence_rates <- analyze_binary_outcome_rates(
    data,
    outcome_var = "recurrence1",
    time_var = "tt_recurrence_months",
    event_var = "recurrence_event",
    confounders = confounders,
    analysis_type = "post_treatment_only",
    dataset_name = dataset_name,
    output_dirs = output_dirs,
    prefix = prefix
)
recurrence_time_to_event <- analyze_time_to_event_outcomes(
    data,
    time_var = "tt_recurrence_months",
    event_var = "recurrence_event",
    group_var = "treatment_group",
    confounders = confounders,
    ylab = "Local Recurrence-Free Probability",
    analysis_type = "post_treatment_only",
    dataset_name = dataset_name,
    output_dirs = output_dirs,
    prefix = prefix
)
recurrence_time_to_event$ph_diagnostics <- run_or_skip_proportional_hazards_diagnostics(
    cox_model = recurrence_time_to_event$cox_model,
    outcome_name = "Local Recurrence-Free Probability",
    output_dir = output_dirs$obj1_recurrence,
    file_prefix = paste0(prefix, "local_recurrence_free_probability_"),
    dataset_name = dataset_name,
    data = data,
    time_var = "tt_recurrence_months",
    event_var = "recurrence_event",
    variables = unique(c("treatment_group", confounders)),
    reason = "Local recurrence proportional hazards diagnostics were not run because no Cox model was fit."
)
annotate_objective1_survival_effect_summary(
    output_dir = output_dirs$obj1_recurrence,
    prefix = prefix,
    outcome_label = "Local Recurrence-Free Probability",
    ph_diagnostics = recurrence_time_to_event$ph_diagnostics,
    rmst_results = NULL
)
logger::log_info(formatted("Local recurrence Cox time-to-event analysis completed", indent = 1))
```

Replace lines 321-334 with:

```r
# 1b. Metastatic progression: descriptive support plus Cox-led time-to-event analysis
logger::log_info(formatted("Executing metastasis event-support summary and Cox time-to-metastasis analysis", indent = 1))
mets_rates <- analyze_binary_outcome_rates(
    data,
    outcome_var = "mets_progression",
    time_var = "tt_mets_months",
    event_var = "mets_event",
    confounders = confounders,
    analysis_type = "post_treatment_only",
    dataset_name = dataset_name,
    output_dirs = output_dirs,
    prefix = prefix
)
mets_time_to_event <- analyze_time_to_event_outcomes(
    data,
    time_var = "tt_mets_months",
    event_var = "mets_event",
    group_var = "treatment_group",
    confounders = confounders,
    ylab = "Metastasis-Free Survival Probability",
    analysis_type = "post_treatment_only",
    dataset_name = dataset_name,
    output_dirs = output_dirs,
    prefix = prefix
)
mets_time_to_event$ph_diagnostics <- run_or_skip_proportional_hazards_diagnostics(
    cox_model = mets_time_to_event$cox_model,
    outcome_name = "Metastasis-Free Survival Probability",
    output_dir = output_dirs$obj1_mets,
    file_prefix = paste0(prefix, "metastasis_free_survival_probability_"),
    dataset_name = dataset_name,
    data = data,
    time_var = "tt_mets_months",
    event_var = "mets_event",
    variables = unique(c("treatment_group", confounders)),
    reason = "Metastasis proportional hazards diagnostics were not run because no Cox model was fit."
)
annotate_objective1_survival_effect_summary(
    output_dir = output_dirs$obj1_mets,
    prefix = prefix,
    outcome_label = "Metastasis-Free Survival Probability",
    ph_diagnostics = mets_time_to_event$ph_diagnostics,
    rmst_results = NULL
)
logger::log_info(formatted("Metastasis Cox time-to-event analysis completed", indent = 1))
```

- [ ] **Step 6: Update the Objective 1 result list**

At the end of `run_objective_1()`, include both new result names. The returned list should contain at least:

```r
list(
    recurrence_rates = recurrence_rates,
    recurrence_time_to_event = recurrence_time_to_event,
    recurrence_os = recurrence_os,
    recurrence_pfs = recurrence_pfs,
    mets_rates = mets_rates,
    mets_time_to_event = mets_time_to_event,
    metastasis_os = metastasis_os,
    metastasis_pfs = metastasis_pfs,
    os_analysis = os_analysis,
    pfs_analysis = pfs_analysis,
    height_changes = height_changes,
    tumor_size_summary = tumor_size_summary,
    baseline_diameter_summary = baseline_diameter_summary,
    primary_subgroup_results = primary_subgroup_results,
    sensitivity_subgroup_results = sensitivity_subgroup_results,
    primary_height_forest_plot = primary_height_forest_plot,
    sensitivity_height_forest_plot = sensitivity_height_forest_plot
)
```

- [ ] **Step 7: Update existing Objective 1 tests for renamed event-support workbook**

In `<PEER_REVIEW_REPO>/tests/testthat/test_objective1_primary_outcomes.R`, update the test named `"Objective 1 recurrence and metastasis rate summaries include co-primary cumulative incidence"`:

```r
test_that("Objective 1 recurrence and metastasis event-support summaries include cumulative incidence", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "objective1_cumulative_incidence_test")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    recurrence_summary_path <- file.path(pipeline$output_dirs$obj1_recurrence, "test_recurrence1_event_support_summary.xlsx")
    mets_summary_path <- file.path(pipeline$output_dirs$obj1_mets, "test_mets_progression_event_support_summary.xlsx")

    for (summary_path in c(recurrence_summary_path, mets_summary_path)) {
        expect_true(file.exists(summary_path))
        expect_true(all(c(
            "descriptive_event_counts",
            "cumulative_incidence",
            "competing_risk_support",
            "estimand_notes"
        ) %in% readxl::excel_sheets(summary_path)))

        descriptive_counts <- readxl::read_xlsx(summary_path, sheet = "descriptive_event_counts")
        cumulative_incidence <- readxl::read_xlsx(summary_path, sheet = "cumulative_incidence")
        estimand_notes <- readxl::read_xlsx(summary_path, sheet = "estimand_notes")

        expect_true(all(descriptive_counts$estimand == "descriptive_ever_observed"))
        expect_true(any(grepl("adjusted Cox models are the lead", descriptive_counts$notes, fixed = TRUE)))
        expect_true(any(cumulative_incidence$status == "completed"))
        expect_true(any(grepl("competing event", cumulative_incidence$notes, fixed = TRUE)))
        expect_true("gray_test_global_curve_p_value" %in% names(cumulative_incidence))
        expect_false("gray_test_p_value" %in% names(cumulative_incidence))
        expect_true(any(grepl("not a per-horizon p-value", cumulative_incidence$notes, fixed = TRUE)))
        expect_true(all(c("descriptive_ever_observed", "competing_risk_cumulative_incidence") %in% estimand_notes$estimand))
        expect_false(any(estimand_notes$role == "co-primary"))
    }

    expect_false(is.null(pipeline$results$recurrence_rates$cumulative_incidence))
    expect_false(is.null(pipeline$results$mets_rates$cumulative_incidence))
})
```

- [ ] **Step 8: Run Objective 1 tests**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_peer_review_revision_contract.R')"
Rscript -e "testthat::test_file('tests/testthat/test_objective1_primary_outcomes.R')"
```

Expected:

```text
PASS
PASS
```

- [ ] **Step 9: Commit Cox-led recurrence/metastasis revision**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add scripts/analysis/survival_outcomes.R scripts/analysis/binary_outcomes.R scripts/workflow/objective_1_primary_outcomes.R tests/testthat/test_objective1_primary_outcomes.R tests/testthat/test_peer_review_revision_contract.R
git commit -m "fix: make recurrence and metastasis Cox-led endpoints"
```

Expected:

```text
[peer-review-statistical-revision ...] fix: make recurrence and metastasis Cox-led endpoints
```

---

### Task 5: Add 5-Year-Capped OS/PFS Cox Sensitivity Without Truncating KM Plots

**Files:**
- Modify: `<PEER_REVIEW_REPO>/scripts/analysis/survival_outcomes.R`
- Modify: `<PEER_REVIEW_REPO>/scripts/workflow/objective_1_primary_outcomes.R`
- Test: `<PEER_REVIEW_REPO>/tests/testthat/test_peer_review_revision_contract.R`

- [ ] **Step 1: Add a helper for Cox-only administrative horizon truncation**

Add this function in `<PEER_REVIEW_REPO>/scripts/analysis/survival_outcomes.R` before `analyze_time_to_event_outcomes()`:

```r
#' Fit a Cox model with administrative censoring at a fixed horizon
#'
#' @param data Data frame containing time, event, treatment, and covariate columns.
#' @param time_var Character scalar follow-up time column in months.
#' @param event_var Character scalar event indicator column.
#' @param horizon_months Numeric administrative censoring horizon in months.
#' @param group_var Character scalar treatment/group variable.
#' @param confounders Character vector of covariates.
#' @param output_dir Directory for effect-summary output.
#' @param prefix File prefix.
#' @param analysis_label Character label used in effect-summary workbook.
#' @param dataset_name Character dataset/cohort label.
#' @return List with `data`, `model`, `effect_summary`, `diagnostics`, and
#'   `ph_diagnostics`.
fit_capped_cox_sensitivity <- function(data,
                                       time_var,
                                       event_var,
                                       horizon_months = 60,
                                       group_var = "treatment_group",
                                       confounders = NULL,
                                       output_dir,
                                       prefix,
                                       analysis_label,
                                       dataset_name = NULL) {
    required_cols <- unique(c(time_var, event_var, group_var, confounders))
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop(sprintf(
            "Capped Cox sensitivity cannot run; missing columns: %s",
            paste(missing_cols, collapse = ", ")
        ), call. = FALSE)
    }

    capped_data <- normalize_treatment_group_data(data) %>%
        enforce_unordered_factors() %>%
        dplyr::filter(!is.na(.data[[time_var]]), .data[[time_var]] >= 0) %>%
        dplyr::mutate(
            capped_time_months = pmin(.data[[time_var]], horizon_months),
            capped_event = dplyr::if_else(.data[[time_var]] <= horizon_months & .data[[event_var]] == 1, 1, 0)
        )

    variables <- unique(c(group_var, confounders))
    exclusion_result <- apply_sparse_level_exclusions(
        data = capped_data,
        variables = variables[variables %in% names(capped_data)],
        analysis_name = paste0(make_filename_safe(analysis_label), "_5yr_capped_cox"),
        id_col = pick_sparse_level_id_col(capped_data),
        level_exclusions = MODELING_LEVEL_EXCLUSIONS
    )
    model_data <- exclusion_result$data

    if (nrow(model_data) == 0 || length(unique(stats::na.omit(model_data[[group_var]]))) < 2) {
        diagnostics <- build_survival_skip_diagnostics(
            data = model_data,
            event_var = "capped_event",
            variables = variables,
            analysis_name = paste0(make_filename_safe(analysis_label), "_5yr_capped_cox"),
            dataset_name = dataset_name %||% "unspecified_dataset",
            reason = "The 5-year capped Cox sensitivity was skipped because the post-exclusion dataset did not retain enough usable rows or group variation.",
            narrative_lines = c(
                sprintf("Administrative censoring horizon: %.0f months.", horizon_months),
                sprintf("Rows retained after sparse-level exclusions: %d.", nrow(model_data))
            ),
            filter_stats = exclusion_result$filter_stats,
            sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
            modeled_n = nrow(model_data),
            status = "skipped",
            time_var = "capped_time_months"
        )
        save_skipped_model_outputs(
            analysis_name = paste0(make_filename_safe(analysis_label), "_5yr_capped_cox"),
            dataset_name = dataset_name %||% "unspecified_dataset",
            output_dir = output_dir,
            prefix = prefix %||% "",
            reason = diagnostics$reason,
            diagnostics = diagnostics
        )
        return(list(data = model_data, model = NULL, effect_summary = empty_effect_summary_rows(), diagnostics = diagnostics))
    }

    result <- generate_regression_table(
        data = model_data,
        outcome_var = "capped_event",
        predictor_vars = group_var,
        confounders = confounders,
        model_type = "cox",
        effect_measure = "HR",
        analysis_name = paste0(make_filename_safe(analysis_label), "_5yr_capped"),
        dataset_name = dataset_name %||% "unspecified_dataset",
        output_dir = output_dir,
        prefix = prefix,
        time_var = "capped_time_months",
        event_var = "capped_event",
        treatment_var = group_var,
        sparse_level_diagnostics = exclusion_result$sparse_level_diagnostics,
        filter_stats = exclusion_result$filter_stats
    )

    effect_summary <- summarize_cox_hr(
        model = result$model,
        dataset_name = dataset_name,
        analysis_label = analysis_label,
        model_label = "Adjusted Cox (5-year capped)",
        group_var = group_var,
        data_source_label = "Cox dataset administratively censored at 60 months"
    )
    if (is.null(effect_summary)) {
        effect_summary <- empty_effect_summary_rows()
    }
    if (nrow(effect_summary) > 0) {
        write_readable_xlsx(
            effect_summary,
            file.path(output_dir, paste0(prefix, make_filename_safe(analysis_label), "_5yr_capped_effect_summary.xlsx"))
        )
    }

    ph_diagnostics <- run_or_skip_proportional_hazards_diagnostics(
        cox_model = result$model,
        outcome_name = paste0(analysis_label, " (5-year capped)"),
        output_dir = output_dir,
        file_prefix = paste0(prefix, make_filename_safe(analysis_label), "_5yr_capped_"),
        dataset_name = dataset_name,
        data = model_data,
        time_var = "capped_time_months",
        event_var = "capped_event",
        variables = variables,
        reason = "The 5-year capped Cox sensitivity did not fit a Cox model."
    )

    list(
        data = model_data,
        model = result$model,
        effect_summary = effect_summary,
        diagnostics = result$diagnostics,
        ph_diagnostics = ph_diagnostics
    )
}
```

- [ ] **Step 2: Call the helper after OS and PFS full KM/Cox analyses**

In `<PEER_REVIEW_REPO>/scripts/workflow/objective_1_primary_outcomes.R`, after the OS PH annotation block, add:

```r
os_5yr_capped <- fit_capped_cox_sensitivity(
    data = data,
    time_var = "tt_death_months",
    event_var = "death_event",
    horizon_months = 60,
    group_var = "treatment_group",
    confounders = confounders,
    output_dir = output_dirs$obj1_os,
    prefix = prefix,
    analysis_label = "Overall Survival Probability",
    dataset_name = dataset_name
)
```

After the PFS PH annotation block, add:

```r
pfs_5yr_capped <- fit_capped_cox_sensitivity(
    data = data,
    time_var = "tt_pfs_months",
    event_var = "pfs_event",
    horizon_months = 60,
    group_var = "treatment_group",
    confounders = confounders,
    output_dir = output_dirs$obj1_pfs,
    prefix = prefix,
    analysis_label = "Progression-Free Survival Probability",
    dataset_name = dataset_name
)
```

Add `os_5yr_capped = os_5yr_capped` and `pfs_5yr_capped = pfs_5yr_capped` to the Objective 1 return list.

Do not describe the full-follow-up PFS HR as unqualified Cox lead inference: the submitted restricted-cohort PFS model already had a global PH warning (`p = 0.0241`). For each full and capped model, inspect the treatment-term and global Schoenfeld tests. If the treatment term has a PH warning, do not report a single treatment HR as a stable summary; report the issue and retain the KM curve as descriptive context. A capped analysis is a sensitivity analysis, not an automatic repair for a PH violation.

- [ ] **Step 3: Run the five-year capped tests**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_peer_review_revision_contract.R', filter='five-year capped')"
```

Expected:

```text
PASS
```

The test must assert that each fitted capped model writes either a PH diagnostic workbook/summary or an explicit skip explanation.

- [ ] **Step 4: Commit five-year capped sensitivity**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add scripts/analysis/survival_outcomes.R scripts/workflow/objective_1_primary_outcomes.R tests/testthat/test_peer_review_revision_contract.R
git commit -m "feat: add five-year capped Cox sensitivity analyses"
```

Expected:

```text
[peer-review-statistical-revision ...] feat: add five-year capped Cox sensitivity analyses
```

---

## Goal Group 3: Audit And Propensity Tools

**Tasks:** 6, 7.

**Recommended Codex goal text for Nick:**

```text
Execute Goal Group 3 from docs/superpowers/plans/2026-06-26-peer-review-statistical-revision.md in the peer-review worktree: implement and test the peer-review follow-up/data-availability audit tool and the propensity-score feasibility tool, keeping both as on-demand scripts outside the main workflow. Stop after Task 7 and report changed files, exact commands run, tests passed/failed, runtime artifacts generated, unresolved decisions/blockers, remaining task IDs, and the recommended next Codex goal text.
```

**Required end-of-goal checkpoint:** changed files, exact commands run, tests passed/failed, runtime artifacts generated, unresolved decisions/blockers, remaining task IDs, and recommended next goal text.

### Task 6: Add Follow-Up, Treatment-Detail, Dose/Proximity, And Restricted-Cohort Feasibility Audit

**Files:**
- Create: `<PEER_REVIEW_REPO>/scripts/tools/peer_review_followup_audit.R`
- Create: `<PEER_REVIEW_REPO>/tests/testthat/test_peer_review_data_availability.R`

- [ ] **Step 1: Create the on-demand audit tool**

Create `<PEER_REVIEW_REPO>/scripts/tools/peer_review_followup_audit.R` with reusable functions and a direct-run entrypoint. This tool is not sourced by `scripts/load_all.R` and is not called by Objective 1 automatically. Its purpose is to generate one reviewer-response evidence workbook that Nick can inspect before deciding what belongs in the manuscript response.

```r
# Peer-review revision data availability and follow-up audits

#' Summarize a numeric variable by treatment group
#'
#' @param data Data frame.
#' @param value_var Character scalar numeric variable.
#' @param group_var Character scalar grouping variable.
#' @return Tibble with n, nonmissing, mean, median, min, and max.
summarize_numeric_by_group_for_review <- function(data, value_var, group_var = "treatment_group") {
    if (!all(c(value_var, group_var) %in% names(data))) {
        return(tibble::tibble())
    }

    data %>%
        dplyr::mutate(.value = suppressWarnings(as.numeric(.data[[value_var]]))) %>%
        dplyr::group_by(.data[[group_var]]) %>%
        dplyr::summarise(
            variable = value_var,
            n_rows = dplyr::n(),
            n_nonmissing = sum(!is.na(.data$.value)),
            mean = mean(.data$.value, na.rm = TRUE),
            median = stats::median(.data$.value, na.rm = TRUE),
            min = min(.data$.value, na.rm = TRUE),
            max = max(.data$.value, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        dplyr::rename(treatment_group = 1)
}

#' Summarize non-missing availability for selected columns
#'
#' @param data Data frame.
#' @param variables Character vector.
#' @param group_var Character scalar grouping variable.
#' @return Tibble with availability counts by treatment group.
summarize_availability_by_group_for_review <- function(data, variables, group_var = "treatment_group") {
    present_vars <- intersect(variables, names(data))
    if (length(present_vars) == 0 || !group_var %in% names(data)) {
        return(tibble::tibble())
    }

    purrr::map_dfr(present_vars, function(var) {
        data %>%
            dplyr::group_by(.data[[group_var]]) %>%
            dplyr::summarise(
                variable = var,
                n_rows = dplyr::n(),
                n_nonmissing = sum(!is.na(.data[[var]])),
                pct_nonmissing = round(100 * n_nonmissing / n_rows, 1),
                .groups = "drop"
            ) %>%
            dplyr::rename(treatment_group = 1)
    })
}

#' Normalize column names for reviewer source-field matching
#'
#' @param x Character vector.
#' @return Normalized character vector.
normalize_review_field_name <- function(x) {
    x %>%
        tolower() %>%
        gsub("[^a-z0-9]+", "_", .) %>%
        gsub("^_|_$", "", .)
}

#' Match requested source fields using normalized names and synonyms
#'
#' @param requested Character vector of requested field names.
#' @param checked_source_columns Character vector of source columns.
#' @return Tibble with source-field matching evidence.
match_requested_source_fields <- function(requested, checked_source_columns) {
    if (is.data.frame(checked_source_columns) && "variable" %in% names(checked_source_columns)) {
        checked_source_columns <- checked_source_columns$variable
    }
    checked_source_columns <- unique(as.character(checked_source_columns))
    normalized_sources <- normalize_review_field_name(checked_source_columns)
    synonym_patterns <- list(
        prescription_dose = "prescription.*dose|rx.*dose|dose.*gy",
        gk_prescription_dose = "gk.*dose|gamma.*knife.*dose|srs.*dose",
        gk_isodose_line = "isodose|iso.*dose",
        gk_number_of_shots = "shot|isocenter|isocentre",
        tumor_to_disc_distance = "disc.*distance|optic.*disc.*distance|distance.*disc",
        tumor_to_fovea_distance = "fovea.*distance|macula.*distance|distance.*fovea|distance.*macula",
        optic_disc_dose = "optic.*disc.*dose|disc.*dose",
        macular_dose = "macula.*dose|fovea.*dose",
        lens_dose = "lens.*dose",
        longitudinal_visual_field_loss = "visual.*field.*follow|visual.*field.*loss|vf.*follow"
    )
    purrr::map_dfr(requested, function(variable) {
        normalized_variable <- normalize_review_field_name(variable)
        pattern <- synonym_patterns[[variable]]
        exact_hits <- checked_source_columns[normalized_sources == normalized_variable]
        synonym_hits <- if (!is.null(pattern)) {
            checked_source_columns[grepl(pattern, normalized_sources)]
        } else {
            character()
        }
        hits <- unique(c(exact_hits, synonym_hits))
        tibble::tibble(
            variable = variable,
            present_in_checked_source_columns = length(hits) > 0,
            source_column_evidence = paste(hits, collapse = "; ")
        )
    })
}

#' Normalize optic-nerve involvement/abutment values
#'
#' @param x Vector of optic-nerve values.
#' @return Logical vector, TRUE when coded as involved/abutting/positive.
normalize_optic_nerve_involvement <- function(x) {
    normalized <- normalize_review_field_name(as.character(x))
    dplyr::case_when(
        normalized %in% c("y", "yes", "true", "1", "positive", "involved", "abutment", "abutting", "touching") ~ TRUE,
        normalized %in% c("n", "no", "false", "0", "negative", "not_involved", "not_abutting", "none") ~ FALSE,
        TRUE ~ NA
    )
}

#' Verify restricted cohort eligibility criteria used in reviewer response
#'
#' @param data Data frame.
#' @return Tibble with cutoff and optic-nerve support.
verify_restricted_cohort_eligibility <- function(data) {
    required_cols <- c("initial_tumor_diameter", "initial_tumor_height", "optic_nerve")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        return(tibble::tibble(
            check = "restricted_eligibility",
            status = "unavailable",
            detail = paste("Missing columns:", paste(missing_cols, collapse = ", "))
        ))
    }

    too_large <- data %>%
        dplyr::filter(.data$initial_tumor_diameter > 20 | .data$initial_tumor_height > 10)
    optic_nerve_positive <- data %>%
        dplyr::mutate(.optic_nerve_involved = normalize_optic_nerve_involvement(.data$optic_nerve)) %>%
        dplyr::filter(.data$.optic_nerve_involved %in% TRUE)

    tibble::tibble(
        check = c("restricted_size_cutoffs", "restricted_optic_nerve_status"),
        status = c(
            ifelse(nrow(too_large) == 0, "passed", "failed"),
            ifelse(nrow(optic_nerve_positive) == 0, "passed", "failed")
        ),
        detail = c(
            sprintf(
                "%d of %d rows exceeded diameter >20 mm or height >10 mm.",
                nrow(too_large),
                nrow(data)
            ),
            sprintf(
                "%d of %d rows had optic_nerve coded as positive/abutment/involvement.",
                nrow(optic_nerve_positive),
                nrow(data)
            )
        )
    )
}

#' Build peer-review data availability audit workbook tables
#'
#' @param data Data frame.
#' @param dataset_name Character dataset/cohort label.
#' @param checked_source_columns Character vector of raw/source columns checked.
#' @return Named list of tables.
build_peer_review_data_availability_audit <- function(data, dataset_name, checked_source_columns = character()) {
    treatment_detail_vars <- c(
        "radionuclide", "plaque_size", "plaque_notch",
        "initial_plaque", "initial_plaque_date"
    )
    requested_absent_vars <- c(
        "prescription_dose", "prescription_depth", "implant_duration",
        "gk_prescription_dose", "gk_isodose_line", "gk_maximum_dose",
        "gk_number_of_shots", "gk_isocenters",
        "tumor_to_disc_distance", "tumor_to_fovea_distance",
        "optic_disc_dose", "macular_dose", "lens_dose",
        "longitudinal_visual_field_loss"
    )
    followup_vars <- c("follow_up_months")
    timing_vars <- c("last_followup", "last_height_date", "treatment_date")
    visual_vars <- c("initial_vision", "last_vision", "vision_change", "visual_field_defect")
    geometry_vars <- c("initial_tumor_height", "initial_tumor_diameter", "initial_t_stage_simple", "optic_nerve", "srf")

    present_cols <- names(data)
    source_matches <- match_requested_source_fields(requested_absent_vars, checked_source_columns)
    absent_requested <- tibble::tibble(
        variable = requested_absent_vars,
        present_in_derived_runtime_dataset = normalize_review_field_name(requested_absent_vars) %in% normalize_review_field_name(present_cols)
    ) %>%
        dplyr::left_join(source_matches, by = "variable") %>%
        dplyr::mutate(
        absence_claim_supported = !present_in_derived_runtime_dataset & !present_in_checked_source_columns,
        reviewer_relevance = dplyr::case_when(
            grepl("dose|isodose|shot|isocenter", .data$variable) ~ "radiation treatment detail/dosimetry",
            grepl("disc|fovea|macular|lens", .data$variable) ~ "proximity or organ-at-risk dose",
            grepl("visual_field", .data$variable) ~ "longitudinal visual-field outcome",
            TRUE ~ "requested reviewer covariate"
        )
        )

    list(
        dataset = tibble::tibble(dataset_name = dataset_name, n_rows = nrow(data)),
        followup_months = summarize_numeric_by_group_for_review(data, "follow_up_months"),
        treatment_detail_availability = summarize_availability_by_group_for_review(data, treatment_detail_vars),
        timing_availability = summarize_availability_by_group_for_review(data, timing_vars),
        visual_availability = summarize_availability_by_group_for_review(data, visual_vars),
        geometry_availability = summarize_availability_by_group_for_review(data, geometry_vars),
        requested_absent_fields = absent_requested,
        restricted_eligibility_check = verify_restricted_cohort_eligibility(data)
    )
}

#' Read source spreadsheet column names for reviewer-response absence checks
#'
#' @param source_files Character vector of source workbook paths.
#' @return Tibble with file, sheet, and column names.
read_checked_source_columns_for_review <- function(source_files) {
    purrr::map_dfr(source_files[file.exists(source_files)], function(path) {
        purrr::map_dfr(readxl::excel_sheets(path), function(sheet) {
            cols <- names(readxl::read_excel(path, sheet = sheet, n_max = 0))
            tibble::tibble(
                source_file = basename(path),
                sheet = sheet,
                variable = cols
            )
        })
    })
}

#' Write peer-review data availability audit workbook
#'
#' @param data Data frame.
#' @param dataset_name Character dataset/cohort label.
#' @param output_dir Directory to write workbook.
#' @param prefix File prefix.
#' @param checked_source_columns Character vector of raw/source columns checked.
#' @param source_column_audit Optional table of checked source files/sheets/columns.
#' @return Path to written workbook.
write_peer_review_data_availability_audit <- function(data,
                                                      dataset_name,
                                                      output_dir,
                                                      prefix,
                                                      checked_source_columns = character(),
                                                      source_column_audit = tibble::tibble()) {
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    audit <- build_peer_review_data_availability_audit(
        data = data,
        dataset_name = dataset_name,
        checked_source_columns = checked_source_columns
    )
    audit$source_column_audit <- source_column_audit
    output_path <- file.path(output_dir, paste0(prefix, "followup_and_data_availability.xlsx"))
    write_readable_xlsx(audit, output_path)
    output_path
}

if (sys.nframe() == 0) {
    source(here::here("scripts", "load_all.R"))
    source_files <- c(
        file.path(RAW_DATA_DIR, "Ocular Melanoma Master Spreadsheet REVISED FOR STATS (5-10-25, TJM).xlsx"),
        file.path(RAW_DATA_DIR, "logMar_initial_recent.xlsx"),
        file.path(RAW_DATA_DIR, "tocheck_logMar_initial_recent.xlsx"),
        file.path(RAW_DATA_DIR, "Updated_Data_Dictionary.xlsx")
    )
    source_column_audit <- read_checked_source_columns_for_review(source_files)
    checked_source_columns <- unique(source_column_audit$variable)
    cohorts <- list(
        full = file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"),
        restricted = file.path(PROCESSED_DATA_DIR, "uveal_melanoma_restricted_cohort.rds")
    )
    output_dir <- file.path(OUTPUT_DIR, "peer_review_revision_audits")
    for (cohort_name in names(cohorts)) {
        write_peer_review_data_availability_audit(
            data = readRDS(cohorts[[cohort_name]]),
            dataset_name = cohort_name,
            output_dir = output_dir,
            prefix = paste0(cohort_name, "_"),
            checked_source_columns = checked_source_columns,
            source_column_audit = source_column_audit
        )
    }
}
```

- [ ] **Step 2: Do not source or call the audit tool from the main workflow**

Do not edit `scripts/load_all.R`. Do not add `obj1_peer_review_followup_audits` to Objective 1 output-directory construction. Do not add a `peer_review_followup_audit` return value to `run_objective_1()`. This audit is a decision-support tool, not part of the routine Objective 1 analysis contract.

- [ ] **Step 3: Write audit tests**

Create `<PEER_REVIEW_REPO>/tests/testthat/test_peer_review_data_availability.R` with:

```r
test_that("peer-review data availability audit reports present and absent reviewer-requested fields", {
    data <- create_test_dataset()
    data$radionuclide <- c("Pd-103", "I-125", rep(NA_character_, nrow(data) - 2))
    data$plaque_size <- c(16, 18, rep(NA_real_, nrow(data) - 2))
    data$plaque_notch <- c("N", "Y", rep(NA_character_, nrow(data) - 2))
    data$follow_up_months <- seq_len(nrow(data))
    data$initial_tumor_diameter <- pmin(data$initial_tumor_diameter, 20)
    data$initial_tumor_height <- pmin(data$initial_tumor_height, 10)

    source(here::here("scripts", "tools", "peer_review_followup_audit.R"))
    audit <- build_peer_review_data_availability_audit(
        data,
        "test_cohort",
        checked_source_columns = c("radionuclide", "plaque_size", "plaque_notch")
    )

    expect_true(all(c(
        "dataset",
        "followup_months",
        "treatment_detail_availability",
        "requested_absent_fields",
        "restricted_eligibility_check"
    ) %in% names(audit)))
    expect_true(any(audit$treatment_detail_availability$variable == "radionuclide"))
    expect_true(any(audit$requested_absent_fields$variable == "gk_prescription_dose"))
    expect_false(audit$requested_absent_fields$present_in_derived_runtime_dataset[audit$requested_absent_fields$variable == "gk_prescription_dose"])
    expect_false(audit$requested_absent_fields$present_in_checked_source_columns[audit$requested_absent_fields$variable == "gk_prescription_dose"])
    expect_true(audit$requested_absent_fields$absence_claim_supported[audit$requested_absent_fields$variable == "gk_prescription_dose"])
    expect_true(all(audit$restricted_eligibility_check$status == "passed"))
    expect_true("restricted_optic_nerve_status" %in% audit$restricted_eligibility_check$check)
})
```

- [ ] **Step 4: Run audit tests**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_peer_review_data_availability.R')"
```

Expected:

```text
PASS
```

- [ ] **Step 5: Run the audit tool explicitly and review the workbook**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript scripts/tools/peer_review_followup_audit.R
```

Expected:

```text
/Users/ncamarda/ProjectsRuntime/uveal_melanoma/Analysis/peer_review_revision_audits/full_followup_and_data_availability.xlsx
/Users/ncamarda/ProjectsRuntime/uveal_melanoma/Analysis/peer_review_revision_audits/restricted_followup_and_data_availability.xlsx
```

Use these workbooks to populate the single response document. Do not add these audit results to the main Objective 1 workflow unless a specific reusable table becomes part of the revised manuscript analysis contract.

- [ ] **Step 6: Commit the audit tool**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add scripts/tools/peer_review_followup_audit.R tests/testthat/test_peer_review_data_availability.R
git commit -m "feat: add peer-review follow-up and data availability audit tool"
```

Expected:

```text
[peer-review-statistical-revision ...] feat: add peer-review follow-up and data availability audit tool
```

---

### Task 7: Run Propensity Score Feasibility Before Deciding Whether To Add A Sensitivity Model

**Files:**
- Create: `<PEER_REVIEW_REPO>/scripts/tools/propensity_score_feasibility.R`
- Create: `<PEER_REVIEW_REPO>/tests/testthat/test_propensity_score_feasibility.R`

- [ ] **Step 1: Create the on-demand propensity feasibility tool**

Create `<PEER_REVIEW_REPO>/scripts/tools/propensity_score_feasibility.R` with reusable functions and a direct-run entrypoint. This tool is not sourced by `scripts/load_all.R` and is not called by Objective 1 automatically. Its purpose is to answer Tim's propensity-score question by documenting feasibility, overlap, covariate support, and limitations before deciding whether any propensity-adjusted Cox sensitivity should enter the manuscript workflow.

The tool may fit diagnostic GLMs while screening covariates, but no propensity model may be treated as reportable until the workbook documents all-missing, single-level/zero-variance, aliased/collinear, complete-case, overlap, and separation-prone checks. Separation-prone fits are allowed only as feasibility diagnostics and must set `reportable_sensitivity = FALSE`.

```r
# Propensity score feasibility tool for peer-review revision

#' Select baseline variables for treatment propensity score
#'
#' @param data Data frame.
#' @return Character vector of available baseline covariates.
select_propensity_covariates <- function(data) {
    candidates <- c(
        "age_at_diagnosis",
        "sex",
        "location",
        "initial_tumor_height",
        "initial_tumor_diameter",
        "initial_t_stage_simple",
        "initial_overall_stage",
        "srf",
        "visual_field_defect",
        "vision_loss_blurred_vision",
        "initial_vision"
    )
    candidates[candidates %in% names(data)]
}

#' Screen propensity covariates before fitting
#'
#' @param data Data frame after treatment and covariate selection.
#' @param covariates Candidate covariates.
#' @return List with selected covariates and exclusion diagnostics.
screen_propensity_covariates <- function(data, covariates) {
    diagnostics <- purrr::map_dfr(covariates, function(covariate) {
        values <- data[[covariate]]
        nonmissing <- sum(!is.na(values))
        unique_nonmissing <- length(unique(values[!is.na(values)]))
        tibble::tibble(
            covariate = covariate,
            nonmissing = nonmissing,
            unique_nonmissing = unique_nonmissing,
            exclude_reason = dplyr::case_when(
                nonmissing == 0 ~ "all_missing",
                unique_nonmissing < 2 ~ "single_level_or_zero_variance",
                TRUE ~ NA_character_
            )
        )
    })

    selected <- diagnostics %>%
        dplyr::filter(is.na(.data$exclude_reason)) %>%
        dplyr::pull(.data$covariate)

    list(selected = selected, diagnostics = diagnostics)
}

#' Remove aliased propensity covariates after model-matrix construction
#'
#' @param ps_data Complete-case propensity-score data.
#' @param covariates Screened covariates.
#' @return List with selected covariates and alias diagnostics.
drop_aliased_propensity_covariates <- function(ps_data, covariates) {
    selected <- covariates
    alias_rows <- tibble::tibble(covariate = character(), exclude_reason = character())
    repeat {
        if (length(selected) == 0) {
            break
        }
        formula <- stats::as.formula(paste(".treated_gksrs ~", paste(selected, collapse = " + ")))
        fit <- stats::glm(formula, data = ps_data, family = stats::binomial())
        aliased_terms <- names(stats::coef(fit))[is.na(stats::coef(fit))]
        aliased_terms <- setdiff(aliased_terms, "(Intercept)")
        if (length(aliased_terms) == 0) {
            break
        }
        drop_covariate <- selected[vapply(selected, function(covariate) {
            any(startsWith(aliased_terms, covariate))
        }, logical(1))][1]
        alias_rows <- dplyr::bind_rows(
            alias_rows,
            tibble::tibble(covariate = drop_covariate, exclude_reason = "aliased_or_collinear")
        )
        selected <- setdiff(selected, drop_covariate)
    }
    list(selected = selected, diagnostics = alias_rows)
}

#' Fit treatment propensity score model and diagnose overlap
#'
#' @param data Data frame.
#' @param treatment_var Character scalar treatment variable.
#' @return List with fitted model, data, diagnostics, and selected covariates.
fit_treatment_propensity_score <- function(data, treatment_var = "treatment_group") {
    covariates <- select_propensity_covariates(data)
    if (!treatment_var %in% names(data)) {
        stop(sprintf("Treatment variable `%s` is missing.", treatment_var), call. = FALSE)
    }
    if (length(covariates) == 0) {
        stop("No baseline propensity covariates were available.", call. = FALSE)
    }

    selected_data <- normalize_treatment_group_data(data) %>%
        enforce_unordered_factors() %>%
        dplyr::select(dplyr::all_of(c(treatment_var, covariates)))

    covariate_screen <- screen_propensity_covariates(selected_data, covariates)
    covariates <- covariate_screen$selected
    if (length(covariates) == 0) {
        return(list(
            model = NULL,
            data = tibble::tibble(),
            overlap = tibble::tibble(),
            diagnostics = tibble::tibble(
                status = "not_fit",
                interpretation = "Propensity model not fit because all candidate covariates were all-missing, single-level, or zero-variance."
            ),
            covariates = character(),
            covariate_diagnostics = covariate_screen$diagnostics
        ))
    }

    ps_data <- selected_data %>%
        dplyr::select(dplyr::all_of(c(treatment_var, covariates))) %>%
        tidyr::drop_na()
    if (nrow(ps_data) < 10 || dplyr::n_distinct(ps_data[[treatment_var]]) < 2) {
        return(list(
            model = NULL,
            data = ps_data,
            overlap = tibble::tibble(),
            diagnostics = tibble::tibble(
                status = "not_fit",
                n_complete_rows = nrow(ps_data),
                n_treatment_groups = dplyr::n_distinct(ps_data[[treatment_var]]),
                interpretation = "Propensity model not fit because complete-case data did not retain enough rows or both treatment groups."
            ),
            covariates = covariates,
            covariate_diagnostics = covariate_screen$diagnostics %>%
                dplyr::mutate(selected = is.na(.data$exclude_reason))
        ))
    }

    ps_data$.treated_gksrs <- as.integer(ps_data[[treatment_var]] == "GKSRS")
    alias_screen <- drop_aliased_propensity_covariates(ps_data, covariates)
    covariates <- alias_screen$selected
    covariate_diagnostics <- covariate_screen$diagnostics %>%
        dplyr::left_join(alias_screen$diagnostics, by = "covariate", suffix = c("", "_alias")) %>%
        dplyr::mutate(
            exclude_reason = dplyr::coalesce(.data$exclude_reason, .data$exclude_reason_alias),
            selected = is.na(.data$exclude_reason)
        ) %>%
        dplyr::select(
            .data$covariate,
            .data$nonmissing,
            .data$unique_nonmissing,
            .data$selected,
            .data$exclude_reason
        )
    if (length(covariates) == 0) {
        return(list(
            model = NULL,
            data = ps_data,
            overlap = tibble::tibble(),
            diagnostics = tibble::tibble(
                status = "not_fit",
                interpretation = "Propensity model not fit because screened covariates were aliased or collinear."
            ),
            covariates = character(),
            covariate_diagnostics = covariate_diagnostics
        ))
    }

    formula <- stats::as.formula(paste(".treated_gksrs ~", paste(covariates, collapse = " + ")))
    model <- suppressWarnings(stats::glm(formula, data = ps_data, family = stats::binomial()))
    ps_data$.propensity_score <- stats::predict(model, type = "response")
    separation_flag <- any(ps_data$.propensity_score < 0.02 | ps_data$.propensity_score > 0.98)

    overlap <- ps_data %>%
        dplyr::group_by(.data[[treatment_var]]) %>%
        dplyr::summarise(
            n = dplyr::n(),
            min_ps = min(.data$.propensity_score),
            q25_ps = stats::quantile(.data$.propensity_score, 0.25),
            median_ps = stats::median(.data$.propensity_score),
            q75_ps = stats::quantile(.data$.propensity_score, 0.75),
            max_ps = max(.data$.propensity_score),
            .groups = "drop"
        ) %>%
        dplyr::rename(treatment_group = 1)

    common_min <- max(overlap$min_ps)
    common_max <- min(overlap$max_ps)
    ps_data$.in_common_support <- ps_data$.propensity_score >= common_min & ps_data$.propensity_score <= common_max

    diagnostics <- tibble::tibble(
        n_model_rows = nrow(ps_data),
        n_covariates = length(covariates),
        common_support_min = common_min,
        common_support_max = common_max,
        n_in_common_support = sum(ps_data$.in_common_support),
        pct_in_common_support = round(100 * mean(ps_data$.in_common_support), 1),
        separation_prone = separation_flag,
        reportable_sensitivity = nrow(ps_data) >= 50 && sum(ps_data$.in_common_support) >= 40 && common_min < common_max && !separation_flag,
        interpretation = "Propensity score summarizes measured treatment-selection covariates; it does not address unmeasured confounding, missing dosimetry, visual-field absence, or low event counts."
    )

    list(
        model = model,
        data = ps_data,
        overlap = overlap,
        diagnostics = diagnostics,
        covariates = covariates,
        covariate_diagnostics = covariate_diagnostics
    )
}

if (sys.nframe() == 0) {
    source(here::here("scripts", "load_all.R"))
    cohorts <- list(
        full = file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"),
        restricted = file.path(PROCESSED_DATA_DIR, "uveal_melanoma_restricted_cohort.rds")
    )
    output_dir <- file.path(OUTPUT_DIR, "peer_review_revision_audits")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    for (cohort_name in names(cohorts)) {
        ps_fit <- tryCatch(
            fit_treatment_propensity_score(readRDS(cohorts[[cohort_name]])),
            error = function(e) {
                list(
                    model = NULL,
                    data = tibble::tibble(),
                    overlap = tibble::tibble(),
                    diagnostics = tibble::tibble(
                        status = "failed",
                        interpretation = paste("Propensity score feasibility failed:", e$message)
                    ),
                    covariates = character(),
                    covariate_diagnostics = tibble::tibble()
                )
            }
        )
        write_readable_xlsx(
            list(
                diagnostics = ps_fit$diagnostics,
                overlap = ps_fit$overlap,
                selected_covariates = tibble::tibble(covariate = ps_fit$covariates),
                covariate_diagnostics = ps_fit$covariate_diagnostics
            ),
            file.path(output_dir, paste0(cohort_name, "_propensity_score_feasibility.xlsx"))
        )
    }
}
```

- [ ] **Step 2: Do not source or call the propensity tool from the main workflow**

Do not edit `scripts/load_all.R`. Do not add propensity-score fields or return values to `run_objective_1()` in this task. A propensity-adjusted Cox model can be promoted into `scripts/analysis/` and Objective 1 only after the feasibility workbook shows acceptable overlap/support and the single response document records why the sensitivity is reportable.

- [ ] **Step 3: Add tests**

Create `<PEER_REVIEW_REPO>/tests/testthat/test_propensity_score_feasibility.R` with:

```r
test_that("propensity score feasibility uses baseline covariates and records interpretation limits", {
    data <- create_test_dataset()
    data$initial_tumor_height <- seq(2, 2 + nrow(data) - 1)
    data$initial_tumor_diameter <- seq(8, 8 + nrow(data) - 1)
    data$initial_vision <- seq(20, 20 + nrow(data) - 1)
    data$srf <- rep(c("Y", "N"), length.out = nrow(data))
    data$visual_field_defect <- rep(c("Y", "N"), length.out = nrow(data))
    data$vision_loss_blurred_vision <- rep(c("Y", "N"), length.out = nrow(data))
    data$all_missing_covariate <- NA_real_
    data$single_level_covariate <- "constant"

    source(here::here("scripts", "tools", "propensity_score_feasibility.R"))
    ps_fit <- fit_treatment_propensity_score(data)

    expect_s3_class(ps_fit$model, "glm")
    expect_true("age_at_diagnosis" %in% ps_fit$covariates)
    expect_true("initial_tumor_height" %in% ps_fit$covariates)
    expect_true(".propensity_score" %in% names(ps_fit$data))
    expect_true(all(ps_fit$data$.propensity_score > 0 & ps_fit$data$.propensity_score < 1))
    expect_true("covariate_diagnostics" %in% names(ps_fit))
    expect_true(all(c("covariate", "selected", "exclude_reason") %in% names(ps_fit$covariate_diagnostics)))
    expect_true(any(grepl("does not address unmeasured confounding", ps_fit$diagnostics$interpretation, fixed = TRUE)))
})

test_that("propensity score screening records excluded covariates instead of failing whole audit", {
    data <- create_test_dataset()
    data$age_at_diagnosis <- seq_len(nrow(data))
    data$all_missing_candidate <- NA_real_
    data$single_level_candidate <- "same"

    source(here::here("scripts", "tools", "propensity_score_feasibility.R"))
    screen <- screen_propensity_covariates(data, c("age_at_diagnosis", "all_missing_candidate", "single_level_candidate"))

    expect_equal(screen$selected, "age_at_diagnosis")
    expect_true(any(screen$diagnostics$exclude_reason == "all_missing", na.rm = TRUE))
    expect_true(any(screen$diagnostics$exclude_reason == "single_level_or_zero_variance", na.rm = TRUE))
})

test_that("propensity score feasibility records aliased covariates", {
    source(here::here("scripts", "tools", "propensity_score_feasibility.R"))
    ps_data <- tibble::tibble(
        treatment_group = rep(c("PBT", "GKSRS"), each = 20),
        .treated_gksrs = rep(c(0L, 1L), each = 20),
        initial_tumor_height = seq_len(40),
        initial_tumor_diameter = seq_len(40)
    )
    alias_screen <- drop_aliased_propensity_covariates(
        ps_data,
        c("initial_tumor_height", "initial_tumor_diameter")
    )

    expect_true(any(alias_screen$diagnostics$exclude_reason == "aliased_or_collinear", na.rm = TRUE))
})

test_that("propensity score feasibility blocks separation-prone models from reportable sensitivity", {
    data <- tibble::tibble(
        treatment_group = rep(c("PBT", "GKSRS"), each = 30),
        initial_vision = rep(c(0, 1), each = 30)
    )

    source(here::here("scripts", "tools", "propensity_score_feasibility.R"))
    ps_fit <- fit_treatment_propensity_score(data)

    expect_true("separation_prone" %in% names(ps_fit$diagnostics))
    expect_true(isTRUE(ps_fit$diagnostics$separation_prone[[1]]))
    expect_false(isTRUE(ps_fit$diagnostics$reportable_sensitivity[[1]]))
})
```

- [ ] **Step 4: Run tests**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_propensity_score_feasibility.R')"
```

Expected:

```text
PASS
```

- [ ] **Step 5: Run the propensity feasibility tool explicitly and review the workbook**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript scripts/tools/propensity_score_feasibility.R
```

Expected:

```text
/Users/ncamarda/ProjectsRuntime/uveal_melanoma/Analysis/peer_review_revision_audits/full_propensity_score_feasibility.xlsx
/Users/ncamarda/ProjectsRuntime/uveal_melanoma/Analysis/peer_review_revision_audits/restricted_propensity_score_feasibility.xlsx
```

Use these workbooks to decide one of two actions in `docs/peer_review_revision_response.md`:

- **Feasibility only:** report that propensity-score diagnostics were attempted but not used as a sensitivity analysis because overlap/support was inadequate or event counts remained limiting.
- **Promote to workflow:** create a later task that adds a clearly labelled propensity-adjusted Cox sensitivity model to `scripts/analysis/` and Objective 1, with tests and manuscript limitations.

- [ ] **Step 6: Commit propensity feasibility**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add scripts/tools/propensity_score_feasibility.R tests/testthat/test_propensity_score_feasibility.R
git commit -m "feat: add propensity score feasibility audit tool"
```

Expected:

```text
[peer-review-statistical-revision ...] feat: add propensity score feasibility audit tool
```

---

## Goal Group 4: Objective 2, Tumor Height, And Subgroups

**Tasks:** 8, 8A, 9, 10.

**Recommended Codex goal text for Nick:**

```text
Execute Goal Group 4 from docs/superpowers/plans/2026-06-26-peer-review-statistical-revision.md in the peer-review worktree: implement Objective 2 latest-VA minimum-follow-up sensitivity, SRD/SRG scope clarification, tumor-height timing/demotion guardrail, and PRAME/T4 subgroup pruning diagnostics. Stop after Task 10 and report changed files, exact commands run, tests passed/failed, runtime artifacts generated, unresolved decisions/blockers, remaining task IDs, and the recommended next Codex goal text.
```

**Required end-of-goal checkpoint:** changed files, exact commands run, tests passed/failed, runtime artifacts generated, unresolved decisions/blockers, remaining task IDs, and recommended next goal text.

### Task 8: Add Visual-Acuity Minimum-Follow-Up Sensitivity Using The Latest-VA Timing Field

**Files:**
- Modify: `<PEER_REVIEW_REPO>/scripts/analysis/vision_safety_analysis.R`
- Modify: `<PEER_REVIEW_REPO>/docs/CALCULATIONS.md`
- Test: `<PEER_REVIEW_REPO>/tests/testthat/test_objective2_safety_toxicity.R`

- [ ] **Step 0: Prespecify the follow-up threshold before reviewing the sensitivity result**

The reviewer requested an adequate-minimum-follow-up sensitivity but did not specify a duration. The `36`-month threshold in the examples below is a proposed clinical decision, not an established source fact. Tim/Angie/Nick must record the rationale and threshold before rerunning the model; do not select the threshold based on which result is most favorable. If 36 months is retained, describe it as a three-year latest-visual-acuity follow-up sensitivity and label whether the explicit or proxy timing surface is being used.

- [ ] **Step 1: Add latest-VA follow-up helper**

The analytic dataset has `last_vision` as the latest visual-acuity value. Derive explicit timing from `treatment_date` to `last_followup`, derive a separately labeled proxy timing surface from general `follow_up_months` when explicit timing is missing, and keep tumor-height timing out of the VA proxy.

In `<PEER_REVIEW_REPO>/scripts/analysis/vision_safety_analysis.R`, before `analyze_visual_acuity_changes()`, add:

```r
#' Add latest visual-acuity follow-up timing for reviewer-response sensitivity
#'
#' @param data Data frame.
#' @return Data frame with `last_vision_followup_months`.
add_last_vision_followup_months <- function(data) {
    if (all(c("treatment_date", "last_followup") %in% names(data))) {
        data$last_vision_followup_months <- suppressWarnings(lubridate::time_length(
            lubridate::interval(data$treatment_date, data$last_followup),
            unit = "months"
        ))
    } else if ("follow_up_months" %in% names(data)) {
        data$last_vision_followup_months <- suppressWarnings(as.numeric(data$follow_up_months))
    } else {
        data$last_vision_followup_months <- NA_real_
    }
    data
}

#' Summarize visual-acuity follow-up timing by treatment group
#'
#' @param data Data frame.
#' @param value_var Character scalar timing variable.
#' @return Tibble with treatment-group timing summary.
summarize_vision_followup_by_group <- function(data, value_var = "last_vision_followup_months") {
    if (!all(c("treatment_group", value_var) %in% names(data))) {
        return(tibble::tibble())
    }
    data %>%
        dplyr::group_by(.data$treatment_group) %>%
        dplyr::summarise(
            variable = value_var,
            n_rows = dplyr::n(),
            n_nonmissing = sum(!is.na(.data[[value_var]])),
            median_months = stats::median(.data[[value_var]], na.rm = TRUE),
            min_months = min(.data[[value_var]], na.rm = TRUE),
            max_months = max(.data[[value_var]], na.rm = TRUE),
            .groups = "drop"
        )
}

#' Build minimum-follow-up visual-acuity sensitivity summary
#'
#' @param data Data frame with visual-acuity change and latest-VA follow-up timing.
#' @param min_followup_months Numeric minimum follow-up threshold.
#' @return List with filtered data and summary table.
build_visual_acuity_min_followup_sensitivity <- function(data, min_followup_months = 36) {
    followup_data <- add_last_vision_followup_months(data)
    filtered <- followup_data %>%
        dplyr::filter(!is.na(.data$vision_change), !is.na(.data$last_vision_followup_months), .data$last_vision_followup_months >= min_followup_months)
    summary <- filtered %>%
        dplyr::group_by(.data$treatment_group) %>%
        dplyr::summarise(
            min_followup_months = min_followup_months,
            n = dplyr::n(),
            median_last_vision_followup_months = stats::median(.data$last_vision_followup_months, na.rm = TRUE),
            median_vision_change = stats::median(.data$vision_change, na.rm = TRUE),
            min_vision_change = min(.data$vision_change, na.rm = TRUE),
            max_vision_change = max(.data$vision_change, na.rm = TRUE),
            .groups = "drop"
        )
    list(data = filtered, summary = summary)
}
```

- [ ] **Step 2: Use latest-VA follow-up helper inside `analyze_visual_acuity_changes()`**

At the beginning of `analyze_visual_acuity_changes()`, immediately after `data <- normalize_treatment_group_data(data)`, add:

```r
data <- add_last_vision_followup_months(data)
```

After writing `vision_effect_summary`, rerun the visual-acuity treatment-effect model inside the minimum-follow-up subset. If model support is inadequate, write an explicit skipped-model diagnostic and label the sensitivity as descriptive only.

```r
visual_followup_sensitivity <- build_visual_acuity_min_followup_sensitivity(data, min_followup_months = 36)
visual_followup_model <- if (
    nrow(visual_followup_sensitivity$data) > 0 &&
        dplyr::n_distinct(stats::na.omit(visual_followup_sensitivity$data$treatment_group)) >= 2
) {
    generate_regression_table(
        data = visual_followup_sensitivity$data,
        outcome_var = "vision_change",
        predictor_vars = "treatment_group",
        confounders = confounders,
        model_type = "linear",
        effect_measure = "MD",
        analysis_name = "vision_change_minimum_followup_36_months",
        dataset_name = dataset_name %||% "vision_followup_sensitivity",
        output_dir = output_dirs$obj2_vision,
        prefix = paste0(prefix, "minimum_followup_36_months_")
    )
} else {
    list(
        table = NULL,
        model = NULL,
        diagnostics = tibble::tibble(
            status = "skipped",
            reason = "Minimum-follow-up visual-acuity treatment-effect model skipped because the subset did not retain enough treatment-group support."
        )
    )
}
write_readable_xlsx(
    list(
        minimum_followup_36_months = visual_followup_sensitivity$summary,
        available_last_vision_followup = summarize_vision_followup_by_group(data, "last_vision_followup_months"),
        treatment_effect_model = tibble::tibble(
            model_status = ifelse(is.null(visual_followup_model$model), "skipped", "completed"),
            model = "vision_change ~ treatment_group + confounders",
            subset = "last_vision_followup_months >= 36"
        ),
        limitation = tibble::tibble(
            note = "The primary latest-VA timing surface uses treatment-to-last_followup when recorded; the proxy surface uses general follow_up_months when explicit timing is missing."
        )
    ),
    file.path(output_dirs$obj2_vision, paste0(prefix, "vision_followup_sensitivity.xlsx"))
)
```

Add `visual_followup_sensitivity = visual_followup_sensitivity` and `visual_followup_model = visual_followup_model` to the returned list.

- [ ] **Step 3: Update the visual endpoint note**

Replace `vision_change_contract_note <- paste(...)` with:

```r
vision_change_contract_note <- paste(
    "Vision endpoint is visual-acuity change score",
    "(initial vision minus final or recurrence-pre-treatment vision);",
    "baseline visual acuity and latest-VA follow-up time are reviewer-response sensitivity considerations;",
    "explicit latest-VA timing uses treatment-to-last_followup; proxy timing uses general follow_up_months when explicit timing is missing."
)
```

- [ ] **Step 4: Do not reflexively add baseline VA to the change-score model**

Adding baseline visual acuity as a covariate to a baseline-minus-follow-up change score can create regression-to-the-mean and mathematical-coupling problems. Do not implement that as an automatic "more adjusted" version of the submitted model.

First inspect whether `initial_vision`, the final VA value, tumor height, basal diameter, subretinal fluid, treatment year, and `last_vision_followup_months` are sufficiently complete. Only if that review supports it, fit a clearly labelled **secondary ANCOVA-style sensitivity model** with follow-up logMAR as the outcome and baseline logMAR as a covariate. Because key dose/proximity predictors remain unavailable in the current checked source fields, this sensitivity still cannot establish causal modality-related visual-acuity preservation. If the data check fails or model support is thin, do not fit a substitute model; report the existing VA analysis as limited and soften the manuscript claim.

- [ ] **Step 5: Run Objective 2 tests**

Before running tests, add this assertion to `<PEER_REVIEW_REPO>/tests/testthat/test_objective2_safety_toxicity.R`:

```r
test_that("visual-acuity minimum-follow-up sensitivity reruns treatment-effect model", {
    pipeline <- run_objective2_test(create_test_dataset(), output_tag = "objective2_vision_min_followup")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    sensitivity_path <- file.path(
        pipeline$output_dirs$obj2_vision,
        "test_vision_followup_sensitivity.xlsx"
    )
    expect_workbook_has_sheets(
        sensitivity_path,
        c("minimum_followup_36_months", "available_last_vision_followup", "treatment_effect_model", "limitation")
    )

    model_status <- readxl::read_xlsx(sensitivity_path, sheet = "treatment_effect_model")
    expect_true(all(c("model_status", "model", "subset") %in% names(model_status)))
    expect_true(model_status$model_status[[1]] %in% c("completed", "skipped"))
})
```

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_objective2_safety_toxicity.R')"
```

Expected:

```text
PASS
```

- [ ] **Step 6: Commit visual follow-up sensitivity**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add scripts/analysis/vision_safety_analysis.R docs/CALCULATIONS.md tests/testthat/test_objective2_safety_toxicity.R
git commit -m "feat: add visual-acuity follow-up sensitivity outputs"
```

Expected:

```text
[peer-review-statistical-revision ...] feat: add visual-acuity follow-up sensitivity outputs
```

---

### Task 8A: Clarify Objective 2 SRD/SRG Radiation-Induced Versus All-Cause Scope

**Files:**
- Modify: `<PEER_REVIEW_REPO>/scripts/analysis/vision_safety_analysis.R`
- Modify: `<PEER_REVIEW_REPO>/docs/CALCULATIONS.md`
- Modify: `<PEER_REVIEW_REPO>/docs/STATISTICAL_METHODS.md`
- Test: `<PEER_REVIEW_REPO>/tests/testthat/test_objective2_safety_toxicity.R`

- [ ] **Step 1: Audit source fields before changing SRD/SRG labels**

Use the Task 6 source-absence rule. Check derived runtime columns, raw/source columns, and data dictionary for radiation-induced SRD/SRG fields or timing/source qualifiers. Record the finding in `docs/peer_review_revision_response.md`.

- [ ] **Step 2: Implement the supported endpoint label**

If source fields distinguish radiation-induced SRD/SRG, derive explicit radiation-induced fields in Objective 0/Objective 2 and use those for reviewer-facing toxicity outputs.

If source fields do not distinguish etiology, keep the current fields but label them as all-cause recorded burden:

```r
objective2_toxicity_scope_note <- tibble::tibble(
    endpoint_family = "retinal_detachment",
    scope = "all_cause_recorded_burden",
    reviewer_label = "Serous retinal detachment was analyzed as recorded burden by available follow-up, not as adjudicated radiation-induced incidence.",
    limitation = "The current checked source fields do not establish radiation-induced attribution for each SRD/SRG record."
)
```

Write this note as a sheet in the Objective 2 toxicity workbook or in `vision_followup_sensitivity.xlsx` if that is the only Objective 2 reviewer-facing workbook touched by this task. Do not call SRD/SRG radiation-induced unless the source audit supports attribution.

- [ ] **Step 3: Add toxicity-scope test**

In `<PEER_REVIEW_REPO>/tests/testthat/test_objective2_safety_toxicity.R`, add:

```r
test_that("Objective 2 SRD/SRG reviewer-facing output declares radiation-induced versus all-cause scope", {
    pipeline <- run_objective2_test(create_test_dataset(), output_tag = "objective2_srd_scope")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    candidate_files <- list.files(
        pipeline$output_dirs$obj2_vision,
        pattern = "(toxicity|vision_followup_sensitivity).*\\.xlsx$",
        full.names = TRUE
    )
    expect_true(length(candidate_files) > 0)
    has_scope_sheet <- any(vapply(candidate_files, function(path) {
        "toxicity_scope" %in% readxl::excel_sheets(path)
    }, logical(1)))
    expect_true(has_scope_sheet)
})
```

- [ ] **Step 4: Run Objective 2 tests**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_objective2_safety_toxicity.R')"
```

Expected:

```text
PASS
```

- [ ] **Step 5: Commit SRD/SRG scope clarification**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add scripts/analysis/vision_safety_analysis.R docs/CALCULATIONS.md docs/STATISTICAL_METHODS.md tests/testthat/test_objective2_safety_toxicity.R docs/peer_review_revision_response.md
git commit -m "fix: clarify objective 2 retinal detachment toxicity scope"
```

Expected:

```text
[peer-review-statistical-revision ...] fix: clarify objective 2 retinal detachment toxicity scope
```

---

### Task 9: Add Tumor-Height Timing Summary And Demotion Guardrail

**Files:**
- Modify: `<PEER_REVIEW_REPO>/scripts/analysis/tumor_height_analysis.R`
- Modify: `<PEER_REVIEW_REPO>/docs/CALCULATIONS.md`
- Test: `<PEER_REVIEW_REPO>/tests/testthat/test_objective1_primary_outcomes.R`

- [ ] **Step 1: Add tumor-height timing helper**

In `<PEER_REVIEW_REPO>/scripts/analysis/tumor_height_analysis.R`, before `analyze_tumor_height_changes()`, add:

```r
#' Add tumor-height assessment timing
#'
#' @param data Data frame.
#' @return Data frame with `last_height_followup_months`.
add_tumor_height_followup_timing <- function(data) {
    if (all(c("treatment_date", "last_height_date") %in% names(data))) {
        data$last_height_followup_months <- suppressWarnings(lubridate::time_length(
            lubridate::interval(data$treatment_date, data$last_height_date),
            unit = "months"
        ))
    } else if ("follow_up_months" %in% names(data)) {
        data$last_height_followup_months <- suppressWarnings(as.numeric(data$follow_up_months))
    } else {
        data$last_height_followup_months <- NA_real_
    }
    data
}

#' Build tumor-height imaging timing audit for reviewer response
#'
#' @param data Data frame.
#' @return List with timing summary and negative-interval detail.
build_tumor_height_timing_audit <- function(data) {
    timed_data <- add_tumor_height_followup_timing(data)
    if (!all(c("treatment_group", "last_height_followup_months") %in% names(timed_data))) {
        return(list(summary = tibble::tibble(), negative_interval_detail = tibble::tibble()))
    }
    summary <- timed_data %>%
        dplyr::group_by(.data$treatment_group) %>%
        dplyr::summarise(
            variable = "last_height_followup_months",
            n_rows = dplyr::n(),
            n_nonmissing = sum(!is.na(.data$last_height_followup_months)),
            median_months = stats::median(.data$last_height_followup_months, na.rm = TRUE),
            min_months = min(.data$last_height_followup_months, na.rm = TRUE),
            max_months = max(.data$last_height_followup_months, na.rm = TRUE),
            n_negative_intervals = sum(.data$last_height_followup_months < 0, na.rm = TRUE),
            .groups = "drop"
        )
    negative_detail <- timed_data %>%
        dplyr::filter(!is.na(.data$last_height_followup_months), .data$last_height_followup_months < 0) %>%
        dplyr::select(dplyr::any_of(c(
            "record_id", "treatment_group", "treatment_date", "last_height_date",
            "last_height_followup_months", "initial_tumor_height", "last_height", "height_change"
        )))
    list(summary = summary, negative_interval_detail = negative_detail)
}
```

- [ ] **Step 2: Report timing before deciding whether the tumor-height comparison is reportable**

At the beginning of `analyze_tumor_height_changes()`, after `data <- normalize_treatment_group_data(data)`, add:

```r
data <- add_tumor_height_followup_timing(data)
```

Do **not** add `last_height_followup_months` to either treatment-effect model as if it were a baseline confounder. Measurement time occurs after treatment and can reflect treatment-specific surveillance or outcome processes; conditioning on it can create a new bias rather than correct the comparison. Keep the existing baseline-height sensitivity model, then use the timing summary to make the reporting decision:

- If timing distributions are broadly comparable and the model remains adequately supported, report tumor-height change as a limited secondary association with the timing table alongside it.
- If timing differs materially between arms or the follow-up pattern is too heterogeneous, remove the comparative regression result from the manuscript and report tumor-height reduction descriptively with an explicit limitation.

After the sensitivity model block, write timing audit:

```r
tumor_height_timing <- build_tumor_height_timing_audit(data)
write_readable_xlsx(
    list(
        timing_summary = tumor_height_timing$summary,
        negative_interval_detail = tumor_height_timing$negative_interval_detail
    ),
    file.path(output_dirs$obj1_height_primary, paste0(prefix, "tumor_height_timing_summary.xlsx"))
)
```

Add `timing_summary = tumor_height_timing` to the returned list.

- [ ] **Step 3: Update the test for timing output**

In `<PEER_REVIEW_REPO>/tests/testthat/test_objective1_primary_outcomes.R`, add:

```r
test_that("Objective 1 tumor-height analysis writes timing summary", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "objective1_tumor_height_timing")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    timing_path <- file.path(
        pipeline$output_dirs$obj1_height_primary,
        "test_tumor_height_timing_summary.xlsx"
    )
    expect_true(file.exists(timing_path))
    expect_true(all(c("timing_summary", "negative_interval_detail") %in% readxl::excel_sheets(timing_path)))
    timing_rows <- readxl::read_xlsx(timing_path, sheet = "timing_summary")
    expect_true("variable" %in% names(timing_rows))
    expect_true(any(timing_rows$variable == "last_height_followup_months"))
})
```

- [ ] **Step 4: Run Objective 1 tests**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_objective1_primary_outcomes.R')"
```

Expected:

```text
PASS
```

- [ ] **Step 5: Commit tumor-height timing**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add scripts/analysis/tumor_height_analysis.R tests/testthat/test_objective1_primary_outcomes.R docs/CALCULATIONS.md
git commit -m "feat: add tumor-height timing summary and reporting guardrail"
```

Expected:

```text
[peer-review-statistical-revision ...] feat: add tumor-height timing adjustment and summary
```

---

### Task 10: Prune PRAME And T4 Reviewer-Facing Subgroup Outputs

**Files:**
- Modify: `<PEER_REVIEW_REPO>/scripts/config/modeling_policy.R`
- Modify: `<PEER_REVIEW_REPO>/scripts/subgroup/subgroup_data_prep.R`
- Modify: `<PEER_REVIEW_REPO>/scripts/visualization/forest_plot_data.R`
- Test: `<PEER_REVIEW_REPO>/tests/testthat/test_forest_plot_labels.R`
- Test: `<PEER_REVIEW_REPO>/tests/testthat/test_objective1_primary_outcomes.R`

- [ ] **Step 1: Remove PRAME from reviewer-facing Objective 1 subgroup list**

In `<PEER_REVIEW_REPO>/scripts/config/modeling_policy.R`, remove `"gep12_prame_status"` from `subgroup_vars`.

The reviewer-facing `subgroup_vars` block should become:

```r
subgroup_vars <- c(
    "age_at_diagnosis_general_pop_median", "sex", "location", "initial_t_stage_simple",
    "initial_tumor_height", "initial_tumor_diameter",
    "initial_overall_stage", "biopsy1_gep", "gep_class_simple", "optic_nerve"
)
```

- [ ] **Step 2: Exclude T4 levels before subgroup modeling**

In `<PEER_REVIEW_REPO>/scripts/subgroup/subgroup_data_prep.R`, add this helper:

```r
#' Drop reviewer-excluded sparse subgroup levels
#'
#' @param data Data frame.
#' @param subgroup_var Character subgroup variable.
#' @return Data frame with reviewer-excluded subgroup levels removed.
drop_reviewer_excluded_subgroup_levels <- function(data, subgroup_var) {
    if (!subgroup_var %in% names(data)) {
        return(data)
    }
    if (identical(subgroup_var, "initial_t_stage_simple")) {
        return(data %>% dplyr::filter(is.na(.data[[subgroup_var]]) | .data[[subgroup_var]] != "T4"))
    }
    data
}
```

At the start of each subgroup model-prep function that receives `subgroup_var`, add:

```r
data <- drop_reviewer_excluded_subgroup_levels(data, subgroup_var)
```

The helper must also return or attach the number of excluded rows when the surrounding call path supports diagnostics. If the existing prep functions cannot carry attributes reliably, compute the excluded counts immediately before and after pruning in the caller and pass those counts to the reviewer-facing pruning audit in Step 3.

- [ ] **Step 3: Add diagnostics note for excluded subgroup levels**

In `<PEER_REVIEW_REPO>/scripts/visualization/forest_plot_data.R`, when constructing forest plot diagnostics rows, add columns that record both the exclusion reason and the number of rows removed. Compute the excluded-count source before the diagnostic mutation so missing optional columns cannot break the diagnostics path:

```r
.reviewer_excluded_n_source <- if ("n_excluded" %in% names(diagnostics)) {
    diagnostics$n_excluded
} else {
    rep(0L, nrow(diagnostics))
}

diagnostics <- diagnostics %>%
    dplyr::mutate(
        reviewer_exclusion_note = dplyr::case_when(
            .data$subgroup_var == "initial_t_stage_simple" ~ "T4 excluded from reviewer-facing subgroup displays due to sparse and inconsistent support.",
            .data$subgroup_var == "gep12_prame_status" ~ "PRAME status excluded from local-recurrence reviewer-facing subgroup displays because event support is inadequate.",
            TRUE ~ NA_character_
        ),
        reviewer_excluded_level = dplyr::case_when(
            .data$subgroup_var == "initial_t_stage_simple" ~ "T4",
            .data$subgroup_var == "gep12_prame_status" ~ "all PRAME rows in reviewer-facing local-recurrence subgroup surface",
            TRUE ~ NA_character_
        ),
        reviewer_excluded_n = dplyr::case_when(
            .data$subgroup_var %in% c("initial_t_stage_simple", "gep12_prame_status") ~ as.integer(.reviewer_excluded_n_source),
            TRUE ~ NA_integer_
        )
    )
```

The rendered forest plot data must not contain displayed T4 rows or PRAME local-recurrence rows. The diagnostics workbook must contain the exclusion reason and excluded-count support so the rows do not silently disappear.

Because `gep12_prame_status` is removed from `subgroup_vars`, PRAME may not appear in ordinary forest diagnostics. Add a separate `reviewer_pruning_audit` sheet to the subgroup diagnostics workbook with one row per reviewer-excluded subgroup surface:

```r
reviewer_pruning_audit <- tibble::tibble(
    subgroup_var = c("gep12_prame_status", "initial_t_stage_simple"),
    excluded_level = c("PRAME local-recurrence subgroup surface", "T4"),
    excluded_n = c(prame_excluded_n, t4_excluded_n),
    reason = c(
        "PRAME local-recurrence reviewer-facing subgroup display removed because event support is inadequate.",
        "T4 reviewer-facing subgroup display removed because support is sparse and inconsistent."
    )
)
```

`prame_excluded_n` and `t4_excluded_n` must come from the data actually entering the Objective 1 reviewer-facing subgroup surface, not from a hard-coded placeholder.

- [ ] **Step 4: Run subgroup and forest plot tests**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_forest_plot_labels.R')"
Rscript -e "testthat::test_file('tests/testthat/test_objective1_primary_outcomes.R')"
```

Expected:

```text
PASS
PASS
```

Before running, add these assertions to `<PEER_REVIEW_REPO>/tests/testthat/test_forest_plot_labels.R` or `<PEER_REVIEW_REPO>/tests/testthat/test_objective1_primary_outcomes.R`:

```r
test_that("reviewer-facing subgroup diagnostics record PRAME and T4 exclusions", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "objective1_subgroup_reviewer_pruning")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    diagnostics_files <- list.files(
        pipeline$output_dirs$obj1_forest_plots,
        pattern = "diagnostics.*\\.xlsx$",
        recursive = TRUE,
        full.names = TRUE
    )
    expect_true(length(diagnostics_files) > 0)

    diagnostics <- purrr::map_dfr(diagnostics_files, function(path) {
        sheets <- readxl::excel_sheets(path)
        purrr::map_dfr(sheets, ~ readxl::read_xlsx(path, sheet = .x))
    })
    expect_true("reviewer_exclusion_note" %in% names(diagnostics))
    subgroup_levels <- if ("subgroup_level" %in% names(diagnostics)) diagnostics$subgroup_level else character()
    exclusion_notes <- if ("reviewer_exclusion_note" %in% names(diagnostics)) diagnostics$reviewer_exclusion_note else character()
    expect_false(any(grepl("T4", subgroup_levels, fixed = TRUE) & is.na(exclusion_notes)))
    expect_true(any(grepl("T4 excluded", exclusion_notes, fixed = TRUE)))

    audit_files <- diagnostics_files[ purrr::map_lgl(diagnostics_files, ~ "reviewer_pruning_audit" %in% readxl::excel_sheets(.x)) ]
    expect_true(length(audit_files) > 0)
    pruning_audit <- purrr::map_dfr(audit_files, ~ readxl::read_xlsx(.x, sheet = "reviewer_pruning_audit"))
    expect_true(any(pruning_audit$subgroup_var == "gep12_prame_status"))
    expect_true(any(pruning_audit$subgroup_var == "initial_t_stage_simple" & pruning_audit$excluded_level == "T4"))
    expect_true(all(pruning_audit$excluded_n >= 0, na.rm = TRUE))
})
```

- [ ] **Step 5: Commit subgroup pruning**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add scripts/config/modeling_policy.R scripts/subgroup/subgroup_data_prep.R scripts/visualization/forest_plot_data.R tests/testthat/test_forest_plot_labels.R tests/testthat/test_objective1_primary_outcomes.R
git commit -m "fix: prune sparse reviewer-facing subgroup outputs"
```

Expected:

```text
[peer-review-statistical-revision ...] fix: prune sparse reviewer-facing subgroup outputs
```

---

## Goal Group 5: Documentation And Response

**Tasks:** 11, 12, 14.

**Recommended Codex goal text for Nick:**

```text
Execute Goal Group 5 from docs/superpowers/plans/2026-06-26-peer-review-statistical-revision.md in the peer-review worktree: create and populate the single coauthor response/results document, align methods/calculations/statistical-methods/interpretation docs, and validate response-document path aliases and Markdown after population. Stop after Task 14 and report changed files, exact commands run, tests passed/failed, runtime artifacts generated, unresolved decisions/blockers, remaining task IDs, and the recommended next Codex goal text.
```

**Required end-of-goal checkpoint:** changed files, exact commands run, tests passed/failed, runtime artifacts generated, unresolved decisions/blockers, remaining task IDs, and recommended next goal text.

### Task 11: Create The Single Coauthor Response And Results Document

**Files:**
- Create: `<PEER_REVIEW_REPO>/docs/peer_review_revision_response.md`

- [ ] **Step 1: Create the reviewer checklist document**

Create `<PEER_REVIEW_REPO>/docs/peer_review_revision_response.md` as a Tim-comment-indexed working response document. The document must be organized first around the requests Tim directed to Nick in the June 24 shared Google Doc, not around internal analysis modules. Each Tim-directed item must preserve enough Google Doc context to let Nick answer Tim without re-opening scattered notes: reviewer source, Tim comment/request, adjacent reviewer concern, current status, planned/implemented analysis, result, manuscript/response action, limitation, and exact runtime evidence alias.

```markdown
# Peer Review Revision Response And Results

This is the only coauthor-facing document for the response to Tim. It is built around Tim-directed Google Doc to-dos for Nick. Each item includes the Google Doc context, Tim's request, the analysis or documentation action, current status, result when available, manuscript/response change, limitation, owner, and a supporting artifact path expressed as a project path alias such as `<OUTPUT_DIR>/peer_review_revision_audits/...` or `<PROCESSED_DATA_DIR>/...`. Do not create a separate action matrix, data-availability memo, or results summary.

## Source Packet

- Editorial decision email dated June 21, 2026; resubmission due August 20, 2026.
- Tim email dated June 22, 2026: major revisions; both reviewers concerned about radiation-treatment details and statistical methods; some treatment-detail requests may be infeasible in the 60-day response window.
- Angie email dated June 23, 2026: most statistical comments make sense; meet with Nick to decide response.
- Shared Google Doc created June 24, 2026 by Tim for coauthor review and response drafting.
- Pasted statistical memo: de-escalation and refocusing, Cox-led inference, follow-up centrality, weakened claims.

## Tim-Directed Nick To-Do Index

Every Tim-directed comment for Nick must appear as a separate indexed item using this template. The `Direct answer` field is required because Tim needs a plain answer before the methods/result details.

```markdown
### T-NICK-XX: Short action title

> **STATUS:** Not started / Implemented pending verification / Pending runtime result / Decision needed / Clinical coauthor input needed / Complete.

**Direct answer:** One or two sentences answering Tim's or the reviewer's actual question before citing artifacts.
**Google Doc context:** Reviewer source, adjacent reviewer issue, and Tim's comment/request to Nick.
**Owner:** Nick, or Nick plus named coauthor.
**Action required:** Analysis, audit, documentation, or manuscript response action.
**Method / implementation:** Endpoint, model, adjustment set, horizon, feasibility rule, or source-field audit actually used.
**Result:** Numeric result and diagnostic status when available; otherwise the exact missing result still needed.
**Response/manuscript language:** Draft response point or exact manuscript change.
**Limitation / interpretation:** What the result can and cannot support.
**Evidence:** Project path alias to the runtime artifact or repo file supporting the row.
```

Minimum required indexed Tim/Nick items:

- `T-NICK-01`: Cox-only local recurrence and metastatic progression reanalysis.
- `T-NICK-02`: PH assumption confirmation with Schoenfeld residual diagnostics.
- `T-NICK-03`: Continuous-variable/dichotomization correction, especially continuous age.
- `T-NICK-04`: PFS definition and death/progression component reconciliation.
- `T-NICK-05`: Time-to-local-recurrence and time-to-metastasis Cox-led outputs.
- `T-NICK-06`: Five-year-capped OS/PFS HR sensitivity.
- `T-NICK-07`: Propensity-score feasibility and decision about whether any PS sensitivity is reportable.
- `T-NICK-08`: Visual-acuity follow-up timing, minimum-follow-up sensitivity, and adjusted visual-acuity model response.
- `T-NICK-09`: Follow-up duration summaries and endpoint-specific follow-up evidence.
- `T-NICK-10`: Tumor-height timing and interpretation guardrail.
- `T-NICK-11`: PRAME/T4 subgroup removal or demotion.
- `T-NICK-12`: Dose, proximity, optic nerve, macula/fovea, visual-field, and treatment-detail data-availability audit.
- `T-NICK-13`: Adverse-event grading/SRD/SRG scope clarification.
- `T-NICK-14`: Reviewer-response language guardrails: no equivalence, no simulated randomization, no broad vision-preservation claim.
- `T-NICK-15`: Table 1 p-value handling, explicitly noting Tim planned manual removal if still true.

The document may include a compact clinical-coauthor section after the Tim/Nick index, but the Tim/Nick index remains the primary checklist Nick uses to respond to Tim.

## Clinical Technique Checklist

| Source | Request | Needed Input | Owner |
| --- | --- | --- | --- |
| Reviewer 1 | PBT treatment details | Radionuclide, prescription approach, implant duration, plaque diameter, notched plaques | Tim / Melhus |
| Reviewer 1 | GK treatment details | Dose range, isodose approach, target definition, margin, setup, constraints | Mignano / Tim |
| Reviewer 2 | Radiation dose incorporation | Clinical explanation for why dose adjustment is unavailable/non-comparable | Tim / Melhus / Mignano |
| Reviewer 2 | Adverse event grading | Confirm grading exists only for visual acuity and not NVG/retinopathy/SRD | Tim / clinical team |
| Reviewers 1 and 2 | Replace UpToDate/EyeWiki | Primary sources and authoritative reviews | Tim / clinical team |

## Language Guardrails

- Replace "same efficacy" with "no statistically significant difference observed" and include imprecision/power limitations.
- Remove "preferred modality" unless framed as a hypothesis for future prospective evaluation.
- Remove "simulate randomization"; use "restricted cohort intended to improve comparability".
- Use "visual-acuity preservation" rather than broad "vision preservation".
- Label subgroup analyses exploratory and emphasize multiplicity/limited event support.
```

- [ ] **Step 2: Append data availability and scope limits to the same document**

Append the following section to `<PEER_REVIEW_REPO>/docs/peer_review_revision_response.md`:

```markdown
# Data Availability And Scope Limits

## Current Runtime Dataset Evidence

Runtime dataset aliases inspected:

- `<PROCESSED_DATA_DIR>/uveal_melanoma_full_cohort.rds`
- `<PROCESSED_DATA_DIR>/uveal_melanoma_restricted_cohort.rds`

Current reviewer-relevant fields present in both runtime datasets:

- Treatment/follow-up: `treatment_group`, `treatment_date`, `follow_up_months`, `last_followup`, `last_known_alive_date`
- Outcomes: `recurrence_event`, `tt_recurrence_months`, `mets_event`, `tt_mets_months`, `death_event`, `tt_death_months`, `pfs_event`, `tt_pfs_months`
- Visual acuity: `initial_vision`, `last_vision`, `vision_change`
- Tumor height: `initial_tumor_height`, `initial_tumor_diameter`, `last_height`, `last_height_date`, `height_change`
- Baseline geometry/symptoms: `location`, `optic_nerve`, `srf`, `visual_field_defect`, `vision_loss_blurred_vision`, `initial_t_stage_simple`, `initial_overall_stage`
- PBT-specific fields: `radionuclide`, `plaque_size`, `plaque_notch`, `initial_plaque`, `initial_plaque_date`

Fields not found in current runtime datasets:

- GK prescription dose, prescription isodose line, maximum dose, number of shots, number of isocenters
- Macular dose, optic-disc dose, lens dose
- Tumor-to-fovea distance, tumor-to-disc distance
- Longitudinal post-treatment visual-field outcome
- Eye-exam frequency schedule

## Implication For Reviewer Response

The current data can support:

- Cox-led time-to-local-recurrence and time-to-metastasis analyses.
- PH assumption reporting.
- Follow-up duration by treatment arm.
- Visual-acuity minimum-follow-up sensitivity with explicit treatment-to-`last_followup` timing and separately labeled proxy general-follow-up timing.
- Latest-VA reviewer-predictor sensitivity including viable baseline predictors requested by the reviewer: `initial_vision`, explicit latest-VA follow-up duration, `initial_tumor_height`, `initial_tumor_diameter`, `initial_t_stage_simple`, `srf`, `optic_nerve` where variable, centered treatment year, and the shared confounder set.
- Tumor-height imaging timing summaries.
- PBT descriptive treatment details for radionuclide, plaque size, and notched plaque use.
- Restricted-cohort cutoff verification for diameter <=20 mm and height <=10 mm.

The current data cannot support without new chart or plan review:

- Adjusting treatment-effect models for radiation dose across both modalities.
- Comparing optic nerve, retina, macula, fovea, or lens dose across both modalities.
- Evaluating longitudinal visual-field loss.
- Fully grading non-VA adverse-event severity if those grades were not collected.

## Recommended Response Strategy

- Add available treatment details and follow-up summaries.
- State missing dosimetry/proximity/visual-field data as explicit limitations.
- Avoid firm modality-comparison conclusions for visual acuity because treatment selection, tumor geometry, dose, and follow-up timing remain incompletely captured. The latest-VA reviewer-predictor sensitivity attenuated the apparent change-score association: full cohort MD -0.200 logMAR (95% CI -0.462 to 0.063; p = 0.135; n = 208) and restricted cohort MD -0.126 logMAR (95% CI -0.429 to 0.177; p = 0.411; n = 126).
- Frame the study as observational outcome reporting from routine clinical care, not as equivalence, noninferiority, or causal superiority evidence.
```

- [ ] **Step 3: Commit reviewer docs**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add docs/peer_review_revision_response.md
git commit -m "docs: add peer-review response checklist"
```

Expected:

```text
[peer-review-statistical-revision ...] docs: add peer-review response checklist
```

---

### Task 12: Align Manuscript Methods And Interpretation Docs

**Files:**
- Modify: `<PEER_REVIEW_REPO>/docs/METHODS_SECTION_PAPER.md`
- Modify: `<PEER_REVIEW_REPO>/docs/STATISTICAL_METHODS.md`
- Modify: `<PEER_REVIEW_REPO>/docs/INTERPRETATION_GUIDE.md`
- Modify: `<PEER_REVIEW_REPO>/docs/CALCULATIONS.md`
- Test: `<PEER_REVIEW_REPO>/tests/testthat/test_doc_contract_alignment.R`

- [ ] **Step 1: Replace the methods summary paragraph**

In `<PEER_REVIEW_REPO>/docs/METHODS_SECTION_PAPER.md`, replace the opening statistical-methods paragraphs with:

```markdown
Patients were analyzed in overlapping cohorts derived from the same cleaned analytic dataset: a full treatment cohort and a restricted cohort limited to patients considered eligible for either local therapy. Baseline characteristics were summarized descriptively by treatment group. Baseline p-values were not used to test comparability because treatment allocation was observational and clinically selected.

Adjusted treatment-effect analyses used a parsimonious prespecified covariate set of continuous age at diagnosis, sex, and tumor location. Local recurrence and metastatic progression were treated as time-dependent endpoints. The lead inferential analyses for these endpoints were Cox proportional-hazards models for time to local recurrence and time to metastatic progression, with proportional-hazards assumptions assessed using scaled Schoenfeld residuals. Descriptive event counts and cumulative-incidence summaries were retained as supportive context.

Overall survival was defined from treatment to death from any cause. [After the Task 3A endpoint decision, insert the exact approved PFS definition here. Under the recommended standard definition, PFS is time from treatment to the first of local recurrence, metastatic progression, or death from any cause.] Kaplan-Meier curves were retained for visualization with numbers at risk and preserve observed follow-up; unstable late tails should be addressed in figure selection, captioning, or explicitly approved sensitivity analyses rather than by silently changing plotted event/censoring data. Adjusted Cox models were used as model-based treatment-effect summaries subject to their PH diagnostics. A 5-year administratively censored Cox sensitivity analysis was added for OS and PFS because late risk sets were sparse, particularly in the GKSRS cohort.

Visual-acuity change was calculated as baseline minus follow-up logMAR so that negative values indicate worsening visual acuity. For patients with local recurrence, the follow-up value was defined as the measurement obtained immediately before salvage treatment in order to isolate the effect of primary therapy. Reviewer-response sensitivity analyses repeated visual-acuity summaries among patients meeting minimum follow-up thresholds using explicit treatment-to-`last_followup` timing and a separately labeled proxy general-follow-up timing surface. A separate latest-VA sensitivity model used follow-up logMAR as the outcome and adjusted for baseline logMAR, explicit latest-VA follow-up duration, viable reviewer-requested baseline predictors, and the shared covariate set. The analysis does not evaluate longitudinal visual-field loss and cannot adjust for unavailable macular/foveal proximity or radiation-dose fields.

Tumor-height change was defined as follow-up height minus baseline height, such that negative values indicate tumor shrinkage; for recurrent cases, the follow-up measurement was the pre-salvage height. Reviewer-response outputs summarize time from treatment to follow-up height measurement, and treatment-effect interpretation is limited by unequal imaging follow-up.

Subgroup analyses were exploratory. Sparse subgroup levels requested for removal during reviewer response, including PRAME rows without local-recurrence support and T4 subgroup displays, were removed from reviewer-facing outputs. Interaction results are interpreted cautiously because of limited event counts and multiplicity.
```

- [ ] **Step 2: Update PFS wording in calculations**

Ensure `<PEER_REVIEW_REPO>/docs/CALCULATIONS.md` contains:

```markdown
**Reviewer-response clarification:** The PFS definition below must match the approved Task 3A endpoint contract exactly. State all event components and explicitly state whether death without documented progression is an event. A figure caption alone cannot repair a mismatch between the PFS label and the implemented event/time derivation.
```

- [ ] **Step 3: Add interpretation guardrails**

In `<PEER_REVIEW_REPO>/docs/INTERPRETATION_GUIDE.md`, add:

```markdown
## Peer-Review Revision Language Guardrails

- Do not interpret a non-significant treatment effect as equivalent efficacy or noninferiority.
- Use "no statistically significant difference was observed" rather than "same efficacy".
- Do not describe the restricted cohort as simulating randomization.
- Use "visual-acuity preservation" when referring to the measured visual endpoint; do not use broad "vision preservation" unless visual-field outcomes are analyzed.
- State that missing dosimetry, optic-disc/macular/foveal proximity, and longitudinal visual-field outcomes limit causal interpretation of visual-acuity differences.
- Treat subgroup and interaction analyses as exploratory because event counts are low and multiplicity is substantial.
```

- [ ] **Step 4: Run doc contract tests**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_doc_contract_alignment.R')"
```

Expected:

```text
PASS
```

- [ ] **Step 5: Validate Markdown rendering and path contracts**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_peer_review_artifact_verification.R')"
Rscript - <<'RS'
docs <- c(
    "docs/peer_review_revision_response.md",
    "docs/METHODS_SECTION_PAPER.md",
    "docs/STATISTICAL_METHODS.md",
    "docs/INTERPRETATION_GUIDE.md",
    "docs/CALCULATIONS.md"
)
missing <- docs[!file.exists(docs)]
if (length(missing)) stop("Missing docs: ", paste(missing, collapse = ", "))
absolute_path_hits <- unlist(lapply(docs, function(path) {
    lines <- readLines(path, warn = FALSE)
    hits <- grep("/Users/ncamarda/", lines, value = TRUE, fixed = TRUE)
    if (length(hits)) paste(path, hits, sep = ": ")
}))
if (length(absolute_path_hits)) stop("Committed docs contain source-machine absolute paths:\n", paste(absolute_path_hits, collapse = "\n"))
cat("Markdown/path contract check passed\n")
RS
```

If a local Markdown renderer is available, preview the touched docs and confirm that tables, code fences, and reviewer checklist formatting render correctly. Do not add absolute local paths to committed reviewer-facing docs such as `docs/peer_review_revision_response.md`; use `<OUTPUT_DIR>`, `<PROCESSED_DATA_DIR>`, or repo-relative paths there. The exception is operator-facing material: this implementation plan and internal audit workbook sheets such as `clickable_paths` may use absolute local paths or Markdown `file://` links to make runtime artifacts easy to open.

Expected:

```text
PASS
Markdown/path contract check passed
```

- [ ] **Step 6: Commit doc alignment**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add docs/METHODS_SECTION_PAPER.md docs/STATISTICAL_METHODS.md docs/INTERPRETATION_GUIDE.md docs/CALCULATIONS.md tests/testthat/test_doc_contract_alignment.R
git commit -m "docs: align methods with peer-review statistical revision"
```

Expected:

```text
[peer-review-statistical-revision ...] docs: align methods with peer-review statistical revision
```

---

### Task 14: Populate And Verify The Single Coauthor Response Document

**Files:**
- Modify: `/Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision/docs/peer_review_revision_response.md`

- [ ] **Step 1: Populate every checklist row from verified runtime evidence**

After the restricted-cohort run, complete every `T-NICK-XX` item in `docs/peer_review_revision_response.md` with these labelled fields, in this order. Do not use `Complete` until the row has exact evidence aliases and the final response-document validation has passed.

```markdown
### T-NICK-XX: [Tim-directed checklist item]

> **STATUS:** Not started / Implemented pending verification / Pending runtime result / Decision needed / Clinical coauthor input needed / Complete.

**Direct answer:** One or two sentences answering Tim's or the reviewer's actual question before citing artifacts.
**Google Doc context:** Reviewer source, adjacent reviewer issue, and Tim's comment/request to Nick.
**Owner:** Nick, or Nick plus named coauthor.
**Action required:** Analysis, audit, documentation, or manuscript response action.
**Method / implementation:** Endpoint definition, model, adjustment set, horizon, PH/feasibility rule, and source-field audit actually used.
**Result:** Cohort, event count, effect estimate, 95% CI, p-value if reportable, and the exact diagnostic result that qualifies interpretation.
**Response/manuscript language:** Exact methods/results/discussion or response-document revision required.
**Limitation / interpretation:** Data, estimand, model-support, or interpretation constraint.
**Evidence:** Project path alias to the one or two runtime artifacts that support this row, for example `<OUTPUT_DIR>/peer_review_revision_audits/restricted_followup_and_data_availability.xlsx`.
```

Do not bulk-append a filesystem inventory. Runtime workbooks remain evidence, not a second coauthor-facing packet. Record only the artifact path aliases required to reproduce each stated result.

- [ ] **Step 2: Verify the document is complete and singular**

Before sharing the document with Tim, verify that every `T-NICK-XX` item listed in Task 11 is present and has a non-placeholder status, result or missing-result statement, response/manuscript action, limitation, and evidence alias. The completed document must cover recurrence, metastasis, PH assumptions, continuous age, PFS definition, five-year OS/PFS, follow-up, visual acuity, tumor height, propensity score, subgroups, radiation details/dosimetry, adverse-event grading, SRD/SRG scope, optic-nerve/proximity, Table 1 p-values, and manuscript-language changes. Confirm no `peer_review_revision_action_matrix.md`, `peer_review_revision_data_availability.md`, or `peer_review_revision_results_summary.md` has been created.

- [ ] **Step 3: Rerun response-document path and Markdown validation**

After populating `docs/peer_review_revision_response.md`, rerun the committed-document path check and inspect Markdown rendering. This must happen after results and evidence path aliases are filled in.

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_peer_review_artifact_verification.R')"
Rscript - <<'RS'
doc <- "docs/peer_review_revision_response.md"
lines <- readLines(doc, warn = FALSE)
if (any(grepl("/Users/ncamarda/", lines, fixed = TRUE))) {
    stop("Response document contains source-machine absolute paths; use <OUTPUT_DIR>, <PROCESSED_DATA_DIR>, or repo-relative aliases.")
}
required_headings <- c(
    "PFS", "visual acuity", "tumor height", "propensity", "adverse-event grading",
    "SRD", "SRG", "radiation", "optic"
)
missing <- required_headings[!vapply(required_headings, function(x) any(grepl(x, lines, ignore.case = TRUE)), logical(1))]
if (length(missing)) stop("Response document missing expected reviewer-response terms: ", paste(missing, collapse = ", "))
cat("Response document validation passed\n")
RS
```

Expected:

```text
PASS
Response document validation passed
```

- [ ] **Step 4: Commit the single response document**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git add docs/peer_review_revision_response.md
git commit -m "docs: complete peer-review response and results"
```

Expected:

```text
[peer-review-statistical-revision ...] docs: complete peer-review response and results
```

---

## Goal Group 6: Verification Only

**Tasks:** 13 and Final Verification Checklist.

**Recommended Codex goal text for Nick:**

```text
Execute Goal Group 6 from docs/superpowers/plans/2026-06-26-peer-review-statistical-revision.md in the peer-review worktree: run the verification-only pass, including targeted tests, restricted-cohort analysis, on-demand audit and propensity tools, fresh-artifact checks, workbook inspection, required figure/image visual inspection, full test suite if feasible, and the Final Verification Checklist. Do not add new analysis scope unless verification reveals a defect. Stop after the final checklist and report changed files, exact commands run, tests passed/failed, runtime artifacts generated, inspected figure paths, unresolved decisions/blockers, remaining task IDs, and whether the branch is ready for review.
```

**Required end-of-goal checkpoint:** changed files, exact commands run, tests passed/failed, runtime artifacts generated, inspected figure paths, unresolved decisions/blockers, remaining task IDs, and branch readiness for review.

### Task 13: Run Targeted Pipeline And Verify Reviewer Artifacts

**Files:**
- Runtime outputs under `/Users/ncamarda/ProjectsRuntime/uveal_melanoma/Analysis/`
- No source edits unless verification reveals a defect.

- [ ] **Step 1: Run targeted tests**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_file('tests/testthat/test_peer_review_revision_contract.R')"
Rscript -e "testthat::test_file('tests/testthat/test_peer_review_artifact_verification.R')"
Rscript -e "testthat::test_file('tests/testthat/test_objective3_objective4_scope_protection.R')"
Rscript -e "testthat::test_file('tests/testthat/test_peer_review_data_availability.R')"
Rscript -e "testthat::test_file('tests/testthat/test_propensity_score_feasibility.R')"
Rscript -e "testthat::test_file('tests/testthat/test_objective1_primary_outcomes.R')"
Rscript -e "testthat::test_file('tests/testthat/test_objective2_safety_toxicity.R')"
Rscript -e "testthat::test_file('tests/testthat/test_doc_contract_alignment.R')"
```

Expected:

```text
PASS
PASS
PASS
PASS
PASS
PASS
PASS
PASS
```

- [ ] **Step 2: Run restricted cohort analysis**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
export PEER_REVIEW_VERIFY_STARTED_AT="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
Rscript -e "source('scripts/load_all.R'); run_my_analysis('uveal_melanoma_restricted_cohort')"
Rscript scripts/tools/peer_review_followup_audit.R
Rscript scripts/tools/propensity_score_feasibility.R
```

Expected:

```text
Analysis completed
```

If the macOS OpenMP SHM2 crash occurs, stop repeated retries and have Nick run the same command locally in a normal terminal, then paste the output.

- [ ] **Step 3: Verify required reviewer artifacts from current run**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript - <<'RS'
source("scripts/load_all.R")
started <- as.POSIXct(Sys.getenv("PEER_REVIEW_VERIFY_STARTED_AT"), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
if (is.na(started)) stop("PEER_REVIEW_VERIFY_STARTED_AT is missing; rerun Step 2 and Step 3 together.")
required_patterns <- c(
    "local_recurrence_free_probability_effect_summary\\.xlsx$",
    "metastasis_free_survival_probability_effect_summary\\.xlsx$",
    "overall_survival_probability_5yr_capped_effect_summary\\.xlsx$",
    "progression_free_survival_probability_5yr_capped_effect_summary\\.xlsx$",
    "tumor_height_timing_summary\\.xlsx$",
    "vision_followup_sensitivity\\.xlsx$",
    "restricted_followup_and_data_availability\\.xlsx$",
    "restricted_propensity_score_feasibility\\.xlsx$"
)
files <- list.files(OUTPUT_DIR, recursive = TRUE, full.names = TRUE)
for (pattern in required_patterns) {
    hits <- files[grepl(pattern, basename(files))]
    restricted_hits <- hits[grepl("restricted|uveal_melanoma_restricted_cohort", hits, ignore.case = TRUE)]
    if (length(restricted_hits) > 0) {
        hits <- restricted_hits
    }
    if (length(hits) == 0) stop(sprintf("Missing required artifact pattern: %s", pattern))
    fresh <- hits[file.info(hits)$mtime >= started]
    if (length(fresh) == 0) stop(sprintf("Only stale artifacts found for pattern: %s", pattern))
    cat("FRESH:", fresh[[length(fresh)]], "\n")
}
RS
```

Expected output includes at least one restricted-cohort file for each of:

```text
local_recurrence_free_probability_effect_summary.xlsx
metastasis_free_survival_probability_effect_summary.xlsx
overall_survival_probability_5yr_capped_effect_summary.xlsx
progression_free_survival_probability_5yr_capped_effect_summary.xlsx
followup_and_data_availability.xlsx
propensity_score_feasibility.xlsx
tumor_height_timing_summary.xlsx
vision_followup_sensitivity.xlsx
```

- [ ] **Step 4: Inspect critical workbooks for reviewer-facing values**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "library(readxl); files <- list.files('/Users/ncamarda/ProjectsRuntime/uveal_melanoma/Analysis', pattern='(local_recurrence_free_probability_effect_summary|metastasis_free_survival_probability_effect_summary|5yr_capped_effect_summary|propensity_score_feasibility|followup_and_data_availability|vision_followup_sensitivity|tumor_height_timing_summary)\\\\.xlsx$', recursive=TRUE, full.names=TRUE); for (f in files) { cat('\\nFILE:', f, '\\n'); print(readxl::excel_sheets(f)); }"
```

Expected:

```text
FILE: ...
[1] ...
```

Every listed workbook should be readable by `readxl::excel_sheets()`.

- [ ] **Step 5: Inspect regenerated figures in an image viewer**

If Task 13 or any earlier task regenerated figure, plot, PNG, TIFF, JPEG, SVG, or PDF image artifacts, open the current-run files in an image viewer and record the inspected paths in the execution summary. This is required even when the generating script and tests pass.

Run this command to list recently modified figure-like artifacts under the runtime output root:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
find /Users/ncamarda/ProjectsRuntime/uveal_melanoma/Analysis \
  -type f \( -iname '*.png' -o -iname '*.jpg' -o -iname '*.jpeg' -o -iname '*.tif' -o -iname '*.tiff' -o -iname '*.svg' -o -iname '*.pdf' \) \
  -newermt "$PEER_REVIEW_VERIFY_STARTED_AT" \
  -print
```

For each regenerated reviewer-facing figure, open it with Preview or another image viewer:

```bash
open "/absolute/path/to/regenerated_figure.ext"
```

Verify, as applicable:

- The figure opens and is not blank, corrupt, cropped incorrectly, or visually unreadable.
- Endpoint labels match the revised endpoint contract, especially PFS, recurrence, metastasis, OS, and subgroup labels.
- Obsolete reviewer-facing elements are absent, including inappropriate logistic-primary framing, unsupported PRAME/T4 displays, and visible log-rank/RMST emphasis if those were demoted.
- Risk tables, legends, captions embedded in the plot, axis labels, confidence intervals, and treatment-group colors remain intelligible.
- The intended change is visible in the rendered figure, not only present in the source code.

Expected:

```text
Regenerated figures inspected visually; inspected file paths recorded in the execution summary.
```

- [ ] **Step 6: Run full test suite**

Run:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
Rscript -e "testthat::test_dir('tests/testthat')"
```

Expected:

```text
PASS
```

- [ ] **Step 7: Commit verification fixes only if needed**

If Step 1 through Step 6 required source fixes, commit those exact files:

```bash
cd /Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision
git status --short
git add \
  scripts/config/modeling_policy.R \
  scripts/analysis/survival_outcomes.R \
  scripts/analysis/binary_outcomes.R \
  scripts/workflow/objective_1_primary_outcomes.R \
  scripts/tools/peer_review_followup_audit.R \
  scripts/tools/propensity_score_feasibility.R \
  scripts/analysis/vision_safety_analysis.R \
  scripts/analysis/tumor_height_analysis.R \
  scripts/subgroup/subgroup_data_prep.R \
  scripts/visualization/forest_plot_data.R \
  tests/testthat/test_peer_review_revision_contract.R \
  docs/METHODS_SECTION_PAPER.md \
  docs/CALCULATIONS.md \
  docs/STATISTICAL_METHODS.md \
  docs/INTERPRETATION_GUIDE.md \
  docs/peer_review_revision_response.md
git commit -m "fix: stabilize peer-review revision outputs"
```

Expected:

```text
[peer-review-statistical-revision ...] fix: stabilize peer-review revision outputs
```

---

## Final Verification Checklist

- [ ] `git status --short` is clean in `/Users/ncamarda/Projects/uveal_melanoma-peer-review-statistical-revision`.
- [ ] `Rscript -e "testthat::test_dir('tests/testthat')"` passes or the exact failing tests are documented with reason.
- [ ] Restricted-cohort pipeline run completes or Nick runs locally if OpenMP SHM2 prevents assistant execution.
- [ ] Recurrence/metastasis reviewer-response outputs contain HRs, not ORs.
- [ ] PH diagnostics exist or skip explanations exist for every Cox model shown in response text.
- [ ] Continuous age is used in adjusted models.
- [ ] The approved PFS event/time contract, derived cohort data, figures, docs, and response text all match; the endpoint is not relabelled by caption alone.
- [ ] 5-year-capped OS/PFS sensitivity outputs exist.
- [ ] Follow-up duration, visual-acuity latest-follow-up sensitivity, and tumor-height imaging-time outputs exist.
- [ ] Propensity score feasibility output exists and is not described as simulated randomization.
- [ ] Adverse-event grading availability is explicitly stated; SRD/SRG outputs are labelled radiation-induced only if source evidence supports attribution, otherwise all-cause recorded burden.
- [ ] Dose/proximity/visual-field absences are documented as limitations rather than silently ignored.
- [ ] PRAME local-recurrence and T4 reviewer-facing subgroup displays are removed.
- [ ] Every regenerated reviewer-facing figure has been opened in an image viewer, visually inspected for the intended changes, and recorded in the execution summary.
- [ ] Methods, calculations, statistical methods, and interpretation docs no longer describe recurrence/metastasis logistic regression as primary inference.
- [ ] Manuscript response language avoids "same efficacy", "preferred modality", "simulate randomization", and broad "vision preservation".

## 2026-06-28 Completion Audit

This branch is complete for Nick-side code, runtime artifact, test, and baseline-documentation work after the final policy correction above.

Completed and verified:

- `docs/peer_review_revision_response.md` was restored to baseline and is not part of the final diff.
- `docs/PR_VS_ORIGINAL_RESULTS_AUDIT.md` was removed from the worktree and is not part of the final diff.
- Durable method/interpretation updates were placed in existing baseline documentation, including `docs/TECHNICAL.md`, `docs/STATISTICAL_METHODS.md`, and `docs/FIGURE_COUNTS_AUDIT.md`.
- The provenance plan remains commit-eligible and explicitly records that the response tracker and forensic audit are working notes, not deliverable commit artifacts.
- Full and restricted runtime evidence files exist for recurrence/metastasis effect summaries, 5-year OS/PFS sensitivity summaries, follow-up/data-availability audits, propensity-score feasibility audits, and tumor-height timing summaries.
- Visual spot-check opened the full and restricted local-recurrence and PFS KM plots. The plots are readable, have risk tables, omit visible log-rank p-values, and display observed follow-up under the configured 180-month maximum cap.
- Targeted verification passed with exit code 0:
  - `tests/testthat/test_doc_contract_alignment.R`
  - `tests/testthat/test_peer_review_artifact_verification.R`
  - `tests/testthat/test_peer_review_data_availability.R`
  - `tests/testthat/test_propensity_score_feasibility.R`
  - `tests/testthat/test_objective3_objective4_scope_protection.R`
  - `tests/testthat/test_peer_review_revision_contract.R`
  - `tests/testthat/test_objective1_primary_outcomes.R`
  - `tests/testthat/test_objective2_safety_toxicity.R`

Known residual items outside Nick-side completion:

- Tim/Angie still choose manuscript emphasis for visual-acuity cutoff reporting.
- Tim/Angie still choose final manuscript placement and wording for tumor-height analyses.
- Clinical coauthors still own final GK/PBT technique prose and any treatment-plan details not present in the analytic dataset.
- Tim owns final manuscript language cleanup and Table 1 p-value removal.
- A full `testthat::test_dir("tests/testthat")` run was not repeated in this final cleanup pass; the peer-review-specific targeted suite above was repeated after removing the non-deliverable working-note docs.

## Self-Review

Spec coverage:

- Tim's explicit Nick requests are covered in Tasks 3, 4, 5, 7, 8, 10, and 12.
- Reviewers' time-to-event and follow-up concerns are covered in Tasks 4, 5, 6, 8, 9, and 12.
- Radiation-treatment detail and dosimetry feasibility are covered in Tasks 6, 11, 12, and 14.
- Visual-field/proximity limitations are covered in Tasks 6, 8, 11, 12, and 14.
- Table 1 p-value removal is assigned to Tim in the single response document, with methods docs updated to descriptive baseline summaries in Task 12.
- Subgroup multiplicity and sparse support are covered in Tasks 10 and 12.

Placeholder scan:

- The plan contains no unresolved placeholder strings in committed source snippets.
- Runtime artifact paths are discovered by command after the restricted-cohort run instead of represented with placeholder strings.

Type consistency:

- New Objective 1 return names are consistently `recurrence_time_to_event`, `mets_time_to_event`, `os_5yr_capped`, and `pfs_5yr_capped`; reviewer-response audit and propensity feasibility outputs are on-demand tool artifacts, not Objective 1 return values.
- New helper functions are consistently named `write_peer_review_data_availability_audit()`, `fit_treatment_propensity_score()`, `fit_capped_cox_sensitivity()`, `build_visual_acuity_min_followup_sensitivity()`, and `build_tumor_height_timing_audit()`.
- New expected filenames use `make_filename_safe()` outputs implied by the labels in the implementation snippets.
