# Objective 4 AAO Validation Remediation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Deliver one fail-closed Objective 4 no-GEP pipeline with correct baseline-metastasis eligibility, fold-local censoring correction, competing-risk MSS targeting, deterministic nested CV, target-population performance, and a machine-readable AAO presentation gate.

**Architecture:** Keep raw clinical facts in Objective 0 and derive explicit analysis eligibility without patient-ID exceptions. Move reusable horizon-status, censoring, weighted-metric, and nested-CV behavior into a focused Objective 4 utility module; leave workbook orchestration and reader-facing assembly in the existing no-GEP report module. Compare corrected outputs against both the protected base and an immutable accepted-abstract contract before any publication or slide refresh.

**Tech Stack:** R, testthat, dplyr, survival, glmnet, yaml, jsonlite, openxlsx, existing runtime comparator.

**Spec:** `docs/superpowers/specs/2026-08-20-objective4-aao-validation-remediation-design.md`

## Global Constraints

- Patient 22 is baseline metastatic for treatment-origin MFS but retains dated initial Stage IIIA cT4a cN0 cM0 and imaging-unconfirmed provenance.
- Implement the Patient 22 decision through a general `mets_progression_date <= treatment_date` rule; never branch on patient ID.
- MFS estimates incident metastasis after treatment; MSS estimates melanoma-death cumulative incidence with other-cause death competing.
- Censoring distributions are estimated inside each outer-training fold and applied to that fold's assessment rows.
- All reported performance is out-of-fold; apparent performance may be diagnostic only.
- No raw-binary fallback or silent degradation is permitted.
- All stochastic behavior is controlled by constants defined once in `scripts/config/gep_policy.R`.
- Accepted-abstract values remain immutable reference values and are not overwritten by corrected results.
- Do not publish canonical outputs or refresh presentation artifacts until the presentation gate is cleared.

---

### Task 1: Baseline-metastasis endpoint contract

**Files:**
- Modify: `scripts/data_helper/data_derivation.R`
- Modify: `scripts/tools/derived_variables_documentation.R`
- Modify: `scripts/config/objective0_contracts.R`
- Test: `tests/testthat/test_objective0_data_processing_portable.R`
- Test: `tests/testthat/test_objective0_validation_engine.R`

**Interfaces:**
- Produces: `mets_at_or_before_treatment`, `mets_free_at_baseline`, `mets_event_analysis`, and `tt_mets_months_analysis`.
- Contract: raw `mets_event` and `tt_mets_months` remain unchanged; analysis fields are `NA` when baseline metastatic disease makes incident MFS undefined.

- [ ] **Step 1: Add a failing boundary test**

Create a three-row fixture with metastasis one day before, exactly on, and one day after treatment. Assert literal expectations:

```r
expect_identical(derived$mets_at_or_before_treatment, c(TRUE, TRUE, FALSE))
expect_identical(derived$mets_free_at_baseline, c(FALSE, FALSE, TRUE))
expect_identical(derived$mets_event_analysis, c(NA_integer_, NA_integer_, 1L))
expect_true(all(is.na(derived$tt_mets_months_analysis[1:2])))
expect_gt(derived$tt_mets_months_analysis[[3]], 0)
```

- [ ] **Step 2: Run the targeted test and verify RED**

Run:

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter objective0_data_processing_portable
```

Expected: failure because the on-treatment row is currently baseline-free and analysis fields retain zero/negative times.

- [ ] **Step 3: Implement the general endpoint rule**

Derive the flags before analysis times and set analysis event/time to missing for on-or-before-treatment metastasis. Use analysis fields for MFS horizon events, horizon times, event types, and MFS eligibility. Preserve raw timing fields for audit.

- [ ] **Step 4: Add validation-engine expectations**

Assert that an adjudicated on-treatment metastasis is an informational baseline-disease finding rather than an unresolved non-positive-time warning, while a contradictory or missing-date event remains fail-closed.

- [ ] **Step 5: Run Objective 0 tests and verify GREEN**

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter 'objective0_(data_processing_portable|validation_engine)'
```

- [ ] **Step 6: Commit**

```bash
git add -- scripts/data_helper/data_derivation.R scripts/tools/derived_variables_documentation.R scripts/config/objective0_contracts.R tests/testthat/test_objective0_data_processing_portable.R tests/testthat/test_objective0_validation_engine.R
git commit -m "Define baseline metastasis eligibility for MFS"
```

### Task 2: Deterministic horizon and censoring primitives

**Files:**
- Create: `scripts/gep/utils/gep_exploratory_internal_validation.R`
- Modify: `scripts/load_all.R`
- Modify: `scripts/config/gep_policy.R`
- Test: `tests/testthat/test_objective4_internal_validation.R`
- Modify: `tests/testthat/required-test-files.txt`

**Interfaces:**
- Produces: `derive_horizon_status(time, event_type, horizon_months)`, `fit_training_censoring_distribution(time, event_type, horizon_months)`, `predict_censoring_survival(censoring_fit, times)`, `derive_fold_ipcw_payload(training, assessment, time_var, event_type_var, horizon_months)`, and `create_deterministic_fold_ids(strata, folds, seed, stable_id)`.
- Event types: `0L` censored/event-free observation, `1L` target event, `2L` competing event.

- [ ] **Step 1: Write failing hand-derived tests**

Test that target events and competing events before the horizon have known status, early censoring does not, and event-free follow-up through the horizon does. Test that changing assessment censoring times leaves the fitted training censoring survival unchanged.

- [ ] **Step 2: Verify RED**

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter objective4_internal_validation
```

Expected: failure because the new interfaces do not exist.

- [ ] **Step 3: Define production constants once**

Add:

```r
GEP_EXPLORATORY_CV_SEED <- 20260820L
GEP_EXPLORATORY_CV_REPEATS <- 20L
GEP_EXPLORATORY_OUTER_FOLDS <- 5L
GEP_EXPLORATORY_INNER_FOLDS <- 5L
```

- [ ] **Step 4: Implement censoring and deterministic-fold primitives**

Fit censoring Kaplan-Meier curves only on outer-training rows. For target events use censoring survival immediately before event time; for horizon survivors use survival at the horizon; for competing events use survival immediately before the competing-event time. Cap and normalize positive training weights using the existing configured cap, and apply the training-derived cap to assessment weights without refitting.

- [ ] **Step 5: Run tests and verify GREEN**

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter objective4_internal_validation
```

- [ ] **Step 6: Commit**

```bash
git add -- scripts/gep/utils/gep_exploratory_internal_validation.R scripts/load_all.R scripts/config/gep_policy.R tests/testthat/test_objective4_internal_validation.R tests/testthat/required-test-files.txt
git commit -m "Add fold-local censoring validation primitives"
```

### Task 3: Censoring-aware OOF AUC, Brier, and calibration

**Files:**
- Modify: `scripts/gep/utils/gep_exploratory_internal_validation.R`
- Test: `tests/testthat/test_objective4_internal_validation.R`

**Interfaces:**
- Produces: `calculate_ipcw_auc(outcome, score, weight)`, `calculate_ipcw_brier(outcome, score, weight)`, and `summarize_ipcw_calibration(outcome, predicted, weight)`.

- [ ] **Step 1: Write failing literal metric tests**

Use four observations with literal outcomes, scores, and unequal weights. Hand-calculate weighted concordant case-control mass and weighted squared error. Include tests that zero-weight censored rows cannot affect the result and that a missing weighted case or control returns an explicit unsupported status.

- [ ] **Step 2: Verify RED**

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter objective4_internal_validation
```

- [ ] **Step 3: Implement weighted metrics**

Calculate AUC from weighted case-control pairs with half credit for ties; calculate Brier as the sum of weighted squared errors divided by positive weight mass. Fit calibration intercept/slope with binomial GLMs using assessment IPCW weights and return explicit sparse-support statuses.

- [ ] **Step 4: Run tests and verify GREEN**

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter objective4_internal_validation
```

- [ ] **Step 5: Commit**

```bash
git add -- scripts/gep/utils/gep_exploratory_internal_validation.R tests/testthat/test_objective4_internal_validation.R
git commit -m "Evaluate horizon predictions with IPCW metrics"
```

### Task 4: Deterministic nested-CV ridge engine

**Files:**
- Modify: `scripts/gep/utils/gep_exploratory_internal_validation.R`
- Modify: `scripts/gep/orchestration/gep_exploratory_no_gep_report.R`
- Test: `tests/testthat/test_objective4_internal_validation.R`
- Test: `tests/integration/test_exploratory_no_gep_report.R`

**Interfaces:**
- Produces: `cross_validate_horizon_ridge(data, predictors, time_var, event_type_var, horizon_months, stable_id_var, seed, repeats, outer_folds, inner_folds)` returning keyed row-level OOF predictions, fold metadata, and scoped metrics.
- Produces: deterministic final-fit `foldid` for coefficient/scoring models.

- [ ] **Step 1: Write failing leakage and determinism tests**

Assert that assessment-only censoring perturbations do not change training weights or fitted lambda, identical calls return identical keyed OOF predictions, and row reordering followed by stable-ID sorting returns identical predictions.

- [ ] **Step 2: Verify RED**

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter 'objective4_internal_validation|exploratory_no_gep_report'
```

- [ ] **Step 3: Implement nested CV**

Use deterministic outer fold IDs and deterministic inner `foldid` values passed to `glmnet::cv.glmnet`. Fit with outer-training IPCW weights, predict all complete-predictor assessment rows, and attach assessment weights from the outer-training censoring model. Do not catch model errors as missing predictions; stop with repeat/fold context.

- [ ] **Step 4: Remove silent fallback**

Delete `fallback_to_raw` and `raw_binary_fallback`. Direct models must request and report `ipcw_horizon_mfs` or `ipcw_horizon_competing_risk_mss`; infeasibility is an error. Retain any raw-binary comparison only as an explicitly named, separately invoked diagnostic that cannot populate primary outputs.

- [ ] **Step 5: Run tests and verify GREEN**

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter 'objective4_internal_validation|exploratory_no_gep_report'
```

- [ ] **Step 6: Commit**

```bash
git add -- scripts/gep/utils/gep_exploratory_internal_validation.R scripts/gep/orchestration/gep_exploratory_no_gep_report.R tests/testthat/test_objective4_internal_validation.R tests/integration/test_exploratory_no_gep_report.R
git commit -m "Replace no-GEP validation with deterministic nested CV"
```

### Task 5: Align MFS and MSS targets and report target-population performance

**Files:**
- Modify: `scripts/gep/orchestration/gep_exploratory_no_gep_report.R`
- Modify: `scripts/gep/utils/gep_model_evaluation_metrics.R`
- Test: `tests/testthat/test_objective4_gep_analysis_portable.R`
- Test: `tests/integration/test_exploratory_no_gep_report.R`

**Interfaces:**
- MFS event type: metastasis `1L`, otherwise censored `0L`, using Objective 0 analysis time/event fields.
- MSS event type: melanoma death `1L`, other-cause death `2L`, otherwise censored `0L`.
- Produces scoped rows with `performance_scope` equal to `Overall` or `No GEP`, plus explicit support counts and method identifiers.

- [ ] **Step 1: Write failing target tests**

Assert that a competing death before 60 months is a known MSS non-event with positive evaluation weight, while early loss to follow-up is unknown with zero evaluation weight. Assert that baseline-metastatic MFS rows never enter MFS OOF metrics.

- [ ] **Step 2: Verify RED**

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter 'objective4_gep_analysis_portable|exploratory_no_gep_report'
```

- [ ] **Step 3: Wire MFS and MSS model targets**

Route MFS through the incident post-treatment analysis fields. Route MSS through the three-state death outcome. Ensure the final MSS prediction is described as 60-month melanoma-death cumulative-incidence risk.

- [ ] **Step 4: Add overall and no-GEP OOF summaries**

Aggregate each repeat's keyed OOF predictions with the same assessment weights for both scopes. Report GEP Not Tested and Failed/Indeterminate counts descriptively, but do not emit subgroup AUCs when weighted case/control support is inadequate.

- [ ] **Step 5: Update workbook and narrative fields**

Replace `95% repeated-CV interval` with `95% repeated-partition stability interval`, write the exact model/evaluation method, and remove claims that unweighted known-status AUCs are censoring-aware.

- [ ] **Step 6: Run tests and verify GREEN**

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter 'objective4_gep_analysis_portable|exploratory_no_gep_report'
```

- [ ] **Step 7: Commit**

```bash
git add -- scripts/gep/orchestration/gep_exploratory_no_gep_report.R scripts/gep/utils/gep_model_evaluation_metrics.R tests/testthat/test_objective4_gep_analysis_portable.R tests/integration/test_exploratory_no_gep_report.R
git commit -m "Align Objective 4 MFS and MSS validation targets"
```

### Task 6: Accepted-abstract contract and presentation gate

**Files:**
- Create: `docs/maintenance/objective4_aao_accepted_abstract_contract.yaml`
- Create: `scripts/tools/evaluate_objective4_aao_gate.R`
- Create: `tests/testthat/test_objective4_aao_gate.R`
- Modify: `tests/testthat/required-test-files.txt`

**Interfaces:**
- Contract contains accepted abstract ID `30085896`, submitted cohort size, metrics/rates, conclusion categories, and review thresholds; it explicitly records that subgroup counts were not stated in the accepted abstract.
- CLI consumes `--contract`, `--candidate-workbook`, and `--report`; returns 0 only for `pass`, and nonzero for `review` or `fail`.

- [ ] **Step 1: Write failing parser and gate tests**

Create synthetic candidate workbook fixtures for: unchanged pass; AUC delta above 0.02 review; rate delta above 5 percentage points review; ordering reversal fail; surrogate conclusion reversal fail; missing required method fail.

- [ ] **Step 2: Verify RED**

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter objective4_aao_gate
```

- [ ] **Step 3: Add the immutable accepted contract**

Record submitted cohort `n = 260`; AUCs `0.686`, `0.663`, and `0.515`; MFS/MSS rates `2.9%/0.0%`, `15.0%/9.1%`, `60.0%/33.3%`, and `53.7%/38.3%` for Class 1, not tested, failed/indeterminate, and Class 2; and the three submitted conclusion categories. Record `subgroup_counts_reported = false`. Store candidate values only in generated gate reports, never in this contract.

- [ ] **Step 4: Implement the fail-closed gate**

Read required workbook sheets by labels, validate method identifiers and scopes, calculate deltas, test ordering/conclusion rules, and write a PHI-free JSON report with `pass`, `review`, or `fail` plus reasons.

- [ ] **Step 5: Run tests and verify GREEN**

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter objective4_aao_gate
```

- [ ] **Step 6: Commit**

```bash
git add -- docs/maintenance/objective4_aao_accepted_abstract_contract.yaml scripts/tools/evaluate_objective4_aao_gate.R tests/testthat/test_objective4_aao_gate.R tests/testthat/required-test-files.txt
git commit -m "Add AAO accepted-abstract presentation gate"
```

### Task 7: Protected actual-data base-versus-candidate run

**Files:**
- Create: `docs/validation/objective4-aao-validation-remediation.md`
- Generated only: `runtime/runs/objective4-aao-validation-remediation-base/`
- Generated only: `runtime/runs/objective4-aao-validation-remediation-candidate/`

**Interfaces:**
- Consumes: current raw workbook, protected base SHA `1a79c6895e25aeabb4b36cc9f1b0c2353e1e0133`, candidate branch, `docs/maintenance/important_results_contract.yaml`, and the AAO contract.
- Produces: run manifests, protected comparator report, AAO gate report, and a claim ledger without patient-level data.

- [ ] **Step 1: Run isolated base Objective 0 through Objective 4**

Use a detached base worktree and route all outputs to `runtime/runs/objective4-aao-validation-remediation-base/`. Record Git SHA, raw-workbook SHA-256, `renv.lock` SHA-256, analytic-RDS SHA-256, seeds, commands, and output hashes.

- [ ] **Step 2: Run isolated candidate Objective 0 through Objective 4**

Run the same commands and manifest procedure under `runtime/runs/objective4-aao-validation-remediation-candidate/`.

- [ ] **Step 3: Run the protected comparator**

Set `UVEAL_WORKSPACE_ROOT` to the canonical workspace root before running the
commands below.

```bash
Rscript scripts/tools/compare_important_results.R \
  --base-runtime "$UVEAL_WORKSPACE_ROOT/runtime/runs/objective4-aao-validation-remediation-base" \
  --candidate-runtime "$UVEAL_WORKSPACE_ROOT/runtime/runs/objective4-aao-validation-remediation-candidate" \
  --contract docs/maintenance/important_results_contract.yaml \
  --report "$UVEAL_WORKSPACE_ROOT/runtime/runs/objective4-aao-validation-remediation-candidate/protected-comparison.json"
```

Expected: changed Objective 4 artifacts are reported as differences; unrelated protected results remain identical.

- [ ] **Step 4: Run the AAO gate**

```bash
Rscript scripts/tools/evaluate_objective4_aao_gate.R \
  --contract docs/maintenance/objective4_aao_accepted_abstract_contract.yaml \
  --candidate-workbook "$UVEAL_WORKSPACE_ROOT/runtime/runs/objective4-aao-validation-remediation-candidate/Analysis/uveal_full/04_GEP_Validation/d_exploratory_no_gep/full_cohort_exploratory_no_gep_report.xlsx" \
  --report "$UVEAL_WORKSPACE_ROOT/runtime/runs/objective4-aao-validation-remediation-candidate/objective4-aao-gate.json"
```

- [ ] **Step 5: Record validation evidence**

Write cohort counts, endpoint support, overall/no-GEP OOF metrics, accepted-value deltas, ordering checks, conclusions, exact commands, and artifact hashes to `docs/validation/objective4-aao-validation-remediation.md`.

- [ ] **Step 6: Commit**

```bash
git add -- docs/validation/objective4-aao-validation-remediation.md
git commit -m "Record Objective 4 AAO validation evidence"
```

### Task 8: Documentation and full verification

**Files:**
- Modify: `docs/STATISTICAL_METHODS.md`
- Modify: `docs/INTERPRETATION_GUIDE.md`
- Modify: `docs/OBJECTIVES.md` only if its current-state target wording requires correction.

- [ ] **Step 1: Update present-state methods**

Document fold-local censoring estimation, weighted OOF metrics, competing-risk MSS horizon target, deterministic seeds, target-population scope, baseline-metastasis eligibility, and fail-closed method behavior.

- [ ] **Step 2: Update interpretation limits**

State that the corrected values are internally validated exploratory performance, distinguish overall from no-GEP performance, and prohibit interpreting partition-stability intervals as sampling confidence intervals.

- [ ] **Step 3: Run focused tests**

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter 'objective0|objective4|doc_contract_alignment'
```

- [ ] **Step 4: Run the complete portable gate**

```bash
Rscript scripts/tools/run_portable_suite.R
```

Expected: unit/synthetic integration tests and lint all pass with zero warnings or skips.

- [ ] **Step 5: Verify diff and commit**

```bash
git diff --check
git status --short
git add -- docs/STATISTICAL_METHODS.md docs/INTERPRETATION_GUIDE.md docs/OBJECTIVES.md
git commit -m "Document corrected Objective 4 validation methods"
```

### Task 9: Presentation refresh after gate clearance

**Files:**
- Generated/published Objective 4 outputs under the canonical runtime and approved Project Vault output snapshot.
- Update the AAO PowerPoint and presenter guide in the Project Vault poster directory.

**Interfaces:**
- Consumes: reviewed `objective4-aao-gate.json`, candidate claim ledger, official AAO template, accepted abstract, and current disclosure.
- Produces: provenance-aligned Objective 4 report, refreshed PowerPoint, refreshed presenter guide, and rendered-slide QA evidence.

- [ ] **Step 1: Stop if the gate is not cleared**

Do not publish or edit presentation artifacts when gate status is `review` or `fail` without explicit investigator clearance recorded in the validation document.

- [ ] **Step 2: Regenerate canonical Objective 4 outputs**

Run Objective 0 and Objective 4 from the reviewed candidate revision, verify the canonical run manifest, then perform a publish dry run before `publish_outputs()`.

- [ ] **Step 3: Refresh presentation claims**

Update only values and wording supported by the corrected claim ledger. Preserve the accepted abstract's scientific question and explicitly identify estimator-driven changes that require softened interpretation.

- [ ] **Step 4: Add the nested-validation presenter explanation**

Verify the final explanation against the implemented estimator before editing
the presentation materials. Keep the visible methods slide concise with
`Nested repeated 5-fold cross-validation` or equally brief verified wording.
Add the following explanation to the methods-slide speaker notes and presenter
guide:

> We used nested cross-validation. Within each outer training set, inner folds
> selected the ridge penalty. The resulting model then predicted the held-out
> outer fold, which had no role in tuning, fitting, or estimating censoring
> weights. Repeating this process provided genuinely out-of-fold predictions
> for every patient.

Add the three-minute spoken version, `Model tuning occurred only within the
training data, and performance was evaluated on held-out patients.` Add a
likely-question response explaining that separating model tuning from outer-fold
evaluation limits optimistic performance estimates. Do not describe the
partition-stability intervals as population confidence intervals.

- [ ] **Step 5: Verify presentation artifacts**

Render every slide, run overflow/placeholder checks, verify disclosure and
schedule details, reconcile every numerical claim back to the corrected
workbook, and confirm that the visible methods wording, speaker notes, and
presenter guide all describe the same implemented validation procedure.

- [ ] **Step 6: Record final hashes and handoff**

Record the final PowerPoint, presenter guide, Objective 4 workbook, source revision, input workbook, and package-lock hashes in the presentation-preparation record.
