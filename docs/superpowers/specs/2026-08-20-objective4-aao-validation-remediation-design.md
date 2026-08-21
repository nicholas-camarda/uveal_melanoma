# Objective 4 AAO Validation Remediation Design

## Goal

Produce one reproducible, fail-closed Objective 4 no-GEP analysis whose post-treatment MFS population, censoring treatment, MSS competing-risk target, randomization, internal validation, and AAO presentation gate are aligned.

## Scientific contracts

### Patient 22

The raw source preserves both dated facts:

- initial AJCC staging on 2024-05-09 is Stage IIIA, cT4a cN0 cM0;
- the 2024-06-11 Gamma Knife planning MRI is retained as an investigator-adjudicated extrahepatic metastasis event that was imaging-suspected and never confirmed by dedicated imaging or pathology.

Because the adjudicated lesion was detected on the treatment date, Patient 22 has baseline metastatic disease for analyses whose time origin is initial treatment. The patient remains in the overall cohort and in analyses that do not require metastasis-free status at treatment, but is excluded from incident post-treatment MFS risk sets, MFS horizon outcomes, and MFS model evaluation. This is implemented as a general on-or-before-treatment rule, never as a patient-ID exception.

### Direct MFS target

The target is metastasis cumulative risk by 60 months among patients metastasis-free immediately after the treatment time origin. Death without metastasis remains a censoring event for this specific endpoint. Baseline metastatic disease is ineligible rather than counted as a time-zero incident event.

### Direct MSS target

The target is melanoma-death cumulative incidence by 60 months. Other-cause death before 60 months is a known competing outcome, not censoring. The direct horizon model therefore uses melanoma death as outcome 1, competing death as a known outcome 0, and IPCW only for loss to observation before the horizon.

## Validation architecture

The direct MFS and MSS models use repeated nested cross-validation:

1. Assign deterministic outer folds from a configured base seed.
2. For every outer fold, estimate the censoring distribution using only the outer-training rows.
3. Derive training weights and horizon outcomes from that training-only censoring model.
4. Generate deterministic inner `foldid` values and tune ridge `lambda` only within the outer-training rows.
5. Predict every outer-assessment row with complete baseline predictors.
6. Derive assessment IPCW weights from the outer-training censoring distribution.
7. Calculate out-of-fold time-dependent/IPCW AUC and Brier scores from assessment predictions and weights.

The final full-data ridge fit is retained only for coefficients and future scoring. All reported performance comes from out-of-fold predictions. The pipeline must stop if the requested direct model cannot use its declared censoring-aware method; raw-binary fallback is removed.

The surrogate Class-2-like model remains a deterministic nested-CV ridge classifier because censoring and competing risks do not apply to its molecular-class resemblance target.

## Performance scopes

Every repeated-CV result reports two prespecified scopes from the same out-of-fold predictions:

- overall complete-predictor validation population;
- no-GEP target population, with GEP Not Tested and GEP Failed/Indeterminate retained as separate descriptive subgroups.

If a scope lacks enough weighted cases or controls for a metric, the output records an explicit unsupported status and does not substitute an apparent, raw-binary, or broader-population estimate.

## Determinism

Objective 4 no-GEP seeds, outer repeats, outer folds, and inner folds are defined once in `scripts/config/gep_policy.R`. Every stochastic `glmnet` tuning path receives explicit deterministic `foldid` values. Outputs record the seed contract and fold counts. Re-running identical code and data must reproduce predictions and metrics exactly within the protected comparator tolerance.

## AAO accepted-abstract contract and gate

The accepted abstract is an immutable comparison reference, not an expected-value test for corrected estimators. Its contract records the submitted cohort count of 260, notes that subgroup counts were not stated, records the submitted AUCs and observed 5-year rates, and preserves three submitted conclusion categories: moderate direct prognostic stratification, failure to recover molecular class, and non-homogeneity of the two no-GEP groups.

The candidate gate compares corrected results with that reference and emits:

- `pass`: no ordering reversal, absolute AUC change at most 0.02, absolute observed-risk change at most 5 percentage points, and no conclusion-category change;
- `review`: any numeric threshold is crossed but the conclusion category remains supportable with revised wording;
- `fail`: a conclusion reverses, the molecular-surrogate conclusion changes, a required method is not the declared method, or a required metric is unsupported.

Repeated-fold percentile intervals are labeled partition-stability intervals, never confidence intervals. A presentation refresh is blocked until the gate result and claim ledger are reviewed.

## Protected comparison and publication

The current `master` revision is the protected base. Base and candidate runs use isolated runtime roots with recorded Git SHA, input-workbook SHA-256, analytic-RDS SHA-256, package-lock SHA-256, seed contract, and output hashes. The existing important-results comparator is run first to expose every changed protected artifact. A dedicated Objective 4 AAO gate then classifies expected scientific changes rather than weakening the protected comparator.

No canonical runtime artifact, published output, PowerPoint, or presenter guide is refreshed until the corrected candidate passes or receives explicit review clearance.

## Test strategy

- Unit tests: before/on/after-treatment metastasis boundaries; training-only censoring distributions; weighted AUC/Brier hand calculations; competing deaths as known MSS non-events; deterministic inner/outer folds; failure when the declared method is infeasible.
- Integration tests: repeated nested CV returns aligned row-level OOF predictions; overall and no-GEP scopes are computed from those predictions; reruns are identical; raw fallback is absent.
- Regression tests: accepted-abstract contract identity and parsing; gate pass/review/fail fixtures; Patient 22 actual-data endpoint eligibility; base-versus-candidate manifest and comparator invocation.
- Perturbation tests: changing an assessment-fold censoring time cannot change that fold's training censoring model; row reordering with a stable patient key cannot change keyed OOF predictions.

## Documentation and presentation wording

Repository methods and interpretation documentation will state the current estimator directly. The presentation claim ledger will distinguish the accepted values from corrected reportable values and will state whether any implication changed. Historical comparison language belongs in the AAO gate artifact and PR validation record, not in present-state README prose.
