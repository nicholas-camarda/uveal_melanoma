# Objective 4 AAO validation evidence

## Decision

The corrected candidate is **not presentation-cleared yet**. Both protected
actual-data runs completed Objective 0 through Objective 4 without fatal
workflow errors, and the AAO gate returned `review`, not `fail`. The accepted
abstract's three conclusion categories remain supportable, but all three AUCs
changed by more than the prespecified 0.02 review threshold. The presentation
therefore requires investigator review and revised numbers before publication
or slide refresh.

The general protected-results comparator could not evaluate base-versus-
candidate equality because its checked-in contract describes synthetic
artifacts that the production workflow does not generate. This is an
infrastructure blocker, not evidence that unrelated protected results are
equal or different. No synthetic substitute or fallback comparison was used.

## Reproducibility boundary

| Item | Protected base | Candidate |
|---|---|---|
| Git SHA | `1a79c6895e25aeabb4b36cc9f1b0c2353e1e0133` | `34de189010a559554a0d4ab8ff36e9079b8af42d` |
| Raw workbook SHA-256 | `f047fd021cf96c74e03ca884a13b739bfe33a2ca260e82f8af0c486de3c5e1ad` | same |
| `renv.lock` SHA-256 | `189e6e0a5d89abaef5144e8bc8ca609cb4472401f544523ba44e72b3b23f99ed` | same |
| Full-cohort analytic RDS SHA-256 | `9db09c6315db99b69df59b347d13b0bd2798985bbd19e95edec99e0c5234b260` | `fcdbcdcf35a81bdc8b5b029f8ab2907207fd6c59b7d95e6e2983f301e6920b50` |
| No-GEP report workbook SHA-256 | `ddb69f6e02ff2ba96594b6ed7ca0d7feaec9c7f8e907617ed798cafface92f59` | `c826c1a2c9606dc111b6e6251c10c817b64396296a38723509a03ca771f07af5` |
| Completed TXT log SHA-256 | `bf26c8f1861ea698ffccfe9605ff4c85e19a7d0f685993ff7849d6d6a6d6de31` | `bec47069e8847ad649cf97f58e657215e9b8000754ad1535de79aa5a0183d599` |
| Run state | `completed_with_warnings` | `completed_with_warnings` |

Both runs used the corrected source workbook above from clean isolated runtime
roots. Objective 0 reported no hard validation errors. The recorded warnings
were feasibility/status skips in sparse secondary GEP/PRAME components; the
primary no-GEP report was generated in both runs. These warnings do not replace
the full Task 8 verification gate.

The protected base used its historical exploratory defaults: base seed 123,
20 repeated five-fold partitions, and no explicitly locked inner `glmnet`
`foldid`. The candidate used the single policy contract: seed 20260820, 20
repeats, five outer folds, and five inner folds.

Complete file-level SHA-256 manifests are outside Git at:

- `/Users/ncamarda/Workspaces/uveal-melanoma/runtime/runs/objective4-aao-validation-remediation-base/output-sha256.csv`
- `/Users/ncamarda/Workspaces/uveal-melanoma/runtime/runs/objective4-aao-validation-remediation-candidate/output-sha256.csv`

Their run manifests are `run-manifest.json` in the same roots. The manifest
SHA-256 values are:

- base run manifest: `f3534e31e78452c6e2b8ed32d9c7b86368f3f547159aab5da53e0be398737d0a`
- candidate run manifest: `f40255b2de6b0e6efb3deef045a20b93ff0defd40875d48b9cfa231610ab0859`
- base output-hash manifest: `72c13d940b68dd675de4665e2dddcf0c73cbb577ea4b147533630bc11096396a`
- candidate output-hash manifest: `5276beaf02832b9ab40592e0c2842fcbc4380e3583a39eadeaa0ad013889ce8d`

## Cohort and endpoint support

Objective 0 produced 260 full-cohort, 167 restricted-cohort, and 92 GKSRS-only
records. In the full cohort, one adjudicated metastasis was present at the
treatment-time origin and was excluded from the incident post-treatment MFS
risk set. The source event and date remain preserved; the post-treatment MFS
analysis fields are ineligible for that baseline event. The resulting full
cohort contained 43 incident post-treatment metastasis events, 34 melanoma
deaths, and 23 competing deaths over all follow-up.

The primary full direct models used complete baseline predictors and generated
out-of-fold predictions for the following populations:

| Endpoint and scope | OOF n | Raw events | Positive evaluation-weight n | Cases / controls | Weighted cases / controls | Status |
|---|---:|---:|---:|---:|---:|---|
| MFS, Overall | 243 | 32 | 138 | 32 / 106 | 24.719 / 113.349 | `ok` |
| MFS, No GEP | 165 | 19 | 94 | 19 / 75 | 15.332 / 80.198 | `ok` |
| MSS, Overall | 244 | 20 | 149 | 20 / 129 | 16.852 / 132.229 | `ok` |
| MSS, No GEP | 166 | 12 | 106 | 12 / 94 | 10.256 / 95.498 | `ok` |

MFS estimates 60-month incident post-treatment metastasis risk. MSS estimates
60-month melanoma-death cumulative-incidence risk, treating other-cause death
as a known competing outcome. Evaluation weights for each assessment fold came
from that fold's outer-training censoring distribution.

## Candidate out-of-fold performance

| Model and scope | IPCW OOF AUC | 95% partition-stability interval | IPCW Brier | Calibration slope |
|---|---:|---|---:|---:|
| Direct MFS, Overall | 0.656 | 0.618 to 0.687 | 0.142 | 0.910 |
| Direct MFS, No GEP | 0.636 | 0.591 to 0.686 | 0.132 | 0.759 |
| Direct MSS, Overall | 0.603 | 0.498 to 0.660 | 0.101 | 0.501 |
| Direct MSS, No GEP | 0.589 | 0.431 to 0.640 | 0.089 | 0.331 |

The molecular-surrogate model had repeated out-of-fold AUC 0.563 and Brier
0.225. It remains a weak molecular approximation and is not suitable for
molecular reassignment. The intervals above describe stability across repeated
partitions; they are not sampling confidence intervals.

The protected base workbook reported AUCs 0.515, 0.686, and 0.662 for the
surrogate, direct MFS, and direct MSS models. Its direct-model support was only
the known-status subset (MFS n=139; MSS n=132), and it did not report the
candidate's Overall and No-GEP OOF scopes. Those values are therefore useful
historical anchors but are not estimator-equivalent to the corrected IPCW OOF
results.

## Accepted-abstract gate

The immutable accepted-abstract contract is abstract `30085896`, cohort
`n=260`. Its contract SHA-256 is
`6372efebf395276c63ccbbe2fe5daf1f6d6c425e23b54709e3c94481e0373e34`.
The generated gate report SHA-256 is
`c5f4d4b522777d5aabd59927c2c2d860f5511bc378c747466bea92a797ba5918`.

| AUC | Accepted | Candidate | Signed change | Absolute change | Gate |
|---|---:|---:|---:|---:|---|
| Molecular surrogate | 0.515 | 0.563 | +0.048 | 0.048 | Review |
| Direct MFS, Overall | 0.686 | 0.656 | -0.030 | 0.030 | Review |
| Direct MSS, Overall | 0.663 | 0.603 | -0.060 | 0.060 | Review |

Every observed five-year risk changed by less than 0.5 percentage points,
well below the five-percentage-point review threshold:

| Group | MFS accepted / candidate | MSS accepted / candidate |
|---|---:|---:|
| Class 1 | 2.9% / 2.94% | 0.0% / 0.0% |
| GEP Not Tested | 15.0% / 14.58% | 9.1% / 9.06% |
| GEP Failed/Indeterminate | 60.0% / 60.0% | 33.3% / 33.33% |
| Class 2 | 53.7% / 53.70% | 38.3% / 38.32% |

No ordering or structured conclusion reversal was detected. In particular:

- definitive Class 1 remained the lowest observed-risk group;
- GEP Failed/Indeterminate remained higher risk than GEP Not Tested for both
  observed MFS and MSS;
- the candidate's median direct predicted risks were also higher for
  Failed/Indeterminate than Not Tested;
- the surrogate result continued to reject molecular-class recovery from
  baseline clinical features; and
- the two no-GEP groups remained non-homogeneous.

The gate status is `review` solely because the three absolute AUC changes
exceed 0.02. The scientific direction did not reverse, but the accepted AUCs
must not be presented as corrected results. Publication and presentation
refresh remain blocked pending explicit review clearance.

## Protected comparator infrastructure blocker

`docs/maintenance/important_results_contract.yaml` has SHA-256
`952c5dd12193a992b4da42d49b8e8f2fe90e5b37245495e519ec5c28ec486f48`.
Repository-wide search and the original bootstrap plan show that this is a
synthetic comparator contract exercised by synthetic tests. It requires:

- `cohort/membership.json`
- `results/important_results.json`
- `results/displayed_labels.txt`
- `plots/plot_payload.json`
- `tables/important_results.xlsx`

No production extractor or registry mapping creates those paths from an
Objective 0 through Objective 4 run. The exact comparator invocation therefore
returned nonzero and wrote a sanitized report in which all five comparisons
failed as `required artifact missing`. The report SHA-256 is
`34f77f80b4626f796e0493a1ab785c4ba47118b5d0658fd95e77145423cf5e9b`.

Consequently, this validation does **not** claim that unrelated protected
results are identical. A production extraction contract must be implemented
and reviewed before that claim can be made. The AAO-specific gate remains
valid because it reads the generated candidate workbook directly under its
separate immutable contract.

## Exact workflow commands

Base and candidate used the same R expression and input path; only the detached
code checkout and isolated runtime root differed:

```bash
env UVEAL_WORKSPACE_ROOT=/Users/ncamarda/Workspaces/uveal-melanoma \
  OCULAR_RUNTIME_ROOT=<isolated-runtime-root> \
  RAW_DATA_DIR='/Users/ncamarda/Library/CloudStorage/OneDrive-Personal/Project Vault/Research/uveal-melanoma/Original Files' \
  Rscript -e "source('scripts/load_all.R'); RECREATE_ANALYTIC_DATASETS <- TRUE; result <- run_my_analysis('uveal_melanoma_full_cohort', c(0, 4)); if (identical(result\$run_state, 'failed')) stop(sprintf('run failed: %s', paste(result\$fatal_issues, collapse=' | '))); message(sprintf('RUN_STATE=%s', result\$run_state)); if (length(result\$warning_issues)) message(sprintf('WARNING_ISSUES=%s', paste(result\$warning_issues, collapse=' | ')))"
```

The protected base command ran from the detached base worktree with
`<isolated-runtime-root>` set to
`/Users/ncamarda/Workspaces/uveal-melanoma/runtime/runs/objective4-aao-validation-remediation-base`.
The candidate command ran from this branch with the root ending in
`objective4-aao-validation-remediation-candidate`.

```bash
env UVEAL_WORKSPACE_ROOT=/Users/ncamarda/Workspaces/uveal-melanoma \
  Rscript scripts/tools/compare_important_results.R \
  --base-runtime /Users/ncamarda/Workspaces/uveal-melanoma/runtime/runs/objective4-aao-validation-remediation-base \
  --candidate-runtime /Users/ncamarda/Workspaces/uveal-melanoma/runtime/runs/objective4-aao-validation-remediation-candidate \
  --contract docs/maintenance/important_results_contract.yaml \
  --report /Users/ncamarda/Workspaces/uveal-melanoma/runtime/runs/objective4-aao-validation-remediation-candidate/protected-comparison.json
```

```bash
env UVEAL_WORKSPACE_ROOT=/Users/ncamarda/Workspaces/uveal-melanoma \
  Rscript scripts/tools/evaluate_objective4_aao_gate.R \
  --contract docs/maintenance/objective4_aao_accepted_abstract_contract.yaml \
  --candidate-workbook /Users/ncamarda/Workspaces/uveal-melanoma/runtime/runs/objective4-aao-validation-remediation-candidate/Analysis/uveal_full/04_GEP_Validation/d_exploratory_no_gep/full_cohort_exploratory_no_gep_report.xlsx \
  --report /Users/ncamarda/Workspaces/uveal-melanoma/runtime/runs/objective4-aao-validation-remediation-candidate/objective4-aao-gate.json
```

The final two commands returned 1 by contract: the comparator failed for
missing synthetic-contract artifacts, and the AAO gate classified the
candidate as `review`.
