# Objectives 1–3 Release Certification Design

## Purpose

Certify the current Objective 1–3 analyses before collaborator sharing. The certification must connect canonical raw inputs, regenerated analytic datasets, analysis code, runtime artifacts, documentation, reviewer notes, and interpretations without adding parallel analysis machinery.

## Scope

- Regenerate every analytic dataset from the canonical raw inputs.
- Run the established analysis workflow for all cohorts.
- Certify the full and restricted cohorts for collaborator-facing Objectives 1–3.
- Check the GKSRS cohort only for internal pipeline integrity.
- Repair CI so routine pushes test a reproducible environment and report failures clearly.
- Reconcile tracked documentation and the ignored local reviewer response, audit tracker, and paper methods notes with certified results.

Objective 4 may run through the existing full workflow, but its scientific review is outside this certification.

## Non-Goals

- No new analysis or certification entry point.
- No new statistical estimand or model.
- No broad refactor.
- No duplicate configuration, endpoint contract, manifest, or documentation surface.
- No automatic OneDrive publication.
- No inclusion of local reviewer documents in Git.
- No fallback, compatibility, or silent-rescue behavior.

## Current Evidence

- The failed GitHub Actions run stopped during dependency installation while compiling `Deriv 4.3.0` against R 4.4.3. Portable tests, integration tests, and lint did not run.
- The current runtime contains retired Objective 3 flat outputs and the former `b_proportional_hazards_diagnostics/` mirror. An in-place rerun therefore cannot prove that every remaining artifact is current.
- The repository already provides the required orchestration and audit machinery:
  - `main_execution()`
  - `RECREATE_ANALYTIC_DATASETS`
  - `OCULAR_RUNTIME_ROOT`
  - Objective 0 downstream endpoint validation
  - `run_selected_objectives()`
  - `run_peer_review_followup_audits()`
  - `audit_effect_summary_directory()`
  - `run_tool_refresh_suite()`
  - `scripts/tools/run_testthat.R`
  - `publish_outputs(dry_run = TRUE)`

## Minimum-Intervention Rule

Use an existing command, contract, validator, or report whenever it can perform the required work. Change code only when a concrete failed check proves that the current implementation is incorrect or incomplete.

Every implementation edit must:

1. address one demonstrated failure;
2. preserve correct existing behavior;
3. avoid duplicated logic;
4. include a focused regression check when feasible;
5. carry before-and-after evidence;
6. remain in a narrowly scoped commit.

Operational correctness means that every defined gate passes, every expected statistical limitation is documented, and no discrepancy remains unexplained.

## Execution Design

### 1. Clean regeneration

Run the existing workflow against a newly created temporary runtime:

```sh
cert_runtime=$(mktemp -d /tmp/uveal-certification.XXXXXX)
OCULAR_RUNTIME_ROOT="$cert_runtime" Rscript -e \
  'source("scripts/load_all.R"); RECREATE_ANALYTIC_DATASETS <- TRUE; main_execution()'
```

This command regenerates all analytic datasets and executes the established workflow without allowing old runtime files to satisfy freshness checks.

The run must record:

- Git commit;
- raw-input file identity and modification time;
- regenerated dataset hashes;
- cohort and treatment counts;
- endpoint event counts;
- JSON run log;
- final run state.

### 2. Existing validation and audit commands

Use the current Objective 0 validation results, test runners, audit helpers, documentation refresh, and publish dry run. Do not wrap them in another analysis entry point.

The canonical runtime remains unchanged until the clean run and comparison pass. Publishing remains a separately approved action.

### 3. Runtime promotion

After certification, replace the generated analytic datasets and Objective 1–3 artifact surfaces with the clean outputs as one reviewed operation. Retired paths must not be carried forward. Objective 4 and unrelated runtime material remain outside this promotion unless independently reviewed.

## Certification Matrix

Each check receives one status:

- `PASS`: correct and internally consistent.
- `PASS WITH CAVEAT`: correct, with an explicit statistical or data limitation that restricts interpretation.
- `BLOCK`: failed execution, unexplained discrepancy, stale artifact, unsupported interpretation, or missing evidence.

No collaborator packet is approved while a `BLOCK` remains.

### Data provenance

- Verify raw-input availability and identity.
- Compare regenerated patient IDs, cohort membership, treatment counts, endpoint counts, and exclusions with the current analytic datasets.
- Require patient-level explanations for unexpected membership or endpoint changes.
- Verify all Objective 0 hard-error contracts for Objectives 1–3.

### Objective 1

- Verify local-recurrence, metastatic-progression, overall-survival, and PFS time/event definitions.
- Verify that PFS is the first local recurrence, metastatic progression, or death.
- Verify Cox/HR contracts, modeled populations, event counts, KM risk sets, PH diagnostics, and 60-month sensitivity analyses.
- Verify exploratory age, T4, and GEP subgroup policies, treatment-column alignment, interaction reporting, and outcome-specific non-estimability.
- Verify tumor-height timing and the required limitation on comparative interpretation.

### Objective 2

- Keep descriptive VA, adjusted VA, latest-VA ANCOVA, and 12/36/60-month minimum-follow-up change-score analyses distinct.
- Verify modeled denominators, treatment coding, baseline VA, follow-up duration, viable reviewer predictors, adjustment variables, effect measures, and model labels.
- Verify that retinopathy, NVG, and SRD represent recorded burden by available follow-up rather than time-to-toxicity incidence.
- Verify SRD scope and every explicit model skip.

### Objective 3

- Verify that PFS-2 is time to second local recurrence after first-recurrence treatment.
- Verify that death before second recurrence is censoring.
- Verify salvage-treatment groups, analyzable population, event support, KM summaries, RMST outputs, and PH handling.
- Require unsupported treatment comparisons to remain explicitly skipped.
- Confirm that retired flat and mirror outputs are absent from the clean runtime.

### Documentation and interpretation

Reconcile:

- `docs/TECHNICAL.md`
- `docs/STATISTICAL_METHODS.md`
- `docs/CALCULATIONS.md`
- generated study documentation
- local `docs/peer_review_revision_response.md`
- local `docs/PR_VS_ORIGINAL_RESULTS_AUDIT.md`
- local `docs/METHODS_SECTION_PAPER.md`

Every reported number, endpoint definition, model family, effect measure, limitation, and evidence path must match the regenerated artifacts. Observational results must not be described as causal, equivalent, noninferior, or proving the same efficacy.

Update existing reviewer documents rather than creating another reporting system. The local reviewer files remain ignored and untracked.

## CI Design

Retain `.github/workflows/portable-tests.yml` as the CI entry point.

Required CI must:

1. use a fixed R version and a committed, verified dependency resolution;
2. run environment setup, portable tests, synthetic integration tests, and lint/documentation checks as clearly named stages;
3. use the same fail-sensitive commands documented for local use;
4. provide a concise failure-stage summary and preserve detailed logs;
5. remain independent of private analytic data.

A separate scheduled compatibility run may test newer R and package versions. Dependency drift discovered there must not silently change the required push environment.

Local collaborator certification additionally requires the private-data regeneration and Objective 1–3 review. Portable CI cannot certify private results.

## Failure Handling

- Execution or validation error: `BLOCK`.
- Missing or stale required artifact: `BLOCK`.
- Unexplained change in patient count, event count, estimate, or treatment label: `BLOCK`.
- Documentation or interpretation mismatch: `BLOCK`.
- Expected sparse-data, PH, or insufficient-event limitation: `PASS WITH CAVEAT` only when the affected model, reason, and interpretation restriction are explicit.
- A failed stage must leave the canonical runtime unchanged.
- Old outputs must never substitute for failed regeneration.

## Verification Requirements

Use the existing checks first:

```sh
Rscript scripts/tools/run_testthat.R tests/testthat
OCULAR_RUN_INTEGRATION_TESTS=true Rscript scripts/tools/run_testthat.R tests/integration
Rscript -e 'lints <- lintr::lint_package(); if (length(lints) > 0L) { print(lints); stop(sprintf("%d lint(s) found", length(lints))) }'
```

Add or modify tests only for concrete gaps found during certification. Priority failure cases include:

- changed cohort membership without an audit explanation;
- swapped treatment-arm counts;
- incorrect endpoint or effect-measure metadata;
- stale retired output paths;
- documentation numbers that do not match artifacts;
- an unjustified model estimate where the supported result is a skip.

## Commit Design

Use concise, purpose-specific commits:

1. CI reproducibility, if required.
2. Verified analysis correction, only if the audit proves one is necessary.
3. Documentation alignment.

Stage explicit file lists. Never stage the ignored local reviewer documents.

## Acceptance Criteria

The collaborator release is ready when:

- clean regeneration completes without fatal errors;
- all Objective 0 hard-error validations pass;
- all Objective 1–3 certification checks for full and restricted cohorts are `PASS` or `PASS WITH CAVEAT`;
- GKSRS completes its internal integrity check;
- all result changes are explained;
- tracked and local documentation match certified artifacts;
- portable tests, feasible integration tests, and lint pass in the verified local environment;
- required GitHub CI passes from dependency setup through all checks;
- the canonical runtime contains only reviewed current Objective 1–3 outputs;
- the exact collaborator-facing files and caveats have been identified;
- no publication or external sharing occurs without explicit approval.
