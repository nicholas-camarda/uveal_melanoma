# Test-suite remediation design

Date: 2026-08-15  
Baseline: clean `master` at `14b27b7d7ffeda3c6da2ea2ba08108f669bd6846`  
Task branch: `codex/test-suite-remediation`

## Goal

Make the existing test suite simpler, hermetic, deterministic, and fail-closed with the smallest sufficient changes. The result must provide one canonical portable-suite command, complete required CI coverage, warning- and skip-sensitive execution, correct local actual-data routing, and intentional single execution of each full Objective 1–4 pipeline and merged-table path.

This is testing-infrastructure work. It must not change cohorts, endpoints, estimands, model specifications, production results, private data, or publication behavior.

## Non-negotiable boundaries

- Work occurs only in the dedicated `test-suite-remediation` worktree on `codex/test-suite-remediation`.
- Canonical `source/` remains clean and is used only for read-only baseline inspection and, after explicit approval, post-merge fast-forwarding.
- Disposable test and comparison output belongs under `$TMPDIR`; `runtime/runs` and canonical runtime are not workspaces or test silos.
- Private data remains local and read-only. No private rows, identifiers, dates, derived private hashes, fixtures, logs, or artifacts enter Git, GitHub, CI, PR text, or test output.
- Prefer edits to existing helpers, fixtures, runners, and workflow files. Add only one small portable-suite entrypoint if the current directory runner cannot express the complete gate cleanly.
- No compatibility shims, fallback paths, warning suppression, blanket skip allowances, or secondary meta-testing framework.
- The final PR is not merged automatically.

## Observed baseline

Fresh worktree execution on `14b27b7`:

```text
Rscript scripts/tools/run_testthat.R tests/testthat
FAIL 0 | WARN 958 | SKIP 1 | PASS 1548
Duration: 284.1 s

OCULAR_RUN_INTEGRATION_TESTS=true \
  Rscript scripts/tools/run_testthat.R tests/integration
FAIL 0 | WARN 0 | SKIP 6 | PASS 10
Duration: 1.1 s
```

Structural baseline:

- Required CI selects 9 of 40 unit files and 100 of 263 `test_that()` cases.
- The required branch-protection context is `required`; GitHub skips the full job on pull requests and `master` pushes.
- Six actual-data files contain 64 cases but skip at file scope because `tests/integration/helper-bootstrap.R` replaces both input paths with empty temporary directories.
- `run_objective1_test()` is defined twice and called 17 times; `run_objective_1()` also has direct test calls. Full and filtered execution therefore use different helper definitions and repeatedly run the entire Objective 1 pipeline.
- Objective 2 has multiple full-pipeline calls, Objective 3 has one, and Objective 4 has portable and actual-data full-pipeline calls. Execution counts are not asserted.
- The runner uses `stop_on_warning = FALSE`; unexpected warnings do not fail. The one portable skip is an obsolete optional-document test.
- Unit and integration entrypoints are fragmented across the workflow, README, `tests/testthat.R`, and the directory runner.

## Minimal design

### 1. Existing synthetic fixtures become the single portable data authority

Retain `tests/testthat/test_helper_data.R`. Do not add a calibration framework or generated data artifact.

- Keep fixed seeds and deterministic generation.
- Extend the existing synthetic fixture only where the consolidated Objective 1–4 runs require a balanced, warning-free schema.
- Use broad, rounded study-shaped distributions already represented in fixture code: treatment balance, age, sex, tumor dimensions, event/censoring support, follow-up, missingness, GEP/PRAME, and cohort membership.
- Guarantee model feasibility directly: both treatment arms, sufficient events, viable factor levels, nonnegative chronology, and complete model frames for happy-path pipeline tests.
- Retain separate small adversarial fixtures for sparse, one-arm, missing, or non-estimable behavior. Those tests must assert the expected warning or structured skip artifact directly rather than polluting the happy-path run.
- Preserve the existing privacy contract: no file reads, identifiers, calendar dates, or free text in the portable synthetic fixture.

### 2. One helper-owned full-pipeline fixture per objective

Use testthat helpers loaded before test files. Keep one definition for each full-pipeline test runner.

- Objective 1: one session-scoped canonical full-pipeline result, shared by every artifact and result contract that uses the standard synthetic cohort.
- Objective 2: one shared canonical full-pipeline result; tests with modified minimum-follow-up or scope behavior call the smallest relevant analysis function instead of rerunning the full objective.
- Objective 3: retain one canonical full-pipeline run.
- Objective 4: retain one canonical portable full-pipeline run; local actual-data validation may have one separately cached actual-data run.
- Merged tables: one intentional synthetic execution in the portable suite.
- Scenario-specific behavior should be tested below the objective orchestrator whenever the full pipeline is not necessary.
- A simple counter in the existing helper state records full-pipeline and merged-table invocations. A direct regression test asserts the expected counts. No separate instrumentation package or framework is introduced.
- Remove the duplicate `run_objective1_test()` definition and stop sourcing `test_helper_data.R` through multiple names.

### 3. Warnings and skips fail closed

Enhance `scripts/tools/run_testthat.R` minimally:

- Default to failure on unexpected warnings.
- Add direct result inspection that fails on unexpected skips for portable lanes.
- Keep an explicit targeted-test mode for development, but do not provide a global warning or skip bypass to CI.
- Expected warnings use `expect_warning()` or equivalent direct condition assertions. Happy-path fixtures must emit none.
- Remove the obsolete optional response-document skip test or replace it with a deterministic contract on a tracked artifact.

### 4. One canonical portable-suite command

Add one small entrypoint, `scripts/tools/run_portable_suite.R`, that runs in fresh child processes:

1. all files under `tests/testthat` with unexpected warning/skip failure;
2. the portable synthetic integration file(s) with the same policy;
3. `lintr::lint_package()`;
4. direct suite-completeness checks: discovered versus executed test files/cases and the Objective 1–4/merged-table invocation contract.

The entrypoint delegates test execution to the existing `run_testthat.R`; it does not recreate testthat reporting. README, contributor guidance, and GitHub CI use this exact command.

### 5. Integration inputs and outputs are separated

Repair `tests/integration/helper-bootstrap.R` so it never derives actual inputs from the temporary output root.

- Portable integration remains entirely synthetic and uses temporary inputs/outputs.
- The actual-data command requires explicit `OCULAR_INTEGRATION_RAW_DATA_DIR` and `OCULAR_INTEGRATION_PROCESSED_DATA_DIR` values.
- When the actual-data lane is enabled, missing directories or required files are errors, not skips.
- Actual inputs are read-only; all generated outputs remain in the integration `$TMPDIR` root.
- Direct tests set fake explicit input directories and prove that bootstrap preserves them while redirecting every output path.
- The local real-data run uses canonical private paths only through environment variables supplied at invocation. Its report contains aggregate test counts and sanitized reasons only.

### 6. Required CI runs the complete portable suite

Simplify `.github/workflows/portable-tests.yml`:

- One required job on pull requests and `master` pushes.
- The job restores the locked R environment and runs `Rscript scripts/tools/run_portable_suite.R`.
- Remove the filename filter and manual-only full job.
- Keep the existing pinned R version, package repository snapshot, actions, permissions, and concurrency behavior.
- Update `test_ci_contract.R` to fail if CI reintroduces a test filter, stops running the canonical command, disables warning/skip failure, or omits PR/push execution.
- Keep the existing required context name `required`; no branch-protection or broader repository-infrastructure change is needed.

## Gate validation

The candidate gate must be tested, not assumed. In disposable copies under `$TMPDIR`, introduce one mutation at a time and require the canonical command to exit nonzero for:

- a failing expectation;
- an unexpected warning;
- an unexpected skip;
- an omitted test file/case relative to discovery;
- broken actual-data input routing;
- duplicate Objective 1, 2, 3, or 4 full-pipeline execution;
- duplicate merged-table execution.

Each disposable mutation is discarded after the rejection is observed. No mutation is committed or applied to canonical `master`.

## Controlled baseline-versus-candidate comparison

Before requesting merge approval:

- Use clean canonical `master` `14b27b7` as baseline and the task worktree HEAD as candidate.
- Use the same R installation, lockfile-keyed library, commands, synthetic inputs, and environment variables.
- Run both from fresh processes and isolate all outputs under separate `$TMPDIR` roots.
- Run the final candidate portable suite at least twice; require identical file/case counts and pass/fail/warning/skip totals.
- Where inexpensive, run files in reverse or randomized order in a fresh process and require the same material results.
- Run the local actual-data lane read-only from both trees where command compatibility permits; compare only sanitized counts/routing status, never private contents.

## Acceptance criteria

The PR is ready for merge review only when all are true:

1. Complete portable suite: zero failures, zero unasserted warnings, zero skips.
2. Every discovered portable test file and case is executed by the canonical command.
3. Required GitHub CI uses the canonical command and passes on the PR.
4. Objective 1, 2, 3, and 4 full pipelines and merged tables each have one documented canonical portable execution, with no accidental reruns.
5. Actual-data routing uses explicit read-only inputs, temporary outputs, and fails closed on missing prerequisites.
6. All seven representative gate mutations are rejected.
7. Two fresh candidate runs are materially identical.
8. Canonical `master` remains clean and unchanged throughout candidate work.
9. No private or generated sensitive artifact appears in Git status, Git history, the PR, or CI.
10. Any scientific-behavior contracts added in this PR are limited to direct regression coverage needed by the infrastructure changes. Broader scientific expansion is deferred until this gate is stable.

## Deliverable

One focused draft pull request containing the smallest coherent set of helper, fixture, runner, workflow, documentation, and regression-test changes required to meet the acceptance criteria. The PR remains unmerged until explicit user approval.
