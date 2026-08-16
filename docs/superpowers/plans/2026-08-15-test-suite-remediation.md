# Test Suite Remediation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the fragmented permissive test gate with one complete, warning- and skip-sensitive portable suite while consolidating full Objective 1–4 and merged-table executions around deterministic shared fixtures.

**Architecture:** Keep testthat and the current project helpers. Extend the existing deterministic fixtures, give each objective one helper-owned cached full run, split portable and private integration directories, and add one thin portable-suite process orchestrator that delegates to the existing directory runner. GitHub CI invokes only that canonical command.

**Tech Stack:** R 4.4.x, testthat 3.2.3, withr, existing project scripts/helpers, GitHub Actions, lintr.

## Global Constraints

- Work only in the dedicated `test-suite-remediation` worktree on `codex/test-suite-remediation`.
- Never write test or validation artifacts to canonical `source/`, canonical runtime, `runtime/runs`, Project Vault outputs, or private input directories.
- Private data remains local, read-only, and absent from Git, GitHub, CI, logs, fixtures, identifiers, dates, and derived hashes.
- Use the smallest sufficient implementation and existing architecture; no calibration framework, plugin, service, second test framework, or compatibility fallback.
- No production scientific change: cohorts, endpoints, estimands, models, contrasts, outputs, and interpretations remain unchanged.
- Every behavior change follows red-green-refactor and every final claim uses fresh verification.
- The final pull request remains unmerged until explicit user approval.

---

### Task 1: Make the existing directory runner report and reject warnings, skips, and omissions

**Files:**
- Modify: `scripts/tools/run_testthat.R`
- Modify: `tests/testthat/test_ci_contract.R`

**Interfaces:**
- Consumes: `testthat::test_dir()` result and the requested test directory/filter.
- Produces: `summarize_testthat_result(result, test_dir, filter = NULL)` and `assert_testthat_result(summary, fail_on_warning = TRUE, fail_on_skip = TRUE)`; directory execution exits nonzero for failures, warnings, skips, or discovered-but-unexecuted files.

- [ ] **Step 1: Add failing runner behavior tests**

Add direct subprocess tests to `test_ci_contract.R` that create temporary test directories containing literal test files:

```r
writeLines('test_that("passes", expect_true(TRUE))', file.path(tmp, "test_pass.R"))
writeLines('test_that("warns", { warning("sentinel warning"); succeed() })', file.path(tmp, "test_warn.R"))
writeLines('test_that("skips", skip("sentinel skip"))', file.path(tmp, "test_skip.R"))
```

Run the real `Rscript scripts/tools/run_testthat.R <tmp>` process and assert literal nonzero status plus `sentinel warning` or `sentinel skip`. Add a two-file directory and pass a filter selecting one file; assert the runner reports the unexecuted discovered file when omission checking is enabled.

- [ ] **Step 2: Verify RED**

Run:

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter '^ci_contract$'
```

Expected: the new warning, skip, and omission subprocess cases fail because the runner currently tolerates warnings/skips and has no discovery comparison.

- [ ] **Step 3: Implement literal result summarization**

In `run_testthat.R`, convert the result with `as.data.frame(result)` and count expectation classes/columns observed in testthat 3.2.3. Discover test files with the same filename rule used by testthat (`^test.*\\.[rR]$`). Normalize executed filenames from the result frame. Return a named list containing:

```r
list(
    discovered_files = sort(discovered_files),
    executed_files = sort(executed_files),
    cases = nrow(result_frame),
    failures = sum(result_frame$failed),
    warnings = sum(result_frame$warning),
    skips = sum(result_frame$skipped)
)
```

Use the actual data-frame column names confirmed by `str(as.data.frame(result))`; do not infer counts by grepping console output.

- [ ] **Step 4: Implement fail-closed assertions**

Run `testthat::test_dir()` to completion with `stop_on_failure = FALSE` and `stop_on_warning = FALSE`, then call `assert_testthat_result()`. The assertion must `stop()` with a concise count summary when failures are nonzero, warnings are nonzero, skips are nonzero, or (when no filter is supplied) `setdiff(discovered_files, executed_files)` is nonempty. A targeted filter is allowed for developer runs and checks only files selected by that filter.

- [ ] **Step 5: Verify GREEN and regression behavior**

Run the focused contract and manually run the temporary warning/skip examples through the real runner. Expected: the contract passes, while each mutated directory exits nonzero for the named reason.

- [ ] **Step 6: Commit**

```bash
git add scripts/tools/run_testthat.R tests/testthat/test_ci_contract.R
git commit -m "Make test runner fail closed"
```

### Task 2: Separate portable synthetic integration from private actual-data integration

**Files:**
- Create: `tests/portable/helper-bootstrap.R`
- Move: `tests/integration/test_portable_smoke.R` to `tests/portable/test_portable_smoke.R`
- Modify: `tests/integration/helper-bootstrap.R`
- Create: `tests/testthat/test_integration_routing.R`
- Modify: `README.md`
- Modify: `docs/TECHNICAL.md`

**Interfaces:**
- Consumes: `OCULAR_INTEGRATION_RAW_DATA_DIR`, `OCULAR_INTEGRATION_PROCESSED_DATA_DIR`, and existing project path configuration.
- Produces: hermetic `tests/portable` execution; actual-data bootstrap that reads only explicit inputs and writes only below a temporary output root.

- [ ] **Step 1: Add failing routing tests**

In `test_integration_routing.R`, launch the integration helper in a fresh R process with temporary fake raw/processed input directories and sentinel files. Assert:

```r
expect_identical(normalizePath(RAW_DATA_DIR), normalizePath(fake_raw))
expect_identical(normalizePath(PROCESSED_DATA_DIR), normalizePath(fake_processed))
expect_true(startsWith(normalizePath(OUTPUT_DIR), normalizePath(tempdir_root)))
expect_false(startsWith(normalizePath(OUTPUT_DIR), normalizePath(fake_processed)))
```

Add missing-input subprocess cases and expect nonzero exit with `OCULAR_INTEGRATION_RAW_DATA_DIR` or `OCULAR_INTEGRATION_PROCESSED_DATA_DIR` in the error.

- [ ] **Step 2: Verify RED**

Run the routing test through the focused directory runner. Expected: current bootstrap replaces both sentinels with empty temporary paths and the missing-input invocation skips instead of failing.

- [ ] **Step 3: Create the portable bootstrap and move the smoke test**

The portable helper creates one temporary runtime/export tree, sets all input and output paths inside it, sources `scripts/load_all.R` and `tests/testthat/test_helper_data.R`, and registers teardown cleanup. The moved smoke test retains its current synthetic behavior and contains no actual-data enable/skip call.

- [ ] **Step 4: Repair the actual-data bootstrap**

At process start, require both explicit integration input environment variables. Normalize them with `mustWork = TRUE`, verify the raw workbook and three canonical RDS files exist, then set `RAW_DATA_DIR` and `PROCESSED_DATA_DIR` to those paths. Create only output/log/export directories under a new temporary integration output root. Replace `skip_if_local_data_unavailable()` with a hard prerequisite assertion executed once by the helper.

- [ ] **Step 5: Remove file-scope enable/availability skips**

Remove `skip_if_integration_disabled()` and `skip_if_local_data_unavailable()` calls from actual-data test files. The lane is selected by running `tests/integration`; selection is explicit and prerequisites fail closed.

- [ ] **Step 6: Verify GREEN**

Run:

```bash
Rscript scripts/tools/run_testthat.R tests/testthat --filter '^integration_routing$'
Rscript scripts/tools/run_testthat.R tests/portable
```

Expected: routing contract passes; portable integration passes with zero failures, warnings, or skips; an actual-data invocation without explicit paths exits nonzero.

- [ ] **Step 7: Commit**

```bash
git add tests/portable tests/integration tests/testthat/test_integration_routing.R README.md docs/TECHNICAL.md
git commit -m "Separate portable and private integration lanes"
```

### Task 3: Consolidate deterministic full-pipeline fixtures for Objectives 1–4

**Files:**
- Modify: `tests/testthat/test_helper_data.R`
- Modify: `tests/testthat/helper-peer-review-revision.R`
- Delete: `tests/testthat/helper-data.R`
- Modify: `tests/testthat/test_objective1_primary_outcomes.R`
- Modify: `tests/testthat/test_objective1_age_decade_sensitivity.R`
- Modify: `tests/testthat/test_peer_review_revision_contract.R`
- Modify: `tests/testthat/test_objective2_safety_toxicity.R`
- Modify: `tests/testthat/test_objective3_repeat_radiation.R`
- Modify: `tests/testthat/test_objective4_gep_analysis_portable.R`
- Create: `tests/testthat/helper-objective-fixtures.R`
- Create: `tests/testthat/test_objective_execution_contract.R`

**Interfaces:**
- Consumes: existing `create_test_dataset()`, `create_synthetic_ci_dataset()`, objective orchestrators, and output-directory builders.
- Produces: `get_objective1_pipeline()`, `get_objective2_pipeline()`, `get_objective3_pipeline()`, `get_objective4_pipeline()`, `get_merged_tables_fixture()`, and suite-wide execution counts recorded at the real Objective 1--4 and merge entrypoints.

- [ ] **Step 1: Add failing cache and entrypoint-count contract**

Write `test_objective_execution_contract.R` so calling every `get_objective*_pipeline()` and `get_merged_tables_fixture()` twice returns the identical cached object/path and yields literal counts:

```r
expect_identical(
    objective_execution_counts(),
    c(objective1 = 1L, objective2 = 1L, objective3 = 1L, objective4 = 1L, merged_tables = 1L)
)
```

Also assert there is exactly one `run_objective1_test` function in the loaded helper environment by calling the helper API, not by grepping source. Add a direct second entrypoint-call subprocess mutation and require the suite teardown contract to reject it, proving the counter observes real orchestration rather than only cache getter calls.

- [ ] **Step 2: Verify RED**

Run the focused contract. Expected: cached getters and count API are absent.

- [ ] **Step 3: Implement one simple helper cache**

Create one private environment in `helper-objective-fixtures.R`:

```r
.objective_fixture_state <- new.env(parent = emptyenv())
.objective_fixture_state$results <- list()
.objective_fixture_state$counts <- setNames(integer(5), c("objective1", "objective2", "objective3", "objective4", "merged_tables"))
```

Each getter checks one fixed key, builds the existing output directories beneath `TEST_OUTPUT_DIR`, runs the real orchestrator once, and returns the stored result. Install one narrow test-helper wrapper around each real Objective 1--4 and merged-table entrypoint so every actual orchestration increments the corresponding counter, including calls made outside a getter. Register one suite teardown assertion requiring the literal expected counts. Do not implement general cache keys, arbitrary factories, or source-text policing.

- [ ] **Step 4: Use one balanced deterministic happy-path dataset**

Extend the existing fixture functions rather than adding another generator. Use a fixed seed, at least 80 rows, balanced treatment/sex/location/stage levels, sufficient event counts in each arm, nonnegative follow-up, and deterministic missingness outside the complete happy-path model frame. Retain separate current small fixtures for sparse/non-estimable tests. Update `test_synthetic_fixture_contract.R` with hand-derived range/support assertions.

- [ ] **Step 5: Replace repeated Objective 1 full runs**

Delete the top-level `run_objective1_test()` definition from `test_objective1_primary_outcomes.R`. Replace standard-data calls in Objective 1, age-decade, and peer-review files with `get_objective1_pipeline()`. Remove per-test cleanup of the shared output root. Rewrite modified-data contracts (KM display cap, propensity route, timing perturbations) to call the smallest existing analysis/helper function that owns the behavior; extract one small production predicate only if the routing branch cannot otherwise be exercised directly.

- [ ] **Step 6: Replace repeated Objective 2 runs**

Use `get_objective2_pipeline()` for the canonical artifact contract. Rewrite minimum-follow-up and SRD-scope cases to call `analyze_visual_acuity_changes()` or `analyze_radiation_complications()` directly with their modified data and focused output directories.

- [ ] **Step 7: Keep one Objective 3 and Objective 4 full run**

Move their existing runner setup into the shared helper getters. Tests use the cached results. Lower-level error/edge contracts continue to call only the lower-level function under test.

- [ ] **Step 8: Add one real synthetic merged-table fixture**

Use the deterministic synthetic full/restricted/GKSRS cohorts and the existing merge function once. Assert the expected merged artifact exists and record one counter increment.

- [ ] **Step 9: Verify GREEN and counts**

Run the affected objective files and execution contract in one fresh process. Expected literal counts are one each and no duplicate helper definition changes behavior between filtered and full runs.

- [ ] **Step 10: Commit**

```bash
git add tests/testthat scripts
git commit -m "Consolidate objective test executions"
```

### Task 4: Eliminate unasserted warnings and the obsolete portable skip

**Files:**
- Modify: `tests/testthat/test_helper_data.R`
- Modify: affected objective/table/RMST tests identified by the post-consolidation warning report
- Modify or delete: `tests/testthat/test_peer_review_artifact_verification.R`
- Modify production plotting/model code only when a warning occurs for valid happy-path inputs and the warning identifies an actual implementation defect.

**Interfaces:**
- Consumes: consolidated suite with fail-closed runner.
- Produces: zero top-level warnings and zero skips; intentional sparse/non-estimable conditions asserted inside their focused tests.

- [ ] **Step 1: Run the consolidated full suite and capture exact remaining warning cases**

Run the real directory runner. Record each remaining warning by test file, test case, condition message, and stack origin. Group only identical root causes; do not group by superficial message text.

- [ ] **Step 2: Write or tighten the failing expectation for each intentional condition**

For sparse-factor, one-arm, insufficient-event, or non-estimable scenarios, use `expect_warning(expression, regexp = <literal root message>)` when a warning is the public contract. When the public contract is a structured skip artifact or returned status, change the fixture/call so no warning escapes and assert the artifact/status directly.

- [ ] **Step 3: Remove happy-path fixture warnings**

Adjust only deterministic synthetic support: increase event counts, balance factor combinations, add valid time points, and avoid separated model frames. Re-run the single affected test after each fixture adjustment and require the warning count to fall for the named reason.

- [ ] **Step 4: Correct genuine implementation warnings if any remain**

For duplicate ggplot scale/coordinate warnings or equivalent implementation defects on valid inputs, first add a focused plot-object test that fails on the current warning, then remove the duplicate scale/coordinate addition without changing labels, limits, payloads, or saved dimensions.

- [ ] **Step 5: Remove the obsolete response-document skip**

If `docs/peer_review_revision_response.md` is not a tracked deliverable, delete only that optional test block and retain the independent artifact-freshness helper test. If it is tracked by the time of implementation, replace the skip branch with an unconditional path-safety assertion.

- [ ] **Step 6: Verify GREEN**

Run the entire unit directory with the fail-closed runner. Expected: zero failures, zero warnings, zero skips, all discovered files executed.

- [ ] **Step 7: Commit**

```bash
git add tests scripts
git commit -m "Remove test warning and skip debt"
```

### Task 5: Add the one canonical portable-suite command and require it in CI

**Files:**
- Create: `scripts/tools/run_portable_suite.R`
- Modify: `.github/workflows/portable-tests.yml`
- Modify: `tests/testthat/test_ci_contract.R`
- Modify: `README.md`
- Modify: `CONTRIBUTING.md`
- Modify: `docs/TECHNICAL.md`

**Interfaces:**
- Consumes: fail-closed `run_testthat.R`, `tests/testthat`, `tests/portable`, and lintr.
- Produces: `Rscript scripts/tools/run_portable_suite.R`, the sole required local/CI gate.

- [ ] **Step 1: Add failing canonical-command contract**

Add a subprocess test that runs the new path and currently fails because it is absent. Add behavior assertions against a copied workflow fixture: PR and `master` push triggers exist, exactly one required test command is used, and no `--filter` or manual-only full job can satisfy the gate.

- [ ] **Step 2: Verify RED**

Run `test_ci_contract.R`. Expected: missing portable-suite script and filtered workflow assertions fail.

- [ ] **Step 3: Implement the thin process orchestrator**

`run_portable_suite.R` uses the current Rscript executable and `system2()` to run, in order:

```text
scripts/tools/run_testthat.R tests/testthat
scripts/tools/run_testthat.R tests/portable
lintr::lint_package()
```

It prints one concise stage heading and exits immediately with the child status on failure. It contains no test discovery, result parsing, or duplicate reporting logic beyond delegating to `run_testthat.R`.

- [ ] **Step 4: Simplify GitHub Actions**

Keep setup, locked environment verification, permissions, concurrency, R 4.4.3, and the pinned repository snapshot. Replace filtered/manual jobs with one `required` job that runs `Rscript scripts/tools/run_portable_suite.R` on pull requests and `master` pushes.

- [ ] **Step 5: Update operational documentation**

Document the canonical portable command and the explicit local actual-data command with its two input environment variables. State current behavior directly without migration language.

- [ ] **Step 6: Verify GREEN**

Run the CI contract and then the canonical portable command. Expected: complete unit and portable directories execute with zero failure/warning/skip and lint passes.

- [ ] **Step 7: Commit**

```bash
git add scripts/tools/run_portable_suite.R .github/workflows/portable-tests.yml tests/testthat/test_ci_contract.R README.md CONTRIBUTING.md docs/TECHNICAL.md
git commit -m "Require the complete portable test suite"
```

### Task 6: Validate the private actual-data lane locally and expand only direct scientific contracts

**Files:**
- Modify: existing integration tests only where routing or deterministic assertions require it.
- Modify: existing scientific contract tests only for direct gaps exposed by the stabilized infrastructure.

**Interfaces:**
- Consumes: explicit private raw/processed input directories; writes to `$TMPDIR` only.
- Produces: a zero-skip actual-data run and direct endpoint/cohort/model-artifact contracts without private outputs.

- [ ] **Step 1: Run the actual-data lane with explicit read-only inputs**

Invoke:

```bash
OCULAR_INTEGRATION_RAW_DATA_DIR='<private raw directory>' \
OCULAR_INTEGRATION_PROCESSED_DATA_DIR='<private processed directory>' \
Rscript scripts/tools/run_testthat.R tests/integration
```

Do not redirect output into the repository. Record only aggregate test counts, warning/skip totals, runtime, and sanitized failure reasons.

- [ ] **Step 2: Add direct scientific contracts only where the run exposes a gap**

Permitted additions are literal boundary matrices for nonnegative event times/censoring, cohort row conservation/order invariance, sparse-support returned status, model-frame N reconciliation, and fitted-object versus workbook payload equality. Do not add generic framework code or restate existing tests.

- [ ] **Step 3: Verify focused and complete lanes**

Run every changed contract focused, then rerun the canonical portable suite and actual-data lane. Expected: both are zero-failure; portable is zero-warning/zero-skip; actual-data has zero routing skips and only explicitly asserted scientific conditions.

- [ ] **Step 4: Commit if and only if direct gaps required changes**

```bash
git add tests
git commit -m "Expand direct scientific regression contracts"
```

If no gap is found, make no empty or documentation-only commit.

### Task 7: Prove gate rejection and controlled baseline-versus-candidate improvement

**Files:**
- Create: `docs/maintenance/test-suite-remediation-validation.md`
- No committed mutation files.

**Interfaces:**
- Consumes: clean canonical master and candidate worktree, same R/library/input configuration.
- Produces: sanitized, reproducible before/after evidence and mutation-rejection results.

- [ ] **Step 1: Confirm both trees and record immutable SHAs**

Require canonical `master` clean at `14b27b7` and candidate clean at its final commit. Fetch without modifying canonical files. Record R version, testthat version, renv lock hash, and exact commands; do not record private hashes.

- [ ] **Step 2: Run controlled baseline commands from fresh processes**

Run baseline full unit and enabled integration commands with temporary outputs. Record:

```text
unit: PASS 1548 | FAIL 0 | WARN 958 | SKIP 1 | 284.1 s reference
integration: PASS 10 | FAIL 0 | WARN 0 | SKIP 6 | 1.1 s reference
required CI selection: 9/40 files, 100/263 cases
```

Refresh timings/counts if the controlled rerun differs; the comparison report uses observed values, not copied expectations.

- [ ] **Step 3: Run candidate portable suite twice**

Use two fresh processes and separate `$TMPDIR` roots. Require identical discovered/executed files, case counts, objective/merge counts, and failure/warning/skip totals. Record runtimes separately.

- [ ] **Step 4: Vary execution order**

Run a disposable reversed file-order copy or supported randomized-order invocation. Require materially identical totals and no missing helper/state failures.

- [ ] **Step 5: Run seven disposable gate mutations**

Create seven temporary copies outside the repository. Introduce exactly one literal mutation in each: failing expectation, warning, skip, runner filter that omits a discovered test file, invalid integration input path, direct second Objective 1--4 entrypoint execution, and direct second merged-table execution. Run the canonical command and record nonzero status plus sanitized rejection reason. Delete the temporary copies afterward.

- [ ] **Step 6: Audit sensitive and generated files**

Inspect `git status`, `git diff --stat`, tracked paths, and diff content. Fail if any RDS/XLSX/private path/log/runtime output/identifier/date fixture or generated artifact is present.

- [ ] **Step 7: Write the validation report**

Include a compact table for baseline versus candidate: passes, failures, warnings, skips, discovered/executed files/cases, Objective 1–4 and merge counts, portable integration, actual-data routing, CI job behavior, and runtime. List remaining deficiencies explicitly.

- [ ] **Step 8: Commit validation evidence**

```bash
git add docs/maintenance/test-suite-remediation-validation.md
git commit -m "Document test suite validation evidence"
```

### Task 8: Publish the focused PR and stop at merge approval

**Files:**
- No additional implementation files unless CI exposes a candidate regression.

**Interfaces:**
- Consumes: clean validated candidate branch.
- Produces: one draft pull request with required CI passing; no merge.

- [ ] **Step 1: Run final verification**

Run canonical portable suite, explicit actual-data lane, lint through the canonical suite, `git diff --check`, Git status, sensitive-data audit, and base-drift check.

- [ ] **Step 2: Review the complete diff**

Verify every changed file maps to the spec and no broader repository infrastructure, production science, or unrelated documentation changed.

- [ ] **Step 3: Push and open one draft PR**

Use branch `codex/test-suite-remediation`. The PR body includes what was broken, minimal changes, baseline/candidate table, mutation evidence, exact validation commands, remaining deficiencies, and explicit statement that private data stayed local/read-only.

- [ ] **Step 4: Wait for required CI**

Confirm the required job actually ran `scripts/tools/run_portable_suite.R`, not a filter or skipped job. If CI fails, classify the exact environment/test/lint cause and fix only candidate regressions with TDD.

- [ ] **Step 5: Stop for merge approval**

Report whether the PR is safe to merge and wait. Do not merge or update canonical `master` without explicit user approval.
