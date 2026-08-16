# Test Suite Remediation Validation

## Scope and privacy boundary

The portable suite uses deterministic synthetic data and temporary output roots. The local actual-data lane accepts explicit read-only raw and processed input directories and writes only to a temporary output root. Actual data, identifiers, hashes, logs, fixtures, and generated artifacts are excluded from Git and CI.

## Canonical commands

The required local and CI command is:

```sh
Rscript scripts/tools/run_portable_suite.R
```

The optional private local lane is:

```sh
OCULAR_INTEGRATION_RAW_DATA_DIR='<private raw directory>' \
OCULAR_INTEGRATION_PROCESSED_DATA_DIR='<private processed directory>' \
Rscript scripts/tools/run_testthat.R tests/integration
```

## Controlled baseline comparison

Both revisions were run in detached or task-specific worktrees with the same local R environment. Generated output was disposable. The baseline was clean `master` at `14b27b7`; the candidate was the remediation branch.

| Measure | Clean master | Candidate |
|---|---:|---:|
| Unit test files expected/executed | 40/40 locally; 9/40 required in CI | 41/41 locally and required in CI |
| Declared unit test cases | 263 | 271 |
| Unit expectations passed | 1,548 | 1,593 |
| Unit failures | 0 | 0 |
| Unit warnings | 958 | 0 |
| Unit skips | 1 | 0 |
| Unit runtime | 263.9 s | 78.0 s |
| Synthetic integration files | Portable smoke mixed into the private directory | 1/1 dedicated portable file |
| Synthetic integration expectations | 10 | 10 |
| Actual-data integration files executed | 1/7 effectively portable; six files skipped | 6/6 actual-data files |
| Actual-data integration result | 10 pass, 6 skip; private inputs were not reached | 654 pass, 0 fail, 0 warning, 0 skip |
| Objective 1 full executions | 19 | 1 |
| Objective 2 full executions | 3 | 1 |
| Objective 3 full executions | 1 | 1 |
| Objective 4 full executions | 1 | 1 |
| Merged-table full executions | 0 | 1 |

The candidate count contract is evaluated during teardown and requires exactly one execution of each Objective 1-4 entrypoint and the merged-table entrypoint. Focused branch tests use direct component calls without incrementing the full-pipeline counters.

## Fail-closed gate validation

Representative mutations were made only in a disposable detached worktree and were removed afterward.

| Deliberate mutation | Required result |
|---|---|
| Failing assertion | Rejected with exit 1 and `failures=1` |
| Unexpected warning | Rejected with exit 1 and `warnings=1` |
| Unexpected skip | Rejected with exit 1 and `skips=1` |
| Discovered file with no executed case | Rejected with exit 1 and the omitted filename |
| Deleted declared test case | Rejected with exit 1 because the committed 271-case inventory no longer matched |
| Invalid actual-data input routing | Rejected with exit 1 during bootstrap |
| Every Objective 1-4 and merged-table entrypoint executed twice | Rejected with exit 1 and `objective1=2, objective2=2, objective3=2, objective4=2, merged_tables=2` |

## Warning policy

Unexpected R warnings fail the suite. Deterministic sparse-data Cox convergence and chi-square approximation warnings in the read-only actual-data lane are captured by recognized message text and directly asserted; all other warnings propagate to the fail-closed runner. Missing plot-scale levels, absent-column access, duplicate coordinates, test-fixture separation, and optional-output warnings were eliminated at their sources.

## Scientific contracts

The synthetic fixture is deterministic, privacy-safe, balanced across treatment support, and distribution-shaped for age, stage, tumor height, events, censoring, and GEP classes. Direct endpoint assertions cover first-event PFS behavior, negative-time preservation for validation, PFS-2 censoring, exact five-year MFS/MSS boundaries, same-day events, and competing deaths.

## CI contract

GitHub Actions has one required `required` job for pull requests and pushes to `master`. It fails if the restored `renv` environment is not synchronized, then invokes only the canonical portable command, which runs the complete unit suite, dedicated synthetic integration suite, and lint with warnings promoted to errors. Workflow and runner contract tests reject reintroduction of filters, manual-only full coverage, warning/skip tolerance, omitted files or cases, private-data routing, or fragmented entrypoints.

## Remaining boundaries

The private actual-data lane is intentionally local and cannot be a GitHub-required check. Its clean result must be reported separately from portable CI. The suite establishes implementation and selected scientific-behavior contracts; it does not establish external validity or make every fitted model scientifically reportable.
