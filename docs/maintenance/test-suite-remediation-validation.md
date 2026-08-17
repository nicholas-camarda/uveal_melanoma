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

Both revisions were run in detached or task-specific worktrees with the same
local R environment on 2026-08-17. Generated output was disposable. The
baseline was clean `master` at `a5ace62`; the candidate was the remediation
branch based on `e34d9be` plus the review fixes recorded here.

| Measure | Clean master | Candidate |
|---|---:|---:|
| Unit test files expected/executed | 41/41 locally; 9/41 required in CI | 44/44 locally and required in CI |
| Test inventory contract | Brittle source declaration count | Checked-in file manifest plus dynamic testthat execution |
| Dynamically executed unit cases | Not reported by the master runner; 268 source declarations | 285 |
| Unit expectations passed | 1,582 | 1,682 |
| Unit failures | 0 | 0 |
| Unit warnings | 958 | 0 |
| Unit skips | 0 | 0 |
| Unit runtime | 287.7 s | 96.0 s final normal; 89.8 s reversed; 88.6 s seeded-random |
| Synthetic integration files | Portable smoke mixed into the private directory | 1/1 dedicated portable file |
| Synthetic integration expectations | 10 | 10 |
| Actual-data integration files executed | 1/7 effectively portable; six files skipped | 6/6 actual-data files |
| Actual-data integration cases | Private inputs were not reached | 63 |
| Actual-data integration result | 10 pass, 6 skip; private inputs were not reached | 654 pass, 0 fail, 0 warning, 0 skip in 77.4 s |
| Objective 1 full executions | 19 | 1 |
| Objective 2 full executions | 3 | 1 |
| Objective 3 full executions | 1 | 1 |
| Objective 4 full executions | 1 | 1 |
| Merged-table full executions | 0 | 1 |

The candidate count contract is evaluated during teardown and requires exactly
one execution of each Objective 1-4 entrypoint and the merged-table entrypoint.
The suite is explicitly serial because those five expensive integrations share
one suite-scoped cache. Wrappers preserve and restore the exact production
function objects, cached results and temporary roots are cleared even after a
failed count assertion, and two same-process testthat runs prove that setup is
fresh and path/global state is restored. Normal, reversed, and seeded-random
file orders produced identical results.

The unit, synthetic, and actual-data directories each have a checked-in file
manifest. The runner rejects missing, unexpected, or unexecuted files in every
canonical lane, including the private Objective 0/4 integration inventory.

## Testing-surface accounting

| Surface | Portable evidence | Private actual-data evidence |
|---|---|---|
| Objective 0 | Derivation, endpoint boundary, factor, validation-engine, and cohort-criteria contracts | Raw-data loading/cleaning, derived variables, factor preparation, and three cohort outputs |
| Objective 1 | Primary outcomes, age-decade and subgroup policies, propensity sensitivity, and one cached full execution | Preprocessed variables and downstream consistency checks |
| Objective 2 | Safety/toxicity component contracts and one cached full execution | Shared preprocessed-variable and logging regressions |
| Objective 3 | Repeat-radiation contracts, scope protection, and one cached full execution | Shared preprocessed-variable and logging regressions |
| Objective 4 | Portable GEP/MFS contracts, scope protection, and one cached full execution | Cached actual-data GEP pipeline plus component, reporting, and boundary checks |
| Cross-objective outputs | Merged tables execute once; publication, provenance, workbook, figure, documentation, and runtime-path contracts | Six integration files run against explicitly routed read-only inputs and temporary outputs |
| End-to-end | Dedicated deterministic synthetic smoke test | Objective 0 and Objective 4 actual-data integration paths |

## Fail-closed gate validation

Representative mutations were made only in a disposable detached worktree and were removed afterward.

| Deliberate mutation | Required result |
|---|---|
| Failing assertion | Rejected with exit 1 and `failures=1` |
| Unexpected helper warning | Rejected with non-zero exit and the helper sentinel |
| Unexpected setup warning | Rejected with non-zero exit and the setup sentinel |
| Unexpected top-level test-file warning | Rejected with non-zero exit and the top-level sentinel |
| Unexpected test-body warning | Rejected with non-zero exit and the test-body sentinel |
| Unexpected teardown warning | Rejected with non-zero exit and the teardown sentinel |
| Warning asserted by `expect_warning()` | Accepted with exit 0 |
| Unexpected skip | Rejected with exit 1 and `skips=1` |
| Manifest-listed file missing from disk | Rejected with the missing filename |
| Unlisted test file on disk | Rejected with the unexpected filename |
| Actual-data integration manifest drift | Rejected before a partial private-lane result can be accepted |
| Generated/parameterized `test_that()` cases | Accepted and counted dynamically |
| Invalid actual-data input routing | Rejected with exit 1 during bootstrap |
| Every Objective 1-4 and merged-table entrypoint executed twice | Rejected with exit 1 and `objective1=2, objective2=2, objective3=2, objective4=2, merged_tables=2` |

## Warning policy

Unexpected R warnings fail the suite whether they arise while sourcing helpers,
running setup, evaluating a top-level test file, executing a test body, or
running teardown. Expected warnings asserted by testthat remain valid.
Deterministic sparse-data Cox convergence and chi-square approximation warnings
in the read-only actual-data lane are captured by recognized message text and
directly asserted; all other warnings propagate to the fail-closed runner.
Missing plot-scale levels, absent-column access, duplicate coordinates,
test-fixture separation, and optional-output warnings were eliminated at their
sources.

## Function documentation contract

Every named function introduced or materially changed by PR 18, PR 19, or this
review remediation now has an immediately attached purpose block, documentation
for every parameter, a return-value contract, and focused comments for
non-obvious behavior. The accounting is 24/24 original PR 18 functions, 20/20
original PR 19 functions, and 23/23 functions added during remediation: 67/67
overall across the comparator, runners, Objective 1 workflow, fixtures,
lifecycle helpers, and contract-test subprocess helpers. `CONTRIBUTING.md`
records the same rule for future changes.

## Protected workbook comparison

The protected-results comparator now lives with other executable utilities at
`scripts/tools/compare_important_results.R`. Workbook comparison resolves cell
style references through OOXML `cellXfs` and compares effective built-in or
custom number-format codes rather than raw style IDs. Equivalent style-table
representations pass; formula, value, displayed number format, sheet,
dimension, and cell-reference changes fail. Comparator tests and temporary
archive extraction clean up on both success and error.

## Scientific contracts

The synthetic fixture is deterministic, privacy-safe, balanced across treatment support, and distribution-shaped for age, stage, tumor height, events, censoring, and GEP classes. Direct endpoint assertions cover first-event PFS behavior, negative-time preservation for validation, PFS-2 censoring, exact five-year MFS/MSS boundaries, same-day events, and competing deaths.

## CI contract

GitHub Actions has one required `required` job for pull requests and pushes to
`master`. It fails if any locked package is missing or differs in version,
source, or recorded revision, then invokes only the canonical portable command,
which runs the complete unit suite, dedicated synthetic integration suite, and
lint with warnings promoted to errors. Workflow and runner contract tests
reject reintroduction of filters, manual-only full coverage, warning/skip
tolerance, required-file inventory drift, private-data routing, or fragmented
entrypoints. Static AST case counting was removed because it cannot correctly
model generated or parameterized testthat cases.

## Remaining boundaries

The private actual-data lane is intentionally local and cannot be a GitHub-required check. Its clean result must be reported separately from portable CI. The suite establishes implementation and selected scientific-behavior contracts; it does not establish external validity or make every fitted model scientifically reportable.
