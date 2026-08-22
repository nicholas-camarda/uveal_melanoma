# Contributing

## Git workflow

Use two development lanes. A commit is a save point, not a unit of review: make as many focused commits as useful, then use at most one pull request for the completed logical task.

### Fast lane: direct to `master`

Use a direct push only for clearly non-substantive documentation edits, such as:

- spelling, punctuation, or formatting;
- an unambiguous broken-link repair;
- wording cleanup that does not change a scientific, statistical, operational, or software claim.

Before committing, update local `master` with `git pull --ff-only`. Inspect the complete outgoing diff before pushing. A Markdown-only change is not automatically trivial: treatment descriptions, Methods or Results language, commands, paths, configuration contracts, and figure interpretations use the protected lane.

### Protected lane: one task branch and one pull request

Use a task branch and pull request for:

- code, tests, configuration, or GitHub workflows;
- raw-data handling, cohort or endpoint definitions, and model behavior;
- figures, tables, results, or scientific interpretation;
- paths, commands, runtime or publishing contracts;
- documentation that changes analysis meaning, reproducibility, or claimed software behavior.

Group every commit for the same logical task in one pull request. Merge only after the required CI check passes. When uncertain which lane applies, use the protected lane.

Before opening or merging a protected-lane pull request, run the same complete
portable gate used by CI:

```sh
Rscript scripts/tools/run_portable_suite.R
```

Targeted tests do not replace this complete gate. Record the command, exit
status, and tested Git SHA in the pull-request description or a comment. After
every PR-ready push, watch the required remote check to completion:

```sh
gh pr checks <number> --watch
```

Do not report a pull request as ready, fixed, or passing until the required
GitHub check is green for the current head SHA. If CI fails, retrieve the
failed-job log, reproduce that exact stage locally, fix the root cause, rerun
the complete portable gate, push, and watch the replacement check to green.

### Function documentation

Every new or materially changed function must include a language-appropriate
docstring or roxygen block immediately above its definition. Document the
function's purpose, every argument (including optional/defaulted arguments),
and its return value. Add concise inline comments wherever control flow,
resource routing, warning handling, caching, or other behavior would not be
obvious from the code itself. Keep comments focused on the contract and the
reason for a non-obvious decision; do not restate straightforward syntax.

### Test execution model

The portable testthat suite runs test files serially. Objective 1--4 and
merged-table integration fixtures share one suite-scoped cache so each
expensive full pipeline executes exactly once. The setup lifecycle installs
temporary execution counters and teardown restores the original production
entrypoints. Do not enable test-file parallelism unless the shared fixture
architecture is first replaced with process-safe state and equivalent
execution-count, isolation, and runtime evidence.

This policy is portable across projects, but direct pushes depend on each repository's GitHub protection settings. Do not create custom publishing commands unless repeated friction demonstrates that they are needed.
