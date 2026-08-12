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

This policy is portable across projects, but direct pushes depend on each repository's GitHub protection settings. Do not create custom publishing commands unless repeated friction demonstrates that they are needed.
