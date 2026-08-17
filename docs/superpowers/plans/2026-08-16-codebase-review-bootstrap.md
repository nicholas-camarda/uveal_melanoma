# Codebase Review Bootstrap Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Establish the protected comparison and coverage contracts required before autonomous consolidation review can begin.

**Architecture:** Add a synthetic YAML coverage ledger that records review units and evidence without claiming any unit is complete. Add a declarative YAML contract describing important result artifact classes, then implement one R CLI that compares paired base/candidate runtime roots and emits only sanitized pass/fail reasons. Keep all comparison semantics local to the CLI and exercise them with synthetic artifacts so no private workbook or patient-level data enters the repository.

**Tech Stack:** R 4.4.3 lockfile environment, `yaml`, `jsonlite`, `readxl`, `openxlsx`, `testthat`, shell CLI arguments, and existing `scripts/tools/run_testthat.R`.

## Global Constraints

- Do not change production analysis code, estimands, endpoints, cohorts, censoring, models, estimates, tables, figures, or documentation claims.
- Use one canonical comparator implementation; do not add fallbacks, compatibility shims, or duplicated comparison paths.
- Reports must contain only relative artifact identifiers, comparison status, and sanitized reasons; never emit compared values, workbook contents, identifiers, or identifier-derived hashes.
- Numeric comparison allows absolute difference `1e-12` or relative difference `1e-10` only for undisplayed numeric values; displayed strings are exact.
- Workbook comparison must preserve sheet order, dimensions, formulas, and meaningful cell semantics.

---

### Task 1: Contract tests for the protected bootstrap

**Files:**
- Create: `tests/testthat/test_codebase_review_contract.R`
- Create: `docs/maintenance/codebase_review_coverage.yaml`
- Create: `docs/maintenance/important_results_contract.yaml`
- Create: `scripts/tools/compare_important_results.R`

**Interfaces:**
- `load_codebase_review_coverage(path)` returns a validated list with `version` and ordered `units`.
- `load_important_results_contract(path)` returns a validated list with ordered `comparisons`.
- The CLI accepts `--base-runtime`, `--candidate-runtime`, `--contract`, and `--report`, exits nonzero on any mismatch or malformed input, and writes a sanitized report on every comparison attempt.

- [ ] **Step 1: Write failing contract tests**

  Add tests that require the ledger, contract, and comparator files; validate the ledger has the required review-unit IDs and no completed unit without evidence; validate contract comparison types and relative paths; and run the CLI against synthetic JSON, text, plot-metadata, cohort, and workbook fixtures for exact matches, allowed hidden numeric tolerance, displayed-string mismatch, ordered-cohort mismatch, workbook formula/sheet mismatch, missing artifacts, and sanitized reporting.

- [ ] **Step 2: Run the focused tests and verify the expected red state**

  Run:

  ```sh
  Rscript scripts/tools/run_testthat.R tests/testthat --filter 'codebase_review_contract'
  ```

  Expected: failure because the three protected-lane files do not yet exist.

### Task 2: Implement the minimal declarative contracts and comparator

**Files:**
- Modify: `docs/maintenance/codebase_review_coverage.yaml`
- Modify: `docs/maintenance/important_results_contract.yaml`
- Modify: `scripts/tools/compare_important_results.R`

**Interfaces:**
- Coverage units cover cohort construction, endpoint/censoring, modeling, survival, GEP, tables/figures, and documentation/paths, with initial status `not_reviewed` and no fabricated review evidence.
- Contract entries identify artifact class (`json`, `text`, `cohort`, `plot_metadata`, or `workbook`), relative path, and comparison policy without embedding private values.
- Comparator functions compare recursively while preserving declared order and distinguish displayed strings from hidden numeric values.

- [ ] **Step 1: Add the synthetic ledger and contract YAML**

  Define the ordered review units and the synthetic contract entries used by the tests. Keep all paths relative to a runtime root and all statuses explicit.

- [ ] **Step 2: Implement the comparator CLI**

  Implement strict argument parsing, contract validation, artifact resolution under each runtime root, recursive JSON/text/plot/cohort comparison, semantic workbook comparison through `openxlsx`, tolerance handling for undisplayed numeric values, and sanitized report writing. Return a failing process status whenever any required comparison fails.

- [ ] **Step 3: Run the focused tests and verify green**

  Run the focused contract test command from Task 1. Expected: all contract tests pass with zero failures and zero warnings.

### Task 3: Protected-lane validation and handoff

**Files:**
- Modify: `README.md` only if the new CLI command is otherwise undiscoverable; do not change scientific documentation.

**Interfaces:**
- The existing full suite and portable smoke runner remain the required validation entrypoints.

- [ ] **Step 1: Run focused, portable, full, smoke, lint, and diff checks**

  Run the exact commands documented in the pull-request report, including the focused contract tests, fast portable suite, full data-free suite, synthetic integration smoke, changed-file lint, sensitive-data audit, artifact-routing audit, and final clean-diff check.

- [ ] **Step 2: Confirm paired source-input fingerprints and base drift**

  Re-fingerprint the read-only raw inputs after all candidate validation and confirm the candidate remains based on the reviewed `origin/master` commit.

- [ ] **Step 3: Commit and open one protected-lane pull request**

  Commit only the bootstrap files and any necessary command documentation. The pull request description must state the unit, concrete contract, exact commands, sanitized validation result, residual risks, and that no private-data-backed paired analysis outputs were changed.
