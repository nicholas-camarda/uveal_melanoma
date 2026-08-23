# Production Protected-Results Comparator Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Add a production-only, growth-aware bundle builder and comparator contract so real base/candidate Objective 0–4 runs verify intended changes without freezing cohort size or exposing private data.

**Architecture:** Keep the current synthetic comparator contract intact. Add a separate production contract and one bundle-builder CLI that extracts only declared semantic artifacts beneath an isolated runtime. Extend the existing comparator with one comparison engine and explicit `must_equal`, `must_change`, and `may_change` expectations; reports remain aggregate and PHI-free.

**Tech Stack:** R 4.4.3, yaml, jsonlite, readxl/openxlsx, testthat, existing comparator CLI, isolated runtime roots.

**Spec:** `docs/superpowers/specs/2026-08-23-production-protected-results-comparator-design.md`

## Global constraints

- Never place raw workbook contents, patient identifiers, or identifier-derived hashes in Git or sanitized reports.
- Do not compare every runtime file; protect only contract-declared artifacts.
- Do not hard-code current cohort size or event counts.
- Allow candidate cohort growth through `candidate_superset`; fail on shared-row changes or removals.
- Missing or ambiguous declared artifacts fail closed; no nearby-file or stale-runtime fallback.
- Keep synthetic comparator behavior and tests passing.
- Run complete Objectives 0–4 for the current production contract; do not treat a partial 0/4 run as complete.

### Task 1: Production contract and synthetic fixtures

**Files:**
- Create: `docs/maintenance/production_results_comparison_contract.yaml`
- Create: `tests/testthat/test_production_results_comparison.R`
- Modify: `tests/testthat/required-test-files.txt`

**Interfaces:**
- Contract entries declare `id`, `objective`, `domain`, `source`, `extraction`, `output`, `type`, `expectation`, and `required`.
- Supported expectations are `must_equal`, `must_change`, and `may_change`.
- `candidate_superset` is the only growth policy for row-bearing projections.

- [ ] Write RED tests for contract schema, safe paths, candidate growth, removals, changed shared rows, missing source, and unsupported extraction.
- [ ] Add synthetic runtime fixtures containing one JSON projection, one cohort projection, one text artifact, and one workbook projection.
- [ ] Add exact tests that permit added candidate IDs but reject removed IDs and changed shared values.
- [ ] Verify RED with `Rscript scripts/tools/run_testthat.R tests/testthat --filter production_results_comparison`.
- [ ] Implement contract parsing and fixture helpers without embedding private values.
- [ ] Run the focused suite and commit `Add production comparison contract tests`.

### Task 2: Production bundle builder

**Files:**
- Create: `scripts/tools/build_production_comparison_bundle.R`
- Modify: `tests/testthat/test_production_results_comparison.R`

**Interfaces:**
- CLI: `Rscript scripts/tools/build_production_comparison_bundle.R --runtime-root ROOT --contract CONTRACT --output-root OUTPUT --report REPORT`.
- Projection modes: `copy_json`, `copy_text`, `copy_workbook`, and `project_rds` with explicit column lists and stable ID.
- Builder report contains only artifact IDs, extraction status, and sanitized reason codes.

- [ ] Add RED tests for all projection modes, typed missingness, stable-ID sorting, duplicate IDs, missing declared columns, and output path traversal.
- [ ] Implement strict contract validation and source resolution beneath the runtime root.
- [ ] Implement `project_rds` with explicit columns, stable-ID sorting, required unique IDs, and candidate-growth-compatible output.
- [ ] Implement deterministic copies for JSON, text, and workbooks; reject ambiguous or missing sources.
- [ ] Add PHI-pattern tests to reports and verify unlisted auxiliary files are not copied.
- [ ] Run focused tests, lint the new CLI, and commit `Add production comparison bundle builder`.

### Task 3: Growth-aware comparator expectations

**Files:**
- Modify: `scripts/tools/compare_important_results.R`
- Modify: `tests/testthat/test_codebase_review_contract.R`
- Modify: `tests/testthat/test_production_results_comparison.R`

**Interfaces:**
- Existing contracts without `expectation` default to `must_equal`.
- Production reports add objective, domain, expectation, and status fields while retaining sanitized output.
- Exit status is nonzero for missing required artifacts, `must_equal` differences, or `must_change` matches; `may_change` never fails by itself.

- [ ] Add RED tests for `must_equal`, `must_change`, and `may_change`, including exact-threshold behavior and missing production artifacts.
- [ ] Add RED tests for candidate-superset cohort comparison and unlisted auxiliary files.
- [ ] Extend the single comparison engine with expectation validation; do not duplicate type-specific comparison implementations.
- [ ] Preserve all synthetic comparator tests and sanitized-report rules.
- [ ] Run both comparator suites, lint, and `git diff --check`; commit `Add growth-aware protected comparison expectations`.

### Task 4: Separate comparator branch and actual-data verification

**Files:**
- Create/update: `docs/validation/production-protected-results-comparator.md`
- Generated only: `runtime/runs/production-protected-results-base/`
- Generated only: `runtime/runs/production-protected-results-candidate/`

- [ ] Create a separate branch/worktree from the reviewed base and keep the comparator changes isolated from the AAO presentation PR.
- [ ] Run complete Objective 0–4 base and candidate workflows with identical current raw and lock fingerprints.
- [ ] Build both production bundles with the same contract and record only aggregate manifests locally.
- [ ] Run the protected comparator and record which required artifacts match, change as expected, or fail.
- [ ] Run the AAO gate separately against the candidate workbook; do not treat comparator status as scientific clearance.
- [ ] Commit the PHI-free validation record and run the full portable suite at the exact head.

### Task 5: Handoff to AAO materials

- [ ] Keep the AAO branch’s scientific conclusion explicit: modest MFS discrimination, weak/uncertain MSS discrimination, no molecular relabeling, and descriptive sparse subgroup ordering only.
- [ ] Refresh the PowerPoint and presenter guide only after written investigator clearance of the revised interpretation.
- [ ] Keep the accepted abstract unchanged as the historical submission; label corrected presentation values as updated analysis where appropriate.

