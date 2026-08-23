# Production Protected-Results Comparator Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Use the existing publish artifact registry and comparator to compare a small, declared set of real base/candidate Objective 0–4 outputs without exposing private data or freezing future cohort size.

**Architecture:** Keep `publish_outputs.R` as the source of allowed reader-facing artifacts and keep `compare_important_results.R` as the only comparison engine. Add one production contract listing selected existing artifacts and extend the comparator with `must_equal`, `must_change`, and `may_change` expectations. Do not add a bundle builder, RDS projection layer, or second artifact registry.

**Tech Stack:** Existing R comparator, `yaml`, `jsonlite`, `readxl/openxlsx`, `publish_outputs.R`, testthat.

**Spec:** `docs/superpowers/specs/2026-08-23-production-protected-results-comparator-design.md`

## Global constraints

- Use only artifacts already admitted by `publish_outputs.R`’s allowlist.
- Do not hard-code cohort size, event count, or row count.
- Paired base/candidate runs must use the same current raw workbook and `renv.lock`.
- Keep private runtime files local; reports contain only artifact IDs, statuses, and sanitized reasons.
- Preserve all current synthetic comparator behavior and tests.
- No new bundle builder, projection framework, fallback path, or duplicate registry.

### Task 1: Production contract and tests

**Files:**
- Create: `docs/maintenance/production_results_comparison_contract.yaml`
- Create: `tests/testthat/test_production_results_comparison.R`
- Modify: `tests/testthat/required-test-files.txt`

- [ ] Write RED tests for production contract schema, safe paths, allowed existing artifact paths, expectation values, and no hard-coded counts.
- [ ] Add synthetic base/candidate runtime fixtures using the existing comparator artifact types.
- [ ] Add tests for `must_equal`, `must_change`, `may_change`, missing artifacts, malformed artifacts, and unlisted auxiliary files.
- [ ] Verify RED with `Rscript scripts/tools/run_testthat.R tests/testthat --filter production_results_comparison`.
- [ ] Add a minimal contract selecting existing full-cohort Objective 0, Objective 1, Objective 2, Objective 3, and Objective 4 reader artifacts; do not add a new source registry.
- [ ] Run the focused suite and commit `Add production protected-results contract tests`.

### Task 2: Minimal comparator expectation extension

**Files:**
- Modify: `scripts/tools/compare_important_results.R`
- Modify: `tests/testthat/test_production_results_comparison.R`

- [ ] Add RED assertions that existing synthetic contracts without `expectation` retain `must_equal` behavior.
- [ ] Implement one shared expectation evaluator around the current `compare_one()` result; do not duplicate JSON/text/cohort/workbook comparison code.
- [ ] Add `expectation`, objective, and domain to production report entries while preserving the existing sanitized report shape for synthetic contracts.
- [ ] Return nonzero for required missing artifacts, `must_equal` differences, and `must_change` matches; `may_change` alone does not fail.
- [ ] Run synthetic and production-focused suites, lint touched R files, and commit `Add expected-change protected comparison semantics`.

### Task 3: Real paired runtime comparison

**Files:**
- Create/update: `docs/validation/production-protected-results-comparator.md`
- Generated only: `runtime/runs/production-protected-results-base/`
- Generated only: `runtime/runs/production-protected-results-candidate/`

- [ ] Run complete Objectives 0–4 in clean isolated base and candidate roots with identical raw and lock fingerprints.
- [ ] Confirm every contract path is produced by the existing publish allowlist; do not copy or reshape files.
- [ ] Run `compare_important_results.R` with the production contract and record which declared artifacts match, change as expected, or fail.
- [ ] Run the AAO gate separately against the candidate workbook; comparator status does not determine scientific presentation clearance.
- [ ] Record only aggregate statuses and hashes of non-sensitive reports in the validation document; keep private runtime files out of Git.
- [ ] Run `Rscript scripts/tools/run_portable_suite.R` at the exact comparator head and commit the PHI-free validation record.

### Task 4: AAO handoff

- [ ] Keep the AAO interpretation endpoint-specific: modest MFS discrimination, weak/uncertain MSS discrimination, no molecular relabeling, and descriptive sparse subgroup ordering only.
- [ ] Refresh PowerPoint and presenter guide only after written investigator clearance of the revised interpretation.
- [ ] Keep the accepted abstract unchanged as the historical submission; label corrected presentation values as updated analysis where appropriate.
