## Context

Objective 0 prepares analytic cohorts, validates derivations, and publishes reconciliation artifacts that downstream reviewers treat as source-of-truth. The current implementation has two integrity gaps:

1. Non-recreate runs can republish validation outputs without restoring the full reconciliation audit, especially manual date corrections.
2. Impossible chronology can be clamped to zero in derived analysis times and then only surfaced as a warning, which means execution can succeed after the data have already been altered.

This change is cross-cutting because it affects derivation, validation, reporting, and regression coverage.

## Goals / Non-Goals

**Goals:**
- Preserve full audit state regardless of whether Objective 0 rebuilds or reloads cohorts.
- Prevent silently mutated analytic times from being treated as valid downstream inputs.
- Make Objective 0 outputs self-describing about provenance and validation status.
- Lock the contract with tests so later refactors cannot regress quietly.

**Non-Goals:**
- Re-architect the entire cohort-build pipeline.
- Redesign every validation rule in Objective 0.
- Change cohort inclusion logic or downstream endpoint definitions beyond chronology enforcement.
- Replace the existing Objective 0 validation bundle/reporting system with a new audit framework.

## Implementation Constraint

Extend the existing Objective 0 validation engine, reconciliation audit workbooks, validation bundles, and provenance/reporting helpers. Reload provenance, chronology hard-failure status, and missing audit components should appear in existing validation outputs or their established workbook/narrative surfaces unless a new artifact is explicitly necessary.

## Decisions

### Decision: Treat chronology that alters analytic endpoints as a hard contract violation

Objective 0 will fail closed, or publish an explicit hard-stop artifact, when impossible chronology affects a derived survival or event-time field. This is preferable to clamping to zero and continuing because downstream models cannot distinguish corrected data from impossible source sequences.

Alternative considered:
- Keep warnings and attach more diagnostics. Rejected because it still permits downstream analyses on knowingly altered endpoints.

### Decision: Rehydrate prior audit artifacts before publishing reload-path validation outputs

The reload path will reuse persisted reconciliation detail, manual correction logs, and provenance metadata before writing new validation bundles. This keeps the outward-facing artifacts complete even when raw-data recreation is intentionally skipped.

Alternative considered:
- Document that non-recreate runs have thinner audit outputs. Rejected because it weakens reviewer trust and makes the runtime contract path-dependent.

### Decision: Publish provenance fields in Objective 0 outputs

Validation summaries SHALL explicitly identify whether cohorts were rebuilt from raw input or revalidated from existing runtime artifacts. This keeps reload-path execution honest without forcing costly rebuilds every time.

Alternative considered:
- Keep provenance implicit in logs only. Rejected because logs are not a stable review artifact.

### Decision: Add regression coverage around contract behavior, not just function success

The test suite will assert audit persistence, chronology failure behavior, and reload-path artifact completeness. The historical Objective 4 Training/Testing split is being retired as an active contract, so Objective 0 hardening should not add new fatal split-shape enforcement.

## Risks / Trade-offs

- [Stricter chronology enforcement may fail current cohorts] -> Mitigation: publish explicit failure artifacts with row-level diagnostics so the fix path is clear.
- [Reload-path artifact restoration may require reading older runtime outputs] -> Mitigation: scope the rehydration to known audit artifacts and fail loudly if expected components are missing.
- [Additional tests may be slower] -> Mitigation: keep unit tests small and use synthetic fixtures for edge-case contracts.

## Migration Plan

1. Update derivation and validation helpers to stop treating endpoint-altering chronology as warning-only.
2. Update the reload path to restore full audit content and stamp provenance fields.
3. Regenerate Objective 0 validation artifacts on a representative cohort.
4. Add regression tests for rebuild and reload paths before broader rollout.

## Open Questions

None. This change is intentionally framed as contract hardening only.
