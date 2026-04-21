## Context

The repo review found several cross-cutting drift points: stale `final_data/...` references, stale `other_map.rds` documentation, an earlier absence of an `analysis_registry.yaml` contract reference, and two tool-output roots where only one is canonical. It also found that Objective 4 still documents a `Training` / `Testing` split as if it were a core active validation contract, even though the current analytic workflow does not materially use that split for the main GEP validation metrics and the decision pass retired it. These are documentation and provenance problems more than methodological ones, but they make the runtime contract harder to trust.

## Goals / Non-Goals

**Goals:**
- Align docs and examples with the current runtime and export layout.
- Remove or revise Objective 4 `Training` / `Testing` split language so it is treated as retired, not as an active validation contract.
- Mark canonical versus legacy tool-output locations explicitly.
- Add checks that catch stale contract references in future edits.

**Non-Goals:**
- Move or delete large runtime trees as part of the spec itself.
- Redesign the overall path architecture for the repo.
- Add heavy automation or a new registry system just to police documentation drift.

## Implementation Constraint

Prefer lightweight doc/test checks that reuse existing doc-alignment coverage, path constants, and runtime/output utilities. Cleanup should clarify current contracts in existing docs and checks rather than introduce a second source of path or artifact truth.

## Decisions

### Decision: Treat code-driven config as the canonical contract and make docs follow it

The repo should have one plainly documented path truth source, with docs and examples anchored to current config rather than older path conventions.

### Decision: Retire stale Objective 4 split language

Because `gep_validation_set` is not driving the main Objective 4 validation results and adds confusion, the repo should stop presenting Training/Testing as a core scientific contract. Remaining code may retain analyzable/non-analyzable eligibility flags, but Training/Testing wording should be removed from active docs and checks.

### Decision: Keep legacy output roots visible only as deprecated context

If a legacy or nested output root still exists on disk, the docs should identify it as non-canonical rather than pretending it is current.

### Decision: Add lightweight drift checks instead of heavy automation

Simple grep-style or doc-alignment tests are enough to stop the same stale references from creeping back in.

## Risks / Trade-offs

- [Docs may need repeated refreshes as the runtime layout evolves] -> Mitigation: keep the checks small and cheap so they are easy to maintain.
- [Canonicalizing output language may expose old artifacts as stale] -> Mitigation: mark them deprecated rather than forcing destructive cleanup.

## Migration Plan

1. Update docs and examples to current path truth.
2. Remove the retired Objective 4 Training/Testing split contract language.
3. Clarify canonical versus legacy tool-output roots.
4. Add drift checks for stale path and artifact references.

## Open Questions

None. This is a cleanup and contract-clarity change.
