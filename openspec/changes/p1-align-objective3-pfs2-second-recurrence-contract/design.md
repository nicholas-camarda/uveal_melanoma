## Context

Objective 3 is implemented as a PFS-2 analysis of retreated recurrent patients. The current repo has three integrity problems: the endpoint definition in code does not match the docs, low-`n` sparse cases return without explicit skip artifacts, and the fitted Cox output can survive even when the treatment reference arm has zero events and all main contrasts are extreme. On top of that, the derivation logic depends on the order in which raw `Y/N` values are converted to display-friendly factor levels.

## Goals / Non-Goals

**Goals:**
- Pick one PFS-2 endpoint definition and enforce it everywhere.
- Ensure every sparse-data branch leaves explicit review artifacts.
- Prevent non-estimable Cox treatment outputs from looking successful.
- Make PFS-2 derivation robust to raw-versus-display coding order.

**Non-Goals:**
- Expand Objective 3 beyond its current focus on the repeat-radiation/PFS-2 workflow.
- Add new treatment groups or redesign the full repeat-radiation study question.

## Decisions

### Decision: Define Objective 3 PFS-2 as second local recurrence only

Objective 3 PFS-2 will be treated as freedom from second local recurrence after first-recurrence treatment. Death before second recurrence is not part of the primary PFS-2 event definition for this change. The implementation and reader-facing labels must stop implying a recurrence/death composite unless a future change explicitly adds that separate endpoint.

### Decision: Unify sparse-data reporting across all skip branches

Low-patient and low-event branches will both publish stable skip artifacts, so empty folders no longer have to be interpreted by guesswork.

### Decision: Fail closed on non-estimable treatment contrasts

Objective 3 will not present a Cox result as successful when the reference arm has zero events or when treatment contrasts are filtered out as extreme.

### Decision: Normalize recurrence coding before derivation

Derivation will normalize input coding at entry instead of relying on a fragile assumption about preprocessing order.

## Risks / Trade-offs

- [Endpoint alignment may materially change historical PFS-2 event counts] -> Mitigation: preserve a clear migration note and regenerate runtime outputs after the choice is finalized.
- [More skip artifacts may reduce the number of “completed” outputs] -> Mitigation: prioritize interpretive honesty over cosmetic completion.

## Migration Plan

1. Update derivation, reporting labels, and tests to state second-local-recurrence-only PFS-2.
2. Remove or revise documentation that describes Objective 3 PFS-2 as a recurrence/death composite.
3. Add sparse-data and estimability guardrails.
4. Regenerate Objective 3 outputs for all cohorts.

## Open Questions

None. The endpoint decision is second local recurrence only.
