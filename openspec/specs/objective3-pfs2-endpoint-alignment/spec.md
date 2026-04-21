# objective3-pfs2-endpoint-alignment Specification

## Purpose
Define the active Objective 3 PFS-2 endpoint contract as time from first-recurrence treatment to second local recurrence, with death before second local recurrence treated as censoring for the primary endpoint.

## Requirements
### Requirement: Objective 3 SHALL use second-local-recurrence-only PFS-2 across code and docs
Objective 3 derivation, reporting, and documentation MUST define primary PFS-2 as time from first-recurrence treatment to second local recurrence. Death before second local recurrence MUST be treated as censoring for this primary endpoint, not as a PFS-2 event.

#### Scenario: Derived event coding matches documented endpoint
- **WHEN** Objective 3 derives `pfs2_event`
- **THEN** the event is coded only for second local recurrence
- **AND** death before second local recurrence is not coded as a primary PFS-2 event

#### Scenario: Exported wording matches runtime derivation
- **WHEN** Objective 3 writes plots, effect summaries, diagnostics, or narrative text
- **THEN** the wording of those artifacts matches the actual endpoint used in derivation

### Requirement: Objective 3 SHALL test death-before-second-recurrence handling
Objective 3 MUST include regression coverage for a retreated patient who dies before second recurrence.

#### Scenario: Death-before-second-recurrence fixture locks endpoint contract
- **WHEN** the Objective 3 test suite evaluates a patient who dies before `recurrence2`
- **THEN** the asserted event status remains non-event/censored for the primary PFS-2 endpoint
- **AND** exported wording does not describe the primary endpoint as a recurrence/death composite
