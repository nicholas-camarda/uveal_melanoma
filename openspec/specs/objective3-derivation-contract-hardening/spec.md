# objective3-derivation-contract-hardening Specification

## Purpose
Define Objective 3 derivation invariants that keep PFS-2 fields stable across raw and display-coded recurrence inputs.

## Requirements
### Requirement: Objective 3 derivation SHALL be invariant to raw-versus-display recurrence coding
Objective 3 derivation MUST normalize recurrence indicator coding so that valid inputs produce the same derived PFS-2 fields regardless of whether recurrence flags arrive as raw `Y/N` values or display values such as `Yes/No`.

#### Scenario: Raw and display coding yield the same derived PFS-2 values
- **WHEN** Objective 3 derivation is run on equivalent data coded with raw and display recurrence flags
- **THEN** the derived `tt_pfs2_*` and `pfs2_event` fields are identical
