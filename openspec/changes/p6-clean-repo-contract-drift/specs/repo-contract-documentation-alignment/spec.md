## ADDED Requirements

### Requirement: Repo documentation SHALL match the current path and artifact contract
Repo documentation and examples MUST reflect the current configured runtime root, export root, and active artifact set.

#### Scenario: Stale path examples are removed or updated
- **WHEN** documentation references an obsolete path convention
- **THEN** the reference is updated to the current configured contract or marked deprecated explicitly

#### Scenario: Retired artifacts are not documented as active outputs
- **WHEN** an artifact is no longer part of the active runtime contract
- **THEN** the docs do not present it as a current expected output
