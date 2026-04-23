## Purpose
Define the repo-wide documentation contract so active docs, manuscript-support docs, and examples reflect the current runtime/export path truth and active artifact surface.

## Requirements

### Requirement: Repo documentation SHALL match the current path and artifact contract
Repo documentation and examples MUST reflect the current configured runtime root, export root, and active artifact set across all repository docs, including manuscript-support and GEP-support docs.

#### Scenario: Stale path examples are removed or updated
- **WHEN** documentation references an obsolete path convention
- **THEN** the reference is updated to the current configured contract or removed from active guidance

#### Scenario: Paper-facing docs stay current
- **WHEN** a repository doc is used to support active Objective 1/2 or Objective 4 manuscript work
- **THEN** it is treated as current-facing documentation and updated to the live path and artifact contract

#### Scenario: Retired artifacts are not documented as active outputs
- **WHEN** an artifact is no longer part of the active runtime contract
- **THEN** the docs do not present it as a current expected output

#### Scenario: All docs are scanned for drift
- **WHEN** repo-level contract cleanup runs
- **THEN** all repo docs are scanned for stale path and artifact references
- **AND** fixes prioritize current-facing docs without exempting the rest of the repository from review

### Requirement: Public/current-facing docs SHALL avoid maintainer-specific absolute paths
Public/current-facing docs MUST NOT rely on maintainer-specific absolute paths as active usage guidance.

#### Scenario: Public docs are rewritten to portable guidance
- **WHEN** a public/current-facing doc describes runtime, export, raw-input, or tool-output locations
- **THEN** it uses portable or configured-contract language rather than maintainer-specific absolute paths

#### Scenario: Internal docs may retain operational specifics
- **WHEN** an internal operational doc needs exact local path truth
- **THEN** it may retain that detail while staying aligned with the configured contract
