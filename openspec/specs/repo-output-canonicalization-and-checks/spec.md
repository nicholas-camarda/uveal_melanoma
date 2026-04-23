## Purpose
Define the canonical output-root contract for the repo and require lightweight checks that catch stale path or artifact references in docs and contract-facing utilities.

## Requirements

### Requirement: Repo tooling SHALL identify canonical output roots
Repo tooling and documentation MUST identify which output root is canonical and MUST NOT present stale or obsolete output-root examples as active guidance.

#### Scenario: Tool output root is labeled canonical
- **WHEN** the repo documents or exports tool-generated artifacts
- **THEN** it identifies the canonical root
- **AND** it removes stale bad output-root examples from active docs and utility examples

#### Scenario: Workspace audit confirms no move is needed
- **WHEN** Workspace Governor assesses the repo and finds the workspace already compliant
- **THEN** P6 proceeds as a documentation-contract cleanup without introducing workspace-move work

#### Scenario: Public and repo-facing docs avoid maintainer-only path leakage
- **WHEN** repo docs describe runtime, export, or tool output locations
- **THEN** they do not rely on stale or maintainer-only absolute path examples as active guidance
- **AND** contract-facing utilities use the canonical configured roots rather than obsolete examples

### Requirement: Repo SHALL have lightweight checks for contract drift
The repo MUST include checks that catch stale path or artifact references in documentation and contract-facing utilities.

#### Scenario: Drift check catches stale artifact reference
- **WHEN** a doc or utility reintroduces a retired artifact or obsolete path reference
- **THEN** the contract check fails and points to the stale reference

#### Scenario: Drift review uses specialized audits
- **WHEN** repo-level contract cleanup is performed
- **THEN** Documentation Wizard and Workspace Governor non-mutating audits are reviewed before edits
- **AND** any number of narrowly scoped subagents may be used to distribute doc-surface, path-contract, and regression-check review
