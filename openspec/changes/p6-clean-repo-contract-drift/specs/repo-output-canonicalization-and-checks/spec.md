## ADDED Requirements

### Requirement: Repo tooling SHALL identify canonical output roots
Repo tooling and documentation MUST identify which output root is canonical when legacy or duplicate output locations also exist.

#### Scenario: Tool output root is labeled canonical
- **WHEN** the repo documents or exports tool-generated artifacts
- **THEN** it identifies the canonical root and, if needed, labels any secondary root as legacy or non-canonical

### Requirement: Repo SHALL have lightweight checks for contract drift
The repo MUST include checks that catch stale path or artifact references in documentation and contract-facing utilities.

#### Scenario: Drift check catches stale artifact reference
- **WHEN** a doc or utility reintroduces a retired artifact or obsolete path reference
- **THEN** the contract check fails and points to the stale reference
