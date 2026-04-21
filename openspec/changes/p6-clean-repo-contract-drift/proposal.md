## Why

Several repo-level contract artifacts have drifted away from reality, including stale path examples, stale artifact references, and duplicate output roots that can confuse reviewers. These are not the most scientifically consequential issues, but they create unnecessary friction and make downstream review less reliable.

## What Changes

- Update repo docs and examples so they reflect the current runtime/export contract and current artifact set.
- Clarify canonical versus legacy output roots for tool-generated artifacts.
- Remove the Objective 4 `Training` / `Testing` split from active repo contract language because it is being retired as analytically non-central.
- Add lightweight contract checks so stale references do not recur quietly.

## Capabilities

### New Capabilities
- `repo-contract-documentation-alignment`: Repo docs and examples reflect the current path, artifact, and output contract.
- `repo-objective4-split-contract-cleanup`: Repo docs and contract checks describe the Objective 4 `Training` / `Testing` split as retired rather than active validation metadata.
- `repo-output-canonicalization-and-checks`: Repo tooling and tests identify canonical output roots and catch stale contract references.

### Modified Capabilities

None.

## Impact

- Affected docs: [README.md](/Users/ncamarda/Projects/uveal_melanoma/README.md), [TECHNICAL.md](/Users/ncamarda/Projects/uveal_melanoma/docs/TECHNICAL.md)
- Affected utilities and checks: [output_utilities.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/utils/output_utilities.R), doc-alignment tests, and any tooling that references canonical output roots
