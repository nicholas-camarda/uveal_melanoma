## Why

Objective 0 is the gatekeeper for every downstream analysis, but the current pipeline can preserve incomplete audit state on non-recreate runs and can downgrade impossible chronology into warnings after analytic times have already been altered. That combination weakens the trustworthiness of every later objective and leaves too much room for silent contract drift.

## What Changes

- Preserve full reconciliation and manual-correction audit content when Objective 0 revalidates existing runtime cohorts instead of recreating them.
- Promote impossible chronology that changes analytic endpoints from a warning-only condition to an explicit failure or published hard-stop artifact.
- Add Objective 0 regression coverage for reload-path behavior, chronology handling, and artifact contract publication.
- Add outward-facing provenance so Objective 0 outputs clearly distinguish rebuilt cohorts from revalidated existing runtime cohorts.

## Capabilities

### New Capabilities
- `objective0-validation-state-preservation`: Objective 0 preserves full audit and provenance state across recreate and non-recreate execution paths.
- `objective0-chronology-failure-enforcement`: Objective 0 blocks or explicitly hard-fails impossible chronology that would otherwise mutate derived analytic times.
- `objective0-contract-regression-testing`: Objective 0 publishes and tests its validation and artifact contracts across rebuild and reload paths, without preserving the retired Objective 4 Training/Testing split as a fatal preprocessing contract.

### Modified Capabilities

None.

## Impact

- Affected workflow: [objective_0_data_processing.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/workflow/objective_0_data_processing.R)
- Affected derivation and validation helpers: [data_derivation.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/data_helper/data_derivation.R), [objective0_validation_engine.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/utils/objective0_validation_engine.R), [validation_reporting.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/utils/validation_reporting.R)
- Affected tests and docs: Objective 0 validation tests, runtime contract docs, and publication-facing validation summaries
