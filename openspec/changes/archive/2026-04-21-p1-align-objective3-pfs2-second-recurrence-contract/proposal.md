## Why

Objective 3 is small but high-risk because its single endpoint is currently inconsistent across code, docs, and runtime interpretation. The current implementation also leaves sparse-cohort output folders ambiguous and can publish technically completed Cox outputs that are not substantively estimable.

## What Changes

- Align the PFS-2 endpoint definition across code, docs, and runtime wording.
- Make sparse-support skip behavior explicit for both low-patient and low-event paths.
- Add treatment-support guardrails so non-estimable Cox comparisons are downgraded or skipped instead of looking complete.
- Harden preprocessing assumptions around raw versus display coding for recurrence fields.

## Capabilities

### New Capabilities
- `objective3-pfs2-endpoint-alignment`: Objective 3 uses one documented PFS-2 endpoint definition across derivation, reporting, and tests.
- `objective3-skip-and-estimability-guardrails`: Objective 3 publishes explicit sparse-data artifacts and blocks non-estimable fitted outputs.
- `objective3-derivation-contract-hardening`: Objective 3 derivation no longer depends on brittle raw-versus-display coding order.

### Modified Capabilities

None.

## Impact

- Affected derivation and workflow files: `scripts/data_helper/data_derivation.R`, `scripts/workflow/objective_3_repeat_radiation.R`
- Affected analysis and reporting logic: `scripts/analysis/survival_outcomes.R`
- Affected docs, runtime outputs, and Objective 3 tests
