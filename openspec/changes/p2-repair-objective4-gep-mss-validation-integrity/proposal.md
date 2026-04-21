## Why

Objective 4 contains the richest validation framework in the repo, but several integrity issues remain: MSS manuscript-facing validation is not consistently aligned to competing-risk absolute-risk targets, the historical `Training` / `Testing` split adds confusion without a clear analytic role, and some reader-facing reports overclaim clinical support or contain QC contradictions.

## What Changes

- Align manuscript-facing MSS validation metrics to a competing-risk-consistent estimand.
- Retire the Objective 4 `Training` / `Testing` split from active validation contracts and reader-facing interpretation.
- Add stronger reporting guardrails so sparse follow-up, unsupported extrapolation, and unavailable calibration suppress overconfident clinical language.
- Repair QC/reporting drift in the simple MSS layer, unified comparison outputs, and no-GEP appendix.

## Capabilities

### New Capabilities
- `objective4-mss-competing-risk-alignment`: Objective 4 manuscript-facing MSS validation metrics target a competing-risk-consistent absolute-risk estimand, and the simple MSS summary reuses the same observed-risk logic.
- `objective4-reporting-and-qc-guardrails`: Objective 4 reader-facing outputs enforce narrative, estimand, and QC sanity constraints.
- `objective4-validation-split-retirement`: Objective 4 no longer presents `Training` / `Testing` split metadata as an active validation contract.

### Modified Capabilities

None.

## Impact

- Affected orchestration and cores: [gep_evaluation_core_mss.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/gep/cores/gep_evaluation_core_mss.R), [gep_evaluation_core_mfs.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/gep/cores/gep_evaluation_core_mfs.R)
- Affected reporting layers: [gep_simple_validation.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/gep/reporting/gep_simple_validation.R), [gep_clinical_interpretation.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/gep/reporting/gep_clinical_interpretation.R), [gep_summary_generation.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/gep/reporting/gep_summary_generation.R), [gep_output_consolidation.R](/Users/ncamarda/Projects/uveal_melanoma/scripts/gep/reporting/gep_output_consolidation.R)
- Affected no-GEP appendix outputs and Objective 4 integration tests
