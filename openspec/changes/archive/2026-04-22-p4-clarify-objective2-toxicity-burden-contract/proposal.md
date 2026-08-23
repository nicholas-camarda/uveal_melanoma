## Why

Objective 2 currently mixes comparative toxicity language with methods that mostly quantify recorded burden by follow-up rather than incidence on equal risk time. Vision and complication outputs are useful, but the contract is not explicit enough about what is actually being estimated.

## What Changes

- Define Objective 2 complication outputs as burden-style follow-up summaries because the current source files do not expose reliable retinopathy, NVG, or SRD onset-date fields.
- Bring the vision-model documentation into line with the actual adjustment set and endpoint definition.
- Add assumption, sparsity, and endpoint-validation guardrails so Objective 0 validates toxicity inputs and Objective 2 outputs fail softly and read honestly.

## Capabilities

### New Capabilities
- `objective2-toxicity-estimand-guardrails`: Objective 2 states and enforces whether toxicity outputs are burden summaries or risk-time analyses.
- `objective2-vision-model-contract-alignment`: Objective 2 vision models and documentation agree on baseline adjustment, follow-up framing, and endpoint wording.
- `objective2-assumption-and-missingness-guardrails`: Objective 2 consumes Objective 0-validated toxicity endpoints and handles sparse support and untested assumptions explicitly in outputs and tests.

### Modified Capabilities

None.

## Impact

- Affected analysis module: `scripts/analysis/vision_safety_analysis.R`
- Affected validation/prep code and methods docs: `scripts/utils/objective0_validation_engine.R`, `scripts/data_helper/data_derivation.R`, `docs/STATISTICAL_METHODS.md`
- Affected Objective 2 runtime narratives, summaries, and tests
