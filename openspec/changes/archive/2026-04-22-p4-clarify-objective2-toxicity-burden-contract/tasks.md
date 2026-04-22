## 1. Estimand Alignment

- [x] 1.1 Document Objective 2 complication endpoints as recorded toxicity burden by available follow-up
- [x] 1.2 Align complication model selection, wording, and summary outputs to the burden-by-follow-up estimand
- [x] 1.3 Add tests that lock the selected estimand wording and behavior

## 2. Vision Contract Repair

- [x] 2.1 Revise docs and outputs to the implemented Objective 2 change-score contract without adding baseline vision to the P4 models
- [x] 2.2 Update endpoint wording so the mixed timing of the current vision outcome is explicit

## 3. Guardrails and Diagnostics

- [x] 3.1 Add Objective 0 validation/prep coverage for retinopathy, NVG, and SRD endpoint fields in included analytic rows
- [x] 3.2 Remove Objective 2 analysis-local raw-value recoding and consume only Objective 0-validated/prepared toxicity burden fields
- [x] 3.3 Guard degenerate vision inference paths so they publish skip artifacts instead of hard-stopping
- [x] 3.4 Add locally seeded simulated Fisher p-values for Objective 2 categorical descriptive summaries and tests proving rerun stability
- [x] 3.5 Add not-formally-tested ordinal assumption-status reporting and regression tests for Objective 0 toxicity endpoint validation, absence of Objective 2 local recoding, sparse support, and denominator coherence
