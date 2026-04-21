## 1. Estimand Alignment

- [ ] 1.1 Document Objective 2 complication endpoints as recorded toxicity burden by available follow-up
- [ ] 1.2 Align complication model selection, wording, and summary outputs to the burden-by-follow-up estimand
- [ ] 1.3 Add tests that lock the selected estimand wording and behavior

## 2. Vision Contract Repair

- [ ] 2.1 Either add baseline vision to the Objective 2 vision models or revise the docs and outputs to the implemented change-score contract
- [ ] 2.2 Update endpoint wording so the mixed timing of the current vision outcome is explicit

## 3. Guardrails and Diagnostics

- [ ] 3.1 Implement one consistent missingness policy across Objective 2 complication artifacts
- [ ] 3.2 Guard degenerate vision inference paths so they publish skip artifacts instead of hard-stopping
- [ ] 3.3 Add ordinal assumption-status reporting and regression tests for sparse support and missingness coherence
