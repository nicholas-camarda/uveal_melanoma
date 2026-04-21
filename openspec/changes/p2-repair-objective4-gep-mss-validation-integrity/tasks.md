## 1. Validation Split Retirement

- [ ] 1.1 Remove or demote Objective 4 `Training` / `Testing` split language from active validation contracts
- [ ] 1.2 Update Objective 4 and Objective 0 tests/docs so `gep_validation_set` is not treated as a required Training/Testing partition
- [ ] 1.3 Preserve only the eligibility metadata needed for analyzable versus non-analyzable GEP rows

## 2. MSS Competing-Risk Alignment

- [ ] 2.1 Implement a competing-risk-consistent primary MSS validation lane for calibration and discrimination
- [ ] 2.2 Demote or relabel non-primary MSS metrics that remain technical sidecars
- [ ] 2.3 Update tests to lock the primary MSS estimand contract

## 3. Simple MSS Alignment

- [ ] 3.1 Refactor the simple MSS summary so it reuses the same corrected observed-risk logic as the primary MSS path
- [ ] 3.2 If any residual simple MSS output remains methodologically lighter-weight, label it explicitly as QC-only and exclude it from primary interpretation

## 4. Reporting and QC Guardrails

- [ ] 4.1 Add narrative guardrails tied to follow-up support, extrapolation support, and calibration availability while keeping 7-year and 10-year results in the main output set
- [ ] 4.2 Add interval-sanity assertions for no-GEP outputs and estimand metadata for unified cross-outcome comparisons
- [ ] 4.3 Expand Objective 4 tests from artifact existence to wording, method, and QC integrity
