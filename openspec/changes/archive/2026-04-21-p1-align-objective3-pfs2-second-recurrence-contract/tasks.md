## 1. Endpoint Alignment

- [x] 1.1 Document the canonical Objective 3 PFS-2 endpoint as second local recurrence only
- [x] 1.2 Update Objective 3 derivation, labels, and docs to remove recurrence/death-composite wording
- [x] 1.3 Add regression tests for death-before-second-recurrence behavior

## 2. Sparse-Data and Estimability Guardrails

- [x] 2.1 Make the low-patient branch publish the same skip artifact classes as the low-event branch
- [x] 2.2 Add guardrails that downgrade or skip non-estimable Cox treatment outputs
- [x] 2.3 Add censoring-support diagnostics and interpretation downgrades for heavy or imbalanced censoring

## 3. Derivation Hardening

- [x] 3.1 Normalize recurrence coding before Objective 3 derivation
- [x] 3.2 Add tests proving raw-versus-display coding invariance for PFS-2 fields
- [x] 3.3 Regenerate and review Objective 3 outputs across all cohorts after the contract changes
