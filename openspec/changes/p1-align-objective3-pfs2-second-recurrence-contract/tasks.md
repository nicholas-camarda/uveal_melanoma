## 1. Endpoint Alignment

- [ ] 1.1 Document the canonical Objective 3 PFS-2 endpoint as second local recurrence only
- [ ] 1.2 Update Objective 3 derivation, labels, and docs to remove recurrence/death-composite wording
- [ ] 1.3 Add regression tests for death-before-second-recurrence behavior

## 2. Sparse-Data and Estimability Guardrails

- [ ] 2.1 Make the low-patient branch publish the same skip artifact classes as the low-event branch
- [ ] 2.2 Add guardrails that downgrade or skip non-estimable Cox treatment outputs

## 3. Derivation Hardening

- [ ] 3.1 Normalize recurrence coding before Objective 3 derivation
- [ ] 3.2 Add tests proving raw-versus-display coding invariance for PFS-2 fields
- [ ] 3.3 Regenerate and review Objective 3 outputs across all cohorts after the contract changes
