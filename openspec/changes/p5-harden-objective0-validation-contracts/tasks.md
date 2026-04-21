## 1. Validation State Preservation

- [ ] 1.1 Update the non-recreate Objective 0 path to restore persisted reconciliation detail, manual correction detail, and provenance fields before writing validation outputs
- [ ] 1.2 Add explicit provenance fields or narrative text that distinguish rebuilt cohorts from revalidated runtime cohorts

## 2. Chronology Enforcement

- [ ] 2.1 Refactor derived-time chronology handling so endpoint-altering impossible intervals no longer clamp silently to zero
- [ ] 2.2 Publish explicit hard-stop diagnostics for chronology failures with row-level and field-level detail

## 3. Contract Regression Coverage

- [ ] 3.1 Add Objective 0 tests covering recreate versus non-recreate artifact parity
- [ ] 3.2 Add tests covering chronology hard-failure behavior without reintroducing GEP Training/Testing split-shape fatal enforcement
- [ ] 3.3 Update Objective 0 documentation to match the hardened validation and provenance contract
