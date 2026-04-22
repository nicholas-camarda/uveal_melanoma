## 1. Event-Time Estimand Repair

- [x] 1.1 Document Objective 1a/1b binary and competing-risk cumulative incidence as co-primary estimands
- [x] 1.2 Preserve binary recurrence/metastatic progression outputs and add co-primary cumulative-incidence implementations with matching report outputs
- [x] 1.3 Update tests to lock co-primary output labeling, death handling, and no-sole-logistic-primary behavior

## 2. Cohort and Interpretation Guardrails

- [x] 2.1 Add one centralized Objective 1 cohort-interpretation note to high-level reader-facing summaries without duplicating boilerplate across low-level artifacts
- [x] 2.2 Add graded PH interpretation for Objective 1 Cox-based survival summaries, using existing survival/PH/RMST/effect-summary artifacts and RMST/KM-first language only for material PH violations when supported
- [x] 2.3 Add artifact-level notes to legacy post-baseline OS/PFS folders

## 3. Subgroup Contract Alignment

- [x] 3.1 Document the subgroup runtime contract as consolidated multi-sheet Excel diagnostics workbooks, forest plots, and interaction RDS outputs, with per-subgroup HTML files treated only as ancillary previews if retained
- [x] 3.2 Align subgroup docs and tests to the stable runtime contract without adding a parallel subgroup reporting path or requiring per-subgroup workbook/HTML artifact proliferation
- [x] 3.3 Add explicit exploratory labeling for sparse-support subgroup surfaces
