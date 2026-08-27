# Figure Counts Audit

This file is generated from canonical runtime artifacts and centralized config constants. It is a current-state cohort and figure-count audit; it does **not** validate the rendered flowchart image itself.

## Canonical sources

- `runtime/Analytic Dataset/cohort_summary_statistics.json`
- `runtime/Analysis/uveal_full/00_General/removed_patients_summary.tsv`
- `scripts/utils/cohort_summary_export.R`
- `scripts/utils/config_constants.R`

## Current exclusion summary

- Total exclusions: **4**
- Stage IV exclusions: **3**
- Manual exclusions: **1**

Row-level exclusion details remain in the private runtime audit workbook and are not included in this public document.

## Current cohort counts used for figure-facing summaries

### Full Cohort

Overall cohort N = **260**

| Treatment | N | Local recurrence | Metastasis | Alive | Lost to follow-up | Dead |
|---|---:|---:|---:|---:|---:|---:|
| PBT | 121 | 16 | 23 | 72 | 16 | 33 |
| GKSRS | 139 | 17 | 21 | 90 | 25 | 24 |

### Restricted Cohort

Overall cohort N = **167**

| Treatment | N | Local recurrence | Metastasis | Alive | Lost to follow-up | Dead |
|---|---:|---:|---:|---:|---:|---:|
| PBT | 103 | 11 | 19 | 64 | 12 | 27 |
| GKSRS |  64 |  8 |  9 | 40 | 12 | 12 |

## Vital-status rule definition

- Data cutoff date: `2025-03-04`
- Lost-to-follow-up threshold: `450` days
- `dead` if `death_event == 1`
- otherwise `alive` if `days_since_last_contact <= LOST_TO_FOLLOWUP_CUTOFF_DAYS`
- otherwise `lost_to_followup`

## Interpretation

- The counts above are the current runtime source of truth for figure-facing cohort summaries.
- If the rendered figure disagrees with this file, the figure should be updated; this file is intentionally artifact-first rather than image-first.
- GKSRS-only cohort counts are still exported elsewhere, but they are not the primary target of the current figure-count audit.
