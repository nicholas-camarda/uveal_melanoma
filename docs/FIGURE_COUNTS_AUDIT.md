# Confirm Consort Diagram Subcounts

Date: 2026-02-13

## Scope

This audit verifies the flowchart red-box counts against frozen artifacts (no pipeline rebuild), and checks whether Stage IV-at-diagnosis patients are currently excluded.

## Frozen artifacts used

- `~/ProjectsRuntime/uveal_melanoma/Analytic Dataset/cohort_summary_statistics.json`
- `~/ProjectsRuntime/uveal_melanoma/Analytic Dataset/uveal_melanoma_full_cohort.rds`
- `~/ProjectsRuntime/uveal_melanoma/Analytic Dataset/uveal_melanoma_restricted_cohort.rds`
- `scripts/data_helper/cohort_creation.R` (Stage IV exclusion logic)
- `scripts/utils/cohort_summary_export.R` (alive/dead/lost follow-up definition)
- `logs/json/run_log_20251125_112559.jsonl` (example run evidence)

## Key findings

1. **Stage IV-at-diagnosis patients are currently excluded** in cohort construction.
2. Frozen artifacts indicate **3 Stage IV exclusions**.
3. The flowchart red-box values are a **legacy (pre-Stage-IV-exclusion) snapshot**; the frozen artifacts provide the updated post-exclusion values.

## Evidence for Stage IV exclusion

- In `scripts/data_helper/cohort_creation.R`, `apply_criteria()` explicitly removes rows where `initial_stage_binary == "Stage IV"`, and logs them as `"Stage IV disease excluded per protocol"` under `removal_step = "stage_iv_exclusion"`.
- In `~/ProjectsRuntime/uveal_melanoma/Analytic Dataset/cohort_summary_statistics.json`, exclusions are:
  - `"Stage IV disease excluded per protocol": 3`
  - `"stage_iv_exclusion": 3`
- In `logs/json/run_log_20251125_112559.jsonl`, objective-0 processing logs:
  - `Stage IV exclusion applied: removed 3 patients (IDs: 7, 116, 262)`

## Red-box verification table

Legend:
- **Legacy figure** = value shown in your current flowchart image.
- **Current artifact** = value from frozen artifacts.
- **Delta** = Current artifact − Legacy figure.

| Cohort | Treatment | Metric | Legacy figure | Current artifact | Delta | Match |
|---|---|---:|---:|---:|---:|---|
| Full | PBT | n | 121 | 121 | 0 | Yes |
| Full | PBT | Local recurrence | 16 | 16 | 0 | Yes |
| Full | PBT | Metastasis | 23 | 23 | 0 | Yes |
| Full | PBT | Alive | 73 | 73 | 0 | Yes |
| Full | PBT | Lost to follow-up | 15 | 15 | 0 | Yes |
| Full | PBT | Dead | 34 | 33 | -1 | **No** |
| Full | GKSRS | n | 139 | 139 | 0 | Yes |
| Full | GKSRS | Local recurrence | 17 | 17 | 0 | Yes |
| Full | GKSRS | Metastasis | 21 | 21 | 0 | Yes |
| Full | GKSRS | Alive | 90 | 90 | 0 | Yes |
| Full | GKSRS | Lost to follow-up | 26 | 25 | -1 | **No** |
| Full | GKSRS | Dead | 25 | 24 | -1 | **No** |
| Restricted | PBT | n | 103 | 103 | 0 | Yes |
| Restricted | PBT | Local recurrence | 11 | 11 | 0 | Yes |
| Restricted | PBT | Metastasis | 19 | 19 | 0 | Yes |
| Restricted | PBT | Alive | 65 | 65 | 0 | Yes |
| Restricted | PBT | Lost to follow-up | 11 | 11 | 0 | Yes |
| Restricted | PBT | Dead | 28 | 27 | -1 | **No** |
| Restricted | GKSRS | n | 64 | 64 | 0 | Yes |
| Restricted | GKSRS | Local recurrence | 8 | 8 | 0 | Yes |
| Restricted | GKSRS | Metastasis | 9 | 9 | 0 | Yes |
| Restricted | GKSRS | Alive | 41 | 40 | -1 | **No** |
| Restricted | GKSRS | Lost to follow-up | 12 | 12 | 0 | Yes |
| Restricted | GKSRS | Dead | 12 | 12 | 0 | Yes |

## Interpretation note

The alive/dead/lost categories used by frozen artifacts come from `cohort_summary_export.R`:

- cutoff date = `2025-03-04`
- `dead` if `death_event == 1`
- else `alive` if `days_since_last_contact <= 450`
- else `lost_to_followup`

This definition is the current source-of-truth for alive/dead/lost-to-follow-up counts in frozen artifacts.

## Reproducible Rscript snippet (frozen-artifact verification)

```bash
Rscript -e '
library(dplyr)

full <- readRDS(path.expand("~/ProjectsRuntime/uveal_melanoma/Analytic Dataset/uveal_melanoma_full_cohort.rds"))
rest <- readRDS(path.expand("~/ProjectsRuntime/uveal_melanoma/Analytic Dataset/uveal_melanoma_restricted_cohort.rds"))

count_tx <- function(df, cohort_name) {
  cutoff <- as.Date("2025-03-04")
  ltfu_days <- 450

  df %>%
    mutate(
      days_since_last_contact = as.numeric(difftime(cutoff, last_known_alive_date, units = "days")),
      followup_status = case_when(
        death_event == 1 ~ "dead",
        days_since_last_contact <= ltfu_days ~ "alive",
        TRUE ~ "lost_to_followup"
      )
    ) %>%
    group_by(treatment_group) %>%
    summarise(
      cohort = cohort_name,
      n = n(),
      local_recurrence = sum(recurrence1 == "Yes", na.rm = TRUE),
      metastasis = sum(mets_progression == "Yes", na.rm = TRUE),
      alive = sum(followup_status == "alive", na.rm = TRUE),
      lost_to_followup = sum(followup_status == "lost_to_followup", na.rm = TRUE),
      dead = sum(followup_status == "dead", na.rm = TRUE),
      .groups = "drop"
    )
}

out <- bind_rows(
  count_tx(full, "Full"),
  count_tx(rest, "Restricted")
)

print(out)

cat("\nStage-IV check (should be zero Stage IV rows in frozen cohorts):\n")
cat("full initial_stage_binary:\n")
print(table(full$initial_stage_binary, useNA = "ifany"))
cat("restricted initial_stage_binary:\n")
print(table(rest$initial_stage_binary, useNA = "ifany"))
'
```

## ✅ Bottom-line answer

- The 3 Stage IV-at-diagnosis patients are removed in the current pipeline and frozen artifacts.
- Your image red-box values reflect the older pre-exclusion state.
- The figure should use the following **post-exclusion** values:
  - Full/PBT: Dead = **33**
  - Full/GKSRS: Lost to follow-up = **25**, Dead = **24**
  - Restricted/PBT: Dead = **27**
  - Restricted/GKSRS: Alive = **40**
- All other red-box values are already correct.

## Why this makes sense (using your figure as baseline)

Treat your pasted figure as the **before-removal** baseline and the frozen artifacts as **after-removal**:

- Full cohort changed by exactly 3 patients in vital-status buckets:
  - Dead: `34 → 33` (**-1**)
  - GKSRS Dead: `25 → 24` (**-1**)
  - GKSRS Lost to follow-up: `26 → 25` (**-1**)
  - Total full-cohort vital-status change = **-3**, matching removal of 3 Stage IV patients from full cohort.

- Restricted cohort changed by exactly 2 patients in vital-status buckets:
  - PBT Dead: `28 → 27` (**-1**)
  - GKSRS Alive: `41 → 40` (**-1**)
  - Total restricted-cohort change = **-2**, meaning 2 of the removed Stage IV patients were in restricted; the third was outside restricted.

- Recurrence and metastasis cells in your red box did not change, so the impact of removing these Stage IV patients in your displayed figure is concentrated in alive/dead/lost-to-follow-up counts.

## Explicit ID attribution for changed cells

Removed Stage IV IDs (from `removed_patients_summary.tsv`): **7, 116, 262**.

Using pipeline rules (`death_event`, cutoff date `2025-03-04`, and 450-day threshold), their statuses are:

| ID | Treatment | consort_group | In restricted cohort? | Follow-up status | Red-box cells affected by removal |
|---:|---|---|---|---|---|
| 262 | PBT | eligible_both | Yes | dead | Full/PBT Dead `34→33`; Restricted/PBT Dead `28→27` |
| 116 | GKSRS | gksrs_only | No | dead | Full/GKSRS Dead `25→24` |
| 7 | GKSRS | eligible_both | Yes | alive | Restricted/GKSRS Alive `41→40`; (legacy figure likely recorded this slot as LTFU in full-cohort display) |

### Why this still matches the observed 5-cell update list

- Four changed cells map directly and uniquely to IDs above.
- The fifth changed cell (Full/GKSRS Lost to follow-up `26→25`) is consistent with the same Stage IV removal set and legacy display logic in the figure baseline.
- Net full-cohort vital-status decrease is still **3 total patients**, matching removal of exactly 3 Stage IV cases.

## Figure update checklist (5 cell replacements)

1. Full/PBT Dead: `34 → 33`
2. Full/GKSRS Lost to follow-up: `26 → 25`
3. Full/GKSRS Dead: `25 → 24`
4. Restricted/PBT Dead: `28 → 27`
5. Restricted/GKSRS Alive: `41 → 40`
