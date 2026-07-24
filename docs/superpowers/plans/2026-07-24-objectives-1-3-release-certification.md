# Objectives 1–3 Release Certification Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rebuild all analytic datasets, certify collaborator-facing Objectives 1–3 from clean outputs, reconcile documentation and interpretations, and make required GitHub CI reproducible.

**Architecture:** Reuse `main_execution()`, Objective 0 validation, existing audit helpers, existing test runners, and the current publish dry run. Generate into a clean `OCULAR_RUNTIME_ROOT`, compare against the canonical runtime, promote only reviewed Objective 1–3 surfaces, and change source code only when a concrete failed gate proves that a minimal correction is necessary.

**Tech Stack:** R 4.4.3 in GitHub Actions, base R, tidyverse, survival, testthat, lintr, r-lib/actions, Posit Public Package Manager, shell, `jq`, `rsync`, Git, and GitHub CLI.

## Global Constraints

- Full and restricted cohorts are collaborator-facing; GKSRS is an internal integrity check only.
- Regenerate every analytic dataset from canonical raw inputs.
- Use existing commands before adding functions, files, configuration, or documentation surfaces.
- Add no analysis or certification entry point.
- Preserve existing statistical estimands unless a failed correctness gate proves one is wrong.
- Expected statistical limitations may pass only with an explicit caveat.
- Any unexplained patient-count, event-count, estimate, treatment-label, artifact, documentation, or interpretation discrepancy blocks release.
- Use no fallback, compatibility, or silent-rescue logic.
- Keep `docs/peer_review_revision_response.md`, `docs/PR_VS_ORIGINAL_RESULTS_AUDIT.md`, and `docs/METHODS_SECTION_PAPER.md` ignored and untracked.
- Do not publish to OneDrive or send collaborator files without explicit approval.
- Stage explicit file lists and use concise, purpose-specific commits.

---

## File Structure

**Tracked files expected to change**

- `.github/workflows/portable-tests.yml` — freeze the required CI R and CRAN environment while retaining the existing test commands.
- `tests/testthat/test_ci_contract.R` — fail if required CI returns to a drifting R or CRAN environment.
- `docs/TECHNICAL.md` — change only if regenerated artifacts prove current prose inaccurate.
- `docs/STATISTICAL_METHODS.md` — change only if regenerated model contracts or interpretations differ.
- `docs/CALCULATIONS.md` — change only if regenerated endpoint definitions differ.
- Generated tracked study documentation — refresh through `run_tool_refresh_suite()` and commit only evidence-backed changes.

**Local-only files that may be updated but never committed**

- `docs/peer_review_revision_response.md`
- `docs/PR_VS_ORIGINAL_RESULTS_AUDIT.md`
- `docs/METHODS_SECTION_PAPER.md`

**Runtime surfaces**

- Clean staging root: a new `/tmp/uveal-certification.XXXXXX` directory.
- Canonical generated root: `../runtime`.
- Collaborator-facing analyses:
  - `Analysis/uveal_full/{01_Efficacy,02_Safety,03_Repeat_Radiation}`
  - `Analysis/uveal_restricted/{01_Efficacy,02_Safety,03_Repeat_Radiation}`
- Internal integrity analyses:
  - `Analysis/gksrs/{01_Efficacy,02_Safety,03_Repeat_Radiation}`

---

### Task 1: Freeze the Existing Required CI Environment

**Files:**

- Modify: `tests/testthat/test_ci_contract.R`
- Modify: `.github/workflows/portable-tests.yml`

**Interfaces:**

- Consumes: the existing `Portable CI` workflow and fail-sensitive test commands.
- Produces: required CI on exact R 4.4.3 against the frozen 2026-07-19 Posit CRAN snapshot.

- [ ] **Step 1: Add a failing CI environment contract test**

Append this test to `tests/testthat/test_ci_contract.R`:

```r
test_that("required portable CI uses the verified frozen R environment", {
    workflow_text <- paste(
        readLines(here::here(".github", "workflows", "portable-tests.yml"), warn = FALSE),
        collapse = "\n"
    )

    expect_match(workflow_text, 'r-version: "4.4.3"', fixed = TRUE)
    expect_match(workflow_text, "use-public-rspm: false", fixed = TRUE)
    expect_match(
        workflow_text,
        'cran: "https://packagemanager.posit.co/cran/__linux__/noble/2026-07-19"',
        fixed = TRUE
    )
    expect_false(grepl(
        "packagemanager\\.posit\\.co/cran/__linux__/noble/latest",
        workflow_text
    ))
})
```

- [ ] **Step 2: Run the focused test and verify that it fails**

Run:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test_ci_contract.R", stop_on_failure = TRUE)'
```

Expected: failure because the workflow currently uses `r-version: "4.4"` and the moving `latest` Posit repository.

- [ ] **Step 3: Make the minimum workflow change**

Change only the setup inputs in `.github/workflows/portable-tests.yml`:

```yaml
      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: "4.4.3"
          use-public-rspm: false
          cran: "https://packagemanager.posit.co/cran/__linux__/noble/2026-07-19"
      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          cache-version: 2
```

Retain the existing `extra-packages` list and all existing test/lint steps unchanged.

The frozen repository contains `Deriv 4.2.0` with no compiled code; the moving repository selected newly released `Deriv 4.3.0`, which failed before tests ran.

- [ ] **Step 4: Run the focused test and verify that it passes**

Run:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test_ci_contract.R", stop_on_failure = TRUE)'
```

Expected: all `test_ci_contract.R` tests pass.

- [ ] **Step 5: Confirm the snapshot is reachable and contains the verified package**

Run:

```sh
curl -L -sS \
  "https://packagemanager.posit.co/cran/__linux__/noble/2026-07-19/src/contrib/PACKAGES.gz" |
  gzip -dc |
  grep -A5 -B1 "^Package: Deriv$"
```

Expected:

```text
Package: Deriv
Version: 4.2.0
Imports: methods
Suggests: testthat (>= 0.11.0)
NeedsCompilation: no
```

- [ ] **Step 6: Commit the CI correction**

```sh
git add .github/workflows/portable-tests.yml tests/testthat/test_ci_contract.R
git commit -m "Stabilize portable CI dependencies"
```

---

### Task 2: Regenerate All Analytic Datasets in a Clean Runtime

**Files:**

- Modify: none.
- Runtime output: `/tmp/uveal-certification.XXXXXX`
- Evidence: `/tmp/uveal_cert_runtime_path`, `/tmp/uveal_certification_run.log`, `/tmp/uveal_certification_provenance.txt`

**Interfaces:**

- Consumes: canonical raw workbook, `RECREATE_ANALYTIC_DATASETS`, `main_execution()`, and `OCULAR_RUNTIME_ROOT`.
- Produces: a clean full-pipeline runtime containing regenerated datasets and current Objective 1–4 outputs.

- [ ] **Step 1: Record the source commit and raw-input checksum**

Run:

```sh
raw_path=$(Rscript -e 'source("scripts/load_all.R"); cat(file.path(RAW_DATA_DIR, INPUT_FILENAME))' 2>/dev/null)
{
  git rev-parse HEAD
  stat -f "%Sm %N" -t "%Y-%m-%d %H:%M:%S" "$raw_path"
  shasum -a 256 "$raw_path"
} > /tmp/uveal_certification_provenance.txt
```

Expected: one Git SHA, one raw-input timestamp/path, and one SHA-256 checksum.

- [ ] **Step 2: Create and record a clean staging runtime**

Run:

```sh
cert_runtime=$(mktemp -d /tmp/uveal-certification.XXXXXX)
printf "%s\n" "$cert_runtime" > /tmp/uveal_cert_runtime_path
test -d "$cert_runtime"
```

Expected: `/tmp/uveal_cert_runtime_path` contains an empty, existing directory.

- [ ] **Step 3: Run the existing regeneration workflow**

Run:

```sh
cert_runtime=$(cat /tmp/uveal_cert_runtime_path)
set -o pipefail
OCULAR_RUNTIME_ROOT="$cert_runtime" Rscript -e '
source("scripts/load_all.R")
RECREATE_ANALYTIC_DATASETS <- TRUE
result <- main_execution()
if (identical(result$run_state, "failed")) {
    quit(status = 1L)
}
' 2>&1 | tee /tmp/uveal_certification_run.log
```

Expected: exit status 0 and a final `ANALYSES COMPLETED` message. `COMPLETED WITH WARNINGS` is allowed only for documented feasibility or model-assumption limitations.

- [ ] **Step 4: Verify all regenerated analytic datasets exist**

Run:

```sh
cert_runtime=$(cat /tmp/uveal_cert_runtime_path)
for cohort in \
  uveal_melanoma_full_cohort \
  uveal_melanoma_restricted_cohort \
  uveal_melanoma_gksrs_only_cohort
do
  test -s "$cert_runtime/Analytic Dataset/$cohort.rds"
done
shasum -a 256 "$cert_runtime"/Analytic\ Dataset/*.rds |
  tee -a /tmp/uveal_certification_provenance.txt
```

Expected: three non-empty RDS files and three SHA-256 hashes.

- [ ] **Step 5: Verify the JSON log has no errors**

Run:

```sh
cert_runtime=$(cat /tmp/uveal_cert_runtime_path)
cert_json=$(ls -t "$cert_runtime"/logs/json/run_log_*.jsonl | head -n 1)
if jq -e 'select(.level_text == "ERROR")' "$cert_json" >/dev/null; then
  jq -r 'select(.level_text == "ERROR") | [.cohort,.objective,.message] | @tsv' "$cert_json"
  exit 1
fi
jq -r 'select(.message | contains("ANALYSES COMPLETED")) | .message' "$cert_json"
```

Expected: no error rows and one final completion message.

---

### Task 3: Compare Regenerated Data With the Current Canonical Data

**Files:**

- Modify: none.
- Evidence: `/tmp/uveal_dataset_comparison_summary.csv`, `/tmp/uveal_dataset_comparison_details.csv`

**Interfaces:**

- Consumes: staged and canonical analytic RDS files.
- Produces: patient-level and variable-level evidence that every changed value is identified.

- [ ] **Step 1: Run the patient and endpoint comparison**

Run:

```sh
export CERT_RUNTIME=$(cat /tmp/uveal_cert_runtime_path)
Rscript - <<'RS'
canonical_root <- normalizePath("../runtime/Analytic Dataset", mustWork = TRUE)
staged_root <- file.path(Sys.getenv("CERT_RUNTIME"), "Analytic Dataset")
cohorts <- c(
    "uveal_melanoma_full_cohort",
    "uveal_melanoma_restricted_cohort",
    "uveal_melanoma_gksrs_only_cohort"
)
audit_columns <- c(
    "id", "treatment_group",
    "recurrence_event", "tt_recurrence_months",
    "mets_event", "tt_mets_months",
    "death_event", "tt_death_months",
    "pfs_event", "tt_pfs_months",
    "initial_vision", "last_vision", "last_followup",
    "height_change",
    "retinopathy_burden_event", "nvg_burden_event", "srd_burden_event",
    "recurrence1_treatment_clean", "pfs2_event", "tt_pfs2_months"
)
summary_rows <- list()
detail_rows <- list()

normalize_value <- function(x) {
    value <- as.character(x)
    value[is.na(value)] <- "<NA>"
    value
}

for (cohort in cohorts) {
    old <- readRDS(file.path(canonical_root, paste0(cohort, ".rds")))
    new <- readRDS(file.path(staged_root, paste0(cohort, ".rds")))
    if (!"id" %in% names(old) || !"id" %in% names(new)) {
        stop(sprintf("Missing id in %s", cohort))
    }

    old_ids <- normalize_value(old$id)
    new_ids <- normalize_value(new$id)
    missing_from_old <- setdiff(audit_columns, names(old))
    missing_from_new <- setdiff(audit_columns, names(new))
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
        cohort = cohort,
        check = c(
            "row_count",
            "added_ids",
            "removed_ids",
            "missing_audit_columns"
        ),
        old_value = c(
            nrow(old),
            0L,
            length(setdiff(old_ids, new_ids)),
            length(missing_from_old)
        ),
        new_value = c(
            nrow(new),
            length(setdiff(new_ids, old_ids)),
            0L,
            length(missing_from_new)
        )
    )

    shared_ids <- intersect(old_ids, new_ids)
    old <- old[match(shared_ids, old_ids), , drop = FALSE]
    new <- new[match(shared_ids, new_ids), , drop = FALSE]
    shared_columns <- audit_columns[
        audit_columns %in% names(old) & audit_columns %in% names(new)
    ]

    for (column in shared_columns) {
        old_value <- normalize_value(old[[column]])
        new_value <- normalize_value(new[[column]])
        changed <- old_value != new_value
        summary_rows[[length(summary_rows) + 1L]] <- data.frame(
            cohort = cohort,
            check = paste0("changed:", column),
            old_value = sum(changed),
            new_value = sum(changed)
        )
        if (any(changed)) {
            detail_rows[[length(detail_rows) + 1L]] <- data.frame(
                cohort = cohort,
                id = shared_ids[changed],
                variable = column,
                old_value = old_value[changed],
                new_value = new_value[changed]
            )
        }
    }
}

summary_result <- do.call(rbind, summary_rows)
detail_result <- if (length(detail_rows)) {
    do.call(rbind, detail_rows)
} else {
    data.frame(
        cohort = character(),
        id = character(),
        variable = character(),
        old_value = character(),
        new_value = character()
    )
}
write.csv(summary_result, "/tmp/uveal_dataset_comparison_summary.csv", row.names = FALSE)
write.csv(detail_result, "/tmp/uveal_dataset_comparison_details.csv", row.names = FALSE)

blocking <- any(summary_result$old_value != summary_result$new_value) ||
    nrow(detail_result) > 0 ||
    any(vapply(cohorts, function(cohort) {
        old <- readRDS(file.path(canonical_root, paste0(cohort, ".rds")))
        new <- readRDS(file.path(staged_root, paste0(cohort, ".rds")))
        length(setdiff(audit_columns, names(old))) > 0L ||
            length(setdiff(audit_columns, names(new))) > 0L
    }, logical(1)))
if (blocking) {
    print(summary_result[summary_result$old_value != summary_result$new_value, ])
    print(detail_result)
    quit(status = 2L)
}
RS
```

Expected: exit status 0, no added or removed IDs, and no changed audited endpoint values. Any difference is a release block until its patient-level source is explained.

- [ ] **Step 2: Verify cohort population contracts**

Run:

```sh
export CERT_RUNTIME=$(cat /tmp/uveal_cert_runtime_path)
OCULAR_RUNTIME_ROOT="$CERT_RUNTIME" Rscript -e '
source("scripts/load_all.R")
for (cohort in c(
    "uveal_melanoma_full_cohort",
    "uveal_melanoma_restricted_cohort",
    "uveal_melanoma_gksrs_only_cohort"
)) {
    data <- readRDS(file.path(PROCESSED_DATA_DIR, paste0(cohort, ".rds")))
    cat(cohort, nrow(data), "\n")
    print(table(data$treatment_group, useNA = "ifany"))
}
'
```

Expected: full `n=260`, restricted `n=167`, and GKSRS integrity counts matching the regenerated Objective 0 audit. If a contract intentionally changes, stop and update the contract only after the patient-level evidence is reviewed.

---

### Task 4: Certify Objective 1

**Files:**

- Modify only on proven mismatch: `scripts/config/objective1_contracts.R`, `scripts/workflow/objective_1_primary_outcomes.R`, `docs/STATISTICAL_METHODS.md`, `docs/CALCULATIONS.md`.
- Test: `tests/testthat/test_objective1_primary_outcomes.R`
- Test: `tests/testthat/test_objective1_subgroup_policy.R`
- Test: `tests/testthat/test_objective1_age_decade_sensitivity.R`
- Test: `tests/testthat/test_peer_review_revision_contract.R`
- Test: `tests/testthat/test_survival_population_audit.R`

**Interfaces:**

- Consumes: clean Objective 1 runtime artifacts and centralized outcome/population contracts.
- Produces: verified recurrence, metastasis, OS, PFS, tumor-height, and subgroup evidence for full/restricted collaborator reporting.

- [ ] **Step 1: Run the focused Objective 1 contract suite**

```sh
Rscript -e '
files <- c(
    "tests/testthat/test_objective1_primary_outcomes.R",
    "tests/testthat/test_objective1_subgroup_policy.R",
    "tests/testthat/test_objective1_age_decade_sensitivity.R",
    "tests/testthat/test_peer_review_revision_contract.R",
    "tests/testthat/test_survival_population_audit.R"
)
for (file in files) {
    testthat::test_file(file, stop_on_failure = TRUE, stop_on_warning = FALSE)
}
'
```

Expected: zero failures.

- [ ] **Step 2: Verify effect-measure and endpoint metadata in clean forest diagnostics**

```sh
export CERT_RUNTIME=$(cat /tmp/uveal_cert_runtime_path)
Rscript - <<'RS'
root <- Sys.getenv("CERT_RUNTIME")
paths <- c(
    full = file.path(root, "Analysis/uveal_full/01_Efficacy/g_subgroup_analysis/forest_plots/full_cohort_forest_plot_diagnostics.xlsx"),
    restricted = file.path(root, "Analysis/uveal_restricted/01_Efficacy/g_subgroup_analysis/forest_plots/restricted_cohort_forest_plot_diagnostics.xlsx")
)
expected <- list(
    local_recurrence = c("tt_recurrence_months", "recurrence_event"),
    metastatic_progression = c("tt_mets_months", "mets_event"),
    overall_survival = c("tt_death_months", "death_event"),
    progression_free_survival = c("tt_pfs_months", "pfs_event")
)
for (path in paths) {
    stopifnot(file.exists(path))
    for (sheet in names(expected)) {
        data <- readxl::read_xlsx(path, sheet = sheet)
        stopifnot(
            identical(unique(data$model_family), "Cox proportional hazards"),
            identical(unique(data$effect_measure), "HR"),
            identical(unique(data$time_variable), expected[[sheet]][[1]]),
            identical(unique(data$event_variable), expected[[sheet]][[2]])
        )
    }
}
RS
```

Expected: exit status 0.

- [ ] **Step 3: Verify PFS composition directly in both collaborator cohorts**

```sh
export CERT_RUNTIME=$(cat /tmp/uveal_cert_runtime_path)
Rscript - <<'RS'
root <- file.path(Sys.getenv("CERT_RUNTIME"), "Analytic Dataset")
for (cohort in c("uveal_melanoma_full_cohort", "uveal_melanoma_restricted_cohort")) {
    data <- readRDS(file.path(root, paste0(cohort, ".rds")))
    expected_event <- as.integer(
        data$recurrence_event == 1 |
        data$mets_event == 1 |
        data$death_event == 1
    )
    stopifnot(identical(as.integer(data$pfs_event), expected_event))
    expected_time <- pmin(
        data$tt_recurrence_months,
        data$tt_mets_months,
        data$tt_death_months,
        na.rm = FALSE
    )
    stopifnot(isTRUE(all.equal(data$tt_pfs_months, expected_time)))
}
RS
```

Expected: exit status 0.

- [ ] **Step 4: Audit full/restricted warnings and PH caveats**

```sh
cert_runtime=$(cat /tmp/uveal_cert_runtime_path)
cert_json=$(ls -t "$cert_runtime"/logs/json/run_log_*.jsonl | head -n 1)
jq -r '
select(
  .level_text == "WARN" and
  (.objective == "objective_1_primary_outcomes") and
  ((.cohort | contains("full")) or (.cohort | contains("restricted")))
) |
[.cohort,.message] | @tsv
' "$cert_json" | sort -u
```

Expected: only explicit PH or feasibility caveats. Confirm each against the corresponding PH diagnostics workbook and the reviewer response. An unexplained model failure is a block.

- [ ] **Step 5: Visually inspect the eight collaborator-facing forest plots**

Inspect these staged files for correct PBT/GKSRS column alignment, `n/N` counts, age `<63`/`≥63`, T4/GEP display, HR labeling, confidence intervals, and non-estimable text:

```text
Analysis/uveal_full/01_Efficacy/g_subgroup_analysis/forest_plots/full_cohort_{local_recurrence,metastatic_progression,overall_survival,progression_free_survival}_subgroup_forest_plot.png
Analysis/uveal_restricted/01_Efficacy/g_subgroup_analysis/forest_plots/restricted_cohort_{local_recurrence,metastatic_progression,overall_survival,progression_free_survival}_subgroup_forest_plot.png
```

Expected: every displayed count matches its workbook row and treatment header.

- [ ] **Step 6: Stop on a failed Objective 1 gate**

If any prior step fails, do not edit multiple surfaces speculatively. Record the exact artifact, patient rows, code path, and failed expectation; then make the smallest source correction with a focused failing test before continuing.

---

### Task 5: Certify Objective 2

**Files:**

- Modify only on proven mismatch: `scripts/analysis/vision_safety_analysis.R`, `scripts/workflow/objective_2_safety_toxicity.R`, `scripts/config/objective0_contracts.R`, `docs/STATISTICAL_METHODS.md`.
- Test: `tests/testthat/test_objective2_safety_toxicity.R`
- Test: `tests/testthat/test_peer_review_data_availability.R`

**Interfaces:**

- Consumes: clean Objective 2 artifacts and Objective 0 burden endpoints.
- Produces: verified VA, latest-VA, minimum-follow-up, retinopathy, NVG, and SRD evidence.

- [ ] **Step 1: Run the focused Objective 2 suite**

```sh
Rscript -e '
for (file in c(
    "tests/testthat/test_objective2_safety_toxicity.R",
    "tests/testthat/test_peer_review_data_availability.R"
)) {
    testthat::test_file(file, stop_on_failure = TRUE, stop_on_warning = FALSE)
}
'
```

Expected: zero failures.

- [ ] **Step 2: Verify latest-VA and minimum-follow-up outputs remain distinct**

```sh
export CERT_RUNTIME=$(cat /tmp/uveal_cert_runtime_path)
Rscript - <<'RS'
root <- Sys.getenv("CERT_RUNTIME")
for (scope in c("uveal_full", "uveal_restricted")) {
    prefix <- if (scope == "uveal_full") "full_cohort_" else "restricted_cohort_"
    base <- file.path(root, "Analysis", scope, "02_Safety/a_vision_changes/04_sensitivity")
    followup_path <- file.path(base, paste0(prefix, "vision_followup_sensitivity.xlsx"))
    latest_path <- file.path(base, paste0(prefix, "va_latest_reviewer_sens_diagnostics.xlsx"))
    stopifnot(file.exists(followup_path), file.exists(latest_path))

    followup_sheets <- readxl::excel_sheets(followup_path)
    stopifnot(all(c(
        "explicit_min_followup_12mo",
        "explicit_min_followup_36mo",
        "explicit_min_followup_60mo",
        "treatment_effect_model",
        "latest_va_reviewer_model",
        "reviewer_predictor_availability",
        "limitation"
    ) %in% followup_sheets))

    model_summary <- readxl::read_xlsx(latest_path, sheet = "Model_summary")
    model_text <- paste(unlist(model_summary), collapse = " ")
    stopifnot(
        grepl("last_vision", model_text, fixed = TRUE),
        grepl("initial_vision", model_text, fixed = TRUE)
    )
}
RS
```

Expected: exit status 0. The latest-recorded VA sensitivity is an adjusted latest-VA model; the 12/36/60-month analyses are change-score models restricted by minimum follow-up.

- [ ] **Step 3: Audit effect-summary workbooks with the existing helper**

```sh
cert_runtime=$(cat /tmp/uveal_cert_runtime_path)
OCULAR_RUNTIME_ROOT="$cert_runtime" Rscript -e '
source("scripts/load_all.R")
source("scripts/tools/effect_summary_audit.R")
findings <- audit_effect_summary_directory(
    base_dir = OUTPUT_DIR,
    output_path = file.path(TOOLS_OUTPUT_DIR, "effect_summary_audit.csv")
)
if (nrow(findings) > 0) {
    print(findings)
    quit(status = 1L)
}
'
```

Expected: zero findings.

- [ ] **Step 4: Verify toxicity endpoint contracts directly**

```sh
export CERT_RUNTIME=$(cat /tmp/uveal_cert_runtime_path)
Rscript - <<'RS'
root <- file.path(Sys.getenv("CERT_RUNTIME"), "Analytic Dataset")
required <- c(
    "retinopathy_burden_event",
    "nvg_burden_event",
    "srd_burden_event"
)
for (cohort in c("uveal_melanoma_full_cohort", "uveal_melanoma_restricted_cohort")) {
    data <- readRDS(file.path(root, paste0(cohort, ".rds")))
    stopifnot(all(required %in% names(data)))
    for (variable in required) {
        stopifnot(all(stats::na.omit(data[[variable]]) %in% c(0, 1)))
    }
}
RS
```

Expected: exit status 0. Interpret these variables as recorded burden by available follow-up, not time-to-toxicity incidence.

- [ ] **Step 5: Audit full/restricted Objective 2 warnings**

```sh
cert_runtime=$(cat /tmp/uveal_cert_runtime_path)
cert_json=$(ls -t "$cert_runtime"/logs/json/run_log_*.jsonl | head -n 1)
jq -r '
select(
  .level_text == "WARN" and
  (.objective == "objective_2_safety_toxicity") and
  ((.cohort | contains("full")) or (.cohort | contains("restricted")))
) |
[.cohort,.message] | @tsv
' "$cert_json" | sort -u
```

Expected: only explicit insufficient-event or feasibility skips, each matched to a `_SKIPPED.html` and diagnostics workbook.

---

### Task 6: Certify Objective 3 and Retired-Artifact Absence

**Files:**

- Modify only on proven mismatch: `scripts/analysis/pfs2_analysis.R`, `scripts/workflow/objective_3_repeat_radiation.R`, `scripts/config/objective0_contracts.R`, `docs/TECHNICAL.md`.
- Test: `tests/testthat/test_objective3_repeat_radiation.R`
- Test: `tests/testthat/test_objective3_objective4_scope_protection.R`

**Interfaces:**

- Consumes: clean Objective 3 PFS-2 data and artifacts.
- Produces: verified second-recurrence endpoint handling and proof that retired runtime paths are absent.

- [ ] **Step 1: Run the focused Objective 3 suite**

```sh
Rscript -e '
for (file in c(
    "tests/testthat/test_objective3_repeat_radiation.R",
    "tests/testthat/test_objective3_objective4_scope_protection.R"
)) {
    testthat::test_file(file, stop_on_failure = TRUE, stop_on_warning = FALSE)
}
'
```

Expected: zero failures.

- [ ] **Step 2: Verify the PFS-2 derivation contract**

```sh
export CERT_RUNTIME=$(cat /tmp/uveal_cert_runtime_path)
Rscript - <<'RS'
source("scripts/load_all.R")
contract <- OBJECTIVE3_PFS2_DERIVATION_CONTRACT
stopifnot(
    all(c("pfs2_event", "tt_pfs2_months") %in% contract$derived_fields),
    identical(
        contract$death_handling,
        "death before second local recurrence is censoring"
    )
)
root <- file.path(Sys.getenv("CERT_RUNTIME"), "Analytic Dataset")
for (cohort in c(
    "uveal_melanoma_full_cohort",
    "uveal_melanoma_restricted_cohort",
    "uveal_melanoma_gksrs_only_cohort"
)) {
    data <- readRDS(file.path(root, paste0(cohort, ".rds")))
    stopifnot(all(c(
        "recurrence1_treatment_clean",
        "pfs2_event",
        "tt_pfs2_months"
    ) %in% names(data)))
}
RS
```

Expected: exit status 0.

- [ ] **Step 3: Verify full-cohort treatment estimability is represented honestly**

```sh
export CERT_RUNTIME=$(cat /tmp/uveal_cert_runtime_path)
Rscript - <<'RS'
path <- file.path(
    Sys.getenv("CERT_RUNTIME"),
    "Analysis/uveal_full/03_Repeat_Radiation/a_pfs2/01_cohort_support/full_cohort_pfs2_treatment_summary.xlsx"
)
stopifnot(file.exists(path))
stopifnot(all(c(
    "raw_primary_vs_salvage",
    "model_primary_vs_salvage",
    "censoring_support",
    "interpretation_guardrails",
    "treatment_estimability"
) %in% readxl::excel_sheets(path)))
estimability <- readxl::read_xlsx(path, sheet = "treatment_estimability")
stopifnot(any(grepl("zero", paste(unlist(estimability), collapse = " "), ignore.case = TRUE)))
RS
```

Expected: the zero-event reference-arm limitation is explicit and the unsupported Cox comparison remains skipped.

- [ ] **Step 4: Prove retired Objective 3 paths are absent**

```sh
cert_runtime=$(cat /tmp/uveal_cert_runtime_path)
retired=$(find "$cert_runtime/Analysis" \
  \( -path "*/03_Repeat_Radiation/b_proportional_hazards_diagnostics/*" \
     -o -path "*/03_Repeat_Radiation/a_pfs2/*" \) \
  -type f |
  awk '
    /b_proportional_hazards_diagnostics/ { print; next }
    /a_pfs2\/[^/]+$/ { print }
  ')
if [ -n "$retired" ]; then
  printf "%s\n" "$retired"
  exit 1
fi
```

Expected: no output and exit status 0.

- [ ] **Step 5: Audit Objective 3 warnings**

```sh
cert_runtime=$(cat /tmp/uveal_cert_runtime_path)
cert_json=$(ls -t "$cert_runtime"/logs/json/run_log_*.jsonl | head -n 1)
jq -r '
select(.level_text == "WARN" and .objective == "objective_3_repeat_radiation") |
[.cohort,.message] | @tsv
' "$cert_json" | sort -u
```

Expected: only explicit PFS-2 support or estimability limitations with corresponding structured skip artifacts.

---

### Task 7: Promote Reviewed Outputs and Reconcile Existing Documentation

**Files:**

- Runtime modification: explicit Objective 1–3 cohort directories and analytic datasets under `../runtime`.
- Possible tracked modification: `docs/TECHNICAL.md`, `docs/STATISTICAL_METHODS.md`, `docs/CALCULATIONS.md`, generated study docs.
- Local-only modification: reviewer response, audit tracker, and paper methods notes.

**Interfaces:**

- Consumes: clean staged outputs that passed Tasks 2–6.
- Produces: canonical current runtime plus aligned existing documentation.

- [ ] **Step 1: Promote regenerated datasets and Objective 1–3 directories**

Run only after every prior gate passes:

```sh
cert_runtime=$(cat /tmp/uveal_cert_runtime_path)
rsync -a --delete \
  "$cert_runtime/Analytic Dataset/" \
  "../runtime/Analytic Dataset/"

for cohort in uveal_full uveal_restricted gksrs
do
  for objective_dir in 01_Efficacy 02_Safety 03_Repeat_Radiation
  do
    rsync -a --delete \
      "$cert_runtime/Analysis/$cohort/$objective_dir/" \
      "../runtime/Analysis/$cohort/$objective_dir/"
  done
done
```

Expected: exact staged Objective 1–3 trees replace the canonical generated trees, removing stale flat and mirror files.

- [ ] **Step 2: Refresh the current merged Objective 0–2 tables**

Copy only the existing merged files relevant to cohort description, Objective 1, and Objective 2:

```sh
cert_runtime=$(cat /tmp/uveal_cert_runtime_path)
for file in \
  merged_baseline_characteristics.html \
  merged_baseline_characteristics.xlsx \
  merged_baseline_characteristics_all_three_cohorts.html \
  merged_baseline_characteristics_all_three_cohorts.xlsx \
  merged_baseline_characteristics_full_vs_gksrs_only.html \
  merged_baseline_characteristics_full_vs_gksrs_only.xlsx \
  merged_recurrence_metastatic_progression.html \
  merged_recurrence_metastatic_progression.xlsx \
  merged_adverse_events.html \
  merged_adverse_events.xlsx
do
  rsync -a \
    "$cert_runtime/Analysis/merged_tables/$file" \
    "../runtime/Analysis/merged_tables/$file"
done
```

Expected: ten files copied successfully.

- [ ] **Step 3: Run the existing peer-review and study-document refreshes**

```sh
Rscript -e '
source("scripts/load_all.R")
source("scripts/tools/peer_review_followup_audit.R")
run_peer_review_followup_audits()
'

Rscript scripts/tools/run_tool_refreshes.R
```

Expected: peer-review audit workbooks and tool-refresh manifest report success.

- [ ] **Step 4: Check the required documentation statements**

Run:

```sh
rg -n \
  "local recurrence, metastatic progression, or death|Cox proportional|hazard ratio|continuous age|minimum-followup|minimum-follow-up|latest-VA|recorded burden|second local recurrence|death before second" \
  docs/TECHNICAL.md \
  docs/STATISTICAL_METHODS.md \
  docs/CALCULATIONS.md \
  docs/peer_review_revision_response.md \
  docs/PR_VS_ORIGINAL_RESULTS_AUDIT.md \
  docs/METHODS_SECTION_PAPER.md
```

Expected: current definitions are present and mutually consistent. Compare every numeric reviewer statement against the promoted workbook named as its evidence.

- [ ] **Step 5: Apply only evidence-backed documentation corrections**

If a statement disagrees with a promoted artifact, change that exact sentence or table cell. Do not add another tracker or summary document. Preserve these distinctions:

```text
Objective 1 PFS = first local recurrence, metastatic progression, or death.
Objective 1 recurrence/metastasis subgroup effects = Cox HRs.
Adjusted-model age = continuous; exploratory forest age = <63 versus ≥63.
Latest-VA reviewer sensitivity = adjusted latest-VA model with baseline VA and follow-up duration.
12/36/60-month minimum-follow-up analyses = change-score subset sensitivities.
Objective 2 toxicity = recorded burden by available follow-up.
Objective 3 PFS-2 event = second local recurrence; death beforehand = censoring.
```

- [ ] **Step 6: Prove the local reviewer files remain ignored**

```sh
git check-ignore -v \
  docs/peer_review_revision_response.md \
  docs/PR_VS_ORIGINAL_RESULTS_AUDIT.md \
  docs/METHODS_SECTION_PAPER.md

for file in \
  docs/peer_review_revision_response.md \
  docs/PR_VS_ORIGINAL_RESULTS_AUDIT.md \
  docs/METHODS_SECTION_PAPER.md
do
  if git ls-files --error-unmatch "$file" >/dev/null 2>&1; then
    printf "ERROR: tracked local reviewer file: %s\n" "$file"
    exit 1
  fi
done
```

Expected: all three files are ignored and none is tracked.

- [ ] **Step 7: Commit tracked documentation changes only when present**

Inspect:

```sh
git diff --check
git status --short
git diff -- docs/TECHNICAL.md docs/STATISTICAL_METHODS.md docs/CALCULATIONS.md docs/dependency_diagram.md docs/FIGURE_COUNTS_AUDIT.md
```

If the tracked documentation changed, stage only the verified files:

```sh
git add docs/TECHNICAL.md docs/STATISTICAL_METHODS.md docs/CALCULATIONS.md docs/dependency_diagram.md docs/FIGURE_COUNTS_AUDIT.md
git diff --cached --check
git commit -m "Align Objectives 1-3 documentation"
```

If none changed, do not create an empty documentation commit.

---

### Task 8: Run Final Verification, Push, and Observe Required CI

**Files:**

- Modify: none unless a concrete final check fails.
- Evidence: `/tmp/uveal_testthat_release.log`, `/tmp/uveal_integration_release.log`, GitHub Actions run.

**Interfaces:**

- Consumes: committed CI change, promoted runtime, aligned documentation.
- Produces: a green local verification record and a green required GitHub CI run.

- [ ] **Step 1: Run the complete portable suite**

```sh
Rscript scripts/tools/run_testthat.R tests/testthat \
  > /tmp/uveal_testthat_release.log 2>&1
tail -n 8 /tmp/uveal_testthat_release.log
```

Expected: `FAIL 0` and all portable tests pass.

- [ ] **Step 2: Run the feasible integration suite**

```sh
OCULAR_RUN_INTEGRATION_TESTS=true \
  Rscript scripts/tools/run_testthat.R tests/integration \
  > /tmp/uveal_integration_release.log 2>&1
tail -n 20 /tmp/uveal_integration_release.log
```

Expected: `FAIL 0`. Data-dependent skips are acceptable only when the isolated integration runtime explicitly reports the missing local analytic-data prerequisite.

- [ ] **Step 3: Run lint exactly as CI runs it**

```sh
Rscript -e '
lints <- lintr::lint_package()
if (length(lints) > 0L) {
    print(lints)
    stop(sprintf("%d lint(s) found", length(lints)))
}
'
```

Expected: exit status 0.

- [ ] **Step 4: Run the publish dry run without copying files**

```sh
Rscript -e '
source("scripts/load_all.R")
result <- publish_outputs(
    cohorts = c(
        "uveal_melanoma_full_cohort",
        "uveal_melanoma_restricted_cohort"
    ),
    dry_run = TRUE
)
print(result$summary)
stopifnot(
    result$summary$missing == 0,
    result$summary$failed == 0
)
'
```

Expected: no missing or failed publish candidates. Do not perform a real publish.

- [ ] **Step 5: Verify commit contents and local-only exclusions**

```sh
git status --short --branch
git log --oneline --decorate -8
git log -8 --name-only --format= |
  rg "peer_review_revision_response|PR_VS_ORIGINAL_RESULTS_AUDIT|METHODS_SECTION_PAPER" &&
  exit 1 || true
```

Expected: no tracked worktree changes and no local reviewer file in recent commits.

- [ ] **Step 6: Push the focused commits**

```sh
git push origin master
```

Expected: push succeeds without force.

- [ ] **Step 7: Watch the CI run through every stage**

```sh
head_sha=$(git rev-parse HEAD)
run_id=""
for attempt in $(seq 1 12)
do
  run_id=$(
    gh run list \
      --workflow "Portable CI" \
      --commit "$head_sha" \
      --limit 1 \
      --json databaseId \
      --jq '.[0].databaseId'
  )
  [ -n "$run_id" ] && break
  sleep 5
done
test -n "$run_id"
gh run watch "$run_id" --exit-status
```

Expected: environment setup, portable tests, integration tests, and lint all complete successfully.

- [ ] **Step 8: Stop and diagnose any CI failure by stage**

If `gh run watch` returns non-zero, run:

```sh
gh run view "$run_id" --json jobs,conclusion,url,headSha,displayTitle
gh run view "$run_id" --log-failed > /tmp/uveal_ci_failed.log
rg -n "Error:|FAIL|failed|compilation|Process completed" /tmp/uveal_ci_failed.log |
  tail -n 80
```

Do not retry unchanged CI. Classify the failure as environment setup, portable test, integration test, lint/documentation, or GitHub infrastructure; then make only the smallest evidence-backed correction.

- [ ] **Step 9: Prepare the collaborator-ready handoff**

Report:

```text
Source commit and green CI URL
Raw-input and analytic-dataset hashes
Full/restricted cohort and event counts
Objective 1–3 PASS or PASS WITH CAVEAT findings
Every changed result with its patient-level explanation
Exact reviewer response and methods files reviewed
Exact figures/workbooks recommended for sharing
Explicit PH, sparse-event, follow-up, observational, and PFS-2 caveats
Confirmation that no OneDrive publication or external send occurred
```

Do not claim completion if any release block remains.
