# Uveal Melanoma Treatment Outcomes Analysis

This repository contains the analysis pipeline for comparing clinical outcomes after Gamma Knife stereotactic radiosurgery (GKSRS) and proton beam therapy (PBT) in uveal melanoma. It produces cohort-specific tables, figures, workbooks, and validation outputs for efficacy, safety, repeat-radiation, and biomarker-validation workflows.

## Documentation Map

Use the documentation set by purpose rather than reading every file linearly.

| Document | Primary use |
|----------|-------------|
| `README.md` | First-stop overview, setup, execution, output map, and links outward |
| `docs/TECHNICAL.md` | Workflow orchestration, directory structure, configuration, artifact contracts, and QA |
| `docs/CALCULATIONS.md` | Derived-variable definitions, endpoint construction, and sign conventions |
| `docs/STATISTICAL_METHODS.md` | Canonical statistical methodology, assumptions, thresholds, and validation metrics |
| `docs/INTERPRETATION_GUIDE.md` | How to read tables, plots, workbooks, and Objective 4 deliverables |
| `docs/METHODS_SECTION_PAPER.md` | Manuscript-facing methods draft derived from the canonical docs |

## Quick Start

### Prerequisites

- R 4.4 or newer
- Access to the project input spreadsheet referenced in `scripts/utils/config_constants.R`
- Installed packages from `scripts/bootstrap_packages.R`

### Run the pipeline

```r
source("scripts/bootstrap_packages.R") # one-time bootstrap on a new machine
source("scripts/load_all.R")

# All cohorts, all objectives
main_execution()

# One cohort, all objectives
run_my_analysis("uveal_melanoma_full_cohort")

# One cohort, one objective
run_specific_objective("uveal_melanoma_full_cohort", 4)
```

### Run tests

```r
# Portable regression suite
Rscript -e "testthat::test_dir('tests/testthat')"

# Local integration suite (requires local cohort data)
Rscript -e "Sys.setenv(OCULAR_RUN_INTEGRATION_TESTS='true'); testthat::test_dir('tests/integration')"
```

## Study Scope

The pipeline works with three intentionally overlapping analytic cohorts created from the same cleaned master dataset.

| Cohort | Runtime dataset id | Primary role |
|--------|--------------------|--------------|
| Full | `uveal_melanoma_full_cohort` | All-comers treatment cohort for real-world comparison |
| Restricted | `uveal_melanoma_restricted_cohort` | Dual-eligibility cohort for a more balanced treatment comparison |
| GKSRS-only | `uveal_melanoma_gksrs_only_cohort` | Patients ineligible for PBT, used to characterize GKSRS in challenging cases |

The current cohort counts and summary totals are written to `~/ProjectsRuntime/uveal_melanoma/Analytic Dataset/cohort_summary_statistics.json` whenever the pipeline is rerun.

The analysis is organized into four main research objectives:

| Objective | Focus | Primary outputs |
|-----------|-------|-----------------|
| 1 | Efficacy | Event summaries, survival outputs, tumor-height analyses, subgroup forest plots |
| 2 | Safety | Vision summaries, adverse-event models, diagnostics, effect-summary workbooks |
| 3 | Repeat radiation | PFS-2 summaries, survival outputs, skip artifacts when data are sparse |
| 4 | GEP validation | Consolidated workbooks, technical workbooks, narrative summaries, KM/CIF displays |

## Output Map

Pipeline outputs are split across runtime storage and synced exports.

```text
~/ProjectsRuntime/uveal_melanoma/
|- Analytic Dataset/
|  |- cohort_summary_statistics.json
|  |- *.rds
|  `- *_derived_precollapse.rds
|- Analysis/
|  |- uveal_full/
|  |- uveal_restricted/
|  |- gksrs/
|  `- merged_tables/
|- logs/
|- test_output/
`- tools_output/

~/Library/CloudStorage/OneDrive-Personal/Research/uveal_melanoma/
|- Original Files/
`- Analysis/
   `- <YYYY-MM-DD>/
```

Within each cohort folder, outputs follow a consistent layout:

- `00_General/`: baseline characteristics, cohort summaries, treatment-duration summaries, exclusion summaries
- `01_Efficacy/`: recurrence, metastasis, survival, tumor-height, subgroup outputs
- `02_Safety/`: vision and radiation-related adverse-event outputs
- `03_Repeat_Radiation/`: PFS-2 summaries and survival artifacts
- `04_GEP_Validation/`: Objective 4 workbooks, plots, and unified summaries

Objective 4 has a deliberate reading path:

1. Start here for the high-level purpose and output location.
2. Use [docs/STATISTICAL_METHODS.md](docs/STATISTICAL_METHODS.md#gep-validation-metrics) for the formal validation framework.
3. Use [docs/INTERPRETATION_GUIDE.md](docs/INTERPRETATION_GUIDE.md#understanding-gep-analysis) for workbook-first reading guidance.
4. Use [docs/TECHNICAL.md](docs/TECHNICAL.md#objective-4-gep-predictive-accuracy) for implementation and artifact contracts.

## Publish Workflow

Publishing is a manual step. Runtime artifacts stay in `~/ProjectsRuntime/uveal_melanoma/...`; synced exports only receive selected final deliverables under `<EXPORT_ROOT>/Analysis/<YYYY-MM-DD>/`.

```r
source("scripts/load_all.R")

# Review the candidate snapshot first
publish_outputs(dry_run = TRUE)

# Copy the approved deliverables into the dated export snapshot
publish_outputs(dry_run = FALSE)
```

The publish step copies only registry-approved deliverables. It excludes `.rds`, diagnostics workbooks, caches, logs, test output, and ad hoc intermediate files by design.

## Configuration

Most run-time configuration lives in `scripts/utils/config_constants.R`. Typical settings to review before a fresh run:

```r
INPUT_FILENAME <- "your_data_file.xlsx"
RECREATE_ANALYTIC_DATASETS <- TRUE
USE_LOGS <- TRUE
```

Objective 4 grouping and display settings are also centralized there through `GEP_GROUPING_SPECS` and `GEP_OBJECTIVE4_GROUPING`.

## Where To Go Next

- Derivations and sign conventions: [docs/CALCULATIONS.md](docs/CALCULATIONS.md)
- Statistical methods and thresholds: [docs/STATISTICAL_METHODS.md](docs/STATISTICAL_METHODS.md)
- Workflow internals and artifact contracts: [docs/TECHNICAL.md](docs/TECHNICAL.md)
- Output interpretation: [docs/INTERPRETATION_GUIDE.md](docs/INTERPRETATION_GUIDE.md)
- Manuscript-facing methods draft: [docs/METHODS_SECTION_PAPER.md](docs/METHODS_SECTION_PAPER.md)
