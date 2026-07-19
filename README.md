# Uveal Melanoma Treatment Outcomes Analysis

This repository contains the analysis pipeline for comparing clinical outcomes after Gamma Knife stereotactic radiosurgery (GKSRS) and proton beam therapy (PBT) in uveal melanoma. It produces cohort-specific tables, figures, workbooks, and validation outputs for efficacy, safety, repeat-radiation, and biomarker-validation workflows.

## Quick Start

This repository does not include the clinical source spreadsheet. To reproduce the analysis, you need a copy of the repository plus the shared project data file.

### Reproduce From Scratch

1. Clone the repository and enter it.

```sh
git clone https://github.com/nicholas-camarda/uveal-melanoma.git
cd uveal-melanoma
```

2. Install R package dependencies.

```sh
Rscript scripts/bootstrap_packages.R
```

3. Place the shared input spreadsheet in the expected raw-data folder.

Canonical source, runtime, raw-data, and publish paths are defined in `scripts/config/project_paths.R`.

In the maintainer's environment, the default raw-data folder is:

```text
~/Library/CloudStorage/OneDrive-Personal/Project Vault/Research/uveal-melanoma/Original Files/
```

The expected filename is the value of `INPUT_FILENAME` in `scripts/utils/config_constants.R`.

On another machine, override paths through the supported environment variables rather than editing tracked configuration. `OCULAR_RUNTIME_ROOT` sets the local runtime root, `OCULAR_EXPORT_PARENT_DIR` sets the Project Vault research parent, and `RAW_DATA_DIR` can set the raw-input directory directly. `PROCESSED_DATA_DIR`, `OUTPUT_DIR`, `LOGS_DIR`, `TOOLS_OUTPUT_DIR`, `TEST_OUTPUT_DIR`, `MERGED_TABLES_DIR`, `SHARE_PACKETS_DIR`, and `DATA_DICTIONARY_PATH` provide narrower overrides when required. All configured paths must be absolute.

4. Run the full pipeline.

```sh
Rscript -e "source('scripts/load_all.R'); main_execution()"
```

5. Review outputs.

Runtime outputs are written to:

```text
~/Workspaces/uveal-melanoma/runtime/
```

### Prerequisites

- R 4.4 or newer
- Access to the project input spreadsheet
- A local clone of this repository

### Run the full pipeline

From the repository root:

```sh
Rscript -e "source('scripts/load_all.R'); main_execution()"
```

### Run a single cohort or objective

```sh
Rscript -e "source('scripts/load_all.R'); run_my_analysis('uveal_melanoma_full_cohort')"
Rscript -e "source('scripts/load_all.R'); run_specific_objective('uveal_melanoma_full_cohort', 4)"
```

Available runtime dataset IDs:

- `uveal_melanoma_full_cohort`
- `uveal_melanoma_restricted_cohort`
- `uveal_melanoma_gksrs_only_cohort`

### Customize local runs with `scripts/main.R`

For repeated local use, you can also edit `scripts/main.R`.

- Set `cohorts_to_run` to choose which cohorts to analyze
- Set `objectives_to_run` to choose which objectives to run
- Uncomment `main_execution()` if you want that script to launch the full pipeline instead

Then run:

```sh
Rscript scripts/main.R
```

This is best treated as a local convenience entrypoint for custom runs. For reproducible documentation and shared instructions, prefer the explicit `Rscript -e "source('scripts/load_all.R'); ..."` commands above.

### Interactive use

If you prefer to work inside an R session:

```r
source("scripts/load_all.R")
main_execution()
```

### Run tests

```sh
# Portable regression suite
Rscript scripts/tools/run_testthat.R tests/testthat

# Local integration suite (requires local cohort data)
OCULAR_RUN_INTEGRATION_TESTS=true Rscript scripts/tools/run_testthat.R tests/integration
```

## Study Scope

The pipeline works with three intentionally overlapping analytic cohorts created from the same cleaned master dataset.

| Cohort | Runtime dataset id | Primary role |
|--------|--------------------|--------------|
| Full | `uveal_melanoma_full_cohort` | All-comers treatment cohort for real-world comparison |
| Restricted | `uveal_melanoma_restricted_cohort` | Dual-eligibility cohort for a more balanced treatment comparison |
| GKSRS-only | `uveal_melanoma_gksrs_only_cohort` | Patients ineligible for PBT, used to characterize GKSRS in challenging cases |

The current cohort counts and summary totals are written to `~/Workspaces/uveal-melanoma/runtime/Analytic Dataset/cohort_summary_statistics.json` whenever the pipeline is rerun.

The analysis is organized into four main research objectives:

| Objective | Focus | Primary outputs |
|-----------|-------|-----------------|
| 1 | Efficacy | Event summaries, survival outputs, tumor-height analyses, subgroup forest plots |
| 2 | Safety | Vision summaries, adverse-event models, diagnostics, effect-summary workbooks |
| 3 | Repeat radiation | PFS-2 summaries, survival outputs, skip artifacts when data are sparse |
| 4 | GEP validation | Consolidated workbooks, technical workbooks, narrative summaries, KM/CIF displays |

For a collaborator-facing overview of the study aims and eligibility logic, see [docs/OBJECTIVES.md](docs/OBJECTIVES.md).

## Output Map

Pipeline outputs are primarily written to the runtime analysis tree:

```text
~/Workspaces/uveal-melanoma/runtime/
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
```

Within each cohort folder, outputs follow a consistent layout:

- `00_General/`: baseline characteristics, cohort summaries, treatment-duration summaries, exclusion summaries, reconciliation audit workbooks (including manual date-correction audit sheets), and Objective 0 validation bundles
- `01_Efficacy/`: recurrence, metastasis, survival, tumor-height, and subgroup outputs; primary endpoint artifacts use typed `01_`–`06_` subfolders and subgroup outputs use `g_subgroup_analysis/`
- `02_Safety/`: vision and radiation-related adverse-event outputs (descriptive → adjusted models → effect summary; vision adds `04_sensitivity/`)
- `03_Repeat_Radiation/`: PFS-2 cohort support, survival artifacts, and PH diagnostics under `a_pfs2/`
- `04_GEP_Validation/`: Objective 4 workbooks, plots, and unified summaries

Objective 4 has a deliberate reading path:

1. Start here for the high-level purpose and output location.
2. Use [docs/STATISTICAL_METHODS.md](docs/STATISTICAL_METHODS.md#gep-validation-metrics) for the formal validation framework.
3. Use [docs/INTERPRETATION_GUIDE.md](docs/INTERPRETATION_GUIDE.md#understanding-gep-analysis) for workbook-first reading guidance.
4. Use [docs/TECHNICAL.md](docs/TECHNICAL.md#objective-4-gep-predictive-accuracy) for implementation and artifact contracts.

## Publish Workflow

Publishing is an optional manual step used in the maintainer's environment to copy selected deliverables from runtime output into a dated export snapshot. If you are reproducing the analysis locally, you can usually ignore this section and work directly from the runtime outputs under `~/Workspaces/uveal-melanoma/runtime/`.

Approved snapshots are published under `~/Library/CloudStorage/OneDrive-Personal/Project Vault/Research/uveal-melanoma/outputs/`. Raw inputs remain in the sibling `Original Files/` directory; generated and intermediate artifacts remain under the local runtime root.

```r
source("scripts/load_all.R")

# Review the candidate snapshot first
publish_outputs(dry_run = TRUE)

# Copy the approved deliverables into the dated export snapshot
publish_outputs(dry_run = FALSE)
```

The publish step copies only registry-approved deliverables. It excludes `.rds`, diagnostics workbooks, caches, logs, test output, and ad hoc intermediate files by design.

## Configuration

Analysis settings live in `scripts/utils/config_constants.R`; canonical filesystem roots and supported path overrides live in `scripts/config/project_paths.R`. Typical analysis settings to review before a fresh run are:

```r
INPUT_FILENAME <- "your_data_file.xlsx"
RECREATE_ANALYTIC_DATASETS <- FALSE
USE_LOGS <- TRUE
```

If you receive a shared spreadsheet with a different filename than `INPUT_FILENAME`, update that constant before running the pipeline.

Objective 4 grouping and display settings are also centralized there through `GEP_GROUPING_SPECS` and `GEP_OBJECTIVE4_GROUPING`.

## Documentation

- [docs/OBJECTIVES.md](docs/OBJECTIVES.md): study aims, objective definitions, subgroup scope, and cohort eligibility logic
- [docs/TECHNICAL.md](docs/TECHNICAL.md): workflow orchestration, directory structure, configuration, artifact contracts, and QA
- [docs/CALCULATIONS.md](docs/CALCULATIONS.md): derived-variable definitions, endpoint construction, and sign conventions
- [docs/STATISTICAL_METHODS.md](docs/STATISTICAL_METHODS.md): statistical methodology, assumptions, thresholds, and validation metrics
- [docs/INTERPRETATION_GUIDE.md](docs/INTERPRETATION_GUIDE.md): how to read tables, plots, workbooks, and Objective 4 deliverables
- [docs/FIGURE_COUNTS_AUDIT.md](docs/FIGURE_COUNTS_AUDIT.md): generated current-state figure/count audit from runtime artifacts
- [docs/dependency_diagram.md](docs/dependency_diagram.md): generated loader/workflow dependency map with full sourced inventory appendix
