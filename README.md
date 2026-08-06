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

2. Restore the locked R 4.4.3 package environment.

```sh
Rscript scripts/bootstrap_packages.R
```

The committed `renv.lock` is the source of truth for package versions. The
repository does not use npm or maintain a Node dependency lockfile.

3. Place the shared input spreadsheet in the expected raw-data folder.

Canonical source, runtime, raw-data, and publish paths are defined in `scripts/config/project_paths.R`. Keep the shared spreadsheet outside the repository; see the [path model](#path-model) for the default layout and configuration options.

The expected filename is the value of `INPUT_FILENAME` in `scripts/utils/config_constants.R`.

On another machine, use the supported absolute-path overrides described in the [path model](#path-model) rather than editing tracked configuration.

4. Run the full pipeline.

```sh
Rscript -e "source('scripts/load_all.R'); main_execution()"
```

5. Review outputs.

The [path model](#path-model) below shows the default output layout and how to relocate it on another machine.

### Prerequisites

- R 4.4.3
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
# Fast, data-free suite used by the required pull-request check
Rscript scripts/tools/run_testthat.R tests/testthat --filter '^(ci_contract|path_runtime_publish|forest_plot_labels|objective0_data_processing_portable|objective1_subgroup_policy|objective4_gep_analysis_portable|sparse_factor_handling|propensity_score_sensitivity|synthetic_fixture_contract)$'
Rscript scripts/tools/run_testthat.R tests/integration --filter 'portable_smoke'

# Full data-free regression suite
Rscript scripts/tools/run_testthat.R tests/testthat
Rscript scripts/tools/run_testthat.R tests/integration --filter 'portable_smoke'

# Broader local integration suite (requires the private cohort data)
OCULAR_RUN_INTEGRATION_TESTS=true Rscript scripts/tools/run_testthat.R tests/integration
```

The synthetic fixture exercises portable schema, missingness, censoring, GEP,
PRAME, treatment, sparse-group, and one-arm paths. It is not a substitute for
the local data-backed validation of scientific estimates.

## Path Model

`scripts/config/project_paths.R` is the only source of truth for filesystem paths. The pipeline resolves complete absolute paths there; do not edit individual output locations. Configure the relevant root instead.

The runtime root is local and generated artifacts stay below it. Its default child locations are fixed:

```text
RUNTIME_ROOT                         # Override with OCULAR_RUNTIME_ROOT
├── Analytic Dataset/                # PROCESSED_DATA_DIR
├── Analysis/                        # OUTPUT_DIR
│   └── merged_tables/               # MERGED_TABLES_DIR
├── logs/                            # LOGS_DIR
├── tools_output/                    # TOOLS_OUTPUT_DIR
├── test_output/                     # TEST_OUTPUT_DIR
└── share_packets/                   # SHARE_PACKETS_DIR
```

The raw-input and published-output locations share a separately configured export root:

```text
EXPORT_ROOT                           # Derived from OCULAR_EXPORT_PARENT_DIR
├── Original Files/                  # RAW_DATA_DIR
│   └── Data Dictionary.xlsx         # DATA_DICTIONARY_PATH
└── outputs/                         # EXPORT_ANALYSIS_DIR
    └── <snapshot-id>/
```

On a new machine, set `OCULAR_RUNTIME_ROOT` and `OCULAR_EXPORT_PARENT_DIR` to absolute paths. Use `RAW_DATA_DIR` only when the input spreadsheet lives somewhere else. The narrower `PROCESSED_DATA_DIR`, `OUTPUT_DIR`, `LOGS_DIR`, `TOOLS_OUTPUT_DIR`, `TEST_OUTPUT_DIR`, `MERGED_TABLES_DIR`, `SHARE_PACKETS_DIR`, and `DATA_DICTIONARY_PATH` overrides are available for an intentional nonstandard layout. `scripts/config/project_paths.R` contains the maintained default root values for the maintainer environment.

## Study Scope

The pipeline works with three intentionally overlapping analytic cohorts created from the same cleaned master dataset.

| Cohort | Runtime dataset id | Primary role |
|--------|--------------------|--------------|
| Full | `uveal_melanoma_full_cohort` | All-comers treatment cohort for real-world comparison |
| Restricted | `uveal_melanoma_restricted_cohort` | Dual-eligibility cohort for a more balanced treatment comparison |
| GKSRS-only | `uveal_melanoma_gksrs_only_cohort` | Patients ineligible for PBT, used to characterize GKSRS in challenging cases |

The current cohort counts and summary totals are written to `Analytic Dataset/cohort_summary_statistics.json` whenever the pipeline is rerun (see the [path model](#path-model)).

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
Runtime output root
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
- `01_Efficacy/`: recurrence, metastasis, survival, tumor-height, and subgroup outputs; primary endpoint artifacts use typed `01_`–`06_` subfolders and subgroup outputs use `g_subgroup_analysis/`. Only the restricted cohort also has `h_propensity_score_sensitivity/`, containing the overlap-weighted sensitivity workbook, plots, narrative summary, and technical audit.
- `02_Safety/`: vision and radiation-related adverse-event outputs (descriptive → adjusted models → effect summary; vision adds `04_sensitivity/`)
- `03_Repeat_Radiation/`: PFS-2 cohort support, survival artifacts, and PH diagnostics under `a_pfs2/`
- `04_GEP_Validation/`: Objective 4 workbooks, plots, and unified summaries

Objective 4 has a deliberate reading path:

1. Start here for the high-level purpose and output location.
2. Use [docs/STATISTICAL_METHODS.md](docs/STATISTICAL_METHODS.md#gep-validation-metrics) for the formal validation framework.
3. Use [docs/INTERPRETATION_GUIDE.md](docs/INTERPRETATION_GUIDE.md#understanding-gep-analysis) for workbook-first reading guidance.
4. Use [docs/TECHNICAL.md](docs/TECHNICAL.md#objective-4-gep-predictive-accuracy) for implementation and artifact contracts.

## Publish Workflow

Publishing is an optional manual step that copies selected deliverables from the runtime analysis directory into a dated export snapshot. If you are reproducing the analysis locally, you can usually ignore this section and work directly from the runtime outputs.

Raw inputs remain in the configured input directory; generated and intermediate artifacts remain in the local runtime tree. Approved snapshots use `outputs/<snapshot-id>/` under the configured export root and never overwrite an existing snapshot. See the [path model](#path-model) for the exact defaults.

```r
source("scripts/load_all.R")

# Review the candidate snapshot first
publish_outputs(dry_run = TRUE)

# Copy the approved deliverables into the dated export snapshot
publish_outputs(dry_run = FALSE)
```

The publish step copies only registry-approved deliverables. It excludes `.rds`, diagnostics workbooks, caches, logs, test output, and ad hoc intermediate files by design.

The direct command interface is dry-run by default:

```sh
Rscript scripts/workflow/publish_outputs.R --dry-run
Rscript scripts/workflow/publish_outputs.R --execute --snapshot-id YYYY-MM-DD
```

Use `--cohorts cohort1,cohort2` to limit the copy and `--no-merged-tables` to omit merged tables. `--execute` performs the copy and writes `publish_manifest.csv`; review the dry-run report first.

## Configuration

Analysis settings live in `scripts/utils/config_constants.R`; canonical filesystem roots and supported path overrides live in `scripts/config/project_paths.R`. Typical execution settings to review before a fresh run are:

```r
RECREATE_ANALYTIC_DATASETS <- FALSE
USE_LOGS <- TRUE
```

`INPUT_FILENAME` is maintained in the data-processing policy. If the shared spreadsheet has a different name, provide it at the configured raw-data location under that expected name, or make a deliberate local configuration change before recreating analytic datasets.

Objective 4 grouping and display settings are also centralized there through `GEP_GROUPING_SPECS` and `GEP_OBJECTIVE4_GROUPING`.

## Documentation

- [docs/OBJECTIVES.md](docs/OBJECTIVES.md): study aims, objective definitions, subgroup scope, and cohort eligibility logic
- [docs/TECHNICAL.md](docs/TECHNICAL.md): workflow orchestration, directory structure, configuration, artifact contracts, and QA
- [docs/CALCULATIONS.md](docs/CALCULATIONS.md): derived-variable definitions, endpoint construction, and sign conventions
- [docs/STATISTICAL_METHODS.md](docs/STATISTICAL_METHODS.md): statistical methodology, assumptions, thresholds, and validation metrics
- [docs/INTERPRETATION_GUIDE.md](docs/INTERPRETATION_GUIDE.md): how to read tables, plots, workbooks, and Objective 4 deliverables
- [docs/FIGURE_COUNTS_AUDIT.md](docs/FIGURE_COUNTS_AUDIT.md): generated current-state figure/count audit from runtime artifacts
- [docs/dependency_diagram.md](docs/dependency_diagram.md): generated loader/workflow dependency map with full sourced inventory appendix
