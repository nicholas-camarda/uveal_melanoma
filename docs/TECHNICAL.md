# Technical Documentation

This document is the implementation reference for the repository. It covers workflow orchestration, directory structure, configuration, artifact contracts, and quality-assurance behavior.

For other documentation roles:

- Use [CALCULATIONS.md](CALCULATIONS.md) for derived-variable definitions and endpoint construction.
- Use [STATISTICAL_METHODS.md](STATISTICAL_METHODS.md) for the canonical statistical methodology.
- Use [INTERPRETATION_GUIDE.md](INTERPRETATION_GUIDE.md) for workbook, table, and figure interpretation.
- Use [README.md](../README.md) for the GitHub-facing quick start and top-level path map.

---

## Table of Contents

- [Documentation Boundaries](#documentation-boundaries)
- [Workflow Orchestration System](#workflow-orchestration-system)
- [Cohort Definitions](#cohort-definitions)
   - [Dataset Identities and Construction](#dataset-identities-and-construction)
  - [Vital Status and Follow-up Classification](#vital-status-and-follow-up-classification)
- [Directory Structure](#directory-structure)
- [Data Processing Workflow](#data-processing-workflow)
- [Quality Assurance](#quality-assurance)
- [Data Limitations](#data-limitations)
- [Research Objectives](#research-objectives)
- [Subgroup Filtering](#subgroup-filtering)

---

## Documentation Boundaries

The core documentation set is intentionally split by role so updates have one primary home.

| Topic | Primary home | What stays here |
|-------|--------------|-----------------|
| Running the project and finding outputs | `README.md` | Short overview, entry commands, output map, and links outward |
| Derived variables and endpoint definitions | `CALCULATIONS.md` | Cross-links only when implementation context is needed |
| Statistical methods, assumptions, and thresholds | `STATISTICAL_METHODS.md` | References to scripts or artifact contracts only when needed for implementation clarity |
| Output-reading guidance | `INTERPRETATION_GUIDE.md` | References to where artifacts are generated, not how to interpret them |
| Workflow internals and artifact contracts | `TECHNICAL.md` | This document |

When content overlaps, this document should keep the implementation and storage contract and defer formula definitions, inferential rationale, and reader guidance to the documents above.

---

## Workflow Orchestration System

The analysis employs a modular workflow system for flexibility and robust error handling.

### Core Execution Functions

| Function | Purpose | Use Case |
|----------|---------|----------|
| `main_execution()` | Complete pipeline execution | Full analysis for all cohorts and objectives |
| `run_my_analysis(dataset_name)` | Cohort-specific analysis | All objectives for a specific patient cohort |
| `run_specific_objective(dataset_name, objective_number)` | Targeted analysis | Single objective for focused research questions |

### Objective-Specific Workflow Scripts

Each research objective has a dedicated workflow script:

| Objective | Script | Primary Functions | Analysis Type |
|-----------|--------|-------------------|---------------|
| **0: Data Processing** | `objective_0_data_processing.R` | Data cleaning, validation, cohort creation | Data preparation |
| **1: Efficacy Analysis** | `objective_1_primary_outcomes.R` | Survival analysis, tumor height, subgroups | Primary outcomes |
| **2: Safety/Toxicity** | `objective_2_safety_toxicity.R` | Vision changes, complications | Safety endpoints |
| **3: Repeat Radiation** | `objective_3_repeat_radiation.R` | PFS-2 analysis, diagnostics | Second-line treatment |
| **4: GEP Validation** | `objective_4_gep_analysis.R` | Predictive accuracy testing | Biomarker validation |

---

## Cohort Definitions

The analysis employs three patient cohorts to address different clinical questions and minimize treatment selection bias.

### Full Cohort

**Definition:** All patients who received either GKSRS or PBT

**Purpose:** Real-world effectiveness comparison across the complete spectrum of tumor characteristics

**Clinical Value:** Provides comprehensive treatment effectiveness data for real-world decision making

### Restricted Cohort

**Definition:** Patients eligible for **both** treatment modalities

**Eligibility Criteria:**
- Tumor diameter ≤20mm
- Tumor height ≤10mm
- No optic nerve involvement
- Suitable for both GKSRS and PBT

**Purpose:** Balanced comparison minimizing treatment selection bias

**Clinical Value:** Direct treatment comparison in patients where both options are clinically appropriate

### GKSRS-Only Cohort

**Definition:** Patients **ineligible** for PBT

**Exclusion Criteria:**
- Tumor diameter >20mm, OR
- Tumor height >10mm, OR
- Optic nerve involvement

**Purpose:** GKSRS effectiveness assessment in challenging cases

**Clinical Value:** Demonstrates GKSRS utility in patients where PBT is not feasible

### Cohort Selection Rationale

This three-cohort design addresses key clinical and methodological challenges:

- **Treatment Selection Bias:** Restricted cohort provides balanced comparison
- **Real-World Applicability:** Full cohort reflects actual clinical practice
- **Treatment Limitations:** GKSRS-only cohort shows effectiveness in challenging cases
- **Statistical Power:** Adequate sample sizes for robust statistical analysis

### Dataset Identities and Construction

The analytic datasets in `~/ProjectsRuntime/uveal_melanoma/Analytic Dataset/` are not separate raw sources. They are three derived views of the same cleaned and fully processed master table, created once in Objective 0 and then reused by Objectives 1-4.

| Dataset file | Runtime dataset id | Output folder | What it means | How it is constructed | Why it exists |
|--------------|--------------------|---------------|---------------|-----------------------|---------------|
| `uveal_melanoma_full_cohort.rds` | `uveal_melanoma_full_cohort` | `uveal_full/` | Canonical all-comers treatment cohort | Start from the cleaned master dataset, apply global exclusions, derive all analytic variables once, then retain every patient treated with either GKSRS or PBT | Preserves the real-world treatment population and serves as the broadest cohort for descriptive and outcome analyses |
| `uveal_melanoma_restricted_cohort.rds` | `uveal_melanoma_restricted_cohort` | `uveal_restricted/` | Dual-eligibility comparison cohort | Subset the full cohort to patients who meet the predefined criteria for both modalities: tumor diameter `<= 20 mm`, tumor height `<= 10 mm`, and no optic nerve involvement | Minimizes treatment-selection bias when directly comparing GKSRS and PBT |
| `uveal_melanoma_gksrs_only_cohort.rds` | `uveal_melanoma_gksrs_only_cohort` | `gksrs/` | Modality-limited challenging-case cohort | Subset the full cohort to patients who fail PBT eligibility because of tumor diameter `> 20 mm`, tumor height `> 10 mm`, or optic nerve involvement | Isolates the population in which GKSRS may still be clinically feasible when PBT is not |

These three files should be interpreted as intentionally overlapping analytic cohorts rather than mutually independent studies:

- The **full cohort** is the parent treatment cohort.
- The **restricted cohort** is the clinically balanced subset of the full cohort.
- The **GKSRS-only cohort** is the clinically excluded-from-PBT subset of the full cohort.

The additional files in `~/ProjectsRuntime/uveal_melanoma/Analytic Dataset/` support consistent downstream reporting:

| Supporting file | What it contains | Why it is saved |
|-----------------|------------------|-----------------|
| `*_derived_precollapse.rds` | Cohort-specific analytic data before sparse factor levels are collapsed for modeling | Preserves original factor levels so merged baseline tables and review outputs can stay aligned with cohort-specific tables |

For semantically meaningful GEP fields such as `biopsy1_gep`, `gep_class_simple`, `prame_status`, and `gep12_prame_status`, the pipeline now uses a two-layer contract:

- Post-collapse cohort `.rds` files remain the model-facing artifacts used for sparse-category protection.
- Reader-facing outputs restore those GEP variables from the matching `*_derived_precollapse.rds` artifact when it exists, so plots, workbooks, and simple QC tables show the canonical GEP labels.

Construction happens in a fixed order:

1. Load and clean the raw spreadsheet.
2. Derive dates, follow-up, endpoints, treatment flags, GEP fields, and other analysis variables once.
3. Apply global exclusions before cohort assignment.
4. Save the full cohort.
5. Split that parent cohort into the restricted and GKSRS-only datasets using the predefined eligibility rules above.
6. Save supporting `*_derived_precollapse.rds` artifacts for output consistency.

### Vital Status and Follow-up Classification

**Data Cutoff:** March 4, 2025 (per data dictionary)

Patients are classified into three vital status categories for summary reporting:

| Status | Definition | Criterion |
|--------|------------|-----------|
| **Dead** | Death occurred | `death_event = 1` |
| **Alive** | Under active follow-up | `death_event = 0` AND last contact ≤450 days (~15 months) from cutoff |
| **Lost to Follow-up** | No recent contact | `death_event = 0` AND last contact >450 days from cutoff |

**Rationale for 450-Day Cutoff:**
- Accounts for typical ophthalmology follow-up intervals (6-12 months)
- Balances clinical reality of scheduled monitoring versus true loss to follow-up
- Empirically determined to best separate actively followed versus lost patients

**Survival Analysis Treatment:**
- Both "alive" and "lost to follow-up" patients are censored at `last_known_alive_date`
- Lost to follow-up does not indicate missing data; these patients contributed valid follow-up time
- Total person-years of follow-up includes all patients up to their last documented contact

**Implementation:** `scripts/utils/cohort_summary_export.R` with cutoff constants centralized in `scripts/utils/config_constants.R`

**See also:** [Lost to Follow-up Classification](CALCULATIONS.md#lost-to-follow-up-classification) for detailed calculation methodology

---

## Directory Structure

### Output Organization

Analysis outputs follow a **cohort → objective → sub-objective** structure:

```
~/ProjectsRuntime/uveal_melanoma/
├── Analytic Dataset/               # Processed RDS files and runtime metadata
│   ├── uveal_melanoma_full_cohort.rds
│   ├── uveal_melanoma_restricted_cohort.rds
│   ├── uveal_melanoma_gksrs_only_cohort.rds
│   └── *_derived_precollapse.rds
├── Analysis/                       # Runtime analysis outputs by cohort
│   ├── uveal_full/                 # Full cohort runtime outputs
│   │   ├── 00_General/
│   │   │   ├── cohort_summary.tsv
│   │   │   ├── cohort_summary.txt
│   │   │   ├── baseline_characteristics/
│   │   │   ├── treatment_duration/
│   │   │   └── removed_patients_summary.tsv
│   │   ├── 01_Efficacy/
│   │   │   ├── a_recurrence/
│   │   │   ├── b_metastatic_progression/
│   │   │   ├── c_overall_survival/
│   │   │   ├── d_progression_free_survival/
│   │   │   ├── e_tumor_height_primary/
│   │   │   ├── f_tumor_height_sensitivity/
│   │   │   ├── g_subgroup_analysis/
│   │   │   │   ├── tumor_height_primary/
│   │   │   │   ├── tumor_height_sensitivity/
│   │   │   │   └── forest_plots/
│   │   │   └── h_proportional_hazards_diagnostics/
│   │   ├── 02_Safety/
│   │   │   ├── a_vision_changes/
│   │   │   ├── b_retinopathy/
│   │   │   ├── c_neovascular_glaucoma/
│   │   │   └── d_serous_retinal_detachment/
│   │   ├── 03_Repeat_Radiation/
│   │   │   ├── a_pfs2/
│   │   │   └── b_proportional_hazards_diagnostics/
│   │   └── 04_GEP_Validation/
│   │       ├── a_metastasis_free_survival/
│   │       └── b_melanoma_specific_survival/
│   ├── uveal_restricted/           # Restricted cohort runtime outputs
│   ├── gksrs/                      # GKSRS-only cohort runtime outputs
│   └── merged_tables/              # Cross-cohort comparisons
├── logs/                           # Execution logs
│   ├── txt/
│   └── json/
├── test_output/                    # Testing artifacts
└── tools_output/                   # Documentation/audit tool artifacts

~/Library/CloudStorage/OneDrive-Personal/Research/uveal_melanoma/
├── Original Files/                 # Authoritative raw input data
└── Analysis/
    └── <YYYY-MM-DD>/               # Published final deliverables only
```

### Script Organization

The current script tree is modularized by responsibility. `scripts/load_all.R` is the canonical loader; `scripts/main.R` is a convenience wrapper rather than the primary documentation entry point.

```
scripts/
├── bootstrap_packages.R            # Explicit dependency bootstrap/install step
├── load_all.R                      # Load packages, config, and project modules
├── main.R                          # Optional convenience wrapper for interactive use
├── analysis/                       # Objective 1-3 modeling and summary engines
│   ├── binary_outcomes.R
│   ├── rmst_visualization.R
│   ├── survival_outcomes.R
│   ├── tumor_height_analysis.R
│   └── vision_safety_analysis.R
├── data_helper/                    # Raw-data loading, derivation, cohort creation
│   ├── cohort_creation.R
│   ├── cohort_orchestration.R
│   ├── data_derivation.R
│   ├── data_loading.R
│   ├── data_summaries.R
│   ├── data_utilities.R
│   └── gep_missing_data_analysis.R
├── gep/                            # Objective 4 evaluation, reporting, visuals
│   ├── cores/
│   ├── orchestration/
│   ├── reporting/
│   ├── utils/
│   └── visualization/
├── subgroup/                       # Subgroup data prep, modeling, formatting
│   ├── subgroup_binary.R
│   ├── subgroup_data_prep.R
│   ├── subgroup_formatting.R
│   ├── subgroup_height.R
│   └── subgroup_survival.R
├── tables/                         # Regression-table generation pipeline
│   ├── table_diagnostics.R
│   ├── table_formatting.R
│   ├── table_generation_core.R
│   ├── table_io.R
│   └── table_model_fitting.R
├── tools/                          # Documentation and diagnostic utilities
├── utils/                          # Shared config, logging, output, validation helpers
├── visualization/                  # Forest-plot generation helpers
└── workflow/                       # Objective orchestration and publishing
```

---

## Data Processing Workflow

### Pipeline Overview

1. **Data Loading** (`load_and_clean_data()`)
   - Read raw Excel file
   - Initial data validation
   - Type conversions
   - Date parsing

2. **Data Processing** (`create_analytic_dataset()`)
   - Variable derivation (see [CALCULATIONS.md](CALCULATIONS.md))
   - Factor level standardization
   - Missing data handling
   - Derived variable creation

3. **Cohort Creation** (`apply_inclusion_exclusion_criteria()`)
   - Apply eligibility criteria
   - Assign cohort memberships
   - Document exclusions
   - Create `removed_patients_summary.tsv`

4. **Save Datasets** 
   - Store processed RDS files
   - Save pre-collapsed factor levels
   - Write `cohort_summary.tsv` and `cohort_summary.txt` into each cohort's `00_General/` directory
   - Write `{cohort_name}_validation_summary.txt` and `{cohort_name}_validation_bundle.xlsx` into each cohort's `00_General/` directory
   - Refresh generated study docs when Objective 0 finishes without hard validation errors

5. **Run Analyses**
   - Load RDS datasets
   - Execute objective-specific workflows
   - Generate outputs

### Tool Refresh Outputs

Documentation-oriented utilities under [scripts/tools](../scripts/tools) write their canonical runtime artifacts to `~/ProjectsRuntime/uveal_melanoma/tools_output/`, which is the path behind `TOOLS_OUTPUT_DIR` in [scripts/utils/config_constants.R](../scripts/utils/config_constants.R).

The current refresh entry point is [scripts/tools/run_tool_refreshes.R](../scripts/tools/run_tool_refreshes.R). It orchestrates the documentation-focused tools, writes per-tool run summaries, and leaves behind a suite-level manifest so periodic refreshes can be audited without opening the workbooks themselves.

Current canonical outputs from the refreshed tool suite include:

- `derived_variables_documentation.xlsx`
- `derived_variables_documentation_validation.csv`
- `comprehensive_variable_census.xlsx`
- `comprehensive_variable_census.rds`
- `comprehensive_variable_census.html`
- `docs/dependency_diagram.md`
- `docs/FIGURE_COUNTS_AUDIT.md`
- timestamped `*_run_*_summary.csv` and `*_run_*_summary.txt` files for each tool execution

These files are treated as documentation and audit artifacts, not analysis outputs. They should stay synchronized with the active workflow and be regenerated when the derived-variable catalog, variable census, or tool logic changes.

See [README.md](../README.md) for the top-level execution entry points and output map. The step list above is the canonical technical sequence for how data move through the pipeline.

---

## Quality Assurance

### Data Quality Checkpoints

**Cohort Curation:**
- Stage IV cases (IDs 7, 116, 262) removed before cohort assignment
- Manually excluded records (ID 271) documented
- `removed_patients_summary.tsv` exported to each cohort's `00_General/` directory
- `cohort_summary.tsv` and `cohort_summary.txt` exported to each cohort's `00_General/` directory
- Reviewers can audit exclusion decisions

**Data Validation:**
- Automatic checks for data integrity
- Type validation for all variables
- Range checks for continuous variables
- Consistency checks across related fields
- Structured Objective 0 findings classified as `hard_error`, `warning`, or `info`
- Endpoint chronology hard errors preserve impossible negative event times in diagnostics rather than silently clamping them
- Objective 1-4 input readiness is checked from a centralized Objective 0 downstream-variable registry
- Reload-mode validation reuses persisted reconciliation and manual-date-correction audit sheets when raw recreation is skipped
- Cohort-level validation bundles published into `00_General/` for reviewer audit

**Factor Level Management:**
- Consistent handling of categorical variables
- Centralized level labels in `config_constants.R`
- Objective 0 owns canonical factor construction; downstream code may preserve, restore, or drop levels for display, but model-facing ad hoc factor coercion is rejected by the factor-level audit
- Pre-collapse data preservation for baseline tables

**Cohort Assignment:**
- Automated application of inclusion/exclusion criteria
- Documented eligibility logic
- Audit trail for all exclusions

**Output Validation:**
- Error handling at each step
- Comprehensive logging
- Graceful degradation when analyses cannot complete
- Clear error messages with reasons

### Calculation Consistency

All derived variables calculated once in Objective 0 (`data_derivation.R`) to ensure:

- Single source of truth
- Consistency between individual and merged tables
- Maintainability (one place to update formulas)
- Auditability (clear calculation logic)
- Merged baseline outputs now preserve the legacy full-vs-restricted files and add a separately named all-three-cohort comparison

See [CALCULATIONS.md](CALCULATIONS.md) for detailed variable derivation formulas.

---

## Data Limitations

The analysis pipeline includes robust error handling for situations where data limitations prevent certain analyses from completing.

### Cohort-Specific Limitations

#### GKSRS-Only Cohort

**Objective 3 (PFS-2 Analysis):** This cohort can fail the PFS-2 guardrails because valid second-recurrence follow-up and events are sparse.
- Summary tables are still generated.
- Survival curves and Cox models are skipped when fewer than 10 analyzable patients or fewer than 5 total events remain.
- Skip artifacts are written intentionally so the absence of a model is explicit rather than silent.

#### Restricted Cohort

- Generally sufficient sample size for most analyses
- Occasional rare category handling in subgroup analyses due to smaller size than full cohort
- Optic nerve abutment excluded from baseline tables (all patients have optic_nerve="N" by eligibility)

#### Full Cohort

- Generally sufficient sample size for most analyses
- Occasional rare category handling in subgroup analyses

#### Objective 4 (GEP Validation) Denominator Constraints

- Main MFS/MSS validation subsets include only definitive raw DecisionDx Class 1 / Class 2 labels with valid endpoint-specific imported GEP probabilities.
- Nondefinitive labels (`Failed`, `Unknown`, discordant, and not-reported patterns) are intentionally excluded from primary Objective 4 denominators.
- Sparse definitive-label distributions can limit some horizon-specific summary metrics or PRAME comparisons in smaller cohorts; the pipeline writes explanatory outputs rather than silently dropping sections.

### Automatic Error Handling

**Minimum Event Requirements:**
- Adjusted adverse-event logistic models require 10+ events
- PFS-2 survival analyses require 10+ analyzable patients and 5+ total events
- Proportional-hazards diagnostics use a 10-event reporting floor
- Survival analyses require 5+ total events
- Cox regression requires 2+ groups with events
- Logistic regression requires adequate observations per category

**Rare Category Management:**
- Sparse or explicitly excluded levels are removed from model-specific analysis copies and documented in diagnostics.
- Variables with insufficient levels after sparse-level handling are excluded from affected models.
- GEP display variables use canonical labels; `Other` is not a valid raw or display GEP label in current artifacts.

**Graceful Degradation:**
- When full analyses cannot be completed, summary statistics still generated
- Missing analyses documented in logs with specific reasons
- Analysis-not-performed files created with explanations

**Comprehensive Logging:**
- All limitations and skipped analyses logged with timestamps
- Detailed error messages explain why analyses were skipped
- Both text and JSON logs available

---

## Research Objectives

For a collaborator-facing overview of the study aims, subgroup scope, and cohort eligibility logic, use [OBJECTIVES.md](OBJECTIVES.md). This section remains the implementation-facing contract for how those objectives are executed in the current pipeline.

### Objective 0: Data Processing and Validation

Objective 0 is the upstream preparation stage for every downstream cohort and analysis. It owns raw-data cleaning, validation, derived-variable creation, cohort construction, and the publication of cohort-level audit artifacts into each cohort's `00_General/` directory.

**Objective 0 audit-trail note:** loader-side event/date reconciliations are published into each cohort's `00_General/` directory as a single stable workbook named `{cohort_name}_event_data_reconcilitation.xlsx`, alongside the cohort summary and removed-patient artifacts. That workbook now carries both the event/date reconciliation sheets and a `Manual_Date_Corrections` sheet for any versioned raw-date corrections applied during loading.
The manual-correction sheet includes the corrected field, rationale, confidence tier, supporting columns, supporting values, and simple support-gap metrics so reviewers can see whether the corrected value improves local chronology rather than relying on an undocumented override.

**Objective 0 chronology note:** treatment-before-diagnosis gaps larger than `7` days are hard-stop validation failures. Reverse-order gaps of `1-7` days are retained as warnings and published for manual review in the validation bundle instead of being silently rewritten.

### Objective 1: Efficacy Analysis (COMPLETE)

| Sub-objective | Method | Implementation | Outputs | Location |
|---------------|--------|----------------|---------|----------|
| **1a. Local Recurrence** | Time-to-event analysis with descriptive event support | `analyze_time_to_event_outcomes()` plus event-support summaries | Cox models/effect summaries (.html/.xlsx), PH diagnostics, KM plots (.png), descriptive event-support workbooks (.xlsx) | `{cohort}/01_Efficacy/a_recurrence/` |
| **1b. Metastatic Progression** | Time-to-event analysis with descriptive event support | `analyze_time_to_event_outcomes()` plus event-support summaries | Cox models/effect summaries (.html/.xlsx), PH diagnostics, KM plots (.png), descriptive event-support workbooks (.xlsx) | `{cohort}/01_Efficacy/b_metastatic_progression/` |
| **1c. Overall Survival** | Kaplan-Meier + Cox regression + RMST analysis | `analyze_time_to_event_outcomes()` | Survival tables (.xlsx), Cox models (.html), `overall_survival_probability_effect_summary.xlsx`, survival curves (.png), RMST plots (.png) | `{cohort}/01_Efficacy/c_overall_survival/` |
| **1d. Progression-Free Survival** | Composite endpoint (local recurrence, metastatic progression, or death) | `analyze_time_to_event_outcomes()` | Survival tables (.xlsx), Cox models (.html), `progression_free_survival_probability_effect_summary.xlsx`, survival curves (.png), RMST plots (.png) | `{cohort}/01_Efficacy/d_progression_free_survival/` |
| **1e. Tumor Height (Primary)** | Linear regression without baseline adjustment | `analyze_tumor_height_changes()` | Change summaries (.html), regression models (.html) | `{cohort}/01_Efficacy/e_tumor_height_primary/` |
| **1f. Tumor Height (Sensitivity)** | Linear regression with baseline adjustment | `analyze_tumor_height_changes()` | Change summaries (.html), regression models (.html) | `{cohort}/01_Efficacy/f_tumor_height_sensitivity/` |
| **1g. Subgroup Analysis** | Interaction testing across patient subgroups | `analyze_treatment_effect_subgroups_*()` | Subgroup tables (.xlsx), forest plots (.png), diagnostics (.xlsx) | `{cohort}/01_Efficacy/g_subgroup_analysis/` |

Legacy exploratory note: recurrence-stratified and metastasis-stratified OS/PFS subfolders can also appear under `a_recurrence/` and `b_metastatic_progression/`. These are retained historical one-off post-baseline summaries, not part of the formal Objective 1 contract, and should not be interpreted as valid baseline treatment comparisons.

### Objective 2: Safety/Toxicity Analysis (COMPLETE)

| Sub-objective | Method | Implementation | Outputs | Location |
|---------------|--------|----------------|---------|----------|
| **2a. Vision Changes** | Descriptive logMAR/Snellen reporting plus adjusted linear and ordinal regression | `analyze_visual_acuity_changes()` | `vision_changes.html`, descriptive Snellen summary/distribution workbooks, adjusted LogMAR linear model (.html + diagnostics), adjusted Snellen Line Change linear model (.html + diagnostics), adjusted Snellen Line Change Distribution ordinal model (.html + diagnostics), and `vision_effect_summary.xlsx` | `{cohort}/02_Safety/a_vision_changes/` |
| **2b. Radiation Retinopathy** | Recorded burden-by-follow-up logistic analysis | `analyze_radiation_complications()` | Complication rates (.xlsx), adjusted logistic model (.html + diagnostics), `retinopathy_effect_summary.xlsx`, or explicit skip artifact when model not fit | `{cohort}/02_Safety/b_retinopathy/` |
| **2c. Neovascular Glaucoma** | Recorded burden-by-follow-up logistic analysis | `analyze_radiation_complications()` | Complication rates (.xlsx), adjusted logistic model (.html + diagnostics), `neovascular_glaucoma_effect_summary.xlsx`, or explicit skip artifact when model not fit | `{cohort}/02_Safety/c_neovascular_glaucoma/` |
| **2d. Serous Retinal Detachment** | Recorded burden-by-follow-up logistic analysis (all recorded SRD causes in the published implementation) | `analyze_radiation_complications()` | Complication rates (.xlsx), adjusted logistic model (.html + diagnostics), `serous_retinal_detachment_effect_summary.xlsx`, or explicit skip artifact when model not fit | `{cohort}/02_Safety/d_serous_retinal_detachment/` |

Effect-summary workbooks follow model-family-specific inference conventions and should match the corresponding HTML tables: linear rows report mean differences with Wald CIs/p-values, logistic rows report ORs with model-based Wald CIs and the pipeline's standard term-level p-values, Cox rows report HRs with native Cox CIs/p-values, and ordinal rows report proportional-odds ORs with 95% Wald CIs and likelihood-ratio-test p-values. Objective 2 toxicity rows consume Objective 0-prepared burden fields (`retinopathy_burden_event`, `nvg_burden_event`, `srd_burden_event`) and label them as recorded burden by available follow-up, not time-to-toxicity incidence.

**Objective 2 output convention:** adjusted analyses now always live inside their own side-effect subfolder. When an adjusted model is skipped because of insufficient events, no usable variation, or fit failure, the pipeline writes a `_SKIPPED.html` explanation file plus the diagnostics workbook instead of leaving the folder without an adjusted-analysis artifact.

**Objective 2d scope:** SRD outputs keep all recorded SRD causes, including mass-induced SRD when present.

### Objective 3: Repeat Radiation Efficacy (COMPLETE)

**3a. Progression-Free Survival-2 (PFS-2)**

**Method:** Survival analysis for patients with local recurrence receiving second-line treatment

**Purpose:** Evaluate effectiveness of second-line radiation treatments for patients who experience local recurrence after initial therapy

**Implementation:** `analyze_pfs2()` in `scripts/workflow/objective_3_repeat_radiation.R`

**Outputs:** PFS-2 characteristics tables (.xlsx), survival curves (.png), Cox models (.html)

**Location:** `{cohort}/03_Repeat_Radiation/a_pfs2/`

**Note:** Analysis automatically skips survival modeling when insufficient events present (minimum: 5 total events across 2+ treatment groups)

### Objective 4: GEP Predictive Accuracy

**Purpose:** Validate externally supplied lab-reported GEP survival probabilities against real outcomes for metastasis-free survival (MFS) and melanoma-specific survival (MSS)

**Location:** `{cohort}/04_GEP_Validation/a_metastasis_free_survival/` and `b_melanoma_specific_survival/`

**Implementation:** `scripts/workflow/objective_4_gep_analysis.R`

**Implementation contract:**
- Objective 4 validates imported lab-reported predictions; it does not fit a new base prognostic model.
- The 5-year predictions come from `biopsy1_gep_mfs` / `biopsy1_gep_mss`, and preprocessing derives 7-year and 10-year expected survival from the same 5-year values.
- Primary validation denominators are restricted to definitive raw DecisionDx labels with valid endpoint-specific prediction fields.
- Main endpoints are metastasis events (MFS) and melanoma-specific death (MSS); companion MSS competing-risk analyses handle non-melanoma death explicitly.
- Reader-facing 5-year MFS "actual" values are now censoring-aware Kaplan-Meier estimates at 60 months; raw by-60-month event counts remain available only as descriptive sensitivity outputs.
- MSS observed-versus-expected summaries use censoring-aware Aalen-Johansen CIF pseudo-event counts at the horizon (melanoma death with non-melanoma death treated as a competing event).
- Integrated AUC is no longer silently replaced when not estimable; status, method, and NA-reason fields are carried into the consolidated and unified workbooks.
- The full-cohort exploratory no-GEP workflow now reuses the Objective 0-prepared cohort contract, derives expected group counts from the prepared snapshot, and prefers IPCW horizon direct-risk models with censoring-aware observed summaries.

**Artifact hierarchy:**
- Outcome-specific consolidated workbooks:
   - `a_metastasis_free_survival/05_summary_tables/*_MFS_consolidated_summary.xlsx`
   - `b_melanoma_specific_survival/03_summary_tables/*_MSS_consolidated_summary.xlsx`
- Outcome-specific technical detail workbooks:
   - `a_metastasis_free_survival/05_summary_tables/*mfs_validation_technical_details.xlsx`
   - `b_melanoma_specific_survival/03_summary_tables/*mss_validation_technical_details.xlsx`
- Outcome-specific narrative summaries:
   - `a_metastasis_free_survival/05_summary_tables/*mfs_validation_narrative_summary.md`
   - `b_melanoma_specific_survival/03_summary_tables/*mss_validation_narrative_summary.md`
- Cross-outcome workbook at the root of `04_GEP_Validation/`:
   - `*unified_gep_validation_summary.xlsx`
   - For the full cohort, this workbook now also includes `No_GEP_Overview`, `No_GEP_Model_Comparison`, and `No_GEP_Risk_Strata`
- Simple QC workbook under `04_GEP_Validation/unified_summary/`:
   - `*simple_gep_validation.xlsx`
   - For MFS, this workbook reports KM-observed 5-year MFS rather than a naive `1 - mfs_event_5yr` average.
- Limited visuals: KM curves for MFS, CIF curves for MSS, and optional outcome-specific PRAME delta-C PNGs (`*mfs_prame_delta_c.png`, `*mss_prame_delta_c.png`)

**Workbook contract:** the consolidated outcome workbook is the primary review artifact. Technical workbooks retain lower-level detail, and the root unified workbook is comparison-only (`*_Comparison` sheet naming). For full cohort runs, the unified workbook may append compact `No_GEP_*` summary tabs.

**No-GEP reporting contract:** the no-GEP appendix and compact unified-workbook tabs use a documented 0-to-1 probability scale for threshold fields and now include overlap diagnostics comparing `GEP Failed/Indeterminate` with `GEP Not Tested`.

**Display contract:** reader-facing outputs restore canonical labels from matching `*_derived_precollapse.rds` artifacts (for `biopsy1_gep`, `gep_class_simple`, `prame_status`, and `gep12_prame_status`) when available. Objective 4 entry points refresh eligibility flags from stored raw labels before analysis, preventing stale cohort artifacts from leaking nondefinitive rows into definitive Class 1 / Class 2 denominators.

For readability, the reader-facing MSS CIF PNG now uses `gep_class_simple` and shows only definitive `Class 1` versus `Class 2` strata. This does not change the technical MSS competing-risk tables or model fits, which still use the more granular `biopsy1_gep` grouping in the companion outputs.

The grouping choices for Objective 4 are centralized in `scripts/utils/config_constants.R` via `GEP_GROUPING_SPECS` and `GEP_OBJECTIVE4_GROUPING`. Change grouping there first so updates propagate through orchestration, reporting, and visualization.

**Important layout note:** cross-cutting cohort outputs such as baseline characteristics and treatment-duration summaries belong in `00_General/` inside each cohort folder, not in a shared top-level `Analysis/General/` directory.

See [STATISTICAL_METHODS.md](STATISTICAL_METHODS.md#gep-validation-metrics) for formal metric definitions and assumptions, and [INTERPRETATION_GUIDE.md](INTERPRETATION_GUIDE.md#understanding-gep-analysis) for workbook-reading guidance.

---

## Subgroup Filtering

Subgroup analysis implements rigorous filtering criteria to ensure statistical validity and publication-quality results.

### Filtering Requirements

**Minimum sample size:** ≥2 patients in each treatment group

**Minimum events:** ≥1 event in each treatment group (for survival outcomes)

**Statistical stability:** Groups with insufficient events excluded from analysis

### Risk Comparison Stability

**Zero events in one group → Stable comparison**
- Cox model can handle "no risk vs some risk" comparisons mathematically
- Example: ≥80 years group with 0 PBT events vs 2 GKSRS events produces stable HR

**Very few events vs many events → Unstable comparison**
- Extreme imbalances create statistical instability and infinite confidence intervals
- Example: 50-59 years group with 1 PBT event vs 8 GKSRS events produces infinite HR
- These groups automatically marked as "skipped_non_finite" in forest plot diagnostics

### Quality Assurance Benefits

The filtering ensures publication-quality results by preventing:

1. Models running on statistically unstable subgroups
2. Infinite hazard ratios and confidence intervals
3. Inconsistent filtering between different outcome types
4. Meaningless statistical comparisons that could mislead interpretation

### Implementation

Filtering logic implemented in `subgroup_data_prep.R`:
- Checks sample size per treatment arm
- Checks event counts per treatment arm
- Documents exclusion reasons
- Provides diagnostic Excel workbooks showing why subgroups were excluded

---

## Configuration

### Key Configuration Files

**`scripts/utils/config_constants.R`**
- Public configuration entry point sourced by `scripts/load_all.R`
- Deterministically sources private modules under `scripts/config/`
- Exposes paths, data-processing policy, modeling policy, Objective 0 contracts, display labels, and GEP policy as the same global objects consumed by downstream code

Private config modules under `scripts/config/`:
- `project_paths.R`: project roots, runtime/export paths, publish artifact registry, and path helpers
- `data_processing_policy.R`: source workbook filename, manual exclusions, manual date corrections, date/time constants, and data-quality thresholds
- `modeling_policy.R`: treatment/factor levels, confounders, subgroup variables, sparse-level policy, and model feasibility thresholds
- `gep_policy.R`: Objective 4 GEP constants, definitive-label policy, and grouping specifications
- `objective0_contracts.R`: Objective 0 structural requirements, derived-output manifest, downstream input contract, Objective 2 toxicity mapping, Objective 3 PFS-2 contract, and Objective 4 GEP derivation contract
- `labels_display.R`: table labels, display labels, baseline table variables, plot dimensions, and vision line-change display policy

Objective 0 contract responsibilities:
- `OBJECTIVE0_GLOBAL_REQUIRED_VARIABLES` checks global structural fields that must exist before objective-specific validation.
- `OBJECTIVE0_DERIVED_OUTPUT_MANIFEST` checks fields created by Objective 0 data derivation.
- `OBJECTIVE0_DOWNSTREAM_INPUT_CONTRACT` checks important Objective 1-4 endpoint, eligibility, adjustment, and prediction inputs before downstream scripts consume them.
- `OBJECTIVE2_TOXICITY_ENDPOINTS` maps toxicity source fields to Objective 0 burden fields consumed by Objective 2.
- `OBJECTIVE3_PFS2_DERIVATION_CONTRACT` and `OBJECTIVE4_GEP_DERIVATION_CONTRACT` protect row-wise source-derived endpoint logic that presence/domain checks alone cannot prove.

**Key Settings:**
```r
INPUT_FILENAME <- "your_data_file.xlsx"
RECREATE_ANALYTIC_DATASETS <- FALSE
USE_LOGS <- TRUE
VERBOSE <- TRUE
```

### Confounders

Standard confounders used across analyses:
```r
confounders <- c(
    "age_at_diagnosis",
    "sex",
    "location"
)
```

Different confounder sets available:
- `confounders_w_optic` - Includes optic nerve
- `confounders_wo_optic` - Excludes optic nerve (for restricted cohort)
- `confounders_w_stage` - Includes staging variables
- Custom sets can be defined per analysis

---

## Testing

The repository uses two test lanes with separate bootstrap helpers:

- `tests/testthat/`: portable regression tests loaded through `tests/testthat/helper-bootstrap.R`
- `tests/integration/`: opt-in local integration tests loaded through `tests/integration/helper-bootstrap.R`

Run tests with the shell entry points used elsewhere in the repository:

```sh
Rscript -e "testthat::test_dir('tests/testthat')"
Rscript -e "testthat::test_file('tests/testthat/test_objective2_safety_toxicity.R')"
Rscript -e "Sys.setenv(OCULAR_RUN_INTEGRATION_TESTS='true'); testthat::test_dir('tests/integration')"
```

Integration tests are intentionally gated by `OCULAR_RUN_INTEGRATION_TESTS` so routine regression runs do not assume local cohort data are available.
