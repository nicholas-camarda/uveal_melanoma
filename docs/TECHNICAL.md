# Technical Documentation

This document provides detailed technical information about the implementation, workflow system, and quality assurance procedures.

---

## Table of Contents

- [Workflow Orchestration System](#workflow-orchestration-system)
- [Cohort Definitions](#cohort-definitions)
- [Directory Structure](#directory-structure)
- [Data Processing Workflow](#data-processing-workflow)
- [Quality Assurance](#quality-assurance)
- [Data Limitations](#data-limitations)
- [Research Objectives](#research-objectives)
- [Subgroup Filtering](#subgroup-filtering)

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

### Full Cohort (n=260)

**Definition:** All patients who received either GKSRS or PBT brachytherapy

**Purpose:** Real-world effectiveness comparison across the complete spectrum of tumor characteristics

**Clinical Value:** Provides comprehensive treatment effectiveness data for real-world decision making

### Restricted Cohort (n=167)

**Definition:** Patients eligible for **both** treatment modalities

**Eligibility Criteria:**
- Tumor diameter ≤20mm
- Tumor height ≤10mm
- No optic nerve involvement
- Suitable for both GKSRS and PBT

**Purpose:** Balanced comparison minimizing treatment selection bias

**Clinical Value:** Direct treatment comparison in patients where both options are clinically appropriate

### GKSRS-Only Cohort (n=92)

**Definition:** Patients **ineligible** for PBT brachytherapy

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

---

## Directory Structure

### Output Organization

Analysis outputs follow a **cohort → objective → sub-objective** structure:

```
project_working_directory/
├── data/                           # Raw data files
├── final_data/
│   ├── Analytic Dataset/           # Processed RDS files
│   │   ├── uveal_melanoma_full_cohort.rds
│   │   ├── uveal_melanoma_restricted_cohort.rds
│   │   ├── uveal_melanoma_gksrs_only_cohort.rds
│   │   ├── *_derived_precollapse.rds
│   │   └── other_map.rds
│   ├── Original Files/             # Raw input data
│   └── Analysis/                   # Analysis outputs by cohort
│       ├── uveal_full/             # Full cohort (n=260)
│       │   ├── 00_General/
│       │   │   ├── baseline_characteristics/
│       │   │   ├── treatment_duration/
│       │   │   └── removed_patients_summary.tsv
│       │   ├── 01_Efficacy/
│       │   │   ├── a_recurrence/
│       │   │   ├── b_metastatic_progression/
│       │   │   ├── c_overall_survival/
│       │   │   ├── d_progression_free_survival/
│       │   │   ├── e_tumor_height_primary/
│       │   │   ├── f_tumor_height_sensitivity/
│       │   │   ├── g_subgroup_analysis/
│       │   │   │   ├── tumor_height_primary/
│       │   │   │   ├── tumor_height_sensitivity/
│       │   │   │   └── forest_plots/
│       │   │   └── h_proportional_hazards_diagnostics/
│       │   ├── 02_Safety/
│       │   │   ├── a_vision_changes/
│       │   │   ├── b_retinopathy/
│       │   │   ├── c_neovascular_glaucoma/
│       │   │   └── d_serous_retinal_detachment/
│       │   ├── 03_Repeat_Radiation/
│       │   │   ├── a_pfs2/
│       │   │   └── b_proportional_hazards_diagnostics/
│       │   └── 04_GEP_Validation/  # 🚧 Under construction
│       │       ├── a_metastasis_free_survival/
│       │       └── b_melanoma_specific_survival/
│       ├── uveal_restricted/        # Restricted cohort (n=167)
│       ├── gksrs/                   # GKSRS-only cohort (n=92)
│       └── merged_tables/           # Cross-cohort comparisons
├── logs/                            # Execution logs
│   ├── txt/
│   └── json/
├── docs/                            # Documentation
├── scripts/                         # Analysis code
└── tests/                           # Testing framework
```

### Script Organization

```
scripts/
├── main.R                          # Main execution entrypoint
├── load_all.R                      # Dependency loader
├── analysis/                       # Statistical analysis functions
│   ├── binary_outcomes.R
│   ├── survival_analysis.R
│   ├── tumor_height_analysis.R
│   └── vision_safety_analysis.R
├── subgroup/                       # Subgroup analysis
│   ├── subgroup_analysis.R
│   └── subgroup_data_prep.R
├── tables/                         # Table generation
│   ├── regression_tables.R
│   └── summary_tables.R
├── visualization/                  # Plot generation
│   ├── forest_plot_core.R
│   ├── forest_plot_formatting.R
│   ├── survival_plots.R
│   └── rmst_plots.R
├── utils/                          # Core utilities
│   ├── config_constants.R
│   ├── data_utilities.R
│   ├── forest_plot_diagnostics.R
│   ├── logging_utilities.R
│   └── output_utilities.R
├── data_helper/                    # Data processing
│   ├── cohort_creation.R
│   ├── data_derivation.R
│   ├── data_loading.R
│   └── data_summaries.R
├── workflow/                       # Objective workflows
│   ├── objective_0_data_processing.R
│   ├── objective_1_primary_outcomes.R
│   ├── objective_2_safety_toxicity.R
│   ├── objective_3_repeat_radiation.R
│   └── objective_4_gep_analysis.R
└── tools/                          # Diagnostic tools
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
   - Store other_map for reference

5. **Run Analyses**
   - Load RDS datasets
   - Execute objective-specific workflows
   - Generate outputs

### Data Flow Diagram

See README.md for the Mermaid diagram illustrating the complete data flow.

---

## Quality Assurance

### Data Quality Checkpoints

**Cohort Curation:**
- Stage IV cases (IDs 7, 116, 262) removed before cohort assignment
- Manually excluded records (ID 271) documented
- `removed_patients_summary.tsv` exported to each cohort's `00_General/` directory
- Reviewers can audit exclusion decisions

**Data Validation:**
- Automatic checks for data integrity
- Type validation for all variables
- Range checks for continuous variables
- Consistency checks across related fields

**Factor Level Management:**
- Consistent handling of categorical variables
- Centralized level labels in `config_constants.R`
- Automatic cleanup of factor levels
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

See [CALCULATIONS.md](CALCULATIONS.md) for detailed variable derivation formulas.

---

## Data Limitations

The analysis pipeline includes robust error handling for situations where data limitations prevent certain analyses from completing.

### Cohort-Specific Limitations

#### GKSRS-Only Cohort (n=92)

**Objective 3 (PFS-2 Analysis):** Insufficient events for survival analysis
- Only 13 patients with valid PFS-2 data
- Only 3 total second recurrence events (minimum required: 5)
- Events concentrated in only 2 treatment groups (GKSRS: 1, TTT: 2)
- Summary tables generated, but survival curves and Cox models skipped

#### Restricted Cohort (n=167)

- Generally sufficient sample size for most analyses
- Occasional rare category handling in subgroup analyses due to smaller size than full cohort
- Optic nerve abutment excluded from baseline tables (all patients have optic_nerve="N" by eligibility)

#### Full Cohort (n=260)

- Generally sufficient sample size for most analyses
- Occasional rare category handling in subgroup analyses

### Automatic Error Handling

**Minimum Event Requirements:**
- Survival analyses require 5+ total events
- Cox regression requires 2+ groups with events
- Logistic regression requires adequate observations per category

**Rare Category Management:**
- Categories with <5 observations automatically collapsed
- Variables with insufficient levels after collapsing excluded from models
- Other_map tracks all collapsed categories

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

### Objective 1: Efficacy Analysis (COMPLETE)

| Sub-objective | Method | Implementation | Outputs | Location |
|---------------|--------|----------------|---------|----------|
| **1a. Local Recurrence** | Binary outcome analysis with logistic regression | `analyze_binary_outcome_rates()` | Event rates (.xlsx), logistic regression models (.html) | `{cohort}/01_Efficacy/a_recurrence/` |
| **1b. Metastatic Progression** | Binary outcome analysis with logistic regression | `analyze_binary_outcome_rates()` | Event rates (.xlsx), logistic regression models (.html) | `{cohort}/01_Efficacy/b_metastatic_progression/` |
| **1c. Overall Survival** | Kaplan-Meier + Cox regression + RMST analysis | `analyze_time_to_event_outcomes()` | Survival tables (.xlsx), Cox models (.html), survival curves (.png), RMST plots (.png) | `{cohort}/01_Efficacy/c_overall_survival/` |
| **1d. Progression-Free Survival** | Composite endpoint (recurrence OR death) | `analyze_time_to_event_outcomes()` | Survival tables (.xlsx), Cox models (.html), survival curves (.png), RMST plots (.png) | `{cohort}/01_Efficacy/d_progression_free_survival/` |
| **1e. Tumor Height (Primary)** | Linear regression without baseline adjustment | `analyze_tumor_height_changes()` | Change summaries (.html), regression models (.html) | `{cohort}/01_Efficacy/e_tumor_height_primary/` |
| **1f. Tumor Height (Sensitivity)** | Linear regression with baseline adjustment | `analyze_tumor_height_changes()` | Change summaries (.html), regression models (.html) | `{cohort}/01_Efficacy/f_tumor_height_sensitivity/` |
| **1g. Subgroup Analysis** | Interaction testing across patient subgroups | `analyze_treatment_effect_subgroups_*()` | Subgroup tables (.xlsx), forest plots (.png), diagnostics (.xlsx) | `{cohort}/01_Efficacy/g_subgroup_analysis/` |

### Objective 2: Safety/Toxicity Analysis (COMPLETE)

| Sub-objective | Method | Implementation | Outputs | Location |
|---------------|--------|----------------|---------|----------|
| **2a. Vision Changes** | Linear regression of visual acuity changes | `analyze_visual_acuity_changes()` | Vision change summaries (.html), regression models (.html) | `{cohort}/02_Safety/a_vision_changes/` |
| **2b. Radiation Retinopathy** | Binary outcome analysis with logistic regression | `analyze_radiation_complications()` | Complication rates (.xlsx), logistic regression models (.html) | `{cohort}/02_Safety/b_retinopathy/` |
| **2c. Neovascular Glaucoma** | Binary outcome analysis with logistic regression | `analyze_radiation_complications()` | Complication rates (.xlsx), logistic regression models (.html) | `{cohort}/02_Safety/c_neovascular_glaucoma/` |
| **2d. Serous Retinal Detachment** | Binary outcome analysis (radiation-induced only) | `analyze_radiation_complications()` | Complication rates (.xlsx), logistic regression models (.html) | `{cohort}/02_Safety/d_serous_retinal_detachment/` |

### Objective 3: Repeat Radiation Efficacy (COMPLETE)

**3a. Progression-Free Survival-2 (PFS-2)**

**Method:** Survival analysis for patients with local recurrence receiving second-line treatment

**Purpose:** Evaluate effectiveness of second-line radiation treatments for patients who experience local recurrence after initial therapy

**Implementation:** `analyze_pfs2()` in `scripts/workflow/objective_3_repeat_radiation.R`

**Outputs:** PFS-2 characteristics tables (.xlsx), survival curves (.png), Cox models (.html)

**Location:** `{cohort}/03_Repeat_Radiation/a_pfs2/`

**Note:** Analysis automatically skips survival modeling when insufficient events present (minimum: 5 total events across 2+ treatment groups)

### Objective 4: GEP Predictive Accuracy (🚧 IN PROGRESS)

**Purpose:** Validate lab-reported GEP probabilities against real outcomes for metastasis-free survival (MFS) and melanoma-specific survival (MSS)

**Location:** `{cohort}/04_GEP_Validation/a_metastasis_free_survival/` and `b_melanoma_specific_survival/`

**Implementation:** `scripts/workflow/objective_4_gep_analysis.R`

**Planned Outputs:** Calibration plots, discrimination metrics, clinical utility analyses

See [STATISTICAL_METHODS.md](STATISTICAL_METHODS.md) for detailed GEP validation methodology.

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
- Input filename
- Analysis settings
- Variable lists for baseline tables
- Continuous variable specifications
- Factor level labels
- Confounder lists

**Key Settings:**
```r
INPUT_FILENAME <- "your_data_file.xlsx"
RECREATE_ANALYTIC_DATASETS <- TRUE
USE_LOGS <- TRUE
VERBOSE <- TRUE
```

### Confounders

Standard confounders used across analyses:
```r
confounders <- c(
    "age_at_diagnosis",
    "sex",
    "location",
    "optic_nerve"
)
```

Different confounder sets available:
- `confounders_w_optic` - Includes optic nerve
- `confounders_wo_optic` - Excludes optic nerve (for restricted cohort)
- `confounders_w_stage` - Includes staging variables
- Custom sets can be defined per analysis

---

## Testing

Test framework located in `tests/testthat/`:
- `test_objective2_safety_toxicity.R`
- `test_objective3_repeat_radiation.R`
- Helper files and fixtures as `helper-*.R`

Run tests:
```r
testthat::test_dir("tests/testthat")
testthat::test_file("tests/testthat/test_objective2_safety_toxicity.R")
```

See [AGENTS.md](../AGENTS.md) for testing guidelines and development conventions.
