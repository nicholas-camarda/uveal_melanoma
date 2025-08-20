# Uveal Melanoma Treatment Outcomes Research Platform: GKSRS vs PBT Analysis Pipeline

## Overview

This research platform provides a comprehensive for analyzing clinical outcomes in uveal melanoma patients treated with either Gamma Knife Stereotactic Radiosurgery (GKSRS) or Proton Beam Therapy (PBT) brachytherapy.

### **Key Features**
- **Complete Analysis Pipeline**: From raw data to publication-ready outputs
- **4 Primary Study Objectives**: Efficacy, safety, repeat radiation, and GEP validation
- **Multiple Patient Cohorts**: Full, restricted, and GKSRS-only cohorts for comprehensive analysis
- **Professional Outputs**: Excel tables, HTML reports, hihg-quality figures
- **Statistical Rigor**: Comprehensive survival analysis, subgroup testing, and validation methods

**Author:** Nicholas Camarda
**Last Updated:** August 18, 2025

---

## Table of Contents

### **Getting Started**
- [Quick Start](#quick-start)
- [Requirements](#requirements)
- [Installation](#installation)

### **Research Framework**
- [Study Objectives](#study-objectives)
- [Cohort Definitions](#cohort-definitions)
- [Data Processing Workflow](#data-processing-workflow)

### **Implementation Status**
- [Objective 1: Efficacy Analysis](#objective-1-efficacy-analysis-complete)
- [Objective 2: Safety/Toxicity Analysis](#objective-2-safetytoxicity-analysis-complete)
- [Objective 3: Repeat Radiation Efficacy](#objective-3-repeat-radiation-efficacy-complete)
- [Objective 4: GEP Predictive Accuracy](#objective-4-gep-predictive-accuracy-complete)

### **Technical Details**
- [Key Features](#key-features)
- [Survival Analysis Features](#survival-analysis-features)
- [Directory Structure](#directory-structure)
- [Workflow Orchestration System](#workflow-orchestration-system)

### **Usage & Maintenance**
- [Usage Instructions](#usage)
- [Data Limitations](#data-limitations-and-analysis-constraints)
- [Logging and Monitoring](#logging-and-monitoring)

---

## Quick Start

### **Prerequisites**
1. **R Environment**: R >= 4.4.0 with required packages
2. **Data Files**: Place your raw Excel data in the `data/` directory
3. **Configuration**: Review and adjust settings in `scripts/utils/config_constants.R`

### **Step-by-Step Setup**
1. **Clone and Navigate**: Clone the repository and navigate to the project root
2. **Install Dependencies**: Required packages will be installed automatically via `scripts/load_all.R`
3. **Configure Analysis**: Set your input filename and analysis preferences
4. **Run Analysis**: Execute the pipeline for your desired objectives

### **Basic Usage Examples**
Open an R session in the project root and run:

```r
# Load all helper functions and dependencies
source("scripts/load_all.R")    

# Option 1: Complete analysis for all cohorts and objectives
main_execution()

# Option 2: All objectives for a specific cohort
run_my_analysis("uveal_melanoma_full_cohort")      # Full cohort (n=263)
run_my_analysis("uveal_melanoma_restricted_cohort") # Restricted cohort (n=169)
run_my_analysis("uveal_melanoma_gksrs_only_cohort") # GKSRS-only cohort (n=93)

# Option 3: Specific objectives for targeted analysis
run_specific_objective("uveal_melanoma_full_cohort", 1)  # Efficacy analysis only
run_specific_objective("uveal_melanoma_full_cohort", 2)  # Safety/toxicity only
run_specific_objective("uveal_melanoma_full_cohort", 3)  # Repeat radiation only
run_specific_objective("uveal_melanoma_full_cohort", 4)  # GEP validation only
```

### **What You'll Get**
- **Excel Tables**: Publication-ready summary statistics and regression results
- **HTML Reports**: Interactive tables with proper formatting and styling
- **Publication Figures**: High-resolution PNG plots (300 DPI) for manuscripts
- **Comprehensive Logs**: Detailed execution logs with timestamps and error handling
- **Organized Outputs**: Results organized by cohort → objective → sub-objective structure

### **Output Organization**
All outputs are automatically organized in `final_data/Analysis/` with the following structure:
- **`{cohort}/01_Efficacy/`**: Primary outcomes, tumor height changes, subgroup analysis
- **`{cohort}/02_Safety/`**: Vision changes, radiation complications
- **`{cohort}/03_Repeat_Radiation/`**: PFS-2 analysis and diagnostics
- **`{cohort}/04_GEP_Validation/`**: Gene expression profile validation

---

## Study Objectives

This research platform addresses four prioritized clinical research questions, each designed to provide comprehensive insights into uveal melanoma treatment outcomes. All objectives have been **100% completed** with publication-ready outputs.

### **Objective 1: Treatment Efficacy Comparison** ✅ **COMPLETE**
**Primary Research Question:** How do clinical outcomes compare between GKSRS and PBT treatments?  
**Purpose:** Evaluate the comparative effectiveness of two radiation treatment modalities for uveal melanoma, providing evidence-based guidance for treatment selection.

| Sub-objective | Analysis Type | Status | Outputs |
|---------------|---------------|---------|----------|
| **1a. Local Recurrence** | Binary outcome analysis | ✅ Complete | Event rates, logistic regression models |
| **1b. Metastatic Progression** | Binary outcome analysis | ✅ Complete | Event rates, logistic regression models |
| **1c. Overall Survival** | Survival analysis + RMST | ✅ Complete | KM curves, Cox models, RMST analysis |
| **1d. Progression-Free Survival** | Composite endpoint analysis | ✅ Complete | Survival curves, Cox models, RMST analysis |
| **1e. Tumor Height Changes** | Linear regression (primary) | ✅ Complete | Change summaries, regression models |
| **1f. Tumor Height Changes** | Linear regression (sensitivity) | ✅ Complete | Baseline-adjusted models |
| **1g. Subgroup Analysis** | Treatment effect heterogeneity | ✅ Complete | Forest plots, interaction testing |

### **Objective 2: Safety and Toxicity Profile** ✅ **COMPLETE**
**Primary Research Question:** What are the comparative safety profiles between treatments?  
**Purpose:** Assess treatment-related complications and side effects to inform patient counseling and treatment decision-making.

| Sub-objective | Analysis Type | Status | Outputs |
|---------------|---------------|---------|----------|
| **2a. Vision Changes** | Linear regression | ✅ Complete | Visual acuity change analysis |
| **2b. Radiation Retinopathy** | Binary outcome analysis | ✅ Complete | Complication rates, risk factors |
| **2c. Neovascular Glaucoma** | Binary outcome analysis | ✅ Complete | Complication rates, risk factors |
| **2d. Serous Retinal Detachment** | Binary outcome analysis | ✅ Complete | Radiation-induced SRD analysis |

### **Objective 3: Repeat Radiation Efficacy** ✅ **COMPLETE**
**Primary Research Question:** How effective are second-line treatments for patients with local recurrence?  
**Purpose:** Evaluate outcomes for patients requiring additional treatment, informing salvage therapy strategies and patient prognosis.

| Sub-objective | Analysis Type | Status | Outputs |
|---------------|---------------|---------|----------|
| **3a. PFS-2 Analysis** | Survival analysis | ✅ Complete | PFS-2 characteristics, survival curves |

### **Objective 4: GEP Predictive Accuracy** ✅ **COMPLETE**
**Primary Research Question:** How well do gene expression profiles predict clinical outcomes?  
**Purpose:** Validate molecular biomarkers for risk stratification and personalized treatment planning in uveal melanoma.

| Sub-objective | Analysis Type | Status | Outputs |
|---------------|---------------|---------|----------|
| **4a. Metastasis-Free Survival** | Multi-timepoint validation | ✅ Complete | Calibration, robust discrimination, DCA |
| **4b. Melanoma-Specific Survival** | Competing risk analysis | ✅ Complete | CIF curves, Fine-Gray models, robust metrics |

---

## Data Processing Workflow

The analysis follows a systematic, validated data processing pipeline designed to ensure data quality and reproducibility:

### **Pipeline Overview**
The workflow transforms raw clinical data into publication-ready analyses through a series of validated checkpoints:

```mermaid
flowchart TD
    A["Raw Excel Data<br/>INPUT_FILENAME"] --> B["Data Loading & Cleaning<br/>load_and_clean_data()"]
    B --> C["Data Processing<br/>create_analytic_dataset()"]
    C --> D["Cohort Creation<br/>apply_criteria()"]
    
    D --> I["Full Cohort<br/>(n=263 patients)"]
    D --> J["Restricted Cohort<br/>(n=169 patients)"]
    D --> K["GKSRS-Only Cohort<br/>(n=93 patients)"]
    
    I --> L["Save to RDS<br/>final_data/Analytic Dataset/"]
    J --> L
    K --> L
    
    L --> M["Workflow Orchestration<br/>run_my_analysis() or run_specific_objective()"]
    M --> N["Load RDS data"]
    N --> O["Create output directories<br/>by cohort and objective"]
    O --> P["Objective-Specific Analysis Functions"]
    
    P --> Q["Objective 1: Efficacy<br/>Primary outcomes & subgroup analysis"]
    P --> R["Objective 2: Safety<br/>Vision changes & complications"]
    P --> S["Objective 3: Repeat Radiation<br/>PFS-2 analysis"]
    P --> T["Objective 4: GEP Validation<br/>Predictive accuracy testing"]
    
    Q --> U["Publication Outputs<br/>Tables, Figures, Models"]
    R --> U
    S --> U
    T --> U
```

### **Quality Assurance Checkpoints**
- **Data Validation**: Automatic checks for data integrity and completeness
- **Factor Level Management**: Consistent handling of categorical variables
- **Cohort Assignment**: Automated application of inclusion/exclusion criteria
- **Output Validation**: Comprehensive error handling and logging

## Workflow Orchestration System

The analysis employs a sophisticated, modular workflow system designed for flexibility, efficiency, and robust error handling:

### **Core Execution Functions**
| Function | Purpose | Use Case |
|----------|---------|----------|
| **`main_execution()`** | Complete pipeline execution | Full analysis for all cohorts and objectives |
| **`run_my_analysis(dataset_name)`** | Cohort-specific analysis | All objectives for a specific patient cohort |
| **`run_specific_objective(dataset_name, objective_number)`** | Targeted analysis | Single objective for focused research questions |

### **Objective-Specific Workflow Scripts**
Each research objective has a dedicated, optimized workflow script:

| Objective | Script | Primary Functions | Analysis Type |
|-----------|--------|-------------------|---------------|
| **0: Data Processing** | `objective_0_data_processing.R` | Data cleaning, validation, cohort creation | Data preparation |
| **1: Efficacy Analysis** | `objective_1_primary_outcomes.R` | Survival analysis, tumor height, subgroups | Primary outcomes |
| **2: Safety/Toxicity** | `objective_2_safety_toxicity.R` | Vision changes, complications | Safety endpoints |
| **3: Repeat Radiation** | `objective_3_repeat_radiation.R` | PFS-2 analysis, diagnostics | Second-line treatment |
| **4: GEP Validation** | `objective_4_gep_analysis.R` | Predictive accuracy testing | Biomarker validation |


---

## Cohort Definitions

The analysis employs three strategically designed patient cohorts to address different clinical questions and minimize treatment selection bias:

### **Full Cohort** (n=263)
**Definition:** All patients who received either GKSRS or PBT brachytherapy  
**Purpose:** Real-world effectiveness comparison across the complete spectrum of tumor characteristics  
**Clinical Value:** Provides comprehensive treatment effectiveness data for real-world decision making

### **Restricted Cohort** (n=169) 
**Definition:** Patients eligible for **both** treatment modalities  
**Eligibility Criteria:**
- Tumor diameter ≤20mm
- Tumor height ≤10mm  
- No optic nerve involvement
- Suitable for both GKSRS and PBT

**Purpose:** Balanced comparison minimizing treatment selection bias  
**Clinical Value:** Direct treatment comparison in patients where both options are clinically appropriate

### **GKSRS-Only Cohort** (n=93)
**Definition:** Patients **ineligible** for PBT brachytherapy  
**Exclusion Criteria:**
- Tumor diameter >20mm, OR
- Tumor height >10mm, OR  
- Optic nerve involvement

**Purpose:** GKSRS effectiveness assessment in challenging cases  
**Clinical Value:** Demonstrates GKSRS utility in patients where PBT is not feasible

### **Cohort Selection Rationale**
This three-cohort design addresses key clinical and methodological challenges:
- **Treatment Selection Bias**: Restricted cohort provides balanced comparison
- **Real-World Applicability**: Full cohort reflects actual clinical practice
- **Treatment Limitations**: GKSRS-only cohort shows effectiveness in challenging cases
- **Statistical Power**: Adequate sample sizes for robust statistical analysis

---

## Directory Structure

The project employs a logical, hierarchical organization designed for clarity, maintainability, and easy navigation:

### **Output Organization**
Analysis outputs follow a **cohort → objective → sub-objective** structure for intuitive navigation:

```
project_working_directory/
├── data/                                       # Raw data files
├── final_data/
│   ├── Analytic Dataset/                       # Processed RDS files
│   └── Analysis/                               # Analysis outputs by cohort
│       ├── uveal_full/                         # Full cohort (n=263)
│       │   ├── 00_General/                     # Baseline characteristics
│       │   ├── 01_Efficacy/                    # Primary outcomes
│       │   │   ├── a_recurrence/               # Local recurrence analysis
│       │   │   ├── b_metastatic_progression/   # Metastatic progression
│       │   │   ├── c_overall_survival/         # Overall survival
│       │   │   ├── d_progression_free_survival/ # PFS analysis
│       │   │   ├── e_tumor_height_primary/     # Primary height analysis
│       │   │   ├── f_tumor_height_sensitivity/ # Sensitivity height analysis
│       │   │   ├── g_subgroup_analysis/        # Subgroup analysis
│       │   │   │   ├── tumor_height_primary/   # Height subgroups
│       │   │   │   ├── tumor_height_sensitivity/ # Sensitivity subgroups
│       │   │   │   └── forest_plots/           # Forest plot outputs
│       │   │   └── h_proportional_hazards/     # PH assumption testing
│       │   ├── 02_Safety/                      # Safety endpoints
│       │   │   ├── a_vision_changes/           # Vision change analysis
│       │   │   ├── b_retinopathy/              # Retinopathy rates
│       │   │   ├── c_neovascular_glaucoma/     # NVG rates
│       │   │   └── d_serous_retinal_detachment/ # SRD rates
│       │   ├── 03_Repeat_Radiation/            # Second-line treatment
│       │   │   ├── a_pfs2/                     # PFS-2 analysis
│       │   │   └── b_proportional_hazards/     # PH diagnostics
│       │   └── 04_GEP_Validation/              # Biomarker validation
│       │       ├── a_metastasis_free_survival/ # MFS validation
│       │       └── b_melanoma_specific_survival/ # MSS validation
│       ├── uveal_restricted/                   # Restricted cohort (n=169)
│       └── gksrs/                              # GKSRS-only cohort (n=93)
├── logs/                                       # Execution logs
└── scripts/                                    # Analysis code
```

### **Script Organization**
The codebase is organized into logical, focused modules:

#### **Core Analysis Scripts**
- **`main.R`**: Main execution entrypoints
- **`load_all.R`**: Central dependency loader and environment setup

#### **Analysis Modules**
- **`analysis/`**: Core statistical analysis functions
- **`subgroup/`**: Subgroup analysis and interaction testing
- **`tables/`**: Table generation and formatting utilities
- **`visualization/`**: Plot generation and styling

#### **Utility Modules**
- **`utils/`**: Core utilities, configuration, and validation
- **`data_helper/`**: Data processing and cohort management
- **`workflow/`**: Objective-specific execution workflows

#### **Testing & Validation**
- **`tests/`**: Unit tests and validation scripts
- **`tools/`**: Standalone analysis and diagnostic tools

---

## Implementation Status: Analysis Pipeline

### **OBJECTIVE 1: Efficacy Analysis** ✅ **COMPLETE**

All primary efficacy analyses have been implemented with comprehensive outputs through the new workflow system:

| Sub-objective | Method | Implementation | Outputs | Location |
|---------------|--------|----------------|---------|----------|
| **1a. Local Recurrence** | Binary outcome analysis with logistic regression | `analyze_binary_outcome_rates()` function | Event rates (.xlsx), logistic regression models (.html) | `{cohort}/01_Efficacy/a_recurrence/` |
| **1b. Metastatic Progression** | Binary outcome analysis with logistic regression | `analyze_binary_outcome_rates()` function | Event rates (.xlsx), logistic regression models (.html) | `{cohort}/01_Efficacy/b_metastatic_progression/` |
| **1c. Overall Survival** | Kaplan-Meier + Cox regression + RMST analysis | `analyze_time_to_event_outcomes()` function | Survival tables (.xlsx), Cox models (.html), survival curves (.png), RMST progression plots (.png) | `{cohort}/01_Efficacy/c_overall_survival/` |
| **1d. Progression-Free Survival** | Composite endpoint (progression OR death) with full survival analysis | `analyze_time_to_event_outcomes()` function | Survival tables (.xlsx), Cox models (.html), survival curves (.png), RMST progression plots (.png) | `{cohort}/01_Efficacy/d_progression_free_survival/` |
| **1e. Tumor Height Changes (Primary)** | Linear regression without baseline height adjustment (avoids overadjustment bias) | `analyze_tumor_height_changes()` function | Change summaries (.html), regression models (.html) | `{cohort}/01_Efficacy/e_tumor_height_primary/` |
| **1f. Tumor Height Changes (Sensitivity)** | Linear regression with baseline height adjustment (robustness check) | `analyze_tumor_height_changes()` function | Change summaries (.html), regression models (.html) | `{cohort}/01_Efficacy/f_tumor_height_sensitivity/` |

#### **1g. Subgroup Analysis** ✅ **COMPLETE**
**Method:** Interaction testing across patient subgroups for treatment effect heterogeneity  
**Implementation:** Unified `subgroup_analysis.R` with dedicated functions:
  - `analyze_treatment_effect_subgroups_survival()` - For survival outcomes
  - `analyze_treatment_effect_subgroups_binary()` - For binary outcomes  
  - `analyze_treatment_effect_subgroups_height()` - For tumor height changes

**Subgroups:** Age, sex, tumor location, initial tumor height/diameter  
**Forest Plots:** Comprehensive forest plot visualization with `create_single_cohort_forest_plot()`  
**Outputs:** 
  - **Primary tumor height subgroups:** `{cohort}/01_Efficacy/g_subgroup_analysis/tumor_height_primary/`
  - **Sensitivity tumor height subgroups:** `{cohort}/01_Efficacy/g_subgroup_analysis/tumor_height_sensitivity/`
  - **Forest plots:** `{cohort}/01_Efficacy/g_subgroup_analysis/forest_plots/`

##### **Statistical Filtering and Stability**
The subgroup analysis implements rigorous filtering criteria to ensure statistical validity and publication-quality results:

**Filtering Requirements:**
- **Minimum sample size:** ≥2 patients in each treatment group
- **Minimum events:** ≥1 event in each treatment group (for survival outcomes)
- **Statistical stability:** Groups with insufficient events are excluded from analysis

**Risk Comparison Stability:**
- **Zero events in one group → Stable comparison** (risk vs no risk)
  - The Cox model can handle "no risk vs some risk" comparisons mathematically
  - Example: ≥80 years group with 0 PBT events vs 2 GKSRS events produces stable HR
- **Very few events vs many events → Unstable comparison** (unreliable relative risk estimation)
  - Extreme imbalances create statistical instability and infinite confidence intervals
  - Example: 50-59 years group with 1 PBT event vs 8 GKSRS events produces infinite HR
  - These groups are automatically marked as "skipped_non_finite" in forest plot diagnostics

**Quality Assurance Benefits:**
The filtering ensures publication-quality results by preventing:
1. Models running on statistically unstable subgroups
2. Infinite hazard ratios and confidence intervals  
3. Inconsistent filtering between different outcome types
4. Meaningless statistical comparisons that could mislead interpretation

### **OBJECTIVE 2: Safety/Toxicity Analysis** ✅ **COMPLETE**

All safety endpoint analyses have been implemented through the workflow system:

| Sub-objective | Method | Implementation | Outputs | Location |
|---------------|--------|----------------|---------|----------|
| **2a. Vision Changes** | Linear regression analysis of visual acuity changes | `analyze_visual_acuity_changes()` function | Vision change summaries (.html), regression models (.html) | `{cohort}/02_Safety/a_vision_changes/` |
| **2b. Radiation Retinopathy** | Binary outcome analysis with logistic regression | `analyze_radiation_complications()` function | Complication rates (.xlsx), logistic regression models (.html) | `{cohort}/02_Safety/b_retinopathy/` |
| **2c. Neovascular Glaucoma** | Binary outcome analysis with logistic regression | `analyze_radiation_complications()` function | Complication rates (.xlsx), logistic regression models (.html) | `{cohort}/02_Safety/c_neovascular_glaucoma/` |
| **2d. Serous Retinal Detachment** | Binary outcome analysis (radiation-induced only) with logistic regression | `analyze_radiation_complications()` function | Complication rates (.xlsx), logistic regression models (.html) | `{cohort}/02_Safety/d_serous_retinal_detachment/` |

### **OBJECTIVE 3: Repeat Radiation Efficacy** ✅ **COMPLETE**

#### **3a. Progression-Free Survival-2 (PFS-2)**
**Method:** Survival analysis for patients with local recurrence receiving second-line treatment  
**Purpose:** This analysis evaluates the effectiveness of second-line radiation treatments for patients who experience local recurrence after initial therapy, helping inform salvage treatment decisions and patient counseling about prognosis after recurrence.  
**Implementation:** `analyze_pfs2()` function in `scripts/workflow/objective_3_repeat_radiation.R`  
**Outputs:** PFS-2 characteristics tables (.xlsx), survival curves (.png), Cox models (.html)  
**Location:** `{cohort}/03_Repeat_Radiation/a_pfs2/`  
**Note:** Analysis automatically skips survival modeling when insufficient events are present (minimum: 5 total events across 2+ treatment groups)

### **OBJECTIVE 4: GEP Predictive Accuracy** ✅ **COMPLETE**

Gene expression profile validation analyses using survival model validation methods with a **robust discrimination approach** that focuses on clinically meaningful metrics:

#### **4a. Metastasis-Free Survival Validation**
**Status:** Fully implemented with robust validation suite  
**Method:** Multi-timepoint validation (5, 7, 10 years) with comprehensive statistical testing:
- **Calibration Metrics:** Nam-D'Agostino χ² tests, ICI (Integrated Calibration Index), calibration slope and intercept
- **Overall Prediction Accuracy:** Brier Score with fallback methods for robust calculation
- **Robust Discrimination:** Harrell's C-index (primary metric) and integrated AUC over time periods
- **Clinical Utility:** Decision curve analysis with net benefit assessment
- **Clinical Value:** IPA (Index of Prediction Accuracy) measuring incremental benefit over baseline

**Purpose:** This analysis tests how well the gene expression profile predicts whether patients will develop distant metastases over time. It provides statistical validation of the GEP model's prognostic accuracy through calibration tests, discrimination metrics, and observed vs. expected event rates at multiple time points.  
**Implementation:** `analyze_gep_mfs_validation()` function in `scripts/workflow/objective_4_gep_analysis.R`  
**Outputs:** Comprehensive validation reports (.xlsx), calibration plots (.png), discrimination metrics (.xlsx), decision curves (.png)  
**Location:** `{cohort}/04_GEP_Validation/a_metastasis_free_survival/`

#### **4b. Melanoma-Specific Survival Validation**  
**Status:** Fully implemented with dual competing-risk models and robust metrics  
**Method:** Standard survival analysis plus Fine-Gray competing risk models with cumulative incidence functions. The analysis employs a **robust discrimination strategy** that focuses on metrics that work reliably with our data distribution:
- **Calibration Metrics:** Same comprehensive suite as MFS validation
- **Overall Prediction Accuracy:** Brier Score with robust fallback methods
- **Robust Discrimination:** Harrell's C-index (primary) and integrated AUC over time periods
- **Clinical Utility:** Decision curve analysis with net benefit assessment
- **Clinical Value:** IPA calculation for incremental benefit measurement

**Purpose:** This analysis evaluates the gene expression profile's ability to predict melanoma-related mortality while accounting for other causes of death (competing risks). It provides statistical validation of the GEP model's prognostic accuracy through calibration and discrimination metrics, which can inform clinical interpretation of risk predictions.  
**Implementation:** `analyze_gep_mss_validation()` function in `scripts/workflow/objective_4_gep_analysis.R`  
**Outputs:** Standard and competing risk validation reports (.xlsx), cumulative incidence curves (.png), discrimination and DCA summaries/plots when available (.xlsx/.png)  
**Location:** `{cohort}/04_GEP_Validation/b_melanoma_specific_survival/`

#### **Robust Discrimination Approach**
- Implemented: Harrell's C-index (primary), integrated AUC (iAUC); removed Uno's C-index and point AUC.
- Consolidated outputs now include: calibration (ICI, slope, Nam-D'Agostino, Brier), discrimination (Harrell C, iAUC, cumulative/time-averaged where available), decision curves, and IPA.

#### **Secondary Analyses**
- **PRAME-Augmented Models:** Net reclassification index comparing GEP-only vs GEP+PRAME models
- **Missing Data Assessment:** Multiple imputation sensitivity analysis and baseline comparison of GEP-tested vs missing patients
- **Bootstrap Validation:** Optimism-corrected calibration slopes and intercepts (200 bootstrap iterations)
- **Events-per-Endpoint Analysis:** Automatic assessment of statistical power (target: ≥100 events per timepoint)
- **Output Consolidation:** All metrics (calibration + robust discrimination + clinical utility) displayed together in consolidated summaries

---

## Data Limitations and Analysis Constraints

The analysis pipeline includes robust error handling for situations where data limitations prevent certain analyses from completing. This is particularly relevant for smaller cohorts and rare outcomes.

### **Cohort-Specific Limitations**

#### **GKSRS-Only Cohort (n=93)**
- **Step 3 (PFS-2 Analysis):** Insufficient events for survival analysis
  - Only 13 patients with valid PFS-2 data
  - Only 3 total second recurrence events (minimum required: 5)
  - Events concentrated in only 2 treatment groups (GKSRS: 1, TTT: 2)
  - Summary tables are generated, but survival curves and Cox models are skipped

#### **Restricted Cohort (n=169)**
- Generally sufficient sample size for most analyses
- Occasional rare category handling in subgroup analyses due to smaller size than full cohort

#### **Full Cohort (n=263)**
- Generally sufficient sample size for most analyses
- Occasional rare category handling in subgroup analyses

### **Automatic Error Handling**

The analysis pipeline includes built-in safeguards:

1. **Minimum Event Requirements:** 
   - Survival analyses require 5+ total events
   - Cox regression requires 2+ groups with events
   - Logistic regression requires adequate observations per category

2. **Rare Category Management:**
   - Categories with <5 observations are automatically collapsed
   - Variables with insufficient levels after collapsing are excluded from models

3. **Graceful Degradation:**
   - When full analyses cannot be completed, summary statistics are still generated
   - Missing analyses are clearly documented in logs with specific reasons

4. **Comprehensive Logging:**
   - All limitations and skipped analyses are logged with timestamps
   - Detailed error messages explain exactly why analyses were skipped


---

## Survival Analysis Features

### **Restricted Mean Survival Time (RMST) Analysis**

All survival endpoints include comprehensive RMST analysis. RMST is used because hazard ratios from Cox regression only tell us about relative risk, not the actual magnitude of survival benefit in clinically meaningful time units (months/years). RMST quantifies the average survival time difference between treatments, making results more interpretable for clinicians and patients.

#### **RMST Outputs Generated:**
1. **📈 Survival Rate Tables:** 1, 3, 5, 10, and 15-year survival probabilities by treatment
2. **📊 RMST Comparison Tables:** Mean survival time differences (GKSRS vs PBT) at each time point  
3. **📉 P-value Progression Plots:** Visual representation of statistical significance evolution over time

#### **🎨 RMST P-value Progression Plots**
- **Purpose:** Shows how treatment differences evolve across follow-up periods
- **Features:**
  - P-values plotted across multiple time points (1, 3, 5, 10, 15 years)
  - Color-coded significance levels (red = significant, blue = not significant)
  - Reference lines at p = 0.05 and p = 0.01
  - Annotations with exact p-values and RMST differences in months
  - Treatment direction indicators (+ = GKSRS advantage, - = GKSRS disadvantage)
- **Clinical Value:** Identifies optimal time points for treatment comparisons and quantifies survival benefit magnitude
- **Location:** `{cohort}/01_Efficacy/{outcome}/` for OS and PFS analyses

#### **Example Interpretation:**
- **p = 0.033, +2.1 mo** at 3 years = GKSRS provides 2.1 months longer survival (p = 0.033)
- **p = 0.331, -1.2 mo** at 10 years = No significant difference, slight numerical GKSRS disadvantage

### **Proportional Hazards Assumption Testing**

All Cox regression analyses automatically include comprehensive testing of the proportional hazards (PH) assumption using Schoenfeld residuals to detect time-varying treatment effects.

#### **What is the Proportional Hazards Assumption?**
The Cox proportional hazards model assumes that the hazard ratio between treatment groups remains **constant over time**. When this assumption is violated, it means the treatment effect changes over time - for example, "PBT significant survival early on then GKSRS seems to take over."

#### **Files Generated**
For each survival outcome, the analysis creates these files in the `h_proportional_hazards_diagnostics/` directory:

**1. Test Results (`*_proportional_hazards_tests.xlsx`)**
- **P_Value**: Statistical test for each variable
- **PH_Assumption**: "VIOLATED" if p < 0.05, "OK" if p ≥ 0.05
- **Interpretation**: Plain English explanation of the test result

**2. Diagnostic Plots**
- **Individual plots** (`*_schoenfeld_*.png`): One plot per variable showing residuals vs time
- **Combined plot** (`*_schoenfeld_combined.png`): All variables in one figure

**3. Summary Text** (`*_proportional_hazards_summary.txt`)
- Detailed interpretation and recommendations

#### **How to Interpret Results**

**Statistical Tests:**
- **p < 0.05**: **VIOLATION** - The treatment effect changes significantly over time
- **p ≥ 0.05**: **OK** - No evidence that treatment effect changes over time
- **Global test**: Overall test across all variables in the model

**Schoenfeld Residual Plots:**
These plots show if the treatment effect is constant over time:
- **Flat horizontal line around zero**: PH assumption is satisfied
- **Clear trend (slope up or down)**: PH assumption is violated
- **Curved pattern**: Treatment effect changes non-linearly over time

#### **What to Do if PH is Violated**

1. **Time-varying coefficients**: Fit models that allow treatment effects to change over time
2. **Stratification**: Stratify by the violating variable
3. **Piecewise models**: Fit separate models for early vs late time periods
4. **Alternative models**: Consider accelerated failure time models

#### **Clinical Interpretation**

If **treatment_group** violates the PH assumption:
- Early survival advantage may favor one treatment
- Late survival advantage may favor the other treatment
- The overall hazard ratio from Cox regression may be misleading
- Consider reporting separate effects for early vs late periods

**Example Scenario**: Overall Survival analysis shows treatment_group p = 0.02 (VIOLATED)

**Clinical meaning**: 
- The treatment effect is not constant over time
- One treatment may be better in the short term, the other in the long term
- The single hazard ratio from Cox regression doesn't tell the full story
- Need to examine when the treatment effects cross over

**Outputs Location:**
- **Objective 1 (Efficacy)**: `{cohort}/01_Efficacy/h_proportional_hazards_diagnostics/`
- **Objective 3 (PFS-2)**: `{cohort}/03_Repeat_Radiation/b_proportional_hazards_diagnostics/`

### **GEP Analysis (Objective 4: GEP Validation)**

#### **What This Analysis Does and Why It Matters**
- **Purpose**: Validates whether lab-reported gene expression profile (GEP) probabilities actually match real patient outcomes
- **Two Outcomes**: Metastasis-free survival (MFS) and melanoma-specific survival (MSS)
- **Time Points**: 5, 7, and 10 years (clinically relevant follow-up periods)
- **Clinical Value**: Helps determine if GEP testing should guide treatment decisions and patient counseling

#### **What You Get (Output Files and Structure)**

By cohort in `final_data/Analysis/<cohort>/04_GEP_Validation/`:

**`a_metastasis_free_survival/`**
- `*_mfs_validation_summary.xlsx`: Comprehensive workbook with all metric categories (Calibration, Overall Prediction Accuracy, Robust Discrimination, Clinical Utility, Clinical Value)
- `*_mfs_validation_summary.txt`: Human-readable summary of key metrics
- `*_mfs_survival_curves.png`: Kaplan-Meier curves by GEP class with log-rank test

**`b_melanoma_specific_survival/`**
- `*_mss_validation_summary.xlsx`: Comprehensive workbook with competing risk analysis and all metric categories
- `*_mss_validation_summary.txt`: Human-readable summary of analyses performed
- `*_mss_cumulative_incidence_curves.png`: Competing risk CIFs by GEP class

**`unified_summary/`**
- `*_gep_comprehensive_report.txt`: Integrated analysis summary
- `*_gep_comparison_table.xlsx`: Side-by-side MFS vs MSS metrics with all categories
- `*_mfs_consolidated_summary.xlsx`: All MFS metrics consolidated in one view
- `*_mss_consolidated_summary.xlsx`: All MSS metrics consolidated in one view

#### **Understanding the Statistical Terms (In Plain Language)**

**Calibration: "Are the predictions accurate?"**
- **What it measures**: How close predicted risks are to what actually happened
- **Calibration slope**: 
  - **1.0** = Perfect predictions (if GEP says 20% risk, exactly 20% of patients have events)
  - **>1.0** = Predictions too extreme (GEP overestimates high-risk, underestimates low-risk)
  - **<1.0** = Predictions too conservative (GEP underestimates high-risk, overestimates low-risk)
- **ICI (Integrated Calibration Index)**: Lower is better (closer to 0 = better average agreement)
- **Nam-D'Agostino test**: P-value < 0.05 means predictions don't match reality

**Discrimination: "Can the test separate high-risk from low-risk patients?"**
- **What it measures**: How well GEP distinguishes between patients who will vs won't have events
- **Harrell's C-index** (Primary Metric): 
  - **0.5** = No better than random guessing
  - **0.7-0.8** = Good discrimination
  - **0.8-0.9** = Very good discrimination  
  - **>0.9** = Excellent discrimination
- **Integrated AUC over time periods**: More robust than point estimates at specific timepoints
  - **Why this approach**: Our data has events spread across time, not concentrated at exact 5yr/7yr/10yr marks
  - **Clinical benefit**: Measures discrimination ability across entire follow-up period, not arbitrary timepoints

**Observed vs Expected: "Do the numbers match reality?"**
- **What it measures**: Compares predicted event rates to actual event rates in each risk group
- **O/E ratio**:
  - **1.0** = Perfect prediction
  - **>1.0** = Model underestimated risk (more events than predicted)
  - **<1.0** = Model overestimated risk (fewer events than predicted)

**Decision Curve Analysis: "Would using this test help patients?"**
- **What it measures**: Whether using GEP testing to make treatment decisions would benefit patients overall
- **Net benefit**: How much better the test is than treating everyone or treating no one
  - **Positive values**: Test strategy is better than alternatives
  - **Negative values**: Test strategy is worse than alternatives
  - **Magnitude**: Higher positive values = more clinical benefit
  - **Example**: Net benefit of 0.15 means 15% more patients benefit from using the test vs treating everyone
- **Clinical interpretation**: If "GEP" line is above "Treat All" and above 0, the test provides clinical value

**Net Reclassification Index (NRI): "Does adding a new biomarker improve risk stratification?"**
- **What it measures**: How much better a new model (e.g., GEP + PRAME) reclassifies patients compared to the base model (GEP only)
- **Statistical procedure**: 
  1. **Risk categorization**: Patients are classified into risk categories (low/medium/high) using both models
  2. **Reclassification counting**: Count how many patients move between categories when adding the new biomarker
  3. **Event-based weighting**: Weight reclassifications by whether patients actually had events
  4. **NRI calculation**: NRI = (Proportion of events correctly reclassified up) - (Proportion of events incorrectly reclassified down) + (Proportion of non-events correctly reclassified down) - (Proportion of non-events incorrectly reclassified up)
- **NRI interpretation**:
  - **NRI = 0**: No improvement in reclassification (perfect balance between correct and incorrect moves)
  - **NRI > 0**: New biomarker improves reclassification
  - **NRI < 0**: New biomarker worsens reclassification
  - **Clinical significance**: NRI of 0 means the new biomarker doesn't add meaningful predictive value beyond what's already available
- **Why NRI = 0 occurs**:
  - **Perfect balance**: Any patients moved to higher risk categories are exactly balanced by patients moved to lower risk categories
  - **No net improvement**: The new biomarker doesn't provide additional discriminatory information
  - **Clinical implication**: Adding the biomarker (e.g., PRAME) may not be worth the additional cost/complexity

#### **Technical Implementation: How the Analysis Works**

**Data Preparation**:
- **GEP Classes**: Patients grouped into risk classes (Class 1 = low risk, Class 2 = high risk)
- **Time variable**: Years from treatment to death or last follow-up
- **Event Coding**: 0 = censored, 1 = event of interest, 2 = competing event (for MSS)
- **Validation approach**: Both models tested against GEP predictions using Brier scores and IPA metrics

**Statistical Methods**:
- **Calibration**: Tests whether predicted risks match observed risks using logistic regression
- **Discrimination**: Measures how well the model separates high vs low risk patients
- **Observed vs Expected**: Compares actual event rates to predicted rates in each risk class
- **Decision Curves**: Evaluates clinical utility by comparing net benefit of using the test

**Validation Metrics**:
- **Calibration**: Calibration slope, ICI, Nam-D'Agostino test p-value
- **Overall Prediction Accuracy**: Brier Score with robust fallback methods
- **Robust Discrimination**: Harrell's C-index (primary), integrated AUC over time periods
- **Clinical Utility**: Decision curve analysis with net benefit assessment
- **Clinical Value**: IPA (Index of Prediction Accuracy) for incremental benefit measurement
- **Observed vs Expected**: O/E ratios with Poisson confidence intervals
- **Bootstrap Confidence Intervals**: Used internally to provide uncertainty estimates
- **Optimism Correction**: Adjusts for overfitting by comparing bootstrap results to original data
- **Stratified Sampling**: Ensures each GEP class is properly represented in validation samples

**Clinical Interpretation**:
- **Concordant results**: Both approaches yield similar conclusions about GEP validity
- **Discordant results**: May indicate that GEP affects instantaneous risk differently than cumulative risk
- **Complementary value**: Both perspectives enhance understanding of GEP clinical utility

#### **Why Different Plot Types for MFS vs MSS?**

**MFS (Metastasis-Free Survival) = Standard Survival Analysis**
- **Event**: Metastasis (first occurrence)
- **Analysis**: **Kaplan-Meier curves** - shows probability of staying metastasis-free over time
- **Why**: Single, well-defined event with other events treated as censoring

**MSS (Melanoma-Specific Survival) = Competing Risks Analysis**
- **Event**: Melanoma death
- **Competing Event**: Non-melanoma death (e.g., heart attack, car accident)
- **Analysis**: **Cumulative Incidence Functions (CIF)** - shows probability of melanoma death over time
- **Why**: Two types of events that can happen, with non-melanoma death "competing" with melanoma death

**Why Not the Other Way Around?**
- **MFS with CIF**: Doesn't make sense - metastasis is a single event type
- **MSS with KM**: Would be wrong - would overestimate melanoma survival by treating other deaths as censoring

**In the Data**:
- **MFS**: 16 metastasis events out of 86 patients → KM curves show metastasis-free survival probability
- **MSS**: 15 melanoma deaths, 0 competing deaths → CIF shows cumulative probability of melanoma death

#### **Competing Risk Analysis: Two Different Models**

Both cause-specific Cox regression and Fine-Gray subdistribution hazards are appropriate but answer **different clinical questions**:

**Cause-Specific Cox Regression (`riskRegression::CSC`)**
- **Question:** "What factors affect the **rate** of melanoma death among patients who haven't died yet?"
- **Interpretation:** How does GEP risk affect the hazard of dying from melanoma at any given time
- **Clinical Use:** Understanding biological mechanisms, treatment effects on disease progression
- **Result:** Cause-specific hazard ratio (HR)

**Fine-Gray Subdistribution Hazards (`riskRegression::FGR`)**
- **Question:** "What factors affect the **cumulative probability** of eventually dying from melanoma?"
- **Interpretation:** How does GEP risk affect the absolute risk of melanoma death over time
- **Clinical Use:** Patient counseling about long-term prognosis, clinical prediction models
- **Result:** Subdistribution hazard ratio (SHR)

##### **Practical Example**

Consider a high-risk GEP patient:

- **Cause-specific HR = 3.0**: "Among patients still alive, high-risk GEP patients have 3x the rate of melanoma death"
- **Subdistribution SHR = 2.5**: "High-risk GEP patients have 2.5x higher cumulative probability of melanoma death"

The cause-specific HR is typically larger because it conditions on survival, while the subdistribution SHR accounts for competing mortality reducing overall melanoma death risk.

#### **Robust Discrimination Implementation**

**What We Implemented:**
- **Robust Discrimination Strategy**: Replaced fragile Uno's C-index and time-dependent AUC with Harrell's C-index and integrated AUC
- **Complete Output Consolidation**: All metrics (calibration + robust discrimination + clinical utility) displayed together
- **Statistical Rigor**: Proper categorization of metrics by statistical function (calibration vs discrimination vs clinical utility)

**Why This Approach:**
Our data has events spread across time, not concentrated at exact 5yr/7yr/10yr timepoints. Traditional time-dependent metrics were fragile and clinically nonsensical.

**Current Implementation Status:**
- ✅ **Calibration Metrics**: ICI, Slope, Nam-D'Agostino - all working perfectly
- ✅ **Overall Prediction Accuracy**: Brier Score with robust fallback methods
- ✅ **Robust Discrimination**: Harrell's C-index (primary) and integrated AUC over time periods
- ✅ **Clinical Utility**: Decision curve analysis with net benefit assessment
- ✅ **Clinical Value**: IPA calculation for incremental benefit measurement
- ✅ **Output Consolidation**: All metrics displayed together in consolidated summaries

**What You Get Now:**
- **Comprehensive validation reports** with all metric categories properly grouped
- **Robust discrimination metrics** that work reliably with our data distribution
- **Clinical utility assessment** through decision curve analysis and IPA
- **Transparent documentation** of why certain metrics can't be calculated with our data

#### **Statistical Metric Categorization**

**Proper Statistical Grouping:**
The GEP analysis now properly categorizes metrics by their statistical function, ensuring clinical researchers can interpret results correctly:

**Calibration Metrics** (How well predicted probabilities match observed frequencies):
- **ICI (Integrated Calibration Index)**: Measures overall calibration across the entire risk range
- **Calibration Slope**: Linear relationship between predicted and observed risks
- **Nam-D'Agostino Test**: Statistical test for calibration goodness-of-fit
- **Calibration Intercept**: Systematic bias in predictions

**Overall Prediction Accuracy** (Global measure of prediction quality):
- **Brier Score**: Mean squared error between predicted probabilities and observed outcomes
- **Note**: Brier Score is NOT a calibration metric - it measures overall prediction accuracy

**Robust Discrimination Metrics** (Ability to distinguish between high and low risk patients):
- **Harrell's C-index**: **PRIMARY DISCRIMINATION METRIC** - Overall discrimination ability across entire follow-up period
- **Integrated AUC (iAUC)**: Area under ROC curve integrated over time periods (more robust than point estimates)

**Clinical Utility Metrics** (Practical value for clinical decision-making):
- **Decision Curve Analysis**: Net benefit across different risk thresholds
- **Net Benefit**: Clinical utility compared to treating all or none

**Clinical Value Assessment** (Incremental value over baseline):
- **IPA (Index of Prediction Accuracy)**: Improvement in prediction accuracy over null model

**Why This Categorization Matters:**
1. **Calibration ≠ Discrimination**: A model can be perfectly calibrated but have poor discrimination
2. **Brier Score is Global**: Measures overall prediction accuracy, not calibration or discrimination specifically
3. **IPA is Clinical Value**: Measures incremental benefit over baseline, not model performance per se
4. **Proper Grouping Essential**: Incorrect categorization leads to misinterpretation of results

---

## Requirements

### **R Version**
- **R >= 4.4.0**

### **Required R Packages**
```r
# Core data manipulation and analysis
tidyverse, readxl, writexl, lubridate, janitor, openxlsx

# Statistical analysis and tables  
gtsummary, survival, survminer, survRM2, gt, broom.helpers, parameters, cardx

# Visualization and plots
forestploter, grid, cowplot, ggplot2

# Advanced GEP validation (Objective 4)
rms, pec, survcomp, riskRegression, cmprsk, pROC, rmda, VIM, mice

# Testing and documentation
testthat, usethis
```

### **Installation**

Running `scripts/load_all.R` in an R session will install the required packages automatically.

---

## Usage

### **1. 📁 Prepare Data**
Place your raw Excel data file in the `final_data/Original Files/` directory.

### **2. ⚙️ Configure Analysis**
Edit `scripts/utils/config_constants.R` to set:
```r
# Input filename
INPUT_FILENAME <- "Ocular Melanoma Master Spreadsheet REVISED FOR STATS (5-10-25, TJM).xlsx"

# Analysis settings
RECREATE_ANALYTIC_DATASETS <- TRUE  # Set to TRUE for fresh analysis
USE_LOGS <- TRUE                    # Enable detailed logging
VERBOSE <- TRUE                     # Show detailed progress
```

### **3. 🚀 Run Analysis**

#### **Option A: Full Analysis (All Objectives, All Cohorts)**
```r
# Run the complete pipeline for all cohorts
# Uncomment the main_execution() line in scripts/main.R, then run:
source("scripts/main.R")
```

#### **Option B: Single Cohort Analysis**
```r
# Run all objectives for a specific cohort
run_my_analysis("uveal_melanoma_full_cohort")
run_my_analysis("uveal_melanoma_restricted_cohort")
run_my_analysis("uveal_melanoma_gksrs_only_cohort")
```

#### **Option C: Specific Objective Analysis**
```r
# Run individual objectives for targeted analysis
run_specific_objective("uveal_melanoma_full_cohort", 0)  # Data processing only
run_specific_objective("uveal_melanoma_full_cohort", 1)  # Primary outcomes only
run_specific_objective("uveal_melanoma_full_cohort", 2)  # Safety/toxicity only
run_specific_objective("uveal_melanoma_full_cohort", 3)  # Repeat radiation only
run_specific_objective("uveal_melanoma_full_cohort", 4)  # GEP validation only
```

### **4. 📊 Analysis Execution**
The pipeline executes:
- **Objective 0:** Data cleaning and validation
- **Objective 1:** Primary outcomes (efficacy analysis)
- **Objective 2:** Safety and toxicity endpoints
- **Objective 3:** Repeat radiation efficacy (PFS-2)
- **Objective 4:** GEP predictive accuracy validation

### **5. 📋 Logging and Monitoring**
- **Log Files:** Detailed logs saved to `logs/` directory with timestamps
- **Progress Tracking:** Real-time progress updates during analysis
- **Error Handling:** Comprehensive error reporting and recovery

### **6. 🔧 Current Configuration**
The current `scripts/main.R` is configured to run specific objectives for debugging:
- **To Run Full Analysis:** Uncomment the `main_execution()` line
- **To Run Specific Objectives:** Uncomment the desired `run_specific_objective()` lines

---

## License

*Research use only - no license currently specified.*
---



