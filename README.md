# Uveal Melanoma Data Processing & Analysis

## Overview

This project provides a complete pipeline for processing, cleaning, and analyzing clinical data for uveal melanoma patients, with a focus on comparing outcomes between Gamma Knife Stereotactic Radiosurgery (GKSRS) and plaque brachytherapy. The workflow includes data cleaning, cohort creation, derivation of analytic variables, summary table generation, and statistical analyses (including survival and subgroup analyses).

**Author:** Nicholas Camarda  
**Date:** 5/10/2025

---

## Cohort Definitions

The analysis includes three distinct patient cohorts based on tumor characteristics and treatment eligibility:

### 1. **Full Cohort** (`uveal_melanoma_full_cohort`)
- **Definition:** All patients who received either GKSRS or plaque brachytherapy
- **Sample Size:** ~263 patients
- **Purpose:** Primary analysis comparing outcomes between all GKSRS and plaque patients
- **Clinical Rationale:** Represents the real-world effectiveness comparison across all tumor sizes and locations

### 2. **Restricted Cohort** (`uveal_melanoma_restricted_cohort`) 
- **Definition:** Patients eligible for **both** GKSRS and plaque brachytherapy
- **Inclusion Criteria:**
  - Tumor diameter ≤ 20mm **AND**
  - Tumor height ≤ 10mm **AND** 
  - No optic nerve involvement
- **Sample Size:** ~169 patients  
- **Purpose:** Balanced comparison minimizing treatment selection bias
- **Clinical Rationale:** These patients could have received either treatment, making the comparison more equitable and reducing confounding by tumor characteristics

### 3. **GKSRS-Only Cohort** (`uveal_melanoma_gksrs_only_cohort`)
- **Definition:** Patients who were **ineligible** for plaque brachytherapy
- **Inclusion Criteria (any of the following):**
  - Tumor diameter > 20mm **OR**
  - Tumor height > 10mm **OR**
  - Optic nerve involvement present
- **Sample Size:** ~93 patients
- **Purpose:** Outcomes analysis for patients with contraindications to plaque therapy
- **Clinical Rationale:** Demonstrates GKSRS effectiveness in cases where plaque brachytherapy is not feasible due to tumor size or proximity to critical structures

> **Note:** The restricted cohort provides the most methodologically rigorous comparison since both treatment groups had similar baseline tumor characteristics and treatment options. The full cohort reflects real-world practice patterns, while the GKSRS-only cohort shows outcomes in more challenging cases.

---

## Directory Structure

```
project_working_directory/
├── data/                        # Place for raw data files
├── final_data/
│   ├── Analytic Dataset/        # Processed and cleaned datasets (RDS, Excel)
│   ├── Analysis/
│   │   ├── gksrs/               # GKSRS-only cohort results
│   │   │   ├── tables/
│   │   │   │   ├── primary_outcomes/
│   │   │   │   │   ├── recurrence/
│   │   │   │   │   ├── metastatic_progression/
│   │   │   │   │   ├── overall_survival/
│   │   │   │   │   ├── progression_free_survival/
│   │   │   │   │   └── tumor_height_change/
│   │   │   │   │       ├── primary_analysis/          # Main height analysis (no baseline adjustment)
│   │   │   │   │       ├── sensitivity_analysis/      # Alternative main analysis (with baseline adjustment)
│   │   │   │   │       └── subgroup_interactions/     # Treatment effect heterogeneity testing (differ across subgroups?)
│   │   │   │   │           ├── without_baseline_height/  # Primary subgroup analysis
│   │   │   │   │           └── with_baseline_height/     # Sensitivity subgroup analysis
│   │   │   │   └── treatment_duration/
│   │   │   └── figures/
│   │   ├── uveal_full/          # Full cohort results (same structure as above)
│   │   └── uveal_restricted/    # Restricted cohort results (same structure as above)
│   └── Original Files/          # Raw, unmodified data files and documentation
├── logs/                        # Analysis logs (when enabled)
├── scripts/
│   ├── data_processing.R        # Data cleaning and cohort creation
│   ├── main.R                   # Main script to run the full pipeline
│   ├── uveal_melanoma_analysis.R# Statistical analysis and visualization
│   └── unit_tests.R             # Unit tests for data processing functions
└── README.md
```

### Analysis Directory Structure Explained

Each cohort directory (`gksrs/`, `uveal_full/`, `uveal_restricted/`) contains:

#### **Tables Structure:**
- **`primary_outcomes/`**: Core clinical endpoints
  - **`recurrence/`**: Local tumor recurrence analysis
  - **`metastatic_progression/`**: Distant metastasis analysis  
  - **`overall_survival/`**: Death from any cause
  - **`progression_free_survival/`**: Time to progression or death
  - **`tumor_height_change/`**: Change in tumor dimensions over time
    - **`primary_analysis/`**: Main regression analysis without baseline height adjustment (avoids overadjustment bias)
    - **`sensitivity_analysis/`**: Alternative analysis including baseline height as covariate (tests robustness)
    - **`subgroup_interactions/`**: Tests whether treatment effects vary across patient subgroups
      - **`without_baseline_height/`**: Primary subgroup analysis (interaction tests without baseline adjustment)
      - **`with_baseline_height/`**: Sensitivity subgroup analysis (interaction tests with baseline adjustment)
- **`treatment_duration/`**: Analysis of treatment characteristics and duration

#### **Primary vs Sensitivity Analysis:**
- **Primary Analysis**: Preferred approach that avoids potential overadjustment bias when analyzing change scores
- **Sensitivity Analysis**: Alternative approach including baseline values to test robustness of findings
- **Subgroup Interactions**: Tests whether treatment effects differ across patient characteristics (age, sex, tumor location, etc.)

#### **Files Generated:**
- **Tables**: HTML files with publication-ready formatted results
- **Data**: RDS files with detailed statistical results for further analysis
- **Figures**: Survival curves, forest plots, and other visualizations

---

## Objectives Accomplished and Roadmap

### **✅ COMPLETED: Objective #1 - Efficacy Analysis**

All efficacy analyses comparing plaque brachytherapy vs GKSRS have been implemented and are generating results:

#### **1a. ✅ Recurrence Rates**
- **Status:** COMPLETE - Time-to-event analysis with logistic regression
- **Output:** Kaplan-Meier curves, Cox proportional hazards models, summary tables
- **Location:** `final_data/Analysis/{cohort}/tables/primary_outcomes/recurrence/`

#### **1b. ✅ Metastatic Progression Rates** 
- **Status:** COMPLETE - Time-to-event analysis with logistic regression
- **Output:** Kaplan-Meier curves, Cox proportional hazards models, summary tables
- **Location:** `final_data/Analysis/{cohort}/tables/primary_outcomes/metastatic_progression/`

#### **1c. ✅ Overall Survival**
- **Status:** COMPLETE - Survival analysis with confounder adjustment
- **Output:** Kaplan-Meier curves, Cox proportional hazards models, median survival estimates, RMST p-value progression plots
- **Location:** `final_data/Analysis/{cohort}/tables/primary_outcomes/overall_survival/`

#### **1d. ✅ Progression-Free Survival**
- **Status:** COMPLETE - Composite endpoint (progression OR death)
- **Output:** Kaplan-Meier curves, Cox proportional hazards models, summary tables, RMST p-value progression plots
- **Location:** `final_data/Analysis/{cohort}/tables/primary_outcomes/progression_free_survival/`

#### **1e. ✅ Tumor Height Changes**
- **Status:** COMPLETE - Linear regression analysis with primary/sensitivity approaches
- **Implementation:** 
  - Primary analysis (without baseline height adjustment)
  - Sensitivity analysis (with baseline height adjustment)
  - Proper handling of pre-retreatment measurements for recurrent cases
- **Output:** Mean changes by treatment group, regression models, publication-ready tables
- **Location:** `final_data/Analysis/{cohort}/tables/primary_outcomes/tumor_height_change/`

#### **1f. ✅ Subgroup Analysis**
- **Status:** COMPLETE - Treatment effect heterogeneity testing across patient characteristics
- **Subgroups Tested:** Age, sex, tumor location, T-stage, tumor height, tumor diameter, GEP class, optic nerve involvement
- **Methods:** Interaction terms in regression models, forest plots, primary/sensitivity analyses
- **Output:** Interaction p-values, subgroup-specific treatment effects, formatted tables
- **Location:** `final_data/Analysis/{cohort}/tables/primary_outcomes/tumor_height_change/subgroup_interactions/`

### **✅ COMPLETED: Objective #2 - Safety/Toxicity Analysis**

All safety and toxicity analyses have been implemented and are generating results:

#### **2a. ✅ Vision Changes**
- **Status:** COMPLETE - Linear regression analysis comparing treatment groups
- **Implementation:** Proper handling of pre-retreatment vision for recurrent cases
- **Output:** Mean vision changes, statistical comparisons, regression models
- **Location:** `final_data/Analysis/{cohort}/tables/safety_toxicity/vision_change/`

#### **2b. ✅ Radiation Sequelae Rates**
- **Status:** COMPLETE - All three radiation sequelae implemented with logistic regression

**2b1. ✅ Retinopathy**
- **Implementation:** Binary outcome analysis with confounder adjustment
- **Output:** Rate summaries (.xlsx), descriptive tables (.html), logistic regression (.html)
- **Location:** `final_data/Analysis/{cohort}/tables/safety_toxicity/radiation_sequelae/retinopathy/`

**2b2. ✅ Neovascular Glaucoma (NVG)**
- **Implementation:** Binary outcome analysis with confounder adjustment  
- **Output:** Rate summaries (.xlsx), descriptive tables (.html), logistic regression (.html)
- **Location:** `final_data/Analysis/{cohort}/tables/safety_toxicity/radiation_sequelae/nvg/`

**2b3. ✅ Serous Retinal Detachment (SRD)**
- **Implementation:** Radiation-induced cases only (excludes mass-induced), binary outcome analysis
- **Special Filtering:** Only includes patients with `srd_cause == "Radiation"` as per objectives
- **Output:** Rate summaries (.xlsx), descriptive tables (.html), logistic regression (.html)
- **Location:** `final_data/Analysis/{cohort}/tables/safety_toxicity/radiation_sequelae/srg/`

### **🚧 TODO: Objective #3 - Repeat Radiation Efficacy** 

#### **3a. ⏳ Progression-Free Survival-2**
- **Status:** NOT YET IMPLEMENTED
- **Requirements:** 
  - Filter to patients with local recurrence (`recurrence1 == "Y"`)
  - Compare GKSRS vs enucleation vs TTT (from `recurrence1_treatment`)
  - Calculate PFS-2 from `recurrence1_treatment_date` to `recurrence2_date` or `last_followup`
- **Complexity:** Medium - Requires filtering recurrent patients and new time-to-event calculation
- **Estimated Implementation:** 1-2 hours

### **🚧 TODO: Objective #4 - Gene Expression Profile (GEP) Predictive Accuracy**

#### **4a. ⏳ Metastasis-Free Survival (MFS) Validation**
- **Status:** NOT YET IMPLEMENTED  
- **Requirements:**
  - Compare actual 5-year MFS rates vs expected rates (`biopsy1_gep_mfs`)
  - Calibration analysis and concordance statistics
- **Complexity:** Medium-High - Requires survival prediction validation methods

#### **4b. ⏳ Melanoma-Specific Survival (MSS) Validation**
- **Status:** NOT YET IMPLEMENTED
- **Requirements:**
  - Compare actual 5-year MSS rates vs expected rates (`biopsy1_gep_mss`) 
  - Calibration analysis and concordance statistics
- **Complexity:** Medium-High - Requires survival prediction validation methods

### **🎯 Implementation Priority**

1. **NEXT:** Objective #3 (Repeat Radiation) - Straightforward extension of existing survival analysis methods
2. **THEN:** Objective #4 (GEP Validation) - More complex validation methodology required

### **📊 Current Data Coverage**

- **Full Cohort:** 263 patients (all analyses)
- **Restricted Cohort:** ~169 patients (eligible for both treatments) 
- **GKSRS-Only Cohort:** ~93 patients (ineligible for plaque therapy)

### **⚙️ Technical Implementation Notes**

- All analyses adjust for confounders where appropriate
- Rare event handling implemented (threshold-based confounder selection)
- Primary/sensitivity analysis framework for robust findings
- Consistent Excel (.xlsx) output format throughout
- Publication-ready HTML tables generated for all analyses
- Proper handling of missing data and edge cases

---

## Features

- **Data Cleaning:** Standardizes missing values, removes duplicates, and harmonizes variable formats.
- **Cohort Creation:** Automatically splits data into full, restricted (eligible for both treatments), and GKSRS-only cohorts.
- **Derived Variables:** Calculates age at diagnosis, follow-up time, time-to-event variables, and event indicators.
- **Summary Tables:** Generates publication-ready tables of baseline characteristics and treatment duration.
- **Statistical Analysis:** Includes logistic regression, Kaplan-Meier survival analysis, Cox proportional hazards models, Restricted Mean Survival Time (RMST) analysis, and subgroup/forest plot analyses.
- **Reproducibility:** All steps are scripted in R and can be run end-to-end.
- **Unit Testing:** Includes a suite of tests for core data processing functions.

---

## Advanced Survival Analysis Features

### **Restricted Mean Survival Time (RMST) Analysis**

In addition to standard Cox proportional hazards models, the pipeline includes comprehensive RMST analysis for survival endpoints:

#### **What is RMST?**
- **Definition:** Mean survival time up to a specific time point (e.g., 5 years)
- **Advantage:** Provides clinically interpretable differences in months of survival
- **Robustness:** Does not require proportional hazards assumption

#### **RMST Outputs Generated:**
1. **Survival Rate Tables:** 1, 3, 5, 10, and 15-year survival probabilities by treatment group
2. **RMST Comparison Tables:** Mean survival time differences (GKSRS vs Plaque) at each time point
3. **P-value Progression Plots:** Visual representation of statistical significance over time

#### **RMST P-value Progression Plots**
- **Purpose:** Shows how treatment differences evolve over follow-up periods
- **Features:**
  - P-values plotted across 1, 3, 5, 10, and 15-year time points
  - Color-coded significance (red = significant, blue = not significant)
  - Reference lines at p = 0.05 and p = 0.01
  - Annotations showing exact p-values and RMST differences in months
  - Clear indication of treatment direction (+ = GKSRS advantage, - = GKSRS disadvantage vs Plaque)
- **Clinical Value:** Identifies optimal time points for comparing treatments and quantifies survival benefit magnitude
- **Location:** `final_data/Analysis/{cohort}/figures/*_rmst_pvalue_progression.png`

#### **Example Interpretation:**
- **p = 0.033, +2.1 mo** at 3 years = GKSRS provides 2.1 months longer survival than Plaque (p = 0.033)
- **p = 0.331, -1.2 mo** at 10 years = No significant difference, slight numerical disadvantage for GKSRS

---

## Requirements

- **R (>= 4.0.0)**
- **R Packages:**
  - tidyverse
  - readxl
  - writexl
  - lubridate
  - gtsummary
  - janitor
  - DiagrammeR
  - DiagrammeRsvg
  - rsvg
  - gt
  - survival
  - survminer
  - survRM2
  - forestploter
  - grid
  - cowplot
  - testthat (for unit tests)

Install all required packages with:

```r
install.packages(c(
  "tidyverse", "readxl", "writexl", "lubridate", "gtsummary", "janitor",
  "DiagrammeR", "DiagrammeRsvg", "rsvg", "gt", "survival", "survminer", "survRM2",
  "forestploter", "grid", "cowplot", "testthat"
))
```

---

## Usage

### 1. Prepare Data

- Place your raw Excel data file(s) in `final_data/Original Files/`.

### 2. Run the Pipeline

From the R console or RStudio, run:

```r
# Run the main script
source("scripts/main.R")
```

This will:
- Clean and process the raw data
- Create analytic datasets for each cohort
- Generate summary tables and figures
- Save all outputs in `final_data/Analytic Dataset/` and `final_data/Analysis/`

### 3. Unit Testing

To validate the data processing pipeline:

```r
library(testthat)
source("scripts/data_processing.R")
testthat::test_file("scripts/unit_tests.R")
```

---

## Outputs

- **Processed Datasets:** `.rds` and `.xlsx` files in `final_data/Analytic Dataset/`
- **Tables:** HTML and CSV tables in `final_data/Analysis/[cohort]/tables/`
- **Figures:** PNG and PDF plots in `final_data/Analysis/[cohort]/figures/`

---

## Data Dictionary

A detailed data dictionary is available in `final_data/Original Files/Data Dictionary.xlsx`.

---

## Sample Results

- Baseline characteristics: `final_data/Analysis/uveal_full/tables/uveal_full_baseline_characteristics.html`
- Survival curves: `final_data/Analysis/uveal_full/figures/uveal_full_Overall Survival Probability_survival.png`
- Treatment duration: `final_data/Analysis/uveal_full/tables/uveal_full_treatment_duration_summary.html`

---

## License

*No license currently.*

---
