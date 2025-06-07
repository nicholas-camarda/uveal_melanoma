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
│   │   │   │   │       ├── sensitivity_analysis/      # Alternative analysis (with baseline adjustment)
│   │   │   │   │       └── subgroup_interactions/     # Treatment effect heterogeneity testing
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

## Features

- **Data Cleaning:** Standardizes missing values, removes duplicates, and harmonizes variable formats.
- **Cohort Creation:** Automatically splits data into full, restricted (eligible for both treatments), and GKSRS-only cohorts.
- **Derived Variables:** Calculates age at diagnosis, follow-up time, time-to-event variables, and event indicators.
- **Summary Tables:** Generates publication-ready tables of baseline characteristics and treatment duration.
- **Statistical Analysis:** Includes logistic regression, Kaplan-Meier survival analysis, Cox proportional hazards models, and subgroup/forest plot analyses.
- **Reproducibility:** All steps are scripted in R and can be run end-to-end.
- **Unit Testing:** Includes a suite of tests for core data processing functions.

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
  - forestploter
  - grid
  - cowplot
  - testthat (for unit tests)

Install all required packages with:

```r
install.packages(c(
  "tidyverse", "readxl", "writexl", "lubridate", "gtsummary", "janitor",
  "DiagrammeR", "DiagrammeRsvg", "rsvg", "gt", "survival", "survminer",
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
