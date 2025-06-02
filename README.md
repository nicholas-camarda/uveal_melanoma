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
│   │   ├── uveal_full/          # Full cohort results  
│   │   └── uveal_restricted/    # Restricted cohort results
│   └── Original Files/          # Raw, unmodified data files and documentation
├── scripts/
│   ├── data_processing.R        # Data cleaning and cohort creation
│   ├── main.R                   # Main script to run the full pipeline
│   ├── uveal_melanoma_analysis.R# Statistical analysis and visualization
│   └── unit_tests.R             # Unit tests for data processing functions
└── README.md
```

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
