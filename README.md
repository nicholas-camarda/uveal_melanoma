# Uveal Melanoma Data Processing & Analysis

## Overview

This project provides a complete pipeline for processing, cleaning, and analyzing clinical data for uveal melanoma patients, with a focus on comparing outcomes between Gamma Knife Stereotactic Radiosurgery (GKSRS) and plaque brachytherapy. The workflow includes data cleaning, cohort creation, derivation of analytic variables, summary table generation, and statistical analyses (including survival and subgroup analyses).

**Author:** Nicholas Camarda  
**Date:** 5/10/2025

---

## Directory Structure

```
project_working_directory/
├── data/                        # Place for raw data files
├── final_data/
│   ├── Analytic Dataset/        # Processed and cleaned datasets (RDS, Excel)
│   ├── Analysis/
│   │   ├── gksrs/
│   │   ├── uveal_full/
│   │   └── uveal_restricted/
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
- (Optional) Review the data dictionary in `final_data/Original Files/Data Dictionary.xlsx` for variable definitions.

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
