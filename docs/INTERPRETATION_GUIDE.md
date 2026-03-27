# Interpretation Guide

This guide is for reading the generated outputs after the pipeline has run. It explains how to navigate the folders, read the workbooks, and interpret the tables and figures in plain language.

It is not the canonical source for variable derivations or statistical assumptions:

- Use [CALCULATIONS.md](CALCULATIONS.md) for how values and endpoints are constructed.
- Use [STATISTICAL_METHODS.md](STATISTICAL_METHODS.md) for formal model definitions, thresholds, and validation metrics.
- Use [TECHNICAL.md](TECHNICAL.md) for workflow internals and artifact contracts.

---

## Table of Contents

- [Quick Start](#quick-start)
- [Reading Summary Tables](#reading-summary-tables)
- [Understanding Regression Outputs](#understanding-regression-outputs)
- [Interpreting Survival Curves](#interpreting-survival-curves)
- [Reading Forest Plots](#reading-forest-plots)
- [Understanding RMST Analysis](#understanding-rmst-analysis)
- [Interpreting Proportional Hazards Diagnostics](#interpreting-proportional-hazards-diagnostics)
- [Understanding GEP Analysis](#understanding-gep-analysis)
- [Understanding PRAME Enhancement Outputs](#understanding-prame-enhancement-outputs)
- [Clinical Significance vs Statistical Significance](#clinical-significance-vs-statistical-significance)
- [Common Pitfalls](#common-pitfalls)
- [Troubleshooting](#troubleshooting)

---

## Quick Start

If you only need the shortest route:

1. Find the cohort folder under `~/ProjectsRuntime/uveal_melanoma/Analysis/`.
2. Open the objective folder that matches your question.
3. Start with the consolidated `.xlsx` workbook or summary `.html` artifact in that folder.
4. Use this guide to interpret what you are seeing.
5. If you need to know how a value was calculated, move to [CALCULATIONS.md](CALCULATIONS.md). If you need the formal method, move to [STATISTICAL_METHODS.md](STATISTICAL_METHODS.md).

### Finding Your Analysis

**Step 1:** Identify your cohort
- Full cohort (n=260): `~/ProjectsRuntime/uveal_melanoma/Analysis/uveal_full/`
- Restricted cohort (n=167): `~/ProjectsRuntime/uveal_melanoma/Analysis/uveal_restricted/`
- GKSRS-only cohort (n=92): `~/ProjectsRuntime/uveal_melanoma/Analysis/gksrs/`

**Step 2:** Navigate to objective folder
- Objective 0: `00_General/` (baseline characteristics, patient flow)
- Objective 1: `01_Efficacy/` (survival, recurrence, tumor height)
- Objective 2: `02_Safety/` (vision changes, complications)
- Objective 3: `03_Repeat_Radiation/` (PFS-2 for recurrence patients)
- Objective 4: `04_GEP_Validation/` (predictive accuracy)

**Step 3:** Open relevant files
- `.xlsx` - Summary statistics, regression tables
- `.html` - Interactive regression tables with details
- `.png` - Survival curves, forest plots, RMST visualizations

### Key Files in Each Analysis

| File Pattern | Content | When to Use |
|--------------|---------|-------------|
| `*_summary.xlsx` | Event rates, means, medians | Quick overview of raw data |
| `*_cox_coxph.html` | Cox regression models | Adjusted treatment effects for survival |
| `*_logistic_glm.html` | Logistic regression | Adjusted odds ratios for binary outcomes |
| `*_linear_lm.html` | Linear regression | Mean differences for continuous outcomes |
| `*_km.png` | Kaplan-Meier plots | Visual survival comparison |
| `*_rmst_pvalue_progression.png` | RMST p-value progression plots | Alternative time-horizon view when PH is weak or RMST is the focus |
| `*_subgroup_forest_plot.png` | Subgroup analysis | Treatment effect heterogeneity |

---

## Reading Summary Tables

### Baseline Characteristics Tables

**File:** `00_General/baseline_characteristics/baseline_characteristics.xlsx`

**Structure:**
- Rows: Patient characteristics (age, sex, tumor features)
- Columns: Treatment groups (PBT, GKSRS) + Overall
- Values: n (%), median [IQR], mean (SD)

**How to Read:**

```
Characteristic          PBT (n=100)    GKSRS (n=60)    p-value
Age at diagnosis        62 [52, 71]    65 [55, 73]     0.23
Sex                                                     0.45
  Male                  55 (55%)       30 (50%)
  Female                45 (45%)       30 (50%)
Initial tumor height    5.2 (2.1)      5.5 (2.3)       0.41
```

**Interpretation:**
- **Age:** Median PBT patient was 62 years old; GKSRS was 65 years
- **Sex:** Similar gender distribution (p=0.45, not significantly different)
- **Tumor height:** Mean PBT height was 5.2 mm; GKSRS was 5.5 mm (p=0.41, similar)
- **P-values:** Test whether groups differ at baseline (ideally p > 0.05, indicating balance)

**Clinical Insight:** Groups well-balanced at baseline → fair comparison

### Event Rate Tables

**File:** `{objective}/{sub_objective}/*_summary.xlsx`

**Example:**
```
Outcome              PBT           GKSRS         p-value
Local recurrence     15/100 (15%)  12/60 (20%)   0.41
```

**Interpretation:**
- 15% of PBT patients experienced recurrence
- 20% of GKSRS patients experienced recurrence
- Difference not statistically significant (p=0.41)

**Clinical Insight:** Crude rates suggest similar local control

---

## Understanding Regression Outputs

### Cox Regression Tables (Survival Outcomes)

**File:** `*_cox_coxph.html`

**Example Output:**

```
Characteristic          HR      95% CI          p-value
Treatment
  PBT                   —       —               
  GKSRS                 0.65    0.45, 0.95      0.025
Age (per year)          1.03    1.01, 1.05      0.008
Sex
  Male                  —       —
  Female                0.82    0.58, 1.15      0.25
```

**How to Read:**

**Hazard Ratio (HR):**
- HR = 1.0: No difference
- HR < 1.0: Lower hazard (protective)
- HR > 1.0: Higher hazard (harmful)

**GKSRS HR = 0.65:**
- GKSRS patients have 65% the hazard of PBT patients
- Equivalently: 35% reduction in hazard (1 - 0.65 = 0.35)
- **Clinical meaning:** GKSRS associated with lower risk of event

**Age HR = 1.03:**
- Each additional year of age increases hazard by 3%
- 10-year difference → 1.03^10 = 1.34 (34% higher hazard)

**95% CI = (0.45, 0.95):**
- We're 95% confident true HR is between 0.45 and 0.95
- Does not include 1.0 → statistically significant
- Width indicates precision (narrow = more precise)

**P-value = 0.025:**
- Probability of observing this difference if no true difference exists
- p < 0.05 → statistically significant
- Likelihood ratio test preferred over Wald test

**Reference Categories (—):**
- Other groups compared to this baseline
- PBT is reference for treatment
- Male is reference for sex

**Clinical Interpretation:**
After adjusting for age, sex, location, and optic nerve involvement, GKSRS treatment is associated with a 35% reduction in hazard compared to PBT (HR=0.65, 95% CI: 0.45-0.95, p=0.025).

### Logistic Regression Tables (Binary Outcomes)

**File:** `*_logistic_glm.html`

**Example Output:**

```
Characteristic          OR      95% CI          p-value
Treatment
  PBT                   —       —               
  GKSRS                 1.45    0.85, 2.48      0.17
Age (per year)          1.02    0.98, 1.06      0.35
```

**How to Read:**

**Odds Ratio (OR):**
- OR = 1.0: No difference in odds
- OR < 1.0: Lower odds
- OR > 1.0: Higher odds

**GKSRS OR = 1.45:**
- GKSRS patients have 1.45 times the odds of the outcome
- 45% higher odds compared to PBT
- **Clinical meaning:** GKSRS associated with higher odds of complication

**95% CI = (0.85, 2.48):**
- Includes 1.0 → not statistically significant
- Wide interval → imprecise estimate (small sample or few events)

**Clinical Interpretation:**
GKSRS treatment is associated with 45% higher odds of radiation retinopathy compared to PBT, but this difference is not statistically significant (OR=1.45, 95% CI: 0.85-2.48, p=0.17).

### Linear Regression Tables (Continuous Outcomes)

**File:** `*_linear_lm.html`

**Example Output:**

```
Characteristic          Beta    95% CI          p-value
Treatment
  PBT                   —       —               
  GKSRS                 -0.35   -0.68, -0.02    0.038
Baseline tumor height   -0.42   -0.55, -0.29    <0.001
```

**How to Read:**

**Regression Coefficient (Beta):**
- Units same as outcome variable
- Beta = 0: No difference
- Beta < 0: Decrease in outcome
- Beta > 0: Increase in outcome

**GKSRS Beta = -0.35 mm:**
- GKSRS patients have 0.35 mm greater decrease in tumor height
- **Clinical meaning:** GKSRS produces more tumor shrinkage

**Baseline tumor height Beta = -0.42:**
- Each 1 mm increase in baseline height → 0.42 mm greater decrease
- Larger tumors shrink more (regression to mean)

**Clinical Interpretation:**
GKSRS treatment results in 0.35 mm greater tumor height reduction compared to PBT (β=-0.35, 95% CI: -0.68 to -0.02, p=0.038), after adjusting for baseline tumor height and other confounders.

For Objective 2 vision there are two different linear outputs:
- `*_logmar_vision_change_adjusted_lm.html` models continuous logMAR change.
- `*_snellen_line_change_adjusted_lm.html` models the exact integer `Snellen Line Change` outcome.

The Snellen linear model is the direct adjusted analogue of the descriptive `Snellen Line Change` row, whereas the ordinal Snellen model uses the separate 7-level `Snellen Line Change Distribution`.

### Ordinal Regression Tables (Ordered Outcomes)

**File:** `*_ordinal_polr.html`

**Example Output:**

```
Characteristic          OR      95% CI          p-value
Treatment
  PBT                   —       —               
  GKSRS                 1.40    1.05, 1.88      0.022
Age (per year)          0.98    0.96, 1.00      0.071
```

**How to Read:**

**Ordinal Odds Ratio (OR):**
- OR = 1.0: No difference in distribution across ordered outcome levels
- OR > 1.0: Higher odds of being in an earlier ordered category
- OR < 1.0: Higher odds of being in a later ordered category

**For Snellen Line Change Distribution in Objective 2a:**
- Categories are ordered from `≥3-line improvement` to `≥3-line loss`
- The stable category is `Stable (0-line change)` after nearest-line rounding with halves away from zero
- OR > 1.0 therefore favors better vision outcomes
- OR < 1.0 therefore favors worse vision outcomes

**GKSRS OR = 1.40:**
- GKSRS patients have 40% higher odds of falling into a better Snellen line-change category than PBT

**Effect Summary Workbooks**

- `*_effect_summary.xlsx` files are flat one-sheet workbooks that combine descriptive, unadjusted, and adjusted rows for the corresponding analysis folder.
- In `a_vision_changes`, use `analysis_label` to distinguish `LogMAR Vision Change`, `Snellen Line Change`, and `Snellen Line Change Distribution`.
- `model_status = "DESCRIPTIVE"` marks summary rows, `model_status = "FIT"` marks modeled rows, and `model_status = "SKIPPED"` documents analyses that could not be fit.
- Workbook methods are model-family specific and match the reader-facing HTML tables: linear rows use mean differences with Wald CIs/p-values, logistic rows use ORs with model-based Wald CIs and the pipeline's standard term-level p-values, Cox rows use HRs with native Cox CIs/p-values, and ordinal rows use proportional-odds ORs with 95% Wald CIs plus likelihood-ratio-test p-values.
- **Clinical meaning:** the outcome distribution is shifted toward more improvement or less loss

**95% CI = (1.05, 1.88):**
- Excludes 1.0 → statistically significant
- Entire interval above 1.0 → consistent direction toward better ordered outcomes

**Clinical Interpretation:**
After adjustment, GKSRS is associated with higher odds of a better Snellen line-change category than PBT (OR=1.40, 95% CI: 1.05-1.88, p=0.022).

---

## Interpreting Survival Curves

### Kaplan-Meier Plots

**File:** `*_km.png`

**Key Components:**

**1. Survival Curves (Lines)**
- **Y-axis:** Probability of survival (0-100%)
- **X-axis:** Time since treatment (years)
- **Separate lines:** One per treatment group
- **Curve steps down:** Each time an event occurs

**2. Confidence Bands (Shaded Regions)**
- Show uncertainty around survival estimates
- Wider bands = more uncertainty (fewer patients at risk)
- Narrow bands = more precise (more patients)

**3. Numbers at Risk (Below X-axis)**
```
Time:     0      1      2      3      4      5
PBT:    100     95     88     80     72     65
GKSRS:   60     58     54     50     45     40
```
- Shows patients still under observation at each time
- Decreases due to events and censoring
- Smaller numbers → less reliable estimates

**4. Median Survival (Dashed Lines)**
- Time when 50% of patients have experienced event
- Where survival curve crosses 50% line
- "Not reached" if curve stays above 50%

**How to Read:**

**Example:** 5-year overall survival
- PBT curve at year 5: 75%
- GKSRS curve at year 5: 82%
- Interpretation: 75% of PBT patients survived 5 years; 82% of GKSRS patients survived

**Comparing Curves:**
- **Curves separate early:** Treatment effects emerge quickly
- **Curves cross:** Treatment effects change over time (PH violation)
- **Curves parallel:** Consistent treatment effect (PH holds)
- **Curves overlap:** No meaningful difference

**Log-Rank P-value:**
- Tests whether survival distributions differ overall
- p < 0.05: Statistically significant difference
- Doesn't specify direction or magnitude

**Clinical Interpretation:**
The 5-year overall survival was 75% for PBT and 82% for GKSRS (log-rank p=0.12). While GKSRS shows numerically higher survival, the difference is not statistically significant. Larger sample sizes or longer follow-up may be needed.

### Cumulative Incidence Plots (Competing Risks)

**Similar to K-M plots but:**
- Account for competing events (e.g., death before recurrence)
- More accurate event probabilities
- Gray's test instead of log-rank test

**Current Objective 4 display note:** the reader-facing MSS CIF PNG is intentionally simplified to `Class 1` versus `Class 2` using `gep_class_simple`, and excludes `GEP Not Tested` / `GEP Failed/Indeterminate` from the plotted strata. The companion technical competing-risk tables and models still use the more granular `biopsy1_gep` grouping, so the plot is a cleaner display layer rather than a redefinition of the technical MSS analysis.

---

## Understanding RMST Analysis

### What is RMST?

**Restricted Mean Survival Time:** Average time survived up to a specified time point (τ)

**Why Use RMST?**
- Doesn't require proportional hazards assumption
- Clinically interpretable ("months gained/lost")
- Robust alternative when PH violated

### RMST Difference Plots

**File:** `*_rmst_pvalue_progression.png`

**Key Components:**

**1. Survival Curves with Shaded Areas**
- **Shaded regions:** Area under survival curve = RMST
- **Larger area:** Longer average survival
- **Compare shading:** Visual difference in survival time

**2. RMST Estimates (Text)**
```
PBT RMST:    4.2 years (95% CI: 3.9-4.5)
GKSRS RMST:  4.6 years (95% CI: 4.2-4.9)
Difference:  0.4 years (95% CI: 0.1-0.7)
P-value:     0.015
```

**How to Interpret:**

**RMST Values:**
- PBT patients survived an average of 4.2 years up to 5-year follow-up
- GKSRS patients survived an average of 4.6 years
- GKSRS patients lived 0.4 years (4.8 months) longer on average

**Statistical Significance:**
- p = 0.015: Difference is statistically significant
- 95% CI (0.1-0.7): Difference likely between 1.2 and 8.4 months

**Clinical Interpretation:**
Within the first 5 years, GKSRS patients survived an average of 4.6 years compared to 4.2 years for PBT patients, representing a clinically meaningful gain of 4.8 months (RMST difference=0.4 years, 95% CI: 0.1-0.7, p=0.015).

### Comparing RMST to Cox HR

| Scenario | RMST Interpretation | Cox HR Interpretation |
|----------|---------------------|----------------------|
| **RMST diff = +0.5 years, HR = 0.70** | GKSRS patients gained 6 months on average | GKSRS reduced hazard by 30% |
| **RMST diff = 0 years, HR = 1.00** | No survival difference | No hazard difference |
| **PH violated** | RMST still valid | Cox HR may be misleading |

**When Results Disagree:**
- RMST significant, Cox not: Effect real but varying over time
- Cox significant, RMST not: Effect consistent but small in absolute terms
- Trust RMST when PH assumption violated

---

## Reading Forest Plots

### Structure

**File:** `*_subgroup_forest_plot.png`

**Layout:**
```
Subgroup          n    Events   HR [95% CI]      Forest Plot    P-interact
Overall          160     45     0.65 [0.45-0.95]     ♦             —
Age
  <60 years       70     18     0.55 [0.30-1.02]     ●——●
  ≥60 years       90     27     0.72 [0.45-1.15]      ●——●          0.43
Sex
  Male            88     25     0.60 [0.35-1.03]    ●——●
  Female          72     20     0.75 [0.42-1.35]      ●——●          0.58
```

**Key Components:**

**1. Subgroup Names (Left)**
- Patient subgroups being compared
- Hierarchical structure (category → levels)

**2. Sample Sizes**
- n = total patients in subgroup
- Events = number who experienced outcome
- Higher numbers = more reliable estimates

**3. Effect Estimates (Center)**
- HR [95% CI] or OR [95% CI]
- Numerical value of treatment effect per subgroup

**4. Forest Plot (Right)**
- **Point estimate (●):** HR or OR value
- **Horizontal lines:** 95% confidence interval
- **Vertical reference line:** HR/OR = 1.0 (no effect)
- **Left of line:** Protective effect (HR/OR < 1)
- **Right of line:** Harmful effect (HR/OR > 1)

**5. P-for-Interaction**
- Tests whether treatment effect differs across subgroup levels
- p < 0.05: Significant heterogeneity (effect modification)
- p ≥ 0.05: Consistent effect across subgroups

**6. Overall Effect (Diamond ♦)**
- Pooled estimate across all patients
- Width = 95% CI
- Reference for comparing subgroups

**How to Read:**

**Age Subgroup Example:**
- **<60 years:** HR=0.55 (45% risk reduction)
- **≥60 years:** HR=0.72 (28% risk reduction)
- **P-interact=0.43:** Difference not statistically significant
- **Interpretation:** Treatment appears more effective in younger patients, but evidence is weak (could be chance)

**Confidence Intervals:**
- **Narrow (●—●):** Precise estimate (adequate sample size)
- **Wide (●————●):** Imprecise (small sample or few events)
- **Crosses 1.0:** Not statistically significant
- **Doesn't cross 1.0:** Statistically significant

**Clinical Interpretation:**
Overall, GKSRS reduces the hazard by 35% (HR=0.65, 95% CI: 0.45-0.95). Treatment effects are consistent across age groups (p-interaction=0.43) and sex (p-interaction=0.58), though point estimates suggest slightly stronger effects in younger patients and males. These subgroup differences are not statistically significant and likely reflect random variation.

---

## Interpreting Proportional Hazards Diagnostics

### What is the PH Assumption?

**Assumption:** Hazard ratio remains constant over time

**Example:**
- PH holds: GKSRS always has 70% the hazard of PBT (HR=0.70 at all times)
- PH violated: GKSRS initially has 50% the hazard, but later 90% the hazard

### Diagnostic Files

**File:** `*_ph_diagnostics.xlsx`

**Example:**

```
Variable            Chi-sq    df    p-value
Treatment            4.52     1      0.034
Age                  0.85     1      0.36
Sex                  1.23     1      0.27
Global test          6.89     3      0.075
```

**How to Interpret:**

**Individual P-values:**
- **p > 0.05:** PH assumption holds for that variable
- **p ≤ 0.05:** PH assumption violated for that variable

**Treatment p = 0.034 (VIOLATED):**
- Treatment effect changes over time
- Cox HR is an average (may not reflect reality at any specific time)
- Use RMST as primary analysis

**Age p = 0.36 (HOLDS):**
- Age effect constant over time
- Cox model appropriate for age

**Global test p = 0.075:**
- Overall model PH assumption borderline
- Some variables violate assumption
- Proceed with caution; consider RMST

### Schoenfeld Residual Plots

**File:** `*_ph_plots_treatment.png`

**Components:**
- **X-axis:** Time
- **Y-axis:** Scaled Schoenfeld residuals
- **Points:** Residual at each event time
- **Smooth line:** Trend in residuals over time
- **Confidence band:** Uncertainty around trend

**How to Interpret:**

**Flat trend (horizontal line):**
- PH assumption holds
- Hazard ratio constant over time
- Cox regression appropriate

**Non-flat trend (sloped or curved line):**
- PH assumption violated
- Hazard ratio changes over time
- Use RMST or time-varying models

**Example Patterns:**

**Downward slope:**
- Treatment effect decreases over time
- Initially strong protection, diminishes later

**Upward slope:**
- Treatment effect increases over time
- Initially weak, strengthens later

**U-shape or inverted U:**
- Complex time-varying effects
- Consider stratified or parametric models

**Clinical Implications:**

**PH Holds:**
- Report Cox HR as primary measure
- RMST as secondary confirmation
- Standard interpretation

**PH Violated:**
- Report RMST as primary measure
- Cox HR as secondary (note violation)
- Describe time-varying pattern
- Consider time-stratified analysis

---

## Understanding GEP Analysis

Gene Expression Profiling (GEP) is an external molecular risk assay for metastatic spread. Objective 4 does not recompute the Castle-type 15-gene signature inside this pipeline. Instead, it takes the lab-reported patient-level 5-year GEP survival predictions already present in the analytic dataset, uses those values directly for the 5-year `expected_*` columns, derives the 7-year and 10-year values from the same 5-year probabilities using $S(7) = S(5)^{7/5}$ and $S(10) = S(5)^{10/5}$, and then converts those horizon-specific survival values into risk quantities needed for validation. Those 7-year and 10-year values are assumption-checked extrapolations rather than direct imported assay outputs. The downstream methods then ask different questions: Kaplan-Meier or competing-risk summaries estimate observed outcome risk from follow-up data, calibration tools assess agreement between supplied GEP risk and observed risk, discrimination tools test rank-ordering, PRAME-based analyses evaluate whether reclassification improves clinical utility, and the extrapolation-assumption check asks whether the constant-hazard extension beyond 5 years is reasonably compatible with the observed data.

The main Objective 4 denominator is deliberately stricter than “any row with a GEP-related label.” Only tumors with definitive raw DecisionDx Class 1 or Class 2 calls plus valid endpoint-specific imported probabilities enter the MFS and MSS eligible subsets. Rows representing `GEP Failed/Indeterminate`, `GEP Not Tested`, `Other`, PRAME-not-reported, unknown, or discordant raw labels are excluded from the main analytic denominators even though reader-facing display restoration may still show canonical labels elsewhere in the workbook ecosystem.

### Where to Find the Files

- Objective 4 outputs live under `04_GEP_Validation/` inside each cohort directory.
- Start with the outcome-specific consolidated workbook named `<prefix>MFS_consolidated_summary.xlsx` or `<prefix>MSS_consolidated_summary.xlsx`.
- Technical-detail workbooks `<prefix>mfs_validation_technical_details.xlsx` and `<prefix>mss_validation_technical_details.xlsx` sit in the matching outcome folders. They are now companion detail files rather than alternate summary workbooks.
- Narrative text summaries `<prefix>mfs_validation_narrative_summary.txt` and `<prefix>mss_validation_narrative_summary.txt` sit alongside those technical workbooks.
- Full calibration curve figures `<prefix>mfs_calibration_full.png` and `<prefix>mss_calibration_full.png` sit in the outcome validation folders and summarize calibration across the risk spectrum. Each dot is one predicted-risk quantile bin (x = mean predicted risk in bin; y = KM observed risk at the horizon), and the smooth line is an IPCW-weighted spline recalibration curve when feasible.
- Unified cross-outcome summaries live at the root of `04_GEP_Validation/` as `<prefix>unified_gep_validation_summary.xlsx`. This workbook is comparison-only rather than a second outcome-summary workbook.
- For the full cohort, that unified workbook now also includes compact no-GEP tabs: `No_GEP_Overview`, `No_GEP_Model_Comparison`, and `No_GEP_Risk_Strata`.
- Simple actual-vs-expected QC outputs live in `04_GEP_Validation/unified_summary/` as `<prefix>simple_gep_validation.xlsx`.
- For MFS, treat the observed 5-year value in that QC workbook as Kaplan-Meier MFS at 60 months. It is censoring-aware and should match the 5-year KM summary, not the raw count of patients with `mfs_event_5yr == 0`.
- If you need the full explanation for why this changed and how to interpret old slide numbers, see [OBJECTIVE4_MFS_5YR_DECISION_NOTE.md](OBJECTIVE4_MFS_5YR_DECISION_NOTE.md).

### Workbook Layout at a Glance

| Sheet | Purpose | Where to find detailed metric definitions |
|-------|---------|-------------------------------------------|
| `Observed_Expected_Summary` | Compact overall observed-vs-expected summary by timepoint. | `docs/STATISTICAL_METHODS.md#gep-validation-metrics` |
| `Calibration_Summary` | Predicted vs observed agreement. | `docs/STATISTICAL_METHODS.md#gep-validation-metrics` (Calibration subsection) |
| `Discrimination_Summary` | Rank-order performance across timepoints. | `STATISTICAL_METHODS.md` (Discrimination subsection) |
| `Decision_Curve_Summary` | Net benefit across threshold probabilities. | `STATISTICAL_METHODS.md` (Clinical Utility subsection) |
| `Extrapolation_Assumption_Checks` | Focused support check for the 7-year and 10-year exponential extrapolation. | `STATISTICAL_METHODS.md` (GEP Validation Metrics subsection) |
| `PRAME_Summary` | GEP-only vs GEP-plus-PRAME incremental discrimination comparison; sparse cohorts may show an explanatory placeholder row instead of full results. | Same Clinical Utility subsection |
| `Missing_Data_Summary` | QC signals that contextualize Objective 4. | `STATISTICAL_METHODS.md` (Missing Data diagnostics) |

### Workbook Naming & Refresh Cadence

| Item | Details |
| --- | --- |
| Primary workbook pattern | `<prefix>MFS_consolidated_summary.xlsx` or `<prefix>MSS_consolidated_summary.xlsx`. |
| Technical workbooks | `<prefix>mfs_validation_technical_details.xlsx` and `<prefix>mss_validation_technical_details.xlsx` in the outcome folders; these keep lower-level detail and no longer repeat the consolidated summary calibration/discrimination tables. |
| Narrative summaries | `<prefix>mfs_validation_narrative_summary.txt` and `<prefix>mss_validation_narrative_summary.txt`. |
| Calibration curve PNGs | `<prefix>mfs_calibration_full.png` and `<prefix>mss_calibration_full.png` (one per outcome; faceted by timepoint). |
| Unified workbook | `<prefix>unified_gep_validation_summary.xlsx` at the root of `04_GEP_Validation/`; this workbook uses comparison-only sheet names such as `Calibration_Comparison`, `Discrimination_Comparison`, `PRAME_Comparison`, and `Missing_Data_Comparison`. The full cohort additionally appends `No_GEP_Overview`, `No_GEP_Model_Comparison`, and `No_GEP_Risk_Strata`. |
| Simple QC workbook | `unified_summary/<prefix>simple_gep_validation.xlsx`. For MFS, the observed 5-year value is KM at 60 months. |
| Default directory | `~/ProjectsRuntime/uveal_melanoma/Analysis/<cohort>/04_GEP_Validation/` with `a_metastasis_free_survival/`, `b_melanoma_specific_survival/`, and `unified_summary/`. |
| Outcomes covered | MFS and MSS; unified workbooks stack both. Full-cohort unified workbooks may also append no-GEP comparison sheets. |
| Timepoints | Driven by `GEP_VALIDATION_TIMEPOINTS` (defaults: 5, 7, 10 years). Every sheet carries one row per timepoint requested. |
| How to regenerate | Run Objective 4 via `run_specific_objective("uveal_melanoma_<cohort>", 4)` or the full pipeline. New runs overwrite existing workbooks after passing QC. |

### How to Read the Exploratory No-GEP Workbook

The exploratory no-GEP workbook lives in `04_GEP_Validation/d_exploratory_no_gep/` and is separate from the main Objective 4 validation workbooks. It is designed for patients with `GEP Failed/Indeterminate` or `GEP Not Tested`, where the question is not “did the imported GEP prediction validate?” but rather “what baseline risk estimate is still supportable without a usable GEP result?” The full-cohort unified Objective 4 workbook now mirrors the highest-yield no-GEP summaries in compact comparison tabs, while this appendix workbook retains the row-level detail.

If you only need the highest-yield no-GEP summary inside the root Objective 4 workbook, use this order:

1. `No_GEP_Overview`
  - Use this first for counts, observed outcomes, median no-GEP predicted risks, and the strongest baseline-separation note.
2. `No_GEP_Model_Comparison`
  - Use this next to compare the surrogate, full direct 5-year MFS/MSS models, and parsimonious direct sensitivity models side by side.
  - The `Use_Case` column distinguishes the descriptive surrogate from the clinically preferred direct-risk outputs.
3. `No_GEP_Risk_Ladder`
  - Use this before the pooled-bin sheet when you need to place `GEP Not Tested` and `GEP Failed/Indeterminate` relative to definitive `Class 1` and `Class 2`.
  - This is the safest place to support an “overall between Class 1 and Class 2, but internally heterogeneous” interpretation.
4. `No_GEP_Risk_Strata`
  - Use this to confirm whether low/intermediate/high predicted bins actually show increasing observed 5-year MFS or MSS event rates.

Then open the appendix workbook only if you need the full baseline table, patient-level predictions, or the full predictor-contribution sheets.

Use this reading order:

1. Start with `Summary_and_Guide`.
  - This tab says what was computed, what the main findings were, and how cautious the interpretation should be.
  - If the summary says the surrogate model is only modest, do not treat the surrogate probability as a recovered Class 1 or Class 2 call.
  - Read the surrogate output as: among patients with known Class 1 and Class 2 results, which known clinical pattern does this no-GEP patient look more like?
  - The surrogate is a binary Class 2-vs-Class 1 model, so the stored probability is specifically `P(Class 2-like | baseline features)`.
  - By complement, `1 - surrogate_class2_probability` is the implied `Class 1-like` probability within that surrogate only.
  - Neither number is the probability of the patient's true molecular assay result.
2. Read `Predictor_Contribution`.
  - This tab shows which retained baseline predictors are doing the most work in the exploratory models.
  - Larger absolute ridge coefficients mean stronger contribution to the model's risk ordering, not stronger statistical proof.
3. Check `Risk_Ladder_5yr`.
  - This sheet compares definitive `Class 1`, `GEP Not Tested`, `GEP Failed/Indeterminate`, and definitive `Class 2` on the same 5-year descriptive scale.
  - Use it to support statements about whether the no-GEP groups sit between the definitive GEP groups overall.
  - Do not collapse the two no-GEP groups into one interpretive bucket if this sheet shows they are meaningfully separated.
4. Check `Surrogate_Class2_Model`, `Direct_MFS_Risk_Model`, and `Direct_MSS_Risk_Model`.
  - `CV AUC` tells you whether the model can rank higher-risk versus lower-risk patients better than chance.
  - `CV_AUC_CI_Lower` and `CV_AUC_CI_Upper` show how much the internal-validation signal moves when the fold assignment changes.
  - `CV Brier` gives a compact overall prediction-error summary.
  - Calibration rows tell you whether risks are systematically too high or too low, when the data are rich enough to estimate that reliably.
5. Read `Parsimonious_Sensitivity`.
  - This compares the full direct models against a smaller pre-specified baseline predictor set.
  - If the parsimonious models preserve similar ranking and subgroup ordering, that strengthens the claim without requiring more covariates.
6. Read `No_GEP_Predictions`.
  - This is the patient-level output for the two no-GEP groups.
  - `surrogate_class2_probability` is descriptive only.
  - It is a clinical resemblance score showing how much the patient's baseline profile resembles the observed Class 2 pattern versus the observed Class 1 pattern in the definitive-GEP reference set.
  - If you mentally convert it to the opposite pole, `1 - surrogate_class2_probability` means `Class 1-like` resemblance only.
  - Do not write or say that either of these values is a true molecular `Class 1` or `Class 2` probability.
  - `predicted_mfs_5yr_risk` and `predicted_mss_5yr_risk` are the main clinically usable outputs.
7. Read `Sensitivity_Pooled_No_GEP`.
  - This checks whether pooled low/intermediate/high predicted-risk bins show increasing observed event rates.
  - If observed event rates increase across bins, the model is at least ordering patients in a clinically meaningful direction.

How to interpret the exploratory plots:

- Corrected KM/CIF plots:
  - These show where `GEP Failed/Indeterminate` and `GEP Not Tested` sit relative to definitive `Class 1` and `Class 2`.
  - Curves between definitive `Class 1` and `Class 2` support an overall between-group interpretation, but inspect `GEP Failed/Indeterminate` and `GEP Not Tested` separately before calling the whole no-GEP population “intermediate risk.”
- Density plots:
  - A right-shifted density means the subgroup is receiving higher predicted probability overall.
  - If `GEP Failed/Indeterminate` is shifted to the right of `GEP Not Tested`, it suggests the failed group is clinically higher risk on the retained baseline features.
- Event-rate-by-bin plots:
  - These are a practical check on risk ordering.
  - Rising observed event rates from low to high bins support useful stratification.

Plain-English bottom line:

- The exploratory workbook is best used to support direct baseline risk estimation when GEP is unavailable.
- The surrogate score is best used to say which definitive-GEP clinical pattern a no-GEP patient looks more like, not to say what their true molecular class was.
- If you want the opposite resemblance score, use `1 - surrogate_class2_probability`, but call it `Class 1-like` probability rather than true `Class 1` probability.
- It should not be used to claim true molecular reclassification.
- Group-level separation is usually more reliable than any single patient-level probability.

### Shared Conventions

- **`N`** counts patients who contribute data at that horizon after censoring, predictor filtering, and the definitive-label eligibility rule. For Objective 4, this means only analyzable raw Class 1 / Class 2 DecisionDx calls with valid endpoint-specific imported GEP probabilities enter the main MFS and MSS validation subsets.
- **`Events` / `Non_Events`** always tie to the outcome being modeled (metastasis for MFS, melanoma-specific death for MSS). Non-events are censored survivors.
- **`Timepoint`** corresponds to the year label in `GEP_VALIDATION_TIMEPOINTS`; interpretation tips should explicitly quote it.
- **Fallback + method columns** (`*_Fallback_Used`, `*_Method`) surface whenever the preferred estimator fails; cite them whenever they read `TRUE` to explain unexpected `NA`s.
- **Missing = `NA`** means "metric skipped" rather than "zero." Cross-check the run log (`logs/json/*.jsonl`) for warnings before imputing values.
- **Displayed GEP labels are canonical labels, not sparse-model buckets.** When a cohort has a matching `*_derived_precollapse.rds` artifact, reader-facing Objective 4 outputs restore `biopsy1_gep`, `gep_class_simple`, `prame_status`, and `gep12_prame_status` from that artifact. A literal `Other` label should therefore be interpreted as a bug or an intentionally non-GEP output, not as a valid biological class.
- **The simple QC workbook now follows the same refreshed eligibility rule as the main Objective 4 analysis.** If a row does not have a definitive raw Class 1 / Class 2 DecisionDx label, it should not appear in the simple Class 1 vs Class 2 summary counts.
- **The MSS CIF PNG is more collapsed than the technical MSS workbook.** The figure is shown at the `gep_class_simple` level (`Class 1` vs `Class 2`) for readability, while the technical competing-risk tables may still report the more granular `biopsy1_gep` group structure.

### GEP Quick Read

If you are not statistically inclined, use this order:

1. Check `N` and `Events` first. Very small numbers make every later metric less trustworthy.
2. Check calibration next. Ask: "Were the predicted risks roughly the right size?"
3. Check discrimination after that. Ask: "Did the model rank higher-risk patients above lower-risk patients?"
4. Check the decision-curve sheet last. Ask: "Would using this model actually help a clinical decision?"
5. Read the PRAME sheet only after the base GEP prediction looks at least reasonably calibrated and discriminative.

Fast triage rule:
- `OE_Ratio` near 1, lower `ICI`, and `Slope` near 1 suggest the risk estimates are in the right ballpark.
- Lower `Brier_Score` suggests better overall prediction accuracy at that horizon because the predicted risks sit closer to what actually happened.
- Higher `Harrell_C` and `Integrated_AUC` suggest better patient ranking.
- Positive `Optimal_Net_Benefit` over a sensible threshold range suggests the model could be clinically useful.
- Many `NA` values or `*_Fallback_Used = TRUE` mean the result is more fragile and should be described cautiously.

### GEP Calibration Made Simple

Calibration asks a simple question: **did the supplied GEP prediction get the amount of risk about right?**

Use this reading order:

1. `OE_Ratio`
  - Near 1: overall predicted risk and overall observed risk are similar.
  - Above 1: the supplied GEP prediction may be underpredicting events.
  - Below 1: the supplied GEP prediction may be overpredicting events.
2. `ICI`
  - Lower is better.
  - Think of this as the average prediction error.
3. `Slope`
  - Near 1: the spread of predictions is about right.
  - Below 1: predictions are too extreme.
  - Above 1: predictions are too compressed.
4. `Nam_D_Agostino_p`
  - Small p-value: more evidence that predicted and observed risks are mismatched across risk groups.
  - Larger p-value: no strong evidence of mismatch, but not proof of perfect calibration.

Plain-English example:
- `OE_Ratio = 0.98`, `ICI = 0.04`, `Slope = 0.95`: the supplied GEP prediction is probably estimating risk reasonably well.
- `OE_Ratio = 0.60`, `ICI = 0.14`, `Slope = 0.55`: the supplied GEP prediction is probably miscalibrated and making risks too extreme.

What to say in a write-up:
- "Calibration looked acceptable: overall predicted risk was close to observed risk, average calibration error was small, and the slope was near 1."
- "Calibration looked weak: observed risk did not line up well with predicted risk, and the slope suggested the model was over-separating patients."

### GEP Discrimination Made Simple

Discrimination asks a different question: **did the model put sicker patients above healthier patients?**

Start with `Harrell_C`:
- Higher is better.
- Around 0.5 means the ranking is close to random.
- Around 0.7 means the ranking is often useful.
- Much higher than that suggests stronger separation.

Then use the supporting fields:
- `Integrated_AUC`: average ranking performance over follow-up.
- `Cumulative_Discrimination`: average ranking performance across the prespecified 5-, 7-, and 10-year windows.
- `Time_averaged_Discrimination`: average ranking performance across monthly follow-up landmarks.
- `IPA`: whether the model improves on a very simple benchmark.

Important caution:
- MFS `Harrell_C` and MSS `Harrell_C` are not identical estimands in this pipeline.
- MFS uses horizon-truncated follow-up.
- MSS uses full observed follow-up in the horizon-specific analysis set.
- Compare MFS-to-MFS and MSS-to-MSS more confidently than MFS-to-MSS.

Plain-English example:
- `Harrell_C = 0.72`: the model usually ranks patients who fail earlier above patients who remain event-free longer.
- `Harrell_C = 0.54`: the model is only a little better than chance at ranking patients.

What to say in a write-up:
- "Discrimination was reasonable: higher-risk patients generally experienced the endpoint sooner than lower-risk patients."
- "Discrimination was weak: the model did not separate higher-risk from lower-risk patients very well."

### GEP Decision Curve Made Simple

Decision-curve analysis asks: **if we used this model in practice, would it help decision-making?**

Read it this way:

1. `Optimal_Net_Benefit`
  - Positive is better.
  - Negative means the model is not helping at that threshold.
2. `Threshold_Range_Min` / `Threshold_Range_Max`
  - These show the probability range where the model is giving positive net benefit.
  - If that range lines up with a clinically realistic action threshold, the result is more useful.
3. `Area_Between_Curves`
  - Bigger positive values suggest a larger overall gain across the evaluated threshold range.

Plain-English example:
- Positive net benefit from 10% to 30% means the model may help if your clinical action threshold lives in that range.
- No positive net benefit means the model may not improve decisions beyond simpler strategies.

What to say in a write-up:
- "The decision-curve analysis suggested clinical usefulness across the 10% to 30% threshold range."
- "The decision-curve analysis did not show a clear net-benefit advantage over simple default strategies."

### Sheet Dictionary (Quick Reference)

Metric formulas remain in `STATISTICAL_METHODS.md`; the notes below clarify what each column represents inside the workbook.

#### `Observed_Expected_Summary`

- `Timepoint`, `N`: context columns described above.
- `Observed`, `Expected`, `OE_Ratio`, `CI_Lower`, `CI_Upper`: overall observed-versus-expected counts and ratio for calibration-in-the-large. For MFS, `Observed` is a KM-derived pseudo-count on the cohort denominator scale and the interval is Greenwood-derived rather than Poisson.
- `OE_Chi_Square_p`: p-value for the overall O/E goodness-of-fit comparison across the class-level observed and expected event counts. This is not the grouped Greenwood Nam-D'Agostino calibration p-value.

Important distinction:
- `OE_Chi_Square_p` belongs to `Observed_Expected_Summary` and summarizes the overall O/E count mismatch.
- `Nam_D_Agostino_p` belongs to `Calibration_Summary` and summarizes grouped survival calibration using Kaplan-Meier observed risk with Greenwood variance.

#### `Calibration_Summary`

- `Timepoint`, `N`: context columns described above.
- `Prediction_Source`: `Imported` at 5 years and `Extrapolated from imported 5-year value` at 7 and 10 years.
- `Extrapolation_Assumption`: `Not Applicable` at 5 years and `Exponential constant hazard` at 7 and 10 years.
- `Assumption_Support_Status`: interpretive support tier for the later-horizon extrapolation. `Supported` means the data were reasonably compatible with constant hazard, `Weakly Supported` means the diagnostics were mixed or limited, and `Unsupported` means the data either argued against constant hazard or were too sparse to assess it well.
- `Assumption_Support_Notes`: plain-language reason for the support tier. Treat `Unsupported` later-horizon rows as exploratory.
- `Nam_D_Agostino_p`: p-value from the grouped Greenwood Nam-D'Agostino survival-calibration test. Smaller values mean stronger evidence that predicted and observed risk do not agree well across the risk groups.
- `Nam_D_Agostino_Method`: method label for the grouped goodness-of-fit field; currently this should read `greenwood_nam_dagostino`.
- `ICI`: Integrated Calibration Index — lower is better.
- `ICI_Method`: tells you whether the ICI came from the preferred IPCW-smoothed recalibration curve or from the grouped Kaplan-Meier fallback. Cite this column whenever comparing cohorts or horizons.
- `Slope`: horizon-specific IPCW-weighted logistic recalibration slope (ideal = 1.0).
- `Slope_Method`: method label for the slope field; currently this should read `ipcw_logit` when the fit is supportable. `ipcw_logit_unavailable` means the weighted slope fit was too sparse or too numerically unstable to report responsibly.
- `Brier_Score`: Overall prediction-accuracy check at that horizon. Lower is better.
- `Brier_Method` + `Brier_Fallback_Used`: tell you whether the score came from the preferred calculation or from a fallback path. If a fallback was used, describe the result more cautiously and see `STATISTICAL_METHODS.md` for the technical details.

##### How to interpret the main calibration fields

The grouped Greenwood Nam-D'Agostino statistic is the workbook's formal grouped calibration test. The full equation is documented in [docs/STATISTICAL_METHODS.md](docs/STATISTICAL_METHODS.md). In plain language, the test asks whether the model's predicted event counts and the observed event counts line up across low-, medium-, and high-risk groups once censoring is taken seriously.

How to read the chi-square result:
- `Nam_D_Agostino_p < 0.05`: evidence of miscalibration. The observed risks differ from the predicted risks more than would usually be expected by chance alone.
- `Nam_D_Agostino_p >= 0.05`: no strong evidence of miscalibration. This does not prove the model is perfectly calibrated; it only means the grouped test did not detect a clear mismatch.
- Larger cohorts can detect smaller departures from calibration, so the p-value should always be read alongside `ICI`, `Slope`, and `N` rather than in isolation.

ICI can be thought of as the average absolute calibration error. The exact method-specific formulas are documented in [docs/STATISTICAL_METHODS.md](docs/STATISTICAL_METHODS.md). In plain language, lower ICI means the average prediction is closer to what actually happened.

The calibration slope comes from the horizon-specific recalibration model, with the formal equation documented in [docs/STATISTICAL_METHODS.md](docs/STATISTICAL_METHODS.md).

How to read the slope:
- `Slope` near 1: prediction spread is about right.
- `Slope < 1`: predictions are too extreme, so low risks are too low and high risks are too high.
- `Slope > 1`: predictions are too compressed, so the model is not separating low and high risk strongly enough.
- `Slope = NA` with `Slope_Method = ipcw_logit_unavailable`: the fit was too sparse or unstable to trust, so the workbook intentionally withholds the number.

The Brier score is the workbook's overall accuracy check at that horizon. Lower values mean the predicted risks were closer to what actually happened. There is no single universal cutoff that counts as "good" in every cohort, so this field is most useful when comparing the same outcome and timepoint across cohorts or models.

How to read the Brier fields:
- `Brier_Score` near 0: predictions were very close to the observed horizon outcomes.
- Larger `Brier_Score`: worse overall probabilistic accuracy.
- `Brier_Method`: check whether the score came from the preferred calculation or a fallback path.
- `Brier_Fallback_Used = TRUE`: mention in the write-up that the score came from a fallback path rather than the preferred calculation.

Practical rule: use the grouped chi-square p-value to ask, "Is there evidence of group-level miscalibration?" and use `ICI` plus `Slope` to ask, "How large is the mismatch, and in what direction?"

If you want the shortest possible version, read [GEP Calibration Made Simple](#gep-calibration-made-simple) first.

#### `Discrimination_Summary`

- `Prediction_Source`, `Extrapolation_Assumption`, `Assumption_Support_Status`, `Assumption_Support_Notes`: read these exactly as in `Calibration_Summary`. They are there so you do not accidentally treat 7-year and 10-year concordance results as if they were based on independently imported assay predictions.
- `Events`: Number of outcome events accumulated by that year.
- `Harrell_C`: Primary concordance field. For MFS, this is a horizon-specific concordance estimate after truncating follow-up at the sheet's timepoint. For MSS, this is computed from the full observed follow-up within the horizon-specific analytic subset rather than the same horizon-truncated estimand used for MFS.
- `Integrated_AUC`: Mean `riskRegression::Score()` AUC over monthly follow-up intervals rather than a single landmark AUC.
- `Cumulative_Discrimination`: Mean truncated concordance across the prespecified 5-, 7-, and 10-year windows that had enough events to evaluate.
- `Time_averaged_Discrimination`: Mean truncated concordance across monthly follow-up landmarks.
- `IPA`, `IPA_Method`, `IPA_Fallback_Used`: Index of Prediction Accuracy at that horizon. The preferred estimator is the Brier-score comparison against the null event-rate benchmark; the method columns tell you when the pipeline had to fall back to the AUC-based or simplified estimator.

See [STATISTICAL_METHODS.md](STATISTICAL_METHODS.md#gep-validation-metrics) for the exact formulas and implementation details.

If you want the shortest possible version, read [GEP Discrimination Made Simple](#gep-discrimination-made-simple) first.

#### `Decision_Curve_Summary`

- `Prediction_Source`, `Extrapolation_Assumption`, `Assumption_Support_Status`, `Assumption_Support_Notes`: use these to qualify 7-year and 10-year clinical-utility interpretations. If support is weak or unsupported, present the decision-curve result cautiously.
- `Event_Rate`: Observed event rate at the horizon (use it to sanity-check IPA swings).
- `Optimal_Threshold`: Probability threshold delivering maximum net benefit; mirrors `Net_Benefit_Threshold` when present.
- `Optimal_Net_Benefit`: Net benefit at the optimal threshold relative to treat-all/none.
- `Threshold_Range_Min` / `Threshold_Range_Max`: Boundaries for clinically reasonable thresholds configured when building the decision curves.
- `Area_Between_Curves`: Integral of the net-benefit gain vs treat-none across the range.

If you want the shortest possible version, read [GEP Decision Curve Made Simple](#gep-decision-curve-made-simple) first.

#### `Extrapolation_Assumption_Checks`

- `Exponential_Hazard_Per_Year`: hazard implied by the intercept-only exponential model.
- `Weibull_Shape`: the key non-exponential comparator. Values near 1 are more compatible with constant hazard.
- `Delta_AIC_Weibull_minus_Exponential`: negative values favor Weibull, positive values favor exponential.
- `Pre5yr_Hazard_Per_Year` and `Post5yr_Hazard_Per_Year`: crude piecewise hazards before and after 5 years.
- `Post_vs_Pre_Hazard_Ratio`: a quick screen for an obvious hazard break after 5 years.
- `Support_Status`: overall interpretation used to label 7-year and 10-year workbook rows.
- `Support_Note`: plain-language explanation of why the later-horizon extrapolation was judged supported, weakly supported, or unsupported.

Practical reading rule:
- `Supported`: later-horizon extrapolation looked reasonably compatible with the observed data.
- `Weakly Supported`: later-horizon extrapolation was not clearly contradicted, but evidence was limited or mixed.
- `Unsupported`: later-horizon extrapolation should be treated as exploratory rather than defensible.

Important distinction:
- `Unsupported` does not always mean the data proved the hazard was non-constant.
- It can also mean the check was too sparse to be informative, or that the fitting routine encountered a technical issue that prevented a meaningful comparison.
- Read `Support_Note` before interpreting `Unsupported` as scientific evidence against exponential extrapolation.

How to read common `Support_Note` patterns:
- `Fewer than 10 events were available...`
  This means the data were too sparse to interrogate hazard shape responsibly. It is an information problem, not proof that exponential extrapolation is wrong.
- `The extrapolation check could not be completed...`
  This means the parametric comparison failed technically. Interpret this as “the assumption has not yet been adequately checked,” not as “the assumption has been disproved.”
- `At least one diagnostic favored non-constant hazard...`
  This is the strongest form of `Unsupported`, because it means the diagnostic comparisons themselves pointed away from constant hazard.

Why the 5-year model can still be fine when 7-year and 10-year rows are not:
- The 5-year GEP value is imported directly from the assay workflow.
- The 7-year and 10-year values are not directly imported assay outputs in this pipeline. They are extrapolated from the 5-year value using a constant-hazard rule.
- So a later-horizon problem usually means the extension rule is questionable, not that the imported 5-year prediction necessarily failed.

Why the pipeline does not simply switch to a “better” later-horizon model:
- Replacing exponential extrapolation with a Weibull or flexible model would introduce a new modeling layer based on this dataset rather than on the imported assay.
- That may be useful as a sensitivity analysis, but it is not the same as validating the original assay output.
- For that reason, the main pipeline keeps the original 5-year imported prediction as primary and uses the extrapolation support tier to qualify the 7-year and 10-year interpretation.

#### `PRAME_Summary`

- `N`, `Events`: Sample sizes for the PRAME-complete subset at that timepoint.
- `Base_Harrell_C`, `Enhanced_Harrell_C`: Discrimination for the GEP-only and GEP-plus-PRAME models on the same patients.
- `Delta_Harrell_C`: Improvement in Harrell's C after adding PRAME.
- `Delta_CI_Lower`, `Delta_CI_Upper`: Bootstrap interval for the delta-C estimate.
- `LR_p`: Likelihood-ratio p-value for the nested-model comparison.
- `PRAME_HR`, `PRAME_HR_CI_Lower`, `PRAME_HR_CI_Upper`: PRAME effect estimate from the enhanced model.
- `Analysis_Tier`: `Primary` for MFS, `Exploratory` for MSS.
- `Interpretation`: Auto-generated summary tying delta C and model support into plain language.

#### `Missing_Data_Summary`

Single column dictionary:

| Metric | Meaning |
| --- | --- |
| `Total_Patients_n` | Patients evaluated for missingness diagnostics inside this run. |
| `Missingness_Groups_n` | Distinct missingness patterns detected (rows in the patterns table, when available). |
| `Baseline_Variables_with_Significant_Differences_n` | Count of baseline covariates that differ across missingness groups (p < 0.05). |
| `Survival_by_Missingness_Logrank_p` | Log-rank p-value comparing survival between missingness strata. |
| `Imputable_Patients_n` | Patients that could be recovered through imputation diagnostics. |

### Differences Between MFS and MSS Tabs

- **Event definition:** MFS treats metastasis as the event and censors deaths without metastasis; MSS uses cause-specific death as the event.
- **Discrimination definition:** MFS `Harrell_C` is a horizon-specific concordance estimate after truncating follow-up at the requested timepoint, whereas MSS `Harrell_C` uses full observed follow-up in the horizon-specific analysis set. Do not read those two columns as identical estimands.
- **Modeling approach:** MSS commonly has fewer usable events than MFS and therefore more `*_Fallback_Used` flags or `NA` fields when horizons are sparse.
- **Sample size:** MSS tables often have lower `N`, which in turn drives more `NA` metrics. Always sanity-check counts before comparing outcomes.

### How to Use the Sheets

1. **Check sample counts first.** When `N` or `Events` fall below the exploratory thresholds (20 total / 5 events), annotate results as preliminary.
2. **Reference `STATISTICAL_METHODS.md#gep-validation-metrics` for formulas.** Use this guide for workbook logistics and the statistics doc for the math.
3. **Summarize per timepoint:** Combine one calibration sentence, one discrimination sentence, and (if available) a decision-curve takeaway. Use the template below and cite the workbook filename.
4. **Escalate flags:** If the Missing Data sheet lists significant baseline differences or non-random loss, note it in your write-up before quoting performance.
5. **Document fallbacks:** Whenever `*_Fallback_Used == TRUE`, mention that the simplified estimator supplied the metric.

### Suggested Interpretation Template

> "For 5-year metastasis-free survival (`uveal_full_mfs_consolidated_summary.xlsx`), Harrell's C was 0.71 with calibration slope 0.98 and positive decision-curve net benefit at 10–30% thresholds (see `STATISTICAL_METHODS.md#gep-validation-metrics`)."

---

## Understanding PRAME Incremental Outputs

### What The PRAME Rows Now Mean

- The PRAME sheets no longer use heuristic risk multipliers or reclassification counts.
- Each row compares two models fitted on the same PRAME-complete patients at that timepoint: GEP alone versus GEP plus PRAME.
- The main question is whether PRAME improves discrimination beyond the imported GEP prediction already supplied by the lab-derived fields.

### How To Read The Result

1. Start with `Delta_Harrell_C`.
2. Check whether `Delta_CI_Lower` to `Delta_CI_Upper` stays above 0.
3. Use `LR_p` and the PRAME hazard ratio as support, not as the primary decision rule.
4. Treat MSS rows as exploratory even when they look favorable, because the MSS concordance estimand differs from MFS in this pipeline.

### Practical Tips

1. Check banner messages and placeholder text first; sparse cohorts can still produce explanatory rows instead of numeric estimates.
2. If the workbook also contains `mfs_prame_delta_c.png` or `mss_prame_delta_c.png`, use the plot and the table together. They should tell the same story.
3. Prefer the consolidated workbook over the technical workbook for interpretation; the unified workbook is comparison-only.

---

## Clinical Significance vs Statistical Significance

### Understanding P-values

**P-value:** Probability of observing this difference if no true difference exists

**Common Misinterpretations:**
- ❌ p < 0.05 means clinically important
- ❌ p > 0.05 means no difference
- ❌ Smaller p-value = larger effect

**Correct Interpretation:**
- ✅ p < 0.05: Evidence against null hypothesis (unlikely due to chance alone)
- ✅ p > 0.05: Insufficient evidence to reject null (doesn't prove no difference)
- ✅ P-value measures statistical evidence, not clinical importance

### Assessing Clinical Significance

**Focus on Effect Size and Confidence Interval, Not Just P-value**

**Example 1: Statistically Significant but Clinically Trivial**
```
Vision change: β = -0.05 logMAR (95% CI: -0.08 to -0.02), p = 0.001
```
- Statistically significant (p < 0.05)
- Effect size tiny (0.05 logMAR ≈ half a line on eye chart)
- **Conclusion:** Not clinically meaningful despite significance

**Example 2: Not Significant but Potentially Important**
```
Overall survival: HR = 0.70 (95% CI: 0.45 to 1.10), p = 0.12
```
- Not statistically significant (p > 0.05)
- Point estimate suggests 30% risk reduction
- Wide CI includes both large benefit (55% reduction) and harm (10% increase)
- **Conclusion:** Possibly clinically important, but underpowered study

**Example 3: Statistically and Clinically Significant**
```
Local recurrence: OR = 0.45 (95% CI: 0.25 to 0.80), p = 0.007
```
- Statistically significant (p < 0.05)
- Large effect (55% reduction in odds)
- Narrow CI (excludes no effect)
- **Conclusion:** Strong evidence for meaningful clinical benefit

### Clinical Significance Thresholds

| Outcome | Minimal Clinically Important Difference (MCID) |
|---------|-----------------------------------------------|
| **Vision change** | 0.2 logMAR (2 lines on eye chart) |
| **Tumor height** | 1.0 mm reduction |
| **Overall survival HR** | 0.70 or 1.43 (30% change) |
| **Complication OR** | 0.50 or 2.0 (50% change) |

**How to Use:**
1. Check if confidence interval includes MCID
2. If yes: Clinically significant difference possible
3. If no: Effect likely too small to matter clinically

---

## Common Pitfalls

### 1. Confusing Association with Causation

**Issue:** Observational data can show associations but cannot prove causation

**Example:**
- GKSRS patients have better survival than PBT patients
- ❌ Conclusion: GKSRS causes better survival
- ✅ Conclusion: GKSRS is associated with better survival, but residual confounding possible

**Why:** Patients weren't randomized → unmeasured differences may explain results

### 2. Ignoring Confidence Intervals

**Issue:** Focusing only on point estimates without considering uncertainty

**Example:**
```
HR = 0.65 (95% CI: 0.45 to 0.95)
```
- Point estimate: 35% risk reduction
- CI range: 5% to 55% reduction
- **Truth likely somewhere in that range, not exactly 35%**

### 3. Over-Interpreting Subgroup Analyses

**Issue:** Finding "significant" subgroup differences when none truly exist

**Multiple Comparisons Problem:**
- Test 20 subgroups → expect 1 false positive by chance (p < 0.05)
- Require p < 0.05 for interaction, not individual subgroup p-values
- Exploratory findings need confirmation

**Example:**
- GKSRS HR = 0.50 in males (p=0.04), HR = 0.80 in females (p=0.35)
- P-for-interaction = 0.25
- **Conclusion:** No strong evidence for sex differences (interaction p > 0.05)

### 4. Ignoring PH Violations

**Issue:** Reporting Cox HR when proportional hazards assumption violated

**Example:**
```
Cox HR = 0.70 (p=0.05)
PH test: p = 0.02 (VIOLATED)
RMST difference = 0.1 years (p=0.30)
```

**Problem:** HR averages time-varying effects; may not reflect reality
**Solution:** Report RMST as primary, note PH violation

### 5. Misinterpreting Non-Significance

**Issue:** Concluding "no difference" when p > 0.05

**Example:**
```
HR = 0.75 (95% CI: 0.50 to 1.12), p = 0.16
```
- ❌ "GKSRS and PBT have equivalent survival"
- ✅ "We cannot rule out a 25% risk reduction or 12% risk increase with GKSRS"

**Reality:** Study may be underpowered; true effect remains uncertain

### 6. Ignoring Baseline Imbalances

**Issue:** Comparing treatment groups with different baseline characteristics

**Example:**
- PBT group: Mean age 55, mean tumor height 4 mm
- GKSRS group: Mean age 68, mean tumor height 7 mm
- Survival differs between groups
- **Problem:** Difference may reflect age/tumor size, not treatment

**Solution:** Check baseline tables; rely on adjusted regression models

---

## Troubleshooting

### "Why are survival curves missing?"

**Possible Reasons:**
1. **Insufficient events:** <5 total events (minimum for survival analysis)
2. **Insufficient groups:** Events in only 1 treatment group
3. **Analysis skipped:** Check logs for explanation

**How to Check:**
- Open `*_summary.xlsx` to see event counts
- Read `logs/txt/run_log_*.txt` for skip reasons
- Look for `*_analysis_not_performed.txt` files

**Example:**
```
GKSRS-only cohort, PFS-2: Only 3 events (need 5)
→ Survival curves not generated
→ Summary tables still available
```

### "Why do forest plots have missing subgroups?"

**Possible Reasons:**
1. **Sample size too small:** <2 patients per treatment arm
2. **No events:** 0 events in subgroup
3. **Statistical instability:** Extreme event imbalances

**How to Check:**
- Open `*_subgroup_diagnostics.xlsx`
- Look for "exclusion_reason" column
- Common reasons: "insufficient_sample", "zero_events", "skipped_non_finite"

**Example:**
```
Age 80+ years subgroup:
- PBT: n=3, events=0
- GKSRS: n=5, events=2
→ Excluded due to zero events in PBT arm
```

### "Why do regression tables show different results than summary tables?"

**Answer:** Adjustment for confounders

**Example:**
```
Summary table (unadjusted):
- PBT recurrence: 15%
- GKSRS recurrence: 20%
- Difference: +5% (GKSRS worse)

Regression table (adjusted):
- GKSRS OR = 0.75 (GKSRS better)

Explanation:
- GKSRS patients older with larger tumors (higher baseline risk)
- After adjusting for age and tumor size, GKSRS actually performs better
```

**Lesson:** Always interpret adjusted estimates, not crude rates

### "Why does RMST show significance but Cox doesn't (or vice versa)?"

**RMST Significant, Cox Not:**
- Treatment effect varies over time (PH violated)
- RMST captures net benefit; Cox averages contradictory effects
- **Trust RMST**

**Cox Significant, RMST Not:**
- Effect is small in absolute terms but consistent relative risk
- RMST may be underpowered for small differences
- Both are valid; report both

**Example:**
```
Cox HR = 0.70 (p=0.04)
RMST difference = 0.2 years (p=0.15)
PH test: p=0.03 (violated)

Interpretation: Treatment reduces hazard by 30% on average, but effect 
varies over time. Absolute survival gain is 2.4 months, which is not 
statistically significant with current sample size. Report RMST as primary 
given PH violation.
```

### "How do I export specific results?"

**Excel Tables:**
- Open in Excel or LibreOffice
- Copy relevant cells
- Paste into manuscript

**HTML Tables:**
- Open in browser
- Right-click → "Save As" → PDF (print to PDF)
- Or copy table and paste into Word

**PNG Figures:**
- Already publication-ready
- Resolution: 300 DPI
- Insert directly into manuscript

**Tips:**
- `merged_tables/` folder has cross-cohort comparisons, including a separately labeled baseline table spanning full, restricted, and GKSRS-only cohorts
- `*_summary.xlsx` has descriptive statistics
- `*.html` tables have full model details with diagnostics

---

## Next Steps

After interpreting results:

1. **Verify Findings:**
   - Check baseline balance
   - Review exclusion reasons
   - Validate event counts

2. **Draft Interpretation:**
   - Focus on adjusted estimates (regression tables)
   - Consider clinical significance (effect sizes, MCID)
   - Note limitations (sample size, PH violations)

3. **Plan Manuscript:**
   - Table 1: Baseline characteristics (`baseline_characteristics.xlsx`)
   - Table 2: Primary outcomes (survival, recurrence rates)
   - Table 3: Secondary outcomes (complications, tumor response)
   - Figure 1: Survival curves
   - Figure 2: Forest plots (subgroup analysis)

4. **Consult Statistical Methods:**
   - See [STATISTICAL_METHODS.md](STATISTICAL_METHODS.md) for methodology details
   - See [TECHNICAL.md](TECHNICAL.md) for implementation specifics
   - See [CALCULATIONS.md](CALCULATIONS.md) for variable definitions

5. **Report Results:**
   - Cite the analysis pipeline (see README.md)
   - Include software versions from logs
   - Describe quality assurance procedures

---

## Getting Help

**Documentation:**
- [README.md](../README.md) - Overview and quick start
- [CALCULATIONS.md](CALCULATIONS.md) - Variable derivations
- [TECHNICAL.md](TECHNICAL.md) - Implementation details
- [STATISTICAL_METHODS.md](STATISTICAL_METHODS.md) - Statistical approaches

**Common Issues:**
- Check logs in `logs/txt/` for error messages
- Review diagnostics workbooks (`*_diagnostics.xlsx`)
- Verify input data quality
- Confirm adequate sample sizes and event counts

**Contact:**
Refer to README.md for author contact information and citation details.
