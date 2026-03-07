# Statistical Methods

This document explains the statistical approaches used throughout the analysis pipeline.

---

## Table of Contents

- [Overview](#overview)
- [Survival Analysis](#survival-analysis)
- [Restricted Mean Survival Time (RMST)](#restricted-mean-survival-time-rmst)
- [Proportional Hazards Assumption Testing](#proportional-hazards-assumption-testing)
- [Competing Risks Analysis](#competing-risks-analysis)
- [Binary Outcomes](#binary-outcomes)
- [Continuous Outcomes](#continuous-outcomes)
- [Subgroup Analysis](#subgroup-analysis)
- [GEP Validation Metrics](#gep-validation-metrics)
- [Multiple Testing](#multiple-testing)
- [Statistical Metric Categorization](#statistical-metric-categorization)

---

## Overview

The analysis employs standard statistical methods appropriate for comparative effectiveness research:

| Analysis Type | Method | R Package | Function |
|---------------|--------|-----------|----------|
| **Survival analysis** | Kaplan-Meier, Cox regression, RMST | `survival` | `survfit()`, `coxph()`, `rmst2()` |
| **Binary outcomes** | Logistic regression | Base R | `glm(family="binomial")` |
| **Continuous outcomes** | Linear regression | Base R | `lm()` |
| **Table generation** | Summary tables, regression tables | `gtsummary` | `tbl_summary()`, `tbl_regression()` |
| **Proportional hazards testing** | Scaled Schoenfeld residuals | `survival` | `cox.zph()` |
| **Competing risks** | Cumulative incidence functions | `cmprsk` | `cuminc()` |

---

## Survival Analysis

### Kaplan-Meier Method

**Purpose:** Estimate survival probabilities over time

**Implementation:** `survival::survfit()`

**Outputs:** 
- Survival curves stratified by treatment
- Median survival times with 95% CI
- Survival probability tables

**Key Features:**
- Right-censored survival data
- Non-parametric estimation
- No distributional assumptions

### Cox Proportional Hazards Regression

**Purpose:** Compare hazard rates between treatment groups while adjusting for confounders

**Implementation:** `survival::coxph()`

**Model Formula:**
```r
Surv(time, event) ~ treatment + age_at_diagnosis + sex + location + optic_nerve
```

**Outputs:**
- Hazard ratios with 95% CI
- P-values from likelihood ratio tests
- Model diagnostics

**Assumptions:**
- Proportional hazards (tested with `cox.zph()`)
- Independent censoring
- Multiplicative hazard effects

### Log-Rank Test

**Purpose:** Compare survival distributions between groups

**Implementation:** Built into `survival::survfit()`

**Interpretation:**
- p < 0.05 indicates significantly different survival distributions
- Non-parametric test (no distributional assumptions)
- Sensitive to differences across entire follow-up period

---

## Restricted Mean Survival Time (RMST)

### Motivation

**Problem:** Cox regression assumes proportional hazards (constant hazard ratio over time), which is often violated in real data

**Solution:** RMST provides a non-parametric, clinically interpretable alternative that does not require the proportional hazards assumption

### What is RMST?

**Definition:** The area under the survival curve up to a specified time point (τ)

**Clinical Interpretation:** The average survival time (in years) up to time τ

**Example:**
- RMST at 3 years = 2.5 years means patients survived an average of 2.5 years during the first 3 years of follow-up
- RMST difference = 0.3 years means one treatment group survived 0.3 years (3.6 months) longer on average

### RMST Analysis Implementation

**Package:** `survRM2::rmst2()`

**Time Horizon (τ):**
- Objective 1c (Overall Survival): 5 years
- Objective 1d (PFS): 3 years
- Objective 3a (PFS-2): 3 years
- Objective 4 (MFS, MSS): 5 years

**Outputs:**
1. **RMST Estimates:** Mean survival time for each treatment group
2. **RMST Difference:** Treatment effect (years gained/lost)
3. **P-value:** Statistical significance of difference
4. **95% CI:** Uncertainty around difference
5. **Visualization:** RMST curves with shaded areas

### Advantages of RMST

| Feature | RMST | Cox HR |
|---------|------|--------|
| **Assumption-free** | ✓ No proportional hazards assumption | ✗ Requires proportional hazards |
| **Clinical interpretation** | ✓ Years gained/lost | ✗ Multiplicative ratio |
| **Non-parametric** | ✓ Distribution-free | ✗ Assumes exponential-family baseline |
| **Direct comparison** | ✓ Absolute difference | ✗ Relative measure |

### When to Use RMST vs Cox

**Use RMST when:**
- Proportional hazards assumption violated
- Direct clinical interpretation needed ("X months gained")
- Regulatory submissions requiring robust methods
- Communicating results to non-statisticians

**Use Cox when:**
- Proportional hazards assumption holds
- Relative risk measures preferred
- Standard in your clinical field
- Comparing multiple adjusted models

**Best Practice:** Report both RMST and Cox HR when possible

---

## Proportional Hazards Assumption Testing

The proportional hazards (PH) assumption states that the hazard ratio between treatment groups remains constant over time.

### Testing Methodology

**Method:** Scaled Schoenfeld residuals test

**Implementation:** `survival::cox.zph()`

**Test Statistic:** Correlation between scaled residuals and time

**Interpretation:**
- **p > 0.05:** PH assumption holds (constant HR over time)
- **p ≤ 0.05:** PH assumption violated (time-varying HR)

### Diagnostic Outputs

For each time-to-event analysis with ≥10 total events, the pipeline generates:

**1. Statistical Tests** (`*_ph_diagnostics.xlsx`)
- Global test p-value (overall model assumption)
- Per-covariate test p-values
- Chi-square statistics
- Degrees of freedom

**2. Schoenfeld Residual Plots** (`*_ph_plots_{variable}.png`)
- Residuals vs time scatter plots
- Smoothed trend line
- 95% confidence band
- Flat trend = PH holds; non-flat = PH violation

### Clinical Implications

**When PH holds:**
- Cox HR provides valid treatment effect estimate
- HR interpretation straightforward (constant over time)
- Standard survival analysis appropriate

**When PH violated:**
- Cox HR may be misleading (average effect that varies over time)
- RMST provides more appropriate analysis
- Consider time-stratified models or parametric alternatives
- Report both Cox and RMST results

### Example Interpretation

```
Treatment PH Test: p = 0.03 (VIOLATION)
```

**Interpretation:** The treatment hazard ratio changes over time. For example, GKSRS may be more protective early but less protective late in follow-up. Use RMST for primary analysis and report Cox HR as secondary.

### Automation

The pipeline automatically:
1. Tests PH assumption for all Cox models with ≥10 events
2. Generates diagnostic plots for each covariate
3. Documents violations in Excel workbooks
4. Provides RMST as alternative when PH violated
5. Stores diagnostics in `{cohort}/{objective}/h_proportional_hazards_diagnostics/`

**Minimum Event Threshold:** PH testing skipped when <10 total events (insufficient power for reliable testing)

---

## Competing Risks Analysis

### What are Competing Risks?

**Definition:** Events that preclude or alter the probability of the event of interest

**Example:** Death is a competing risk for local recurrence because patients who die cannot subsequently develop recurrence

### Why Standard Kaplan-Meier Fails

**Problem:** Kaplan-Meier treats competing events as censoring, which:
- Overestimates event probabilities
- Assumes patients remain "at risk" after competing event
- Produces biased cumulative incidence estimates

**Solution:** Cumulative Incidence Functions (CIF) account for competing events

### Implementation

**Package:** `cmprsk::cuminc()`

**Event Coding:**
```r
0 = Censored (still at risk)
1 = Event of interest (e.g., local recurrence)
2 = Competing event (e.g., death before recurrence)
```

**Outputs:**
1. **Cumulative Incidence Curves:** Probability of event over time accounting for competing events
2. **Gray's Test:** Statistical comparison between treatment groups
3. **Fine-Gray Model:** Regression with competing risks adjustment

### When Competing Risks Matter

| Outcome | Competing Event | Impact |
|---------|-----------------|--------|
| **Local Recurrence** | Death before recurrence | High (older patients, comorbidities) |
| **Metastatic Progression** | Death before metastasis | High (aggressive disease) |
| **PFS** | None (composite endpoint) | N/A (already accounts for death) |
| **Overall Survival** | None (death is the outcome) | N/A |
| **Vision Changes** | Death before vision loss | Moderate |

### Interpretation

**Cumulative Incidence at 5 years = 15%**
- 15% of patients developed the event by 5 years, accounting for those who died first
- More realistic than K-M estimate which might overestimate as 20%

**Gray's Test p = 0.03**
- Treatment groups differ significantly in cumulative incidence
- Accounts for differential competing risk rates

---

## Binary Outcomes

### Logistic Regression

**Purpose:** Compare binary outcome rates (yes/no events) between treatment groups

**Implementation:** `glm(family = binomial(link = "logit"))`

**Model Formula:**
```r
outcome ~ treatment + age_at_diagnosis + sex + location + optic_nerve
```

**Outputs:**
- Odds ratios with 95% CI
- P-values from likelihood ratio tests
- Model diagnostics

**Interpretation:**
- OR > 1: Higher odds in treatment group
- OR < 1: Lower odds in treatment group
- OR = 1: No difference

### Applications

| Outcome | Analysis Type | Location |
|---------|---------------|----------|
| **Local Recurrence** | Binary (yes/no) | Objective 1a |
| **Metastatic Progression** | Binary (yes/no) | Objective 1b |
| **Radiation Retinopathy** | Binary (yes/no) | Objective 2b |
| **Neovascular Glaucoma** | Binary (yes/no) | Objective 2c |
| **Serous Retinal Detachment** | Binary (yes/no) | Objective 2d |

### Assumptions

- Independent observations
- Linearity of log-odds
- No perfect multicollinearity
- Adequate sample size (rule of thumb: 10 events per predictor)

---

## Continuous Outcomes

### Linear Regression

**Purpose:** Compare continuous outcome changes between treatment groups

**Implementation:** `lm()`

**Model Formulas:**

**Unadjusted (Primary):**
```r
outcome_change ~ treatment
```

**Adjusted for Baseline (Sensitivity):**
```r
outcome_change ~ treatment + baseline_value + confounders
```

**Outputs:**
- Regression coefficients (mean difference)
- 95% CI
- P-values
- R² (variance explained)

### Applications

| Outcome | Primary Analysis | Sensitivity Analysis | Location |
|---------|------------------|---------------------|----------|
| **Tumor Height Change** | Unadjusted | Baseline-adjusted | Objective 1e, 1f |
| **Vision Change** | Unadjusted | Baseline-adjusted | Objective 2a |

### Interpretation

**Tumor Height Change Coefficient = -0.5 mm**
- Treatment group had 0.5 mm greater decrease (more shrinkage) than reference
- Negative change = shrinkage (see [CALCULATIONS.md](CALCULATIONS.md))

**Vision Change Coefficient = -0.2 logMAR**
- Treatment group had 0.2 logMAR less worsening (better vision preservation)
- Negative change = worsening vision (see [CALCULATIONS.md](CALCULATIONS.md))

### Assumptions

- Linearity of relationships
- Homoscedasticity (constant variance)
- Normality of residuals
- Independent observations

---

## Subgroup Analysis

### Purpose

Evaluate whether treatment effects differ across patient subgroups defined by:
- Age groups
- Sex
- Tumor location
- Tumor size categories
- Baseline vision
- Disease stage

### Statistical Approach

**Method:** Stratified analysis with interaction testing

**Steps:**
1. Fit model within each subgroup
2. Estimate treatment effect per subgroup
3. Test treatment × subgroup interaction
4. Generate forest plots visualizing heterogeneity

**Interaction Test:**
```r
outcome ~ treatment * subgroup_variable + confounders
```

**Interpretation:**
- **p < 0.10:** Significant interaction (treatment effect varies by subgroup)
- **p ≥ 0.10:** No significant interaction (consistent treatment effect)

### Filtering Criteria

See [TECHNICAL.md](TECHNICAL.md#subgroup-filtering) for detailed subgroup filtering requirements.

**Summary:**
- Minimum 2 patients per treatment arm
- Minimum 1 event per treatment arm (survival outcomes)
- Automatic exclusion of unstable comparisons

### Outputs

**1. Forest Plots** (`*_forest_plot.png`)
- Treatment effect estimates per subgroup
- 95% confidence intervals
- Overall pooled estimate
- P-value for interaction

**2. Subgroup Tables** (`*_subgroup_results.xlsx`)
- Per-subgroup sample sizes
- Event rates or means
- Effect estimates with CI
- P-values

**3. Diagnostic Workbooks** (`*_subgroup_diagnostics.xlsx`)
- Quality control metrics
- Exclusion documentation
- Missing data patterns
- Statistical warnings

### Interpretation Guidelines

**Significant Interaction (p < 0.10):**
- Treatment effect genuinely differs across subgroups
- Clinically meaningful heterogeneity
- Consider subgroup-specific treatment recommendations

**Non-Significant Interaction (p ≥ 0.10):**
- No strong evidence for differential effects
- Apply overall treatment effect across subgroups
- Observed differences likely due to chance

**Caution:** Subgroup analyses are exploratory and hypothesis-generating. Confirm findings in independent datasets before clinical application.

---

## GEP Validation Metrics

Gene Expression Profiling (GEP) provides lab-reported probabilities of metastasis-free survival (MFS) and melanoma-specific survival (MSS). Objective 4 validates these predictions against observed outcomes.

### Validation Framework

**Goal:** Assess whether GEP probabilities accurately predict patient outcomes

**Analyses:**
1. **Observed vs Expected / Calibration:** Agreement between lab-reported and realized event rates
2. **Discrimination:** Ability to separate patients with vs without events
3. **Clinical Utility:** Impact on clinical decision-making
4. **Competing-risk MSS sensitivity analysis:** Separate accounting for non-melanoma death when evaluating MSS

### Calibration Assessment

**Purpose:** Do predicted probabilities match observed rates?

**Method:** Table-first validation at 5, 7, and 10 years using grouped observed-vs-expected comparisons plus calibration diagnostics.

**Interpretation:**
- **Perfect calibration:** Points lie on 45° diagonal line
- **Overestimation:** Points below diagonal (predicted > observed)
- **Underestimation:** Points above diagonal (predicted < observed)

**Metrics:**
- **Overall O/E ratio:** Total observed events divided by total expected events across GEP classes
- **Exact Poisson CI for overall O/E:** Quantifies uncertainty around the overall observed-to-expected ratio
- **Pearson goodness-of-fit p-value:** Tests whether grouped observed counts depart materially from grouped expected counts across GEP classes
- **Nam-D'Agostino statistic:** Grouped calibration test reported in the consolidated workbook
- **Integrated Calibration Index (ICI):** Average absolute difference between predicted and observed risk
- **Calibration slope:** Should be close to 1.0; reported as the primary slope summary across timepoints

**Endpoint note:**
- MFS uses metastasis events.
- MSS standard validation uses melanoma-specific death as the event.
- Non-melanoma death is handled separately in competing-risk analyses rather than being treated as an MSS event.

### Discrimination Assessment

**Purpose:** Can GEP separate patients with vs without events?

**Metrics:**

**1. C-Statistic (Concordance Index)**
- Range: 0.5 (no discrimination) to 1.0 (perfect discrimination)
- Interpretation:
  - 0.50-0.60: Poor
  - 0.60-0.70: Acceptable
  - 0.70-0.80: Good
  - 0.80-0.90: Excellent
  - 0.90-1.00: Outstanding

**2. Integrated and Time-Aggregated Discrimination**
- **Integrated AUC:** Averages discrimination over follow-up rather than at a single timepoint
- **Cumulative discrimination:** Summarizes separation across prespecified follow-up windows
- **Time-averaged discrimination:** Averages discrimination across the evaluation horizon while accounting for censoring

### Clinical Utility Assessment

**Purpose:** Does GEP improve clinical decisions beyond standard factors?

**Methods:**

**1. Decision Curve Analysis**
- Plots net benefit across decision thresholds
- Compares GEP-based decisions vs clinical factors alone
- Shows range of thresholds where GEP adds value

**2. Reclassification Metrics**
- **Net Reclassification Improvement (NRI):** Proportion correctly reclassified
- **Integrated Discrimination Improvement (IDI):** Improvement in predicted probabilities

**3. Incremental Predictive Value**
- Compare models with vs without GEP
- Likelihood ratio test: p < 0.05 indicates added value
- ΔC-statistic: improvement in discrimination

### Validation Workflow

**Step 1: Data Preparation**
- Load GEP predictions (lab-reported probabilities)
- Link to 5-, 7-, and 10-year survival outcomes (MFS, MSS)
- Exclude patients with missing GEP data
- For MSS, define the primary event as melanoma-specific death and retain competing death indicators for companion competing-risk analyses

**Step 2: Calibration Analysis**
- Compute grouped observed and expected events by GEP class
- Derive overall O/E ratios with exact Poisson confidence intervals
- Compute Pearson goodness-of-fit p-values across GEP classes
- Compute Nam–D’Agostino χ², Integrated Calibration Index (ICI), and calibration slope
- Record results in the consolidated workbook (no standalone calibration PNGs)

**Step 3: Discrimination Analysis**
- Calculate Harrell’s C, integrated AUC, cumulative/time-averaged discrimination
- Store metrics in the discrimination tab; no time-dependent AUC plots are emitted

**Step 4: Clinical Utility Analysis**
- Perform decision-curve calculations and PRAME-based NRI/IDI comparisons
- Capture net-benefit ranges and reclassification tables inside the workbook

**Step 5: Reporting**
- Export outcome-specific consolidated Excel workbooks (`*_MFS_consolidated_summary.xlsx`, `*_MSS_consolidated_summary.xlsx`) and companion text reports
- Export outcome-specific technical workbooks (`*mfs_validation_summary.xlsx`, `*mss_validation_summary.xlsx`) for lower-level observed/expected and competing-risk detail that complements, rather than duplicates, the consolidated summaries
- Export the root-level cross-outcome workbook `*unified_gep_validation_summary.xlsx`
- Export the simple QC workbook `unified_summary/*simple_gep_validation.xlsx`
- Ensure narrative summaries preserve the cohort label used for the run and print the overall O/E ratio with its Poisson CI and Pearson goodness-of-fit p-value
- Provide KM (MFS) or CIF (MSS) curves only; calibration/decision/discrimination visuals live in tables

### Expected Outputs

**Location:** `{cohort}/04_GEP_Validation/` with outcome-specific subfolders for MFS and MSS

**Files:**
- `a_metastasis_free_survival/*_MFS_consolidated_summary.xlsx` — primary MFS review workbook, including `Observed_Expected_Summary`
- `b_melanoma_specific_survival/*_MSS_consolidated_summary.xlsx` — primary MSS review workbook, including `Observed_Expected_Summary`
- `a_metastasis_free_survival/*mfs_validation_summary.xlsx` and `b_melanoma_specific_survival/*mss_validation_summary.xlsx` — technical-detail workbooks without duplicated high-level calibration/discrimination summary sheets
- `a_metastasis_free_survival/*mfs_validation_summary.txt` and `b_melanoma_specific_survival/*mss_validation_summary.txt` — narrative summaries
- `*unified_gep_validation_summary.xlsx` at the root of `04_GEP_Validation/` — comparison-only cross-outcome workbook
- `unified_summary/*simple_gep_validation.*` — optional actual-vs-expected QC output from the simple checker
- Limited PNGs: KM for MFS and CIF for MSS only when survival curves are generated

**Schema note:** `PRAME_Summary` is always written in consolidated workbooks, and `PRAME_Comparison` is always written in unified workbooks. Sparse cohorts may receive explanatory placeholder rows instead of full PRAME reclassification results.

### Interpretation Example

**Calibration:** Slope = 0.95 (good), Intercept = 0.02 (good)
- GEP predictions closely match observed rates

**Discrimination:** C-statistic = 0.75 (good)
- GEP effectively separates patients with vs without metastasis

**Clinical Utility:** Decision curve shows net benefit at thresholds 10-30%
- GEP improves decisions for patients with 10-30% predicted risk
- Outside this range, clinical factors alone may suffice

**Conclusion:** GEP provides well-calibrated, discriminative predictions with clinical utility for intermediate-risk patients.

---

## Statistical Metric Categorization

The analysis pipeline categorizes statistical outputs into three tiers based on clinical interpretability and robustness.

### Primary Metrics (Tier 1)

**Definition:** Model-based estimates with direct clinical interpretation

**Examples:**
- Hazard ratios from Cox regression
- Odds ratios from logistic regression
- Regression coefficients from linear models
- RMST differences
- Risk differences

**Characteristics:**
- Adjusted for confounders
- Direct effect estimates
- Primary basis for clinical conclusions

### Secondary Metrics (Tier 2)

**Definition:** Descriptive statistics and unadjusted comparisons

**Examples:**
- Median survival times
- Event rates by treatment group
- Mean changes in continuous outcomes
- Survival probabilities at specific time points

**Characteristics:**
- Unadjusted for confounders
- Descriptive summaries
- Complement primary metrics

### Tertiary Metrics (Tier 3)

**Definition:** Technical diagnostics and quality metrics

**Examples:**
- P-values from proportional hazards tests
- Model fit statistics (AIC, BIC, R²)
- Residual diagnostics
- Calibration metrics
- Number of observations/events

**Characteristics:**
- Assess model assumptions and quality
- Guide interpretation of primary metrics
- Technical validation

### Reporting Priority

**Clinical Reports:**
1. Primary metrics (adjusted effect estimates)
2. Secondary metrics (descriptive context)
3. Tertiary metrics (footnotes on model quality)

**Statistical Appendices:**
1. All three tiers comprehensively documented
2. Full model diagnostics
3. Sensitivity analyses

**Forest Plots:**
- Display: Primary metrics (effect estimates with CI)
- Annotate: Sample sizes, event counts (secondary metrics)
- Document: Model diagnostics in separate tables (tertiary metrics)

This categorization ensures clinicians focus on actionable findings while maintaining statistical rigor and transparency.

---

## References

Key statistical methods references:

1. **Cox Regression:** Cox DR. Regression models and life-tables. J R Stat Soc Series B. 1972;34(2):187-220.

2. **RMST:** Uno H, et al. Moving beyond the hazard ratio in quantifying the between-group difference in survival analysis. J Clin Oncol. 2014;32(22):2380-2385.

3. **Proportional Hazards Testing:** Grambsch PM, Therneau TM. Proportional hazards tests and diagnostics based on weighted residuals. Biometrika. 1994;81(3):515-526.

4. **Competing Risks:** Fine JP, Gray RJ. A proportional hazards model for the subdistribution of a competing risk. J Am Stat Assoc. 1999;94(446):496-509.

5. **Prediction Model Validation:** Steyerberg EW, et al. Assessing the performance of prediction models. Epidemiology. 2010;21(1):128-138.

See README.md for complete citation of the analysis pipeline and how to cite this work.
