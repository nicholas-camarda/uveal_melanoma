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

**Workbook traceability:**
- The `Calibration_Summary` and `Calibration_Comparison` sheets now also carry method columns so the statistical variant used for each horizon is explicit: `Nam_D_Agostino_Method`, `ICI_Method`, and `Slope_Method`.

**Endpoint note:**
- MFS uses metastasis events.
- MSS standard validation uses melanoma-specific death as the event.
- Non-melanoma death is handled separately in competing-risk analyses rather than being treated as an MSS event.

### How Expected Counts Are Calculated

For Objective 4, the expected event count at time $t$ is derived from the lab-reported survival probability for each patient. If patient $i$ has predicted survival $S_i(t)$, then the corresponding predicted event probability is:

$$
\hat{p}_i(t) = 1 - S_i(t)
$$

The expected number of events in a cohort or subgroup is the sum of those individual predicted event probabilities:

$$
E(t) = \sum_{i=1}^{N} \hat{p}_i(t) = \sum_{i=1}^{N} \left(1 - S_i(t)\right)
$$

This is the quantity reported as `Expected` in the `Observed_Expected_Summary` sheet.

Implementation details:
- In the shared MSS calculator, expected counts are computed directly as `sum(1 - expected_survival)` within each GEP class.
- In the MFS helper, the same quantity is computed algebraically as $N \times (1 - \bar{S}(t))$, which is equivalent to summing $1 - S_i(t)$ across patients.

### How Observed Counts Are Calculated

The current pipeline uses timepoint-specific binary event indicators that are created during preprocessing. For a given landmark year, the observed count is the sum of patients with the corresponding event indicator equal to 1.

Examples:
- MFS uses `mfs_event_5yr`, `mfs_event_7yr`, and `mfs_event_10yr`
- MSS uses `mss_event_5yr`, `mss_event_7yr`, and `mss_event_10yr`

Accordingly,

$$
O(t) = \sum_{i=1}^{N} I\{\text{event by time } t\}
$$

where $I\{\cdot\}$ is the indicator function.

Important distinction:
- The `Observed_Expected_Summary` sheet still reflects the direct timepoint event-count calculation described above.
- The `Observed_Expected_Summary` sheet reports its count-based goodness-of-fit p-value as `OE_Chi_Square_p`.
- The `Calibration_Summary` sheet reports its grouped survival-calibration p-value as `Nam_D_Agostino_p` and uses grouped Kaplan-Meier estimates with Greenwood variance for that field.

These are not interchangeable quantities:
- `OE_Chi_Square_p` is the older overall observed-versus-expected count-comparison p-value attached to the calibration-in-the-large summary.
- `Nam_D_Agostino_p` is the grouped Greenwood/Nam-D'Agostino survival-calibration p-value attached to the grouped calibration summary.

### Overall O/E Ratio and Exact Poisson Confidence Interval

The overall calibration-in-the-large metric is the observed-to-expected ratio:

$$
\text{O/E} = \frac{O}{E}
$$

where $O$ is the total observed event count and $E$ is the total expected event count across GEP classes.

The workbook reports an exact Poisson confidence interval for this ratio by treating the observed count as Poisson and scaling the resulting interval by the fixed expected count. In practice, the pipeline uses `stats::poisson.test()` on $O$ and then divides the lower and upper confidence limits by $E$.

This is equivalent to the standard exact Poisson approach for a standardized event ratio and is preferred over a normal approximation because it respects the asymmetric uncertainty of low event counts and cannot produce impossible negative lower bounds.

The same `Observed_Expected_Summary` sheet also reports `OE_Chi_Square_p`, which is the count-based goodness-of-fit p-value for the overall O/E comparison. It should be interpreted as an overall observed-versus-expected count check, not as the grouped Greenwood/Nam-D'Agostino survival-calibration test.

#### A Note About Denominator Retention and Summary Contracts

Because the Objective 4 workbooks are now used as the primary review artifacts, the denominator fields must remain interpretable and stable across all summary layers.

- In `Observed_Expected_Summary`, the overall `N` field is the total number of evaluable patients contributing to that horizon-specific O/E calculation, not a group-specific subgroup count.
- For MFS, that overall denominator is carried through explicitly from the O/E helper so the consolidated workbook does not lose the cohort-level denominator when it collapses class-specific results into a single overall row.
- If an upstream result shape does not expose that denominator directly, the reporting helper reconstructs it from the class-level counts rather than leaving the workbook denominator blank.
- The grouped calibration table is a separate object from the overall O/E summary. Its `N` column refers to the number of patients entering the calibration analysis at that horizon, whereas the grouped Greenwood statistic itself is then computed within risk groups inside that analysis set.

These denominator rules are intentionally strict because blank or ambiguous `N` fields make the workbook difficult to audit and can obscure whether a reported metric is being driven by a sparse horizon.

### Grouped Calibration and Goodness-of-Fit Fields

The workbook contains both an overall O/E summary and a separate calibration summary. Those fields are not all generated the same way.

#### MFS calibration implementation

For MFS, the calibration helper now performs a grouped Greenwood-Nam-D'Agostino-style survival calibration assessment. In plain language, it compares predicted risk versus observed risk across risk groups while properly accounting for incomplete follow-up:
- Predicted risks are grouped into quantiles with a target of up to 10 groups and at least 3 groups.
- Within each group, expected events are calculated as the sum of predicted risks.
- Observed event risk at the evaluation horizon is estimated with Kaplan-Meier within that risk group. In plain language, Kaplan-Meier is the standard way to estimate the proportion who have had the event by a given time when not everyone is followed for exactly the same duration.
- Observed events are then expressed on the count scale as $O_g = N_g \times \hat{P}_{KM,g}(t)$.
- Greenwood variance from the group-specific Kaplan-Meier estimate supplies the denominator of the grouped goodness-of-fit statistic. In plain language, this is the standard way to quantify how much uncertainty there is around the Kaplan-Meier estimate in each group.

Operationally, the reported statistic is computed on the count scale as:

$$
\chi^2_{GND} = \sum_{g=1}^{G} \frac{(O_g - E_g)^2}{N_g^2 \cdot \widehat{\mathrm{Var}}(\hat{P}_{KM,g}(t))}
$$

with a $\chi^2$ reference distribution using $G-1$ degrees of freedom.

Interpretation of the grouped chi-square result:
- A small p-value for this statistic, conventionally $p < 0.05$, is evidence of grouped miscalibration: the observed event experience differs from the model-predicted event experience across the risk groups more than would usually be expected by chance alone.
- A non-small p-value does not prove perfect calibration. It means the grouped test did not detect a clear mismatch at the available sample size and event count.
- The p-value should therefore be interpreted alongside `N`, ICI, and calibration slope rather than in isolation, especially in sparse horizons where power is limited.

This is now a true censoring-aware grouped survival-calibration test rather than the earlier $(O-E)^2 / E$ approximation. In plain language, the test now uses survival-analysis machinery that respects censoring instead of treating the data like a simple fully observed binary outcome.

#### MSS standard-validation calibration implementation

For MSS standard validation, the grouped calibration p-value now uses the same Greenwood-based grouped survival approach:
- Predicted melanoma-specific death risk is grouped into quantiles.
- Group-specific observed risk is estimated by Kaplan-Meier at the evaluation horizon while treating non-melanoma deaths as censored in the standard MSS analysis. In plain language, the method estimates melanoma-specific death risk over time without counting other causes of death as melanoma events.
- Greenwood variance is used in the grouped goodness-of-fit denominator. In plain language, the denominator is scaled by how uncertain the observed group risk estimate is.

This is a true grouped survival-calibration statistic for both MFS and standard MSS calibration summaries, even though the standard MSS endpoint itself remains distinct from the separate competing-risk MSS analyses. In plain language, the same rigorous grouped calibration idea is used in both places, but the clinical endpoint being studied is still different.

### Integrated Calibration Index (ICI)

The current pipeline uses a censoring-aware horizon-specific ICI strategy with an explicit fallback rule.

For MFS:
- When the effective risk support at the evaluation horizon is rich enough, the reported ICI is computed from an IPCW-weighted logistic spline recalibration curve on the logit-transformed predicted risk. In plain language, the model draws a smooth observed-versus-predicted calibration curve at that timepoint while correcting for unequal follow-up.
- When the effective risk support is too coarse for a stable smooth curve, the method falls back to the grouped Kaplan-Meier absolute calibration error already used for the Greenwood-based grouped calibration summary. In plain language, if there are too few distinct risk values to justify a smooth curve, the pipeline switches to a simpler grouped comparison.

For MSS standard validation:
- The same rule is used: preferred IPCW-smoothed ICI when the horizon-specific predicted risks are sufficiently granular, grouped Kaplan-Meier fallback when they are not.

What this means operationally:

#### IPCW-smoothed ICI path

IPCW stands for inverse-probability-of-censoring weighting. In plain language, it gives extra weight to patients whose follow-up pattern makes them stand in for similar patients who were censored too early to contribute full horizon information.

The problem it solves is that, at a fixed horizon such as 7 years, not every patient has a directly observed event/no-event status:
- a patient who has the event before 7 years is known to be an event,
- a patient followed beyond 7 years without the event is known to be a non-event at 7 years,
- but a patient censored at 4 years has unknown 7-year status.

If those early-censored patients were simply dropped without adjustment, the observed calibration curve could be biased toward the subset with longer follow-up. IPCW addresses this by upweighting patients whose 7-year status is known but whose follow-up pattern is relatively uncommon because of censoring. In plain language, it tries to reduce the bias that would happen if only the best-followed patients shaped the curve.

In the current implementation:
- the pipeline estimates the probability of remaining uncensored up to the relevant contribution time,
- assigns each horizon-known patient a weight of approximately $1 / P(\text{not censored})$,
- fits a weighted logistic spline recalibration curve of the horizon event indicator on the logit-transformed predicted risk, which means it estimates a smooth observed-risk curve from the model’s predicted risks on a scale that behaves better statistically near 0 and 1,
- and computes the ICI as the weighted mean absolute difference between each patient’s predicted risk and the smooth recalibrated observed risk from that curve. In plain language, ICI is the average size of the prediction error after accounting for censoring.

Written schematically, the preferred IPCW-smoothed ICI is:

$$
\mathrm{ICI}_{\mathrm{IPCW}} = \frac{\sum_{i=1}^{N} w_i\,|\hat{p}_i - \hat{o}_i|}{\sum_{i=1}^{N} w_i}
$$

where $w_i$ is the inverse-probability-of-censoring weight for patient $i$, $\hat{p}_i$ is the predicted event risk at the horizon, and $\hat{o}_i$ is the smooth recalibrated observed risk from the IPCW logistic spline curve.

Toy example:
- Suppose the horizon is 7 years.
- Patient A has predicted risk $0.20$ and is followed to 9 years without the event, so A is a known 7-year non-event.
- Patient B has predicted risk $0.20$ but is censored at 4 years, so B has unknown 7-year status and does not directly enter the horizon outcome fit.
- If patients like A have only a $0.60$ probability of remaining uncensored long enough to contribute known 7-year status, A receives weight about $1/0.60 = 1.67$.
- The smooth weighted calibration curve might then estimate that patients around predicted risk $0.20$ have observed 7-year risk $0.27$.
- A contributes roughly $1.67 \times |0.20 - 0.27|$ to the weighted ICI calculation.

This is the preferred ICI because it is censoring-aware and produces a smooth calibration function rather than a grouped step function. In plain language, it makes fuller use of the data when the data are rich enough to support that extra detail.

#### Grouped Kaplan-Meier fallback path

The grouped Kaplan-Meier fallback is used when the horizon-specific predicted risks are too discrete to support a stable smooth IPCW curve. In plain language, this happens when the model effectively gives only a few repeated risk values, so a smooth curve would look more precise than the data justify.

In practice, this happens when many patients share only a few distinct risk values at that horizon after filtering to the subset with enough information to contribute. In the current implementation, the smooth IPCW ICI is attempted only when the horizon-known subset has:
- at least 20 analyzable patients,
- at least 5 events,
- at least 5 non-events,
- and at least 10 distinct predicted-risk values.

If those conditions are not met, the pipeline falls back to the grouped Kaplan-Meier ICI:
- patients are placed into predicted-risk groups,
- observed risk within each group is estimated by Kaplan-Meier at the evaluation horizon,
- each patient is assigned that group-level observed risk,
- and the ICI is the mean absolute difference between the patient’s predicted risk and that grouped Kaplan-Meier observed risk.

Written schematically, the grouped fallback ICI is:

$$
\mathrm{ICI}_{\mathrm{grouped}} = \frac{1}{N} \sum_{i=1}^{N} |\hat{p}_i - \hat{P}_{KM,g(i)}(t)|
$$

where $g(i)$ is the patient's assigned predicted-risk group and $\hat{P}_{KM,g(i)}(t)$ is the Kaplan-Meier observed event risk for that group at horizon $t$.

Toy example:
- Suppose the only distinct 7-year predicted risks in the usable dataset are $0.05$, $0.20$, and $0.60$.
- A smooth spline on only three effective risk levels would be unstable and over-interpretable.
- The pipeline instead forms groups and estimates the observed 7-year event risk in each group by Kaplan-Meier.
- If the group centered around predicted risk $0.20$ has Kaplan-Meier observed risk $0.28$, then each patient in that group contributes $|0.20 - 0.28| = 0.08$ to the ICI before averaging.

This fallback is more conservative than pretending a smooth curve is identifiable when the risk support is too coarse. In plain language, the pipeline intentionally chooses the simpler method rather than over-claiming precision.

In the current data, many horizon-specific GEP predictions collapse to only a few distinct values after filtering to patients who actually contribute information at that horizon, so the grouped-Kaplan-Meier fallback is often the method that is ultimately reported. This is why the workbook now exposes `ICI_Method` explicitly rather than assuming one estimator is used everywhere.

For sparse cohorts or sparse horizons, the summary-writing behavior is deliberate:
- the workbook is still written even if a smooth ICI is not supportable,
- the reported ICI falls back to the grouped Kaplan-Meier version,
- and the method column records that fallback explicitly rather than silently mixing estimators.

Lower ICI values still indicate better calibration regardless of method, but interpretation should always cite the method column when comparing cohorts or timepoints.

### Calibration Slope

The calibration slope is now computed with a single censoring-aware method for both MFS and standard MSS.

For both MFS and MSS standard validation:
- The predicted event risk at the requested horizon is transformed to the logit scale. In plain language, the probabilities are converted to a scale that is easier to model reliably, especially near 0 and 1.
- Patients with known horizon status contribute through inverse-probability-of-censoring weights (IPCW), so early censoring does not get treated as an observed non-event.
- A weighted logistic recalibration model is then fit at that horizon, and the coefficient of the transformed predicted risk is reported as the calibration slope. In plain language, this checks whether high-risk patients are truly experiencing more events than low-risk patients by about the right amount.
- The calibration intercept is estimated from the corresponding IPCW-weighted offset model and stored in the result object, although the main workbook still foregrounds the slope. In plain language, the intercept captures whether predictions are systematically too high or too low overall.
- If that weighted slope fit is numerically unstable, such as under quasi-separation with very large coefficient uncertainty, the slope is withheld and the method column reports the fit as unavailable rather than publishing a spurious extreme estimate. In plain language, if the math is too unstable to trust the number, the pipeline leaves it blank instead of reporting a misleading value.

The recalibration model can be written schematically as:

$$
\operatorname{logit}\{P(Y_t = 1)\} = \alpha + \beta\,\operatorname{logit}(\hat{p}_t)
$$

where $Y_t$ is the horizon-specific event indicator, $\hat{p}_t$ is the model-predicted event risk at that horizon, $\alpha$ is the calibration intercept, and the reported calibration slope is $\beta$.

Operationally, the slope is treated as unavailable when the recalibration fit fails minimum-support checks or crosses the instability thresholds currently encoded in the pipeline constants. At present this includes sparse event/non-event support and quasi-separated fits with implausibly large coefficient magnitude or standard error. In plain language, quasi-separation means the data are so thin or so cleanly split that the model tries to send the slope toward an unrealistically extreme value. In those cases:
- `Slope` is written as missing,
- `Slope_Method` is written as `ipcw_logit_unavailable`,
- and the rest of the summary is still emitted so the horizon remains reviewable rather than disappearing from the workbook.

The intercept may remain estimable even when the slope is withheld. That is intentional: the offset-only IPCW intercept fit can be numerically acceptable in settings where the free slope fit is not.

Interpretation remains standard:
- Slope near 1.0 suggests well-scaled predictions.
- Slope below 1.0 suggests predictions are too extreme.
- Slope above 1.0 suggests predictions are too compressed.


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
- Record results in the consolidated workbook

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
