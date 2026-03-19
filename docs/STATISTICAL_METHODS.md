# Statistical Methods

This document is the canonical statistical-methods reference for the repository. It defines the model families, assumptions, thresholds, and validation metrics used throughout the analysis pipeline.

For output-reading guidance, use [INTERPRETATION_GUIDE.md](INTERPRETATION_GUIDE.md). For derived-variable definitions and endpoint construction, use [CALCULATIONS.md](CALCULATIONS.md).

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

For practical reading of survival curves, see [Kaplan-Meier Plots](INTERPRETATION_GUIDE.md#kaplan-meier-plots).

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

For practical reading of hazard ratios and confidence intervals, see [Cox Regression Tables (Survival Outcomes)](INTERPRETATION_GUIDE.md#cox-regression-tables-survival-outcomes).

### Log-Rank Test

**Purpose:** Compare survival distributions between groups

**Implementation:** Built into `survival::survfit()`

**Interpretation:**
- p < 0.05 indicates significantly different survival distributions
- Non-parametric test (no distributional assumptions)
- Sensitive to differences across entire follow-up period

For practical reading of survival-curve separation and log-rank context, see [Kaplan-Meier Plots](INTERPRETATION_GUIDE.md#kaplan-meier-plots).

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

For plain-English interpretation of RMST outputs, see [Understanding RMST Analysis](INTERPRETATION_GUIDE.md#understanding-rmst-analysis) and [Comparing RMST to Cox HR](INTERPRETATION_GUIDE.md#comparing-rmst-to-cox-hr).

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

Model feasibility thresholds are kept separate by analysis family: adjusted adverse-event logistic regression uses a 10-event minimum, PFS-2 survival modeling uses a 10-patient precheck and a 5-event minimum, and PH diagnostics follow a 10-event reporting floor.

### Automation

The pipeline automatically:
1. Tests PH assumption for all Cox models with ≥10 events
2. Generates diagnostic plots for each covariate
3. Documents violations in Excel workbooks
4. Provides RMST as alternative when PH violated
5. Stores diagnostics in `{cohort}/{objective}/h_proportional_hazards_diagnostics/`

**Minimum Event Threshold:** PH testing skipped when <10 total events (insufficient power for reliable testing)

For practical reading of PH tests and Schoenfeld plots, see [Interpreting Proportional Hazards Diagnostics](INTERPRETATION_GUIDE.md#interpreting-proportional-hazards-diagnostics) and [Schoenfeld Residual Plots](INTERPRETATION_GUIDE.md#schoenfeld-residual-plots).

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

For practical reading of cumulative-incidence outputs, see [Cumulative Incidence Plots (Competing Risks)](INTERPRETATION_GUIDE.md#cumulative-incidence-plots-competing-risks).

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

For practical reading of odds ratios and logistic-regression outputs, see [Logistic Regression Tables (Binary Outcomes)](INTERPRETATION_GUIDE.md#logistic-regression-tables-binary-outcomes).

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
| **Vision Change (logMAR)** | Unadjusted | Baseline-adjusted linear regression | Objective 2a |
| **Snellen Line Change (exact integer lines)** | Descriptive converted summary row | Adjusted linear regression | Objective 2a |

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

For practical reading of linear-regression outputs, see [Linear Regression Tables (Continuous Outcomes)](INTERPRETATION_GUIDE.md#linear-regression-tables-continuous-outcomes).

---

## Ordinal Outcomes

### Ordinal Logistic Regression

**Purpose:** Compare ordered categorical outcomes between treatment groups while preserving clinical rank order.

**Implementation:** `MASS::polr()`

**Model Formula:**

```r
ordered_outcome ~ treatment + confounders
```

**Outputs:**
- Odds ratios for a shift toward earlier ordered categories
- 95% Wald CI
- Overall treatment/variable p-values from likelihood ratio tests
- Diagnostics workbook with sample-size and outcome-level summaries

### Applications

| Outcome | Ordered Levels | Location |
|---------|----------------|----------|
| **Snellen Line Change Distribution** | `≥3-line improvement` to `≥3-line loss` | Objective 2a |

### Interpretation

**Snellen distribution OR = 1.40**
- The comparison treatment has higher odds of falling into a better Snellen Line Change Distribution category
- Because categories are ordered from improvement to loss, OR > 1 favors better vision outcomes and OR < 1 favors worse outcomes

### Assumptions

- Ordered outcome categories are clinically meaningful
- Proportional odds assumption is acceptable
- Independent observations
- Adequate observations across outcome levels

For practical reading of ordered-outcome model tables, see [Ordinal Regression Tables (Ordered Outcomes)](INTERPRETATION_GUIDE.md#ordinal-regression-tables-ordered-outcomes).

For Objective 2 vision, ordinal modeling is reserved for the 7-level `Snellen Line Change Distribution`. The exact integer `Snellen Line Change` outcome is modeled with adjusted linear regression, and a full exact-integer ordinal model is not used because the observed line-count support is very wide and sparse across cohorts. Reader-facing output files therefore separate `Snellen Line Change` from `Snellen Line Change Distribution`, and each Objective 2 subfolder now also includes a flat `*_effect_summary.xlsx` workbook that combines descriptive, unadjusted, and adjusted rows in one sheet.

For consistency across the ordinal HTML tables and the effect-summary workbooks, ordinal (`polr`) treatment effects are reported as proportional-odds ORs with 95% Wald confidence intervals and likelihood-ratio-test p-values. Other model families retain their standard reporting conventions: linear models report mean differences with Wald CIs/p-values, logistic models report odds ratios with model-based Wald CIs and the pipeline's standard term-level p-values, and Cox models report hazard ratios with the native Cox confidence intervals and Cox-model p-values.

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
- **p < 0.05:** Evidence of interaction (treatment effect may vary by subgroup)
- **p ≥ 0.05:** No strong evidence of interaction (overall treatment effect is the safer summary)

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

**Possible Interaction (p < 0.05):**
- Treatment effect genuinely differs across subgroups
- Clinically meaningful heterogeneity
- Consider subgroup-specific treatment recommendations

**No Strong Interaction Signal (p ≥ 0.05):**
- No strong evidence for differential effects
- Apply overall treatment effect across subgroups
- Observed differences likely due to chance

**Caution:** Subgroup analyses are exploratory and hypothesis-generating. Confirm findings in independent datasets before clinical application.

For practical reading of subgroup outputs, see [Reading Forest Plots](INTERPRETATION_GUIDE.md#reading-forest-plots).

---

## GEP Validation Metrics

Gene Expression Profiling (GEP) provides lab-reported probabilities of metastasis-free survival (MFS) and melanoma-specific survival (MSS). Objective 4 validates these predictions against observed outcomes.

For workbook-first reading order and plain-language interpretation, start with [Understanding GEP Analysis](INTERPRETATION_GUIDE.md#understanding-gep-analysis).

### Validation Framework

**Goal:** Assess whether lab-reported GEP survival probabilities accurately predict patient outcomes

In Objective 4, the starting predictions are externally supplied patient-level GEP survival probabilities that are already present in the analytic dataset: `biopsy1_gep_mfs` for metastasis-free survival and `biopsy1_gep_mss` for melanoma-specific survival. The pipeline copies those lab-reported 5-year survival values into the 5-year `expected_*` columns, then derives the 7- and 10-year survival values from the same 5-year probabilities during preprocessing using an exponential-decay extrapolation: $S(7) = S(5)^{7/5}$ and $S(10) = S(5)^{10/5}$. It then converts survival to event risk as $1 - S(t)$ whenever a validation metric needs predicted event probability rather than predicted survival. Objective 4 therefore validates imported GEP predictions; it does not fit a new prognostic model to generate the base GEP probabilities.

The analyzable Objective 4 subset is narrower than “any row with a GEP-related field.” MFS and MSS validation require a definitive raw DecisionDx label, valid endpoint-specific imported GEP probabilities, and the required observed outcome fields. Definitive raw labels are `Class_1A_PRAME_negative`, `Class_1A_PRAME_positive`, `Class_1B_PRAME_negative`, `Class_1B_PRAME_positive`, `Class_2_PRAME_negative`, and `Class_2_PRAME_positive`. Nondefinitive labels such as `*_not_reported`, `Class_2_PRAME_Unknown`, `Class_1A_PRAME_discordant`, `Failed`, `Unknown`, `Other`, and `No` are excluded from `mfs_analysis_eligible` and `mss_analysis_eligible`. Objective 4 entry points refresh these flags before analysis so the definitive-label rule is applied consistently.

#### Training/testing split generation and rationale

Objective 0 creates the `gep_validation_set` variable once during preprocessing on the full cohort. Patients are eligible for this split only when they have analyzable imported GEP data for the validation workflow: non-missing `biopsy1_gep_mfs`, non-missing `biopsy1_gep_mss`, and a definitive simplified GEP class in `GEP_DEFINITIVE_SIMPLE_LEVELS` (`Class 1` or `Class 2`). Everyone else is labeled `No GEP Data`.

The current implementation does not perform independent Bernoulli sampling row by row. Instead, it identifies the full set of eligible rows, calculates `n_training = round(n_eligible * 0.7)`, assigns the remaining eligible rows to `Testing`, builds a vector with those exact counts, shuffles that vector, and writes the shuffled labels back to the eligible rows. The result is therefore an approximately 70/30 partition with deterministic counts for a given eligible sample size, rather than a purely probabilistic split with wider run-to-run variation in the realized proportion.

This split exists to support Objective 4 internal validation summaries and quality-control checks. The imported GEP predictions are already fixed before the analytic pipeline begins, so the split is not used to train a new molecular model. Instead, it provides a stable internal partition for reader-facing training/testing comparisons and for validation outputs that should not silently pool all analyzable GEP rows into a single undifferentiated set.

#### Why Objective 0 treats split failure as fatal

Objective 0 is the only stage that creates and validates `gep_validation_set`, so this is the correct place to fail fast if the full-cohort partition is malformed. A broken split at this stage means the processed analytic datasets no longer satisfy a core preprocessing contract for Objective 4. If the full cohort has analyzable GEP rows but no `Training` rows, no `Testing` rows, an implausible training proportion, or inconsistent counts between `Training`, `Testing`, and `No GEP Data`, downstream Objective 4 summaries become misleading rather than merely incomplete. For that reason, Objective 0 now treats full-cohort split-shape failures as fatal and stops the run before downstream objectives execute.

In practice, the full-cohort split is expected to satisfy broad sanity conditions rather than an exact 70.0/30.0 ratio. The validation checks require count consistency and an approximately 70/30 partition in the full cohort, with a wide tolerance intended to catch corruption or derivation failure rather than ordinary rounding.

#### Permissible subset exceptions, including GKSRS

The `uveal_melanoma_restricted_cohort` and `uveal_melanoma_gksrs_only_cohort` are not assigned new training/testing labels. They inherit the labels created once in the full cohort and then apply additional inclusion filters. Because these are deterministic subsets of the original partition, their apparent training/testing ratio can change substantially after subsetting, and some subsets may even lose one split entirely if the subset is small or clinically selective enough.

This behavior is permissible because the preprocessing contract is defined at the full-cohort level, where the split is generated. Subset cohorts only need to remain internally count-consistent with the inherited labels; they are not required to recreate an approximately 70/30 split. The GKSRS cohort is the clearest example: it is a treatment-defined subset of the full cohort, so enforcing the original split proportion after subsetting would be statistically inappropriate and would generate false validation failures. Objective 0 therefore enforces split-shape rules only on the full cohort while still checking that subset-cohort labels remain structurally consistent.

**Analyses:**
1. **Observed vs Expected / Calibration:** Agreement between lab-reported and realized event rates
2. **Discrimination:** Ability to separate patients with vs without events
3. **Clinical Utility:** Impact on clinical decision-making
4. **Competing-risk MSS sensitivity analysis:** Separate accounting for non-melanoma death when evaluating MSS

**Role of the downstream methods:**
- Kaplan-Meier summaries in MFS and standard MSS grouped calibration estimate observed outcome risk from follow-up data; they are not the source of the GEP prediction.
- Companion competing-risk MSS analyses use cumulative incidence functions to estimate observed melanoma-specific death risk when non-melanoma death is handled explicitly as a competing event.
- IPCW-weighted recalibration models, grouped calibration statistics, discrimination metrics, and decision-curve analysis all evaluate how well the supplied GEP predictions performed.

For a workbook-first overview written for non-statistical readers, see [Understanding GEP Analysis](INTERPRETATION_GUIDE.md#understanding-gep-analysis) and [GEP Quick Read](INTERPRETATION_GUIDE.md#gep-quick-read).

### Calibration Assessment

**Purpose:** Do GEP-predicted event probabilities, derived from lab-reported survival probabilities, match observed event rates?

**Method:** Table-first validation at 5, 7, and 10 years using grouped observed-vs-expected comparisons plus calibration diagnostics.

**Metrics:**
- **Overall O/E ratio:** Total observed events divided by total expected events across GEP classes
- **Exact Poisson CI for overall O/E:** Quantifies uncertainty around the overall observed-to-expected ratio
- **Pearson goodness-of-fit p-value:** Tests whether grouped observed counts depart materially from grouped expected counts across GEP classes
- **Nam-D'Agostino statistic:** Grouped calibration test reported in the consolidated workbook
- **Integrated Calibration Index (ICI):** Average absolute difference between predicted and observed risk
- **Calibration slope:** Should be close to 1.0; reported as the primary slope summary across timepoints
- **Brier score:** Horizon-specific mean squared error between predicted event risk and observed horizon outcome, carried in the workbook as a compact overall accuracy summary

**Workbook traceability:**
- The `Calibration_Summary` and `Calibration_Comparison` sheets now also carry method columns so the statistical variant used for each horizon is explicit: `Nam_D_Agostino_Method`, `ICI_Method`, and `Slope_Method`.

**Endpoint note:**
- MFS uses metastasis events.
- MSS standard validation uses melanoma-specific death as the event.
- Non-melanoma death is handled separately in competing-risk analyses rather than being treated as an MSS event.

### How Expected Counts Are Calculated

For Objective 4, the expected event count at time $t$ is derived from the patient-level GEP survival probability carried into the analytic dataset. At 5 years this comes directly from the lab-reported value. At 7 and 10 years, the current pipeline does not read separate source columns; instead it extrapolates from the imported 5-year survival using $S(7) = S(5)^{7/5}$ and $S(10) = S(5)^{10/5}$, implemented as `biopsy1_gep_mfs^(7/5)` / `biopsy1_gep_mfs^(10/5)` for MFS and `biopsy1_gep_mss^(7/5)` / `biopsy1_gep_mss^(10/5)` for MSS. If patient $i$ has predicted survival $S_i(t)$, then the predicted event probability is:

$$
\hat{p}_i(t) = 1 - S_i(t)
$$

The expected number of events in a cohort or subgroup is:

$$
E(t) = \sum_{i=1}^{N} \hat{p}_i(t) = \sum_{i=1}^{N} \left(1 - S_i(t)\right)
$$

This is the quantity reported as `Expected` in the `Observed_Expected_Summary` sheet.

Implementation notes:
- In the shared MSS calculator, expected counts are computed directly as `sum(1 - expected_survival)` within each GEP class.
- In the MFS helper, the same quantity is computed algebraically as $N \times (1 - \bar{S}(t))$, which is equivalent to summing $1 - S_i(t)$ across patients.

### How Observed Counts Are Calculated

The current pipeline uses timepoint-specific binary event indicators created during preprocessing. For a given landmark year, the observed count is the sum of patients with the corresponding event indicator equal to 1.

Examples:
- MFS uses `mfs_event_5yr`, `mfs_event_7yr`, and `mfs_event_10yr`
- MSS uses `mss_event_5yr`, `mss_event_7yr`, and `mss_event_10yr`

Accordingly,

$$
O(t) = \sum_{i=1}^{N} I\{\text{event by time } t\}
$$

where $I\{\cdot\}$ is the indicator function.

Sheet distinction:
- The `Observed_Expected_Summary` sheet still reflects the direct timepoint event-count calculation described above.
- The `Observed_Expected_Summary` sheet reports its count-based goodness-of-fit p-value as `OE_Chi_Square_p`.
- The `Calibration_Summary` sheet reports its grouped survival-calibration p-value as `Nam_D_Agostino_p` and uses grouped Kaplan-Meier estimates with Greenwood variance for that field.

These are not interchangeable quantities:
- `OE_Chi_Square_p` is the overall observed-versus-expected count-comparison p-value attached to the calibration-in-the-large summary.
- `Nam_D_Agostino_p` is the grouped Greenwood/Nam-D'Agostino survival-calibration p-value attached to the grouped calibration summary.

### Overall O/E Ratio and Exact Poisson Confidence Interval

The overall calibration-in-the-large metric is the observed-to-expected ratio:

$$
\text{O/E} = \frac{O}{E}
$$

where $O$ is the total observed event count and $E$ is the total expected event count across GEP classes.

The workbook reports an exact Poisson confidence interval for this ratio by treating the observed count as Poisson and scaling the resulting interval by the fixed expected count. In practice, the pipeline uses `stats::poisson.test()` on $O$ and then divides the lower and upper confidence limits by $E$.

The same `Observed_Expected_Summary` sheet also reports `OE_Chi_Square_p`, which is the count-based goodness-of-fit p-value for the overall O/E comparison rather than the grouped Greenwood/Nam-D'Agostino statistic.

#### A Note About Denominator Retention and Summary Contracts

Because the Objective 4 workbooks are the primary review artifacts, denominator fields need to remain stable across summary layers.

- In `Observed_Expected_Summary`, the overall `N` field is the total number of evaluable patients contributing to that horizon-specific O/E calculation, not a group-specific subgroup count.
- For MFS, that overall denominator is carried through explicitly from the O/E helper so the consolidated workbook does not lose the cohort-level denominator when it collapses class-specific results into a single overall row.
- If an upstream result shape does not expose that denominator directly, the reporting helper reconstructs it from the class-level counts rather than leaving the workbook denominator blank.
- The grouped calibration table is a separate object from the overall O/E summary. Its `N` column refers to the number of patients entering the calibration analysis at that horizon, whereas the grouped Greenwood statistic itself is then computed within risk groups inside that analysis set.

These denominator rules prevent blank or ambiguous `N` fields in sparse horizons.

### Grouped Calibration and Goodness-of-Fit Fields

The workbook contains both an overall O/E summary and a separate calibration summary. Those fields are generated differently.

#### MFS calibration implementation

For MFS, grouped calibration uses a Greenwood-Nam-D'Agostino-style survival-calibration statistic:
- Predicted risks are grouped into quantiles with a target of up to 10 groups and at least 3 groups.
- Within each group, expected events are calculated as the sum of predicted risks.
- Observed event risk at the evaluation horizon is estimated with Kaplan-Meier within that risk group.
- Observed events are then expressed on the count scale as $O_g = N_g \times \hat{P}_{KM,g}(t)$.
- Greenwood variance from the group-specific Kaplan-Meier estimate supplies the denominator of the grouped goodness-of-fit statistic.

Operationally, the reported statistic is computed on the count scale as:

$$
\chi^2_{GND} = \sum_{g=1}^{G} \frac{(O_g - E_g)^2}{N_g^2 \cdot \widehat{\mathrm{Var}}(\hat{P}_{KM,g}(t))}
$$

with a $\chi^2$ reference distribution using $G-1$ degrees of freedom.

**Practical interpretation:** Smaller `Nam_D_Agostino_p` values suggest stronger evidence of grouped miscalibration. This means that when we divide the subjects into groups (e.g., deciles of predicted risk), the supplied GEP-based predicted risk does not match the actual observed risk in those groups. 

For a plain-English reading order for calibration outputs, see [GEP Calibration Made Simple](INTERPRETATION_GUIDE.md#gep-calibration-made-simple).

#### MSS standard-validation calibration implementation

For MSS standard validation, the grouped calibration p-value uses the same Greenwood-based grouped survival approach:
- Predicted melanoma-specific death risk is grouped into quantiles.
- Group-specific observed risk is estimated by Kaplan-Meier at the evaluation horizon while treating non-melanoma deaths as censored in the standard MSS analysis.
- Greenwood variance is used in the grouped goodness-of-fit denominator.

This grouped survival-calibration statistic is used for both MFS and standard MSS calibration summaries, while the competing-risk MSS analyses remain separate.

### Integrated Calibration Index (ICI)

The current pipeline uses a censoring-aware horizon-specific ICI strategy with an explicit fallback rule.

For MFS:
- When the effective risk support at the evaluation horizon is rich enough, the reported ICI is computed from an IPCW-weighted logistic spline recalibration curve on the logit-transformed predicted risk.
- When the effective risk support is too coarse for a stable smooth curve, the method falls back to the grouped Kaplan-Meier absolute calibration error already used for the Greenwood-based grouped calibration summary.

For MSS standard validation:
- The same rule is used: preferred IPCW-smoothed ICI when the horizon-specific predicted risks are sufficiently granular, grouped Kaplan-Meier fallback when they are not.

#### IPCW-smoothed ICI path

IPCW stands for inverse-probability-of-censoring weighting. It upweights patients whose horizon status is observed in settings where censoring would otherwise thin the usable sample.

In the current implementation:
- the pipeline estimates the probability of remaining uncensored up to the relevant contribution time,
- assigns each horizon-known patient a weight of approximately $1 / P(\text{not censored})$,
- fits a weighted logistic spline recalibration curve of the horizon event indicator on the logit-transformed predicted risk,
- and computes the ICI as the weighted mean absolute difference between each patient’s predicted risk and the smooth recalibrated observed risk from that curve.

Written schematically, the preferred IPCW-smoothed ICI is:

$$
\mathrm{ICI}_{\mathrm{IPCW}} = \frac{\sum_{i=1}^{N} w_i\,|\hat{p}_i - \hat{o}_i|}{\sum_{i=1}^{N} w_i}
$$

where $w_i$ is the inverse-probability-of-censoring weight for patient $i$, $\hat{p}_i$ is the predicted event risk at the horizon, and $\hat{o}_i$ is the smooth recalibrated observed risk from the IPCW logistic spline curve.

#### Grouped Kaplan-Meier fallback path

The grouped Kaplan-Meier fallback is used when the horizon-specific predicted risks are too discrete to support a stable smooth IPCW curve. In the current implementation, the smooth IPCW ICI is attempted only when the horizon-known subset has:
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

For sparse cohorts or sparse horizons, the summary-writing behavior is deliberate:
- the workbook is still written even if a smooth ICI is not supportable,
- the reported ICI falls back to the grouped Kaplan-Meier version,
- and the method column records that fallback explicitly rather than silently mixing estimators.

**Practical interpretation:** Lower `ICI` is better, but comparisons should cite `ICI_Method`.

### Calibration Slope

The calibration slope is now computed with a single censoring-aware method for both MFS and standard MSS.

For both MFS and MSS standard validation:
- The predicted event risk at the requested horizon is transformed to the logit scale.
- Patients with known horizon status contribute through inverse-probability-of-censoring weights (IPCW), so early censoring does not get treated as an observed non-event.
- A weighted logistic recalibration model is then fit at that horizon, and the coefficient of the transformed predicted risk is reported as the calibration slope.
- The calibration intercept is estimated from the corresponding IPCW-weighted offset model and stored in the result object, although the main workbook still foregrounds the slope.
- If that weighted slope fit is numerically unstable, such as under quasi-separation with very large coefficient uncertainty, the slope is withheld and the method column reports the fit as unavailable rather than publishing a spurious extreme estimate.

The recalibration model can be written schematically as:

$$
\operatorname{logit}\{P(Y_t = 1)\} = \alpha + \beta\,\operatorname{logit}(\hat{p}_t)
$$

where $Y_t$ is the horizon-specific event indicator, $\hat{p}_t$ is the model-predicted event risk at that horizon, $\alpha$ is the calibration intercept, and the reported calibration slope is $\beta$.

Operationally, the slope is treated as unavailable when the recalibration fit fails minimum-support checks or crosses the instability thresholds currently encoded in the pipeline constants. At present this includes sparse event/non-event support and quasi-separated fits with implausibly large coefficient magnitude or standard error. In those cases:
- `Slope` is written as missing,
- `Slope_Method` is written as `ipcw_logit_unavailable`,
- and the rest of the summary is still emitted so the horizon remains reviewable rather than disappearing from the workbook.

The intercept may remain estimable even when the slope is withheld. That is intentional: the offset-only IPCW intercept fit can be numerically acceptable in settings where the free slope fit is not.

**Practical interpretation:** `Slope` near 1 is best; values below 1 suggest predictions are too extreme, and values above 1 suggest predictions are too compressed.

For a plain-English reading order for calibration outputs, see [GEP Calibration Made Simple](INTERPRETATION_GUIDE.md#gep-calibration-made-simple).

### Brier Score and `Brier_Method`

The Objective 4 workbook also reports a Brier score in `Calibration_Summary`. This is not a separate model-fitting procedure. It is an accuracy metric that summarizes how close the predicted event probabilities were to the observed horizon outcomes.

For patient $i$ at horizon $t$, let $\hat{p}_i(t)$ be the predicted event risk and let $Y_i(t)$ be the binary indicator for whether the event had occurred by that horizon. The current pipeline's preferred Brier calculation is:

$$
\mathrm{Brier}(t) = \frac{1}{N} \sum_{i=1}^{N} \left(\hat{p}_i(t) - Y_i(t)\right)^2
$$

where lower values indicate better overall probabilistic accuracy. A value of $0$ would be perfect. Because the Brier score depends on outcome frequency, it is more meaningful for like-for-like comparisons within the same endpoint and timepoint than as an absolute universal threshold.

Current implementation details:
- `Brier_Method = time_dependent`: preferred horizon-specific squared-error calculation.
- `Brier_Method = simple_fallback`: simplified fallback using the same mean-squared-error idea if the preferred path errors.
- `Brier_Method = basic_last_resort`: rough aggregate approximation based on the mean predicted risk and overall event rate if both earlier paths fail.
- `Brier_Method = insufficient_data`, `calculation_failed`, or `all_methods_failed`: the score was unavailable or too degraded for normal interpretation.
- `Brier_Fallback_Used = TRUE`: the reported value did not come from the preferred path and should be described as a fallback estimate.

In this pipeline, the Brier score is a secondary calibration/accuracy check. The primary calibration interpretation should still rest on the grouped Greenwood Nam-D'Agostino result, the ICI, and the calibration slope.


### Discrimination Assessment

**Purpose:** Can GEP separate patients with vs without events?

These fields are written to the `Discrimination_Summary` sheet of the consolidated Objective 4 workbooks.

### Harrell's C (`Harrell_C`)

**Package / function:** `survcomp::concordance.index()` with `method = "noether"`
- The function evaluates a model's ability to order subjects by their risk. For a given pair of individuals, they are considered "concordant" if their predicted risk matches their actual event times in the correct order. The C-index is essentially the frequency of such concordant pairs among all usable (comparable) pairs.

**MFS implementation:**
- In `perform_discrimination_mfs()`, the code first truncates follow-up at the requested horizon.
- The event indicator becomes `observed_event == 1 & observed_time <= timepoint_months`.
- The survival time becomes `pmin(observed_time, timepoint_months)`.
- `Harrell_C` is then computed on those horizon-truncated data, so the MFS value is a horizon-specific concordance estimate.

**MSS implementation:**
- In `perform_discrimination_mss()`, the primary `Harrell_C` uses the same `survcomp::concordance.index()` call but on full observed follow-up in the horizon-specific analytic subset.
- The code passes `time_to_event` and `event_occurred` directly rather than truncating the primary concordance calculation at the landmark year.
- This means the MSS `Harrell_C` is not the same estimand as the MFS `Harrell_C`, even though both appear in the same workbook column.

**Fallback:** If `survcomp` fails, the code falls back to concordance from `survival::coxph()`.

**Practical interpretation:** Higher `Harrell_C` means better rank-order separation of higher-risk versus lower-risk patients.

For a plain-English reading order for discrimination outputs, see [GEP Discrimination Made Simple](INTERPRETATION_GUIDE.md#gep-discrimination-made-simple).

### Integrated and Time-Aggregated Discrimination

The pipeline intentionally removed Uno's C and single-timepoint time-dependent AUC because those metrics were too fragile for the current event pattern. The replacement discrimination fields are more stable summaries over follow-up.

**Integrated AUC (`Integrated_AUC`):**
- The code fits `coxph(Surv(observed_time, observed_event) ~ predicted_risk)`.
- It then calls `riskRegression::Score()` with monthly evaluation times: `seq(0, max(observed_time), by = 12)`.
- The reported integrated AUC is the mean of the returned AUC values across those time periods.

**Cumulative discrimination (`Cumulative_Discrimination`):**
- The code recomputes truncated Harrell-style concordance across prespecified 5-, 7-, and 10-year windows.
- The workbook value is the mean of the available window-specific concordance estimates.

**Time-averaged discrimination (`Time_averaged_Discrimination`):**
- The code recomputes truncated Harrell-style concordance at monthly follow-up landmarks.
- The workbook value is the mean of those monthly concordance estimates.

**Practical interpretation:** Higher `Integrated_AUC`, `Cumulative_Discrimination`, and `Time_averaged_Discrimination` indicate better average discrimination over follow-up.

For a plain-English reading order for discrimination outputs, see [GEP Discrimination Made Simple](INTERPRETATION_GUIDE.md#gep-discrimination-made-simple).

### Clinical Utility Assessment

**Purpose:** Does GEP improve clinical decisions beyond standard factors?

The current Objective 4 implementation reports clinical-utility metrics through the `Decision_Curve_Summary` and `PRAME_Summary` sheets, with the `IPA` field retained in `Discrimination_Summary` because it is generated alongside the other discrimination outputs.

### Decision Curve Analysis

**Implementation:** `perform_decision_curve_analysis_mfs()` and `perform_decision_curve_analysis_mss()`

**Predicted risk:**
- MFS uses `1 - expected_mfs_{timepoint}yr`.
- MSS uses `1 - expected_mss_{timepoint}yr`.

**Observed outcome:**
- The code constructs a binary outcome indicating whether the endpoint occurred by the requested horizon.

**Threshold grid:**
- Decision curves are evaluated on `seq(GEP_DCA_THRESHOLD_MIN, GEP_DCA_THRESHOLD_MAX, by = GEP_DCA_THRESHOLD_STEP)`.

**Net benefit formula:**

$$
NB(p_t) = \frac{TP}{N} - \frac{FP}{N} \cdot \frac{p_t}{1 - p_t}
$$

where $p_t$ is the decision threshold, $TP$ is the number of true positives, and $FP$ is the number of false positives under the threshold-based treatment rule.

**Workbook outputs:**
- `Event_Rate`
- `Optimal_Threshold`
- `Optimal_Net_Benefit`
- `Threshold_Range_Min` / `Threshold_Range_Max`
- `Area_Between_Curves`

`Area_Between_Curves` is currently computed as the summed difference between the model and treat-all net-benefit curves over the evaluated threshold grid, scaled by the fixed threshold step.

**Practical interpretation:** Positive net benefit means the model outperforms a treat-none strategy at that threshold; larger positive values indicate greater potential clinical utility.

For a plain-English reading order for decision-curve outputs, see [GEP Decision Curve Made Simple](INTERPRETATION_GUIDE.md#gep-decision-curve-made-simple).

### Index of Prediction Accuracy (`IPA`)

**Implementation:** `calculate_ipa_survival()`

The code uses a three-level fallback strategy:

**Preferred method:** Brier-score comparison at the requested horizon

$$
IPA = \frac{BS_{null} - BS_{model}}{BS_{null}}
$$

where the null model predicts the overall horizon event rate for everyone.

**Fallback 1:** AUC-based comparison

$$
IPA = \frac{AUC_{model} - 0.5}{1 - 0.5}
$$

using `pROC::roc()` when available, or a simpler correlation-based AUC surrogate if `pROC` is unavailable.

**Fallback 2:** Spearman-correlation shortcut

$$
IPA \approx \frac{\rho_{Spearman}}{2}
$$

This final fallback is only used if both earlier methods fail.

The workbook records the active path in `IPA_Method` and whether a fallback was needed in `IPA_Fallback_Used`.

**Practical interpretation:** Positive `IPA` means the model improves on the null benchmark at that horizon; values near zero indicate little gain.

For a plain-English reading order for discrimination outputs, see [GEP Discrimination Made Simple](INTERPRETATION_GUIDE.md#gep-discrimination-made-simple).

### PRAME Incremental Discrimination Metrics

**Implementation:** `perform_prame_augmented_analysis_mfs()` and `perform_prame_augmented_analysis_mss()`

PRAME comparison is optional and is only run on the PRAME-complete subset with binary `Positive` / `Negative` status and the required endpoint-specific GEP predictions.

At each timepoint, the pipeline fits two Cox models on the same analytic cohort:
- Base model: imported GEP risk only.
- Enhanced model: imported GEP risk plus PRAME status.

The primary PRAME question is whether the enhanced model improves discrimination beyond the imported GEP prediction already present in the dataset.

**Primary metric:**

$$
\Delta C = C_{GEP + PRAME} - C_{GEP\ only}
$$

- `Base_Harrell_C`: Harrell's C for the GEP-only model.
- `Enhanced_Harrell_C`: Harrell's C for the GEP-plus-PRAME model.
- `Delta_Harrell_C`: paired improvement in discrimination on the same patients.
- `Delta_CI_Lower` / `Delta_CI_Upper`: bootstrap percentile interval for $\Delta C$.

**Secondary support:**
- `LR_p`: likelihood-ratio p-value comparing the nested Cox models.
- `PRAME_HR` with confidence limits: PRAME hazard ratio from the enhanced model.
- `Analysis_Tier`: `Primary` for MFS and `Exploratory` for MSS.

**Estimand note:** the PRAME comparison inherits the main Objective 4 discrimination estimand for each outcome. MFS uses horizon-truncated follow-up at the requested landmark, whereas MSS uses full observed follow-up inside the horizon-specific analytic subset.

**Practical interpretation:** positive `Delta_Harrell_C` values favor the enhanced model, but the confidence interval and `LR_p` should be reviewed before calling the improvement convincing.

For a plain-English reading order for PRAME outputs, see [Understanding PRAME Incremental Outputs](INTERPRETATION_GUIDE.md#understanding-prame-incremental-outputs).

### Validation Workflow

**Step 1: Data Preparation**
- Load GEP predictions (lab-reported probabilities)
- Link to 5-, 7-, and 10-year survival outcomes (MFS, MSS)
- Exclude patients without definitive analyzable GEP labels or valid endpoint-specific GEP predictions
- For MSS, define the primary event as melanoma-specific death and retain competing death indicators for companion competing-risk analyses

**Step 2: Calibration Analysis**
- Compute grouped observed and expected events by GEP class
- Derive overall O/E ratios with exact Poisson confidence intervals
- Compute Pearson goodness-of-fit p-values across GEP classes
- Compute Nam–D’Agostino χ², Integrated Calibration Index (ICI), and calibration slope
- Record results in the consolidated workbook

**Step 3: Discrimination Analysis**
- Calculate `Harrell_C`, `Integrated_AUC`, `Cumulative_Discrimination`, and `Time_averaged_Discrimination`
- Use horizon-truncated concordance for MFS and full-follow-up primary concordance for MSS
- Store these fields together with `IPA`, `IPA_Method`, and `IPA_Fallback_Used` in the discrimination tab; no time-dependent AUC plots are emitted

**Step 4: Clinical Utility Analysis**
- Perform decision-curve calculations on the horizon-specific binary outcome
- Compute `IPA` using the preferred Brier-score comparison with documented fallbacks
- Run optional PRAME incremental discrimination comparisons when the PRAME-complete subset is adequate
- Capture delta-C summaries, nested-model support metrics, and interpretation rows inside the workbook

**Step 5: Reporting**
- Export outcome-specific consolidated Excel workbooks (`*_MFS_consolidated_summary.xlsx`, `*_MSS_consolidated_summary.xlsx`) and companion text reports
- Export outcome-specific technical workbooks (`*mfs_validation_technical_details.xlsx`, `*mss_validation_technical_details.xlsx`) for lower-level observed/expected and competing-risk detail that complements, rather than duplicates, the consolidated summaries
- Export the root-level cross-outcome workbook `*unified_gep_validation_summary.xlsx`
- Export the simple QC workbook `unified_summary/*simple_gep_validation.xlsx`
- For the full cohort only, append compact exploratory no-GEP summary sheets to the root unified workbook so the main Objective 4 file also summarizes baseline-only risk support for `GEP Failed/Indeterminate` and `GEP Not Tested`
- Ensure narrative summaries preserve the cohort label used for the run and print the overall O/E ratio with its Poisson CI and Pearson goodness-of-fit p-value
- Provide KM (MFS) or CIF (MSS) curves only; calibration/decision/discrimination visuals live in tables

### Expected Outputs

**Location:** `{cohort}/04_GEP_Validation/` with outcome-specific subfolders for MFS and MSS

**Files:**
- `a_metastasis_free_survival/*_MFS_consolidated_summary.xlsx` — primary MFS review workbook, including `Observed_Expected_Summary`
- `b_melanoma_specific_survival/*_MSS_consolidated_summary.xlsx` — primary MSS review workbook, including `Observed_Expected_Summary`
- `a_metastasis_free_survival/*mfs_validation_technical_details.xlsx` and `b_melanoma_specific_survival/*mss_validation_technical_details.xlsx` — technical-detail workbooks without duplicated high-level calibration/discrimination summary sheets
- `a_metastasis_free_survival/*mfs_validation_narrative_summary.txt` and `b_melanoma_specific_survival/*mss_validation_narrative_summary.txt` — narrative summaries
- `*unified_gep_validation_summary.xlsx` at the root of `04_GEP_Validation/` — comparison-only cross-outcome workbook
- For the full cohort, the unified workbook also includes `No_GEP_Overview`, `No_GEP_Model_Comparison`, and `No_GEP_Risk_Strata`
- `unified_summary/*simple_gep_validation.*` — optional actual-vs-expected QC output from the simple checker
- Limited PNGs: KM for MFS, CIF for MSS, and optional outcome-specific PRAME delta-C plots (`*mfs_prame_delta_c.png`, `*mss_prame_delta_c.png`)

**Schema note:** `PRAME_Summary` is always written in consolidated workbooks, and `PRAME_Comparison` is always written in unified workbooks. Sparse cohorts may receive explanatory placeholder rows instead of full PRAME incremental-comparison results. The full-cohort unified workbook may additionally append `No_GEP_*` tabs, but restricted and GKSRS cohorts do not currently receive those exploratory sheets.

### Exploratory No-GEP Risk Workflow

The standalone no-GEP exploratory report lives under `04_GEP_Validation/d_exploratory_no_gep/` and is generated by `run_exploratory_no_gep_report("uveal_melanoma_full_cohort")`. It remains the appendix-style detailed artifact for patients without usable GEP. The full-cohort Objective 4 unified workbook now also carries a compact no-GEP summary layer so the root `*unified_gep_validation_summary.xlsx` file can summarize this question without requiring the reader to open the appendix workbook first.

This workflow keeps `GEP Failed/Indeterminate` and `GEP Not Tested` separate in the main summaries. It builds three modeling populations:

- definitive-GEP training set: `Class 1` and `Class 2` only
- no-GEP prediction set: `GEP Failed/Indeterminate` and `GEP Not Tested`
- direct-risk modeling sets for 5-year MFS and 5-year MSS

#### Data preparation and verification

Before fitting exploratory models, the workflow:

- restores the report-facing GEP class variables used for Objective 4 summaries
- derives `no_gep_group` and the fixed binary baseline indicators (`ciliary_involvement`, `optic_nerve_involvement`)
- derives 5-year binary endpoints (`mfs_event_5yr`, `mss_event_5yr`) when needed from event/time fields
- optionally verifies expected cohort counts and simplified KM risk-table row/count alignment for the four exploratory GEP groups

#### Candidate predictor screening and retained set

The exploratory workflow starts from a predefined baseline candidate set:

- `age_at_diagnosis`
- `sex`
- `initial_tumor_height`
- `initial_tumor_diameter`
- `location`
- `initial_t_stage_simple`
- `internal_reflectivity`
- `srf`
- `initial_vision`
- `optic_nerve_involvement`

Factor predictors are collapsed for rare categories, then screened across the surrogate, direct-MFS, and direct-MSS modeling datasets. Predictors are retained only if they satisfy completeness and post-collapse level-count criteria. The final retained predictor set is then used consistently across all three exploratory models and is written to the workbook.

The derived `ciliary_involvement` field is included in no-GEP prediction outputs for clinical context, but it is not part of the baseline candidate predictor set listed above.

#### Exploratory model definitions

Three ridge-penalized logistic regression models are fit with `glmnet::cv.glmnet(..., family = "binomial", alpha = 0)`:

1. surrogate `Class 2-like` model fitted only on definitive `Class 1` versus `Class 2` patients, with binary outcome coding `class2_outcome = 1` for `Class 2` and `0` for `Class 1`
2. direct 5-year MFS risk model fitted on the full eligible cohort with a 5-year metastasis endpoint
3. direct 5-year MSS risk model fitted on the full eligible cohort with a 5-year melanoma-specific death endpoint

#### Ridge regression: form and motivation

The ridge logistic models minimize the negative log-likelihood plus an $L_2$ penalty:

$$
\hat{\beta} = \arg\min_{\beta_0,\beta}\left\{-\sum_{i=1}^{n}\left[y_i\eta_i - \log\left(1 + e^{\eta_i}\right)\right] + \lambda\sum_{j=1}^{p}\beta_j^2\right\},\quad \eta_i = \beta_0 + x_i^T\beta
$$

The motivation is to shrink coefficients toward zero to reduce variance and overfitting, especially when predictors are correlated or event counts are modest. The tuning parameter $\lambda$ controls shrinkage strength and is typically selected by cross-validation.

All three models use the same screened retained predictor set so interpretation stays aligned across outputs. The workbook reports:

- sample size and event count
- apparent AUC and Brier score
- cross-validated AUC and Brier score
- calibration status, intercept, and slope
- cross-validation folds used (`cv_folds`)
- ridge tuning parameters (`lambda_min`, `lambda_1se`)

Cross-validation uses out-of-fold predictions with fold counts chosen adaptively (up to 5) to preserve class support in sparse settings.

The coefficient tables in this report are ridge-shrunken design-matrix coefficients at `lambda.min`. They are included for directionality and relative contribution ranking, not p-value-based inference.

For clarity, the surrogate model is not trying to reconstruct the molecular assay itself. It uses patients with known definitive GEP labels as a teaching set, learns what the observed baseline clinicopathologic patterns of the Class 1 and Class 2 groups look like in this cohort, and then outputs for each no-GEP patient the probability that their baseline profile more closely resembles the observed Class 2 pattern than the observed Class 1 pattern. This is best interpreted as a clinical resemblance score anchored to the known definitive-GEP patients.

Because the surrogate is a binary logistic model, the reported `surrogate_class2_probability` is mathematically:

$$
P(\text{Class 2-like} \mid \text{baseline features})
$$

and its complement is:

$$
1 - P(\text{Class 2-like} \mid \text{baseline features}) = P(\text{Class 1-like} \mid \text{baseline features})
$$

within this two-class surrogate framework. That complement may therefore be read as a `Class 1-like` clinical resemblance probability, but it must not be described as the probability of a true molecular `Class 1` assay result. Symmetrically, `surrogate_class2_probability` must not be described as the probability of a true molecular `Class 2` assay result. Both quantities are only probabilities of resembling one of the two definitive-GEP clinical-pattern reference groups used to train the surrogate.

#### Prediction summaries

The exploratory workbook includes:

- row-level no-GEP predictions with surrogate class-likeness and direct 5-year MFS/MSS risk probabilities
- grouped summaries split by `GEP Failed/Indeterminate` and `GEP Not Tested`
- pooled low/intermediate/high-style sensitivity summaries based on quantile bins

The full-cohort unified Objective 4 workbook adds a compact version of the same material:

- `No_GEP_Overview` — group counts, observed outcomes, median no-GEP predicted risks, and a short interpretation note
- `No_GEP_Model_Comparison` — side-by-side surrogate/direct MFS/direct MSS model metrics with top predictors and intended use
- `No_GEP_Risk_Strata` — subgroup-by-bin event-rate summaries showing whether predicted ordering tracks observed 5-year outcomes

#### Interpretation contract

The surrogate `Class 2-like` probability is descriptive and should not be interpreted as recovered molecular class. The direct MFS/MSS outputs are the primary clinically useful quantities from this workflow. In other words:

- use surrogate probabilities to describe whether a no-GEP patient clinically resembles the higher-risk definitive-GEP population more than the lower-risk definitive-GEP population
- if needed, the implied `Class 1-like` probability is simply `1 - surrogate_class2_probability`, but this is still only a clinical resemblance probability, not a recovered molecular-class probability
- use direct 5-year MFS/MSS predictions as the main risk estimates when a future patient has failed or unavailable GEP
- interpret both as internally validated exploratory outputs rather than externally confirmed bedside calculators

### Quick Interpretation Shortcuts

- `OE_Ratio`: values near 1 indicate better calibration-in-the-large.
- `Nam_D_Agostino_p`: smaller values suggest stronger evidence of grouped miscalibration.
- `ICI`: lower is better.
- `Slope`: values near 1 are better.
- `Harrell_C`, `Integrated_AUC`, `Cumulative_Discrimination`, `Time_averaged_Discrimination`: higher is better.
- `IPA`: positive values indicate improvement over the null benchmark.
- `Optimal_Net_Benefit` and a positive threshold range: suggest potential clinical utility over that threshold region.
- `Delta_Harrell_C`: positive values favor the PRAME-enhanced model.

For fuller narrative interpretation and workbook examples, see [GEP Quick Read](INTERPRETATION_GUIDE.md#gep-quick-read), [GEP Calibration Made Simple](INTERPRETATION_GUIDE.md#gep-calibration-made-simple), [GEP Discrimination Made Simple](INTERPRETATION_GUIDE.md#gep-discrimination-made-simple), and [GEP Decision Curve Made Simple](INTERPRETATION_GUIDE.md#gep-decision-curve-made-simple).

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
