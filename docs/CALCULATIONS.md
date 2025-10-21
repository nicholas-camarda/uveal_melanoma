# Derived Variable Calculations

This document explains how key derived variables are calculated in the analysis pipeline. All calculations are performed once in **Objective 0 (Data Derivation)** in `scripts/data_helper/data_derivation.R` to ensure consistency across all analyses.

---

## Table of Contents

- [Tumor Height Change](#tumor-height-change)
- [Vision Change](#vision-change)
- [Time-to-Event Variables](#time-to-event-variables)
- [Event Indicators](#event-indicators)
- [Age Calculations](#age-calculations)
- [Follow-up Duration](#follow-up-duration)

---

## Tumor Height Change

### **Purpose**

Measures tumor response to radiation treatment by calculating the change in tumor height from baseline to follow-up.

### **Formula**

```r
height_change = case_when(
    recurrence1 == "Y" ~ recurrence1_pretreatment_height - initial_tumor_height,
    TRUE ~ last_height - initial_tumor_height
)
```

### **Sign Convention**

- **Negative value** = tumor shrank (good clinical outcome)
- **Positive value** = tumor grew (poor clinical outcome)

### **Example**

- **Non-recurrence patient:**

  - Initial height: 6.0 mm
  - Last height: 4.5 mm
  - **height_change = 4.5 - 6.0 = -1.5 mm** (tumor shrank by 1.5 mm ✓)
- **Recurrence patient:**

  - Initial height: 7.0 mm
  - Recurrence1 pretreatment height: 8.5 mm
  - **height_change = 8.5 - 7.0 = +1.5 mm** (tumor grew by 1.5 mm before recurrence ✗)

### **Why Different Measurements for Recurrence Patients?**

**Non-recurrence patients (`recurrence1 == "N"`):**

- Use `last_height` = most recent tumor measurement during follow-up
- Captures the full treatment response over the entire follow-up period
- Reflects stable post-treatment outcome

**Recurrence patients (`recurrence1 == "Y"`):**

- Use `recurrence1_pretreatment_height` = height measured just before salvage treatment
- **Why not last_height?** Because `last_height` for recurrence patients reflects the outcome after salvage treatment, not the primary treatment we're evaluating
- **Clinical rationale:** We want to assess the primary treatment's effectiveness, measured at the point of recurrence (before salvage treatment confounds the result)
- **Interpretation:** Shows whether the tumor was controlled by primary treatment or continued growing before needing salvage therapy

### **Clinical Interpretation**

- **Median negative values (e.g., -0.9 mm):** Most patients experienced tumor shrinkage after treatment (good)
- **Positive values:** Tumor growth, indicating treatment failure or progression
- **Larger negative values:** Greater tumor response to treatment

### **Location in Code**

`scripts/data_helper/data_derivation.R`, lines ~98-103

---

## Vision Change

### **Purpose**

Measures visual acuity changes from baseline to follow-up using the logMAR scale.

### **Formula**

```r
vision_change = case_when(
    recurrence1 == "Y" ~ initial_vision - recurrence1_pretreatment_vision,
    TRUE ~ initial_vision - last_vision
)
```

### **Sign Convention**

- **Negative value** = vision worsened (logMAR increased)
- **Positive value** = vision improved (logMAR decreased)

### **Understanding logMAR Scale**

- **logMAR 0.0** = 20/20 vision (normal)
- **Positive logMAR** = worse than 20/20 (e.g., +0.3 = 20/40 vision)
- **Negative logMAR** = better than 20/20 (e.g., -0.1 = 20/16 vision)
- **Higher logMAR = worse vision**

### **Example**

- **Non-recurrence patient:**

  - Initial vision: 0.2 logMAR (20/32)
  - Last vision: 0.5 logMAR (20/63)
  - **vision_change = 0.2 - 0.5 = -0.3** (vision worsened by 3 lines on eye chart ✗)
- **Recurrence patient:**

  - Initial vision: 0.1 logMAR (20/25)
  - Recurrence1 pretreatment vision: 0.4 logMAR (20/50)
  - **vision_change = 0.1 - 0.4 = -0.3** (vision worsened by 3 lines ✗)

### **Why Different Measurements for Recurrence Patients?**

**Same logic as tumor height:**

- **Non-recurrence patients:** Use `last_vision` = most recent vision measurement
- **Recurrence patients:** Use `recurrence1_pretreatment_vision` = vision just before salvage treatment
- **Rationale:** Isolate the visual outcome of primary treatment, before salvage treatment affects vision further

### **Clinical Interpretation**

- **Median negative values (e.g., -0.2):** Most patients experienced vision loss after radiation (expected radiation toxicity)
- **Positive values:** Vision improvement (uncommon but possible if initial tumor affected vision)
- **Large negative values:** Severe vision loss, often due to radiation retinopathy

### **Expected Clinical Pattern**

Vision loss is **expected** after radiation treatment due to:

- Radiation retinopathy (49-50% of patients)
- Macular edema
- Optic neuropathy
- Vascular damage

### **Location in Code**

`scripts/data_helper/data_derivation.R`, lines ~106-111

---

## Time-to-Event Variables

### **Time to Recurrence**

**Formula:**

```r
tt_recurrence_months = case_when(
    recurrence1 == "Y" ~ interval(treatment_date, recurrence1_date) / months(1),
    TRUE ~ interval(treatment_date, last_known_alive_date) / months(1)
)
```

**Components:**

- **Event patients:** Time from treatment to documented recurrence
- **Censored patients:** Time from treatment to last known alive (no recurrence observed)

**Sign Convention:**

- Always positive (time cannot be negative)
- Negative values are set to 0 via `tt_recurrence_months_analysis`

---

### **Time to Metastasis**

**Formula:**

```r
tt_mets_months = case_when(
    mets_progression == "Y" ~ interval(treatment_date, mets_progression_date) / months(1),
    TRUE ~ interval(treatment_date, last_known_alive_date) / months(1)
)
```

**Clinical Note:**

- Captures time to **distant** metastases (not local recurrence)
- Used for metastasis-free survival analysis

---

### **Time to Death**

**Formula:**

```r
tt_death_months = case_when(
    !is.na(dod) ~ interval(treatment_date, dod) / months(1),
    TRUE ~ interval(treatment_date, last_known_alive_date) / months(1)
)
```

**Components:**

- **Death event:** Time from treatment to death (any cause)
- **Censored:** Time from treatment to last known alive

---

### **Progression-Free Survival (PFS)**

**Formula:**

```r
tt_pfs_months_analysis = pmin(tt_recurrence_months_analysis, tt_death_months_analysis, na.rm = FALSE)
```

**Definition:**

- **Composite endpoint:** First occurrence of either local recurrence OR death
- Takes the **minimum** time between recurrence and death
- If recurrence occurs before death, PFS time = time to recurrence
- If death occurs before recurrence, PFS time = time to death

**Example:**

- Patient has recurrence at 24 months, death at 60 months
- **PFS time = 24 months** (first event)

---

### **Progression-Free Survival-2 (PFS-2)**

**Purpose:** Evaluate effectiveness of salvage treatment after local recurrence

**Formula:**

```r
# For patients with recurrence who received salvage treatment
tt_pfs2_months = case_when(
    recurrence2 == "Y" ~ interval(recurrence1_treatment_date, recurrence2_date) / months(1),
    TRUE ~ interval(recurrence1_treatment_date, last_known_alive_date) / months(1)
)
```

**Components:**

- **Starting point:** Date of salvage treatment (not primary treatment)
- **Event:** Second recurrence OR death
- **Censoring:** Last known alive without second recurrence

**Clinical Interpretation:**

- Measures how long salvage treatment controlled the tumor
- Helps inform second-line treatment decisions

---

## Event Indicators

### **Binary Event Variables**

All event indicators are coded as:

- **0** = Event did not occur (censored)
- **1** = Event occurred

**Examples:**

```r
recurrence_event = if_else(recurrence1 == "Y", 1, 0, missing = 0)
mets_event = if_else(mets_progression == "Y", 1, 0, missing = 0)
death_event = if_else(!is.na(dod), 1, 0, missing = 0)
```

---

### **Melanoma-Specific Death Event**

**Formula:**

```r
melanoma_death_event = case_when(
    cod == "Metastatic_Uveal_Melanoma" ~ 1,
    TRUE ~ 0
)
```

**Purpose:**

- Used in melanoma-specific survival (MSS) analysis
- Distinguishes melanoma deaths from other causes of death

**Competing Death Event:**

```r
competing_death_event = case_when(
    !is.na(dod) & cod != "Metastatic_Uveal_Melanoma" ~ 1,
    TRUE ~ 0
)
```

**Why This Matters:**

- For competing risks analysis
- Non-melanoma deaths (heart attack, car accident, etc.) are treated as competing events, not censoring

---

## Age Calculations

### **Age at Diagnosis**

**Formula:**

```r
age_at_diagnosis = interval(dob, diagnosis_date) / years(1)
```

**Binned Version:**

```r
age_at_diagnosis_binned = case_when(
    age_at_diagnosis < 50 ~ "<50",
    age_at_diagnosis >= 50 & age_at_diagnosis < 60 ~ "50-59",
    age_at_diagnosis >= 60 & age_at_diagnosis < 70 ~ "60-69",
    age_at_diagnosis >= 70 ~ ">=70"
)
```

**Clinical Rationale:**

- Binned age used for subgroup analysis
- Categories chosen based on clinical relevance and sample size balance

---

## Follow-up Duration

### **Total Follow-up Time**

**Formula:**

```r
total_years = interval(treatment_date, last_known_alive_date) / years(1)
```

**Components:**

- **Start:** Primary treatment date
- **End:** Last known alive date (from multiple sources)

**Last Known Alive Date Determination:**

```r
last_known_alive_date = pmax(
    last_followup,
    recurrence1_date,
    recurrence2_date,
    recurrence3_date,
    mets_progression_date,
    dod,
    na.rm = TRUE
)
```

**Logic:**

- Takes the **maximum** date across all follow-up events
- Ensures we capture the most recent patient contact
- Critical for censoring in survival analyses

---

## Consolidation Notes

### **Single Source of Truth**

All these calculations are performed **once** in `data_derivation.R` (Objective 0) and saved to the analytic datasets. This ensures:

✅ **Consistency:** Individual and merged tables show identical values
✅ **Maintainability:** One place to update if formulas change
✅ **Auditability:** Clear documentation of calculation logic
✅ **Efficiency:** No redundant recalculation across analyses

---

## References

**Implementation:**
All calculations performed in `scripts/data_helper/data_derivation.R`

**Validation:**
Calculation correctness validated through:

- Comparison of individual and merged tables
- Manual spot-checks of derived values
- Clinical plausibility checks (e.g., negative tumor shrinkage expected)
