# RMST Analysis Label Correction Summary

## Issue Identified

Your collaborator correctly identified a **critical labeling error** in the RMST analysis. The manuscript text had **swapped the treatment group labels** - attributing PBT values to GKSRS and vice versa.

## Root Cause

The RMST analysis code was outputting results with generic column names (`RMST_Group1` and `RMST_Group2`) without identifying which treatment group each represented. The group assignment was:

- **Group1 (arm=0)** = **GKSRS** (first alphabetically: "GKSRS" < "PBT")
- **Group2 (arm=1)** = **PBT** (second alphabetically)

The manuscript author mistakenly assumed Group1=PBT and Group2=GKSRS, leading to reversed interpretations.

---

## Code Fix Applied

Modified `scripts/analysis/survival_outcomes.R` to add explicit group name columns:

- Added `Group1_Name` and `Group2_Name` columns to RMST output files
- Now clearly shows: `Group1_Name = "GKSRS"` and `Group2_Name = "PBT"`

This makes it impossible to misinterpret the results.

---

## Manuscript Corrections Required

### ✅ CORRECT INTERPRETATION

**For survival analyses, HIGHER RMST = BETTER outcome** (more time alive/event-free)

The Kaplan-Meier curves correctly show **PBT with higher survival rates** than GKSRS at most timepoints. The RMST analysis now confirms PBT has **higher RMST values**, which is **consistent** with the KM curves.

### 📝 Required Text Corrections

---

#### **Overall Survival (OS) - Restricted Cohort**

**CURRENT (INCORRECT) TEXT:**
> "In the restricted cohort, restricted mean survival time (RMST) analysis showed no significant differences at 3 years (PBT 33.65 months vs GKSRS 34.29 months, difference 0.64 months, p=0.54) or 5 years (PBT 52.15 months vs GKSRS 54.82 months, difference 2.66 months, p=0.30)"

**CORRECTED TEXT:**
> "In the restricted cohort, restricted mean survival time (RMST) analysis showed no significant differences at 3 years (**GKSRS 33.65 months vs PBT 34.29 months**, difference 0.64 months, p=0.54) or 5 years (**GKSRS 52.15 months vs PBT 54.82 months**, difference 2.66 months, p=0.30)"

**VERIFIED VALUES:**
- 3 years: GKSRS 33.6 months, PBT 34.3 months (difference: +0.7 months favoring PBT)
- 5 years: GKSRS 52.2 months, PBT 54.8 months (difference: +2.6 months favoring PBT)

---

#### **Overall Survival (OS) - Full Cohort**

**CURRENT (INCORRECT) TEXT:**
> "with RMST showing no significant differences at 3 years (difference 0.66 months, p=0.39) or 5 years (difference 1.79 months, p=0.34)"

**CORRECTED TEXT:**
> "with RMST showing no significant differences at 3 years (**GKSRS 33.9 months vs PBT 34.5 months**, difference 0.66 months, p=0.39) or 5 years (**GKSRS 53.2 months vs PBT 55.0 months**, difference 1.79 months, p=0.34)"

**VERIFIED VALUES:**
- 3 years: GKSRS 33.9 months, PBT 34.5 months (difference: +0.66 months favoring PBT)
- 5 years: GKSRS 53.2 months, PBT 55.0 months (difference: +1.79 months favoring PBT)

---

#### **Progression-Free Survival (PFS) - Restricted Cohort** ⚠️ CRITICAL ERROR

**CURRENT (INCORRECT) TEXT:**
> "In the restricted cohort, RMST analysis revealed a borderline significant difference at 5 years (PBT 47.16 months vs GKSRS 52.67 months, difference 5.51 months **favoring GKSRS**, p=0.068)."

**CORRECTED TEXT:**
> "In the restricted cohort, RMST analysis revealed a borderline significant difference at 5 years (**GKSRS 47.16 months vs PBT 52.67 months**, difference 5.51 months **favoring PBT**, p=0.068)."

**VERIFIED VALUES:**
- 5 years: GKSRS 47.2 months, PBT 52.7 months (difference: +5.5 months favoring **PBT**, not GKSRS!)

**INTERPRETATION:** PBT patients had 5.5 more months of progression-free survival on average. This is **consistent** with the KM curves showing higher PFS rates for PBT.

---

#### **Progression-Free Survival (PFS) - Full Cohort** ⚠️ CRITICAL ERROR

**CURRENT (INCORRECT) TEXT:**
> "In the full cohort, RMST analysis showed significant differences **favoring GKSRS** at both 3 years (PBT 31.52 months vs GKSRS 33.76 months, difference 2.23 months, p=0.029) and 5 years (PBT 47.87 months vs GKSRS 52.58 months, difference 4.71 months, p=0.042)"

**CORRECTED TEXT:**
> "In the full cohort, RMST analysis showed significant differences **favoring PBT** at both 3 years (**GKSRS 31.52 months vs PBT 33.76 months**, difference 2.23 months, p=0.029) and 5 years (**GKSRS 47.87 months vs PBT 52.58 months**, difference 4.71 months, p=0.042)"

**VERIFIED VALUES:**
- 3 years: GKSRS 31.5 months, PBT 33.8 months (difference: +2.2 months favoring **PBT**, not GKSRS!)
- 5 years: GKSRS 47.9 months, PBT 52.6 months (difference: +4.7 months favoring **PBT**, not GKSRS!)

**INTERPRETATION:** PBT patients had significantly more progression-free time. This is **consistent** with the KM curves showing higher PFS rates for PBT at these timepoints.

---

## Summary of Corrections

| Analysis | Timepoint | Current Text | Should Be | Difference Direction |
|----------|-----------|--------------|-----------|---------------------|
| **Restricted OS** | 3 years | PBT 33.65 vs GKSRS 34.29 | GKSRS 33.6 vs PBT 34.3 | Non-significant, slight PBT advantage |
| **Restricted OS** | 5 years | PBT 52.15 vs GKSRS 54.82 | GKSRS 52.2 vs PBT 54.8 | Non-significant, slight PBT advantage |
| **Full OS** | 3 years | (values missing) | GKSRS 33.9 vs PBT 34.5 | Non-significant, slight PBT advantage |
| **Full OS** | 5 years | (values missing) | GKSRS 53.2 vs PBT 55.0 | Non-significant, slight PBT advantage |
| **Restricted PFS** | 5 years | PBT 47.16 vs GKSRS 52.67, **favoring GKSRS** | GKSRS 47.2 vs PBT 52.7, **favoring PBT** | ⚠️ **REVERSED!** |
| **Full PFS** | 3 years | PBT 31.52 vs GKSRS 33.76, **favoring GKSRS** | GKSRS 31.5 vs PBT 33.8, **favoring PBT** | ⚠️ **REVERSED!** |
| **Full PFS** | 5 years | PBT 47.87 vs GKSRS 52.58, **favoring GKSRS** | GKSRS 47.9 vs PBT 52.6, **favoring PBT** | ⚠️ **REVERSED!** |

---

## Clinical Significance

**The corrected analysis shows:**

1. **Overall Survival:** No significant differences, with slight (non-significant) numerical advantage for PBT
2. **Progression-Free Survival:** PBT shows significant advantage in full cohort and borderline advantage in restricted cohort

**This is NOW CONSISTENT with the Kaplan-Meier curves**, which show PBT with higher survival rates at most timepoints.

---

## Your Collaborator Was Correct

Your collaborator's observation was spot-on:
> "The KM curves for PBT were higher at those timepoints than GK, so that didn't seem to make sense"

The KM curves correctly showed PBT advantage, but the text incorrectly claimed GKSRS advantage. **The KM curves were right all along** - it was the RMST interpretation that was backwards.

---

## Files Updated

1. ✅ `scripts/analysis/survival_outcomes.R` - Added `Group1_Name` and `Group2_Name` columns
2. ✅ All RMST Excel files now contain explicit treatment labels
3. ✅ Full analysis re-run completed successfully

## Next Steps

1. ⚠️ **Update manuscript text** with corrected RMST values and interpretations
2. ✅ Verify corrections against updated Excel files in `final_data/Analysis/`
3. ✅ Re-read entire results section to ensure no other labeling errors exist
