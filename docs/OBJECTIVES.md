# Study Objectives

This document is a reference for the study objectives and their implementation in the current repository. It is not a step-by-step guide to running the pipeline or interpreting outputs. For those purposes, use [README.md](README.md).

For implementation detail, output contracts, and workflow behavior, use [TECHNICAL.md](TECHNICAL.md). For formal methods, use [STATISTICAL_METHODS.md](STATISTICAL_METHODS.md). For endpoint construction, use [CALCULATIONS.md](CALCULATIONS.md).

## Project Purpose

The project compares clinical outcomes after primary radiation treatment for uveal melanoma, evaluates toxicity, studies outcomes after repeat radiation for local recurrence, and assesses the predictive accuracy of externally supplied Gene Expression Profile (GEP) predictions.

The source planning document frames the primary treatment comparison as plaque brachytherapy versus Gamma Knife stereotactic radiosurgery (GKSRS). The current repository documentation and active pipeline describe the primary comparison as proton beam therapy (PBT) versus GKSRS. This page uses the current repository framing while preserving the same four statistical priorities.

## Objective 1: Efficacy of Primary Treatment

Goal: compare efficacy outcomes after the primary treatment modalities used in the current pipeline, with the source planning document's treatment-comparison intent carried forward.

Current pipeline objective: `01_Efficacy/`

Key sub-objectives:

- `1a. Local recurrence`: compare rates of `recurrence1`
- `1b. Metastatic progression`: compare rates of `mets_progression`
- `1c. Overall survival`: compare survival after primary treatment
- `1d. Progression-free survival`: compare progression-free survival in the current pipeline's efficacy framework
- `1e. Tumor height change (primary)`: evaluate change from `initial_tumor_height` to `last_height`
- `1f. Tumor height change (sensitivity)`: retain the same clinical question with the pipeline's sensitivity-model specification
- `1g. Subgroup analysis`: evaluate whether efficacy patterns differ across baseline subgroups

Tumor-height note:

- The source planning document explicitly asks for pretreatment-versus-follow-up tumor-height change.
- It also states that for retreated local progression, the retreatment context should use `initial_tumor_height - recurrence1_pretreatment_height` when that question is relevant.

### Objective 1 Subgroup Scope

The source planning document explicitly prioritizes efficacy subgroup analysis using these baseline variables:

- age
- sex
- location
- `initial_T_stage`
- `initial_tumor_height`
- `initial_tumor_diameter`
- `biopsy1_gep`
- `optic_nerve`, with the source note that this should be compared only in the overall treatment groups

The current repository implements subgroup outputs within the Objective 1 subgroup-analysis workflow and documents filtering/stability rules in [TECHNICAL.md](TECHNICAL.md#subgroup-filtering).

## Objective 2: Safety and Toxicity of Primary Treatment

Goal: compare treatment-related visual and radiation-toxicity outcomes after the primary treatment modalities.

Current pipeline objective: `02_Safety/`

Key sub-objectives:

- `2a. Vision change`: evaluate change in vision from `initial_vision` to `last_vision`
- retreatment vision context: when local progression occurs and the eye is retreated, the source planning document specifies the corresponding pretreatment-to-retreatment comparison using `initial_vision - recurrence1_pretreatment_vision`
- `2b. Radiation retinopathy`: compare rates of `retinopathy`
- `2c. Neovascular glaucoma`: compare rates of `nvg`
- `2d. Serous retinal detachment`: compare rates of `srg`, restricted to radiation-induced cases where `srd_cause` indicates radiation-induced cause

This objective is about toxicity burden, not just treatment efficacy. The current pipeline documents the implemented model families and output structure in [TECHNICAL.md](TECHNICAL.md#objective-2-safetytoxicity-analysis-complete).

## Objective 3: Efficacy of Repeat Radiation

Goal: evaluate outcomes after second-line treatment among patients who develop local progression after primary therapy.

Current pipeline objective: `03_Repeat_Radiation/`

Primary sub-objective:

- `3a. Progression-Free Survival-2 (PFS-2)`: among patients with `recurrence1 == "Y"` who undergo retreatment, compare second-line outcomes across retreatment groups

The source planning document defines this specifically for patients retreated with GKSRS versus enucleation versus TTT, using `recurrence1_treatment`.

Time-to-event definition from the source planning document:

- if second local progression occurs (`recurrence2 == "Y"`), calculate time as `recurrence2_date - recurrence1_treatment_date`
- if second local progression does not occur, calculate time as `last_followup - recurrence1_treatment_date`

The current repository implements this as the PFS-2 workflow under `03_Repeat_Radiation/a_pfs2/`.

## Objective 4: Predictive Accuracy of Gene Expression Profile (GEP)

Goal: evaluate how well externally supplied GEP predictions align with observed patient outcomes.

Current pipeline objective: `04_GEP_Validation/`

Key sub-objectives from the source planning document:

- compare actual versus expected 5-year metastasis-free survival (MFS) using the imported GEP prediction field `biopsy1_gep_mfs`
- compare actual versus expected 5-year melanoma-specific survival (MSS) using the imported GEP prediction field `biopsy1_gep_mss`

Current pipeline interpretation:

- the repository validates imported patient-level GEP predictions rather than fitting a new base molecular prognostic model
- the current implementation extends the imported 5-year prediction framework into additional horizon-specific validation outputs, but the core scientific question remains whether reported GEP risk aligns with observed outcomes

For the full validation framework and workbook structure, use [STATISTICAL_METHODS.md](STATISTICAL_METHODS.md#gep-validation-metrics) and [INTERPRETATION_GUIDE.md](INTERPRETATION_GUIDE.md#understanding-gep-analysis).

## Inclusion and Exclusion Criteria for Cohort Logic

The source planning document explicitly defines the subgroup-comparison eligibility logic that underlies the restricted versus GKSRS-only cohort split.

### Dual-Eligibility Criteria

These criteria define patients considered candidates for both primary treatment modalities in the source planning document and correspond to the repository's restricted-cohort logic:

- maximum pretreatment tumor basal diameter `<= 20 mm`
- maximum pretreatment B-scan tumor height `<= 10 mm`
- no tumor abutment of the optic nerve

### GKSRS-Only Criteria

These criteria define patients considered candidates for only one treatment modality in the source planning document and correspond to the repository's GKSRS-only cohort logic:

- maximum pretreatment tumor basal diameter `> 20 mm`
- maximum pretreatment B-scan tumor height `> 10 mm`
- tumor abutment of the optic nerve

The source planning document also notes that optic-nerve abutment includes patients treated with either GKSRS or notched plaque brachytherapy. In the current repository, these rules are reflected in the restricted and GKSRS-only derived cohorts described in [README.md](../README.md) and [TECHNICAL.md](TECHNICAL.md#dataset-identities-and-construction).

## How to Use This Page

Use this page when you need the study aims in one place.

- For a quick project overview and run instructions, use [README.md](../README.md).
- For implementation details and artifact locations, use [TECHNICAL.md](TECHNICAL.md).
- For formal statistical methods, use [STATISTICAL_METHODS.md](STATISTICAL_METHODS.md).
- For variable derivations and endpoint construction, use [CALCULATIONS.md](CALCULATIONS.md).
