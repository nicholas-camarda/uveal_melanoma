# Helper file for test data creation
# This file provides the create_test_dataset function used by all objective test files

# Load required packages for test data creation

create_test_dataset <- function() {
  # Create deterministic test dataset for all objectives
  set.seed(12345)

  # Create base dates for consistency
  base_date <- as.Date("2020-01-01")

  # Create tumor height variables first since they're referenced later
  initial_tumor_height <- c(
    3.5, 4.2, 2.8, 5.1, 3.9, 4.7, 3.1, 4.8, 3.3, 4.0,
    3.8, 4.5, 2.9, 5.3, 3.6, 4.1, 3.4, 4.9, 3.2, 4.3
  )
  final_tumor_height <- c(
    2.1, 3.8, 1.9, 4.2, 2.5, 3.1, 2.0, 3.5, 2.2, 2.8,
    2.7, 3.9, 1.8, 4.5, 2.4, 2.9, 2.1, 3.7, 2.0, 3.2
  )

  tibble::tibble(
    patient_id = 1:20,

    # Required variables for create_derived_variables()
    initial_gk = rep(c("Y", "N"), each = 10),
    initial_plaque = rep(c("N", "Y"), each = 10),
    initial_gk_date = rep(c(base_date, as.Date(NA)), each = 10),
    initial_plaque_date = rep(c(as.Date(NA), base_date), each = 10),
    date_diagnosis = base_date + sample(0:30, 20, replace = TRUE),
    dob = base_date - (365 * sample(50:80, 20, replace = TRUE)),
    last_known_alive_date = base_date + sample(365:1825, 20, replace = TRUE),

    # Treatment variables (these will be derived, but we need them for testing)
    treatment_group = rep(c("GKSRS", "Plaque"), each = 10),
    consort_group = rep(c("GKSRS", "Plaque"), each = 10),

    # Survival variables
    tt_mets_months = c(
      12, 24, 36, 48, 60, 18, 30, 42, 54, 66,
      15, 27, 39, 51, 63, 21, 33, 45, 57, 69
    ),
    mets_event = c(
      1, 1, 1, 0, 0, 1, 1, 0, 0, 0,
      1, 1, 0, 0, 0, 1, 0, 0, 0, 0
    ),
    tt_death_months = c(
      24, 48, 72, 96, 120, 36, 60, 84, 108, 132,
      30, 54, 78, 102, 126, 42, 66, 90, 114, 138
    ),
    death_event = c(
      1, 1, 0, 0, 0, 1, 0, 0, 0, 0,
      1, 0, 0, 0, 0, 1, 0, 0, 0, 0
    ),
    tt_death_years = tt_death_months / 12,

    # Local recurrence variables - use "Y"/"N" format as expected by create_derived_variables
    recurrence1 = c(
      "N", "Y", "N", "N", "N", "Y", "N", "N", "N", "N",
      "N", "N", "Y", "N", "N", "N", "N", "N", "N", "N"
    ),
    tt_recurrence_months = c(
      999, 24, 999, 999, 999, 18, 999, 999, 999, 999,
      999, 999, 27, 999, 999, 999, 999, 999, 999, 999
    ),
    recurrence_event = c(
      0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
      0, 0, 1, 0, 0, 0, 0, 0, 0, 0
    ),

    # Additional recurrence variables needed by create_derived_variables
    recurrence1_date = case_when(
      recurrence1 == "Y" ~ base_date + sample(30:180, 20, replace = TRUE),
      TRUE ~ as.Date(NA)
    ),
    recurrence1_treatment_date = case_when(
      recurrence1 == "Y" ~ base_date + sample(30:180, 20, replace = TRUE),
      TRUE ~ as.Date(NA)
    ),
    recurrence2 = rep("N", 20),
    recurrence2_date = rep(as.Date(NA), 20),

    # Metastasis variables needed by create_derived_variables
    mets_progression = case_when(
      mets_event == 1 ~ "Y",
      TRUE ~ "N"
    ),
    mets_progression_date = case_when(
      mets_event == 1 ~ base_date + sample(30:180, 20, replace = TRUE),
      TRUE ~ as.Date(NA)
    ),

    # Death variables needed by create_derived_variables
    dod = case_when(
      death_event == 1 ~ base_date + sample(30:180, 20, replace = TRUE),
      TRUE ~ as.Date(NA)
    ),

    # Additional variables needed by create_derived_variables
    cod = rep("Metastatic_Uveal_Melanoma", 20), # Cause of death
    last_height = final_tumor_height,
    recurrence1_pretreatment_height = initial_tumor_height,
    recurrence1_treatment = rep("GKSRS", 20),
    initial_overall_stage = rep("2A", 20), # Add missing stage variable

    # PFS variables
    tt_pfs_months = pmin(tt_mets_months, tt_death_months),
    pfs_event = as.integer(mets_event == 1 | death_event == 1),

    # Tumor height variables
    initial_tumor_height = initial_tumor_height,
    final_tumor_height = final_tumor_height,
    height_change = final_tumor_height - initial_tumor_height,

    # Vision variables
    baseline_vision = c(
      20 / 40, 20 / 50, 20 / 30, 20 / 60, 20 / 25, 20 / 45, 20 / 35, 20 / 55, 20 / 40, 20 / 50,
      20 / 35, 20 / 45, 20 / 30, 20 / 55, 20 / 40, 20 / 50, 20 / 35, 20 / 45, 20 / 30, 20 / 55
    ),
    final_vision = baseline_vision - c(
      0.32, 0.24, 0.16, 0.08, -0.08, -0.16, -0.24, -0.32, 0.12, 0.04,
      0.28, 0.18, 0.02, -0.02, -0.12, -0.22, -0.31, 0.09, -0.19, 0.14
    ),
    initial_vision = baseline_vision,
    last_vision = final_vision,
    recurrence1_pretreatment_vision = final_vision,
    vision_change = baseline_vision - final_vision,

    # Objective 2 adverse events
    retinopathy = c(
      rep("Y", 6), rep("N", 4),
      rep("Y", 6), rep("N", 4)
    ),
    nvg = c(
      rep("Y", 3), rep("N", 7),
      rep("Y", 2), rep("N", 8)
    ),
    srd = c(
      rep("Y", 5), rep("N", 5),
      rep("Y", 6), rep("N", 4)
    ),

    # Toxicity variables
    toxicity_grade = sample(0:4, 20, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.08, 0.02)),
    toxicity_type = sample(c("None", "Mild", "Moderate", "Severe"), 20, replace = TRUE),

    # Adverse events variables
    adverse_event_count = rpois(20, lambda = 2),
    serious_adverse_event = sample(c(0, 1), 20, replace = TRUE, prob = c(0.8, 0.2)),

    # Repeat radiation variables
    repeat_radiation_event = c(
      0, 1, 0, 0, 1, 0, 1, 0, 0, 1,
      0, 0, 1, 0, 0, 1, 0, 0, 1, 0
    ),
    tt_repeat_radiation_months = c(
      999, 24, 999, 999, 36, 999, 18, 999, 999, 42,
      999, 999, 30, 999, 999, 48, 999, 999, 27, 999
    ),
    repeat_radiation_technique = c(
      NA, "GKSRS", NA, NA, "Plaque", NA, "GKSRS", NA, NA, "Plaque",
      NA, NA, "GKSRS", NA, NA, "Plaque", NA, NA, "GKSRS", NA
    ),
    repeat_radiation_dose = c(
      NA, 24, NA, NA, 85, NA, 20, NA, NA, 80,
      NA, NA, 22, NA, NA, 90, NA, NA, 25, NA
    ),

    # GEP variables
    biopsy1_gep = rep(c("Class 1", "Class 2"), each = 10),
    biopsy1_gep_mfs = c(rep(0.2, 10), rep(0.8, 10)),
    biopsy1_gep_mss = c(rep(0.15, 10), rep(0.85, 10)),

    # Derived GEP variables (created by create_derived_variables)
    gep_class_simple = rep(c("Class 1", "Class 2"), each = 10),
    prame_status = rep(c("Negative", "Positive"), each = 10),
    gep_validation_set = rep("Eligible", 20),

    # Melanoma-specific death variables
    melanoma_death_event = c(
      1, 1, 0, 0, 0, 1, 0, 0, 0, 0,
      1, 0, 0, 0, 0, 1, 0, 0, 0, 0
    ),
    competing_death_event = c(
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    ),

    # Expected survival probabilities
    expected_mfs_5yr = c(rep(0.8, 10), rep(0.2, 10)),
    expected_mfs_7yr = c(rep(0.7, 10), rep(0.15, 10)),
    expected_mfs_10yr = c(rep(0.6, 10), rep(0.1, 10)),
    expected_mss_5yr = c(rep(0.85, 10), rep(0.15, 10)),
    expected_mss_7yr = c(rep(0.75, 10), rep(0.1, 10)),
    expected_mss_10yr = c(rep(0.65, 10), rep(0.05, 10)),

    # Time-specific event indicators for GEP analysis
    mfs_event_5yr = c(rep(0, 10), rep(1, 10)),
    mfs_event_7yr = c(rep(0, 10), rep(1, 10)),
    mfs_event_10yr = c(rep(0, 10), rep(1, 10)),

    # Demographics
    age_at_diagnosis = c(
      65, 72, 58, 69, 75, 61, 68, 71, 64, 67,
      63, 70, 59, 66, 73, 62, 69, 71, 65, 68
    ),
    age_at_diagnosis_general_pop_median = factor(
      ifelse(age_at_diagnosis >= 66, "Older", "Younger"),
      levels = c("Younger", "Older")
    ),
    sex = rep(c("Male", "Female"), each = 10),
    location = rep(c("Choroid", "Ciliary Body"), each = 10),
    initial_t_stage = rep(c("T1", "T2", "T3"), length.out = 20),
    initial_tumor_diameter = runif(20, 5, 20),
    optic_nerve = rep(c("Involved", "Not Involved"), each = 10)
  )
}
