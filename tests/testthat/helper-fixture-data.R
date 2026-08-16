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
    treatment_date = base_date,
    date_diagnosis = base_date + sample(0:30, 20, replace = TRUE),
    dob = base_date - (365 * sample(50:80, 20, replace = TRUE)),
    last_known_alive_date = base_date + sample(365:1825, 20, replace = TRUE),
    last_followup = base_date + seq(365, by = 45, length.out = 20),

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
    recurrence1_treatment_clean = ifelse(recurrence1 == "Y", "GKSRS", NA_character_),

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
    vision_line_change = compute_line_change_lines(vision_change),
    vision_line_change_bucket = assign_line_change_bucket(vision_line_change),

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
    retinopathy_burden_event = c(
      rep(1L, 6), rep(0L, 4),
      rep(1L, 6), rep(0L, 4)
    ),
    nvg_burden_event = c(
      rep(1L, 3), rep(0L, 7),
      rep(1L, 2), rep(0L, 8)
    ),
    srd_burden_event = c(
      rep(1L, 5), rep(0L, 5),
      rep(1L, 6), rep(0L, 4)
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
    gep12_prame_status = factor(
      rep(c("Negative", "Positive"), each = 10),
      levels = c("Negative", "Positive")
    ),
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

    # Canonical Objective 0 risk, eligibility, event-type, and time fields
    predicted_mfs_risk_5yr = 1 - expected_mfs_5yr,
    predicted_mfs_risk_7yr = 1 - expected_mfs_7yr,
    predicted_mfs_risk_10yr = 1 - expected_mfs_10yr,
    predicted_mss_risk_5yr = 1 - expected_mss_5yr,
    predicted_mss_risk_7yr = 1 - expected_mss_7yr,
    predicted_mss_risk_10yr = 1 - expected_mss_10yr,

    # Time-specific event indicators for GEP analysis
    mfs_event_5yr = c(rep(0, 10), rep(1, 10)),
    mfs_event_7yr = c(rep(0, 10), rep(1, 10)),
    mfs_event_10yr = c(rep(0, 10), rep(1, 10)),
    mss_event_5yr = as.integer(tt_death_years <= 5 & melanoma_death_event == 1),
    mss_event_7yr = as.integer(tt_death_years <= 7 & melanoma_death_event == 1),
    mss_event_10yr = as.integer(tt_death_years <= 10 & melanoma_death_event == 1),
    event_type_mss_5yr = dplyr::case_when(
      melanoma_death_event == 1 & tt_death_years <= 5 ~ 1L,
      competing_death_event == 1 & tt_death_years <= 5 ~ 2L,
      TRUE ~ 0L
    ),
    event_type_mss_7yr = dplyr::case_when(
      melanoma_death_event == 1 & tt_death_years <= 7 ~ 1L,
      competing_death_event == 1 & tt_death_years <= 7 ~ 2L,
      TRUE ~ 0L
    ),
    event_type_mss_10yr = dplyr::case_when(
      melanoma_death_event == 1 & tt_death_years <= 10 ~ 1L,
      competing_death_event == 1 & tt_death_years <= 10 ~ 2L,
      TRUE ~ 0L
    ),
    tt_mfs_5yr = pmin(tt_mets_months, 60),
    tt_mfs_7yr = pmin(tt_mets_months, 84),
    tt_mfs_10yr = pmin(tt_mets_months, 120),
    tt_mss_5yr = pmin(tt_death_years, 5),
    tt_mss_7yr = pmin(tt_death_years, 7),
    tt_mss_10yr = pmin(tt_death_years, 10),
    mfs_analysis_eligible = TRUE,
    mss_analysis_eligible = TRUE,

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

#' Create a balanced deterministic dataset for successful full-pipeline tests
#'
#' This extends the date-complete unit fixture with distribution-shaped values
#' and enough independent support for adjusted and subgroup models. Values are
#' synthetic and generated without reading project data or runtime artifacts.
create_pipeline_test_dataset <- function(n = 96L, seed = 20260816L) {
  stopifnot(length(n) == 1L, n >= 80L, n == as.integer(n))
  withr::with_seed(seed, {
    row_index <- rep(seq_len(20L), length.out = n)
    data <- create_test_dataset()[row_index, , drop = FALSE]
    arm <- rep(c("PBT", "GKSRS"), length.out = n)
    event_pattern <- sample(c(rep(1L, n %/% 3L), rep(0L, n - n %/% 3L)))
    mets_pattern <- sample(c(rep(1L, n %/% 3L), rep(0L, n - n %/% 3L)))
    death_pattern <- sample(c(rep(1L, n %/% 4L), rep(0L, n - n %/% 4L)))

    data$patient_id <- seq_len(n)
    data$treatment_group <- factor(arm, levels = c("PBT", "GKSRS"))
    data$consort_group <- arm
    data$sex <- factor(rep(c("Female", "Male", "Male", "Female"), length.out = n))
    data$location <- factor(
      rep(c("Choroidal", "Ciliary Body", "Cilio-Choroidal"), length.out = n)
    )
    data$initial_t_stage <- factor(
      rep(c("T1", "T2", "T3", "T4"), length.out = n),
      levels = c("T1", "T2", "T3", "T4")
    )
    data$initial_t_stage_simple <- data$initial_t_stage
    data$initial_overall_stage <- factor(
      rep(c("1", "2A", "2B", "3A"), length.out = n)
    )
    data$optic_nerve <- factor(rep(c("No", "No", "Yes", "No"), length.out = n))
    data$age_at_diagnosis <- round(pmin(85, pmax(35, rnorm(n, 65, 10))))
    data$age_at_diagnosis_general_pop_median <- factor(
      ifelse(data$age_at_diagnosis >= 63, "Older", "Younger"),
      levels = c("Younger", "Older")
    )
    data$initial_tumor_height <- round(rlnorm(n, log(4.5), 0.32), 3)
    data$initial_tumor_diameter <- round(runif(n, 6, 19), 3)
    data$final_tumor_height <- pmax(
      0.5,
      round(data$initial_tumor_height + rnorm(n, -1.1, 0.9), 3)
    )
    data$last_height <- data$final_tumor_height
    data$height_change <- data$final_tumor_height - data$initial_tumor_height

    data$recurrence_event <- event_pattern
    data$recurrence1 <- ifelse(event_pattern == 1L, "Y", "N")
    data$tt_recurrence_months <- ifelse(
      event_pattern == 1L,
      sample(seq(12, 108, by = 6), n, replace = TRUE),
      sample(seq(72, 240, by = 6), n, replace = TRUE)
    )
    data$mets_event <- mets_pattern
    data$mets_progression <- ifelse(data$mets_event == 1L, "Y", "N")
    data$tt_mets_months <- ifelse(
      data$mets_event == 1L,
      sample(seq(18, 114, by = 6), n, replace = TRUE),
      sample(seq(72, 240, by = 6), n, replace = TRUE)
    )
    data$death_event <- death_pattern
    data$melanoma_death_event <- death_pattern
    data$competing_death_event <- 0L
    data$tt_death_months <- ifelse(
      death_pattern == 1L,
      sample(seq(24, 120, by = 6), n, replace = TRUE),
      sample(seq(84, 240, by = 6), n, replace = TRUE)
    )
    data$tt_death_years <- data$tt_death_months / 12
    data$tt_pfs_months <- pmin(data$tt_mets_months, data$tt_death_months)
    data$pfs_event <- as.integer(data$mets_event == 1L | data$death_event == 1L)

    data$biopsy1_gep <- factor(
      rep(c("Class 1", "Class 2", "Class 1", "Class 2"), length.out = n)
    )
    data$gep_class_simple <- data$biopsy1_gep
    data$prame_status <- factor(rep(c("Negative", "Positive"), length.out = n))
    data$gep12_prame_status <- data$prame_status
    data$retinopathy_burden_event <- rep(c(1L, 0L, 0L, 1L, 0L), length.out = n)
    data$srd_burden_event <- rep(c(0L, 1L, 0L, 0L, 1L), length.out = n)
    data$nvg_burden_event <- rep(c(0L, 0L, 1L, 0L, 0L, 0L), length.out = n)

    data
  })
}

# This fixture is deliberately separate from create_test_dataset(). The latter
# mirrors the date-heavy input contract used by Objective 0 and several unit
# tests; this fixture is the small, data-free contract used by portable CI.
# Values are rounded synthetic proportions inspired by the study design, not
# copied observations or estimates from the clinical workbook.
SYNTHETIC_CI_FIXTURE_VERSION <- "2026-08-06.1"
SYNTHETIC_CI_FIXTURE_SEED <- 20260806L
SYNTHETIC_CI_TREATMENT_LEVELS <- c("PBT", "GKSRS")
SYNTHETIC_CI_COHORT_LEVELS <- c("full", "restricted", "gksrs_only")
SYNTHETIC_CI_GEP_LEVELS <- c("Class 1", "Class 2")

synthetic_ci_required_columns <- function() {
  c(
    "treatment_group", "consort_group", "synthetic_cohort",
    "gep_class_simple", "biopsy1_gep", "biopsy1_gep_mfs", "biopsy1_gep_mss", "prame_status",
    "gep12_prame_status", "gep_validation_set", "tt_mets_months",
    "mets_event", "tt_death_months", "tt_death_years",
    "melanoma_death_event", "competing_death_event",
    "expected_mfs_5yr", "expected_mss_5yr",
    "mfs_analysis_eligible", "mss_analysis_eligible", "initial_tumor_height",
    "age_at_diagnosis", "sex"
  )
}

#' Generate the small deterministic cohort used by portable CI.
#'
#' The generator has no filesystem or clinical-data dependency. It intentionally
#' omits patient identifiers, calendar dates, and free-text fields while still
#' exercising both treatments, all supported cohort labels, missing values,
#' censoring, sparse groups, GEP/PRAME values, and one-arm GKSRS-only data.
create_synthetic_ci_dataset <- function(n = 48L, seed = SYNTHETIC_CI_FIXTURE_SEED) {
  if (length(n) != 1L || is.na(n) || n < 24L || n != as.integer(n)) {
    stop("n must be one integer of at least 24 rows.", call. = FALSE)
  }
  if (length(seed) != 1L || is.na(seed)) {
    stop("seed must be one non-missing value.", call. = FALSE)
  }

  withr::with_seed(seed, {
    n <- as.integer(n)
    synthetic_cohort <- factor(
      rep(SYNTHETIC_CI_COHORT_LEVELS, length.out = n),
      levels = SYNTHETIC_CI_COHORT_LEVELS
    )

    treatment_group <- sample(
      SYNTHETIC_CI_TREATMENT_LEVELS,
      size = n,
      replace = TRUE,
      prob = c(0.42, 0.58)
    )
    # Guarantee coverage for the two comparison arms and a one-arm cohort.
    treatment_group[seq_len(min(4L, n))] <- c("PBT", "GKSRS", "GKSRS", "PBT")
    treatment_group[synthetic_cohort == "gksrs_only"] <- "GKSRS"
    treatment_group <- factor(treatment_group, levels = SYNTHETIC_CI_TREATMENT_LEVELS)

    gep_class <- sample(
      SYNTHETIC_CI_GEP_LEVELS,
      size = n,
      replace = TRUE,
      prob = c(0.62, 0.38)
    )
    gep_class[seq_len(min(2L, n))] <- SYNTHETIC_CI_GEP_LEVELS
    gep_class <- factor(gep_class, levels = SYNTHETIC_CI_GEP_LEVELS)

    mets_event <- rbinom(n, size = 1L, prob = 0.28)
    mets_event[seq_len(min(2L, n))] <- c(1L, 0L)
    tt_mets_months <- sample(c(18, 30, 42, 54, 66, 78, 96), n, replace = TRUE)
    tt_mets_months[mets_event == 1L] <- sample(c(18, 30, 42, 54), sum(mets_event == 1L), replace = TRUE)

    death_event <- rbinom(n, size = 1L, prob = 0.16)
    death_event[seq_len(min(2L, n))] <- c(1L, 0L)
    tt_death_months <- sample(c(24, 36, 48, 60, 72, 84, 108), n, replace = TRUE)
    tt_death_months[death_event == 1L] <- sample(c(24, 36, 48, 60), sum(death_event == 1L), replace = TRUE)

    expected_mfs <- ifelse(gep_class == "Class 1", 0.82, 0.58)
    expected_mss <- ifelse(gep_class == "Class 1", 0.91, 0.70)
    expected_mfs <- as.numeric(expected_mfs)
    expected_mss <- as.numeric(expected_mss)

    prame_status <- sample(c("Negative", "Positive"), n, replace = TRUE, prob = c(0.55, 0.45))
    prame_status[c(9L, min(27L, n))] <- NA_character_

    tibble::tibble(
      treatment_group = treatment_group,
      consort_group = treatment_group,
      synthetic_cohort = synthetic_cohort,
      gep_class_simple = gep_class,
      biopsy1_gep = factor(as.character(gep_class), levels = SYNTHETIC_CI_GEP_LEVELS),
      biopsy1_gep_mfs = expected_mfs,
      biopsy1_gep_mss = expected_mss,
      prame_status = factor(prame_status, levels = c("Negative", "Positive")),
      gep12_prame_status = factor(prame_status, levels = c("Negative", "Positive")),
      gep_validation_set = rep("Eligible", n),
      tt_mets_months = as.numeric(tt_mets_months),
      mets_event = as.integer(mets_event),
      tt_death_months = as.numeric(tt_death_months),
      tt_death_years = as.numeric(tt_death_months / 12),
      melanoma_death_event = as.integer(death_event),
      competing_death_event = as.integer(rep(0L, n)),
      expected_mfs_5yr = expected_mfs,
      expected_mss_5yr = expected_mss,
      mfs_analysis_eligible = TRUE,
      mss_analysis_eligible = TRUE,
      initial_tumor_height = round(rlnorm(n, meanlog = log(3.8), sdlog = 0.28), 1),
      age_at_diagnosis = round(rnorm(n, mean = 65, sd = 8), 0),
      sex = factor(
        sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.48, 0.52)),
        levels = c("Female", "Male")
      ),
      mfs_event_5yr = as.integer(tt_mets_months <= 60 & mets_event == 1L),
      mss_event_5yr = as.integer(tt_death_months <= 60 & death_event == 1L)
    ) %>%
      dplyr::mutate(
        initial_tumor_height = dplyr::if_else(
          dplyr::row_number() %in% c(7L, min(31L, n)),
          NA_real_,
          initial_tumor_height
        )
      ) %>%
      structure(
        synthetic_fixture_version = SYNTHETIC_CI_FIXTURE_VERSION,
        synthetic_fixture_seed = seed
      )
  })
}
