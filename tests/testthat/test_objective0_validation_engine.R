make_objective0_validation_dataset <- function() {
    tibble::tibble(
        id = 1:3,
        treatment_group = factor(c("PBT", "GKSRS", "PBT"), levels = c("PBT", "GKSRS")),
        age_at_diagnosis = c(55, 63, 70),
        age_at_diagnosis_binned = factor(c("Younger", "Older", "Older")),
        age_at_diagnosis_general_pop_median = factor(c("Younger", "Older", "Older")),
        sex = factor(c("Female", "Male", "Female"), levels = c("Female", "Male")),
        location = factor(c("Choroidal", "Ciliary Body", "Choroidal"), levels = c("Choroidal", "Ciliary Body")),
        initial_tumor_height = c(5, 6, 7),
        initial_tumor_diameter = c(12, 14, 16),
        initial_vision = c(0.1, 0.2, 0.3),
        last_vision = c(0.0, 0.4, 0.3),
        recurrence1_pretreatment_vision = c(NA_real_, 0.4, NA_real_),
        initial_t_stage_simple = factor(c("T1", "T2", "T2"), levels = c("T1", "T2", "T3", "T4")),
        recurrence1 = factor(c("No", "Yes", "No"), levels = c("No", "Yes")),
        recurrence2 = factor(c("No", "No", "No"), levels = c("No", "Yes")),
        recurrence2_date = as.Date(c(NA, NA, NA)),
        mets_progression = factor(c("No", "No", "Yes"), levels = c("No", "Yes")),
        dod = as.Date(c(NA, NA, "2022-08-01")),
        last_known_alive_date = as.Date(c("2025-02-01", "2025-02-15", "2025-03-01")),
        last_known_alive_source = c("last_height_date", "date_diagnosis", "dod"),
        treatment_date = as.Date(c("2020-01-01", "2020-01-15", "2020-02-01")),
        date_diagnosis = as.Date(c("2019-12-15", "2020-01-01", "2020-01-20")),
        dob = as.Date(c("1965-01-01", "1960-01-01", "1955-01-01")),
        initial_tumor_height_binned = factor(c("<=10", "<=10", "<=10")),
        initial_tumor_diameter_binned = factor(c("<=20", "<=20", "<=20")),
        initial_stage_binary = factor(c("Stage I-III", "Stage I-III", "Stage I-III"), levels = c("Stage I-III", "Stage IV")),
        gep_class_simple = factor(c("Class 1", "Class 2", "Class 1"), levels = c("Class 1", "Class 2", "GEP Failed/Indeterminate", "GEP Not Tested")),
        prame_status = factor(c("Negative", "Positive", "Negative"), levels = c("Negative", "Positive", "Unknown", "Not Available")),
        gep12_prame_status = factor(c("Negative", "Positive", "Negative"), levels = c("Negative", "Positive")),
        recurrence1_treatment_clean = factor(c(NA, "GKSRS", NA), levels = c("GKSRS", "Enucleation", "TTT")),
        biopsy1_gep = factor(
            c("Class 1 PRAME Negative", "Class 2 PRAME Positive", "Class 1 PRAME Negative"),
            levels = c(
                "Class 1 PRAME Negative",
                "Class 1 PRAME Positive",
                "Class 2 PRAME Negative",
                "Class 2 PRAME Positive",
                "GEP Failed/Indeterminate",
                "GEP Not Tested"
            )
        ),
        biopsy1_gep_mfs = c(0.95, 0.55, 0.90),
        biopsy1_gep_mss = c(0.96, 0.60, 0.92),
        tt_recurrence_months = c(12, 18, 24),
        tt_mets_months = c(20, 22, 10),
        tt_death_months = c(40, 36, 30),
        tt_death_years = c(3.3, 3.0, 2.5),
        tt_recurrence_months_analysis = c(12, 18, 24),
        tt_mets_months_analysis = c(20, 22, 10),
        tt_death_months_analysis = c(40, 36, 30),
        tt_pfs_months = c(12, 18, 24),
        tt_pfs_months_analysis = c(12, 18, 24),
        tt_pfs2_months = c(
            NA_real_,
            lubridate::time_length(lubridate::interval(as.Date("2021-08-01"), as.Date("2025-02-15")), "months"),
            NA_real_
        ),
        tt_pfs2_years = c(
            NA_real_,
            lubridate::time_length(lubridate::interval(as.Date("2021-08-01"), as.Date("2025-02-15")), "years"),
            NA_real_
        ),
        recurrence_event = c(0L, 1L, 0L),
        mets_event = c(0L, 0L, 1L),
        death_event = c(0, 0, 1),
        pfs_event = c(0L, 1L, 1L),
        melanoma_death_event = c(0L, 0L, 1L),
        competing_death_event = c(0L, 0L, 0L),
        pfs2_event = c(NA_integer_, 0L, NA_integer_),
        recurrence1_treatment = c(NA_character_, "GKSRS", NA_character_),
        recurrence1_treatment_date = as.Date(c(NA, "2021-08-01", NA)),
        retinopathy = c("N", "Y", "N"),
        nvg = c("N", "N", "Y"),
        srd = c("Y", "N", "N"),
        retinopathy_burden_event = c(0L, 1L, 0L),
        nvg_burden_event = c(0L, 0L, 1L),
        srd_burden_event = c(1L, 0L, 0L),
        height_change = c(-1, 0.5, -0.2),
        vision_change = c(0.1, -0.2, 0),
        consort_group = c("eligible_both", "eligible_both", "eligible_both"),
        optic_nerve = factor(c("No", "No", "No"), levels = c("No", "Yes")),
        gep_validation_set = c("Eligible", "Eligible", "Eligible"),
        mfs_analysis_eligible = c(TRUE, TRUE, TRUE),
        mss_analysis_eligible = c(TRUE, TRUE, TRUE),
        expected_mfs_5yr = c(0.95, 0.55, 0.90),
        expected_mfs_7yr = c(0.95, 0.55, 0.90)^(7 / 5),
        expected_mfs_10yr = c(0.95, 0.55, 0.90)^(10 / 5),
        expected_mss_5yr = c(0.96, 0.60, 0.92),
        expected_mss_7yr = c(0.96, 0.60, 0.92)^(7 / 5),
        expected_mss_10yr = c(0.96, 0.60, 0.92)^(10 / 5),
        predicted_mfs_risk_5yr = c(0.05, 0.45, 0.10),
        predicted_mfs_risk_7yr = 1 - c(0.95, 0.55, 0.90)^(7 / 5),
        predicted_mfs_risk_10yr = 1 - c(0.95, 0.55, 0.90)^(10 / 5),
        predicted_mss_risk_5yr = c(0.04, 0.40, 0.08),
        predicted_mss_risk_7yr = 1 - c(0.96, 0.60, 0.92)^(7 / 5),
        predicted_mss_risk_10yr = 1 - c(0.96, 0.60, 0.92)^(10 / 5),
        mfs_event_5yr = c(0L, 0L, 1L),
        mfs_event_7yr = c(0L, 0L, 1L),
        mfs_event_10yr = c(0L, 0L, 1L),
        mss_event_5yr = c(0L, 0L, 1L),
        mss_event_7yr = c(0L, 0L, 1L),
        mss_event_10yr = c(0L, 0L, 1L),
        event_type_mfs_5yr = c(0L, 0L, 1L),
        event_type_mfs_7yr = c(0L, 0L, 1L),
        event_type_mfs_10yr = c(0L, 0L, 1L),
        event_type_mss_5yr = c(0L, 0L, 1L),
        event_type_mss_7yr = c(0L, 0L, 1L),
        event_type_mss_10yr = c(0L, 0L, 1L),
        tt_mfs_5yr = c(20, 22, 10),
        tt_mfs_7yr = c(20, 22, 10),
        tt_mfs_10yr = c(20, 22, 10),
        tt_mss_5yr = c(3.3, 3.0, 2.5),
        tt_mss_7yr = c(3.3, 3.0, 2.5),
        tt_mss_10yr = c(3.3, 3.0, 2.5)
    )
}

test_that("structured validation result treats warnings as non-blocking", {
    validation_result <- validate_processing_pipeline(
        make_objective0_validation_dataset(),
        stop_on_failure = FALSE
    )

    expect_true(validation_result$success)
    expect_true("validation_findings" %in% names(validation_result))
    expect_true(all(c("check_id", "scope", "cohort", "severity", "status", "metric", "value", "message") %in% names(validation_result$validation_findings)))
    expect_true(any(validation_result$validation_findings$severity == "warning"))
})

test_that("structured validation result blocks hard errors", {
    duplicate_id_data <- make_objective0_validation_dataset()
    duplicate_id_data$id[3] <- duplicate_id_data$id[1]

    validation_result <- validate_processing_pipeline(
        duplicate_id_data,
        stop_on_failure = FALSE
    )

    expect_false(validation_result$success)
    expect_true(validation_result$has_hard_errors)
    expect_true(any(validation_result$validation_findings$check_id == "duplicate_patient_ids"))
    expect_true(any(validation_result$validation_findings$severity == "hard_error"))
})

test_that("endpoint chronology violations are hard errors, not silent clamps", {
    invalid_chronology_data <- make_objective0_validation_dataset()
    invalid_chronology_data$tt_recurrence_months[2] <- -1
    invalid_chronology_data$tt_recurrence_months_analysis[2] <- -1
    invalid_chronology_data$tt_pfs_months[2] <- -1
    invalid_chronology_data$tt_pfs_months_analysis[2] <- -1

    validation_result <- validate_processing_pipeline(
        invalid_chronology_data,
        stop_on_failure = FALSE
    )

    chronology_findings <- validation_result$validation_findings %>%
        dplyr::filter(.data$check_id == "endpoint_event_times_nonnegative")
    chronology_details <- validation_result$detail_tables %>%
        dplyr::filter(.data$detail_sheet == "Endpoint_Chronology_Failures")

    expect_false(validation_result$success)
    expect_equal(chronology_findings$severity[[1]], "hard_error")
    expect_equal(chronology_findings$status[[1]], "fail")
    expect_true(any(chronology_details$field_name == "tt_recurrence_months"))
    expect_true(any(chronology_details$field_name == "tt_pfs_months_analysis"))
})

test_that("downstream objective input contract catches missing and invalid inputs", {
    invalid_contract_data <- make_objective0_validation_dataset()
    invalid_contract_data$predicted_mfs_risk_5yr[1] <- 1.5
    invalid_contract_data$event_type_mfs_5yr[2] <- 4L
    invalid_contract_data$pfs_event[3] <- NA_integer_
    invalid_contract_data$recurrence_event <- NULL

    validation_result <- validate_processing_pipeline(
        invalid_contract_data,
        stop_on_failure = FALSE
    )

    contract_findings <- validation_result$validation_findings %>%
        dplyr::filter(.data$finding_group == "downstream_input_contract")
    invalid_details <- validation_result$detail_tables %>%
        dplyr::filter(.data$detail_sheet == "Downstream_Input_Invalid")
    missing_details <- validation_result$detail_tables %>%
        dplyr::filter(.data$detail_sheet == "Downstream_Input_Missing")

    expect_false(validation_result$success)
    expect_true(any(contract_findings$check_id == "downstream_objective_inputs_present" & contract_findings$status == "fail"))
    expect_true(any(contract_findings$check_id == "downstream_objective_inputs_valid" & contract_findings$status == "fail"))
    expect_true(any(invalid_details$variable_name == "predicted_mfs_risk_5yr"))
    expect_true(any(invalid_details$variable_name == "event_type_mfs_5yr"))
    expect_true(any(invalid_details$variable_name == "pfs_event"))
    expect_true(any(missing_details$variable_name == "recurrence_event"))
})

test_that("Objective 2 optic nerve contract allows missing full-cohort descriptors", {
    contract_data <- make_objective0_validation_dataset()
    contract_data$optic_nerve[1] <- NA

    validation_result <- validate_processing_pipeline(
        contract_data,
        stop_on_failure = FALSE
    )
    invalid_details <- validation_result$detail_tables %>%
        dplyr::filter(.data$detail_sheet == "Downstream_Input_Invalid")
    optic_nerve_missing_issue <- "variable_name" %in% names(invalid_details) &&
        "issue_type" %in% names(invalid_details) &&
        any(
            invalid_details$variable_name == "optic_nerve" &
                invalid_details$issue_type == "missing_required_value"
        )

    expect_true(validation_result$success)
    expect_false(optic_nerve_missing_issue)

    contract_data$optic_nerve <- as.character(contract_data$optic_nerve)
    contract_data$optic_nerve[1] <- "Unknown"
    invalid_result <- validate_processing_pipeline(
        contract_data,
        stop_on_failure = FALSE
    )
    invalid_details <- invalid_result$detail_tables %>%
        dplyr::filter(.data$detail_sheet == "Downstream_Input_Invalid")

    expect_false(invalid_result$success)
    expect_true(any(
        invalid_details$variable_name == "optic_nerve" &
            invalid_details$issue_type == "invalid_domain_value"
    ))
})

test_that("full-cohort-only special cases are audited and barred from subcohorts", {
    special_case_data <- make_objective0_validation_dataset()
    special_case_data$consort_group[1] <- CONSORT_GROUP_FULL_ONLY_SPECIAL_CASE
    special_case_data$cohort_assignment_special_case <- c(IRIS_OPTIC_NERVE_SPECIAL_CASE, NA_character_, NA_character_)
    special_case_data$cohort_assignment_note <- c(
        "Iris tumor: raw optic_nerve=N/A interpreted as non-abutment/not applicable; retained in full cohort only.",
        NA_character_,
        NA_character_
    )

    full_result <- collect_single_cohort_validation(
        special_case_data,
        "uveal_melanoma_full_cohort"
    )
    full_findings <- full_result$validation_findings %>%
        dplyr::filter(.data$check_id == "full_cohort_only_special_cases")
    full_details <- full_result$detail_tables %>%
        dplyr::filter(.data$detail_sheet == "Full_Cohort_Only_Special_Cases")

    expect_true(full_result$success)
    expect_equal(full_findings$status[[1]], "info")
    expect_true(any(full_details$id == 1))
    expect_true(any(full_details$cohort_assignment_special_case == IRIS_OPTIC_NERVE_SPECIAL_CASE))

    restricted_result <- collect_single_cohort_validation(
        special_case_data,
        "uveal_melanoma_restricted_cohort"
    )
    restricted_findings <- restricted_result$validation_findings %>%
        dplyr::filter(.data$check_id == "full_cohort_only_special_cases")

    expect_false(restricted_result$success)
    expect_equal(restricted_findings$severity[[1]], "hard_error")
    expect_equal(restricted_findings$status[[1]], "fail")
})

test_that("retired other and unresolved cohort states fail validation", {
    other_data <- make_objective0_validation_dataset()
    other_data$consort_group[1] <- "other"

    other_result <- collect_single_cohort_validation(
        other_data,
        "uveal_melanoma_full_cohort"
    )
    other_findings <- other_result$validation_findings %>%
        dplyr::filter(.data$check_id == "retired_other_consort_group_absent")

    expect_false(other_result$success)
    expect_equal(other_findings$severity[[1]], "hard_error")
    expect_equal(other_findings$status[[1]], "fail")

    unresolved_data <- make_objective0_validation_dataset()
    unresolved_data$consort_group[1] <- CONSORT_GROUP_UNCLASSIFIED_FIELDS
    unresolved_result <- collect_single_cohort_validation(
        unresolved_data,
        "uveal_melanoma_full_cohort"
    )
    unresolved_findings <- unresolved_result$validation_findings %>%
        dplyr::filter(.data$check_id == "unclassified_cohort_fields_absent")

    expect_false(unresolved_result$success)
    expect_equal(unresolved_findings$severity[[1]], "hard_error")
    expect_equal(unresolved_findings$status[[1]], "fail")
})

test_that("GEP registry uses eligibility labels rather than train/test split enforcement", {
    validation_result <- validate_processing_pipeline(
        make_objective0_validation_dataset(),
        stop_on_failure = FALSE
    )

    contract_missing <- validation_result$detail_tables %>%
        dplyr::filter(.data$detail_sheet == "Downstream_Input_Missing")
    missing_variable_names <- if ("variable_name" %in% names(contract_missing)) {
        contract_missing$variable_name
    } else {
        character()
    }

    expect_true(validation_result$success)
    expect_false(any(grepl("train|test", missing_variable_names, ignore.case = TRUE)))
})

test_that("last_known_alive_source is treated as provenance text, not a date field", {
    validation_result <- validate_processing_pipeline(
        make_objective0_validation_dataset(),
        stop_on_failure = FALSE
    )

    date_storage_finding <- validation_result$validation_findings %>%
        dplyr::filter(.data$check_id == "date_columns_are_date_like")

    expect_equal(date_storage_finding$status[[1]], "pass")
    expect_false(grepl("last_known_alive_source", date_storage_finding$value[[1]] %||% ""))
})

test_that("minor treatment-before-diagnosis gaps are warnings, not hard errors", {
    minor_gap_data <- make_objective0_validation_dataset()
    minor_gap_data$treatment_date[2] <- as.Date("2019-12-30")
    minor_gap_data$date_diagnosis[2] <- as.Date("2020-01-01")

    validation_result <- validate_processing_pipeline(
        minor_gap_data,
        stop_on_failure = FALSE
    )

    expect_true(validation_result$success)
    expect_true(any(validation_result$validation_findings$check_id == "treatment_before_diagnosis_minor_gap"))
    expect_false(any(
        validation_result$validation_findings$check_id == "treatment_after_diagnosis" &
            validation_result$validation_findings$status == "fail"
    ))
})

test_that("major treatment-before-diagnosis gaps remain hard errors", {
    major_gap_data <- make_objective0_validation_dataset()
    major_gap_data$treatment_date[2] <- as.Date("2019-10-01")
    major_gap_data$date_diagnosis[2] <- as.Date("2020-01-01")

    validation_result <- validate_processing_pipeline(
        major_gap_data,
        stop_on_failure = FALSE
    )

    expect_false(validation_result$success)
    expect_true(any(
        validation_result$validation_findings$check_id == "treatment_after_diagnosis" &
            validation_result$validation_findings$status == "fail"
    ))
})

test_that("Objective 0 validates Objective 2 toxicity endpoint burden fields", {
    invalid_toxicity_data <- make_objective0_validation_dataset()
    invalid_toxicity_data$retinopathy[1] <- "Unknown"
    invalid_toxicity_data$nvg_burden_event[2] <- NA_integer_
    invalid_toxicity_data$srd_burden_event[3] <- 1L

    validation_result <- validate_processing_pipeline(
        invalid_toxicity_data,
        stop_on_failure = FALSE
    )

    toxicity_findings <- validation_result$validation_findings %>%
        dplyr::filter(.data$finding_group == "objective2_toxicity_endpoints")

    expect_false(validation_result$success)
    expect_true(any(toxicity_findings$check_id == "objective2_retinopathy_source_valid" & toxicity_findings$status == "fail"))
    expect_true(any(toxicity_findings$check_id == "objective2_nvg_burden_event_binary_complete" & toxicity_findings$status == "fail"))
    expect_true(any(toxicity_findings$check_id == "objective2_srd_burden_event_matches_source" & toxicity_findings$status == "fail"))
})

test_that("Objective 1 endpoint invariants preserve local recurrence/death PFS", {
    invariant_data <- make_objective0_validation_dataset()
    invariant_data$mets_event[1] <- 1L
    invariant_data$pfs_event[1] <- 1L

    validation_result <- validate_processing_pipeline(
        invariant_data,
        stop_on_failure = FALSE
    )

    invariant_details <- validation_result$detail_tables %>%
        dplyr::filter(.data$detail_sheet == "Objective1_Endpoint_Invariants")

    expect_false(validation_result$success)
    expect_true(any(invariant_details$field_name == "pfs_event"))
    expect_equal(invariant_details$expected_value[invariant_details$field_name == "pfs_event"][[1]], "0")
})

test_that("Objective 3 PFS-2 derivation contract handles death-before-second-recurrence censoring", {
    pfs2_data <- make_objective0_validation_dataset()
    pfs2_data$recurrence2[2] <- "Yes"
    pfs2_data$recurrence2_date[2] <- as.Date("2023-03-01")
    pfs2_data$dod[2] <- as.Date("2022-08-01")
    pfs2_data$pfs2_event[2] <- 0L
    pfs2_data$tt_pfs2_months[2] <- lubridate::time_length(lubridate::interval(as.Date("2021-08-01"), as.Date("2022-08-01")), "months")
    pfs2_data$tt_pfs2_years[2] <- lubridate::time_length(lubridate::interval(as.Date("2021-08-01"), as.Date("2022-08-01")), "years")

    pass_result <- validate_objective3_pfs2_derivation_contract(pfs2_data, "unit")
    expect_true(all(pass_result$findings$status == "pass"))

    pfs2_data$pfs2_event[2] <- 1L
    fail_result <- validate_objective3_pfs2_derivation_contract(pfs2_data, "unit")

    expect_equal(fail_result$findings$status[[1]], "fail")
    expect_true(any(fail_result$details$field_name == "pfs2_event"))
})

test_that("Objective 4 GEP derivation contract catches formula and split-label drift", {
    gep_data <- make_objective0_validation_dataset()
    gep_data$expected_mfs_7yr[1] <- gep_data$biopsy1_gep_mfs[1]
    gep_data$gep_validation_set[2] <- "Training"

    validation_result <- validate_processing_pipeline(
        gep_data,
        stop_on_failure = FALSE
    )

    gep_details <- validation_result$detail_tables %>%
        dplyr::filter(.data$detail_sheet == "Objective4_GEP_Derivation")

    expect_false(validation_result$success)
    expect_true(any(gep_details$field_name == "expected_mfs_7yr"))
    expect_true(any(gep_details$field_name == "gep_validation_set"))
})

test_that("Objective 0 contract registries do not drift from endpoint mappings", {
    contract_pairs <- OBJECTIVE0_DOWNSTREAM_INPUT_CONTRACT %>%
        dplyr::select("objective_id", "variable_name")

    expect_equal(nrow(contract_pairs), nrow(dplyr::distinct(contract_pairs)))
    expect_true(all(OBJECTIVE2_TOXICITY_ENDPOINTS$source_field %in% contract_pairs$variable_name[contract_pairs$objective_id == "objective2"]))
    expect_true(all(OBJECTIVE2_TOXICITY_ENDPOINTS$analysis_field %in% contract_pairs$variable_name[contract_pairs$objective_id == "objective2"]))
    expect_true(all(OBJECTIVE3_PFS2_DERIVATION_CONTRACT$source_fields %in% contract_pairs$variable_name[contract_pairs$objective_id == "objective3"]))
    expect_true(all(OBJECTIVE3_PFS2_DERIVATION_CONTRACT$derived_fields %in% contract_pairs$variable_name[contract_pairs$objective_id == "objective3"]))

    gep_contract_fields <- unique(c(
        OBJECTIVE4_GEP_DERIVATION_CONTRACT$source_probability_field,
        OBJECTIVE4_GEP_DERIVATION_CONTRACT$expected_survival_field,
        OBJECTIVE4_GEP_DERIVATION_CONTRACT$predicted_risk_field,
        OBJECTIVE4_GEP_DERIVATION_CONTRACT$event_field,
        OBJECTIVE4_GEP_DERIVATION_CONTRACT$event_type_field,
        OBJECTIVE4_GEP_DERIVATION_CONTRACT$time_field,
        OBJECTIVE4_GEP_DERIVATION_CONTRACT$eligibility_field,
        "gep_validation_set"
    ))
    expect_true(all(gep_contract_fields %in% contract_pairs$variable_name[contract_pairs$objective_id == "objective4"]))
})

test_that("config_constants remains the only public config source entry point", {
    load_all_text <- readLines(here::here("scripts", "load_all.R"), warn = FALSE)
    config_source_lines <- grep("source\\(here\\(\"scripts\", \"utils\", \"config\",", load_all_text, value = TRUE)
    expect_length(config_source_lines, 0)
    expect_true(any(grepl("config_constants\\.R", load_all_text)))

    config_constants_text <- readLines(here::here("scripts", "utils", "config_constants.R"), warn = FALSE)
    expect_false(any(grepl("scripts.*utils.*config", config_constants_text)))
    expect_true(any(grepl("scripts.*config", config_constants_text)))
    expect_true(dir.exists(here::here("scripts", "config")))
    expect_false(any(file.exists(list.files(here::here("scripts", "utils", "config"), full.names = TRUE))))

    required_objects <- c(
        "PROJECT_ROOT", "INPUT_FILENAME", "OBJECTIVE0_GLOBAL_REQUIRED_VARIABLES",
        "OBJECTIVE0_DERIVED_OUTPUT_MANIFEST", "OBJECTIVE0_DOWNSTREAM_INPUT_CONTRACT",
        "OBJECTIVE2_TOXICITY_ENDPOINTS", "OBJECTIVE3_PFS2_DERIVATION_CONTRACT",
        "OBJECTIVE4_GEP_DERIVATION_CONTRACT", "GEP_VALIDATION_TIMEPOINTS",
        "STANDARD_TABLE_LABELS"
    )
    expect_true(all(vapply(required_objects, exists, logical(1), inherits = TRUE)))
})

test_that("Objective 0 validation artifacts are written into 00_General", {
    output_root <- tempfile("objective0-validation-bundle-")
    dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
    withr::defer(unlink(output_root, recursive = TRUE, force = TRUE), envir = parent.frame())

    output_dirs <- list(
        full_cohort = list(
            baseline_characteristics = file.path(output_root, "uveal_full", "00_General", "baseline_characteristics")
        ),
        restricted_cohort = list(
            baseline_characteristics = file.path(output_root, "uveal_restricted", "00_General", "baseline_characteristics")
        ),
        gksrs_only_cohort = list(
            baseline_characteristics = file.path(output_root, "gksrs", "00_General", "baseline_characteristics")
        )
    )

    validation_result <- validate_processing_pipeline(
        make_objective0_validation_dataset(),
        stop_on_failure = FALSE
    )
    validation_result$metadata <- list(
        objective0_dataset_mode = "recreate_from_raw",
        raw_data_dir = RAW_DATA_DIR
    )
    reconciliation_audit <- list(
        audit_summary = tibble::tibble(
            source_workbook = "unit.xlsx",
            id_column = "id",
            event_var = "mets_progression",
            date_var = "mets_progression_date",
            records_with_present_date = 1L,
            records_marked_event_yes_after = 1L,
            n_event_set_to_yes = 0L,
            n_event_set_to_no_missing_date = 0L,
            n_rows_reconciled = 0L
        ),
        audit_rows = empty_event_date_audit_rows(),
        manual_date_corrections = tibble::tibble(
            source_workbook = "unit.xlsx",
            id_column = "id",
            study_id = "11",
            column_name = "date_diagnosis",
            original_value = "2020-01-01",
            corrected_value = "2010-01-01",
            correction_reason = "Unit-test audit row",
            action_taken = "manual_source_date_correction"
        )
    )

    written_paths <- write_objective0_validation_artifacts(
        validation_result = validation_result,
        output_dirs = output_dirs,
        reconciliation_audit = reconciliation_audit
    )

    expect_true(file.exists(written_paths$full_cohort$summary_path))
    expect_true(file.exists(written_paths$full_cohort$bundle_path))

    bundle_sheets <- readxl::excel_sheets(written_paths$full_cohort$bundle_path)
    expect_true(all(c(
        "Validation_Summary",
        "Validation_Provenance",
        "Validation_Findings",
        "Critical_Variable_Checks",
        "Factor_Level_Checks",
        "Cohort_Rule_Checks",
        "Data_Quality_Checks",
        "Reconciliation_Summary",
        "Manual_Date_Corrections"
    ) %in% bundle_sheets))

    provenance <- readxl::read_xlsx(written_paths$full_cohort$bundle_path, sheet = "Validation_Provenance")
    expect_true(any(provenance$field == "objective0_dataset_mode"))
})

test_that("reload audit rehydration preserves reconciliation and manual correction sheets", {
    output_root <- tempfile("objective0-reload-audit-")
    dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
    withr::defer(unlink(output_root, recursive = TRUE, force = TRUE), envir = parent.frame())

    output_dirs <- list(
        full_cohort = list(
            baseline_characteristics = file.path(output_root, "uveal_full", "00_General", "baseline_characteristics")
        )
    )
    general_dir <- dirname(output_dirs$full_cohort$baseline_characteristics)
    dir.create(general_dir, recursive = TRUE, showWarnings = FALSE)

    reconciliation_summary <- tibble::tibble(
        source_workbook = "unit.xlsx",
        id_column = "id",
        event_var = "recurrence1",
        date_var = "recurrence1_date",
        records_with_present_date = 1L,
        records_marked_event_yes_after = 1L,
        n_event_set_to_yes = 1L,
        n_event_set_to_no_missing_date = 0L,
        n_rows_reconciled = 1L
    )
    manual_date_corrections <- tibble::tibble(
        source_workbook = "unit.xlsx",
        id_column = "id",
        study_id = "42",
        column_name = "date_diagnosis",
        original_value = "2020-01-01",
        corrected_value = "2010-01-01",
        correction_reason = "Unit-test persisted correction",
        confidence_tier = "high",
        supporting_columns = "last_followup",
        supporting_values = "2011-01-01",
        original_support_gap_days = 3650,
        corrected_support_gap_days = 365,
        gap_improvement_days = 3285,
        action_taken = "manual_source_date_correction"
    )
    reconciled_changes <- empty_event_date_audit_rows() %>%
        dplyr::add_row(
            source_workbook = "unit.xlsx",
            id_column = "id",
            study_id = "42",
            row_index = 1L,
            event_var = "recurrence1",
            date_var = "recurrence1_date",
            original_event = "N",
            original_date = "2021-01-01",
            reconciled_event = "Y",
            reconciled_date = "2021-01-01",
            original_state = "event=N | date=2021-01-01",
            reconciled_state = "event=Y | date=2021-01-01",
            action_taken = "event_set_to_yes"
        )

    write_readable_xlsx(
        list(
            Audit_Metadata = tibble::tibble(generated_at = "unit"),
            Reconciliation_Summary = reconciliation_summary,
            Reconciled_Changes = reconciled_changes,
            Manual_Date_Corrections = manual_date_corrections
        ),
        file.path(general_dir, "full_cohort_event_data_reconcilitation.xlsx")
    )

    rehydrated <- rehydrate_objective0_audit_state(output_dirs)
    validation_result <- validate_processing_pipeline(
        make_objective0_validation_dataset(),
        stop_on_failure = FALSE
    )
    validation_result$metadata <- list(objective0_dataset_mode = "reload_existing_processed")
    validation_result <- append_validation_result_components(
        validation_result,
        findings = rehydrated$findings,
        detail_tables = rehydrated$details
    )

    written_paths <- write_objective0_validation_artifacts(
        validation_result = validation_result,
        output_dirs = output_dirs,
        rehydrated_audit = rehydrated$audit_by_cohort
    )

    expect_equal(nrow(rehydrated$findings), 0)
    expect_true(file.exists(written_paths$full_cohort$bundle_path))
    rehydrated_summary <- readxl::read_xlsx(written_paths$full_cohort$bundle_path, sheet = "Reconciliation_Summary")
    rehydrated_manual <- readxl::read_xlsx(written_paths$full_cohort$bundle_path, sheet = "Manual_Date_Corrections")
    expect_equal(rehydrated_summary$n_rows_reconciled[[1]], 1)
    expect_equal(rehydrated_manual$study_id[[1]], "42")
    expect_true(any(validation_result$detail_tables$detail_sheet == "Event_Date_Reconciliations"))
})

test_that("reload audit rehydration reports missing persisted workbooks explicitly", {
    output_root <- tempfile("objective0-missing-reload-audit-")
    dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
    withr::defer(unlink(output_root, recursive = TRUE, force = TRUE), envir = parent.frame())

    output_dirs <- list(
        full_cohort = list(
            baseline_characteristics = file.path(output_root, "uveal_full", "00_General", "baseline_characteristics")
        )
    )
    dir.create(dirname(output_dirs$full_cohort$baseline_characteristics), recursive = TRUE, showWarnings = FALSE)

    rehydrated <- rehydrate_objective0_audit_state(output_dirs)

    expect_true(any(rehydrated$findings$check_id == "objective0_reload_reconciliation_workbook_present"))
    expect_equal(rehydrated$findings$severity[[1]], "warning")
    expect_equal(rehydrated$findings$status[[1]], "warn")
})
