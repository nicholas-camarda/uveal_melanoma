#' Apply inclusion/exclusion criteria to create cohorts
#'
#' Filters and mutates the cleaned data to create three cohorts: full, restricted, and GKSRS-only.
#' Requires consort_group to be present in the data from `load_and_clean_data()`
#'
#' @param data Data frame. Cleaned patient-level data.
#'
#' @return A list with three tibbles: full, restricted, and gksrs-only cohorts
apply_criteria <- function(data) {
    logger::log_info("Applying inclusion/exclusion criteria to full cohort to generate restricted and GKSRS-only cohorts")

    removal_details <- dplyr::tibble(
        id = numeric(0),
        removal_reason = character(0),
        removal_step = character(0),
        consort_group = character(0),
        treatment_group = character(0),
        initial_overall_stage = character(0)
    )

    # Exclude Stage IV patients globally per spec (focus on localized treatment)
    total_before <- nrow(data)
    stage_iv_ids <- data$id[!is.na(data$initial_stage_binary) & data$initial_stage_binary == "Stage IV"]
    num_stage_iv <- length(stage_iv_ids)
    data_no_stage_iv <- data %>%
        filter(initial_stage_binary != "Stage IV" | is.na(initial_stage_binary))
    if (num_stage_iv > 0) {
        stage_iv_records <- data %>%
            filter(id %in% stage_iv_ids) %>%
            mutate(
                removal_reason = "Stage IV disease excluded per protocol",
                removal_step = "stage_iv_exclusion",
                consort_group = as.character(consort_group),
                treatment_group = as.character(treatment_group),
                initial_overall_stage = as.character(initial_overall_stage)
            ) %>%
            select(id, removal_reason, removal_step, consort_group, treatment_group, initial_overall_stage)

        removal_details <- dplyr::bind_rows(removal_details, stage_iv_records)
    }
    logger::log_info(sprintf("Stage IV exclusion applied: removed %d patients%s",
        num_stage_iv,
        if (num_stage_iv > 0) sprintf(" (IDs: %s)", paste(stage_iv_ids, collapse = ", ")) else ""
    ))
    logger::log_info(sprintf("Remaining after Stage IV exclusion: %d of %d patients", nrow(data_no_stage_iv), total_before))

    # Remove any specifically excluded IDs
    before_specific_excl <- nrow(data_no_stage_iv)
    data_after_specific <- data_no_stage_iv %>%
        filter(!(id %in% SPECIFIC_PATIENTS_TO_EXCLUDE) | is.na(id))
    num_specific_removed <- before_specific_excl - nrow(data_after_specific)
    if (num_specific_removed > 0) {
        specific_ids <- setdiff(data_no_stage_iv$id, data_after_specific$id)
        specific_records <- data_no_stage_iv %>%
            filter(id %in% specific_ids) %>%
            mutate(
                removal_reason = "Excluded per SPECIFIC_PATIENTS_TO_EXCLUDE configuration",
                removal_step = "manual_exclusion",
                consort_group = as.character(consort_group),
                treatment_group = as.character(treatment_group),
                initial_overall_stage = as.character(initial_overall_stage)
            ) %>%
            select(id, removal_reason, removal_step, consort_group, treatment_group, initial_overall_stage)

        removal_details <- dplyr::bind_rows(removal_details, specific_records)

        logger::log_info(sprintf(
            "SPECIFIC_PATIENTS_TO_EXCLUDE removed %d patients (IDs: %s)",
            num_specific_removed,
            paste(specific_ids, collapse = ", ")
        ))
    } else {
        logger::log_info("SPECIFIC_PATIENTS_TO_EXCLUDE removed 0 patients")
    }
    logger::log_info(sprintf("Remaining after specific exclusions: %d patients", nrow(data_after_specific)))

    # Remove records that still lack explicit analyzable cohort assignment.
    before_missing_filter <- nrow(data_after_specific)
    data_after_missing <- data_after_specific %>%
        filter(!is.na(consort_group)) %>%
        filter(consort_group != CONSORT_GROUP_UNCLASSIFIED_FIELDS) %>%
        filter(!is.na(treatment_group))
    num_missing_removed <- before_missing_filter - nrow(data_after_missing)
    if (num_missing_removed > 0) {
        missing_ids <- setdiff(data_after_specific$id, data_after_missing$id)
        missing_records <- data_after_specific %>%
            filter(id %in% missing_ids) %>%
            mutate(
                removal_reason = "Missing or unresolved cohort-defining fields",
                removal_step = "missing_cohort_fields",
                consort_group = as.character(consort_group),
                treatment_group = as.character(treatment_group),
                initial_overall_stage = as.character(initial_overall_stage)
            ) %>%
            select(id, removal_reason, removal_step, consort_group, treatment_group, initial_overall_stage)

        removal_details <- dplyr::bind_rows(removal_details, missing_records)

        logger::log_info(sprintf(
            "Missing/unresolved cohort-field filter removed %d patients (IDs: %s)",
            num_missing_removed,
            paste(missing_ids, collapse = ", ")
        ))
    } else {
        logger::log_info("No patients removed for missing or unresolved cohort-defining fields")
    }
    logger::log_info(sprintf("Remaining after consort/treatment filter: %d patients", nrow(data_after_missing)))

    total_removed <- nrow(removal_details)
    logger::log_info(sprintf("Total patients removed prior to cohort assignment: %d", total_removed))

    full_cohort <- data_after_missing %>%
        mutate(cohort = "All Patients")

    restricted_cohort <- full_cohort %>%
        filter(consort_group == CONSORT_GROUP_ELIGIBLE_BOTH) %>%
        mutate(cohort = "Restricted Cohort (Eligible for Both Treatments)")

    gksrs_only_cohort <- full_cohort %>%
        filter(consort_group == CONSORT_GROUP_GKSRS_ONLY) %>%
        mutate(cohort = "GKSRS-Only Cohort (Ineligible for PBT)")

    factored_filtered_data <- list(
        uveal_melanoma_full_cohort = full_cohort,
        uveal_melanoma_restricted_cohort = restricted_cohort,
        uveal_melanoma_gksrs_only_cohort = gksrs_only_cohort
    )

    logger::log_info(sprintf("Created %d cohorts", length(factored_filtered_data)))
    for (cohort in names(factored_filtered_data)) {
        logger::log_info(sprintf("Cohort '%s': %d patients", cohort, nrow(factored_filtered_data[[cohort]])))
    }

    # Data validation step now happens outside apply criteria
    return(list(
        cohorts = factored_filtered_data,
        removal_log = removal_details
    ))
}

#' Prepare factor levels for key variables
#'
#' Converts relevant variables to factors with specified levels and orderings for analysis and modeling.
#'
#' @param data Data frame. Patient-level data.
#'
#' @return A list with element `data` containing factored patient-level data
prepare_factor_levels <- function(data) {
    logger::log_info("Preparing factor levels for variables")

    data <- data %>%
        mutate(
            # Preserve original raw GEP text before any recoding for downstream flags
            biopsy1_gep_text_raw = as.character(biopsy1_gep),
            # Preserve original raw GEP values for accurate untested flag
            biopsy1_gep_original = biopsy1_gep,
            treatment_group = normalize_treatment_group_values(treatment_group),
            recurrence1 = factor(recurrence1, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            mets_progression = factor(mets_progression, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            treatment_group = factor(treatment_group, levels = TREATMENT_FACTOR_LEVELS),
            recurrence1_treatment_clean = coerce_to_factor_preserving_levels(recurrence1_treatment_clean),
            sex = factor(sex, levels = SEX_FACTOR_LEVELS, labels = SEX_FACTOR_LEVELS),
            location = factor(location,
                levels = c("Choroidal", "Ciliary_Body", "Cilio_Choroidal", "Conjunctival", "Irido_Ciliary", "Iris"),
                labels = c("Choroidal", "Ciliary Body", "Cilio-Choroidal", "Conjunctival", "Irido-Ciliary", "Iris"),
                ordered = FALSE
            ),
            optic_nerve = factor(optic_nerve, levels = YN_RAW_LEVELS, labels = c("No", "Yes")),
            internal_reflectivity = factor(internal_reflectivity,
                levels = c("Very_Low", "Low", "Low_Medium", "Medium", "Medium_High", "High", "Unknown"),
                labels = c("Very Low", "Low", "Low-Medium", "Medium", "Medium-High", "High", "Unknown"),
                ordered = FALSE
            ),
            # Force both Yes/No levels even if only one present in the data to stabilize tables
            srf = factor(as.character(srf), levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            op = factor(as.character(op), levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            symptoms = factor(as.character(symptoms), levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            vision_loss_blurred_vision = factor(as.character(vision_loss_blurred_vision), levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            visual_field_defect = factor(as.character(visual_field_defect), levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            flashes_photopsia = factor(as.character(flashes_photopsia), levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            floaters = factor(as.character(floaters), levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            pain = factor(as.character(pain), levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            initial_overall_stage = factor(initial_overall_stage, levels = c("1", "2A", "2B", "3A", "3B", "3C", "4"), ordered = FALSE),
            initial_stage_binary = factor(ifelse(initial_overall_stage == "4", "Stage IV", "Stage I-III"),
                levels = c("Stage I-III", "Stage IV"), ordered = FALSE
            ),
            biopsy1_gep_raw = factor(
                case_when(
                    biopsy1_gep == "DISCORDANT CASTLE RESULTS: Class 1A, PRAME not reported" ~ "Class_1A_PRAME_discordant",
                    TRUE ~ biopsy1_gep
                ),
                levels = c(
                    "Class_1A_PRAME_negative", "Class_1A_PRAME_positive", "Class_1A_PRAME_not_reported",
                    "Class_1B_PRAME_negative", "Class_1B_PRAME_positive",
                    "Class_2_PRAME_negative", "Class_2_PRAME_positive", "Class_2_PRAME_Unknown", "Class_2_PRAME_not_reported",
                    "Failed", "Unknown", "Class_1A_PRAME_discordant", "No"
                ), ordered = FALSE
            ),
            # More user-friendly GEP display combining class and PRAME status
            biopsy1_gep = factor(
                case_when(
                    biopsy1_gep_raw %in% c("Class_1A_PRAME_negative", "Class_1B_PRAME_negative") ~ "Class 1 PRAME Negative",
                    biopsy1_gep_raw %in% c("Class_1A_PRAME_positive", "Class_1B_PRAME_positive") ~ "Class 1 PRAME Positive",
                    biopsy1_gep_raw %in% c("Class_2_PRAME_negative") ~ "Class 2 PRAME Negative",
                    biopsy1_gep_raw %in% c("Class_2_PRAME_positive") ~ "Class 2 PRAME Positive",
                    biopsy1_gep_raw %in% c("No", "N/A") | is.na(biopsy1_gep_raw) ~ "GEP Not Tested",
                    biopsy1_gep_raw %in% c(
                        "Failed", "Class_1A_PRAME_not_reported", "Class_2_PRAME_not_reported",
                        "Class_2_PRAME_Unknown", "Class_1A_PRAME_discordant", "Unknown"
                    ) ~ "GEP Failed/Indeterminate",
                    TRUE ~ NA_character_
                ),
                levels = c(
                    "Class 1 PRAME Negative",
                    "Class 1 PRAME Positive",
                    "Class 2 PRAME Negative",
                    "Class 2 PRAME Positive",
                    "GEP Failed/Indeterminate", 
                    "GEP Not Tested"
                ), ordered = FALSE
            ),
            # Simple GEP class is now binary: Class 1 vs Class 2
            gep_class_simple = factor(
                case_when(
                    biopsy1_gep_raw %in% GEP_CLASS_1_DEFINITIVE_RAW_LEVELS ~ "Class 1",
                    biopsy1_gep_raw %in% GEP_CLASS_2_DEFINITIVE_RAW_LEVELS ~ "Class 2",
                    biopsy1_gep_raw %in% GEP_NOT_TESTED_RAW_LEVELS | is.na(biopsy1_gep_raw) ~ "GEP Not Tested",
                    biopsy1_gep_raw %in% GEP_FAILED_OR_INDETERMINATE_RAW_LEVELS ~ "GEP Failed/Indeterminate",
                    TRUE ~ NA_character_
                ),
                levels = c("Class 1", "Class 2", "GEP Failed/Indeterminate", "GEP Not Tested"), ordered = FALSE
            ),
            prame_status = factor(prame_status, levels = c("Negative", "Positive", "Unknown", "Not Available"), ordered = FALSE)
        )

    # Create collapsed T-stage variable (T1..T4) if needed
    data <- data %>% mutate(
        initial_t_stage_simple = factor(
            case_when(
                grepl("^T1", initial_t_stage, ignore.case = TRUE) ~ "T1",
                grepl("^T2", initial_t_stage, ignore.case = TRUE) ~ "T2",
                grepl("^T3", initial_t_stage, ignore.case = TRUE) ~ "T3",
                grepl("^T4", initial_t_stage, ignore.case = TRUE) ~ "T4",
                TRUE ~ as.character(initial_t_stage)
            ),
            levels = c("T1", "T2", "T3", "T4"), ordered = FALSE # DO NOT USE ORDERED IT FUCKS UP THE MODEL
        )
    )
    

    if (VERBOSE) {
        logger::log_info("\nNew factor levels:")
        factor_vars <- names(data)[sapply(data, is.factor)]
        for (var in factor_vars) {
            message(sprintf("##### %s:", var))
            print(table(data[[var]], useNA = "ifany"))
        }
    }

    logger::log_info("Creating all subgroup variables for analysis")
    data <- create_binned_continuous_variables(data)

    data <- enforce_unordered_factors(data, verbose = VERBOSE)

    return(list(data = data))
}

#' Save each cohort separately (Excel and RDS)
#'
#' @param cohort_data Named list of data frames for each cohort
#' @return None; writes files to disk
save_cohorts <- function(cohort_data) {
    logger::log_info(sprintf("Saving processed data in %s", PROCESSED_DATA_DIR))

    for (cohort_name in names(cohort_data)) {
        logger::log_info(sprintf("Saving cohort: %s", cohort_name))
        write_readable_xlsx(cohort_data[[cohort_name]], file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".xlsx")))
        saveRDS(cohort_data[[cohort_name]], file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".rds")))
    }
}
