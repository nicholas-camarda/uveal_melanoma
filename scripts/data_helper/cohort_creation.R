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

    # Exclude Stage IV patients globally per spec (focus on localized treatment)
    total_before <- nrow(data)
    stage_iv_ids <- data$id[!is.na(data$initial_stage_binary) & data$initial_stage_binary == "Stage IV"]
    num_stage_iv <- length(stage_iv_ids)
    data_no_stage_iv <- data %>%
        filter(initial_stage_binary != "Stage IV" | is.na(initial_stage_binary))
    logger::log_info(sprintf("Stage IV exclusion applied: removed %d patients%s",
        num_stage_iv,
        if (num_stage_iv > 0) sprintf(" (IDs: %s)", paste(stage_iv_ids, collapse = ", ")) else ""
    ))

    # Remove records with missing cohort-defining fields
    before_missing_filter <- nrow(data_no_stage_iv)
    data_after_missing <- data_no_stage_iv %>%
        filter(!is.na(consort_group)) %>%
        filter(!is.na(treatment_group))
    num_missing_removed <- before_missing_filter - nrow(data_after_missing)
    if (num_missing_removed > 0) {
        logger::log_info(sprintf(
            "Removed %d patients due to missing consort_group or treatment_group",
            num_missing_removed
        ))
    } else {
        logger::log_info("No patients removed for missing consort_group or treatment_group")
    }

    # Remove any specifically excluded IDs
    before_specific_excl <- nrow(data_after_missing)
    data_after_specific <- data_after_missing %>%
        filter(!(id %in% SPECIFIC_PATIENTS_TO_EXCLUDE) | is.na(id))
    num_specific_removed <- before_specific_excl - nrow(data_after_specific)
    logger::log_info(sprintf("IDs of patients removed by SPECIFIC_PATIENTS_TO_EXCLUDE: %s",
        if (num_specific_removed > 0) paste(SPECIFIC_PATIENTS_TO_EXCLUDE, collapse = ", ") else "None"))

    full_cohort <- data_after_specific %>%
        mutate(cohort = "All Patients")

    restricted_cohort <- full_cohort %>%
        filter(consort_group == "eligible_both") %>%
        mutate(cohort = "Restricted Cohort (Eligible for Both Treatments)")

    gksrs_only_cohort <- full_cohort %>%
        filter(consort_group == "gksrs_only") %>%
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
    return(factored_filtered_data)
}

#' Prepare factor levels for key variables
#'
#' Converts relevant variables to factors with specified levels and orderings for analysis and modeling.
#'
#' @param data Data frame. Patient-level data.
#'
#' @return A list with elements `data` (factored data) and `other_map` placeholder
prepare_factor_levels <- function(data) {
    logger::log_info("Preparing factor levels for variables")

    data <- data %>%
        mutate(
            # Preserve original raw GEP text before any recoding for downstream flags
            biopsy1_gep_text_raw = as.character(biopsy1_gep),
            # Preserve original raw GEP values for accurate untested flag
            biopsy1_gep_original = biopsy1_gep,
            recurrence1 = factor(recurrence1, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            mets_progression = factor(mets_progression, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            treatment_group = factor(treatment_group, levels = TREATMENT_FACTOR_LEVELS),
            recurrence1_treatment_clean = factor(recurrence1_treatment_clean, ordered = FALSE),
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
            biopsy1_gep = case_when(
                biopsy1_gep == "DISCORDANT CASTLE RESULTS: Class 1A, PRAME not reported" ~ "Class_1A_PRAME_discordant",
                TRUE ~ biopsy1_gep
            ),
            biopsy1_gep_raw = factor(biopsy1_gep,
                levels = c(
                    "Class_1A_PRAME_negative", "Class_1A_PRAME_positive", "Class_1A_PRAME_not_reported",
                    "Class_1B_PRAME_negative", "Class_1B_PRAME_positive",
                    "Class_2_PRAME_negative", "Class_2_PRAME_positive", "Class_2_PRAME_Unknown", "Class_2_PRAME_not_reported",
                    "Failed", "Unknown", "Class_1A_PRAME_discordant", "Other", "No"
                ), ordered = FALSE
            ),
            biopsy1_gep_display = case_when(
                biopsy1_gep_raw == "Class_1A_PRAME_negative" ~ "Class 1A PRAME Negative",
                biopsy1_gep_raw == "Class_1A_PRAME_positive" ~ "Class 1A PRAME Positive",
                biopsy1_gep_raw == "Class_1A_PRAME_not_reported" ~ "Class 1A PRAME Not Reported",
                biopsy1_gep_raw == "Class_1B_PRAME_negative" ~ "Class 1B PRAME Negative",
                biopsy1_gep_raw == "Class_1B_PRAME_positive" ~ "Class 1B PRAME Positive",
                biopsy1_gep_raw == "Class_2_PRAME_negative" ~ "Class 2 PRAME Negative",
                biopsy1_gep_raw == "Class_2_PRAME_positive" ~ "Class 2 PRAME Positive",
                biopsy1_gep_raw == "Class_2_PRAME_Unknown" ~ "Class 2 PRAME Unknown",
                biopsy1_gep_raw == "Class_2_PRAME_not_reported" ~ "Class 2 PRAME Not Reported",
                biopsy1_gep_raw == "Failed" ~ "Failed",
                biopsy1_gep_raw == "Unknown" ~ "Unknown",
                biopsy1_gep_raw == "Class_1A_PRAME_discordant" ~ "Class 1A PRAME Discordant",
                biopsy1_gep_raw == "Other" ~ "Other",
                biopsy1_gep_raw == "No" ~ "No",
                TRUE ~ NA_character_
            ),
            biopsy1_gep = factor(
                case_when(
                    grepl("Class_1", biopsy1_gep_raw, fixed = TRUE) ~ "Class 1",
                    grepl("Class_2", biopsy1_gep_raw, fixed = TRUE) ~ "Class 2",
                    biopsy1_gep_raw == "No" | biopsy1_gep_raw == "N/A" | is.na(biopsy1_gep_raw) ~ "GEP Not Tested",
                    biopsy1_gep_raw == "Failed" | biopsy1_gep_raw == "Other" ~ "GEP Failed/Indeterminate",
                    TRUE ~ NA_character_
                ),
                levels = c("Class 1", "Class 2", "GEP Not Tested", "GEP Failed/Indeterminate"), ordered = FALSE
            ),
            # Simple GEP class is now binary: Class 1 vs Class 2
            gep_class_simple = factor(
                case_when(
                    grepl("Class_1", biopsy1_gep_raw, fixed = TRUE) ~ "Class 1",
                    grepl("Class_2", biopsy1_gep_raw, fixed = TRUE) ~ "Class 2",
                    biopsy1_gep_raw == "No" ~ "No",
                    TRUE ~ NA_character_
                ),
                levels = c("Class 1", "Class 2", "No"), ordered = FALSE
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

    return(list(data = data, other_map = list()))
}

#' Save each cohort separately (Excel and RDS)
#'
#' @param cohort_data Named list of data frames for each cohort
#' @return None; writes files to disk
save_cohorts <- function(cohort_data) {
    logger::log_info(sprintf("Saving processed data in %s", PROCESSED_DATA_DIR))

    for (cohort_name in names(cohort_data)) {
        logger::log_info(sprintf("Saving cohort: %s", cohort_name))
        write_xlsx(cohort_data[[cohort_name]], file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".xlsx")))
        saveRDS(cohort_data[[cohort_name]], file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".rds")))
    }
}
