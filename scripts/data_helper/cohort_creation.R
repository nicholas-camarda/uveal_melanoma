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

    full_cohort <- data %>%
        filter(!is.na(consort_group)) %>%
        filter(!is.na(treatment_group)) %>%
        filter(!(id %in% SPECIFIC_PATIENTS_TO_EXCLUDE) | is.na(id)) %>%
        mutate(cohort = "All Patients")

    logger::log_info(sprintf("Removed %d patients from full cohort based on NA values in consort_group, treatment_group, or id", nrow(data) - nrow(full_cohort)))
    logger::log_info(sprintf("IDs of patients removed: %s", paste(SPECIFIC_PATIENTS_TO_EXCLUDE, collapse = ", ")))

    restricted_cohort <- full_cohort %>%
        filter(consort_group == "eligible_both") %>%
        mutate(cohort = "Restricted Cohort (Eligible for Both Treatments)")

    gksrs_only_cohort <- full_cohort %>%
        filter(consort_group == "gksrs_only") %>%
        mutate(cohort = "GKSRS-Only Cohort (Ineligible for Plaque)")

    factored_filtered_data <- list(
        uveal_melanoma_full_cohort = full_cohort,
        uveal_melanoma_restricted_cohort = restricted_cohort,
        uveal_melanoma_gksrs_only_cohort = gksrs_only_cohort
    )

    logger::log_info(sprintf("Created %d cohorts", length(factored_filtered_data)))
    for (cohort in names(factored_filtered_data)) {
        logger::log_info(sprintf("Cohort '%s': %d patients", cohort, nrow(factored_filtered_data[[cohort]])))
    }

    generate_validation_report(factored_filtered_data)
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
            biopsy1_gep = case_when(
                biopsy1_gep == "DISCORDANT CASTLE RESULTS: Class 1A, PRAME not reported" ~ "Class_1A_PRAME_discordant",
                TRUE ~ biopsy1_gep
            )
        ) %>%
        mutate(
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
            optic_nerve = factor(optic_nerve, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            internal_reflectivity = factor(internal_reflectivity,
                levels = c("Very_Low", "Low", "Low_Medium", "Medium", "Medium_High", "High", "Unknown"),
                labels = c("Very Low", "Low", "Low-Medium", "Medium", "Medium-High", "High", "Unknown"),
                ordered = FALSE
            ),
            srf = factor(srf, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            op = factor(op, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            symptoms = factor(symptoms, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            vision_loss_blurred_vision = factor(vision_loss_blurred_vision, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            visual_field_defect = factor(visual_field_defect, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            flashes_photopsia = factor(flashes_photopsia, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            floaters = factor(floaters, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            pain = factor(pain, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            initial_overall_stage = factor(initial_overall_stage, levels = c("1", "2A", "2B", "3A", "3B", "3C", "4"), ordered = FALSE),
            initial_stage_binary = factor(ifelse(initial_overall_stage == "4", "Stage IV", "Stage I-III"),
                levels = c("Stage I-III", "Stage IV"), ordered = FALSE
            ),
            biopsy1_gep = factor(biopsy1_gep,
                levels = c(
                    "Class_1A_PRAME_negative", "Class_1A_PRAME_positive", "Class_1A_PRAME_not_reported",
                    "Class_1B_PRAME_negative", "Class_1B_PRAME_positive",
                    "Class_2_PRAME_negative", "Class_2_PRAME_positive", "Class_2_PRAME_Unknown", "Class_2_PRAME_not_reported",
                    "Failed", "Unknown", "Class_1A_PRAME_discordant"
                ), ordered = FALSE
            ),
            gep_class_simple = factor(gep_class_simple, levels = c("Class 1A", "Class 1B", "Class 2"), ordered = FALSE),
            prame_status = factor(prame_status, levels = c("Negative", "Positive", "Unknown", "Not Available"), ordered = FALSE)
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
