#' Create derived variables for the full dataset
#'
#' Adds derived variables (dates, follow-up, time-to-event, event indicators, etc.) to the full data frame.
#'
#' @param data Data frame. Cleaned patient-level data.
#'
#' @return Data frame with derived variables added.
#'
#' @examples
#' create_derived_variables(cleaned_data)
create_derived_variables <- function(data) {
    logger::log_info("Creating derived variables")

    data <- data %>%
        mutate(
            treatment_group = case_when(
                initial_gk == "Y" & initial_plaque == "N" ~ "GKSRS",
                initial_gk == "N" & initial_plaque == "Y" ~ "Plaque",
                TRUE ~ NA_character_
            )
        )

    data <- data %>%
        mutate(age_at_diagnosis = as.numeric(difftime(date_diagnosis, dob, units = "days") / DAYS_IN_YEAR))

    data <- data %>%
        mutate(
            follow_up_days = as.numeric(difftime(last_known_alive_date, date_diagnosis, units = "days")),
            follow_up_years = follow_up_days / DAYS_IN_YEAR,
            follow_up_months = follow_up_days / DAYS_IN_MONTH
        )

    data <- data %>%
        mutate(
            treatment_date = case_when(
                treatment_group == "GKSRS" ~ initial_gk_date,
                treatment_group == "Plaque" ~ initial_plaque_date,
                TRUE ~ NA_Date_
            )
        ) %>%
        mutate(treatment_group = factor(treatment_group, levels = TREATMENT_FACTOR_LEVELS))

    data <- data %>%
        mutate(
            tt_recurrence_months = case_when(
                recurrence1 == "Y" ~ time_length(interval(treatment_date, recurrence1_date), "months"),
                TRUE ~ time_length(interval(treatment_date, last_known_alive_date), "months")
            ),
            tt_mets_months = case_when(
                mets_progression == "Y" ~ time_length(interval(treatment_date, mets_progression_date), "months"),
                TRUE ~ time_length(interval(treatment_date, last_known_alive_date), "months")
            ),
            tt_death_months = case_when(
                !is.na(dod) ~ time_length(interval(treatment_date, dod), "months"),
                TRUE ~ time_length(interval(treatment_date, last_known_alive_date), "months")
            ),
            tt_pfs_months = pmin(tt_recurrence_months, tt_death_months, na.rm = FALSE),
            tt_pfs2_months = case_when(
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) & recurrence2 == "Y" & !is.na(recurrence2_date) ~ time_length(interval(recurrence1_treatment_date, recurrence2_date), "months"),
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) ~ time_length(interval(recurrence1_treatment_date, last_known_alive_date), "months"),
                TRUE ~ NA_real_
            ),
            tt_recurrence = case_when(
                recurrence1 == "Y" ~ as.numeric(difftime(recurrence1_date, treatment_date, units = "days")),
                TRUE ~ as.numeric(difftime(last_known_alive_date, treatment_date, units = "days"))
            ),
            tt_pfs2 = case_when(
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) & recurrence2 == "Y" & !is.na(recurrence2_date) ~ as.numeric(difftime(recurrence2_date, recurrence1_treatment_date, units = "days")),
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) ~ as.numeric(difftime(last_known_alive_date, recurrence1_treatment_date, units = "days")),
                TRUE ~ NA_real_
            ),
            tt_mets = case_when(
                mets_progression == "Y" ~ as.numeric(difftime(mets_progression_date, treatment_date, units = "days")),
                TRUE ~ as.numeric(difftime(last_known_alive_date, treatment_date, units = "days"))
            ),
            tt_death = case_when(
                !is.na(dod) ~ as.numeric(difftime(dod, treatment_date, units = "days")),
                TRUE ~ as.numeric(difftime(last_known_alive_date, treatment_date, units = "days"))
            ),
            tt_recurrence_years = case_when(
                recurrence1 == "Y" ~ time_length(interval(treatment_date, recurrence1_date), "years"),
                TRUE ~ time_length(interval(treatment_date, last_known_alive_date), "years")
            ),
            tt_mets_years = case_when(
                mets_progression == "Y" ~ time_length(interval(treatment_date, mets_progression_date), "years"),
                TRUE ~ time_length(interval(treatment_date, last_known_alive_date), "years")
            ),
            height_change = case_when(
                recurrence1 == "Y" ~ initial_tumor_height - recurrence1_pretreatment_height,
                TRUE ~ initial_tumor_height - last_height
            ),
            tt_death_years = case_when(
                !is.na(dod) ~ time_length(interval(treatment_date, dod), "years"),
                TRUE ~ time_length(interval(treatment_date, last_known_alive_date), "years")
            ),
            tt_pfs2_years = case_when(
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) & recurrence2 == "Y" & !is.na(recurrence2_date) ~ time_length(interval(recurrence1_treatment_date, recurrence2_date), "years"),
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) ~ time_length(interval(recurrence1_treatment_date, last_known_alive_date), "years"),
                TRUE ~ NA_real_
            ),
            mets_before_treatment = tt_mets_months < 0,
            recurrence_before_treatment = tt_recurrence_months < 0,
            death_before_treatment = tt_death_months < 0,
            tt_mets_months_analysis = if_else(tt_mets_months < 0, 0, tt_mets_months),
            tt_recurrence_months_analysis = if_else(tt_recurrence_months < 0, 0, tt_recurrence_months),
            tt_death_months_analysis = if_else(tt_death_months < 0, 0, tt_death_months),
            tt_pfs_months_analysis = pmin(tt_recurrence_months_analysis, tt_death_months_analysis, na.rm = FALSE)
        )

    data <- data %>%
        mutate(
            recurrence_event = if_else(recurrence1 == "Y", 1, 0, missing = 0),
            mets_event = if_else(mets_progression == "Y", 1, 0, missing = 0),
            death_event = if_else(!is.na(dod), 1, 0, missing = 0),
            pfs_event = if_else(recurrence_event == 1 | death_event == 1, 1, 0),
            pfs2_event = case_when(
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) & recurrence2 == "Y" & !is.na(recurrence2_date) ~ 1,
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) ~ 0,
                TRUE ~ NA_real_
            ),
            recurrence1_treatment_clean = case_when(
                recurrence1 == "Y" & !is.na(recurrence1_treatment) ~ case_when(
                    str_detect(tolower(recurrence1_treatment), "gk") ~ "GKSRS",
                    str_detect(tolower(recurrence1_treatment), "enuc") ~ "Enucleation",
                    str_detect(tolower(recurrence1_treatment), "ttt") ~ "TTT",
                    TRUE ~ recurrence1_treatment
                ),
                TRUE ~ NA_character_
            )
        )

    data <- data %>%
        mutate(mets_free_at_baseline = !(mets_progression == "Y" & mets_progression_date < treatment_date))

    data <- data %>%
        mutate(
            gep_class_simple = case_when(
                str_detect(biopsy1_gep, "Class_1A") ~ "Class 1A",
                str_detect(biopsy1_gep, "Class_1B") ~ "Class 1B",
                str_detect(biopsy1_gep, "Class_2") ~ "Class 2",
                TRUE ~ NA_character_
            ),
            expected_mfs_5yr = biopsy1_gep_mfs,
            expected_mfs_7yr = case_when(
                !is.na(biopsy1_gep_mfs) ~ biopsy1_gep_mfs^(7 / 5),
                TRUE ~ NA_real_
            ),
            expected_mfs_10yr = case_when(
                !is.na(biopsy1_gep_mfs) ~ biopsy1_gep_mfs^(10 / 5),
                TRUE ~ NA_real_
            ),
            expected_mss_5yr = biopsy1_gep_mss,
            expected_mss_7yr = case_when(
                !is.na(biopsy1_gep_mss) ~ biopsy1_gep_mss^(7 / 5),
                TRUE ~ NA_real_
            ),
            expected_mss_10yr = case_when(
                !is.na(biopsy1_gep_mss) ~ biopsy1_gep_mss^(10 / 5),
                TRUE ~ NA_real_
            ),
            prame_status = case_when(
                str_detect(biopsy1_gep, "PRAME_positive") ~ "Positive",
                str_detect(biopsy1_gep, "PRAME_negative") ~ "Negative",
                str_detect(biopsy1_gep, "PRAME_not_reported|PRAME_Unknown") ~ "Unknown",
                TRUE ~ "Not Available"
            )
        )

    set.seed(12345)
    data <- data %>%
        mutate(
            gep_validation_set = case_when(
                !is.na(biopsy1_gep_mfs) & !is.na(biopsy1_gep_mss) &
                    gep_class_simple %in% c("Class 1A", "Class 1B", "Class 2") ~
                    sample(c("Training", "Testing"), n(), replace = TRUE, prob = c(0.7, 0.3)),
                TRUE ~ "No GEP Data"
            )
        )

    data <- data %>%
        mutate(
            # Preserve original stage values; forced collapse to 'Other' will happen centrally
            initial_overall_stage_modified = factor(
                as.character(initial_overall_stage),
                levels = c("1", "2A", "2B", "3A", "3B", "3C", "4"),
                ordered = FALSE
            )
        )

    return(data)
}

#' Create binned continuous variables for subgroup analysis
#'
#' Creates binned versions of continuous variables for subgroup analysis using clinical or legacy cutoffs.
#'
#' @param data Data frame
#' @return Data frame with binned continuous variables added
create_binned_continuous_variables <- function(data) {
    logger::log_info("Creating subgroup variables for analysis")

    data <- data %>%
        mutate(
            age_at_diagnosis_binned = factor(
                ifelse(age_at_diagnosis < median(age_at_diagnosis, na.rm = TRUE),
                    paste0("< ", round(median(age_at_diagnosis, na.rm = TRUE), 1)),
                    paste0("≥ ", round(median(age_at_diagnosis, na.rm = TRUE), 1))
                ),
                levels = c(
                    paste0("< ", round(median(age_at_diagnosis, na.rm = TRUE), 1)),
                    paste0("≥ ", round(median(age_at_diagnosis, na.rm = TRUE), 1))
                )
            ),
            initial_tumor_height_binned = if (USE_CLINICAL_BINNING_CONTINUOUS) {
                factor(
                    case_when(
                        initial_tumor_height <= T_STAGE_HEIGHT_CUTOFFS[1] ~ paste0("≤ ", T_STAGE_HEIGHT_CUTOFFS[1], " mm"),
                        initial_tumor_height <= T_STAGE_HEIGHT_CUTOFFS[2] ~ paste0(T_STAGE_HEIGHT_CUTOFFS[1] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[2], " mm"),
                        initial_tumor_height <= T_STAGE_HEIGHT_CUTOFFS[3] ~ paste0(T_STAGE_HEIGHT_CUTOFFS[2] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[3], " mm"),
                        initial_tumor_height <= T_STAGE_HEIGHT_CUTOFFS[4] ~ paste0(T_STAGE_HEIGHT_CUTOFFS[3] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[4], " mm"),
                        initial_tumor_height <= T_STAGE_HEIGHT_CUTOFFS[5] ~ paste0(T_STAGE_HEIGHT_CUTOFFS[4] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[5], " mm"),
                        initial_tumor_height > T_STAGE_HEIGHT_CUTOFFS[5] ~ paste0("> ", T_STAGE_HEIGHT_CUTOFFS[5], " mm"),
                        TRUE ~ NA_character_
                    ),
                    levels = c(
                        paste0("≤ ", T_STAGE_HEIGHT_CUTOFFS[1], " mm"),
                        paste0(T_STAGE_HEIGHT_CUTOFFS[1] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[2], " mm"),
                        paste0(T_STAGE_HEIGHT_CUTOFFS[2] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[3], " mm"),
                        paste0(T_STAGE_HEIGHT_CUTOFFS[3] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[4], " mm"),
                        paste0(T_STAGE_HEIGHT_CUTOFFS[4] + 0.1, "-", T_STAGE_HEIGHT_CUTOFFS[5], " mm"),
                        paste0("> ", T_STAGE_HEIGHT_CUTOFFS[5], " mm")
                    )
                )
            } else {
                factor(
                    ifelse(initial_tumor_height < LEGACY_CUTOFFS$initial_tumor_height,
                        paste0("< ", LEGACY_CUTOFFS$initial_tumor_height, " mm"),
                        paste0("≥ ", LEGACY_CUTOFFS$initial_tumor_height, " mm")
                    ),
                    levels = c(
                        paste0("< ", LEGACY_CUTOFFS$initial_tumor_height, " mm"),
                        paste0("≥ ", LEGACY_CUTOFFS$initial_tumor_height, " mm")
                    )
                )
            },
            initial_tumor_diameter_binned = if (USE_CLINICAL_BINNING_CONTINUOUS) {
                factor(
                    case_when(
                        initial_tumor_diameter <= T_STAGE_DIAMETER_CUTOFFS[1] ~ paste0("≤ ", T_STAGE_DIAMETER_CUTOFFS[1], " mm"),
                        initial_tumor_diameter <= T_STAGE_DIAMETER_CUTOFFS[2] ~ paste0(T_STAGE_DIAMETER_CUTOFFS[1] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[2], " mm"),
                        initial_tumor_diameter <= T_STAGE_DIAMETER_CUTOFFS[3] ~ paste0(T_STAGE_DIAMETER_CUTOFFS[2] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[3], " mm"),
                        initial_tumor_diameter <= T_STAGE_DIAMETER_CUTOFFS[4] ~ paste0(T_STAGE_DIAMETER_CUTOFFS[3] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[4], " mm"),
                        initial_tumor_diameter <= T_STAGE_DIAMETER_CUTOFFS[5] ~ paste0(T_STAGE_DIAMETER_CUTOFFS[4] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[5], " mm"),
                        initial_tumor_diameter <= T_STAGE_DIAMETER_CUTOFFS[6] ~ paste0(T_STAGE_DIAMETER_CUTOFFS[5] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[6], " mm"),
                        initial_tumor_diameter > T_STAGE_DIAMETER_CUTOFFS[6] ~ paste0("> ", T_STAGE_DIAMETER_CUTOFFS[6], " mm"),
                        TRUE ~ NA_character_
                    ),
                    levels = c(
                        paste0("≤ ", T_STAGE_DIAMETER_CUTOFFS[1], " mm"),
                        paste0(T_STAGE_DIAMETER_CUTOFFS[1] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[2], " mm"),
                        paste0(T_STAGE_DIAMETER_CUTOFFS[2] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[3], " mm"),
                        paste0(T_STAGE_DIAMETER_CUTOFFS[3] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[4], " mm"),
                        paste0(T_STAGE_DIAMETER_CUTOFFS[4] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[5], " mm"),
                        paste0(T_STAGE_DIAMETER_CUTOFFS[5] + 0.1, "-", T_STAGE_DIAMETER_CUTOFFS[6], " mm"),
                        paste0("> ", T_STAGE_DIAMETER_CUTOFFS[6], " mm")
                    )
                )
            } else {
                factor(
                    ifelse(initial_tumor_diameter < LEGACY_CUTOFFS$initial_tumor_diameter,
                        paste0("< ", LEGACY_CUTOFFS$initial_tumor_diameter, " mm"),
                        paste0("≥ ", LEGACY_CUTOFFS$initial_tumor_diameter, " mm")
                    ),
                    levels = c(
                        paste0("< ", LEGACY_CUTOFFS$initial_tumor_diameter, " mm"),
                        paste0("≥ ", LEGACY_CUTOFFS$initial_tumor_diameter, " mm")
                    )
                )
            }
        )

    return(data)
}
