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
    
    old_variables <- colnames(data)

    new_data <- data %>%
        mutate(
            treatment_group = case_when(
                initial_gk == "Y" & initial_plaque == "N" ~ "GKSRS",
                initial_gk == "N" & initial_plaque == "Y" ~ "PBT",
                TRUE ~ NA_character_
            )
        ) %>%
        mutate(age_at_diagnosis = as.numeric(difftime(date_diagnosis, dob, units = "days") / DAYS_IN_YEAR)) %>%
        mutate(
            follow_up_days = as.numeric(difftime(last_known_alive_date, date_diagnosis, units = "days")),
            follow_up_years = follow_up_days / DAYS_IN_YEAR,
            follow_up_months = follow_up_days / DAYS_IN_MONTH
        ) %>%
        mutate(
            treatment_date = case_when(
                treatment_group == "GKSRS" ~ initial_gk_date,
                treatment_group == "PBT" ~ initial_plaque_date,
                TRUE ~ NA_Date_
            )
        ) %>%
        mutate(treatment_group = factor(treatment_group, levels = TREATMENT_FACTOR_LEVELS)) %>%
        mutate(
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
            tt_recurrence_years = case_when(
                recurrence1 == "Y" ~ time_length(interval(treatment_date, recurrence1_date), "years"),
                TRUE ~ time_length(interval(treatment_date, last_known_alive_date), "years")
            ),
            tt_mets_years = case_when(
                mets_progression == "Y" ~ time_length(interval(treatment_date, mets_progression_date), "years"),
                TRUE ~ time_length(interval(treatment_date, last_known_alive_date), "years")
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
            tt_pfs_months_analysis = pmin(tt_recurrence_months_analysis, tt_death_months_analysis, na.rm = FALSE),
            # Tumor height change: Per project goals 1e
            # Formula: last_height - initial_tumor_height (or recurrence1_pretreatment_height - initial)
            # Negative = tumor decreased/shrank (good), Positive = tumor increased/grew (bad)
            height_change = case_when(
                recurrence1 == "Y" ~ recurrence1_pretreatment_height - initial_tumor_height,
                TRUE ~ last_height - initial_tumor_height
            ),
            # Vision change: Per project goals 2a
            # Formula: initial_vision - last_vision (or recurrence1_pretreatment_vision)
            # Negative = vision worsened (higher logMAR), Positive = vision improved
            vision_change = case_when(
                recurrence1 == "Y" ~ initial_vision - recurrence1_pretreatment_vision,
                TRUE ~ initial_vision - last_vision
            ),
        ) %>%
        mutate(
            recurrence_event = if_else(recurrence1 == "Y", 1, 0, missing = 0),
            mets_event = if_else(mets_progression == "Y", 1, 0, missing = 0),
            death_event = if_else(!is.na(dod), 1, 0, missing = 0),
            # Melanoma-specific death event using cause of death when available
            # Event = metastasis only (mets_event with tt_mets_months)
            # Non‑metastatic deaths are censored
            melanoma_death_event = case_when(
                cod == "Metastatic_Uveal_Melanoma" ~ 1,
                TRUE ~ 0
            ),
            # Competing death is any non-melanoma death
            # MSS: Yes, with your edit. Event = 1 only when cod == "Metastatic_Uveal_Melanoma"
            # All other deaths become competing; survivors are censored
            competing_death_event = case_when(
                death_event == 0 ~ 0L,
                melanoma_death_event == 1 ~ 0L,
                TRUE ~ 1L
            ),
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
        ) %>%
        mutate(mets_free_at_baseline = !(mets_progression == "Y" & mets_progression_date < treatment_date)) %>%
        mutate(
            gep_class_simple = case_when(
                str_detect(biopsy1_gep, "Class_1") ~ "Class 1",
                str_detect(biopsy1_gep, "Class_2") ~ "Class 2",
                # We treat DISCORDANT as NA
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
            ),
            # Limit PRAME subgrouping to tumors with definitive Class 1/2 results
            gep12_prame_status = factor(
                case_when(
                    gep_class_simple %in% c("Class 1", "Class 2") & prame_status %in% c("Positive", "Negative") ~ prame_status,
                    TRUE ~ NA_character_
                ),
                levels = c("Negative", "Positive")
            ),
            # Create GEP validation set with proper Training/Testing split
            # Use a simpler approach that doesn't rely on factor levels
            gep_validation_set = if_else(
                !is.na(biopsy1_gep_mfs) & !is.na(biopsy1_gep_mss) &
                    !is.na(gep_class_simple) & gep_class_simple %in% c("Class 1", "Class 2"),
                "Eligible",
                "No GEP Data"
            )
        ) %>%
        mutate(
             # Pre-process GEP analysis variables to ensure consistency and prevent output ordering issues
            # Time-specific event indicators for consistent analysis (prevents timepoint ordering issues)
            mfs_event_5yr = if_else(mets_event == 1 & tt_mets_months <= 60, 1, 0),
            mfs_event_7yr = if_else(mets_event == 1 & tt_mets_months <= 84, 1, 0),
            mfs_event_10yr = if_else(mets_event == 1 & tt_mets_months <= 120, 1, 0),
            
            mss_event_5yr = if_else(melanoma_death_event == 1 & tt_death_years <= 5, 1, 0),
            mss_event_7yr = if_else(melanoma_death_event == 1 & tt_death_years <= 7, 1, 0),
            mss_event_10yr = if_else(melanoma_death_event == 1 & tt_death_years <= 10, 1, 0),
            
            # Pre-calculated risk variables (prevents redundant calculations in analysis)
            predicted_mfs_risk_5yr = 1 - expected_mfs_5yr,
            predicted_mfs_risk_7yr = 1 - expected_mfs_7yr,
            predicted_mfs_risk_10yr = 1 - expected_mfs_10yr,
            
            predicted_mss_risk_5yr = 1 - expected_mss_5yr,
            predicted_mss_risk_7yr = 1 - expected_mss_7yr,
            predicted_mss_risk_10yr = 1 - expected_mss_10yr,
            
            # Competing risk event type classifications (prevents analysis-time creation)
            # NA handling required: case_when() returns NA when conditions involve NA values, not FALSE
            # Without explicit NA checks, variables like mets_event == 1 return NA if mets_event is NA
            # This causes event_type_mfs_*yr variables to be NA instead of 0 (censored)
            event_type_mfs_5yr = case_when(
                !is.na(mets_event) & mets_event == 1 & !is.na(tt_mets_months) & tt_mets_months <= 60 ~ 1,  # Metastasis event
                !is.na(death_event) & death_event == 1 & !is.na(tt_death_years) & tt_death_years <= 5 & !is.na(melanoma_death_event) & melanoma_death_event == 0 ~ 2,  # Competing death
                TRUE ~ 0  # Censored
            ),
            event_type_mfs_7yr = case_when(
                !is.na(mets_event) & mets_event == 1 & !is.na(tt_mets_months) & tt_mets_months <= 84 ~ 1,  # Metastasis event
                !is.na(death_event) & death_event == 1 & !is.na(tt_death_years) & tt_death_years <= 7 & !is.na(melanoma_death_event) & melanoma_death_event == 0 ~ 2,  # Competing death
                TRUE ~ 0  # Censored
            ),
            event_type_mfs_10yr = case_when(
                !is.na(mets_event) & mets_event == 1 & !is.na(tt_mets_months) & tt_mets_months <= 120 ~ 1,  # Metastasis event
                !is.na(death_event) & death_event == 1 & !is.na(tt_death_years) & tt_death_years <= 10 & !is.na(melanoma_death_event) & melanoma_death_event == 0 ~ 2,  # Competing death
                TRUE ~ 0  # Censored
            ),
            
            # Competing risk event type variables with validation
            # NA handling required: case_when() returns NA when conditions involve NA values, not FALSE
            # Without explicit NA checks, melanoma_death_event == 1 returns NA if melanoma_death_event is NA
            # This causes event_type_mss_*yr variables to be NA instead of 0L (censored)
            event_type_mss_5yr = case_when(
                !is.na(melanoma_death_event) & melanoma_death_event == 1 & !is.na(tt_death_years) & tt_death_years <= 5 ~ 1L,  # Melanoma death
                !is.na(competing_death_event) & competing_death_event == 1 & !is.na(tt_death_years) & tt_death_years <= 5 ~ 2L,  # Competing death
                TRUE ~ 0L  # Censored
            ),
            event_type_mss_7yr = case_when(
                !is.na(melanoma_death_event) & melanoma_death_event == 1 & !is.na(tt_death_years) & tt_death_years <= 7 ~ 1L,  # Melanoma death
                !is.na(competing_death_event) & competing_death_event == 1 & !is.na(tt_death_years) & tt_death_years <= 7 ~ 2L,  # Competing death
                TRUE ~ 0L  # Censored
            ),
            event_type_mss_10yr = case_when(
                !is.na(melanoma_death_event) & melanoma_death_event == 1 & !is.na(tt_death_years) & tt_death_years <= 10 ~ 1L,  # Melanoma death
                !is.na(competing_death_event) & competing_death_event == 1 & !is.na(tt_death_years) & tt_death_years <= 10 ~ 2L,  # Competing death
                TRUE ~ 0L  # Censored
            ),
            
            # Time-to-event variables for specific timepoints (prevents analysis-time creation)
            # MFS: already in months (correct)
            tt_mfs_5yr = pmin(tt_mets_months, 60),
            tt_mfs_7yr = pmin(tt_mets_months, 84),
            tt_mfs_10yr = pmin(tt_mets_months, 120),
            
            # MSS: keep years for compatibility
            tt_mss_5yr = pmin(tt_death_years, 5),
            tt_mss_7yr = pmin(tt_death_years, 7),
            tt_mss_10yr = pmin(tt_death_years, 10),
            
            # Statistical summary variables (prevents analysis-time calculations)
            mfs_analysis_eligible = !is.na(biopsy1_gep) & 
                                   !biopsy1_gep %in% c("GEP Failed/Indeterminate", "GEP Not Tested") &
                                   !is.na(tt_mets_months) & 
                                   !is.na(mets_event) &
                                   tt_mets_months >= 0 &
                                   biopsy1_gep_mfs >= 0 & biopsy1_gep_mfs <= 1,

            mss_analysis_eligible = !is.na(biopsy1_gep) & 
                                   !biopsy1_gep %in% c("GEP Failed/Indeterminate", "GEP Not Tested") &
                                   !is.na(tt_death_years) & 
                                   !is.na(melanoma_death_event) &
                                   !is.na(competing_death_event) &
                                   tt_death_years >= 0 &
                                   biopsy1_gep_mss >= 0 & biopsy1_gep_mss <= 1
        ) %>%
        mutate(
            # Create missing data indicator variables
            has_gep = !is.na(biopsy1_gep) & !biopsy1_gep %in% c("GEP Failed/Indeterminate", "GEP Not Tested"),
            has_gep_mfs = !is.na(biopsy1_gep_mfs),
            has_gep_mss = !is.na(biopsy1_gep_mss),
            has_prame = !is.na(prame_status) &
                prame_status %in% c("Positive", "Negative"),
            missing_gep_group = case_when(
                has_gep & has_gep_mfs & has_gep_mss ~ "Complete GEP",
                has_gep & (has_gep_mfs | has_gep_mss) ~ "Partial GEP",
                TRUE ~ "No GEP"
            )
        ) %>%
        mutate()

    new_variables <- setdiff(colnames(new_data), old_variables)
    if (length(new_variables) > 0) {
        logger::log_info("New derived variables created:")
        logger::log_info(sprintf("%s", paste(new_variables, collapse = ", ")))
    } else {
        logger::log_info("No new derived variables created")
    }
    
    # Apply Training/Testing split to eligible patients using base R approach
    # This needs to be done after the dplyr pipeline is complete
    eligible_indices <- which(new_data$gep_validation_set == "Eligible")
    if (length(eligible_indices) > 0) {
        n_eligible <- length(eligible_indices)
        n_training <- round(n_eligible * 0.7)
        n_testing <- n_eligible - n_training
        
        # Create the sampling vector
        sampled_values <- c(rep("Training", n_training), rep("Testing", n_testing))
        
        # Shuffle the vector to randomize the assignment
        sampled_values <- sample(sampled_values)
        
        # Assign to eligible patients
        new_data$gep_validation_set[eligible_indices] <- sampled_values
    }
        
    return(new_data)
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
                case_when(
                    age_at_diagnosis < 40 ~ "< 40 years",
                    age_at_diagnosis < 50 ~ "40-49 years",
                    age_at_diagnosis < 60 ~ "50-59 years",
                    age_at_diagnosis < 70 ~ "60-69 years",
                    age_at_diagnosis < 80 ~ "70-79 years",
                    age_at_diagnosis >= 80 ~ "≥ 80 years",
                    TRUE ~ NA_character_
                ),
                levels = c(
                    "< 40 years", "40-49 years", "50-59 years",
                    "60-69 years", "70-79 years", "≥ 80 years"
                )
            ),
            age_at_diagnosis_general_pop_median = factor(
                case_when(
                    is.na(age_at_diagnosis) ~ NA_character_,
                    age_at_diagnosis < GENERAL_POP_MEDIAN_AGE_CUTOFF ~ paste0("< ", GENERAL_POP_MEDIAN_AGE_CUTOFF, " years"),
                    TRUE ~ paste0("≥ ", GENERAL_POP_MEDIAN_AGE_CUTOFF, " years")
                ),
                levels = c(
                    paste0("< ", GENERAL_POP_MEDIAN_AGE_CUTOFF, " years"),
                    paste0("≥ ", GENERAL_POP_MEDIAN_AGE_CUTOFF, " years")
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
