# Uveal Melanoma Data Processing Script
# Author: Nicholas Camarda
# Date: 5/10/2025
# Description: Script to process raw data into analytic dataset for uveal melanoma analysis

# Source centralized configuration (must be first)
source("scripts/utils/analysis_config.R")



# Note: Directory creation is handled in main.R after libraries are loaded

# NOTE: Old cohort directory structure no longer used
# Directory structure is now created dynamically by create_output_structure() function
# which creates cohort -> objective -> sub-objective structure

# DEPRECATED: Old cohort-specific directories (now handled by create_output_structure)
# COHORT_DIRS <- list(
#     full = file.path(ANALYSIS_DIR, "uveal_full"),
#     restricted = file.path(ANALYSIS_DIR, "uveal_restricted"),
#     gksrs = file.path(ANALYSIS_DIR, "gksrs")
# )
# 
# # Create tables and figures subdirectories for each cohort
# for (dir in COHORT_DIRS) {
#     dir.create(file.path(dir, "tables"), showWarnings = FALSE, recursive = TRUE)
#     dir.create(file.path(dir, "figures"), showWarnings = FALSE, recursive = TRUE)
# }

#' DEPRECATED: Get cohort directory and file prefix
#'
#' This function is deprecated. Directory structure is now handled by create_output_structure()
#' in scripts/utils/output_utilities.R which creates cohort -> objective -> sub-objective structure.
#'
#' @param cohort_name Character. Name of the cohort (e.g., 'full_cohort', 'restricted_cohort', 'gksrs_only_cohort').
#'
#' @return A list with elements:
#'   - dir: Path to the output directory for the cohort.
#'   - prefix: File prefix for saving outputs.
#'
#' @examples
#' get_cohort_info("full_cohort")
get_cohort_info <- function(cohort_name) {
    warning("get_cohort_info() is deprecated. Use create_output_structure() in main.R instead.")
    
    # Map cohort names to directory names (DEPRECATED MAPPING)
    cohort_map <- list(
        "uveal_melanoma_full_cohort" = "uveal_full",
        "uveal_melanoma_gksrs_only_cohort" = "uveal_restricted",
        "uveal_melanoma_restricted_cohort" = "gksrs"
    )

    dir_name <- cohort_map[[cohort_name]]
    if (is.null(dir_name)) {
        stop(sprintf("Unknown cohort name: %s", cohort_name))
    }

    # Create file prefix based on cohort name
    file_prefix <- paste0(dir_name, "_")

            return(list(
            dir = file.path(ANALYSIS_DIR, dir_name),
            prefix = file_prefix
        ))
}

#' Check and fix consistency between event indicators and dates
#'
#' Ensures logical consistency between binary event indicators and their associated dates.
#' If a date exists but the event is marked as 'N' or NA, updates event to 'Y'.
#' If an event is marked as 'Y' but has no date, sets date to NA.
#'
#' @param data Data frame containing event and date variables
#' @param event_var Name of the event indicator variable (character)
#' @param date_var Name of the date variable (character)
#' @param event_yes Value indicating event occurred (default "Y")
#' @param event_no Value indicating event did not occur (default "N")
#'
#' @return Data frame with consistent event indicators and dates
#'
#' @examples
#' fix_event_date_consistency(data, "recurrence1", "recurrence1_date")
fix_event_date_consistency <- function(data, event_var, date_var, event_yes = "Y", event_no = "N") {
    log_enhanced(sprintf("Checking consistency between %s and %s", event_var, date_var), level = "INFO")
    
    # Before the mutate, calculate inconsistencies
    n_event_should_be_yes <- sum(!is.na(data[[date_var]]) & data[[event_var]] != event_yes, na.rm = TRUE)
    n_date_should_be_na <- sum(data[[event_var]] == event_yes & is.na(data[[date_var]]), na.rm = TRUE)

    # Handle all event/date consistency cases in one mutate
    data <- data %>%
        mutate(
            !!event_var := case_when(
                !is.na(.data[[date_var]]) ~ event_yes,
                .data[[event_var]] == event_yes & is.na(.data[[date_var]]) ~ event_no,
                TRUE ~ event_no
            ),
            !!date_var := if_else(.data[[event_var]] == event_yes, .data[[date_var]], as.Date(NA))
        )
        
    if (VERBOSE) {
        log_enhanced(sprintf("Found %d events with dates", sum(!is.na(data[[date_var]]))), level = "INFO")
        log_enhanced(sprintf("Found %d events marked as '%s'", sum(data[[event_var]] == event_yes, na.rm = TRUE), event_yes), level = "INFO")
        log_enhanced(sprintf(
            "Event/date consistency check for '%s' and '%s':", event_var, date_var
        ), level = "INFO")
        log_enhanced(sprintf(
            "  - Number of records with a non-missing %s: %d", date_var, sum(!is.na(data[[date_var]]))
        ), level = "INFO")
        log_enhanced(sprintf(
            "  - Number of records with %s marked as '%s': %d", event_var, event_yes, sum(data[[event_var]] == event_yes, na.rm = TRUE)
        ), level = "INFO")
        log_enhanced(sprintf(
            "  - Fixed %d records where %s was not '%s' but %s was present (set event to '%s')",
            n_event_should_be_yes, event_var, event_yes, date_var, event_yes
        ), level = "INFO")
        log_enhanced(sprintf(
            "  - Fixed %d records where %s was '%s' but %s was missing (set date to NA)",
            n_date_should_be_na, event_var, event_yes, date_var
        ), level = "INFO")
    }
    
    return(data)
}

#' Load and clean raw data
#'
#' Reads the main Excel data file, standardizes NA values, cleans up location values, removes empty/duplicate rows, and assigns consort_group for inclusion/exclusion.
#'
#' @param filename Character. Name of the Excel file to load.
#' @return A cleaned data.frame/tibble with standardized variables and consort_group assigned.
#'
#' @details
#' - Removes columns containing '...'.
#' - Converts various NA strings to NA.
#' - Standardizes location values.
#' - Removes empty and duplicate rows.
#' - Assigns consort_group based on tumor size and optic nerve involvement.
#'
#' @examples
#' load_and_clean_data()
load_and_clean_data <- function(filename) {
    # Read the Excel file
    log_enhanced(sprintf("Loading data from directory: %s", RAW_DATA_DIR), level = "INFO")
    log_enhanced(sprintf("Loading data from file: %s", filename), level = "INFO")
    raw_data <- read_excel(
        file.path(RAW_DATA_DIR, filename),
        sheet = 1 # Main data sheet
    ) %>%
        # Remove any columns that contain "...", particularly the last 2
        dplyr::select(-contains("..."))

    # Basic cleaning steps
    cleaned_data <- raw_data %>%
        # Standardize NA values
        mutate(across(everything(), ~ {
            if (is.character(.)) {
                # Convert various forms of NA to actual NA
                case_when(
                    . %in% c("NA", "N/A", "n/a", "na", "", " ") ~ NA_character_,
                    TRUE ~ .
                )
            } else {
                .
            }
        })) %>%
        # Standardize location values
        mutate(
            location = case_when(
                location %in% c("Cilio_Choroidal", "Cilio_choroidal") ~ "Cilio_Choroidal",
                TRUE ~ location
            )
        ) %>%
        # Remove any completely empty rows
        filter(!if_all(everything(), is.na)) %>%
        # Remove any duplicate rows
        distinct() %>%
        # Assign consort_group variable for inclusion/exclusion
        mutate(
            consort_group = case_when(
                !is.na(initial_gk) | !is.na(initial_plaque) ~ # If patient had either GK or plaque treatment
                    case_when(
                        # Criteria for "eligible_both" group:
                        initial_tumor_diameter <= TUMOR_DIAMETER_THRESHOLD & # Tumor diameter ≤ 20mm
                            initial_tumor_height <= TUMOR_HEIGHT_THRESHOLD & # Tumor height ≤ 10mm
                            optic_nerve == "N" ~ "eligible_both", # No optic nerve involvement

                        # Criteria for "gksrs_only" group:
                        initial_tumor_diameter > TUMOR_DIAMETER_THRESHOLD | # Tumor diameter > 20mm
                            initial_tumor_height > TUMOR_HEIGHT_THRESHOLD | # Tumor height > 10mm
                            optic_nerve == "Y" ~ "gksrs_only", # Has optic nerve involvement

                        TRUE ~ "other" # Catch-all for any other cases
                    ),
                TRUE ~ NA_character_ # If no treatment data, mark as NA
            )
        )
    
    log_enhanced("eligible_both: initial_tumor_diameter <= 20mm, initial_tumor_height <= 10mm, optic_nerve == 'N'", level = "INFO")
    log_enhanced("gksrs_only: initial_tumor_diameter > 20mm, initial_tumor_height > 10mm, optic_nerve == 'Y'", level = "INFO")
    log_enhanced("other: catch-all for any other cases", level = "INFO")
    message("\n")
    log_enhanced(sprintf("Found %d patients in full cohort", nrow(cleaned_data)), level = "INFO")
    log_enhanced(sprintf("Found %d patients in restricted cohort", nrow(cleaned_data %>% filter(consort_group == "eligible_both"))), level = "INFO")
    log_enhanced(sprintf("Found %d patients in GKSRS-only cohort", nrow(cleaned_data %>% filter(consort_group == "gksrs_only"))), level = "INFO")
    log_enhanced(sprintf("Found %d patients in other cohort", nrow(cleaned_data %>% filter(consort_group == "other"))), level = "INFO")
    print(cleaned_data %>% filter(consort_group == "other") %>% select(id, initial_tumor_diameter, initial_tumor_height, optic_nerve))
    message("\n")
    log_enhanced("NOTE: NOT splitting into cohorts yet!", level = "INFO")
    message("\n")


    # Check and fix consistency between event indicators and dates
    cleaned_data <- fix_event_date_consistency(cleaned_data, "initial_gk", "initial_gk_date")
    cleaned_data <- fix_event_date_consistency(cleaned_data, "initial_plaque", "initial_plaque_date")
    cleaned_data <- fix_event_date_consistency(cleaned_data, "recurrence1", "recurrence1_date")
    cleaned_data <- fix_event_date_consistency(cleaned_data, "recurrence2", "recurrence2_date")
    cleaned_data <- fix_event_date_consistency(cleaned_data, "recurrence3", "recurrence3_date")
    cleaned_data <- fix_event_date_consistency(cleaned_data, "mets_progression", "mets_progression_date")
    cleaned_data <- fix_event_date_consistency(cleaned_data, "enucleation", "enucleation_date")

    # Convert all relevant columns to Date type
    cleaned_data <- cleaned_data %>%
        mutate(across(contains("date|dob|dod|last\\_followup", ignore.case = TRUE), as.Date))
    
    # Identify all relevant date columns (excluding dob and last_known_alive_date)
    date_cols <- colnames(cleaned_data)[
        grepl("date", colnames(cleaned_data), ignore.case = TRUE) |
            grepl("dob", colnames(cleaned_data), ignore.case = TRUE) |
            grepl("dod", colnames(cleaned_data), ignore.case = TRUE) |
            grepl("last_followup", colnames(cleaned_data), ignore.case = TRUE)
    ]

    # Calculate last_known_alive_date
    cleaned_data_final <- cleaned_data %>%
        mutate(
            last_known_alive_date = pmax(!!!syms(date_cols), na.rm = TRUE),
            last_known_alive_source = apply( # for each row, find the max date and return the column name of the max date
                pick(all_of(date_cols)), # ensures only the date columns are passed to apply
                1, # for each row
                function(row) {
                    if (all(is.na(row))) {
                        return(NA_character_)
                    } # if all dates are NA, return NA
                    max_date <- max(row, na.rm = TRUE) # find the max date
                    names(row)[which(row == max_date)[1]] # get column name of max date
                }
            )
        )

    log_enhanced(sprintf("Loaded %d rows of raw data", nrow(cleaned_data_final)), level = "INFO")

    return(cleaned_data_final)
}

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
    log_enhanced("Creating derived variables", level = "INFO")

    # Create treatment_group before using it
    data <- data %>%
        mutate(
            treatment_group = case_when(
                initial_gk == "Y" & initial_plaque == "N" ~ "GKSRS",
                initial_gk == "N" & initial_plaque == "Y" ~ "Plaque",
                TRUE ~ NA_character_
            )
        )

    log_enhanced("Calculating age at diagnosis", level = "INFO")
    data <- data %>%
        mutate(age_at_diagnosis = as.numeric(difftime(date_diagnosis, dob, units = "days") / DAYS_IN_YEAR))

    log_enhanced("Calculating follow-up times", level = "INFO")
    data <- data %>%
        mutate(
            follow_up_days = as.numeric(difftime(last_known_alive_date, date_diagnosis, units = "days")),
            follow_up_years = follow_up_days / DAYS_IN_YEAR,
            follow_up_months = follow_up_days / DAYS_IN_MONTH
        )

    log_enhanced("Setting treatment dates", level = "INFO")
    data <- data %>%
        mutate(
            treatment_date = case_when(
                treatment_group == "GKSRS" ~ initial_gk_date,
                treatment_group == "Plaque" ~ initial_plaque_date,
                TRUE ~ NA_Date_
            )
        ) %>%
        # Set Plaque as reference group (using centralized factor levels)
        mutate(treatment_group = factor(treatment_group, levels = TREATMENT_FACTOR_LEVELS))
    
    # !DEBUG
    # data %>%
    #     select(id, treatment_group, age_at_diagnosis, follow_up_days, follow_up_years, treatment_date, initial_gk_date, initial_plaque_date) %>%
    #     print(n = Inf)

    log_enhanced("Calculating time-to-event (ie, tt_) variables", level = "INFO")
    data <- data %>%
        mutate(
            # Primary time-to-event variables in MONTHS (oncology standard)
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
            # Create progression-free survival time (first of recurrence OR death)
            tt_pfs_months = pmin(tt_recurrence_months, tt_death_months, na.rm = FALSE),
            
            # PFS-2 variables (progression-free survival after first recurrence treatment)
            tt_pfs2_months = case_when(
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) & recurrence2 == "Y" & !is.na(recurrence2_date) ~ 
                    time_length(interval(recurrence1_treatment_date, recurrence2_date), "months"),
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) ~ 
                    time_length(interval(recurrence1_treatment_date, last_known_alive_date), "months"),
                TRUE ~ NA_real_
            ),
            
            # Legacy variables in days (kept for backward compatibility)
            tt_recurrence = case_when(
                recurrence1 == "Y" ~ as.numeric(difftime(recurrence1_date, treatment_date, units = "days")),
                TRUE ~ as.numeric(difftime(last_known_alive_date, treatment_date, units = "days"))
            ),
            tt_pfs2 = case_when(
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) & recurrence2 == "Y" & !is.na(recurrence2_date) ~ 
                    as.numeric(difftime(recurrence2_date, recurrence1_treatment_date, units = "days")),
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) ~ 
                    as.numeric(difftime(last_known_alive_date, recurrence1_treatment_date, units = "days")),
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
            # Years (for reference)
            tt_recurrence_years = case_when(
                recurrence1 == "Y" ~ time_length(interval(treatment_date, recurrence1_date), "years"),
                TRUE ~ time_length(interval(treatment_date, last_known_alive_date), "years")
            ),
            tt_mets_years = case_when(
                mets_progression == "Y" ~ time_length(interval(treatment_date, mets_progression_date), "years"),
                TRUE ~ time_length(interval(treatment_date, last_known_alive_date), "years")
            ),
            
            # Calculate height changes (row-level)
            height_change = case_when(
                # Calculate height change as the difference between the initial 
                # tumor height and the height at the time of recurrence *or* last follow-up
                # Post treatment1 height = recurrence1 pretreatment height
                recurrence1 == "Y" ~ initial_tumor_height - recurrence1_pretreatment_height,
                TRUE ~ initial_tumor_height - last_height
            ),
            tt_death_years = case_when(
                !is.na(dod) ~ time_length(interval(treatment_date, dod), "years"),
                TRUE ~ time_length(interval(treatment_date, last_known_alive_date), "years")
            ),
            tt_pfs2_years = case_when(
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) & recurrence2 == "Y" & !is.na(recurrence2_date) ~ 
                    time_length(interval(recurrence1_treatment_date, recurrence2_date), "years"),
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) ~ 
                    time_length(interval(recurrence1_treatment_date, last_known_alive_date), "years"),
                TRUE ~ NA_real_
            ),
            
            # Analysis flags for pre-treatment events
            mets_before_treatment = tt_mets_months < 0,
            recurrence_before_treatment = tt_recurrence_months < 0,
            death_before_treatment = tt_death_months < 0,
            
            # Analysis-ready time variables (set negative values to 0 for post-treatment analyses)
            tt_mets_months_analysis = if_else(tt_mets_months < 0, 0, tt_mets_months),
            tt_recurrence_months_analysis = if_else(tt_recurrence_months < 0, 0, tt_recurrence_months),
            tt_death_months_analysis = if_else(tt_death_months < 0, 0, tt_death_months),
            tt_pfs_months_analysis = pmin(tt_recurrence_months_analysis, tt_death_months_analysis, na.rm = FALSE)
        )

    # data %>%
    #     select(id, tt_death, dod, treatment_group, age_at_diagnosis, follow_up_days, follow_up_years, treatment_date, initial_gk_date, initial_plaque_date) %>%
    #     print(n = Inf)

    log_enhanced("Creating event indicators (ie, recurrence_event, mets_event, death_event, pfs_event, pfs2_event)", level = "INFO")
    data <- data %>%
        mutate(
            recurrence_event = if_else(recurrence1 == "Y", 1, 0, missing = 0),
            mets_event = if_else(mets_progression == "Y", 1, 0, missing = 0),
            death_event = if_else(!is.na(dod), 1, 0, missing = 0),
            # Progression-free survival event: progression OR death (whichever comes first)
            pfs_event = if_else(recurrence_event == 1 | death_event == 1, 1, 0),
            # PFS-2 event: 1 if 2nd recurrence occurred, 0 if censored (only for patients with first recurrence)
            pfs2_event = case_when(
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) & recurrence2 == "Y" & !is.na(recurrence2_date) ~ 1,
                recurrence1 == "Y" & !is.na(recurrence1_treatment_date) ~ 0,
                TRUE ~ NA_real_
            ),
            # Clean recurrence treatment variable for PFS-2 analysis
            recurrence1_treatment_clean = case_when(
                recurrence1 == "Y" & !is.na(recurrence1_treatment) ~ case_when(
                    str_detect(tolower(recurrence1_treatment), "gk") ~ "GKSRS",
                    str_detect(tolower(recurrence1_treatment), "enuc") ~ "Enucleation", 
                    str_detect(tolower(recurrence1_treatment), "ttt") ~ "TTT",
                    TRUE ~ "Other"
                ),
                TRUE ~ NA_character_
            )
        )
    
    # Identify patients who were mets-free at baseline
    data <- data %>%
        mutate(mets_free_at_baseline = !(mets_progression == "Y" & mets_progression_date < treatment_date))

    log_enhanced("Creating GEP validation variables (Objective 4)", level = "INFO")
    data <- data %>%
        mutate(
            # Create simplified GEP classes for primary analysis
            gep_class_simple = case_when(
                str_detect(biopsy1_gep, "Class_1A") ~ "Class 1A",
                str_detect(biopsy1_gep, "Class_1B") ~ "Class 1B",
                str_detect(biopsy1_gep, "Class_2") ~ "Class 2",
                TRUE ~ NA_character_
            ),
            # Convert expected MFS to survival probability at multiple timepoints
            expected_mfs_5yr = biopsy1_gep_mfs,
            # Extrapolate to 7 and 10 years assuming exponential decay (constant hazard)
            expected_mfs_7yr = case_when(
                !is.na(biopsy1_gep_mfs) ~ biopsy1_gep_mfs^(7 / 5),
                TRUE ~ NA_real_
            ),
            expected_mfs_10yr = case_when(
                !is.na(biopsy1_gep_mfs) ~ biopsy1_gep_mfs^(10 / 5),
                TRUE ~ NA_real_
            ),
            # Convert expected MSS to survival probability at multiple timepoints
            expected_mss_5yr = biopsy1_gep_mss,
            expected_mss_7yr = case_when(
                !is.na(biopsy1_gep_mss) ~ biopsy1_gep_mss^(7 / 5),
                TRUE ~ NA_real_
            ),
            expected_mss_10yr = case_when(
                !is.na(biopsy1_gep_mss) ~ biopsy1_gep_mss^(10 / 5),
                TRUE ~ NA_real_
            ),
            # Create PRAME status for secondary analysis
            prame_status = case_when(
                str_detect(biopsy1_gep, "PRAME_positive") ~ "Positive",
                str_detect(biopsy1_gep, "PRAME_negative") ~ "Negative",
                str_detect(biopsy1_gep, "PRAME_not_reported|PRAME_Unknown") ~ "Unknown",
                TRUE ~ "Not Available"
            )
        )

    # Create training/testing split for GEP validation
    log_enhanced("Creating training/testing split for GEP validation", level = "INFO")
    set.seed(12345)  # For reproducible splits
    data <- data %>%
        mutate(
            gep_validation_set = case_when(
                # Only split patients with valid GEP data
                !is.na(biopsy1_gep_mfs) & !is.na(biopsy1_gep_mss) & 
                gep_class_simple %in% c("Class 1A", "Class 1B", "Class 2") ~ 
                    sample(c("Training", "Testing"), n(), replace = TRUE, prob = c(0.7, 0.3)),
                TRUE ~ "No GEP Data"
            )
        )

    # Create modified overall stage variable (excluding stages with insufficient numbers)
    log_enhanced(sprintf("Creating modified overall stage variable (excluding stages: %s)", 
                        paste(STAGES_TO_EXCLUDE_FROM_MODIFIED, collapse = ", ")), level = "INFO")
    data <- data %>%
        mutate(
            initial_overall_stage_modified = case_when(
                initial_overall_stage %in% STAGES_TO_EXCLUDE_FROM_MODIFIED ~ NA_character_,
                TRUE ~ as.character(initial_overall_stage)
            )
        ) %>%
        mutate(
            initial_overall_stage_modified = factor(initial_overall_stage_modified, 
                                                   levels = c("1", "2A", "2B", "3A"),
                                                   ordered = FALSE)  # CRITICAL: Use treatment contrasts, not polynomial
        )

    # data %>%
    #     select(id, mets_free_at_baseline, tt_death, dod, death_event, treatment_group) %>%
    #     print(n = Inf)

    return(data)
}

#' Apply inclusion/exclusion criteria to create cohorts
#'
#' Filters and mutates the cleaned data to create three cohorts: full, restricted, and GKSRS-only.
#' Requires consort_group to be present in the data from `load_and_clean_data()`
#'
#' @param data Data frame. Cleaned patient-level data.
#'
#' @return A list with three tibbles:
#'   - full_cohort: All eligible patients.
#'   - restricted_cohort: Patients eligible for both treatments.
#'   - gksrs_only_cohort: Patients ineligible for plaque.
#'
#' @examples
#' apply_criteria(cleaned_data)
apply_criteria <- function(data) {
    # Create full cohort (all patients treated with either GK or plaque)

    log_enhanced(sprintf("Applying inclusion/exclusion criteria to full cohort to generate restricted and GKSRS-only cohorts"), level = "INFO")

    full_cohort <- data %>%
        filter(!is.na(consort_group)) %>%
        filter(!is.na(treatment_group)) %>%
        filter(!(id %in% SPECIFIC_PATIENTS_TO_EXCLUDE) | is.na(id)) %>%
        mutate(cohort = "All Patients")
    
    log_enhanced(sprintf("Removed %d patients from full cohort based on NA values in consort_group, treatment_group, or id", nrow(data) - nrow(full_cohort)), level = "INFO")
    log_enhanced(sprintf("IDs of patients removed: %s", paste(SPECIFIC_PATIENTS_TO_EXCLUDE, collapse = ", ")), level = "INFO")

    # Restricted cohort: eligible for both treatments
    restricted_cohort <- full_cohort %>%
        filter(consort_group == "eligible_both") %>%
        mutate(cohort = "Restricted Cohort (Eligible for Both Treatments)")

    # GKSRS-only cohort: ineligible for plaque
    gksrs_only_cohort <- full_cohort %>%
        filter(consort_group == "gksrs_only") %>%
        mutate(cohort = "GKSRS-Only Cohort (Ineligible for Plaque)")
    

    factored_filtered_data <- list(
        uveal_melanoma_full_cohort = full_cohort,
        uveal_melanoma_restricted_cohort = restricted_cohort,
        uveal_melanoma_gksrs_only_cohort = gksrs_only_cohort
    )

    log_enhanced(sprintf("Created %d cohorts", length(factored_filtered_data)), level = "INFO")
    for (cohort in names(factored_filtered_data)) {
        log_enhanced(sprintf("Cohort '%s': %d patients", cohort, nrow(factored_filtered_data[[cohort]])), level = "INFO")
    }

    # CRITICAL: Run all core validations (integrity, factor levels, GEP variables)
    # run_all_core_validations(factored_filtered_data, phase = "data_processing")

    # Produce a comprehensive validation report for the logs directory (optional)
    generate_validation_report(factored_filtered_data)

    return(factored_filtered_data)
}

#' Prepare factor levels for key variables
#'
#' Converts relevant variables to factors with specified levels and orderings for analysis and modeling.
#'
#' @param data Data frame. Patient-level data.
#'
#' @return Data frame with updated factor variables.
#'
#' @examples
#' prepare_factor_levels(data)
prepare_factor_levels <- function(data) {
    log_enhanced("Preparing factor levels for variables", level = "INFO")

    # Clean problematic GEP level names before factoring
    data <- data %>%
        mutate(
            # Clean the problematic GEP level name
            biopsy1_gep = case_when(
                biopsy1_gep == "DISCORDANT CASTLE RESULTS: Class 1A, PRAME not reported" ~ "Class_1A_PRAME_discordant",
                TRUE ~ biopsy1_gep
            )
        ) %>%
        mutate(
            # Outcome variables (using centralized Y/N factor levels)
            recurrence1 = factor(recurrence1, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            mets_progression = factor(mets_progression, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),

            # Treatment group (using centralized factor levels)
            treatment_group = factor(treatment_group,
                levels = TREATMENT_FACTOR_LEVELS
            ),
            
            # Recurrence treatment group for PFS-2 analysis
            recurrence1_treatment_clean = factor(recurrence1_treatment_clean,
                levels = c("Enucleation", "GKSRS", "TTT", "Other"),
                ordered = FALSE  # CRITICAL: Use treatment contrasts, not polynomial
            ),

            # Demographics (using centralized factor levels)
            sex = factor(sex,
                levels = SEX_FACTOR_LEVELS,
                labels = SEX_FACTOR_LEVELS
            ),
            location = factor(location,
                levels = c("Choroidal", "Ciliary_Body", "Cilio_Choroidal", "Conjunctival", "Irido_Ciliary", "Iris"),
                labels = c("Choroidal", "Ciliary Body", "Cilio-Choroidal", "Conjunctival", "Irido-Ciliary", "Iris"),
                ordered = FALSE  # CRITICAL: Use treatment contrasts, not polynomial
            ),
            optic_nerve = factor(optic_nerve, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),

            # Tumor characteristics
            internal_reflectivity = factor(internal_reflectivity,
                levels = c("Very_Low", "Low", "Low_Medium", "Medium", "Medium_High", "High", "Unknown"),
                labels = c("Very Low", "Low", "Low-Medium", "Medium", "Medium-High", "High", "Unknown"),
                ordered = FALSE  # CRITICAL: Use treatment contrasts, not polynomial
            ),
            srf = factor(srf, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            op = factor(op, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            symptoms = factor(symptoms, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            vision_loss_blurred_vision = factor(vision_loss_blurred_vision, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            visual_field_defect = factor(visual_field_defect, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            flashes_photopsia = factor(flashes_photopsia, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            floaters = factor(floaters, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),
            pain = factor(pain, levels = YN_RAW_LEVELS, labels = YN_DISPLAY_LABELS),

            # Staging
            initial_overall_stage = factor(initial_overall_stage,
                levels = c("1", "2A", "2B", "3A", "3B", "3C", "4"),
                ordered = FALSE  # CRITICAL: Use treatment contrasts, not polynomial
            ),
            
            # Create binary stage variable for confounder adjustment
            # Stage IV has very few patients (n=3), so group with Stage I-III for analysis
            initial_stage_binary = factor(
                ifelse(initial_overall_stage == "4", "Stage IV", "Stage I-III"),
                levels = c("Stage I-III", "Stage IV"),  # Stage I-III as reference
                ordered = FALSE  # CRITICAL: Use treatment contrasts, not polynomial
            ),
            biopsy1_gep = factor(biopsy1_gep,
                levels = c(
                    # Class 1A
                    "Class_1A_PRAME_negative",
                    "Class_1A_PRAME_positive",
                    "Class_1A_PRAME_not_reported",
                    # Class 1B
                    "Class_1B_PRAME_negative",
                    "Class_1B_PRAME_positive",
                    # Class 2
                    "Class_2_PRAME_negative",
                    "Class_2_PRAME_positive",
                    "Class_2_PRAME_Unknown",
                    "Class_2_PRAME_not_reported",
                    # Special cases
                    "Failed",
                    "Unknown",
                    "Class_1A_PRAME_discordant"  # Clean name for problematic case
                ), ordered = FALSE  # CRITICAL: Use treatment contrasts, not polynomial
            ),
            
            # GEP-related factors for analysis (Objective 4)
            gep_class_simple = factor(gep_class_simple, 
                levels = c("Class 1A", "Class 1B", "Class 2"),
                ordered = FALSE  # CRITICAL: Use treatment contrasts, not polynomial
            ),
            prame_status = factor(prame_status, 
                levels = c("Negative", "Positive", "Unknown", "Not Available"),
                ordered = FALSE  # CRITICAL: Use treatment contrasts, not polynomial
            )
        )

    # Log new factor levels
    if (VERBOSE) {
        log_enhanced("\nNew factor levels:", level = "INFO")
        factor_vars <- names(data)[sapply(data, is.factor)]
        for (var in factor_vars) {
            message(sprintf("##### %s:", var))
            print(table(data[[var]], useNA = "ifany"))
        }
    }

    # CRITICAL: Create ALL subgroup variables (both static and dynamic) in data processing
    log_enhanced("Creating all subgroup variables for analysis", level = "INFO")
    data <- create_all_subgroup_variables(data)
    
    # CRITICAL: Apply rare category handling to ALL variables that need it (confounders + subgroup variables)
    log_enhanced("Applying rare category handling to all variables", level = "INFO")
    subgroup_vars_to_process <- paste0(continuous_subgroup_vars, "_binned")
    all_vars_to_process <- c(confounders, subgroup_vars_to_process)
    
    rare_result <- handle_rare_categories(data, vars = all_vars_to_process, threshold = THRESHOLD_RARITY)
    data <- rare_result$data
    
    # Log rare category changes for all variables
    if (length(rare_result$other_map) > 0) {
        log_enhanced("Rare categories were collapsed into 'Other':", level = "INFO")
        for (var in names(rare_result$other_map)) {
            log_enhanced(sprintf("  %s: %s", var, paste(rare_result$other_map[[var]], collapse = ", ")), level = "INFO")
        }
    }
    
    # CRITICAL: Ensure all factors are unordered for modeling (use treatment contrasts, not polynomial)
    data <- enforce_unordered_factors(data, verbose = VERBOSE)

    return(list(data = data, other_map = rare_result$other_map))
}

#' Create all subgroup variables for analysis
#'
#' Creates both static and dynamic subgroup variables for all cohorts.
#' This includes binned versions of continuous variables and any other
#' subgroup variables needed for analysis.
#'
#' @param data Data frame
#' @return Data frame with all subgroup variables added
create_all_subgroup_variables <- function(data) {
    log_enhanced("Creating subgroup variables for analysis", level = "INFO")
    
    # Create binned versions of continuous variables
    data <- data %>%
        mutate(
            # Age at diagnosis - median split
            age_at_diagnosis_binned = factor(
                ifelse(age_at_diagnosis < median(age_at_diagnosis, na.rm = TRUE),
                       paste0("< ", round(median(age_at_diagnosis, na.rm = TRUE), 1)),
                       paste0("≥ ", round(median(age_at_diagnosis, na.rm = TRUE), 1))),
                levels = c(
                    paste0("< ", round(median(age_at_diagnosis, na.rm = TRUE), 1)),
                    paste0("≥ ", round(median(age_at_diagnosis, na.rm = TRUE), 1))
                )
            ),
            
            # Tumor height - T-stage clinical bins
            initial_tumor_height_binned = factor(
                case_when(
                    initial_tumor_height <= 3.0 ~ "≤ 3.0 mm",
                    initial_tumor_height <= 6.0 ~ "3.1-6.0 mm", 
                    initial_tumor_height <= 9.0 ~ "6.1-9.0 mm",
                    initial_tumor_height <= 12.0 ~ "9.1-12.0 mm",
                    initial_tumor_height <= 15.0 ~ "12.1-15.0 mm",
                    initial_tumor_height > 15.0 ~ "> 15.0 mm",
                    TRUE ~ NA_character_
                ),
                levels = c("≤ 3.0 mm", "3.1-6.0 mm", "6.1-9.0 mm", 
                          "9.1-12.0 mm", "12.1-15.0 mm", "> 15.0 mm")
            ),
            
            # Tumor diameter - T-stage clinical bins  
            initial_tumor_diameter_binned = factor(
                case_when(
                    initial_tumor_diameter <= 3.0 ~ "≤ 3.0 mm",
                    initial_tumor_diameter <= 6.0 ~ "3.1-6.0 mm",
                    initial_tumor_diameter <= 9.0 ~ "6.1-9.0 mm", 
                    initial_tumor_diameter <= 12.0 ~ "9.1-12.0 mm",
                    initial_tumor_diameter <= 15.0 ~ "12.1-15.0 mm",
                    initial_tumor_diameter <= 18.0 ~ "15.1-18.0 mm",
                    initial_tumor_diameter > 18.0 ~ "> 18.0 mm",
                    TRUE ~ NA_character_
                ),
                levels = c("≤ 3.0 mm", "3.1-6.0 mm", "6.1-9.0 mm",
                          "9.1-12.0 mm", "12.1-15.0 mm", "15.1-18.0 mm", "> 18.0 mm")
            )
        )
    
    return(data)
}

#' Calculate treatment duration metrics
#'
#' Computes follow-up time, 5-year intervals, and summary statistics for each treatment group.
#'
#' @param data Data frame. Patient-level data with follow-up and treatment dates.
#'
#' @return A list with:
#'   - interval_metrics: Data frame of patient counts per interval and group.
#'   - summary_stats: Data frame of summary statistics by treatment group.
#'
#' @examples
#' calculate_treatment_duration_metrics(data)
calculate_treatment_duration_metrics <- function(data) {

    # Calculate years from treatment date to last follow-up
    data <- data %>%
        mutate(
            # Calculate total follow-up time in days
            total_followup_days = as.numeric(difftime(last_known_alive_date, treatment_date, units = "days")),
            # Calculate years, handling NA and negative values
            total_years = case_when(
                is.na(total_followup_days) | total_followup_days < 0 ~ 0,
                TRUE ~ total_followup_days / DAYS_IN_YEAR
            )
        )

    # Log any problematic cases
    if (VERBOSE) {
        log_enhanced("Checking for problematic follow-up times:", level = "INFO")
        problematic_cases <- data %>%
            # Valid follow-up time is greater than 0 and not NA
            filter(is.na(total_followup_days) | total_followup_days < 0) %>%
            select(id, treatment_group, treatment_date, last_known_alive_date, total_followup_days)
        
        if (nrow(problematic_cases) > 0) {
            print(problematic_cases)
        } else {
            log_enhanced("No problematic follow-up times found", level = "INFO")
        }
    }

    # Create 5-year intervals
    max_years <- ceiling(max(data$total_years, na.rm = TRUE))
    intervals <- seq(0, max_years, by = 5)

    # Create interval data for each patient
    interval_data <- data %>%
        select(id, treatment_group, total_years) %>%
        filter(total_years > 0) %>% # Only include patients with valid follow-up
        # Create a row for each interval for each patient
        crossing(interval_end = intervals) %>%
        # Only keep intervals where the patient was still in follow-up
        filter(interval_end <= total_years)

    # Calculate patients per treatment per interval
    interval_metrics <- interval_data %>%
        group_by(interval_end, treatment_group) %>%
        summarise(
            n_patients = n(),
            .groups = "drop"
        ) %>%
        pivot_wider(
            names_from = treatment_group,
            values_from = n_patients,
            names_prefix = "n_"
        ) %>%
        mutate(
            interval_label = sprintf("%d years", interval_end)
        )

    # Add summary statistics
    log_enhanced("\nTreatment duration summary:", level = "INFO")
    summary_stats <- data %>%
        group_by(treatment_group) %>%
        summarise(
            n_total = n(),
            n_valid_followup = sum(!is.na(total_followup_days) & total_followup_days >= 0),
            mean_followup_years = mean(total_years[!is.na(total_years) & total_years >= 0], na.rm = TRUE),
            median_followup_years = median(total_years[!is.na(total_years) & total_years >= 0], na.rm = TRUE),
            max_followup_years = max(total_years[!is.na(total_years) & total_years >= 0], na.rm = TRUE),
            .groups = "drop"
        )
    if (VERBOSE) {
        print(summary_stats)
    }
    
    return(list(
        interval_metrics = interval_metrics,
        summary_stats = summary_stats
    ))
}

#' Create summary tables using gtsummary
#'
#' Generates and saves summary tables for each cohort, including baseline characteristics and treatment duration metrics.
#'
#' @param data_list List of data frames. Each element is a cohort data frame.
#'
#' @return A named list of lists, each containing the summary tables for a cohort.
#'
#' @examples
#' create_summary_tables(list(full_cohort = df1, ...), level = "INFO")
create_summary_tables <- function(data_list, output_dirs = NULL) {
    log_enhanced("Creating summary tables", level = "INFO")

    # Use globally defined variables for baseline characteristics summary
    vars_to_summarize <- BASELINE_VARIABLES_TO_SUMMARIZE

    log_enhanced(sprintf("Summarizing %d variables", length(vars_to_summarize)), level = "INFO")

    # Create tables for each cohort
    tables <- lapply(names(data_list), function(cohort_name) {
        message(sprintf("\nCreating table for cohort: %s", cohort_name))
        data <- data_list[[cohort_name]]

        # Set up file prefix based on cohort name
        prefix <- case_when(
            grepl("full", cohort_name) ~ "full_cohort_",
            grepl("restricted", cohort_name) ~ "restricted_cohort_", 
            grepl("gksrs", cohort_name) ~ "gksrs_only_cohort_",
            TRUE ~ paste0(cohort_name, "_")
        )
        
        # Use objective-based directory structure
        if (!is.null(output_dirs) && !is.null(output_dirs[[cohort_name]])) {
            treatment_duration_dir <- output_dirs[[cohort_name]]$treatment_duration
            baseline_output_dir <- output_dirs[[cohort_name]]$baseline_characteristics
        } else {
            # Fallback to main Analysis directory
            treatment_duration_dir <- file.path("final_data/Analysis", "00_General", "treatment_duration")
            baseline_output_dir <- file.path("final_data/Analysis", "00_General", "baseline_characteristics")
        }
        
        # Ensure directories exist
        dir.create(treatment_duration_dir, showWarnings = FALSE, recursive = TRUE)
        dir.create(baseline_output_dir, showWarnings = FALSE, recursive = TRUE)

        # Calculate treatment duration metrics
        log_enhanced("Calculating treatment duration metrics", level = "INFO")
        duration_metrics <- calculate_treatment_duration_metrics(data)

        # Save duration metrics
        log_enhanced("Saving treatment duration metrics", level = "INFO")
        write.csv(
            duration_metrics$interval_metrics,
            file.path(treatment_duration_dir, paste0(prefix, "treatment_duration_metrics.csv")),
            row.names = FALSE
        )

        # Save summary statistics
        write.csv(
            duration_metrics$summary_stats,
            file.path(treatment_duration_dir, paste0(prefix, "treatment_duration_summary.csv")),
            row.names = FALSE
        )

        log_enhanced("Preparing variables for table", level = "INFO")
        data <- data %>%
            select(all_of(vars_to_summarize), treatment_group) 

        # Check for variables with insufficient levels for statistical testing (but keep all for display)
        log_enhanced("Checking variable levels for statistical testing", level = "INFO")
        vars_with_insufficient_levels <- c()
        
        for (var in vars_to_summarize) {
            if (var %in% names(data)) {
                if (is.factor(data[[var]]) || is.character(data[[var]])) {
                    # Check levels for categorical variables
                    level_counts <- table(data[[var]], useNA = "no")
                    valid_levels <- sum(level_counts > 0)
                    
                    if (valid_levels < 2) {
                        log_enhanced(sprintf("Variable '%s' has insufficient levels for statistical testing (%d levels). Will display but skip p-value. Counts: %s", 
                                           var, valid_levels, paste(names(level_counts), "=", level_counts, collapse=", ")), 
                                     level = "INFO")
                        vars_with_insufficient_levels <- c(vars_with_insufficient_levels, var)
                    }
                }
            } else {
                log_enhanced(sprintf("Variable '%s' not found in data, excluding from summary table", var), level = "WARNING")
            }
        }
        
        # Keep all available variables for display (only exclude truly missing ones)
        available_vars <- intersect(vars_to_summarize, names(data))
        log_enhanced(sprintf("Displaying %d baseline variables (%d have insufficient levels for testing)", 
                           length(available_vars), length(vars_with_insufficient_levels)), level = "INFO")
        
        if (length(vars_with_insufficient_levels) > 0) {
            log_enhanced(sprintf("Variables with insufficient levels for p-values: %s", 
                               paste(vars_with_insufficient_levels, collapse = ", ")), level = "INFO")
        }
        
        # Update data selection to include all available variables
        data <- data %>%
            select(all_of(available_vars), treatment_group)

        log_enhanced("Creating summary table", level = "INFO")
        tbl <- data %>%
            tbl_summary(
                by = treatment_group,
                type = list(
                    age_at_diagnosis ~ "continuous",
                    initial_vision ~ "continuous",
                    initial_tumor_height ~ "continuous",
                    initial_tumor_diameter ~ "continuous"
                ),
                statistic = list(
                    all_continuous() ~ "{mean} ({sd})",
                    all_categorical() ~ "{n} ({p}%)"
                ),
                digits = list(all_continuous() ~ 1, all_categorical() ~ 1),
                missing = "no",
                label = STANDARD_TABLE_LABELS[intersect(names(STANDARD_TABLE_LABELS), available_vars)]  # Only use labels for available variables
            ) %>%
            add_overall()
        
        # Add p-values with error handling for variables with insufficient levels
        log_enhanced("Adding statistical tests (will skip variables with insufficient levels)", level = "INFO")
        tbl <- tryCatch({
            tbl %>%
                add_p(test = list(all_categorical() ~ "fisher.test"), 
                      test.args = list(all_categorical() ~ list(simulate.p.value = TRUE)))
        }, error = function(e) {
            log_enhanced(sprintf("Some statistical tests failed (expected for variables with <2 levels): %s", e$message), level = "INFO")
            # Return table without p-values if there are issues
            tbl
        })
        
        # Continue with formatting
        tbl <- tbl %>%
            bold_labels() %>%       # Built-in gtsummary function for bold variable labels!
            modify_header(
                label = "**Characteristic**",
                stat_0 = "**Overall**\nN = {N}"
            ) %>%
            modify_caption("Baseline Characteristics")
        
        # Convert to gt with error handling
        log_enhanced("Converting to gt table format", level = "INFO")
        tbl <- tryCatch({
            tbl %>% as_gt()
        }, error = function(e) {
            log_enhanced(sprintf("Error in as_gt(): %s", e$message), level = "ERROR")
            log_enhanced("This may be due to variables with insufficient levels for statistical comparison", level = "INFO")
            stop(sprintf("Failed to create baseline characteristics table: %s", e$message))
        })

        # Add treatment duration metrics to the table
        log_enhanced("Adding treatment duration metrics to table", level = "INFO")
        duration_tbl <- duration_metrics$interval_metrics %>%
            select(interval_label, `n_Plaque`, `n_GKSRS`) %>%
            gt() %>%
            tab_header(
                title = "Number of Patients by Treatment Group Over Time",
                subtitle = "5-year intervals of patients remaining in study"
            ) %>%
            cols_label(
                interval_label = "Time Point",
                `n_Plaque` = "Plaque Brachytherapy",
                `n_GKSRS` = "Gamma Knife SRS"
            ) %>%
            tab_options(
                heading.title.font.size = 20,
                heading.subtitle.font.size = 16,
                column_labels.font.size = 14,
                data_row.padding = px(8)
            )

        # Create summary statistics table
        summary_tbl <- duration_metrics$summary_stats %>%
            gt() %>%
            tab_header(
                title = "Treatment Duration Summary Statistics",
                subtitle = "Follow-up time statistics by treatment group"
            ) %>%
            cols_label(
                treatment_group = "Treatment Group",
                n_total = "Total Patients",
                n_valid_followup = "Patients with Valid Follow-up",
                mean_followup_years = "Mean Follow-up (years)",
                median_followup_years = "Median Follow-up (years)",
                max_followup_years = "Maximum Follow-up (years)"
            ) %>%
            fmt_number(
                columns = c(mean_followup_years, median_followup_years, max_followup_years),
                decimals = 1
            ) %>%
            tab_options(
                heading.title.font.size = 20,
                heading.subtitle.font.size = 16,
                column_labels.font.size = 14,
                data_row.padding = px(8)
            )

        # Save tables
        log_enhanced("Saving tables", level = "INFO")
        save_gt_html(
            duration_tbl,
            filename = file.path(treatment_duration_dir, paste0(prefix, "treatment_duration.html"))
        )

        save_gt_html(
            summary_tbl,
            filename = file.path(treatment_duration_dir, paste0(prefix, "treatment_duration_summary.html"))
        )

        # baseline_output_dir was already set above
        
        # Save baseline characteristics table with automatic factor level indentation
        log_enhanced("Saving baseline table with automatic factor level indentation", level = "INFO")
        save_gt_html(
            tbl,
            filename = file.path(baseline_output_dir, paste0(prefix, "baseline_characteristics.html"))
        )

        return(list(
            baseline_table = tbl,
            duration_table = duration_tbl,
            summary_table = summary_tbl
        ))
    })

    names(tables) <- names(data_list)
    return(tables)
}

#' Save each cohort separately
#'
#' Saves each cohort as an Excel and RDS file in the processed data directory.
#'
#' @param cohort_data A named list of data frames. Each element is a cohort data frame.
#'
#' @return None. Side effect: saves files to the processed data directory.
save_cohorts <- function(cohort_data) {
    log_enhanced(sprintf("Saving processed data in %s", PROCESSED_DATA_DIR), level = "INFO")

    for (cohort_name in names(cohort_data)) {
        log_enhanced(sprintf("Saving cohort: %s", cohort_name), level = "INFO")
        # Save as Excel
        write_xlsx(
            cohort_data[[cohort_name]],
            file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".xlsx"))
        )
        # Save as RDS
        saveRDS(
            cohort_data[[cohort_name]],
            file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".rds"))
        )
    }
}

#' Main processing function to create analytic dataset
#'
#' Orchestrates the full data processing pipeline: loads, cleans, applies criteria, creates derived variables, summary tables, and saves outputs.
#'
#' @return A list with:
#'   - analytic_data: Named list of processed cohort data frames.
#'   - summary_tables: Named list of summary tables for each cohort.
#'   - other_map: Named list mapping variable names to categories collapsed into "Other" for each cohort.
#'
#' @examples
#' create_analytic_dataset()
create_analytic_dataset <- function() {
    log_enhanced("Starting data processing pipeline", level = "INFO")

    # Load and clean raw data
    log_enhanced("Loading and cleaning raw data", level = "INFO")
    raw_data <- load_and_clean_data(INPUT_FILENAME)
    log_enhanced(sprintf("Loaded %d rows of raw data", nrow(raw_data)), level = "INFO")

    # Create derived variables BEFORE splitting into cohorts
    log_enhanced("Creating derived variables", level = "INFO")
    derived_data <- create_derived_variables(raw_data)

    log_enhanced("Preparing factor levels", level = "INFO")
    factored_data <- prepare_factor_levels(derived_data)

    # Apply inclusion/exclusion criteria (split into cohorts)
    log_enhanced("Applying inclusion/exclusion criteria", level = "INFO")
    factored_filtered_data <- apply_criteria(factored_data)
    log_enhanced(sprintf("Created %d cohorts", length(factored_filtered_data)), level = "INFO")
    for (cohort in names(factored_filtered_data)) {
        log_enhanced(sprintf("Cohort '%s': %d patients", cohort, nrow(factored_filtered_data[[cohort]])), level = "INFO")
    }

    # Collapse rare categories and track which categories were collapsed into "Other"
    log_enhanced("Collapsing rare categories", level = "INFO")
    other_map <- list()
    for (cohort_name in names(factored_filtered_data)) {
        log_enhanced(sprintf("Processing rare categories for cohort: %s", cohort_name), level = "INFO")
        
        # Get the list of variables that might need category collapsing
        # Focus on categorical variables that are commonly used in analysis
        potential_vars <- c("location", "initial_t_stage", "biopsy1_gep", "srd_cause")
        
        # Filter to variables that exist in the data and are factors
        factor_vars <- intersect(potential_vars, names(factored_filtered_data[[cohort_name]]))
        factor_vars <- factor_vars[sapply(factored_filtered_data[[cohort_name]][factor_vars], is.factor)]
        
        if (length(factor_vars) > 0) {
            # Collapse rare categories for this cohort
            collapse_result <- collapse_rare_categories(factored_filtered_data[[cohort_name]], factor_vars)
            factored_filtered_data[[cohort_name]] <- collapse_result$data
            other_map[[cohort_name]] <- collapse_result$other_map
            
            # Log what was collapsed
            if (length(collapse_result$other_map) > 0) {
                log_enhanced(sprintf("Categories collapsed into 'Other' for cohort %s:", cohort_name), level = "INFO")
                for (var_name in names(collapse_result$other_map)) {
                    collapsed_cats <- collapse_result$other_map[[var_name]]
                    log_enhanced(sprintf("  %s: %s", var_name, paste(collapsed_cats, collapse = ", ")), level = "INFO")
                }
            } else {
                log_enhanced(sprintf("No categories collapsed for cohort %s", cohort_name), level = "INFO")
            }
        } else {
            other_map[[cohort_name]] <- list()
            log_enhanced(sprintf("No factor variables to process for cohort %s", cohort_name), level = "INFO")
        }
    }

    # Create summary tables
    log_enhanced("Creating summary tables", level = "INFO")
    summary_tables <- create_summary_tables(factored_filtered_data)

    # Save each cohort separately
    log_enhanced("Saving processed data", level = "INFO")
    for (cohort_name in names(factored_filtered_data)) {
        log_enhanced(sprintf("Saving cohort: %s", cohort_name), level = "INFO")
        # Save as Excel
        write_xlsx(
            factored_filtered_data[[cohort_name]],
            file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".xlsx"))
        )
        # Save as RDS
        saveRDS(
            factored_filtered_data[[cohort_name]],
            file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".rds"))
        )
    }

    # Save the other_map information for use in analysis
    saveRDS(other_map, file.path(PROCESSED_DATA_DIR, "other_map.rds"))
    log_enhanced("Saved other_map information for tracking collapsed categories", level = "INFO")

    # Optional: generate a detailed log file for review
    generate_validation_report(factored_filtered_data)

    return(list(
        analytic_data = factored_filtered_data,
        summary_tables = summary_tables,
        other_map = other_map
    ))
}

















