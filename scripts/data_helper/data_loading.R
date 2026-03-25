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
    logger::log_info(sprintf("Checking consistency between %s and %s", event_var, date_var))

    n_event_should_be_yes <- sum(!is.na(data[[date_var]]) & data[[event_var]] != event_yes, na.rm = TRUE)
    n_date_should_be_na <- sum(data[[event_var]] == event_yes & is.na(data[[date_var]]), na.rm = TRUE)

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
        logger::log_info(sprintf("Found %d events with dates", sum(!is.na(data[[date_var]]))))
        logger::log_info(sprintf("Found %d events marked as '%s'", sum(data[[event_var]] == event_yes, na.rm = TRUE), event_yes))
        logger::log_info(sprintf(
            "Event/date consistency check for '%s' and '%s':", event_var, date_var
        ))
        logger::log_info(sprintf(
            "  - Number of records with a non-missing %s: %d", date_var, sum(!is.na(data[[date_var]]))
        ))
        logger::log_info(sprintf(
            "  - Number of records with %s marked as '%s': %d", event_var, event_yes, sum(data[[event_var]] == event_yes, na.rm = TRUE)
        ))
        logger::log_info(sprintf(
            "  - Fixed %d records where %s was not '%s' but %s was present (set event to '%s')",
            n_event_should_be_yes, event_var, event_yes, date_var, event_yes
        ))
        logger::log_info(sprintf(
            "  - Fixed %d records where %s was '%s' but %s was missing (set date to NA)",
            n_date_should_be_na, event_var, event_yes, date_var
        ))
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
    assert_required_input_paths(input_filename = filename, require_data_dictionary = FALSE)
    logger::log_info(sprintf("Loading data from directory: %s", RAW_DATA_DIR))
    logger::log_info(sprintf("Loading data from file: %s", filename))
    raw_data <- read_excel(
        file.path(RAW_DATA_DIR, filename),
        sheet = 1
    ) %>%
        dplyr::select(-contains("..."))

    cleaned_data <- raw_data %>%
        mutate(across(everything(), ~ {
            if (is.character(.)) {
                case_when(
                    . %in% c("NA", "N/A", "n/a", "na", "", " ") ~ NA_character_,
                    TRUE ~ .
                )
            } else {
                .
            }
        })) %>%
        mutate(
            location = case_when(
                location %in% c("Cilio_Choroidal", "Cilio_choroidal") ~ "Cilio_Choroidal",
                TRUE ~ location
            )
        ) %>%
        filter(!if_all(everything(), is.na)) %>%
        distinct() %>%
        mutate(
            consort_group = case_when(
                !is.na(initial_gk) | !is.na(initial_plaque) ~ case_when(
                    initial_tumor_diameter <= TUMOR_DIAMETER_THRESHOLD &
                        initial_tumor_height <= TUMOR_HEIGHT_THRESHOLD &
                        optic_nerve == "N" ~ "eligible_both",
                    initial_tumor_diameter > TUMOR_DIAMETER_THRESHOLD |
                        initial_tumor_height > TUMOR_HEIGHT_THRESHOLD |
                        optic_nerve == "Y" ~ "gksrs_only",
                    TRUE ~ "other"
                ),
                TRUE ~ NA_character_
            )
        )

    logger::log_info("eligible_both: initial_tumor_diameter <= 20mm, initial_tumor_height <= 10mm, optic_nerve == 'N'")
    logger::log_info("gksrs_only: initial_tumor_diameter > 20mm, initial_tumor_height > 10mm, optic_nerve == 'Y'")
    logger::log_info("other: catch-all for any other cases")
    message("\n")
    logger::log_info(sprintf("Found %d patients in full cohort", nrow(cleaned_data)))
    logger::log_info(sprintf("Found %d patients in restricted cohort", nrow(cleaned_data %>% filter(consort_group == "eligible_both"))))
    logger::log_info(sprintf("Found %d patients in GKSRS-only cohort", nrow(cleaned_data %>% filter(consort_group == "gksrs_only"))))
    logger::log_info(sprintf("Found %d patients in other cohort", nrow(cleaned_data %>% filter(consort_group == "other"))))
    print(cleaned_data %>% filter(consort_group == "other") %>% select(id, initial_tumor_diameter, initial_tumor_height, optic_nerve))
    message("\n")
    logger::log_info("NOTE: NOT splitting into cohorts yet!")
    message("\n")

    cleaned_data <- fix_event_date_consistency(cleaned_data, "initial_gk", "initial_gk_date")
    cleaned_data <- fix_event_date_consistency(cleaned_data, "initial_plaque", "initial_plaque_date")
    cleaned_data <- fix_event_date_consistency(cleaned_data, "recurrence1", "recurrence1_date")
    cleaned_data <- fix_event_date_consistency(cleaned_data, "recurrence2", "recurrence2_date")
    cleaned_data <- fix_event_date_consistency(cleaned_data, "recurrence3", "recurrence3_date")
    cleaned_data <- fix_event_date_consistency(cleaned_data, "mets_progression", "mets_progression_date")
    cleaned_data <- fix_event_date_consistency(cleaned_data, "enucleation", "enucleation_date")

    cleaned_data <- cleaned_data %>%
        mutate(across(contains("date|dob|dod|last\\_followup", ignore.case = TRUE), as.Date))

    date_cols <- colnames(cleaned_data)[
        grepl("date", colnames(cleaned_data), ignore.case = TRUE) |
            grepl("dob", colnames(cleaned_data), ignore.case = TRUE) |
            grepl("dod", colnames(cleaned_data), ignore.case = TRUE) |
            grepl("last_followup", colnames(cleaned_data), ignore.case = TRUE)
    ]

    cleaned_data_final <- cleaned_data %>%
        mutate(
            last_known_alive_date = pmax(!!!syms(date_cols), na.rm = TRUE),
            last_known_alive_source = apply(
                pick(all_of(date_cols)),
                1,
                function(row) {
                    if (all(is.na(row))) {
                        return(NA_character_)
                    }
                    max_date <- max(row, na.rm = TRUE)
                    names(row)[which(row == max_date)[1]]
                }
            )
        )

    logger::log_info(sprintf("Loaded %d rows of raw data", nrow(cleaned_data_final)))

    return(cleaned_data_final)
}
