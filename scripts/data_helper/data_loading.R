#' Pick the preferred identifier column for reconciliation audit artifacts
#'
#' @param data Data frame under review
#' @return Character scalar column name or `NA_character_` when unavailable
pick_event_date_audit_id_col <- function(data) {
    id_candidates <- c("study_id", "id", "patient_id", "record_id", "case_id")
    matching_col <- id_candidates[id_candidates %in% names(data)][1]
    if (length(matching_col) == 0 || is.na(matching_col)) {
        return(NA_character_)
    }

    matching_col
}

#' Convert an event/date pair into a compact audit string
#'
#' @param event_value Event indicator value
#' @param date_value Date value
#' @return Character scalar summary of the paired state
format_event_date_state <- function(event_value, date_value) {
    event_text <- ifelse(is.na(event_value), "NA", as.character(event_value))
    date_text <- ifelse(is.na(date_value), "NA", as.character(date_value))
    sprintf("event=%s | date=%s", event_text, date_text)
}

#' Create an empty reconciliation-audit table with the canonical columns
#'
#' @return Tibble with audit columns and zero rows
empty_event_date_audit_rows <- function() {
    tibble::tibble(
        source_workbook = character(),
        id_column = character(),
        study_id = character(),
        row_index = integer(),
        event_var = character(),
        date_var = character(),
        original_event = character(),
        original_date = character(),
        reconciled_event = character(),
        reconciled_date = character(),
        original_state = character(),
        reconciled_state = character(),
        action_taken = character()
    )
}

#' Create an empty reconciliation-audit summary table with canonical columns
#'
#' @return Tibble with summary columns and zero rows
empty_event_date_audit_summary <- function() {
    tibble::tibble(
        source_workbook = character(),
        id_column = character(),
        event_var = character(),
        date_var = character(),
        records_with_present_date = integer(),
        records_marked_event_yes_after = integer(),
        n_event_set_to_yes = integer(),
        n_event_set_to_no_missing_date = integer(),
        n_rows_reconciled = integer()
    )
}

#' Create an empty manual-date-correction audit table with canonical columns
#'
#' @return Tibble with manual date correction audit columns and zero rows
empty_manual_date_correction_audit_rows <- function() {
    tibble::tibble(
        source_workbook = character(),
        id_column = character(),
        study_id = character(),
        column_name = character(),
        original_value = character(),
        corrected_value = character(),
        correction_reason = character(),
        confidence_tier = character(),
        supporting_columns = character(),
        supporting_values = character(),
        original_support_gap_days = numeric(),
        corrected_support_gap_days = numeric(),
        gap_improvement_days = numeric(),
        action_taken = character()
    )
}

#' Apply versioned manual source-date corrections during data loading
#'
#' Uses the configured correction table to overwrite specific raw date values
#' before event/date reconciliation runs, while recording a row-level audit trail
#' that can be published with Objective 0 artifacts.
#'
#' @param data Data frame containing the raw workbook contents after basic cleaning
#' @param corrections Tibble of manual corrections, typically `MANUAL_DATE_CORRECTIONS`
#' @param id_col Identifier column used to match correction rows to data rows
#' @param source_workbook Source workbook name for audit reporting
#' @return Named list with corrected `data` and row-level `audit_rows`
apply_manual_date_corrections <- function(data,
                                          corrections = MANUAL_DATE_CORRECTIONS,
                                          id_col = NA_character_,
                                          source_workbook = NA_character_) {
    if (is.null(corrections) || nrow(corrections) == 0) {
        return(list(
            data = data,
            audit_rows = empty_manual_date_correction_audit_rows()
        ))
    }

    if (is.na(id_col) || !id_col %in% names(data)) {
        logger::log_warn("Manual date corrections were configured but no usable ID column was available; no corrections were applied.")
        return(list(
            data = data,
            audit_rows = empty_manual_date_correction_audit_rows()
        ))
    }

    corrected_data <- data
    audit_rows <- list()

    coerce_manual_correction_value <- function(template_column, corrected_value) {
        if (inherits(template_column, c("POSIXct", "POSIXt"))) {
            tz_value <- attr(template_column, "tzone", exact = TRUE) %||% "UTC"
            return(as.POSIXct(as.Date(corrected_value), tz = tz_value))
        }

        if (inherits(template_column, "Date")) {
            return(as.Date(corrected_value))
        }

        corrected_value
    }

    build_supporting_value_summary <- function(data_row, supporting_cols) {
        if (length(supporting_cols) == 0) {
            return(NA_character_)
        }

        pieces <- purrr::map_chr(supporting_cols, function(col_name) {
            if (!col_name %in% names(data_row)) {
                return(sprintf("%s=<missing_column>", col_name))
            }

            value <- data_row[[col_name]][[1]]
            value_text <- if (is.na(value)) {
                "NA"
            } else {
                as.character(as.Date(value))
            }

            sprintf("%s=%s", col_name, value_text)
        })

        paste(pieces, collapse = "; ")
    }

    compute_support_gap <- function(candidate_value, data_row, supporting_cols) {
        if (length(supporting_cols) == 0 || is.na(candidate_value)) {
            return(NA_real_)
        }

        support_dates <- purrr::map(supporting_cols, function(col_name) {
            if (!col_name %in% names(data_row)) {
                return(NA)
            }
            data_row[[col_name]][[1]]
        }) %>%
            purrr::compact() %>%
            unlist()

        if (length(support_dates) == 0) {
            return(NA_real_)
        }

        support_dates <- as.Date(support_dates)
        support_dates <- support_dates[!is.na(support_dates)]

        if (length(support_dates) == 0) {
            return(NA_real_)
        }

        min(abs(as.numeric(as.Date(candidate_value) - support_dates)))
    }

    for (row_index in seq_len(nrow(corrections))) {
        correction_row <- corrections[row_index, , drop = FALSE]
        target_id <- as.character(correction_row$study_id[[1]])
        target_col <- correction_row$column_name[[1]]
        corrected_value <- correction_row$corrected_value[[1]]
        correction_reason <- correction_row$correction_reason[[1]]
        confidence_tier <- correction_row$confidence_tier[[1]] %||% NA_character_
        supporting_columns <- correction_row$supporting_columns[[1]] %||% NA_character_
        supporting_cols <- trimws(unlist(strsplit(as.character(supporting_columns), ",", fixed = TRUE)))
        supporting_cols <- supporting_cols[nzchar(supporting_cols)]

        if (!target_col %in% names(corrected_data)) {
            logger::log_warn(sprintf(
                "Skipping manual date correction for study_id=%s because column '%s' is not present.",
                target_id,
                target_col
            ))
            next
        }

        matching_rows <- which(as.character(corrected_data[[id_col]]) == target_id)
        if (length(matching_rows) == 0) {
            logger::log_warn(sprintf(
                "Skipping manual date correction for study_id=%s because no matching row was found.",
                target_id
            ))
            next
        }

        for (match_idx in matching_rows) {
            original_value <- corrected_data[[target_col]][[match_idx]]
            corrected_value_cast <- coerce_manual_correction_value(corrected_data[[target_col]], corrected_value)
            corrected_data[[target_col]][[match_idx]] <- corrected_value_cast
            data_row <- corrected_data[match_idx, , drop = FALSE]
            original_support_gap <- compute_support_gap(original_value, data_row, supporting_cols)
            corrected_support_gap <- compute_support_gap(corrected_value_cast, data_row, supporting_cols)

            audit_rows[[length(audit_rows) + 1L]] <- tibble::tibble(
                source_workbook = basename(source_workbook),
                id_column = id_col,
                study_id = target_id,
                column_name = target_col,
                original_value = ifelse(is.na(original_value), NA_character_, as.character(as.Date(original_value))),
                corrected_value = ifelse(is.na(corrected_value_cast), NA_character_, as.character(as.Date(corrected_value_cast))),
                correction_reason = correction_reason,
                confidence_tier = as.character(confidence_tier),
                supporting_columns = ifelse(length(supporting_cols) == 0, NA_character_, paste(supporting_cols, collapse = ", ")),
                supporting_values = build_supporting_value_summary(data_row, supporting_cols),
                original_support_gap_days = original_support_gap,
                corrected_support_gap_days = corrected_support_gap,
                gap_improvement_days = ifelse(
                    is.na(original_support_gap) || is.na(corrected_support_gap),
                    NA_real_,
                    original_support_gap - corrected_support_gap
                ),
                action_taken = "manual_source_date_correction"
            )
        }
    }

    audit_rows <- dplyr::bind_rows(audit_rows)

    if (nrow(audit_rows) > 0) {
        logger::log_warn(sprintf(
            "Applied %d versioned manual raw-date correction(s) during Objective 0 loading. Review the 00_General audit workbook for details.",
            nrow(audit_rows)
        ))
    }

    list(
        data = corrected_data,
        audit_rows = audit_rows %||% empty_manual_date_correction_audit_rows()
    )
}

#' List the raw input columns expected by the loader audit
#'
#' @return Character vector of required raw workbook column names
required_raw_input_columns <- function() {
    c(
        "id",
        "initial_gk",
        "initial_plaque",
        "initial_gk_date",
        "initial_plaque_date",
        "date_diagnosis",
        "dob",
        "initial_tumor_height",
        "initial_tumor_diameter",
        "optic_nerve",
        "recurrence1",
        "recurrence1_date",
        "mets_progression",
        "mets_progression_date",
        "enucleation",
        "enucleation_date"
    )
}

#' Collect loader-side audit metadata about the raw workbook input
#'
#' Summarizes row counts, required-column coverage, and duplicate identifier
#' patterns so Objective 0 reporting can show what changed between the raw sheet
#' and the cleaned distinct dataset.
#'
#' @param raw_data Raw workbook data before distinct-row cleanup
#' @param cleaned_data Cleaned workbook data after distinct-row cleanup
#' @param source_workbook Source workbook name for audit reporting
#' @return Named list containing row counts, required-column checks, and
#'   duplicate-ID summaries
collect_raw_input_audit <- function(raw_data, cleaned_data, source_workbook) {
    id_col <- pick_event_date_audit_id_col(cleaned_data)
    required_columns <- required_raw_input_columns()
    missing_required <- setdiff(required_columns, names(raw_data))

    duplicate_id_rows <- tibble::tibble()
    if (!is.na(id_col) && id_col %in% names(cleaned_data)) {
        duplicate_id_rows <- cleaned_data %>%
            dplyr::filter(!is.na(.data[[id_col]])) %>%
            dplyr::count(.data[[id_col]], name = "n_records") %>%
            dplyr::filter(.data$n_records > 1) %>%
            dplyr::rename(study_id = !!id_col)
    }

    list(
        source_workbook = basename(source_workbook),
        id_column = id_col,
        raw_row_count = nrow(raw_data),
        cleaned_row_count = nrow(cleaned_data),
        required_columns = required_columns,
        missing_required_columns = missing_required,
        duplicate_id_rows = duplicate_id_rows,
        duplicate_row_count = nrow(raw_data) - nrow(cleaned_data)
    )
}

#' Identify iris-tumor optic-nerve non-applicability rows
#'
#' Finds raw rows where `optic_nerve` is recorded as `N/A` for an iris tumor.
#' In this project, that raw state means optic nerve abutment is not applicable,
#' but the row remains a full-cohort-only special case rather than a restricted
#' or GKSRS-only cohort member.
#'
#' @param raw_data Data frame read directly from the source workbook.
#' @return Tibble with row-level special-case audit fields.
collect_iris_optic_nerve_special_cases <- function(raw_data) {
    required_fields <- c("id", "location", "optic_nerve")
    if (!all(required_fields %in% names(raw_data))) {
        return(tibble::tibble(
            id = numeric(),
            raw_location = character(),
            raw_optic_nerve = character(),
            special_case = character(),
            interpretation = character()
        ))
    }

    raw_data %>%
        dplyr::transmute(
            id = .data$id,
            raw_location = as.character(.data$location),
            raw_optic_nerve = as.character(.data$optic_nerve)
        ) %>%
        dplyr::filter(.data$raw_location == "Iris", .data$raw_optic_nerve == "N/A") %>%
        dplyr::mutate(
            special_case = IRIS_OPTIC_NERVE_SPECIAL_CASE,
            interpretation = "Iris tumor: raw optic_nerve=N/A interpreted as non-abutment/not applicable; retained in full cohort only."
        )
}

#' Write runtime reconciliation artifacts for loader-side event/date repairs
#'
#' @param audit_rows Row-level reconciliation audit rows
#' @param audit_summary Per-variable summary table
#' @param source_workbook Source workbook basename
#' @param id_column Identifier column used for audit rows
#' @param output_dir Directory where the reviewable artifacts should be written
#' @param artifact_filename Optional stable workbook filename
#' @param manual_date_corrections Optional row-level manual correction audit
#'   table to include as a dedicated workbook sheet
#' @return Named list with workbook path
write_event_date_reconciliation_audit <- function(audit_rows,
                                                  audit_summary,
                                                  source_workbook,
                                                  id_column = NA_character_,
                                                  output_dir = NULL,
                                                  artifact_filename = NULL,
                                                  manual_date_corrections = NULL) {
    audit_dir <- output_dir %||% file.path(OUTPUT_DIR, "event_date_reconciliation_audit")
    dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

    if (is.null(artifact_filename) || !nzchar(artifact_filename)) {
        workbook_stub <- gsub("[^A-Za-z0-9]+", "_", tools::file_path_sans_ext(basename(source_workbook)))
        artifact_filename <- sprintf("event_date_reconciliation_%s.xlsx", workbook_stub)
    }

    xlsx_path <- file.path(audit_dir, artifact_filename)

    audit_rows_to_write <- audit_rows
    if (is.null(audit_rows_to_write) || nrow(audit_rows_to_write) == 0) {
        audit_rows_to_write <- empty_event_date_audit_rows()
    }

    audit_summary_to_write <- audit_summary
    if (is.null(audit_summary_to_write) || nrow(audit_summary_to_write) == 0) {
        audit_summary_to_write <- empty_event_date_audit_summary()
    }

    audit_metadata <- tibble::tibble(
        generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
        source_workbook = basename(source_workbook),
        id_column = ifelse(is.na(id_column), NA_character_, id_column),
        total_reconciled_rows = nrow(audit_rows_to_write)
    )

    manual_corrections_to_write <- manual_date_corrections
    if (is.null(manual_corrections_to_write) || nrow(manual_corrections_to_write) == 0) {
        manual_corrections_to_write <- empty_manual_date_correction_audit_rows()
    }

    write_readable_xlsx(
        list(
            Audit_Metadata = audit_metadata,
            Reconciliation_Summary = audit_summary_to_write,
            Reconciled_Changes = audit_rows_to_write,
            Manual_Date_Corrections = manual_corrections_to_write
        ),
        xlsx_path
    )

    logger::log_info(sprintf(
        "Event/date reconciliation audit written to %s",
        xlsx_path
    ))

    list(
        xlsx_path = xlsx_path
    )
}

#' Check and fix consistency between event indicators and dates
#'
#' Ensures logical consistency between binary event indicators and their associated dates.
#' If a date exists but the event is marked as 'N' or NA, updates event to 'Y'.
#' If an event is marked as 'Y' but has no date, sets the event to 'N' and records
#' a row-level reconciliation audit entry.
#'
#' @param data Data frame containing event and date variables
#' @param event_var Name of the event indicator variable (character)
#' @param date_var Name of the date variable (character)
#' @param event_yes Value indicating event occurred (default "Y")
#' @param event_no Value indicating event did not occur (default "N")
#' @param id_col Optional identifier column used in reconciliation audit rows
#' @param source_workbook Optional workbook name for audit artifacts
#'
#' @return Named list with reconciled `data`, row-level `audit_rows`, and
#'   per-variable `audit_summary`
#'
#' @examples
#' fix_event_date_consistency(data, "recurrence1", "recurrence1_date")
fix_event_date_consistency <- function(data,
                                       event_var,
                                       date_var,
                                       event_yes = "Y",
                                       event_no = "N",
                                       id_col = NA_character_,
                                       source_workbook = NA_character_) {
    event_var_name <- event_var
    date_var_name <- date_var
    logger::log_info(sprintf("Checking consistency between %s and %s", event_var, date_var))

    original_event <- data[[event_var]]
    original_date <- data[[date_var]]
    set_event_to_yes <- !is.na(original_date) & (is.na(original_event) | original_event != event_yes)
    set_event_to_no_missing_date <- original_event == event_yes & is.na(original_date)

    n_event_should_be_yes <- sum(set_event_to_yes, na.rm = TRUE)
    n_date_should_be_na <- sum(data[[event_var]] == event_yes & is.na(data[[date_var]]), na.rm = TRUE)

    reconciled_data <- data %>%
        mutate(
            !!event_var := case_when(
                !is.na(.data[[date_var]]) ~ event_yes,
                .data[[event_var]] == event_yes & is.na(.data[[date_var]]) ~ event_no,
                TRUE ~ event_no
            ),
            !!date_var := if_else(.data[[event_var]] == event_yes, .data[[date_var]], as.Date(NA))
        )

    id_values <- if (!is.na(id_col) && id_col %in% names(data)) {
        as.character(data[[id_col]])
    } else {
        rep(NA_character_, nrow(data))
    }

    reconciled_rows <- set_event_to_yes | set_event_to_no_missing_date
    audit_rows <- empty_event_date_audit_rows()
    if (any(reconciled_rows, na.rm = TRUE)) {
        audit_rows <- tibble::tibble(
            source_workbook = basename(source_workbook),
            id_column = ifelse(is.na(id_col), NA_character_, id_col),
            study_id = id_values,
            row_index = seq_len(nrow(data)),
            event_var = event_var_name,
            date_var = date_var_name,
            original_event = ifelse(is.na(original_event), NA_character_, as.character(original_event)),
            original_date = ifelse(is.na(original_date), NA_character_, as.character(original_date)),
            reconciled_event = ifelse(is.na(reconciled_data[[event_var_name]]), NA_character_, as.character(reconciled_data[[event_var_name]])),
            reconciled_date = ifelse(is.na(reconciled_data[[date_var_name]]), NA_character_, as.character(reconciled_data[[date_var_name]])),
            original_state = purrr::map2_chr(original_event, original_date, format_event_date_state),
            reconciled_state = purrr::map2_chr(reconciled_data[[event_var_name]], reconciled_data[[date_var_name]], format_event_date_state),
            action_taken = dplyr::case_when(
                set_event_to_yes ~ "set_event_to_yes_from_present_date",
                set_event_to_no_missing_date ~ "set_event_to_no_and_clear_missing_date",
                TRUE ~ NA_character_
            )
        ) %>%
            dplyr::filter(reconciled_rows)
    }

    audit_summary <- tibble::tibble(
        source_workbook = basename(source_workbook),
        id_column = ifelse(is.na(id_col), NA_character_, id_col),
        event_var = event_var_name,
        date_var = date_var_name,
        records_with_present_date = sum(!is.na(reconciled_data[[date_var_name]])),
        records_marked_event_yes_after = sum(reconciled_data[[event_var_name]] == event_yes, na.rm = TRUE),
        n_event_set_to_yes = n_event_should_be_yes,
        n_event_set_to_no_missing_date = sum(set_event_to_no_missing_date, na.rm = TRUE),
        n_rows_reconciled = nrow(audit_rows)
    )

    if (VERBOSE) {
        logger::log_info(sprintf("Found %d events with dates", sum(!is.na(reconciled_data[[date_var]]))))
        logger::log_info(sprintf("Found %d events marked as '%s'", sum(reconciled_data[[event_var]] == event_yes, na.rm = TRUE), event_yes))
        logger::log_info(sprintf(
            "Event/date consistency check for '%s' and '%s':", event_var, date_var
        ))
        logger::log_info(sprintf(
            "  - Number of records with a non-missing %s: %d", date_var, sum(!is.na(reconciled_data[[date_var]]))
        ))
        logger::log_info(sprintf(
            "  - Number of records with %s marked as '%s': %d", event_var, event_yes, sum(reconciled_data[[event_var]] == event_yes, na.rm = TRUE)
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

    return(list(
        data = reconciled_data,
        audit_rows = audit_rows,
        audit_summary = audit_summary
    ))
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

    iris_optic_nerve_special_cases <- collect_iris_optic_nerve_special_cases(raw_data)
    iris_optic_nerve_special_case_ids <- iris_optic_nerve_special_cases$id

    cleaned_data_pre_distinct <- raw_data %>%
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
            ),
            cohort_assignment_special_case = dplyr::case_when(
                id %in% iris_optic_nerve_special_case_ids ~ IRIS_OPTIC_NERVE_SPECIAL_CASE,
                TRUE ~ NA_character_
            ),
            cohort_assignment_note = dplyr::case_when(
                id %in% iris_optic_nerve_special_case_ids ~ "Iris tumor: raw optic_nerve=N/A interpreted as non-abutment/not applicable; retained in full cohort only.",
                TRUE ~ NA_character_
            ),
            optic_nerve = dplyr::case_when(
                id %in% iris_optic_nerve_special_case_ids ~ "N",
                TRUE ~ optic_nerve
            )
        ) %>%
        filter(!if_all(everything(), is.na))

    cleaned_data <- cleaned_data_pre_distinct %>%
        distinct() %>%
        mutate(
            consort_group = case_when(
                !is.na(initial_gk) | !is.na(initial_plaque) ~ case_when(
                    cohort_assignment_special_case == IRIS_OPTIC_NERVE_SPECIAL_CASE ~ CONSORT_GROUP_FULL_ONLY_SPECIAL_CASE,
                    initial_tumor_diameter <= TUMOR_DIAMETER_THRESHOLD &
                        initial_tumor_height <= TUMOR_HEIGHT_THRESHOLD &
                        optic_nerve == "N" ~ CONSORT_GROUP_ELIGIBLE_BOTH,
                    initial_tumor_diameter > TUMOR_DIAMETER_THRESHOLD |
                        initial_tumor_height > TUMOR_HEIGHT_THRESHOLD |
                        optic_nerve == "Y" ~ CONSORT_GROUP_GKSRS_ONLY,
                    TRUE ~ CONSORT_GROUP_UNCLASSIFIED_FIELDS
                ),
                TRUE ~ NA_character_
            )
        )

    raw_input_audit <- collect_raw_input_audit(
        raw_data = raw_data,
        cleaned_data = cleaned_data,
        source_workbook = filename
    )
    raw_input_audit$iris_optic_nerve_special_cases <- iris_optic_nerve_special_cases

    logger::log_info("eligible_both: initial_tumor_diameter <= 20mm, initial_tumor_height <= 10mm, optic_nerve == 'N'")
    logger::log_info("gksrs_only: initial_tumor_diameter > 20mm, initial_tumor_height > 10mm, optic_nerve == 'Y'")
    logger::log_info("full_cohort_only_special_case: audited records retained in the full cohort but excluded from restricted and GKSRS-only subcohorts")
    logger::log_info("unclassified_cohort_fields: treated rows with unresolved cohort-defining fields; removed or reported before analysis")
    message("\n")
    logger::log_info(sprintf("Found %d patients in full cohort", nrow(cleaned_data)))
    logger::log_info(sprintf("Found %d patients in restricted cohort", nrow(cleaned_data %>% filter(consort_group == "eligible_both"))))
    logger::log_info(sprintf("Found %d patients in GKSRS-only cohort", nrow(cleaned_data %>% filter(consort_group == "gksrs_only"))))
    special_case_count <- nrow(cleaned_data %>% filter(consort_group == CONSORT_GROUP_FULL_ONLY_SPECIAL_CASE))
    unclassified_count <- nrow(cleaned_data %>% filter(consort_group == CONSORT_GROUP_UNCLASSIFIED_FIELDS))
    logger::log_info(sprintf("Found %d full-cohort-only special-case patients", special_case_count))
    logger::log_info(sprintf("Found %d patients with unresolved cohort-defining fields", unclassified_count))
    if (unclassified_count > 0) {
        logger::log_warn("Patients have unresolved cohort-defining fields and will be removed or reported before analytic cohorts are finalized.")
    }
    message("\n")
    logger::log_info("NOTE: NOT splitting into cohorts yet!")
    message("\n")

    audit_id_col <- pick_event_date_audit_id_col(cleaned_data)
    if (is.na(audit_id_col)) {
        logger::log_warn("No preferred study ID column found for event/date reconciliation audit; row indices will be retained without study IDs.")
    }

    audit_rows <- list()
    audit_summaries <- list()
    event_date_pairs <- list(
        c("initial_gk", "initial_gk_date"),
        c("initial_plaque", "initial_plaque_date"),
        c("recurrence1", "recurrence1_date"),
        c("recurrence2", "recurrence2_date"),
        c("recurrence3", "recurrence3_date"),
        c("mets_progression", "mets_progression_date"),
        c("enucleation", "enucleation_date")
    )

    manual_date_correction_result <- apply_manual_date_corrections(
        cleaned_data,
        corrections = MANUAL_DATE_CORRECTIONS,
        id_col = audit_id_col,
        source_workbook = filename
    )
    cleaned_data <- manual_date_correction_result$data

    for (pair in event_date_pairs) {
        reconciliation_result <- fix_event_date_consistency(
            cleaned_data,
            event_var = pair[[1]],
            date_var = pair[[2]],
            id_col = audit_id_col,
            source_workbook = filename
        )
        cleaned_data <- reconciliation_result$data
        audit_rows[[length(audit_rows) + 1L]] <- reconciliation_result$audit_rows
        audit_summaries[[length(audit_summaries) + 1L]] <- reconciliation_result$audit_summary
    }

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

    attr(cleaned_data_final, "event_date_reconciliation_audit") <- list(
        audit_rows = dplyr::bind_rows(audit_rows),
        audit_summary = dplyr::bind_rows(audit_summaries),
        source_workbook = filename,
        id_column = audit_id_col,
        manual_date_corrections = manual_date_correction_result$audit_rows
    )
    attr(cleaned_data_final, "raw_input_audit") <- raw_input_audit
    logger::log_info("Loader-side event/date reconciliation details staged for Objective 0 publication into cohort 00_General folders.")

    logger::log_info(sprintf("Loaded %d rows of raw data", nrow(cleaned_data_final)))

    return(cleaned_data_final)
}
