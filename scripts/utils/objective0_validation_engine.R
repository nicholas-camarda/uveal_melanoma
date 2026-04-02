# Objective 0 structured validation engine

is_yes_value <- function(x) {
    normalized <- tolower(trimws(as.character(x)))
    normalized %in% c("y", "yes", "1", "true")
}

collapse_affected_ids <- function(ids, max_ids = 10L) {
    ids <- unique(as.character(ids[!is.na(ids)]))
    if (length(ids) == 0) {
        return(NA_character_)
    }

    if (length(ids) > max_ids) {
        paste(c(ids[seq_len(max_ids)], sprintf("... (+%d more)", length(ids) - max_ids)), collapse = ", ")
    } else {
        paste(ids, collapse = ", ")
    }
}

empty_input_audit <- function() {
    list(
        source_workbook = NA_character_,
        id_column = NA_character_,
        raw_row_count = 0L,
        cleaned_row_count = 0L,
        required_columns = character(),
        missing_required_columns = character(),
        duplicate_id_rows = tibble::tibble(),
        duplicate_row_count = 0L
    )
}

collect_raw_input_validation_findings <- function(input_audit = NULL) {
    findings <- empty_validation_findings()
    details <- empty_validation_detail_table()

    if (is.null(input_audit)) {
        findings <- dplyr::bind_rows(
            findings,
            new_validation_finding(
                check_id = "raw_input_audit_unavailable",
                finding_group = "raw_input",
                scope = "global",
                severity = "warning",
                status = "warn",
                metric = "input_audit",
                value = "missing",
                message = "Raw-input audit details were not available for this Objective 0 run."
            )
        )
        return(list(findings = findings, details = details))
    }

    missing_required <- input_audit$missing_required_columns %||% character()
    findings <- dplyr::bind_rows(
        findings,
        new_validation_finding(
            check_id = "required_input_columns",
            finding_group = "raw_input",
            scope = "global",
            severity = if (length(missing_required) > 0) "hard_error" else "info",
            status = if (length(missing_required) > 0) "fail" else "pass",
            metric = "missing_required_columns",
            value = paste(missing_required, collapse = ", "),
            message = if (length(missing_required) > 0) {
                sprintf("Missing required raw-input columns: %s", paste(missing_required, collapse = ", "))
            } else {
                "All required raw-input columns were present."
            },
            affected_n = length(missing_required)
        ),
        new_validation_finding(
            check_id = "raw_input_row_counts",
            finding_group = "raw_input",
            scope = "global",
            severity = "info",
            status = "info",
            metric = "raw_vs_cleaned_rows",
            value = sprintf("%s -> %s", input_audit$raw_row_count %||% NA_integer_, input_audit$cleaned_row_count %||% NA_integer_),
            message = sprintf(
                "Raw input rows: %s; cleaned rows after empty-row/duplicate-row handling: %s.",
                input_audit$raw_row_count %||% NA_integer_,
                input_audit$cleaned_row_count %||% NA_integer_
            )
        )
    )

    duplicate_id_rows <- input_audit$duplicate_id_rows %||% tibble::tibble()
    findings <- dplyr::bind_rows(
        findings,
        new_validation_finding(
            check_id = "raw_input_duplicate_ids",
            finding_group = "raw_input",
            scope = "global",
            severity = if (nrow(duplicate_id_rows) > 0) "hard_error" else "info",
            status = if (nrow(duplicate_id_rows) > 0) "fail" else "pass",
            metric = "duplicate_ids",
            value = collapse_affected_ids(duplicate_id_rows$study_id %||% character()),
            message = if (nrow(duplicate_id_rows) > 0) {
                sprintf("Raw input contains duplicate IDs in '%s'.", input_audit$id_column %||% "id")
            } else {
                "No duplicate IDs detected in the cleaned raw input."
            },
            affected_n = nrow(duplicate_id_rows),
            affected_ids = collapse_affected_ids(duplicate_id_rows$study_id %||% character())
        )
    )

    if (nrow(duplicate_id_rows) > 0) {
        details <- dplyr::bind_rows(
            details,
            new_validation_detail_table(
                detail_sheet = "Raw_Input_Duplicate_IDs",
                data = duplicate_id_rows,
                scope = "global",
                check_id = "raw_input_duplicate_ids"
            )
        )
    }

    list(findings = findings, details = details)
}

collect_reconciliation_validation_findings <- function(reconciliation_audit = NULL, warning_threshold = 5L) {
    findings <- empty_validation_findings()
    details <- empty_validation_detail_table()

    if (is.null(reconciliation_audit) || is.null(reconciliation_audit$audit_summary)) {
        return(list(findings = findings, details = details))
    }

    audit_summary <- tibble::as_tibble(reconciliation_audit$audit_summary)
    audit_rows <- tibble::as_tibble(reconciliation_audit$audit_rows %||% empty_event_date_audit_rows())

    summary_findings <- purrr::pmap_dfr(audit_summary, function(...) {
        row <- tibble::as_tibble(list(...))
        reconciled_n <- row$n_rows_reconciled[[1]] %||% 0L
        new_validation_finding(
            check_id = paste0("reconciliation_", row$event_var[[1]]),
            finding_group = "data_quality",
            scope = "global",
            severity = if (reconciled_n >= warning_threshold) "warning" else "info",
            status = if (reconciled_n >= warning_threshold) "warn" else "info",
            metric = "n_rows_reconciled",
            value = reconciled_n,
            message = sprintf(
                "Event/date reconciliation for %s vs %s changed %d row(s).",
                row$event_var[[1]],
                row$date_var[[1]],
                reconciled_n
            ),
            affected_n = reconciled_n
        )
    })

    findings <- dplyr::bind_rows(findings, summary_findings)

    if (nrow(audit_rows) > 0) {
        details <- dplyr::bind_rows(
            details,
            new_validation_detail_table(
                detail_sheet = "Event_Date_Reconciliations",
                data = audit_rows,
                scope = "global",
                check_id = "event_date_reconciliation_rows"
            )
        )
    }

    manual_date_corrections <- tibble::as_tibble(reconciliation_audit$manual_date_corrections %||% empty_manual_date_correction_audit_rows())
    findings <- dplyr::bind_rows(
        findings,
        new_validation_finding(
            check_id = "manual_date_corrections_applied",
            finding_group = "data_quality",
            scope = "global",
            severity = "info",
            status = "info",
            metric = "n_manual_date_corrections",
            value = nrow(manual_date_corrections),
            message = if (nrow(manual_date_corrections) > 0) {
                sprintf(
                    "Applied %d versioned manual raw-date correction(s); review the Manual_Date_Corrections sheet for the audit trail.",
                    nrow(manual_date_corrections)
                )
            } else {
                "No versioned manual raw-date corrections were applied during loading."
            },
            affected_n = nrow(manual_date_corrections),
            affected_ids = collapse_affected_ids(manual_date_corrections$study_id %||% character())
        )
    )

    if (nrow(manual_date_corrections) > 0) {
        details <- dplyr::bind_rows(
            details,
            new_validation_detail_table(
                detail_sheet = "Manual_Date_Corrections",
                data = manual_date_corrections,
                scope = "global",
                check_id = "manual_date_corrections_applied"
            )
        )
    }

    list(findings = findings, details = details)
}

collect_single_cohort_validation <- function(data, cohort_name) {
    findings <- empty_validation_findings()
    details <- empty_validation_detail_table()

    add_finding <- function(...) {
        findings <<- dplyr::bind_rows(findings, new_validation_finding(cohort = cohort_name, scope = "cohort", ...))
    }

    add_detail <- function(detail_sheet, data) {
        details <<- dplyr::bind_rows(
            details,
            new_validation_detail_table(
                detail_sheet = detail_sheet,
                data = data,
                scope = "cohort",
                cohort = cohort_name,
                check_id = detail_sheet
            )
        )
    }

    add_finding(
        check_id = "cohort_row_count",
        finding_group = "structure",
        severity = if (nrow(data) == 0) "hard_error" else "info",
        status = if (nrow(data) == 0) "fail" else "pass",
        metric = "n_rows",
        value = nrow(data),
        message = if (nrow(data) == 0) "Dataset is empty." else sprintf("Dataset contains %d rows.", nrow(data)),
        affected_n = nrow(data)
    )

    add_finding(
        check_id = "minimum_columns_after_processing",
        finding_group = "structure",
        severity = if (ncol(data) < MINIMUM_COLUMNS_AFTER_PROCESSING) "warning" else "info",
        status = if (ncol(data) < MINIMUM_COLUMNS_AFTER_PROCESSING) "warn" else "pass",
        metric = "n_columns",
        value = ncol(data),
        message = if (ncol(data) < MINIMUM_COLUMNS_AFTER_PROCESSING) {
            sprintf("Dataset has only %d columns; expected at least %d after processing.", ncol(data), MINIMUM_COLUMNS_AFTER_PROCESSING)
        } else {
            sprintf("Dataset column count (%d) meets the expected minimum.", ncol(data))
        }
    )

    if ("id" %in% names(data)) {
        duplicate_ids <- data %>%
            dplyr::filter(!is.na(.data$id)) %>%
            dplyr::count(.data$id, name = "n_records") %>%
            dplyr::filter(.data$n_records > 1)
        add_finding(
            check_id = "duplicate_patient_ids",
            finding_group = "structure",
            severity = if (nrow(duplicate_ids) > 0) "hard_error" else "info",
            status = if (nrow(duplicate_ids) > 0) "fail" else "pass",
            metric = "duplicate_ids",
            value = nrow(duplicate_ids),
            message = if (nrow(duplicate_ids) > 0) {
                "Duplicate patient IDs detected in processed cohort data."
            } else {
                "No duplicate patient IDs detected in processed cohort data."
            },
            affected_n = nrow(duplicate_ids),
            affected_ids = collapse_affected_ids(duplicate_ids$id)
        )
        if (nrow(duplicate_ids) > 0) {
            add_detail("Duplicate_Patient_IDs", duplicate_ids)
        }
    }

    duplicate_rows <- data[duplicated(data), , drop = FALSE]
    add_finding(
        check_id = "duplicate_analytic_rows",
        finding_group = "structure",
        severity = if (nrow(duplicate_rows) > 0) "warning" else "info",
        status = if (nrow(duplicate_rows) > 0) "warn" else "pass",
        metric = "duplicate_rows",
        value = nrow(duplicate_rows),
        message = if (nrow(duplicate_rows) > 0) {
            sprintf("Detected %d fully duplicated analytic row(s).", nrow(duplicate_rows))
        } else {
            "No fully duplicated analytic rows detected."
        },
        affected_n = nrow(duplicate_rows)
    )
    if (nrow(duplicate_rows) > 0) {
        add_detail("Duplicate_Analytic_Rows", duplicate_rows)
    }

    missing_critical <- setdiff(CRITICAL_VARIABLES, names(data))
    add_finding(
        check_id = "critical_variables_present",
        finding_group = "critical_variables",
        severity = if (length(missing_critical) > 0) "hard_error" else "info",
        status = if (length(missing_critical) > 0) "fail" else "pass",
        metric = "missing_critical_variables",
        value = paste(missing_critical, collapse = ", "),
        message = if (length(missing_critical) > 0) {
            sprintf("Missing critical variables: %s", paste(missing_critical, collapse = ", "))
        } else {
            "All critical variables are present."
        },
        affected_n = length(missing_critical)
    )

    missing_derived <- setdiff(DERIVED_VARIABLES, names(data))
    add_finding(
        check_id = "derived_variables_present",
        finding_group = "critical_variables",
        severity = if (length(missing_derived) > 0) "hard_error" else "info",
        status = if (length(missing_derived) > 0) "fail" else "pass",
        metric = "missing_derived_variables",
        value = paste(missing_derived, collapse = ", "),
        message = if (length(missing_derived) > 0) {
            sprintf("Missing derived variables: %s", paste(missing_derived, collapse = ", "))
        } else {
            "All required derived variables are present."
        },
        affected_n = length(missing_derived)
    )

    date_cols <- names(data)[
        grepl("date|dob|dod|last_known_alive", names(data), ignore.case = TRUE) &
            !grepl("_source$", names(data), ignore.case = TRUE)
    ]
    non_date_cols <- date_cols[!vapply(data[date_cols], function(col) inherits(col, c("Date", "POSIXct", "POSIXt")), logical(1))]
    add_finding(
        check_id = "date_columns_are_date_like",
        finding_group = "date_checks",
        severity = if (length(non_date_cols) > 0) "hard_error" else "info",
        status = if (length(non_date_cols) > 0) "fail" else "pass",
        metric = "non_date_columns",
        value = paste(non_date_cols, collapse = ", "),
        message = if (length(non_date_cols) > 0) {
            sprintf("Date-like columns are not stored as Date/POSIX types: %s", paste(non_date_cols, collapse = ", "))
        } else {
            "All date-like columns use Date/POSIX storage."
        },
        affected_n = length(non_date_cols)
    )

    if ("treatment_date" %in% names(data) && "last_known_alive_date" %in% names(data)) {
        negative_followup_rows <- data %>%
            dplyr::filter(!is.na(.data$treatment_date), !is.na(.data$last_known_alive_date)) %>%
            dplyr::mutate(followup_days = as.numeric(difftime(.data$last_known_alive_date, .data$treatment_date, units = "days"))) %>%
            dplyr::filter(.data$followup_days < 0)

        add_finding(
            check_id = "negative_followup_intervals",
            finding_group = "date_checks",
            severity = if (nrow(negative_followup_rows) > 0) "hard_error" else "info",
            status = if (nrow(negative_followup_rows) > 0) "fail" else "pass",
            metric = "negative_followup_rows",
            value = nrow(negative_followup_rows),
            message = if (nrow(negative_followup_rows) > 0) {
                "Detected negative treatment-to-last-known-alive intervals."
            } else {
                "No negative treatment-to-last-known-alive intervals detected."
            },
            affected_n = nrow(negative_followup_rows),
            affected_ids = collapse_affected_ids(negative_followup_rows$id %||% character())
        )

        if (nrow(negative_followup_rows) > 0) {
            add_detail("Negative_Followup_Intervals", negative_followup_rows)
        }
    }

    if (all(c("dob", "date_diagnosis") %in% names(data))) {
        invalid_dob_rows <- data %>%
            dplyr::filter(!is.na(.data$dob), !is.na(.data$date_diagnosis), .data$dob > .data$date_diagnosis)

        add_finding(
            check_id = "dob_before_diagnosis",
            finding_group = "date_checks",
            severity = if (nrow(invalid_dob_rows) > 0) "hard_error" else "info",
            status = if (nrow(invalid_dob_rows) > 0) "fail" else "pass",
            metric = "dob_after_diagnosis_rows",
            value = nrow(invalid_dob_rows),
            message = if (nrow(invalid_dob_rows) > 0) {
                "DOB occurs after diagnosis date for one or more rows."
            } else {
                "All DOB values occur on or before diagnosis date."
            },
            affected_n = nrow(invalid_dob_rows),
            affected_ids = collapse_affected_ids(invalid_dob_rows$id %||% character())
        )

        if (nrow(invalid_dob_rows) > 0) {
            add_detail("DOB_After_Diagnosis", invalid_dob_rows)
        }
    }

    if (all(c("date_diagnosis", "treatment_date") %in% names(data))) {
        treatment_before_diagnosis_rows <- data %>%
            dplyr::filter(!is.na(.data$date_diagnosis), !is.na(.data$treatment_date), .data$treatment_date < .data$date_diagnosis) %>%
            dplyr::mutate(diagnosis_treatment_gap_days = as.numeric(.data$treatment_date - .data$date_diagnosis))

        minor_gap_rows <- treatment_before_diagnosis_rows %>%
            dplyr::filter(.data$diagnosis_treatment_gap_days >= -MINOR_TREATMENT_DIAGNOSIS_GAP_DAYS)

        major_gap_rows <- treatment_before_diagnosis_rows %>%
            dplyr::filter(.data$diagnosis_treatment_gap_days < -MINOR_TREATMENT_DIAGNOSIS_GAP_DAYS)

        add_finding(
            check_id = "treatment_after_diagnosis",
            finding_group = "date_checks",
            severity = if (nrow(major_gap_rows) > 0) "hard_error" else "info",
            status = if (nrow(major_gap_rows) > 0) "fail" else "pass",
            metric = "treatment_before_diagnosis_rows",
            value = nrow(major_gap_rows),
            message = if (nrow(major_gap_rows) > 0) {
                sprintf(
                    "Treatment date occurs more than %d day(s) before diagnosis date for one or more rows.",
                    MINOR_TREATMENT_DIAGNOSIS_GAP_DAYS
                )
            } else {
                sprintf(
                    "No treatment dates occur more than %d day(s) before diagnosis date.",
                    MINOR_TREATMENT_DIAGNOSIS_GAP_DAYS
                )
            },
            affected_n = nrow(major_gap_rows),
            affected_ids = collapse_affected_ids(major_gap_rows$id %||% character())
        )

        if (nrow(major_gap_rows) > 0) {
            add_detail("Treatment_Before_Diagnosis", major_gap_rows)
        }

        add_finding(
            check_id = "treatment_before_diagnosis_minor_gap",
            finding_group = "date_checks",
            severity = if (nrow(minor_gap_rows) > 0) "warning" else "info",
            status = if (nrow(minor_gap_rows) > 0) "warn" else "pass",
            metric = "minor_reverse_gap_rows",
            value = nrow(minor_gap_rows),
            message = if (nrow(minor_gap_rows) > 0) {
                sprintf(
                    "Treatment date occurs 1-%d day(s) before diagnosis date for one or more rows; these rows are published for manual review but do not block the pipeline.",
                    MINOR_TREATMENT_DIAGNOSIS_GAP_DAYS
                )
            } else {
                sprintf(
                    "No treatment dates occur within 1-%d day(s) before diagnosis date.",
                    MINOR_TREATMENT_DIAGNOSIS_GAP_DAYS
                )
            },
            affected_n = nrow(minor_gap_rows),
            affected_ids = collapse_affected_ids(minor_gap_rows$id %||% character())
        )

        if (nrow(minor_gap_rows) > 0) {
            add_detail("Treatment_Before_Diagnosis_Minor_Gap", minor_gap_rows)
        }
    }

    if ("age_at_diagnosis" %in% names(data)) {
        negative_age_rows <- data %>%
            dplyr::filter(!is.na(.data$age_at_diagnosis), .data$age_at_diagnosis < 0)
        implausible_age_rows <- data %>%
            dplyr::filter(!is.na(.data$age_at_diagnosis), .data$age_at_diagnosis > 120)

        add_finding(
            check_id = "non_negative_ages",
            finding_group = "derived_ranges",
            severity = if (nrow(negative_age_rows) > 0) "hard_error" else "info",
            status = if (nrow(negative_age_rows) > 0) "fail" else "pass",
            metric = "negative_ages",
            value = nrow(negative_age_rows),
            message = if (nrow(negative_age_rows) > 0) "Negative ages detected." else "No negative ages detected.",
            affected_n = nrow(negative_age_rows),
            affected_ids = collapse_affected_ids(negative_age_rows$id %||% character())
        )

        add_finding(
            check_id = "implausible_ages",
            finding_group = "derived_ranges",
            severity = if (nrow(implausible_age_rows) > 0) "warning" else "info",
            status = if (nrow(implausible_age_rows) > 0) "warn" else "pass",
            metric = "ages_gt_120",
            value = nrow(implausible_age_rows),
            message = if (nrow(implausible_age_rows) > 0) "Implausibly high ages (>120 years) detected." else "No implausibly high ages detected.",
            affected_n = nrow(implausible_age_rows),
            affected_ids = collapse_affected_ids(implausible_age_rows$id %||% character())
        )
    }

    for (measure_name in c("initial_tumor_height", "initial_tumor_diameter")) {
        if (!measure_name %in% names(data)) {
            next
        }

        invalid_rows <- data %>%
            dplyr::filter(!is.na(.data[[measure_name]]), .data[[measure_name]] < 0)
        extreme_threshold <- if (identical(measure_name, "initial_tumor_height")) 25 else 40
        extreme_rows <- data %>%
            dplyr::filter(!is.na(.data[[measure_name]]), .data[[measure_name]] > extreme_threshold)

        add_finding(
            check_id = paste0(measure_name, "_non_negative"),
            finding_group = "derived_ranges",
            severity = if (nrow(invalid_rows) > 0) "hard_error" else "info",
            status = if (nrow(invalid_rows) > 0) "fail" else "pass",
            metric = measure_name,
            value = nrow(invalid_rows),
            message = if (nrow(invalid_rows) > 0) {
                sprintf("%s contains negative values.", measure_name)
            } else {
                sprintf("%s has no negative values.", measure_name)
            },
            affected_n = nrow(invalid_rows),
            affected_ids = collapse_affected_ids(invalid_rows$id %||% character())
        )
        add_finding(
            check_id = paste0(measure_name, "_extreme_values"),
            finding_group = "derived_ranges",
            severity = if (nrow(extreme_rows) > 0) "warning" else "info",
            status = if (nrow(extreme_rows) > 0) "warn" else "pass",
            metric = measure_name,
            value = nrow(extreme_rows),
            message = if (nrow(extreme_rows) > 0) {
                sprintf("%s exceeds the review threshold of %s for one or more rows.", measure_name, extreme_threshold)
            } else {
                sprintf("%s is within the review threshold of %s.", measure_name, extreme_threshold)
            },
            affected_n = nrow(extreme_rows),
            affected_ids = collapse_affected_ids(extreme_rows$id %||% character())
        )
    }

    for (factor_name in CRITICAL_FACTORS) {
        if (!factor_name %in% names(data)) {
            next
        }

        add_finding(
            check_id = paste0(factor_name, "_is_factor"),
            finding_group = "factor_levels",
            severity = if (!is.factor(data[[factor_name]])) "hard_error" else "info",
            status = if (!is.factor(data[[factor_name]])) "fail" else "pass",
            metric = factor_name,
            value = paste(class(data[[factor_name]]), collapse = ", "),
            message = if (!is.factor(data[[factor_name]])) {
                sprintf("%s is not stored as a factor.", factor_name)
            } else {
                sprintf("%s is stored as a factor.", factor_name)
            }
        )
    }

    expected_factors <- get_canonical_factor_level_expectations()
    for (factor_name in names(expected_factors)) {
        if (!factor_name %in% names(data) || !is.factor(data[[factor_name]])) {
            next
        }

        expected_levels <- expected_factors[[factor_name]]$levels
        actual_levels <- levels(data[[factor_name]])
        reference_level <- expected_factors[[factor_name]]$reference
        unexpected_levels <- setdiff(actual_levels, expected_levels)

        add_finding(
            check_id = paste0(factor_name, "_allowed_levels"),
            finding_group = "factor_levels",
            severity = if (length(unexpected_levels) > 0) "hard_error" else "info",
            status = if (length(unexpected_levels) > 0) "fail" else "pass",
            metric = factor_name,
            value = paste(actual_levels, collapse = ", "),
            message = if (length(unexpected_levels) > 0) {
                sprintf("Unexpected factor levels for %s: %s", factor_name, paste(unexpected_levels, collapse = ", "))
            } else {
                sprintf("%s levels stay within the canonical set.", factor_name)
            },
            affected_n = length(unexpected_levels)
        )

        add_finding(
            check_id = paste0(factor_name, "_reference_level"),
            finding_group = "factor_levels",
            severity = if (length(actual_levels) > 0 && actual_levels[[1]] != reference_level) "hard_error" else "info",
            status = if (length(actual_levels) > 0 && actual_levels[[1]] != reference_level) "fail" else "pass",
            metric = factor_name,
            value = if (length(actual_levels) > 0) actual_levels[[1]] else NA_character_,
            message = if (length(actual_levels) > 0 && actual_levels[[1]] != reference_level) {
                sprintf("Reference level for %s is '%s'; expected '%s'.", factor_name, actual_levels[[1]], reference_level)
            } else {
                sprintf("%s reference level matches '%s'.", factor_name, reference_level)
            }
        )
    }

    if ("treatment_group" %in% names(data)) {
        treatment_values <- unique(as.character(stats::na.omit(data$treatment_group)))
        add_finding(
            check_id = "treatment_group_structure",
            finding_group = "cohort_rules",
            severity = if (length(treatment_values) == 0) "hard_error" else if (length(treatment_values) == 1) "warning" else "info",
            status = if (length(treatment_values) == 0) "fail" else if (length(treatment_values) == 1) "warn" else "pass",
            metric = "treatment_groups_present",
            value = paste(treatment_values, collapse = ", "),
            message = if (length(treatment_values) == 0) {
                "No treatment groups are present in this cohort."
            } else if (length(treatment_values) == 1) {
                sprintf("Only one treatment group is present: %s", paste(treatment_values, collapse = ", "))
            } else {
                "Both treatment groups are represented in this cohort."
            },
            affected_n = length(treatment_values)
        )
    }

    for (var_name in MISSING_DATA_CHECK_VARIABLES) {
        if (!var_name %in% names(data)) {
            next
        }

        missing_pct <- round(mean(is.na(data[[var_name]])) * 100, 1)
        add_finding(
            check_id = paste0(var_name, "_missingness"),
            finding_group = "data_quality",
            severity = if (missing_pct > MAXIMUM_MISSING_DATA_PERCENTAGE) "warning" else "info",
            status = if (missing_pct > MAXIMUM_MISSING_DATA_PERCENTAGE) "warn" else "pass",
            metric = paste0(var_name, "_missing_pct"),
            value = missing_pct,
            message = if (missing_pct > MAXIMUM_MISSING_DATA_PERCENTAGE) {
                sprintf("%s has %.1f%% missingness, exceeding the %.1f%% review threshold.", var_name, missing_pct, MAXIMUM_MISSING_DATA_PERCENTAGE)
            } else {
                sprintf("%s missingness is %.1f%%.", var_name, missing_pct)
            }
        )
    }

    if (all(c("recurrence1", "tt_recurrence_months") %in% names(data))) {
        inconsistent_recurrence <- data %>%
            dplyr::filter(is_yes_value(.data$recurrence1), !is.na(.data$tt_recurrence_months), .data$tt_recurrence_months <= 0)
        add_finding(
            check_id = "recurrence_timing_consistency",
            finding_group = "data_quality",
            severity = if (nrow(inconsistent_recurrence) > 0) "warning" else "info",
            status = if (nrow(inconsistent_recurrence) > 0) "warn" else "pass",
            metric = "non_positive_recurrence_times",
            value = nrow(inconsistent_recurrence),
            message = if (nrow(inconsistent_recurrence) > 0) {
                "Rows with recurrence flagged but non-positive recurrence time were detected."
            } else {
                "Recurrence timing checks passed."
            },
            affected_n = nrow(inconsistent_recurrence),
            affected_ids = collapse_affected_ids(inconsistent_recurrence$id %||% character())
        )
    }

    if (all(c("mets_progression", "tt_mets_months") %in% names(data))) {
        inconsistent_mets <- data %>%
            dplyr::filter(is_yes_value(.data$mets_progression), !is.na(.data$tt_mets_months), .data$tt_mets_months <= 0)
        add_finding(
            check_id = "metastasis_timing_consistency",
            finding_group = "data_quality",
            severity = if (nrow(inconsistent_mets) > 0) "warning" else "info",
            status = if (nrow(inconsistent_mets) > 0) "warn" else "pass",
            metric = "non_positive_metastasis_times",
            value = nrow(inconsistent_mets),
            message = if (nrow(inconsistent_mets) > 0) {
                "Rows with metastasis flagged but non-positive metastasis time were detected."
            } else {
                "Metastasis timing checks passed."
            },
            affected_n = nrow(inconsistent_mets),
            affected_ids = collapse_affected_ids(inconsistent_mets$id %||% character())
        )
    }

    if (all(c("death_event", "tt_death_months") %in% names(data))) {
        inconsistent_death <- data %>%
            dplyr::filter(.data$death_event == 1, !is.na(.data$tt_death_months), .data$tt_death_months <= 0)
        add_finding(
            check_id = "death_timing_consistency",
            finding_group = "data_quality",
            severity = if (nrow(inconsistent_death) > 0) "warning" else "info",
            status = if (nrow(inconsistent_death) > 0) "warn" else "pass",
            metric = "non_positive_death_times",
            value = nrow(inconsistent_death),
            message = if (nrow(inconsistent_death) > 0) {
                "Rows with death flagged but non-positive death time were detected."
            } else {
                "Death timing checks passed."
            },
            affected_n = nrow(inconsistent_death),
            affected_ids = collapse_affected_ids(inconsistent_death$id %||% character())
        )
    }

    if (identical(cohort_name, "uveal_melanoma_restricted_cohort")) {
        restricted_violations <- data %>%
            dplyr::filter(
                .data$initial_tumor_diameter > TUMOR_DIAMETER_THRESHOLD |
                    .data$initial_tumor_height > TUMOR_HEIGHT_THRESHOLD |
                    .data$optic_nerve %in% c("Y", "Yes")
            )
        add_finding(
            check_id = "restricted_cohort_eligibility",
            finding_group = "cohort_rules",
            severity = if (nrow(restricted_violations) > 0) "hard_error" else "info",
            status = if (nrow(restricted_violations) > 0) "fail" else "pass",
            metric = "restricted_violations",
            value = nrow(restricted_violations),
            message = if (nrow(restricted_violations) > 0) {
                "Restricted cohort contains eligibility violations."
            } else {
                "Restricted cohort eligibility rules are satisfied."
            },
            affected_n = nrow(restricted_violations),
            affected_ids = collapse_affected_ids(restricted_violations$id %||% character())
        )
        if (nrow(restricted_violations) > 0) {
            add_detail("Restricted_Cohort_Violations", restricted_violations)
        }
    }

    if (identical(cohort_name, "uveal_melanoma_gksrs_only_cohort")) {
        gksrs_violations <- data %>%
            dplyr::filter(
                !(
                    .data$initial_tumor_diameter > TUMOR_DIAMETER_THRESHOLD |
                        .data$initial_tumor_height > TUMOR_HEIGHT_THRESHOLD |
                        .data$optic_nerve %in% c("Y", "Yes")
                )
            )
        add_finding(
            check_id = "gksrs_only_cohort_eligibility",
            finding_group = "cohort_rules",
            severity = if (nrow(gksrs_violations) > 0) "hard_error" else "info",
            status = if (nrow(gksrs_violations) > 0) "fail" else "pass",
            metric = "gksrs_only_violations",
            value = nrow(gksrs_violations),
            message = if (nrow(gksrs_violations) > 0) {
                "GKSRS-only cohort contains rows that do not violate PBT eligibility."
            } else {
                "GKSRS-only cohort eligibility rules are satisfied."
            },
            affected_n = nrow(gksrs_violations),
            affected_ids = collapse_affected_ids(gksrs_violations$id %||% character())
        )
        if (nrow(gksrs_violations) > 0) {
            add_detail("GKSRS_Only_Cohort_Violations", gksrs_violations)
        }
    }

    if ("initial_stage_binary" %in% names(data)) {
        stage_iv_rows <- data %>%
            dplyr::filter(.data$initial_stage_binary == "Stage IV")
        add_finding(
            check_id = "stage_iv_excluded_from_cohorts",
            finding_group = "cohort_rules",
            severity = if (nrow(stage_iv_rows) > 0) "hard_error" else "info",
            status = if (nrow(stage_iv_rows) > 0) "fail" else "pass",
            metric = "stage_iv_rows",
            value = nrow(stage_iv_rows),
            message = if (nrow(stage_iv_rows) > 0) {
                "Stage IV rows remain in an analytic cohort after exclusion processing."
            } else {
                "No Stage IV rows remain in the analytic cohort."
            },
            affected_n = nrow(stage_iv_rows),
            affected_ids = collapse_affected_ids(stage_iv_rows$id %||% character())
        )
    }

    build_validation_result(
        findings = findings,
        detail_tables = details,
        validated_cohorts = if (any(findings$severity == "hard_error" & findings$status == "fail", na.rm = TRUE)) character() else cohort_name
    )
}

collect_cross_cohort_validation <- function(cohort_list, removal_log = NULL) {
    findings <- empty_validation_findings()
    details <- empty_validation_detail_table()
    validated <- character()

    expected_names <- get_expected_analytic_cohort_names()
    missing_names <- setdiff(expected_names, names(cohort_list))
    findings <- dplyr::bind_rows(
        findings,
        new_validation_finding(
            check_id = "expected_cohorts_present",
            finding_group = "cross_cohort",
            scope = "cross_cohort",
            severity = if (length(missing_names) > 0) "hard_error" else "info",
            status = if (length(missing_names) > 0) "fail" else "pass",
            metric = "missing_cohorts",
            value = paste(missing_names, collapse = ", "),
            message = if (length(missing_names) > 0) {
                sprintf("Missing expected cohorts: %s", paste(missing_names, collapse = ", "))
            } else {
                "All expected analytic cohorts are present."
            },
            affected_n = length(missing_names)
        )
    )

    if (length(missing_names) == 0) {
        n_full <- nrow(cohort_list$uveal_melanoma_full_cohort)
        n_restricted <- nrow(cohort_list$uveal_melanoma_restricted_cohort)
        n_gksrs <- nrow(cohort_list$uveal_melanoma_gksrs_only_cohort)

        findings <- dplyr::bind_rows(
            findings,
            new_validation_finding(
                check_id = "full_cohort_is_largest",
                finding_group = "cross_cohort",
                scope = "cross_cohort",
                severity = if (n_full < n_restricted || n_full < n_gksrs) "hard_error" else "info",
                status = if (n_full < n_restricted || n_full < n_gksrs) "fail" else "pass",
                metric = "cohort_sizes",
                value = sprintf("full=%d, restricted=%d, gksrs_only=%d", n_full, n_restricted, n_gksrs),
                message = if (n_full < n_restricted || n_full < n_gksrs) {
                    "Full cohort is not the largest cohort."
                } else {
                    "Full cohort is the largest cohort as expected."
                }
            ),
            new_validation_finding(
                check_id = "subset_size_relationships",
                finding_group = "cross_cohort",
                scope = "cross_cohort",
                severity = if (abs(n_full - (n_restricted + n_gksrs)) > 10) "warning" else "info",
                status = if (abs(n_full - (n_restricted + n_gksrs)) > 10) "warn" else "pass",
                metric = "full_minus_subsets",
                value = n_full - (n_restricted + n_gksrs),
                message = if (abs(n_full - (n_restricted + n_gksrs)) > 10) {
                    "Full cohort size differs from restricted + GKSRS-only cohorts by more than 10 patients."
                } else {
                    "Full cohort size relationship to restricted + GKSRS-only cohorts is within the expected tolerance."
                }
            )
        )

        overlap_ids <- intersect(
            cohort_list$uveal_melanoma_restricted_cohort$id %||% integer(),
            cohort_list$uveal_melanoma_gksrs_only_cohort$id %||% integer()
        )
        findings <- dplyr::bind_rows(
            findings,
            new_validation_finding(
                check_id = "restricted_gksrs_overlap",
                finding_group = "cross_cohort",
                scope = "cross_cohort",
                severity = if (length(overlap_ids) > 0) "hard_error" else "info",
                status = if (length(overlap_ids) > 0) "fail" else "pass",
                metric = "overlap_ids",
                value = collapse_affected_ids(overlap_ids),
                message = if (length(overlap_ids) > 0) {
                    "Restricted and GKSRS-only cohorts overlap."
                } else {
                    "Restricted and GKSRS-only cohorts are disjoint."
                },
                affected_n = length(overlap_ids),
                affected_ids = collapse_affected_ids(overlap_ids)
            )
        )

        if (length(overlap_ids) > 0) {
            details <- dplyr::bind_rows(
                details,
                new_validation_detail_table(
                    detail_sheet = "Restricted_GKSRS_Overlap",
                    data = tibble::tibble(id = overlap_ids),
                    scope = "cross_cohort",
                    check_id = "restricted_gksrs_overlap"
                )
            )
        }
    }

    if (!is.null(removal_log) && nrow(removal_log) > 0) {
        stage_iv_n <- sum(removal_log$removal_step == "stage_iv_exclusion", na.rm = TRUE)
        manual_n <- sum(removal_log$removal_step == "manual_exclusion", na.rm = TRUE)
        findings <- dplyr::bind_rows(
            findings,
            new_validation_finding(
                check_id = "exclusion_accounting",
                finding_group = "cross_cohort",
                scope = "cross_cohort",
                severity = "info",
                status = "info",
                metric = "stage_iv_manual_exclusions",
                value = sprintf("stage_iv=%d, manual=%d", stage_iv_n, manual_n),
                message = sprintf(
                    "Removal log contains %d Stage IV exclusions and %d manual exclusions.",
                    stage_iv_n,
                    manual_n
                ),
                affected_n = nrow(removal_log)
            )
        )
    }

    expected_factors <- get_canonical_factor_level_expectations()
    for (factor_name in names(expected_factors)) {
        critical <- isTRUE(expected_factors[[factor_name]]$critical)
        cohort_levels <- purrr::map(cohort_list, function(df) {
            if (factor_name %in% names(df) && is.factor(df[[factor_name]])) {
                levels(df[[factor_name]])
            } else {
                NULL
            }
        })
        cohort_levels <- cohort_levels[!vapply(cohort_levels, is.null, logical(1))]
        cohort_levels <- cohort_levels[vapply(cohort_levels, length, integer(1)) >= 2]
        if (length(cohort_levels) <= 1) {
            next
        }

        level_signatures <- vapply(cohort_levels, paste, collapse = " | ", character(1))
        level_match <- length(unique(level_signatures)) == 1
        findings <- dplyr::bind_rows(
            findings,
            new_validation_finding(
                check_id = paste0("cross_cohort_levels_", factor_name),
                finding_group = "cross_cohort",
                scope = "cross_cohort",
                severity = if (!level_match && critical) "hard_error" else if (!level_match) "warning" else "info",
                status = if (!level_match && critical) "fail" else if (!level_match) "warn" else "pass",
                metric = factor_name,
                value = paste(level_signatures, collapse = " || "),
                message = if (!level_match) {
                    sprintf("Cross-cohort factor levels differ for %s.", factor_name)
                } else {
                    sprintf("Cross-cohort factor levels match for %s.", factor_name)
                }
            )
        )
    }

    if (file.exists(file.path(PROCESSED_DATA_DIR, "cohort_summary_statistics.json"))) {
        summary_json <- jsonlite::read_json(file.path(PROCESSED_DATA_DIR, "cohort_summary_statistics.json"), simplifyVector = TRUE)
        if (length(missing_names) == 0 && !is.null(summary_json$cohorts)) {
            summary_matches <- identical(
                summary_json$cohorts$full_cohort$total,
                nrow(cohort_list$uveal_melanoma_full_cohort)
            ) &&
                identical(
                    summary_json$cohorts$restricted_cohort$total,
                    nrow(cohort_list$uveal_melanoma_restricted_cohort)
                ) &&
                identical(
                    summary_json$cohorts$gksrs_only_cohort$total,
                    nrow(cohort_list$uveal_melanoma_gksrs_only_cohort)
                )

            findings <- dplyr::bind_rows(
                findings,
                new_validation_finding(
                    check_id = "cohort_summary_json_matches_runtime",
                    finding_group = "cross_cohort",
                    scope = "cross_cohort",
                    severity = if (!summary_matches) "hard_error" else "info",
                    status = if (!summary_matches) "fail" else "pass",
                    metric = "cohort_summary_statistics.json",
                    value = file.path(PROCESSED_DATA_DIR, "cohort_summary_statistics.json"),
                    message = if (!summary_matches) {
                        "cohort_summary_statistics.json does not match current cohort object sizes."
                    } else {
                        "cohort_summary_statistics.json matches current cohort object sizes."
                    }
                )
            )
        }
    }

    build_validation_result(
        findings = findings,
        detail_tables = details,
        validated_cohorts = validated
    )
}

validate_single_cohort_comprehensive <- function(data, cohort_name) {
    collect_single_cohort_validation(data, cohort_name)$success
}

validate_cohort_integrity <- function(cohort_list) {
    collect_cross_cohort_validation(cohort_list)$success
}

validate_factor_level_consistency <- function(cohort_list, phase = "data_processing") {
    collect_cross_cohort_validation(cohort_list)$success
}

validate_cross_cohort_consistency <- function(cohort_list, removal_log = NULL) {
    collect_cross_cohort_validation(cohort_list, removal_log = removal_log)$success
}

validate_processed_files_exist <- function(cohort_list) {
    validation_passed <- TRUE
    for (cohort_name in names(cohort_list)) {
        rds_file <- file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".rds"))
        excel_file <- file.path(PROCESSED_DATA_DIR, paste0(cohort_name, ".xlsx"))
        if (!file.exists(rds_file) || !file.exists(excel_file)) {
            validation_passed <- FALSE
        }
    }
    validation_passed
}

validate_processing_pipeline <- function(data,
                                         stop_on_failure = TRUE,
                                         input_audit = NULL,
                                         removal_log = NULL,
                                         reconciliation_audit = NULL) {
    logger::log_info("=== COMPREHENSIVE DATA PROCESSING VALIDATION ===")

    findings <- empty_validation_findings()
    details <- empty_validation_detail_table()
    validated_cohorts <- character()

    raw_input_result <- collect_raw_input_validation_findings(input_audit)
    findings <- dplyr::bind_rows(findings, raw_input_result$findings)
    details <- dplyr::bind_rows(details, raw_input_result$details)

    reconciliation_result <- collect_reconciliation_validation_findings(reconciliation_audit)
    findings <- dplyr::bind_rows(findings, reconciliation_result$findings)
    details <- dplyr::bind_rows(details, reconciliation_result$details)

    if (is.list(data) && !is.data.frame(data)) {
        for (cohort_name in names(data)) {
            cohort_result <- collect_single_cohort_validation(data[[cohort_name]], cohort_name)
            findings <- dplyr::bind_rows(findings, cohort_result$validation_findings)
            details <- dplyr::bind_rows(details, cohort_result$detail_tables)
            validated_cohorts <- unique(c(validated_cohorts, cohort_result$validated_cohorts))
        }

        cross_result <- collect_cross_cohort_validation(data, removal_log = removal_log)
        findings <- dplyr::bind_rows(findings, cross_result$validation_findings)
        details <- dplyr::bind_rows(details, cross_result$detail_tables)

        processed_file_finding <- new_validation_finding(
            check_id = "processed_files_exist",
            finding_group = "cross_cohort",
            scope = "cross_cohort",
            severity = if (!validate_processed_files_exist(data)) "hard_error" else "info",
            status = if (!validate_processed_files_exist(data)) "fail" else "pass",
            metric = "processed_files",
            value = PROCESSED_DATA_DIR,
            message = if (!validate_processed_files_exist(data)) {
                "One or more processed cohort files are missing."
            } else {
                "All processed cohort files are present."
            }
        )
        findings <- dplyr::bind_rows(findings, processed_file_finding)
    } else {
        single_result <- collect_single_cohort_validation(data, "single_dataset")
        findings <- dplyr::bind_rows(findings, single_result$validation_findings)
        details <- dplyr::bind_rows(details, single_result$detail_tables)
        validated_cohorts <- unique(c(validated_cohorts, single_result$validated_cohorts))
    }

    validation_result <- build_validation_result(
        findings = findings,
        detail_tables = details,
        validated_cohorts = validated_cohorts,
        metadata = list(
            has_reconciliation_audit = !is.null(reconciliation_audit),
            has_raw_input_audit = !is.null(input_audit)
        )
    )

    if (validation_result$success) {
        logger::log_info("=== ALL DATA PROCESSING VALIDATIONS PASSED ===")
    } else {
        logger::log_error("=== DATA PROCESSING VALIDATION FAILED - SEE OBJECTIVE 0 ARTIFACTS ===")
        if (isTRUE(stop_on_failure)) {
            stop("Data processing validation failed. Please inspect the Objective 0 validation bundle before proceeding.")
        }
    }

    validation_result
}

generate_validation_report <- function(data) {
    validate_processing_pipeline(data, stop_on_failure = TRUE)$success
}
