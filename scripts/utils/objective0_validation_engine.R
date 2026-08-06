# Objective 0 structured validation engine

is_yes_value <- function(x) {
    normalized <- tolower(trimws(as.character(x)))
    normalized %in% c("y", "yes", "1", "true")
}

#' Normalize raw or display yes/no values to logical indicators
#'
#' @param x Vector containing raw (`Y`/`N`) or display (`Yes`/`No`) values.
#' @return Logical vector where yes-like values are `TRUE`, no-like values are
#'   `FALSE`, and unrecognized or missing values are `NA`.
normalize_yes_no_value <- function(x) {
    normalized <- tolower(trimws(as.character(x)))
    dplyr::case_when(
        normalized %in% c("y", "yes", "1", "true") ~ TRUE,
        normalized %in% c("n", "no", "0", "false") ~ FALSE,
        TRUE ~ NA
    )
}

#' Compare numeric contract fields with NA-aware tolerance
#'
#' @param observed Numeric vector of observed values.
#' @param expected Numeric vector of source-derived expected values.
#' @param tolerance Numeric absolute tolerance for finite values.
#' @return Logical vector indicating row-wise equality.
contract_numeric_equal <- function(observed, expected, tolerance = 1e-6) {
    both_missing <- is.na(observed) & is.na(expected)
    both_present_equal <- !is.na(observed) & !is.na(expected) & abs(observed - expected) <= tolerance
    both_missing | both_present_equal
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

#' Validate Objective 2 toxicity endpoint source and burden fields
#'
#' Checks that recorded toxicity endpoint source fields are canonical Y/N values,
#' that the Objective 0-derived burden fields are complete binary 0/1 values in
#' included analytic rows, and that each burden field matches its source field.
#'
#' @param data Data frame for a single analytic cohort.
#' @param cohort_name Character scalar identifying the cohort under validation.
#' @return List containing validation `findings` and detail `details` tables.
validate_objective2_toxicity_endpoint_contract <- function(data, cohort_name) {
    findings <- empty_validation_findings()
    details <- empty_validation_detail_table()

    add_toxicity_finding <- function(...) {
        findings <<- dplyr::bind_rows(findings, new_validation_finding(cohort = cohort_name, scope = "cohort", ...))
    }

    add_toxicity_detail <- function(detail_sheet, data, check_id) {
        details <<- dplyr::bind_rows(
            details,
            new_validation_detail_table(
                detail_sheet = detail_sheet,
                data = data,
                scope = "cohort",
                cohort = cohort_name,
                check_id = check_id
            )
        )
    }

    for (endpoint_index in seq_len(nrow(OBJECTIVE2_TOXICITY_ENDPOINTS))) {
        endpoint <- OBJECTIVE2_TOXICITY_ENDPOINTS[endpoint_index, ]
        source_field <- endpoint$source_field[[1]]
        analysis_field <- endpoint$analysis_field[[1]]
        endpoint_label <- endpoint$endpoint_label[[1]]

        source_missing <- !source_field %in% names(data)
        add_toxicity_finding(
            check_id = paste0("objective2_", source_field, "_source_present"),
            finding_group = "objective2_toxicity_endpoints",
            severity = if (source_missing) "hard_error" else "info",
            status = if (source_missing) "fail" else "pass",
            metric = "source_field",
            value = source_field,
            message = if (source_missing) {
                sprintf("Objective 2 source field '%s' is missing.", source_field)
            } else {
                sprintf("Objective 2 source field '%s' is present.", source_field)
            },
            affected_n = as.integer(source_missing)
        )

        analysis_missing <- !analysis_field %in% names(data)
        add_toxicity_finding(
            check_id = paste0("objective2_", analysis_field, "_present"),
            finding_group = "objective2_toxicity_endpoints",
            severity = if (analysis_missing) "hard_error" else "info",
            status = if (analysis_missing) "fail" else "pass",
            metric = "analysis_field",
            value = analysis_field,
            message = if (analysis_missing) {
                sprintf("Objective 2 burden field '%s' is missing.", analysis_field)
            } else {
                sprintf("Objective 2 burden field '%s' is present.", analysis_field)
            },
            affected_n = as.integer(analysis_missing)
        )

        if (source_missing || analysis_missing) {
            next
        }

        source_values <- as.character(data[[source_field]])
        source_invalid <- is.na(source_values) | !source_values %in% c("Y", "N")
        source_invalid_rows <- data[source_invalid, , drop = FALSE] %>%
            dplyr::select(dplyr::any_of(c("id", source_field, analysis_field)))

        add_toxicity_finding(
            check_id = paste0("objective2_", source_field, "_source_valid"),
            finding_group = "objective2_toxicity_endpoints",
            severity = if (nrow(source_invalid_rows) > 0) "hard_error" else "info",
            status = if (nrow(source_invalid_rows) > 0) "fail" else "pass",
            metric = "invalid_or_missing_source_rows",
            value = nrow(source_invalid_rows),
            message = if (nrow(source_invalid_rows) > 0) {
                sprintf("%s source values must be canonical Y/N in included analytic rows.", endpoint_label)
            } else {
                sprintf("%s source values are complete canonical Y/N values.", endpoint_label)
            },
            affected_n = nrow(source_invalid_rows),
            affected_ids = collapse_affected_ids(source_invalid_rows$id %||% character())
        )

        if (nrow(source_invalid_rows) > 0) {
            add_toxicity_detail(
                detail_sheet = paste0("Obj2_", source_field, "_Source"),
                data = source_invalid_rows,
                check_id = paste0("objective2_", source_field, "_source_valid")
            )
        }

        burden_values <- suppressWarnings(as.numeric(as.character(data[[analysis_field]])))
        burden_invalid <- is.na(data[[analysis_field]]) | is.na(burden_values) | !burden_values %in% c(0, 1)
        burden_invalid_rows <- data[burden_invalid, , drop = FALSE] %>%
            dplyr::select(dplyr::any_of(c("id", source_field, analysis_field)))

        add_toxicity_finding(
            check_id = paste0("objective2_", analysis_field, "_binary_complete"),
            finding_group = "objective2_toxicity_endpoints",
            severity = if (nrow(burden_invalid_rows) > 0) "hard_error" else "info",
            status = if (nrow(burden_invalid_rows) > 0) "fail" else "pass",
            metric = "invalid_or_missing_burden_rows",
            value = nrow(burden_invalid_rows),
            message = if (nrow(burden_invalid_rows) > 0) {
                sprintf("%s burden field must be complete binary 0/1 in included analytic rows.", endpoint_label)
            } else {
                sprintf("%s burden field is complete binary 0/1.", endpoint_label)
            },
            affected_n = nrow(burden_invalid_rows),
            affected_ids = collapse_affected_ids(burden_invalid_rows$id %||% character())
        )

        if (nrow(burden_invalid_rows) > 0) {
            add_toxicity_detail(
                detail_sheet = paste0("Obj2_", source_field, "_Burden"),
                data = burden_invalid_rows,
                check_id = paste0("objective2_", analysis_field, "_binary_complete")
            )
        }

        comparable_rows <- !source_invalid & !burden_invalid
        expected_burden <- dplyr::case_when(
            source_values == "Y" ~ 1,
            source_values == "N" ~ 0,
            TRUE ~ NA_real_
        )
        mismatch_rows <- data[comparable_rows & burden_values != expected_burden, , drop = FALSE] %>%
            dplyr::select(dplyr::any_of(c("id", source_field, analysis_field)))

        add_toxicity_finding(
            check_id = paste0("objective2_", analysis_field, "_matches_source"),
            finding_group = "objective2_toxicity_endpoints",
            severity = if (nrow(mismatch_rows) > 0) "hard_error" else "info",
            status = if (nrow(mismatch_rows) > 0) "fail" else "pass",
            metric = "burden_source_mismatches",
            value = nrow(mismatch_rows),
            message = if (nrow(mismatch_rows) > 0) {
                sprintf("%s burden field does not match its source field for one or more rows.", endpoint_label)
            } else {
                sprintf("%s burden field matches its source field.", endpoint_label)
            },
            affected_n = nrow(mismatch_rows),
            affected_ids = collapse_affected_ids(mismatch_rows$id %||% character())
        )

        if (nrow(mismatch_rows) > 0) {
            add_toxicity_detail(
                detail_sheet = paste0("Obj2_", source_field, "_Mismatch"),
                data = mismatch_rows,
                check_id = paste0("objective2_", analysis_field, "_matches_source")
            )
        }
    }

    list(findings = findings, details = details)
}

#' Validate endpoint chronology fields before downstream objectives run
#'
#' Fails hard when Objective 0-derived event-time fields contain negative
#' values. These fields define event/censoring intervals, so downstream analyses
#' must not normalize impossible chronology into apparently valid follow-up.
#'
#' @param data Data frame for a single analytic cohort.
#' @param cohort_name Character scalar identifying the cohort under validation.
#' @return List containing validation `findings` and detail `details` tables.
validate_endpoint_chronology_contract <- function(data, cohort_name) {
    findings <- empty_validation_findings()
    details <- empty_validation_detail_table()
    endpoint_time_fields <- c(
        "tt_recurrence_months", "tt_recurrence_months_analysis",
        "tt_mets_months", "tt_mets_months_analysis",
        "tt_death_months", "tt_death_months_analysis",
        "tt_pfs_months", "tt_pfs_months_analysis",
        "tt_pfs2_months", "tt_pfs2_years",
        "tt_mfs_5yr", "tt_mfs_7yr", "tt_mfs_10yr",
        "tt_mss_5yr", "tt_mss_7yr", "tt_mss_10yr"
    )
    present_fields <- intersect(endpoint_time_fields, names(data))

    negative_rows <- purrr::map_dfr(present_fields, function(field_name) {
        numeric_values <- suppressWarnings(as.numeric(data[[field_name]]))
        bad_index <- which(!is.na(numeric_values) & numeric_values < 0)
        if (length(bad_index) == 0) {
            return(tibble::tibble(
                id = data$id[integer()] %||% character(),
                field_name = character(),
                field_value = numeric(),
                row_index = integer()
            ))
        }

        tibble::tibble(
            id = data$id[bad_index] %||% NA_character_,
            field_name = field_name,
            field_value = numeric_values[bad_index],
            row_index = bad_index
        )
    })

    findings <- dplyr::bind_rows(
        findings,
        new_validation_finding(
            check_id = "endpoint_event_times_nonnegative",
            finding_group = "endpoint_chronology",
            scope = "cohort",
            cohort = cohort_name,
            severity = if (nrow(negative_rows) > 0) "hard_error" else "info",
            status = if (nrow(negative_rows) > 0) "fail" else "pass",
            metric = "negative_endpoint_time_values",
            value = nrow(negative_rows),
            message = if (nrow(negative_rows) > 0) {
                "Objective 0-derived endpoint time fields contain negative values; downstream event-time analyses are blocked until chronology is corrected upstream."
            } else {
                "All Objective 0-derived endpoint time fields are non-negative where present."
            },
            affected_n = nrow(negative_rows),
            affected_ids = collapse_affected_ids(if ("id" %in% names(negative_rows)) negative_rows$id else character())
        )
    )

    if (nrow(negative_rows) > 0) {
        details <- dplyr::bind_rows(
            details,
            new_validation_detail_table(
                detail_sheet = "Endpoint_Chronology_Failures",
                data = negative_rows,
                scope = "cohort",
                cohort = cohort_name,
                check_id = "endpoint_event_times_nonnegative"
            )
        )
    }

    list(findings = findings, details = details)
}

#' Return allowed values for a downstream objective contract domain
#'
#' @param domain Character scalar domain code from
#'   `OBJECTIVE0_DOWNSTREAM_INPUT_CONTRACT`.
#' @return Character vector of allowed values, or `NULL` for numeric-only domains.
get_objective0_contract_allowed_values <- function(domain) {
    switch(
        domain,
        treatment_factor = TREATMENT_FACTOR_LEVELS,
        yn_display = YN_DISPLAY_LABELS,
        yn_raw = YN_RAW_LEVELS,
        yn_raw_or_display = c(YN_RAW_LEVELS, YN_DISPLAY_LABELS),
        sex_factor = SEX_FACTOR_LEVELS,
        gep_class_simple = c("Class 1", "Class 2", "GEP Failed/Indeterminate", "GEP Not Tested"),
        no_gep_group = c("GEP Failed/Indeterminate", "GEP Not Tested"),
        vision_line_change_bucket = VISION_LINE_CHANGE_CATEGORY_LEVELS,
        vision_followup_timing_source = c(
            "explicit_last_followup",
            "proxy_general_recorded_followup",
            "missing_timing"
        ),
        prame_status = c("Negative", "Positive", "Unknown", "Not Available"),
        gep12_prame_status = c("Negative", "Positive"),
        gep_validation_set = c("Eligible", "No GEP Data"),
        NULL
    )
}

#' Validate Objective 1 source-derived endpoint invariants
#'
#' Checks that Objective 0 recurrence, metastasis, death, and PFS fields match
#' source indicators. Objective 1 PFS is the first local recurrence, metastatic
#' progression, or death from any cause.
#'
#' @param data Data frame for a single analytic cohort.
#' @param cohort_name Character scalar identifying the cohort under validation.
#' @return List containing validation `findings` and detail `details` tables.
validate_objective1_endpoint_invariants <- function(data, cohort_name) {
    findings <- empty_validation_findings()
    details <- empty_validation_detail_table()
    required_fields <- c(
        "recurrence1", "mets_progression", "dod",
        "recurrence_event", "mets_event", "death_event", "pfs_event",
        "tt_recurrence_months", "tt_mets_months", "tt_death_months", "tt_pfs_months",
        "tt_recurrence_months_analysis", "tt_mets_months_analysis",
        "tt_death_months_analysis", "tt_pfs_months_analysis"
    )
    missing_fields <- setdiff(required_fields, names(data))

    if (length(missing_fields) > 0) {
        findings <- dplyr::bind_rows(
            findings,
            new_validation_finding(
                check_id = "objective1_endpoint_invariants",
                finding_group = "objective1_endpoint_invariants",
                scope = "cohort",
                cohort = cohort_name,
                severity = "hard_error",
                status = "fail",
                metric = "missing_invariant_fields",
                value = paste(missing_fields, collapse = ", "),
                message = sprintf("Objective 1 endpoint invariant checks could not run because required field(s) are missing: %s.", paste(missing_fields, collapse = ", ")),
                affected_n = length(missing_fields)
            )
        )
        return(list(findings = findings, details = details))
    }

    expected <- tibble::tibble(
        row_index = seq_len(nrow(data)),
        id = data$id %||% NA_character_,
        recurrence_event = ifelse(normalize_yes_no_value(data$recurrence1) %in% TRUE, 1L, 0L),
        mets_event = ifelse(normalize_yes_no_value(data$mets_progression) %in% TRUE, 1L, 0L),
        death_event = ifelse(!is.na(data$dod), 1L, 0L),
        tt_pfs_months = pmin(data$tt_recurrence_months, data$tt_mets_months, data$tt_death_months, na.rm = FALSE),
        tt_pfs_months_analysis = pmin(
            data$tt_recurrence_months_analysis,
            data$tt_mets_months_analysis,
            data$tt_death_months_analysis,
            na.rm = FALSE
        )
    ) %>%
        dplyr::mutate(pfs_event = ifelse(.data$recurrence_event == 1L | .data$mets_event == 1L | .data$death_event == 1L, 1L, 0L))

    mismatch_rows <- purrr::map_dfr(c(
        "recurrence_event", "mets_event", "death_event", "pfs_event",
        "tt_pfs_months", "tt_pfs_months_analysis"
    ), function(field_name) {
        observed <- suppressWarnings(as.numeric(as.character(data[[field_name]])))
        expected_values <- expected[[field_name]]
        mismatched <- !contract_numeric_equal(observed, expected_values)
        tibble::tibble(
            id = expected$id[mismatched],
            row_index = expected$row_index[mismatched],
            field_name = field_name,
            observed_value = as.character(observed[mismatched]),
            expected_value = as.character(expected_values[mismatched])
        )
    })

    findings <- dplyr::bind_rows(
        findings,
        new_validation_finding(
            check_id = "objective1_endpoint_invariants",
            finding_group = "objective1_endpoint_invariants",
            scope = "cohort",
            cohort = cohort_name,
            severity = if (nrow(mismatch_rows) > 0) "hard_error" else "info",
            status = if (nrow(mismatch_rows) > 0) "fail" else "pass",
            metric = "source_derived_endpoint_mismatches",
            value = nrow(mismatch_rows),
            message = if (nrow(mismatch_rows) > 0) {
                "Objective 1 source-derived endpoint invariants failed; PFS must be the first local recurrence, metastatic progression, or death."
            } else {
                "Objective 1 recurrence, metastasis, death, and PFS invariants passed: PFS is the first local recurrence, metastatic progression, or death from any cause."
            },
            affected_n = nrow(mismatch_rows),
            affected_ids = collapse_affected_ids(mismatch_rows$id %||% character())
        )
    )

    if (nrow(mismatch_rows) > 0) {
        details <- dplyr::bind_rows(
            details,
            new_validation_detail_table(
                detail_sheet = "Objective1_Endpoint_Invariants",
                data = mismatch_rows,
                scope = "cohort",
                cohort = cohort_name,
                check_id = "objective1_endpoint_invariants"
            )
        )
    }

    list(findings = findings, details = details)
}

#' Validate Objective 3 PFS-2 source-derived contract
#'
#' Checks that PFS-2 event and time fields match first-recurrence treatment,
#' second local recurrence, death-as-censoring, and last-known-alive censoring.
#'
#' @param data Data frame for a single analytic cohort.
#' @param cohort_name Character scalar identifying the cohort under validation.
#' @return List containing validation `findings` and detail `details` tables.
validate_objective3_pfs2_derivation_contract <- function(data, cohort_name) {
    findings <- empty_validation_findings()
    details <- empty_validation_detail_table()
    required_fields <- c(
        OBJECTIVE3_PFS2_DERIVATION_CONTRACT$source_fields,
        OBJECTIVE3_PFS2_DERIVATION_CONTRACT$derived_fields
    )
    missing_fields <- setdiff(required_fields, names(data))

    if (length(missing_fields) > 0) {
        findings <- dplyr::bind_rows(
            findings,
            new_validation_finding(
                check_id = "objective3_pfs2_derivation_contract",
                finding_group = "objective3_pfs2_derivation",
                scope = "cohort",
                cohort = cohort_name,
                severity = "hard_error",
                status = "fail",
                metric = "missing_pfs2_contract_fields",
                value = paste(missing_fields, collapse = ", "),
                message = sprintf("Objective 3 PFS-2 derivation contract could not run because required field(s) are missing: %s.", paste(missing_fields, collapse = ", ")),
                affected_n = length(missing_fields)
            )
        )
        return(list(findings = findings, details = details))
    }

    recurrence1_yes <- normalize_yes_no_value(data$recurrence1)
    recurrence2_yes <- normalize_yes_no_value(data$recurrence2)
    has_origin <- recurrence1_yes %in% TRUE & !is.na(data$recurrence1_treatment_date)
    second_recurrence_observed <- has_origin & recurrence2_yes %in% TRUE &
        !is.na(data$recurrence2_date) & (is.na(data$dod) | data$recurrence2_date <= data$dod)
    censor_date <- dplyr::case_when(
        has_origin & !is.na(data$dod) & (is.na(data$recurrence2_date) | data$recurrence2_date > data$dod) ~ data$dod,
        has_origin ~ data$last_known_alive_date,
        TRUE ~ as.Date(NA)
    )
    end_date <- dplyr::case_when(
        second_recurrence_observed ~ data$recurrence2_date,
        has_origin ~ censor_date,
        TRUE ~ as.Date(NA)
    )
    expected_clean <- dplyr::case_when(
        recurrence1_yes %in% TRUE & !is.na(data$recurrence1_treatment) & grepl("gk", tolower(data$recurrence1_treatment)) ~ "GKSRS",
        recurrence1_yes %in% TRUE & !is.na(data$recurrence1_treatment) & grepl("enuc", tolower(data$recurrence1_treatment)) ~ "Enucleation",
        recurrence1_yes %in% TRUE & !is.na(data$recurrence1_treatment) & grepl("ttt", tolower(data$recurrence1_treatment)) ~ "TTT",
        recurrence1_yes %in% TRUE & !is.na(data$recurrence1_treatment) ~ as.character(data$recurrence1_treatment),
        TRUE ~ NA_character_
    )
    expected <- tibble::tibble(
        row_index = seq_len(nrow(data)),
        id = data$id %||% NA_character_,
        pfs2_event = dplyr::case_when(
            second_recurrence_observed ~ 1,
            has_origin ~ 0,
            TRUE ~ NA_real_
        ),
        tt_pfs2_months = dplyr::case_when(
            has_origin & !is.na(end_date) ~ lubridate::time_length(lubridate::interval(data$recurrence1_treatment_date, end_date), "months"),
            TRUE ~ NA_real_
        ),
        tt_pfs2_years = dplyr::case_when(
            has_origin & !is.na(end_date) ~ lubridate::time_length(lubridate::interval(data$recurrence1_treatment_date, end_date), "years"),
            TRUE ~ NA_real_
        ),
        recurrence1_treatment_clean = expected_clean
    )

    numeric_mismatches <- purrr::map_dfr(c("pfs2_event", "tt_pfs2_months", "tt_pfs2_years"), function(field_name) {
        observed <- suppressWarnings(as.numeric(as.character(data[[field_name]])))
        expected_values <- expected[[field_name]]
        mismatched <- !contract_numeric_equal(observed, expected_values)
        tibble::tibble(
            id = expected$id[mismatched],
            row_index = expected$row_index[mismatched],
            field_name = field_name,
            observed_value = as.character(observed[mismatched]),
            expected_value = as.character(expected_values[mismatched])
        )
    })
    clean_mismatches <- tibble::tibble(
        id = expected$id,
        row_index = expected$row_index,
        field_name = "recurrence1_treatment_clean",
        observed_value = as.character(data$recurrence1_treatment_clean),
        expected_value = expected$recurrence1_treatment_clean
    ) %>%
        dplyr::filter(!(
            (is.na(.data$observed_value) & is.na(.data$expected_value)) |
                (!is.na(.data$observed_value) & !is.na(.data$expected_value) & .data$observed_value == .data$expected_value)
        ))

    mismatch_rows <- dplyr::bind_rows(numeric_mismatches, clean_mismatches)

    findings <- dplyr::bind_rows(
        findings,
        new_validation_finding(
            check_id = "objective3_pfs2_derivation_contract",
            finding_group = "objective3_pfs2_derivation",
            scope = "cohort",
            cohort = cohort_name,
            severity = if (nrow(mismatch_rows) > 0) "hard_error" else "info",
            status = if (nrow(mismatch_rows) > 0) "fail" else "pass",
            metric = "pfs2_source_derived_mismatches",
            value = nrow(mismatch_rows),
            message = if (nrow(mismatch_rows) > 0) {
                "Objective 3 PFS-2 event/time fields do not match the source-date contract."
            } else {
                "Objective 3 PFS-2 event/time fields match the source-date contract."
            },
            affected_n = nrow(mismatch_rows),
            affected_ids = collapse_affected_ids(mismatch_rows$id %||% character())
        )
    )

    if (nrow(mismatch_rows) > 0) {
        details <- dplyr::bind_rows(
            details,
            new_validation_detail_table(
                detail_sheet = "Objective3_PFS2_Derivation",
                data = mismatch_rows,
                scope = "cohort",
                cohort = cohort_name,
                check_id = "objective3_pfs2_derivation_contract"
            )
        )
    }

    list(findings = findings, details = details)
}

#' Validate Objective 4 GEP source-derived contract
#'
#' Checks expected survival, predicted risk, horizon event/type/time fields,
#' endpoint-specific eligibility flags, and the GEP availability label.
#'
#' @param data Data frame for a single analytic cohort.
#' @param cohort_name Character scalar identifying the cohort under validation.
#' @return List containing validation `findings` and detail `details` tables.
validate_objective4_gep_derivation_contract <- function(data, cohort_name) {
    findings <- empty_validation_findings()
    details <- empty_validation_detail_table()
    required_fields <- unique(c(
        "biopsy1_gep", "gep_class_simple", "biopsy1_gep_mfs", "biopsy1_gep_mss",
        "mets_event", "tt_mets_months", "death_event", "tt_death_years",
        "melanoma_death_event", "competing_death_event", "gep_validation_set",
        OBJECTIVE4_GEP_DERIVATION_CONTRACT$source_probability_field,
        OBJECTIVE4_GEP_DERIVATION_CONTRACT$expected_survival_field,
        OBJECTIVE4_GEP_DERIVATION_CONTRACT$predicted_risk_field,
        OBJECTIVE4_GEP_DERIVATION_CONTRACT$event_field,
        OBJECTIVE4_GEP_DERIVATION_CONTRACT$event_type_field,
        OBJECTIVE4_GEP_DERIVATION_CONTRACT$time_field,
        OBJECTIVE4_GEP_DERIVATION_CONTRACT$eligibility_field
    ))
    missing_fields <- setdiff(required_fields, names(data))

    if (length(missing_fields) > 0) {
        findings <- dplyr::bind_rows(
            findings,
            new_validation_finding(
                check_id = "objective4_gep_derivation_contract",
                finding_group = "objective4_gep_derivation",
                scope = "cohort",
                cohort = cohort_name,
                severity = "hard_error",
                status = "fail",
                metric = "missing_gep_contract_fields",
                value = paste(missing_fields, collapse = ", "),
                message = sprintf("Objective 4 GEP derivation contract could not run because required field(s) are missing: %s.", paste(missing_fields, collapse = ", ")),
                affected_n = length(missing_fields)
            )
        )
        return(list(findings = findings, details = details))
    }

    simple_values <- as.character(data$gep_class_simple)
    biopsy_values <- as.character(data$biopsy1_gep)
    definitive_gep <- !is.na(simple_values) & simple_values %in% GEP_DEFINITIVE_SIMPLE_LEVELS &
        !is.na(biopsy_values) & !biopsy_values %in% GEP_INVALID_ANALYSIS_LABELS
    valid_mfs <- !is.na(data$biopsy1_gep_mfs) & data$biopsy1_gep_mfs >= 0 & data$biopsy1_gep_mfs <= 1
    valid_mss <- !is.na(data$biopsy1_gep_mss) & data$biopsy1_gep_mss >= 0 & data$biopsy1_gep_mss <= 1
    expected_gep_set <- ifelse(definitive_gep & valid_mfs & valid_mss, "Eligible", "No GEP Data")

    mismatch_rows <- purrr::pmap_dfr(OBJECTIVE4_GEP_DERIVATION_CONTRACT, function(
        outcome, horizon_years, horizon_months, source_probability_field,
        expected_survival_field, predicted_risk_field, event_field,
        event_type_field, time_field, time_unit, eligibility_field
    ) {
        source_probability <- data[[source_probability_field]]
        expected_survival <- ifelse(!is.na(source_probability), source_probability^(horizon_years / 5), NA_real_)
        expected_risk <- 1 - expected_survival
        if (identical(outcome, "mfs")) {
            expected_event <- ifelse(data$mets_event == 1 & data$tt_mets_months <= horizon_months, 1, 0)
            expected_type <- dplyr::case_when(
                !is.na(data$mets_event) & data$mets_event == 1 & !is.na(data$tt_mets_months) & data$tt_mets_months <= horizon_months ~ 1,
                !is.na(data$death_event) & data$death_event == 1 & !is.na(data$tt_death_years) & data$tt_death_years <= horizon_years & !is.na(data$melanoma_death_event) & data$melanoma_death_event == 0 ~ 2,
                TRUE ~ 0
            )
            expected_time <- pmin(data$tt_mets_months, horizon_months)
            expected_eligible <- definitive_gep & valid_mfs & !is.na(data$tt_mets_months) & !is.na(data$mets_event) & data$tt_mets_months >= 0
        } else {
            expected_event <- ifelse(data$melanoma_death_event == 1 & data$tt_death_years <= horizon_years, 1, 0)
            expected_type <- dplyr::case_when(
                !is.na(data$melanoma_death_event) & data$melanoma_death_event == 1 & !is.na(data$tt_death_years) & data$tt_death_years <= horizon_years ~ 1L,
                !is.na(data$competing_death_event) & data$competing_death_event == 1 & !is.na(data$tt_death_years) & data$tt_death_years <= horizon_years ~ 2L,
                TRUE ~ 0L
            )
            expected_time <- pmin(data$tt_death_years, horizon_years)
            expected_eligible <- definitive_gep & valid_mss & !is.na(data$tt_death_years) &
                !is.na(data$melanoma_death_event) & !is.na(data$competing_death_event) & data$tt_death_years >= 0
        }

        checks <- list(
            expected_survival_field = expected_survival,
            predicted_risk_field = expected_risk,
            event_field = expected_event,
            event_type_field = expected_type,
            time_field = expected_time,
            eligibility_field = expected_eligible
        )
        purrr::imap_dfr(checks, function(expected_values, field_pointer) {
            field_name <- get(field_pointer)
            observed <- data[[field_name]]
            if (is.logical(expected_values)) {
                observed_logical <- as.logical(observed)
                mismatched <- !(is.na(observed_logical) & is.na(expected_values)) &
                    !( !is.na(observed_logical) & !is.na(expected_values) & observed_logical == expected_values)
                observed_text <- as.character(observed[mismatched])
                expected_text <- as.character(expected_values[mismatched])
            } else {
                observed_numeric <- suppressWarnings(as.numeric(as.character(observed)))
                mismatched <- !contract_numeric_equal(observed_numeric, expected_values)
                observed_text <- as.character(observed_numeric[mismatched])
                expected_text <- as.character(expected_values[mismatched])
            }
            tibble::tibble(
                id = data$id[mismatched] %||% NA_character_,
                row_index = which(mismatched),
                field_name = field_name,
                observed_value = observed_text,
                expected_value = expected_text
            )
        })
    })

    gep_set_mismatches <- tibble::tibble(
        id = data$id %||% NA_character_,
        row_index = seq_len(nrow(data)),
        field_name = "gep_validation_set",
        observed_value = as.character(data$gep_validation_set),
        expected_value = expected_gep_set
    ) %>%
        dplyr::filter(.data$observed_value != .data$expected_value | is.na(.data$observed_value) != is.na(.data$expected_value))
    retired_split_rows <- tibble::tibble(
        id = data$id %||% NA_character_,
        row_index = seq_len(nrow(data)),
        field_name = "gep_validation_set",
        observed_value = as.character(data$gep_validation_set),
        expected_value = "Eligible or No GEP Data"
    ) %>%
        dplyr::filter(.data$observed_value %in% c("Training", "Testing"))
    mismatch_rows <- dplyr::bind_rows(mismatch_rows, gep_set_mismatches, retired_split_rows) %>%
        dplyr::distinct()

    findings <- dplyr::bind_rows(
        findings,
        new_validation_finding(
            check_id = "objective4_gep_derivation_contract",
            finding_group = "objective4_gep_derivation",
            scope = "cohort",
            cohort = cohort_name,
            severity = if (nrow(mismatch_rows) > 0) "hard_error" else "info",
            status = if (nrow(mismatch_rows) > 0) "fail" else "pass",
            metric = "gep_source_derived_mismatches",
            value = nrow(mismatch_rows),
            message = if (nrow(mismatch_rows) > 0) {
                "Objective 4 GEP derived probabilities, horizon fields, eligibility, or availability labels do not match the source-derived contract."
            } else {
                "Objective 4 GEP derived probabilities, horizon fields, eligibility, and availability labels match the source-derived contract."
            },
            affected_n = nrow(mismatch_rows),
            affected_ids = collapse_affected_ids(mismatch_rows$id %||% character())
        )
    )

    if (nrow(mismatch_rows) > 0) {
        details <- dplyr::bind_rows(
            details,
            new_validation_detail_table(
                detail_sheet = "Objective4_GEP_Derivation",
                data = mismatch_rows,
                scope = "cohort",
                cohort = cohort_name,
                check_id = "objective4_gep_derivation_contract"
            )
        )
    }

    list(findings = findings, details = details)
}

#' Validate one variable against a downstream objective contract row
#'
#' @param values Vector from the cohort data.
#' @param domain Character scalar expected domain code.
#' @param missing_policy Character scalar, either `complete` or `optional`.
#' @return List containing logical invalid-row and missing-row indicators.
evaluate_objective0_contract_values <- function(values, domain, missing_policy) {
    missing_rows <- is.na(values)
    invalid_rows <- rep(FALSE, length(values))
    allowed_values <- get_objective0_contract_allowed_values(domain)

    if (!is.null(allowed_values)) {
        invalid_rows <- !missing_rows & !as.character(values) %in% allowed_values
    } else if (domain %in% c("binary_01", "event_type_012", "nonnegative_numeric", "nonnegative_integer", "numeric", "probability", "logical")) {
        numeric_values <- suppressWarnings(as.numeric(as.character(values)))
        numeric_missing <- is.na(numeric_values)
        if (domain == "binary_01") {
            invalid_rows <- !missing_rows & (numeric_missing | !numeric_values %in% c(0, 1))
        } else if (domain == "event_type_012") {
            invalid_rows <- !missing_rows & (numeric_missing | !numeric_values %in% c(0, 1, 2))
        } else if (domain == "nonnegative_numeric") {
            invalid_rows <- !missing_rows & (numeric_missing | !is.finite(numeric_values) | numeric_values < 0)
        } else if (domain == "nonnegative_integer") {
            invalid_rows <- !missing_rows & (
                numeric_missing |
                    !is.finite(numeric_values) |
                    numeric_values < 0 |
                    numeric_values != floor(numeric_values)
            )
        } else if (domain == "probability") {
            invalid_rows <- !missing_rows & (numeric_missing | !is.finite(numeric_values) | numeric_values < 0 | numeric_values > 1)
        } else if (domain == "logical") {
            normalized_values <- tolower(as.character(values))
            invalid_rows <- !missing_rows & !normalized_values %in% c("true", "false", "1", "0")
        } else {
            invalid_rows <- !missing_rows & (numeric_missing | !is.finite(numeric_values))
        }
    }

    list(
        invalid_rows = invalid_rows,
        missing_rows = if (identical(missing_policy, "complete")) missing_rows else rep(FALSE, length(values))
    )
}

#' Validate downstream objective input variables from the central registry
#'
#' Checks that Objective 1-4 model-facing inputs exist and remain inside their
#' expected domains before downstream scripts can interpret them.
#'
#' @param data Data frame for a single analytic cohort.
#' @param cohort_name Character scalar identifying the cohort under validation.
#' @param registry Contract table, defaulting to
#'   `OBJECTIVE0_DOWNSTREAM_INPUT_CONTRACT`.
#' @return List containing validation `findings` and detail `details` tables.
validate_downstream_objective_input_contract <- function(data,
                                                         cohort_name,
                                                         registry = OBJECTIVE0_DOWNSTREAM_INPUT_CONTRACT) {
    findings <- empty_validation_findings()
    details <- empty_validation_detail_table()

    missing_variables <- registry %>%
        dplyr::filter(!.data$variable_name %in% names(data)) %>%
        dplyr::mutate(issue_type = "missing_variable")
    hard_missing_variables <- missing_variables %>%
        dplyr::filter(.data$severity == "hard_error")
    warning_missing_variables <- missing_variables %>%
        dplyr::filter(.data$severity != "hard_error")

    findings <- dplyr::bind_rows(
        findings,
        new_validation_finding(
            check_id = "downstream_objective_inputs_present",
            finding_group = "downstream_input_contract",
            scope = "cohort",
            cohort = cohort_name,
            severity = if (nrow(hard_missing_variables) > 0) "hard_error" else if (nrow(warning_missing_variables) > 0) "warning" else "info",
            status = if (nrow(hard_missing_variables) > 0) "fail" else if (nrow(warning_missing_variables) > 0) "warn" else "pass",
            metric = "missing_downstream_input_variables",
            value = paste(missing_variables$variable_name, collapse = ", "),
            message = if (nrow(missing_variables) > 0) {
                sprintf(
                    "Objective 0 output is missing downstream objective input variable(s): %s.",
                    paste(missing_variables$variable_name, collapse = ", ")
                )
            } else {
                "All registered downstream objective input variables are present."
            },
            affected_n = nrow(missing_variables)
        )
    )

    domain_issues <- list()
    present_registry <- registry %>%
        dplyr::filter(.data$variable_name %in% names(data))

    for (row_index in seq_len(nrow(present_registry))) {
        contract_row <- present_registry[row_index, ]
        variable_name <- contract_row$variable_name[[1]]
        contract_result <- evaluate_objective0_contract_values(
            values = data[[variable_name]],
            domain = contract_row$expected_domain[[1]],
            missing_policy = contract_row$missing_policy[[1]]
        )

        issue_index <- which(contract_result$invalid_rows | contract_result$missing_rows)
        if (length(issue_index) == 0) {
            next
        }

        issue_type <- ifelse(contract_result$missing_rows[issue_index], "missing_required_value", "invalid_domain_value")
        domain_issues[[length(domain_issues) + 1L]] <- tibble::tibble(
            objective_id = contract_row$objective_id[[1]],
            variable_name = variable_name,
            variable_role = contract_row$variable_role[[1]],
            expected_domain = contract_row$expected_domain[[1]],
            missing_policy = contract_row$missing_policy[[1]],
            issue_type = issue_type,
            id = data$id[issue_index] %||% NA_character_,
            row_index = issue_index,
            observed_value = as.character(data[[variable_name]][issue_index])
        )
    }

    domain_issues <- if (length(domain_issues) > 0) {
        dplyr::bind_rows(domain_issues)
    } else {
        tibble::tibble(
            objective_id = character(),
            variable_name = character(),
            variable_role = character(),
            expected_domain = character(),
            missing_policy = character(),
            issue_type = character(),
            id = data$id[integer()] %||% character(),
            row_index = integer(),
            observed_value = character()
        )
    }

    hard_domain_issues <- if (nrow(domain_issues) > 0) {
        domain_issues %>%
            dplyr::left_join(
                registry %>% dplyr::select("objective_id", "variable_name", "severity"),
                by = c("objective_id", "variable_name")
            ) %>%
            dplyr::filter(.data$severity == "hard_error")
    } else {
        tibble::tibble()
    }

    warning_domain_issues <- if (nrow(domain_issues) > 0) {
        domain_issues %>%
            dplyr::left_join(
                registry %>% dplyr::select("objective_id", "variable_name", "severity"),
                by = c("objective_id", "variable_name")
            ) %>%
            dplyr::filter(.data$severity != "hard_error")
    } else {
        tibble::tibble()
    }

    findings <- dplyr::bind_rows(
        findings,
        new_validation_finding(
            check_id = "downstream_objective_inputs_valid",
            finding_group = "downstream_input_contract",
            scope = "cohort",
            cohort = cohort_name,
            severity = if (nrow(hard_domain_issues) > 0) "hard_error" else if (nrow(warning_domain_issues) > 0) "warning" else "info",
            status = if (nrow(hard_domain_issues) > 0) "fail" else if (nrow(warning_domain_issues) > 0) "warn" else "pass",
            metric = "invalid_downstream_input_values",
            value = nrow(domain_issues),
            message = if (nrow(domain_issues) > 0) {
                "Registered downstream objective input variables contain missing required values or out-of-domain values."
            } else {
                "Registered downstream objective input values match their expected domains."
            },
            affected_n = nrow(domain_issues),
            affected_ids = collapse_affected_ids(if ("id" %in% names(domain_issues)) domain_issues$id else character())
        )
    )

    if (nrow(missing_variables) > 0) {
        details <- dplyr::bind_rows(
            details,
            new_validation_detail_table(
                detail_sheet = "Downstream_Input_Missing",
                data = missing_variables,
                scope = "cohort",
                cohort = cohort_name,
                check_id = "downstream_objective_inputs_present"
            )
        )
    }

    if (nrow(domain_issues) > 0) {
        details <- dplyr::bind_rows(
            details,
            new_validation_detail_table(
                detail_sheet = "Downstream_Input_Invalid",
                data = domain_issues,
                scope = "cohort",
                cohort = cohort_name,
                check_id = "downstream_objective_inputs_valid"
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

    if ("consort_group" %in% names(data)) {
        consort_values <- as.character(data$consort_group)
        other_rows <- data[!is.na(consort_values) & consort_values == "other", , drop = FALSE]
        unclassified_rows <- data[!is.na(consort_values) & consort_values == CONSORT_GROUP_UNCLASSIFIED_FIELDS, , drop = FALSE]
        special_case_rows <- data[!is.na(consort_values) & consort_values == CONSORT_GROUP_FULL_ONLY_SPECIAL_CASE, , drop = FALSE]
        special_case_in_subcohort <- !identical(cohort_name, "uveal_melanoma_full_cohort") && nrow(special_case_rows) > 0

        add_finding(
            check_id = "retired_other_consort_group_absent",
            finding_group = "cohort_rules",
            severity = if (nrow(other_rows) > 0) "hard_error" else "info",
            status = if (nrow(other_rows) > 0) "fail" else "pass",
            metric = "other_consort_group_rows",
            value = nrow(other_rows),
            message = if (nrow(other_rows) > 0) {
                "Analytic cohort contains retired consort_group == 'other' rows."
            } else {
                "Retired consort_group == 'other' rows are absent."
            },
            affected_n = nrow(other_rows),
            affected_ids = collapse_affected_ids(other_rows$id %||% character())
        )
        if (nrow(other_rows) > 0) {
            add_detail("Retired_Other_Consort_Group", other_rows)
        }

        add_finding(
            check_id = "unclassified_cohort_fields_absent",
            finding_group = "cohort_rules",
            severity = if (nrow(unclassified_rows) > 0) "hard_error" else "info",
            status = if (nrow(unclassified_rows) > 0) "fail" else "pass",
            metric = "unclassified_cohort_rows",
            value = nrow(unclassified_rows),
            message = if (nrow(unclassified_rows) > 0) {
                "Analytic cohort contains rows with unresolved cohort-defining fields."
            } else {
                "Rows with unresolved cohort-defining fields are absent from analytic cohorts."
            },
            affected_n = nrow(unclassified_rows),
            affected_ids = collapse_affected_ids(unclassified_rows$id %||% character())
        )
        if (nrow(unclassified_rows) > 0) {
            add_detail("Unclassified_Cohort_Fields", unclassified_rows)
        }

        add_finding(
            check_id = "full_cohort_only_special_cases",
            finding_group = "cohort_rules",
            severity = if (special_case_in_subcohort) "hard_error" else "info",
            status = if (special_case_in_subcohort) "fail" else if (nrow(special_case_rows) > 0) "info" else "pass",
            metric = "full_cohort_only_special_case_rows",
            value = nrow(special_case_rows),
            message = if (special_case_in_subcohort) {
                "Full-cohort-only special-case rows are present in a restricted or GKSRS-only subcohort."
            } else if (nrow(special_case_rows) > 0) {
                "Full-cohort-only special-case rows are audited and retained outside restricted/GKSRS-only subcohorts."
            } else {
                "No full-cohort-only special-case rows are present."
            },
            affected_n = nrow(special_case_rows),
            affected_ids = collapse_affected_ids(special_case_rows$id %||% character())
        )
        if (nrow(special_case_rows) > 0) {
            add_detail("Full_Cohort_Only_Special_Cases", special_case_rows)
        }
    }

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

    missing_critical <- setdiff(OBJECTIVE0_GLOBAL_REQUIRED_VARIABLES, names(data))
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

    missing_derived <- setdiff(OBJECTIVE0_DERIVED_OUTPUT_MANIFEST, names(data))
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

    toxicity_endpoint_result <- validate_objective2_toxicity_endpoint_contract(data, cohort_name)
    findings <- dplyr::bind_rows(findings, toxicity_endpoint_result$findings)
    details <- dplyr::bind_rows(details, toxicity_endpoint_result$details)

    endpoint_chronology_result <- validate_endpoint_chronology_contract(data, cohort_name)
    findings <- dplyr::bind_rows(findings, endpoint_chronology_result$findings)
    details <- dplyr::bind_rows(details, endpoint_chronology_result$details)

    downstream_input_result <- validate_downstream_objective_input_contract(data, cohort_name)
    findings <- dplyr::bind_rows(findings, downstream_input_result$findings)
    details <- dplyr::bind_rows(details, downstream_input_result$details)

    objective1_invariant_result <- validate_objective1_endpoint_invariants(data, cohort_name)
    findings <- dplyr::bind_rows(findings, objective1_invariant_result$findings)
    details <- dplyr::bind_rows(details, objective1_invariant_result$details)

    objective3_pfs2_result <- validate_objective3_pfs2_derivation_contract(data, cohort_name)
    findings <- dplyr::bind_rows(findings, objective3_pfs2_result$findings)
    details <- dplyr::bind_rows(details, objective3_pfs2_result$details)

    objective4_gep_result <- validate_objective4_gep_derivation_contract(data, cohort_name)
    findings <- dplyr::bind_rows(findings, objective4_gep_result$findings)
    details <- dplyr::bind_rows(details, objective4_gep_result$details)

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
