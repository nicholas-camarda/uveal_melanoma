# Structured validation reporting helpers for Objective 0

empty_validation_findings <- function() {
    tibble::tibble(
        check_id = character(),
        finding_group = character(),
        scope = character(),
        cohort = character(),
        severity = character(),
        status = character(),
        metric = character(),
        value = character(),
        message = character(),
        affected_n = integer(),
        affected_ids = character()
    )
}

new_validation_finding <- function(check_id,
                                   finding_group,
                                   scope,
                                   cohort = NA_character_,
                                   severity = c("info", "warning", "hard_error"),
                                   status = c("pass", "warn", "fail", "info"),
                                   metric = NA_character_,
                                   value = NA_character_,
                                   message,
                                   affected_n = NA_integer_,
                                   affected_ids = NA_character_) {
    severity <- match.arg(severity)
    status <- match.arg(status)

    tibble::tibble(
        check_id = check_id,
        finding_group = finding_group,
        scope = scope,
        cohort = cohort,
        severity = severity,
        status = status,
        metric = ifelse(is.na(metric), NA_character_, as.character(metric)),
        value = ifelse(is.na(value), NA_character_, as.character(value)),
        message = as.character(message),
        affected_n = ifelse(is.na(affected_n), NA_integer_, as.integer(affected_n)),
        affected_ids = ifelse(is.na(affected_ids), NA_character_, as.character(affected_ids))
    )
}

empty_validation_detail_table <- function() {
    tibble::tibble(
        detail_sheet = character(),
        scope = character(),
        cohort = character(),
        check_id = character()
    )
}

new_validation_detail_table <- function(detail_sheet,
                                        data,
                                        scope = "cohort",
                                        cohort = NA_character_,
                                        check_id = NA_character_) {
    detail_data <- tibble::as_tibble(data)
    detail_data$detail_sheet <- detail_sheet
    detail_data$scope <- scope
    detail_data$cohort <- cohort
    detail_data$check_id <- check_id
    detail_data
}

sanitize_validation_sheet_name <- function(sheet_name, default = "Details") {
    cleaned_name <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", sheet_name)
    cleaned_name <- gsub("_+", "_", cleaned_name)
    cleaned_name <- gsub("^_|_$", "", cleaned_name)

    if (!nzchar(cleaned_name)) {
        cleaned_name <- default
    }

    substr(cleaned_name, 1, 31)
}

cohort_key_to_dataset_name <- function(cohort_key) {
    dplyr::case_when(
        identical(cohort_key, "full_cohort") ~ "uveal_melanoma_full_cohort",
        identical(cohort_key, "restricted_cohort") ~ "uveal_melanoma_restricted_cohort",
        identical(cohort_key, "gksrs_only_cohort") ~ "uveal_melanoma_gksrs_only_cohort",
        TRUE ~ cohort_key
    )
}

dataset_name_to_cohort_key <- function(dataset_name) {
    dplyr::case_when(
        identical(dataset_name, "uveal_melanoma_full_cohort") ~ "full_cohort",
        identical(dataset_name, "uveal_melanoma_restricted_cohort") ~ "restricted_cohort",
        identical(dataset_name, "uveal_melanoma_gksrs_only_cohort") ~ "gksrs_only_cohort",
        TRUE ~ dataset_name
    )
}

severity_rank <- function(severity) {
    dplyr::case_when(
        severity == "hard_error" ~ 3L,
        severity == "warning" ~ 2L,
        severity == "info" ~ 1L,
        TRUE ~ 0L
    )
}

build_validation_result <- function(findings,
                                    detail_tables = NULL,
                                    validated_cohorts = character(),
                                    metadata = list()) {
    normalized_findings <- findings %||% empty_validation_findings()
    normalized_details <- detail_tables %||% empty_validation_detail_table()

    has_hard_errors <- any(
        normalized_findings$severity == "hard_error" &
            normalized_findings$status == "fail",
        na.rm = TRUE
    )

    hard_error_codes <- normalized_findings %>%
        dplyr::filter(.data$severity == "hard_error", .data$status == "fail") %>%
        dplyr::pull(.data$check_id) %>%
        unique()

    warning_codes <- normalized_findings %>%
        dplyr::filter(.data$severity == "warning", .data$status %in% c("warn", "fail")) %>%
        dplyr::pull(.data$check_id) %>%
        unique()

    list(
        success = !has_hard_errors,
        has_hard_errors = has_hard_errors,
        validated_cohorts = unique(validated_cohorts),
        validation_errors = hard_error_codes,
        warning_issues = warning_codes,
        validation_findings = normalized_findings %>%
            dplyr::mutate(severity_rank = severity_rank(.data$severity)) %>%
            dplyr::arrange(dplyr::desc(.data$severity_rank), .data$finding_group, .data$check_id) %>%
            dplyr::select(-"severity_rank"),
        detail_tables = normalized_details,
        metadata = metadata
    )
}

#' Append findings and detail tables to a structured validation result
#'
#' Rebuilds a validation result after late validation checks, preserving
#' metadata and validated-cohort state while recomputing success/hard-error
#' fields from the combined findings.
#'
#' @param validation_result Existing structured validation result.
#' @param findings Additional validation findings.
#' @param detail_tables Additional detail rows.
#' @return Updated structured validation result.
append_validation_result_components <- function(validation_result,
                                                findings = NULL,
                                                detail_tables = NULL) {
    build_validation_result(
        findings = dplyr::bind_rows(
            validation_result$validation_findings %||% empty_validation_findings(),
            findings %||% empty_validation_findings()
        ),
        detail_tables = dplyr::bind_rows(
            validation_result$detail_tables %||% empty_validation_detail_table(),
            detail_tables %||% empty_validation_detail_table()
        ),
        validated_cohorts = validation_result$validated_cohorts %||% character(),
        metadata = validation_result$metadata %||% list()
    )
}

#' Locate the persisted event-date reconciliation workbook for a cohort
#'
#' @param general_dir Objective 0 `00_General` directory for a cohort.
#' @param cohort_key Short cohort key such as `full_cohort`.
#' @return Character path or `NA_character_` when unavailable.
find_existing_reconciliation_workbook <- function(general_dir, cohort_key) {
    preferred_path <- file.path(general_dir, sprintf("%s_event_data_reconcilitation.xlsx", cohort_key))
    candidate_paths <- c(
        preferred_path,
        list.files(
            general_dir,
            pattern = "^event_date_.*\\.xlsx$",
            full.names = TRUE
        )
    )
    candidate_paths <- unique(candidate_paths[file.exists(candidate_paths)])

    if (length(candidate_paths) == 0) {
        return(NA_character_)
    }

    candidate_paths[[1]]
}

#' Read one sheet from a persisted Objective 0 reconciliation workbook
#'
#' @param general_dir Objective 0 `00_General` directory for a cohort.
#' @param cohort_key Short cohort key such as `full_cohort`.
#' @param sheet_name Workbook sheet to read.
#' @param empty_table Zero-row tibble returned when the workbook/sheet is absent.
#' @return Tibble containing the recovered sheet data.
read_existing_reconciliation_sheet <- function(general_dir,
                                               cohort_key,
                                               sheet_name,
                                               empty_table) {
    workbook_path <- find_existing_reconciliation_workbook(general_dir, cohort_key)
    if (is.na(workbook_path)) {
        return(empty_table)
    }

    tryCatch(
        {
            if (!sheet_name %in% readxl::excel_sheets(workbook_path)) {
                return(empty_table)
            }
            tibble::as_tibble(readxl::read_xlsx(workbook_path, sheet = sheet_name))
        },
        error = function(e) {
            logger::log_warn(sprintf(
                "Unable to read existing reconciliation sheet %s for %s from %s: %s",
                sheet_name,
                cohort_key,
                workbook_path,
                conditionMessage(e)
            ))
            empty_table
        }
    )
}

#' Read a persisted reconciliation summary for validation-bundle reuse
#'
#' @param general_dir Objective 0 `00_General` directory for a cohort.
#' @param cohort_key Short cohort key such as `full_cohort`.
#' @return Reconciliation summary tibble.
read_existing_reconciliation_summary <- function(general_dir, cohort_key) {
    read_existing_reconciliation_sheet(
        general_dir = general_dir,
        cohort_key = cohort_key,
        sheet_name = "Reconciliation_Summary",
        empty_table = empty_event_date_audit_summary()
    )
}

#' Rehydrate persisted Objective 0 audit state during reload-mode runs
#'
#' Recovers existing reconciliation sheets so reload-mode validation bundles do
#' not erase audit details created during the original raw-data recreation pass.
#'
#' @param output_dirs Objective output directory list from `build_objective_0_output_dirs()`.
#' @return List with `audit_by_cohort`, validation `findings`, and `details`.
rehydrate_objective0_audit_state <- function(output_dirs) {
    findings <- empty_validation_findings()
    details <- empty_validation_detail_table()
    audit_by_cohort <- list()

    for (cohort_key in names(output_dirs)) {
        cohort_dirs <- output_dirs[[cohort_key]]
        if (is.null(cohort_dirs) || !"baseline_characteristics" %in% names(cohort_dirs)) {
            next
        }

        cohort_dataset_name <- cohort_key_to_dataset_name(cohort_key)
        general_dir <- dirname(cohort_dirs$baseline_characteristics)
        workbook_path <- find_existing_reconciliation_workbook(general_dir, cohort_key)

        if (is.na(workbook_path)) {
            findings <- dplyr::bind_rows(
                findings,
                new_validation_finding(
                    check_id = "objective0_reload_reconciliation_workbook_present",
                    finding_group = "objective0_reload_audit",
                    scope = "cohort",
                    cohort = cohort_dataset_name,
                    severity = "warning",
                    status = "warn",
                    metric = "missing_reconciliation_workbook",
                    value = file.path(general_dir, sprintf("%s_event_data_reconcilitation.xlsx", cohort_key)),
                    message = "Reload-mode Objective 0 could not find the persisted event/date reconciliation workbook; reconciliation audit sheets will be empty.",
                    affected_n = 1L
                )
            )
            audit_by_cohort[[cohort_key]] <- list(
                reconciliation_summary = empty_event_date_audit_summary(),
                reconciled_changes = empty_event_date_audit_rows()
            )
            next
        }

        workbook_sheets <- tryCatch(readxl::excel_sheets(workbook_path), error = function(e) character())
        expected_sheets <- c("Reconciliation_Summary", "Reconciled_Changes")
        missing_sheets <- setdiff(expected_sheets, workbook_sheets)
        if (length(missing_sheets) > 0) {
            findings <- dplyr::bind_rows(
                findings,
                new_validation_finding(
                    check_id = "objective0_reload_reconciliation_sheets_present",
                    finding_group = "objective0_reload_audit",
                    scope = "cohort",
                    cohort = cohort_dataset_name,
                    severity = "warning",
                    status = "warn",
                    metric = "missing_reconciliation_sheets",
                    value = paste(missing_sheets, collapse = ", "),
                    message = sprintf(
                        "Reload-mode Objective 0 found %s but it is missing sheet(s): %s.",
                        basename(workbook_path),
                        paste(missing_sheets, collapse = ", ")
                    ),
                    affected_n = length(missing_sheets)
                )
            )
        }

        reconciliation_summary <- read_existing_reconciliation_sheet(
            general_dir, cohort_key, "Reconciliation_Summary", empty_event_date_audit_summary()
        )
        reconciled_changes <- read_existing_reconciliation_sheet(
            general_dir, cohort_key, "Reconciled_Changes", empty_event_date_audit_rows()
        )
        audit_by_cohort[[cohort_key]] <- list(
            reconciliation_summary = reconciliation_summary,
            reconciled_changes = reconciled_changes
        )

        if (nrow(reconciled_changes) > 0) {
            details <- dplyr::bind_rows(
                details,
                new_validation_detail_table(
                    detail_sheet = "Event_Date_Reconciliations",
                    data = reconciled_changes,
                    scope = "cohort",
                    cohort = cohort_dataset_name,
                    check_id = "objective0_reload_reconciled_changes_rehydrated"
                )
            )
        }
    }

    list(
        audit_by_cohort = audit_by_cohort,
        findings = findings,
        details = details
    )
}

build_validation_summary_table <- function(findings, cohort_dataset_name) {
    relevant_findings <- findings %>%
        dplyr::filter(
            is.na(.data$cohort) |
                .data$cohort == cohort_dataset_name |
                .data$scope %in% c("global", "cross_cohort")
        )

    if (nrow(relevant_findings) == 0) {
        return(tibble::tibble(
            severity = "info",
            status = "pass",
            finding_group = "validation",
            n_findings = 0L
        ))
    }

    relevant_findings %>%
        dplyr::count(.data$severity, .data$status, .data$finding_group, name = "n_findings") %>%
        dplyr::arrange(dplyr::desc(severity_rank(.data$severity)), .data$finding_group, .data$status)
}

#' Convert validation metadata into a compact provenance table
#'
#' @param validation_result Structured validation result.
#' @return Two-column tibble suitable for workbook output.
build_validation_provenance_table <- function(validation_result) {
    metadata <- validation_result$metadata %||% list()
    if (length(metadata) == 0) {
        return(tibble::tibble(field = "provenance", value = "not recorded"))
    }

    tibble::tibble(
        field = names(metadata),
        value = vapply(metadata, function(value) {
            if (length(value) == 0 || is.null(value)) {
                return(NA_character_)
            }
            paste(as.character(value), collapse = ", ")
        }, character(1))
    )
}

render_validation_summary_text <- function(cohort_label,
                                           cohort_dataset_name,
                                           findings,
                                           summary_table,
                                           provenance_table = NULL) {
    relevant_findings <- findings %>%
        dplyr::filter(
            is.na(.data$cohort) |
                .data$cohort == cohort_dataset_name |
                .data$scope %in% c("global", "cross_cohort")
        )

    hard_errors <- relevant_findings %>%
        dplyr::filter(.data$severity == "hard_error", .data$status == "fail")
    warnings <- relevant_findings %>%
        dplyr::filter(.data$severity == "warning", .data$status %in% c("warn", "fail"))

    summary_lines <- c(
        sprintf("%s Validation Summary", toupper(cohort_label)),
        paste(rep("=", nchar(cohort_label) + 19), collapse = ""),
        sprintf("Runtime dataset id: %s", cohort_dataset_name),
        sprintf("Hard errors: %d", nrow(hard_errors)),
        sprintf("Warnings: %d", nrow(warnings)),
        ""
    )

    if (!is.null(provenance_table) && nrow(provenance_table) > 0) {
        provenance_lines <- provenance_table %>%
            dplyr::mutate(line = sprintf("  - %s: %s", .data$field, .data$value)) %>%
            dplyr::pull(.data$line)
        summary_lines <- c(summary_lines, "Provenance:", provenance_lines, "")
    }

    summary_lines <- c(summary_lines, "Grouped counts:")
    grouped_lines <- summary_table %>%
        dplyr::mutate(line = sprintf(
            "  - [%s/%s] %s: %d",
            .data$severity,
            .data$status,
            .data$finding_group,
            .data$n_findings
        )) %>%
        dplyr::pull(.data$line)
    summary_lines <- c(summary_lines, grouped_lines, "")

    if (nrow(hard_errors) > 0) {
        summary_lines <- c(
            summary_lines,
            "Hard-error findings:",
            hard_errors %>%
                dplyr::mutate(line = sprintf("  - %s: %s", .data$check_id, .data$message)) %>%
                dplyr::pull(.data$line),
            ""
        )
    }

    if (nrow(warnings) > 0) {
        summary_lines <- c(
            summary_lines,
            "Warnings:",
            warnings %>%
                dplyr::mutate(line = sprintf("  - %s: %s", .data$check_id, .data$message)) %>%
                dplyr::pull(.data$line)
        )
    }

    summary_lines
}

write_objective0_validation_artifacts <- function(validation_result,
                                                  output_dirs,
                                                  reconciliation_audit = NULL,
                                                  rehydrated_audit = NULL) {
    if (is.null(output_dirs) || length(output_dirs) == 0) {
        return(invisible(list()))
    }

    findings <- validation_result$validation_findings %||% empty_validation_findings()
    detail_tables <- validation_result$detail_tables %||% empty_validation_detail_table()
    provenance_table <- build_validation_provenance_table(validation_result)
    written_paths <- list()

    for (cohort_key in names(output_dirs)) {
        cohort_dirs <- output_dirs[[cohort_key]]
        if (is.null(cohort_dirs) || !"baseline_characteristics" %in% names(cohort_dirs)) {
            next
        }

        general_dir <- dirname(cohort_dirs$baseline_characteristics)
        dir.create(general_dir, recursive = TRUE, showWarnings = FALSE)

        cohort_dataset_name <- cohort_key_to_dataset_name(cohort_key)
        summary_table <- build_validation_summary_table(findings, cohort_dataset_name)
        summary_lines <- render_validation_summary_text(
            cohort_label = gsub("_cohort$", "", cohort_key),
            cohort_dataset_name = cohort_dataset_name,
            findings = findings,
            summary_table = summary_table,
            provenance_table = provenance_table
        )

        relevant_findings <- findings %>%
            dplyr::filter(
                is.na(.data$cohort) |
                    .data$cohort == cohort_dataset_name |
                    .data$scope %in% c("global", "cross_cohort")
            )

        cohort_rehydrated_audit <- rehydrated_audit[[cohort_key]] %||% NULL

        reconciliation_summary <- if (!is.null(reconciliation_audit) &&
            !is.null(reconciliation_audit$audit_summary)) {
            reconciliation_audit$audit_summary
        } else if (!is.null(cohort_rehydrated_audit) &&
            !is.null(cohort_rehydrated_audit$reconciliation_summary)) {
            cohort_rehydrated_audit$reconciliation_summary
        } else {
            read_existing_reconciliation_summary(general_dir, cohort_key)
        }

        workbook_sheets <- list(
            Validation_Summary = summary_table,
            Validation_Provenance = provenance_table,
            Validation_Findings = relevant_findings,
            Critical_Variable_Checks = relevant_findings %>%
                dplyr::filter(.data$finding_group == "critical_variables"),
            Factor_Level_Checks = relevant_findings %>%
                dplyr::filter(.data$finding_group == "factor_levels"),
            Cohort_Rule_Checks = relevant_findings %>%
                dplyr::filter(.data$finding_group %in% c("cohort_rules", "cross_cohort", "raw_input", "downstream_input_contract")),
            Data_Quality_Checks = relevant_findings %>%
                dplyr::filter(.data$finding_group %in% c("data_quality", "date_checks", "derived_ranges", "structure", "endpoint_chronology", "objective0_reload_audit")),
            Reconciliation_Summary = reconciliation_summary
        )

        if (nrow(detail_tables) > 0) {
            relevant_details <- detail_tables %>%
                dplyr::filter(
                    is.na(.data$cohort) |
                        .data$cohort == cohort_dataset_name |
                        .data$scope %in% c("global", "cross_cohort")
                )
            if (nrow(relevant_details) > 0) {
                for (sheet_name in unique(relevant_details$detail_sheet)) {
                    detail_sheet_data <- relevant_details %>%
                        dplyr::filter(.data$detail_sheet == sheet_name) %>%
                        dplyr::select(-dplyr::any_of(c("detail_sheet", "scope", "cohort")))
                    workbook_sheets[[sanitize_validation_sheet_name(sheet_name)]] <- detail_sheet_data
                }
            }
        }

        workbook_sheets <- purrr::imap(workbook_sheets, function(sheet_data, sheet_name) {
            tibble::as_tibble(sheet_data %||% tibble::tibble(note = sprintf("No rows for %s", sheet_name)))
        })

        summary_path <- file.path(general_dir, sprintf("%s_validation_summary.txt", cohort_key))
        bundle_path <- file.path(general_dir, sprintf("%s_validation_bundle.xlsx", cohort_key))

        writeLines(summary_lines, summary_path)
        write_readable_xlsx(workbook_sheets, bundle_path)

        logger::log_info(sprintf("Objective 0 validation summary written to %s", summary_path))
        logger::log_info(sprintf("Objective 0 validation bundle written to %s", bundle_path))

        written_paths[[cohort_key]] <- list(
            summary_path = summary_path,
            bundle_path = bundle_path
        )
    }

    invisible(written_paths)
}
