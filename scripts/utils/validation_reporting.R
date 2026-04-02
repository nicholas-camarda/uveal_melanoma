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

read_existing_reconciliation_summary <- function(general_dir, cohort_key) {
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
        return(empty_event_date_audit_summary())
    }

    tryCatch(
        {
            summary_sheet <- readxl::read_xlsx(candidate_paths[[1]], sheet = "Reconciliation_Summary")
            tibble::as_tibble(summary_sheet)
        },
        error = function(e) {
            logger::log_warn(sprintf(
                "Unable to read existing reconciliation summary for %s from %s: %s",
                cohort_key,
                candidate_paths[[1]],
                conditionMessage(e)
            ))
            empty_event_date_audit_summary()
        }
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

render_validation_summary_text <- function(cohort_label,
                                           cohort_dataset_name,
                                           findings,
                                           summary_table) {
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
                                                  reconciliation_audit = NULL) {
    if (is.null(output_dirs) || length(output_dirs) == 0) {
        return(invisible(list()))
    }

    findings <- validation_result$validation_findings %||% empty_validation_findings()
    detail_tables <- validation_result$detail_tables %||% empty_validation_detail_table()
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
            summary_table = summary_table
        )

        relevant_findings <- findings %>%
            dplyr::filter(
                is.na(.data$cohort) |
                    .data$cohort == cohort_dataset_name |
                    .data$scope %in% c("global", "cross_cohort")
            )

        reconciliation_summary <- if (!is.null(reconciliation_audit) &&
            !is.null(reconciliation_audit$audit_summary)) {
            reconciliation_audit$audit_summary
        } else {
            read_existing_reconciliation_summary(general_dir, cohort_key)
        }

        manual_date_corrections <- if (!is.null(reconciliation_audit) &&
            !is.null(reconciliation_audit$manual_date_corrections)) {
            tibble::as_tibble(reconciliation_audit$manual_date_corrections)
        } else {
            empty_manual_date_correction_audit_rows()
        }

        workbook_sheets <- list(
            Validation_Summary = summary_table,
            Validation_Findings = relevant_findings,
            Critical_Variable_Checks = relevant_findings %>%
                dplyr::filter(.data$finding_group == "critical_variables"),
            Factor_Level_Checks = relevant_findings %>%
                dplyr::filter(.data$finding_group == "factor_levels"),
            Cohort_Rule_Checks = relevant_findings %>%
                dplyr::filter(.data$finding_group %in% c("cohort_rules", "cross_cohort", "raw_input")),
            Data_Quality_Checks = relevant_findings %>%
                dplyr::filter(.data$finding_group %in% c("data_quality", "date_checks", "derived_ranges", "structure")),
            Reconciliation_Summary = reconciliation_summary,
            Manual_Date_Corrections = manual_date_corrections
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
        writexl::write_xlsx(workbook_sheets, bundle_path)

        logger::log_info(sprintf("Objective 0 validation summary written to %s", summary_path))
        logger::log_info(sprintf("Objective 0 validation bundle written to %s", bundle_path))

        written_paths[[cohort_key]] <- list(
            summary_path = summary_path,
            bundle_path = bundle_path
        )
    }

    invisible(written_paths)
}
