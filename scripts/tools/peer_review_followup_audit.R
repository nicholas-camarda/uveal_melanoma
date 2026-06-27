#' Normalize optic nerve abutment or involvement values
#'
#' @param x Vector containing optic nerve values.
#' @return Logical vector where `TRUE` indicates abutment or involvement.
normalize_optic_nerve_involvement <- function(x) {
    normalized <- tolower(trimws(as.character(x)))
    normalized %in% c(
        "y", "yes", "true", "1", "involved", "positive",
        "abutment", "abutting", "abuts", "involvement"
    )
}

#' Summarize follow-up and latest visual acuity timing fields
#'
#' @param data Analytic cohort data.
#' @return Tibble with availability and timing summaries.
summarize_peer_review_followup <- function(data) {
    treatment_date <- if ("treatment_date" %in% names(data)) data$treatment_date else rep(as.Date(NA), nrow(data))
    last_followup <- if ("last_followup" %in% names(data)) data$last_followup else rep(as.Date(NA), nrow(data))
    latest_va_followup_months <- as.numeric(difftime(last_followup, treatment_date, units = "days")) / 30.4375
    fields <- c(
        "treatment_date",
        "last_followup",
        "last_vision",
        "latest_vision_followup_months",
        "follow_up_months",
        "follow_up_years"
    )

    tibble::tibble(
        field = fields,
        present = fields %in% names(data) | fields == "latest_vision_followup_months",
        non_missing_n = c(
            sum(!is.na(treatment_date)),
            sum(!is.na(last_followup)),
            if ("last_vision" %in% names(data)) sum(!is.na(data$last_vision)) else 0L,
            sum(!is.na(latest_va_followup_months)),
            if ("follow_up_months" %in% names(data)) sum(!is.na(data$follow_up_months)) else 0L,
            if ("follow_up_years" %in% names(data)) sum(!is.na(data$follow_up_years)) else 0L
        ),
        denominator_n = nrow(data),
        min_value = c(
            NA_real_,
            NA_real_,
            if ("last_vision" %in% names(data)) suppressWarnings(min(data$last_vision, na.rm = TRUE)) else NA_real_,
            suppressWarnings(min(latest_va_followup_months, na.rm = TRUE)),
            if ("follow_up_months" %in% names(data)) suppressWarnings(min(data$follow_up_months, na.rm = TRUE)) else NA_real_,
            if ("follow_up_years" %in% names(data)) suppressWarnings(min(data$follow_up_years, na.rm = TRUE)) else NA_real_
        ),
        median_value = c(
            NA_real_,
            NA_real_,
            if ("last_vision" %in% names(data)) suppressWarnings(stats::median(data$last_vision, na.rm = TRUE)) else NA_real_,
            suppressWarnings(stats::median(latest_va_followup_months, na.rm = TRUE)),
            if ("follow_up_months" %in% names(data)) suppressWarnings(stats::median(data$follow_up_months, na.rm = TRUE)) else NA_real_,
            if ("follow_up_years" %in% names(data)) suppressWarnings(stats::median(data$follow_up_years, na.rm = TRUE)) else NA_real_
        ),
        max_value = c(
            NA_real_,
            NA_real_,
            if ("last_vision" %in% names(data)) suppressWarnings(max(data$last_vision, na.rm = TRUE)) else NA_real_,
            suppressWarnings(max(latest_va_followup_months, na.rm = TRUE)),
            if ("follow_up_months" %in% names(data)) suppressWarnings(max(data$follow_up_months, na.rm = TRUE)) else NA_real_,
            if ("follow_up_years" %in% names(data)) suppressWarnings(max(data$follow_up_years, na.rm = TRUE)) else NA_real_
        ),
        note = c(
            "Treatment anchor used for latest VA follow-up timing.",
            "Last follow-up date; latest VA is treated as associated with this follow-up.",
            "Latest visual acuity value, not itself a date.",
            "Derived as treatment_date to last_followup when both dates are present.",
            "Existing total follow-up field.",
            "Existing total follow-up field."
        )
    ) %>%
        dplyr::mutate(
            min_value = dplyr::if_else(is.infinite(.data$min_value), NA_real_, .data$min_value),
            median_value = dplyr::if_else(is.infinite(.data$median_value), NA_real_, .data$median_value),
            max_value = dplyr::if_else(is.infinite(.data$max_value), NA_real_, .data$max_value)
        )
}

#' Summarize radiation detail fields relevant to reviewer comments
#'
#' @param data Analytic cohort data.
#' @return Tibble with field availability.
summarize_peer_review_radiation_availability <- function(data) {
    requested_fields <- c(
        "initial_gk", "initial_gk_date", "initial_plaque", "initial_plaque_date",
        "radionuclide", "plaque_size", "plaque_notch", "optic_nerve",
        "macula_distance", "fovea_distance", "optic_nerve_distance", "dose_to_macula",
        "dose_to_fovea", "dose_to_optic_nerve", "gk_margin_dose", "gk_isodose",
        "gk_shots", "gk_isocenters"
    )

    tibble::tibble(field = requested_fields) %>%
        dplyr::mutate(
            present = .data$field %in% names(data),
            non_missing_n = purrr::map_int(
                .data$field,
                ~ if (.x %in% names(data)) sum(!is.na(data[[.x]])) else 0L
            ),
            denominator_n = nrow(data),
            reviewer_use = dplyr::case_when(
                .data$field == "optic_nerve" ~ "Proximity/abutment eligibility and subgroup descriptor.",
                grepl("dose|isodose|shots|isocenters|distance|macula|fovea", .data$field) ~
                    "Reviewer-requested dosimetry/proximity detail; absent here if present=FALSE.",
                TRUE ~ "Treatment detail available for descriptive context if non-missing."
            )
        )
}

#' Check restricted-cohort size and optic nerve eligibility rules
#'
#' @param data Analytic cohort data.
#' @return Tibble with pass/fail eligibility checks.
summarize_restricted_cohort_eligibility <- function(data) {
    required_cols <- c("initial_tumor_diameter", "initial_tumor_height", "optic_nerve")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0L) {
        return(tibble::tibble(
            check = c("restricted_size_cutoffs", "restricted_optic_nerve_status"),
            status = "missing_required_columns",
            n_violations = NA_integer_,
            denominator_n = nrow(data),
            details = paste("Missing columns:", paste(missing_cols, collapse = ", "))
        ))
    }

    size_violations <- data %>%
        dplyr::filter(
            !is.na(.data$initial_tumor_diameter) & !is.na(.data$initial_tumor_height),
            .data$initial_tumor_diameter > 20 | .data$initial_tumor_height > 10
        )
    optic_nerve_positive <- data %>%
        dplyr::mutate(.optic_nerve_involved = normalize_optic_nerve_involvement(.data$optic_nerve)) %>%
        dplyr::filter(.data$.optic_nerve_involved %in% TRUE)

    tibble::tibble(
        check = c("restricted_size_cutoffs", "restricted_optic_nerve_status"),
        status = c(
            ifelse(nrow(size_violations) == 0L, "passed", "failed"),
            ifelse(nrow(optic_nerve_positive) == 0L, "passed", "failed")
        ),
        n_violations = c(nrow(size_violations), nrow(optic_nerve_positive)),
        denominator_n = nrow(data),
        details = c(
            sprintf(
                "%d rows exceeded diameter >20 mm or height >10 mm among rows with both fields present.",
                nrow(size_violations)
            ),
            sprintf(
                "%d rows had optic_nerve coded as positive/abutment/involvement.",
                nrow(optic_nerve_positive)
            )
        )
    )
}

#' Read available source workbook columns without loading full source data
#'
#' @param raw_data_dir Directory containing source workbooks.
#' @return Tibble with workbook, sheet, and column metadata.
read_peer_review_source_columns <- function(raw_data_dir = RAW_DATA_DIR) {
    source_files <- list.files(raw_data_dir, pattern = "\\.xlsx$", full.names = TRUE)
    if (length(source_files) == 0L) {
        return(tibble::tibble(
            source_file = character(),
            sheet = character(),
            column_name = character()
        ))
    }

    purrr::map_dfr(source_files, function(source_file) {
        sheets <- tryCatch(openxlsx::getSheetNames(source_file), error = function(e) character())
        purrr::map_dfr(sheets, function(sheet) {
            columns <- tryCatch(
                names(openxlsx::read.xlsx(source_file, sheet = sheet, rows = 1, colNames = TRUE)),
                error = function(e) character()
            )
            tibble::tibble(
                source_file = basename(source_file),
                sheet = sheet,
                column_name = columns
            )
        })
    })
}

#' Build peer-review follow-up and data availability audit tables
#'
#' @param data Analytic cohort data.
#' @param cohort_name Short cohort label for workbook output.
#' @param raw_data_dir Directory containing source workbooks.
#' @return Named list of audit tables.
build_peer_review_followup_audit <- function(data, cohort_name, raw_data_dir = RAW_DATA_DIR) {
    latest_va_months <- if (all(c("treatment_date", "last_followup") %in% names(data))) {
        as.numeric(difftime(data$last_followup, data$treatment_date, units = "days")) / 30.4375
    } else {
        rep(NA_real_, nrow(data))
    }

    data_profile <- tibble::tibble(
        cohort = cohort_name,
        n_patients = nrow(data),
        treatment_groups = paste(sort(unique(as.character(data$treatment_group))), collapse = ", "),
        latest_va_followup_12mo_n = sum(!is.na(latest_va_months) & latest_va_months >= 12),
        latest_va_followup_36mo_n = sum(!is.na(latest_va_months) & latest_va_months >= 36),
        latest_va_followup_60mo_n = sum(!is.na(latest_va_months) & latest_va_months >= 60)
    )

    list(
        data_profile = data_profile,
        followup_availability = summarize_peer_review_followup(data),
        radiation_availability = summarize_peer_review_radiation_availability(data),
        restricted_eligibility_check = summarize_restricted_cohort_eligibility(data),
        source_workbook_columns = read_peer_review_source_columns(raw_data_dir)
    )
}

#' Write a peer-review follow-up audit workbook
#'
#' @param audit Named list returned by `build_peer_review_followup_audit()`.
#' @param path Output workbook path.
#' @return Output path invisibly.
write_peer_review_followup_audit <- function(audit, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    openxlsx::write.xlsx(audit, file = path, overwrite = TRUE)
    invisible(path)
}

#' Run peer-review follow-up audits for full and restricted cohorts
#'
#' @return Character vector of created workbook paths.
run_peer_review_followup_audits <- function() {
    output_dir <- file.path(OUTPUT_DIR, "peer_review_revision_audits")
    cohort_paths <- c(
        full = file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"),
        restricted = file.path(PROCESSED_DATA_DIR, "uveal_melanoma_restricted_cohort.rds")
    )

    purrr::imap_chr(cohort_paths, function(cohort_path, cohort_name) {
        data <- readRDS(cohort_path)
        audit <- build_peer_review_followup_audit(data, cohort_name)
        output_path <- file.path(output_dir, paste0(cohort_name, "_followup_and_data_availability.xlsx"))
        write_peer_review_followup_audit(audit, output_path)
        output_path
    })
}

if (identical(environment(), globalenv()) && sys.nframe() == 0L) {
    if (!exists("OUTPUT_DIR", inherits = TRUE)) {
        source(here::here("scripts", "load_all.R"))
    }
    paths <- run_peer_review_followup_audits()
    message("Created peer-review follow-up audit workbooks:")
    message(paste(paths, collapse = "\n"))
}
