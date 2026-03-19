# Effect Summary Audit Utilities

#' Normalize effect-summary text fields for audit checks
#'
#' @param value Character vector or scalar to normalize.
#'
#' @return Character vector with trimmed values and `NA` replaced by `""`.
normalize_effect_summary_text <- function(value) {
    value <- as.character(value %||% "")
    value[is.na(value)] <- ""
    trimws(value)
}

#' Parse a normalized covariate list from effect-summary metadata
#'
#' @param covariates_used_norm Scalar character string from `covariates_used`.
#'
#' @return Character vector of listed covariates with empty sentinels removed.
parse_listed_covariates <- function(covariates_used_norm) {
    listed_covariates <- unlist(strsplit(covariates_used_norm %||% "", ",", fixed = TRUE))
    listed_covariates <- trimws(as.character(listed_covariates))
    listed_covariates <- listed_covariates[!listed_covariates %in% c("", "None", "NA")]
    unique(listed_covariates)
}

#' Determine whether workbook-specific confounder expectations should apply
#'
#' @param effect_summary_path Character path to an effect-summary workbook.
#'
#' @return Logical scalar indicating whether expected confounders should be
#'   enforced for this workbook.
workbook_requires_expected_confounders <- function(effect_summary_path) {
    grepl(
        "/04_GEP_Validation/a_metastasis_free_survival/",
        effect_summary_path,
        fixed = TRUE
    )
}

#' Extract modeled sample-size metadata from a paired diagnostics workbook
#'
#' @param effect_summary_path Character path to an effect-summary workbook.
#'
#' @return A tibble with `analysis_label`, `diagnostics_n_total`, and
#'   `diagnostics_n_events`, or an empty tibble when no paired diagnostics
#'   workbook is available.
extract_effect_summary_diagnostics_metadata <- function(effect_summary_path) {
    diagnostics_path <- sub("_effect_summary\\.xlsx$", "_cox_diagnostics.xlsx", effect_summary_path)
    if (identical(diagnostics_path, effect_summary_path) || !file.exists(diagnostics_path)) {
        return(tibble::tibble())
    }

    model_summary <- tryCatch(
        readxl::read_xlsx(diagnostics_path, sheet = "Model_summary"),
        error = function(e) NULL
    )
    if (is.null(model_summary) || !nrow(model_summary)) {
        return(tibble::tibble())
    }

    required_cols <- c("analysis_type", "n_total", "n_events")
    if (!all(required_cols %in% names(model_summary))) {
        return(tibble::tibble())
    }

    model_summary %>%
        dplyr::transmute(
            analysis_label = gsub(
                "_cox$",
                "",
                gsub("^unified_", "", .data$analysis_type)
            ),
            diagnostics_n_total = as.numeric(.data$n_total),
            diagnostics_n_events = as.numeric(.data$n_events)
        )
}

#' Audit one effect-summary workbook for silent metadata and adjustment issues
#'
#' @param effect_summary_path Character path to an effect-summary workbook.
#' @param expected_confounders Optional character vector of expected covariates
#'   for adjusted models.
#'
#' @return A tibble of audit findings with one row per detected issue.
audit_effect_summary_workbook <- function(effect_summary_path,
                                          expected_confounders = NULL) {
    if (!file.exists(effect_summary_path)) {
        stop(sprintf("Effect summary workbook not found: %s", effect_summary_path))
    }

    expected_confounders <- unique(stats::na.omit(as.character(expected_confounders)))
    expected_confounders <- expected_confounders[nzchar(expected_confounders)]

    effect_summary <- readxl::read_xlsx(effect_summary_path)
    if (!nrow(effect_summary)) {
        return(tibble::tibble())
    }

    diagnostics_metadata <- extract_effect_summary_diagnostics_metadata(effect_summary_path)
    if (nrow(diagnostics_metadata) > 0) {
        effect_summary <- effect_summary %>%
            dplyr::left_join(diagnostics_metadata, by = "analysis_label")
    } else {
        effect_summary <- effect_summary %>%
            dplyr::mutate(
                diagnostics_n_total = NA_real_,
                diagnostics_n_events = NA_real_
            )
    }

    effect_summary <- effect_summary %>%
        dplyr::mutate(
            workbook_path = effect_summary_path,
            covariates_used_norm = normalize_effect_summary_text(.data$covariates_used),
            model_formula_norm = normalize_effect_summary_text(.data$model_formula),
            is_adjusted = grepl("^Adjusted", .data$model_label),
            missing_covariates = .data$covariates_used_norm %in% c("", "None", "NA")
        )

    findings <- list()

    cox_n_mismatch <- effect_summary %>%
        dplyr::filter(
            .data$effect_measure == "HR",
            is.finite(.data$diagnostics_n_total),
            is.finite(.data$diagnostics_n_events),
            .data$diagnostics_n_total > .data$diagnostics_n_events,
            .data$n_patients == .data$n_events,
            .data$n_patients != .data$diagnostics_n_total
        ) %>%
        dplyr::transmute(
            workbook_path = .data$workbook_path,
            analysis_label = .data$analysis_label,
            model_label = .data$model_label,
            term = .data$term,
            issue_type = "cox_n_patients_misreported",
            expected_interpretation = sprintf(
                "Use modeled sample size n=%s and event count=%s from diagnostics, not n_patients=%s.",
                .data$diagnostics_n_total,
                .data$diagnostics_n_events,
                .data$n_patients
            )
        )
    findings[[length(findings) + 1L]] <- cox_n_mismatch

    adjusted_missing_covariates <- effect_summary %>%
        dplyr::filter(.data$is_adjusted, .data$missing_covariates) %>%
        dplyr::transmute(
            workbook_path = .data$workbook_path,
            analysis_label = .data$analysis_label,
            model_label = .data$model_label,
            term = .data$term,
            issue_type = "adjusted_model_missing_covariates",
            expected_interpretation = "Adjusted model row should list the covariates used and include them in the fitted formula."
        )
    findings[[length(findings) + 1L]] <- adjusted_missing_covariates

    adjusted_formula_missing_listed_covariates <- effect_summary %>%
        dplyr::filter(.data$is_adjusted, !.data$missing_covariates) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            listed_covariates = list(parse_listed_covariates(.data$covariates_used_norm)),
            missing_listed_covariates = list(.data$listed_covariates[!vapply(
                .data$listed_covariates,
                function(covariate) {
                    grepl(covariate, .data$model_formula_norm, fixed = TRUE)
                },
                logical(1)
            )])
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(lengths(.data$missing_listed_covariates) > 0) %>%
        dplyr::transmute(
            workbook_path = .data$workbook_path,
            analysis_label = .data$analysis_label,
            model_label = .data$model_label,
            term = .data$term,
            issue_type = "adjusted_model_formula_missing_listed_covariates",
            expected_interpretation = paste(
                "Adjusted model formula is missing covariates listed in metadata:",
                vapply(.data$missing_listed_covariates, paste, character(1), collapse = ", ")
            )
        )
    findings[[length(findings) + 1L]] <- adjusted_formula_missing_listed_covariates

    if (length(expected_confounders) > 0 &&
        workbook_requires_expected_confounders(effect_summary_path)) {
        adjusted_missing_expected <- effect_summary %>%
            dplyr::filter(.data$is_adjusted) %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
                missing_expected_confounders = list(expected_confounders[!vapply(
                    expected_confounders,
                    function(confounder) {
                        grepl(confounder, .data$covariates_used_norm, fixed = TRUE) ||
                            grepl(confounder, .data$model_formula_norm, fixed = TRUE)
                    },
                    logical(1)
                )])
            ) %>%
            dplyr::ungroup() %>%
            dplyr::filter(lengths(.data$missing_expected_confounders) > 0) %>%
            dplyr::transmute(
                workbook_path = .data$workbook_path,
                analysis_label = .data$analysis_label,
                model_label = .data$model_label,
                term = .data$term,
                issue_type = "adjusted_model_formula_missing_expected_confounders",
                expected_interpretation = paste(
                    "Adjusted model should include expected confounders:",
                    vapply(.data$missing_expected_confounders, paste, character(1), collapse = ", ")
                )
            )
        findings[[length(findings) + 1L]] <- adjusted_missing_expected
    }

    adjusted_matches_unadjusted <- effect_summary %>%
        dplyr::filter(.data$is_adjusted) %>%
        dplyr::select(
            "analysis_label",
            "term",
            adjusted_model_label = "model_label",
            adjusted_covariates = "covariates_used_norm",
            adjusted_formula = "model_formula_norm",
            adjusted_estimate = "estimate",
            adjusted_ci_lower = "ci_lower",
            adjusted_ci_upper = "ci_upper",
            adjusted_p_value = "p_value",
            workbook_path = "workbook_path"
        ) %>%
        dplyr::inner_join(
            effect_summary %>%
                dplyr::filter(.data$model_label == "Unadjusted (Cox data)") %>%
                dplyr::select(
                    "analysis_label",
                    "term",
                    unadjusted_model_label = "model_label",
                    unadjusted_formula = "model_formula_norm",
                    unadjusted_estimate = "estimate",
                    unadjusted_ci_lower = "ci_lower",
                    unadjusted_ci_upper = "ci_upper",
                    unadjusted_p_value = "p_value"
                ),
            by = c("analysis_label", "term")
        ) %>%
        dplyr::filter(
            .data$adjusted_covariates %in% c("", "None", "NA"),
            .data$adjusted_formula == .data$unadjusted_formula |
                (.data$adjusted_estimate == .data$unadjusted_estimate &
                    .data$adjusted_ci_lower == .data$unadjusted_ci_lower &
                    .data$adjusted_ci_upper == .data$unadjusted_ci_upper &
                    dplyr::coalesce(.data$adjusted_p_value, NA_real_) == dplyr::coalesce(.data$unadjusted_p_value, NA_real_))
        ) %>%
        dplyr::transmute(
            workbook_path = .data$workbook_path,
            analysis_label = .data$analysis_label,
            model_label = .data$adjusted_model_label,
            term = .data$term,
            issue_type = "adjusted_matches_unadjusted_without_covariates",
            expected_interpretation = "Adjusted row matches the unadjusted Cox row because no covariates were actually included."
        )
    findings[[length(findings) + 1L]] <- adjusted_matches_unadjusted

    findings <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, findings)
    if (!length(findings)) {
        return(tibble::tibble())
    }

    dplyr::bind_rows(findings) %>%
        dplyr::distinct()
}

#' Audit effect-summary workbooks under a directory and optionally write a CSV
#'
#' @param base_dir Character path to the directory containing effect-summary
#'   workbooks.
#' @param expected_confounders Optional character vector of expected covariates
#'   for adjusted models.
#' @param output_path Optional CSV path for a machine-readable audit artifact.
#'
#' @return A tibble of audit findings across all discovered workbooks.
audit_effect_summary_directory <- function(base_dir,
                                           expected_confounders = NULL,
                                           output_path = NULL) {
    workbook_paths <- list.files(
        path = base_dir,
        pattern = "_effect_summary\\.xlsx$",
        recursive = TRUE,
        full.names = TRUE
    )
    workbook_paths <- workbook_paths[!grepl("/~\\$", workbook_paths)]

    findings <- lapply(workbook_paths, function(workbook_path) {
        audit_effect_summary_workbook(
            effect_summary_path = workbook_path,
            expected_confounders = expected_confounders
        )
    })
    findings <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, findings)

    consolidated_findings <- if (length(findings) == 0) {
        tibble::tibble()
    } else {
        dplyr::bind_rows(findings) %>%
            dplyr::arrange(.data$workbook_path, .data$analysis_label, .data$model_label, .data$term, .data$issue_type)
    }

    if (!is.null(output_path)) {
        dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
        utils::write.csv(consolidated_findings, output_path, row.names = FALSE)
        logger::log_info(sprintf("Effect-summary audit findings written to %s", output_path))
    }

    consolidated_findings
}
