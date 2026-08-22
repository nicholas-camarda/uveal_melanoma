#!/usr/bin/env Rscript

AAO_GATE_VERSION <- 1L

stop_gate_usage <- function() {
    stop(
        paste(
            "Usage: Rscript scripts/tools/evaluate_objective4_aao_gate.R",
            "--contract CONTRACT --candidate-workbook WORKBOOK --report REPORT"
        ),
        call. = FALSE
    )
}

parse_aao_gate_cli <- function(args) {
    required <- c("contract", "candidate-workbook", "report")
    if (length(args) != 6L || length(args) %% 2L != 0L) {
        stop_gate_usage()
    }

    values <- list()
    for (index in seq(1L, length(args), by = 2L)) {
        raw_option <- args[[index]]
        option <- sub("^--", "", raw_option)
        if (!startsWith(raw_option, "--") || !option %in% required ||
            !nzchar(args[[index + 1L]]) || !is.null(values[[option]])) {
            stop_gate_usage()
        }
        values[[option]] <- args[[index + 1L]]
    }
    if (!setequal(names(values), required)) {
        stop_gate_usage()
    }
    values
}

is_scalar_number <- function(value) {
    is.numeric(value) && length(value) == 1L && is.finite(value)
}

validate_aao_contract <- function(contract) {
    if (!is.list(contract) || !identical(as.integer(contract$version), AAO_GATE_VERSION)) {
        stop("AAO gate contract version is unsupported", call. = FALSE)
    }
    accepted <- contract$accepted_abstract
    if (!is.list(accepted) || !identical(as.character(accepted$id), "30085896") ||
        !identical(as.integer(accepted$submitted_cohort_n), 260L) ||
        !identical(accepted$subgroup_counts_reported, FALSE)) {
        stop("Accepted-abstract identity contract is invalid", call. = FALSE)
    }

    auc <- unlist(accepted$auc, use.names = TRUE)
    required_auc <- c("direct_mfs", "direct_mss", "molecular_surrogate")
    if (!setequal(names(auc), required_auc) || any(!is.finite(as.numeric(auc)))) {
        stop("Accepted AUC contract is invalid", call. = FALSE)
    }
    rates <- accepted$observed_rates
    required_groups <- c("class_1", "not_tested", "failed_indeterminate", "class_2")
    if (!is.list(rates) || !setequal(names(rates), required_groups) ||
        any(!vapply(rates, function(group) {
            is.list(group) && setequal(names(group), c("mfs", "mss")) &&
                all(vapply(group, is_scalar_number, logical(1))) &&
                all(unlist(group, use.names = FALSE) >= 0 & unlist(group, use.names = FALSE) <= 1)
        }, logical(1)))) {
        stop("Accepted observed-rate contract is invalid", call. = FALSE)
    }

    conclusion_ids <- vapply(accepted$conclusions, `[[`, character(1), "id")
    if (!setequal(conclusion_ids, c(
        "moderate_direct_prognostic_stratification",
        "failure_to_recover_molecular_class",
        "no_gep_groups_non_homogeneous"
    ))) {
        stop("Accepted conclusion contract is invalid", call. = FALSE)
    }
    thresholds <- contract$review_thresholds
    if (!is_scalar_number(thresholds$absolute_auc_change) || thresholds$absolute_auc_change < 0 ||
        !is_scalar_number(thresholds$absolute_rate_change_percentage_points) ||
        thresholds$absolute_rate_change_percentage_points < 0) {
        stop("AAO review thresholds are invalid", call. = FALSE)
    }
    candidate <- contract$candidate_workbook
    if (!is.list(candidate) || !length(candidate$required_sheets) ||
        !is.list(candidate$models) || !is.list(candidate$group_labels) ||
        !is.list(candidate$required_orderings) || !is.list(contract$conclusion_checks)) {
        stop("Candidate workbook contract is invalid", call. = FALSE)
    }
    contract
}

load_aao_contract <- function(path) {
    if (!file.exists(path)) {
        stop("AAO gate contract is missing", call. = FALSE)
    }
    validate_aao_contract(yaml::read_yaml(path))
}

read_required_aao_sheet <- function(path, sheet, required_columns) {
    data <- openxlsx::read.xlsx(path, sheet = sheet, check.names = FALSE)
    missing_columns <- setdiff(required_columns, names(data))
    if (length(missing_columns)) {
        stop(sprintf("Required columns are missing from %s", sheet), call. = FALSE)
    }
    data
}

read_aao_candidate_workbook <- function(path, contract) {
    if (!file.exists(path)) {
        stop("Candidate workbook is missing", call. = FALSE)
    }
    sheets <- openxlsx::getSheetNames(path)
    if (!all(contract$candidate_workbook$required_sheets %in% sheets)) {
        stop("Candidate workbook is missing a required labeled sheet", call. = FALSE)
    }
    list(
        performance = read_required_aao_sheet(
            path,
            "Model_Performance",
            c("model", "performance_scope", "model_method", "evaluation_method", "metric_status", "cv_auc")
        ),
        risk = read_required_aao_sheet(
            path,
            "Risk_Ladder_5yr",
            c(
                "group", "n", "observed_5yr_mfs_event_rate", "mfs_observed_method",
                "observed_5yr_mss_event_rate", "mss_observed_method"
            )
        ),
        conclusions = read_required_aao_sheet(
            path,
            "Start_Here",
            c("label", "value")
        )
    )
}

new_gate_reason <- function(id, status, message) {
    list(id = id, status = status, message = message)
}

exceeds_aao_threshold <- function(value, threshold) {
    value > threshold + sqrt(.Machine$double.eps)
}

extract_model_metric <- function(performance, model_id, model_spec) {
    model_rows <- performance[performance$model == model_spec$label, , drop = FALSE]
    if (!nrow(model_rows)) {
        return(list(
            reason = new_gate_reason(
                paste0("required_method_", model_id), "fail", "Required model row is missing"
            )
        ))
    }

    required_scopes <- unlist(model_spec$required_scopes, use.names = FALSE)
    observed_scopes <- unique(stats::na.omit(as.character(model_rows$performance_scope)))
    if (length(required_scopes) && !all(required_scopes %in% observed_scopes)) {
        return(list(
            reason = new_gate_reason(
                paste0("required_scope_", model_id), "fail", "Required performance scope is missing"
            )
        ))
    }
    if (anyNA(model_rows$model_method) || anyNA(model_rows$evaluation_method) ||
        any(model_rows$model_method != model_spec$model_method) ||
        any(model_rows$evaluation_method != model_spec$evaluation_method)) {
        return(list(
            reason = new_gate_reason(
                paste0("required_method_", model_id), "fail", "Declared model or evaluation method is not the required method"
            )
        ))
    }
    if (isTRUE(model_spec$metric_status_required) &&
        (any(is.na(model_rows$metric_status)) || any(model_rows$metric_status != "ok"))) {
        return(list(
            reason = new_gate_reason(
                paste0("unsupported_metric_", model_id), "fail", "Required metric is unsupported"
            )
        ))
    }

    comparison_scope <- model_spec$comparison_scope
    comparison_rows <- if (is.null(comparison_scope)) {
        model_rows
    } else {
        model_rows[model_rows$performance_scope == comparison_scope & !is.na(model_rows$performance_scope), , drop = FALSE]
    }
    if (nrow(comparison_rows) != 1L || !is_scalar_number(comparison_rows$cv_auc[[1L]]) ||
        comparison_rows$cv_auc[[1L]] < 0 || comparison_rows$cv_auc[[1L]] > 1) {
        return(list(
            reason = new_gate_reason(
                paste0("required_value_", model_id), "fail", "Required AUC is missing, duplicated, or invalid"
            )
        ))
    }
    list(value = as.numeric(comparison_rows$cv_auc[[1L]]))
}

extract_candidate_rates <- function(risk, contract) {
    group_labels <- contract$candidate_workbook$group_labels
    result <- list()
    for (group_id in names(group_labels)) {
        rows <- risk[risk$group == group_labels[[group_id]], , drop = FALSE]
        if (nrow(rows) != 1L) {
            stop("Required risk-ladder group is missing or duplicated", call. = FALSE)
        }
        if (!identical(as.character(rows$mfs_observed_method[[1L]]), contract$candidate_workbook$observed_rate_methods$mfs) ||
            !identical(as.character(rows$mss_observed_method[[1L]]), contract$candidate_workbook$observed_rate_methods$mss)) {
            stop("Observed-risk method identifier is invalid", call. = FALSE)
        }
        values <- c(
            mfs = as.numeric(rows$observed_5yr_mfs_event_rate[[1L]]),
            mss = as.numeric(rows$observed_5yr_mss_event_rate[[1L]])
        )
        if (any(!is.finite(values)) || any(values < 0 | values > 1)) {
            stop("Required observed risk is missing or invalid", call. = FALSE)
        }
        result[[group_id]] <- as.list(values)
    }
    group_n <- as.numeric(risk$n[match(unlist(group_labels, use.names = FALSE), risk$group)])
    if (any(!is.finite(group_n)) || any(group_n < 0) || any(group_n != floor(group_n))) {
        stop("Candidate cohort size is invalid", call. = FALSE)
    }
    cohort_n <- sum(group_n)
    list(rates = result, cohort_n = as.integer(cohort_n))
}

check_candidate_conclusions <- function(conclusions, contract) {
    reasons <- list()
    if (anyDuplicated(conclusions$label)) {
        return(list(new_gate_reason("candidate_conclusions", "fail", "Conclusion labels are duplicated")))
    }
    for (conclusion_id in names(contract$conclusion_checks)) {
        check <- contract$conclusion_checks[[conclusion_id]]
        labels <- unlist(check$labels, use.names = FALSE)
        rows <- conclusions[match(labels, conclusions$label), , drop = FALSE]
        if (nrow(rows) != length(labels) || any(is.na(rows$label)) || any(is.na(rows$value))) {
            reasons[[length(reasons) + 1L]] <- new_gate_reason(
                paste0("conclusion_reversal_", conclusion_id), "fail", "Required conclusion statement is missing"
            )
            next
        }
        combined <- tolower(paste(rows$value, collapse = " "))
        phrases <- tolower(unlist(check$required_phrases, use.names = FALSE))
        if (!all(vapply(phrases, grepl, logical(1), x = combined, fixed = TRUE))) {
            reasons[[length(reasons) + 1L]] <- new_gate_reason(
                paste0("conclusion_reversal_", conclusion_id), "fail", "Accepted conclusion category is not supported by candidate wording"
            )
        }
    }
    reasons
}

classify_gate_status <- function(reasons) {
    statuses <- vapply(reasons, `[[`, character(1), "status")
    if ("fail" %in% statuses) {
        "fail"
    } else if ("review" %in% statuses) {
        "review"
    } else {
        "pass"
    }
}

write_aao_gate_report <- function(path, report) {
    report_dir <- dirname(path)
    if (!dir.exists(report_dir)) {
        dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
    }
    jsonlite::write_json(report, path, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 10)
    invisible(path)
}

evaluate_objective4_aao_gate <- function(contract_path, candidate_workbook, report_path) {
    contract <- tryCatch(load_aao_contract(contract_path), error = identity)
    if (inherits(contract, "error")) {
        report <- list(
            gate_version = AAO_GATE_VERSION,
            status = "fail",
            accepted_abstract_id = "30085896",
            reasons = list(new_gate_reason("contract", "fail", "Accepted-abstract contract is invalid"))
        )
        write_aao_gate_report(report_path, report)
        return("fail")
    }

    candidate <- tryCatch(read_aao_candidate_workbook(candidate_workbook, contract), error = identity)
    if (inherits(candidate, "error")) {
        report <- list(
            gate_version = AAO_GATE_VERSION,
            status = "fail",
            accepted_abstract_id = contract$accepted_abstract$id,
            reasons = list(new_gate_reason("candidate_workbook", "fail", "Candidate workbook contract could not be satisfied"))
        )
        write_aao_gate_report(report_path, report)
        return("fail")
    }

    reasons <- list()
    auc_comparisons <- list()
    for (model_id in names(contract$candidate_workbook$models)) {
        extracted <- extract_model_metric(
            candidate$performance,
            model_id,
            contract$candidate_workbook$models[[model_id]]
        )
        if (!is.null(extracted$reason)) {
            reasons[[length(reasons) + 1L]] <- extracted$reason
            next
        }
        accepted_value <- as.numeric(contract$accepted_abstract$auc[[model_id]])
        delta <- extracted$value - accepted_value
        auc_comparisons[[model_id]] <- list(
            accepted = accepted_value,
            candidate = extracted$value,
            absolute_delta = abs(delta)
        )
        if (exceeds_aao_threshold(abs(delta), contract$review_thresholds$absolute_auc_change)) {
            reasons[[length(reasons) + 1L]] <- new_gate_reason(
                paste0("auc_delta_", model_id), "review", "Absolute AUC change exceeds the accepted-abstract review threshold"
            )
        }
    }

    extracted_rates <- tryCatch(extract_candidate_rates(candidate$risk, contract), error = identity)
    rate_comparisons <- list()
    candidate_cohort_n <- NULL
    if (inherits(extracted_rates, "error")) {
        reasons[[length(reasons) + 1L]] <- new_gate_reason(
            "observed_rates", "fail", "Required observed-rate value or method is invalid"
        )
    } else {
        candidate_cohort_n <- extracted_rates$cohort_n
        if (!identical(candidate_cohort_n, as.integer(contract$accepted_abstract$submitted_cohort_n))) {
            reasons[[length(reasons) + 1L]] <- new_gate_reason(
                "cohort_size_change", "review", "Candidate cohort size differs from the accepted abstract"
            )
        }
        for (group_id in names(extracted_rates$rates)) {
            rate_comparisons[[group_id]] <- list()
            for (endpoint in c("mfs", "mss")) {
                accepted_value <- as.numeric(contract$accepted_abstract$observed_rates[[group_id]][[endpoint]])
                candidate_value <- as.numeric(extracted_rates$rates[[group_id]][[endpoint]])
                delta_pp <- 100 * (candidate_value - accepted_value)
                rate_comparisons[[group_id]][[endpoint]] <- list(
                    accepted = accepted_value,
                    candidate = candidate_value,
                    absolute_delta_percentage_points = abs(delta_pp)
                )
                if (exceeds_aao_threshold(
                    abs(delta_pp),
                    contract$review_thresholds$absolute_rate_change_percentage_points
                )) {
                    reasons[[length(reasons) + 1L]] <- new_gate_reason(
                        paste0("rate_delta_", group_id, "_", endpoint),
                        "review",
                        "Absolute observed-risk change exceeds the accepted-abstract review threshold"
                    )
                }
            }
        }

        for (ordering in contract$candidate_workbook$required_orderings) {
            higher <- extracted_rates$rates[[ordering$higher]][[ordering$endpoint]]
            lower <- extracted_rates$rates[[ordering$lower]][[ordering$endpoint]]
            if (!isTRUE(higher > lower)) {
                reasons[[length(reasons) + 1L]] <- new_gate_reason(
                    paste0("ordering_reversal_", ordering$id),
                    "fail",
                    "A required accepted-abstract risk ordering is reversed or tied"
                )
            }
        }
    }

    reasons <- c(reasons, check_candidate_conclusions(candidate$conclusions, contract))
    status <- classify_gate_status(reasons)
    report <- list(
        gate_version = AAO_GATE_VERSION,
        status = status,
        accepted_abstract_id = contract$accepted_abstract$id,
        accepted_submitted_cohort_n = as.integer(contract$accepted_abstract$submitted_cohort_n),
        candidate_cohort_n = candidate_cohort_n,
        subgroup_counts_reported_in_accepted_abstract = FALSE,
        comparisons = list(auc = auc_comparisons, observed_rates = rate_comparisons),
        reasons = reasons
    )
    write_aao_gate_report(report_path, report)
    status
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
    options <- tryCatch(parse_aao_gate_cli(args), error = identity)
    if (inherits(options, "error")) {
        return(2L)
    }
    status <- evaluate_objective4_aao_gate(
        options$contract,
        options[["candidate-workbook"]],
        options$report
    )
    if (identical(status, "pass")) 0L else 1L
}

if (sys.nframe() == 0L) {
    required_packages <- c("jsonlite", "openxlsx", "yaml")
    missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_packages)) {
        stop("Required AAO gate packages are unavailable", call. = FALSE)
    }
    quit(save = "no", status = main())
}
