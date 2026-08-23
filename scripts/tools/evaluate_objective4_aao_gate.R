#!/usr/bin/env Rscript

AAO_GATE_VERSION <- 1L
AAO_ACCEPTED_CONTRACT_FINGERPRINT <- "091c01f5569d0f88b1b638d5c177e49ebf56e90b4d89ab8e35c9e12c82a5e3fc"

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

is_scalar_character <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

is_nonempty_character <- function(value) {
    is.character(value) && length(value) > 0L && !anyNA(value) && all(nzchar(value))
}

has_exact_names <- function(value, expected) {
    is.list(value) && identical(names(value), expected)
}

calculate_aao_contract_fingerprint <- function(contract) {
    payload <- contract[setdiff(names(contract), "immutable_fingerprint_sha256")]
    canonical_json <- jsonlite::toJSON(
        payload,
        auto_unbox = TRUE,
        null = "null",
        digits = NA,
        pretty = FALSE
    )
    digest::digest(canonical_json, algo = "sha256", serialize = FALSE)
}

validate_aao_contract <- function(contract) {
    expected_top_names <- c(
        "version", "immutable_fingerprint_sha256", "accepted_abstract",
        "review_thresholds", "candidate_workbook", "conclusion_rules"
    )
    if (!has_exact_names(contract, expected_top_names) ||
        !identical(contract$version, AAO_GATE_VERSION) ||
        !is_scalar_character(contract$immutable_fingerprint_sha256) ||
        !identical(contract$immutable_fingerprint_sha256, AAO_ACCEPTED_CONTRACT_FINGERPRINT) ||
        !grepl("^[0-9a-f]{64}$", contract$immutable_fingerprint_sha256)) {
        stop("AAO gate contract version is unsupported", call. = FALSE)
    }
    accepted <- contract$accepted_abstract
    if (!has_exact_names(accepted, c(
        "id", "submitted_cohort_n", "subgroup_counts_reported", "auc",
        "observed_rates", "conclusions"
    )) || !identical(accepted$id, "30085896") ||
        !identical(accepted$submitted_cohort_n, 260L) ||
        !identical(accepted$subgroup_counts_reported, FALSE)) {
        stop("Accepted-abstract identity contract is invalid", call. = FALSE)
    }

    required_auc <- c("direct_mfs", "direct_mss", "molecular_surrogate")
    if (!has_exact_names(accepted$auc, required_auc) ||
        !all(vapply(accepted$auc, is_scalar_number, logical(1))) ||
        any(unlist(accepted$auc, use.names = FALSE) < 0 | unlist(accepted$auc, use.names = FALSE) > 1)) {
        stop("Accepted AUC contract is invalid", call. = FALSE)
    }
    rates <- accepted$observed_rates
    required_groups <- c("class_1", "not_tested", "failed_indeterminate", "class_2")
    if (!has_exact_names(rates, required_groups) ||
        any(!vapply(rates, function(group) {
            has_exact_names(group, c("mfs", "mss")) &&
                all(vapply(group, is_scalar_number, logical(1))) &&
                all(unlist(group, use.names = FALSE) >= 0 & unlist(group, use.names = FALSE) <= 1)
        }, logical(1)))) {
        stop("Accepted observed-rate contract is invalid", call. = FALSE)
    }

    expected_conclusions <- c(
        "moderate_direct_prognostic_stratification",
        "failure_to_recover_molecular_class",
        "no_gep_groups_non_homogeneous"
    )
    if (!is.list(accepted$conclusions) || length(accepted$conclusions) != 3L ||
        !all(vapply(accepted$conclusions, function(conclusion) {
            has_exact_names(conclusion, c("id", "category", "workbook_labels")) &&
                is_scalar_character(conclusion$id) &&
                is_scalar_character(conclusion$category) &&
                is_nonempty_character(conclusion$workbook_labels)
        }, logical(1)))) {
        stop("Accepted conclusion contract is invalid", call. = FALSE)
    }
    conclusion_ids <- vapply(accepted$conclusions, `[[`, character(1), "id")
    conclusion_categories <- vapply(accepted$conclusions, `[[`, character(1), "category")
    if (!identical(conclusion_ids, expected_conclusions) ||
        !identical(conclusion_categories, expected_conclusions) ||
        !identical(
            lapply(accepted$conclusions, `[[`, "workbook_labels"),
            list(c("takeaway_1"), c("takeaway_2", "takeaway_3"), c("takeaway_4"))
        )) {
        stop("Accepted conclusion mapping is invalid", call. = FALSE)
    }

    thresholds <- contract$review_thresholds
    if (!has_exact_names(thresholds, c(
        "absolute_auc_change", "absolute_rate_change_percentage_points"
    )) || !is_scalar_number(thresholds$absolute_auc_change) || thresholds$absolute_auc_change < 0 ||
        !is_scalar_number(thresholds$absolute_rate_change_percentage_points) ||
        thresholds$absolute_rate_change_percentage_points < 0) {
        stop("AAO review thresholds are invalid", call. = FALSE)
    }

    candidate <- contract$candidate_workbook
    expected_models <- c("molecular_surrogate", "direct_mfs", "direct_mss")
    if (!has_exact_names(candidate, c(
        "required_sheets", "models", "observed_rate_methods", "group_labels",
        "structured_conclusions", "required_orderings"
    )) || !identical(
        candidate$required_sheets,
        c("Model_Performance", "Risk_Ladder_5yr", "Start_Here")
    ) || !has_exact_names(candidate$models, expected_models)) {
        stop("Candidate workbook contract is invalid", call. = FALSE)
    }

    for (model_id in names(candidate$models)) {
        model <- candidate$models[[model_id]]
        if (!has_exact_names(model, c(
            "label", "comparison_scope", "required_scopes", "model_method",
            "evaluation_method", "metric_status_required"
        )) || !is_scalar_character(model$label) ||
            !(is.null(model$comparison_scope) || is_scalar_character(model$comparison_scope)) ||
            !(is.character(model$required_scopes) || length(model$required_scopes) == 0L) ||
            anyNA(model$required_scopes) ||
            !is_scalar_character(model$model_method) ||
            !is_scalar_character(model$evaluation_method) ||
            !is.logical(model$metric_status_required) || length(model$metric_status_required) != 1L ||
            is.na(model$metric_status_required)) {
            stop("Candidate model mapping is invalid", call. = FALSE)
        }
    }
    if (length(candidate$models$molecular_surrogate$required_scopes) != 0L ||
        !is.null(candidate$models$molecular_surrogate$comparison_scope) ||
        !identical(candidate$models$direct_mfs$required_scopes, c("Overall", "No GEP")) ||
        !identical(candidate$models$direct_mss$required_scopes, c("Overall", "No GEP")) ||
        !identical(candidate$models$direct_mfs$comparison_scope, "Overall") ||
        !identical(candidate$models$direct_mss$comparison_scope, "Overall")) {
        stop("Candidate performance-scope mapping is invalid", call. = FALSE)
    }
    if (!has_exact_names(candidate$observed_rate_methods, c("mfs", "mss")) ||
        !all(vapply(candidate$observed_rate_methods, is_scalar_character, logical(1))) ||
        !has_exact_names(candidate$group_labels, required_groups) ||
        !all(vapply(candidate$group_labels, is_scalar_character, logical(1))) ||
        anyDuplicated(unlist(candidate$group_labels, use.names = FALSE))) {
        stop("Candidate observed-rate mapping is invalid", call. = FALSE)
    }

    structured <- candidate$structured_conclusions
    if (!has_exact_names(structured, c(
        "sheet", "id_column", "category_column", "optional", "categories"
    )) || !is_scalar_character(structured$sheet) ||
        !is_scalar_character(structured$id_column) ||
        !is_scalar_character(structured$category_column) ||
        !is.logical(structured$optional) || length(structured$optional) != 1L ||
        is.na(structured$optional) ||
        !has_exact_names(structured$categories, expected_conclusions) ||
        !all(vapply(structured$categories, function(category) {
            has_exact_names(category, c("expected", "contradictory")) &&
                is_scalar_character(category$expected) &&
                is_scalar_character(category$contradictory) &&
                !identical(category$expected, category$contradictory)
        }, logical(1)))) {
        stop("Structured conclusion mapping is invalid", call. = FALSE)
    }
    if (!identical(structured$sheet, "Start_Here") ||
        !identical(structured$id_column, "conclusion_id") ||
        !identical(structured$category_column, "conclusion_category") ||
        !identical(structured$optional, TRUE) ||
        !identical(
            unname(vapply(structured$categories, `[[`, character(1), "expected")),
            expected_conclusions
        )) {
        stop("Structured conclusion contract is invalid", call. = FALSE)
    }

    orderings <- candidate$required_orderings
    if (!is.list(orderings) || !length(orderings) ||
        !all(vapply(orderings, function(ordering) {
            has_exact_names(ordering, c("id", "endpoint", "higher", "lower")) &&
                all(vapply(ordering, is_scalar_character, logical(1))) &&
                ordering$endpoint %in% c("mfs", "mss") &&
                ordering$higher %in% required_groups && ordering$lower %in% required_groups &&
                !identical(ordering$higher, ordering$lower)
        }, logical(1)))) {
        stop("Candidate ordering contract is invalid", call. = FALSE)
    }
    ordering_ids <- vapply(orderings, `[[`, character(1), "id")
    if (anyDuplicated(ordering_ids)) {
        stop("Candidate ordering IDs are duplicated", call. = FALSE)
    }

    rules <- contract$conclusion_rules
    if (!has_exact_names(rules, expected_conclusions)) {
        stop("Conclusion-rule contract is invalid", call. = FALSE)
    }
    for (conclusion_id in names(rules)) {
        rule <- rules[[conclusion_id]]
        if (!has_exact_names(rule, c("result", "prose")) || !is.list(rule$result) ||
            !has_exact_names(rule$prose, c("labels", "required_phrases", "forbidden_phrases")) ||
            !is_nonempty_character(rule$prose$labels) ||
            !is_nonempty_character(rule$prose$required_phrases) ||
            !is_nonempty_character(rule$prose$forbidden_phrases)) {
            stop("Conclusion-rule structure is invalid", call. = FALSE)
        }
        accepted_labels <- accepted$conclusions[[match(conclusion_id, conclusion_ids)]]$workbook_labels
        if (!identical(rule$prose$labels, accepted_labels)) {
            stop("Conclusion-rule workbook labels are invalid", call. = FALSE)
        }
    }
    direct_rule <- rules$moderate_direct_prognostic_stratification$result
    surrogate_rule <- rules$failure_to_recover_molecular_class$result
    group_rule <- rules$no_gep_groups_non_homogeneous$result
    if (!has_exact_names(direct_rule, c("type", "models", "threshold")) ||
        !identical(direct_rule$type, "minimum_auc") ||
        !identical(direct_rule$models, c("direct_mfs", "direct_mss")) ||
        !is_scalar_number(direct_rule$threshold) ||
        direct_rule$threshold < 0 || direct_rule$threshold > 1 ||
        !has_exact_names(surrogate_rule, c("type")) ||
        !identical(surrogate_rule$type, "structured_category") ||
        !has_exact_names(group_rule, c("type", "ordering_ids")) ||
        !identical(group_rule$type, "required_orderings") ||
        !is_nonempty_character(group_rule$ordering_ids) ||
        !all(group_rule$ordering_ids %in% ordering_ids)) {
        stop("Conclusion result rule is invalid", call. = FALSE)
    }

    if (!identical(
        calculate_aao_contract_fingerprint(contract),
        contract$immutable_fingerprint_sha256
    )) {
        stop("AAO gate contract fingerprint is invalid", call. = FALSE)
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
    model_rows <- performance[
        !is.na(performance$model) & performance$model == model_spec$label,
        ,
        drop = FALSE
    ]
    if (!nrow(model_rows)) {
        return(list(
            reason = new_gate_reason(
                paste0("required_method_", model_id), "fail", "Required model row is missing"
            )
        ))
    }

    required_scopes <- unlist(model_spec$required_scopes, use.names = FALSE)
    observed_scopes <- as.character(model_rows$performance_scope)
    scope_contract_satisfied <- if (!length(required_scopes)) {
        nrow(model_rows) == 1L && all(is.na(observed_scopes) | !nzchar(observed_scopes))
    } else {
        !anyNA(observed_scopes) && all(nzchar(observed_scopes)) &&
            nrow(model_rows) == length(required_scopes) &&
            setequal(observed_scopes, required_scopes) &&
            all(table(factor(observed_scopes, levels = required_scopes)) == 1L)
    }
    if (!scope_contract_satisfied) {
        return(list(
            reason = new_gate_reason(
                paste0("required_scope_", model_id),
                "fail",
                "Required performance scopes must be exact and unique"
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

check_candidate_prose_consistency <- function(conclusions, contract) {
    reasons <- list()
    for (conclusion_id in names(contract$conclusion_rules)) {
        prose <- contract$conclusion_rules[[conclusion_id]]$prose
        labels <- unlist(prose$labels, use.names = FALSE)
        label_counts <- vapply(labels, function(label) {
            sum(!is.na(conclusions$label) & conclusions$label == label)
        }, integer(1))
        if (any(label_counts != 1L)) {
            reasons[[length(reasons) + 1L]] <- new_gate_reason(
                paste0("conclusion_inconsistent_", conclusion_id),
                "fail",
                "Required conclusion statement is missing or duplicated"
            )
            next
        }
        rows <- conclusions[match(labels, conclusions$label), , drop = FALSE]
        if (nrow(rows) != length(labels) || any(is.na(rows$label)) || any(is.na(rows$value))) {
            reasons[[length(reasons) + 1L]] <- new_gate_reason(
                paste0("conclusion_inconsistent_", conclusion_id),
                "fail",
                "Required conclusion statement is missing"
            )
            next
        }
        combined <- tolower(paste(rows$value, collapse = " "))
        required <- tolower(unlist(prose$required_phrases, use.names = FALSE))
        forbidden <- tolower(unlist(prose$forbidden_phrases, use.names = FALSE))
        has_required <- all(vapply(required, grepl, logical(1), x = combined, fixed = TRUE))
        has_forbidden <- any(vapply(forbidden, grepl, logical(1), x = combined, fixed = TRUE))
        if (!has_required || has_forbidden) {
            reasons[[length(reasons) + 1L]] <- new_gate_reason(
                paste0("conclusion_inconsistent_", conclusion_id),
                "fail",
                "Candidate wording is inconsistent with the result-derived conclusion category"
            )
        }
    }
    reasons
}

evaluate_structured_candidate_conclusions <- function(conclusions, contract) {
    structured <- contract$candidate_workbook$structured_conclusions
    id_column <- structured$id_column
    category_column <- structured$category_column
    has_id <- id_column %in% names(conclusions)
    has_category <- category_column %in% names(conclusions)
    if (!has_id && !has_category && isTRUE(structured$optional)) {
        return(list())
    }
    if (!has_id || !has_category) {
        return(list(new_gate_reason(
            "candidate_conclusions",
            "fail",
            "Structured conclusion columns are incomplete"
        )))
    }

    ids <- as.character(conclusions[[id_column]])
    categories <- as.character(conclusions[[category_column]])
    populated <- (!is.na(ids) & nzchar(ids)) | (!is.na(categories) & nzchar(categories))
    ids <- ids[populated]
    categories <- categories[populated]
    expected_ids <- names(structured$categories)
    if (length(ids) != length(expected_ids) || anyNA(ids) || anyNA(categories) ||
        any(!nzchar(ids)) || any(!nzchar(categories)) || anyDuplicated(ids) ||
        !setequal(ids, expected_ids)) {
        return(list(new_gate_reason(
            "candidate_conclusions",
            "fail",
            "Structured conclusion rows are missing, duplicated, or unexpected"
        )))
    }

    reasons <- list()
    for (conclusion_id in expected_ids) {
        category <- categories[match(conclusion_id, ids)]
        category_contract <- structured$categories[[conclusion_id]]
        allowed <- unlist(category_contract, use.names = FALSE)
        if (!category %in% allowed) {
            reasons[[length(reasons) + 1L]] <- new_gate_reason(
                "candidate_conclusions",
                "fail",
                "Structured conclusion category is unsupported"
            )
        } else if (!identical(category, category_contract$expected)) {
            reasons[[length(reasons) + 1L]] <- new_gate_reason(
                paste0("conclusion_reversal_", conclusion_id),
                "fail",
                "Structured candidate conclusion contradicts the accepted category"
            )
        }
    }
    reasons
}

evaluate_result_conclusion_rules <- function(auc_values, extracted_rates, contract) {
    reasons <- list()
    rules <- contract$conclusion_rules

    direct <- rules$moderate_direct_prognostic_stratification$result
    direct_values <- unlist(auc_values[direct$models], use.names = FALSE)
    if (length(direct_values) == length(direct$models) &&
        all(is.finite(direct_values)) && any(direct_values < direct$threshold)) {
        reasons[[length(reasons) + 1L]] <- new_gate_reason(
            "conclusion_reversal_moderate_direct_prognostic_stratification",
            "fail",
            "A direct-model AUC is below the frozen moderate-prognostic threshold"
        )
    }

    ordering_rule <- rules$no_gep_groups_non_homogeneous$result
    if (!inherits(extracted_rates, "error")) {
        orderings <- contract$candidate_workbook$required_orderings
        ordering_by_id <- stats::setNames(orderings, vapply(orderings, `[[`, character(1), "id"))
        ordering_supported <- vapply(ordering_rule$ordering_ids, function(ordering_id) {
            ordering <- ordering_by_id[[ordering_id]]
            higher <- extracted_rates$rates[[ordering$higher]][[ordering$endpoint]]
            lower <- extracted_rates$rates[[ordering$lower]][[ordering$endpoint]]
            isTRUE(higher > lower)
        }, logical(1))
        if (!all(ordering_supported)) {
            reasons[[length(reasons) + 1L]] <- new_gate_reason(
                "conclusion_reversal_no_gep_groups_non_homogeneous",
                "fail",
                "The required no-GEP subgroup risk separation is not supported"
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
    candidate_auc_values <- list()
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
        candidate_auc_values[[model_id]] <- extracted$value
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

    reasons <- c(
        reasons,
        evaluate_result_conclusion_rules(candidate_auc_values, extracted_rates, contract),
        evaluate_structured_candidate_conclusions(candidate$conclusions, contract),
        check_candidate_prose_consistency(candidate$conclusions, contract)
    )
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
    required_packages <- c("digest", "jsonlite", "openxlsx", "yaml")
    missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_packages)) {
        stop("Required AAO gate packages are unavailable", call. = FALSE)
    }
    quit(save = "no", status = main())
}
