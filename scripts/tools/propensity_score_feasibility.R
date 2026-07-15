#' Select baseline variables for treatment propensity-score feasibility
#'
#' @param data Analytic cohort data.
#' @return Character vector of candidate baseline covariates.
select_propensity_covariates <- function(data) {
    candidate_covariates <- c(
        "age_at_diagnosis",
        "sex",
        "location",
        "initial_tumor_height",
        "initial_tumor_diameter",
        "initial_vision",
        "initial_t_stage_simple",
        "initial_overall_stage",
        "optic_nerve",
        "srf",
        "visual_field_defect",
        "vision_loss_blurred_vision"
    )
    intersect(candidate_covariates, names(data))
}

#' Screen propensity covariates before model fitting
#'
#' @param data Candidate modeling data.
#' @param covariates Candidate covariate names.
#' @return Tibble with covariate-level status.
screen_propensity_covariates <- function(data, covariates) {
    purrr::map_dfr(covariates, function(covariate) {
        values <- data[[covariate]]
        non_missing_values <- values[!is.na(values)]
        unique_n <- dplyr::n_distinct(non_missing_values)
        zero_variance <- if (is.numeric(non_missing_values)) {
            length(non_missing_values) > 0L && stats::var(non_missing_values) == 0
        } else {
            unique_n <= 1L
        }
        status <- dplyr::case_when(
            length(non_missing_values) == 0L ~ "excluded_all_missing",
            unique_n <= 1L ~ "excluded_single_level",
            isTRUE(zero_variance) ~ "excluded_zero_variance",
            TRUE ~ "candidate"
        )
        tibble::tibble(
            covariate = covariate,
            status = status,
            non_missing_n = length(non_missing_values),
            unique_n = unique_n
        )
    })
}

#' Drop aliased or collinear propensity covariates
#'
#' @param ps_data Complete-case propensity-score data.
#' @param covariates Candidate covariate names after missingness and variance checks.
#' @param treatment_var Treatment group variable.
#' @return List with retained covariates and alias diagnostics.
drop_aliased_propensity_covariates <- function(ps_data, covariates, treatment_var = "treatment_group") {
    if (length(covariates) == 0L) {
        return(list(
            retained_covariates = character(),
            alias_diagnostics = tibble::tibble(
                covariate = character(),
                status = character(),
                detail = character()
            )
        ))
    }
    ps_data[[treatment_var]] <- coerce_to_factor_preserving_levels(ps_data[[treatment_var]])

    formula <- stats::as.formula(paste(treatment_var, "~", paste(covariates, collapse = " + ")))
    model_matrix_formula <- stats::as.formula(paste("~", paste(covariates, collapse = " + ")))
    model_matrix <- tryCatch(
        stats::model.matrix(model_matrix_formula, data = ps_data),
        error = function(e) e
    )
    if (inherits(model_matrix, "error")) {
        return(list(
            retained_covariates = character(),
            alias_diagnostics = tibble::tibble(
                covariate = covariates,
                status = "excluded_model_matrix_error",
                detail = conditionMessage(model_matrix)
            )
        ))
    }
    matrix_rank <- qr(model_matrix)$rank
    dependent_columns <- character()
    if (matrix_rank < ncol(model_matrix)) {
        dependent_columns <- colnames(model_matrix)[qr(model_matrix)$pivot[(matrix_rank + 1L):ncol(model_matrix)]]
    }

    model <- tryCatch(
        suppressWarnings(stats::glm(formula, data = ps_data, family = stats::binomial())),
        error = function(e) e
    )
    if (inherits(model, "error")) {
        return(list(
            retained_covariates = character(),
            alias_diagnostics = tibble::tibble(
                covariate = covariates,
                status = "excluded_model_matrix_error",
                detail = conditionMessage(model)
            )
        ))
    }

    aliased_terms <- names(stats::coef(model))[is.na(stats::coef(model))]
    aliased_covariates <- unique(unlist(purrr::map(covariates, function(covariate) {
        matched <- grepl(paste0("^", covariate), c(aliased_terms, dependent_columns))
        if (any(matched)) covariate else character()
    })))
    retained_covariates <- setdiff(covariates, aliased_covariates)
    tibble::tibble(
        covariate = covariates,
        status = ifelse(covariates %in% aliased_covariates, "excluded_aliased_or_collinear", "retained"),
        detail = ifelse(
            covariates %in% aliased_covariates,
            "Coefficient was aliased in the diagnostic treatment model.",
            "No alias detected in the diagnostic treatment model."
        )
    ) %>%
        list(retained_covariates = retained_covariates, alias_diagnostics = .)
}

#' Fit diagnostic treatment propensity score and diagnose feasibility
#'
#' @param data Analytic cohort data.
#' @param treatment_var Treatment variable name.
#' @return Named list of feasibility data, diagnostics, and reportability flag.
fit_treatment_propensity_score <- function(data, treatment_var = "treatment_group") {
    if (!treatment_var %in% names(data)) {
        stop(sprintf("Treatment variable is missing: %s", treatment_var), call. = FALSE)
    }

    covariates <- select_propensity_covariates(data)
    selected_data <- data %>%
        dplyr::filter(!is.na(.data[[treatment_var]])) %>%
        dplyr::mutate("{treatment_var}" := coerce_to_factor_preserving_levels(.data[[treatment_var]]))

    treatment_levels <- levels(selected_data[[treatment_var]])
    if (length(treatment_levels) != 2L) {
        stop("Propensity-score feasibility requires exactly two treatment groups.", call. = FALSE)
    }

    covariate_screen <- screen_propensity_covariates(selected_data, covariates)
    retained_after_screen <- covariate_screen %>%
        dplyr::filter(.data$status == "candidate") %>%
        dplyr::pull(.data$covariate)

    if (length(retained_after_screen) == 0L) {
        stop("No baseline propensity covariates were available after screening.", call. = FALSE)
    }

    ps_data <- selected_data %>%
        dplyr::select(dplyr::all_of(c(treatment_var, retained_after_screen))) %>%
        tidyr::drop_na()
    complete_case_summary <- tibble::tibble(
        input_n = nrow(selected_data),
        complete_case_n = nrow(ps_data),
        complete_case_fraction = nrow(ps_data) / nrow(selected_data)
    )

    if (nrow(ps_data) < 10L) {
        stop("Propensity-score feasibility has fewer than 10 complete cases.", call. = FALSE)
    }

    alias_screen <- drop_aliased_propensity_covariates(ps_data, retained_after_screen, treatment_var = treatment_var)
    retained_covariates <- alias_screen$retained_covariates
    if (length(retained_covariates) == 0L) {
        stop("No propensity covariates remained after alias/collinearity screening.", call. = FALSE)
    }

    formula <- stats::as.formula(paste(treatment_var, "~", paste(retained_covariates, collapse = " + ")))
    model <- suppressWarnings(stats::glm(formula, data = ps_data, family = stats::binomial()))
    ps_data$.propensity_score <- stats::predict(model, type = "response")
    ps_data$.in_common_support <- FALSE

    overlap_by_treatment <- ps_data %>%
        dplyr::group_by(.data[[treatment_var]]) %>%
        dplyr::summarise(
            n = dplyr::n(),
            min_ps = min(.data$.propensity_score),
            q25_ps = as.numeric(stats::quantile(.data$.propensity_score, 0.25)),
            median_ps = stats::median(.data$.propensity_score),
            q75_ps = as.numeric(stats::quantile(.data$.propensity_score, 0.75)),
            max_ps = max(.data$.propensity_score),
            .groups = "drop"
        )
    common_min <- max(overlap_by_treatment$min_ps)
    common_max <- min(overlap_by_treatment$max_ps)
    ps_data$.in_common_support <- ps_data$.propensity_score >= common_min & ps_data$.propensity_score <= common_max

    separation_prone <- any(ps_data$.propensity_score < 0.02 | ps_data$.propensity_score > 0.98)
    common_support_fraction <- mean(ps_data$.in_common_support)
    reportable_sensitivity <- isFALSE(separation_prone) &&
        is.finite(common_support_fraction) &&
        common_support_fraction >= 0.8 &&
        nrow(ps_data) >= 50L

    diagnostics <- tibble::tibble(
        diagnostic = c(
            "complete_case_fraction",
            "common_support_fraction",
            "minimum_propensity_score",
            "maximum_propensity_score",
            "separation_prone",
            "reportable_sensitivity"
        ),
        value = c(
            complete_case_summary$complete_case_fraction,
            common_support_fraction,
            min(ps_data$.propensity_score),
            max(ps_data$.propensity_score),
            as.numeric(separation_prone),
            as.numeric(reportable_sensitivity)
        ),
        interpretation = c(
            "Fraction of treatment-known patients retained after covariate complete-case filtering.",
            "Fraction of complete cases inside overlapping observed propensity-score ranges.",
            "Near-zero values indicate possible separation or poor overlap.",
            "Near-one values indicate possible separation or poor overlap.",
            "TRUE blocks promotion to a reportable propensity-adjusted sensitivity.",
            "TRUE means diagnostics are minimally adequate for a later promoted sensitivity model."
        )
    )

    list(
        data = ps_data,
        model = model,
        covariate_screen = covariate_screen,
        alias_diagnostics = alias_screen$alias_diagnostics,
        complete_case_summary = complete_case_summary,
        overlap_by_treatment = overlap_by_treatment,
        diagnostics = diagnostics,
        retained_covariates = tibble::tibble(covariate = retained_covariates),
        reportable_sensitivity = reportable_sensitivity
    )
}

#' Write propensity-score feasibility workbook
#'
#' @param ps_fit Output from `fit_treatment_propensity_score()`.
#' @param path Output workbook path.
#' @return Output path invisibly.
write_propensity_score_feasibility <- function(ps_fit, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    openxlsx::write.xlsx(
        list(
            diagnostics = ps_fit$diagnostics,
            complete_case_summary = ps_fit$complete_case_summary,
            covariate_screen = ps_fit$covariate_screen,
            alias_diagnostics = ps_fit$alias_diagnostics,
            retained_covariates = ps_fit$retained_covariates,
            overlap_by_treatment = ps_fit$overlap_by_treatment,
            propensity_scores = ps_fit$data %>%
                dplyr::select(dplyr::any_of(c("treatment_group", ".propensity_score", ".in_common_support")))
        ),
        file = path,
        overwrite = TRUE
    )
    invisible(path)
}

#' Run propensity-score feasibility audits for full and restricted cohorts
#'
#' @return Character vector of created workbook paths.
run_propensity_score_feasibility_audits <- function() {
    output_dir <- PEER_REVIEW_REVISION_AUDITS_DIR
    cohort_paths <- c(
        full = file.path(PROCESSED_DATA_DIR, "uveal_melanoma_full_cohort.rds"),
        restricted = file.path(PROCESSED_DATA_DIR, "uveal_melanoma_restricted_cohort.rds")
    )

    purrr::imap_chr(cohort_paths, function(cohort_path, cohort_name) {
        data <- readRDS(cohort_path)
        ps_fit <- fit_treatment_propensity_score(data)
        output_path <- file.path(output_dir, paste0(cohort_name, "_propensity_score_feasibility.xlsx"))
        write_propensity_score_feasibility(ps_fit, output_path)
        output_path
    })
}

if (identical(environment(), globalenv()) && sys.nframe() == 0L) {
    if (!exists("OUTPUT_DIR", inherits = TRUE)) {
        source(here::here("scripts", "load_all.R"))
    }
    paths <- run_propensity_score_feasibility_audits()
    message("Created propensity-score feasibility workbooks:")
    message(paste(paths, collapse = "\n"))
}
