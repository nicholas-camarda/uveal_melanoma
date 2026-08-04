create_propensity_test_data <- function(n_per_arm = 40L) {
    n <- 2L * n_per_arm
    arm_index <- rep(seq_len(n_per_arm), times = 2L)
    treatment_year <- 2010L + ((arm_index * 3L) %% 16L)
    treatment_date <- as.Date(sprintf("%d-01-01", treatment_year)) + seq_len(n)
    tibble::tibble(
        id = seq_len(n),
        treatment_group = factor(
            rep(c("PBT", "GKSRS"), each = n_per_arm),
            levels = TREATMENT_FACTOR_LEVELS
        ),
        treatment_date = treatment_date,
        treatment_year = treatment_year,
        age_at_diagnosis = 42 + ((arm_index * 7L) %% 43L),
        sex = factor(ifelse(arm_index %% 2L == 0L, "Female", "Male")),
        location = factor(ifelse(arm_index %% 3L == 0L, "Cilio-Choroidal", "Choroidal")),
        initial_tumor_height = 2 + ((arm_index * 11L) %% 70L) / 10,
        initial_tumor_diameter = 5 + ((arm_index * 13L) %% 130L) / 10,
        srf = factor(ifelse(arm_index %% 5L < 2L, "No", "Yes")),
        tt_recurrence_months = 5 + arm_index * 2,
        recurrence_event = as.integer(arm_index %% 4L == 0L),
        tt_mets_months = 6 + arm_index * 2,
        mets_event = as.integer(arm_index %% 3L == 0L),
        tt_death_months = 7 + arm_index * 2,
        death_event = as.integer(arm_index %% 4L == 1L),
        tt_pfs_months = 4 + arm_index * 2,
        pfs_event = as.integer(arm_index %% 2L == 0L)
    )
}

test_that("propensity sensitivity uses the approved centralized policy", {
    expect_identical(
        OBJECTIVE1_PROPENSITY_COVARIATES,
        c(
            "age_at_diagnosis", "sex", "location", "initial_tumor_height",
            "initial_tumor_diameter", "srf", "treatment_year"
        )
    )
    expect_identical(OBJECTIVE1_PROPENSITY_DATASET, "uveal_melanoma_restricted_cohort")
    expect_identical(OBJECTIVE1_PROPENSITY_ESTIMAND, "ATO")
    expect_identical(OBJECTIVE1_PROPENSITY_BALANCE_THRESHOLD, 0.10)
    expect_identical(TREATMENT_REFERENCE_LEVEL, "PBT")
    expect_identical(TREATMENT_COMPARISON_LEVEL, "GKSRS")
    expect_identical(
        names(OBJECTIVE1_PROPENSITY_EXPECTED_POPULATIONS),
        c("surface", "n_patients", "n_events", "population_fingerprint")
    )
    expect_setequal(
        names(OBJECTIVE1_SUBGROUP_OUTCOME_SPECS),
        c(
            "local_recurrence", "metastatic_progression",
            "overall_survival", "progression_free_survival"
        )
    )
})

test_that("propensity population preparation is read-only and deterministic", {
    data <- create_propensity_test_data()
    original <- data

    prepared <- prepare_objective1_propensity_population(data, "synthetic_restricted")

    expect_identical(data, original)
    expect_identical(prepared$data, original)
    expect_identical(prepared$selection_index, seq_len(nrow(data)))
    expect_equal(prepared$population_audit$input_n, nrow(data))
    expect_equal(prepared$population_audit$model_n, nrow(data))
    expect_match(prepared$population_audit$population_fingerprint, "^[0-9a-f]{64}$")
})

test_that("propensity population preparation applies only the location sparsity rule", {
    data <- create_propensity_test_data()
    data$location <- as.character(data$location)
    data$location[1] <- "Ciliary Body"
    data$location <- factor(data$location)

    prepared <- prepare_objective1_propensity_population(data, "synthetic_restricted")

    expect_equal(prepared$selection_index, 2:nrow(data))
    expect_equal(prepared$sparse_audit$excluded_levels, "Ciliary Body")
    expect_equal(prepared$sparse_audit$excluded_n, 1L)
})

test_that("propensity population preparation fails closed on invalid inputs", {
    data <- create_propensity_test_data()

    expect_error(
        prepare_objective1_propensity_population(dplyr::select(data, -"srf"), "synthetic"),
        "missing required propensity columns"
    )

    missing_height <- data
    missing_height$initial_tumor_height[1] <- NA_real_
    expect_error(
        prepare_objective1_propensity_population(missing_height, "synthetic"),
        "missing required propensity values"
    )

    reversed <- data
    reversed$treatment_group <- factor(
        reversed$treatment_group,
        levels = rev(TREATMENT_FACTOR_LEVELS)
    )
    expect_error(
        prepare_objective1_propensity_population(reversed, "synthetic"),
        "treatment factor levels"
    )

    unexpected <- data
    unexpected$treatment_group <- factor(
        as.character(unexpected$treatment_group),
        levels = c(TREATMENT_FACTOR_LEVELS, "Other")
    )
    expect_error(
        prepare_objective1_propensity_population(unexpected, "synthetic"),
        "treatment factor levels"
    )
})

test_that("propensity design fits the exact model and named overlap weights", {
    prepared <- prepare_objective1_propensity_population(
        create_propensity_test_data(),
        "synthetic_restricted"
    )
    design <- fit_objective1_propensity_weights(prepared)

    expect_identical(
        design$formula_text,
        paste(
            "treatment_group ~ age_at_diagnosis + sex + location +",
            "initial_tumor_height + initial_tumor_diameter + srf + treatment_year"
        )
    )
    expect_true(all(design$data$.propensity_score > 0 & design$data$.propensity_score < 1))
    expect_true(all(design$data$.overlap_weight > 0 & design$data$.overlap_weight < 1))
    expect_equal(
        design$data$.overlap_weight[design$data$treatment_group == TREATMENT_COMPARISON_LEVEL],
        1 - design$data$.propensity_score[design$data$treatment_group == TREATMENT_COMPARISON_LEVEL],
        tolerance = 1e-14
    )
    expect_equal(
        design$data$.overlap_weight[design$data$treatment_group == TREATMENT_REFERENCE_LEVEL],
        design$data$.propensity_score[design$data$treatment_group == TREATMENT_REFERENCE_LEVEL],
        tolerance = 1e-14
    )
    expect_setequal(attr(stats::terms(design$model), "term.labels"), OBJECTIVE1_PROPENSITY_COVARIATES)
    expect_false(any(c(
        "optic_nerve", "initial_t_stage_simple", "initial_vision",
        "orange_pigment", "treatment_year_centered"
    ) %in% attr(stats::terms(design$model), "term.labels")))
    expect_match(design$weight_fingerprint, "^[0-9a-f]{64}$")
})

test_that("propensity design reports positive ESS and exact modeled-term balance", {
    prepared <- prepare_objective1_propensity_population(
        create_propensity_test_data(),
        "synthetic_restricted"
    )
    design <- fit_objective1_propensity_weights(prepared)

    expect_setequal(design$ess_summary$treatment_group, c(TREATMENT_FACTOR_LEVELS, "Total"))
    expect_true(all(design$ess_summary$effective_sample_size > 0))
    expect_true(all(is.finite(design$balance_table$weighted_smd)))
    expect_lt(max(design$balance_table$weighted_abs_smd), 1e-8)
    expect_false(any(design$balance_table$weighted_exceeds_threshold))
    expect_identical(design$diagnostics$estimand[[1]], OBJECTIVE1_PROPENSITY_ESTIMAND)
})

test_that("propensity design rejects aliased required terms", {
    data <- create_propensity_test_data()
    data$initial_tumor_diameter <- data$initial_tumor_height
    prepared <- prepare_objective1_propensity_population(data, "synthetic_restricted")

    expect_error(
        fit_objective1_propensity_weights(prepared),
        "aliased or non-finite coefficients"
    )
})

test_that("weighted endpoint models preserve order, direction, and one weight vector", {
    prepared <- prepare_objective1_propensity_population(
        create_propensity_test_data(),
        "synthetic_restricted"
    )
    design <- fit_objective1_propensity_weights(prepared)
    results <- fit_objective1_weighted_endpoints(design)

    expect_identical(names(results$fits), names(OBJECTIVE1_SUBGROUP_OUTCOME_SPECS))
    expect_identical(
        results$weighted_cox_results$outcome_key,
        names(OBJECTIVE1_SUBGROUP_OUTCOME_SPECS)
    )
    expect_true(all(results$weighted_cox_results$effect_measure == "HR"))
    expect_true(all(results$weighted_cox_results$comparison == "GKSRS vs PBT"))
    expect_true(all(results$weighted_cox_results$estimand == OBJECTIVE1_PROPENSITY_ESTIMAND))
    expect_true(length(unique(results$weighted_cox_results$weight_fingerprint)) == 1L)
    expect_identical(
        unique(results$weighted_cox_results$weight_fingerprint),
        design$weight_fingerprint
    )
    expect_true(all(vapply(results$fits, function(x) !is.null(x$model$naive.var), logical(1))))
    expect_true(all(vapply(
        results$fits,
        function(x) identical(names(stats::coef(x$model)), "treatment_groupGKSRS"),
        logical(1)
    )))
})

test_that("weighted endpoint rows use the canonical schema and valid PH diagnostics", {
    prepared <- prepare_objective1_propensity_population(
        create_propensity_test_data(),
        "synthetic_restricted"
    )
    design <- fit_objective1_propensity_weights(prepared)
    results <- fit_objective1_weighted_endpoints(design)

    expect_identical(
        names(results$weighted_cox_results),
        c(
            "outcome_key", "outcome", "time_var", "event_var", "endpoint_estimand",
            "model_family", "effect_measure", "comparison", "estimand", "weight_method",
            "n", "events", "pbt_n", "pbt_events", "gksrs_n", "gksrs_events",
            "weighted_ess_total", "weighted_ess_pbt", "weighted_ess_gksrs",
            "estimate", "conf_low", "conf_high", "p_value", "ph_global_p",
            "weight_fingerprint", "status", "interpretation"
        )
    )
    expect_true(all(results$weighted_cox_results$status == "estimated"))
    expect_true(all(results$weighted_cox_results$estimate > 0))
    expect_true(all(results$weighted_cox_results$conf_low > 0))
    expect_true(all(results$weighted_cox_results$conf_high > 0))
    expect_setequal(unique(results$ph_diagnostics$term), c("treatment_group", "GLOBAL"))
    expect_true(all(results$ph_diagnostics$status == "tested"))
})

test_that("weighted endpoint arm counts are named and row-order invariant", {
    data <- create_propensity_test_data()
    data <- data[rev(seq_len(nrow(data))), , drop = FALSE]
    prepared <- prepare_objective1_propensity_population(data, "synthetic_restricted")
    design <- fit_objective1_propensity_weights(prepared)
    results <- fit_objective1_weighted_endpoints(design)

    for (outcome_key in names(OBJECTIVE1_SUBGROUP_OUTCOME_SPECS)) {
        spec <- OBJECTIVE1_SUBGROUP_OUTCOME_SPECS[[outcome_key]]
        row <- results$weighted_cox_results %>%
            dplyr::filter(.data$outcome_key == .env$outcome_key)
        expect_equal(
            row$pbt_n,
            sum(design$data$treatment_group == TREATMENT_REFERENCE_LEVEL)
        )
        expect_equal(
            row$gksrs_n,
            sum(design$data$treatment_group == TREATMENT_COMPARISON_LEVEL)
        )
        expect_equal(
            row$pbt_events,
            sum(
                design$data[[spec$event_var]][
                    design$data$treatment_group == TREATMENT_REFERENCE_LEVEL
                ]
            )
        )
        expect_equal(
            row$gksrs_events,
            sum(
                design$data[[spec$event_var]][
                    design$data$treatment_group == TREATMENT_COMPARISON_LEVEL
                ]
            )
        )
    }
})

test_that("weighted endpoint support derives competing deaths for recurrence and metastasis", {
    prepared <- prepare_objective1_propensity_population(
        create_propensity_test_data(),
        "synthetic_restricted"
    )
    design <- fit_objective1_propensity_weights(prepared)
    results <- fit_objective1_weighted_endpoints(design)
    support <- results$endpoint_support %>%
        dplyr::filter(.data$outcome_key %in% c("local_recurrence", "metastatic_progression"))

    expect_true(all(c(
        "competing_deaths", "pbt_competing_deaths", "gksrs_competing_deaths"
    ) %in% names(support)))
    expect_true(all(support$competing_deaths >= 0))
    expect_equal(
        support$competing_deaths,
        support$pbt_competing_deaths + support$gksrs_competing_deaths
    )
})

test_that("propensity artifacts share exact labels, direction, and workbook values", {
    prepared <- prepare_objective1_propensity_population(
        create_propensity_test_data(),
        "synthetic_restricted"
    )
    design <- fit_objective1_propensity_weights(prepared)
    endpoints <- fit_objective1_weighted_endpoints(design)
    artifacts <- build_objective1_propensity_artifacts(design, endpoints)

    expect_identical(artifacts$plots$forest$labels$x, "Overlap-weighted HR (95% CI)")
    expect_true(all(artifacts$weighted_cox_results$comparison == "GKSRS vs PBT"))
    expect_setequal(
        unique(artifacts$plots$balance$data$weighting),
        c("Unweighted", "Overlap weighted")
    )

    output_dir <- tempfile("propensity-artifacts-", tmpdir = TEST_OUTPUT_DIR)
    dir.create(output_dir, recursive = TRUE)
    withr::defer(unlink(output_dir, recursive = TRUE, force = TRUE), envir = parent.frame())
    paths <- write_objective1_propensity_artifacts(
        artifacts,
        output_dir,
        "restricted_cohort_"
    )

    expect_identical(
        names(paths),
        c(
            "workbook", "propensity_overlap_plot", "covariate_balance_plot",
            "results_forest_plot", "weighted_schoenfeld_plot",
            "markdown_summary", "technical_audit_rds"
        )
    )
    expect_identical(
        basename(unname(paths)),
        paste0("restricted_cohort_", unname(OBJECTIVE1_PROPENSITY_ARTIFACT_BASENAMES))
    )
    expect_true(all(file.exists(paths)))
    expect_true(all(file.info(paths)$size > 0))
    expect_identical(readxl::excel_sheets(paths[["workbook"]]), OBJECTIVE1_PROPENSITY_WORKBOOK_SHEETS)

    workbook_results <- readxl::read_xlsx(paths[["workbook"]], sheet = "weighted_cox_results")
    expect_identical(workbook_results$effect_measure, endpoints$weighted_cox_results$effect_measure)
    expect_identical(workbook_results$comparison, endpoints$weighted_cox_results$comparison)
    expect_equal(
        workbook_results[c("estimate", "conf_low", "conf_high")],
        endpoints$weighted_cox_results[c("estimate", "conf_low", "conf_high")],
        tolerance = 1e-12
    )
    expect_equal(
        workbook_results[c("pbt_n", "pbt_events", "gksrs_n", "gksrs_events")],
        endpoints$weighted_cox_results[c("pbt_n", "pbt_events", "gksrs_n", "gksrs_events")]
    )

    support_start <- artifacts$workbook_layout$analysis_population$endpoint_support
    workbook_support <- openxlsx::read.xlsx(
        paths[["workbook"]],
        sheet = "analysis_population",
        startRow = support_start
    )
    expect_equal(
        workbook_support[c(
            "competing_deaths", "pbt_competing_deaths", "gksrs_competing_deaths"
        )],
        as.data.frame(endpoints$endpoint_support[c(
            "competing_deaths", "pbt_competing_deaths", "gksrs_competing_deaths"
        )])
    )
})

test_that("technical audit RDS reconstructs the propensity and endpoint designs", {
    data <- create_propensity_test_data()
    data$location <- as.character(data$location)
    data$location[1] <- "Ciliary Body"
    data$location <- factor(data$location)
    prepared <- prepare_objective1_propensity_population(data, "synthetic_restricted")
    design <- fit_objective1_propensity_weights(prepared)
    endpoints <- fit_objective1_weighted_endpoints(design)
    artifacts <- build_objective1_propensity_artifacts(design, endpoints)

    output_dir <- tempfile("propensity-audit-", tmpdir = TEST_OUTPUT_DIR)
    dir.create(output_dir, recursive = TRUE)
    withr::defer(unlink(output_dir, recursive = TRUE, force = TRUE), envir = parent.frame())
    paths <- write_objective1_propensity_artifacts(
        artifacts,
        output_dir,
        "restricted_cohort_"
    )
    audit <- readRDS(paths[["technical_audit_rds"]])

    expect_identical(
        names(audit),
        c(
            "schema_version", "analysis_specification", "provenance", "cohort_flow",
            "population_checks", "patient_design", "propensity_model_matrix",
            "propensity_model", "propensity_diagnostics", "endpoint_design",
            "weighted_cox_models", "ph_objects", "weighted_cox_results",
            "endpoint_support", "ph_diagnostics"
        )
    )
    expect_identical(audit$schema_version, OBJECTIVE1_PROPENSITY_AUDIT_SCHEMA_VERSION)
    expect_equal(nrow(audit$patient_design), nrow(data))
    expect_equal(sum(audit$patient_design$analysis_included), nrow(data) - 1L)
    excluded <- audit$patient_design[!audit$patient_design$analysis_included, ]
    expect_equal(nrow(excluded), 1L)
    expect_match(excluded$exclusion_reason, "Sparse location level")
    expect_true(is.na(excluded$propensity_score))
    expect_true(is.na(excluded$overlap_weight))
    included <- audit$patient_design[audit$patient_design$analysis_included, ]
    expect_equal(included$propensity_score, unname(design$data$.propensity_score), tolerance = 1e-12)
    expect_equal(included$overlap_weight, unname(design$data$.overlap_weight), tolerance = 1e-12)

    matrix <- as.matrix(audit$propensity_model_matrix[, -1, drop = FALSE])
    reconstructed <- stats::plogis(drop(matrix %*% stats::coef(audit$propensity_model)))
    expect_equal(reconstructed, unname(design$data$.propensity_score), tolerance = 1e-12)
    expect_identical(
        unique(audit$weighted_cox_results$weight_fingerprint),
        audit$provenance$fingerprints$weight
    )
    expect_identical(
        names(audit$weighted_cox_models),
        names(OBJECTIVE1_SUBGROUP_OUTCOME_SPECS)
    )
    expect_false(any(grepl("(/Users/|file://)", capture.output(str(audit$provenance)))))
})
