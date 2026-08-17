library(testthat)

test_that("create_comprehensive_diagnostics exposes sparse-level diagnostics schema", {
    set.seed(123)

    test_data <- data.frame(
        outcome = stats::rbinom(100, 1, 0.3),
        treatment_group = factor(sample(c("Control", "Treatment"), 100, replace = TRUE)),
        age = stats::rnorm(100, 60, 15),
        sex = factor(sample(c("Male", "Female"), 100, replace = TRUE)),
        stringsAsFactors = FALSE
    )

    model_fit <- glm(outcome ~ treatment_group + age + sex, data = test_data, family = binomial())

    sparse_level_diagnostics <- tibble::tibble(
        analysis_name = "test_analysis",
        variable = "location",
        level = "Peripheral",
        observed_n = 3L,
        action = "excluded_rows",
        reason = "observed count below threshold (5)",
        threshold = 5,
        reference_level = "Macular",
        rows_removed = 3L,
        row_ids = "101, 102, 103",
        source = "sparse_level"
    )

    table_result <- create_gtsummary_table(
        model_fit = model_fit,
        effect_measure = "OR",
        analysis_name = "test_analysis",
        data = test_data,
        outcome_var = "outcome",
        confounders = c("age", "sex"),
        outcome_type = "binary",
        sparse_level_diagnostics = sparse_level_diagnostics
    )

    result <- create_comprehensive_diagnostics(
        model_fit = model_fit,
        data = test_data,
        outcome_var = "outcome",
        predictor_vars = "treatment_group",
        confounders = c("age", "sex"),
        analysis_name = "test_analysis",
        dataset_name = "test_dataset",
        table_result = table_result,
        sparse_level_diagnostics = sparse_level_diagnostics,
        filter_stats = list(
            initial_n = 103L,
            model_n = 100L,
            removed_n = 3L,
            removed_pct = 2.9,
            removal_reason = "Excluded sparse categorical levels before modeling"
        )
    )

    expect_true(is.list(result))
    expect_true(all(c(
        "model_summary",
        "model_diagnostics_tab",
        "data_characteristics",
        "sparse_level_diagnostics",
        "raw_model_output",
        "filtering_summary",
        "reference_levels",
        "sample_size_summary",
        "covariate_variation"
    ) %in% names(result)))
    expect_false("other_level_details" %in% names(result))

    expect_s3_class(result$sparse_level_diagnostics, "data.frame")
    expect_equal(result$sparse_level_diagnostics$variable, "location")
    expect_equal(result$sparse_level_diagnostics$level, "Peripheral")
    input_stage <- result$sample_size_summary[
        result$sample_size_summary$stage == "Model input after pre-fit exclusions",
        , drop = FALSE
    ]
    expect_equal(input_stage$n, 100L)
    expect_equal(input_stage$excluded_from_previous_n, 3L)
    expect_match(input_stage$exclusion_reason, "Excluded sparse categorical levels", fixed = TRUE)
    expect_equal(result$filtering_summary$extreme_estimates_removed, 0)
    expect_equal(result$filtering_summary$rows_removed, 0)
})

test_that("fitted-model diagnostics obtain analytic N from the fitted object", {
    test_data <- data.frame(
        id = seq_len(6),
        outcome = c(0.13, 0.21, NA_real_, 0.37, 0.52, 0.68),
        treatment_group = factor(rep(c("Control", "Treatment"), each = 3)),
        age = c(50, 55, 60, 65, 70, 75)
    )
    model_fit <- stats::lm(outcome ~ treatment_group + age, data = test_data)

    table_result <- create_gtsummary_table(
        model_fit = model_fit,
        effect_measure = "MD",
        analysis_name = "fitted_n_test",
        data = test_data,
        outcome_var = "outcome",
        confounders = "age",
        outcome_type = "continuous"
    )

    result <- create_comprehensive_diagnostics(
        model_fit = model_fit,
        data = test_data,
        outcome_var = "outcome",
        predictor_vars = "treatment_group",
        confounders = "age",
        analysis_name = "fitted_n_test",
        dataset_name = "test_dataset",
        table_result = table_result,
        filter_stats = list(
            initial_n = nrow(test_data),
            model_n = nrow(test_data),
            removed_n = 0L,
            removed_pct = 0,
            removal_reason = "No prefit exclusions"
        )
    )

    expect_equal(stats::nobs(model_fit), 5L)
    expect_equal(
        result$sample_size_summary$n[
            result$sample_size_summary$stage == "Fitted model-frame rows"
        ],
        stats::nobs(model_fit)
    )
    expect_equal(result$model_summary$fitted_n, 5L)
    expect_equal(result$model_summary$initial_n, 6L)
    expect_equal(result$model_summary$input_n, 6L)
    expect_equal(result$model_summary$n_total, 5L)
    expect_equal(result$model_summary$prefit_excluded_n, 0L)
    expect_equal(result$model_summary$complete_case_excluded_n, 1L)
    expect_equal(result$model_summary$total_excluded_n, 1L)
    expect_identical(result$model_summary$sample_size_reconciliation, "reconciled")
    expect_equal(result$sample_size_summary$stage, c(
        "Initial analysis cohort",
        "Model input after pre-fit exclusions",
        "Fitted model-frame rows"
    ))
    expect_equal(result$sample_size_summary$n, c(6L, 6L, 5L))
    expect_equal(result$sample_size_summary$excluded_from_previous_n, c(NA, 0L, 1L))
    expect_identical(result$sample_size_summary$status, c("available", "available", "fitted"))
    expect_equal(result$model_excluded_rows$row_index, 3L)
    expect_identical(result$model_excluded_rows$row_id, "3")
    expect_identical(result$model_excluded_rows$exclusion_stage, "model_fit")
})

test_that("Cox fitted-model diagnostics count modeled rows rather than events", {
    test_data <- data.frame(
        time_months = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21),
        status = c(1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1),
        treatment_group = factor(
            rep(c("Control", "Treatment"), 6),
            levels = c("Control", "Treatment")
        )
    )
    model_fit <- survival::coxph(
        survival::Surv(time_months, status) ~ treatment_group,
        data = test_data,
        model = TRUE
    )

    table_result <- create_gtsummary_table(
        model_fit = model_fit,
        effect_measure = "HR",
        analysis_name = "cox_fitted_n_test",
        data = test_data,
        outcome_var = "status",
        confounders = character(),
        outcome_type = "survival"
    )

    result <- create_comprehensive_diagnostics(
        model_fit = model_fit,
        data = test_data,
        outcome_var = "status",
        predictor_vars = "treatment_group",
        confounders = character(),
        analysis_name = "cox_fitted_n_test",
        dataset_name = "test_dataset",
        table_result = table_result,
        filter_stats = list(
            initial_n = nrow(test_data),
            model_n = nrow(test_data),
            removed_n = 0L,
            removed_pct = 0,
            removal_reason = "No prefit exclusions"
        )
    )

    expect_equal(as.integer(model_fit$n), 12L)
    expect_equal(as.integer(summary(model_fit)$nevent), 6L)
    expect_equal(
        result$sample_size_summary$n[
            result$sample_size_summary$stage == "Fitted model-frame rows"
        ],
        12L
    )
    expect_equal(result$model_summary$fitted_n, 12L)
    expect_equal(result$model_summary$initial_n, 12L)
    expect_equal(result$model_summary$input_n, 12L)
    expect_equal(result$model_summary$n_total, 12L)
    expect_equal(result$model_summary$complete_case_excluded_n, 0L)
    expect_identical(result$model_summary$sample_size_reconciliation, "reconciled")
})

test_that("model summary puts the fitted sample size alongside model metadata", {
    test_data <- data.frame(
        outcome = c(0, 0, 1, 1, NA_real_, 1, 1, 0, rep(c(0, 1, 1, 0), 4)),
        treatment_group = factor(rep(c("Control", "Treatment"), 12)),
        age = seq(45, 79, length.out = 24),
        stringsAsFactors = FALSE
    )
    model_fit <- stats::glm(outcome ~ treatment_group + age, data = test_data, family = binomial())
    table_result <- create_gtsummary_table(
        model_fit = model_fit,
        effect_measure = "OR",
        analysis_name = "front_of_workbook_n_test",
        data = test_data,
        outcome_var = "outcome",
        confounders = "age",
        outcome_type = "binary"
    )

    result <- create_comprehensive_diagnostics(
        model_fit = model_fit,
        data = test_data,
        outcome_var = "outcome",
        predictor_vars = "treatment_group",
        confounders = "age",
        analysis_name = "front_of_workbook_n_test",
        dataset_name = "test_dataset",
        table_result = table_result
    )

    expect_equal(result$model_summary$fitted_n, 23L)
    expect_equal(result$model_summary$input_n, 24L)
    expect_true(all(c(
        "input_n", "fitted_n", "prefit_excluded_n", "complete_case_excluded_n",
        "total_excluded_n", "reported_input_n", "sample_size_reconciliation"
    ) %in% names(result$model_summary)))
    expect_lt(match("fitted_n", names(result$model_summary)), match("n_events", names(result$model_summary)))
})

test_that("skipped diagnostics reconcile eligible input without claiming a fitted model", {
    summary <- build_sample_size_summary_tab(
        filter_stats = list(initial_n = 40L, model_n = 37L, removed_n = 3L),
        dataset_name = "test_dataset",
        analysis_name = "skipped_n_test",
        input_n = 37L,
        fitted_n = NA_integer_
    )

    expect_equal(summary$stage, c(
        "Initial analysis cohort",
        "Model input after pre-fit exclusions",
        "Fitted model-frame rows"
    ))
    expect_equal(summary$n, c(40L, 37L, NA_integer_))
    expect_equal(summary$excluded_from_previous_n, c(NA, 3L, NA_integer_))
    expect_identical(summary$status, c("available", "not_fitted", "not_fitted"))
    expect_identical(summary$reconciliation, c("not_applicable", "matched", "not_fitted"))
    expect_match(
        build_sample_size_source_note(summary),
        "40 initial participants; 37 entered the model-eligibility dataset",
        fixed = TRUE
    )
})

test_that("sample-size diagnostics flag stale prefit counts instead of hiding them", {
    test_data <- data.frame(
        outcome = c(0.2, 0.4, 0.6, 0.8),
        treatment_group = factor(c("Control", "Treatment", "Control", "Treatment")),
        stringsAsFactors = FALSE
    )
    model_fit <- stats::lm(outcome ~ treatment_group, data = test_data)
    audit <- build_model_sample_size_audit(
        model_fit = model_fit,
        data = test_data,
        filter_stats = list(initial_n = 4L, model_n = 3L)
    )

    expect_identical(audit$prefit_filter_reconciliation, "mismatch")
    expect_identical(audit$sample_size_reconciliation, "inconsistent")
})

test_that("supported fitted model classes use their model-frame row count", {
    set.seed(42)
    n <- 30L
    test_data <- data.frame(
        id = seq_len(n),
        continuous_outcome = seq_len(n) / 10,
        binary_outcome = sample(c(0, 1), n, replace = TRUE),
        ordinal_outcome = ordered(sample(c("low", "mid", "high"), n, replace = TRUE), levels = c("low", "mid", "high")),
        time_months = seq(10, 9 + n),
        status = sample(c(0, 1), n, replace = TRUE),
        treatment_group = factor(sample(c("Control", "Treatment"), n, replace = TRUE)),
        age = stats::rnorm(n, 60, 10),
        stringsAsFactors = FALSE
    )
    test_data$continuous_outcome[n] <- NA_real_
    test_data$binary_outcome[n] <- NA_real_
    test_data$ordinal_outcome[n] <- NA

    models <- list(
        linear = stats::lm(continuous_outcome ~ treatment_group + age, data = test_data),
        logistic = stats::glm(binary_outcome ~ treatment_group + age, data = test_data, family = binomial()),
        ordinal = MASS::polr(ordinal_outcome ~ treatment_group + age, data = test_data, Hess = TRUE, model = TRUE),
        cox = survival::coxph(survival::Surv(time_months, status) ~ treatment_group + age, data = test_data, model = TRUE)
    )

    audits <- lapply(models, function(model_fit) {
        build_model_sample_size_audit(
            model_fit = model_fit,
            data = test_data,
            filter_stats = list(initial_n = nrow(test_data), model_n = nrow(test_data))
        )
    })

    expect_equal(unname(vapply(audits, `[[`, integer(1), "fitted_n")), c(29L, 29L, 29L, 30L))
    expect_true(all(vapply(audits, `[[`, character(1), "sample_size_reconciliation") == "reconciled"))
})

test_that("diagnostics workbook omits redundant excluded rows worksheet", {
    set.seed(456)

    test_data <- data.frame(
        outcome = stats::rbinom(80, 1, 0.35),
        treatment_group = factor(sample(c("Control", "Treatment"), 80, replace = TRUE)),
        age = stats::rnorm(80, 60, 15),
        sex = factor(sample(c("Male", "Female"), 80, replace = TRUE)),
        stringsAsFactors = FALSE
    )

    model_fit <- glm(outcome ~ treatment_group + age + sex, data = test_data, family = binomial())

    sparse_level_diagnostics <- tibble::tibble(
        analysis_name = "test_analysis",
        variable = "location",
        level = "Peripheral",
        observed_n = 3L,
        action = "excluded_rows",
        reason = "observed count below threshold (5)",
        threshold = 5,
        reference_level = "Macular",
        rows_removed = 3L,
        row_ids = "101, 102, 103",
        source = "sparse_level"
    )

    table_result <- create_gtsummary_table(
        model_fit = model_fit,
        effect_measure = "OR",
        analysis_name = "test_analysis",
        data = test_data,
        outcome_var = "outcome",
        confounders = c("age", "sex"),
        outcome_type = "binary",
        sparse_level_diagnostics = sparse_level_diagnostics
    )

    diagnostics <- create_comprehensive_diagnostics(
        model_fit = model_fit,
        data = test_data,
        outcome_var = "outcome",
        predictor_vars = "treatment_group",
        confounders = c("age", "sex"),
        analysis_name = "test_analysis",
        dataset_name = "test_dataset",
        table_result = table_result,
        sparse_level_diagnostics = sparse_level_diagnostics
    )

    output_dir <- tempfile("diagnostics-output-")
    dir.create(output_dir, recursive = TRUE)
    withr::defer(unlink(output_dir, recursive = TRUE), envir = parent.frame())

    save_table_outputs(
        table_result = table_result,
        raw_output = diagnostics$raw_model_output,
        model_fit = model_fit,
        analysis_name = "test_analysis",
        dataset_name = "test_dataset",
        output_dir = output_dir,
        prefix = "unit_",
        diagnostics = diagnostics,
        data = test_data,
        outcome_var = "outcome",
        confounders = c("age", "sex")
    )

    diagnostics_path <- file.path(output_dir, "unit_test_analysis_diagnostics.xlsx")
    sheets <- readxl::excel_sheets(diagnostics_path)
    model_summary <- readxl::read_xlsx(diagnostics_path, sheet = "Model_summary")
    sample_size_summary <- readxl::read_xlsx(diagnostics_path, sheet = "Sample_size_summary")

    expect_equal(sheets[1:3], c("Model_summary", "Sample_size_summary", "Model_excluded_rows"))
    expect_true("Sparse_level_diagnostics" %in% sheets)
    expect_true("Model_excluded_rows" %in% sheets)
    expect_false("Excluded_Rows" %in% sheets)
    expect_true(all(c("input_n", "fitted_n", "n_total") %in% names(model_summary)))
    expect_equal(model_summary$fitted_n, model_summary$n_total)
    expect_equal(sample_size_summary$stage, c(
        "Initial analysis cohort",
        "Model input after pre-fit exclusions",
        "Fitted model-frame rows"
    ))
    expect_true(all(c("n", "excluded_from_previous_n", "exclusion_reason", "source") %in% names(sample_size_summary)))
})

test_that("shared skip diagnostics render structured HTML and workbook tabs", {
    diagnostics <- build_skip_report_diagnostics(
        status = "skipped",
        analysis_name = "unit_skip",
        dataset_name = "unit_dataset",
        reason = "Adjusted model skipped because only 4 events were observed.",
        narrative_lines = c(
            "Only 4 modeled events remained after exclusions.",
            "The configured minimum for adjusted fitting is 10 events."
        ),
        sample_size_summary = tibble::tibble(
            dataset_name = "unit_dataset",
            analysis_name = "unit_skip",
            stage_order = 1:3,
            stage = c(
                "Initial analysis cohort",
                "Model input after pre-fit exclusions",
                "Fitted model-frame rows"
            ),
            n = c(40L, 37L, NA_integer_),
            excluded_from_previous_n = c(NA_integer_, 3L, NA_integer_),
            excluded_pct = c(NA_real_, 7.5, NA_real_),
            exclusion_reason = c(
                "Starting analysis cohort",
                "Excluded sparse categorical levels before modeling",
                "No fitted model was produced"
            ),
            source = c(
                "filter_stats$initial_n",
                "rows passed to model fitting",
                "model fit"
            ),
            status = c("available", "not_fitted", "not_fitted"),
            reconciliation = c("not_applicable", "matched", "not_fitted")
        ),
        skip_summary = build_skip_summary_tab(list(
            modeled_events = 4L,
            minimum_events_required = 10L
        )),
        event_support = tibble::tibble(
            variable = "sex",
            level = c("Female", "Male"),
            n_total = c(12L, 25L),
            n_events = c(0L, 4L),
            n_non_events = c(12L, 21L),
            event_rate_percent = c(0, 16),
            support_flag = c("zero_events", "usable")
        ),
        model_context = build_model_context_tab(list(
            model_type = "logistic",
            formula = "outcome ~ treatment_group + sex"
        ))
    )

    html_text <- render_skip_report_html(
        analysis_name = "unit_skip",
        dataset_name = "unit_dataset",
        reason = diagnostics$reason,
        diagnostics = diagnostics
    )

    expect_match(html_text, "Adjusted Analysis Not Fit", fixed = TRUE)
    expect_match(html_text, "Why The Model Was Not Fit", fixed = TRUE)
    expect_match(html_text, "Modeled Outcome Counts By Covariate Level", fixed = TRUE)
    expect_match(html_text, "Sample Size Audit", fixed = TRUE)

    output_dir <- tempfile("skip-diagnostics-")
    dir.create(output_dir, recursive = TRUE)
    withr::defer(unlink(output_dir, recursive = TRUE), envir = parent.frame())

    diagnostics_path <- file.path(output_dir, "unit_skip_diagnostics.xlsx")
    write_diagnostics_workbook(diagnostics, diagnostics_path)
    sheets <- readxl::excel_sheets(diagnostics_path)

    expect_true(all(c("Skip_summary", "Narrative_summary", "Event_support", "Model_context") %in% sheets))
})

test_that("no-content diagnostic HTML uses the shared structured skip layout", {
    set.seed(789)

    test_data <- data.frame(
        outcome = stats::rbinom(80, 1, 0.35),
        treatment_group = factor(sample(c("Control", "Treatment"), 80, replace = TRUE)),
        age = stats::rnorm(80, 60, 15),
        stringsAsFactors = FALSE
    )

    model_fit <- glm(outcome ~ treatment_group + age, data = test_data, family = binomial())
    table_result <- create_gtsummary_table(
        model_fit = model_fit,
        effect_measure = "OR",
        analysis_name = "unit_no_content",
        data = test_data,
        outcome_var = "outcome",
        confounders = "age",
        outcome_type = "binary"
    )

    output_dir <- tempfile("no-content-output-")
    dir.create(output_dir, recursive = TRUE)
    withr::defer(unlink(output_dir, recursive = TRUE), envir = parent.frame())

    save_table_outputs(
        table_result = table_result,
        raw_output = "No meaningful content available due to extreme estimates or model convergence issues.",
        model_fit = model_fit,
        analysis_name = "unit_no_content",
        dataset_name = "test_dataset",
        output_dir = output_dir,
        prefix = "unit_",
        diagnostics = list(
            filtering_summary = data.frame(
                table_has_meaningful_content = FALSE,
                main_predictor_filtered = FALSE
            ),
            sample_size_summary = tibble::tibble(
                dataset_name = "test_dataset",
                analysis_name = "unit_no_content",
                stage_order = 1:3,
                stage = c(
                    "Initial analysis cohort",
                    "Model input after pre-fit exclusions",
                    "Fitted model-frame rows"
                ),
                n = c(80L, 80L, NA_integer_),
                excluded_from_previous_n = c(NA_integer_, 0L, NA_integer_),
                excluded_pct = c(NA_real_, 0, NA_real_),
                exclusion_reason = c(
                    "Starting analysis cohort",
                    "No pre-fit exclusions",
                    "No fitted model was produced"
                ),
                source = c(
                    "filter_stats$initial_n",
                    "rows passed to model fitting",
                    "model fit"
                ),
                status = c("available", "not_fitted", "not_fitted"),
                reconciliation = c("not_applicable", "matched", "not_fitted")
            )
        ),
        data = test_data,
        outcome_var = "outcome",
        confounders = "age"
    )

    html_path <- file.path(output_dir, "unit_unit_no_content_NO_CONTENT_DIAGNOSTIC.html")
    html_text <- paste(readLines(html_path, warn = FALSE), collapse = "\n")

    expect_match(html_text, "Report Content Not Available", fixed = TRUE)
    expect_match(html_text, "Why The Report Was Not Available", fixed = TRUE)
    expect_match(html_text, "Skip Summary", fixed = TRUE)
})

test_that("filtering summary counts filtered coefficients from raw model output", {
    raw_model_output_tab <- data.frame(
        variable_base = c("age", "sex", "location", "location"),
        variable = c("age", "Male", "Peripheral", "Factor Label"),
        row_type = c("Coefficient", "Coefficient", "Coefficient", "Factor Label"),
        inclusion_status = c("Included", "Filtered", "Filtered", "Filtered"),
        filtering_reason = c("None", "Sparse level", "Sparse level", "Sparse level"),
        stringsAsFactors = FALSE
    )

    conf_int <- matrix(
        c(0.1, 0.2, 0.3, 0.4),
        nrow = 2,
        dimnames = list(c("age", "sexMale"), c("2.5 %", "97.5 %"))
    )

    result <- create_filtering_summary_tab(
        raw_model_output_tab = raw_model_output_tab,
        conf_int = conf_int,
        predictor_vars = "treatment_group"
    )

    expect_equal(result$total_coefficients, 3)
    expect_equal(result$extreme_estimates_removed, 2)
    expect_equal(result$rows_removed, 2)
    expect_equal(result$remaining_coefficients, 1)
})

test_that("public APIs no longer expose other_map plumbing", {
    expect_false("other_map" %in% names(formals(analyze_gep_mfs_validation)))
    expect_false("other_map" %in% names(formals(analyze_gep_mss_validation)))
    expect_false("other_map" %in% names(formals(create_mfs_gep_visuals)))
    expect_false("other_map" %in% names(formals(create_mss_gep_visuals)))
    expect_false("other_map" %in% names(formals(create_mss_cumulative_incidence_curves)))
    expect_false("other_map" %in% names(formals(create_forest_plot)))
    expect_false("other_map" %in% names(formals(create_single_cohort_forest_plot)))
    expect_false("other_map" %in% names(formals(create_forest_plot_diagnostics)))
})

test_that("factor labels appear before corresponding coefficients in diagnostics output", {
    set.seed(404)
    test_data <- data.frame(
        outcome = rbinom(100, 1, 0.3),
        treatment_group = factor(sample(c("PBT", "GKSRS"), 100, replace = TRUE)),
        age = rnorm(100, 60, 15),
        sex = factor(sample(c("Male", "Female"), 100, replace = TRUE)),
        location = factor(sample(c("Choroidal", "Cilio-Choroidal", "Other"), 100, replace = TRUE)),
        stringsAsFactors = FALSE
    )

    model_fit <- glm(outcome ~ treatment_group + age + sex + location, data = test_data, family = binomial())
    table_result <- create_gtsummary_table(
        model_fit = model_fit,
        effect_measure = "OR",
        analysis_name = "ordering_test",
        data = test_data,
        outcome_var = "outcome",
        confounders = c("age", "sex", "location"),
        outcome_type = "binary"
    )
    diagnostics <- create_comprehensive_diagnostics(
        model_fit = model_fit,
        data = test_data,
        outcome_var = "outcome",
        predictor_vars = "treatment_group",
        confounders = c("age", "sex", "location"),
        analysis_name = "ordering_test",
        dataset_name = "test_dataset",
        table_result = table_result
    )

    raw_output <- diagnostics$raw_model_output
    treatment_factor_idx <- which(raw_output$variable == "treatment_group" & raw_output$row_type == "Factor Label")
    treatment_coeff_idx <- which(raw_output$variable_base == "treatment_group" & raw_output$row_type == "Coefficient")

    if (length(treatment_factor_idx) == 0 || length(treatment_coeff_idx) == 0) {
        skip("No treatment factor rows available for ordering check in this diagnostic output.")
    }

    expect_true(all(treatment_factor_idx < treatment_coeff_idx))
})
