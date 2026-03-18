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
    expect_equal(result$sample_size_summary$removed_n, 3L)
    expect_match(result$sample_size_summary$removal_reason, "Excluded sparse categorical levels", fixed = TRUE)
    expect_equal(result$filtering_summary$extreme_estimates_removed, 0)
    expect_equal(result$filtering_summary$rows_removed, 0)
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

    expect_true("Sparse_level_diagnostics" %in% sheets)
    expect_false("Excluded_Rows" %in% sheets)
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
            initial_n = 40L,
            modeled_n = 37L,
            removed_n = 3L,
            removed_pct = 7.5,
            removal_reason = "Excluded sparse categorical levels before modeling"
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
                initial_n = 80L,
                modeled_n = 80L,
                removed_n = 0L,
                removed_pct = 0,
                removal_reason = "No pre-model exclusions"
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
    treatment_coeff_idx <- which(grepl("^treatment_group", raw_output$variable) & raw_output$row_type == "Coefficient")

    if (length(treatment_factor_idx) == 0 || length(treatment_coeff_idx) == 0) {
        skip("No treatment factor rows available for ordering check in this diagnostic output.")
    }

    expect_true(all(treatment_factor_idx < treatment_coeff_idx))
})
