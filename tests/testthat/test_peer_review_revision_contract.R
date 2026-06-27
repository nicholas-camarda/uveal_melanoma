test_that("reviewer-response adjusted models use continuous age rather than dichotomized age", {
    expect_true("age_at_diagnosis" %in% confounders)
    expect_false("age_at_diagnosis_general_pop_median" %in% confounders)
})

test_that("Objective 1 returns Cox-led local recurrence and metastasis time-to-event analyses", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "peer_review_objective1_tte_contract")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    expect_true("recurrence_time_to_event" %in% names(pipeline$results))
    expect_true("mets_time_to_event" %in% names(pipeline$results))
    expect_s3_class(pipeline$results$recurrence_time_to_event$cox_model, "coxph")
    expect_s3_class(pipeline$results$mets_time_to_event$cox_model, "coxph")

    recurrence_summary <- file.path(
        pipeline$output_dirs$obj1_recurrence,
        "test_local_recurrence_free_probability_effect_summary.xlsx"
    )
    metastasis_summary <- file.path(
        pipeline$output_dirs$obj1_mets,
        "test_metastasis_free_survival_probability_effect_summary.xlsx"
    )
    expect_true(file.exists(recurrence_summary))
    expect_true(file.exists(metastasis_summary))

    if (file.exists(recurrence_summary) && file.exists(metastasis_summary)) {
        recurrence_rows <- readxl::read_xlsx(recurrence_summary)
        metastasis_rows <- readxl::read_xlsx(metastasis_summary)
        expect_true(any(recurrence_rows$effect_measure == "HR"))
        expect_true(any(metastasis_rows$effect_measure == "HR"))
        expect_false(any(recurrence_rows$effect_measure == "OR"))
        expect_false(any(metastasis_rows$effect_measure == "OR"))
    }
})

test_that("Objective 1 recurrence and metastasis descriptive summaries are not labeled co-primary", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "peer_review_descriptive_event_support")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    recurrence_summary_path <- file.path(pipeline$output_dirs$obj1_recurrence, "test_recurrence1_event_support_summary.xlsx")
    mets_summary_path <- file.path(pipeline$output_dirs$obj1_mets, "test_mets_progression_event_support_summary.xlsx")

    expect_true(file.exists(recurrence_summary_path))
    expect_true(file.exists(mets_summary_path))

    for (summary_path in c(recurrence_summary_path, mets_summary_path)) {
        if (!file.exists(summary_path)) {
            next
        }
        expect_true(all(c(
            "descriptive_event_counts",
            "cumulative_incidence",
            "competing_risk_support",
            "estimand_notes"
        ) %in% readxl::excel_sheets(summary_path)))

        estimand_notes <- readxl::read_xlsx(summary_path, sheet = "estimand_notes")
        expect_true(all(estimand_notes$role %in% c("descriptive_support", "supportive_time_to_event_context")))
        expect_false(any(estimand_notes$role == "co-primary"))
    }
})

test_that("Objective 1 writes five-year capped OS and PFS Cox sensitivity summaries", {
    pipeline <- run_objective1_test(create_test_dataset(), output_tag = "peer_review_five_year_capped_contract")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    os_summary <- file.path(
        pipeline$output_dirs$obj1_os,
        "test_overall_survival_probability_5yr_capped_effect_summary.xlsx"
    )
    pfs_summary <- file.path(
        pipeline$output_dirs$obj1_pfs,
        "test_progression_free_survival_probability_5yr_capped_effect_summary.xlsx"
    )

    expect_true(file.exists(os_summary))
    expect_true(file.exists(pfs_summary))

    if (file.exists(os_summary) && file.exists(pfs_summary)) {
        os_rows <- readxl::read_xlsx(os_summary)
        pfs_rows <- readxl::read_xlsx(pfs_summary)
        expect_true(any(grepl("5-year capped", os_rows$model_label, fixed = TRUE)))
        expect_true(any(grepl("5-year capped", pfs_rows$model_label, fixed = TRUE)))
        expect_true(all(os_rows$n_patients >= os_rows$n_events, na.rm = TRUE))
        expect_true(all(pfs_rows$n_patients >= pfs_rows$n_events, na.rm = TRUE))
    }

    ph_or_skip_patterns <- c(
        "test_overall_survival_probability_5yr_capped_ph_diagnostics\\.xlsx$",
        "test_overall_survival_probability_5yr_capped_skipped.*\\.(xlsx|txt)$",
        "test_progression_free_survival_probability_5yr_capped_ph_diagnostics\\.xlsx$",
        "test_progression_free_survival_probability_5yr_capped_skipped.*\\.(xlsx|txt)$"
    )
    for (pattern in ph_or_skip_patterns[c(1, 2)]) {
        os_hits <- list.files(pipeline$output_dirs$obj1_os, pattern = pattern, full.names = TRUE)
        if (length(os_hits) > 0) break
    }
    for (pattern in ph_or_skip_patterns[c(3, 4)]) {
        pfs_hits <- list.files(pipeline$output_dirs$obj1_pfs, pattern = pattern, full.names = TRUE)
        if (length(pfs_hits) > 0) break
    }
    expect_true(length(os_hits) > 0, info = "Capped OS model must write PH diagnostics or an explicit skip artifact.")
    expect_true(length(pfs_hits) > 0, info = "Capped PFS model must write PH diagnostics or an explicit skip artifact.")
})

test_that("Objective 1 KM figures use 15-year display cap without changing follow-up data", {
    expect_equal(SURVIVAL_XAXIS_MAX_MONTHS, 180)

    data <- create_test_dataset()
    data$tt_recurrence_months[1] <- 187
    data$recurrence_event[1] <- 1
    data$tt_pfs_months[1] <- 187
    data$pfs_event[1] <- 1

    pipeline <- run_objective1_test(data, output_tag = "peer_review_km_display_cap")
    withr::defer(unlink(pipeline$test_output_dir, recursive = TRUE), envir = parent.frame())

    recurrence_plot <- pipeline$results$recurrence_time_to_event$plot$plot
    pfs_plot <- pipeline$results$pfs_analysis$plot$plot
    recurrence_x_range <- ggplot2::ggplot_build(recurrence_plot)$layout$panel_params[[1]]$x.range
    pfs_x_range <- ggplot2::ggplot_build(pfs_plot)$layout$panel_params[[1]]$x.range

    expect_lte(max(recurrence_x_range), 180)
    expect_lte(max(pfs_x_range), 180)
    expect_gt(max(data$tt_recurrence_months, na.rm = TRUE), 180)
    expect_gt(max(data$tt_pfs_months, na.rm = TRUE), 180)
})

test_that("Endpoint and claim audit covers reviewer-facing high-risk endpoints", {
    audit_path <- testthat::test_path("..", "..", "docs", "peer_review_revision_response.md")
    expect_true(file.exists(audit_path))

    audit_text <- paste(readLines(audit_path, warn = FALSE), collapse = "\n")
    required_sections <- c(
        "### PFS",
        "### PFS-2",
        "### Local Recurrence",
        "### Metastatic Progression",
        "### Vision Change",
        "### Tumor-Height Change",
        "### Adverse Events",
        "### Dosimetry/Proximity Availability"
    )
    expect_true(all(required_sections %in% stringr::str_extract_all(audit_text, "### [^\n]+")[[1]]))

    for (section in required_sections) {
        section_start <- regexpr(section, audit_text, fixed = TRUE)[[1]]
        expect_gt(section_start, 0)
        section_text <- substr(audit_text, section_start, nchar(audit_text))
        next_header <- regexpr("\n### ", section_text)
        if (next_header[[1]] > 1) {
            section_text <- substr(section_text, 1, next_header[[1]] - 1)
        }
        expect_true(grepl("Outcome label:", section_text, fixed = TRUE))
        expect_true(grepl("Code path:", section_text, fixed = TRUE))
        expect_true(grepl("Time variable:", section_text, fixed = TRUE))
        expect_true(grepl("Event variable:", section_text, fixed = TRUE))
        expect_true(grepl("Event definition:", section_text, fixed = TRUE))
        expect_true(grepl("Censoring/competing-event rule:", section_text, fixed = TRUE))
        expect_true(grepl("Runtime source fields:", section_text, fixed = TRUE))
        expect_true(grepl("Reviewer question addressed:", section_text, fixed = TRUE))
        expect_true(grepl("Action:", section_text, fixed = TRUE))
    }
})

test_that("Survival route keys separate Objective 1 metastasis and Objective 4 MFS outputs", {
    output_dirs <- list(
        baseline_characteristics = tempfile("baseline"),
        obj1_mets = tempfile("obj1_mets"),
        obj4_mfs = tempfile("obj4_mfs")
    )

    expect_identical(
        determine_survival_output_dir("Metastasis-Free Survival Probability", output_dirs, route_key = "obj4_mfs"),
        output_dirs$obj4_mfs
    )
    expect_identical(
        determine_survival_output_dir("Metastasis-Free Survival Probability", output_dirs, route_key = "obj1_mets"),
        output_dirs$obj1_mets
    )
    expect_error(
        determine_survival_output_dir("Metastasis-Free Survival Probability", output_dirs, route_key = "not_a_route"),
        "Unknown survival output route_key"
    )
})
