binary_forest_plot_results <- list(
    age_at_diagnosis_general_pop_median = list(
        interaction_p = 0.12,
        subgroup_effects = data.frame(
            subgroup_variable = "age_at_diagnosis_general_pop_median",
            subgroup_level = c("Low", "High"),
            n_total = c(111, 143),
            n_plaque = c(61, 76),
            n_gksrs = c(50, 67),
            events_plaque = c(10, 15),
            events_gksrs = c(8, 11),
            treatment_effect = c(1.25, 0.88),
            ci_lower = c(0.85, 0.55),
            ci_upper = c(1.85, 1.41),
            p_value = c(0.23, 0.60),
            stringsAsFactors = FALSE
        )
    )
)

md_forest_plot_results <- list(
    age_at_diagnosis_general_pop_median = list(
        interaction_p = 0.18,
        subgroup_effects = data.frame(
            subgroup_variable = "age_at_diagnosis_general_pop_median",
            subgroup_level = c("Low", "High"),
            n_total = c(111, 143),
            n_plaque = c(61, 76),
            n_gksrs = c(50, 67),
            events_plaque = c(10, 15),
            events_gksrs = c(8, 11),
            treatment_effect = c(0.83, 1.62),
            ci_lower = c(0.22, 0.05),
            ci_upper = c(1.43, 3.19),
            p_value = c(0.008, 0.04),
            stringsAsFactors = FALSE
        )
    )
)

test_that("forest plot count headers use compact n/N notation", {
    binary_plot_data <- create_forest_plot_data(
        subgroup_results = binary_forest_plot_results,
        variable_order = names(binary_forest_plot_results),
        treatment_labels = TREATMENT_LABELS,
        effect_measure = "OR"
    )
    md_plot_data <- create_forest_plot_data(
        subgroup_results = md_forest_plot_results,
        variable_order = names(md_forest_plot_results),
        treatment_labels = TREATMENT_LABELS,
        effect_measure = "MD"
    )

    expect_equal(ncol(binary_plot_data$data_frame), 7L)
    expect_equal(ncol(md_plot_data$data_frame), 7L)
    expect_equal(names(binary_plot_data$data_frame)[4], " ")
    expect_equal(names(md_plot_data$data_frame)[4], " ")

    expect_equal(names(binary_plot_data$data_frame)[2:3], c("PBT n/N", "GKSRS n/N"))
    expect_equal(names(md_plot_data$data_frame)[2:3], c("PBT n/N", "GKSRS n/N"))

    expect_equal(binary_plot_data$data_frame[[names(binary_plot_data$data_frame)[2]]][2], "10/61")
    expect_equal(binary_plot_data$data_frame[[names(binary_plot_data$data_frame)[3]]][2], "8/50")
    expect_equal(md_plot_data$data_frame[[names(md_plot_data$data_frame)[2]]][2], "61/111")
    expect_equal(md_plot_data$data_frame[[names(md_plot_data$data_frame)[3]]][2], "50/111")
})

test_that("forest plot arm counts follow treatment keys rather than column positions", {
    configured_order <- create_forest_plot_data(
        subgroup_results = binary_forest_plot_results,
        variable_order = names(binary_forest_plot_results),
        treatment_labels = c("PBT", "GKSRS"),
        effect_measure = "OR"
    )
    reversed_order <- create_forest_plot_data(
        subgroup_results = binary_forest_plot_results,
        variable_order = names(binary_forest_plot_results),
        treatment_labels = c("GKSRS", "PBT"),
        effect_measure = "OR"
    )

    expect_identical(
        unname(as.character(configured_order$data_frame[2, c("PBT n/N", "GKSRS n/N")])),
        c("10/61", "8/50")
    )
    expect_identical(
        unname(as.character(reversed_order$data_frame[2, c("GKSRS n/N", "PBT n/N")])),
        c("8/50", "10/61")
    )
})

test_that("forest plot arm mapping rejects ambiguous treatment labels", {
    expect_error(
        create_forest_plot_data(
            subgroup_results = binary_forest_plot_results,
            variable_order = names(binary_forest_plot_results),
            treatment_labels = c("PBT", "Plaque"),
            effect_measure = "OR"
        ),
        "exactly PBT and GKSRS"
    )
    expect_error(
        create_forest_plot_data(
            subgroup_results = binary_forest_plot_results,
            variable_order = names(binary_forest_plot_results),
            treatment_labels = c("PBT", "PBT"),
            effect_measure = "OR"
        ),
        "exactly PBT and GKSRS"
    )
})

test_that("forest plots render for binary and MD outcomes with unchanged row structure", {
    binary_plot_data <- create_forest_plot_data(
        subgroup_results = binary_forest_plot_results,
        variable_order = names(binary_forest_plot_results),
        treatment_labels = TREATMENT_LABELS,
        effect_measure = "OR"
    )
    md_plot_data <- create_forest_plot_data(
        subgroup_results = md_forest_plot_results,
        variable_order = names(md_forest_plot_results),
        treatment_labels = TREATMENT_LABELS,
        effect_measure = "MD"
    )

    binary_png <- tempfile(fileext = ".png")
    md_png <- tempfile(fileext = ".png")

    expect_no_error({
        grDevices::png(binary_png, width = 1400, height = 1100, res = 144)
        tryCatch(
            {
                binary_plot <- create_single_cohort_forest_plot(
                    subgroup_results = binary_forest_plot_results,
                    outcome_name = "Local Recurrence",
                    cohort_name = "Test Cohort",
                    treatment_labels = TREATMENT_LABELS,
                    variable_order = names(binary_forest_plot_results),
                    effect_measure = "OR",
                    favours_labels = FAVOURS_LABELS,
                    title = "Subgroup Analysis: Local Recurrence (Test Cohort)"
                )
                expect_equal(attr(binary_plot, "forest_row_count"), nrow(binary_plot_data$data_frame))
                plot(binary_plot)
            },
            finally = grDevices::dev.off()
        )
    })

    expect_no_error({
        grDevices::png(md_png, width = 1400, height = 1100, res = 144)
        tryCatch(
            {
                md_plot <- create_single_cohort_forest_plot(
                    subgroup_results = md_forest_plot_results,
                    outcome_name = "Tumor Height Change",
                    cohort_name = "Test Cohort",
                    treatment_labels = TREATMENT_LABELS,
                    variable_order = names(md_forest_plot_results),
                    effect_measure = "MD",
                    favours_labels = FAVOURS_LABELS,
                    title = "Subgroup Analysis: Tumor Height Change (Test Cohort)"
                )
                expect_equal(attr(md_plot, "forest_row_count"), nrow(md_plot_data$data_frame))
                plot(md_plot)
            },
            finally = grDevices::dev.off()
        )
    })

    unlink(c(binary_png, md_png))
})
