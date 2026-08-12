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

collect_forest_text_labels <- function(grob) {
    labels <- if (!is.null(grob$label)) as.character(grob$label) else character()
    children <- c(
        if (!is.null(grob$grobs)) grob$grobs else list(),
        if (!is.null(grob$children)) as.list(grob$children) else list()
    )
    for (child in children) {
        labels <- c(labels, collect_forest_text_labels(child))
    }
    labels
}

test_that("configured forest arrows match the GKSRS-versus-PBT adverse-outcome contrast", {
    plot_obj <- create_single_cohort_forest_plot(
        subgroup_results = binary_forest_plot_results,
        outcome_name = "Local Recurrence",
        treatment_labels = TREATMENT_LABELS,
        variable_order = names(binary_forest_plot_results),
        effect_measure = "HR",
        favours_labels = FAVOURS_LABELS
    )

    arrow_labels <- collect_forest_text_labels(plot_obj)
    arrow_labels <- arrow_labels[grepl("^Favors ", arrow_labels)]

    expect_identical(arrow_labels, c("Favors GKSRS", "Favors PBT"))
})

test_that("default forest arrows match the GKSRS-versus-PBT adverse-outcome contrast", {
    plot_obj <- create_single_cohort_forest_plot(
        subgroup_results = binary_forest_plot_results,
        outcome_name = "Local Recurrence",
        treatment_labels = TREATMENT_LABELS,
        variable_order = names(binary_forest_plot_results),
        effect_measure = "HR"
    )

    arrow_labels <- collect_forest_text_labels(plot_obj)
    arrow_labels <- arrow_labels[grepl("^Favors ", arrow_labels)]

    expect_identical(arrow_labels, c("Favors GKSRS", "Favors PBT"))
})

test_that("continuous tumor-height arrows match the signed change contrast", {
    plot_obj <- create_single_cohort_forest_plot(
        subgroup_results = md_forest_plot_results,
        outcome_name = "Tumor Height Change",
        treatment_labels = TREATMENT_LABELS,
        variable_order = names(md_forest_plot_results),
        effect_measure = "MD"
    )

    arrow_labels <- collect_forest_text_labels(plot_obj)
    arrow_labels <- arrow_labels[grepl("^Favors ", arrow_labels)]

    # height_change = follow-up minus baseline: more-negative values mean
    # greater shrinkage, so the left side favors the GKSRS coefficient.
    expect_identical(arrow_labels, c("Favors GKSRS", "Favors PBT"))
})

test_that("propensity forest axis uses readable symmetric log ticks", {
    propensity_results <- tibble::tibble(
        outcome = c(
            "Local Recurrence", "Metastatic Progression",
            "Overall Survival", "Progression-Free Survival"
        ),
        n = 164,
        pbt_n = 100,
        pbt_events = c(10, 19, 27, 33),
        gksrs_n = 64,
        gksrs_events = c(8, 9, 12, 18),
        estimate = c(1.53, 0.71, 0.87, 0.94),
        conf_low = c(0.57, 0.30, 0.41, 0.50),
        conf_high = c(4.10, 1.67, 1.84, 1.78),
        p_value = c(0.40, 0.43, 0.71, 0.85)
    )
    plot_obj <- create_objective1_propensity_forest_plot(propensity_results)
    xaxis <- plot_obj$grobs[[grep("^xaxis-", plot_obj$layout$name)]]

    expect_identical(
        as.character(xaxis$children$label$label),
        c("0.25", "0.5", "1", "2", "4")
    )
})

test_that("forest direction arrows stay anchored to the plotted axis", {
    plot_obj <- create_single_cohort_forest_plot(
        subgroup_results = binary_forest_plot_results,
        outcome_name = "Local Recurrence",
        treatment_labels = TREATMENT_LABELS,
        variable_order = names(binary_forest_plot_results),
        effect_measure = "HR"
    )
    arrow_grob <- plot_obj$grobs[[grep("^arrow-", plot_obj$layout$name)]]

    expect_identical(arrow_grob$children$arrow.text.left$just, "left")
    expect_identical(arrow_grob$children$arrow.text.right$just, "right")
    expect_identical(grid::unitType(arrow_grob$children$arrow.left$x0), "npc")
    expect_identical(grid::unitType(arrow_grob$children$arrow.right$x0), "npc")
    expect_equal(as.numeric(arrow_grob$children$arrow.left$x0), 0)
    expect_equal(as.numeric(arrow_grob$children$arrow.right$x0), 1)
})

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
