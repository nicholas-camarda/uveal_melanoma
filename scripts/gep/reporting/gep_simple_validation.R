# Simple GEP Validation (Project Goals)
# Computes expected vs actual 5-year MFS/MSS by GEP class and saves outputs

#' Build a reader-facing simple GEP validation plot
#'
#' Build a side-by-side expected vs actual 5-year survival point plot for a
#' simple GEP class comparison.
#'
#' @param results_df Data frame of class-level expected and actual rates.
#' @param title_text Plot title.
#' @param cohort_label Optional subtitle identifying the cohort.
#' @return A `ggplot` object.
build_simple_gep_plot <- function(results_df, title_text, cohort_label = NULL) {
    x_label_map <- if ("plot_x_label" %in% names(results_df)) {
        stats::setNames(results_df$plot_x_label, results_df$gep_class_simple)
    } else {
        waiver()
    }

    rate_range <- range(c(results_df$expected_rate, results_df$actual_rate), na.rm = TRUE)
    y_padding <- max(diff(rate_range) * 0.2, 0.03)
    y_min <- max(0, rate_range[1] - y_padding)
    y_max <- min(1, rate_range[2] + y_padding)

    ggplot(results_df, aes(x = gep_class_simple)) +
        geom_segment(
            aes(
                x = gep_class_simple, xend = gep_class_simple,
                y = expected_rate, yend = actual_rate
            ),
            linetype = "dashed",
            linewidth = 0.8,
            alpha = 0.6,
            color = "gray45"
        ) +
        geom_point(aes(y = expected_rate, color = "Expected"), size = 4.5) +
        geom_point(aes(y = actual_rate, color = "Actual"), size = 4.5) +
        labs(
            title = title_text,
            subtitle = cohort_label,
            x = "GEP Class",
            y = "Survival Rate",
            color = "Rate Type"
        ) +
        scale_y_continuous(
            limits = c(y_min, y_max),
            expand = expansion(mult = c(0.01, 0.02))
        ) +
        scale_x_discrete(
            labels = x_label_map,
            expand = expansion(mult = c(0.2, 0.25))
        ) +
        scale_color_manual(
            values = {
                pal <- get_qualitative_palette(2)
                names(pal) <- c("Expected", "Actual")
                pal
            },
            breaks = c("Actual", "Expected")
        ) +
        guides(color = guide_legend(override.aes = list(size = 5))) +
        theme_classic(base_size = 18) +
        theme(
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            plot.title = element_text(size = 22, face = "bold", margin = margin(b = 10)),
            plot.subtitle = element_text(size = 17, margin = margin(b = 8)),
            axis.title = element_text(size = 19),
            axis.text = element_text(size = 16),
            axis.text.x = element_text(lineheight = 0.95),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.title = element_text(size = 17, face = "bold"),
            legend.text = element_text(size = 15),
            legend.margin = margin(),
            legend.box.margin = margin(b = 2),
            plot.margin = margin(8, 18, 26, 8),
            axis.line = element_line(linewidth = 0.9),
            axis.ticks = element_line(linewidth = 0.9)
        ) +
        coord_cartesian(clip = "off")
}

#' Save simple GEP validation plots
#'
#' Write the expected-vs-actual MFS and MSS validation plots to disk with
#' enough vertical space for the multi-line x-axis labels.
#'
#' @param mfs_results Data frame of MFS class-level expected/actual rates.
#' @param mss_results Data frame of MSS class-level expected/actual rates.
#' @param mfs_output_dir Directory path to save the MFS image.
#' @param mss_output_dir Directory path to save the MSS image.
#' @param prefix Filename prefix for saved files.
#' @param dataset_name Optional dataset identifier used for cohort labeling.
#' @return Invisibly returns `NULL` after writing files.
create_simple_gep_plots <- function(mfs_results, mss_results, mfs_output_dir, mss_output_dir, prefix, dataset_name = NULL) {
    cohort_label <- format_objective4_gep_cohort_label(dataset_name)

    simple_plot_width <- 10
    simple_plot_height <- 7

    validation_mfs_dir <- ensure_output_dir(mfs_output_dir)
    validation_mss_dir <- ensure_output_dir(mss_output_dir)

    mfs_plot <- build_simple_gep_plot(
        mfs_results,
        "5-Year MFS: Expected vs Actual Rates",
        cohort_label = cohort_label
    )

    ggsave(file.path(validation_mfs_dir, paste0(prefix, "simple_mfs_validation.png")),
        mfs_plot,
        width = simple_plot_width, height = simple_plot_height, dpi = PLOT_DPI, bg = "white"
    )

    mss_plot <- build_simple_gep_plot(
        mss_results,
        "5-Year MSS: Expected vs Actual Rates",
        cohort_label = cohort_label
    )

    ggsave(file.path(validation_mss_dir, paste0(prefix, "simple_mss_validation.png")),
        mss_plot,
        width = simple_plot_width, height = simple_plot_height, dpi = PLOT_DPI, bg = "white"
    )
}

#' Create simple GEP report
#'
#' Write a text-based summary of 5-year expected vs actual survival by GEP
#' class for MFS and MSS, including an overall summary table.
#'
#' @param mfs_results Data frame of MFS class-level results
#' @param mss_results Data frame of MSS class-level results
#' @param overall_summary Data frame with overall expected/actual summaries
#' @param output_dir Directory to write the report
#' @param prefix Filename prefix for saved files
#' @return Invisibly returns NULL after writing files
create_simple_gep_report <- function(mfs_results, mss_results, overall_summary, output_dir, prefix) {
    report_content <- c(
        "SIMPLE GEP VALIDATION REPORT",
        "===========================",
        "",
        "Goal:",
        "Compare actual rates vs expected reported rates of 5-year MFS and MSS",
        "",
        "METASTASIS-FREE SURVIVAL (MFS) - 5 YEAR:",
        "----------------------------------------"
    )
    for (i in seq_len(nrow(mfs_results))) {
        row <- mfs_results[i, ]
        report_content <- c(
            report_content,
            sprintf("  %s (n=%d):", row$gep_class_simple, row$n),
            sprintf("    Expected: %.3f (%.1f%%)", row$expected_rate, row$expected_rate * 100),
            sprintf("    Actual:   %.3f (%.1f%%)", row$actual_rate, row$actual_rate * 100),
            sprintf("    Difference: %.3f (%.1f%%)", row$difference, row$percent_difference),
            ""
        )
    }
    report_content <- c(
        report_content,
        "MELANOMA-SPECIFIC SURVIVAL (MSS) - 5 YEAR:",
        "------------------------------------------"
    )
    for (i in seq_len(nrow(mss_results))) {
        row <- mss_results[i, ]
        report_content <- c(
            report_content,
            sprintf("  %s (n=%d):", row$gep_class_simple, row$n),
            sprintf("    Expected: %.3f (%.1f%%)", row$expected_rate, row$expected_rate * 100),
            sprintf("    Actual:   %.3f (%.1f%%)", row$actual_rate, row$actual_rate * 100),
            sprintf("    Difference: %.3f (%.1f%%)", row$difference, row$percent_difference),
            ""
        )
    }
    report_content <- c(
        report_content,
        "OVERALL SUMMARY:",
        "---------------",
        sprintf(
            "MFS - Overall: Expected %.1f%%, Actual %.1f%%, Difference %.1f%%",
            overall_summary$overall_expected[1] * 100,
            overall_summary$overall_actual[1] * 100,
            overall_summary$overall_percent_difference[1]
        ),
        sprintf(
            "MSS - Overall: Expected %.1f%%, Actual %.1f%%, Difference %.1f%%",
            overall_summary$overall_expected[2] * 100,
            overall_summary$overall_actual[2] * 100,
            overall_summary$overall_percent_difference[2]
        ),
        "",
        "INTERPRETATION:",
        "--------------",
        "Positive differences indicate GEP predictions were conservative (actual survival better than predicted)",
        "Negative differences indicate GEP predictions were optimistic (actual survival worse than predicted)",
        "Values close to 0 indicate good predictive accuracy"
    )
    writeLines(report_content, file.path(output_dir, paste0(prefix, "simple_gep_validation_report.txt")))
}

#' Simple GEP validation - Actual vs Expected rates
#'
#' Compute 5-year expected vs actual survival by GEP class for MFS and MSS,
#' save summary tables and plots, and return the key data frames.
#'
#' @param data Data frame with required GEP predictions and outcomes
#' @param output_dirs List of output directories (expects elements `obj4_mfs` and `obj4_mss`)
#' @param prefix Filename prefix for saved files
#' @return A list with `mfs_results`, `mss_results`, and `overall_summary`
simple_gep_validation <- function(data, output_dirs, prefix, dataset_name = NULL) {
    logger::log_info("Starting SIMPLE GEP validation (Project Goals)")

    data <- refresh_gep_analysis_flags(data)

    # Resolve directories
    mfs_dir <- output_dirs$obj4_mfs
    mss_dir <- output_dirs$obj4_mss
    mfs_validation_dir <- output_dirs$obj4_mfs_validation %||% mfs_dir
    mss_validation_dir <- output_dirs$obj4_mss_validation %||% mss_dir
    gep_base_dir <- dirname(mfs_dir)
    unified_dir <- file.path(gep_base_dir, "unified_summary")

    for (d in c(mfs_dir, mss_dir, mfs_validation_dir, mss_validation_dir, unified_dir)) {
        if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }

    expected_mfs_col <- if ("expected_mfs_5yr" %in% names(data)) "expected_mfs_5yr" else "biopsy1_gep_mfs"
    expected_mss_col <- if ("expected_mss_5yr" %in% names(data)) "expected_mss_5yr" else "biopsy1_gep_mss"
    mss_event_col <- if ("mss_event_5yr" %in% names(data)) "mss_event_5yr" else NULL
    mss_time_col <- dplyr::case_when(
        "tt_death_months" %in% names(data) ~ "tt_death_months",
        "tt_death_years" %in% names(data) ~ "tt_death_years",
        TRUE ~ NA_character_
    )

    mfs_data <- data %>%
        filter(
            !is.na(.data[[expected_mfs_col]]),
            .data[[expected_mfs_col]] >= 0 & .data[[expected_mfs_col]] <= 1
        ) %>%
        restore_gep_display_variables(dataset_name = dataset_name) %>%
        filter(
            if ("mfs_analysis_eligible" %in% names(.)) mfs_analysis_eligible else !is.na(tt_mets_months) & !is.na(mets_event),
            !is.na(.data[[expected_mfs_col]]),
            !is.na(tt_mets_months)
        ) %>%
        mutate(
            expected_mfs_5yr = .data[[expected_mfs_col]],
            time_to_5yr = pmin(tt_mets_months, 60)
        )

    mfs_expected_by_class <- mfs_data %>%
        group_by(gep_class_simple) %>%
        summarise(
            n = n(),
            expected_rate = mean(expected_mfs_5yr, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        dplyr::mutate(gep_class_simple = as.character(.data$gep_class_simple))

    mfs_observed_by_class <- split(mfs_data, as.character(mfs_data$gep_class_simple)) %>%
        lapply(function(class_data) {
            km_metrics <- estimate_mfs_km_at_horizon(
                data = class_data,
                timepoint_months = 60
            )
            data.frame(
                gep_class_simple = as.character(class_data$gep_class_simple[[1]]),
                actual_rate = km_metrics$survival,
                actual_rate_ci_lower = km_metrics$survival_ci_lower,
                actual_rate_ci_upper = km_metrics$survival_ci_upper,
                stringsAsFactors = FALSE
            )
        }) %>%
        dplyr::bind_rows()

    mfs_results <- mfs_expected_by_class %>%
        dplyr::left_join(
            mfs_observed_by_class,
            by = "gep_class_simple"
        ) %>%
        mutate(
            difference = actual_rate - expected_rate,
            percent_difference = (difference / expected_rate) * 100
        )

    mfs_results <- mfs_results %>%
        dplyr::left_join(
            build_objective4_simple_mfs_plot_annotations(mfs_data),
            by = "gep_class_simple"
        )

    mss_data <- data %>%
        filter(
            !is.na(.data[[expected_mss_col]]),
            .data[[expected_mss_col]] >= 0 & .data[[expected_mss_col]] <= 1
        ) %>%
        restore_gep_display_variables(dataset_name = dataset_name) %>%
        filter(
            if ("mss_analysis_eligible" %in% names(.)) mss_analysis_eligible else !is.na(melanoma_death_event) & !is.na(.data[[mss_time_col]]),
            !is.na(.data[[expected_mss_col]]),
            !is.na(.data[[mss_time_col]])
        ) %>%
        mutate(
            expected_mss_5yr = .data[[expected_mss_col]],
            actual_mss_5yr = if (!is.null(mss_event_col)) {
                1 - .data[[mss_event_col]]
            } else if (mss_time_col == "tt_death_months") {
                ifelse(tt_death_months > 60 | (tt_death_months <= 60 & melanoma_death_event == 0), 1, 0)
            } else {
                ifelse(tt_death_years > 5 | (tt_death_years <= 5 & melanoma_death_event == 0), 1, 0)
            },
            time_to_5yr = if (mss_time_col == "tt_death_months") pmin(tt_death_months, 60) else pmin(tt_death_years * 12, 60)
        )

    mss_results <- mss_data %>%
        group_by(gep_class_simple) %>%
        summarise(
            n = n(),
            expected_rate = mean(expected_mss_5yr, na.rm = TRUE),
            actual_rate = mean(actual_mss_5yr, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        mutate(
            difference = actual_rate - expected_rate,
            percent_difference = (difference / expected_rate) * 100
        )

    logger::log_info(sprintf(
        "Analysis dataset: %d patients with valid MFS predictions; %d patients with valid MSS predictions",
        nrow(mfs_data),
        nrow(mss_data)
    ))

    logger::log_info("Creating simple validation summary")
    overall_mfs_km <- estimate_mfs_km_at_horizon(
        data = mfs_data,
        timepoint_months = 60
    )
    overall_summary <- data.frame(
        outcome = c("MFS", "MSS"),
        total_patients = c(nrow(mfs_data), nrow(mss_data)),
        overall_expected = c(
            mean(mfs_data$expected_mfs_5yr, na.rm = TRUE),
            mean(mss_data$expected_mss_5yr, na.rm = TRUE)
        ),
        overall_actual = c(
            overall_mfs_km$survival,
            mean(mss_data$actual_mss_5yr, na.rm = TRUE)
        ),
        overall_difference = c(
            overall_mfs_km$survival - mean(mfs_data$expected_mfs_5yr, na.rm = TRUE),
            mean(mss_data$actual_mss_5yr, na.rm = TRUE) - mean(mss_data$expected_mss_5yr, na.rm = TRUE)
        ),
        stringsAsFactors = FALSE
    ) %>%
        mutate(
            overall_percent_difference = (overall_difference / overall_expected) * 100
        )

    write_gep_workbook(list(
        "MFS_By_Class" = mfs_results,
        "MSS_By_Class" = mss_results,
        "Overall_Summary" = overall_summary
    ), file.path(unified_dir, paste0(prefix, "simple_gep_validation.xlsx")))

    create_simple_gep_plots(
        mfs_results,
        mss_results,
        mfs_validation_dir,
        mss_validation_dir,
        prefix,
        dataset_name = dataset_name
    )
    create_simple_gep_report(mfs_results, mss_results, overall_summary, unified_dir, prefix)

    logger::log_info("Simple GEP validation completed")
    return(list(
        mfs_results = mfs_results,
        mss_results = mss_results,
        overall_summary = overall_summary
    ))
}
