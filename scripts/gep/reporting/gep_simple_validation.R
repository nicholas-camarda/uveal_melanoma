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
    mfs_table <- data.frame(
        Class = mfs_results$gep_class_simple,
        n = as.character(mfs_results$n),
        Expected = sprintf("%.3f (%.1f%%)", mfs_results$expected_rate, mfs_results$expected_rate * 100),
        Actual = sprintf("%.3f (%.1f%%)", mfs_results$actual_rate, mfs_results$actual_rate * 100),
        Difference = sprintf("%.3f (%.1f%%)", mfs_results$difference, mfs_results$percent_difference),
        stringsAsFactors = FALSE
    )
    mss_table <- data.frame(
        Class = mss_results$gep_class_simple,
        n = as.character(mss_results$n),
        Expected = sprintf("%.3f (%.1f%%)", mss_results$expected_rate, mss_results$expected_rate * 100),
        Actual = sprintf("%.3f (%.1f%%)", mss_results$actual_rate, mss_results$actual_rate * 100),
        Difference = sprintf("%.3f (%.1f%%)", mss_results$difference, mss_results$percent_difference),
        stringsAsFactors = FALSE
    )
    overall_table <- data.frame(
        Outcome = c("MFS", "MSS"),
        Expected = sprintf("%.1f%%", overall_summary$overall_expected * 100),
        Actual = sprintf("%.1f%%", overall_summary$overall_actual * 100),
        Difference = sprintf("%.1f%%", overall_summary$overall_percent_difference),
        stringsAsFactors = FALSE
    )
    report_content <- c(
        md_heading("Simple GEP Validation Report", 1L),
        "",
        md_heading("Goal", 2L),
        md_bullet("Compare actual rates vs expected reported rates of 5-year MFS and MSS."),
        "",
        md_heading("Metastasis-Free Survival (MFS) - 5 Year", 2L),
        md_table(mfs_table),
        "",
        md_heading("Melanoma-Specific Survival (MSS) - 5 Year", 2L),
        md_table(mss_table),
        "",
        md_heading("Overall Summary", 2L),
        md_table(overall_table),
        "",
        md_heading("Interpretation", 2L),
        md_bullet("Positive differences indicate GEP predictions were conservative (actual survival better than predicted)."),
        md_bullet("Negative differences indicate GEP predictions were optimistic (actual survival worse than predicted)."),
        md_bullet("Values close to 0 indicate good predictive accuracy.")
    )
    writeLines(report_content, file.path(output_dir, paste0(prefix, "simple_gep_validation_report.md")))
}

#' Summarize Simple MSS Actual Survival by GEP Class
#'
#' Convert the primary competing-risk MSS observed-risk definition into the
#' simple reader-facing expected-vs-actual survival table.
#'
#' @param mss_data Data frame filtered to MSS-eligible rows.
#' @param time_var Character name of the month-scale death follow-up column.
#' @param timepoint_months Numeric evaluation horizon in months.
#' @return Data frame with class-level observed survival and method metadata.
summarize_simple_mss_actual_by_class <- function(mss_data,
                                                 time_var = "simple_mss_time_months",
                                                 timepoint_months = 60) {
    split(mss_data, as.character(mss_data$gep_class_simple)) %>%
        lapply(function(class_data) {
            cif_metrics <- estimate_mss_cif_at_horizon(
                data = class_data,
                timepoint_months = timepoint_months,
                time_var = time_var,
                melanoma_event_var = "melanoma_death_event",
                competing_event_var = "competing_death_event"
            )
            data.frame(
                gep_class_simple = as.character(class_data$gep_class_simple[[1]]),
                actual_rate = 1 - cif_metrics$cif,
                observed_melanoma_death_risk = cif_metrics$cif,
                actual_rate_method = cif_metrics$observed_method,
                observed_melanoma_deaths_by_horizon = cif_metrics$raw_events_by_horizon,
                estimand = "5-year MSS survival derived from melanoma-death CIF with non-melanoma death as a competing event",
                analysis_tier = "reader_facing_primary_aligned",
                stringsAsFactors = FALSE
            )
        }) %>%
        dplyr::bind_rows()
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
            simple_mss_time_months = if (mss_time_col == "tt_death_months") tt_death_months else tt_death_years * 12,
            time_to_5yr = if (mss_time_col == "tt_death_months") pmin(tt_death_months, 60) else pmin(tt_death_years * 12, 60)
        )

    mss_expected_by_class <- mss_data %>%
        group_by(gep_class_simple) %>%
        summarise(
            n = n(),
            expected_rate = mean(expected_mss_5yr, na.rm = TRUE),
            .groups = "drop"
        )

    mss_actual_by_class <- summarize_simple_mss_actual_by_class(
        mss_data = mss_data,
        time_var = "simple_mss_time_months",
        timepoint_months = 60
    )

    mss_results <- mss_expected_by_class %>%
        dplyr::left_join(
            mss_actual_by_class,
            by = "gep_class_simple"
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
    overall_mss_cif <- estimate_mss_cif_at_horizon(
        data = mss_data,
        timepoint_months = 60,
        time_var = "simple_mss_time_months",
        melanoma_event_var = "melanoma_death_event",
        competing_event_var = "competing_death_event"
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
            1 - overall_mss_cif$cif
        ),
        overall_difference = c(
            overall_mfs_km$survival - mean(mfs_data$expected_mfs_5yr, na.rm = TRUE),
            (1 - overall_mss_cif$cif) - mean(mss_data$expected_mss_5yr, na.rm = TRUE)
        ),
        actual_rate_method = c(
            "kaplan_meier_at_horizon",
            overall_mss_cif$observed_method
        ),
        estimand = c(
            "5-year metastasis-free survival using Kaplan-Meier",
            "5-year MSS survival derived from melanoma-death CIF with non-melanoma death as a competing event"
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
