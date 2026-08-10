# Simple GEP Validation (Project Goals)
# Computes expected vs actual 5-year MFS/MSS by GEP class and saves outputs

#' Build simple validation caption text
#'
#' @param results_df Data frame of class-level expected and actual rates.
#'
#' @return Character scalar caption explaining reference marks and, when
#'   available, treatment composition.
build_simple_gep_plot_caption <- function(results_df) {
    caption_lines <- c(
        "Dashed diagonal: observed = predicted 5-year survival.",
        "Gray vertical segments: observed-predicted gap."
    )

    if ("treatment_mix" %in% names(results_df)) {
        treatment_rows <- results_df %>%
            dplyr::filter(!is.na(.data$treatment_mix) & nzchar(.data$treatment_mix)) %>%
            dplyr::arrange(.data$gep_class_simple)

        if (nrow(treatment_rows) > 0) {
            caption_lines <- c(
                caption_lines,
                "Treatment mix among MFS-eligible rows:",
                sprintf("%s: %s", treatment_rows$gep_class_simple, treatment_rows$treatment_mix)
            )
        }
    }

    paste(caption_lines, collapse = "\n")
}

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
    class_palette <- get_gep_class_palette(results_df$gep_class_simple)
    rate_range <- range(c(results_df$expected_rate, results_df$actual_rate), na.rm = TRUE)
    lower_limit <- max(0, floor((rate_range[1] - 0.03) * 20) / 20)
    upper_limit <- min(1.01, ceiling((rate_range[2] + 0.02) * 20) / 20)
    axis_span <- max(upper_limit - lower_limit, 0.15)
    label_offset_x <- min(0.014, axis_span * 0.04)
    label_offset_y <- min(0.014, axis_span * 0.04)
    point_label <- if ("class_event_label" %in% names(results_df)) {
        sprintf(
            "%s\n%s",
            results_df$gep_class_simple,
            results_df$class_event_label
        )
    } else if ("observed_melanoma_deaths_by_horizon" %in% names(results_df)) {
        sprintf(
            "%s\n5-year melanoma deaths: %d/%d",
            results_df$gep_class_simple,
            results_df$observed_melanoma_deaths_by_horizon,
            results_df$n
        )
    } else if ("n" %in% names(results_df)) {
        sprintf("%s\nn=%d", results_df$gep_class_simple, results_df$n)
    } else {
        results_df$gep_class_simple
    }

    annotation_df <- results_df %>%
        dplyr::mutate(
            point_label = point_label,
            horizontal_direction = dplyr::if_else(
                .data$expected_rate >= stats::median(.data$expected_rate, na.rm = TRUE),
                -1,
                1
            ),
            vertical_direction = dplyr::if_else(
                .data$actual_rate >= stats::median(.data$actual_rate, na.rm = TRUE),
                -1,
                1
            ),
            label_x = .data$expected_rate + (.data$horizontal_direction * label_offset_x),
            label_y = .data$actual_rate + (.data$vertical_direction * label_offset_y),
            label_x = pmin(upper_limit - 0.008, pmax(lower_limit + 0.008, .data$label_x)),
            label_y = pmin(upper_limit - 0.008, pmax(lower_limit + 0.008, .data$label_y)),
            label_hjust = dplyr::if_else(.data$horizontal_direction < 0, 1, 0),
            label_vjust = dplyr::if_else(.data$vertical_direction < 0, 1, 0),
            connector_x = .data$expected_rate + (.data$horizontal_direction * label_offset_x * 0.8),
            connector_y = .data$actual_rate + (.data$vertical_direction * label_offset_y * 0.8),
            connector_x = pmin(upper_limit - 0.008, pmax(lower_limit + 0.008, .data$connector_x)),
            connector_y = pmin(upper_limit - 0.008, pmax(lower_limit + 0.008, .data$connector_y))
        )

    if (nrow(annotation_df) > 1) {
        overlap_mask <- abs(annotation_df$label_y - dplyr::lag(annotation_df$label_y)) < (label_offset_y * 0.85)
        overlap_mask[is.na(overlap_mask)] <- FALSE
        if (any(overlap_mask)) {
            annotation_df$label_y[overlap_mask] <- pmin(
                upper_limit - 0.008,
                pmax(
                    lower_limit + 0.008,
                    annotation_df$label_y[overlap_mask] + (label_offset_y * 0.5 * annotation_df$vertical_direction[overlap_mask])
                )
            )
        }
    }

    ggplot(results_df, aes(x = expected_rate, y = actual_rate, color = gep_class_simple)) +
        geom_abline(
            slope = 1,
            intercept = 0,
            linetype = "dashed",
            linewidth = 0.9,
            color = "gray65",
            show.legend = FALSE
        ) +
        geom_segment(
            aes(
                x = .data$expected_rate,
                xend = .data$expected_rate,
                y = .data$expected_rate,
                yend = .data$actual_rate
            ),
            inherit.aes = FALSE,
            linetype = "dashed",
            linewidth = 0.8,
            alpha = 0.7,
            color = "gray60"
        ) +
        geom_point(size = 5.3) +
        geom_segment(
            data = annotation_df,
            aes(
                x = .data$expected_rate,
                y = .data$actual_rate,
                xend = .data$connector_x,
                yend = .data$connector_y
            ),
            inherit.aes = FALSE,
            linewidth = 0.5,
            color = "gray55",
            show.legend = FALSE
        ) +
        geom_label(
            data = annotation_df,
            aes(
                x = .data$label_x,
                y = .data$label_y,
                label = .data$point_label,
                hjust = .data$label_hjust,
                vjust = .data$label_vjust
            ),
            inherit.aes = FALSE,
            color = "gray25",
            fill = "white",
            label.size = 0.18,
            label.padding = grid::unit(0.1, "lines"),
            label.r = grid::unit(0.05, "lines"),
            size = 3.3,
            lineheight = 0.95,
            show.legend = FALSE
        ) +
        labs(
            title = title_text,
            subtitle = cohort_label,
            x = "Predicted 5-year survival",
            y = "Observed 5-year survival",
            color = "GEP class",
            caption = build_simple_gep_plot_caption(results_df)
        ) +
        scale_x_continuous(
            limits = c(lower_limit, upper_limit),
            labels = scales::label_percent(accuracy = 1),
            expand = expansion(mult = c(0.03, 0.04))
        ) +
        scale_y_continuous(
            limits = c(lower_limit, upper_limit),
            labels = scales::label_percent(accuracy = 1),
            expand = expansion(mult = c(0.03, 0.06))
        ) +
        scale_color_manual(values = class_palette) +
        guides(
            color = guide_legend(order = 1, override.aes = list(size = 5.2))
        ) +
        theme_classic(base_size = 15) +
        theme(
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            plot.title = element_text(size = 18, face = "bold", margin = margin(b = 8)),
            plot.subtitle = element_text(size = 14, margin = margin(b = 8)),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 13),
            plot.caption = element_text(size = 10.5, hjust = 0, color = "gray35", lineheight = 1.05, margin = margin(t = 12)),
            legend.position = "top",
            legend.box = "vertical",
            legend.direction = "horizontal",
            legend.title = element_text(size = 14, face = "bold"),
            legend.text = element_text(size = 13),
            legend.margin = margin(),
            legend.box.margin = margin(b = 2),
            plot.margin = margin(8, 14, 18, 8),
            axis.line = element_line(linewidth = 0.9),
            axis.ticks = element_line(linewidth = 0.9)
        ) +
        coord_equal(clip = "off")
}

#' Build a Poster-Ready Simple GEP MFS Validation Plot
#'
#' Creates a compact observed-vs-predicted MFS panel for poster placement. The
#' plot removes the long explanatory caption and labels classes directly so the
#' panel can be placed below KM curves without shrinking unreadably.
#'
#' @param results_df Data frame of MFS class-level expected and actual rates.
#' @param cohort_label Optional cohort label used as the panel title.
#' @param fixed_limits Numeric length-two axis limits shared across poster panels.
#' @return A `ggplot` object.
build_simple_gep_poster_mfs_plot <- function(results_df,
                                             cohort_label = NULL,
                                             fixed_limits = NULL) {
    if (is.null(results_df) || nrow(results_df) == 0) {
        stop("Poster simple MFS validation plot requires non-empty results.", call. = FALSE)
    }
    if (is.null(fixed_limits)) {
        rate_range <- range(c(results_df$expected_rate, results_df$actual_rate), na.rm = TRUE)
        fixed_limits <- c(
            min(0.35, max(0, floor((rate_range[[1]] - 0.03) * 20) / 20)),
            min(1.01, max(1, ceiling((rate_range[[2]] + 0.02) * 20) / 20))
        )
    }
    if (!is.numeric(fixed_limits) || length(fixed_limits) != 2 || fixed_limits[[1]] >= fixed_limits[[2]]) {
        stop("fixed_limits must be a numeric length-two vector with increasing values.", call. = FALSE)
    }

    class_palette <- get_gep_class_palette(results_df$gep_class_simple)
    axis_span <- fixed_limits[[2]] - fixed_limits[[1]]
    label_offset <- min(0.035, axis_span * 0.06)

    point_label <- if ("class_event_label" %in% names(results_df)) {
        sprintf("%s\n%s", results_df$gep_class_simple, results_df$class_event_label)
    } else if ("n" %in% names(results_df)) {
        sprintf("%s\nn=%d", results_df$gep_class_simple, results_df$n)
    } else {
        results_df$gep_class_simple
    }

    annotation_df <- results_df %>%
        dplyr::mutate(
            point_label = point_label,
            horizontal_direction = dplyr::if_else(
                .data$expected_rate >= stats::median(.data$expected_rate, na.rm = TRUE),
                -1,
                1
            ),
            vertical_direction = dplyr::if_else(
                .data$actual_rate >= stats::median(.data$actual_rate, na.rm = TRUE),
                -1,
                1
            ),
            label_x = .data$expected_rate + (.data$horizontal_direction * label_offset),
            label_y = .data$actual_rate + (.data$vertical_direction * label_offset),
            label_x = pmin(fixed_limits[[2]] - 0.01, pmax(fixed_limits[[1]] + 0.01, .data$label_x)),
            label_y = pmin(fixed_limits[[2]] - 0.01, pmax(fixed_limits[[1]] + 0.01, .data$label_y)),
            label_hjust = dplyr::if_else(.data$horizontal_direction < 0, 1, 0),
            label_vjust = dplyr::if_else(.data$vertical_direction < 0, 1, 0)
        )

    axis_breaks <- seq(0.4, 1.0, by = 0.2)
    axis_breaks <- axis_breaks[axis_breaks >= fixed_limits[[1]] & axis_breaks <= fixed_limits[[2]]]

    ggplot2::ggplot(results_df, ggplot2::aes(x = .data$expected_rate, y = .data$actual_rate, color = .data$gep_class_simple)) +
        ggplot2::geom_abline(
            slope = 1,
            intercept = 0,
            linetype = "dashed",
            linewidth = 0.9,
            color = "gray65",
            show.legend = FALSE
        ) +
        ggplot2::geom_segment(
            ggplot2::aes(
                x = .data$expected_rate,
                xend = .data$expected_rate,
                y = .data$expected_rate,
                yend = .data$actual_rate
            ),
            inherit.aes = FALSE,
            linetype = "dashed",
            linewidth = 0.7,
            alpha = 0.65,
            color = "gray60"
        ) +
        ggplot2::geom_point(size = 5.8) +
        ggplot2::geom_label(
            data = annotation_df,
            ggplot2::aes(
                x = .data$label_x,
                y = .data$label_y,
                label = .data$point_label,
                hjust = .data$label_hjust,
                vjust = .data$label_vjust
            ),
            inherit.aes = FALSE,
            color = "gray20",
            fill = "white",
            label.size = 0.18,
            label.padding = grid::unit(0.12, "lines"),
            label.r = grid::unit(0.04, "lines"),
            size = 4.2,
            lineheight = 0.95,
            show.legend = FALSE
        ) +
        ggplot2::labs(
            title = cohort_label %||% "Simple GEP MFS validation",
            x = "Predicted 5-year MFS",
            y = "Observed 5-year MFS",
            color = "GEP class"
        ) +
        ggplot2::scale_x_continuous(
            limits = fixed_limits,
            breaks = axis_breaks,
            labels = scales::label_percent(accuracy = 1),
            expand = ggplot2::expansion(mult = c(0.03, 0.04))
        ) +
        ggplot2::scale_y_continuous(
            limits = fixed_limits,
            breaks = axis_breaks,
            labels = scales::label_percent(accuracy = 1),
            expand = ggplot2::expansion(mult = c(0.03, 0.04))
        ) +
        ggplot2::scale_color_manual(values = class_palette, guide = "none") +
        ggplot2::theme_classic(base_size = 16) +
        ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = "white", color = NA),
            panel.background = ggplot2::element_rect(fill = "white", color = NA),
            plot.title = ggplot2::element_text(size = 20, face = "bold", margin = ggplot2::margin(b = 6)),
            axis.title = ggplot2::element_text(size = 18, face = "bold"),
            axis.text = ggplot2::element_text(size = 15, color = "gray25"),
            plot.margin = ggplot2::margin(6, 12, 8, 6),
            axis.line = ggplot2::element_line(linewidth = 0.9),
            axis.ticks = ggplot2::element_line(linewidth = 0.9)
        ) +
        ggplot2::coord_equal(clip = "off")
}

#' Save simple GEP validation plots
#'
#' Write the expected-vs-actual MFS and MSS validation plots to disk with
#' enough space for point annotations and the explanatory caption.
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

    simple_plot_width <- 8.2
    simple_plot_height <- 6.4

    validation_mfs_dir <- ensure_output_dir(mfs_output_dir)
    validation_mss_dir <- ensure_output_dir(mss_output_dir)

    mfs_plot <- build_simple_gep_plot(
        mfs_results,
        "Observed vs Predicted 5-Year MFS",
        cohort_label = cohort_label
    )

    ggsave(file.path(validation_mfs_dir, paste0(prefix, "simple_mfs_validation.png")),
        mfs_plot,
        width = simple_plot_width, height = simple_plot_height, dpi = PLOT_DPI, bg = "white"
    )

    mfs_poster_plot <- build_simple_gep_poster_mfs_plot(
        mfs_results,
        cohort_label = cohort_label
    )

    ggsave(file.path(validation_mfs_dir, paste0(prefix, "poster_simple_mfs_validation.png")),
        mfs_poster_plot,
        width = 7.4, height = 3.4, dpi = PLOT_DPI, bg = "white"
    )

    mss_plot <- build_simple_gep_plot(
        mss_results,
        "Observed vs Predicted 5-Year MSS",
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

#' Get default cohort sources for the three-panel Objective 4 MFS figure
#'
#' @return Data frame describing runtime simple-validation workbooks for the
#'   full, restricted, and GKSRS-only cohorts.
get_objective4_simple_mfs_three_panel_sources <- function() {
    data.frame(
        dataset_name = c(
            "uveal_melanoma_full_cohort",
            "uveal_melanoma_restricted_cohort",
            "uveal_melanoma_gksrs_only_cohort"
        ),
        cohort_dir = c("uveal_full", "uveal_restricted", "gksrs"),
        prefix = c("full_cohort_", "restricted_cohort_", "gksrs_only_cohort_"),
        cohort_label = c("Full Cohort", "Restricted Cohort", "GKSRS-Only Cohort"),
        stringsAsFactors = FALSE
    )
}

#' Get default cohort sources for the two-panel Objective 4 MFS poster figure
#'
#' @return Data frame describing runtime simple-validation workbooks for the
#'   full and GKSRS-only cohorts.
get_objective4_simple_mfs_two_panel_sources <- function() {
    sources <- get_objective4_simple_mfs_three_panel_sources()
    sources[
        sources$dataset_name %in% c("uveal_melanoma_full_cohort", "uveal_melanoma_gksrs_only_cohort"),
        ,
        drop = FALSE
    ]
}

#' Read simple MFS validation rows for the three-panel Objective 4 figure
#'
#' @param cohort_sources Data frame with `cohort_dir`, `prefix`, and
#'   `cohort_label` columns. Defaults to the three Objective 4 poster cohorts.
#' @param runtime_output_dir Runtime analysis root containing cohort outputs.
#'
#' @return Data frame with MFS simple-validation rows and cohort metadata.
read_objective4_simple_mfs_three_panel_data <- function(cohort_sources = get_objective4_simple_mfs_three_panel_sources(),
                                                        runtime_output_dir = OUTPUT_DIR) {
    required_source_cols <- c("cohort_dir", "prefix", "cohort_label")
    missing_source_cols <- setdiff(required_source_cols, names(cohort_sources))
    if (length(missing_source_cols) > 0) {
        stop(
            sprintf("Three-panel source table is missing columns: %s", paste(missing_source_cols, collapse = ", ")),
            call. = FALSE
        )
    }

    panel_rows <- lapply(seq_len(nrow(cohort_sources)), function(row_index) {
        source_row <- cohort_sources[row_index, , drop = FALSE]
        workbook_path <- file.path(
            runtime_output_dir,
            source_row$cohort_dir,
            "04_GEP_Validation",
            "unified_summary",
            paste0(source_row$prefix, "simple_gep_validation.xlsx")
        )

        if (!file.exists(workbook_path)) {
            stop(
                sprintf("Missing simple GEP validation workbook for %s: %s", source_row$cohort_label, workbook_path),
                call. = FALSE
            )
        }

        mfs_rows <- readxl::read_excel(workbook_path, sheet = "MFS_By_Class") %>%
            dplyr::mutate(dplyr::across(where(is.factor), as.character)) %>%
            as.data.frame(stringsAsFactors = FALSE)

        required_mfs_cols <- c("gep_class_simple", "n", "expected_rate", "actual_rate")
        missing_mfs_cols <- setdiff(required_mfs_cols, names(mfs_rows))
        if (length(missing_mfs_cols) > 0) {
            stop(
                sprintf(
                    "Workbook %s is missing MFS columns: %s",
                    workbook_path,
                    paste(missing_mfs_cols, collapse = ", ")
                ),
                call. = FALSE
            )
        }

        if (!"class_event_label" %in% names(mfs_rows)) {
            mfs_rows$class_event_label <- sprintf("n=%d", mfs_rows$n)
        }
        if (!"treatment_mix" %in% names(mfs_rows)) {
            mfs_rows$treatment_mix <- NA_character_
        }

        mfs_rows$dataset_name <- source_row$dataset_name %||% source_row$cohort_dir
        mfs_rows$cohort_dir <- source_row$cohort_dir
        mfs_rows$cohort_label <- source_row$cohort_label
        mfs_rows$cohort_order <- row_index
        mfs_rows$source_workbook <- workbook_path
        mfs_rows
    })

    dplyr::bind_rows(panel_rows) %>%
        dplyr::mutate(
            cohort_label = factor(.data$cohort_label, levels = unique(cohort_sources$cohort_label)),
            gep_class_simple = as.character(.data$gep_class_simple),
            expected_rate = as.numeric(.data$expected_rate),
            actual_rate = as.numeric(.data$actual_rate)
        )
}

#' Build annotation rows for the three-panel Objective 4 MFS figure
#'
#' @param panel_data Combined MFS validation data with cohort labels.
#' @param fixed_limits Numeric length-two axis limits shared by all panels.
#'
#' @return Data frame with point-label coordinates and connector endpoints.
build_objective4_simple_mfs_three_panel_annotations <- function(panel_data, fixed_limits) {
    lower_limit <- fixed_limits[[1]]
    upper_limit <- fixed_limits[[2]]
    axis_span <- max(upper_limit - lower_limit, 0.15)
    label_offset_x <- min(0.018, axis_span * 0.035)
    label_offset_y <- min(0.018, axis_span * 0.035)

    split(panel_data, as.character(panel_data$cohort_label)) %>%
        lapply(function(cohort_data) {
            point_label <- if ("class_event_label" %in% names(cohort_data)) {
                sprintf("%s\n%s", cohort_data$gep_class_simple, cohort_data$class_event_label)
            } else {
                sprintf("%s\nn=%d", cohort_data$gep_class_simple, cohort_data$n)
            }

            cohort_data %>%
                dplyr::mutate(
                    point_label = point_label,
                    horizontal_direction = dplyr::if_else(
                        .data$expected_rate >= stats::median(.data$expected_rate, na.rm = TRUE),
                        -1,
                        1
                    ),
                    vertical_direction = dplyr::if_else(
                        .data$actual_rate >= stats::median(.data$actual_rate, na.rm = TRUE),
                        -1,
                        1
                    ),
                    label_x = .data$expected_rate + (.data$horizontal_direction * label_offset_x),
                    label_y = .data$actual_rate + (.data$vertical_direction * label_offset_y),
                    label_x = pmin(upper_limit - 0.006, pmax(lower_limit + 0.006, .data$label_x)),
                    label_y = pmin(upper_limit - 0.006, pmax(lower_limit + 0.006, .data$label_y)),
                    label_hjust = dplyr::if_else(.data$horizontal_direction < 0, 1, 0),
                    label_vjust = dplyr::if_else(.data$vertical_direction < 0, 1, 0),
                    connector_x = .data$expected_rate + (.data$horizontal_direction * label_offset_x * 0.8),
                    connector_y = .data$actual_rate + (.data$vertical_direction * label_offset_y * 0.8),
                    connector_x = pmin(upper_limit - 0.006, pmax(lower_limit + 0.006, .data$connector_x)),
                    connector_y = pmin(upper_limit - 0.006, pmax(lower_limit + 0.006, .data$connector_y))
                )
        }) %>%
        dplyr::bind_rows()
}

#' Build the three-panel Objective 4 simple MFS validation plot
#'
#' @param panel_data Combined MFS validation data with one row per cohort/class.
#' @param fixed_limits Optional numeric length-two limits used for both x and y
#'   axes. When `NULL`, limits are computed once across all panels.
#'
#' @return A `ggplot` object with fixed axes across cohort panels.
build_objective4_simple_mfs_three_panel_plot <- function(panel_data, fixed_limits = NULL) {
    if (is.null(panel_data) || nrow(panel_data) == 0) {
        stop("Three-panel MFS plot requires non-empty panel_data.", call. = FALSE)
    }

    if (is.null(fixed_limits)) {
        rate_range <- range(c(panel_data$expected_rate, panel_data$actual_rate), na.rm = TRUE)
        lower_limit <- max(0, floor((rate_range[[1]] - 0.03) * 20) / 20)
        upper_limit <- min(1.01, max(1, ceiling((rate_range[[2]] + 0.02) * 20) / 20))
        fixed_limits <- c(lower_limit, upper_limit)
    }

    if (!is.numeric(fixed_limits) || length(fixed_limits) != 2 || fixed_limits[[1]] >= fixed_limits[[2]]) {
        stop("fixed_limits must be a numeric length-two vector with increasing values.", call. = FALSE)
    }

    annotation_df <- build_objective4_simple_mfs_three_panel_annotations(
        panel_data = panel_data,
        fixed_limits = fixed_limits
    )
    class_palette <- get_gep_class_palette(panel_data$gep_class_simple)
    axis_breaks <- seq(0, 1, by = 0.2)
    axis_breaks <- axis_breaks[axis_breaks >= fixed_limits[[1]] & axis_breaks <= fixed_limits[[2]]]
    if (!1 %in% axis_breaks && fixed_limits[[2]] >= 1) {
        axis_breaks <- sort(unique(c(axis_breaks, 1)))
    }

    ggplot(panel_data, aes(x = expected_rate, y = actual_rate, color = gep_class_simple)) +
        geom_abline(
            slope = 1,
            intercept = 0,
            linetype = "dashed",
            linewidth = 0.8,
            color = "gray65",
            show.legend = FALSE
        ) +
        geom_segment(
            aes(
                x = .data$expected_rate,
                xend = .data$expected_rate,
                y = .data$expected_rate,
                yend = .data$actual_rate
            ),
            inherit.aes = FALSE,
            linetype = "dashed",
            linewidth = 0.7,
            alpha = 0.7,
            color = "gray60"
        ) +
        geom_point(size = 4.1) +
        geom_segment(
            data = annotation_df,
            aes(
                x = .data$expected_rate,
                y = .data$actual_rate,
                xend = .data$connector_x,
                yend = .data$connector_y
            ),
            inherit.aes = FALSE,
            linewidth = 0.4,
            color = "gray55",
            show.legend = FALSE
        ) +
        geom_label(
            data = annotation_df,
            aes(
                x = .data$label_x,
                y = .data$label_y,
                label = .data$point_label,
                hjust = .data$label_hjust,
                vjust = .data$label_vjust
            ),
            inherit.aes = FALSE,
            color = "gray25",
            fill = "white",
            label.size = 0.16,
            label.padding = grid::unit(0.08, "lines"),
            label.r = grid::unit(0.04, "lines"),
            size = 2.8,
            lineheight = 0.95,
            show.legend = FALSE
        ) +
        facet_wrap(~cohort_label, nrow = 1) +
        labs(
            title = "Observed vs Predicted 5-Year MFS",
            x = "Predicted 5-year survival",
            y = "Observed 5-year survival",
            color = "GEP class",
            caption = paste(
                "Fixed x/y axes across panels for direct cohort comparison.",
                "Dashed diagonal: observed = predicted 5-year survival.",
                "Gray vertical segments: observed-predicted gap.",
                sep = "\n"
            )
        ) +
        scale_x_continuous(
            limits = fixed_limits,
            breaks = axis_breaks,
            labels = scales::label_percent(accuracy = 1),
            expand = expansion(mult = c(0.03, 0.04))
        ) +
        scale_y_continuous(
            limits = fixed_limits,
            breaks = axis_breaks,
            labels = scales::label_percent(accuracy = 1),
            expand = expansion(mult = c(0.03, 0.05))
        ) +
        scale_color_manual(values = class_palette) +
        theme_classic(base_size = 13) +
        theme(
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            plot.title = element_text(size = 18, face = "bold", margin = margin(b = 8)),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 11),
            strip.background = element_blank(),
            strip.text = element_text(size = 13, face = "bold", margin = margin(b = 6)),
            plot.caption = element_text(size = 9.5, hjust = 0, color = "gray35", lineheight = 1.05, margin = margin(t = 10)),
            legend.position = "none",
            panel.spacing.x = grid::unit(0.55, "lines"),
            plot.margin = margin(8, 12, 12, 8),
            axis.line = element_line(linewidth = 0.8),
            axis.ticks = element_line(linewidth = 0.8)
        ) +
        coord_equal(clip = "off")
}

#' Write a three-cohort Objective 4 simple MFS validation report
#'
#' @param panel_data Optional combined MFS validation data. When `NULL`, data
#'   are read from the three cohort simple-validation workbooks.
#' @param output_dir Directory for the combined poster figure and support files.
#' @param filename_stem Filename stem for generated artifacts.
#' @param fixed_limits Optional numeric length-two fixed axis limits for both
#'   x and y axes.
#' @param save_pdf Logical indicating whether to also write a PDF figure.
#'
#' @return List with paths, data, plot object, and fixed axis limits.
create_objective4_simple_mfs_three_panel_report <- function(panel_data = NULL,
                                                            output_dir = file.path(MERGED_TABLES_DIR, "objective4_poster_figures"),
                                                            filename_stem = "objective4_three_cohort_simple_mfs_validation",
                                                            fixed_limits = NULL,
                                                            save_pdf = TRUE) {
    if (is.null(panel_data)) {
        panel_data <- read_objective4_simple_mfs_three_panel_data()
    }

    if (is.null(fixed_limits)) {
        rate_range <- range(c(panel_data$expected_rate, panel_data$actual_rate), na.rm = TRUE)
        fixed_limits <- c(
            max(0, floor((rate_range[[1]] - 0.03) * 20) / 20),
            min(1.01, max(1, ceiling((rate_range[[2]] + 0.02) * 20) / 20))
        )
    }

    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    plot_obj <- build_objective4_simple_mfs_three_panel_plot(
        panel_data = panel_data,
        fixed_limits = fixed_limits
    )

    png_path <- file.path(output_dir, paste0(filename_stem, ".png"))
    pdf_path <- file.path(output_dir, paste0(filename_stem, ".pdf"))
    treatment_mix_path <- file.path(output_dir, paste0(filename_stem, "_treatment_mix.csv"))
    report_path <- file.path(output_dir, paste0(filename_stem, "_report.md"))

    ggsave(png_path, plot_obj, width = 12, height = 5.4, dpi = PLOT_DPI, bg = "white")
    if (isTRUE(save_pdf)) {
        ggsave(pdf_path, plot_obj, width = 12, height = 5.4, bg = "white")
    } else {
        pdf_path <- NA_character_
    }

    treatment_mix <- panel_data %>%
        dplyr::transmute(
            cohort = as.character(.data$cohort_label),
            gep_class = .data$gep_class_simple,
            n = .data$n,
            five_year_mets = .data$class_event_label,
            treatment_mix = .data$treatment_mix
        ) %>%
        dplyr::arrange(.data$cohort, .data$gep_class)
    utils::write.csv(treatment_mix, treatment_mix_path, row.names = FALSE)

    cohort_count_label <- switch(
        as.character(length(unique(as.character(panel_data$cohort_label)))),
        `2` = "Two-Cohort",
        `3` = "Three-Cohort",
        "Multi-Cohort"
    )

    report_lines <- c(
        md_heading(sprintf("Objective 4 %s Simple MFS Validation", cohort_count_label), 1L),
        "",
        sprintf("Figure: `%s`", basename(png_path)),
        "",
        sprintf(
            "Fixed axis limits for both x and y: %.0f%% to %.0f%%.",
            fixed_limits[[1]] * 100,
            fixed_limits[[2]] * 100
        ),
        "",
        md_bullet("All panels use identical x/y limits and `coord_equal()` so distances from the dashed diagonal are directly comparable."),
        md_bullet("The dashed diagonal represents observed = predicted 5-year MFS."),
        md_bullet("Gray vertical segments show the observed-predicted survival gap."),
        md_bullet("Treatment mix is kept out of the point labels and summarized below to preserve figure readability."),
        "",
        md_heading("Treatment Mix", 2L),
        md_table(as.data.frame(treatment_mix, stringsAsFactors = FALSE))
    )
    writeLines(report_lines, report_path)

    list(
        plot = plot_obj,
        data = panel_data,
        fixed_limits = fixed_limits,
        paths = list(
            png = png_path,
            pdf = pdf_path,
            treatment_mix = treatment_mix_path,
            report = report_path
        )
    )
}

#' Write a two-cohort Objective 4 simple MFS validation report
#'
#' @param output_dir Directory for the combined poster figure and support files.
#' @param filename_stem Filename stem for generated artifacts.
#' @param fixed_limits Optional numeric length-two fixed axis limits for both
#'   x and y axes.
#' @param save_pdf Logical indicating whether to also write a PDF figure.
#'
#' @return List with paths, data, plot object, and fixed axis limits.
create_objective4_simple_mfs_two_panel_report <- function(output_dir = file.path(MERGED_TABLES_DIR, "objective4_poster_figures"),
                                                          filename_stem = "objective4_two_cohort_simple_mfs_validation",
                                                          fixed_limits = NULL,
                                                          save_pdf = TRUE) {
    panel_data <- read_objective4_simple_mfs_three_panel_data(
        cohort_sources = get_objective4_simple_mfs_two_panel_sources()
    )

    create_objective4_simple_mfs_three_panel_report(
        panel_data = panel_data,
        output_dir = output_dir,
        filename_stem = filename_stem,
        fixed_limits = fixed_limits,
        save_pdf = save_pdf
    )
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

    required_canonical_fields <- c(
        "expected_mfs_5yr", "expected_mss_5yr",
        "mfs_event_5yr", "mss_event_5yr",
        "mfs_analysis_eligible", "mss_analysis_eligible",
        "tt_mets_months", "tt_death_months"
    )
    missing_canonical_fields <- setdiff(required_canonical_fields, names(data))
    if (length(missing_canonical_fields) > 0) {
        stop(sprintf(
            paste(
                "Simple GEP validation requires canonical Objective 0 fields:",
                "%s"
            ),
            paste(missing_canonical_fields, collapse = ", ")
        ))
    }

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

    expected_mfs_col <- "expected_mfs_5yr"
    expected_mss_col <- "expected_mss_5yr"
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
