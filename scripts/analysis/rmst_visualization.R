# RMST Visualization

#' Plot RMST p-value progression over time
#'
#' Creates a visualization showing how RMST p-values change over time points
#'
#' @param rmst_results Data frame with RMST analysis results
#' @param outcome_label Character string for the outcome being analyzed
#' @param output_dirs List of output directories organized by analysis type
#' @param prefix Character string used as a file prefix for output files
#' @param group1_name Character string for the first group (coded as 0 in RMST)
#' @param group2_name Character string for the second group (coded as 1 in RMST)
#' @param group_var Character string for the grouping variable name (used for palette selection)
#' @param route_key Optional explicit survival-output route key.
#' @return ggplot object
plot_rmst_pvalue_progression <- function(rmst_results, outcome_label, output_dirs, prefix, group1_name = "Group 1", group2_name = "Group 2", group_var = "treatment_group", route_key = NULL) {
    wrap_plot_text <- function(text, width) {
        paste(strwrap(text, width = width), collapse = "\n")
    }

    diff_column <- if ("RMST_Difference_Months" %in% names(rmst_results)) {
        "RMST_Difference_Months"
    } else if ("RMST_Difference" %in% names(rmst_results)) {
        "RMST_Difference"
    } else {
        NULL
    }

    group1_label <- ifelse(is.null(group1_name) || is.na(group1_name) || group1_name == "", "Group 1", group1_name)
    group2_label <- ifelse(is.null(group2_name) || is.na(group2_name) || group2_name == "", "Group 2", group2_name)

    subtitle_text <- wrap_plot_text(
        sprintf("%s advantage when above zero; %s advantage when below", group2_label, group1_label),
        width = 60
    )

    y_axis_text <- sprintf("RMST Difference\n(%s minus %s, mo)", group2_label, group1_label)

    title_text <- wrap_plot_text(
        paste("RMST Difference vs Time:", outcome_label),
        width = 72
    )

    format_sigfig <- function(values, digits = 4, force_sign = FALSE) {
        vapply(values, function(val) {
            if (is.na(val)) {
                return("NA")
            }
            val_sig <- signif(val, digits = digits)
            formatted <- format(val_sig, digits = digits, trim = TRUE, scientific = abs(val_sig) >= 10^digits)
            formatted <- trimws(formatted)
            if (force_sign && !startsWith(formatted, "-")) {
                formatted <- paste0("+", formatted)
            }
            formatted
        }, character(1))
    }

    format_p_value <- function(p_val, digits = 4) {
        cutoff <- 10^(-digits)
        vapply(p_val, function(val) {
            if (is.na(val)) {
                return("NA")
            }
            if (val < cutoff) {
                return(sprintf("<%.*f", digits, cutoff))
            }
            format_sigfig(val, digits = digits, force_sign = FALSE)
        }, character(1))
    }

    if (is.null(diff_column)) {
        logger::log_info(sprintf("Skipping RMST plot for %s: missing RMST difference column", outcome_label))
        return(NULL)
    }

    plot_data <- rmst_results %>%
        dplyr::mutate(RMST_Diff_Value = !!rlang::sym(diff_column)) %>%
        dplyr::filter(!is.na(RMST_Diff_Value), !is.na(Time_Point_Years)) %>%
        dplyr::arrange(Time_Point_Years)

    if (nrow(plot_data) == 0) {
        logger::log_info(sprintf("Skipping RMST plot for %s: no valid RMST data (non-binary grouping or all analyses failed)", outcome_label))
        return(NULL)
    }

    if (!"RMST_Difference_Months" %in% names(plot_data)) {
        plot_data <- plot_data %>%
            dplyr::mutate(RMST_Difference_Months = RMST_Diff_Value)
    }

    if (!"RMST_Difference_Lower_Months" %in% names(plot_data)) {
        plot_data <- plot_data %>%
            dplyr::mutate(
                RMST_Difference_Lower_Months = RMST_Difference_Months,
                RMST_Difference_Upper_Months = RMST_Difference_Months
            )
    }

    if (!"RMST_Difference_Lower_Years" %in% names(plot_data)) {
        plot_data <- plot_data %>%
            dplyr::mutate(
                RMST_Difference_Lower_Years = RMST_Difference_Lower_Months / 12,
                RMST_Difference_Upper_Years = RMST_Difference_Upper_Months / 12
            )
    }

    plot_data <- plot_data %>%
        dplyr::mutate(
            Significance_Label = dplyr::case_when(
                is.na(RMST_P_Value) ~ "p-value unavailable",
                RMST_P_Value < 0.05 ~ "p < 0.05",
                TRUE ~ "p ≥ 0.05"
            ),
            Advantage_Label = dplyr::case_when(
                is.na(RMST_Diff_Value) ~ "Advantage unavailable",
                RMST_Diff_Value > 0 ~ sprintf("%s advantage", group2_label),
                RMST_Diff_Value < 0 ~ sprintf("%s advantage", group1_label),
                TRUE ~ "No difference"
            ),
            Time_Label = paste0(Time_Point_Years, " yr"),
            P_Value_Label = format_p_value(RMST_P_Value)
        )
    plot_data$Advantage_Label <- factor(
        plot_data$Advantage_Label,
        levels = c(
            sprintf("%s advantage", group2_label),
            sprintf("%s advantage", group1_label),
            "No difference",
            "Advantage unavailable"
        )
    )
    plot_data$Significance_Label <- factor(
        plot_data$Significance_Label,
        levels = c(
            "p < 0.05",
            "p ≥ 0.05",
            "p-value unavailable"
        )
    )

    time_axis <- plot_data %>%
        dplyr::distinct(Time_Point_Years, Time_Label) %>%
        dplyr::arrange(Time_Point_Years)

    time_min <- min(time_axis$Time_Point_Years, na.rm = TRUE)
    time_max <- max(time_axis$Time_Point_Years, na.rm = TRUE)
    time_span <- max(time_max - time_min, 1)
    left_padding <- max(time_span * 0.08, 0.6)
    right_padding <- max(time_span * 0.04, 0.4)
    x_limits <- c(time_min - left_padding, time_max + right_padding)
    metric_label_x <- time_min - left_padding * 0.7

    max_diff <- suppressWarnings(max(
        abs(c(plot_data$RMST_Difference_Lower_Months, plot_data$RMST_Difference_Upper_Months)),
        na.rm = TRUE
    ))
    if (!is.finite(max_diff)) {
        max_diff <- suppressWarnings(max(abs(plot_data$RMST_Difference_Months), na.rm = TRUE))
    }
    if (!is.finite(max_diff) || max_diff == 0) {
        max_diff <- 1
    }
    y_limit <- max_diff * 1.15

    treatment_palette <- tryCatch(
        {
            get_palette_by_variable(group_var, c(group1_label, group2_label))
        },
        error = function(e) {
            stats::setNames(
                c("#0072B5FF", "#BC3C29FF"),
                c(group1_label, group2_label)
            )
        }
    )
    if (any(is.na(treatment_palette))) {
        replacement <- stats::setNames(
            c("#0072B5FF", "#BC3C29FF"),
            c(group1_label, group2_label)
        )
        na_idx <- which(is.na(treatment_palette))
        treatment_palette[na_idx] <- replacement[names(treatment_palette)[na_idx]]
    }
    default_pair <- c("#0072B5FF", "#BC3C29FF")
    pal_group1 <- treatment_palette[as.character(group1_label)]
    pal_group2 <- treatment_palette[as.character(group2_label)]
    if (is.na(pal_group1)) pal_group1 <- default_pair[1]
    if (is.na(pal_group2)) pal_group2 <- default_pair[2]
    advantage_colors <- stats::setNames(
        object = c(
            pal_group2,
            pal_group1,
            "#6c757d",
            "#bfbfbf"
        ),
        nm = c(
            sprintf("%s advantage", group2_label),
            sprintf("%s advantage", group1_label),
            "No difference",
            "Advantage unavailable"
        )
    )
    significance_colors <- c(
        "p < 0.05" = "#08306B",
        "p ≥ 0.05" = "#c7dcee",
        "p-value unavailable" = "#ededed"
    )

    ribbon_plot <- ggplot(plot_data, aes(x = Time_Point_Years, y = RMST_Difference_Months)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "#737373", linewidth = 0.6) +
        geom_errorbar(
            aes(
                ymin = RMST_Difference_Lower_Months,
                ymax = RMST_Difference_Upper_Months
            ),
            width = 0.35,
            color = "#5c6f82",
            linewidth = 0.7,
            alpha = 0.9
        ) +
        geom_line(color = "#1b1b1b", linewidth = 1.15, na.rm = TRUE) +
        geom_point(
            aes(color = Advantage_Label, fill = Significance_Label),
            shape = 21,
            size = 4.6,
            stroke = 1.2,
            alpha = 0.95,
            na.rm = TRUE
        ) +
        scale_fill_manual(
            values = significance_colors,
            guide = "none"
        ) +
        scale_color_manual(values = advantage_colors, name = "RMST Advantage", drop = TRUE) +
        scale_x_continuous(
            breaks = time_axis$Time_Point_Years,
            labels = time_axis$Time_Label,
            limits = x_limits,
            expand = c(0, 0)
        ) +
        scale_y_continuous(
            limits = c(-y_limit, y_limit),
            labels = scales::label_number(accuracy = 0.5)
        ) +
        labs(
            title = title_text,
            subtitle = subtitle_text,
            x = NULL,
            y = y_axis_text,
            caption = "Whiskers reflect 95% Wald CI from survRM2::rmst2"
        ) +
        theme_minimal(base_size = 13) +
        theme(
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            plot.subtitle = element_text(size = 10.5, hjust = 0.5, lineheight = 0.98, margin = margin(b = 8)),
            axis.title = element_text(face = "bold"),
            axis.title.x = element_text(margin = margin(t = 8)),
            axis.title.y = element_text(margin = margin(r = 10), lineheight = 0.95),
            legend.title = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            plot.caption = element_text(size = 9, hjust = 0.5, margin = margin(t = 6)),
            plot.margin = margin(t = 12, r = 28, b = 45, l = 28)
        ) +
        guides(
            color = guide_legend(
                order = 1,
                override.aes = list(
                    shape = 21,
                    size = 4.2,
                    fill = "#fdfdfd",
                    linewidth = 0.9
                ),
                title = "RMST Advantage"
            )
        )

    metric_levels <- c(
        "p-value",
        "Diff (mo)",
        sprintf("%s RMST (mo)", group2_label),
        sprintf("%s RMST (mo)", group1_label)
    )
    metric_levels_desc <- rev(metric_levels)

    table_long <- plot_data %>%
        dplyr::transmute(
            Time_Point_Years = Time_Point_Years,
            Time_Label = Time_Label,
            `p-value` = format_p_value(RMST_P_Value),
            `Diff (mo)` = format_sigfig(RMST_Difference_Months, digits = 4, force_sign = TRUE),
            "Group2" = format_sigfig(RMST_Group2_Years * 12, digits = 4),
            "Group1" = format_sigfig(RMST_Group1_Years * 12, digits = 4)
        ) %>%
        dplyr::rename(
            !!metric_levels[3] := "Group2",
            !!metric_levels[4] := "Group1"
        ) %>%
        tidyr::pivot_longer(
            cols = all_of(metric_levels),
            names_to = "Metric",
            values_to = "Display_Value"
        ) %>%
        dplyr::mutate(Metric = factor(Metric, levels = metric_levels_desc))

    metric_label_df <- tibble::tibble(
        Metric = factor(metric_levels_desc, levels = metric_levels_desc),
        Metric_Label = metric_levels_desc
    )

    table_plot <- ggplot(table_long, aes(x = Time_Point_Years, y = Metric, label = Display_Value)) +
        geom_text(size = 4.1, fontface = "plain", color = "#1f1f1f") +
        geom_text(
            data = metric_label_df,
            aes(x = metric_label_x, y = Metric, label = Metric_Label),
            inherit.aes = FALSE,
            hjust = 1,
            size = 4.2,
            fontface = "bold",
            color = "#1f1f1f"
        ) +
        scale_x_continuous(
            limits = x_limits,
            breaks = time_axis$Time_Point_Years,
            labels = time_axis$Time_Label,
            expand = c(0, 0)
        ) +
        scale_y_discrete(expand = expansion(add = c(0.35, 0.35))) +
        labs(x = "Analysis horizon (years)", y = NULL) +
        theme_void(base_size = 12) +
        theme(
            axis.text.x = element_text(face = "bold", color = "#1f1f1f", margin = margin(t = 6)),
            axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
            plot.margin = margin(t = 0, r = 60, b = 5, l = 80)
        ) +
        coord_cartesian(clip = "off")

    ribbon_plot_with_legend <- ribbon_plot +
        theme(
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.box.margin = margin(t = 2, r = 0, b = 6, l = 0),
            legend.margin = margin(t = 4),
            legend.spacing.y = unit(4, "pt")
        )

    combined_plot <- cowplot::plot_grid(
        ribbon_plot_with_legend,
        table_plot,
        ncol = 1,
        align = "v",
        rel_heights = c(0.78, 0.22)
    )

    if (is.null(output_dirs)) {
        warning("output_dirs is NULL, cannot save RMST plot")
        return(combined_plot)
    }

    output_dir <- determine_survival_output_dir(outcome_label, output_dirs, route_key = route_key)

    if (is.null(output_dir)) {
        warning("Could not determine output directory for outcome_label: ", outcome_label)
        return(combined_plot)
    }

    output_dir <- ensure_output_dir(resolve_endpoint_output_dir(output_dirs, output_dir, "rmst"))

    filename <- paste0(prefix, make_filename_safe(outcome_label), "_rmst_pvalue_progression.png")
    if (is.null(filename) || filename == "" || is.na(filename)) {
        warning("Generated filename is empty or invalid")
        return(combined_plot)
    }

    filepath <- file.path(output_dir, filename)

    ggsave(
        filepath,
        combined_plot,
        width = RMST_PLOT_WIDTH, height = RMST_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white"
    )

    # Legacy RMST p-value progression plot (deprecated Nov 2025, retained for reference)
    # p <- ggplot(plot_data, aes(x = Time_Point_Years, y = RMST_P_Value)) +
    #     geom_line(linewidth = 1.2, color = "steelblue", alpha = 0.8) +
    #     geom_point(aes(color = Significant, size = Significant), alpha = 0.9) +
    #     geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 0.8) +
    #     geom_hline(yintercept = 0.01, linetype = "dotted", color = "darkred", linewidth = 0.6) +
    #     annotate("text",
    #         x = max(plot_data$Time_Point_Years), y = 0.05, label = "p = 0.05",
    #         hjust = -0.1, vjust = -0.2, color = "red", size = 3.5
    #     ) +
    #     annotate("text",
    #         x = max(plot_data$Time_Point_Years), y = 0.01, label = "p = 0.01",
    #         hjust = -0.1, vjust = -0.2, color = "darkred", size = 3.5
    #     ) +
    #     scale_color_manual(
    #         values = c("TRUE" = "#E31A1C", "FALSE" = "#1F78B4"),
    #         labels = c("TRUE" = "Significant (p < 0.05)", "FALSE" = "Not significant"),
    #         name = "Statistical Significance"
    #     ) +
    #     scale_size_manual(
    #         values = c("TRUE" = 4, "FALSE" = 2.5),
    #         guide = "none"
    #     ) +
    #     scale_x_continuous(
    #         breaks = plot_data$Time_Point_Years,
    #         labels = paste0(plot_data$Time_Point_Years, " yr"),
    #         limits = c(
    #             min(plot_data$Time_Point_Years),
    #             max(plot_data$Time_Point_Years) + 1.25
    #         )
    #     ) +
    #     scale_y_continuous(
    #         limits = c(0, max(plot_data$RMST_P_Value) * 1.1),
    #         breaks = c(0, seq(0.1, 1, 0.1))
    #     ) +
    #     labs(
    #         title = paste("RMST P-value Progression:", outcome_label),
    #         subtitle = "Restricted Mean Survival Time Analysis at Different Time Points",
    #         x = "Analysis Time Point",
    #         y = "P-value",
    #         caption = sprintf("Dashed line: p = 0.05 | Dotted line: p = 0.01\nRMST difference: + = %s advantage, - = %s advantage", group2_label, group1_label)
    #     ) +
    #     theme_minimal() +
    #     theme(
    #         plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    #         plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20)),
    #         axis.title = element_text(size = 14, face = "bold"),
    #         axis.text = element_text(size = 12),
    #         legend.title = element_text(size = 12, face = "bold"),
    #         legend.text = element_text(size = 11),
    #         legend.position = "bottom",
    #         panel.grid.minor = element_blank(),
    #         plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 15))
    #     )
    # Legacy code intentionally commented out per visualization refresh request.

    return(combined_plot)
}
