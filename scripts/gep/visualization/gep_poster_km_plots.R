#' Prepare Simple Binary GEP MFS Data for Poster KM Panels
#'
#' Filters an analytic cohort to rows usable for the poster-oriented Class 1
#' versus Class 2 metastasis-free survival Kaplan-Meier panel.
#'
#' @param data Data frame containing `gep_class_simple`, `tt_mets_months`, and
#'   `mets_event`.
#' @return Data frame with factor-ordered Class 1 and Class 2 rows.
prepare_mfs_simple_binary_poster_km_data <- function(data) {
    required_cols <- c("gep_class_simple", "tt_mets_months", "mets_event")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop(
            sprintf("Poster KM data is missing required columns: %s", paste(missing_cols, collapse = ", ")),
            call. = FALSE
        )
    }

    data %>%
        dplyr::filter(
            !is.na(.data$gep_class_simple),
            .data$gep_class_simple %in% c("Class 1", "Class 2"),
            !is.na(.data$tt_mets_months),
            !is.na(.data$mets_event),
            .data$tt_mets_months >= 0
        ) %>%
        dplyr::mutate(
            gep_class_simple = factor(
                as.character(.data$gep_class_simple),
                levels = c("Class 1", "Class 2")
            )
        ) %>%
        as.data.frame()
}

#' Derive a Reader-Facing Poster KM Panel Title
#'
#' @param dataset_name Dataset identifier or display label.
#' @param n Number of rows in the plotted simple binary GEP cohort.
#' @return Character title with cohort label and sample size.
derive_mfs_simple_binary_poster_km_title <- function(dataset_name, n) {
    cohort_label <- dplyr::case_when(
        grepl("full", dataset_name %||% "", ignore.case = TRUE) ~ "Overall cohort",
        grepl("gksrs|GK-SRS", dataset_name %||% "", ignore.case = TRUE) ~ "GK-SRS-eligible subgroup",
        grepl("restricted", dataset_name %||% "", ignore.case = TRUE) ~ "Restricted cohort",
        TRUE ~ dataset_name %||% "Simple GEP cohort"
    )

    sprintf("%s (n=%d)", cohort_label, n)
}

#' Build an Explicit Number-at-Risk Table for Poster KM Panels
#'
#' @param data Prepared simple binary GEP KM data.
#' @param time_breaks Numeric vector of risk-table time points in months.
#' @return Data frame with class, time, and number at risk.
build_mfs_simple_binary_poster_risk_table <- function(data, time_breaks) {
    class_levels <- c("Class 1", "Class 2")
    expand.grid(
        time = time_breaks,
        gep_class_simple = factor(class_levels, levels = class_levels),
        KEEP.OUT.ATTRS = FALSE
    ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            n_risk = sum(
                data$gep_class_simple == .data$gep_class_simple &
                    !is.na(data$tt_mets_months) &
                    data$tt_mets_months >= .data$time
            )
        ) %>%
        dplyr::ungroup()
}

#' Build the Poster KM Risk Table Plot
#'
#' @param risk_data Data frame from `build_mfs_simple_binary_poster_risk_table()`.
#' @param time_breaks Numeric vector of displayed time points in months.
#' @param x_max_months Maximum displayed month.
#' @return A `ggplot` risk-table object.
build_mfs_simple_binary_poster_risk_table_plot <- function(risk_data,
                                                           time_breaks,
                                                           x_max_months = 120) {
    ggplot2::ggplot(risk_data, ggplot2::aes(x = .data$time, y = .data$gep_class_simple, label = .data$n_risk)) +
        ggplot2::geom_text(size = 5.1, color = "black") +
        ggplot2::annotate("text", x = -5, y = 2.72, label = "No. at risk", hjust = 0, size = 4.5, color = "black") +
        ggplot2::scale_x_continuous(
            limits = c(-6, x_max_months),
            breaks = time_breaks,
            expand = ggplot2::expansion(mult = c(0, 0.02))
        ) +
        ggplot2::scale_y_discrete(
            limits = rev(c("Class 1", "Class 2")),
            expand = ggplot2::expansion(mult = c(0.28, 0.28))
        ) +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(size = 13, color = "black"),
            axis.text.y = ggplot2::element_text(size = 14, color = "black"),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_line(color = "#EFEFEF", linewidth = 0.4),
            plot.margin = ggplot2::margin(t = 2, r = 10, b = 0, l = 4)
        )
}

#' Build a Poster-Ready Simple Binary GEP MFS KM Panel
#'
#' @param data Data frame containing simple GEP MFS data.
#' @param panel_title Title printed above the KM panel.
#' @param x_max_months Maximum displayed follow-up in months.
#' @param time_break_months Spacing for x-axis and risk-table time points.
#' @param reference_line_alpha Alpha for the horizontal 50% reference line.
#' @return A cowplot object combining KM curve and explicit risk table.
build_mfs_simple_binary_poster_km_panel <- function(data,
                                                    panel_title,
                                                    x_max_months = 120,
                                                    time_break_months = 24,
                                                    reference_line_alpha = 0.35) {
    plot_data <- prepare_mfs_simple_binary_poster_km_data(data)
    if (nrow(plot_data) == 0 || length(unique(stats::na.omit(plot_data$gep_class_simple))) < 2) {
        stop("Poster KM panel requires analyzable Class 1 and Class 2 rows.", call. = FALSE)
    }

    class_levels <- c("Class 1", "Class 2")
    time_breaks <- seq(0, x_max_months, by = time_break_months)
    fit <- survival::survfit(survival::Surv(tt_mets_months, mets_event) ~ gep_class_simple, data = plot_data)
    fit$call$formula <- survival::Surv(tt_mets_months, mets_event) ~ gep_class_simple
    class_palette <- get_gep_class_palette(class_levels)

    surv_plot <- survminer::ggsurvplot(
        fit = fit,
        data = plot_data,
        palette = unname(class_palette[class_levels]),
        risk.table = FALSE,
        conf.int = FALSE,
        pval = TRUE,
        pval.coord = c(6, 0.18),
        pval.size = 6.5,
        censor.size = 5.2,
        size = 1.25,
        break.time.by = time_break_months,
        xlim = c(0, x_max_months),
        ylim = c(0, 1),
        legend.labs = class_levels,
        legend.title = "GEP",
        xlab = "Time (months)",
        ylab = "MFS (%)",
        ggtheme = ggplot2::theme_minimal(base_size = 15)
    )

    surv_plot$plot <- remove_plot_scales(surv_plot$plot, aesthetics = c("y"))

    km_plot <- surv_plot$plot +
        ggplot2::scale_y_continuous(
            limits = c(0, 1),
            breaks = seq(0, 1, by = 0.25),
            labels = function(x) x * 100,
            name = "MFS (%)"
        ) +
        ggplot2::geom_hline(
            yintercept = 0.5,
            color = "black",
            linewidth = 0.7,
            alpha = reference_line_alpha
        ) +
        ggplot2::labs(title = panel_title, x = "Time (months)") +
        ggplot2::theme_minimal(base_size = 15) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(size = 20, face = "bold", color = "black", margin = ggplot2::margin(b = 5)),
            axis.title = ggplot2::element_text(size = 18, face = "bold", color = "black"),
            axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
            axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
            axis.text = ggplot2::element_text(size = 15, color = "black"),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.title = ggplot2::element_text(size = 15, color = "black"),
            legend.text = ggplot2::element_text(size = 15, color = "black"),
            legend.margin = ggplot2::margin(t = -2, b = -2),
            legend.box.margin = ggplot2::margin(t = -6, b = -8),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(color = "#E6E6E6", linewidth = 0.45),
            plot.margin = ggplot2::margin(t = 5, r = 10, b = 0, l = 4)
        )

    risk_table_plot <- build_mfs_simple_binary_poster_risk_table_plot(
        risk_data = build_mfs_simple_binary_poster_risk_table(plot_data, time_breaks),
        time_breaks = time_breaks,
        x_max_months = x_max_months
    )

    cowplot::plot_grid(km_plot, risk_table_plot, ncol = 1, align = "v", rel_heights = c(0.78, 0.22))
}

#' Write a Poster-Ready Simple Binary GEP MFS KM Panel
#'
#' @param data Data frame containing simple GEP MFS data.
#' @param output_dir Directory where the PNG should be saved.
#' @param prefix Filename prefix.
#' @param dataset_name Dataset identifier used to derive the panel title.
#' @param panel_title Optional explicit panel title.
#' @param width Plot width in inches.
#' @param height Plot height in inches.
#' @param dpi Output DPI.
#' @param filename Optional explicit output filename.
#' @return Character path to the saved PNG.
write_mfs_simple_binary_poster_km_panel <- function(data,
                                                    output_dir,
                                                    prefix,
                                                    dataset_name,
                                                    panel_title = NULL,
                                                    width = 7.4,
                                                    height = 4.95,
                                                    dpi = PLOT_DPI,
                                                    filename = NULL) {
    plot_data <- prepare_mfs_simple_binary_poster_km_data(data)
    panel_title <- panel_title %||% derive_mfs_simple_binary_poster_km_title(dataset_name, nrow(plot_data))
    plot_obj <- build_mfs_simple_binary_poster_km_panel(
        data = plot_data,
        panel_title = panel_title
    )

    output_dir <- ensure_output_dir(output_dir)
    filename <- filename %||% paste0(prefix, "poster_simple_gep_binary_mfs_km_120mo.png")
    output_path <- file.path(output_dir, filename)
    ggplot2::ggsave(output_path, plot_obj, width = width, height = height, dpi = dpi, bg = "white")
    output_path
}

#' Write a Two-Cohort Poster-Ready Simple Binary GEP MFS KM Stack
#'
#' @param full_data Full cohort analytic data.
#' @param gksrs_data GK-SRS-only cohort analytic data.
#' @param output_dir Directory where the combined PNG should be saved.
#' @param filename Output filename.
#' @param width Plot width in inches.
#' @param height Plot height in inches.
#' @param dpi Output DPI.
#' @return List containing the combined PNG path and plot object.
write_mfs_simple_binary_poster_km_stack <- function(full_data,
                                                    gksrs_data,
                                                    output_dir,
                                                    filename = "objective4_two_cohort_simple_gep_mfs_km_stack.png",
                                                    width = 7.4,
                                                    height = 10.05,
                                                    dpi = PLOT_DPI) {
    full_plot_data <- prepare_mfs_simple_binary_poster_km_data(full_data)
    gksrs_plot_data <- prepare_mfs_simple_binary_poster_km_data(gksrs_data)

    full_panel <- build_mfs_simple_binary_poster_km_panel(
        data = full_plot_data,
        panel_title = derive_mfs_simple_binary_poster_km_title("uveal_melanoma_full_cohort", nrow(full_plot_data))
    )
    gksrs_panel <- build_mfs_simple_binary_poster_km_panel(
        data = gksrs_plot_data,
        panel_title = derive_mfs_simple_binary_poster_km_title("uveal_melanoma_gksrs_only_cohort", nrow(gksrs_plot_data))
    )

    output_dir <- ensure_output_dir(output_dir)
    output_path <- file.path(output_dir, filename)
    plot_obj <- cowplot::plot_grid(full_panel, gksrs_panel, ncol = 1, rel_heights = c(1, 1))
    ggplot2::ggsave(output_path, plot_obj, width = width, height = height, dpi = dpi, bg = "white")

    list(
        png = output_path,
        plot = plot_obj
    )
}
