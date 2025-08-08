# GEP Visualization Functions
# Visualization-only: calibration, discrimination, decision curves, integrated visuals

#' Create unified GEP validation visual outputs
create_gep_validation_visuals <- function(mfs_results, mss_results, output_dir, prefix) {
    log_enhanced("Creating unified GEP validation visual outputs", level = "INFO")
    if (!is.null(mfs_results)) {
        create_calibration_plots(mfs_results, "MFS", output_dir, prefix)
    }
    if (!is.null(mss_results)) {
        create_calibration_plots(mss_results, "MSS", output_dir, prefix)
    }
    create_discrimination_plots(mfs_results, mss_results, output_dir, prefix)
    create_decision_curve_plots(mfs_results, mss_results, output_dir, prefix)
    log_enhanced("GEP validation visual outputs created", level = "INFO")
}

#' Create calibration plots
create_calibration_plots <- function(results, outcome_type, output_dir, prefix) {
    log_enhanced(sprintf("Creating calibration plots for %s", outcome_type), level = "DEBUG")
    for (tp_name in names(results$standard_validation)) {
        tp_results <- results$standard_validation[[tp_name]]
        if (!is.null(tp_results$observed_expected) && is.data.frame(tp_results$observed_expected)) {
            cal_plot <- ggplot() +
                geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
                geom_point(aes(x = expected_rate, y = observed_rate), data = tp_results$observed_expected) +
                labs(
                    title = sprintf("%s Calibration Plot - %s", outcome_type, tp_name),
                    x = "Expected Rate",
                    y = "Observed Rate"
                ) +
                theme_classic() +
                theme(
                    plot.background = element_rect(fill = "white"),
                    panel.background = element_rect(fill = "white")
                )
            plot_path <- file.path(output_dir, paste0(prefix, outcome_type, "_calibration_", tp_name, ".png"))
            ggsave(plot_path, cal_plot, width = 8, height = 6, dpi = 300, bg = "white")
        } else {
            log_enhanced(sprintf("Skipping calibration plot for %s - %s: no valid observed_expected data", outcome_type, tp_name), level = "WARN")
        }
    }
}

#' Create discrimination plots
create_discrimination_plots <- function(mfs_results, mss_results, output_dir, prefix) {
    log_enhanced("Creating discrimination plots", level = "DEBUG")
    disc_data <- data.frame()
    if (!is.null(mfs_results)) {
        for (tp_name in names(mfs_results$standard_validation)) {
            tp_results <- mfs_results$standard_validation[[tp_name]]
            if (!is.null(tp_results$discrimination)) {
                disc_data <- rbind(disc_data, data.frame(
                    outcome = "MFS",
                    timepoint = tp_name,
                    harrell_c = tp_results$discrimination$harrell_c,
                    uno_c = tp_results$discrimination$uno_c,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
    if (!is.null(mss_results)) {
        for (tp_name in names(mss_results$standard_validation)) {
            tp_results <- mss_results$standard_validation[[tp_name]]
            if (!is.null(tp_results$discrimination)) {
                disc_data <- rbind(disc_data, data.frame(
                    outcome = "MSS",
                    timepoint = tp_name,
                    harrell_c = tp_results$discrimination$harrell_c,
                    uno_c = tp_results$discrimination$uno_c,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
    if (nrow(disc_data) > 0) {
        disc_plot <- ggplot(disc_data, aes(x = timepoint, y = harrell_c, color = outcome, group = outcome)) +
            geom_point(size = 3) +
            geom_line() +
            labs(
                title = "GEP Discrimination Comparison",
                x = "Timepoint",
                y = "Harrell's C-Index",
                color = "Outcome"
            ) +
            theme_classic() +
            theme(
                plot.background = element_rect(fill = "white"),
                panel.background = element_rect(fill = "white")
            )
        plot_path <- file.path(output_dir, paste0(prefix, "gep_combined_discrimination.png"))
        ggsave(plot_path, disc_plot, width = 10, height = 8, dpi = 300, bg = "white")
    }
}

#' Create decision curve plots
create_decision_curve_plots <- function(mfs_results, mss_results, output_dir, prefix) {
    log_enhanced("Creating decision curve plots", level = "DEBUG")
    if (!is.null(mfs_results)) {
        create_decision_curve_plot(mfs_results, "MFS", output_dir, prefix)
    }
    if (!is.null(mss_results)) {
        create_decision_curve_plot(mss_results, "MSS", output_dir, prefix)
    }
}

#' Create decision curve plot for specific outcome
create_decision_curve_plot <- function(results, outcome_type, output_dir, prefix) {
    threshold <- seq(0, 1, by = 0.01)
    net_benefit <- threshold * 0.5
    dc_data <- data.frame(
        threshold = threshold,
        net_benefit = net_benefit,
        stringsAsFactors = FALSE
    )
    dc_plot <- ggplot(dc_data, aes(x = threshold, y = net_benefit)) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
        labs(
            title = sprintf("%s Decision Curve Analysis", outcome_type),
            x = "Threshold Probability",
            y = "Net Benefit"
        ) +
        theme_classic() +
        theme(
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white")
        )
    plot_path <- file.path(output_dir, paste0(prefix, outcome_type, "_decision_curve.png"))
    ggsave(plot_path, dc_plot, width = 8, height = 6, dpi = 300, bg = "white")
}

#' Create integrated GEP visualizations
create_integrated_gep_visuals <- function(mfs_results, mss_results, output_dir, prefix) {
    log_enhanced("Creating integrated GEP visualizations", level = "DEBUG")
    create_combined_calibration_plot(mfs_results, mss_results, output_dir, prefix)
    create_combined_discrimination_plot(mfs_results, mss_results, output_dir, prefix)
    create_performance_comparison_plot(mfs_results, mss_results, output_dir, prefix)
}

#' Create combined calibration plot
create_combined_calibration_plot <- function(mfs_results, mss_results, output_dir, prefix) {
    cal_data <- data.frame()
    if (!is.null(mfs_results) && !is.null(mfs_results$validation_results)) {
        for (tp_name in names(mfs_results$validation_results)) {
            tp_results <- mfs_results$validation_results[[tp_name]]
            if (!is.null(tp_results$observed_expected) && is.data.frame(tp_results$observed_expected)) {
                mfs_data <- tp_results$observed_expected
                if ("expected_rate" %in% names(mfs_data) && "observed_rate" %in% names(mfs_data)) {
                    cal_data <- rbind(cal_data, data.frame(
                        outcome = "MFS",
                        timepoint = tp_name,
                        expected_rate = mfs_data$expected_rate,
                        observed_rate = mfs_data$observed_rate,
                        stringsAsFactors = FALSE
                    ))
                }
            }
        }
    }
    if (!is.null(mss_results) && !is.null(mss_results$standard_validation)) {
        for (tp_name in names(mss_results$standard_validation)) {
            tp_results <- mss_results$standard_validation[[tp_name]]
            if (!is.null(tp_results$observed_expected) && is.data.frame(tp_results$observed_expected)) {
                mss_data <- tp_results$observed_expected
                if ("expected_rate" %in% names(mss_data) && "observed_rate" %in% names(mss_data)) {
                    cal_data <- rbind(cal_data, data.frame(
                        outcome = "MSS",
                        timepoint = tp_name,
                        expected_rate = mss_data$expected_rate,
                        observed_rate = mss_data$observed_rate,
                        stringsAsFactors = FALSE
                    ))
                }
            }
        }
    }
    if (nrow(cal_data) > 0) {
        cal_plot <- ggplot(cal_data, aes(x = expected_rate, y = observed_rate, color = outcome, shape = timepoint)) +
            geom_point(size = 3) +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
            labs(
                title = "GEP Calibration Comparison",
                x = "Expected Rate",
                y = "Observed Rate",
                color = "Outcome",
                shape = "Timepoint"
            ) +
            theme_classic() +
            theme(
                plot.background = element_rect(fill = "white"),
                panel.background = element_rect(fill = "white")
            )
        plot_path <- file.path(output_dir, paste0(prefix, "gep_combined_calibration.png"))
        ggsave(plot_path, cal_plot, width = 10, height = 8, dpi = 300, bg = "white")
    } else {
        log_enhanced("No valid calibration data found for combined plot", level = "WARN")
    }
}

#' Create combined discrimination plot
create_combined_discrimination_plot <- function(mfs_results, mss_results, output_dir, prefix) {
    disc_data <- data.frame()
    if (!is.null(mfs_results) && !is.null(mfs_results$validation_results)) {
        for (tp_name in names(mfs_results$validation_results)) {
            tp_results <- mfs_results$validation_results[[tp_name]]
            if (!is.null(tp_results$discrimination)) {
                disc_data <- rbind(disc_data, data.frame(
                    outcome = "MFS",
                    timepoint = tp_name,
                    harrell_c = tp_results$discrimination$harrell_c,
                    uno_c = tp_results$discrimination$uno_c,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
    if (!is.null(mss_results) && !is.null(mss_results$standard_validation)) {
        for (tp_name in names(mss_results$standard_validation)) {
            tp_results <- mss_results$standard_validation[[tp_name]]
            if (!is.null(tp_results$discrimination)) {
                disc_data <- rbind(disc_data, data.frame(
                    outcome = "MSS",
                    timepoint = tp_name,
                    harrell_c = tp_results$discrimination$harrell_c,
                    uno_c = tp_results$discrimination$uno_c,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
    if (nrow(disc_data) > 0) {
        disc_plot <- ggplot(disc_data, aes(x = timepoint, y = harrell_c, color = outcome, group = outcome)) +
            geom_point(size = 3) +
            geom_line() +
            labs(
                title = "GEP Discrimination Comparison",
                x = "Timepoint",
                y = "Harrell's C-Index",
                color = "Outcome"
            ) +
            theme_classic() +
            theme(
                plot.background = element_rect(fill = "white"),
                panel.background = element_rect(fill = "white")
            )
        plot_path <- file.path(output_dir, paste0(prefix, "gep_combined_discrimination.png"))
        ggsave(plot_path, disc_plot, width = 10, height = 8, dpi = 300, bg = "white")
    }
}

#' Create performance comparison plot
create_performance_comparison_plot <- function(mfs_results, mss_results, output_dir, prefix) {
    perf_data <- data.frame()
    if (!is.null(mfs_results) && !is.null(mfs_results$validation_results)) {
        for (tp_name in names(mfs_results$validation_results)) {
            tp_results <- mfs_results$validation_results[[tp_name]]
            cal_slope <- if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$slope)) tp_results$calibration$slope else NA
            harrell_c <- if (!is.null(tp_results$discrimination) && !is.null(tp_results$discrimination$harrell_c)) tp_results$discrimination$harrell_c else NA
            perf_data <- rbind(perf_data, data.frame(
                outcome = "MFS",
                timepoint = tp_name,
                calibration_slope = cal_slope,
                harrell_c = harrell_c,
                stringsAsFactors = FALSE
            ))
        }
    }
    if (!is.null(mss_results) && !is.null(mss_results$standard_validation)) {
        for (tp_name in names(mss_results$standard_validation)) {
            tp_results <- mss_results$standard_validation[[tp_name]]
            cal_slope <- if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$slope)) tp_results$calibration$slope else NA
            harrell_c <- if (!is.null(tp_results$discrimination) && !is.null(tp_results$discrimination$harrell_c)) tp_results$discrimination$harrell_c else NA
            perf_data <- rbind(perf_data, data.frame(
                outcome = "MSS",
                timepoint = tp_name,
                calibration_slope = cal_slope,
                harrell_c = harrell_c,
                stringsAsFactors = FALSE
            ))
        }
    }
    if (nrow(perf_data) > 0) {
        perf_plot <- ggplot(perf_data, aes(x = harrell_c, y = calibration_slope, color = outcome, shape = timepoint)) +
            geom_point(size = 3) +
            geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
            labs(
                title = "GEP Performance Comparison",
                x = "Harrell's C-Index",
                y = "Calibration Slope",
                color = "Outcome",
                shape = "Timepoint"
            ) +
            theme_classic() +
            theme(
                plot.background = element_rect(fill = "white"),
                panel.background = element_rect(fill = "white")
            )
        plot_path <- file.path(output_dir, paste0(prefix, "gep_performance_comparison.png"))
        ggsave(plot_path, perf_plot, width = 10, height = 8, dpi = 300, bg = "white")
    } else {
        log_enhanced("No valid performance data found for comparison plot", level = "WARN")
    }
}
