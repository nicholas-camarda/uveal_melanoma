# GEP Visualization Functions
# Visualization-only: calibration, discrimination, decision curves, integrated visuals

#' Create unified GEP validation visual outputs
#'
#' Generate and save calibration, discrimination, and decision-curve plots for
#' GEP validation results. This function is a thin orchestrator that delegates
#' to specific plotting helpers and ensures the output directory exists.
#'
#' @param mfs_results list|NULL A list containing MFS validation results. Expected
#'   structure: list(validation_results = named list per timepoint). Can be NULL.
#' @param mss_results list|NULL A list containing MSS validation results. Expected
#'   structure: list(standard_validation = named list per timepoint). Can be NULL.
#' @param output_dir character Path to directory where images will be saved.
#' @param prefix character Filename prefix to prepend to saved artifacts.
#' @return Invisibly returns NULL after writing files to disk.
#' @examples
#' # create_gep_validation_visuals(mfs_results, mss_results, "path/to/dir", "prefix_")
create_gep_validation_visuals <- function(mfs_results, mss_results, output_dir, prefix) {
    logger::log_info("Creating unified GEP validation visual outputs")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (!is.null(mfs_results)) {
        create_calibration_plots(mfs_results, "MFS", output_dir, prefix)
        create_single_outcome_discrimination_plot(mfs_results, "MFS", output_dir, prefix)
        create_single_outcome_performance_plot(mfs_results, "MFS", output_dir, prefix)
    }
    if (!is.null(mss_results)) {
        create_calibration_plots(mss_results, "MSS", output_dir, prefix)
        create_single_outcome_discrimination_plot(mss_results, "MSS", output_dir, prefix)
        create_single_outcome_performance_plot(mss_results, "MSS", output_dir, prefix)
    }
    create_discrimination_plots(mfs_results, mss_results, output_dir, prefix)
    create_decision_curve_plots(mfs_results, mss_results, output_dir, prefix)
    logger::log_info("GEP validation visual outputs created")
}

#' Create calibration plots for a given outcome
#'
#' Builds scatter plots of observed vs expected event rates by timepoint.
#' Handles differences in result structures between MFS (list with per-class
#' results) and MSS (tabular observed_expected data.frames).
#'
#' @param results list Container holding results for an outcome (MFS or MSS).
#' @param outcome_type character Label used in plot titles (e.g., "MFS", "MSS").
#' @param output_dir character Destination directory for saved PNGs.
#' @param prefix character Filename prefix for saved PNGs.
#' @return Invisibly returns NULL after writing files.
create_calibration_plots <- function(results, outcome_type, output_dir, prefix) {
    logger::log_info(formatted(sprintf("Creating calibration plots for %s", outcome_type)))

    # Detect which container key holds per-timepoint results to support both MFS and MSS
    containers <- list(
        validation_results = results$validation_results,
        standard_validation = results$standard_validation,
        standard_results = results$standard_results
    )
    tp_container <- NULL
    if (!is.null(containers$validation_results)) tp_container <- containers$validation_results
    else if (!is.null(containers$standard_validation)) tp_container <- containers$standard_validation
    else if (!is.null(containers$standard_results)) tp_container <- containers$standard_results

    if (is.null(tp_container)) {
        logger::log_warn(sprintf("No calibration data container found for %s", outcome_type))
        return(invisible(NULL))
    }

    for (tp_name in names(tp_container)) {
        tp_results <- tp_container[[tp_name]]
        oe_df <- NULL
        # MFS stores observed_expected as a rich list; MSS typically stores a data.frame
        if (!is.null(tp_results$observed_expected)) {
            if (is.data.frame(tp_results$observed_expected)) {
                oe_df <- tp_results$observed_expected
            } else if (is.list(tp_results$observed_expected) && !is.null(tp_results$observed_expected$results_by_class)) {
                # Convert class-level list structure to tidy data for plotting
                class_list <- tp_results$observed_expected$results_by_class
                oe_df <- purrr::map_dfr(names(class_list), function(cls) {
                    x <- class_list[[cls]]
                    data.frame(
                        gep_class_simple = cls,
                        expected_rate = ifelse(!is.null(x$expected) && !is.null(x$n) && x$n > 0, x$expected / x$n, NA_real_),
                        observed_rate = ifelse(!is.null(x$observed) && !is.null(x$n) && x$n > 0, x$observed / x$n, NA_real_),
                        stringsAsFactors = FALSE
                    )
                })
            }
        }
        if (!is.null(oe_df) && all(c("expected_rate", "observed_rate") %in% names(oe_df))) {
            cal_plot <- ggplot() +
                geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
                geom_point(aes(x = expected_rate, y = observed_rate), data = oe_df) +
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
            logger::log_warn(sprintf("Skipping calibration plot for %s - %s: no valid observed_expected data", outcome_type, tp_name))
        }
    }
}

#' Create discrimination plots across outcomes
#'
#' Compiles discrimination metrics across MFS and MSS and produces a line plot
#' of Harrell's C by timepoint. Supports both MFS and MSS result container keys.
#'
#' @param mfs_results list|NULL MFS results (may be NULL if not available).
#' @param mss_results list|NULL MSS results (may be NULL if not available).
#' @param output_dir character Directory where the combined plot will be saved.
#' @param prefix character Filename prefix for the saved plot.
#' @return Invisibly returns NULL after saving plot when data present.
create_discrimination_plots <- function(mfs_results, mss_results, output_dir, prefix) {
    logger::log_debug("Creating discrimination plots")
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

    # MSS may store either $standard_validation or $standard_results depending on calling site
    mss_container <- NULL
    if (!is.null(mss_results)) {
        if (!is.null(mss_results$standard_validation)) mss_container <- mss_results$standard_validation
        else if (!is.null(mss_results$standard_results)) mss_container <- mss_results$standard_results
    }
    if (!is.null(mss_container)) {
        for (tp_name in names(mss_container)) {
            tp_results <- mss_container[[tp_name]]
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

    # Only produce a combined comparison when there are at least two distinct outcomes
    if (nrow(disc_data) > 0 && length(unique(disc_data$outcome)) >= 2) {
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
    } else {
        logger::log_info("Skipping combined discrimination plot: requires both MFS and MSS data")
    }
}

#' Create decision curve plots for MFS and MSS
#'
#' Convenience wrapper that calls the single-outcome decision curve plotter for
#' each available outcome.
#'
#' @param mfs_results list|NULL MFS results (may be NULL).
#' @param mss_results list|NULL MSS results (may be NULL).
#' @param output_dir character Directory for saved PNGs.
#' @param prefix character Filename prefix for saved PNGs.
#' @return Invisibly returns NULL after writing files.
create_decision_curve_plots <- function(mfs_results, mss_results, output_dir, prefix) {
    logger::log_info("Creating decision curve plots")
    if (!is.null(mfs_results)) {
        create_decision_curve_plot(mfs_results, "MFS", output_dir, prefix)
    }
    if (!is.null(mss_results)) {
        create_decision_curve_plot(mss_results, "MSS", output_dir, prefix)
    }
}

#' Create a decision curve plot for one outcome
#'
#' Attempts to use real DCA outputs when available (MFS uses
#' `validation_results[[timepoint]]$decision_curve$dca_curve_data`).
#' If no valid DCA data are present, this function will log a message and
#' skip plotting rather than producing a placeholder figure.
#'
#' @param results list Result container for a single outcome.
#' @param outcome_type character Outcome label used in titles.
#' @param output_dir character Directory for saved PNGs.
#' @param prefix character Filename prefix for saved PNGs.
#' @return Invisibly returns NULL after writing files or skipping when unavailable.
create_decision_curve_plot <- function(results, outcome_type, output_dir, prefix) {
    # Track whether we managed to use computed DCA results
    used_real <- FALSE

    # MFS path: validation_results[[tp]]$decision_curve$dca_curve_data
    if (!is.null(results$validation_results)) {
        for (tp_name in names(results$validation_results)) {
            tp_results <- results$validation_results[[tp_name]]
            if (!is.null(tp_results$decision_curve) && !is.null(tp_results$decision_curve$dca_curve_data)) {
                dca_df <- tp_results$decision_curve$dca_curve_data
                if (all(c("threshold", "net_benefit_model", "net_benefit_all", "net_benefit_none") %in% names(dca_df))) {
                    used_real <- TRUE
                    dc_plot <- ggplot(dca_df, aes(x = threshold)) +
                        geom_line(aes(y = net_benefit_model, color = "Model")) +
                        geom_line(aes(y = net_benefit_all, color = "Treat All"), linetype = "dashed") +
                        geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
                        scale_color_manual(values = c("Model" = "#1b9e77", "Treat All" = "#d95f02")) +
                        labs(
                            title = sprintf("%s Decision Curve (%s)", outcome_type, tp_name),
                            x = "Threshold Probability",
                            y = "Net Benefit",
                            color = "Strategy"
                        ) +
                        theme_classic() +
                        theme(
                            plot.background = element_rect(fill = "white"),
                            panel.background = element_rect(fill = "white")
                        )
                    plot_path <- file.path(output_dir, paste0(prefix, outcome_type, "_decision_curve_", tp_name, ".png"))
                    ggsave(plot_path, dc_plot, width = 8, height = 6, dpi = 300, bg = "white")
                }
            }
        }
    }

    # MSS path: standard_validation/standard_results containers
    if (!used_real) {
        container <- NULL
        if (!is.null(results$standard_validation)) container <- results$standard_validation
        else if (!is.null(results$standard_results)) container <- results$standard_results
        if (!is.null(container)) {
            for (tp_name in names(container)) {
                tp_results <- container[[tp_name]]
                if (!is.null(tp_results$decision_curve) && !is.null(tp_results$decision_curve$dca_curve_data)) {
                    dca_df <- tp_results$decision_curve$dca_curve_data
                    if (all(c("threshold", "net_benefit_model", "net_benefit_all", "net_benefit_none") %in% names(dca_df))) {
                        used_real <- TRUE
                        dc_plot <- ggplot(dca_df, aes(x = threshold)) +
                            geom_line(aes(y = net_benefit_model, color = "Model")) +
                            geom_line(aes(y = net_benefit_all, color = "Treat All"), linetype = "dashed") +
                            geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
                            scale_color_manual(values = c("Model" = "#1b9e77", "Treat All" = "#d95f02")) +
                            labs(
                                title = sprintf("%s Decision Curve (%s)", outcome_type, tp_name),
                                x = "Threshold Probability",
                                y = "Net Benefit",
                                color = "Strategy"
                            ) +
                            theme_classic() +
                            theme(
                                plot.background = element_rect(fill = "white"),
                                panel.background = element_rect(fill = "white")
                            )
                        plot_path <- file.path(output_dir, paste0(prefix, outcome_type, "_decision_curve_", tp_name, ".png"))
                        ggsave(plot_path, dc_plot, width = 8, height = 6, dpi = 300, bg = "white")
                    }
                }
            }
        }
    }

    if (!used_real) {
        # Gracefully skip instead of drawing a placeholder
        logger::log_info(sprintf("Skipping %s decision curve: no valid DCA data available", outcome_type))
        return(invisible(NULL))
    }
}

#' Create integrated GEP visualizations
#'
#' Produces combined calibration, discrimination, and performance comparison
#' visuals across outcomes (MFS/MSS) in the specified directory.
#'
#' @param mfs_results list|NULL MFS validation results (or NULL).
#' @param mss_results list|NULL MSS validation results (or NULL).
#' @param output_dir character Directory for the combined visuals.
#' @param prefix character Filename prefix for saved PNGs.
#' @return Invisibly returns NULL after writing files.
create_integrated_gep_visuals <- function(mfs_results, mss_results, output_dir, prefix) {
    logger::log_info("Creating integrated GEP visualizations")
    create_combined_calibration_plot(mfs_results, mss_results, output_dir, prefix)
    create_combined_discrimination_plot(mfs_results, mss_results, output_dir, prefix)
    create_performance_comparison_plot(mfs_results, mss_results, output_dir, prefix)
}

#' Create combined calibration plot across outcomes
#'
#' Builds a single scatter plot of observed vs expected rates for both MFS and
#' MSS, aggregating across available timepoints. Handles conversion from class
#' list structures (MFS) to a tidy tabular format.
#'
#' @param mfs_results list|NULL MFS results container (or NULL).
#' @param mss_results list|NULL MSS results container (or NULL).
#' @param output_dir character Output directory for the saved plot.
#' @param prefix character Filename prefix for the saved plot.
#' @return Invisibly returns NULL after writing file when data present.
create_combined_calibration_plot <- function(mfs_results, mss_results, output_dir, prefix) {
    cal_data <- data.frame()
    if (!is.null(mfs_results) && !is.null(mfs_results$validation_results)) {
        for (tp_name in names(mfs_results$validation_results)) {
            tp_results <- mfs_results$validation_results[[tp_name]]
            if (!is.null(tp_results$observed_expected) && !is.null(tp_results$observed_expected$results_by_class)) {
                for (cls in names(tp_results$observed_expected$results_by_class)) {
                    x <- tp_results$observed_expected$results_by_class[[cls]]
                    cal_data <- rbind(cal_data, data.frame(
                        outcome = "MFS",
                        timepoint = tp_name,
                        expected_rate = as.numeric(x$expected / x$n),
                        observed_rate = as.numeric(x$observed / x$n),
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
                mss_df <- tp_results$observed_expected
                # Accept either expected/observed counts or rates
                if (all(c("expected","observed","n") %in% names(mss_df))) {
                    tmp <- data.frame(
                        outcome = "MSS",
                        timepoint = tp_name,
                        expected_rate = as.numeric(mss_df$expected / mss_df$n),
                        observed_rate = as.numeric(mss_df$observed / mss_df$n),
                        stringsAsFactors = FALSE
                    )
                } else if (all(c("expected_rate","observed_rate") %in% names(mss_df))) {
                    tmp <- data.frame(
                        outcome = "MSS",
                        timepoint = tp_name,
                        expected_rate = as.numeric(mss_df$expected_rate),
                        observed_rate = as.numeric(mss_df$observed_rate),
                        stringsAsFactors = FALSE
                    )
                } else {
                    tmp <- NULL
                }
                if (!is.null(tmp)) cal_data <- rbind(cal_data, tmp)
            }
        }
    }
    if (nrow(cal_data) > 0 && length(unique(cal_data$outcome)) >= 2) {
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
        logger::log_info("Skipping combined calibration plot: requires both MFS and MSS data")
    }
}

#' Create combined discrimination plot across outcomes
#'
#' Aggregates discrimination metrics (Harrell's C, Uno's C when available) for
#' MFS and MSS into a single line plot by timepoint.
#'
#' @inheritParams create_combined_calibration_plot
#' @return Invisibly returns NULL after writing file when data present.
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
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
    if (nrow(disc_data) > 0 && length(unique(disc_data$outcome)) >= 2) {
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
    } else {
        logger::log_info("Skipping combined discrimination plot: requires both MFS and MSS data")
    }
}

#' Create performance comparison plot (calibration vs discrimination)
#'
#' Produces a scatter plot of calibration slope (y) vs Harrell's C (x) across
#' outcomes and timepoints to summarize overall performance.
#'
#' @inheritParams create_combined_calibration_plot
#' @return Invisibly returns NULL after writing file when data present.
create_performance_comparison_plot <- function(mfs_results, mss_results, output_dir, prefix) {
    perf_data <- data.frame()
    if (!is.null(mfs_results) && !is.null(mfs_results$validation_results)) {
        for (tp_name in names(mfs_results$validation_results)) {
            tp_results <- mfs_results$validation_results[[tp_name]]
            cal_slope <- if (!is.null(tp_results$calibration) && !is.null(tp_results$calibration$calibration_slope)) tp_results$calibration$calibration_slope else NA
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
    if (nrow(perf_data) > 0 && length(unique(perf_data$outcome)) >= 2) {
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
        logger::log_info("Skipping performance comparison plot: requires both MFS and MSS data")
    }
}

#' Create per-outcome discrimination plot (Harrell's C over time)
#'
#' Builds a discrimination-over-time plot for a single outcome and saves it to
#' the outcome's directory. Robust to different container keys.
#'
#' @param results list Container with either validation_results (MFS) or
#'   standard_validation/standard_results (MSS)
#' @param outcome_type character One of "MFS" or "MSS"
#' @param output_dir character Destination directory
#' @param prefix character Filename prefix
create_single_outcome_discrimination_plot <- function(results, outcome_type, output_dir, prefix) {
    container <- NULL
    if (!is.null(results$validation_results)) container <- results$validation_results
    else if (!is.null(results$standard_validation)) container <- results$standard_validation
    else if (!is.null(results$standard_results)) container <- results$standard_results
    if (is.null(container)) return(invisible(NULL))

    disc_df <- data.frame()
    for (tp_name in names(container)) {
        tp <- container[[tp_name]]
        if (!is.null(tp$discrimination) && !is.null(tp$discrimination$harrell_c)) {
            disc_df <- rbind(disc_df, data.frame(
                timepoint = tp_name,
                harrell_c = as.numeric(tp$discrimination$harrell_c),
                stringsAsFactors = FALSE
            ))
        }
    }
    if (nrow(disc_df) == 0) return(invisible(NULL))

    p <- ggplot(disc_df, aes(x = timepoint, y = harrell_c, group = 1)) +
        geom_point(size = 3, color = "#E64B35FF") +
        geom_line(color = "#E64B35FF") +
        labs(title = sprintf("%s Discrimination Over Time", outcome_type), x = "Timepoint", y = "Harrell's C-Index") +
        theme_classic() +
        theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
    ggsave(file.path(output_dir, paste0(prefix, outcome_type, "_discrimination_over_time.png")), p, width = 10, height = 8, dpi = 300, bg = "white")
}

#' Create per-outcome performance plot (Calibration slope vs C-index)
#'
#' Produces a scatter of calibration slope (y) vs Harrell's C (x) across
#' timepoints for a single outcome.
create_single_outcome_performance_plot <- function(results, outcome_type, output_dir, prefix) {
    container <- NULL
    if (!is.null(results$validation_results)) container <- results$validation_results
    else if (!is.null(results$standard_validation)) container <- results$standard_validation
    else if (!is.null(results$standard_results)) container <- results$standard_results
    if (is.null(container)) return(invisible(NULL))

    perf_df <- data.frame()
    for (tp_name in names(container)) {
        tp <- container[[tp_name]]
        cal_slope <- NA_real_
        if (!is.null(tp$calibration)) {
            cal_slope <- if (!is.null(tp$calibration$calibration_slope)) tp$calibration$calibration_slope else tp$calibration$slope
        }
        harrell_c <- if (!is.null(tp$discrimination)) tp$discrimination$harrell_c else NA
        perf_df <- rbind(perf_df, data.frame(
            timepoint = tp_name,
            calibration_slope = as.numeric(cal_slope),
            harrell_c = as.numeric(harrell_c),
            stringsAsFactors = FALSE
        ))
    }
    if (nrow(perf_df) == 0) return(invisible(NULL))

    p <- ggplot(perf_df, aes(x = harrell_c, y = calibration_slope, shape = timepoint)) +
        geom_point(size = 3, color = "#E64B35FF") +
        geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
        labs(title = sprintf("%s Performance (Calibration vs C)", outcome_type), x = "Harrell's C-Index", y = "Calibration Slope", shape = "Timepoint") +
        theme_classic() +
        theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
    ggsave(file.path(output_dir, paste0(prefix, outcome_type, "_performance.png")), p, width = 10, height = 8, dpi = 300, bg = "white")
}
