# GEP Visualization Functions
# Visualization-only: calibration, discrimination, decision curves, integrated visuals

#' Create MFS-specific GEP validation visuals
#'
#' Generates calibration, discrimination, performance, decision curves, and KM
#' survival curves for MFS only. Writes to subfolders under output_dir.
#'
#' @param mfs_results list MFS validation results container
#' @param mfs_data data.frame Raw data for MFS survival curve
#' @param output_dir character Destination directory (MFS outcome folder)
#' @param prefix character Filename prefix
create_mfs_gep_visuals <- function(mfs_results, mfs_data, output_dir, prefix, group_var = "biopsy1_gep", other_map = list(), dataset_name = "GEP Validation") {
    # Directly write into centralized visuals folder created by create_output_structure()

    # Calibration plots
    # create_calibration_plots(mfs_results, "MFS", output_dir, prefix)

    # Discrimination plots (per-outcome)
    # create_single_outcome_discrimination_plot(mfs_results, "MFS", output_dir, prefix)
    # REMOVED: Redundant performance plot that duplicates discrimination metrics
    # create_single_outcome_performance_plot(mfs_results, "MFS", output_dir, prefix)

    # Decision curves
    # create_decision_curve_plot(mfs_results, "MFS", output_dir, prefix)

    # Survival curves (KM)
    if (!is.null(mfs_data) && nrow(mfs_data) > 0) {
        create_mfs_survival_curves(mfs_data, output_dir, prefix, group_var = group_var, other_map = other_map, dataset_name = dataset_name)
    }

    invisible(NULL)
}

#' Create MSS-specific GEP validation visuals
#'
#' Generates calibration, discrimination, performance, decision curves, and CIF
#' curves for MSS only. Writes to subfolders under output_dir.
#'
#' @param mss_results list MSS validation results container
#' @param mss_data data.frame Raw data for CIF curves
#' @param output_dir character Destination directory (MSS outcome folder)
#' @param prefix character Filename prefix
create_mss_gep_visuals <- function(mss_results, mss_data, output_dir, prefix, group_var = "biopsy1_gep", other_map = list()) {
    # Directly write into centralized visuals folder created by create_output_structure()

    # Calibration plots (removed per spec; included in consolidated summary)
    # create_calibration_plots(mss_results, "MSS", output_dir, prefix)

    # Discrimination plots (per-outcome)
    # create_single_outcome_discrimination_plot(mss_results, "MSS", output_dir, prefix)
    # REMOVED: Redundant performance plot that duplicates discrimination metrics
    # create_single_outcome_performance_plot(mss_results, "MSS", output_dir, prefix)

    # Decision curves (removed per spec; included in consolidated summary)
    # create_decision_curve_plot(mss_results, "MSS", output_dir, prefix)

    # CIF curves
    if (!is.null(mss_data) && nrow(mss_data) > 0) {
        # Create CIF curves for 5-year timepoint (most common for MSS analysis)
        # Extract competing risks results for the 5-year timepoint if available
        competing_results_5yr <- NULL
        if (!is.null(mss_results$competing_risk_validation) && "5yr" %in% names(mss_results$competing_risk_validation)) {
            competing_results_5yr <- mss_results$competing_risk_validation$`5yr`
        }

        create_mss_cumulative_incidence_curves(
            mss_data, 5, output_dir, prefix,
            group_var = group_var,
            other_map = other_map,
            competing_results = competing_results_5yr
        )
    }

    invisible(NULL)
}

#' Create unified GEP visuals across outcomes (no survival curves)
#'
#' Produces combined calibration, discrimination, and performance visuals when
#' both outcomes are available.
#'
#' @param mfs_results list MFS results
#' @param mss_results list MSS results
#' @param output_dir character Unified directory
#' @param prefix character Filename prefix
create_unified_gep_visuals <- function(mfs_results, mss_results, output_dir, prefix) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    # create_combined_calibration_plot(mfs_results, mss_results, output_dir, prefix)
    # create_combined_discrimination_plot(mfs_results, mss_results, output_dir, prefix)
    # REMOVED: Redundant performance comparison plot that duplicates discrimination metrics
    # create_performance_comparison_plot(mfs_results, mss_results, output_dir, prefix)
    invisible(NULL)
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
    logger::log_info(formatted(sprintf("Creating calibration plots for %s", outcome_type), indent = 1))

    # Detect which container key holds per-timepoint results to support both MFS and MSS
    containers <- list(
        validation_results = results$validation_results,
        standard_validation = results$standard_validation,
        standard_results = results$standard_results
    )
    tp_container <- NULL
    if (!is.null(containers$validation_results)) {
        tp_container <- containers$validation_results
    } else if (!is.null(containers$standard_validation)) {
        tp_container <- containers$standard_validation
    } else if (!is.null(containers$standard_results)) tp_container <- containers$standard_results

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
                        biopsy1_gep = cls,
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
            # ensure calibration subfolder exists under the outcome folder
            cal_dir <- file.path(output_dir, "calibration")
            if (!dir.exists(cal_dir)) dir.create(cal_dir, recursive = TRUE, showWarnings = FALSE)
            plot_path <- file.path(cal_dir, paste0(prefix, outcome_type, "_calibration_", tp_name, ".png"))
            ggsave(plot_path, cal_plot, width = SMALL_PLOT_WIDTH, height = SMALL_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white")
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
                    integrated_auc = tp_results$discrimination$integrated_auc,
                    stringsAsFactors = FALSE
                ))
            }
        }
    }

    # MSS may store either $standard_validation or $standard_results depending on calling site
    mss_container <- NULL
    if (!is.null(mss_results)) {
        if (!is.null(mss_results$standard_validation)) {
            mss_container <- mss_results$standard_validation
        } else if (!is.null(mss_results$standard_results)) mss_container <- mss_results$standard_results
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
        ggsave(plot_path, disc_plot, width = DEFAULT_PLOT_WIDTH, height = DEFAULT_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white")
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
        dca_dir_mfs <- file.path(output_dir, "decision_curves")
        if (!dir.exists(dca_dir_mfs)) dir.create(dca_dir_mfs, recursive = TRUE, showWarnings = FALSE)
        create_decision_curve_plot(mfs_results, "MFS", dca_dir_mfs, prefix)
    }
    if (!is.null(mss_results)) {
        dca_dir_mss <- file.path(output_dir, "decision_curves")
        if (!dir.exists(dca_dir_mss)) dir.create(dca_dir_mss, recursive = TRUE, showWarnings = FALSE)
        create_decision_curve_plot(mss_results, "MSS", dca_dir_mss, prefix)
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
    emitted <- FALSE

    # MFS path: validation_results[[tp]]$decision_curve$dca_curve_data
    if (!is.null(results$validation_results)) {
        for (tp_name in names(results$validation_results)) {
            tp_results <- results$validation_results[[tp_name]]
            if (!is.null(tp_results$decision_curve) && !is.null(tp_results$decision_curve$dca_curve_data)) {
                dca_df <- tp_results$decision_curve$dca_curve_data
                if (all(c("threshold", "net_benefit_model", "net_benefit_all", "net_benefit_none") %in% names(dca_df))) {
                    dc_plot <- ggplot(dca_df, aes(x = threshold)) +
                        geom_line(aes(y = net_benefit_model, color = "Model")) +
                        geom_line(aes(y = net_benefit_all, color = "Treat All"), linetype = "dashed") +
                        geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
                        scale_color_manual(values = {
                            pal <- get_qualitative_palette(2)
                            names(pal) <- c("Model", "Treat All")
                            pal
                        }) +
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
                    ggsave(plot_path, dc_plot, width = SMALL_PLOT_WIDTH, height = SMALL_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white")
                    emitted <- TRUE
                }
            }
        }
    }

    # MSS path: standard_validation/standard_results containers
    if (!emitted) {
        container <- NULL
        if (!is.null(results$standard_validation)) {
            container <- results$standard_validation
        } else if (!is.null(results$standard_results)) container <- results$standard_results
        if (!is.null(container)) {
            for (tp_name in names(container)) {
                tp_results <- container[[tp_name]]
                if (!is.null(tp_results$decision_curve) && !is.null(tp_results$decision_curve$dca_curve_data)) {
                    dca_df <- tp_results$decision_curve$dca_curve_data
                    if (all(c("threshold", "net_benefit_model", "net_benefit_all", "net_benefit_none") %in% names(dca_df))) {
                        dc_plot <- ggplot(dca_df, aes(x = threshold)) +
                            geom_line(aes(y = net_benefit_model, color = "Model")) +
                            geom_line(aes(y = net_benefit_all, color = "Treat All"), linetype = "dashed") +
                            geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
                            scale_color_manual(values = {
                                pal <- get_qualitative_palette(2)
                                names(pal) <- c("Model", "Treat All")
                                pal
                            }) +
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
                        ggsave(plot_path, dc_plot, width = SMALL_PLOT_WIDTH, height = SMALL_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white")
                        emitted <- TRUE
                    }
                }
            }
        }
    }

    if (!emitted) {
        logger::log_info(sprintf("Skipping %s decision curve: no valid DCA data available", outcome_type))
    }
    invisible(NULL)
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
    # create_combined_calibration_plot(mfs_results, mss_results, output_dir, prefix)
    # create_combined_discrimination_plot(mfs_results, mss_results, output_dir, prefix)
    # create_performance_comparison_plot(mfs_results, mss_results, output_dir, prefix)
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
                if (all(c("expected", "observed", "n") %in% names(mss_df))) {
                    tmp <- data.frame(
                        outcome = "MSS",
                        timepoint = tp_name,
                        expected_rate = as.numeric(mss_df$expected / mss_df$n),
                        observed_rate = as.numeric(mss_df$observed / mss_df$n),
                        stringsAsFactors = FALSE
                    )
                } else if (all(c("expected_rate", "observed_rate") %in% names(mss_df))) {
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
    if (nrow(cal_data) > 0) {
        # Create plot whether we have one or both outcomes
        has_multiple_outcomes <- length(unique(cal_data$outcome)) >= 2

        if (has_multiple_outcomes) {
            # Combined plot with both outcomes
            cal_plot <- ggplot(cal_data, aes(x = expected_rate, y = observed_rate, color = outcome, shape = timepoint)) +
                geom_point(size = 3) +
                geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
                labs(
                    title = "GEP Calibration Comparison (MFS + MSS)",
                    x = "Expected Rate",
                    y = "Observed Rate",
                    color = "Outcome",
                    shape = "Timepoint"
                ) +
                scale_color_manual(values = {
                    pal <- get_qualitative_palette(2)
                    names(pal) <- c("MFS", "MSS")
                    pal
                }) +
                theme_classic() +
                theme(
                    plot.background = element_rect(fill = "white"),
                    panel.background = element_rect(fill = "white")
                )
        } else {
            # Single outcome plot
            outcome_name <- unique(cal_data$outcome)[1]
            cal_plot <- ggplot(cal_data, aes(x = expected_rate, y = observed_rate, shape = timepoint)) +
                geom_point(size = 3, color = get_qualitative_palette(1)[1]) +
                geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
                labs(
                    title = sprintf("GEP Calibration - %s Only", outcome_name),
                    x = "Expected Rate",
                    y = "Observed Rate",
                    shape = "Timepoint"
                ) +
                theme_classic() +
                theme(
                    plot.background = element_rect(fill = "white"),
                    panel.background = element_rect(fill = "white")
                )
        }

        plot_path <- file.path(output_dir, paste0(prefix, "gep_calibration_", ifelse(has_multiple_outcomes, "combined", tolower(unique(cal_data$outcome)[1])), ".png"))
        ggsave(plot_path, cal_plot, width = DEFAULT_PLOT_WIDTH, height = DEFAULT_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white")
        logger::log_info(sprintf("Calibration plot saved: %s", plot_path))
    } else {
        logger::log_info("Skipping calibration plot: no calibration data available")
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
    if (nrow(disc_data) > 0) {
        # Create plot whether we have one or both outcomes
        has_multiple_outcomes <- length(unique(disc_data$outcome)) >= 2

        if (has_multiple_outcomes) {
            # Combined plot with both outcomes
            disc_plot <- ggplot(disc_data, aes(x = timepoint, y = harrell_c, color = outcome, group = outcome)) +
                geom_point(size = 3) +
                geom_line() +
                labs(
                    title = "GEP Discrimination Comparison (MFS + MSS)",
                    x = "Timepoint",
                    y = "Harrell's C-Index",
                    color = "Outcome"
                ) +
                scale_color_manual(values = {
                    pal <- get_qualitative_palette(2)
                    names(pal) <- c("MFS", "MSS")
                    pal
                }) +
                theme_classic() +
                theme(
                    plot.background = element_rect(fill = "white"),
                    panel.background = element_rect(fill = "white")
                )
        } else {
            # Single outcome plot
            outcome_name <- unique(disc_data$outcome)[1]
            disc_plot <- ggplot(disc_data, aes(x = timepoint, y = harrell_c, group = 1)) +
                geom_point(size = 3, color = get_qualitative_palette(1)[1]) +
                geom_line(color = get_qualitative_palette(1)[1]) +
                labs(
                    title = sprintf("GEP Discrimination - %s Only", outcome_name),
                    x = "Timepoint",
                    y = "Harrell's C-Index"
                ) +
                theme_classic() +
                theme(
                    plot.background = element_rect(fill = "white"),
                    panel.background = element_rect(fill = "white")
                )
        }

        plot_path <- file.path(output_dir, paste0(prefix, "gep_discrimination_", ifelse(has_multiple_outcomes, "combined", tolower(unique(disc_data$outcome)[1])), ".png"))
        ggsave(plot_path, disc_plot, width = DEFAULT_PLOT_WIDTH, height = DEFAULT_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white")
        logger::log_info(sprintf("Discrimination plot saved: %s", plot_path))
    } else {
        logger::log_info("Skipping discrimination plot: no discrimination data available")
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
    if (nrow(perf_data) > 0) {
        # Create plot whether we have one or both outcomes
        has_multiple_outcomes <- length(unique(perf_data$outcome)) >= 2

        if (has_multiple_outcomes) {
            # Combined plot with both outcomes
            perf_plot <- ggplot(perf_data, aes(x = harrell_c, y = calibration_slope, color = outcome, shape = timepoint)) +
                geom_point(size = 3) +
                geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
                labs(
                    title = "GEP Performance Comparison (MFS + MSS)",
                    x = "Harrell's C-Index",
                    y = "Calibration Slope",
                    color = "Outcome",
                    shape = "Timepoint"
                ) +
                scale_color_manual(values = {
                    pal <- get_qualitative_palette(2)
                    names(pal) <- c("MFS", "MSS")
                    pal
                }) +
                theme_classic() +
                theme(
                    plot.background = element_rect(fill = "white"),
                    panel.background = element_rect(fill = "white")
                )
        } else {
            # Single outcome plot
            outcome_name <- unique(perf_data$outcome)[1]
            perf_plot <- ggplot(perf_data, aes(x = harrell_c, y = calibration_slope, shape = timepoint)) +
                geom_point(size = 3, color = get_qualitative_palette(1)[1]) +
                geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
                labs(
                    title = sprintf("GEP Performance - %s Only", outcome_name),
                    x = "Harrell's C-Index",
                    y = "Calibration Slope",
                    shape = "Timepoint"
                ) +
                theme_classic() +
                theme(
                    plot.background = element_rect(fill = "white"),
                    panel.background = element_rect(fill = "white")
                )
        }

        plot_path <- file.path(output_dir, paste0(prefix, "gep_performance_", ifelse(has_multiple_outcomes, "comparison", tolower(unique(perf_data$outcome)[1])), ".png"))
        ggsave(plot_path, perf_plot, width = DEFAULT_PLOT_WIDTH, height = DEFAULT_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white")
        logger::log_info(sprintf("Performance plot saved: %s", plot_path))
    } else {
        logger::log_info("Skipping performance plot: no performance data available")
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
    if (!is.null(results$validation_results)) {
        container <- results$validation_results
    } else if (!is.null(results$standard_validation)) {
        container <- results$standard_validation
    } else if (!is.null(results$standard_results)) container <- results$standard_results
    if (is.null(container)) {
        return(invisible(NULL))
    }

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
    if (nrow(disc_df) == 0) {
        return(invisible(NULL))
    }

    p <- ggplot(disc_df, aes(x = timepoint, y = harrell_c, group = 1)) +
        geom_point(size = 3, color = get_qualitative_palette(1)[1]) +
        geom_line(color = get_qualitative_palette(1)[1]) +
        labs(title = sprintf("%s Discrimination Over Time", outcome_type), x = "Timepoint", y = "Harrell's C-Index") +
        theme_classic() +
        theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
    ggsave(file.path(output_dir, paste0(prefix, outcome_type, "_discrimination_over_time.png")), p, width = DEFAULT_PLOT_WIDTH, height = DEFAULT_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white")
}

#' Create per-outcome performance plot (Calibration slope vs C-index)
#'
#' Produces a scatter of calibration slope (y) vs Harrell's C (x) across
#' timepoints for a single outcome.
create_single_outcome_performance_plot <- function(results, outcome_type, output_dir, prefix) {
    container <- NULL
    if (!is.null(results$validation_results)) {
        container <- results$validation_results
    } else if (!is.null(results$standard_validation)) {
        container <- results$standard_validation
    } else if (!is.null(results$standard_results)) container <- results$standard_results
    if (is.null(container)) {
        return(invisible(NULL))
    }

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
    if (nrow(perf_df) == 0) {
        return(invisible(NULL))
    }

    p <- ggplot(perf_df, aes(x = harrell_c, y = calibration_slope, shape = timepoint)) +
        geom_point(size = 3, color = get_qualitative_palette(1)[1]) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
        labs(title = sprintf("%s Performance (Calibration vs C)", outcome_type), x = "Harrell's C-Index", y = "Calibration Slope", shape = "Timepoint") +
        theme_classic() +
        theme(plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))
    ggsave(file.path(output_dir, paste0(prefix, outcome_type, "_performance.png")), p, width = DEFAULT_PLOT_WIDTH, height = DEFAULT_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white")
}

#' Create survival curves stratified by GEP class for MFS
#'
#' Generates Kaplan-Meier survival curves for metastasis-free survival
#' stratified by GEP class with log-rank test p-values, using consistent
#' project styling.
#'
#' @param data Data frame with survival data
#' @param output_dir Output directory for saved plots
#' @param prefix Filename prefix
#' @return Invisibly returns NULL after saving plots
create_mfs_survival_curves <- function(data, output_dir, prefix, group_var = "biopsy1_gep", other_map = list(), dataset_name = "GEP Validation") {
    logger::log_info("Creating MFS survival curves by GEP class using existing survival analysis infrastructure")

    # Use the existing analyze_time_to_event_outcomes function that handles all the complexity
    # Set up output directories for the existing function
    temp_output_dirs <- list(
        obj1_os = output_dir,
        obj1_pfs = output_dir,
        obj3_pfs2 = output_dir,
        obj4_mfs = output_dir, # Add missing MFS mapping
        baseline_characteristics = output_dir
    )

    # Call the existing comprehensive survival analysis function (now supports non-binary grouping)
    logger::log_info(sprintf("DEBUG: About to call analyze_time_to_event_outcomes with group_var = %s", group_var))
    logger::log_info(sprintf(
        "DEBUG: Data dimensions: %d rows, unique groups: %s",
        nrow(data), paste(unique(data[[group_var]]), collapse = ", ")
    ))

    km_result <- tryCatch(
        {
            analyze_time_to_event_outcomes(
                data = data,
                time_var = "tt_mets_months",
                event_var = "mets_event",
                group_var = group_var,
                confounders = NULL, # No confounders for MFS plotting
                ylab = "Metastasis-Free Survival Probability",
                analysis_type = "post_treatment_only",
                dataset_name = dataset_name,
                other_map = other_map,
                output_dirs = temp_output_dirs,
                prefix = prefix
            )
        },
        error = function(e) {
            logger::log_error(sprintf("ERROR in analyze_time_to_event_outcomes: %s", e$message))
            return(NULL)
        }
    )

    if (!is.null(km_result$plot)) {
        logger::log_info("MFS survival curves created successfully using existing infrastructure")
    } else {
        logger::log_warn("MFS survival curves creation returned no plot")
    }

    invisible(NULL)
}

#' Create MSS cumulative incidence curves using ggsurvfit
#'
#' Creates cumulative incidence function plots for competing risks analysis using
#' the ggsurvfit package, which handles axis formatting automatically.
#'
#' @param data data.frame Raw data for CIF curves
#' @param timepoint numeric Timepoint in years for analysis
#' @param output_dir character Destination directory
#' @param prefix character Filename prefix
#' @param group_var character Grouping variable name
#' @param time_var character Time variable name (in months)
#' @param other_map list Additional variable mappings
#' @param competing_results list Competing risks results (optional)
#' @return Invisibly returns NULL after saving plots
create_mss_cumulative_incidence_curves <- function(data, timepoint, output_dir, prefix, group_var = "biopsy1_gep", time_var = "tt_death_months", other_map = list(), competing_results = NULL) {
    logger::log_info(sprintf("Creating MSS cumulative incidence curves by GEP class for %d-year timepoint using ggsurvfit", timepoint))

    # Convert timepoint to months for consistency with survival time units
    timepoint_months <- timepoint * 12

    # Ensure group_var is a string
    group_var_char <- as.character(group_var)

    # Prepare data for competing risk analysis using pre-processed variables
    # CRITICAL: Apply eligibility filters to prevent segmentation fault
    logger::log_info("Applying MSS analysis eligibility filter for cumulative incidence curves")
    surv_data <- data %>% dplyr::filter(mss_analysis_eligible)
    logger::log_info(sprintf("Before MSS eligibility filter: %d rows", nrow(data)))
    logger::log_info(sprintf("After MSS eligibility filter: %d rows", nrow(surv_data)))

    # Use pre-processed variables instead of recreating them
    # For 5-year analysis, use the specific 5-year time variable
    if (timepoint == 5) {
        time_var_char <- "tt_mss_5yr"
    } else {
        time_var_char <- as.character(time_var)
    }
    event_type_var_char <- paste0("event_type_mss_", timepoint, "yr")

    # Check that required variables exist
    if (!time_var_char %in% names(surv_data)) {
        logger::log_error(sprintf("Required variable '%s' not found in data", time_var_char))
        return(invisible(NULL))
    }
    if (!event_type_var_char %in% names(surv_data)) {
        logger::log_error(sprintf("Required variable '%s' not found in data", event_type_var_char))
        return(invisible(NULL))
    }

    logger::log_info("Filtering data for cumulative incidence curves by group and time variables")
    surv_data <- surv_data %>%
        dplyr::filter(!is.na(.data[[group_var_char]]), !is.na(.data[[time_var_char]])) %>%
        as.data.frame() # Convert to data.frame to avoid tibble subsetting issues
    
    # Convert event variable to factor for tidycmprsk compatibility
    # tidycmprsk expects first level to be censored (0), then competing events (1, 2, etc.)
    surv_data[[event_type_var_char]] <- factor(
        surv_data[[event_type_var_char]],
        levels = c(0, 1, 2),
        labels = c("Censored", "Melanoma Death", "Other Death")
    )

    if (nrow(surv_data) == 0 || length(unique(surv_data[[group_var_char]])) < 2) {
        logger::log_warn("Insufficient MSS data/groups for cumulative incidence curves")
        return(invisible(NULL))
    }

    # Create enhanced title with statistical context
    base_title <- sprintf("Melanoma-Specific Death by GEP Class (%d-Year Analysis)", timepoint)

    # Add competing risks statistics to title if available
    if (!is.null(competing_results)) {
        # Extract Fine-Gray model results if available
        fine_gray_stats <- ""
        if (!is.null(competing_results$fine_gray) && nrow(competing_results$fine_gray) > 0) {
            # Get the most significant result
            sig_results <- competing_results$fine_gray[competing_results$fine_gray$p_value < 0.05, ]
            if (nrow(sig_results) > 0) {
                best_result <- sig_results[which.min(sig_results$p_value), ]

                # Format p-value with scientific notation for very small values
                p_formatted <- if (best_result$p_value < 0.001) {
                    sprintf("%.1e", best_result$p_value)
                } else {
                    sprintf("%.3f", best_result$p_value)
                }

                fine_gray_stats <- sprintf(
                    "\nFine-Gray SHR = %.2f (95%% CI: %.2f-%.2f), p %s",
                    best_result$SHR, best_result$CI_Lower, best_result$CI_Upper, p_formatted
                )
            }
        }

        # Extract cause-specific Cox results if available
        csc_stats <- ""
        if (!is.null(competing_results$cause_specific_cox) && nrow(competing_results$cause_specific_cox) > 0) {
            # Get the most significant result
            sig_results <- competing_results$cause_specific_cox[competing_results$cause_specific_cox$p_value < 0.05, ]
            if (nrow(sig_results) > 0) {
                best_result <- sig_results[which.min(sig_results$p_value), ]

                # Format p-value with scientific notation for very small values
                p_formatted <- if (best_result$p_value < 0.001) {
                    sprintf("%.1e", best_result$p_value)
                } else {
                    sprintf("%.3f", best_result$p_value)
                }

                csc_stats <- sprintf(
                    "\nCause-Specific HR = %.2f (95%% CI: %.2f-%.2f), p %s",
                    best_result$HR, best_result$CI_Lower, best_result$CI_Upper, p_formatted
                )
            }
        }

        # Combine statistics
        if (fine_gray_stats != "" || csc_stats != "") {
            plot_title <- paste0(base_title, fine_gray_stats, csc_stats)
        } else {
            plot_title <- paste0(base_title, "\n(Competing risks models not fitted due to insufficient data quality)")
        }
    } else {
        plot_title <- paste0(base_title, "\n(No competing risks results available)")
    }

    # Use ggsurvfit's ggcuminc function for much simpler CIF plotting
    # This automatically handles axis formatting and prevents the "48" tick mark issue

    # Create the CIF plot using ggcuminc with tidycmprsk
    # First create the competing risks object with tidycmprsk::cuminc
    ci_obj <- tidycmprsk::cuminc(
        formula = as.formula(paste("Surv(", time_var_char, ",", event_type_var_char, ") ~", group_var_char)),
        data = surv_data
    )
    
    # Then use ggcuminc to plot it
    p <- ggcuminc(ci_obj, outcome = "Melanoma Death") + # Focus on melanoma death
        ggplot2::labs(
            title = plot_title,
            subtitle = sprintf(
                "Competing Risks Analysis: %d patients, %d melanoma deaths",
                nrow(surv_data),
                sum(surv_data[[event_type_var_char]] == 1)
            ),
            x = "Time (years)",
            y = "Cumulative Incidence of Melanoma Death",
            color = "GEP Class",
            caption = "Fine-Gray subdistribution hazard ratios shown for significant associations\n* p < 0.05 indicates significant difference"
        ) +
        ggplot2::theme_classic() +
        ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = "white"),
            panel.background = ggplot2::element_rect(fill = "white"),
            plot.title = ggplot2::element_text(size = 12, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 10, color = "darkgray"),
            plot.caption = ggplot2::element_text(size = 9, color = "darkgray", hjust = 0),
            legend.position = "bottom",
            legend.title = ggplot2::element_text(face = "bold"),
            axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
            axis.ticks.x = ggplot2::element_line(color = "black", linewidth = 0.5)
        ) +
        ggplot2::scale_color_manual(values = get_palette_by_variable(group_var_char, unique(surv_data[[group_var_char]]))) +
        ggplot2::coord_cartesian(xlim = c(0, timepoint)) # Limit to timepoint in years

    # Add annotation text with competing risks statistics if available
    annotation_text <- ""

    if (!is.null(competing_results)) {
        # Add Fine-Gray results
        if (!is.null(competing_results$fine_gray) && nrow(competing_results$fine_gray) > 0) {
            annotation_text <- paste0(annotation_text, "Fine-Gray Models:\n")
            for (i in seq_len(min(3, nrow(competing_results$fine_gray)))) {
                result <- competing_results$fine_gray[i, ]
                sig_indicator <- if (result$p_value < 0.05) " *" else ""

                # Format p-value with scientific notation for very small values
                p_formatted <- if (result$p_value < 0.001) {
                    sprintf("%.1e", result$p_value)
                } else {
                    sprintf("%.3f", result$p_value)
                }

                annotation_text <- paste0(
                    annotation_text,
                    sprintf(
                        "%s: SHR = %.2f (%.2f-%.2f), p %s%s\n",
                        result$GEP_Class, result$SHR, result$CI_Lower, result$CI_Upper, p_formatted, sig_indicator
                    )
                )
            }
        } else {
            annotation_text <- paste0(annotation_text, "Fine-Gray Model: Not fitted due to insufficient data quality\n")
        }

        # Add cause-specific Cox results
        if (!is.null(competing_results$cause_specific_cox) && nrow(competing_results$cause_specific_cox) > 0) {
            annotation_text <- paste0(annotation_text, "\nCause-Specific Cox Models:\n")
            for (i in seq_len(min(3, nrow(competing_results$cause_specific_cox)))) {
                result <- competing_results$cause_specific_cox[i, ]
                sig_indicator <- if (result$p_value < 0.05) " *" else ""

                # Format p-value with scientific notation for very small values
                p_formatted <- if (result$p_value < 0.001) {
                    sprintf("%.1e", result$p_value)
                } else {
                    sprintf("%.3f", result$p_value)
                }

                annotation_text <- paste0(
                    annotation_text,
                    sprintf(
                        "%s: HR = %.2f (%.2f-%.2f), p %s%s\n",
                        result$GEP_Class, result$HR, result$CI_Lower, result$CI_Upper, p_formatted, sig_indicator
                    )
                )
            }
        } else {
            annotation_text <- paste0(annotation_text, "\nCause-Specific Cox Model: Not fitted due to insufficient data quality\n")
        }

        # Add explanation for why models weren't fitted
        if (is.null(competing_results$fine_gray) && is.null(competing_results$cause_specific_cox)) {
            annotation_text <- paste0(
                annotation_text, "\nNote: Competing risks models require:\n",
                "• Minimum 10 patients per group\n",
                "• At least one event per group\n",
                "• Sufficient event distribution"
            )
        }
    } else {
        annotation_text <- "No competing risks results available\n\nNote: Models require sufficient data quality and event distribution"
    }

    # Add annotation to plot
    p <- p + ggplot2::annotate(
        "text",
        x = timepoint * 0.7, # Move left to 70% of x-axis to avoid cutoff
        y = 0.7, # Move down to 70% of y-axis for better positioning
        label = annotation_text,
        size = 3,
        color = "darkred",
        fontface = "bold",
        hjust = 0, # Left-align text to prevent cutoff
        vjust = 0.5
    ) +
    ggplot2::theme(
        plot.margin = ggplot2::unit(c(1, 2, 1, 1), "cm") # Increase right margin to accommodate annotation
    )

    plot_path <- file.path(output_dir, paste0(prefix, "mss_cumulative_incidence_curves.png"))
    ggplot2::ggsave(plot_path, p, width = SURVIVAL_PLOT_WIDTH, height = SURVIVAL_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white")
    logger::log_info(sprintf("MSS cumulative incidence curves saved: %s", plot_path))

    # Log competing risks analysis summary if available
    if (!is.null(competing_results)) {
        if (!is.null(competing_results$fine_gray) && nrow(competing_results$fine_gray) > 0) {
            sig_count <- sum(competing_results$fine_gray$p_value < 0.05, na.rm = TRUE)
            logger::log_info(sprintf("Competing risks analysis: %d significant Fine-Gray associations (p < 0.05)", sig_count))
        }
    }


    invisible(NULL)
}
