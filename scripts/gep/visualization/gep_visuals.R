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
#' @param confounders Character vector of confounders to use in adjusted MFS survival models
create_mfs_gep_visuals <- function(mfs_results, mfs_data, output_dir, prefix, confounders = NULL, group_var = "biopsy1_gep", model_group_var = group_var, dataset_name = "GEP Validation", validation_output_dir = output_dir, output_dirs = NULL) {
    # Directly write into centralized visuals folder created by create_output_structure()
    prame_results <- NULL
    if (!is.null(mfs_results$prame_analysis)) {
        prame_results <- mfs_results$prame_analysis
    }

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
        create_mfs_survival_curves(
            mfs_data,
            output_dir,
            prefix,
            confounders = confounders,
            group_var = group_var,
            model_group_var = model_group_var,
            dataset_name = dataset_name,
            output_dirs = output_dirs
        )
    }

    create_prame_incremental_value_plot(prame_results, "MFS", validation_output_dir, prefix)

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
create_mss_gep_visuals <- function(mss_results, mss_data, output_dir, prefix, group_var = "biopsy1_gep", technical_group_var = NULL, cif_output_dir = output_dir, validation_output_dir = output_dir) {
    # Directly write into centralized visuals folder created by create_output_structure()
    prame_results <- mss_results$prame_results %||% NULL

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
            mss_data, 5, cif_output_dir, prefix,
            group_var = group_var,
            technical_group_var = technical_group_var,
            competing_results = competing_results_5yr
        )
    }

    create_prame_incremental_value_plot(prame_results, "MSS", validation_output_dir, prefix)

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

#' Create a PRAME incremental-value delta-C plot
#'
#' Save one outcome-specific dot-and-whisker plot showing the delta Harrell's C
#' estimate and its bootstrap confidence interval for each timepoint.
#'
#' @param prame_results PRAME comparison result object.
#' @param outcome_type Character outcome label (`"MFS"` or `"MSS"`).
#' @param output_dir Character destination directory.
#' @param prefix Character filename prefix.
create_prame_incremental_value_plot <- function(prame_results, outcome_type, output_dir, prefix) {
    if (is.null(prame_results) || is.null(prame_results$comparison_results) || !is.list(prame_results$comparison_results)) {
        return(invisible(NULL))
    }

    plot_rows <- lapply(prame_results$comparison_results, function(result) {
        if (!is.list(result) || !is.finite(result$delta_harrell_c %||% NA_real_)) {
            return(NULL)
        }

        data.frame(
            Timepoint = paste0(result$timepoint, "yr"),
            Delta_Harrell_C = as.numeric(result$delta_harrell_c),
            Delta_CI_Lower = as.numeric(result$delta_ci_lower),
            Delta_CI_Upper = as.numeric(result$delta_ci_upper),
            Analysis_Tier = result$analysis_tier %||% NA_character_,
            stringsAsFactors = FALSE
        )
    })

    plot_rows <- Filter(Negate(is.null), plot_rows)
    if (length(plot_rows) == 0) {
        return(invisible(NULL))
    }

    plot_data <- do.call(rbind, plot_rows)
    plot_data$timepoint_numeric <- suppressWarnings(as.numeric(gsub("yr", "", plot_data$Timepoint)))
    plot_data <- plot_data[order(plot_data$timepoint_numeric, decreasing = TRUE), , drop = FALSE]
    plot_data$Timepoint <- factor(plot_data$Timepoint, levels = plot_data$Timepoint)

    subtitle_label <- unique(stats::na.omit(plot_data$Analysis_Tier))
    if (length(subtitle_label) == 0) {
        subtitle_label <- NA_character_
    }

    x_range <- range(
        c(plot_data$Delta_Harrell_C, plot_data$Delta_CI_Lower, plot_data$Delta_CI_Upper, 0),
        na.rm = TRUE
    )
    x_padding <- max(diff(x_range) * 0.04, 0.003)
    plot_width <- 8.5
    plot_height <- max(4.5, 1.3 * nrow(plot_data) + 0.5)

    p <- ggplot(plot_data, aes(x = Delta_Harrell_C, y = Timepoint)) +
        geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8, color = "gray50") +
        geom_errorbarh(
            aes(xmin = Delta_CI_Lower, xmax = Delta_CI_Upper),
            height = 0.10,
            linewidth = 0.9,
            color = "gray35",
            na.rm = TRUE
        ) +
        geom_point(size = 4.5, color = get_qualitative_palette(1)[1]) +
        labs(
            title = sprintf("%s PRAME Incremental Discrimination", outcome_type),
            subtitle = ifelse(is.na(subtitle_label), NULL, sprintf("%s analysis", subtitle_label)),
            x = "Delta Harrell's C (GEP + PRAME minus GEP only)",
            y = "Timepoint"
        ) +
        scale_x_continuous(
            limits = c(x_range[1] - x_padding, x_range[2] + x_padding),
            expand = expansion(mult = c(0.01, 0.01))
        ) +
        scale_y_discrete(expand = expansion(mult = c(0.08, 0.08))) +
        theme_classic(base_size = 18) +
        theme(
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            plot.title = element_text(size = 22, face = "bold"),
            plot.subtitle = element_text(size = 18, margin = margin(b = 8)),
            axis.title = element_text(size = 19),
            axis.text = element_text(size = 16),
            plot.margin = margin(8, 12, 8, 8),
            axis.line = element_line(linewidth = 0.9),
            axis.ticks = element_line(linewidth = 0.9)
        )

    plot_name <- if (identical(outcome_type, "MFS")) {
        paste0(prefix, "mfs_prame_delta_c.png")
    } else {
        paste0(prefix, "mss_prame_delta_c.png")
    }

    ggsave(
        file.path(output_dir, plot_name),
        p,
        width = plot_width,
        height = plot_height,
        dpi = PLOT_DPI,
        bg = "white"
    )

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
            if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
            plot_path <- file.path(output_dir, paste0(prefix, outcome_type, "_calibration_", tp_name, ".png"))
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
#' @param confounders Character vector of confounders to use in adjusted MFS survival models
#' @return Invisibly returns NULL after saving plots
create_mfs_survival_curves <- function(data, output_dir, prefix, confounders = NULL, group_var = "biopsy1_gep", model_group_var = group_var, dataset_name = "GEP Validation", output_dirs = NULL) {
    logger::log_info("Creating MFS survival curves by GEP class using existing survival analysis infrastructure")

    gep_prame_display_order <- c(
        "Class 1 PRAME Negative",
        "Class 1 PRAME Positive",
        "Class 2 PRAME Negative",
        "Class 2 PRAME Positive",
        "GEP Failed/Indeterminate",
        "GEP Not Tested"
    )
    legend_labels <- if (identical(group_var, "biopsy1_gep")) gep_prame_display_order else NULL

    # Use the existing analyze_time_to_event_outcomes function with centralized output mapping
    resolved_output_dirs <- output_dirs
    if (is.null(resolved_output_dirs)) {
        resolved_output_dirs <- list(
            obj1_os = output_dir,
            obj1_pfs = output_dir,
            obj3_pfs2 = output_dir,
            obj4_mfs = output_dir,
            baseline_characteristics = output_dir
        )
    }

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
                model_group_var = model_group_var,
                confounders = confounders,
                ylab = "Metastasis-Free Survival Probability",
                analysis_type = "post_treatment_only",
                dataset_name = dataset_name,
                legend_labels = legend_labels,
                output_dirs = resolved_output_dirs,
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

    create_mfs_simplified_survival_curves(
        data = data,
        output_dir = output_dir,
        km_output_dir = resolve_obj4_output_dir(resolved_output_dirs, output_dir, "km"),
        prefix = prefix,
        dataset_name = dataset_name
    )

    create_mfs_simple_binary_survival_analysis(
        data = data,
        output_dir = output_dir,
        prefix = prefix,
        dataset_name = dataset_name,
        confounders = confounders,
        output_dirs = resolved_output_dirs
    )

    invisible(NULL)
}

#' Create binary simple-GEP MFS survival analysis using the standard workflow
#'
#' Reuses the existing KM/Cox/RMST pipeline for a Class 1 vs Class 2 analysis,
#' excluding GEP Not Tested and GEP Failed/Indeterminate.
#'
#' @param data Data frame with survival data and GEP labels
#' @param output_dir Output directory for saved plots and tables
#' @param prefix Filename prefix
#' @param dataset_name Dataset label used in subtitles and reports
#' @param confounders Character vector of confounders to use in adjusted MFS survival models
#' @return Invisibly returns the survival-analysis result list or NULL
create_mfs_simple_binary_survival_analysis <- function(data, output_dir, prefix, dataset_name = "GEP Validation", confounders = NULL, output_dirs = NULL) {
    logger::log_info("Creating binary simple-GEP MFS survival analysis using the standard survival workflow")

    plot_data <- data %>%
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

    if (nrow(plot_data) == 0 || length(unique(stats::na.omit(plot_data$gep_class_simple))) < 2) {
        logger::log_warn("Insufficient data/groups for binary simple-GEP MFS survival analysis")
        return(invisible(NULL))
    }

    group_summary <- plot_data %>%
        dplyr::group_by(.data$gep_class_simple) %>%
        dplyr::summarise(
            n = dplyr::n(),
            metastasis_events = sum(.data$mets_event == 1, na.rm = TRUE),
            .groups = "drop"
        )
    logger::log_info(sprintf(
        "Binary simple-GEP MFS groups: %s",
        paste(
            sprintf(
                "%s n=%d events=%d",
                group_summary$gep_class_simple,
                group_summary$n,
                group_summary$metastasis_events
            ),
            collapse = ", "
        )
    ))

    resolved_output_dirs <- output_dirs
    if (is.null(resolved_output_dirs)) {
        resolved_output_dirs <- list(
            obj1_os = output_dir,
            obj1_pfs = output_dir,
            obj3_pfs2 = output_dir,
            obj4_mfs = output_dir,
            baseline_characteristics = output_dir
        )
    }

    binary_prefix <- paste0(prefix, "simple_gep_binary_")
    binary_dataset_name <- if (!is.null(dataset_name)) {
        paste0(dataset_name, " - Simple GEP (Class 1 vs Class 2)")
    } else {
        "Simple GEP (Class 1 vs Class 2)"
    }

    binary_result <- tryCatch(
        {
            analyze_time_to_event_outcomes(
                data = plot_data,
                time_var = "tt_mets_months",
                event_var = "mets_event",
                group_var = "gep_class_simple",
                model_group_var = "gep_class_simple",
                confounders = confounders,
                ylab = "Metastasis-Free Survival Probability",
                analysis_type = "post_treatment_only",
                dataset_name = binary_dataset_name,
                legend_labels = c("Class 1", "Class 2"),
                output_dirs = resolved_output_dirs,
                prefix = binary_prefix
            )
        },
        error = function(e) {
            logger::log_error(sprintf("ERROR in binary simple-GEP MFS survival analysis: %s", e$message))
            NULL
        }
    )

    if (!is.null(binary_result$plot)) {
        logger::log_info("Binary simple-GEP MFS survival analysis created successfully")
    } else {
        logger::log_warn("Binary simple-GEP MFS survival analysis returned no plot")
    }

    invisible(binary_result)
}

#' Create simplified MFS survival curves for Class 1, Class 2, and GEP Not Tested
#'
#' Adds a reader-facing companion KM plot that collapses PRAME subgroups,
#' retains GEP Not Tested, and excludes GEP Failed/Indeterminate.
#'
#' @param data Data frame with survival data and GEP labels
#' @param output_dir Output directory for saved plots
#' @param prefix Filename prefix
#' @param dataset_name Dataset label used in the subtitle
#' @param km_output_dir Directory used for the saved simplified KM figure.
#' @param return_plot When `TRUE`, returns the assembled plot objects for
#'   verification or testing instead of only writing files.
#' @param save_plot When `FALSE`, skips writing the PNG to disk.
#'
#' @return Invisibly returns `NULL` after saving plots, or a list of plot
#'   objects when `return_plot = TRUE`.
create_mfs_simplified_survival_curves <- function(data, output_dir, prefix, dataset_name = "GEP Validation", km_output_dir = output_dir, return_plot = FALSE, save_plot = TRUE) {
    logger::log_info("Creating simplified MFS survival curves for Class 1, Class 2, and GEP Not Tested")

    plot_data <- data %>%
        dplyr::mutate(
            gep_km_simple = dplyr::case_when(
                .data$biopsy1_gep == "GEP Not Tested" ~ "GEP Not Tested",
                .data$gep_class_simple %in% c("Class 1", "Class 2") ~ as.character(.data$gep_class_simple),
                TRUE ~ NA_character_
            )
        ) %>%
        dplyr::filter(
            !is.na(.data$gep_km_simple),
            !is.na(.data$tt_mets_months),
            !is.na(.data$mets_event),
            .data$tt_mets_months >= 0
        ) %>%
        dplyr::mutate(
            gep_km_simple = factor(
                .data$gep_km_simple,
                levels = c("Class 1", "Class 2", "GEP Not Tested")
            )
        ) %>%
        as.data.frame()

    present_levels <- levels(droplevels(plot_data$gep_km_simple))
    if (nrow(plot_data) == 0 || length(unique(stats::na.omit(plot_data$gep_km_simple))) < 2) {
        logger::log_warn("Insufficient data/groups for simplified MFS survival curves")
        return(invisible(NULL))
    }

    logger::log_info(sprintf(
        "Simplified MFS KM groups: %s",
        paste(
            capture.output(print(table(plot_data$gep_km_simple, useNA = "no"))),
            collapse = " "
        )
    ))

    surv_fit <- survival::survfit(
        survival::Surv(tt_mets_months, mets_event) ~ gep_km_simple,
        data = plot_data
    )

    raw_max_time <- max(plot_data$tt_mets_months, na.rm = TRUE)
    max_time <- min(raw_max_time, SURVIVAL_XAXIS_MAX_MONTHS)
    base_by <- if (max_time <= 60) 6 else 12
    x_breaks <- seq(0, ceiling(max_time / base_by) * base_by, by = base_by)
    plot_scale <- SURVIVAL_PLOT_SCALE
    color_palette <- get_palette_by_variable("biopsy1_gep", present_levels)
    surv_plot <- survminer::ggsurvplot(
        fit = surv_fit,
        data = plot_data,
        palette = color_palette,
        risk.table = TRUE,
        conf.int = FALSE,
        pval = TRUE,
        pval.size = 6 * plot_scale,
        title = paste("Kaplan-Meier Survival Curves", "Metastasis-Free Survival Probability", sep = "\n"),
        subtitle = paste(
            c(
                if (!is.null(dataset_name)) paste("Cohort:", dataset_name) else NULL,
                "Simplified display: Class 1, Class 2, and GEP Not Tested"
            ),
            collapse = "\n"
        ),
        xlab = "Time (months)",
        ylab = "Metastasis-Free Survival Probability",
        risk.table.height = 0.18,
        ggtheme = ggplot2::theme_minimal(),
        break.time.by = base_by,
        xlim = c(0, max(x_breaks)),
        ylim = c(0, 1),
        legend.labs = present_levels,
        risk.table.y.text = TRUE,
        tables.y.text = TRUE,
        risk.table.title = "Number at risk",
        font.x = 14 * plot_scale,
        font.y = 14 * plot_scale,
        font.tickslab = 12 * plot_scale,
        font.legend = 14 * plot_scale,
        censor.size = 7 * plot_scale,
        size = 1.2 * plot_scale
    )

    surv_plot$plot <- remove_plot_scales(surv_plot$plot, aesthetics = c("y"))
    surv_plot$table <- remove_plot_scales(surv_plot$table, aesthetics = c("y"))

    surv_plot$plot <- surv_plot$plot +
        ggplot2::guides(color = ggplot2::guide_legend(ncol = 1, byrow = TRUE)) +
        ggplot2::theme(
            legend.position = "bottom",
            legend.box = "vertical",
            legend.text = ggplot2::element_text(size = 16 * plot_scale, color = "black"),
            legend.title = ggplot2::element_text(size = 16 * plot_scale, color = "black"),
            axis.title = ggplot2::element_text(size = 18 * plot_scale, color = "black"),
            axis.title.x = ggplot2::element_text(size = 18 * plot_scale, color = "black", face = "bold", margin = ggplot2::margin(t = 15, r = 0, b = 0, l = 0)),
            axis.title.y = ggplot2::element_text(size = 18 * plot_scale, color = "black", face = "bold", margin = ggplot2::margin(t = 0, r = 6, b = 0, l = 0)),
            axis.text = ggplot2::element_text(size = 14 * plot_scale, color = "black"),
            axis.text.x = ggplot2::element_text(color = "black"),
            axis.text.y = ggplot2::element_text(color = "black"),
            axis.line = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(size = 18 * plot_scale, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 14 * plot_scale),
            plot.margin = ggplot2::margin(t = 10, r = 10, b = 6, l = 4)
        ) +
        ggplot2::scale_y_continuous(
            limits = c(0, 1),
            breaks = seq(0, 1, by = 0.1),
            labels = function(x) x * 100,
            name = "Metastasis-Free Survival Probability (%)"
        ) +
        ggplot2::labs(x = "Time (months)") +
        ggplot2::geom_hline(yintercept = 0.5, linetype = "solid", color = "black", linewidth = 0.9)

    surv_plot$table <- surv_plot$table +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.title = ggplot2::element_text(size = 12 * plot_scale, color = "black"),
            axis.text.y = ggplot2::element_text(size = 12 * plot_scale, color = "black"),
            axis.text.x = ggplot2::element_text(size = 12 * plot_scale, color = "black"),
            strip.text = ggplot2::element_text(size = 12 * plot_scale, color = "black"),
            plot.margin = ggplot2::margin(t = 10, r = 10, b = 4, l = 10)
        )

    clean_strata_label <- function(x) {
        x_chr <- as.character(x)
        ifelse(grepl("=", x_chr), sub("^[^=]*=", "", x_chr), x_chr)
    }

    remap_risk_table_rows <- function(table_frame) {
        if (is.null(table_frame) || nrow(table_frame) == 0) {
            return(table_frame)
        }

        mapped_labels <- NULL
        if ("strata" %in% names(table_frame)) {
            candidate_labels <- clean_strata_label(table_frame$strata)
            if (all(stats::na.omit(candidate_labels) %in% present_levels)) {
                mapped_labels <- candidate_labels
            }
        }

        if (is.null(mapped_labels) && "y" %in% names(table_frame)) {
            y_as_integer <- suppressWarnings(as.integer(as.character(table_frame$y)))
            if (length(y_as_integer) > 0 && any(!is.na(y_as_integer))) {
                candidate_labels <- present_levels[y_as_integer]
                if (all(stats::na.omit(candidate_labels) %in% present_levels)) {
                    mapped_labels <- candidate_labels
                }
            }

            if (is.null(mapped_labels)) {
                candidate_labels <- clean_strata_label(table_frame$y)
                if (all(stats::na.omit(candidate_labels) %in% present_levels)) {
                    mapped_labels <- candidate_labels
                }
            }
        }

        if (is.null(mapped_labels)) {
            return(table_frame)
        }

        mapped_factor <- factor(mapped_labels, levels = present_levels)

        if ("strata" %in% names(table_frame)) {
            table_frame$strata <- mapped_factor
        }
        if ("y" %in% names(table_frame)) {
            table_frame$y <- mapped_factor
        }

        ordering_time <- if ("time" %in% names(table_frame)) {
            table_frame$time
        } else if ("x" %in% names(table_frame)) {
            table_frame$x
        } else {
            seq_len(nrow(table_frame))
        }

        table_frame[order(mapped_factor, ordering_time), , drop = FALSE]
    }

    if (length(surv_plot$table$layers) > 0) {
        for (i in seq_along(surv_plot$table$layers)) {
            if ("GeomText" %in% class(surv_plot$table$layers[[i]]$geom)) {
                surv_plot$table$layers[[i]]$aes_params$size <- 3.4 * plot_scale
            }

            if (!is.null(surv_plot$table$layers[[i]]$data)) {
                surv_plot$table$layers[[i]]$data <- remap_risk_table_rows(surv_plot$table$layers[[i]]$data)
            }
        }
    }

    surv_plot$table$data <- remap_risk_table_rows(surv_plot$table$data)
    surv_plot$table$mapping <- ggplot2::aes(
        x = time,
        y = strata,
        label = llabels,
        shape = strata
    )

    surv_plot$table <- surv_plot$table +
        ggplot2::scale_y_discrete(
            limits = rev(present_levels),
            expand = ggplot2::expansion(mult = c(0.18, 0.18))
        )

    combined_km <- cowplot::plot_grid(
        surv_plot$plot,
        surv_plot$table,
        ncol = 1,
        align = "v",
        rel_heights = c(0.78, 0.22)
    )

    simplified_plot_width <- 13
    simplified_plot_height <- min(
        KM_MAX_HEIGHT,
        max(
            SURVIVAL_PLOT_HEIGHT * 1.35,
            KM_BASE_HEIGHT + max(0, length(present_levels) - 2) * KM_HEIGHT_PER_STRATUM + 2.0
        )
    )

    if (save_plot) {
        target_km_dir <- ensure_output_dir(km_output_dir)
        km_path <- file.path(target_km_dir, paste0(prefix, "mfs_simplified_gep_km.png"))
        ggplot2::ggsave(
            km_path,
            combined_km,
            width = simplified_plot_width,
            height = simplified_plot_height,
            dpi = PLOT_DPI,
            bg = "white"
        )
        logger::log_info(sprintf("Simplified MFS survival curves saved: %s", km_path))
    }

    if (return_plot) {
        return(list(
            plot = surv_plot,
            combined_plot = combined_km,
            plot_data = plot_data,
            present_levels = present_levels
        ))
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
#' @param competing_results list Competing risks results (optional)
#' @return Invisibly returns NULL after saving plots
create_mss_cumulative_incidence_curves <- function(data, timepoint, output_dir, prefix, group_var = "biopsy1_gep", technical_group_var = NULL, time_var = "tt_death_months", competing_results = NULL) {
    logger::log_info(sprintf("Creating MSS cumulative incidence curves by GEP class for %d-year timepoint using ggsurvfit", timepoint))

    # Convert timepoint to months for consistency with survival time units
    timepoint_months <- timepoint * 12

    # Ensure group_var is a string
    group_var_char <- as.character(group_var)
    grouping_spec <- get_gep_grouping_spec(group_var_char)
    technical_group_spec <- if (is.null(technical_group_var)) {
        get_gep_grouping_spec(group_var_char)
    } else {
        get_gep_grouping_spec(as.character(technical_group_var))
    }
    simplified_display <- isTRUE(grouping_spec$reader_facing)

    # Prepare data for competing risk analysis using pre-processed variables
    logger::log_info("Preparing MSS visual dataset for cumulative incidence curves")
    surv_data <- data

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

    if (!is.null(grouping_spec$allowed_levels)) {
        surv_data <- surv_data %>%
            dplyr::filter(.data[[group_var_char]] %in% grouping_spec$allowed_levels) %>%
            as.data.frame()
    }

    melanoma_death_total <- sum(surv_data[[event_type_var_char]] == 1, na.rm = TRUE)

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
    base_title <- if (simplified_display) {
        sprintf("Melanoma-Specific Death by %s (%d-Year Analysis)", grouping_spec$label, timepoint)
    } else {
        sprintf("Melanoma-Specific Death by %s (%d-Year Analysis)", grouping_spec$label, timepoint)
    }

    describe_competing_risk_model_status <- function(model_key, model_label) {
        feasibility <- competing_results$feasibility %||% NULL
        if (is.null(feasibility) || is.null(feasibility$models[[model_key]])) {
            return(sprintf("%s not fitted", model_label))
        }

        model_status <- feasibility$models[[model_key]]
        if (identical(model_status$status, "eligible")) {
            return(sprintf("%s eligible", model_label))
        }

        reason <- model_status$reason %||% "feasibility criteria not met"
        sprintf("%s skipped: %s", model_label, reason)
    }

    # Add competing risks statistics to title if available
    if (!is.null(competing_results) && !simplified_display) {
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
            plot_title <- paste0(
                base_title,
                "\n(",
                paste(
                    describe_competing_risk_model_status("fine_gray", "Fine-Gray"),
                    describe_competing_risk_model_status("cause_specific_cox", "Cause-specific Cox"),
                    sep = " | "
                ),
                ")"
            )
        }
    } else {
        plot_title <- if (simplified_display) {
            paste0(base_title, "\n(Display curves grouped as Class 1 vs Class 2)")
        } else {
            paste0(base_title, "\n(No competing risks results available)")
        }
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
    p <- ggcuminc(ci_obj, outcome = "Melanoma Death")
    p <- remove_plot_scales(p, aesthetics = c("colour", "color", "x", "y"))
    p <- p + # Focus on melanoma death
        ggplot2::labs(
            title = plot_title,
            subtitle = sprintf(
                "Competing Risks Analysis: %d patients, %d melanoma deaths",
                nrow(surv_data),
                melanoma_death_total
            ),
            x = "Time (years)",
            y = "Cumulative Incidence of Melanoma Death",
            color = grouping_spec$legend_title,
            caption = if (simplified_display) {
                "Display curves use simplified Class 1 vs Class 2 grouping for readability.\nTechnical competing-risk model summaries remain available in the companion workbook."
            } else {
                "Fine-Gray subdistribution hazard ratios shown for significant associations\n* p < 0.05 indicates significant difference"
            }
        ) +
        ggplot2::theme_classic() +
        ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = "white"),
            panel.background = ggplot2::element_rect(fill = "white"),
            plot.title = ggplot2::element_text(size = 16, face = "bold", lineheight = 1.05),
            plot.subtitle = ggplot2::element_text(size = 12, color = "darkgray"),
            plot.caption = ggplot2::element_text(size = 10.5, color = "darkgray", hjust = 0, lineheight = 1.1),
            legend.position = "bottom",
            legend.title = ggplot2::element_text(face = "bold", size = 12),
            legend.text = ggplot2::element_text(size = 11),
            axis.title = ggplot2::element_text(size = 13.5),
            axis.text = ggplot2::element_text(size = 11.5),
            axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
            axis.ticks.x = ggplot2::element_line(color = "black", linewidth = 0.5)
        ) +
        ggplot2::scale_color_manual(values = get_palette_by_variable(group_var_char, unique(surv_data[[group_var_char]]))) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.02))) +
        ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.04)),
            labels = scales::label_percent(accuracy = 1)
        ) +
        ggplot2::coord_cartesian(xlim = c(0, timepoint), expand = FALSE) # Limit to timepoint in years with tighter margins

    caption_lines <- character()

    if (simplified_display) {
        caption_lines <- c(
            caption_lines,
            "Display note: curves are collapsed to Class 1 vs Class 2 for reader-facing visualization.",
            sprintf(
                "Technical competing-risk models and tables remain grouped by the %s labels.",
                technical_group_spec$var
            )
        )
    } else if (!is.null(competing_results)) {
        # Add Fine-Gray results
        if (!is.null(competing_results$fine_gray) && nrow(competing_results$fine_gray) > 0) {
            caption_lines <- c(caption_lines, "Fine-Gray models:")
            for (i in seq_len(min(3, nrow(competing_results$fine_gray)))) {
                result <- competing_results$fine_gray[i, ]
                sig_indicator <- if (result$p_value < 0.05) " *" else ""

                # Format p-value with scientific notation for very small values
                p_formatted <- if (result$p_value < 0.001) {
                    sprintf("%.1e", result$p_value)
                } else {
                    sprintf("%.3f", result$p_value)
                }

                caption_lines <- c(
                    caption_lines,
                    sprintf(
                        " • %s: SHR = %.2f (%.2f-%.2f), p %s%s",
                        result$GEP_Class, result$SHR, result$CI_Lower, result$CI_Upper, p_formatted, sig_indicator
                    )
                )
            }
        } else {
            caption_lines <- c(caption_lines, describe_competing_risk_model_status("fine_gray", "Fine-Gray"))
        }

        # Add cause-specific Cox results
        if (!is.null(competing_results$cause_specific_cox) && nrow(competing_results$cause_specific_cox) > 0) {
            caption_lines <- c(caption_lines, "Cause-specific Cox models:")
            for (i in seq_len(min(3, nrow(competing_results$cause_specific_cox)))) {
                result <- competing_results$cause_specific_cox[i, ]
                sig_indicator <- if (result$p_value < 0.05) " *" else ""

                # Format p-value with scientific notation for very small values
                p_formatted <- if (result$p_value < 0.001) {
                    sprintf("%.1e", result$p_value)
                } else {
                    sprintf("%.3f", result$p_value)
                }

                caption_lines <- c(
                    caption_lines,
                    sprintf(
                        " • %s: HR = %.2f (%.2f-%.2f), p %s%s",
                        result$GEP_Class, result$HR, result$CI_Lower, result$CI_Upper, p_formatted, sig_indicator
                    )
                )
            }
        } else {
            caption_lines <- c(caption_lines, describe_competing_risk_model_status("cause_specific_cox", "Cause-specific Cox"))
        }

        if ((is.null(competing_results$fine_gray) || nrow(competing_results$fine_gray) == 0) &&
            (is.null(competing_results$cause_specific_cox) || nrow(competing_results$cause_specific_cox) == 0)) {
            caption_lines <- c(
                caption_lines,
                "Note: See technical workbook feasibility sheet for group-level event and size checks."
            )
        }
        if (!is.null(competing_results$cif_with_ci) &&
            "status" %in% names(competing_results$cif_with_ci) &&
            any(competing_results$cif_with_ci$status != "completed", na.rm = TRUE)) {
            skipped_cif_groups <- competing_results$cif_with_ci %>%
                dplyr::filter(status != "completed") %>%
                dplyr::mutate(reason_label = dplyr::coalesce(skip_reason, status)) %>%
                dplyr::transmute(label = sprintf("%s (%s)", Group, reason_label)) %>%
                dplyr::pull(label)
            caption_lines <- c(
                caption_lines,
                paste0("CIF CI summary skipped for: ", paste(skipped_cif_groups, collapse = ", "))
            )
        }
    } else {
        caption_lines <- c(
            "Competing risks models not available.",
            "Note: Models require sufficient data quality and event distribution."
        )
    }

    caption_text <- paste(caption_lines, collapse = "\n")

    legend_levels <- unique(stats::na.omit(surv_data[[group_var_char]]))
    legend_cols <- if (length(legend_levels) > 4) 2 else 1

    p <- p +
        ggplot2::guides(color = ggplot2::guide_legend(ncol = legend_cols, byrow = TRUE)) +
        ggplot2::theme(legend.box = "vertical", legend.justification = "center") +
        ggplot2::theme(
            plot.margin = ggplot2::unit(c(0.45, 0.45, 0.7, 0.45), "cm"),
            plot.caption = ggplot2::element_text(hjust = 0, size = 10.5, color = "grey40", lineheight = 1.1)
        ) +
        ggplot2::labs(caption = caption_text)

    if (length(p$layers) > 0) {
        for (i in seq_along(p$layers)) {
            geom_classes <- class(p$layers[[i]]$geom)
            if ("GeomStep" %in% geom_classes || "GeomLine" %in% geom_classes) {
                p$layers[[i]]$aes_params$linewidth <- 1.15
            }
        }
    }

    plot_path <- file.path(output_dir, paste0(prefix, "mss_cumulative_incidence_curves.png"))
    ggplot2::ggsave(plot_path, p, width = CIF_PLOT_WIDTH, height = CIF_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white")
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
