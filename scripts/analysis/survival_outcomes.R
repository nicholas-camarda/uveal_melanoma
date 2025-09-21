# Survival Outcomes Analysis

#' Analyze time-to-event outcomes (KM + Cox)
#' @param data Data frame
#' @param time_var Time variable
#' @param event_var Event indicator
#' @param group_var Grouping variable (default 'treatment_group')
#' @param confounders Confounders
#' @param ylab Plot y-axis label
#' @param analysis_type 'post_treatment_only' or 'all_patients'
#' @param dataset_name Dataset label
#' @param legend_labels Optional legend labels
#' @param other_map Optional mapping for 'Other'
#' @param output_dirs Output directories by analysis type
#' @param prefix File prefix for outputs
#' @return List with KM/cox outputs and diagnostics
analyze_time_to_event_outcomes <- function(data, time_var, event_var, group_var = "treatment_group", confounders = NULL, ylab = "Survival Probability", analysis_type = "post_treatment_only", dataset_name = NULL, legend_labels = NULL, other_map = list(), output_dirs = NULL, prefix = NULL) {
    # Check that there are at least two groups for analysis; otherwise, skip Cox model
    if (length(unique(data[[group_var]])) < 2) {
        warning(sprintf("Only one level of %s present; skipping cox model.", group_var))
        return(list(
            fit = NULL,
            plot = NULL,
            median_times = NULL,
            cox_model = NULL,
            cox_table = NULL
        ))
    }

    # Filter data based on analysis type
    fix_event_data <- if (analysis_type == "post_treatment_only") {
        data %>% dplyr::filter(!!sym(time_var) >= 0)
    } else if (analysis_type == "all_patients") {
        data
    } else {
        stop(sprintf("Invalid analysis_type: %s", analysis_type))
    }

    # Ensure factors are not ordered (for plotting/analysis consistency)
    fix_event_data <- enforce_unordered_factors(fix_event_data)
    confounders_to_use <- confounders

    # Construct survival formula for KM and Cox
    surv_formula <- as.formula(
        paste0("Surv(", time_var, ", ", event_var, ") ~ ", group_var)
    )

    # Select relevant columns for analysis
    new_data <- fix_event_data %>%
        dplyr::select(all_of(c(time_var, event_var, group_var, confounders_to_use)))

    # Remove "Other" rows prior to survival modeling
    survival_variables <- unique(c(group_var, confounders_to_use))
    exclusion_result <- exclude_other_categories(
        data = new_data,
        variables = survival_variables[survival_variables %in% names(new_data)],
        other_map = if (is.null(other_map)) list() else other_map
    )

    if (exclusion_result$removed_row_count > 0) {
        logger::log_info(formatted(sprintf(
            "Removed %d rows labelled 'Other' prior to survival modeling (%s)",
            exclusion_result$removed_row_count,
            paste(survival_variables, collapse = ", ")
        ), indent = 1))
    }

    new_data <- exclusion_result$data

    if (nrow(new_data) == 0 || length(unique(stats::na.omit(new_data[[group_var]]))) < 2) {
        logger::log_warn(formatted(
            "Insufficient non-'Other' data available after exclusions; skipping survival analysis.",
            indent = 1
        ))
        empty_df <- data.frame()
        return(list(
            fit = NULL,
            plot = NULL,
            survival_rates = empty_df,
            survival_rates_wide = empty_df,
            rmst_analysis = empty_df,
            rmst_plot = NULL,
            cox_model = NULL,
            cox_table = NULL,
            ph_diagnostics = NULL,
            diagnostics = list(
                other_level_details = exclusion_result$other_level_details,
                raw_model_output = "Model skipped: insufficient data after removing 'Other' levels."
            )
        ))
    }

    # Fit Kaplan-Meier survival curves
    surv_fit <- survival::survfit(surv_formula, data = new_data)
    surv_fit$call$formula <- surv_formula

    # Set up time axis breaks (in months) with legacy cap to avoid extreme tails
    raw_max_time <- max(new_data[[time_var]], na.rm = TRUE)
    max_time <- min(raw_max_time, SURVIVAL_XAXIS_MAX_MONTHS)
    base_by <- if (max_time <= 60) 6 else 12
    x_breaks <- seq(0, ceiling(max_time / base_by) * base_by, by = base_by)

    # Set legend labels and color palette (centralized)
    if (is.null(legend_labels)) {
        legend_labels <- levels(factor(new_data[[group_var]]))
    }
    color_palette <- get_palette_by_variable(group_var, legend_labels)
    # Identify strata requiring de-emphasis (thinner line/partial transparency)
    deemphasised_levels <- intersect(legend_labels, c("GEP Failed/Indeterminate"))

    # Generate Kaplan-Meier plot with risk table
    surv_plot <- survminer::ggsurvplot(
        fit = surv_fit,
        data = new_data,
        palette = color_palette,
        risk.table = TRUE,
        conf.int = FALSE,
        pval = TRUE,
        title = paste("Kaplan-Meier Survival Curves:", ylab),
        subtitle = if (!is.null(dataset_name)) paste("Cohort:", dataset_name) else NULL,
        xlab = "Time (months)",
        ylab = ylab,
        risk.table.height = 0.10,
        ggtheme = theme_minimal(),
        break.time.by = base_by,
        xlim = c(0, max(x_breaks)),
        ylim = c(0, 1),
        legend.labs = legend_labels,
        risk.table.y.text = TRUE,
        tables.y.text = TRUE,
        risk.table.title = "Number at risk"
    )

    legend_override <- NULL
    if (length(deemphasised_levels) > 0) {
        clean_strata <- function(x) {
            ifelse(grepl("=", x), sub("^[^=]*=", "", x), x)
        }

        if (!is.null(surv_plot$plot$data)) {
            surv_plot$plot$data <- surv_plot$plot$data %>%
                dplyr::mutate(
                    line_alpha = ifelse(clean_strata(as.character(strata)) %in% deemphasised_levels, 0.6, 1),
                    line_size = ifelse(clean_strata(as.character(strata)) %in% deemphasised_levels, 0.7, 1)
                )
        }
        if (length(surv_plot$plot$layers) > 0) {
            for (layer_idx in seq_along(surv_plot$plot$layers)) {
                layer_data <- surv_plot$plot$layers[[layer_idx]]$data
                if (!is.null(layer_data) && "strata" %in% names(layer_data)) {
                    surv_plot$plot$layers[[layer_idx]]$data <- layer_data %>%
                        dplyr::mutate(
                            line_alpha = ifelse(clean_strata(as.character(strata)) %in% deemphasised_levels, 0.6, 1),
                            line_size = ifelse(clean_strata(as.character(strata)) %in% deemphasised_levels, 0.7, 1)
                        )
                }
            }
        }

        surv_plot$plot <- surv_plot$plot +
            ggplot2::aes(alpha = line_alpha, size = line_size) +
            ggplot2::scale_color_manual(values = color_palette) +
            ggplot2::scale_alpha_identity(guide = "none") +
            ggplot2::scale_size_identity(guide = "none")

        legend_override_alpha <- ifelse(legend_labels %in% deemphasised_levels, 0.6, 1)
        legend_override_size <- ifelse(legend_labels %in% deemphasised_levels, 0.7, 1)
        legend_override <- list(
            alpha = legend_override_alpha,
            size = legend_override_size,
            colour = color_palette[legend_labels]
        )
    } else {
        surv_plot$plot <- surv_plot$plot + ggplot2::scale_color_manual(values = color_palette)
    }

    legend_cols <- if (length(legend_labels) > 4) 2 else 1
    has_linetype <- "linetype" %in% names(surv_plot$plot$mapping) || any(vapply(surv_plot$plot$layers, function(layer) "linetype" %in% names(layer$mapping), logical(1)))
    guide_params <- list(ncol = legend_cols, byrow = TRUE)
    if (!is.null(legend_override)) {
        legend_override$colour <- color_palette[legend_labels]
        guide_params$override.aes <- legend_override
    }
    guide_args <- list(color = do.call(ggplot2::guide_legend, guide_params))
    if (has_linetype) {
        guide_args$linetype <- ggplot2::guide_legend(ncol = legend_cols, byrow = TRUE)
    }
    surv_plot$plot <- surv_plot$plot +
        do.call(ggplot2::guides, guide_args) +
        ggplot2::theme(
            legend.position = "bottom",
            legend.box = "vertical"
        )
    # Format y-axis as percent
    surv_plot$plot <- surv_plot$plot +
        scale_y_continuous(
            limits = c(0, 1),
            breaks = seq(0, 1, by = 0.1),
            labels = function(x) x * 100,
            name = paste0(ylab, " (%)")
        )
    surv_plot$table <- surv_plot$table + theme_minimal()
    
    # Save KM plot if output_dirs are provided
    if (!is.null(output_dirs)) {
        output_dir <- if (grepl("Overall Survival", ylab)) {
            output_dirs$obj1_os
        } else if (grepl("Progression-Free Survival", ylab)) {
            output_dirs$obj1_pfs
        } else if (grepl("PFS-2", ylab)) {
            output_dirs$obj3_pfs2
        } else if (grepl("Metastasis-Free Survival", ylab)) {
            output_dirs$obj4_mfs
        } else {
            output_dirs$baseline_characteristics
        }
        km_path <- file.path(output_dir, paste0(prefix, make_filename_safe(ylab), "_km.png"))
        # Combine main plot and risk table vertically so the saved image includes both
        combined_km <- cowplot::plot_grid(
            surv_plot$plot,
            surv_plot$table,
            ncol = 1,
            align = "v",
            rel_heights = c(0.86, 0.14)
        )
        # Dynamic height scaling: base on number of strata in the KM fit
        n_groups <- tryCatch(
            {
                length(surv_plot$plot$data$strata %||% levels(new_data[[group_var]]))
            },
            error = function(e) length(levels(new_data[[group_var]]))
        )
        # Calculate dynamic height based on number of strata
        extra_groups <- max(0, n_groups - 2)
        dynamic_height <- KM_BASE_HEIGHT + extra_groups * KM_HEIGHT_PER_STRATUM
        # Prefer taller PFS-2 default if applicable, but cap at KM_MAX_HEIGHT
        base_pref <- if (grepl("PFS-2", ylab)) max(PFS2_PLOT_HEIGHT, SURVIVAL_PLOT_HEIGHT) else SURVIVAL_PLOT_HEIGHT
        plot_height <- min(KM_MAX_HEIGHT, max(base_pref, dynamic_height))
        # Save the combined plot with dynamic height
        ggplot2::ggsave(km_path, combined_km, width = SURVIVAL_PLOT_WIDTH, height = plot_height, dpi = PLOT_DPI, bg = "white")
        logger::log_info(sprintf("KM plot (with risk table) saved: %s", km_path))
    }

    # Define time points (in months) for summary and RMST
    time_points <- SURVIVAL_SUMMARY_TIMEPOINTS_YEARS * 12
    
    # Add debugging and error handling for the summary call
    logger::log_info(sprintf("DEBUG: Time points for summary: %s", paste(time_points, collapse = ", ")))
    logger::log_info(sprintf("DEBUG: Max time in data: %.2f", max(new_data[[time_var]], na.rm = TRUE)))
    logger::log_info(sprintf("DEBUG: Min time in data: %.2f", min(new_data[[time_var]], na.rm = TRUE)))
    
    # Filter time points to only include those within the data range to prevent "invalid 'times' argument" error
    max_data_time <- max(new_data[[time_var]], na.rm = TRUE)
    valid_time_points <- time_points[time_points <= max_data_time]
    
    if (length(valid_time_points) == 0) {
        logger::log_warn("No valid time points for summary - all requested times exceed data range")
        valid_time_points <- c(max_data_time)  # Use max data time as fallback
    }
    
    logger::log_info(sprintf("DEBUG: Valid time points for summary: %s", paste(valid_time_points, collapse = ", ")))

    # Summarize survival at key time points with error handling
    surv_summary <- tryCatch({
        summary(surv_fit, times = valid_time_points)
    }, error = function(e) {
        logger::log_error(sprintf("ERROR in surv_fit summary: %s", e$message))
        logger::log_error("This is likely the source of the 'invalid times argument' error")
        # Return NULL to prevent further errors
        NULL
    })
    
    if (is.null(surv_summary)) {
        logger::log_warn("Survival summary failed - skipping summary statistics and RMST analysis")
        surv_rates <- data.frame(
            Treatment_Group = character(),
            Time_Years = numeric(),
            surv_pct = numeric(),
            lower_pct = numeric(),
            upper_pct = numeric(),
            stringsAsFactors = FALSE
        )
        rmst_results <- data.frame(
            Time_Point_Years = numeric(),
            Time_Point_Months = numeric(),
            RMST_Group1 = numeric(),
            RMST_Group2 = numeric(),
            RMST_Difference = numeric(),
            RMST_P_Value = numeric(),
            Analysis_Type = character(),
            stringsAsFactors = FALSE
        )
    } else {
        surv_rates <- as.data.frame(surv_summary[c("strata", "time", "surv", "lower", "upper")]) %>%
            dplyr::mutate(
                Treatment_Group = sub(".*=", "", strata),
                Time_Years = round(time / 12, 1)
            ) %>%
            dplyr::mutate(
                across(c(surv, lower, upper), ~ round(100 * ., 1), .names = "{.col}_pct")
            ) %>%
            dplyr::select(Treatment_Group, Time_Years, surv_pct, lower_pct, upper_pct)

        # Initialize RMST results table
        rmst_results <- data.frame(
            Time_Point_Years = numeric(),
            Time_Point_Months = numeric(),
            RMST_Group1 = numeric(),
            RMST_Group2 = numeric(),
            RMST_Difference = numeric(),
            RMST_P_Value = numeric(),
            Analysis_Type = character(),
            stringsAsFactors = FALSE
        )

        # Calculate RMST for each time point
        logger::log_info(sprintf("DEBUG: Starting RMST analysis for %d time points", length(valid_time_points)))
        for (time_point in valid_time_points) {
            time_years <- round(time_point / 12, 1)
            logger::log_info(sprintf("DEBUG: Processing RMST for %s years (%.1f months)", time_years, time_point))
            rmst_result <- tryCatch(
                {
                    # Handle RMST for any number of groups (binary or multi-group)
                    unique_groups <- unique(new_data[[group_var]])
                    logger::log_info(sprintf("DEBUG: Unique groups for RMST: %s", paste(unique_groups, collapse = ", ")))
                    
                    if (length(unique_groups) == 2) {
                        # Binary comparison: use 0/1 coding
                        group_binary <- ifelse(new_data[[group_var]] == unique_groups[2], 1, 0)
                        logger::log_info(sprintf("DEBUG: Running RMST for binary comparison: %s vs %s", unique_groups[1], unique_groups[2]))
                        
                        rmst2(
                            time = new_data[[time_var]],
                            status = new_data[[event_var]],
                            arm = group_binary,
                            tau = time_point
                        )
                    } else {
                        # Non-binary groups: skip RMST analysis entirely and log informative message
                        logger::log_info(sprintf("DEBUG: Skipping RMST analysis - non-binary grouping detected (%d groups: %s). RMST analysis requires exactly 2 groups.", 
                                               length(unique_groups), paste(unique_groups, collapse = ", ")))
                        NULL
                    }
                },
                error = function(e) {
                    logger::log_error(sprintf("ERROR in RMST calculation for %.1f years: %s", time_years, e$message))
                    NULL
                }
            )
            if (!is.null(rmst_result)) {
                rmst_results <- rbind(
                    rmst_results,
                    data.frame(
                        Time_Point_Years = time_years,
                        Time_Point_Months = time_point,
                        RMST_Group1 = round(rmst_result$RMST.arm0$rmst[1], 2),
                        RMST_Group2 = round(rmst_result$RMST.arm1$rmst[1], 2),
                        RMST_Difference = round(rmst_result$unadjusted.result[1, 1], 2),
                        RMST_P_Value = round(rmst_result$unadjusted.result[1, 4], 4),
                        Analysis_Type = paste0("Mean survival up to ", time_years, " years"),
                        stringsAsFactors = FALSE
                    )
                )
            } else {
                # Check if we skipped RMST due to non-binary grouping
                unique_groups <- unique(new_data[[group_var]])
                analysis_type_msg <- if (length(unique_groups) < 2) {
                    "Not applicable (insufficient groups)"
                } else if (length(unique_groups) > 2) {
                    "Not applicable (non-binary grouping)"
                } else {
                    "Analysis failed"
                }
                rmst_results <- rbind(
                    rmst_results,
                    data.frame(
                        Time_Point_Years = time_years,
                        Time_Point_Months = time_point,
                        RMST_Group1 = NA,
                        RMST_Group2 = NA,
                        RMST_Difference = NA,
                        RMST_P_Value = NA,
                        Analysis_Type = analysis_type_msg,
                        stringsAsFactors = FALSE
                    )
                )
            }
        }
    }

    # Prepare wide-format survival rates for reporting
    surv_rates_wide <- surv_rates %>%
        dplyr::mutate(Time_Label = paste0(Time_Years, "-year")) %>%
        dplyr::select(Treatment_Group, Time_Label, surv_pct) %>%
        tidyr::pivot_wider(names_from = Time_Label, values_from = surv_pct)
    surv_rates_wide_char <- surv_rates_wide %>%
        dplyr::mutate(across(everything(), as.character))

    # Add RMST P-value and difference rows to wide table
    rmst_pvalue_row <- data.frame(Treatment_Group = "RMST P-Value", stringsAsFactors = FALSE)
    for (i in seq_len(nrow(rmst_results))) {
        time_label <- paste0(rmst_results$Time_Point_Years[i], "-year")
        p_val <- rmst_results$RMST_P_Value[i]
        if (time_label %in% names(surv_rates_wide)) {
            rmst_pvalue_row[[time_label]] <- if (is.na(p_val)) {
                "Analysis failed"
            } else if (p_val < 0.0001) {
                "<0.0001"
            } else {
                sprintf("%.3f", p_val)
            }
        }
    }
    rmst_diff_row <- data.frame(Treatment_Group = "RMST Difference (months)", stringsAsFactors = FALSE)
    for (i in seq_len(nrow(rmst_results))) {
        time_label <- paste0(rmst_results$Time_Point_Years[i], "-year")
        rmst_diff <- rmst_results$RMST_Difference[i]
        if (time_label %in% names(surv_rates_wide)) {
            rmst_diff_row[[time_label]] <- if (is.na(rmst_diff)) "NA" else sprintf("%.1f", rmst_diff)
        }
    }
    surv_rates_wide_with_rmst <- dplyr::bind_rows(
        surv_rates_wide_char,
        rmst_pvalue_row,
        rmst_diff_row
    )

    # Write outputs to Excel files if output_dirs provided
    if (!is.null(output_dirs)) {
        # Default fallback directory
        output_dir <- output_dirs$baseline_characteristics
        if (grepl("Overall Survival", ylab) && !is.null(output_dirs$obj1_os)) {
            output_dir <- output_dirs$obj1_os
        } else if (grepl("Progression-Free Survival", ylab) && !is.null(output_dirs$obj1_pfs)) {
            output_dir <- output_dirs$obj1_pfs
        } else if (grepl("PFS-2", ylab) && !is.null(output_dirs$obj3_pfs2)) {
            output_dir <- output_dirs$obj3_pfs2
        } else if (grepl("Metastasis-Free Survival", ylab)) {
            # For MFS, prefer obj4_mfs when available; otherwise, gracefully fall back
            if (!is.null(output_dirs$obj4_mfs)) {
                output_dir <- output_dirs$obj4_mfs
            } else if (!is.null(output_dirs$obj1_pfs)) {
                # Secondary fallback to primary outcomes directory if present
                output_dir <- output_dirs$obj1_pfs
            } else {
                # Keep baseline_characteristics as final fallback
                logger::log_warn("Output directory for MFS not provided; using baseline_characteristics as fallback")
            }
        }
        writexl::write_xlsx(
            surv_rates,
            path = file.path(output_dir, paste0(prefix, make_filename_safe(ylab), "_survival_rates.xlsx"))
        )
        writexl::write_xlsx(
            surv_rates_wide_with_rmst,
            path = file.path(output_dir, paste0(prefix, make_filename_safe(ylab), "_survival_rates_wide.xlsx"))
        )
        # Only save RMST file if there's actual RMST data (not just "Not applicable" rows)
        rmst_has_data <- any(!is.na(rmst_results$RMST_P_Value) & !grepl("Not applicable", rmst_results$Analysis_Type))
        if (rmst_has_data) {
            writexl::write_xlsx(
                rmst_results,
                path = file.path(output_dir, paste0(prefix, make_filename_safe(ylab), "_rmst_analysis.xlsx"))
            )
            logger::log_info(sprintf("RMST analysis file saved: %s", paste0(prefix, make_filename_safe(ylab), "_rmst_analysis.xlsx")))
        } else {
            logger::log_info(sprintf("Skipping RMST file creation - no valid RMST data available for %s", ylab))
        }
    }

    # Run Cox regression and generate regression table
    logger::log_info(sprintf("DEBUG: About to call generate_regression_table for %s", paste0(ylab, "_cox")))
    cox_result <- tryCatch({
        generate_regression_table(
            data = new_data,
            outcome_var = event_var,
            predictor_vars = group_var,
            confounders = confounders_to_use,
            model_type = "cox",
            effect_measure = "HR",
            analysis_name = paste0(ylab, "_cox"),
            dataset_name = dataset_name,
            output_dir = if (!is.null(output_dirs)) output_dir else "test_output",
            prefix = prefix,
            time_var = time_var,
            event_var = event_var,
            other_map = other_map,
            treatment_var = group_var,
            other_level_details = exclusion_result$other_level_details
        )
    }, error = function(e) {
        logger::log_error(sprintf("ERROR in generate_regression_table: %s", e$message))
        return(NULL)
    })

    # Return all results as a list
    list(
        fit = surv_fit,
        plot = surv_plot,
        survival_rates = surv_rates,
        survival_rates_wide = surv_rates_wide_with_rmst,
        rmst_analysis = rmst_results,
        rmst_plot = tryCatch({
            # Only generate RMST plot if there's valid RMST data
            rmst_has_data <- any(!is.na(rmst_results$RMST_P_Value) & !grepl("Not applicable", rmst_results$Analysis_Type))
            if (rmst_has_data) {
                # Get group names for RMST plot
                unique_groups <- levels(new_data[[group_var]])
                group1_name <- unique_groups[1]
                group2_name <- unique_groups[2]
                
                plot_rmst_pvalue_progression(rmst_results, ylab, output_dirs, prefix, group1_name, group2_name)
            } else {
                logger::log_info(sprintf("Skipping RMST plot generation - no valid RMST data available for %s", ylab))
                NULL
            }
        }, error = function(e) {
            logger::log_warn(sprintf("RMST plot generation failed: %s", e$message))
            NULL
        }),
        cox_model = cox_result$model,
        cox_table = cox_result$table,
        ph_diagnostics = NULL,
        diagnostics = cox_result$diagnostics
    )
}

# PFS-2 Analysis

#' Analyze second progression survival (PFS-2)
#'
#' Analyzes survival from second progression (PFS-2) for patients who experienced a first recurrence.
#'
#' @param data Data frame
#' @param confounders Character vector of confounder variable names
#' @param dataset_name Name of the dataset
#' @param other_map Optional named list for additional mapping
#' @param output_dirs List of output directories organized by analysis type
#' @param prefix Character string used as a file prefix for output files
#' @return List with elements: pfs2_data (data frame), survival_analysis (list), summary_table (gtsummary object)
analyze_pfs2 <- function(data, confounders = NULL, dataset_name = NULL, other_map = list(), output_dirs = NULL, prefix = NULL) {
    logger::log_info("Starting PFS-2 analysis for recurrent patients")

    # Filter to patients with valid PFS-2 data (variables now created in data processing)
    pfs2_data <- data %>%
        filter(
            !is.na(tt_pfs2_months),
            tt_pfs2_months >= 0,
            !is.na(recurrence1_treatment_clean)
        )

    logger::log_info(sprintf("Found %d patients with valid PFS-2 data", nrow(pfs2_data)))

    if (nrow(pfs2_data) == 0) {
        logger::log_info("No patients with valid PFS-2 data found")
        return(list(
            pfs2_data = NULL,
            survival_analysis = NULL,
            summary_table = NULL
        ))
    }

    # Capture raw salvage treatment distribution before filtering
    pfs2_data_raw <- pfs2_data
    raw_primary_vs_salvage <- pfs2_data_raw %>%
        mutate(
            primary_treatment = as.character(treatment_group),
            salvage_treatment = dplyr::case_when(
                is.na(recurrence1_treatment) | recurrence1_treatment == "" ~ "No Salvage Treatment Recorded",
                TRUE ~ as.character(recurrence1_treatment)
            )
        ) %>%
        group_by(primary_treatment, salvage_treatment) %>%
        summarise(
            n = n(),
            events = sum(pfs2_event, na.rm = TRUE),
            event_rate_pct = ifelse(n > 0, round(100 * events / n, 1), NA_real_),
            .groups = "drop"
        ) %>%
        arrange(primary_treatment, desc(n))

    # Remove "Other" categories prior to analysis
    exclusion_vars <- unique(c("recurrence1_treatment_clean", confounders))
    exclusion_result <- exclude_other_categories(
        pfs2_data,
        variables = exclusion_vars[exclusion_vars %in% names(pfs2_data)],
        other_map = other_map
    )
    if (exclusion_result$removed_row_count > 0) {
        logger::log_info(sprintf(
            "Removed %d rows labelled 'Other' prior to PFS-2 analysis",
            exclusion_result$removed_row_count
        ))
    }
    pfs2_data <- exclusion_result$data

    # Summarise treatment distribution and write to file
    treatment_counts <- table(pfs2_data$recurrence1_treatment_clean)
    logger::log_info("Treatment distribution:")
    print(treatment_counts)

    model_primary_vs_salvage <- pfs2_data %>%
        mutate(
            primary_treatment = as.character(treatment_group),
            salvage_treatment = dplyr::case_when(
                is.na(recurrence1_treatment_clean) | recurrence1_treatment_clean == "" ~ "No Salvage Treatment Recorded",
                TRUE ~ as.character(recurrence1_treatment_clean)
            )
        ) %>%
        group_by(primary_treatment, salvage_treatment) %>%
        summarise(
            n = n(),
            events = sum(pfs2_event, na.rm = TRUE),
            event_rate_pct = ifelse(n > 0, round(100 * events / n, 1), NA_real_),
            .groups = "drop"
        ) %>%
        arrange(primary_treatment, desc(n))

    if (!is.null(output_dirs) && !is.null(output_dirs$obj3_pfs2)) {
        summary_path <- file.path(output_dirs$obj3_pfs2, paste0(prefix, "pfs2_treatment_summary.xlsx"))
        writexl::write_xlsx(
            list(
                raw_primary_vs_salvage = raw_primary_vs_salvage,
                model_primary_vs_salvage = model_primary_vs_salvage
            ),
            summary_path
        )
        logger::log_info(sprintf("PFS-2 treatment summary saved to %s", summary_path))
    }

    logger::log_info(sprintf("Final PFS-2 analysis dataset: %d patients", nrow(pfs2_data)))
    logger::log_info(sprintf("PFS-2 events (2nd recurrence): %d", sum(pfs2_data$pfs2_event)))

    # Check if we have enough patients and events for analysis
    if (nrow(pfs2_data) < 10) {
        logger::log_info("Insufficient patients for PFS-2 analysis")
        return(list(
            pfs2_data = pfs2_data,
            survival_analysis = NULL,
            summary_table = NULL
        ))
    }

    # Check if we have enough events for survival analysis
    total_events <- sum(pfs2_data$pfs2_event)

    if (total_events < 5) {
        logger::log_error("ERROR: Insufficient events for PFS-2 survival analysis")
        logger::log_info(sprintf("Total events: %d (minimum 5 required)", total_events))
        logger::log_info("Skipping survival analysis due to insufficient data")

        # Create explanation text file for skipped analysis
        explanation_text <- sprintf(
            "PFS-2 Analysis Skipped - Insufficient Events

            The Issue:
            %s cohort: %d patients total
            PFS-2 eligible patients: %d patients (those with first recurrence)
            PFS-2 events: %d patients (second recurrence)
            Minimum required: 5 events for survival analysis

            Analysis was skipped because there are insufficient events (%d) to perform a meaningful survival analysis. 
            The minimum requirement of 5 events ensures statistical validity and reliable results.

            This is expected behavior for cohorts with limited recurrence data and does not indicate an error.",
            tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name))),
            nrow(data),
            nrow(pfs2_data),
            total_events,
            total_events
        )

        # Save explanation to both a_pfs2 and b_proportional_hazards_diagnostics directories
        if (!is.null(output_dirs)) {
            # Save to a_pfs2 directory
            pfs2_dir <- output_dirs$obj3_pfs2
            if (!is.null(pfs2_dir) && dir.exists(pfs2_dir)) {
                explanation_file <- file.path(pfs2_dir, paste0(prefix, "pfs2_analysis_skipped_explanation.txt"))
                writeLines(explanation_text, explanation_file)
                logger::log_info(sprintf("Explanation saved to: %s", explanation_file))
            }
            
            # Save to b_proportional_hazards_diagnostics directory
            ph_dir <- output_dirs$obj3_ph_diagnostics
            if (!is.null(ph_dir) && dir.exists(ph_dir)) {
                explanation_file <- file.path(ph_dir, paste0(prefix, "pfs2_analysis_skipped_explanation.txt"))
                writeLines(explanation_text, explanation_file)
                logger::log_info(sprintf("Explanation saved to: %s", explanation_file))
            }
        }

        pfs2_survival <- list(
            fit = NULL,
            plot = NULL,
            survival_rates = NULL,
            cox_model = NULL,
            cox_table = NULL
        )
    } else {
        # Use existing analyze_time_to_event_outcomes function with dynamic legend labels
        # Perfect separation handling is already implemented in fit_regression_model()
        logger::log_info("Performing PFS-2 survival analysis")
        pfs2_survival <- analyze_time_to_event_outcomes(
            data = pfs2_data,
            time_var = "tt_pfs2_months",
            event_var = "pfs2_event",
            group_var = "recurrence1_treatment_clean",
            confounders = confounders,
            ylab = "PFS-2 Probability (Freedom from 2nd Recurrence)",
            analysis_type = "all_patients", # PFS-2 analysis includes all recurrent patients
            dataset_name = paste0(dataset_name, "_pfs2_recurrent"),
            other_map = other_map,
            output_dirs = output_dirs,
            prefix = prefix
        )
    }

    logger::log_info("PFS-2 analysis completed")

    # Generate proportional hazards diagnostics for PFS-2 (Objective 3)
    ph_diag_result <- NULL
    if (!is.null(pfs2_survival$cox_model)) {
        ph_output_dir <- if (!is.null(output_dirs)) output_dirs$obj3_ph_diagnostics else getwd()
        ph_file_prefix <- paste0(prefix, make_filename_safe("PFS-2 Probability (Freedom from 2nd Recurrence)"), "_")
        ph_diag_result <- test_proportional_hazards_assumption(
            cox_model = pfs2_survival$cox_model,
            outcome_name = "PFS-2 Probability (Freedom from 2nd Recurrence)",
            output_dir = ph_output_dir,
            file_prefix = ph_file_prefix,
            dataset_name = dataset_name
        )
    }

    return(list(
        pfs2_data = pfs2_data,
        survival_analysis = pfs2_survival,
        summary_table = pfs2_survival$cox_table, # Use the standardized table from generate_regression_table
        raw_primary_vs_salvage = raw_primary_vs_salvage,
        model_primary_vs_salvage = model_primary_vs_salvage,
        ph_diagnostics = ph_diag_result
    ))
}


#' Test Proportional Hazards Assumption using Schoenfeld Residuals
#'
#' Performs comprehensive testing of the proportional hazards assumption for Cox models
#' using Schoenfeld residuals. Creates diagnostic plots and statistical tests to identify
#' time-varying treatment effects and other PH violations.
#'
#' @param cox_model A fitted coxph model object
#' @param outcome_name Character string describing the outcome (e.g., "Overall Survival")
#' @param output_dir Directory path where diagnostic files should be saved
#' @param file_prefix Prefix for output files
#' @param dataset_name Name of the dataset for labeling
#' @return List containing schoenfeld_test, individual_tests, plots, summary
test_proportional_hazards_assumption <- function(cox_model, outcome_name = "Survival", output_dir = NULL, file_prefix = "", dataset_name = NULL) {
    logger::log_info(sprintf("Testing proportional hazards assumption for %s", outcome_name))

    # Check if model is valid
    if (is.null(cox_model) || !inherits(cox_model, "coxph")) {
        logger::log_warn("Invalid Cox model provided - skipping PH assumption testing")
        return(NULL)
    }

    # Set default output directory if not provided
    if (is.null(output_dir)) {
        warning("No output directory provided for proportional hazards testing. Files will be saved to current directory.")
        output_dir <- "."
    }

    # Ensure output directory exists
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    build_ph_failure_note <- function(error_obj) {
        note_filename <- paste0(file_prefix, "proportional_hazards_unavailable.txt")
        note_path <- file.path(output_dir, note_filename)
        dataset_label <- ifelse(is.null(dataset_name), "Not specified", dataset_name)
        model_terms <- tryCatch(attr(cox_model$terms, "term.labels"), error = function(...) character())
        model_formula <- tryCatch(paste(stats::deparse(stats::formula(cox_model)), collapse = " "), error = function(...) "Unavailable")
        total_patients <- tryCatch(cox_model$n, error = function(...) NA_integer_)
        total_events <- tryCatch(cox_model$nevent, error = function(...) NA_integer_)

        note_lines <- c(
            "PROPORTIONAL HAZARDS DIAGNOSTICS NOT AVAILABLE",
            "",
            paste0("Outcome: ", outcome_name),
            paste0("Dataset: ", dataset_label),
            paste0("Error: ", error_obj$message),
            "",
            paste0("Model formula: ", model_formula)
        )

        if (!is.na(total_patients)) {
            note_lines <- c(note_lines, paste0("Patients included: ", total_patients))
        }
        if (!is.na(total_events)) {
            note_lines <- c(note_lines, paste0("Events observed: ", total_events))
        }
        if (length(model_terms) > 0) {
            note_lines <- c(note_lines, paste0("Variables in model: ", paste(model_terms, collapse = ", ")))
        }

        model_frame <- tryCatch(stats::model.frame(cox_model), error = function(...) NULL)
        reason_lines <- character()
        event_section_added <- FALSE

        if (!is.null(model_frame)) {
            response <- model_frame[[1]]
            status <- NULL
            time_values <- NULL
            if (inherits(response, "Surv")) {
                status <- as.numeric(response[, "status"])
                time_values <- as.numeric(response[, "time"])
            }

            if (!is.null(time_values) && length(time_values) > 0 && any(!is.na(time_values))) {
                note_lines <- c(note_lines, paste0(
                    "Follow-up time range (months): ",
                    sprintf("%.2f to %.2f", min(time_values, na.rm = TRUE), max(time_values, na.rm = TRUE))
                ))
            }

            if (!is.null(status) && length(model_terms) > 0) {
                for (term in model_terms) {
                    if (!term %in% names(model_frame)) next
                    var_data <- model_frame[[term]]
                    if (is.null(var_data)) next

                    if (is.factor(var_data) || is.character(var_data) || length(unique(var_data)) <= 10) {
                        if (!event_section_added) {
                            note_lines <- c(note_lines, "", "Event distribution by predictor level:")
                            event_section_added <- TRUE
                        }

                        var_factor <- factor(var_data, exclude = NULL)
                        level_counts <- table(var_factor, useNA = "ifany")
                        event_counts <- tapply(status, var_factor, function(x) sum(x == 1, na.rm = TRUE))
                        event_counts <- event_counts[names(level_counts)]
                        event_counts[is.na(event_counts)] <- 0

                        note_lines <- c(note_lines, paste0("  ", term, ":"))
                        for (lvl in names(level_counts)) {
                            lvl_label <- ifelse(is.na(lvl) || lvl == "", "<Missing>", lvl)
                            note_lines <- c(note_lines, sprintf(
                                "    - %s: n = %d, events = %d",
                                lvl_label,
                                level_counts[[lvl]],
                                event_counts[[lvl]]
                            ))
                        }

                        zero_evt <- names(level_counts)[event_counts == 0]
                        if (length(zero_evt) > 0) {
                            cleaned <- ifelse(zero_evt == "" | is.na(zero_evt), "<Missing>", zero_evt)
                            reason_lines <- c(reason_lines, sprintf(
                                "  * %s has zero events for: %s",
                                term,
                                paste(cleaned, collapse = ", ")
                            ))
                        }

                        saturated_levels <- names(level_counts)[event_counts == level_counts]
                        if (length(saturated_levels) > 0) {
                            cleaned <- ifelse(saturated_levels == "" | is.na(saturated_levels), "<Missing>", saturated_levels)
                            reason_lines <- c(reason_lines, sprintf(
                                "  * %s has events in every patient for: %s",
                                term,
                                paste(cleaned, collapse = ", ")
                            ))
                        }
                    } else {
                        if (!event_section_added) {
                            note_lines <- c(note_lines, "", "Event distribution by predictor level:")
                            event_section_added <- TRUE
                        }
                        unique_vals <- length(unique(stats::na.omit(var_data)))
                        note_lines <- c(note_lines, paste0(
                            "  ", term, ": numeric predictor with ", unique_vals, " unique values"
                        ))
                        reason_lines <- c(reason_lines, sprintf(
                            "  * %s may contribute to singularity (numeric predictor with limited variability)",
                            term
                        ))
                    }
                }
            }
        } else {
            reason_lines <- c(reason_lines, "  * Unable to reconstruct the model frame to summarise predictor levels.")
        }

        coef_values <- tryCatch(stats::coef(cox_model), error = function(...) numeric())
        if (length(coef_values) > 0) {
            non_finite_coefs <- names(coef_values)[!is.finite(coef_values)]
            if (length(non_finite_coefs) > 0) {
                reason_lines <- c(reason_lines, sprintf(
                    "  * Non-finite coefficient estimates detected for: %s",
                    paste(non_finite_coefs, collapse = ", ")
                ))
            }
        }

        note_lines <- c(note_lines, "", "Why diagnostics failed:")
        if (length(reason_lines) > 0) {
            note_lines <- c(note_lines, reason_lines)
        } else {
            note_lines <- c(note_lines, "  * Schoenfeld residual diagnostics require an invertible variance matrix. The fitted Cox model resulted in a singular matrix, typically triggered by sparse events or redundant predictors.")
        }

        note_lines <- c(
            note_lines,
            "",
            "Suggested follow-up actions:",
            "  * Collapse or remove levels with zero events to stabilise the variance matrix.",
            "  * Simplify the model or consider time-varying effects when events are sparse.",
            "  * Verify that each GEP group has at least one event and adequate sample size."
        )

        writeLines(note_lines, note_path)
        logger::log_warn(formatted(sprintf("PH diagnostics unavailable note saved: %s", note_path), indent = 1))
    }

    ph_error <- NULL
    ph_results <- tryCatch(
        {
            # Perform Schoenfeld residuals test
            logger::log_info(formatted("Computing Schoenfeld residuals and correlation tests", indent = 1))
            schoenfeld_test <- survival::cox.zph(cox_model)

            # Extract variable names and test statistics
            var_names <- rownames(schoenfeld_test$table)
            p_values <- schoenfeld_test$table[, "p"]

            # Create summary of PH violations
            ph_summary <- data.frame(
                Variable = var_names,
                Chi_Square = schoenfeld_test$table[, "chisq"],
                DF = schoenfeld_test$table[, "df"],
                P_Value = p_values,
                PH_Assumption = ifelse(p_values < 0.05, "VIOLATED", "OK"),
                Interpretation = case_when(
                    p_values < 0.001 ~ "Strong evidence against PH (p < 0.001)",
                    p_values < 0.01 ~ "Moderate evidence against PH (p < 0.01)",
                    p_values < 0.05 ~ "Some evidence against PH (p < 0.05)",
                    TRUE ~ "No evidence against PH assumption"
                ),
                stringsAsFactors = FALSE
            )

            # Add overall test result
            global_test <- data.frame(
                Variable = "GLOBAL",
                Chi_Square = schoenfeld_test$table["GLOBAL", "chisq"],
                DF = schoenfeld_test$table["GLOBAL", "df"],
                P_Value = schoenfeld_test$table["GLOBAL", "p"],
                PH_Assumption = ifelse(schoenfeld_test$table["GLOBAL", "p"] < 0.05, "VIOLATED", "OK"),
                Interpretation = case_when(
                    schoenfeld_test$table["GLOBAL", "p"] < 0.001 ~ "Strong evidence against PH globally (p < 0.001)",
                    schoenfeld_test$table["GLOBAL", "p"] < 0.01 ~ "Moderate evidence against PH globally (p < 0.01)",
                    schoenfeld_test$table["GLOBAL", "p"] < 0.05 ~ "Some evidence against PH globally (p < 0.05)",
                    TRUE ~ "No evidence against PH assumption globally"
                ),
                stringsAsFactors = FALSE
            )

            ph_summary_with_global <- rbind(ph_summary[var_names != "GLOBAL", ], global_test)

            # Save summary table
            writexl::write_xlsx(
                ph_summary_with_global,
                path = file.path(output_dir, paste0(file_prefix, "proportional_hazards_tests.xlsx"))
            )

            logger::log_info(formatted(
                sprintf(
                    "PH assumption tests saved to: %s",
                    file.path(output_dir, paste0(file_prefix, "proportional_hazards_tests.xlsx"))
                ),
                indent = 1
            ))

            # Log key findings
            violations <- ph_summary_with_global[ph_summary_with_global$PH_Assumption == "VIOLATED", ]
            if (nrow(violations) > 0) {
                logger::log_warn(formatted(sprintf("PH ASSUMPTION VIOLATIONS DETECTED for %d variable(s):", nrow(violations)), indent = 1))
                for (i in seq_len(nrow(violations))) {
                    logger::log_warn(formatted(
                        sprintf(
                            "- %s: p = %.4f (%s)",
                            violations$Variable[i],
                            violations$P_Value[i],
                            violations$Interpretation[i]
                        ),
                        indent = 2
                    ))
                }
            } else {
                logger::log_info(formatted("No PH assumption violations detected", indent = 1))
            }

            # Create diagnostic plots
            logger::log_info(formatted("Creating Schoenfeld residual diagnostic plots", indent = 1))

            # Individual plots for each variable
            individual_plots <- list()
            n_vars <- length(var_names[var_names != "GLOBAL"])

            for (i in seq_along(var_names)) {
                var_name <- var_names[i]
                if (var_name == "GLOBAL") next # Skip global test for individual plots

                logger::log_info(formatted(sprintf("Creating plot for variable: %s", var_name), indent = 2))

                # Create individual plot
                plot_filename <- file.path(output_dir, paste0(file_prefix, "schoenfeld_", gsub("[^A-Za-z0-9]", "_", var_name), ".png"))

                png(plot_filename, width = DEFAULT_PLOT_WIDTH, height = SMALL_PLOT_HEIGHT, units = PLOT_UNITS, res = PLOT_DPI)

                # Set margins to provide more space at top for title
                par(mar = c(5, 4, 6, 2))

                # Plot Schoenfeld residuals vs time
                plot(schoenfeld_test[i],
                    main = sprintf(
                        "Schoenfeld Residuals: %s\n%s (%s)",
                        var_name, outcome_name,
                        ifelse(is.null(dataset_name), "", dataset_name)
                    ),
                    xlab = "Time",
                    ylab = "Schoenfeld Residuals"
                )

                # Add p-value annotation
                p_val <- p_values[i]
                p_text <- if (p_val < 0.001) {
                    "p < 0.001"
                } else {
                    sprintf("p = %.3f", p_val)
                }

                mtext(
                    sprintf(
                        "Correlation test: %s %s",
                        p_text,
                        ifelse(p_val < 0.05, "(PH VIOLATED)", "(PH OK)")
                    ),
                    side = 3, line = 0.5, cex = 0.9,
                    col = ifelse(p_val < 0.05, "red", "darkgreen")
                )

                dev.off()

                individual_plots[[var_name]] <- plot_filename
            }

            # Create combined plot showing all variables
            logger::log_info(formatted("Creating combined diagnostic plot", indent = 1))
            combined_plot_filename <- file.path(output_dir, paste0(file_prefix, "schoenfeld_combined.png"))

            # Calculate grid dimensions
            n_plots <- length(individual_plots)
            n_cols <- min(3, n_plots) # Max 3 columns
            n_rows <- ceiling(n_plots / n_cols)

            png(combined_plot_filename, width = SMALL_PLOT_WIDTH * n_cols, height = SMALL_PLOT_HEIGHT * n_rows + 1.5, units = PLOT_UNITS, res = PLOT_DPI)
            par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 2), oma = c(0, 0, 6, 0))

            for (i in seq_along(var_names)) {
                var_name <- var_names[i]
                if (var_name == "GLOBAL") next

                plot(schoenfeld_test[i],
                    main = sprintf(
                        "%s\n%s", var_name,
                        if (p_values[i] < 0.001) "p < 0.001" else sprintf("p = %.3f", p_values[i])
                    ),
                    xlab = "Time",
                    ylab = "Schoenfeld Residuals",
                    cex.main = 0.9
                )

                # Color-code title based on p-value
                title(
                    main = sprintf(
                        "%s\n%s", var_name,
                        if (p_values[i] < 0.001) "p < 0.001" else sprintf("p = %.3f", p_values[i])
                    ),
                    col.main = ifelse(p_values[i] < 0.05, "red", "darkgreen"),
                    cex.main = 0.9
                )
            }

            # Add overall title with proper spacing from top
            mtext(
                sprintf(
                    "Proportional Hazards Diagnostics: %s\n%s",
                    outcome_name,
                    ifelse(is.null(dataset_name), "", paste("Dataset:", dataset_name))
                ),
                outer = TRUE, cex = 1.1, line = 2.5
            )

            dev.off()

            logger::log_info(formatted(sprintf("Combined diagnostic plot saved: %s", combined_plot_filename), indent = 1))

            # Create summary text file with interpretation
            summary_filename <- file.path(output_dir, paste0(file_prefix, "proportional_hazards_summary.txt"))

            cat("PROPORTIONAL HAZARDS ASSUMPTION TESTING SUMMARY\n", file = summary_filename)
            cat(paste(rep("=", 50), collapse = ""), "\n\n", file = summary_filename, append = TRUE)
            cat(sprintf("Analysis: %s\n", outcome_name), file = summary_filename, append = TRUE)
            cat(sprintf("Dataset: %s\n", ifelse(is.null(dataset_name), "Not specified", dataset_name)),
                file = summary_filename, append = TRUE
            )
            cat(sprintf("Test Date: %s\n\n", Sys.time()), file = summary_filename, append = TRUE)

            cat("INTERPRETATION:\n", file = summary_filename, append = TRUE)
            cat("The proportional hazards assumption requires that hazard ratios remain\n",
                file = summary_filename, append = TRUE
            )
            cat("constant over time. Violations suggest time-varying treatment effects.\n\n",
                file = summary_filename, append = TRUE
            )

            cat("TEST RESULTS:\n", file = summary_filename, append = TRUE)
            cat(
                sprintf(
                    "Global test p-value: %.4f %s\n\n",
                    schoenfeld_test$table["GLOBAL", "p"],
                    ifelse(schoenfeld_test$table["GLOBAL", "p"] < 0.05, "(VIOLATION)", "(OK)")
                ),
                file = summary_filename, append = TRUE
            )

            cat("Individual variable tests:\n", file = summary_filename, append = TRUE)
            for (i in seq_len(nrow(ph_summary_with_global))) {
                row <- ph_summary_with_global[i, ]
                cat(
                    sprintf(
                        "- %s: p = %.4f (%s)\n",
                        row$Variable, row$P_Value, row$PH_Assumption
                    ),
                    file = summary_filename, append = TRUE
                )
            }

            if (nrow(violations) > 0) {
                cat("\nVIOLATIONS DETECTED:\n", file = summary_filename, append = TRUE)
                cat("Variables with p < 0.05 violate the proportional hazards assumption.\n",
                    file = summary_filename, append = TRUE
                )
                cat("Consider stratification, time-varying coefficients, or alternative models.\n",
                    file = summary_filename, append = TRUE
                )
            }

            cat("\nFILES CREATED:\n", file = summary_filename, append = TRUE)
            cat(sprintf("- Test results: %s\n", basename(paste0(file_prefix, "proportional_hazards_tests.xlsx"))),
                file = summary_filename, append = TRUE
            )
            cat(sprintf("- Combined plot: %s\n", basename(combined_plot_filename)),
                file = summary_filename, append = TRUE
            )
            cat("- Individual plots: ", file = summary_filename, append = TRUE)
            cat(paste(basename(unlist(individual_plots)), collapse = ", "), file = summary_filename, append = TRUE)
            cat("\n", file = summary_filename, append = TRUE)

            logger::log_info(formatted(sprintf("Summary interpretation saved: %s", summary_filename), indent = 1))

            logger::log_info("Proportional hazards assumption testing completed")

            list(
                schoenfeld_test = schoenfeld_test,
                individual_tests = p_values,
                ph_summary = ph_summary_with_global,
                plots = list(
                    individual = individual_plots,
                    combined = combined_plot_filename
                ),
                summary_file = summary_filename
            )
        },
        error = function(e) {
            logger::log_error(sprintf("Error in PH assumption testing: %s", e$message))
            ph_error <<- e
            NULL
        }
    )

    if (!is.null(ph_error)) {
        try(build_ph_failure_note(ph_error), silent = TRUE)
    }

    return(ph_results)
}
