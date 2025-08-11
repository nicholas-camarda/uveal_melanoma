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

    # Fit Kaplan-Meier survival curves
    surv_fit <- survival::survfit(surv_formula, data = new_data)
    surv_fit$call$formula <- surv_formula

    # Set up time axis breaks (in months)
    max_time <- max(new_data[[time_var]], na.rm = TRUE)
    x_breaks <- seq(0, ceiling(max_time / 12) * 12, by = 12)

    # Set legend labels and color palette
    if (is.null(legend_labels)) {
        legend_labels <- levels(factor(new_data[[group_var]]))
    }
    n_groups <- length(legend_labels)
    color_palette <- if (n_groups == 2) {
        c("#BC3C29FF", "#0072B5FF")
    } else if (n_groups == 3) {
        c("#BC3C29FF", "#0072B5FF", "#E18727FF")
    } else if (n_groups == 4) {
        c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF")
    } else {
        RColorBrewer::brewer.pal(min(n_groups, 8), "Set1")
    }

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
        break.time.by = 12,
        xlim = c(0, max(x_breaks)),
        ylim = c(0, 1),
        legend.labs = legend_labels,
        risk.table.y.text = TRUE,
        tables.y.text = TRUE,
        risk.table.title = "Number at risk"
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

    # Define time points (in months) for summary and RMST
    time_points <- c(1, 3, 5, 10, 15) * 12

    # Summarize survival at key time points
    surv_summary <- summary(surv_fit, times = time_points)
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
        RMST_Plaque = numeric(),
        RMST_GKSRS = numeric(),
        RMST_Difference = numeric(),
        RMST_P_Value = numeric(),
        Analysis_Type = character(),
        stringsAsFactors = FALSE
    )

    # Calculate RMST for each time point
    for (time_point in time_points) {
        time_years <- round(time_point / 12, 1)
        rmst_result <- tryCatch(
            {
                # Binary treatment: 1 = GKSRS, 0 = Plaque
                treatment_binary <- ifelse(new_data[[group_var]] == "GKSRS", 1, 0)
                rmst2(
                    time = new_data[[time_var]],
                    status = new_data[[event_var]],
                    arm = treatment_binary,
                    tau = time_point
                )
            },
            error = function(e) NULL
        )
        if (!is.null(rmst_result)) {
            rmst_results <- rbind(
                rmst_results,
                data.frame(
                    Time_Point_Years = time_years,
                    Time_Point_Months = time_point,
                    RMST_Plaque = round(rmst_result$RMST.arm0$rmst[1], 2),
                    RMST_GKSRS = round(rmst_result$RMST.arm1$rmst[1], 2),
                    RMST_Difference = round(rmst_result$unadjusted.result[1, 1], 2),
                    RMST_P_Value = round(rmst_result$unadjusted.result[1, 4], 4),
                    Analysis_Type = paste0("Mean survival up to ", time_years, " years"),
                    stringsAsFactors = FALSE
                )
            )
        } else {
            rmst_results <- rbind(
                rmst_results,
                data.frame(
                    Time_Point_Years = time_years,
                    Time_Point_Months = time_point,
                    RMST_Plaque = NA,
                    RMST_GKSRS = NA,
                    RMST_Difference = NA,
                    RMST_P_Value = NA,
                    Analysis_Type = "Analysis failed",
                    stringsAsFactors = FALSE
                )
            )
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
        output_dir <- if (grepl("Overall Survival", ylab)) {
            output_dirs$obj1_os
        } else if (grepl("Progression-Free Survival", ylab)) {
            output_dirs$obj1_pfs
        } else if (grepl("PFS-2", ylab)) {
            output_dirs$obj3_pfs2
        } else {
            output_dirs$baseline_characteristics
        }
        writexl::write_xlsx(
            surv_rates,
            path = file.path(output_dir, paste0(prefix, make_filename_safe(ylab), "_survival_rates.xlsx"))
        )
        writexl::write_xlsx(
            surv_rates_wide_with_rmst,
            path = file.path(output_dir, paste0(prefix, make_filename_safe(ylab), "_survival_rates_wide.xlsx"))
        )
        writexl::write_xlsx(
            rmst_results,
            path = file.path(output_dir, paste0(prefix, make_filename_safe(ylab), "_rmst_analysis.xlsx"))
        )
    }

    # Run Cox regression and generate regression table
    cox_result <- generate_regression_table(
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
        full_data = fix_event_data,
        treatment_var = group_var
    )

    # Return all results as a list
    list(
        fit = surv_fit,
        plot = surv_plot,
        survival_rates = surv_rates,
        survival_rates_wide = surv_rates_wide_with_rmst,
        rmst_analysis = rmst_results,
        rmst_plot = plot_rmst_pvalue_progression(rmst_results, ylab, output_dirs, prefix),
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

    # Show treatment distribution
    treatment_counts <- table(pfs2_data$recurrence1_treatment_clean)
    logger::log_info("Treatment distribution:")
    print(treatment_counts)

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
            legend_labels = levels(pfs2_data$recurrence1_treatment_clean),
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

    tryCatch(
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

                png(plot_filename, width = 10, height = 7, units = "in", res = 300)

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

            png(combined_plot_filename, width = 4 * n_cols, height = 4 * n_rows + 1.5, units = "in", res = 300)
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

            return(list(
                schoenfeld_test = schoenfeld_test,
                individual_tests = p_values,
                ph_summary = ph_summary_with_global,
                plots = list(
                    individual = individual_plots,
                    combined = combined_plot_filename
                ),
                summary_file = summary_filename
            ))
        },
        error = function(e) {
            logger::log_error(sprintf("Error in PH assumption testing: %s", e$message))
            return(NULL)
        }
    )
}
