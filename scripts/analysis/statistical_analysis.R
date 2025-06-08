# Statistical Analysis Functions
# Author: Nicholas Camarda
# Description: Core statistical analysis functions for rates and survival analysis

# This file contains the core statistical analysis functions:
# - calculate_rates(): Event rates and logistic regression
# - analyze_survival(): Kaplan-Meier and Cox regression
# - create_rmst_pvalue_plot(): RMST visualization
# - analyze_pfs2(): PFS-2 analysis for recurrent patients

# Note: Some functions are very large and located in separate files:
# - test_subgroup_interaction() and create_subgroup_tables() are in subgroup_analysis.R
# - analyze_tumor_height_changes() is in tumor_height_analysis.R
# - analyze_vision_changes() and analyze_radiation_sequelae() are in vision_safety_analysis.R

#' Calculate rates and create regression tables
#'
#' Calculates event rates by group and fits a logistic regression model, returning rates, a regression table, and the model object.
#'
#' @param data Data frame.
#' @param outcome_var Name of the outcome variable (character).
#' @param time_var Name of the time-to-event variable (character).
#' @param event_var Name of the event indicator variable (character).
#' @param group_var Name of the grouping variable (default: 'treatment_group').
#' @param confounders Character vector of confounder variable names (default: NULL).
#' @param exclude_before_treatment Logical (default: TRUE). If TRUE, rows with events before treatment are excluded.
#' @param handle_rare Logical (default: TRUE). If TRUE, rare categories in confounders are collapsed into 'Other'.
#' @param dataset_name Name of the dataset (character).
#'
#' @return List with elements: rates (data frame), table (gtsummary object), model (glm object).
#' @examples
#' calculate_rates(data, "recurrence1", "tt_recurrence", "recurrence_event")
calculate_rates <- function(data, outcome_var, time_var, event_var, group_var = "treatment_group", confounders = NULL, exclude_before_treatment = TRUE, handle_rare = TRUE, dataset_name = NULL) {
    # DEBUGGING:
    # outcome_var = "recurrence1"
    # time_var = "tt_recurrence"
    # event_var = "recurrence_event"
    # group_var = "treatment_group"
    # handle_rare = TRUE; exclude_before_treatment = TRUE

     # if the grouping factor has <2 levels, don't fit glm()
     if (length(unique(data[[group_var]])) < 2) {
         warning(sprintf(
             "Only one level of %s present (%s); skipping logistic regression.",
             group_var, unique(data[[group_var]])
         ))
         return(list(
             rates = NULL,
             table = NULL,
             model = NULL
         ))
     }

    # Handle rare categories in confounders for model stability
    rare_fix_data <- data
    if (!is.null(confounders) && handle_rare) {
        rare_fix_data <- handle_rare_categories(data, confounders, threshold = THRESHOLD_RARITY)
    } 

    # Remove rows that have events before treatment
    fix_event_data <- rare_fix_data 
    if (exclude_before_treatment) {
        fix_event_data <- fix_event_data %>%
            filter(!!sym(time_var) >= 0)
        log_message(sprintf("Removed %d rows with %s before treatment", nrow(rare_fix_data) - nrow(fix_event_data), event_var))
    }
    
    # Ensure consistent factor contrasts for modeling
    fix_event_data <- ensure_consistent_contrasts(fix_event_data)

    # Filter and validate confounders using existing function
    if (!is.null(confounders)) {
        confounders_to_use <- generate_valid_confounders(data, confounders, threshold = THRESHOLD_RARITY)
    } else {
        confounders_to_use <- NULL
    }

    # Calculate rates by treatment group
    rates <- fix_event_data %>%
        group_by(!!sym(group_var)) %>%
        summarize(
            n = n(),
            events = sum(!!sym(event_var), na.rm = TRUE),
            rate = events / n * 100,
            .groups = "drop"
        )

    # Determine output directory based on outcome
    if (outcome_var == "recurrence1") {
        output_dir <- output_dirs$recurrence
    } else if (outcome_var == "mets_progression") {
        output_dir <- output_dirs$mets
    } else {
        output_dir <- tables_dir  # fallback
    }
    
    # Save high-level summary table of event rates
    writexl::write_xlsx(
        rates,
        path = file.path(output_dir, paste0(prefix, outcome_var, "_rates_summary.xlsx"))
    )
    
    # check factor levels of all variables in formula
    # fix_event_data %>%
    #     select(all_of(c(outcome_var, group_var, valid_confounders))) %>%
    #     map(~ table(.) %>% .[. > THRESHOLD_RARITY])
    
    # Then use valid_confounders in your formula
    if (length(confounders_to_use) == 0) {
        formula_str <- paste0(outcome_var, " ~ ", group_var)
    } else {
        formula_str <- paste0(outcome_var, " ~ ", group_var, " + ", paste(confounders_to_use, collapse = " + "))
    }
    formula <- as.formula(formula_str)

    # Fit logistic regression
    logit_model <- glm(formula, data = fix_event_data, family = binomial())
    print(summary(logit_model))

    # Get variable labels for better readability
    variable_labels <- get_variable_labels()
    
    # Create table with regression results
    tbl <- tbl_regression(
        logit_model,
        intercept = FALSE,
        exponentiate = TRUE,
        show_single_row = group_var,
        quiet = TRUE,
        label = variable_labels  # Apply human-readable labels
    )
    
    # Add p-values based on toggle setting
    if (SHOW_ALL_PVALUES) {
        # Show individual p-values for each coefficient
        tbl <- tbl  # No modification needed - individual p-values shown by default
    } else {
        # Show only grouped p-values (one per variable)
        tbl <- tbl %>% add_global_p()
    }
    
    tbl <- tbl %>%
        # change the column names
        modify_header(
            label     = "**Variable**",
            estimate  = "**OR**",
            p.value   = "**p-value**",
            quiet = TRUE
        ) %>%
        modify_caption( # shorten the caption
            md(sprintf("Adjusted Odds Ratios for **%s** by Treatment Group and Covariates", outcome_var))
        ) %>%
        modify_footnote(
            # apply the same footnote to every statistic column
            update = all_stat_cols() ~ "Reference level: Plaque"
        )

    # Add source note to the table
    gt_tbl <- as_gt(tbl) %>% 
        tab_source_note(
            source_note = md(sprintf(
                "Reference %s level: **%s**\n\nModel: *%s*\n\n%s\n\n%s",
                group_var,
                levels(fix_event_data[[group_var]])[1], 
                formula_str,
                if (exclude_before_treatment) {
                    sprintf("Number of rows excluded with %s before treatment: %d", event_var, nrow(rare_fix_data) - nrow(fix_event_data))
                } else {
                    sprintf("Number of rows included with %s before treatment: %d", event_var, nrow(rare_fix_data) - nrow(fix_event_data))
                },
                sprintf("Dataset: %s", dataset_name)
            ))
        )
    
    # Save table
    gt_tbl %>%
        gt::gtsave(
            filename = file.path(output_dir, paste0(prefix, outcome_var, "_rates.html"))
        )
    
    return(list(
        rates = rates,
        table = gt_tbl,
        model = logit_model
    ))
}

#' Perform survival analysis
#'
#' Runs Kaplan-Meier and Cox regression for a given time-to-event and event indicator, optionally adjusting for confounders.
#'
#' @param data Data frame.
#' @param time_var Name of the time-to-event variable (character).
#' @param event_var Name of the event indicator variable (character).
#' @param group_var Name of the grouping variable (default: 'treatment_group').
#' @param confounders Character vector of confounder variable names (default: NULL).
#' @param ylab Y-axis label for the survival plot (default: 'Survival Probability').
#' @param exclude_before_treatment Logical (default: TRUE). If TRUE, rows with events before treatment are excluded.
#' @param handle_rare Logical (default: TRUE). If TRUE, rare categories in confounders are collapsed into 'Other'.
#' @param dataset_name Name of the dataset (character).
#' @param legend_labels Character vector of legend labels for the survival plot (default: NULL).
#'
#' @return List with elements: fit (survfit object), plot (ggsurvplot), median_times (data frame), cox_model (coxph object), cox_table (gtsummary object).
#' @examples
#' analyze_survival(data, "tt_death", "death_event")
analyze_survival <- function(data, time_var, event_var, group_var = "treatment_group", confounders = NULL, ylab = "Survival Probability", exclude_before_treatment = TRUE, handle_rare = TRUE, dataset_name = NULL, legend_labels = NULL) {
    # DEBUGGING:
    # time_var = "tt_death"
    # event_var = "death_event"
    # group_var = "treatment_group"
    # ylab = "Survival Probability"
    # exclude_before_treatment = TRUE

    # If only one level of group_var, don't fit cox model
    if (length(unique(data[[group_var]])) < 2) {
        warning(sprintf(
            "Only one level of %s present (%s); skipping cox model.",
            group_var, unique(data[[group_var]])
        ))
        return(list(
            fit = NULL,
            plot = NULL,
            median_times = NULL,
            cox_model = NULL,
            cox_table = NULL
        ))
    }

    # Handle rare categories in confounders for model stability
    rare_fix_data <- data
    if (!is.null(confounders) && handle_rare) {
        rare_fix_data <- handle_rare_categories(data, confounders, threshold = THRESHOLD_RARITY)
    }

    # Remove rows that have events before treatment
    fix_event_data <- rare_fix_data
    if (exclude_before_treatment) {
        fix_event_data <- fix_event_data %>%
            filter(!!sym(time_var) >= 0)
        log_message(sprintf("Removed %d rows with %s before treatment", nrow(rare_fix_data) - nrow(fix_event_data), event_var))
    }
    
    # Ensure consistent factor contrasts for modeling
    fix_event_data <- ensure_consistent_contrasts(fix_event_data)

    # Filter and validate confounders using existing function
    if (!is.null(confounders)) {
        confounders_to_use <- generate_valid_confounders(data, confounders, threshold = THRESHOLD_RARITY)
    } else {
        confounders_to_use <- NULL
    }

    # Create formula for survival analysis
    surv_formula <- as.formula(
        paste0("Surv(", time_var, ", ", event_var, ") ~ ", group_var)
    )
    
    # Select only the variables needed for the formula
    new_data <- fix_event_data %>%
        dplyr::select(all_of(c(time_var, event_var, group_var, confounders_to_use))) 

    # Use formula with column names as strings, not symbols or objects
    surv_fit <- survival::survfit(surv_formula, data = new_data)
    surv_fit$call$formula <- surv_formula # This is necessary for non-standard evaluation

    # Determine appropriate x-axis breaks (yearly intervals: 12, 24, 36, etc.)
    max_time <- max(new_data[[time_var]], na.rm = TRUE)
    x_breaks <- seq(0, ceiling(max_time / 12) * 12, by = 12)
    
    # Determine legend labels
    if (is.null(legend_labels)) {
        # Default to factor levels of the grouping variable
        legend_labels <- levels(factor(new_data[[group_var]]))
    }
    
    # Create dynamic color palette based on number of groups
    n_groups <- length(legend_labels)
    if (n_groups == 2) {
        color_palette <- c("#BC3C29FF", "#0072B5FF")
    } else if (n_groups == 3) {
        color_palette <- c("#BC3C29FF", "#0072B5FF", "#E18727FF")
    } else if (n_groups == 4) {
        color_palette <- c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF")
    } else {
        # For more than 4 groups, use a larger palette
        color_palette <- RColorBrewer::brewer.pal(min(n_groups, 8), "Set1")
    }
    
    surv_plot <- survminer::ggsurvplot(
        fit = surv_fit,
        data = new_data,
        palette = color_palette,  # Dynamic color palette
        risk.table = TRUE,
        conf.int = FALSE,
        pval = TRUE,
        title = paste("Kaplan-Meier Survival Curves:", ylab),  # Add descriptive title
        xlab = "Time (months)",
        ylab = ylab,
        # caption = "Vertical lines (|) indicate censored patients",  # Add caption explaining censoring
        risk.table.height = 0.10,  # Reduce height to compress rows
        ggtheme = theme_minimal(),
        break.time.by = 12,  # Break every 12 months (1 year)
        xlim = c(0, max(x_breaks)),
        legend.labs = legend_labels,  # Dynamic legend labels
        risk.table.y.text = TRUE,   # Show strata labels in risk table
        tables.y.text = TRUE,       # Show strata labels in risk table
        risk.table.title = "Number at risk",  # Add back the risk table title
        # fontsize = 5,  # Smaller font size for risk table
        # risk.table.fontsize = 3
    )

    # Apply additional styling to ensure proper alignment and clean appearance
    # Make main plot text bigger
    surv_plot$plot <- surv_plot$plot +
        theme(
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bigger title
            axis.title.x = element_text(size = 14, face = "plain"),  # Bigger x-axis label
            axis.title.y = element_text(size = 14, face = "plain"),  # Bigger y-axis label
            axis.text.x = element_text(size = 12),  # Bigger x-axis tick labels
            axis.text.y = element_text(size = 12),  # Bigger y-axis tick labels
            legend.key.height = unit(0.5, "line"), # Smaller legend key height, closer rows together
            legend.title = element_text(size = 13, face = "bold"),  # Bigger legend title
            legend.text = element_text(size = 12),  # Bigger legend text
            plot.caption = element_text(size = 11, hjust = 0.5, margin = margin(t = 10))  # Bigger caption
        )
    
    # Style the risk table (keep numbers at risk smaller)
    surv_plot$table <- surv_plot$table +
        theme_minimal() +
        theme(
            plot.margin = unit(c(0, 10, 0, 10), "points"),  # Reduce top margin to bring title closer
            plot.title = element_text(
                hjust = 0, margin = margin(t = 0, b = 0),  # Remove all margins around title
                face = "bold", colour = "black", size = 12  # Bigger risk table title
            ),
            axis.text.y = element_text(size = 11, face = "plain", colour = "black", 
                                       lineheight = 0.5), # Compress line spacing between rows
            axis.text.x = element_blank(),  # Remove x-axis text since it's aligned with plot above
            axis.title.x = element_blank(), # Remove x-axis title
            axis.ticks.x = element_blank(), # Remove x-axis ticks
            axis.ticks.y = element_blank(),
            panel.grid = element_blank(), # Remove grid lines for cleaner look
            panel.border = element_blank(),
            axis.line = element_blank(),
            panel.spacing.y = unit(0, "lines"),  # Remove vertical spacing between panels
            strip.text = element_blank(),  # Remove strip text completely
            legend.key.height = unit(0.5, "line")
        ) +
        labs(title = "Number at risk")  # Explicitly add the title to ensure it shows

    # Define time points in months (e.g., 1, 3, 5, 10, 15 years)
    time_points <- c(1, 3, 5, 10, 15) * 12  # Convert years to months

    # Get survival probabilities at those time points
    surv_summary <- summary(surv_fit, times = time_points)

    # Build a data frame directly from the summary output
    surv_rates <- as.data.frame(surv_summary[c("strata", "time", "surv", "lower", "upper")]) %>%
        mutate(
            Treatment_Group = sub(".*=", "", strata),
            Time_Years = round(time / 12, 1)  # Convert months to years for display
        ) %>%
        mutate(
            across(c(surv, lower, upper), ~ round(100 * ., 1), .names = "{.col}_pct")
        ) %>%
        select(Treatment_Group, Time_Years, surv_pct, lower_pct, upper_pct)

    # Perform RESTRICTED MEAN SURVIVAL TIME (RMST) ANALYSIS
    # This compares mean survival time UP TO each specific time point
    # Answers: "Are there significant differences in survival UP TO time X?"
    
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
    
    for (i in seq_along(time_points)) {
        time_point <- time_points[i]
        time_years <- round(time_point / 12, 1)
        
        # Perform RMST analysis up to this time point
        rmst_result <- tryCatch({
            # Create binary treatment indicator (0 = Plaque, 1 = GKSRS)
            treatment_binary <- ifelse(new_data[[group_var]] == "GKSRS", 1, 0)
            
            # Run RMST analysis
            rmst2(time = new_data[[time_var]], 
                  status = new_data[[event_var]], 
                  arm = treatment_binary, 
                  tau = time_point)
        }, error = function(e) {
            return(NULL)
        })
        
        if (!is.null(rmst_result)) {
            # Extract RMST values and p-value
            rmst_plaque <- rmst_result$RMST.arm0$rmst[1]  # Plaque (arm 0)
            rmst_gksrs <- rmst_result$RMST.arm1$rmst[1]   # GKSRS (arm 1)
            rmst_diff <- rmst_result$unadjusted.result[1, 1]  # Difference
            rmst_pval <- rmst_result$unadjusted.result[1, 4]  # P-value
            
            rmst_results <- rbind(rmst_results, data.frame(
                Time_Point_Years = time_years,
                Time_Point_Months = time_point,
                RMST_Plaque = round(rmst_plaque, 2),
                RMST_GKSRS = round(rmst_gksrs, 2),
                RMST_Difference = round(rmst_diff, 2),
                RMST_P_Value = round(rmst_pval, 4),
                Analysis_Type = paste0("Mean survival up to ", time_years, " years"),
                stringsAsFactors = FALSE
            ))
        } else {
            # Analysis failed
            rmst_results <- rbind(rmst_results, data.frame(
                Time_Point_Years = time_years,
                Time_Point_Months = time_point,
                RMST_Plaque = NA,
                RMST_GKSRS = NA,
                RMST_Difference = NA,
                RMST_P_Value = NA,
                Analysis_Type = "Analysis failed",
                stringsAsFactors = FALSE
            ))
        }
    }
    
    # Simple wide format for survival rates
    surv_rates_wide <- surv_rates %>%
        mutate(Time_Label = paste0(Time_Years, "-year")) %>%
        select(Treatment_Group, Time_Label, surv_pct) %>%
        tidyr::pivot_wider(
            names_from = Time_Label,
            values_from = surv_pct,
        )
    
    # Add RMST p-values as a separate row
    surv_rates_wide_char <- surv_rates_wide %>%
        mutate(across(everything(), as.character))
    
    # Create RMST p-value row
    rmst_pvalue_row <- data.frame(
        Treatment_Group = "RMST P-Value",
        stringsAsFactors = FALSE
    )
    
    # Add RMST p-values for each time point
    for (i in 1:nrow(rmst_results)) {
        time_label <- paste0(rmst_results$Time_Point_Years[i], "-year")
        p_val <- rmst_results$RMST_P_Value[i]
        
        # Format p-value appropriately
        formatted_p <- if (is.na(p_val)) {
            "Analysis failed"
        } else if (p_val < 0.0001) {
            "<0.0001"
        } else {
            sprintf("%.3f", p_val)
        }
        
        # Only add if this time point exists in our survival rates
        if (time_label %in% names(surv_rates_wide)) {
            rmst_pvalue_row[[time_label]] <- formatted_p
        }
    }
    
    # Add RMST difference row (GKSRS - Plaque, in months)
    rmst_diff_row <- data.frame(
        Treatment_Group = "RMST Difference (months)",
        stringsAsFactors = FALSE
    )
    
    for (i in 1:nrow(rmst_results)) {
        time_label <- paste0(rmst_results$Time_Point_Years[i], "-year")
        rmst_diff <- rmst_results$RMST_Difference[i]
        
        # Format difference appropriately
        formatted_diff <- if (is.na(rmst_diff)) {
            "NA"
        } else {
            sprintf("%.1f", rmst_diff)
        }
        
        if (time_label %in% names(surv_rates_wide)) {
            rmst_diff_row[[time_label]] <- formatted_diff
        }
    }
    
    # Combine all rows
    surv_rates_wide_with_rmst <- bind_rows(
        surv_rates_wide_char, 
        rmst_pvalue_row,
        rmst_diff_row
    )

    # Determine output directory based on outcome
    if (grepl("Overall Survival", ylab)) {
        output_dir <- output_dirs$os
    } else if (grepl("Progression-Free Survival", ylab)) {
        output_dir <- output_dirs$pfs
    } else if (grepl("PFS-2", ylab)) {
        # PFS-2 is part of Objective #3 (Repeat Radiation Efficacy) - separate from primary outcomes
        output_dir <- file.path(tables_dir, "repeat_radiation_efficacy")
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    } else {
        output_dir <- tables_dir  # fallback
    }
    
    # Save survival rate tables
    writexl::write_xlsx(
        surv_rates,
        path = file.path(output_dir, paste0(prefix, ylab, "_survival_rates.xlsx"))
    )
    writexl::write_xlsx(
        surv_rates_wide_with_rmst,
        path = file.path(output_dir, paste0(prefix, ylab, "_survival_rates_wide.xlsx"))
    )
    
    # Save detailed RMST analysis results
    writexl::write_xlsx(
        rmst_results,
        path = file.path(output_dir, paste0(prefix, ylab, "_rmst_analysis.xlsx"))
    )

    # Cox model: use original data, not new_data
    if (is.null(confounders_to_use)) {
        formula_cox <- as.formula(paste0("Surv(", time_var, ",", event_var, ") ~ ", group_var))
        formula_str <- paste0("Surv(", time_var, ",", event_var, ") ~ ", group_var)
    } else {
        formula_cox <- as.formula(paste0("Surv(", time_var, ",", event_var, ") ~ ", group_var, " + ", paste(confounders_to_use, collapse = " + ")))
        formula_str <- paste0("Surv(", time_var, ",", event_var, ") ~ ", group_var, " + ", paste(confounders_to_use, collapse = " + "))
    }
    cox_model <- coxph(formula_cox, data = new_data)

    # Check if group_var has only 2 levels for show_single_row
    group_levels <- length(unique(new_data[[group_var]]))
    
    # Get variable labels for better readability
    variable_labels <- get_variable_labels()
    
    cox_table <- tbl_regression(
        cox_model,
        exponentiate = TRUE, # gives you HRs
        label = variable_labels,  # Apply human-readable labels
        show_single_row = if (group_levels == 2) group_var else NULL # only for binary variables
    )
    
    # Add p-values based on toggle setting
    if (SHOW_ALL_PVALUES) {
        # Show individual p-values for each coefficient
        cox_table <- cox_table  # No modification needed - individual p-values shown by default
    } else {
        # Show only grouped p-values (one per variable)
        cox_table <- cox_table %>% add_global_p()
    }
    
    cox_table <- cox_table %>%
        modify_header(
            label    = "**Variable**",
            estimate = "**HR (95 % CI)**",
            p.value  = "**p-value**",
            quiet = TRUE
        ) %>%
        modify_caption(sprintf("%s: Adjusted Cox Proportional-Hazards Model", ylab)) %>%
        modify_footnote(
            update = all_stat_cols() ~ "Reference level: Plaque"
        ) %>%
        as_gt() %>%
        tab_source_note(
            source_note = md(sprintf(
                "Reference %s level: **%s**\n\nModel: *%s*\n\n%s\n\n%s",
                group_var,
                levels(fix_event_data[[group_var]])[1],
                formula_str,
                if (exclude_before_treatment) {
                    sprintf("Number of rows excluded with %s before treatment: %d", event_var, nrow(rare_fix_data) - nrow(fix_event_data))
                } else {
                    sprintf("Number of rows included with %s before treatment: %d", event_var, nrow(rare_fix_data) - nrow(fix_event_data))
                },
                sprintf("Dataset: %s", dataset_name)
            ))
        )

    # Save table
    cox_table %>%
        gt::gtsave(
            filename = file.path(output_dir, paste0(prefix, ylab, "_cox.html"))
        )
    
    combined <- plot_grid(
        surv_plot$plot,
        surv_plot$table,
        ncol    = 1,
        rel_heights = c(4.5, 0.5),  # Make risk table much smaller to compress rows
        align = "v"  # Vertical alignment to ensure x-axes line up
    )

    ggsave(
        file.path(figures_dir, paste0(prefix, ylab, "_survival.png")),
        combined,
        width = 10, height = 8, dpi = 300, bg = "white"
    )
    
    # Create RMST p-value progression plot
    rmst_plot <- create_rmst_pvalue_plot(rmst_results, ylab)
    
    return(list(
        fit = surv_fit,
        plot = surv_plot,
        survival_rates = surv_rates,
        survival_rates_wide = surv_rates_wide_with_rmst,
        rmst_analysis = rmst_results,
        rmst_plot = rmst_plot,
        cox_model = cox_model,
        cox_table = cox_table
    ))
}

#' Create RMST P-value Progression Plot
#'
#' Creates a visualization showing how RMST p-values change over time points
#' and highlights significance thresholds.
#'
#' @param rmst_results Data frame with RMST analysis results
#' @param outcome_label Character string for the outcome being analyzed
#'
#' @return ggplot object
create_rmst_pvalue_plot <- function(rmst_results, outcome_label) {
    # Filter out failed analyses
    plot_data <- rmst_results %>%
        filter(!is.na(RMST_P_Value)) %>%
        mutate(
            Significant = RMST_P_Value < 0.05,
            Log_P_Value = -log10(RMST_P_Value),
            # RMST_Difference is GKSRS - Plaque (positive = GKSRS better, negative = GKSRS worse)
            Direction = case_when(
                !Significant ~ "Not significant",
                RMST_Difference > 0 ~ "GKSRS advantage",
                RMST_Difference < 0 ~ "GKSRS disadvantage",
                TRUE ~ "Not significant"
            ),
            Significance_Level = case_when(
                RMST_P_Value < 0.001 ~ "p < 0.001",
                RMST_P_Value < 0.01 ~ "p < 0.01", 
                RMST_P_Value < 0.05 ~ "p < 0.05",
                TRUE ~ "Not significant"
            )
        )
    
    # Create the plot
    p <- ggplot(plot_data, aes(x = Time_Point_Years, y = RMST_P_Value)) +
        geom_line(size = 1.2, color = "steelblue", alpha = 0.8) +
        geom_point(aes(color = Significant, size = Significant), alpha = 0.9) +
        geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", size = 0.8) +
        geom_hline(yintercept = 0.01, linetype = "dotted", color = "darkred", size = 0.6) +
        annotate("text", x = max(plot_data$Time_Point_Years), y = 0.05, label = "p = 0.05", 
                 hjust = -0.1, vjust = -0.2, color = "red", size = 3.5) +
        annotate("text", x = max(plot_data$Time_Point_Years), y = 0.01, label = "p = 0.01", 
                 hjust = -0.1, vjust = -0.2, color = "darkred", size = 3.5) +
        scale_color_manual(
            values = c("TRUE" = "#E31A1C", "FALSE" = "#1F78B4"),
            labels = c("TRUE" = "Significant (p < 0.05)", "FALSE" = "Not significant"),
            name = "Statistical Significance"
        ) +
        scale_size_manual(
            values = c("TRUE" = 4, "FALSE" = 2.5),
            guide = "none"
        ) +
        scale_x_continuous(
            breaks = plot_data$Time_Point_Years,
            labels = paste0(plot_data$Time_Point_Years, " yr"),
            limits = c(min(plot_data$Time_Point_Years), max(plot_data$Time_Point_Years) + 1.25)
        ) +
        scale_y_continuous(
            limits = c(0, max(plot_data$RMST_P_Value) * 1.1),
            breaks = c(0, seq(0.1, 1, 0.1))
        ) +
        labs(
            title = paste("RMST P-value Progression:", outcome_label),
            subtitle = "Restricted Mean Survival Time Analysis at Different Time Points",
            x = "Analysis Time Point",
            y = "P-value",
            caption = "Dashed line: p = 0.05 | Dotted line: p = 0.01\nRMST difference: + = GKSRS advantage, - = GKSRS disadvantage (vs Plaque)"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20)),
            axis.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "bottom",
            panel.grid.minor = element_blank(),
            plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 15))
        )
    
    # Add text annotations for p-values and direction
    p <- p + geom_text(
        aes(label = sprintf("p=%.3f\n%s%.1f mo", RMST_P_Value, 
                           ifelse(RMST_Difference > 0, "+", ""), RMST_Difference)),
        vjust = -0.8, hjust = 0.5, size = 3, color = "black"
    )
    
    # Save the plot
    output_dir <- switch(
        gsub(".*:", "", outcome_label),
        " Overall Survival Probability" = output_dirs$os,
        " Progression-Free Survival Probability" = output_dirs$pfs,
        figures_dir  # fallback
    )
    
    ggsave(
        file.path(output_dir, paste0(prefix, gsub("[^A-Za-z0-9]", "_", outcome_label), "_rmst_pvalue_progression.png")),
        p,
        width = 10, height = 6, dpi = 300, bg = "white"
    )
    
          return(p)
  }

#' Analyze Progression-Free Survival-2 (PFS-2) for Recurrent Patients
#'
#' Analyzes outcomes for patients who had local recurrence and received additional treatment.
#' Compares different second-line treatments.
#'
#' @param data Data frame with patient data
#' @param confounders Character vector of confounder variable names (default: NULL)
#' @param dataset_name Name of the dataset (character)
#'
#' @return List with survival analysis results
#' @examples
#' analyze_pfs2(data, confounders = c("age_at_diagnosis", "sex"))
analyze_pfs2 <- function(data, confounders = NULL, dataset_name = NULL) {
    log_message("Starting PFS-2 analysis for recurrent patients")
    
    # Filter to patients with valid PFS-2 data (variables now created in data processing)
    pfs2_data <- data %>%
        filter(
            !is.na(tt_pfs2_months), 
            tt_pfs2_months >= 0,
            !is.na(recurrence1_treatment_clean)
        )
    
    log_message(sprintf("Found %d patients with valid PFS-2 data", nrow(pfs2_data)))
    
    if (nrow(pfs2_data) == 0) {
        log_message("No patients with valid PFS-2 data found")
        return(list(
            pfs2_data = NULL,
            survival_analysis = NULL,
            summary_table = NULL
        ))
    }
    
    # Show treatment distribution
    treatment_counts <- table(pfs2_data$recurrence1_treatment_clean)
    log_message("Treatment distribution:")
    print(treatment_counts)
    
    log_message(sprintf("Final PFS-2 analysis dataset: %d patients", nrow(pfs2_data)))
    log_message(sprintf("PFS-2 events (2nd recurrence): %d", sum(pfs2_data$pfs2_event)))
    
    # Check if we have enough patients and events for analysis
    if (nrow(pfs2_data) < 10) {
        log_message("Insufficient patients for PFS-2 analysis")
        return(list(
            pfs2_data = pfs2_data,
            survival_analysis = NULL,
            summary_table = NULL
        ))
    }
    
    # Check if we have enough events per group for survival analysis
    events_per_group <- pfs2_data %>%
        group_by(recurrence1_treatment_clean) %>%
        summarize(events = sum(pfs2_event), .groups = "drop")
    
    total_events <- sum(pfs2_data$pfs2_event)
    groups_with_events <- sum(events_per_group$events > 0)
    
    if (total_events < 5 || groups_with_events < 2) {
        log_message("ERROR: Insufficient events for PFS-2 survival analysis")
        log_message(sprintf("Total events: %d (minimum 5 required)", total_events))
        log_message(sprintf("Groups with events: %d (minimum 2 required)", groups_with_events))
        log_message("Events per group:")
        print(events_per_group)
        log_message("Skipping survival analysis due to insufficient data")
        
        pfs2_survival <- list(
            fit = NULL,
            plot = NULL,
            survival_rates = NULL,
            cox_model = NULL,
            cox_table = NULL
        )
    } else {
        # Use existing analyze_survival function with dynamic legend labels
        log_message("Performing PFS-2 survival analysis")
        pfs2_survival <- analyze_survival(
            data = pfs2_data,
            time_var = "tt_pfs2_months",
            event_var = "pfs2_event", 
            group_var = "recurrence1_treatment_clean",
            confounders = confounders,
            ylab = "PFS-2 Probability (Freedom from 2nd Recurrence)",
            exclude_before_treatment = FALSE,  # Already filtered appropriately
            handle_rare = TRUE,
            dataset_name = paste0(dataset_name, "_pfs2_recurrent"),
            legend_labels = levels(pfs2_data$recurrence1_treatment_clean)
        )
    }
    
    # Create summary table of PFS-2 characteristics
    summary_table <- pfs2_data %>%
        select(recurrence1_treatment_clean, tt_pfs2_months, pfs2_event, 
               age_at_diagnosis, sex) %>%  # Remove location since it was collapsed
        tbl_summary(
            by = recurrence1_treatment_clean,
            missing = "no",
            type = list(
                pfs2_event ~ "continuous"  # Treat as continuous for sum calculation
            ),
            statistic = list(
                tt_pfs2_months ~ "{median} ({p25}, {p75})",
                pfs2_event ~ "{sum}",  # Just show sum, not percentage
                age_at_diagnosis ~ "{mean} ({sd})"
            ),
            digits = list(
                tt_pfs2_months ~ 1,
                age_at_diagnosis ~ 1
            ),
            label = list(
                tt_pfs2_months ~ "PFS-2 Time (months)",
                pfs2_event ~ "2nd Recurrence Events",
                age_at_diagnosis ~ "Age at Diagnosis",
                sex ~ "Sex"
            )
        ) %>%
        add_overall() %>%
        add_p() %>%
        modify_header(
            label = "**Characteristic**",
            stat_0 = "**Overall**\nN = {N}",
            quiet = TRUE
        ) %>%
        modify_caption("PFS-2 Analysis: Characteristics of Recurrent Patients by Second-Line Treatment") %>%
        as_gt()
    
    # Note: File saving is handled by the analyze_survival function called above
    # which properly organizes outputs into the established directory structure
    
    log_message("PFS-2 analysis completed")
    
    return(list(
        pfs2_data = pfs2_data,
        survival_analysis = pfs2_survival,
        summary_table = summary_table
    ))
} 