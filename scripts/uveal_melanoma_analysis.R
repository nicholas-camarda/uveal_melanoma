# Uveal Melanoma Treatment Outcomes Analysis
# Author: Nicholas Camarda
# Date: 5/10/2025
# Description: Analysis comparing outcomes between Gamma Knife and plaque brachytherapy
#              for uveal melanoma treatment, including both full and restricted cohort analyses
#

# Data Structure:
# - final_data/
#   ├── Original Files/    # Raw, unmodified data files
#   ├── Analytic Dataset/  # Processed and cleaned data
#   └── Analysis/         # Output tables and figures

# Install required packages if you don't already have them
# install.packages(c("tidyverse", "readxl", "writexl", "gtsummary", "survRM2", "survival", "survminer", "gt", "forestploter", "grid", "cowplot"))

# Load required libraries
library(tidyverse) # For data manipulation and visualization
library(readxl) # For reading Excel files
library(writexl) # For writing Excel files
library(gtsummary) # For creating publication-ready tables
library(survival) # For survival analysis
library(survminer) # For survival visualization
library(gt) # For table formatting
library(forestploter) # For forest plots
library(grid) # for unit()
library(cowplot) # for combining plots
library(survRM2, quietly = TRUE)

# Source the data processing script
source("scripts/data_processing.R")

# Set to FALSE to suppress detailed logging
VERBOSE <- TRUE 

# Define data paths
DATA_DIR <- "final_data"
RAW_DATA_DIR <- file.path(DATA_DIR, "Original Files")
PROCESSED_DATA_DIR <- file.path(DATA_DIR, "Analytic Dataset")
OUTPUT_DIR <- file.path(DATA_DIR, "Analysis")

# Minimum number of observations required to keep a category
THRESHOLD_RARITY <- 5 

# Define confounders for adjustment
confounders <- c(
    "age_at_diagnosis", "sex", "location",
    # "initial_overall_stage", "initial_tumor_height", "initial_tumor_diameter", "biopsy1_gep",
    "optic_nerve"
)

# Define time conversion constants
DAYS_IN_YEAR <- 365.25
DAYS_IN_MONTH <- 30.44

# Define subgroup variables for analysis
subgroup_vars <- c(
    "age_at_diagnosis", "sex", "location", "initial_overall_stage", "initial_t_stage",
    "initial_tumor_height", "initial_tumor_diameter", "biopsy1_gep", "optic_nerve"
)

#' List available datasets
#'
#' Lists all datasets in the processed data directory that have an .rds extension.
#'
#' @return A character vector of dataset names.
#'
#' @examples
#' list_available_datasets()
list_available_datasets <- function() {
    datasets <- list.files(PROCESSED_DATA_DIR, pattern = "\\.rds$")
    log_message(sprintf("Found %d datasets to analyze", length(datasets)))
    print(datasets)
    return(gsub("\\.rds$", "", datasets))
}

#' Handle rare categories in factor variables
#'
#' Collapses rare categories in specified factor variables into 'Other' if their count is below a threshold.
#'
#' @param data Data frame containing the variables.
#' @param vars Character vector of variable names to check.
#' @param threshold Minimum number of observations required to keep a category (default: 5).
#'
#' @return Data frame with rare categories collapsed to 'Other'.
#' @examples
#' handle_rare_categories(data, vars = c("sex", "location"), threshold = 5)
handle_rare_categories <- function(data, vars, threshold = 5) {
    if (VERBOSE) {
        log_message(sprintf("\nChecking for rare categories (threshold: %d):", threshold))
    }

    for (var in vars) {
        if (is.factor(data[[var]])) {
            log_message(sprintf("Checking for rare categories in %s", var))
            # Get category counts
            cat_counts <- table(data[[var]])
            rare_cats <- names(cat_counts)[cat_counts < threshold]

            if (length(rare_cats) > 0) {
                if (VERBOSE) {
                    log_message(sprintf("\nCollapsing rare categories in %s:", var))
                    for (cat in rare_cats) {
                        log_message(sprintf("- %s (n=%d)", cat, cat_counts[cat]))
                    }
                }

                # Collapse rare categories into "Other"
                data[[var]] <- fct_collapse(data[[var]],
                    Other = rare_cats
                ) %>%
                    fct_relevel("Other", after = Inf)
            }
        }
    }

    return(data)
}

#' Generate valid confounders
#'
#' Generates a list of valid confounders that have more than 1 level and at least THRESHOLD_RARITY counts per level.
#'
#' @param data Data frame.
#' @param confounders Character vector of confounder variable names.
#' @param threshold Minimum number of observations required to keep a category (default: THRESHOLD_RARITY).
#' @return Character vector of valid confounders.
#' @examples
#' generate_valid_confounders(data, confounders)
generate_valid_confounders <- function(data, confounders, threshold = THRESHOLD_RARITY) {
    # Before fitting the model, filter confounders to those with >1 level and at least THRESHOLD_RARITY counts per level
    keep_cfs <- sapply(confounders, function(var) {
        var_data <- data[[var]]
        if (is.factor(var_data)) {
            tab <- table(var_data)
            return(sum(tab >= THRESHOLD_RARITY) >= 2)
        } else {
            # For non-factors, require >1 unique value and at least THRESHOLD_RARITY non-NA values
            return(length(unique(na.omit(var_data))) > 1 && sum(!is.na(var_data)) >= THRESHOLD_RARITY)
        }
    })
    valid_confounders <- confounders[keep_cfs]
    # Check if any confounders were removed
    if (VERBOSE && length(confounders) != length(valid_confounders)) {
        log_message("Removed confounders with only 1 level or <THRESHOLD_RARITY counts:")
        log_message(paste(setdiff(confounders, valid_confounders), collapse = ", "))
    }
    return(valid_confounders)
}

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

    # Filter and validate confounders using existing function
    if (!is.null(confounders)) {
        confounders_to_use <- generate_valid_confounders(data, confounders, threshold = THRESHOLD_RARITY)
    } else {
        confounders_to_use <- NULL
    }

    # Calculate rates by treatment group
    rates <- fix_event_data %>%
        group_by(!!sym(group_var)) %>%
        summarise(
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

    # Create table with regression results
    tbl <- tbl_regression(
        logit_model,
        intercept = FALSE,
        exponentiate = TRUE,
        show_single_row = group_var,
        quiet = TRUE
    ) %>%
        # could remove this to show all p-values
        add_global_p() %>% 
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
#'
#' @return List with elements: fit (survfit object), plot (ggsurvplot), median_times (data frame), cox_model (coxph object), cox_table (gtsummary object).
#' @examples
#' analyze_survival(data, "tt_death", "death_event")
analyze_survival <- function(data, time_var, event_var, group_var = "treatment_group", confounders = NULL, ylab = "Survival Probability", exclude_before_treatment = TRUE, handle_rare = TRUE, dataset_name = NULL) {
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
    
    surv_plot <- survminer::ggsurvplot(
        fit = surv_fit,
        data = new_data,
        palette = c("#BC3C29FF", "#0072B5FF"),  # Use palette instead of col
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
        legend.labs = c("Plaque", "GKSRS"),  # Clean legend labels
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

    # surv_summary <- summary(surv_fit)
    # print(surv_summary$table)
    # median_times <- data.frame(
    #     strata = rownames(surv_summary$table),
    #     records = surv_summary$table[, "records"],
    #     events = surv_summary$table[, "events"],
    #     median_survival = surv_summary$table[, "median"],
    #     conf.lower = surv_summary$table[, "0.95LCL"],
    #     conf.upper = surv_summary$table[, "0.95UCL"]
    # )

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
    } else if (grepl("Progression", ylab)) {
        output_dir <- output_dirs$pfs
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

    # TODO: make confounders a list and integrate into tbl_regression labels = list()
    # confounders <- list(
    #     treatment_group = "Treatment",
    #     age_at_diagnosis = "Age at diagnosis",
    #     sex = "Sex",
    #     location = "Tumor location",
    #     optic_nerve = "Optic nerve involvement"
    # )

    cox_table <- tbl_regression(
        cox_model,
        exponentiate = TRUE, # gives you HRs
        # label = confounders,
        show_single_row = group_var # show Plaque row first
    ) %>%
        # add_nevent() %>% # add a column N(events)
        # add_n() %>% # add a column N(total)
        add_global_p() %>% # overall p for multi-levels
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

#' Analyze tumor height changes
#'
#' Calculates and summarizes changes in tumor height by treatment group, returning summary statistics and a table.
#' Now includes both primary analysis (without baseline height adjustment) and sensitivity analysis (with baseline height adjustment).
#'
#' @param data Data frame with tumor height variables.
#'
#' @return List with elements: changes (summary data frame), table (gtsummary object), primary_regression_model (lm object), primary_regression_table (gtsummary object), sensitivity_regression_model (lm object), sensitivity_regression_table (gtsummary object).
#' @examples
#' analyze_tumor_height_changes(data)
analyze_tumor_height_changes <- function(data) {
    # Calculate height changes (row-level)
    data_with_height_change <- data %>%
        mutate(
            # Calculate height change as the difference between the initial 
            # tumor height and the height at the time of recurrence *or* last follow-up
            # Post treatment1 height = recurrence1 pretreatment height
            height_change = case_when(
                recurrence1 == "Y" ~ initial_tumor_height - recurrence1_pretreatment_height,
                TRUE ~ initial_tumor_height - last_height
            )
        )

    # Summary statistics (grouped)
    height_changes <- data_with_height_change %>%
        group_by(treatment_group) %>%
        summarise(
            n = n(),
            mean_change = mean(height_change, na.rm = TRUE),
            sd_change = sd(height_change, na.rm = TRUE),
            median_change = median(height_change, na.rm = TRUE),
            iqr_change = IQR(height_change, na.rm = TRUE),
            .groups = "drop"
        )
    
    plaque <- data_with_height_change %>% filter(treatment_group == "Plaque")
    gk <- data_with_height_change %>% filter(treatment_group == "GKSRS")
    wilcox.test(height_change ~ treatment_group, data = data_with_height_change)

    # Table for publication (row-level input)
    tbl <- data_with_height_change %>%
        select(treatment_group, height_change) %>%
        tbl_summary(
            missing = "no",
            by = treatment_group,
            type = list(height_change ~ "continuous"),
            statistic = list(height_change ~ "{mean} ({sd})"),
            digits = list(height_change ~ 1),
            # label = list(
            #     height_change ~ "Change in Tumor Height (mm)"
            # )
        ) %>%
        add_p(test = list(all_continuous() ~ "wilcox.test")) %>%
        modify_header(quiet = TRUE) %>%
        modify_caption("Change in Tumor Height (Initial - Last Measured or Pre-Retreatment), by Treatment Group") %>%
        modify_footnote(
            update = all_stat_cols() ~ "Mean (SD)"
        ) %>%
        as_gt()
    
    # Save table
    tbl %>%
        gt::gtsave(
            filename = file.path(output_dirs$height_primary, paste0(prefix, "height_changes.html"))
        )
    
    # PRIMARY ANALYSIS: Linear regression WITHOUT initial tumor height adjustment
    log_message("Fitting PRIMARY linear regression model for tumor height changes (without baseline height adjustment)")
    primary_height_lm <- lm(height_change ~ treatment_group + recurrence1, data = data_with_height_change)
    
    primary_height_lm_tbl <- tbl_regression(primary_height_lm,
        exponentiate = FALSE,
        intercept = FALSE
    ) %>%
        modify_header(
            label = "**Characteristic**",
            estimate = "**Beta**",
            ci = "**95% CI**",
            p.value = "**p-value**",
            quiet = TRUE
        ) %>%
        modify_caption("PRIMARY ANALYSIS: Linear Regression of Change in Tumor Height (without baseline height adjustment)") %>%
        modify_footnote(
            update = all_stat_cols() ~ "Adjusted for treatment group and recurrence status only. Reference level: Plaque. Primary analysis to avoid overadjustment bias."
        ) %>%
        as_gt()
    
    # Save primary table
    primary_height_lm_tbl %>%
        gt::gtsave(
            filename = file.path(output_dirs$height_primary, paste0(prefix, "height_lm_primary.html"))
        )

    # SENSITIVITY ANALYSIS: Linear regression WITH initial tumor height adjustment
    log_message("Fitting SENSITIVITY linear regression model for tumor height changes (with baseline height adjustment)")
    sensitivity_height_lm <- lm(height_change ~ treatment_group + recurrence1 + initial_tumor_height, data = data_with_height_change)
    
    sensitivity_height_lm_tbl <- tbl_regression(sensitivity_height_lm,
        exponentiate = FALSE,
        intercept = FALSE
    ) %>%
        modify_header(
            label = "**Characteristic**",
            estimate = "**Beta**",
            ci = "**95% CI**",
            p.value = "**p-value**",
            quiet = TRUE
        ) %>%
        modify_caption("SENSITIVITY ANALYSIS: Linear Regression of Change in Tumor Height (with baseline height adjustment)") %>%
        modify_footnote(
            update = all_stat_cols() ~ "Adjusted for treatment group, recurrence status, and initial tumor height. Reference level: Plaque. Sensitivity analysis including baseline adjustment."
        ) %>%
        as_gt()
    
    # Save sensitivity table
    sensitivity_height_lm_tbl %>%
        gt::gtsave(
            filename = file.path(output_dirs$height_sensitivity, paste0(prefix, "height_lm_sensitivity.html"))
        )

    return(list(
        changes = height_changes,
        table = tbl,
        primary_regression_model = primary_height_lm,
        primary_regression_table = primary_height_lm_tbl,
        sensitivity_regression_model = sensitivity_height_lm,
        sensitivity_regression_table = sensitivity_height_lm_tbl
    ))
}

#' Bin continuous variables
#'
#' Bins a continuous variable using quantiles or custom breaks.
#'
#' @param vec Numeric vector to bin.
#' @param bins Number of bins (default: 2).
#' @param custom_breaks Optional custom breakpoints.
#' @param varname Optional variable name (for labeling).
#' @param digits_lab Number of digits to round the labels (default: 2).
#'
#' @return Factor with binned values.
#' @examples
#' bin_continuous(1:10, bins = 3)
bin_continuous <- function(vec, bins = 2, custom_breaks = NULL, varname = NULL, digits_lab = 2) {
  if (!is.null(custom_breaks)) {
    cut(vec, breaks = custom_breaks, include.lowest = TRUE, right = FALSE)
  } else {
    # Use quantiles (e.g., median split for bins=2, tertiles for bins=3, etc.)
    q <- quantile(vec, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE)
    # Ensure unique breaks (if not, fallback to pretty)
    if (length(unique(q)) < length(q)) {
      q <- pretty(vec, n = bins)
    }
    cut(vec, breaks = q, include.lowest = TRUE, right = FALSE, dig.lab = digits_lab)
  }
}


#' Summarize key variables in the dataset
#'
#' Prints summary statistics and distributions for key variables in the data.
#'
#' @param data Data frame.
#'
#' @return None. Side effect: prints summary to console.
#' @examples
#' summarize_data(data)
summarize_data <- function(data) {
    if (VERBOSE) {
        log_message("\nData Summary:")
        log_message(sprintf("Total patients: %d", nrow(data)))
        
        log_message("\nTreatment Groups:")
        print(table(data$treatment_group))
        
        log_message("\nCohort Distribution:")
        print(table(data$cohort))
        
        log_message("\nTumor Characteristics:")
        log_message(sprintf("Location: %s", paste(unique(data$location), collapse=", ")))
        log_message(sprintf("Optic Nerve Involvement: %s", paste(unique(data$optic_nerve), collapse=", ")))
        log_message(sprintf("Initial Stage: %s", paste(unique(data$initial_overall_stage), collapse=", ")))
        
        log_message("\nGene Expression Profile:")
        print(table(data$biopsy1_gep))
        
        log_message("\nOutcomes:")
        log_message(sprintf("Recurrence: %d patients", sum(data$recurrence_event)))
        log_message(sprintf("Metastasis: %d patients", sum(data$mets_event)))
        log_message(sprintf("Death: %d patients", sum(data$death_event)))
        
        log_message("\nFollow-up:")
        log_message(sprintf("Median follow-up: %.1f years", median(data$follow_up_years, na.rm=TRUE)))
    }
}

#' Create formatted HTML table for subgroup interaction results
#'
#' Creates a publication-ready HTML table showing treatment effects within each subgroup level
#' with interaction p-values and detailed sample size information.
#'
#' @param subgroup_results List of results from test_subgroup_interaction
#' @param dataset_name Name of the dataset for table caption
#' @param output_dir Directory to save the HTML tables
#' @param prefix File prefix for naming
#'
#' @return None. Saves HTML tables to specified directory.
#' @examples
#' create_subgroup_tables(subgroup_results, "Full Cohort", tables_dir, prefix)
create_subgroup_tables <- function(subgroup_results, dataset_name, subgroup_dir, prefix) {
    
    # subgroup_dir is now passed directly from calling function
    if (!dir.exists(subgroup_dir)) {
        dir.create(subgroup_dir, recursive = TRUE)
    }
    
    # Process each subgroup variable
    for (var_name in names(subgroup_results)) {
        result <- subgroup_results[[var_name]]
        
        if (!is.null(result$subgroup_effects) && nrow(result$subgroup_effects) > 0) {
            
            # Create individual table for this variable
            effects_data <- result$subgroup_effects %>%
                mutate(
                    # Format treatment effect with CI
                    Treatment_Effect_CI = ifelse(
                        is.na(treatment_effect),
                        "Insufficient data",
                        sprintf("%.2f (%.2f, %.2f)", treatment_effect, ci_lower, ci_upper)
                    ),
                    # Format p-value
                    P_Value = ifelse(is.na(p_value), "—", sprintf("%.4f", p_value)),
                    # Format sample sizes
                    Sample_Size = sprintf("%d (%d Plaque + %d GKSRS)", n_total, n_plaque, n_gksrs),
                    # Clean subgroup level names
                    Subgroup_Level = tools::toTitleCase(as.character(subgroup_level))
                ) %>%
                select(
                    `Subgroup Level` = Subgroup_Level,
                    `Sample Size` = Sample_Size,
                    `Treatment Effect (95% CI)` = Treatment_Effect_CI,
                    `P-value` = P_Value
                )
            
            # Format interaction p-value for display
            interaction_text <- ifelse(
                is.na(result$interaction_p), 
                "Model convergence issue", 
                sprintf("%.4f", result$interaction_p)
            )
            
            # Determine significance and interpretation
            interaction_significant <- !is.na(result$interaction_p) && result$interaction_p < 0.05
            significance_text <- ifelse(
                is.na(result$interaction_p),
                "Cannot determine significance due to model issues",
                ifelse(interaction_significant,
                       "**Significant interaction (p < 0.05):** Treatment effect differs across subgroups",
                       "**Non-significant interaction (p ≥ 0.05):** Treatment effect is similar across subgroups")
            )
            
            # Create gt table with interaction info prominently displayed
            var_table <- effects_data %>%
                gt::gt() %>%
                gt::tab_header(
                    title = md(sprintf("**Subgroup Analysis: %s**", tools::toTitleCase(gsub("_", " ", var_name)))),
                    subtitle = md(sprintf("Treatment Effect on Tumor Height Change | **Interaction P-value: %s**", interaction_text))
                ) %>%
                gt::tab_source_note(
                    source_note = md(sprintf(
                        "**Treatment Effect:** GKSRS vs Plaque (reference). Positive values indicate greater height reduction with GKSRS.\n\n%s\n\n**Model:** %s\n\n**Confounders:** %s\n\n**Dataset:** %s",
                        significance_text,
                        ifelse(is.null(result$formula_used), "Model formula not available", result$formula_used),
                        ifelse(is.null(result$confounders_used) || length(result$confounders_used) == 0, 
                               "None", 
                               paste(result$confounders_used, collapse = ", ")),
                        dataset_name
                    ))
                ) %>%
                gt::tab_style(
                    style = list(
                        gt::cell_text(weight = "bold")
                    ),
                    locations = gt::cells_column_labels()
                ) %>%
                gt::tab_style(
                    style = list(
                        gt::cell_text(style = "italic")
                    ),
                    locations = gt::cells_body(
                        columns = "Treatment Effect (95% CI)",
                        rows = grepl("Insufficient", `Treatment Effect (95% CI)`)
                    )
                ) %>%
                # Highlight significant interactions in the subtitle
                gt::tab_style(
                    style = list(
                        gt::cell_fill(color = "#e8f4fd"),
                        gt::cell_text(weight = "bold")
                    ),
                    locations = gt::cells_title(groups = "subtitle")
                ) %>%
                gt::fmt_markdown(columns = everything())
            
            # Apply special styling for significant interactions
            if (interaction_significant) {
                var_table <- var_table %>%
                    gt::tab_style(
                        style = list(
                            gt::cell_fill(color = "#fff3cd"),
                            gt::cell_text(weight = "bold")
                        ),
                        locations = gt::cells_title(groups = "subtitle")
                    )
            }
            
            # Save individual table
            var_table %>%
                gt::gtsave(
                    filename = file.path(subgroup_dir, paste0(prefix, "subgroup_", var_name, ".html"))
                )
        }
    }
    
    log_message(sprintf("Saved subgroup analysis tables to: %s", subgroup_dir))
}

#' Test treatment × subgroup interaction for tumor height change
#'
#' Tests whether treatment effect on tumor height change differs across subgroups
#' using interaction terms. Bins continuous variables at specified percentile.
#' Now handles confounders while automatically excluding the subgroup variable to avoid collinearity.
#'
#' @param data Data frame containing the analysis variables
#' @param subgroup_var Name of the subgroup variable (character)
#' @param percentile_cut Percentile for binning continuous variables (default: 0.5 for median split)
#' @param confounders Character vector of confounders to adjust for (subgroup variable will be automatically excluded)
#' @param include_baseline_height Logical, whether to include initial_tumor_height as a confounder (default: FALSE for primary analysis)
#' @param create_tables Logical, whether to create formatted HTML tables (default: FALSE for individual calls)
#'
#' @return List containing:
#'   - interaction_p: P-value for the interaction term
#'   - subgroup_effects: Data frame with treatment effects in each subgroup
#'   - model: The fitted linear model object
#'   - subgroup_var_used: Name of the binned variable created
#'   - formula_used: The formula used for the model
#'   - confounders_used: Character vector of confounders actually used in the model
#'
#' @examples
#' test_subgroup_interaction(data, "age_at_diagnosis", confounders = c("sex", "location"))
#' test_subgroup_interaction(data, "initial_tumor_height", percentile_cut = 0.75, include_baseline_height = TRUE)
test_subgroup_interaction <- function(data, subgroup_var, percentile_cut = 0.5, confounders = NULL, include_baseline_height = FALSE, create_tables = FALSE) {
    
    # Calculate tumor height change if not already present
    if (!("height_change" %in% names(data))) {
        data <- data %>%
            mutate(
                height_change = case_when(
                    recurrence1 == "Y" ~ initial_tumor_height - recurrence1_pretreatment_height,
                    TRUE ~ initial_tumor_height - last_height
                )
            )
    }
    
    # Check if subgroup variable exists and has variation
    if (!subgroup_var %in% names(data)) {
        warning(sprintf("Variable '%s' not found in data", subgroup_var))
        return(list(interaction_p = NA, subgroup_effects = data.frame(), 
                   model = NULL, subgroup_var_used = NA, formula_used = NA, confounders_used = NA))
    }
    
    # Remove rows with missing subgroup variable
    data <- data %>% filter(!is.na(.data[[subgroup_var]]))
    
    if (nrow(data) == 0) {
        warning(sprintf("No data remaining after removing missing values for '%s'", subgroup_var))
        return(list(interaction_p = NA, subgroup_effects = data.frame(), 
                   model = NULL, subgroup_var_used = NA, formula_used = NA, confounders_used = NA))
    }
    
    # Handle confounders: exclude the subgroup variable to avoid collinearity
    confounders_to_use <- NULL
    if (!is.null(confounders)) {
        # Remove the subgroup variable from confounders if present
        confounders_to_use <- confounders[confounders != subgroup_var]
        
        # Add initial_tumor_height if requested and not already present
        if (include_baseline_height && !"initial_tumor_height" %in% confounders_to_use) {
            confounders_to_use <- c(confounders_to_use, "initial_tumor_height")
        }
        
        # Validate confounders using existing function
        if (length(confounders_to_use) > 0) {
            confounders_to_use <- generate_valid_confounders(data, confounders_to_use, threshold = THRESHOLD_RARITY)
        }
        
        # Log which confounders are being used
        if (VERBOSE) {
            if (subgroup_var %in% confounders) {
                log_message(sprintf("Excluded '%s' from confounders to avoid collinearity with subgroup variable", subgroup_var))
            }
            if (length(confounders_to_use) > 0) {
                log_message(sprintf("Using confounders for %s interaction: %s", subgroup_var, paste(confounders_to_use, collapse = ", ")))
            } else {
                log_message(sprintf("No confounders used for %s interaction", subgroup_var))
            }
        }
    }
    
    # Track if variable was originally continuous for coefficient naming
    was_continuous <- is.numeric(data[[subgroup_var]]) || is.integer(data[[subgroup_var]])
    
    # Bin continuous subgroup variables at specified percentile
    if (was_continuous) {
        # Check for sufficient variation
        unique_vals <- unique(data[[subgroup_var]])
        if (length(unique_vals) < 2) {
            warning(sprintf("Variable '%s' has insufficient variation (only %d unique values)", 
                           subgroup_var, length(unique_vals)))
            return(list(interaction_p = NA, subgroup_effects = data.frame(), 
                       model = NULL, subgroup_var_used = NA, formula_used = NA, confounders_used = confounders_to_use))
        }
        
        # Calculate the percentile cutoff
        cutoff_val <- quantile(data[[subgroup_var]], probs = percentile_cut, na.rm = TRUE)
        
        # Create binned variable with descriptive labels
        subgroup_var_binned <- paste0(subgroup_var, "_binned")
        data[[subgroup_var_binned]] <- factor(
            ifelse(data[[subgroup_var]] < cutoff_val,
                   paste0("< ", round(cutoff_val, 1)),
                   paste0("≥ ", round(cutoff_val, 1))),
            levels = c(paste0("< ", round(cutoff_val, 1)), 
                      paste0("≥ ", round(cutoff_val, 1)))
        )
        subgroup_var_to_use <- subgroup_var_binned
    } else {
        # For categorical variables, use your existing function to handle rare categories
        if (!is.factor(data[[subgroup_var]])) {
            data[[subgroup_var]] <- as.factor(data[[subgroup_var]])
        }
        
        # Convert ordered factors to regular factors to get proper dummy variable contrasts
        if (is.ordered(data[[subgroup_var]])) {
            data[[subgroup_var]] <- factor(data[[subgroup_var]], ordered = FALSE)
        }
        
        # Use your existing handle_rare_categories function
        data <- handle_rare_categories(data, vars = subgroup_var, threshold = THRESHOLD_RARITY)
        
        # Ensure treatment contrasts for clearer coefficient names
        contrasts(data[[subgroup_var]]) <- contr.treatment(nlevels(data[[subgroup_var]]))
        
        subgroup_var_to_use <- subgroup_var
    }
    
    # Check if the binned/categorical variable has at least 2 levels
    subgroup_levels <- levels(data[[subgroup_var_to_use]])
    if (length(subgroup_levels) < 2) {
        warning(sprintf("Subgroup variable '%s' has only %d level(s) after processing", 
                       subgroup_var_to_use, length(subgroup_levels)))
        return(list(interaction_p = NA, subgroup_effects = data.frame(), 
                   model = NULL, subgroup_var_used = subgroup_var_to_use, formula_used = NA, confounders_used = confounders_to_use))
    }
    
    # Check that each level has both treatment groups
    for (level in subgroup_levels) {
        level_data <- data[data[[subgroup_var_to_use]] == level, ]
        treatment_groups <- unique(level_data$treatment_group)
        if (length(treatment_groups) < 2) {
            warning(sprintf("Subgroup level '%s' has only %d treatment group(s): %s", 
                           level, length(treatment_groups), paste(treatment_groups, collapse = ", ")))
        }
    }
    
    # Try to fit the linear model
    tryCatch({
        # Build model formula with interaction term and confounders
        if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
            formula_str <- paste0("height_change ~ treatment_group * ", subgroup_var_to_use)
        } else {
            formula_str <- paste0("height_change ~ treatment_group * ", subgroup_var_to_use, " + ", paste(confounders_to_use, collapse = " + "))
        }
        
        model_formula <- as.formula(formula_str)
        model <- lm(model_formula, data = data)
        
        # DEBUGGING: Print model summary and coefficient names
        if (VERBOSE) {
            log_message(sprintf("Model formula: %s", formula_str))
            log_message("Available coefficients:")
            print(names(coef(model)))
            log_message("Model summary:")
            print(summary(model))
        }
        
        # Extract interaction p-value (test overall interaction significance)
        model_summary <- summary(model)
        
        # For multiple levels, we need to test the overall interaction significance
        if (length(subgroup_levels) == 2) {
            # Simple case: just one interaction term
            # Get the non-reference level for treatment_group
            treatment_levels <- levels(data$treatment_group)
            treatment_nonref <- treatment_levels[treatment_levels != treatment_levels[1]][1]  # Get first non-reference level
            
            # Try to find the interaction term with more flexible naming
            possible_interaction_terms <- c(
                paste0("treatment_group", treatment_nonref, ":", subgroup_var_to_use, subgroup_levels[2]),
                paste0("treatment_group", treatment_nonref, ":", subgroup_var_to_use, "2"),  # R sometimes uses numeric suffixes
                paste0("treatment_group", treatment_nonref, ":", subgroup_var_to_use, "Yes"),  # For Yes/No factors
                paste0("treatment_group", treatment_nonref, ":", subgroup_var_to_use, "No")    # For Yes/No factors
            )
            
            interaction_term <- NULL
            for (term in possible_interaction_terms) {
                if (term %in% rownames(model_summary$coefficients)) {
                    interaction_term <- term
                    break
                }
            }
            
            if (VERBOSE) {
                log_message(sprintf("Looking for interaction terms: %s", paste(possible_interaction_terms, collapse = ", ")))
                if (!is.null(interaction_term)) {
                    log_message(sprintf("Found interaction term: %s", interaction_term))
                } else {
                    log_message("No interaction term found in model coefficients")
                }
            }
            
            if (!is.null(interaction_term)) {
                interaction_p <- model_summary$coefficients[interaction_term, "Pr(>|t|)"]
            } else {
                interaction_p <- NA
            }
        } else {
            # Multiple levels: use F-test for overall interaction significance
            # This requires comparing models with and without interaction
            if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
                formula_no_int <- paste0("height_change ~ treatment_group + ", subgroup_var_to_use)
            } else {
                formula_no_int <- paste0("height_change ~ treatment_group + ", subgroup_var_to_use, " + ", paste(confounders_to_use, collapse = " + "))
            }
            model_no_interaction <- lm(as.formula(formula_no_int), data = data)
            interaction_test <- anova(model_no_interaction, model)
            interaction_p <- interaction_test$`Pr(>F)`[2]
        }
        
        # Calculate treatment effects in each subgroup more clearly
        subgroup_effects <- data.frame()
        
        for (i in seq_along(subgroup_levels)) {
            level <- subgroup_levels[i]
            
            if (VERBOSE) {
                log_message(sprintf("Processing subgroup level %d: %s", i, level))
            }
            
            # Get detailed sample sizes for this subgroup level
            level_data <- data[data[[subgroup_var_to_use]] == level, ]
            n_total <- nrow(level_data)
            n_plaque <- sum(level_data$treatment_group == "Plaque", na.rm = TRUE)
            n_gksrs <- sum(level_data$treatment_group == "GKSRS", na.rm = TRUE)
            
            if (VERBOSE) {
                log_message(sprintf("  Sample sizes - Total: %d, Plaque: %d, GKSRS: %d", n_total, n_plaque, n_gksrs))
            }
            
            if (i == 1) {
                # Reference subgroup: treatment effect = β1 (main effect of treatment)
                # Get the non-reference level for treatment_group
                treatment_levels <- levels(data$treatment_group)
                treatment_nonref <- treatment_levels[treatment_levels != treatment_levels[1]][1]
                coef_idx <- paste0("treatment_group", treatment_nonref)
                
                if (VERBOSE) {
                    log_message(sprintf("  Reference level - Looking for coefficient: %s", coef_idx))
                }
                
                if (coef_idx %in% names(coef(model))) {
                    treatment_effect <- coef(model)[coef_idx]
                    se_effect <- sqrt(vcov(model)[coef_idx, coef_idx])
                    ci_lower <- treatment_effect - 1.96 * se_effect
                    ci_upper <- treatment_effect + 1.96 * se_effect
                    p_val <- model_summary$coefficients[coef_idx, "Pr(>|t|)"]
                    if (VERBOSE) {
                        log_message(sprintf("  Found coefficient - Effect: %.4f, SE: %.4f, p: %.4f", treatment_effect, se_effect, p_val))
                    }
                } else {
                    if (VERBOSE) {
                        log_message(sprintf("  Coefficient '%s' not found", coef_idx))
                    }
                    treatment_effect <- NA; ci_lower <- NA; ci_upper <- NA; p_val <- NA
                }
            } else {
                # Non-reference subgroup: treatment effect = β1 + β3 (main + interaction)
                # Get the non-reference level for treatment_group
                treatment_levels <- levels(data$treatment_group)
                treatment_nonref <- treatment_levels[treatment_levels != treatment_levels[1]][1]
                main_coef_idx <- paste0("treatment_group", treatment_nonref)
                
                # Try multiple possible interaction coefficient names
                possible_interaction_coeffs <- c(
                    paste0("treatment_group", treatment_nonref, ":", subgroup_var_to_use, level),
                    paste0("treatment_group", treatment_nonref, ":", subgroup_var_to_use, i),  # Use level index
                    paste0("treatment_group", treatment_nonref, ":", subgroup_var_to_use, "2"),  # R sometimes uses "2" for second level
                    paste0("treatment_group", treatment_nonref, ":", subgroup_var_to_use, "Yes"),  # For Yes/No variables
                    paste0("treatment_group", treatment_nonref, ":", subgroup_var_to_use, "No")    # For Yes/No variables
                )
                
                interaction_coef_idx <- NULL
                for (coef_name in possible_interaction_coeffs) {
                    if (coef_name %in% names(coef(model))) {
                        interaction_coef_idx <- coef_name
                        break
                    }
                }
                
                if (VERBOSE) {
                    log_message(sprintf("  Non-reference level - Looking for coefficients:"))
                    log_message(sprintf("    Main: %s", main_coef_idx))
                    log_message(sprintf("    Interaction candidates: %s", paste(possible_interaction_coeffs, collapse = ", ")))
                    if (!is.null(interaction_coef_idx)) {
                        log_message(sprintf("    Found interaction: %s", interaction_coef_idx))
                    } else {
                        log_message("    No interaction coefficient found")
                    }
                }
                
                if (main_coef_idx %in% names(coef(model)) && !is.null(interaction_coef_idx)) {
                    # Combined effect
                    treatment_effect <- coef(model)[main_coef_idx] + coef(model)[interaction_coef_idx]
                    
                    # Standard error using variance-covariance matrix
                    var_main <- vcov(model)[main_coef_idx, main_coef_idx]
                    var_int <- vcov(model)[interaction_coef_idx, interaction_coef_idx]
                    cov_main_int <- vcov(model)[main_coef_idx, interaction_coef_idx]
                    se_effect <- sqrt(var_main + var_int + 2 * cov_main_int)
                    
                    ci_lower <- treatment_effect - 1.96 * se_effect
                    ci_upper <- treatment_effect + 1.96 * se_effect
                    
                    # P-value for combined effect (Wald test)
                    t_stat <- treatment_effect / se_effect
                    df <- model$df.residual
                    p_val <- 2 * (1 - pt(abs(t_stat), df))
                    
                    if (VERBOSE) {
                        log_message(sprintf("  Found both coefficients - Combined effect: %.4f, SE: %.4f, p: %.4f", treatment_effect, se_effect, p_val))
                    }
                } else {
                    if (VERBOSE) {
                        log_message(sprintf("  Missing coefficients - Main found: %s, Interaction found: %s", 
                                          main_coef_idx %in% names(coef(model)),
                                          !is.null(interaction_coef_idx)))
                    }
                    treatment_effect <- NA; ci_lower <- NA; ci_upper <- NA; p_val <- NA
                }
            }
            
            # Add to results with clear naming and detailed sample sizes
            subgroup_effects <- rbind(subgroup_effects, data.frame(
                subgroup_variable = subgroup_var,
                subgroup_level = level,
                n_total = n_total,
                n_plaque = n_plaque,
                n_gksrs = n_gksrs,
                treatment_effect = treatment_effect,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                p_value = p_val,
                stringsAsFactors = FALSE
            ))
        }
        
        # Return organized results
        return(list(
            interaction_p = interaction_p,
            subgroup_effects = subgroup_effects,
            model = model,
            subgroup_var_used = subgroup_var_to_use,
            formula_used = formula_str,
            confounders_used = confounders_to_use
        ))
        
    }, error = function(e) {
        warning(sprintf("Error fitting model for '%s': %s", subgroup_var, e$message))
        return(list(interaction_p = NA, subgroup_effects = data.frame(), 
                   model = NULL, subgroup_var_used = subgroup_var_to_use, 
                   formula_used = formula_str, confounders_used = confounders_to_use))
    })
}

#' Create organized output directory structure
#'
#' Creates a well-organized directory structure for analysis outputs
#'
#' @param base_dir Base directory for outputs (e.g., tables_dir or figures_dir)
#' @return List of created directory paths
#' @examples
#' create_output_structure("/path/to/tables")
create_output_structure <- function(base_dir) {
    # Define the directory structure
    dirs <- list(
        recurrence = file.path(base_dir, "primary_outcomes", "recurrence"),
        mets = file.path(base_dir, "primary_outcomes", "metastatic_progression"),
        os = file.path(base_dir, "primary_outcomes", "overall_survival"),
        pfs = file.path(base_dir, "primary_outcomes", "progression_free_survival"),
        height_primary = file.path(base_dir, "primary_outcomes", "tumor_height_change", "primary_analysis"),
        height_sensitivity = file.path(base_dir, "primary_outcomes", "tumor_height_change", "sensitivity_analysis"),
        subgroup_primary = file.path(base_dir, "primary_outcomes", "tumor_height_change", "subgroup_interactions", "without_baseline_height"),
        subgroup_sensitivity = file.path(base_dir, "primary_outcomes", "tumor_height_change", "subgroup_interactions", "with_baseline_height"),
        # Step 2: Safety/Toxicity outcomes
        vision = file.path(base_dir, "safety_toxicity", "vision_change"),
        retinopathy = file.path(base_dir, "safety_toxicity", "radiation_sequelae", "retinopathy"),
        nvg = file.path(base_dir, "safety_toxicity", "radiation_sequelae", "nvg"),
        srg = file.path(base_dir, "safety_toxicity", "radiation_sequelae", "srg"),
        treatment_duration = file.path(base_dir, "treatment_duration")
    )
    
    # Create all directories
    for (dir_path in dirs) {
        if (!dir.exists(dir_path)) {
            dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
            log_message(sprintf("Created directory: %s", dir_path))
        }
    }
    
    return(dirs)
}

#' Analyze vision changes
#'
#' Calculates and summarizes changes in vision by treatment group, returning summary statistics and a table.
#' Simple analysis as required by objective 2a - no subgroup interactions needed.
#'
#' @param data Data frame with vision variables.
#'
#' @return List with elements: changes (summary data frame), table (gtsummary object), regression_model (lm object), regression_table (gtsummary object).
#' @examples
#' analyze_vision_changes(data)
analyze_vision_changes <- function(data) {
    # Calculate vision changes (row-level)
    data_with_vision_change <- data %>%
        mutate(
            # Calculate vision change as the difference between the initial 
            # vision and the vision at the time of recurrence *or* last follow-up
            # Post treatment1 vision = recurrence1 pretreatment vision
            vision_change = case_when(
                recurrence1 == "Y" ~ initial_vision - recurrence1_pretreatment_vision,
                TRUE ~ initial_vision - last_vision
            )
        )

    # Summary statistics (grouped)
    vision_changes <- data_with_vision_change %>%
        group_by(treatment_group) %>%
        summarise(
            n = n(),
            mean_change = mean(vision_change, na.rm = TRUE),
            sd_change = sd(vision_change, na.rm = TRUE),
            median_change = median(vision_change, na.rm = TRUE),
            iqr_change = IQR(vision_change, na.rm = TRUE),
            .groups = "drop"
        )
    
    # Statistical test
    wilcox.test(vision_change ~ treatment_group, data = data_with_vision_change)

    # Table for publication (row-level input)
    tbl <- data_with_vision_change %>%
        select(treatment_group, vision_change) %>%
        tbl_summary(
            missing = "no",
            by = treatment_group,
            type = list(vision_change ~ "continuous"),
            statistic = list(vision_change ~ "{mean} ({sd})"),
            digits = list(vision_change ~ 2),
        ) %>%
        add_p(test = list(all_continuous() ~ "wilcox.test")) %>%
        modify_header(quiet = TRUE) %>%
        modify_caption("Change in Vision (Initial - Last Measured or Pre-Retreatment), by Treatment Group") %>%
        modify_footnote(
            update = all_stat_cols() ~ "Mean (SD)"
        ) %>%
        as_gt()
    
    # Save table
    tbl %>%
        gt::gtsave(
            filename = file.path(output_dirs$vision, paste0(prefix, "vision_changes.html"))
        )
    
    # Linear regression model
    log_message("Fitting linear regression model for vision changes")
    vision_lm <- lm(vision_change ~ treatment_group + recurrence1, data = data_with_vision_change)
    
    vision_lm_tbl <- tbl_regression(vision_lm,
        exponentiate = FALSE,
        intercept = FALSE
    ) %>%
        modify_header(
            label = "**Characteristic**",
            estimate = "**Beta**",
            ci = "**95% CI**",
            p.value = "**p-value**",
            quiet = TRUE
        ) %>%
        modify_caption("Linear Regression of Change in Vision") %>%
        modify_footnote(
            update = all_stat_cols() ~ "Adjusted for treatment group and recurrence status. Reference level: Plaque."
        ) %>%
        as_gt()
    
    # Save regression table
    vision_lm_tbl %>%
        gt::gtsave(
            filename = file.path(output_dirs$vision, paste0(prefix, "vision_regression.html"))
        )

    return(list(
        changes = vision_changes,
        table = tbl,
        regression_model = vision_lm,
        regression_table = vision_lm_tbl
    ))
}


#' Analyze radiation sequelae outcomes
#'
#' Analyzes rates of radiation sequelae (retinopathy, nvg, srg) by treatment group.
#' Reuses the existing calculate_rates function for consistency.
#'
#' @param data Data frame with radiation sequelae variables
#' @param sequela_type Type of sequela to analyze ("retinopathy", "nvg", or "srg")
#' @param confounders Character vector of confounders for adjustment
#' @param dataset_name Name of the dataset for output files
#'
#' @return Results from calculate_rates function
#' @examples
#' analyze_radiation_sequelae(data, "retinopathy", confounders, "uveal_full")
analyze_radiation_sequelae <- function(data, sequela_type, confounders, dataset_name) {
    
    # Validate sequela type
    valid_sequelae <- c("retinopathy", "nvg", "srd")
    if (!sequela_type %in% valid_sequelae) {
        stop(sprintf("Invalid sequela_type '%s'. Must be one of: %s", 
                     sequela_type, paste(valid_sequelae, collapse = ", ")))
    }
    
    # For SRD, filter to only radiation-induced cases as per objectives
    if (sequela_type == "srd") {
        log_message("Filtering SRD to only radiation-induced causes")
        original_n <- nrow(data)
        # Check what values exist in srd_cause
        if ("srd_cause" %in% names(data)) {
            log_message("Available srd_cause values:")
            print(table(data$srd_cause, useNA = "ifany"))
        }
        
        # Filter for radiation-induced SRD analysis: exclude patients with mass-induced SRD
        data <- data %>%
            filter(
                # Keep patients without SRD
                srd == "N" | is.na(srd) |
                # Keep patients with radiation-induced SRD (exclude mass-induced)
                (srd == "Y" & srd_cause == "Radiation")
            )
        log_message(sprintf("Data filtered for radiation-induced SRD: %d -> %d patients", original_n, nrow(data)))
    }
    
    # Check if outcome variable exists
    outcome_var <- sequela_type
    if (!outcome_var %in% names(data)) {
        stop(sprintf("Missing required variable for %s analysis: %s", 
                     sequela_type, outcome_var))
    }
    
    log_message(sprintf("Analyzing %s rates (binary outcome)", toupper(sequela_type)))
    
    # Convert to binary if needed and ensure it's a factor
    data <- data %>%
        mutate(
            !!outcome_var := case_when(
                .data[[outcome_var]] == "Y" ~ "Y",
                .data[[outcome_var]] == "N" ~ "N",
                is.na(.data[[outcome_var]]) ~ "N",
                TRUE ~ "N"
            )
        ) %>%
        mutate(!!outcome_var := factor(.data[[outcome_var]], levels = c("Y", "N")))
    
    # Calculate rates by treatment group
    sequela_rates <- data %>%
        group_by(treatment_group) %>%
        summarise(
            n_total = n(),
            n_events = sum(.data[[outcome_var]] == "Y", na.rm = TRUE),
            rate_percent = round(100 * n_events / n_total, 1),
            .groups = "drop"
        )
    
    # Determine output directory
    output_dir <- switch(sequela_type,
                        "retinopathy" = output_dirs$retinopathy,
                        "nvg" = output_dirs$nvg,
                        "srd" = output_dirs$srg,  # Note: objectives mention "srg" but data has "srd"
                        file.path(tables_dir, "safety_toxicity", "radiation_sequelae"))  # fallback
    
    # Save rates summary
    writexl::write_xlsx(sequela_rates, 
                        file.path(output_dir, paste0(prefix, sequela_type, "_rates_summary.xlsx")))
    
    # Create summary table
    tbl <- data %>%
        select(treatment_group, all_of(outcome_var)) %>%
        tbl_summary(
            by = treatment_group,
            missing = "no",
            type = list(!!outcome_var ~ "categorical"),
            label = list(!!outcome_var ~ paste("Radiation Sequela:", tools::toTitleCase(sequela_type)))
        ) %>%
        modify_header(quiet = TRUE) %>%
        add_p() %>%  # Use gtsummary default test selection
        modify_caption(paste("Rates of", tools::toTitleCase(sequela_type), "by Treatment Group")) %>%
        as_gt()
    
    # Save summary table
    tbl %>%
        gt::gtsave(filename = file.path(output_dir, paste0(prefix, sequela_type, "_summary_table.html")))
    
    # Fit logistic regression if there are enough events and confounders
    model_result <- NULL
    if (sum(data[[outcome_var]] == "Y", na.rm = TRUE) >= 10) {  # Require at least 10 events
        
        # Validate confounders
        valid_confounders <- NULL
        if (!is.null(confounders) && length(confounders) > 0) {
            valid_confounders <- generate_valid_confounders(data, confounders, threshold = THRESHOLD_RARITY)
        }
        
        # Fit logistic regression
        if (is.null(valid_confounders) || length(valid_confounders) == 0) {
            formula_str <- paste0(outcome_var, " ~ treatment_group")
        } else {
            formula_str <- paste0(outcome_var, " ~ treatment_group + ", paste(valid_confounders, collapse = " + "))
        }
        
        model <- glm(as.formula(formula_str), data = data, family = binomial())
        
        model_result <- tbl_regression(model,
            exponentiate = TRUE,
            intercept = FALSE
        ) %>%
            modify_header(
                label = "**Characteristic**",
                estimate = "**OR**",
                ci = "**95% CI**",
                p.value = "**p-value**",
                quiet = TRUE
            ) %>%
            modify_caption(paste("Logistic Regression for", tools::toTitleCase(sequela_type))) %>%
            modify_footnote(
                update = all_stat_cols() ~ "OR = Odds Ratio. Reference level: Plaque."
            ) %>%
            as_gt()
        
        # Save regression table
        model_result %>%
            gt::gtsave(filename = file.path(output_dir, paste0(prefix, sequela_type, "_logistic_regression.html")))
        
    } else {
        log_message(sprintf("Insufficient events for regression modeling (%d events)", sum(data[[outcome_var]] == "Y", na.rm = TRUE)))
    }
    
    return(list(
        rates = sequela_rates,
        table = tbl,
        model = model_result
    ))
}
