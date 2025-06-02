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
# install.packages(c("tidyverse", "readxl", "writexl", "gtsummary", "survival", "survminer", "gt", "forestploter", "grid", "cowplot"))

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

    # Save high-level summary table of event rates
    writexl::write_xlsx(
        rates,
        path = file.path(tables_dir, paste0(prefix, outcome_var, "_rates_summary.xlsx"))
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
        show_single_row = group_var
    ) %>%
        # could remove this to show all p-values
        add_global_p() %>% 
        # change the column names
        modify_header(
            label     = "**Variable**",
            estimate  = "**OR**",
            p.value   = "**p-value**"
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
            filename = file.path(tables_dir, paste0(prefix, outcome_var, "_rates.html"))
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

    surv_plot <- survminer::ggsurvplot(
        fit = surv_fit,
        data = new_data,
        risk.table = TRUE,
        conf.int = TRUE,
        pval = TRUE,
        xlab = "Time (months)",
        ylab = ylab,
        risk.table.height = 0.25,
        ggtheme = theme_bw()
    )

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

    # Wide format for easier viewing
    surv_rates_wide <- surv_rates %>%
        mutate(Time_Label = paste0(Time_Years, "-year")) %>%
        select(Treatment_Group, Time_Label, surv_pct) %>%
        tidyr::pivot_wider(
            names_from = Time_Label,
            values_from = surv_pct,
        )

    # Save high-level summary table of median survival times
    writexl::write_xlsx(
        surv_rates,
        path = file.path(tables_dir, paste0(prefix, ylab, "_survival_rates.xlsx"))
    )
    writexl::write_xlsx(
        surv_rates_wide,
        path = file.path(tables_dir, paste0(prefix, ylab, "_survival_rates_wide.xlsx"))
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
            p.value  = "**p-value**"
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
            filename = file.path(tables_dir, paste0(prefix, ylab, "_cox.html"))
        )

    combined <- plot_grid(
        surv_plot$plot,
        surv_plot$table,
        ncol    = 1,
        rel_heights = c(3,1)
    )

    ggsave(
        file.path(figures_dir, paste0(prefix, ylab, "_survival.png")),
        combined,
        width = 10, height = 8, dpi = 300
    )
    
    return(list(
        fit = surv_fit,
        plot = surv_plot,
        survival_rates = surv_rates,
        survival_rates_wide = surv_rates_wide,
        cox_model = cox_model,
        cox_table = cox_table
    ))
}

#' Analyze tumor height changes
#'
#' Calculates and summarizes changes in tumor height by treatment group, returning summary statistics and a table.
#'
#' @param data Data frame with tumor height variables.
#'
#' @return List with elements: changes (summary data frame), table (gtsummary object), regression_model (lm object), regression_table (gtsummary object).
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
        add_p() %>%
        modify_caption("Change in Tumor Height (Initial - Last Measured or Pre-Retreatment), by Treatment Group") %>%
        modify_footnote(
            update = all_stat_cols() ~ "Mean (SD)"
        ) %>%
        as_gt()
    
    # Save table
    tbl %>%
        gt::gtsave(
            filename = file.path(tables_dir, paste0(prefix, "height_changes.html"))
        )
    
    # Linear regression: adjust for treatment group, recurrence, and INITIAL tumor height
    log_message("Fitting linear regression model for tumor height changes")
    height_lm <- lm(height_change ~ treatment_group + recurrence1 + initial_tumor_height, data = data_with_height_change)
    height_lm_tbl <- tbl_regression(height_lm,
        exponentiate = FALSE,
        intercept = FALSE
    ) %>%
        modify_caption("Linear Regression of Change in Tumor Height (Initial - Last Measured or Pre-Retreatment), Adjusted for Initial Height") %>%
        modify_footnote(
            update = all_stat_cols() ~ "Adjusted for treatment group, recurrence status, and initial tumor height. Reference level: Plaque"
        ) %>%
        as_gt()
    
    # Save table
    height_lm_tbl %>%
        gt::gtsave(
            filename = file.path(tables_dir, paste0(prefix, "height_lm.html"))
        )

    return(list(
        changes = height_changes,
        table = tbl,
        regression_model = height_lm,
        regression_table = height_lm_tbl
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
create_subgroup_tables <- function(subgroup_results, dataset_name, output_dir, prefix) {
    
    # Create subgroup analysis subfolder
    subgroup_dir <- file.path(output_dir, "subgroup_analysis")
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
                        "**Treatment Effect:** GKSRS vs Plaque (reference). Positive values indicate greater height reduction with GKSRS.\n\n%s\n\n**Model:** %s\n\n**Dataset:** %s",
                        significance_text,
                        ifelse(is.null(result$formula_used), "Model formula not available", result$formula_used),
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
#' Now also creates formatted HTML tables in organized subfolder structure.
#'
#' @param data Data frame containing the analysis variables
#' @param subgroup_var Name of the subgroup variable (character)
#' @param percentile_cut Percentile for binning continuous variables (default: 0.5 for median split)
#' @param confounders Character vector of confounders to adjust for
#' @param create_tables Logical, whether to create formatted HTML tables (default: FALSE for individual calls)
#'
#' @return List containing:
#'   - interaction_p: P-value for the interaction term
#'   - subgroup_effects: Data frame with treatment effects in each subgroup
#'   - model: The fitted linear model object
#'   - subgroup_var_used: Name of the binned variable created
#'   - formula_used: The formula used for the model
#'
#' @examples
#' test_subgroup_interaction(data, "age_at_diagnosis")
#' test_subgroup_interaction(data, "initial_tumor_height", percentile_cut = 0.75)
test_subgroup_interaction <- function(data, subgroup_var, percentile_cut = 0.5, confounders = NULL, create_tables = FALSE) {
    
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
                   model = NULL, subgroup_var_used = NA, formula_used = NA))
    }
    
    # Remove rows with missing subgroup variable
    data <- data %>% filter(!is.na(.data[[subgroup_var]]))
    
    if (nrow(data) == 0) {
        warning(sprintf("No data remaining after removing missing values for '%s'", subgroup_var))
        return(list(interaction_p = NA, subgroup_effects = data.frame(), 
                   model = NULL, subgroup_var_used = NA, formula_used = NA))
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
                       model = NULL, subgroup_var_used = NA, formula_used = NA))
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
                   model = NULL, subgroup_var_used = subgroup_var_to_use, formula_used = NA))
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
        # Build model formula with interaction term
        formula_str <- paste0("height_change ~ treatment_group * ", subgroup_var_to_use)
        model_formula <- as.formula(formula_str)
        model <- lm(model_formula, data = data)
        
        # Extract interaction p-value (test overall interaction significance)
        model_summary <- summary(model)
        
        # For multiple levels, we need to test the overall interaction significance
        if (length(subgroup_levels) == 2) {
            # Simple case: just one interaction term
            interaction_term <- paste0("treatment_groupGKSRS:", subgroup_var_to_use, subgroup_levels[2])
            if (interaction_term %in% rownames(model_summary$coefficients)) {
                interaction_p <- model_summary$coefficients[interaction_term, "Pr(>|t|)"]
            } else {
                interaction_p <- NA
            }
        } else {
            # Multiple levels: use F-test for overall interaction significance
            # This requires comparing models with and without interaction
            model_no_interaction <- lm(as.formula(paste0("height_change ~ treatment_group + ", subgroup_var_to_use)), data = data)
            interaction_test <- anova(model_no_interaction, model)
            interaction_p <- interaction_test$`Pr(>F)`[2]
        }
        
        # Calculate treatment effects in each subgroup more clearly
        subgroup_effects <- data.frame()
        
        for (i in seq_along(subgroup_levels)) {
            level <- subgroup_levels[i]
            
            # Get detailed sample sizes for this subgroup level
            level_data <- data[data[[subgroup_var_to_use]] == level, ]
            n_total <- nrow(level_data)
            n_plaque <- sum(level_data$treatment_group == "Plaque", na.rm = TRUE)
            n_gksrs <- sum(level_data$treatment_group == "GKSRS", na.rm = TRUE)
            
            if (i == 1) {
                # Reference subgroup: treatment effect = β1 (main effect of treatment)
                coef_idx <- "treatment_groupGKSRS"
                if (coef_idx %in% names(coef(model))) {
                    treatment_effect <- coef(model)[coef_idx]
                    se_effect <- sqrt(vcov(model)[coef_idx, coef_idx])
                    ci_lower <- treatment_effect - 1.96 * se_effect
                    ci_upper <- treatment_effect + 1.96 * se_effect
                    p_val <- model_summary$coefficients[coef_idx, "Pr(>|t|)"]
                } else {
                    treatment_effect <- NA; ci_lower <- NA; ci_upper <- NA; p_val <- NA
                }
            } else {
                # Non-reference subgroup: treatment effect = β1 + β3 (main + interaction)
                main_coef_idx <- "treatment_groupGKSRS"
                
                # Use different coefficient naming based on variable type
                if (was_continuous) {
                    # For continuous variables that were binned, use level names
                    interaction_coef_idx <- paste0("treatment_groupGKSRS:", subgroup_var_to_use, level)
                } else {
                    # For categorical variables with treatment contrasts, use level indices
                    interaction_coef_idx <- paste0("treatment_groupGKSRS:", subgroup_var_to_use, i)
                }
                
                if (main_coef_idx %in% names(coef(model)) && interaction_coef_idx %in% names(coef(model))) {
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
                } else {
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
            formula_used = formula_str
        ))
        
    }, error = function(e) {
        warning(sprintf("Error fitting model for '%s': %s", subgroup_var, e$message))
        return(list(interaction_p = NA, subgroup_effects = data.frame(), 
                   model = NULL, subgroup_var_used = subgroup_var_to_use, 
                   formula_used = formula_str))
    })
}
