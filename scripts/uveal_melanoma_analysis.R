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

    # Generate valid confounders
    valid_confounders <- generate_valid_confounders(fix_event_data, confounders, threshold = THRESHOLD_RARITY)

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
    if (length(valid_confounders) == 0) {
        formula_str <- paste0(outcome_var, " ~ ", group_var)
    } else {
        formula_str <- paste0(outcome_var, " ~ ", group_var, " + ", paste(valid_confounders, collapse = " + "))
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

    # Generate valid confounders
    valid_confounders <- generate_valid_confounders(fix_event_data, confounders, threshold = THRESHOLD_RARITY)

    # Create formula for survival analysis
    surv_formula <- as.formula(
        paste0("Surv(", time_var, ", ", event_var, ") ~ ", group_var)
    )
    
    # Select only the variables needed for the formula
    new_data <- fix_event_data %>%
        dplyr::select(all_of(c(time_var, event_var, group_var, valid_confounders))) 

    # Use formula with column names as strings, not symbols or objects
    surv_fit <- survival::survfit(surv_formula, data = new_data)
    surv_fit$call$formula <- surv_formula # This is necessary for non-standard evaluation

    surv_plot <- survminer::ggsurvplot(
        fit = surv_fit,
        data = new_data,
        risk.table = TRUE,
        conf.int = TRUE,
        pval = TRUE,
        xlab = "Time (days)",
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

    # Define time points in days (e.g., 1, 3, 5 years)
    time_points <- c(1, 3, 5, 10, 15) * DAYS_IN_YEAR

    # Get survival probabilities at those time points
    surv_summary <- summary(surv_fit, times = time_points)

    # Build a data frame directly from the summary output
    surv_rates <- as.data.frame(surv_summary[c("strata", "time", "surv", "lower", "upper")]) %>%
        mutate(
            Treatment_Group = sub(".*=", "", strata),
            Time_Years = round(time / DAYS_IN_YEAR, 3)
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
    if (is.null(valid_confounders)) {
        formula_cox <- as.formula(paste0("Surv(", time_var, ",", event_var, ") ~ ", group_var))
        formula_str <- paste0("Surv(", time_var, ",", event_var, ") ~ ", group_var)
    } else {
        formula_cox <- as.formula(paste0("Surv(", time_var, ",", event_var, ") ~ ", group_var, " + ", paste(valid_confounders, collapse = " + ")))
        formula_str <- paste0("Surv(", time_var, ",", event_var, ") ~ ", group_var, " + ", paste(valid_confounders, collapse = " + "))
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
    
    # Linear regression: adjust for treatment group and recurrence
    height_lm <- lm(height_change ~ treatment_group + recurrence1 + initial_tumor_height, data = data_with_height_change)
    height_lm_tbl <- tbl_regression(height_lm,
        exponentiate = FALSE,
        intercept = FALSE
    ) %>%
        modify_caption("Linear Regression of Change in Tumor Height (Initial - Last Measured or Pre-Retreatment)") %>%
        modify_footnote(
            update = all_stat_cols() ~ "Reference level: Plaque"
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


#' Make a stratified table
#'
#' Builds a stratified table of a binary endpoint by treatment, stratified by a subgroup variable.
#'
#' @param data Data frame.
#' @param subgroup_var Subgroup variable vector.
#' @param efficacy_var Efficacy variable.
#' @param group_var Group variable.
#'
#' @return gtsummary object.
#' @examples
#' make_subgroup_tbl(data, "age_at_diagnosis", "recurrence1", "treatment_group")
make_subgroup_tbl <- function(data, subgroup_var, efficacy_var = "recurrence1", group_var = "treatment_group") {
    # DEBUGGING:
    # subgroup_var = "age_at_diagnosis"
    # efficacy_var = "recurrence1"
    # treat_var = "treatment_group"

    # if subgroup_var is numeric, bin it
    # binned_data <- data %>%
    #     mutate(
    #         !!subgroup_var := bin_continuous(!!sym(subgroup_var), bins = 3)
    #     )

    # build a tbl_summary of your binary endpoint by treatment
    # base_tbl <- binned_data %>%
    #     select(!!sym(subgroup_var), !!sym(treat_var), !!sym(efficacy_var)) %>%
    #     tbl_summary(
    #         by = treat_var,
    #         include = efficacy_var,
    #         statistic = list(all_of(efficacy_var) ~ "{n} / {N} ({p}%)"),
    #         missing = "no"
    #     ) %>%
    #     add_p() %>% # p-value for treatment effect
    #     modify_header(label = "**Level**") %>%
    #     modify_caption(
    #         paste0(
    #             "Efficacy (", efficacy_var, ") by ", treat_var,
    #             ", stratified by ", subgroup_var
    #         )
    #     )
    # base_tbl

    # subgroup_vars <- c(
    #     "age_at_diagnosis",
    #     "sex",
    #     "location",
    #     "initial_t_stage",
    #     "initial_tumor_height",
    #     "initial_tumor_diameter",
    #     "biopsy1_gep",
    #     "optic_nerve"
    # )

    # 2) define a small function that builds exactly the tbl_summary you want
    # base_rec_table <- function(df) {
    #     df %>%
    #         tbl_summary(
    #             by        = "treatment_group",
    #             include   = "recurrence1",
    #             missing   = "no",
    #             # statistic = all_categorical() ~ "{n} / {N} ({p}%)",
    #             label     = recurrence1 ~ "" # drop the top‐level "recurrence1" label
    #         ) %>%
    #         add_n() %>%
    #         add_p(test = everything() ~ "fisher.test") %>%
    #         modify_header(
    #             # label   = "", # no "Level" column header
    #             stat_1  = "**Plaque n (%)**",
    #             stat_2  = "**GKSRS n (%)**",
    #             p.value = "**p-value**"
    #         )
    # }

    # # 3) call tbl_strata() properly
    # tbl_strata(
    #     data = data, # your full data frame
    #     strata = subgroup_var, # the column you want to split on
    #     .tbl_fun = base_rec_table,
    #     .combine_with = "tbl_stack", # stack each panel vertically
    #      .header = sprintf("**%s: {strata} (N={n})**", subgroup_var)
    # ) %>%
    #     modify_caption(
    #         sprintf("**Subgroup Efficacy:** %s by Treatment Group", efficacy_var)
    #     )
    data$age_at_diagnosis %>% summary()


    data %>%
        select(
            age_at_diagnosis, sex, location, initial_t_stage,
            # initial_tumor_height, initial_tumor_diameter, biopsy1_gep,
            # optic_nerve, 
            recurrence1, 
            treatment_group
        ) %>%
        tbl_strata(
            strata = initial_t_stage,
            .tbl_fun = ~ .x %>%
                tbl_summary(
                    by = treatment_group,
                    missing = "no",
                    # rename your variables here, leave them numeric
                    label = list(
                        age_at_diagnosis ~ "Age at Diagnosis",
                        sex ~ "Sex",
                        location ~ "Location",
                        recurrence1 ~ "Recurrence"
                    ),
                    # force continuous vars to median (Q1, Q3)
                    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
                    # zero decimal places for age
                    digits = all_continuous() ~ 0
                ) %>%
                add_n() %>%
                add_p(
                    test = list(
                        all_continuous() ~ "wilcox.test",
                        all_categorical() ~ "fisher.test"
                    )
                ),
            .combine_with = "tbl_stack",
            .header = md("**{strata}**, N = {n}")
        ) %>%
        modify_caption("**Subgroup Efficacy:** Recurrence by Treatment Group")


    
    return(subgroup_efficacy_tbl)
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

#' Perform subgroup analysis with binning for continuous variables
#'
#' Runs survival analysis within subgroups defined by a variable, binning if continuous.
#'
#' @param data Data frame.
#' @param outcome_var Name of the outcome variable (character).
#' @param time_var Name of the time-to-event variable (character).
#' @param event_var Name of the event indicator variable (character).
#' @param subgroup_var Name of the subgroup variable (character).
#' @param bins Number of bins for continuous variables (default: 2).
#' @param custom_breaks Optional custom breakpoints.
#'
#' @return List of subgroup analysis results.
#' @examples
#' perform_subgroup_analysis(data, "recurrence1", "tt_recurrence", "recurrence_event", "age_at_diagnosis")
#' [DEPRECATED: use subgroup_effects instead]
perform_subgroup_analysis <- function(data, outcome_var, time_var, event_var, subgroup_var, bins = 2, custom_breaks = NULL) {
  # Bin if continuous
  # DEBUGGING:
#   subgroup_var = "age_at_diagnosis"
#   outcome_var = "recurrence1"
#   time_var = "tt_recurrence"
#   event_var = "recurrence_event"
#   bins = 2
#   custom_breaks = NULL

  # TODO: Check this function
  if (is.numeric(data[[subgroup_var]]) || is.integer(data[[subgroup_var]])) {
    binned <- bin_continuous(data[[subgroup_var]], bins = bins, custom_breaks = custom_breaks, varname = subgroup_var)
    data <- data %>% mutate(subgroup_binned = as.character(binned))
    group_var_to_use <- "subgroup_binned"
  } else {
    data <- data %>% mutate(subgroup_binned = as.character(!!sym(subgroup_var)))
    group_var_to_use <- "subgroup_binned"
  }

  # For each subgroup, run survival analysis (return NA for subgroups with <2 treatment groups)
  results <- data %>%
    group_by(.data[[group_var_to_use]]) %>%
    group_map(~{
      subgroup_label <- unique(.x[[group_var_to_use]])[1]
      if (nrow(.x) > 0 && length(unique(.x$treatment_group)) > 1 && !all(is.na(.x[[group_var_to_use]]))) {
        surv_formula <- as.formula(paste0("Surv(", time_var, ",", event_var, ") ~ treatment_group"))
        surv_fit <- survfit(surv_formula, data = .x)
        cox_model <- coxph(surv_formula, data = .x)
        hr <- summary(cox_model)$coefficients[1, "exp(coef)"]
        ci <- summary(cox_model)$conf.int[1, c("lower .95", "upper .95")]
        pval <- summary(cox_model)$coefficients[1, "Pr(>|z|)"]
        list(
          subgroup = subgroup_label,
          n = nrow(.x),
          hr = hr,
          ci_lower = ci[1],
          ci_upper = ci[2],
          pval = pval,
          fit = surv_fit
        )
      } else {
        # Return NA results for subgroups with <2 treatment groups
        list(
          subgroup = subgroup_label,
          n = nrow(.x),
          hr = NA,
          ci_lower = NA,
          ci_upper = NA,
          pval = NA,
          fit = NA
        )
      }
    })
  return(results)
}

#' Perform subgroup analysis with median split for continuous variables
#'
#' Runs survival analysis within subgroups defined by a variable, binning if continuous.
#'
#' @param data Data frame.
#' @param subgroup_var Name of the subgroup variable (character).
#' @param outcome_var Name of the outcome variable (character).
#' @param time_var Name of the time-to-event variable (character).
#' @param event_var Name of the event indicator variable (character).
subgroup_effects <- function(data, subgroup_var, outcome_var, time_var, event_var, group_var = "treatment_group") {
    # If the subgroup variable is continuous, bin at the median; otherwise, use as is
    if (is.numeric(data[[subgroup_var]]) || is.integer(data[[subgroup_var]])) {
        median_val <- round(median(data[[subgroup_var]], na.rm = TRUE), 2)
        # Create a binary subgroup: below median vs. median or above
        data <- data %>%
            mutate(subgroup = factor(
                ifelse(.data[[subgroup_var]] < median_val, 
                       paste0(subgroup_var, "<", median_val), 
                       paste0(subgroup_var, "≥", median_val)
                ),
                levels = c(paste0(subgroup_var, "<", median_val), 
                          paste0(subgroup_var, "≥", median_val))
            ))
    } else {
        # For categorical variables, preserve the original factor levels if they exist
        if (is.factor(data[[subgroup_var]])) {
            data <- data %>% mutate(subgroup = factor(.data[[subgroup_var]], levels = levels(.data[[subgroup_var]])))
        } else {
            # If not a factor, convert to factor maintaining unique values in order of appearance
            data <- data %>% mutate(subgroup = factor(.data[[subgroup_var]], levels = unique(.data[[subgroup_var]])))
        }
    }
    
    # Initialize a list to store results for each subgroup level
    results <- list()
    # Loop over each unique subgroup level in order of factor levels
    for (level in levels(data$subgroup)) {
        # Subset the data to the current subgroup level
        d <- data %>% filter(subgroup == level)
        # Only analyze if both treatment groups are present in this subgroup
        if (length(unique(d[[group_var]])) > 1) {
            # Fit a Cox proportional hazards model for the endpoint in this subgroup
            surv_formula <- as.formula(paste0("Surv(", time_var, ",", event_var, ") ~ ", group_var))
            cox <- coxph(surv_formula, data = d)
            # Extract hazard ratio, confidence interval, and p-value for treatment effect
            hr <- summary(cox)$coefficients[1, "exp(coef)"]
            ci <- summary(cox)$conf.int[1, c("lower .95", "upper .95")]
            pval <- summary(cox)$coefficients[1, "Pr(>|z|)"]
            # Store results for this subgroup
            results[[level]] <- list(
                subgroup = level,
                n = nrow(d),
                hr = hr,
                ci_lower = ci[1],
                ci_upper = ci[2],
                pval = pval
            )
        } else {
            # If only one treatment group is present, store NA for effect estimates
            results[[level]] <- list(
                subgroup = level,
                n = nrow(d),
                hr = NA,
                ci_lower = NA,
                ci_upper = NA,
                pval = NA
            )
        }
    }
    # Convert to tibble for easy plotting
    do.call(rbind, lapply(results, as_tibble))
}


#' Plot a forest plot for a single subgroup‐analysis list
#'
#' @param subgroup_list Named list of tibbles.
#'   Each tibble must have columns:
#'     "subgroup" (term), "hr" (estimate), "ci_lower", "ci_upper", "pval".
#' @param plot_name     Short name for this analysis (used in title & filename)
#' @param figures_dir   Directory in which to save the PDF
#' @param prefix        Filename prefix (e.g. cohort prefix)
#' @param xlab          X-axis label (default: Hazard Ratio)
#' @param title_ext     Suffix for the plot title (default: ": Subgroup Analysis")
#' @param base_size     Base font size (default: 14)
#'
#' @examples
#' plot_subgroup_forest(
#'     subgroup_list = subgroup_analyses_overall_survival,
#'     plot_name     = "overall_survival",
#'     figures_dir   = figures_dir,
#'     prefix        = prefix
#' )
plot_subgroup_forest <- function(subgroup_list, plot_name, figures_dir, prefix, xlab = "Hazard Ratio (GKSRS vs Plaque)", title_ext = ": Subgroup Analysis", base_size = 14) {
    # DEBUGGING:
    # subgroup_list = subgroup_analyses_overall_survival
    # plot_name = "overall_survival"
    # figures_dir = figures_dir
    # prefix = prefix
    # title_ext = ": Subgroup Analysis"
    # xlab = "Hazard Ratio (GKSRS vs Plaque)"
    # base_size = 14

    # TODO: Subgroup_list does not show n for each treatment_group -> must pivot_wider to show n for each treatment_group
    df <- purrr::imap_dfr(subgroup_list, function(tbl, name) {
        tbl %>% dplyr::mutate(subgroup_var = name, .before = 1)
    }) %>%
        dplyr::rename(
            term = subgroup,
            estimate = hr,
            conf.low = ci_lower,
            conf.high = ci_upper,
            p.value = pval
        ) %>%
        mutate(se = (conf.high - conf.low) / 1.96) %>%
        mutate(ci = ifelse(is.na(se), "",
            sprintf(
                "%.2f (%.2f to %.2f)",
                estimate, conf.low, conf.high
            )
        ))
        # %>%
        # pivot_wider(names_from = treatment_group, values_from = c(estimate, conf.low, conf.high, se, ci))
    
    new_df <- df %>%
        na.omit()

    log_message(sprintf("Removed %d rows from forestplot df that were NA", nrow(df) - nrow(new_df)))

    human_name <- tools::toTitleCase(gsub("_", " ", plot_name))
    title <- paste0(human_name, title_ext)
    which_col_ci <- which(colnames(df) == "ci")

    p <- forestploter::forest(new_df,
        est = new_df$estimate,
        lower = new_df$conf.low,
        upper = new_df$conf.high,
        sizes = new_df$se,
        ci_column = which_col_ci,
        ref_line = 1,
        arrow_lab = c("Plaque Better", "GKSRS Better"),
        xlim = c(0, 4),
        ticks_at = c(0.5, 1, 2, 3),
        title = title
    )


    ggsave(
        filename = file.path(figures_dir, paste0(prefix, "forestplot_", plot_name, ".pdf")),
        plot = p,
        width = 30, 
        height = 12
    )

    invisible(p)
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



#' DEPRECATED FOR NOW
# #' Workflow analysis function for uveal melanoma dataset
# #'
# #' Runs the full analysis pipeline for a given dataset, including rates, survival, tumor height, and subgroup analyses.
# #'
# #' @param dataset_name Name of the dataset (character, e.g., 'full_cohort').
# #'
# #' @return List of analysis results.
# #' @examples
# #' run_analysis("full_cohort")
# run_analysis <- function(dataset_name) {
#     # DEBUGGING:
#     # dataset_name = "uveal_melanoma_full_cohort"

#     log_message(sprintf("Starting analysis for dataset: %s", dataset_name))
    
#     # Get cohort info for file paths
#     cohort_info <- get_cohort_info(dataset_name)
#     tables_dir <- file.path(cohort_info$dir, "tables")
#     figures_dir <- file.path(cohort_info$dir, "figures")
#     prefix <- cohort_info$prefix

#     # Load analytic dataset
#     log_message("Loading analytic dataset")
#     data <- readRDS(file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds")))
#     log_message(sprintf("Loaded %d patients", nrow(data)))
    
#     # Summarize key variables before analysis
#     summarize_data(data)

#     # Show confounders
#     log_message(sprintf("Using %d confounders for adjustment", length(confounders)))

#     # 1a. Rates of recurrence
#     log_message("Calculating recurrence rates")
#     recurrence_rates <- calculate_rates(
#         data,
#         outcome_var = "recurrence1",
#         time_var = "tt_recurrence",
#         event_var = "recurrence_event",
#         confounders = confounders
#     )

#     # 1b. Rates of metastatic progression
#     log_message("Calculating metastatic progression rates")
#     mets_rates <- calculate_rates(
#         data,
#         outcome_var = "mets_progression",
#         time_var = "tt_mets",
#         event_var = "mets_event",
#         confounders = confounders
#     )

#     # 1c. Overall Survival
#     log_message("Analyzing overall survival")
#     os_analysis <- analyze_survival(
#         data,
#         time_var = "tt_death",
#         event_var = "death_event",
#         confounders = confounders,
#         ylab = "Overall Survival Probability"
#     )

#     # 1d. Progression Free Survival
#     log_message("Analyzing progression-free survival")
#     pfs_analysis <- analyze_survival(
#         data,
#         time_var = "tt_recurrence",
#         event_var = "recurrence_event",
#         confounders = confounders,
#         ylab = "Progression-Free Survival Probability"
#     )

#     # 1e. Tumor height changes
#     log_message("Analyzing tumor height changes")
#     height_changes <- analyze_tumor_height_changes(data)

#     # 1f. Subgroup analyses (run for all requested variables, but ignore optic_nerve if not looking atoverall group)
#     # forest plot of subgroup analyses for overall survival
#     subgroup_analyses_overall_survival <- list()
#     for (var in subgroup_vars) {
#         subgroup_analyses_overall_survival[[var]] <- subgroup_effects(
#             data,
#             subgroup_var = var,
#             outcome_var = "overall_survival",
#             time_var = "tt_death",
#             event_var = "death_event"
#         )
#     }
#     plot_subgroup_forest(subgroup_analyses_overall_survival, "overall_survival", figures_dir, prefix)

#     # forest plot of subgroup analyses for PFS
#     subgroup_analyses_recurrence <- list()
#     for (var in subgroup_vars) {
#         subgroup_analyses_recurrence[[var]] <- subgroup_effects(
#             data,
#             subgroup_var = var,
#             outcome_var = "recurrence1",
#             time_var = "tt_recurrence",
#             event_var = "recurrence_event"
#         )
#     }
#     plot_subgroup_forest(subgroup_analyses_recurrence, "recurrence", figures_dir, prefix)

#     # forest plot of subgroup analyses for metastasis
#     subgroup_analyses_metastasis <- list()
#     for (var in subgroup_vars) {
#         subgroup_analyses_metastasis[[var]] <- subgroup_effects(
#             data,
#             subgroup_var = var,
#             outcome_var = "mets_progression",
#             time_var = "tt_mets",
#             event_var = "mets_event"
#         )
#     }
#     plot_subgroup_forest(subgroup_analyses_metastasis, "metastasis", figures_dir, prefix)

#     # Save results
#     log_message("Saving analysis results")

#     mets_rates$table %>%
#         as_gt() %>%
#         gt::gtsave(
#             filename = file.path(tables_dir, paste0(prefix, "metastatic_progression_rates.html"))
#         )

#     height_changes$table %>%
#         as_gt() %>%
#         gt::gtsave(
#             filename = file.path(tables_dir, paste0(prefix, "tumor_height_changes.html"))
#         )

#     # Save Cox model tables
#     os_analysis$cox_table %>%
#         as_gt() %>%
#         gt::gtsave(
#             filename = file.path(tables_dir, paste0(prefix, "overall_survival_cox.html"))
#         )

#     pfs_analysis$cox_table %>%
#         as_gt() %>%
#         gt::gtsave(
#             filename = file.path(tables_dir, paste0(prefix, "progression_free_survival_cox.html"))
#         )

#     # Save plots

#     log_message("Saving survival plots")
#     # pdf(file.path(figures_dir, paste0(prefix, "overall_survival.pdf")))
#     # print(os_analysis$plot)
#     # dev.off()
#         ggsave(
#             filename = file.path(figures_dir, paste0(prefix, "overall_survival.pdf")),
#             plot = os_analysis$plot$plot, # note the $plot!
#             width = 7, height = 7
#         )

#     # pdf(file.path(figures_dir, paste0(prefix, "progression_free_survival.pdf")))
#     # print(pfs_analysis$plot)
#     # dev.off()
#     ggsave(
#         filename = file.path(figures_dir, paste0(prefix, "progression_free_survival.pdf")),
#         plot = pfs_analysis$plot$plot, # note the $plot!
#         width = 7, height = 7
#     )

#     # Save subgroup analysis plots
#     # for (var in subgroup_vars) {
#     #     pdf(file.path(figures_dir, paste0(prefix, "subgroup_analysis_", var, ".pdf")))
#     #     for (result in subgroup_analyses[[var]]) {
#     #         if (!is.null(result)) {
#     #             print(result$plot)
#     #         }
#     #     }
#     #     dev.off()
#     # }

#     # Save median survival times
#     write.csv(
#         os_analysis$median_times,
#         file.path(tables_dir, paste0(prefix, "overall_survival_medians.csv")),
#         row.names = FALSE
#     )

#     write.csv(
#         pfs_analysis$median_times,
#         file.path(tables_dir, paste0(prefix, "progression_free_survival_medians.csv")),
#         row.names = FALSE
#     )

#     log_message(sprintf("Analysis complete for dataset: %s", dataset_name))
#     return(list(
#         recurrence_rates = recurrence_rates,
#         mets_rates = mets_rates,
#         os_analysis = os_analysis,
#         pfs_analysis = pfs_analysis,
#         height_changes = height_changes,
#         subgroup_analyses = subgroup_analyses
#     ))
# }

# # # =====================
# # # TEST: Mock data for perform_subgroup_analysis
# # # =====================
# # if (interactive() || ("test_subgroup" %in% commandArgs(trailingOnly = TRUE))) {
# #   # Create mock data
# #   set.seed(123)
# #   mock_data <- tibble(
# #     id = 1:20,
# #     treatment_group = rep(c("Plaque", "GKSRS"), each = 10),
# #     age_at_diagnosis = c(rnorm(10, 60, 5), rnorm(10, 65, 5)),
# #     sex = rep(c("Male", "Female"), 10),
# #     location = rep(c("Choroidal", "Ciliary_Body"), 10),
# #     initial_tumor_height = runif(20, 3, 10),
# #     initial_tumor_diameter = runif(20, 8, 20),
# #     biopsy1_gep = sample(c("Class_1A_PRAME_negative", "Class_2_PRAME_positive"), 20, replace = TRUE),
# #     optic_nerve = sample(c("Yes", "No"), 20, replace = TRUE),
# #     tt_recurrence = rexp(20, 0.1),
# #     recurrence_event = rbinom(20, 1, 0.5),
# #     recurrence1 = ifelse(recurrence_event == 1, "Y", "N")
# #   )

# #   # Test with a continuous variable (age_at_diagnosis)
# #   cat("\n--- Subgroup analysis by age_at_diagnosis (continuous, binned) ---\n")
# #   res_age <- subgroup_effects(
# #     mock_data,
# #     subgroup_var = "age_at_diagnosis",
# #     outcome_var = "recurrence1",
# #     time_var = "tt_recurrence",
# #     event_var = "recurrence_event"
# #   )
# #   print(res_age)

# #   # Test with a categorical variable (sex)
# #   cat("\n--- Subgroup analysis by sex (categorical) ---\n")
# #   res_sex <- perform_subgroup_analysis(
# #     mock_data,
# #     outcome_var = "recurrence1",
# #     time_var = "tt_recurrence",
# #     event_var = "recurrence_event",
# #     subgroup_var = "sex"
# #   )
# #   print(res_sex)
# # }

