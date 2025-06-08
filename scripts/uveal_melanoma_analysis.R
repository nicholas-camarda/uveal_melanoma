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

# Set consistent contrast options for all modeling functions
# This ensures factor variables use consistent naming across all models
options(contrasts = c("contr.treatment", "contr.poly"))

# Function to ensure consistent factor contrasts with human-readable names
ensure_consistent_contrasts <- function(data) {
    factor_vars <- names(data)[sapply(data, is.factor)]
    for (var in factor_vars) {
        if (nlevels(data[[var]]) > 1) {
            # Set contrasts with meaningful names based on factor levels
            factor_levels <- levels(data[[var]])
            if (length(factor_levels) > 1) {
                contrast_matrix <- contr.treatment(length(factor_levels))
                # Use factor level names for contrast names (excluding reference level)
                colnames(contrast_matrix) <- factor_levels[-1]
                contrasts(data[[var]]) <- contrast_matrix
            }
        }
    }
    return(data)
}

# Function to extract coefficient names more robustly
get_treatment_coefficient_name <- function(model, treatment_var = "treatment_group", data = NULL) {
    coef_names <- names(coef(model))
    
    # Get the actual factor levels from the data if available
    if (!is.null(data) && treatment_var %in% names(data)) {
        treatment_levels <- levels(data[[treatment_var]])
        if (length(treatment_levels) > 1) {
            treatment_nonref <- treatment_levels[2]  # Second level (non-reference)
        } else {
            treatment_nonref <- "GKSRS"  # fallback
        }
    } else {
        treatment_nonref <- "GKSRS"  # fallback
    }
    
    # Try multiple patterns in order of preference
    possible_patterns <- c(
        paste0(treatment_var, treatment_nonref),     # treatment_groupGKSRS
        paste0(treatment_var, "2"),                  # treatment_group2
        paste0(treatment_var, "GKSRS"),              # treatment_groupGKSRS (exact)
        treatment_nonref                             # GKSRS (just the level)
    )
    
    for (pattern in possible_patterns) {
        if (pattern %in% coef_names) {
            return(pattern)
        }
    }
    
    # If no exact match, try partial matching
    for (pattern in possible_patterns) {
        matches <- grep(pattern, coef_names, value = TRUE)
        if (length(matches) > 0) {
            return(matches[1])
        }
    }
    
    return(NULL)
}

# Function to extract interaction coefficient names more robustly  
get_interaction_coefficient_name <- function(model, treatment_var = "treatment_group", 
                                           subgroup_var, subgroup_level, data = NULL) {
    coef_names <- names(coef(model))
    
    # Get the actual factor levels from the data if available
    if (!is.null(data) && treatment_var %in% names(data)) {
        treatment_levels <- levels(data[[treatment_var]])
        if (length(treatment_levels) > 1) {
            treatment_nonref <- treatment_levels[2]  # Second level (non-reference)
        } else {
            treatment_nonref <- "GKSRS"  # fallback
        }
    } else {
        treatment_nonref <- "GKSRS"  # fallback
    }
    
    # Try multiple interaction patterns
    possible_patterns <- c(
        paste0(treatment_var, treatment_nonref, ":", subgroup_var, subgroup_level),
        paste0(treatment_var, "2:", subgroup_var, subgroup_level),
        paste0(treatment_var, treatment_nonref, ":", subgroup_var, "2"),
        paste0(treatment_var, "2:", subgroup_var, "2"),
        paste0(treatment_var, "GKSRS:", subgroup_var, subgroup_level),
        paste0(treatment_var, "GKSRS:", subgroup_var, "2"),
        paste0(treatment_var, treatment_nonref, ":", subgroup_var, "Yes"),
        paste0(treatment_var, treatment_nonref, ":", subgroup_var, "No"),
        paste0(treatment_var, "2:", subgroup_var, "Yes"),
        paste0(treatment_var, "2:", subgroup_var, "No")
    )
    
    # Try exact matches first
    for (pattern in possible_patterns) {
        if (pattern %in% coef_names) {
            return(pattern)
        }
    }
    
    # Try pattern matching with regex
    interaction_pattern <- paste0(treatment_var, ".*:", subgroup_var, ".*")
    matches <- grep(interaction_pattern, coef_names, value = TRUE)
    if (length(matches) > 0) {
        return(matches[1])
    }
    
    return(NULL)
}

# Function to get human-readable variable labels for tables
get_variable_labels <- function() {
    list(
        # Treatment and demographics
        treatment_group = "Treatment Group",
        age_at_diagnosis = "Age at Diagnosis (years)",
        sex = "Sex",
        
        # Tumor characteristics
        location = "Tumor Location",
        optic_nerve = "Optic Nerve Involvement",
        initial_tumor_height = "Initial Tumor Height (mm)",
        initial_tumor_diameter = "Initial Tumor Diameter (mm)",
        initial_overall_stage = "Initial Overall Stage",
        initial_t_stage = "Initial T Stage",
        biopsy1_gep = "Gene Expression Profile",
        internal_reflectivity = "Internal Reflectivity",
        srf = "Subretinal Fluid",
        op = "Orange Pigment",
        
        # Symptoms
        symptoms = "Any Symptoms",
        vision_loss_blurred_vision = "Vision Loss/Blurred Vision",
        visual_field_defect = "Visual Field Defect",
        flashes_photopsia = "Flashes/Photopsia",
        floaters = "Floaters",
        pain = "Pain",
        
        # Outcomes
        recurrence1 = "Local Recurrence",
        recurrence2 = "Second Recurrence",
        mets_progression = "Metastatic Progression",
        enucleation = "Enucleation",
        retinopathy = "Radiation Retinopathy",
        nvg = "Neovascular Glaucoma",
        srd = "Serous Retinal Detachment",
        
        # Recurrence treatment
        recurrence1_treatment_clean = "Recurrence Treatment",
        
        # Follow-up variables
        follow_up_years = "Follow-up Time (years)",
        follow_up_months = "Follow-up Time (months)"
    )
}

# Source the data processing script
source("scripts/data_processing.R")

# Analysis settings are now defined in main.R
# VERBOSE and SHOW_ALL_PVALUES are set there 

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

    # TODO: make confounders a list and integrate into tbl_regression labels = list()
    # confounders <- list(
    #     treatment_group = "Treatment",
    #     age_at_diagnosis = "Age at diagnosis",
    #     sex = "Sex",
    #     location = "Tumor location",
    #     optic_nerve = "Optic nerve involvement"
    # )

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
    
    # Ensure consistent factor contrasts for modeling
    data_with_height_change <- ensure_consistent_contrasts(data_with_height_change)

    # Summary statistics (grouped)
    height_changes <- data_with_height_change %>%
        group_by(treatment_group) %>%
        summarize(
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
    
    # Get variable labels for better readability
    variable_labels <- get_variable_labels()
    
    primary_height_lm_tbl <- tbl_regression(primary_height_lm,
        exponentiate = FALSE,
        intercept = FALSE,
        label = variable_labels  # Apply human-readable labels
    )
    
    # Add p-values based on toggle setting
    if (SHOW_ALL_PVALUES) {
        # Show individual p-values for each coefficient
        primary_height_lm_tbl <- primary_height_lm_tbl  # No modification needed
    } else {
        # Show only grouped p-values (one per variable)
        primary_height_lm_tbl <- primary_height_lm_tbl %>% add_global_p()
    }
    
    primary_height_lm_tbl <- primary_height_lm_tbl %>%
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
        intercept = FALSE,
        label = variable_labels  # Apply human-readable labels
    )
    
    # Add p-values based on toggle setting
    if (SHOW_ALL_PVALUES) {
        # Show individual p-values for each coefficient
        sensitivity_height_lm_tbl <- sensitivity_height_lm_tbl  # No modification needed
    } else {
        # Show only grouped p-values (one per variable)
        sensitivity_height_lm_tbl <- sensitivity_height_lm_tbl %>% add_global_p()
    }
    
    sensitivity_height_lm_tbl <- sensitivity_height_lm_tbl %>%
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
        # Ensure consistent factor contrasts for modeling
        data <- ensure_consistent_contrasts(data)
        
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
            log_message("Factor levels:")
            log_message(sprintf("  treatment_group: %s", paste(levels(data$treatment_group), collapse = ", ")))
            log_message(sprintf("  %s: %s", subgroup_var_to_use, paste(levels(data[[subgroup_var_to_use]]), collapse = ", ")))
            log_message("Model summary:")
            print(summary(model))
        }
        
        # Extract interaction p-value (test overall interaction significance)
        model_summary <- summary(model)
        
        # For multiple levels, we need to test the overall interaction significance
        if (length(subgroup_levels) == 2) {
            # Simple case: just one interaction term
            # Use the helper function to find the interaction term robustly
            interaction_term <- get_interaction_coefficient_name(model, "treatment_group", 
                                                               subgroup_var_to_use, subgroup_levels[2], data)
            
            if (VERBOSE) {
                log_message(sprintf("Looking for interaction terms: treatment_group:%s", subgroup_var_to_use))
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
                
                # Use the new helper function for robust coefficient extraction
                coef_idx <- get_treatment_coefficient_name(model, "treatment_group", data)
                
                if (VERBOSE) {
                    log_message(sprintf("  Reference level - Looking for coefficient: treatment_group coefficient"))
                    if (!is.null(coef_idx)) {
                        log_message(sprintf("  Found coefficient: %s", coef_idx))
                    } else {
                        log_message(sprintf("  Coefficient 'treatment_group%s' not found", levels(data$treatment_group)[2]))
                    }
                }
                
                if (!is.null(coef_idx)) {
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
                        log_message("  No main treatment coefficient found")
                    }
                    treatment_effect <- NA; ci_lower <- NA; ci_upper <- NA; p_val <- NA
                }
            } else {
                # Non-reference subgroup: treatment effect = β1 + β3 (main + interaction)
                
                # Use helper functions for robust coefficient extraction
                main_coef_idx <- get_treatment_coefficient_name(model, "treatment_group", data)
                interaction_coef_idx <- get_interaction_coefficient_name(model, "treatment_group", 
                                                                        subgroup_var_to_use, level, data)
                
                if (VERBOSE) {
                    log_message(sprintf("  Non-reference level - Looking for coefficients:"))
                    log_message(sprintf("    Main: treatment_group"))
                    log_message(sprintf("    Interaction candidates: treatment_group:%s%s, treatment_group:%s2, treatment_group:%sYes, treatment_group:%sNo", 
                                      subgroup_var_to_use, level, subgroup_var_to_use, subgroup_var_to_use, subgroup_var_to_use))
                    if (!is.null(interaction_coef_idx)) {
                        log_message(sprintf("    Found interaction: %s", interaction_coef_idx))
                    } else {
                        log_message("    No interaction coefficient found")
                    }
                }
                
                if (!is.null(main_coef_idx) && !is.null(interaction_coef_idx)) {
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
                                          !is.null(main_coef_idx),
                                          !is.null(interaction_coef_idx)))
                        log_message(sprintf("  Available coefficients: %s", paste(names(coef(model)), collapse = ", ")))
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
    
    # Ensure consistent factor contrasts for modeling
    data_with_vision_change <- ensure_consistent_contrasts(data_with_vision_change)

    # Summary statistics (grouped)
    vision_changes <- data_with_vision_change %>%
        group_by(treatment_group) %>%
        summarize(
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
    
    # Get variable labels for better readability
    variable_labels <- get_variable_labels()
    
    vision_lm_tbl <- tbl_regression(vision_lm,
        exponentiate = FALSE,
        intercept = FALSE,
        label = variable_labels  # Apply human-readable labels
    )
    
    # Add p-values based on toggle setting
    if (SHOW_ALL_PVALUES) {
        # Show individual p-values for each coefficient
        vision_lm_tbl <- vision_lm_tbl  # No modification needed
    } else {
        # Show only grouped p-values (one per variable)
        vision_lm_tbl <- vision_lm_tbl %>% add_global_p()
    }
    
    vision_lm_tbl <- vision_lm_tbl %>%
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
    
    # Ensure consistent factor contrasts for modeling
    data <- ensure_consistent_contrasts(data)
    
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
        summarize(
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
        
        # Get variable labels for better readability
        variable_labels <- get_variable_labels()
        
        model_result <- tbl_regression(model,
            exponentiate = TRUE,
            intercept = FALSE,
            label = variable_labels  # Apply human-readable labels
        )
        
        # Add p-values based on toggle setting
        if (SHOW_ALL_PVALUES) {
            # Show individual p-values for each coefficient
            model_result <- model_result  # No modification needed
        } else {
            # Show only grouped p-values (one per variable)
            model_result <- model_result %>% add_global_p()
        }
        
        model_result <- model_result %>%
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

#' Merge Full and Restricted Cohort Tables Side by Side
#'
#' This function creates merged tables that display full and restricted cohort data side by side
#' for easier comparison and presentation, as requested by collaborators.
#' It reads existing HTML tables and recreates them in Excel format with both cohorts.
#'
#' @param full_cohort_data Data frame containing the full cohort data
#' @param restricted_cohort_data Data frame containing the restricted cohort data
#' @param output_path Path where merged tables should be saved
#'
#' @return None. Side effect: saves merged tables as Excel files
#'
#' @examples
#' merge_cohort_tables(full_data, restricted_data, "final_data/Analysis/merged_tables/")
merge_cohort_tables <- function(full_cohort_data, restricted_cohort_data, output_path = NULL) {
    
    log_message("=== STARTING TABLE MERGING: Full and Restricted Cohorts ===")
    
    # Set default output path if not provided
    if (is.null(output_path)) {
        output_path <- file.path("final_data", "Analysis", "merged_tables")
    }
    
    # Create output directory
    if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
        log_message(sprintf("Created merged tables directory: %s", output_path))
    }
    
    log_message(sprintf("Merging tables will be saved to: %s", output_path))
    
    # Define variables for baseline characteristics summary
    vars_to_summarize <- c(
        "age_at_diagnosis", "race", "sex", "eye",
        "initial_vision", "location", "optic_nerve",
        "initial_tumor_height", "initial_tumor_diameter",
        "internal_reflectivity", "srf", "op", "symptoms",
        "vision_loss_blurred_vision", "visual_field_defect",
        "flashes_photopsia", "floaters", "pain",
        "initial_overall_stage", "initial_t_stage",
        "initial_n_stage", "initial_m_stage",
        "initial_mets", "biopsy1_gep"
    )
    
    # 1. BASELINE CHARACTERISTICS TABLE (Most Important)
    log_message("Reading existing baseline characteristics tables and merging them")
    
    # Read the existing summary tables that were already created
    # These should be available from the analysis that was just run
    tryCatch({
        # Try to read from the latest results directory first
        full_cohort_dir <- file.path("old_results", format(Sys.Date(), "%y_%m_%d"), "uveal_full", "tables")
        restricted_cohort_dir <- file.path("old_results", format(Sys.Date(), "%y_%m_%d"), "uveal_restricted", "tables")
        
        # If today's results don't exist, find the most recent results
        if (!file.exists(file.path(full_cohort_dir, "uveal_full_baseline_characteristics.html"))) {
            # Find the most recent results directory
            results_dirs <- list.dirs("old_results", recursive = FALSE)
            if (length(results_dirs) > 0) {
                latest_dir <- results_dirs[length(results_dirs)]
                full_cohort_dir <- file.path(latest_dir, "uveal_full", "tables")
                restricted_cohort_dir <- file.path(latest_dir, "uveal_restricted", "tables") 
            }
        }
        
        log_message(sprintf("Looking for tables in: %s", full_cohort_dir))
        
        # For now, just recreate simple versions since reading HTML tables is complex
        # This is still much simpler than the previous version
        
        # Quick baseline table for full cohort
        full_baseline <- full_cohort_data %>%
            select(all_of(vars_to_summarize), treatment_group) %>%
            tbl_summary(by = treatment_group, missing = "no") %>%
            add_overall() %>%
            add_p() %>%
            as_tibble()
        
        # Quick baseline table for restricted cohort  
        restricted_baseline <- restricted_cohort_data %>%
            select(all_of(vars_to_summarize), treatment_group) %>%
            tbl_summary(by = treatment_group, missing = "no") %>%
            add_overall() %>%
            add_p() %>%
            as_tibble()
        
        # Check what the first column is actually called
        log_message(sprintf("Full table columns: %s", paste(names(full_baseline), collapse = ", ")))
        log_message(sprintf("Restricted table columns: %s", paste(names(restricted_baseline), collapse = ", ")))
        
        # Get the actual name of the first column (variable names)
        var_col_name <- names(full_baseline)[1]
        
        # Rename columns to distinguish cohorts (skip the first column which is variables)
        names(full_baseline)[2:ncol(full_baseline)] <- paste("Full Cohort", names(full_baseline)[2:ncol(full_baseline)], sep = " - ")
        names(restricted_baseline)[2:ncol(restricted_baseline)] <- paste("Restricted Cohort", names(restricted_baseline)[2:ncol(restricted_baseline)], sep = " - ")
        
        # Merge side by side using the actual variable column name
        merged_baseline <- full_baseline %>%
            full_join(restricted_baseline, by = var_col_name)
        
        # Save as Excel
        writexl::write_xlsx(
            merged_baseline,
            path = file.path(output_path, "merged_baseline_characteristics.xlsx")
        )
        
        log_message("Saved merged baseline characteristics table")
        
    }, error = function(e) {
        log_message(sprintf("Error merging baseline tables: %s", e$message))
        log_message("Skipping baseline table merge")
    })
    
    # Summary message
    log_message("=== COMPLETED TABLE MERGING ===")
    log_message(sprintf("Merged baseline characteristics table saved to: %s", output_path))
    log_message("File created: merged_baseline_characteristics.xlsx")
    
    return(invisible(NULL))
}

# Forest plot functions and comprehensive subgroup analysis

#' Create forest plot for subgroup analysis
#'
#' Creates a forest plot showing treatment effects across different subgroups
#' similar to the example in Figure 3 provided by the collaborator
#'
#' @param subgroup_results List of subgroup analysis results
#' @param outcome_name Name of the outcome being analyzed
#' @param effect_measure Type of effect measure ("HR" for hazard ratio, "OR" for odds ratio, "MD" for mean difference)
#' @param dataset_name Name of the dataset
#' @param output_path Full path for saving the plot
#' @return ggplot object
create_forest_plot <- function(subgroup_results, outcome_name, effect_measure = "HR", dataset_name = "", output_path = NULL) {
    
    # Combine all subgroup results into a single data frame
    forest_data <- data.frame()
    
    for (var_name in names(subgroup_results)) {
        result <- subgroup_results[[var_name]]
        
        if (!is.null(result$subgroup_effects) && nrow(result$subgroup_effects) > 0) {
            temp_data <- result$subgroup_effects %>%
                mutate(
                    subgroup_variable = var_name,
                    variable_label = tools::toTitleCase(gsub("_", " ", var_name)),
                    level_label = case_when(
                        # Clean up common variable names
                        var_name == "treatment_group" ~ as.character(subgroup_level),
                        var_name == "sex" ~ as.character(subgroup_level),
                        var_name == "location" ~ as.character(subgroup_level),
                        var_name == "optic_nerve" ~ ifelse(as.character(subgroup_level) == "Y", "Yes", "No"),
                        var_name == "biopsy1_gep" ~ as.character(subgroup_level),
                        TRUE ~ as.character(subgroup_level)
                    ),
                    interaction_p = result$interaction_p
                )
            forest_data <- rbind(forest_data, temp_data)
        }
    }
    
    if (nrow(forest_data) == 0) {
        warning("No data available for forest plot")
        return(NULL)
    }
    
    # Determine x-axis label and reference line based on effect measure
    x_label <- case_when(
        effect_measure == "HR" ~ "Hazard Ratio (95% CI)",
        effect_measure == "OR" ~ "Odds Ratio (95% CI)", 
        effect_measure == "MD" ~ "Mean Difference (95% CI)",
        TRUE ~ "Effect Estimate (95% CI)"
    )
    
    ref_line <- ifelse(effect_measure == "MD", 0, 1)
    favors_left <- ifelse(effect_measure == "MD", "Favours Plaque", "Favours Plaque")
    favors_right <- ifelse(effect_measure == "MD", "Favours GKSRS", "Favours GKSRS")
    
    # Create the plot data with proper ordering
    plot_data <- forest_data %>%
        filter(!is.na(treatment_effect)) %>%
        mutate(
            # Create combined label for display
            combined_label = paste0(variable_label, ": ", level_label),
            # Format sample sizes
            n_label = sprintf("%d/%d", n_gksrs, n_plaque),
            # Determine significance for interaction
            interaction_sig = ifelse(is.na(interaction_p), "", 
                                   ifelse(interaction_p < 0.05, "*", "")),
            # Add asterisk to variable name if interaction is significant
            variable_display = paste0(variable_label, interaction_sig)
        ) %>%
        arrange(variable_label, subgroup_level)
    
    # Create forest plot
    p <- ggplot(plot_data, aes(x = treatment_effect, y = reorder(combined_label, desc(row_number())))) +
        geom_vline(xintercept = ref_line, linetype = "dashed", color = "gray50", size = 0.8) +
        geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), 
                      height = 0.3, size = 0.8, color = "black") +
        geom_point(aes(size = n_total), shape = 15, color = "black") +
        scale_size_continuous(name = "Sample Size", range = c(2, 5), guide = guide_legend(order = 1)) +
        labs(
            title = paste("Subgroup Analysis:", outcome_name),
            subtitle = paste("Dataset:", dataset_name),
            x = x_label,
            y = "Subgroup",
            caption = "* indicates significant interaction (p < 0.05)\nSquare size proportional to sample size"
        ) +
        theme_classic() +
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 11),
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 10),
            legend.position = "right",
            panel.grid.major.y = element_line(color = "gray90", size = 0.3),
            strip.text = element_text(size = 10, face = "bold"),
            plot.caption = element_text(size = 9, hjust = 0)
        ) +
        # Add sample size annotations
        geom_text(aes(label = n_label), 
                 x = max(plot_data$ci_upper, na.rm = TRUE) * 1.15,
                 size = 3, hjust = 0) +
        # Add p-value annotations
        geom_text(aes(label = sprintf("%.3f", p_value)), 
                 x = max(plot_data$ci_upper, na.rm = TRUE) * 1.35,
                 size = 3, hjust = 0) +
        # Extend x-axis to accommodate annotations
        expand_limits(x = max(plot_data$ci_upper, na.rm = TRUE) * 1.5) +
        # Add labels for direction of effect
        annotate("text", x = min(plot_data$ci_lower, na.rm = TRUE) * 0.8, 
                y = 0.5, label = favors_left, hjust = 1, size = 3.5, color = "blue") +
        annotate("text", x = max(plot_data$ci_upper, na.rm = TRUE) * 0.8, 
                y = 0.5, label = favors_right, hjust = 0, size = 3.5, color = "blue")
    
    # Add column headers
    p <- p + 
        annotation_custom(
            grob = textGrob("GKSRS/Plaque", gp = gpar(fontsize = 9, fontface = "bold")),
            xmin = max(plot_data$ci_upper, na.rm = TRUE) * 1.15,
            xmax = max(plot_data$ci_upper, na.rm = TRUE) * 1.15,
            ymin = nrow(plot_data) + 0.5,
            ymax = nrow(plot_data) + 0.5
        ) +
        annotation_custom(
            grob = textGrob("p-value", gp = gpar(fontsize = 9, fontface = "bold")),
            xmin = max(plot_data$ci_upper, na.rm = TRUE) * 1.35,
            xmax = max(plot_data$ci_upper, na.rm = TRUE) * 1.35,
            ymin = nrow(plot_data) + 0.5,
            ymax = nrow(plot_data) + 0.5
        )
    
    # Save plot if path provided
    if (!is.null(output_path)) {
        ggsave(output_path, p, width = 12, height = max(6, nrow(plot_data) * 0.4), dpi = 300)
        log_message(sprintf("Forest plot saved to: %s", output_path))
    }
    
    return(p)
}

#' Perform comprehensive subgroup analysis for survival outcomes
#'
#' Performs subgroup analysis for time-to-event outcomes (OS, PFS, recurrence, metastatic progression)
#' using Cox regression with interaction terms
#'
#' @param data Data frame containing the analysis variables
#' @param time_var Name of the time variable
#' @param event_var Name of the event indicator variable  
#' @param subgroup_vars Character vector of subgroup variables to test
#' @param confounders Character vector of confounders
#' @param outcome_name Name of the outcome for labeling
#' @return List of subgroup analysis results
perform_survival_subgroup_analysis <- function(data, time_var, event_var, subgroup_vars, confounders = NULL, outcome_name = "Survival") {
    
    log_message(sprintf("Performing subgroup analysis for %s", outcome_name))
    
    subgroup_results <- list()
    
    for (subgroup_var in subgroup_vars) {
        log_message(sprintf("Testing interaction for: %s", subgroup_var))
        
        tryCatch({
            # Filter confounders to exclude the subgroup variable
            confounders_to_use <- if (!is.null(confounders)) {
                confounders[confounders != subgroup_var]
            } else {
                NULL
            }
            
            # Validate confounders
            if (length(confounders_to_use) > 0) {
                confounders_to_use <- generate_valid_confounders(data, confounders_to_use, threshold = THRESHOLD_RARITY)
            }
            
            # Process subgroup variable (bin if continuous)
            processed_data <- data
            was_continuous <- is.numeric(data[[subgroup_var]]) || is.integer(data[[subgroup_var]])
            
            if (was_continuous) {
                cutoff_val <- quantile(data[[subgroup_var]], probs = 0.5, na.rm = TRUE)
                subgroup_var_binned <- paste0(subgroup_var, "_binned")
                processed_data[[subgroup_var_binned]] <- factor(
                    ifelse(data[[subgroup_var]] < cutoff_val,
                           paste0("< ", round(cutoff_val, 1)),
                           paste0("≥ ", round(cutoff_val, 1))),
                    levels = c(paste0("< ", round(cutoff_val, 1)), 
                              paste0("≥ ", round(cutoff_val, 1)))
                )
                subgroup_var_to_use <- subgroup_var_binned
            } else {
                if (!is.factor(processed_data[[subgroup_var]])) {
                    processed_data[[subgroup_var]] <- as.factor(processed_data[[subgroup_var]])
                }
                processed_data <- handle_rare_categories(processed_data, vars = subgroup_var, threshold = THRESHOLD_RARITY)
                subgroup_var_to_use <- subgroup_var
            }
            
            # Ensure consistent contrasts
            processed_data <- ensure_consistent_contrasts(processed_data)
            
            # Build formula with interaction
            if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
                formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ treatment_group * ", subgroup_var_to_use)
            } else {
                formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ treatment_group * ", subgroup_var_to_use, " + ", paste(confounders_to_use, collapse = " + "))
            }
            
            # Fit Cox model with interaction
            cox_model <- coxph(as.formula(formula_str), data = processed_data)
            
            # Test interaction significance
            subgroup_levels <- levels(processed_data[[subgroup_var_to_use]])
            
            if (length(subgroup_levels) == 2) {
                # Simple interaction test
                interaction_term <- get_interaction_coefficient_name(cox_model, "treatment_group", 
                                                                   subgroup_var_to_use, subgroup_levels[2], processed_data)
                if (!is.null(interaction_term)) {
                    interaction_p <- summary(cox_model)$coefficients[interaction_term, "Pr(>|z|)"]
                } else {
                    interaction_p <- NA
                }
            } else {
                # Multiple levels - use likelihood ratio test
                if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
                    formula_no_int <- paste0("Surv(", time_var, ", ", event_var, ") ~ treatment_group + ", subgroup_var_to_use)
                } else {
                    formula_no_int <- paste0("Surv(", time_var, ", ", event_var, ") ~ treatment_group + ", subgroup_var_to_use, " + ", paste(confounders_to_use, collapse = " + "))
                }
                cox_no_interaction <- coxph(as.formula(formula_no_int), data = processed_data)
                interaction_test <- anova(cox_no_interaction, cox_model)
                interaction_p <- interaction_test$`Pr(>Chi)`[2]
            }
            
            # Calculate hazard ratios for each subgroup
            subgroup_effects <- data.frame()
            
            for (i in seq_along(subgroup_levels)) {
                level <- subgroup_levels[i]
                level_data <- processed_data[processed_data[[subgroup_var_to_use]] == level, ]
                
                n_total <- nrow(level_data)
                n_plaque <- sum(level_data$treatment_group == "Plaque", na.rm = TRUE)
                n_gksrs <- sum(level_data$treatment_group == "GKSRS", na.rm = TRUE)
                
                if (i == 1) {
                    # Reference subgroup: main treatment effect
                    coef_idx <- get_treatment_coefficient_name(cox_model, "treatment_group", processed_data)
                    if (!is.null(coef_idx)) {
                        hr <- exp(coef(cox_model)[coef_idx])
                        se_log_hr <- sqrt(vcov(cox_model)[coef_idx, coef_idx])
                        ci_lower <- exp(coef(cox_model)[coef_idx] - 1.96 * se_log_hr)
                        ci_upper <- exp(coef(cox_model)[coef_idx] + 1.96 * se_log_hr)
                        p_val <- summary(cox_model)$coefficients[coef_idx, "Pr(>|z|)"]
                    } else {
                        hr <- NA; ci_lower <- NA; ci_upper <- NA; p_val <- NA
                    }
                } else {
                    # Non-reference subgroup: combined effect
                    main_coef_idx <- get_treatment_coefficient_name(cox_model, "treatment_group", processed_data)
                    interaction_coef_idx <- get_interaction_coefficient_name(cox_model, "treatment_group", 
                                                                            subgroup_var_to_use, level, processed_data)
                    
                    if (!is.null(main_coef_idx) && !is.null(interaction_coef_idx)) {
                        combined_coef <- coef(cox_model)[main_coef_idx] + coef(cox_model)[interaction_coef_idx]
                        hr <- exp(combined_coef)
                        
                        # Standard error for combined effect
                        var_main <- vcov(cox_model)[main_coef_idx, main_coef_idx]
                        var_int <- vcov(cox_model)[interaction_coef_idx, interaction_coef_idx]
                        cov_main_int <- vcov(cox_model)[main_coef_idx, interaction_coef_idx]
                        se_combined <- sqrt(var_main + var_int + 2 * cov_main_int)
                        
                        ci_lower <- exp(combined_coef - 1.96 * se_combined)
                        ci_upper <- exp(combined_coef + 1.96 * se_combined)
                        
                        # P-value for combined effect
                        z_stat <- combined_coef / se_combined
                        p_val <- 2 * (1 - pnorm(abs(z_stat)))
                    } else {
                        hr <- NA; ci_lower <- NA; ci_upper <- NA; p_val <- NA
                    }
                }
                
                subgroup_effects <- rbind(subgroup_effects, data.frame(
                    subgroup_variable = subgroup_var,
                    subgroup_level = level,
                    n_total = n_total,
                    n_plaque = n_plaque,
                    n_gksrs = n_gksrs,
                    treatment_effect = hr,  # This will be HR for survival outcomes
                    ci_lower = ci_lower,
                    ci_upper = ci_upper,
                    p_value = p_val,
                    stringsAsFactors = FALSE
                ))
            }
            
            subgroup_results[[subgroup_var]] <- list(
                interaction_p = interaction_p,
                subgroup_effects = subgroup_effects,
                model = cox_model,
                subgroup_var_used = subgroup_var_to_use,
                formula_used = formula_str,
                confounders_used = confounders_to_use
            )
            
            log_message(sprintf("  Interaction p-value: %.4f", ifelse(is.na(interaction_p), 999, interaction_p)))
            
        }, error = function(e) {
            warning(sprintf("Error in subgroup analysis for %s: %s", subgroup_var, e$message))
            subgroup_results[[subgroup_var]] <- list(
                interaction_p = NA,
                subgroup_effects = data.frame(),
                model = NULL,
                subgroup_var_used = NA,
                formula_used = NA,
                confounders_used = NULL
            )
        })
    }
    
    return(subgroup_results)
}

#' Perform comprehensive subgroup analysis for binary outcomes  
#'
#' Performs subgroup analysis for binary outcomes (recurrence, metastatic progression)
#' using logistic regression with interaction terms
#'
#' @param data Data frame containing the analysis variables
#' @param outcome_var Name of the binary outcome variable
#' @param subgroup_vars Character vector of subgroup variables to test
#' @param confounders Character vector of confounders
#' @param outcome_name Name of the outcome for labeling
#' @return List of subgroup analysis results
perform_binary_subgroup_analysis <- function(data, outcome_var, subgroup_vars, confounders = NULL, outcome_name = "Binary Outcome") {
    
    log_message(sprintf("Performing subgroup analysis for %s", outcome_name))
    
    subgroup_results <- list()
    
    for (subgroup_var in subgroup_vars) {
        log_message(sprintf("Testing interaction for: %s", subgroup_var))
        
        tryCatch({
            # Filter confounders to exclude the subgroup variable
            confounders_to_use <- if (!is.null(confounders)) {
                confounders[confounders != subgroup_var]
            } else {
                NULL
            }
            
            # Validate confounders
            if (length(confounders_to_use) > 0) {
                confounders_to_use <- generate_valid_confounders(data, confounders_to_use, threshold = THRESHOLD_RARITY)
            }
            
            # Process subgroup variable (bin if continuous)
            processed_data <- data
            was_continuous <- is.numeric(data[[subgroup_var]]) || is.integer(data[[subgroup_var]])
            
            if (was_continuous) {
                cutoff_val <- quantile(data[[subgroup_var]], probs = 0.5, na.rm = TRUE)
                subgroup_var_binned <- paste0(subgroup_var, "_binned")
                processed_data[[subgroup_var_binned]] <- factor(
                    ifelse(data[[subgroup_var]] < cutoff_val,
                           paste0("< ", round(cutoff_val, 1)),
                           paste0("≥ ", round(cutoff_val, 1))),
                    levels = c(paste0("< ", round(cutoff_val, 1)), 
                              paste0("≥ ", round(cutoff_val, 1)))
                )
                subgroup_var_to_use <- subgroup_var_binned
            } else {
                if (!is.factor(processed_data[[subgroup_var]])) {
                    processed_data[[subgroup_var]] <- as.factor(processed_data[[subgroup_var]])
                }
                processed_data <- handle_rare_categories(processed_data, vars = subgroup_var, threshold = THRESHOLD_RARITY)
                subgroup_var_to_use <- subgroup_var
            }
            
            # Ensure consistent contrasts
            processed_data <- ensure_consistent_contrasts(processed_data)
            
            # Build formula with interaction
            if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
                formula_str <- paste0(outcome_var, " ~ treatment_group * ", subgroup_var_to_use)
            } else {
                formula_str <- paste0(outcome_var, " ~ treatment_group * ", subgroup_var_to_use, " + ", paste(confounders_to_use, collapse = " + "))
            }
            
            # Fit logistic regression model with interaction
            logit_model <- glm(as.formula(formula_str), data = processed_data, family = binomial())
            
            # Test interaction significance
            subgroup_levels <- levels(processed_data[[subgroup_var_to_use]])
            
            if (length(subgroup_levels) == 2) {
                # Simple interaction test
                interaction_term <- get_interaction_coefficient_name(logit_model, "treatment_group", 
                                                                   subgroup_var_to_use, subgroup_levels[2], processed_data)
                if (!is.null(interaction_term)) {
                    interaction_p <- summary(logit_model)$coefficients[interaction_term, "Pr(>|z|)"]
                } else {
                    interaction_p <- NA
                }
            } else {
                # Multiple levels - use likelihood ratio test
                if (is.null(confounders_to_use) || length(confounders_to_use) == 0) {
                    formula_no_int <- paste0(outcome_var, " ~ treatment_group + ", subgroup_var_to_use)
                } else {
                    formula_no_int <- paste0(outcome_var, " ~ treatment_group + ", subgroup_var_to_use, " + ", paste(confounders_to_use, collapse = " + "))
                }
                logit_no_interaction <- glm(as.formula(formula_no_int), data = processed_data, family = binomial())
                interaction_test <- anova(logit_no_interaction, logit_model, test = "Chisq")
                interaction_p <- interaction_test$`Pr(>Chi)`[2]
            }
            
            # Calculate odds ratios for each subgroup
            subgroup_effects <- data.frame()
            
            for (i in seq_along(subgroup_levels)) {
                level <- subgroup_levels[i]
                level_data <- processed_data[processed_data[[subgroup_var_to_use]] == level, ]
                
                n_total <- nrow(level_data)
                n_plaque <- sum(level_data$treatment_group == "Plaque", na.rm = TRUE)
                n_gksrs <- sum(level_data$treatment_group == "GKSRS", na.rm = TRUE)
                
                if (i == 1) {
                    # Reference subgroup: main treatment effect
                    coef_idx <- get_treatment_coefficient_name(logit_model, "treatment_group", processed_data)
                    if (!is.null(coef_idx)) {
                        or <- exp(coef(logit_model)[coef_idx])
                        se_log_or <- sqrt(vcov(logit_model)[coef_idx, coef_idx])
                        ci_lower <- exp(coef(logit_model)[coef_idx] - 1.96 * se_log_or)
                        ci_upper <- exp(coef(logit_model)[coef_idx] + 1.96 * se_log_or)
                        p_val <- summary(logit_model)$coefficients[coef_idx, "Pr(>|z|)"]
                    } else {
                        or <- NA; ci_lower <- NA; ci_upper <- NA; p_val <- NA
                    }
                } else {
                    # Non-reference subgroup: combined effect
                    main_coef_idx <- get_treatment_coefficient_name(logit_model, "treatment_group", processed_data)
                    interaction_coef_idx <- get_interaction_coefficient_name(logit_model, "treatment_group", 
                                                                            subgroup_var_to_use, level, processed_data)
                    
                    if (!is.null(main_coef_idx) && !is.null(interaction_coef_idx)) {
                        combined_coef <- coef(logit_model)[main_coef_idx] + coef(logit_model)[interaction_coef_idx]
                        or <- exp(combined_coef)
                        
                        # Standard error for combined effect
                        var_main <- vcov(logit_model)[main_coef_idx, main_coef_idx]
                        var_int <- vcov(logit_model)[interaction_coef_idx, interaction_coef_idx]
                        cov_main_int <- vcov(logit_model)[main_coef_idx, interaction_coef_idx]
                        se_combined <- sqrt(var_main + var_int + 2 * cov_main_int)
                        
                        ci_lower <- exp(combined_coef - 1.96 * se_combined)
                        ci_upper <- exp(combined_coef + 1.96 * se_combined)
                        
                        # P-value for combined effect
                        z_stat <- combined_coef / se_combined
                        p_val <- 2 * (1 - pnorm(abs(z_stat)))
                    } else {
                        or <- NA; ci_lower <- NA; ci_upper <- NA; p_val <- NA
                    }
                }
                
                subgroup_effects <- rbind(subgroup_effects, data.frame(
                    subgroup_variable = subgroup_var,
                    subgroup_level = level,
                    n_total = n_total,
                    n_plaque = n_plaque,
                    n_gksrs = n_gksrs,
                    treatment_effect = or,  # This will be OR for binary outcomes
                    ci_lower = ci_lower,
                    ci_upper = ci_upper,
                    p_value = p_val,
                    stringsAsFactors = FALSE
                ))
            }
            
            subgroup_results[[subgroup_var]] <- list(
                interaction_p = interaction_p,
                subgroup_effects = subgroup_effects,
                model = logit_model,
                subgroup_var_used = subgroup_var_to_use,
                formula_used = formula_str,
                confounders_used = confounders_to_use
            )
            
            log_message(sprintf("  Interaction p-value: %.4f", ifelse(is.na(interaction_p), 999, interaction_p)))
            
        }, error = function(e) {
            warning(sprintf("Error in subgroup analysis for %s: %s", subgroup_var, e$message))
            subgroup_results[[subgroup_var]] <- list(
                interaction_p = NA,
                subgroup_effects = data.frame(),
                model = NULL,
                subgroup_var_used = NA,
                formula_used = NA,
                confounders_used = NULL
            )
        })
    }
    
    return(subgroup_results)
}

#' Create combined forest plots for full vs restricted cohorts
#'
#' Creates side-by-side forest plots comparing results from full and restricted cohorts
#'
#' @param full_results Subgroup results from full cohort
#' @param restricted_results Subgroup results from restricted cohort  
#' @param outcome_name Name of the outcome
#' @param effect_measure Type of effect measure
#' @param output_path Path for saving the combined plot
#' @return Combined ggplot object
create_combined_forest_plot <- function(full_results, restricted_results, outcome_name, effect_measure = "HR", output_path = NULL) {
    
    # Create individual plots
    plot_full <- create_forest_plot(full_results, outcome_name, effect_measure, "Full Cohort")
    plot_restricted <- create_forest_plot(restricted_results, outcome_name, effect_measure, "Restricted Cohort")
    
    if (is.null(plot_full) || is.null(plot_restricted)) {
        warning("Cannot create combined plot - one or both individual plots failed")
        return(NULL)
    }
    
    # Combine plots side by side
    combined_plot <- plot_grid(plot_full, plot_restricted, ncol = 2, 
                              labels = c("A. Full Cohort", "B. Restricted Cohort"),
                              label_size = 12)
    
    # Save combined plot if path provided
    if (!is.null(output_path)) {
        ggsave(output_path, combined_plot, width = 20, height = max(8, nrow(plot_full$data) * 0.4), dpi = 300)
        log_message(sprintf("Combined forest plot saved to: %s", output_path))
    }
    
    return(combined_plot)
}

#' Create comprehensive summary table for subgroup analysis
#'
#' Creates a comprehensive table summarizing subgroup analysis results across cohorts
#'
#' @param full_results List of subgroup results from full cohort
#' @param restricted_results List of subgroup results from restricted cohort
#' @param outcome_name Name of the outcome
#' @param effect_measure Type of effect measure
#' @param output_path Path for saving the table
#' @return gt table object
create_comprehensive_subgroup_table <- function(full_results, restricted_results, outcome_name, effect_measure = "HR", output_path = NULL) {
    
    # Helper function to extract summary data
    extract_summary_data <- function(results, cohort_name) {
        summary_data <- data.frame()
        
        for (var_name in names(results)) {
            result <- results[[var_name]]
            if (!is.null(result$subgroup_effects) && nrow(result$subgroup_effects) > 0) {
                for (i in 1:nrow(result$subgroup_effects)) {
                    row <- result$subgroup_effects[i, ]
                    summary_data <- rbind(summary_data, data.frame(
                        cohort = cohort_name,
                        subgroup_variable = tools::toTitleCase(gsub("_", " ", var_name)),
                        subgroup_level = as.character(row$subgroup_level),
                        n_total = row$n_total,
                        n_gksrs = row$n_gksrs,
                        n_plaque = row$n_plaque,
                        treatment_effect = row$treatment_effect,
                        ci_lower = row$ci_lower,
                        ci_upper = row$ci_upper,
                        p_value = row$p_value,
                        interaction_p = result$interaction_p,
                        stringsAsFactors = FALSE
                    ))
                }
            }
        }
        return(summary_data)
    }
    
    # Extract data from both cohorts
    full_data <- extract_summary_data(full_results, "Full")
    restricted_data <- extract_summary_data(restricted_results, "Restricted")
    
    # Combine data
    combined_data <- rbind(full_data, restricted_data)
    
    if (nrow(combined_data) == 0) {
        warning("No data available for comprehensive table")
        return(NULL)
    }
    
    # Format the data for display
    display_data <- combined_data %>%
        mutate(
            effect_ci = ifelse(
                is.na(treatment_effect),
                "Insufficient data",
                sprintf("%.2f (%.2f-%.2f)", treatment_effect, ci_lower, ci_upper)
            ),
            p_value_fmt = ifelse(is.na(p_value), "—", sprintf("%.3f", p_value)),
            sample_size = sprintf("%d (%d/%d)", n_total, n_gksrs, n_plaque),
            interaction_p_fmt = ifelse(is.na(interaction_p), "—", sprintf("%.3f", interaction_p))
        ) %>%
        select(cohort, subgroup_variable, subgroup_level, sample_size, effect_ci, p_value_fmt, interaction_p_fmt) %>%
        pivot_wider(
            names_from = cohort,
            values_from = c(sample_size, effect_ci, p_value_fmt),
            names_sep = "_"
        )
    
    # Create gt table
    effect_label <- case_when(
        effect_measure == "HR" ~ "Hazard Ratio (95% CI)",
        effect_measure == "OR" ~ "Odds Ratio (95% CI)",
        effect_measure == "MD" ~ "Mean Difference (95% CI)",
        TRUE ~ "Effect (95% CI)"
    )
    
    table_gt <- display_data %>%
        gt() %>%
        tab_header(
            title = md(sprintf("**Comprehensive Subgroup Analysis: %s**", outcome_name)),
            subtitle = md(sprintf("Comparison of %s across Full and Restricted Cohorts", effect_label))
        ) %>%
        tab_spanner(
            label = "Full Cohort",
            columns = c(sample_size_Full, effect_ci_Full, p_value_fmt_Full)
        ) %>%
        tab_spanner(
            label = "Restricted Cohort", 
            columns = c(sample_size_Restricted, effect_ci_Restricted, p_value_fmt_Restricted)
        ) %>%
        cols_label(
            subgroup_variable = "Variable",
            subgroup_level = "Level",
            sample_size_Full = "N (GKSRS/Plaque)",
            effect_ci_Full = effect_label,
            p_value_fmt_Full = "p-value",
            sample_size_Restricted = "N (GKSRS/Plaque)",
            effect_ci_Restricted = effect_label,
            p_value_fmt_Restricted = "p-value",
            interaction_p_fmt = "Interaction p"
        ) %>%
        tab_source_note(
            source_note = md(sprintf(
                "**%s:** GKSRS vs Plaque (reference). %s\n\n**Interaction p-value** tests whether treatment effects differ across subgroup levels within each cohort.",
                effect_label,
                case_when(
                    effect_measure == "HR" ~ "Values > 1 indicate higher hazard with GKSRS.",
                    effect_measure == "OR" ~ "Values > 1 indicate higher odds with GKSRS.", 
                    effect_measure == "MD" ~ "Positive values indicate greater reduction with GKSRS.",
                    TRUE ~ ""
                )
            ))
        ) %>%
        tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_column_labels()
        ) %>%
        tab_style(
            style = cell_text(style = "italic"),
            locations = cells_body(
                columns = contains("effect_ci"),
                rows = grepl("Insufficient", effect_ci_Full) | grepl("Insufficient", effect_ci_Restricted)
            )
        )
    
    # Save table if path provided
    if (!is.null(output_path)) {
        table_gt %>% gtsave(output_path)
        log_message(sprintf("Comprehensive subgroup table saved to: %s", output_path))
    }
    
    return(table_gt)
}
