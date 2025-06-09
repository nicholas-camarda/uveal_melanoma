# Vision and Safety Analysis Functions
# Author: Nicholas Camarda
# Description: Functions for vision change and radiation sequelae analysis

#' Analyze visual acuity changes
#'
#' Calculates and summarizes changes in visual acuity by treatment group, returning summary statistics and a table.
#' Simple analysis as required by objective 2a - no subgroup interactions needed.
#'
#' @param data Data frame with vision variables.
#'
#' @return List with elements: changes (summary data frame), table (gtsummary object), regression_model (lm object), regression_table (gtsummary object).
#' @examples
#' analyze_visual_acuity_changes(data)
analyze_visual_acuity_changes <- function(data) {
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
            digits = list(vision_change ~ 2)
        ) %>%
        add_p(test = list(all_continuous() ~ "wilcox.test"), quiet = TRUE) %>%
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
        quiet = TRUE,
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

#' Analyze radiation complications
#'
#' Analyzes rates of radiation complications (retinopathy, nvg, srg) by treatment group.
#' Reuses the existing analyze_binary_outcome_rates function for consistency.
#'
#' @param data Data frame with radiation sequelae variables
#' @param sequela_type Type of sequela to analyze ("retinopathy", "nvg", or "srg")
#' @param confounders Character vector of confounders for adjustment
#' @param dataset_name Name of the dataset for output files
#'
#' @return Results from analyze_binary_outcome_rates function
#' @examples
#' analyze_radiation_complications(data, "retinopathy", confounders, "uveal_full")
analyze_radiation_complications <- function(data, sequela_type, confounders = NULL, dataset_name = NULL) {
    
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
        add_p(quiet = TRUE) %>%  # Use gtsummary default test selection
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
            quiet = TRUE,
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