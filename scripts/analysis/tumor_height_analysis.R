# Tumor Height Analysis Functions
# Author: Nicholas Camarda
# Description: Functions specific to tumor height change analysis and subgroup interactions

#' Analyze tumor height reduction
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
    tbl_summary_obj <- data_with_height_change %>%
        select(treatment_group, height_change) %>%
        tbl_summary(
            missing = "no",
            by = treatment_group,
            type = list(height_change ~ "continuous"),
            statistic = list(height_change ~ "{mean} ({sd})"),
            digits = list(height_change ~ 1)
        ) %>%
        add_p(test = list(all_continuous() ~ "wilcox.test"), quiet = TRUE) %>%
        modify_header(quiet = TRUE) %>%
        modify_caption("Change in Tumor Height (Initial - Last Measured or Pre-Retreatment), by Treatment Group") %>%
        modify_footnote(
            update = all_stat_cols() ~ "Mean (SD)"
        )
    
    # Convert to gt table
    tbl <- tbl_summary_obj %>%
        as_gt()
    
    # Save table
    save_gt_html(
        tbl,
        filename = file.path(output_dirs$obj1_height_primary, paste0(prefix, "height_changes.html"))
    )
    
    # PRIMARY ANALYSIS: Linear regression WITHOUT initial tumor height adjustment
    log_enhanced("Fitting PRIMARY linear regression model for tumor height changes (without baseline height adjustment)")
    primary_height_lm <- lm(height_change ~ treatment_group + recurrence1, data = data_with_height_change)
    
    # Get variable labels for better readability
    variable_labels <- get_variable_labels()
    
    primary_height_lm_tbl <- tbl_regression(primary_height_lm,
        exponentiate = FALSE,
        intercept = FALSE,
        quiet = TRUE,
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
        )
    
    # Save primary table
    save_gt_html(
        primary_height_lm_tbl %>% as_gt(),
        filename = file.path(output_dirs$obj1_height_primary, paste0(prefix, "height_lm_primary.html"))
    )

    # SENSITIVITY ANALYSIS: Linear regression WITH initial tumor height adjustment
    log_enhanced("Fitting SENSITIVITY linear regression model for tumor height changes (with baseline height adjustment)")
    sensitivity_height_lm <- lm(height_change ~ treatment_group + recurrence1 + initial_tumor_height, data = data_with_height_change)
    
    sensitivity_height_lm_tbl <- tbl_regression(sensitivity_height_lm,
        exponentiate = FALSE,
        intercept = FALSE,
        quiet = TRUE,
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
        )
    
    # Save sensitivity table
    save_gt_html(
        sensitivity_height_lm_tbl %>% as_gt(),
        filename = file.path(output_dirs$obj1_height_sensitivity, paste0(prefix, "height_lm_sensitivity.html"))
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