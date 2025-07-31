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
analyze_tumor_height_changes <- function(data, other_map = list()) {
    # Use height_change variable that was already calculated in data_processing.R
    data_with_height_change <- enforce_unordered_factors(data)

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
    tbl_summary_obj <- data_with_height_change %>%
        select(treatment_group, height_change) %>%
        tbl_summary(
            by = treatment_group,
            missing = "no",
            label = get_variable_labels(),
            statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
            )
        ) %>%
        add_overall() %>%
        add_p(test = list(all_continuous() ~ "wilcox.test")) %>%
        bold_labels() %>%
        modify_header(
            label = "**Characteristic**",
            stat_0 = "**Overall**\nN = {N}",
            stat_1 = "**Plaque**\nN = {n}",
            stat_2 = "**GKSRS**\nN = {n}",
            p.value = "**p-value**"
        ) %>%
        modify_caption("Tumor Height Changes Analysis") %>%
        as_gt()
    
    # Save table
    save_gt_html(
        tbl_summary_obj,
        filename = file.path(output_dirs$obj1_height_primary, paste0(prefix, "height_changes.html"))
    )
    
    # PRIMARY ANALYSIS: Linear regression WITHOUT initial tumor height adjustment
    log_enhanced("Fitting PRIMARY linear regression model for tumor height changes (without baseline height adjustment)")
    
    # Use the unified table generation system for primary analysis
    primary_result <- generate_regression_table(
        data = data_with_height_change,
        outcome_var = "height_change",
        predictor_vars = "treatment_group",
        confounders = confounders,
        model_type = "linear",
        effect_measure = "MD",  # Mean Difference for continuous outcome
        analysis_name = "height_change_primary",
        dataset_name = "tumor_height",
        output_dir = output_dirs$obj1_height_primary,
        prefix = prefix,
        # handle_rare = TRUE, # REMOVED
        other_map = other_map
    )
    
    primary_height_lm <- primary_result$model
    primary_height_lm_tbl <- primary_result$table

    # SENSITIVITY ANALYSIS: Linear regression WITH initial tumor height adjustment
    log_enhanced("Fitting SENSITIVITY linear regression model for tumor height changes (with baseline height adjustment)")
    
    # Use the unified table generation system for sensitivity analysis
    sensitivity_result <- generate_regression_table(
        data = data_with_height_change,
        outcome_var = "height_change",
        predictor_vars = "treatment_group",
        confounders = c(confounders, "initial_tumor_height"),
        model_type = "linear",
        effect_measure = "MD",  # Mean Difference for continuous outcome
        analysis_name = "height_change_sensitivity",
        dataset_name = "tumor_height",
        output_dir = output_dirs$obj1_height_sensitivity,
        prefix = prefix,
        # handle_rare = TRUE, # REMOVED
        other_map = other_map
    )
    
    sensitivity_height_lm <- sensitivity_result$model
    sensitivity_height_lm_tbl <- sensitivity_result$table

    return(list(
        changes = height_changes,
        table = tbl_summary_obj,
        primary_regression_model = primary_height_lm,
        primary_regression_table = primary_height_lm_tbl,
        sensitivity_regression_model = sensitivity_height_lm,
        sensitivity_regression_table = sensitivity_height_lm_tbl
    ))
} 