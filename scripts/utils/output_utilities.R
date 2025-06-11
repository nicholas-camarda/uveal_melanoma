# Output Utilities and Directory Management Functions
# Author: Nicholas Camarda
# Description: Functions for creating output directories and merging tables

# Unused apply_factor_level_indentation function removed - main workflow now uses bold_labels() and italicize_levels() from gtsummary

# Unused complex styling functions removed - main workflow now uses bold_labels() and italicize_levels() from gtsummary

#' Create organized output directory structure based on study objectives
#'
#' Creates directory structure organized by cohort first, then by study objectives
#' Structure: cohort -> objective -> sub-objectives
#' Includes dedicated directories for proportional hazards assumption diagnostics
#'
#' @param cohort_dir Base directory for this specific cohort
#' @return List of created directory paths
#' @examples
#' create_output_structure("final_data/Analysis/full_cohort")
create_output_structure <- function(cohort_dir) {
    # Define cohort-specific objective-based directory structure
    dirs <- list(
        # OBJECTIVE 1: Efficacy of Plaque vs GKSRS
        obj1_recurrence = file.path(cohort_dir, "01_Efficacy", "a_recurrence"),
        obj1_mets = file.path(cohort_dir, "01_Efficacy", "b_metastatic_progression"),
        obj1_os = file.path(cohort_dir, "01_Efficacy", "c_overall_survival"),
        obj1_pfs = file.path(cohort_dir, "01_Efficacy", "d_progression_free_survival"),
        obj1_ph_diagnostics = file.path(cohort_dir, "01_Efficacy", "h_proportional_hazards_diagnostics"),
        obj1_height_primary = file.path(cohort_dir, "01_Efficacy", "e_tumor_height_primary"),
        obj1_height_sensitivity = file.path(cohort_dir, "01_Efficacy", "f_tumor_height_sensitivity"),
        obj1_subgroup_primary = file.path(cohort_dir, "01_Efficacy", "g_subgroup_analysis", "tumor_height_primary"),
        obj1_subgroup_sensitivity = file.path(cohort_dir, "01_Efficacy", "g_subgroup_analysis", "tumor_height_sensitivity"),
        obj1_subgroup_clinical = file.path(cohort_dir, "01_Efficacy", "g_subgroup_analysis", "clinical_outcomes"),
        obj1_forest_plots = file.path(cohort_dir, "01_Efficacy", "g_subgroup_analysis", "forest_plots"),
        
        # OBJECTIVE 2: Safety/Toxicity of Plaque vs GKSRS
        obj2_vision = file.path(cohort_dir, "02_Safety", "a_vision_changes"),
        obj2_retinopathy = file.path(cohort_dir, "02_Safety", "b_retinopathy"),
        obj2_nvg = file.path(cohort_dir, "02_Safety", "c_neovascular_glaucoma"),
        obj2_srd = file.path(cohort_dir, "02_Safety", "d_serous_retinal_detachment"),
        
        # OBJECTIVE 3: Efficacy of Repeat Radiation
        obj3_pfs2 = file.path(cohort_dir, "03_Repeat_Radiation", "a_pfs2"),
        obj3_ph_diagnostics = file.path(cohort_dir, "03_Repeat_Radiation", "b_proportional_hazards_diagnostics"),
        
        # OBJECTIVE 4: GEP Predictive Accuracy
        obj4_mfs = file.path(cohort_dir, "04_GEP_Validation", "a_metastasis_free_survival"),
        obj4_mss = file.path(cohort_dir, "04_GEP_Validation", "b_melanoma_specific_survival"),
        
        # Cross-cutting analyses (baseline characteristics go here for each cohort)
        baseline_characteristics = file.path(cohort_dir, "00_General", "baseline_characteristics"),
        treatment_duration = file.path(cohort_dir, "00_General", "treatment_duration"),
        
        # Maintain backwards compatibility with old names for existing code
        recurrence = file.path(cohort_dir, "01_Efficacy", "a_recurrence"),
        mets = file.path(cohort_dir, "01_Efficacy", "b_metastatic_progression"),
        os = file.path(cohort_dir, "01_Efficacy", "c_overall_survival"),
        pfs = file.path(cohort_dir, "01_Efficacy", "d_progression_free_survival"),
        height_primary = file.path(cohort_dir, "01_Efficacy", "e_tumor_height_primary"),
        height_sensitivity = file.path(cohort_dir, "01_Efficacy", "f_tumor_height_sensitivity"),
        subgroup_primary = file.path(cohort_dir, "01_Efficacy", "g_subgroup_analysis", "tumor_height_primary"),
        subgroup_sensitivity = file.path(cohort_dir, "01_Efficacy", "g_subgroup_analysis", "tumor_height_sensitivity"),
        vision = file.path(cohort_dir, "02_Safety", "a_vision_changes"),
        retinopathy = file.path(cohort_dir, "02_Safety", "b_retinopathy"),
        nvg = file.path(cohort_dir, "02_Safety", "c_neovascular_glaucoma"),
        srg = file.path(cohort_dir, "02_Safety", "d_serous_retinal_detachment")
    )
    
    # Create all directories
    for (dir_name in names(dirs)) {
        dir_path <- dirs[[dir_name]]
        if (!dir.exists(dir_path)) {
            dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
            if (exists("USE_LOGS") && USE_LOGS) {
                log_enhanced(sprintf("Created directory: %s", dir_path), level = "INFO")
            }
        }
    }
    
    return(dirs)
}

#' Merge baseline characteristics tables from full and restricted cohorts
#'
#' Creates a merged table comparing baseline characteristics between full and restricted cohorts
#' using gtsummary's built-in functions for clean, publication-ready output.
#'
#' @param full_cohort_data Data frame containing full cohort data
#' @param restricted_cohort_data Data frame containing restricted cohort data
#' @param output_path Directory where merged tables should be saved
#' @return Invisibly returns NULL
#'
#' @examples
#' merge_cohort_tables(full_data, restricted_data, "final_data/Analysis/merged_tables/")
merge_cohort_tables <- function(full_cohort_data, restricted_cohort_data, output_path = NULL) {
    
    log_enhanced("=== STARTING TABLE MERGING: Full and Restricted Cohorts ===", level = "INFO")
    
    # Set default output path if not provided
    if (is.null(output_path)) {
        output_path <- file.path("final_data", "Analysis", "merged_tables")
    }
    
    # Create output directory
    if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
        log_enhanced(sprintf("Created merged tables directory: %s", output_path))
    }
    
    log_enhanced(sprintf("Merging tables will be saved to: %s", output_path))
    
    # Use globally defined variables for baseline characteristics summary
    vars_to_summarize <- BASELINE_VARIABLES_TO_SUMMARIZE
    
    # Get variable labels for human-readable display
    variable_labels <- get_variable_labels()
    
    tryCatch({
        # Create baseline table for full cohort
        full_baseline <- full_cohort_data %>%
            select(all_of(vars_to_summarize), treatment_group) %>%
            tbl_summary(
                by = treatment_group,
                missing = "no",
                label = variable_labels,
                statistic = list(
                    all_continuous() ~ "{mean} ({sd})",
                    all_categorical() ~ "{n} ({p}%)"
                )
            ) %>%
            add_overall() %>%
            add_p(test = list(all_categorical() ~ "fisher.test"), 
                  test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))) %>%
            bold_labels() %>%
            modify_header(
                label = "**Characteristic**",
                stat_0 = "**Overall**\nN = {N}",
                stat_1 = "**Plaque**\nN = {n}",
                stat_2 = "**GKSRS**\nN = {n}",
                p.value = "**p-value**"
            )
        
        # Create baseline table for restricted cohort
        restricted_baseline <- restricted_cohort_data %>%
            select(all_of(vars_to_summarize), treatment_group) %>%
            tbl_summary(
                by = treatment_group,
                missing = "no",
                label = variable_labels,
                statistic = list(
                    all_continuous() ~ "{mean} ({sd})",
                    all_categorical() ~ "{n} ({p}%)"
                )
            ) %>%
            add_overall() %>%
            add_p(test = list(all_categorical() ~ "fisher.test"), 
                  test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))) %>%
            bold_labels() %>%
            modify_header(
                label = "**Characteristic**",
                stat_0 = "**Overall**\nN = {N}",
                stat_1 = "**Plaque**\nN = {n}",
                stat_2 = "**GKSRS**\nN = {n}",
                p.value = "**p-value**"
            )
        
        # Merge tables side by side
        merged_table <- tbl_merge(
            tbls = list(full_baseline, restricted_baseline),
            tab_spanner = c("**Full Cohort**", "**Restricted Cohort**")
        ) %>%
            modify_caption("**Table 1: Baseline Characteristics**")
        
        # Save as HTML
        save_gt_html(
            merged_table,
            filename = file.path(output_path, "merged_baseline_characteristics.html")
        )
        
        # Save as Excel
        merged_table %>%
            as_tibble() %>%
            writexl::write_xlsx(
                path = file.path(output_path, "merged_baseline_characteristics.xlsx")
            )
        
        log_enhanced("Saved merged baseline characteristics table (Excel and HTML)")
        
    }, error = function(e) {
        log_enhanced(sprintf("Error merging baseline tables: %s", e$message))
        log_enhanced("Skipping baseline table merge", level = "INFO")
    })
    
    # Summary message
    log_enhanced("=== COMPLETED TABLE MERGING ===", level = "INFO")
    log_enhanced(sprintf("Merged baseline characteristics table saved to: %s", output_path))
    log_enhanced("Files created: merged_baseline_characteristics.xlsx and merged_baseline_characteristics.html", level = "INFO")
    
    return(invisible(NULL))
}

#' Create All Combined Forest Plots and Summary Tables
#'
#' Creates combined forest plots and summary tables for all outcomes across cohorts.
#' This function is called by main.R at the end of analysis to create combined visualizations.
#'
#' @param base_dir Base directory where individual cohort results are stored
#' @param cohort_names Character vector of cohort names (e.g., c("full", "restricted"))
#' @return List of created plots and tables
#'
#' @examples
#' create_all_combined_forest_plots("final_data", c("full", "restricted"))
create_all_combined_forest_plots <- function(base_dir, cohort_names = c("full", "restricted")) {
    
    log_enhanced("Creating all combined forest plots and summary tables", level = "INFO")
    
    # Create output directory for combined plots
    combined_output_dir <- file.path(base_dir, "Analysis", "combined_cohorts")
    if (!dir.exists(combined_output_dir)) {
        dir.create(combined_output_dir, recursive = TRUE, showWarnings = FALSE)
        log_enhanced(sprintf("Created combined output directory: %s", combined_output_dir), level = "INFO", indent = 1)
    }
    
    # Track results
    results <- list()
    
    # Primary outcomes to process
    primary_outcomes <- c(
        "local_recurrence" = "Local Recurrence",
        "metastatic_progression" = "Metastatic Progression", 
        "overall_survival" = "Overall Survival",
        "progression_free_survival" = "Progression-Free Survival"
    )
    
    # Process each primary outcome
    for (outcome_key in names(primary_outcomes)) {
        outcome_name <- primary_outcomes[outcome_key]
        
        tryCatch({
            log_enhanced(sprintf("Processing combined plots for %s", outcome_name), level = "INFO", indent = 1)
            
            # Load subgroup results from both cohorts
            full_results <- NULL
            restricted_results <- NULL
            
            # Attempt to load results from the most recent test output
            test_dirs <- list.dirs("test_output", recursive = FALSE)
            if (length(test_dirs) > 0) {
                latest_test_dir <- test_dirs[length(test_dirs)]
                
                # Look for subgroup results files
                full_file <- file.path(latest_test_dir, "comprehensive", "primary_outcomes", "subgroup_analysis", paste0("full_", outcome_key, "_subgroup_results.rds"))
                restricted_file <- file.path(latest_test_dir, "comprehensive", "primary_outcomes", "subgroup_analysis", paste0("restricted_", outcome_key, "_subgroup_results.rds"))
                
                if (file.exists(full_file)) {
                    full_results <- readRDS(full_file)
                    log_enhanced(sprintf("Loaded full cohort results for %s", outcome_name), level = "INFO", indent = 2)
                }
                
                if (file.exists(restricted_file)) {
                    restricted_results <- readRDS(restricted_file)
                    log_enhanced(sprintf("Loaded restricted cohort results for %s", outcome_name), level = "INFO", indent = 2)
                }
            }
            
            # Create combined forest plot if both results are available
            if (!is.null(full_results) && !is.null(restricted_results)) {
                
                # Determine effect measure
                effect_measure <- ifelse(outcome_key %in% c("overall_survival", "progression_free_survival"), "HR", "OR")
                
                # Use consistent variable order for all forest plots
                variable_order <- FOREST_PLOT_VARIABLE_ORDER
                
                # Create combined forest plot
                combined_plot <- create_combined_forest_plot(
                    full_results = full_results,
                    restricted_results = restricted_results,
                    outcome_name = outcome_name,
                    treatment_labels = c("GKSRS", "Plaque"),
                    variable_order = variable_order,
                    effect_measure = effect_measure,
                    favours_labels = c("Favours GKSRS", "Favours Plaque")
                )
                
                # Save the plot
                plot_path <- file.path(combined_output_dir, paste0("combined_", outcome_key, "_forest_plot.png"))
                png(plot_path, width = 14, height = 10, units = "in", res = 300)
                print(combined_plot)
                dev.off()
                
                log_enhanced(sprintf("Combined forest plot saved: %s", plot_path), level = "INFO", indent = 2)
                
                results[[outcome_key]] <- list(
                    plot = combined_plot,
                    path = plot_path,
                    status = "success"
                )
                
            } else {
                log_enhanced(sprintf("Skipping %s - missing subgroup results", outcome_name), level = "WARN", indent = 2)
                results[[outcome_key]] <- list(
                    status = "skipped",
                    reason = "missing_data"
                )
            }
            
        }, error = function(e) {
            log_enhanced(sprintf("Error creating combined plot for %s: %s", outcome_name, e$message), level = "ERROR", indent = 2)
            results[[outcome_key]] <- list(
                status = "error",
                error = e$message
            )
        })
    }
    
    # Summary
    successful_plots <- sum(sapply(results, function(x) x$status == "success"))
    total_plots <- length(results)
    
    log_enhanced(sprintf("Combined forest plots completed: %d/%d successful", successful_plots, total_plots), level = "INFO")
    
    return(results)
}

#' Save GT table with automatic factor level indentation formatting
#'
#' This is a wrapper around gt::gtsave that automatically applies factor level
#' indentation to HTML tables for consistent formatting across the entire codebase.
#' 
#' @param table A gt table object OR gtsummary table object
#' @param filename File path where to save the HTML table
#' @param ... Additional arguments passed to gt::gtsave
#' @return Invisibly returns the filename
save_gt_html <- function(table, filename, ...) {
    # Simple save function - gtsummary tables with bold_labels() already applied
    # will automatically have proper formatting when converted to gt
    if (inherits(table, "gtsummary")) {
        gt::gtsave(table %>% as_gt(), filename = filename, ...)
    } else {
        gt::gtsave(table, filename = filename, ...)
    }
    
    invisible(filename)
}

#' Clean table headers by removing markdown formatting
clean_table_headers <- function(gt_table) {
    # Get the column labels
    table_data <- gt_table[["_boxhead"]]
    
    if (!is.null(table_data)) {
        # Clean up column labels by removing markdown asterisks
        for (i in 1:nrow(table_data)) {
            if (!is.na(table_data$column_label[i])) {
                # Remove ** from column labels
                clean_label <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", table_data$column_label[i])
                table_data$column_label[i] <- clean_label
            }
        }
        
        # Update the table
        gt_table[["_boxhead"]] <- table_data
    }
    
    return(gt_table)
}

#' Apply publication-ready styling to gt tables
apply_publication_styling <- function(gt_table) {
    gt_table %>%
        tab_options(
            table.font.size = px(12),
            heading.title.font.size = px(14),
            column_labels.font.weight = "bold",
            column_labels.background.color = "#f8f9fa",
            table.border.top.width = px(2),
            table.border.bottom.width = px(2),
            table.border.top.color = "#000000",
            table.border.bottom.color = "#000000",
            column_labels.border.bottom.width = px(1),
            column_labels.border.bottom.color = "#000000"
        ) %>%
        # Style column headers - bold black text on light background
        tab_style(
            style = list(
                cell_text(weight = "bold", color = "black"),
                cell_fill(color = "#f8f9fa")
            ),
            locations = cells_column_labels()
        )
}