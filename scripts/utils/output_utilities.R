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
        # OBJECTIVE 1: Efficacy of PBT vs GKSRS
        obj1_recurrence = file.path(cohort_dir, "01_Efficacy", "a_recurrence"),
        obj1_mets = file.path(cohort_dir, "01_Efficacy", "b_metastatic_progression"),
        obj1_os = file.path(cohort_dir, "01_Efficacy", "c_overall_survival"),
        obj1_pfs = file.path(cohort_dir, "01_Efficacy", "d_progression_free_survival"),
        obj1_ph_diagnostics = file.path(cohort_dir, "01_Efficacy", "h_proportional_hazards_diagnostics"),
        obj1_height_primary = file.path(cohort_dir, "01_Efficacy", "e_tumor_height_primary"),
        obj1_height_sensitivity = file.path(cohort_dir, "01_Efficacy", "f_tumor_height_sensitivity"),
        obj1_subgroup_primary = file.path(cohort_dir, "01_Efficacy", "g_subgroup_analysis", "tumor_height_primary"),
        obj1_subgroup_sensitivity = file.path(cohort_dir, "01_Efficacy", "g_subgroup_analysis", "tumor_height_sensitivity"),
        # obj1_subgroup_clinical = file.path(cohort_dir, "01_Efficacy", "g_subgroup_analysis", "clinical_outcomes"),
        obj1_forest_plots = file.path(cohort_dir, "01_Efficacy", "g_subgroup_analysis", "forest_plots"),

        # OBJECTIVE 2: Safety/Toxicity of PBT vs GKSRS
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
        obj4_ph_diagnostics = file.path(cohort_dir, "04_GEP_Validation", "c_proportional_hazards_diagnostics"),

        # Cross-cutting analyses (baseline characteristics go here for each cohort)
        baseline_characteristics = file.path(cohort_dir, "00_General", "baseline_characteristics"),
        treatment_duration = file.path(cohort_dir, "00_General", "treatment_duration")
    )

    # Create all directories
    for (dir_name in names(dirs)) {
        dir_path <- dirs[[dir_name]]
        if (!dir.exists(dir_path)) {
            dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
            if (exists("USE_LOGS") && USE_LOGS) {
                logger::log_debug(formatted(sprintf("Created directory: %s", dir_path)))
            }
        }
    }


    return(dirs)
}

#' Apply standardized level formatting to categorical display variables
#'
#' Ensures merged tables reuse the same label mappings and underscore cleanup
#' applied when individual cohort tables are generated.
format_levels_for_display <- function(data) {
    formatted <- data

    if (exists("STANDARD_LEVEL_LABELS", inherits = TRUE)) {
        label_maps <- get("STANDARD_LEVEL_LABELS", inherits = TRUE)
        for (var in names(label_maps)) {
            if (var %in% names(formatted) && (is.factor(formatted[[var]]) || is.character(formatted[[var]]))) {
                current_levels <- levels(factor(formatted[[var]]))
                rename_vec <- setNames(current_levels, current_levels)
                for (lvl in names(label_maps[[var]])) {
                    rename_vec[lvl] <- label_maps[[var]][[lvl]]
                }
                formatted[[var]] <- factor(rename_vec[as.character(formatted[[var]])], levels = unique(rename_vec))
            }
        }
    }

    if (exists("AUTO_CLEAN_LEVELS", inherits = TRUE) && isTRUE(get("AUTO_CLEAN_LEVELS", inherits = TRUE))) {
        factor_cols <- names(formatted)[sapply(formatted, is.factor)]
        for (col in factor_cols) {
            levels(formatted[[col]]) <- gsub("_", " ", levels(formatted[[col]]))
        }
    }

    # Drop unused factor levels (especially Stage 4 which has 0 patients)
    factor_cols <- names(formatted)[sapply(formatted, is.factor)]
    for (col in factor_cols) {
        formatted[[col]] <- droplevels(formatted[[col]])
    }

    formatted
}

#' Reapply pre-collapsed factor levels when available
#'
apply_precollapse_levels <- function(data, dataset_name = NULL) {
    if (is.null(dataset_name) || !exists("PROCESSED_DATA_DIR", inherits = TRUE)) {
        return(data)
    }

    precollapse_path <- file.path(PROCESSED_DATA_DIR, paste0(dataset_name, "_derived_precollapse.rds"))
    if (file.exists(precollapse_path)) {
        try({
            precollapse_data <- readRDS(precollapse_path)
            common_cols <- intersect(names(precollapse_data), names(data))
            if (length(common_cols) > 0) {
                data[common_cols] <- precollapse_data[common_cols]
            }
        }, silent = TRUE)
    }

    data
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
merge_cohort_tables <- function(full_cohort_data, restricted_cohort_data, output_path = NULL, dataset_names = list()) {
    logger::log_info("=== STARTING TABLE MERGING: Full and Restricted Cohorts ===")

    # Set default output path if not provided
    if (is.null(output_path)) {
        # MERGED_TABLES_DIR
        output_path <- MERGED_TABLES_DIR
    }

    # Create output directory
    if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
        logger::log_info(sprintf("Created merged tables directory: %s", output_path))
    }

            logger::log_info(sprintf("Merging tables will be saved to: %s", output_path))

    # Use globally defined variables for baseline characteristics summary
    vars_to_summarize <- BASELINE_VARIABLES_TO_SUMMARIZE

    # Get variable labels for human-readable display
    variable_labels <- get_variable_labels()

    # Helper function to filter variables with sufficient variation
    filter_variables_with_variation <- function(data, variables, by_var = "treatment_group") {
        filtered_vars <- c()
        
        for (var in variables) {
            if (var %in% names(data)) {
                # Check if variable has sufficient variation
                if (is.numeric(data[[var]])) {
                    # For numeric variables, check if there's variation
                    if (length(unique(data[[var]])) > 1) {
                        filtered_vars <- c(filtered_vars, var)
                    }
                } else {
                    # For categorical variables, check if there are at least 2 levels
                    # and if the by_var has sufficient variation
                    if (length(unique(data[[var]])) > 1) {
                        # Also check if the by_var has sufficient variation
                        if (by_var %in% names(data) && length(unique(data[[by_var]])) > 1) {
                            filtered_vars <- c(filtered_vars, var)
                        }
                    }
                }
            }
        }
        
        if (length(filtered_vars) < length(variables)) {
            removed_vars <- setdiff(variables, filtered_vars)
            logger::log_info(sprintf("Removed variables with insufficient variation: %s", paste(removed_vars, collapse = ", ")))
        }
        
        return(filtered_vars)
    }

    tryCatch(
        {
            # Restore pre-collapsed levels when files are available to mirror individual cohort tables
            full_cohort_data <- apply_precollapse_levels(full_cohort_data, dataset_names$full)
            restricted_cohort_data <- apply_precollapse_levels(restricted_cohort_data, dataset_names$restricted)

            # Align factor display formatting with individual baseline tables
            full_cohort_formatted <- format_levels_for_display(full_cohort_data)
            restricted_cohort_formatted <- format_levels_for_display(restricted_cohort_data)

            # Filter variables with sufficient variation for full cohort
            full_available <- intersect(vars_to_summarize, names(full_cohort_formatted))
            full_available_filtered <- filter_variables_with_variation(full_cohort_formatted, full_available)

            # Create baseline table for full cohort
            full_baseline <- full_cohort_formatted %>%
                select(any_of(c(full_available_filtered, "treatment_group"))) %>%
                tbl_summary(
                    by = treatment_group,
                    missing = "no",
                    label = variable_labels[intersect(names(variable_labels), full_available_filtered)],
                    statistic = list(
                        all_continuous() ~ "{median} ({min}, {max})",
                        all_categorical() ~ "{n} ({p}%)"
                    ),
                    digits = list(all_continuous() ~ 1, all_categorical() ~ 0)
                ) %>%
                add_overall() %>%
                add_p(
                    test = list(all_categorical() ~ "fisher.test"),
                    test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))
                ) %>%
                bold_labels()

            # Add header modification with error handling for full cohort
            tryCatch({
                full_baseline <- full_baseline %>%
                    modify_header(
                        label = "**Characteristic**",
                        stat_0 = "**Overall**\nN = {N}",
                        stat_1 = "**PBT**\nN = {n}",
                        stat_2 = "**GKSRS**\nN = {n}",
                        p.value = "**p-value**"
                    )
            }, error = function(e) {
                logger::log_warn(sprintf("Warning: Could not modify headers for full cohort table: %s", e$message))
                logger::log_info("Proceeding with default headers for full cohort")
            })

            # Create baseline table for restricted cohort
            restricted_available <- intersect(vars_to_summarize, names(restricted_cohort_formatted))
            restricted_available_filtered <- filter_variables_with_variation(restricted_cohort_formatted, restricted_available)
            restricted_baseline <- restricted_cohort_formatted %>%
                select(any_of(c(restricted_available_filtered, "treatment_group"))) %>%
                tbl_summary(
                    by = treatment_group,
                    missing = "no",
                    label = variable_labels[intersect(names(variable_labels), restricted_available_filtered)],
                    statistic = list(
                        all_continuous() ~ "{median} ({min}, {max})",
                        all_categorical() ~ "{n} ({p}%)"
                    ),
                    digits = list(all_continuous() ~ 1, all_categorical() ~ 0)
                ) %>%
                add_overall() %>%
                add_p(
                    test = list(all_categorical() ~ "fisher.test"),
                    test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))
                ) %>%
                bold_labels()

            # Add header modification with error handling for restricted cohort
            tryCatch({
                restricted_baseline <- restricted_baseline %>%
                    modify_header(
                        label = "**Characteristic**",
                        stat_0 = "**Overall**\nN = {N}",
                        stat_1 = "**PBT**\nN = {n}",
                        stat_2 = "**GKSRS**\nN = {n}",
                        p.value = "**p-value**"
                    )
            }, error = function(e) {
                logger::log_warn(sprintf("Warning: Could not modify headers for restricted cohort table: %s", e$message))
                logger::log_info("Proceeding with default headers for restricted cohort")
            })

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

            logger::log_info("Saved merged baseline characteristics table (Excel and HTML)")
        },
        error = function(e) {
            logger::log_error(sprintf("Error merging baseline tables: %s", e$message))
            logger::log_info("Skipping baseline table merge")
        }
    )

    # Summary message
    logger::log_info("=== COMPLETED TABLE MERGING ===")
    logger::log_info(sprintf("Merged baseline characteristics table saved to: %s", output_path))
    logger::log_info("Files created: merged_baseline_characteristics.xlsx and merged_baseline_characteristics.html")

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
    logger::log_info("Creating all combined forest plots and summary tables")

    # Create output directory for combined plots
    combined_output_dir <- file.path(base_dir, "Analysis", "combined_cohorts")
    if (!dir.exists(combined_output_dir)) {
        dir.create(combined_output_dir, recursive = TRUE, showWarnings = FALSE)
        logger::log_info(formatted(sprintf("Created combined output directory: %s", combined_output_dir), indent = 1))
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

        tryCatch(
            {
                logger::log_info(formatted(sprintf("Processing combined plots for %s", outcome_name), indent = 1))

                # Load subgroup results from both cohorts
                full_results <- NULL
                restricted_results <- NULL

                # Attempt to load results from the most recent test output
                test_dirs <- list.dirs(TEST_OUTPUT_DIR, recursive = FALSE)
                if (length(test_dirs) > 0) {
                    latest_test_dir <- test_dirs[length(test_dirs)]

                    # Look for subgroup results files
                    full_file <- file.path(latest_test_dir, "comprehensive", "primary_outcomes", "subgroup_analysis", paste0("full_", outcome_key, "_subgroup_results.rds"))
                    restricted_file <- file.path(latest_test_dir, "comprehensive", "primary_outcomes", "subgroup_analysis", paste0("restricted_", outcome_key, "_subgroup_results.rds"))

                    if (file.exists(full_file)) {
                        full_results <- readRDS(full_file)
                        logger::log_info(formatted(sprintf("Loaded full cohort results for %s", outcome_name), indent = 2))
                    }

                    if (file.exists(restricted_file)) {
                        restricted_results <- readRDS(restricted_file)
                        logger::log_info(formatted(sprintf("Loaded restricted cohort results for %s", outcome_name), indent = 2))
                    }
                }

                # Create combined forest plot if both results are available
                if (!is.null(full_results) && !is.null(restricted_results)) {
                    # Determine effect measure
                    effect_measure <- ifelse(outcome_key %in% c("overall_survival", "progression_free_survival"), "HR", "OR")

                    # Create combined forest plot
                    combined_plot <- create_combined_forest_plot(
                        full_results = full_results,
                        restricted_results = restricted_results,
                        outcome_name = outcome_name,
                        treatment_labels = c("GKSRS", "PBT"),
                        variable_order = FOREST_PLOT_VARIABLE_ORDER,
                        effect_measure = effect_measure,
                        favours_labels = c("Favors GKSRS", "Favors PBT")
                    )

                    # Save the plot
                    plot_path <- file.path(combined_output_dir, paste0("combined_", outcome_key, "_forest_plot.png"))
                    png(plot_path, width = 14, height = 10, units = "in", res = 300)
                    plot(combined_plot)
                    dev.off()

                    logger::log_info(formatted(sprintf("Combined forest plot saved: %s", plot_path), indent = 2))

                    results[[outcome_key]] <- list(
                        plot = combined_plot,
                        path = plot_path,
                        status = "success"
                    )
                } else {
                    logger::log_warn(formatted(sprintf("Skipping %s - missing subgroup results", outcome_name), indent = 2))
                    results[[outcome_key]] <- list(
                        status = "skipped",
                        reason = "missing_data"
                    )
                }
            },
            error = function(e) {
                logger::log_error(formatted(sprintf("Error creating combined plot for %s: %s", outcome_name, e$message), indent = 2))
                results[[outcome_key]] <- list(
                    status = "error",
                    error = e$message
                )
            }
        )
    }

    # Summary
    successful_plots <- sum(sapply(results, function(x) x$status == "success"))
    total_plots <- length(results)

    logger::log_info(formatted(sprintf("Combined forest plots completed: %d/%d successful", successful_plots, total_plots)))

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

#' Clean gt table headers by removing markdown asterisks
#'
#' @param gt_table A gt table object
#' @return Modified gt table object with cleaned column labels
clean_table_headers <- function(gt_table) {
    # Get the column labels
    table_data <- gt_table[["_boxhead"]]

    if (!is.null(table_data)) {
        # Clean up column labels by removing markdown asterisks
        for (i in seq_len(nrow(table_data))) {
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
#'
#' @param gt_table A gt table object
#' @return Styled gt table object with standardized publication formatting
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

#' Write diagnostics data frame or list of data frames to an Excel workbook
#'
#' @param diagnostics Either a data.frame (single sheet) or named list of data.frames (multiple sheets)
#' @param file_path Full path of the .xlsx to create
#' @return Invisible NULL
write_analysis_diagnostics_excel <- function(diagnostics, file_path) {
    if (is.null(diagnostics) || length(diagnostics) == 0) {
        return(invisible(NULL))
    }
    if (is.data.frame(diagnostics)) {
        writexl::write_xlsx(list(Diagnostics = diagnostics), file_path)
    } else if (is.list(diagnostics)) {
        writexl::write_xlsx(diagnostics, file_path)
    } else {
        stop("diagnostics must be a data.frame or a named list of data.frames")
    }
}

## Note: subdirectory creation for GEP visuals is centralized above; plotting code should not mkdir

#' Merge recurrence and metastatic progression tables from full and restricted cohorts
#'
#' Creates a merged table comparing recurrence and metastatic progression rates between full and restricted cohorts
#' using gtsummary's built-in functions for clean, publication-ready output.
#' Follows the exact same pattern as merge_cohort_tables.
#'
#' @param full_cohort_data Data frame containing full cohort data
#' @param restricted_cohort_data Data frame containing restricted cohort data
#' @param output_path Directory where merged tables should be saved
#' @return Invisibly returns NULL
#'
#' @examples
#' merge_recurrence_metastatic_progression_tables(full_data, restricted_data, "final_data/Analysis/merged_tables/")
merge_recurrence_metastatic_progression_tables <- function(full_cohort_data, restricted_cohort_data, output_path = NULL) {
    logger::log_info("=== STARTING TABLE MERGING: Recurrence and Metastatic Progression ===")

    # Set default output path if not provided
    if (is.null(output_path)) {
        output_path <- MERGED_TABLES_DIR
    }

    # Create output directory
    if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
        logger::log_info(sprintf("Created merged tables directory: %s", output_path))
    }

    logger::log_info(sprintf("Merging tables will be saved to: %s", output_path))

    # Variables to summarize for recurrence and metastatic progression
    outcome_vars <- c("recurrence1", "mets_progression")
    
    # Get variable labels for human-readable display (use existing STANDARD_TABLE_LABELS)
    variable_labels <- get_variable_labels()

    tryCatch(
        {
            # Create baseline table for full cohort
            full_outcomes <- full_cohort_data %>%
                select(any_of(c(outcome_vars, "treatment_group"))) %>%
                tbl_summary(
                    by = treatment_group,
                    missing = "no",
                    label = variable_labels[intersect(names(variable_labels), outcome_vars)],
                    statistic = list(
                        all_categorical() ~ "{n} ({p}%)"
                    )
                ) %>%
                add_overall() %>%
                add_p(
                    test = list(all_categorical() ~ "fisher.test"),
                    test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))
                ) %>%
                bold_labels()

            # Add header modification with error handling for full cohort
            tryCatch({
                full_outcomes <- full_outcomes %>%
                    modify_header(
                        label = "**Outcome**",
                        stat_0 = "**Overall**\nN = {N}",
                        stat_1 = "**PBT**\nN = {n}",
                        stat_2 = "**GKSRS**\nN = {n}",
                        p.value = "**p-value**"
                    )
            }, error = function(e) {
                logger::log_warn(sprintf("Warning: Could not modify headers for full cohort table: %s", e$message))
                logger::log_info("Proceeding with default headers for full cohort")
            })

            # Create baseline table for restricted cohort
            restricted_outcomes <- restricted_cohort_data %>%
                select(any_of(c(outcome_vars, "treatment_group"))) %>%
                tbl_summary(
                    by = treatment_group,
                    missing = "no",
                    label = variable_labels[intersect(names(variable_labels), outcome_vars)],
                    statistic = list(
                        all_categorical() ~ "{n} ({p}%)"
                    )
                ) %>%
                add_overall() %>%
                add_p(
                    test = list(all_categorical() ~ "fisher.test"),
                    test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))
                ) %>%
                bold_labels()

            # Add header modification with error handling for restricted cohort
            tryCatch({
                restricted_outcomes <- restricted_outcomes %>%
                    modify_header(
                        label = "**Outcome**",
                        stat_0 = "**Overall**\nN = {N}",
                        stat_1 = "**PBT**\nN = {n}",
                        stat_2 = "**GKSRS**\nN = {n}",
                        p.value = "**p-value**"
                    )
            }, error = function(e) {
                logger::log_warn(sprintf("Warning: Could not modify headers for restricted cohort table: %s", e$message))
                logger::log_info("Proceeding with default headers for restricted cohort")
            })

            # Merge tables side by side (exact same approach as merge_cohort_tables)
            merged_table <- tbl_merge(
                tbls = list(full_outcomes, restricted_outcomes),
                tab_spanner = c("**Full Cohort**", "**Restricted Cohort**")
            ) %>%
                modify_caption("**Table 2: Local Recurrence and Metastatic Progression Rates**")

            # Save as HTML (same approach as merge_cohort_tables)
            save_gt_html(
                merged_table,
                filename = file.path(output_path, "merged_recurrence_metastatic_progression.html")
            )

            # Save as Excel (same approach as merge_cohort_tables)
            merged_table %>%
                as_tibble() %>%
                writexl::write_xlsx(
                    path = file.path(output_path, "merged_recurrence_metastatic_progression.xlsx")
                )

            logger::log_info("Saved merged recurrence and metastatic progression table (Excel and HTML)")
        },
        error = function(e) {
            logger::log_error(sprintf("Error merging recurrence/metastatic tables: %s", e$message))
            logger::log_info("Skipping recurrence/metastatic table merge")
        }
    )

    # Summary message
    logger::log_info("=== COMPLETED RECURRENCE/METASTATIC TABLE MERGING ===")
    logger::log_info(sprintf("Merged recurrence and metastatic progression table saved to: %s", output_path))
    logger::log_info("Files created: merged_recurrence_metastatic_progression.xlsx and merged_recurrence_metastatic_progression.html")

    return(invisible(NULL))
}

#' Merge adverse events tables from full and restricted cohorts
#'
#' Creates a merged table comparing adverse events between full and restricted cohorts
#' using gtsummary's built-in functions for clean, publication-ready output.
#' Follows the exact same pattern as merge_cohort_tables.
#'
#' @param full_cohort_data Data frame containing full cohort data
#' @param restricted_cohort_data Data frame containing restricted cohort data
#' @param output_path Directory where merged tables should be saved
#' @return Invisibly returns NULL
#'
#' @examples
#' merge_adverse_events_tables(full_data, restricted_data, "final_data/Analysis/merged_tables/")
#' Format count (n) and percent strings without trailing decimals
format_count_percent_stat <- function(values) {
    if (is.null(values)) {
        return(values)
    }

    vapply(values, function(val) {
        if (is.na(val) || !nzchar(val)) {
            return(val)
        }

        match <- regexec("^([0-9]+)(?:\\.[0-9]+)? \\(([^)]*)\\)$", val)
        captured <- regmatches(val, match)[[1]]

        if (length(captured) == 0) {
            return(val)
        }

        count <- sub("\\.[0-9]+$", "", captured[2])
        percent <- captured[3]
        percent <- sub("(\\.0+)(?=%)", "", percent, perl = TRUE)

        sprintf("%s (%s)", count, percent)
    }, character(1), USE.NAMES = FALSE)
}

collapse_binary_outcomes_to_cases <- function(tbl) {
    tbl %>%
        modify_table_body(function(body) {
            case_rows <- body %>%
                filter(row_type == "level", label %in% c("Y", "Yes")) %>%
                select(variable, stat_0, stat_1, stat_2, estimate, conf.low, conf.high, p.value)

            case_rows <- case_rows %>%
                mutate(across(starts_with("stat_"), format_count_percent_stat))

            label_rows <- body %>%
                filter(row_type == "label") %>%
                left_join(case_rows, by = "variable", suffix = c("", "_cases")) %>%
                mutate(
                    stat_0 = coalesce(stat_0_cases, stat_0),
                    stat_1 = coalesce(stat_1_cases, stat_1),
                    stat_2 = coalesce(stat_2_cases, stat_2),
                    estimate = coalesce(estimate_cases, estimate),
                    conf.low = coalesce(conf.low_cases, conf.low),
                    conf.high = coalesce(conf.high_cases, conf.high),
                    p.value = coalesce(p.value_cases, p.value)
                ) %>%
                select(names(body))

            label_rows
        })
}

merge_adverse_events_tables <- function(full_cohort_data, restricted_cohort_data, output_path = NULL) {
    logger::log_info("=== STARTING TABLE MERGING: Adverse Events ===")

    # Set default output path if not provided
    if (is.null(output_path)) {
        output_path <- MERGED_TABLES_DIR
    }

    # Create output directory
    if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
        logger::log_info(sprintf("Created merged tables directory: %s", output_path))
    }

    logger::log_info(sprintf("Merging tables will be saved to: %s", output_path))

    # Get variable labels for human-readable display (use existing STANDARD_TABLE_LABELS)
    variable_labels <- get_variable_labels()

    tryCatch(
        {
            labels <- get_variable_labels()

            # Vision change is already calculated in data derivation (Objective 0)
            # No need to recalculate - use the pre-calculated column
            full_cohort_prepared <- full_cohort_data
            restricted_cohort_prepared <- restricted_cohort_data

            continuous_vars <- c("vision_change")
            binary_vars <- c("retinopathy", "nvg", "srd")
            outcome_vars <- c(continuous_vars, binary_vars)

            available_full <- intersect(outcome_vars, names(full_cohort_prepared))
            available_restricted <- intersect(outcome_vars, names(restricted_cohort_prepared))

            summarise_adverse_outcomes <- function(data, available_vars) {
                if (length(available_vars) == 0) {
                    stop("No adverse event variables available for summarization")
                }

                data %>%
                    select(treatment_group, all_of(available_vars)) %>%
                    tbl_summary(
                        by = treatment_group,
                        missing = "no",
                        label = labels[available_vars],
                        statistic = list(
                            all_continuous() ~ "{median} ({min}, {max})",
                            all_categorical() ~ "{n} ({p}%)"
                        ),
                        digits = list(all_continuous() ~ 1, all_categorical() ~ 1)
                    ) %>%
                    add_overall() %>%
                    add_p(
                        test = list(
                            all_continuous() ~ "wilcox.test",
                            all_categorical() ~ "fisher.test"
                        ),
                        test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))
                    ) %>%
                    bold_labels() %>%
                    modify_header(
                        label = "**Adverse Event**",
                        stat_0 = "**Overall**\nN = {N}",
                        stat_1 = "**PBT**\nN = {n}",
                        stat_2 = "**GKSRS**\nN = {n}",
                        p.value = "**p-value**"
                    ) %>%
                    collapse_binary_outcomes_to_cases()
            }

            full_outcomes <- summarise_adverse_outcomes(full_cohort_prepared, available_full)
            restricted_outcomes <- summarise_adverse_outcomes(restricted_cohort_prepared, available_restricted)

            merged_table <- tbl_merge(
                tbls = list(full_outcomes, restricted_outcomes),
                tab_spanner = c("**Full Cohort**", "**Restricted Cohort**")
            ) %>%
                modify_caption("**Table 3: Adverse Events**")

            save_gt_html(
                merged_table,
                filename = file.path(output_path, "merged_adverse_events.html")
            )

            merged_table %>%
                as_tibble() %>%
                writexl::write_xlsx(
                    path = file.path(output_path, "merged_adverse_events.xlsx")
                )

            logger::log_info("Saved merged adverse events table (Excel and HTML)")
        },
        error = function(e) {
            logger::log_error(sprintf("Error merging adverse events tables: %s", e$message))
            logger::log_info("Skipping adverse events table merge")
        }
    )

    # Summary message
    logger::log_info("=== COMPLETED ADVERSE EVENTS TABLE MERGING ===")
    logger::log_info(sprintf("Merged adverse events table saved to: %s", output_path))
    logger::log_info("Files created: merged_adverse_events.xlsx and merged_adverse_events.html")

    return(invisible(NULL))
}

#' Export descriptive statistics for patients who received repeat treatments
#'
#' Creates a comprehensive dataset of patients who received repeat treatments,
#' including their first treatment, repeat treatment details, and descriptive statistics.
#'
#' @param full_cohort_data Data frame containing full cohort data
#' @param restricted_cohort_data Data frame containing restricted cohort data
#' @param output_path Directory where the Excel file should be saved
#' @return Invisibly returns NULL
#'
#' @examples
#' export_repeat_treatment_descriptive_stats(full_data, restricted_data, "final_data/Analysis/")
export_repeat_treatment_descriptive_stats <- function(full_cohort_data, restricted_cohort_data, output_path = NULL) {
    logger::log_info("=== EXPORTING REPEAT TREATMENT DESCRIPTIVE STATISTICS ===")

    # Set default output path if not provided
    if (is.null(output_path)) {
        output_path <- file.path(DATA_DIR, "Analysis", "merged_tables")
    }

    # Create output directory
    if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
        logger::log_info(sprintf("Created output directory: %s", output_path))
    }

    tryCatch(
        {
            # Function to process repeat treatment data for a cohort
            process_repeat_treatments <- function(cohort_data, cohort_name) {
                # Identify patients with any repeat treatment
                repeat_treatment_patients <- cohort_data %>%
                    filter(!is.na(recurrence1_treatment) | !is.na(recurrence2_treatment) | !is.na(recurrence3_treatment)) %>%
                    mutate(
                        cohort = cohort_name,
                        has_recurrence1_treatment = !is.na(recurrence1_treatment),
                        has_recurrence2_treatment = !is.na(recurrence2_treatment),
                        has_recurrence3_treatment = !is.na(recurrence3_treatment),
                        total_repeat_treatments = rowSums(!is.na(select(., recurrence1_treatment, recurrence2_treatment, recurrence3_treatment)))
                    ) %>%
                    select(
                        # Patient identifiers
                        id, cohort, treatment_group,
                        
                        # First treatment details
                        treatment_date,
                        
                        # Recurrence 1 treatment details (grouped together)
                        recurrence1_treatment, recurrence1_treatment_date, 
                        recurrence1_pretreatment_height, recurrence1_pretreatment_vision,
                        
                        # Recurrence 2 treatment details (grouped together)
                        recurrence2_treatment, recurrence2_treatment_date,
                        recurrence2_pretreatment_height, recurrence2_pretreatment_vision,
                        
                        # Recurrence 3 treatment details (grouped together)
                        recurrence3_treatment, recurrence3_treatment_date,
                        recurrence3_pretreatment_height, recurrence3_pretreatment_vision,
                        
                        # Repeat treatment flags
                        has_recurrence1_treatment, has_recurrence2_treatment, has_recurrence3_treatment, total_repeat_treatments,
                        
                        # Descriptive statistics
                        age_at_diagnosis, sex, race, ethnicity,
                        initial_tumor_height, initial_tumor_diameter, initial_overall_stage,
                        optic_nerve, initial_vision,
                        
                        # Time intervals
                        tt_recurrence_months, tt_recurrence_months_analysis
                    )
                
                return(repeat_treatment_patients)
            }

            # Process both cohorts
            full_repeat_treatments <- process_repeat_treatments(full_cohort_data, "Full Cohort")
            restricted_repeat_treatments <- process_repeat_treatments(restricted_cohort_data, "Restricted Cohort")

            # Combine datasets
            all_repeat_treatments <- bind_rows(full_repeat_treatments, restricted_repeat_treatments)

            # Create summary statistics in wide format: treatments as rows, variables as columns
            create_treatment_type_summary <- function(data) {
                # Get unique repeat treatment types (excluding NA)
                treatment_types <- unique(c(
                    data$recurrence1_treatment,
                    data$recurrence2_treatment, 
                    data$recurrence3_treatment
                ))
                treatment_types <- treatment_types[!is.na(treatment_types)]
                
                # Create summary data frame with treatments as rows
                summary_data <- data.frame(Treatment_Type = treatment_types, stringsAsFactors = FALSE)
                
                # Add total patient counts
                summary_data$Total_Patients <- sapply(treatment_types, function(tx) {
                    sum(data$recurrence1_treatment == tx | data$recurrence2_treatment == tx | data$recurrence3_treatment == tx, na.rm = TRUE)
                })
                
                # Process categorical variables
                categorical_vars <- c("sex", "race", "ethnicity", "initial_overall_stage", "optic_nerve", "treatment_group")
                for (var in categorical_vars) {
                    if (var %in% names(data)) {
                        var_values <- unique(data[[var]])
                        var_values <- var_values[!is.na(var_values)]
                        
                        for (val in var_values) {
                            col_name <- paste0(var, "_", val)
                            summary_data[[col_name]] <- sapply(treatment_types, function(tx) {
                                sum((data$recurrence1_treatment == tx | data$recurrence2_treatment == tx | data$recurrence3_treatment == tx) & 
                                    data[[var]] == val, na.rm = TRUE)
                            })
                        }
                    }
                }
                
                # Process continuous variables
                continuous_vars <- c("age_at_diagnosis", "initial_tumor_height", "initial_tumor_diameter", "initial_vision")
                for (var in continuous_vars) {
                    if (var %in% names(data)) {
                        # Mean
                        col_name_mean <- paste0(var, "_Mean")
                        summary_data[[col_name_mean]] <- sapply(treatment_types, function(tx) {
                            tx_patients <- data[data$recurrence1_treatment == tx | data$recurrence2_treatment == tx | data$recurrence3_treatment == tx, ]
                            mean(tx_patients[[var]], na.rm = TRUE)
                        })
                        
                        # Median
                        col_name_median <- paste0(var, "_Median")
                        summary_data[[col_name_median]] <- sapply(treatment_types, function(tx) {
                            tx_patients <- data[data$recurrence1_treatment == tx | data$recurrence2_treatment == tx | data$recurrence3_treatment == tx, ]
                            median(tx_patients[[var]], na.rm = TRUE)
                        })
                        
                        # SD
                        col_name_sd <- paste0(var, "_SD")
                        summary_data[[col_name_sd]] <- sapply(treatment_types, function(tx) {
                            tx_patients <- data[data$recurrence1_treatment == tx | data$recurrence2_treatment == tx | data$recurrence3_treatment == tx, ]
                            sd(tx_patients[[var]], na.rm = TRUE)
                        })
                    }
                }
                
                # Round numeric values
                numeric_cols <- names(summary_data)[sapply(summary_data, is.numeric)]
                for (col in numeric_cols) {
                    summary_data[[col]] <- round(summary_data[[col]], 2)
                }
                
                return(summary_data)
            }
            
            # Create cohort distribution summary
            create_cohort_distribution_summary <- function(data) {
                # Get unique repeat treatment types
                treatment_types <- unique(c(
                    data$recurrence1_treatment,
                    data$recurrence2_treatment, 
                    data$recurrence3_treatment
                ))
                treatment_types <- treatment_types[!is.na(treatment_types)]
                
                # Create summary by cohort and treatment type
                cohort_summary <- data %>%
                    select(recurrence1_treatment, recurrence2_treatment, recurrence3_treatment, treatment_group, cohort) %>%
                    pivot_longer(
                        cols = c(recurrence1_treatment, recurrence2_treatment, recurrence3_treatment),
                        names_to = "recurrence_number",
                        values_to = "treatment_type"
                    ) %>%
                    filter(!is.na(treatment_type)) %>%
                    group_by(treatment_type, treatment_group, cohort) %>%
                    summarise(count = n(), .groups = "drop") %>%
                    pivot_wider(
                        names_from = c(treatment_group, cohort),
                        values_from = count,
                        values_fill = 0
                    )
                
                return(cohort_summary)
            }
            
            # Generate summaries
            summary_stats <- create_treatment_type_summary(all_repeat_treatments)
            cohort_distribution <- create_cohort_distribution_summary(all_repeat_treatments)

            # Create Excel file with multiple sheets
            excel_file <- file.path(output_path, "repeat_treatment_descriptive_statistics.xlsx")
            
            # Write to Excel with multiple sheets
            writexl::write_xlsx(
                list(
                    "All_Repeat_Treatments" = all_repeat_treatments,
                    "Full_Cohort_Repeat" = full_repeat_treatments,
                    "Restricted_Cohort_Repeat" = restricted_repeat_treatments,
                    "Summary_By_Treatment_Type" = summary_stats,
                    "Cohort_Distribution" = cohort_distribution
                ),
                path = excel_file
            )

            logger::log_info(sprintf("Repeat treatment descriptive statistics exported to: %s", excel_file))
            logger::log_info(sprintf("Total patients with repeat treatments: %d", summary_stats$total_patients))
            logger::log_info(sprintf("Full cohort: %d, Restricted cohort: %d", 
                                   summary_stats$full_cohort_count, summary_stats$restricted_cohort_count))

        },
        error = function(e) {
            logger::log_error(sprintf("Error exporting repeat treatment statistics: %s", e$message))
            stop(e)
        }
    )

    logger::log_info("=== COMPLETED REPEAT TREATMENT STATISTICS EXPORT ===")
    return(invisible(NULL))
}
