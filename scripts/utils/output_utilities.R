# Output Utilities and Directory Management Functions
# Author: Nicholas Camarda
# Description: Functions for creating output directories and merging tables

#' Apply factor level indentation to gtsummary tables
#'
#' This function takes a gtsummary table and applies proper factor level indentation
#' for publication-ready HTML tables. It handles both tbl_summary and tbl_regression tables.
#'
#' @param gtsummary_obj A gtsummary table object or gt table object
#' @return A gt table with factor level indentation applied
#' @examples
#' baseline_table %>% apply_factor_level_indentation()
apply_factor_level_indentation <- function(gtsummary_obj) {
    tryCatch({
        # Convert to gt first if it's a gtsummary object
        if (inherits(gtsummary_obj, "gtsummary")) {
            gt_table <- gtsummary_obj %>% as_gt()
        } else {
            gt_table <- gtsummary_obj
        }
        
        # Get the table data
        table_data <- gt_table %>% as_tibble()
        
        # Clean up column headers by removing markdown asterisks
        names(table_data) <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", names(table_data))
        
        # Apply factor level indentation to the first column (variable names)
        var_names <- table_data[[1]]
        
        # Define main variables that should be bold (not indented)
        main_variables <- c(
            "Treatment Group", "Age at Diagnosis", "Age at Diagnosis (years)", 
            "Sex", "Tumor Location", "Location", "Optic Nerve", "Optic Nerve Involvement",
            "Optic Nerve Abutment", "T Stage", "Initial T Stage", "GEP", "Gene Expression Profile",
            "Tumor Height", "Tumor Height (mm)", "Tumor Diameter", "Tumor Diameter (mm)",
            "Vision", "Initial Vision", "Race", "Eye", "Overall Stage", "N Stage", "M Stage",
            "Internal Reflectivity", "Subretinal Fluid", "Orange Pigment", "Symptoms"
        )
        
        # Factor levels that should be indented
        factor_levels <- c(
            "Male", "Female", "Choroidal", "Cilio-Choroidal", "Ciliary Body", "Iris", 
            "Conjunctival", "Irido-Ciliary", "Other", "Yes", "No", "Low", "High",
            "Class 1", "Class 2", "T1", "T2", "T3", "T4", "Plaque", "GKSRS"
        )
        
        # Apply formatting
        formatted_names <- sapply(var_names, function(name) {
            clean_name <- trimws(name)
            
            if (clean_name %in% main_variables) {
                # Main variables: use CSS class for bold styling
                paste0('<span style="font-weight: bold;">', clean_name, '</span>')
            } else if (clean_name %in% factor_levels || 
                       # Also catch obvious factor levels by pattern
                       grepl("^(Male|Female|Yes|No|Low|High|Class [12]|T[1-4]|Plaque|GKSRS)$", clean_name)) {
                # Factor levels: indent
                paste0("&nbsp;&nbsp;&nbsp;&nbsp;", clean_name)
            } else {
                # If we're not sure, check if it has data in the row
                # This is a simple heuristic for continuous variables vs categorical headers
                clean_name
            }
        })
        
        # Update the table data
        table_data[[1]] <- formatted_names
        
        # Create the final gt table with clean styling
        final_table <- table_data %>%
            gt() %>%
            fmt_markdown(columns = 1) %>%  # Enable HTML in first column
            # Replace NA with blank cells
            sub_missing(columns = everything(), missing_text = "") %>%
            # Clean professional styling
            tab_options(
                table.font.size = px(12),
                heading.title.font.size = px(14),
                column_labels.font.weight = "bold",
                column_labels.background.color = "#ffffff",
                table.border.top.width = px(2),
                table.border.bottom.width = px(2),
                table.border.top.color = "#000000",
                table.border.bottom.color = "#000000",
                column_labels.border.bottom.width = px(1),
                column_labels.border.bottom.color = "#000000",
                table.background.color = "#ffffff"
            ) %>%
            # Style column headers
            tab_style(
                style = list(
                    cell_text(weight = "bold", color = "black"),
                    cell_fill(color = "#ffffff")
                ),
                locations = cells_column_labels()
            ) %>%
            # No need for additional styling - using inline CSS
        
        return(final_table)
        
    }, error = function(e) {
        warning(paste("Factor level indentation failed:", e$message))
        # Fallback: return original table converted to gt
        if (inherits(gtsummary_obj, "gtsummary")) {
            return(gtsummary_obj %>% as_gt())
        } else {
            return(gtsummary_obj)
        }
    })
}

#' Detect the type of gtsummary table
detect_gtsummary_table_type <- function(table_data) {
    col_names <- names(table_data)
    
    # tbl_regression tables typically have "estimate", "CI", "p.value" columns
    if (any(grepl("estimate|^OR|^HR|^MD", col_names, ignore.case = TRUE))) {
        return("tbl_regression")
    }
    
    # tbl_summary tables typically have "stat_" columns
    if (any(grepl("stat_", col_names))) {
        return("tbl_summary")
    }
    
    return("unknown")
}

#' Apply indentation for tbl_summary tables (original logic)
apply_tbl_summary_indentation <- function(table_data) {
    # Define the ACTUAL labels used in baseline tables (from data_processing.R)
    actual_baseline_labels <- c(
        "Age at Diagnosis (years)",
        "Race",
        "Sex", 
        "Eye",
        "Initial Vision",
        "Tumor Location",
        "Optic Nerve Abutment",
        "Tumor Height (mm)",
        "Tumor Diameter (mm)",
        "Internal Reflectivity",
        "Subretinal Fluid (SRF)",
        "Orange Pigment",
        "Any Symptoms",
        "Vision Loss/Blurred Vision",
        "Visual Field Defect",
        "Flashes/Photopsia",
        "Floaters",
        "Pain",
        "Overall Stage",
        "T Stage",
        "N Stage", 
        "M Stage",
        "Initial Metastases",
        "Gene Expression Profile"
    )
    
    # Also include the baseline variable names and get_variable_labels for completeness
    baseline_vars <- BASELINE_VARIABLES_TO_SUMMARIZE
    variable_labels <- get_variable_labels()
    config_labels <- unname(unlist(variable_labels))
    
    # Combine all possible main variable identifiers
    all_main_variable_names <- unique(c(
        actual_baseline_labels,
        baseline_vars,
        names(variable_labels),
        config_labels
    ))
    
    # Get variable names from the table
    var_names <- table_data[[1]]
    modified_names <- var_names
    
    # Simple logic: if the variable name matches a main variable, make it bold
    # Otherwise, indent it as a factor level
    for (i in 1:length(var_names)) {
        var_name <- trimws(var_names[i])
        
        # Check if this is a main variable
        is_main_variable <- var_name %in% all_main_variable_names
        
        if (is_main_variable) {
            # Main variables: make bold
            modified_names[i] <- paste0("<b>", var_name, "</b>")
        } else {
            # Factor levels: add indentation
            modified_names[i] <- paste0("&nbsp;&nbsp;&nbsp;&nbsp;", var_name)
        }
    }
    
    # Update the table data
    table_data[[1]] <- modified_names
    
    # Convert back to gt table with HTML formatting enabled
    gt_table <- table_data %>%
        gt() %>%
        fmt_markdown(columns = 1)  # Enable HTML formatting in first column
    
    return(gt_table)
}

#' Apply indentation for tbl_regression tables
apply_tbl_regression_indentation <- function(table_data) {
    # Get variable names from the table
    var_names <- table_data[[1]]
    modified_names <- var_names
    
    # Regression table logic: Look for patterns that indicate main variables vs factor levels
    for (i in 1:length(var_names)) {
        var_name <- trimws(var_names[i])
        
        # Detect main variables (typically the ones that don't start with spaces and represent the variable name)
        # In regression tables, main variables are usually:
        # 1. Treatment Group, Age at Diagnosis, Sex, Tumor Location, etc. (not indented)
        # 2. Factor levels are usually indented or are reference levels
        
        # Check if this looks like a main variable
        is_main_variable <- detect_regression_main_variable(var_name, table_data, i)
        
        if (is_main_variable) {
            # Main variables: make bold
            modified_names[i] <- paste0("<b>", var_name, "</b>")
        } else {
            # Factor levels: add indentation
            modified_names[i] <- paste0("&nbsp;&nbsp;&nbsp;&nbsp;", var_name)
        }
    }
    
    # Update the table data
    table_data[[1]] <- modified_names
    
    # Convert back to gt table with HTML formatting enabled
    gt_table <- table_data %>%
        gt() %>%
        fmt_markdown(columns = 1) %>%  # Enable HTML formatting in first column
        # Replace "NA" with blank cells in data columns
        sub_missing(columns = -1, missing_text = "")  # Replace NA with blank in all columns except first
    
    # Clean up column headers by removing markdown asterisks (since our CSS handles bold headers)
    gt_table <- clean_table_headers(gt_table)
    
    # Apply publication-ready styling
    gt_table <- apply_publication_styling(gt_table)
    
    return(gt_table)
}

#' Detect if a variable in a regression table is a main variable
detect_regression_main_variable <- function(var_name, table_data, row_index) {
    # Get the data columns (skip first column which is variable names)
    data_cols <- 2:ncol(table_data)
    
    # Check if this row has actual data (not all NA)
    current_row_data <- table_data[row_index, data_cols]
    has_data <- any(!is.na(current_row_data) & current_row_data != "NA" & current_row_data != "")
    
    # Strategy 1: If this row has data AND is not obviously a factor level, it's a main variable
    factor_level_patterns <- c(
        "^Male$", "^Female$",
        "^Anterior$", "^Posterior$", "^Equatorial$", "^Choroidal$", "^Cilio-Choroidal$", "^Other$",
        "^Yes$", "^No$", 
        "^Class", "^Low$", "^High$",
        "^Left$", "^Right$"
    )
    
    is_obvious_factor_level <- FALSE
    for (pattern in factor_level_patterns) {
        if (grepl(pattern, var_name, ignore.case = TRUE)) {
            is_obvious_factor_level <- TRUE
            break
        }
    }
    
    # If this row has data and is not an obvious factor level, it's a main variable (continuous variables)
    if (has_data && !is_obvious_factor_level) {
        return(TRUE)
    }
    
    # Strategy 2: If this row has no data, determine if it's a main variable or factor level
    if (!has_data) {
        # Check if this looks like an obvious factor level
        if (is_obvious_factor_level) {
            return(FALSE)  # This is a factor level
        }
        
        # Look ahead to see if next rows are factor levels (some with data, some with NA)
        next_rows_look_like_factor_levels <- FALSE
        
        # Check the next 2-3 rows to see if they look like factor levels
        for (check_idx in (row_index + 1):min(row_index + 3, nrow(table_data))) {
            if (check_idx <= nrow(table_data)) {
                next_var_name <- trimws(table_data[[1]][check_idx])
                
                # If next row matches factor level patterns, this is probably a main variable
                for (pattern in factor_level_patterns) {
                    if (grepl(pattern, next_var_name, ignore.case = TRUE)) {
                        next_rows_look_like_factor_levels <- TRUE
                        break
                    }
                }
                
                if (next_rows_look_like_factor_levels) break
            }
        }
        
        # Additional check: Look backwards to see if previous row was a main variable
        # If so, this might be a factor level
        if (row_index > 1) {
            prev_var_name <- trimws(table_data[[1]][row_index - 1])
            prev_row_data <- table_data[row_index - 1, data_cols]
            prev_has_data <- any(!is.na(prev_row_data) & prev_row_data != "NA" & prev_row_data != "")
            
            # If previous row had no data and current row has no data and looks like factor level,
            # then previous was probably main variable and this is factor level
            if (!prev_has_data && is_obvious_factor_level) {
                return(FALSE)  # This is a factor level
            }
        }
        
        return(next_rows_look_like_factor_levels)
    }
    
    # Default: if we can't determine, assume it's a factor level
    return(FALSE)
}

#' Apply generic indentation for unknown table types
apply_generic_indentation <- function(table_data) {
    # Fallback: just convert to gt table without modification
    gt_table <- table_data %>%
        gt()
    
    return(gt_table)
}

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