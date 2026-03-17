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
        obj1_recurrence_1a1 = file.path(cohort_dir, "01_Efficacy", "a_recurrence", "1a1_recurrence_stratified_os"),
        obj1_recurrence_1a2 = file.path(cohort_dir, "01_Efficacy", "a_recurrence", "1a2_recurrence_stratified_pfs"),
        obj1_mets = file.path(cohort_dir, "01_Efficacy", "b_metastatic_progression"),
        obj1_mets_2a1 = file.path(cohort_dir, "01_Efficacy", "b_metastatic_progression", "2a1_metastasis_stratified_os"),
        obj1_mets_2a2 = file.path(cohort_dir, "01_Efficacy", "b_metastatic_progression", "2a2_metastasis_stratified_pfs"),
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
        # Base outcome folders (kept for compatibility)
        obj4_mfs = file.path(cohort_dir, "04_GEP_Validation", "a_metastasis_free_survival"),
        obj4_mss = file.path(cohort_dir, "04_GEP_Validation", "b_melanoma_specific_survival"),
        obj4_ph_diagnostics = file.path(cohort_dir, "04_GEP_Validation", "c_proportional_hazards_diagnostics"),
        # MFS-specific subfolders
        obj4_mfs_km = file.path(cohort_dir, "04_GEP_Validation", "a_metastasis_free_survival", "01_km_curves"),
        obj4_mfs_cox = file.path(cohort_dir, "04_GEP_Validation", "a_metastasis_free_survival", "02_cox_models"),
        obj4_mfs_rmst = file.path(cohort_dir, "04_GEP_Validation", "a_metastasis_free_survival", "03_rmst_analysis"),
        obj4_mfs_validation = file.path(cohort_dir, "04_GEP_Validation", "a_metastasis_free_survival", "04_validation"),
        obj4_mfs_summary = file.path(cohort_dir, "04_GEP_Validation", "a_metastasis_free_survival", "05_summary_tables"),
        # MSS-specific subfolders (intentionally asymmetric with MFS)
        obj4_mss_cif = file.path(cohort_dir, "04_GEP_Validation", "b_melanoma_specific_survival", "01_cif_curves"),
        obj4_mss_validation = file.path(cohort_dir, "04_GEP_Validation", "b_melanoma_specific_survival", "02_validation"),
        obj4_mss_summary = file.path(cohort_dir, "04_GEP_Validation", "b_melanoma_specific_survival", "03_summary_tables"),

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

safe_normalize_path <- function(path) {
    if (is.null(path) || is.na(path) || !nzchar(path)) {
        return(NA_character_)
    }
    normalizePath(path, winslash = "/", mustWork = FALSE)
}

resolve_obj4_output_dir <- function(output_dirs, base_dir, artifact_type = c("base", "km", "cox", "rmst", "validation", "summary", "cif")) {
    artifact_type <- match.arg(artifact_type)

    if (is.null(output_dirs) || is.null(base_dir)) {
        return(base_dir)
    }

    normalized_base <- safe_normalize_path(base_dir)
    normalized_mfs <- safe_normalize_path(output_dirs$obj4_mfs %||% NULL)
    normalized_mss <- safe_normalize_path(output_dirs$obj4_mss %||% NULL)

    if (!is.na(normalized_base) && !is.na(normalized_mfs) && identical(normalized_base, normalized_mfs)) {
        key <- switch(
            artifact_type,
            base = "obj4_mfs",
            km = "obj4_mfs_km",
            cox = "obj4_mfs_cox",
            rmst = "obj4_mfs_rmst",
            validation = "obj4_mfs_validation",
            summary = "obj4_mfs_summary",
            cif = "obj4_mfs_summary"
        )
        return(output_dirs[[key]] %||% base_dir)
    }

    if (!is.na(normalized_base) && !is.na(normalized_mss) && identical(normalized_base, normalized_mss)) {
        key <- switch(
            artifact_type,
            base = "obj4_mss",
            km = "obj4_mss_cif",
            cox = "obj4_mss_summary",
            rmst = "obj4_mss_summary",
            validation = "obj4_mss_validation",
            summary = "obj4_mss_summary",
            cif = "obj4_mss_cif"
        )
        return(output_dirs[[key]] %||% base_dir)
    }

    base_dir
}

ensure_output_dir <- function(dir_path) {
    if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
    dir_path
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

format_gep_log_p_value <- function(log_p_value, significant_digits = 4) {
    if (is.infinite(log_p_value) && log_p_value < 0) {
        return("0")
    }

    if (length(log_p_value) == 0 || is.na(log_p_value) || !is.finite(log_p_value)) {
        return("NA")
    }

    exponent <- floor(as.numeric(log_p_value) / log(10))
    if (!is.finite(exponent) || abs(exponent) > .Machine$integer.max) {
        return("0")
    }

    mantissa <- exp(log_p_value - exponent * log(10))

    if (mantissa >= 10) {
        mantissa <- mantissa / 10
        exponent <- exponent + 1
    }

    mantissa_text <- formatC(mantissa, format = "f", digits = significant_digits - 1)
    exponent_value <- as.integer(exponent)
    exponent_sign <- if (exponent_value < 0) "-" else "+"
    exponent_abs <- abs(exponent_value)
    exponent_text <- if (exponent_abs < 10) sprintf("%02d", exponent_abs) else as.character(exponent_abs)

    sprintf("%se%s%s", mantissa_text, exponent_sign, exponent_text)
}

calculate_chisq_log_p_value <- function(chisq_statistic, df, max_terms = 12) {
    if (length(chisq_statistic) == 0 || length(df) == 0 || is.na(chisq_statistic) || is.na(df) ||
        !is.finite(chisq_statistic) || !is.finite(df) || chisq_statistic < 0 || df <= 0) {
        return(NA_real_)
    }

    log_p_value <- stats::pchisq(chisq_statistic, df = df, lower.tail = FALSE, log.p = TRUE)
    if (is.finite(log_p_value)) {
        return(log_p_value)
    }

    -Inf
}

format_gep_p_value <- function(p_value, log_p_value = NULL, decimal_places = 4, significant_digits = 4) {
    if (length(p_value) == 0 || is.na(p_value) || !is.finite(p_value)) {
        if (!is.null(log_p_value) && length(log_p_value) > 0 && !is.na(log_p_value)) {
            return(format_gep_log_p_value(log_p_value, significant_digits = significant_digits))
        }
        return("NA")
    }

    if (p_value == 0) {
        if (!is.null(log_p_value) && length(log_p_value) > 0 && !is.na(log_p_value)) {
            return(format_gep_log_p_value(log_p_value, significant_digits = significant_digits))
        }
        return("0")
    }

    scientific_threshold <- 10^(-decimal_places)
    if (abs(p_value) < scientific_threshold) {
        return(formatC(p_value, format = "e", digits = significant_digits - 1))
    }

    sprintf(paste0("%.", decimal_places, "f"), p_value)
}

load_precollapse_data <- function(dataset_name = NULL) {
    if (is.null(dataset_name) || !exists("PROCESSED_DATA_DIR", inherits = TRUE)) {
        return(NULL)
    }

    precollapse_path <- file.path(PROCESSED_DATA_DIR, paste0(dataset_name, "_derived_precollapse.rds"))
    if (!file.exists(precollapse_path)) {
        return(NULL)
    }

    tryCatch(
        readRDS(precollapse_path),
        error = function(e) NULL
    )
}

restore_precollapse_variables <- function(data, dataset_name = NULL, variables = NULL) {
    precollapse_data <- load_precollapse_data(dataset_name)
    if (is.null(precollapse_data)) {
        return(data)
    }

    common_cols <- intersect(names(precollapse_data), names(data))
    if (!is.null(variables)) {
        common_cols <- intersect(common_cols, variables)
    }

    if (length(common_cols) == 0) {
        return(data)
    }

    key_candidates <- c("id", "patient_id", "record_id", "case_id", "study_id")
    key_col <- key_candidates[key_candidates %in% names(data) & key_candidates %in% names(precollapse_data)][1]

    if (!is.na(key_col) &&
        !anyDuplicated(data[[key_col]]) &&
        !anyDuplicated(precollapse_data[[key_col]])) {
        matched_rows <- match(data[[key_col]], precollapse_data[[key_col]])
        matched <- !is.na(matched_rows)
        restore_cols <- setdiff(common_cols, key_col)

        for (col in restore_cols) {
            restored_values <- data[[col]]

            if (is.factor(precollapse_data[[col]]) || is.factor(restored_values)) {
                restored_chars <- as.character(restored_values)
                restored_chars[matched] <- as.character(precollapse_data[[col]][matched_rows[matched]])
                restored_levels <- unique(c(levels(factor(precollapse_data[[col]])), restored_chars))
                data[[col]] <- factor(restored_chars, levels = restored_levels)
            } else {
                restored_values[matched] <- precollapse_data[[col]][matched_rows[matched]]
                data[[col]] <- restored_values
            }
        }

        return(data)
    }

    if (nrow(precollapse_data) == nrow(data)) {
        data[common_cols] <- precollapse_data[common_cols]
    }

    data
}

restore_gep_display_variables <- function(data, dataset_name = NULL, variables = NULL) {
    if (is.null(variables)) {
        if (exists("GEP_DISPLAY_VARIABLES", inherits = TRUE)) {
            variables <- get("GEP_DISPLAY_VARIABLES", inherits = TRUE)
        } else {
            variables <- c("biopsy1_gep", "gep_class_simple", "prame_status", "gep12_prame_status")
        }
    }

    restore_precollapse_variables(data, dataset_name = dataset_name, variables = variables)
}

#' Harmonize gtsummary headers across tables before stacking
#'
#' Uses the first table as the reference header map so intentionally stacked
#' sections share the same column labels and can be stacked quietly.
#'
#' @param tbls List of gtsummary tables.
#' @return List of gtsummary tables with aligned headers when possible.
harmonize_tbl_stack_headers <- function(tbls) {
    if (length(tbls) <= 1) {
        return(tbls)
    }

    reference_header <- tbls[[1]]$table_styling$header %||% NULL
    if (is.null(reference_header) || nrow(reference_header) == 0) {
        return(tbls)
    }

    lapply(seq_along(tbls), function(tbl_idx) {
        tbl <- tbls[[tbl_idx]]
        if (tbl_idx == 1) {
            return(tbl)
        }

        table_header <- tbl$table_styling$header %||% NULL
        if (is.null(table_header) || nrow(table_header) == 0) {
            return(tbl)
        }

        common_columns <- intersect(reference_header$column, table_header$column)
        if (length(common_columns) == 0) {
            return(tbl)
        }

        desired_labels <- reference_header$label[match(common_columns, reference_header$column)]
        current_labels <- table_header$label[match(common_columns, table_header$column)]
        changed_columns <- common_columns[!is.na(desired_labels) & desired_labels != current_labels]

        if (length(changed_columns) == 0) {
            return(tbl)
        }

        header_updates <- stats::setNames(
            as.list(desired_labels[match(changed_columns, common_columns)]),
            changed_columns
        )

        do.call(gtsummary::modify_header, c(list(x = tbl), header_updates))
    })
}

#' Stack gtsummary tables quietly after aligning shared headers
#'
#' @param tbls List of gtsummary tables to stack.
#' @return A single stacked gtsummary table.
quiet_tbl_stack <- function(tbls) {
    harmonized_tbls <- harmonize_tbl_stack_headers(tbls)
    gtsummary::tbl_stack(tbls = harmonized_tbls, quiet = TRUE)
}

build_merged_baseline_cohort_table <- function(data, dataset_name = NULL) {
    vars_to_summarize <- BASELINE_VARIABLES_TO_SUMMARIZE
    variable_labels <- get_variable_labels()
    cohort_label <- dataset_name %||% "cohort"

    cohort_data <- apply_precollapse_levels(data, dataset_name)
    cohort_data <- format_levels_for_display(cohort_data)

    available_vars <- intersect(vars_to_summarize, names(cohort_data))
    if (!is.null(dataset_name) && grepl("restricted", dataset_name, ignore.case = TRUE)) {
        available_vars <- setdiff(available_vars, "optic_nerve")
    }

    treatment_levels <- character()
    if ("treatment_group" %in% names(cohort_data)) {
        treatment_levels <- unique(stats::na.omit(as.character(cohort_data$treatment_group)))
    }

    if (length(treatment_levels) >= 2) {
        vars_with_insufficient_levels <- c()
        for (var in available_vars) {
            if (var %in% names(cohort_data) && (is.factor(cohort_data[[var]]) || is.character(cohort_data[[var]]))) {
                level_counts <- table(cohort_data[[var]], useNA = "no")
                valid_levels <- sum(level_counts > 0)
                if (valid_levels < 2) {
                    vars_with_insufficient_levels <- c(vars_with_insufficient_levels, var)
                }
            }
        }

        if (length(vars_with_insufficient_levels) > 0) {
            logger::log_info(sprintf(
                "Variables with insufficient levels for p-values in %s: %s",
                cohort_label,
                paste(vars_with_insufficient_levels, collapse = ", ")
            ))
        }

        cohort_table <- cohort_data %>%
            select(any_of(c(available_vars, "treatment_group"))) %>%
            tbl_summary(
                by = treatment_group,
                missing = "no",
                label = variable_labels[intersect(names(variable_labels), available_vars)],
                statistic = list(
                    all_continuous() ~ "{median} ({min}, {max})",
                    all_categorical() ~ "{n} ({p}%)"
                ),
                digits = list(all_continuous() ~ 1, all_categorical() ~ 0)
            ) %>%
            add_overall() %>%
            bold_labels()

        testable_vars <- setdiff(available_vars, vars_with_insufficient_levels)

        cohort_table <- tryCatch(
            {
                if (length(testable_vars) == 0) {
                    return(cohort_table)
                }

                cohort_table %>%
                    add_p(
                        include = any_of(testable_vars),
                        test = list(all_categorical() ~ "fisher.test"),
                        test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))
                    )
            },
            error = function(e) {
                logger::log_warn(sprintf("Warning: Could not add p-values for %s table: %s", cohort_label, e$message))
                cohort_table
            }
        )

        return(tryCatch(
            {
                cohort_table %>%
                    modify_header(
                        label = "**Characteristic**",
                        stat_0 = "**Overall**\nN = {N}",
                        stat_1 = "**PBT**\nN = {n}",
                        stat_2 = "**GKSRS**\nN = {n}",
                        p.value = "**p-value**"
                    )
            },
            error = function(e) {
                logger::log_warn(sprintf("Warning: Could not modify headers for %s table: %s", cohort_label, e$message))
                cohort_table
            }
        ))
    }

    cohort_table <- cohort_data %>%
        select(any_of(available_vars)) %>%
        tbl_summary(
            missing = "no",
            label = variable_labels[intersect(names(variable_labels), available_vars)],
            statistic = list(
                all_continuous() ~ "{median} ({min}, {max})",
                all_categorical() ~ "{n} ({p}%)"
            ),
            digits = list(all_continuous() ~ 1, all_categorical() ~ 0)
        ) %>%
        bold_labels()

    tryCatch(
        {
            cohort_table %>%
                modify_header(
                    label = "**Characteristic**",
                    stat_0 = "**Overall**\nN = {N}"
                )
        },
        error = function(e) {
            logger::log_warn(sprintf("Warning: Could not modify headers for %s table: %s", cohort_label, e$message))
            cohort_table
        }
    )
}

#' Reapply pre-collapsed factor levels when available
#'
apply_precollapse_levels <- function(data, dataset_name = NULL) {
    restore_precollapse_variables(data, dataset_name = dataset_name)
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

    tryCatch(
        {
            full_baseline <- build_merged_baseline_cohort_table(
                full_cohort_data,
                dataset_name = dataset_names$full %||% "uveal_melanoma_full_cohort"
            )
            restricted_baseline <- build_merged_baseline_cohort_table(
                restricted_cohort_data,
                dataset_name = dataset_names$restricted %||% "uveal_melanoma_restricted_cohort"
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

#' Merge baseline characteristics tables from all three analytic cohorts
#'
#' Creates a side-by-side baseline table comparing the full, restricted, and
#' GKSRS-only cohorts while preserving the legacy two-cohort merged outputs.
#' The GKSRS-only cohort is summarized as a single overall arm because it has no
#' PBT comparator by design.
#'
#' @param full_cohort_data Data frame containing full cohort data.
#' @param restricted_cohort_data Data frame containing restricted cohort data.
#' @param gksrs_only_cohort_data Data frame containing GKSRS-only cohort data.
#' @param output_path Directory where merged tables should be saved.
#' @param dataset_names Named list of dataset ids for pre-collapse restoration.
#' @return Invisibly returns NULL.
merge_all_cohort_baseline_tables <- function(full_cohort_data,
                                             restricted_cohort_data,
                                             gksrs_only_cohort_data,
                                             output_path = NULL,
                                             dataset_names = list()) {
    logger::log_info("=== STARTING TABLE MERGING: Full, Restricted, and GKSRS-Only Cohorts ===")

    if (is.null(output_path)) {
        output_path <- MERGED_TABLES_DIR
    }

    if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
        logger::log_info(sprintf("Created merged tables directory: %s", output_path))
    }

    logger::log_info(sprintf("Three-cohort merged tables will be saved to: %s", output_path))

    tryCatch(
        {
            full_baseline <- build_merged_baseline_cohort_table(
                full_cohort_data,
                dataset_name = dataset_names$full %||% "uveal_melanoma_full_cohort"
            )
            restricted_baseline <- build_merged_baseline_cohort_table(
                restricted_cohort_data,
                dataset_name = dataset_names$restricted %||% "uveal_melanoma_restricted_cohort"
            )
            gksrs_only_baseline <- build_merged_baseline_cohort_table(
                gksrs_only_cohort_data,
                dataset_name = dataset_names$gksrs_only %||% "uveal_melanoma_gksrs_only_cohort"
            )

            merged_table <- tbl_merge(
                tbls = list(full_baseline, restricted_baseline, gksrs_only_baseline),
                tab_spanner = c("**Full Cohort**", "**Restricted Cohort**", "**GKSRS-Only Cohort**")
            ) %>%
                modify_caption("**Table 1B: Baseline Characteristics Across All Three Cohorts**")

            save_gt_html(
                merged_table,
                filename = file.path(output_path, "merged_baseline_characteristics_all_three_cohorts.html")
            )

            merged_table %>%
                as_tibble() %>%
                writexl::write_xlsx(
                    path = file.path(output_path, "merged_baseline_characteristics_all_three_cohorts.xlsx")
                )

            logger::log_info("Saved three-cohort merged baseline characteristics table (Excel and HTML)")
        },
        error = function(e) {
            logger::log_error(sprintf("Error merging three-cohort baseline tables: %s", e$message))
            logger::log_info("Skipping three-cohort baseline table merge")
        }
    )

    logger::log_info("=== COMPLETED THREE-COHORT BASELINE TABLE MERGING ===")
    logger::log_info(sprintf("Three-cohort merged baseline characteristics table saved to: %s", output_path))
    logger::log_info("Files created: merged_baseline_characteristics_all_three_cohorts.xlsx and merged_baseline_characteristics_all_three_cohorts.html")

    invisible(NULL)
}

#' Merge baseline characteristics tables for the full and GKSRS-only cohorts
#'
#' Creates a side-by-side baseline table comparing the full cohort against the
#' GKSRS-only cohort. The full cohort retains its within-cohort PBT-vs-GKSRS
#' statistical tests, while the GKSRS-only cohort is summarized descriptively.
#'
#' @param full_cohort_data Data frame containing full cohort data.
#' @param gksrs_only_cohort_data Data frame containing GKSRS-only cohort data.
#' @param output_path Directory where merged tables should be saved.
#' @param dataset_names Named list of dataset ids for pre-collapse restoration.
#' @return Invisibly returns NULL.
merge_full_vs_gksrs_baseline_tables <- function(full_cohort_data,
                                                gksrs_only_cohort_data,
                                                output_path = NULL,
                                                dataset_names = list()) {
    logger::log_info("=== STARTING TABLE MERGING: Full and GKSRS-Only Cohorts ===")

    if (is.null(output_path)) {
        output_path <- MERGED_TABLES_DIR
    }

    if (!dir.exists(output_path)) {
        dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
        logger::log_info(sprintf("Created merged tables directory: %s", output_path))
    }

    logger::log_info(sprintf("Full-vs-GKSRS-only merged tables will be saved to: %s", output_path))

    tryCatch(
        {
            full_baseline <- build_merged_baseline_cohort_table(
                full_cohort_data,
                dataset_name = dataset_names$full %||% "uveal_melanoma_full_cohort"
            )
            gksrs_only_baseline <- build_merged_baseline_cohort_table(
                gksrs_only_cohort_data,
                dataset_name = dataset_names$gksrs_only %||% "uveal_melanoma_gksrs_only_cohort"
            )

            merged_table <- tbl_merge(
                tbls = list(full_baseline, gksrs_only_baseline),
                tab_spanner = c("**Full Cohort**", "**GKSRS-Only Cohort**")
            ) %>%
                modify_caption("**Table 1C: Baseline Characteristics for Full vs GKSRS-Only Cohorts**")

            save_gt_html(
                merged_table,
                filename = file.path(output_path, "merged_baseline_characteristics_full_vs_gksrs_only.html")
            )

            merged_table %>%
                as_tibble() %>%
                writexl::write_xlsx(
                    path = file.path(output_path, "merged_baseline_characteristics_full_vs_gksrs_only.xlsx")
                )

            logger::log_info("Saved full-vs-GKSRS-only merged baseline characteristics table (Excel and HTML)")
        },
        error = function(e) {
            logger::log_error(sprintf("Error merging full-vs-GKSRS-only baseline tables: %s", e$message))
            logger::log_info("Skipping full-vs-GKSRS-only baseline table merge")
        }
    )

    logger::log_info("=== COMPLETED FULL-VS-GKSRS-ONLY BASELINE TABLE MERGING ===")
    logger::log_info(sprintf("Full-vs-GKSRS-only merged baseline characteristics table saved to: %s", output_path))
    logger::log_info("Files created: merged_baseline_characteristics_full_vs_gksrs_only.xlsx and merged_baseline_characteristics_full_vs_gksrs_only.html")

    invisible(NULL)
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

format_count_percent_columns <- function(tbl) {
    if (!inherits(tbl, "tbl_summary")) {
        return(tbl)
    }

    stat_cols <- names(tbl$table_body)
    stat_cols <- stat_cols[grepl("^stat_", stat_cols)]

    if (length(stat_cols) == 0) {
        return(tbl)
    }

    tbl %>%
        modify_table_body(function(body) {
            body %>%
                mutate(across(all_of(stat_cols), format_count_percent_stat))
        })
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

                has_vision_change <- "vision_change" %in% available_vars
                base_vars <- setdiff(available_vars, "vision_change")

                base_tbl <- NULL
                if (length(base_vars) > 0) {
                    base_tbl <- data %>%
                        select(treatment_group, all_of(base_vars)) %>%
                        tbl_summary(
                            by = treatment_group,
                            missing = "no",
                            label = labels[base_vars],
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
                        format_count_percent_columns() %>%
                        bold_labels() %>%
                        modify_header(
                            label = "**Adverse Event**",
                            stat_0 = "**Overall**\nN = {N}",
                            stat_1 = "**PBT**\nN = {n}",
                            stat_2 = "**GKSRS**\nN = {n}",
                            p.value = "**p-value**"
                        ) %>%
                        modify_table_styling(
                            columns = "p.value",
                            footnote = "Wilcoxon rank-sum test for continuous rows; Fisher's exact test (simulated p-value) for categorical rows."
                        ) %>%
                        collapse_binary_outcomes_to_cases()
                }

                if (has_vision_change) {
                    line_counts <- compute_line_change_lines(data$vision_change)
                    line_levels <- line_change_label_levels(line_counts)
                    bucket_levels <- VISION_LINE_CHANGE_CATEGORY_LEVELS

                    vision_change_tbl <- NULL
                    line_change_tbl <- NULL
                    line_change_bucket_tbl <- NULL
                    line_change_summary_tbl <- NULL

                    vision_change_tbl <- data %>%
                        select(treatment_group, vision_change) %>%
                        tbl_summary(
                            by = treatment_group,
                            missing = "no",
                            label = list(vision_change ~ "Vision Change (logMAR)"),
                            statistic = list(vision_change ~ "{median} ({min}, {max})"),
                            digits = list(vision_change ~ 1)
                        ) %>%
                        add_overall() %>%
                        add_p(test = list(all_continuous() ~ "wilcox.test")) %>%
                        bold_labels() %>%
                        modify_header(
                            label = "**Characteristic**",
                            stat_0 = "**Overall**\nN = {N}"
                        )

                    if (length(line_levels) > 0) {
                        line_change_tbl <- data %>%
                            mutate(
                                vision_line_change_category = format_line_change_label(line_counts),
                                vision_line_change_category = factor(vision_line_change_category, levels = line_levels, ordered = TRUE)
                            ) %>%
                            filter(!is.na(vision_line_change_category)) %>%
                            select(treatment_group, vision_line_change_category) %>%
                            tbl_summary(
                                by = treatment_group,
                                missing = "no",
                                type = list(vision_line_change_category ~ "categorical"),
                                statistic = list(all_categorical() ~ "{n} ({p}%)"),
                                digits = list(all_categorical() ~ 1),
                                label = list(vision_line_change_category ~ "Snellen Line Change Integer Distribution")
                            ) %>%
                            add_overall() %>%
                            add_p(
                                test = list(all_categorical() ~ "fisher.test"),
                                test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))
                            ) %>%
                            format_count_percent_columns() %>%
                            bold_labels() %>%
                            modify_header(
                                label = "**Snellen Line Change**",
                                stat_0 = "**Overall**\nN = {N}",
                                stat_1 = "**PBT**\nN = {n}",
                                stat_2 = "**GKSRS**\nN = {n}",
                                p.value = "**p-value**"
                            )
                    }

                    if (any(!is.na(line_counts))) {
                        line_change_bucket_tbl <- data %>%
                            mutate(
                                vision_line_change_bucket = assign_line_change_bucket(line_counts),
                                vision_line_change_bucket = factor(vision_line_change_bucket, levels = bucket_levels, ordered = TRUE)
                            ) %>%
                            filter(!is.na(vision_line_change_bucket)) %>%
                            select(treatment_group, vision_line_change_bucket) %>%
                            tbl_summary(
                                by = treatment_group,
                                missing = "no",
                                type = list(vision_line_change_bucket ~ "categorical"),
                                statistic = list(all_categorical() ~ "{n} ({p}%)"),
                                digits = list(all_categorical() ~ 1),
                                label = list(vision_line_change_bucket ~ "Snellen Line Change Distribution")
                            ) %>%
                            add_overall() %>%
                            add_p(
                                test = list(all_categorical() ~ "fisher.test"),
                                test.args = list(all_categorical() ~ list(simulate.p.value = TRUE))
                            ) %>%
                            format_count_percent_columns() %>%
                            bold_labels() %>%
                            modify_header(
                                label = "**Snellen Line Change Distribution**",
                                stat_0 = "**Overall**\nN = {N}",
                                stat_1 = "**PBT**\nN = {n}",
                                stat_2 = "**GKSRS**\nN = {n}",
                                p.value = "**p-value**"
                            )

                        line_change_summary_tbl <- data %>%
                            select(treatment_group, vision_change) %>%
                            tbl_summary(
                                by = treatment_group,
                                missing = "no",
                                type = list(vision_change ~ "continuous"),
                                statistic = list(vision_change ~ "{median} ({min}, {max})"),
                                digits = list(vision_change ~ 1),
                                label = list(vision_change ~ "Vision Change (logMAR)")
                            ) %>%
                            add_overall() %>%
                            add_p(test = list(all_continuous() ~ "wilcox.test")) %>%
                            bold_labels() %>%
                            modify_header(
                                label = "**Characteristic**",
                                stat_0 = "**Overall**\nN = {N}"
                            ) %>%
                            convert_logmar_summary_table_to_line_summary(
                            label = "Snellen Line Change",
                                caption = "Snellen Line Change Summary"
                            )
                    }

                    stacked_tables <- Filter(
                        Negate(is.null),
                        c(list(vision_change_tbl, line_change_summary_tbl, line_change_bucket_tbl), list(base_tbl))
                    )

                    return(quiet_tbl_stack(stacked_tables))
                }

                base_tbl
            }

            full_outcomes <- summarise_adverse_outcomes(full_cohort_prepared, available_full)
            restricted_outcomes <- summarise_adverse_outcomes(restricted_cohort_prepared, available_restricted)

            merged_table <- tbl_merge(
                tbls = list(full_outcomes, restricted_outcomes),
                tab_spanner = c("**Full Cohort**", "**Restricted Cohort**")
            ) %>%
                modify_caption("**Table 3: Adverse Events**") %>%
                modify_table_styling(
                    columns = starts_with("p.value"),
                    footnote = "Wilcoxon rank-sum test for continuous rows; Fisher's exact test (simulated p-value) for categorical rows."
                )

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
