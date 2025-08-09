#' Calculate treatment duration metrics
#'
#' Computes follow-up time, 5-year intervals, and summary statistics for each treatment group.
#'
#' @param data Data frame. Patient-level data with follow-up and treatment dates.
#'
#' @return A list with interval_metrics and summary_stats
calculate_treatment_duration_metrics <- function(data) {

    data <- data %>%
        mutate(
            total_followup_days = as.numeric(difftime(last_known_alive_date, treatment_date, units = "days")),
            total_years = case_when(
                is.na(total_followup_days) | total_followup_days < 0 ~ 0,
                TRUE ~ total_followup_days / DAYS_IN_YEAR
            )
        )

    if (VERBOSE) {
        log_enhanced("Checking for problematic follow-up times:", level = "INFO")
        problematic_cases <- data %>%
            filter(is.na(total_followup_days) | total_followup_days < 0) %>%
            select(id, treatment_group, treatment_date, last_known_alive_date, total_followup_days)
        if (nrow(problematic_cases) > 0) {
            print(problematic_cases)
        } else {
            log_enhanced("No problematic follow-up times found", level = "INFO")
        }
    }

    max_years <- ceiling(max(data$total_years, na.rm = TRUE))
    intervals <- seq(0, max_years, by = 5)

    interval_data <- data %>%
        select(id, treatment_group, total_years) %>%
        filter(total_years > 0) %>%
        crossing(interval_end = intervals) %>%
        filter(interval_end <= total_years)

    interval_metrics <- interval_data %>%
        group_by(interval_end, treatment_group) %>%
        summarise(
            n_patients = n(),
            .groups = "drop"
        ) %>%
        pivot_wider(
            names_from = treatment_group,
            values_from = n_patients,
            names_prefix = "n_"
        ) %>%
        mutate(interval_label = sprintf("%d years", interval_end))

    log_enhanced("\nTreatment duration summary:", level = "INFO")
    summary_stats <- data %>%
        group_by(treatment_group) %>%
        summarise(
            n_total = n(),
            n_valid_followup = sum(!is.na(total_followup_days) & total_followup_days >= 0),
            mean_followup_years = mean(total_years[!is.na(total_years) & total_years >= 0], na.rm = TRUE),
            median_followup_years = median(total_years[!is.na(total_years) & total_years >= 0], na.rm = TRUE),
            max_followup_years = max(total_years[!is.na(total_years) & total_years >= 0], na.rm = TRUE),
            .groups = "drop"
        )
    if (VERBOSE) print(summary_stats)

    return(list(interval_metrics = interval_metrics, summary_stats = summary_stats))
}

#' Create summary tables using gtsummary
#'
#' Generates and saves summary tables for each cohort, including baseline characteristics and treatment duration metrics.
#'
#' @param data_list List of data frames (cohorts)
#' @param output_dirs Optional list of output directories
#' @return A named list of lists of tables per cohort
create_summary_tables <- function(data_list, output_dirs = NULL) {
    log_enhanced("Creating summary tables", level = "INFO")
    log_enhanced(sprintf("output_dirs parameter: %s", ifelse(is.null(output_dirs), "NULL", "not NULL")), level = "INFO")
    if (!is.null(output_dirs)) {
        log_enhanced(sprintf("output_dirs names: %s", paste(names(output_dirs), collapse = ", ")), level = "INFO")
    }

    vars_to_summarize <- BASELINE_VARIABLES_TO_SUMMARIZE
    log_enhanced(sprintf("Summarizing %d variables", length(vars_to_summarize)), level = "INFO")

    tables <- lapply(names(data_list), function(cohort_name) {
        message(sprintf("\nCreating table for cohort: %s", cohort_name))
        data <- data_list[[cohort_name]]

        prefix <- case_when(
            grepl("full", cohort_name) ~ "full_cohort_",
            grepl("restricted", cohort_name) ~ "restricted_cohort_", 
            grepl("gksrs", cohort_name) ~ "gksrs_only_cohort_",
            TRUE ~ paste0(cohort_name, "_")
        )
        cohort_dir_key <- case_when(
            grepl("uveal_melanoma_full_cohort", cohort_name) ~ "full_cohort",
            grepl("uveal_melanoma_restricted_cohort", cohort_name) ~ "restricted_cohort", 
            grepl("uveal_melanoma_gksrs_only_cohort", cohort_name) ~ "gksrs_only_cohort",
            TRUE ~ cohort_name
        )

        if (!is.null(output_dirs) && !is.null(output_dirs[[cohort_dir_key]])) {
            treatment_duration_dir <- output_dirs[[cohort_dir_key]]$treatment_duration
            baseline_output_dir <- output_dirs[[cohort_dir_key]]$baseline_characteristics
            log_enhanced(sprintf("Using cohort-specific directories for %s (mapped to %s): treatment_duration=%s, baseline=%s", 
                               cohort_name, cohort_dir_key, treatment_duration_dir, baseline_output_dir), level = "INFO")
        } else {
            treatment_duration_dir <- file.path("final_data/Analysis", "General", "treatment_duration")
            baseline_output_dir <- file.path("final_data/Analysis", "General", "baseline_characteristics")
            log_enhanced(sprintf("Using fallback directories for %s (mapped to %s): treatment_duration=%s, baseline=%s", 
                               cohort_name, cohort_dir_key, treatment_duration_dir, baseline_output_dir), level = "WARN")
        }

        dir.create(treatment_duration_dir, showWarnings = FALSE, recursive = TRUE)
        dir.create(baseline_output_dir, showWarnings = FALSE, recursive = TRUE)

        log_enhanced("Calculating treatment duration metrics", level = "INFO")
        duration_metrics <- calculate_treatment_duration_metrics(data)

        write.csv(duration_metrics$interval_metrics, file.path(treatment_duration_dir, paste0(prefix, "treatment_duration_metrics.csv")), row.names = FALSE)
        write.csv(duration_metrics$summary_stats, file.path(treatment_duration_dir, paste0(prefix, "treatment_duration_summary.csv")), row.names = FALSE)

        log_enhanced("Preparing variables for table", level = "INFO")
        data <- data %>% select(all_of(vars_to_summarize), treatment_group)

        log_enhanced("Checking variable levels for statistical testing", level = "INFO")
        vars_with_insufficient_levels <- c()
        for (var in vars_to_summarize) {
            if (var %in% names(data)) {
                if (is.factor(data[[var]]) || is.character(data[[var]])) {
                    level_counts <- table(data[[var]], useNA = "no")
                    valid_levels <- sum(level_counts > 0)
                    if (valid_levels < 2) {
                        log_enhanced(sprintf("Variable '%s' has insufficient levels for statistical testing (%d levels). Will display but skip p-value. Counts: %s", var, valid_levels, paste(names(level_counts), "=", level_counts, collapse=", ")), level = "INFO")
                        vars_with_insufficient_levels <- c(vars_with_insufficient_levels, var)
                    }
                }
            } else {
                log_enhanced(sprintf("Variable '%s' not found in data, excluding from summary table", var), level = "WARNING")
            }
        }

        available_vars <- intersect(vars_to_summarize, names(data))
        log_enhanced(sprintf("Displaying %d baseline variables (%d have insufficient levels for testing)", length(available_vars), length(vars_with_insufficient_levels)), level = "INFO")
        if (length(vars_with_insufficient_levels) > 0) {
            log_enhanced(sprintf("Variables with insufficient levels for p-values: %s", paste(vars_with_insufficient_levels, collapse = ", ")), level = "INFO")
        }

        data <- data %>% select(all_of(available_vars), treatment_group)

        log_enhanced("Creating summary table", level = "INFO")
        tbl <- data %>%
            tbl_summary(
                by = treatment_group,
                type = list(
                    age_at_diagnosis ~ "continuous",
                    initial_vision ~ "continuous",
                    initial_tumor_height ~ "continuous",
                    initial_tumor_diameter ~ "continuous"
                ),
                statistic = list(
                    all_continuous() ~ "{mean} ({sd})",
                    all_categorical() ~ "{n} ({p}%)"
                ),
                digits = list(all_continuous() ~ 1, all_categorical() ~ 1),
                missing = "no",
                label = STANDARD_TABLE_LABELS[intersect(names(STANDARD_TABLE_LABELS), available_vars)]
            ) %>%
            add_overall()

        log_enhanced("Adding statistical tests (will skip variables with insufficient levels)", level = "INFO")
        tbl <- tryCatch({
            tbl %>% add_p(test = list(all_categorical() ~ "fisher.test"), test.args = list(all_categorical() ~ list(simulate.p.value = TRUE)))
        }, error = function(e) {
            log_enhanced(sprintf("Some statistical tests failed (expected for variables with <2 levels): %s", e$message), level = "INFO")
            tbl
        })

        tbl <- tbl %>%
            bold_labels() %>%
            modify_header(label = "**Characteristic**", stat_0 = "**Overall**\nN = {N}") %>%
            modify_caption("Baseline Characteristics")

        gt_tbl <- tryCatch({
            as_gt(tbl)
        }, error = function(e) {
            log_enhanced(sprintf("Error converting to gt: %s", e$message), level = "ERROR")
            NULL
        })

        if (!is.null(gt_tbl)) {
            summary_tbl <- gt_tbl %>%
                tab_options(source_notes.padding = px(8)) %>%
                tab_source_note("Generated by create_summary_tables")

            duration_tbl <- duration_metrics$interval_metrics %>%
                gt() %>%
                tab_header(title = "Treatment Duration by Interval and Group") %>%
                fmt_number(columns = where(is.numeric), decimals = 0)

            save_gt_html(duration_tbl, filename = file.path(treatment_duration_dir, paste0(prefix, "treatment_duration.html")))
            save_gt_html(summary_tbl, filename = file.path(treatment_duration_dir, paste0(prefix, "treatment_duration_summary.html")))

            save_gt_html(tbl, filename = file.path(baseline_output_dir, paste0(prefix, "baseline_characteristics.html")))

            return(list(baseline_table = tbl, duration_table = duration_tbl, summary_table = summary_tbl))
        } else {
            return(list(baseline_table = tbl, duration_table = NULL, summary_table = NULL))
        }
    })

    names(tables) <- names(data_list)
    return(tables)
}
