#' Export Cohort Summary Statistics
#'
#' Generates a comprehensive JSON summary file with all cohort counts,
#' treatment breakdowns, outcome counts, and exclusion details.
#' This file is automatically regenerated with each analysis run.
#'
#' @param cohort_list Named list of cohort data frames from apply_criteria()
#' @param removal_log Data frame with removal details from apply_criteria()
#' @param output_path Path where JSON summary should be saved
#' @param output_dirs Optional list of cohort-specific output directories for
#'   writing per-cohort summary artifacts in `00_General/`
#'
#' @return Invisibly returns the summary list; writes JSON and optional
#'   cohort-specific TSV/text files to disk
#' @export
export_cohort_summary <- function(cohort_list, removal_log = NULL, output_path = NULL, output_dirs = NULL) {
    logger::log_info("=== GENERATING COHORT SUMMARY STATISTICS ===")

    if (is.null(output_path)) {
        output_path <- file.path(PROCESSED_DATA_DIR, "cohort_summary_statistics.json")
    }

    add_followup_status <- function(data) {
        data %>%
            mutate(
                days_since_last_contact = as.numeric(difftime(VITAL_STATUS_DATA_CUTOFF_DATE, last_known_alive_date, units = "days")),
                followup_status = case_when(
                    death_event == 1 ~ "dead",
                    days_since_last_contact <= LOST_TO_FOLLOWUP_CUTOFF_DAYS ~ "alive",
                    TRUE ~ "lost_to_followup"
                )
            )
    }

    get_cohort_definitions <- function() {
        list(
            full_cohort = list(
                runtime_dataset_id = "uveal_melanoma_full_cohort",
                output_folder = "uveal_full/",
                cohort_label = "Full Cohort",
                cohort_description = "Canonical all-comers treatment cohort",
                cohort_construction = "Start from the cleaned master dataset, apply global exclusions, derive all analytic variables once, then retain every patient treated with either GKSRS or PBT.",
                cohort_purpose = "Preserves the real-world treatment population and serves as the broadest cohort for descriptive and outcome analyses.",
                cohort_rule_summary = "All patients treated with either GKSRS or PBT after global exclusions.",
                criteria = NULL
            ),
            restricted_cohort = list(
                runtime_dataset_id = "uveal_melanoma_restricted_cohort",
                output_folder = "uveal_restricted/",
                cohort_label = "Restricted Cohort",
                cohort_description = "Dual-eligibility comparison cohort",
                cohort_construction = sprintf(
                    "Subset the full cohort to patients who meet the predefined criteria for both modalities: tumor diameter <= %d mm, tumor height <= %d mm, and no optic nerve involvement.",
                    TUMOR_DIAMETER_THRESHOLD,
                    TUMOR_HEIGHT_THRESHOLD
                ),
                cohort_purpose = "Minimizes treatment-selection bias when directly comparing GKSRS and PBT.",
                cohort_rule_summary = sprintf(
                    "Eligible for both modalities: tumor diameter <= %d mm, tumor height <= %d mm, and no optic nerve involvement.",
                    TUMOR_DIAMETER_THRESHOLD,
                    TUMOR_HEIGHT_THRESHOLD
                ),
                criteria = list(
                    diameter_mm = paste0("<=", TUMOR_DIAMETER_THRESHOLD),
                    height_mm = paste0("<=", TUMOR_HEIGHT_THRESHOLD),
                    optic_nerve_abutment = FALSE
                )
            ),
            gksrs_only_cohort = list(
                runtime_dataset_id = "uveal_melanoma_gksrs_only_cohort",
                output_folder = "gksrs/",
                cohort_label = "GKSRS-Only Cohort",
                cohort_description = "Modality-limited challenging-case cohort",
                cohort_construction = sprintf(
                    "Subset the full cohort to patients who fail PBT eligibility because of tumor diameter > %d mm, tumor height > %d mm, or optic nerve involvement.",
                    TUMOR_DIAMETER_THRESHOLD,
                    TUMOR_HEIGHT_THRESHOLD
                ),
                cohort_purpose = "Isolates the population in which GKSRS may still be clinically feasible when PBT is not.",
                cohort_rule_summary = sprintf(
                    "Fails PBT eligibility because tumor diameter > %d mm, tumor height > %d mm, or optic nerve involvement.",
                    TUMOR_DIAMETER_THRESHOLD,
                    TUMOR_HEIGHT_THRESHOLD
                ),
                criteria = list(
                    diameter_mm = paste0(">", TUMOR_DIAMETER_THRESHOLD, " OR"),
                    height_mm = paste0(">", TUMOR_HEIGHT_THRESHOLD, " OR"),
                    optic_nerve_abutment = TRUE
                )
            )
        )
    }

    get_named_value <- function(items, name, default = 0) {
        if (is.null(items) || is.null(items[[name]])) {
            return(default)
        }

        items[[name]]
    }

    # Helper to get treatment breakdown
    get_treatment_breakdown <- function(data) {
        # Get treatment counts in correct order (PBT first, as reference)
        treatment_counts <- table(data$treatment_group)
        treatment_list <- as.list(setNames(as.numeric(treatment_counts), names(treatment_counts)))

        # Reorder to put PBT first if it exists
        if ("PBT" %in% names(treatment_list)) {
            treatment_list <- treatment_list[c("PBT", setdiff(names(treatment_list), "PBT"))]
        }

        # Get outcome counts
        local_recurrence <- sum(data$recurrence1 == "Yes", na.rm = TRUE)
        metastasis <- sum(data$mets_progression == "Yes", na.rm = TRUE)

        data_with_followup_status <- add_followup_status(data)

        alive <- sum(data_with_followup_status$followup_status == "alive", na.rm = TRUE)
        dead <- sum(data_with_followup_status$followup_status == "dead", na.rm = TRUE)
        lost_to_followup <- sum(data_with_followup_status$followup_status == "lost_to_followup", na.rm = TRUE)

        list(
            total = nrow(data),
            treatment_groups = treatment_list,
            outcomes = list(
                local_recurrence = local_recurrence,
                metastasis = metastasis,
                alive = alive,
                dead = dead,
                lost_to_followup = lost_to_followup
            )
        )
    }

    # Helper to get treatment-specific outcomes
    get_treatment_outcomes <- function(data) {
        # Get treatments in correct order (PBT first, as reference)
        treatments <- levels(data$treatment_group)
        if (is.null(treatments)) {
            treatments <- sort(unique(as.character(data$treatment_group)))
        }

        data_with_followup_status <- add_followup_status(data)

        treatment_list <- list()

        for (tx in treatments) {
            tx_data <- data_with_followup_status %>% filter(treatment_group == tx)
            treatment_list[[as.character(tx)]] <- list(
                n = nrow(tx_data),
                local_recurrence = sum(tx_data$recurrence1 == "Yes", na.rm = TRUE),
                metastasis = sum(tx_data$mets_progression == "Yes", na.rm = TRUE),
                alive = sum(tx_data$followup_status == "alive", na.rm = TRUE),
                dead = sum(tx_data$followup_status == "dead", na.rm = TRUE),
                lost_to_followup = sum(tx_data$followup_status == "lost_to_followup", na.rm = TRUE)
            )
        }

        treatment_list
    }

    build_cohort_summary_entry <- function(data, definition) {
        cohort_summary <- c(
            get_treatment_breakdown(data),
            list(treatments = get_treatment_outcomes(data))
        )

        if (!is.null(definition$criteria)) {
            cohort_summary$criteria <- definition$criteria
        }

        cohort_summary
    }

    build_exclusion_summary <- function(removal_log) {
        if (!is.null(removal_log) && nrow(removal_log) > 0) {
            return(list(
                total_excluded = nrow(removal_log),
                by_reason = removal_log %>%
                    group_by(removal_reason) %>%
                    summarise(count = n(), .groups = "drop") %>%
                    {setNames(as.list(.$count), .$removal_reason)},
                by_step = removal_log %>%
                    group_by(removal_step) %>%
                    summarise(count = n(), .groups = "drop") %>%
                    {setNames(as.list(.$count), .$removal_step)}
            ))
        }

        list(
            total_excluded = 0,
            note = "No removal log provided or no exclusions applied"
        )
    }

    build_cohort_summary_row <- function(cohort_key, cohort_summary, definition) {
        pbt_summary <- cohort_summary$treatments$PBT
        gksrs_summary <- cohort_summary$treatments$GKSRS
        criteria <- cohort_summary$criteria

        tibble::tibble(
            cohort_key = cohort_key,
            runtime_dataset_id = definition$runtime_dataset_id,
            output_folder = definition$output_folder,
            cohort_label = definition$cohort_label,
            cohort_description = definition$cohort_description,
            cohort_construction = definition$cohort_construction,
            cohort_purpose = definition$cohort_purpose,
            cohort_rule_summary = definition$cohort_rule_summary,
            total_n = cohort_summary$total,
            treatment_n_pbt = get_named_value(cohort_summary$treatment_groups, "PBT"),
            treatment_n_gksrs = get_named_value(cohort_summary$treatment_groups, "GKSRS"),
            local_recurrence_n = cohort_summary$outcomes$local_recurrence,
            metastasis_n = cohort_summary$outcomes$metastasis,
            alive_n = cohort_summary$outcomes$alive,
            lost_to_followup_n = cohort_summary$outcomes$lost_to_followup,
            dead_n = cohort_summary$outcomes$dead,
            criteria_diameter_mm = if (is.null(criteria)) NA_character_ else as.character(criteria$diameter_mm),
            criteria_height_mm = if (is.null(criteria)) NA_character_ else as.character(criteria$height_mm),
            criteria_optic_nerve_abutment = if (is.null(criteria)) NA else criteria$optic_nerve_abutment,
            pbt_n = get_named_value(pbt_summary, "n"),
            pbt_local_recurrence_n = get_named_value(pbt_summary, "local_recurrence"),
            pbt_metastasis_n = get_named_value(pbt_summary, "metastasis"),
            pbt_alive_n = get_named_value(pbt_summary, "alive"),
            pbt_lost_to_followup_n = get_named_value(pbt_summary, "lost_to_followup"),
            pbt_dead_n = get_named_value(pbt_summary, "dead"),
            gksrs_n = get_named_value(gksrs_summary, "n"),
            gksrs_local_recurrence_n = get_named_value(gksrs_summary, "local_recurrence"),
            gksrs_metastasis_n = get_named_value(gksrs_summary, "metastasis"),
            gksrs_alive_n = get_named_value(gksrs_summary, "alive"),
            gksrs_lost_to_followup_n = get_named_value(gksrs_summary, "lost_to_followup"),
            gksrs_dead_n = get_named_value(gksrs_summary, "dead")
        )
    }

    render_treatment_summary_lines <- function(treatment_name, treatment_summary) {
        c(
            sprintf("%s (N = %d)", treatment_name, get_named_value(treatment_summary, "n")),
            sprintf("  - Local recurrence: %d", get_named_value(treatment_summary, "local_recurrence")),
            sprintf("  - Metastasis: %d", get_named_value(treatment_summary, "metastasis")),
            sprintf("  - Alive: %d", get_named_value(treatment_summary, "alive")),
            sprintf("  - Lost to follow-up: %d", get_named_value(treatment_summary, "lost_to_followup")),
            sprintf("  - Dead: %d", get_named_value(treatment_summary, "dead")),
            ""
        )
    }

    render_cohort_summary_text <- function(cohort_summary, definition) {
        criteria <- cohort_summary$criteria

        selection_lines <- c(
            "Selection Rule:",
            sprintf("  %s", definition$cohort_rule_summary),
            ""
        )

        if (!is.null(criteria)) {
            selection_lines <- c(
                selection_lines,
                "Criteria Fields:",
                sprintf("  - Tumor diameter: %s", criteria$diameter_mm),
                sprintf("  - Tumor height: %s", criteria$height_mm),
                sprintf("  - Optic nerve abutment: %s", ifelse(isTRUE(criteria$optic_nerve_abutment), "Yes", "No")),
                ""
            )
        }

        c(
            sprintf("%s Summary", toupper(definition$cohort_label)),
            paste(rep("=", nchar(definition$cohort_label) + 8), collapse = ""),
            sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
            sprintf("Runtime dataset id: %s", definition$runtime_dataset_id),
            sprintf("Output folder: %s", definition$output_folder),
            "",
            "What It Means:",
            sprintf("  %s", definition$cohort_description),
            "",
            "How It Is Constructed:",
            sprintf("  %s", definition$cohort_construction),
            "",
            "Why It Exists:",
            sprintf("  %s", definition$cohort_purpose),
            "",
            selection_lines,
            sprintf("Overall Cohort Counts (N = %d)", cohort_summary$total),
            sprintf("  - PBT: %d", get_named_value(cohort_summary$treatment_groups, "PBT")),
            sprintf("  - GKSRS: %d", get_named_value(cohort_summary$treatment_groups, "GKSRS")),
            sprintf("  - Local recurrence: %d", cohort_summary$outcomes$local_recurrence),
            sprintf("  - Metastasis: %d", cohort_summary$outcomes$metastasis),
            sprintf("  - Alive: %d", cohort_summary$outcomes$alive),
            sprintf("  - Lost to follow-up: %d", cohort_summary$outcomes$lost_to_followup),
            sprintf("  - Dead: %d", cohort_summary$outcomes$dead),
            "",
            "Treatment Arm Details:",
            render_treatment_summary_lines("PBT", cohort_summary$treatments$PBT),
            render_treatment_summary_lines("GKSRS", cohort_summary$treatments$GKSRS),
            "Vital Status Classification:",
            sprintf("  - Data cutoff date: %s", format(VITAL_STATUS_DATA_CUTOFF_DATE, "%Y-%m-%d")),
            sprintf("  - Lost to follow-up threshold: %d days", LOST_TO_FOLLOWUP_CUTOFF_DAYS)
        )
    }

    write_cohort_summary_artifacts <- function(summary_data, output_dirs) {
        cohort_definitions <- get_cohort_definitions()

        for (cohort_key in names(cohort_definitions)) {
            cohort_dirs <- output_dirs[[cohort_key]]
            if (is.null(cohort_dirs) || !"baseline_characteristics" %in% names(cohort_dirs)) {
                next
            }

            general_dir <- dirname(cohort_dirs$baseline_characteristics)
            if (!dir.exists(general_dir)) {
                dir.create(general_dir, recursive = TRUE, showWarnings = FALSE)
            }

            cohort_summary <- summary_data$cohorts[[cohort_key]]
            definition <- cohort_definitions[[cohort_key]]
            summary_row <- build_cohort_summary_row(cohort_key, cohort_summary, definition)
            summary_lines <- render_cohort_summary_text(cohort_summary, definition)

            tsv_path <- file.path(general_dir, "cohort_summary.tsv")
            txt_path <- file.path(general_dir, "cohort_summary.txt")

            readr::write_tsv(summary_row, tsv_path, na = "")
            writeLines(summary_lines, txt_path)

            logger::log_info(sprintf("Cohort summary TSV written: %s", tsv_path))
            logger::log_info(sprintf("Cohort summary text written: %s", txt_path))
        }
    }

    # Build comprehensive summary
    cohort_definitions <- get_cohort_definitions()
    summary_data <- list(
        metadata = list(
            generated_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            generated_by = "export_cohort_summary()",
            description = "Automated cohort summary statistics - regenerated with each analysis run"
        ),

        cohorts = lapply(cohort_definitions, function(definition) {
            build_cohort_summary_entry(cohort_list[[definition$runtime_dataset_id]], definition)
        }),

        exclusions = build_exclusion_summary(removal_log)
    )

    # Write JSON file with pretty formatting
    json_output <- jsonlite::toJSON(summary_data, pretty = TRUE, auto_unbox = TRUE)
    writeLines(json_output, output_path)

    if (!is.null(output_dirs)) {
        write_cohort_summary_artifacts(summary_data, output_dirs)
    }

    logger::log_info(sprintf("Cohort summary exported to: %s", output_path))
    logger::log_info(sprintf("  Full cohort: n=%d", summary_data$cohorts$full_cohort$total))
    logger::log_info(sprintf("  Restricted cohort: n=%d", summary_data$cohorts$restricted_cohort$total))
    logger::log_info(sprintf("  GKSRS-only cohort: n=%d", summary_data$cohorts$gksrs_only_cohort$total))

    invisible(summary_data)
}
