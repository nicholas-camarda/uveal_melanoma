#' Export Cohort Summary Statistics
#'
#' Generates a comprehensive JSON summary file with all cohort counts,
#' treatment breakdowns, outcome counts, and exclusion details.
#' This file is automatically regenerated with each analysis run.
#'
#' @param cohort_list Named list of cohort data frames from apply_criteria()
#' @param removal_log Data frame with removal details from apply_criteria()
#' @param output_path Path where JSON summary should be saved
#'
#' @return Invisibly returns the summary list; writes JSON to disk
#' @export
export_cohort_summary <- function(cohort_list, removal_log = NULL, output_path = NULL) {
    logger::log_info("=== GENERATING COHORT SUMMARY STATISTICS ===")
    
    if (is.null(output_path)) {
        output_path <- file.path(PROCESSED_DATA_DIR, "cohort_summary_statistics.json")
    }
    
    # Helper to get treatment breakdown
    get_treatment_breakdown <- function(data, cohort_name) {
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
        
        # Get vital status counts
        # Data current as of 3/4/2025 per data dictionary
        data_cutoff_date <- as.Date("2025-03-04")
        # Lost to follow-up = no contact within 450 days (~15 months) of data cutoff
        lost_to_followup_cutoff_days <- 450
        
        data_with_followup_status <- data %>%
            mutate(
                days_since_last_contact = as.numeric(difftime(data_cutoff_date, last_known_alive_date, units = "days")),
                followup_status = case_when(
                    death_event == 1 ~ "dead",
                    days_since_last_contact <= lost_to_followup_cutoff_days ~ "alive",
                    TRUE ~ "lost_to_followup"
                )
            )
        
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
        
        # Calculate lost to follow-up status
        data_cutoff_date <- as.Date("2025-03-04")
        lost_to_followup_cutoff_days <- 450
        
        data_with_followup_status <- data %>%
            mutate(
                days_since_last_contact = as.numeric(difftime(data_cutoff_date, last_known_alive_date, units = "days")),
                followup_status = case_when(
                    death_event == 1 ~ "dead",
                    days_since_last_contact <= lost_to_followup_cutoff_days ~ "alive",
                    TRUE ~ "lost_to_followup"
                )
            )
        
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
    
    # Build comprehensive summary
    summary_data <- list(
        metadata = list(
            generated_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            generated_by = "export_cohort_summary()",
            description = "Automated cohort summary statistics - regenerated with each analysis run"
        ),
        
        cohorts = list(
            full_cohort = c(
                get_treatment_breakdown(cohort_list$uveal_melanoma_full_cohort, "Full Cohort"),
                list(treatments = get_treatment_outcomes(cohort_list$uveal_melanoma_full_cohort))
            ),
            
            restricted_cohort = c(
                get_treatment_breakdown(cohort_list$uveal_melanoma_restricted_cohort, "Restricted Cohort"),
                list(
                    criteria = list(
                        diameter_mm = "<=20",
                        height_mm = "<=10",
                        optic_nerve_abutment = FALSE
                    ),
                    treatments = get_treatment_outcomes(cohort_list$uveal_melanoma_restricted_cohort)
                )
            ),
            
            gksrs_only_cohort = c(
                get_treatment_breakdown(cohort_list$uveal_melanoma_gksrs_only_cohort, "GKSRS-Only Cohort"),
                list(
                    criteria = list(
                        diameter_mm = ">20 OR",
                        height_mm = ">10 OR",
                        optic_nerve_abutment = TRUE
                    ),
                    treatments = get_treatment_outcomes(cohort_list$uveal_melanoma_gksrs_only_cohort)
                )
            )
        ),
        
        exclusions = if (!is.null(removal_log) && nrow(removal_log) > 0) {
            list(
                total_excluded = nrow(removal_log),
                by_reason = removal_log %>%
                    group_by(removal_reason) %>%
                    summarise(count = n(), .groups = "drop") %>%
                    {setNames(as.list(.$count), .$removal_reason)},
                by_step = removal_log %>%
                    group_by(removal_step) %>%
                    summarise(count = n(), .groups = "drop") %>%
                    {setNames(as.list(.$count), .$removal_step)}
            )
        } else {
            list(
                total_excluded = 0,
                note = "No removal log provided or no exclusions applied"
            )
        }
    )
    
    # Write JSON file with pretty formatting
    json_output <- jsonlite::toJSON(summary_data, pretty = TRUE, auto_unbox = TRUE)
    writeLines(json_output, output_path)
    
    logger::log_info(sprintf("Cohort summary exported to: %s", output_path))
    logger::log_info(sprintf("  Full cohort: n=%d", summary_data$cohorts$full_cohort$total))
    logger::log_info(sprintf("  Restricted cohort: n=%d", summary_data$cohorts$restricted_cohort$total))
    logger::log_info(sprintf("  GKSRS-only cohort: n=%d", summary_data$cohorts$gksrs_only_cohort$total))
    
    invisible(summary_data)
}
