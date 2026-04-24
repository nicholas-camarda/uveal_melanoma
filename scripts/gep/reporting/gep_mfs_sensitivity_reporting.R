# Objective 4 MFS sensitivity reporting helpers

#' Format an Objective 4 cohort label for reader-facing outputs
#'
#' @param dataset_name Optional character dataset identifier.
#'
#' @return Character scalar cohort label.
format_objective4_gep_cohort_label <- function(dataset_name = NULL) {
    if (is.null(dataset_name) || !nzchar(dataset_name)) {
        return("Objective 4 cohort")
    }

    dplyr::case_when(
        identical(dataset_name, "uveal_melanoma_full_cohort") ~ "Full Cohort",
        identical(dataset_name, "uveal_melanoma_restricted_cohort") ~ "Restricted Cohort",
        identical(dataset_name, "uveal_melanoma_gksrs_only_cohort") ~ "GKSRS-Only Cohort",
        TRUE ~ tools::toTitleCase(gsub("_", " ", gsub("uveal_melanoma_|_cohort", "", dataset_name)))
    )
}

#' Add operational follow-up status used in cohort summary exports
#'
#' @param data Data frame containing `death_event` and `last_known_alive_date`.
#'
#' @return Data frame with `days_since_last_contact` and
#'   `operational_followup_status`.
add_objective4_operational_followup_status <- function(data) {
    death_event_flag <- if ("death_event" %in% names(data)) {
        !is.na(data$death_event) &
            (data$death_event == 1 | as.character(data$death_event) == "Yes")
    } else {
        rep(FALSE, nrow(data))
    }

    last_known_alive_date <- if ("last_known_alive_date" %in% names(data)) {
        as.Date(data$last_known_alive_date)
    } else {
        as.Date(rep(NA_character_, nrow(data)))
    }

    days_since_last_contact <- as.numeric(difftime(
        VITAL_STATUS_DATA_CUTOFF_DATE,
        last_known_alive_date,
        units = "days"
    ))

    data %>%
        dplyr::mutate(
            days_since_last_contact = days_since_last_contact,
            operational_followup_status = dplyr::case_when(
                death_event_flag ~ "dead",
                !is.na(.data$days_since_last_contact) &
                    .data$days_since_last_contact <= LOST_TO_FOLLOWUP_CUTOFF_DAYS ~ "alive",
                TRUE ~ "lost_to_followup"
            )
        )
}

#' Resolve the Survival Time Variable for an Objective 4 Endpoint
#'
#' @param event_prefix Character endpoint prefix, currently `"mfs"` or `"mss"`.
#'
#' @return Character scalar naming the time-to-event column.
resolve_objective4_followup_time_var <- function(event_prefix) {
    dplyr::case_when(
        identical(event_prefix, "mfs") ~ "tt_mets_months",
        identical(event_prefix, "mss") ~ "tt_death_months",
        TRUE ~ NA_character_
    )
}

#' Summarize median follow-up for Objective 4 narrative text
#'
#' @param data Data frame used for the reader-facing validation narrative.
#'
#' @return Named list with overall and GKSRS-only median follow-up years.
summarize_objective4_followup_medians <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
        return(list(
            median_followup_overall = NA_real_,
            median_followup_gksrs_only = NA_real_
        ))
    }

    followup_years <- if ("follow_up_years" %in% names(data)) {
        as.numeric(data$follow_up_years)
    } else if ("follow_up_days" %in% names(data)) {
        as.numeric(data$follow_up_days) / 365.25
    } else if (all(c("date_diagnosis", "last_known_alive_date") %in% names(data))) {
        as.numeric(difftime(
            as.Date(data$last_known_alive_date),
            as.Date(data$date_diagnosis),
            units = "days"
        )) / 365.25
    } else {
        rep(NA_real_, nrow(data))
    }

    valid_followup <- is.finite(followup_years) & followup_years >= 0

    overall_median <- if (any(valid_followup)) {
        stats::median(followup_years[valid_followup])
    } else {
        NA_real_
    }

    gksrs_only_followup <- if ("consort_group" %in% names(data)) {
        tolower(as.character(data$consort_group)) == "gksrs_only"
    } else {
        rep(FALSE, nrow(data))
    }
    gksrs_valid_followup <- valid_followup & gksrs_only_followup

    gksrs_median <- if (any(gksrs_valid_followup)) {
        stats::median(followup_years[gksrs_valid_followup])
    } else {
        NA_real_
    }

    list(
        median_followup_overall = overall_median,
        median_followup_gksrs_only = gksrs_median
    )
}

#' Collect Endpoint-Specific Objective 4 Follow-Up Summaries
#'
#' @param data Data frame passed into Objective 4.
#' @param dataset_name Optional dataset identifier for display restoration.
#' @param eligibility_filter Character eligibility flag column.
#' @param event_prefix Character endpoint prefix used in event columns.
#' @param time_horizon_years Integer horizon in years.
#'
#' @return Named list with operational follow-up summaries, horizon follow-up
#'   summaries, and compact narrative metadata for the requested endpoint.
collect_objective4_endpoint_followup_summary <- function(data,
                                                         dataset_name = NULL,
                                                         eligibility_filter,
                                                         event_prefix,
                                                         time_horizon_years = 5) {
    prepared_data <- refresh_gep_analysis_flags(data)
    if ("treatment_group" %in% names(prepared_data)) {
        prepared_data <- normalize_treatment_group_data(prepared_data, columns = "treatment_group")
    }
    prepared_data <- restore_gep_display_variables(prepared_data, dataset_name = dataset_name)
    prepared_data <- add_objective4_operational_followup_status(prepared_data)
    followup_medians <- summarize_objective4_followup_medians(prepared_data)

    if (!eligibility_filter %in% names(prepared_data)) {
        prepared_data[[eligibility_filter]] <- FALSE
    }

    event_col <- paste0(event_prefix, "_event_", time_horizon_years, "yr")
    time_var <- resolve_objective4_followup_time_var(event_prefix)
    cohort_label <- format_objective4_gep_cohort_label(dataset_name)
    followup_ge_label <- paste0("followup_ge_", time_horizon_years, "yr")
    censored_label <- paste0("censored_pre_", time_horizon_years, "yr")
    event_label <- paste0("event_by_", time_horizon_years, "yr")

    empty_operational <- data.frame(
        operational_followup_status = character(),
        n = integer(),
        cohort_label = character(),
        proportion = numeric(),
        stringsAsFactors = FALSE
    )
    empty_operational_by_class <- data.frame(
        gep_class_simple = character(),
        operational_followup_status = character(),
        n = integer(),
        proportion_within_class = numeric(),
        stringsAsFactors = FALSE
    )
    empty_operational_by_class_treatment <- data.frame(
        gep_class_simple = character(),
        treatment_group = character(),
        operational_followup_status = character(),
        n = integer(),
        proportion_within_class_treatment = numeric(),
        stringsAsFactors = FALSE
    )
    empty_horizon <- data.frame(
        horizon_followup_view = character(),
        n = integer(),
        cohort_label = character(),
        proportion = numeric(),
        stringsAsFactors = FALSE
    )
    empty_horizon_by_class <- data.frame(
        gep_class_simple = character(),
        horizon_followup_view = character(),
        n = integer(),
        proportion_within_class = numeric(),
        stringsAsFactors = FALSE
    )
    empty_horizon_by_class_treatment <- data.frame(
        gep_class_simple = character(),
        treatment_group = character(),
        horizon_followup_view = character(),
        n = integer(),
        proportion_within_class_treatment = numeric(),
        stringsAsFactors = FALSE
    )
    empty_censored_operational_breakdown <- data.frame(
        operational_followup_status = character(),
        n = integer(),
        proportion_within_censored = numeric(),
        stringsAsFactors = FALSE
    )

    if (!all(c(event_col, time_var) %in% names(prepared_data))) {
        return(list(
            cohort_label = cohort_label,
            eligibility_n = 0L,
            event_col = event_col,
            time_var = time_var,
            horizon_years = time_horizon_years,
            event_label = event_label,
            followup_ge_label = followup_ge_label,
            censored_label = censored_label,
            operational_overall = empty_operational,
            operational_by_class = empty_operational_by_class,
            operational_by_class_treatment = empty_operational_by_class_treatment,
            horizon_overall = empty_horizon,
            horizon_by_class = empty_horizon_by_class,
            horizon_by_class_treatment = empty_horizon_by_class_treatment,
            censored_operational_breakdown = empty_censored_operational_breakdown,
            impact_level = "unavailable",
            median_followup_overall = followup_medians$median_followup_overall,
            median_followup_gksrs_only = followup_medians$median_followup_gksrs_only,
            limitation_line = sprintf(
                "%d-year follow-up limitation data were unavailable for this endpoint.",
                time_horizon_years
            )
        ))
    }

    eligible_data <- prepared_data %>%
        dplyr::filter(.data[[eligibility_filter]]) %>%
        dplyr::mutate(
            gep_class_simple = if ("gep_class_simple" %in% names(.)) {
                as.character(.data$gep_class_simple)
            } else if ("biopsy1_gep" %in% names(.)) {
                as.character(.data$biopsy1_gep)
            } else {
                NA_character_
            },
            treatment_group = if ("treatment_group" %in% names(.)) {
                as.character(.data$treatment_group)
            } else {
                NA_character_
            },
            horizon_followup_view = dplyr::case_when(
                !is.na(.data[[event_col]]) & .data[[event_col]] == 1 ~ event_label,
                !is.na(.data[[time_var]]) & .data[[time_var]] >= (time_horizon_years * 12) ~ followup_ge_label,
                TRUE ~ censored_label
            )
        )
    followup_medians <- summarize_objective4_followup_medians(eligible_data)

    if (nrow(eligible_data) == 0) {
        return(list(
            cohort_label = cohort_label,
            eligibility_n = 0L,
            event_col = event_col,
            time_var = time_var,
            horizon_years = time_horizon_years,
            event_label = event_label,
            followup_ge_label = followup_ge_label,
            censored_label = censored_label,
            operational_overall = empty_operational,
            operational_by_class = empty_operational_by_class,
            operational_by_class_treatment = empty_operational_by_class_treatment,
            horizon_overall = empty_horizon,
            horizon_by_class = empty_horizon_by_class,
            horizon_by_class_treatment = empty_horizon_by_class_treatment,
            censored_operational_breakdown = empty_censored_operational_breakdown,
            impact_level = "unavailable",
            median_followup_overall = followup_medians$median_followup_overall,
            median_followup_gksrs_only = followup_medians$median_followup_gksrs_only,
            limitation_line = sprintf(
                "No %s-eligible rows were available for the %d-year follow-up summary.",
                event_prefix,
                time_horizon_years
            )
        ))
    }

    operational_overall <- eligible_data %>%
        dplyr::count(.data$operational_followup_status, name = "n") %>%
        dplyr::mutate(
            cohort_label = cohort_label,
            proportion = .data$n / sum(.data$n)
        )

    operational_by_class <- eligible_data %>%
        dplyr::count(.data$gep_class_simple, .data$operational_followup_status, name = "n") %>%
        dplyr::group_by(.data$gep_class_simple) %>%
        dplyr::mutate(proportion_within_class = .data$n / sum(.data$n)) %>%
        dplyr::ungroup()

    operational_by_class_treatment <- eligible_data %>%
        dplyr::count(.data$gep_class_simple, .data$treatment_group, .data$operational_followup_status, name = "n") %>%
        dplyr::group_by(.data$gep_class_simple, .data$treatment_group) %>%
        dplyr::mutate(proportion_within_class_treatment = .data$n / sum(.data$n)) %>%
        dplyr::ungroup()

    horizon_overall <- eligible_data %>%
        dplyr::count(.data$horizon_followup_view, name = "n") %>%
        dplyr::mutate(
            cohort_label = cohort_label,
            proportion = .data$n / sum(.data$n)
        )

    horizon_by_class <- eligible_data %>%
        dplyr::count(.data$gep_class_simple, .data$horizon_followup_view, name = "n") %>%
        dplyr::group_by(.data$gep_class_simple) %>%
        dplyr::mutate(proportion_within_class = .data$n / sum(.data$n)) %>%
        dplyr::ungroup()

    horizon_by_class_treatment <- eligible_data %>%
        dplyr::count(.data$gep_class_simple, .data$treatment_group, .data$horizon_followup_view, name = "n") %>%
        dplyr::group_by(.data$gep_class_simple, .data$treatment_group) %>%
        dplyr::mutate(proportion_within_class_treatment = .data$n / sum(.data$n)) %>%
        dplyr::ungroup()

    censored_operational_breakdown <- eligible_data %>%
        dplyr::filter(.data$horizon_followup_view == censored_label) %>%
        dplyr::count(.data$operational_followup_status, name = "n") %>%
        dplyr::mutate(proportion_within_censored = .data$n / sum(.data$n))

    overall_total <- sum(horizon_overall$n, na.rm = TRUE)
    censored_n <- horizon_overall$n[horizon_overall$horizon_followup_view == censored_label] %||% 0L
    followup_ge_n <- horizon_overall$n[horizon_overall$horizon_followup_view == followup_ge_label] %||% 0L
    censored_prop <- if (overall_total > 0) censored_n / overall_total else NA_real_
    followup_ge_prop <- if (overall_total > 0) followup_ge_n / overall_total else NA_real_

    class_totals <- horizon_by_class %>%
        dplyr::group_by(.data$gep_class_simple) %>%
        dplyr::summarise(class_n = sum(.data$n, na.rm = TRUE), .groups = "drop")
    class_censored_props <- class_totals %>%
        dplyr::left_join(
            horizon_by_class %>%
                dplyr::filter(.data$horizon_followup_view == censored_label) %>%
                dplyr::select(
                    gep_class_simple,
                    censored_n = n,
                    proportion_within_class
                ),
            by = "gep_class_simple"
        ) %>%
        dplyr::mutate(
            censored_n = dplyr::coalesce(.data$censored_n, 0L),
            proportion_within_class = dplyr::coalesce(.data$proportion_within_class, 0)
        ) %>%
        dplyr::arrange(dplyr::desc(.data$proportion_within_class))

    class_imbalance <- FALSE
    class_imbalance_clause <- NULL
    if (nrow(class_censored_props) >= 2) {
        top_class <- class_censored_props$gep_class_simple[1]
        top_prop <- class_censored_props$proportion_within_class[1]
        bottom_class <- class_censored_props$gep_class_simple[nrow(class_censored_props)]
        bottom_prop <- class_censored_props$proportion_within_class[nrow(class_censored_props)]

        if (is.finite(top_prop) && is.finite(bottom_prop) && (top_prop - bottom_prop) >= 0.20) {
            class_imbalance <- TRUE
            class_imbalance_clause <- sprintf(
                "%s had more pre-%d-year censoring than %s",
                top_class,
                time_horizon_years,
                bottom_class
            )
        }
    }

    impact_level <- dplyr::case_when(
        is.finite(censored_prop) && censored_prop >= 0.35 && is.finite(followup_ge_prop) && censored_prop > followup_ge_prop ~ "high",
        is.finite(censored_prop) && censored_prop >= 0.25 ~ "moderate",
        TRUE ~ "low"
    )

    limitation_line <- dplyr::case_when(
        identical(impact_level, "high") && class_imbalance ~ sprintf(
            "%d-year estimates may be unstable because many patients were censored before %d years; %s.",
            time_horizon_years,
            time_horizon_years,
            class_imbalance_clause
        ),
        identical(impact_level, "high") ~ sprintf(
            "%d-year estimates may be unstable because many patients were censored before %d years.",
            time_horizon_years,
            time_horizon_years
        ),
        identical(impact_level, "moderate") && class_imbalance ~ sprintf(
            "%d-year estimates should be interpreted cautiously because follow-up before %d years was limited; %s.",
            time_horizon_years,
            time_horizon_years,
            class_imbalance_clause
        ),
        identical(impact_level, "moderate") ~ sprintf(
            "%d-year estimates should be interpreted cautiously because follow-up before %d years was limited.",
            time_horizon_years,
            time_horizon_years
        ),
        class_imbalance ~ sprintf(
            "%d-year estimates should be interpreted with follow-up context because %s.",
            time_horizon_years,
            class_imbalance_clause
        ),
        TRUE ~ sprintf(
            "%d-year estimates should be interpreted with routine follow-up context.",
            time_horizon_years
        )
    )

    list(
        cohort_label = cohort_label,
        eligibility_n = nrow(eligible_data),
        event_col = event_col,
        time_var = time_var,
        horizon_years = time_horizon_years,
        event_label = event_label,
        followup_ge_label = followup_ge_label,
        censored_label = censored_label,
        operational_overall = operational_overall,
        operational_by_class = operational_by_class,
        operational_by_class_treatment = operational_by_class_treatment,
        horizon_overall = horizon_overall,
        horizon_by_class = horizon_by_class,
        horizon_by_class_treatment = horizon_by_class_treatment,
        censored_operational_breakdown = censored_operational_breakdown,
        impact_level = impact_level,
        median_followup_overall = followup_medians$median_followup_overall,
        median_followup_gksrs_only = followup_medians$median_followup_gksrs_only,
        limitation_line = limitation_line
    )
}

#' Build a Compact Objective 4 Follow-Up Limitation Narrative Block
#'
#' @param followup_summary Follow-up summary list from
#'   `collect_objective4_endpoint_followup_summary()`.
#' @param include_heading Logical; when `TRUE`, prepend the block heading.
#'
#' @return Character vector of narrative lines.
build_objective4_followup_limitation_block <- function(followup_summary, include_heading = TRUE) {
    if (is.null(followup_summary)) {
        return(character())
    }

    horizon_years <- followup_summary$horizon_years %||% 5L
    total_n <- followup_summary$eligibility_n %||% 0L
    followup_ge_label <- followup_summary$followup_ge_label %||% paste0("followup_ge_", horizon_years, "yr")
    event_label <- followup_summary$event_label %||% paste0("event_by_", horizon_years, "yr")
    censored_label <- followup_summary$censored_label %||% paste0("censored_pre_", horizon_years, "yr")
    horizon_overall <- followup_summary$horizon_overall %||% data.frame()
    horizon_by_class <- followup_summary$horizon_by_class %||% data.frame()
    operational_overall <- followup_summary$operational_overall %||% data.frame()

    lines <- character()
    if (isTRUE(include_heading)) {
        lines <- c(lines, md_heading(sprintf("Follow-Up Limitation (%d-year)", horizon_years), 2L))
    }

    if (total_n == 0 || nrow(horizon_overall) == 0) {
        return(c(lines, followup_summary$limitation_line %||% "Follow-up limitation data unavailable."))
    }

    horizon_counts <- stats::setNames(horizon_overall$n, horizon_overall$horizon_followup_view)
    operational_counts <- stats::setNames(operational_overall$n, operational_overall$operational_followup_status)
    censored_breakdown <- followup_summary$censored_operational_breakdown %||% data.frame()
    horizon_censored_n <- if (censored_label %in% names(horizon_counts)) horizon_counts[[censored_label]] else 0L
    horizon_event_n <- if (event_label %in% names(horizon_counts)) horizon_counts[[event_label]] else 0L
    horizon_followup_ge_n <- if (followup_ge_label %in% names(horizon_counts)) horizon_counts[[followup_ge_label]] else 0L
    operational_alive_n <- if ("alive" %in% names(operational_counts)) operational_counts[["alive"]] else 0L
    operational_dead_n <- if ("dead" %in% names(operational_counts)) operational_counts[["dead"]] else 0L
    operational_lost_n <- if ("lost_to_followup" %in% names(operational_counts)) operational_counts[["lost_to_followup"]] else 0L

    class_censored <- horizon_by_class %>%
        dplyr::filter(.data$horizon_followup_view == censored_label) %>%
        dplyr::arrange(.data$gep_class_simple)

    class_line <- NULL
    if (nrow(class_censored) > 0) {
        class_bits <- vapply(seq_len(nrow(class_censored)), function(i) {
            sprintf(
                "%s %s %d/%d (%.1f%%)",
                class_censored$gep_class_simple[i],
                censored_label,
                class_censored$n[i],
                sum(horizon_by_class$n[horizon_by_class$gep_class_simple == class_censored$gep_class_simple[i]], na.rm = TRUE),
                100 * class_censored$proportion_within_class[i]
            )
        }, character(1))
        class_line <- paste(class_bits, collapse = "; ")
    }

    lines <- c(
        lines,
        followup_summary$limitation_line,
        if (is.finite(followup_summary$median_followup_overall %||% NA_real_)) {
            md_bullet(sprintf(
                "Median follow-up among the %d-patient %s GEP validation subset: %.1f years.",
                total_n,
                followup_summary$cohort_label %||% "validation cohort",
                followup_summary$median_followup_overall
            ))
        } else {
            character()
        },
        md_bullet(sprintf(
            "`%s` means follow-up reached at least %d years without the endpoint occurring before %d years; `censored_pre_%dyr` means follow-up ended before %d years without an observed endpoint.",
            followup_ge_label,
            horizon_years,
            horizon_years,
            horizon_years,
            horizon_years
        )),
        md_bullet(sprintf(
            "%d-year view: %s %d/%d (%.1f%%); %s %d/%d (%.1f%%); %s %d/%d (%.1f%%)",
            horizon_years,
            censored_label,
            horizon_censored_n,
            total_n,
            100 * (horizon_censored_n / total_n),
            event_label,
            horizon_event_n,
            total_n,
            100 * (horizon_event_n / total_n),
            followup_ge_label,
            horizon_followup_ge_n,
            total_n,
            100 * (horizon_followup_ge_n / total_n)
        ))
    )

    if (!is.null(class_line) && nzchar(class_line)) {
        lines <- c(lines, md_bullet(paste0("By class: ", class_line)))
    }

    lines <- c(
        lines,
        md_bullet(sprintf(
            "Operational view: alive %d/%d (%.1f%%); dead %d/%d (%.1f%%); lost_to_followup %d/%d (%.1f%%)",
            operational_alive_n,
            total_n,
            100 * (operational_alive_n / total_n),
            operational_dead_n,
            total_n,
            100 * (operational_dead_n / total_n),
            operational_lost_n,
            total_n,
            100 * (operational_lost_n / total_n)
        ))
    )

    if (nrow(censored_breakdown) > 0) {
        censored_counts <- stats::setNames(censored_breakdown$n, censored_breakdown$operational_followup_status)
        censored_total <- sum(censored_breakdown$n, na.rm = TRUE)
        censored_alive_n <- if ("alive" %in% names(censored_counts)) censored_counts[["alive"]] else 0L
        censored_dead_n <- if ("dead" %in% names(censored_counts)) censored_counts[["dead"]] else 0L
        censored_lost_n <- if ("lost_to_followup" %in% names(censored_counts)) censored_counts[["lost_to_followup"]] else 0L
        lines <- c(
            lines,
            md_bullet(sprintf(
                "Among the %d patients censored before %d years, alive %d (%.1f%%), dead %d (%.1f%%), and lost_to_followup %d (%.1f%%).",
                censored_total,
                horizon_years,
                censored_alive_n,
                100 * (censored_alive_n / censored_total),
                censored_dead_n,
                100 * (censored_dead_n / censored_total),
                censored_lost_n,
                100 * (censored_lost_n / censored_total)
            ))
        )
    }

    lines
}

#' Harmonize 5-year MFS summary inputs across Objective 4 code paths
#'
#' @param data Data frame containing at least `gep_class_simple` and the 5-year
#'   MFS inputs.
#'
#' @return Data frame with the derived columns needed by the class-level MFS
#'   summary helpers.
harmonize_objective4_mfs_summary_input <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
        return(data.frame(
            gep_class_simple = character(),
            treatment_group = character(),
            expected_mfs_5yr = numeric(),
            predicted_mfs_risk_5yr = numeric(),
            observed_events_5yr = integer(),
            actual_mfs_5yr = numeric(),
            five_year_followup_view = character(),
            stringsAsFactors = FALSE
        ))
    }

    harmonized_data <- data
    if ("treatment_group" %in% names(harmonized_data)) {
        harmonized_data <- normalize_treatment_group_data(harmonized_data, columns = "treatment_group")
    }

    observed_events_5yr <- if ("observed_events_5yr" %in% names(harmonized_data)) {
        as.integer(harmonized_data$observed_events_5yr)
    } else if ("mfs_event_5yr" %in% names(harmonized_data)) {
        as.integer(!is.na(harmonized_data$mfs_event_5yr) & harmonized_data$mfs_event_5yr == 1)
    } else {
        rep(0L, nrow(harmonized_data))
    }

    expected_mfs_5yr <- if ("expected_mfs_5yr" %in% names(harmonized_data)) {
        as.numeric(harmonized_data$expected_mfs_5yr)
    } else if ("biopsy1_gep_mfs" %in% names(harmonized_data)) {
        as.numeric(harmonized_data$biopsy1_gep_mfs)
    } else {
        rep(NA_real_, nrow(harmonized_data))
    }

    predicted_mfs_risk_5yr <- if ("predicted_mfs_risk_5yr" %in% names(harmonized_data)) {
        as.numeric(harmonized_data$predicted_mfs_risk_5yr)
    } else {
        1 - expected_mfs_5yr
    }

    five_year_followup_view <- if ("five_year_followup_view" %in% names(harmonized_data)) {
        as.character(harmonized_data$five_year_followup_view)
    } else {
        dplyr::case_when(
            observed_events_5yr == 1 ~ "event_by_5yr",
            "tt_mets_months" %in% names(harmonized_data) &
                !is.na(harmonized_data$tt_mets_months) &
                harmonized_data$tt_mets_months >= 60 ~ "followup_ge_5yr",
            TRUE ~ "censored_pre_5yr"
        )
    }

    actual_mfs_5yr <- if ("actual_mfs_5yr" %in% names(harmonized_data)) {
        as.numeric(harmonized_data$actual_mfs_5yr)
    } else {
        1 - observed_events_5yr
    }

    harmonized_data %>%
        dplyr::mutate(
            gep_class_simple = as.character(.data$gep_class_simple),
            treatment_group = if ("treatment_group" %in% names(harmonized_data)) {
                as.character(.data$treatment_group)
            } else {
                NA_character_
            },
            expected_mfs_5yr = expected_mfs_5yr,
            predicted_mfs_risk_5yr = predicted_mfs_risk_5yr,
            observed_events_5yr = observed_events_5yr,
            actual_mfs_5yr = actual_mfs_5yr,
            five_year_followup_view = five_year_followup_view
        )
}

#' Format treatment composition as a compact label
#'
#' @param treatment_values Vector of treatment labels.
#'
#' @return Character scalar summarizing treatment counts.
format_objective4_treatment_mix_label <- function(treatment_values) {
    clean_values <- as.character(treatment_values)
    clean_values <- clean_values[!is.na(clean_values) & nzchar(clean_values)]

    if (length(clean_values) == 0) {
        return("No treatment data")
    }

    tx_counts <- table(clean_values, useNA = "no")
    tx_order <- c("PBT", "GKSRS")
    ordered_names <- c(tx_order[tx_order %in% names(tx_counts)], setdiff(names(tx_counts), tx_order))

    paste(
        sprintf("%s=%d", ordered_names, as.integer(tx_counts[ordered_names])),
        collapse = ", "
    )
}

#' Pick a Stable Objective 4 Row Identifier Column
#'
#' Selects the first available row identifier column so diagnostics can point
#' to exact patient rows when sensitivity counts change.
#'
#' @param data Data frame containing candidate identifier columns.
#'
#' @return Character scalar column name or `NULL` when no supported identifier
#'   exists.
pick_objective4_row_id_column <- function(data) {
    id_candidates <- c("id", "patient_id", "record_id", "case_id", "study_id")
    id_col <- id_candidates[id_candidates %in% names(data)][1]

    if (length(id_col) == 0 || is.na(id_col)) {
        return(NULL)
    }

    id_col
}

#' Build Objective 4 MFS Event Row Diagnostics
#'
#' Returns the exact MFS-eligible rows that contributed events by GEP class.
#' This is a debug aid for identifying changes in event counts between runs.
#'
#' @param data Prepared Objective 4 MFS sensitivity dataset.
#'
#' @return Data frame with one row per contributing event row, or an empty data
#'   frame when no stable identifier is available.
build_objective4_mfs_event_diagnostics <- function(data) {
    if (is.null(data) || nrow(data) == 0 || !"mfs_analysis_eligible" %in% names(data)) {
        return(data.frame())
    }

    id_col <- pick_objective4_row_id_column(data)
    if (is.null(id_col) || !"mfs_event_5yr" %in% names(data)) {
        return(data.frame())
    }

    event_rows <- data %>%
        dplyr::filter(.data$mfs_analysis_eligible) %>%
        dplyr::filter(!is.na(.data$mfs_event_5yr) & .data$mfs_event_5yr == 1) %>%
        dplyr::mutate(row_id = .data[[id_col]])

    if (nrow(event_rows) == 0) {
        return(data.frame())
    }

    diagnostic_cols <- intersect(
        c("row_id", "gep_class_simple", "biopsy1_gep", "tt_mets_months", "expected_mfs_5yr", "predicted_mfs_risk_5yr", "treatment_group"),
        names(event_rows)
    )

    event_rows %>%
        dplyr::select(dplyr::all_of(diagnostic_cols)) %>%
        dplyr::arrange(.data$gep_class_simple, .data$row_id)
}

#' Prepare the Objective 4 MFS sensitivity dataset
#'
#' @param data Data frame passed into Objective 4.
#' @param dataset_name Optional dataset identifier for display restoration.
#'
#' @return Data frame restricted to `mfs_analysis_eligible` rows with the
#'   derived sensitivity columns needed for reporting.
prepare_objective4_mfs_sensitivity_data <- function(data, dataset_name = NULL) {
    prepared_data <- refresh_gep_analysis_flags(data)

    if ("treatment_group" %in% names(prepared_data)) {
        prepared_data <- normalize_treatment_group_data(prepared_data, columns = "treatment_group")
    }

    prepared_data <- restore_gep_display_variables(prepared_data, dataset_name = dataset_name)
    cohort_label <- format_objective4_gep_cohort_label(dataset_name)

    if (!"mfs_analysis_eligible" %in% names(prepared_data)) {
        prepared_data$mfs_analysis_eligible <- FALSE
    }

    prepared_data <- add_objective4_operational_followup_status(prepared_data)
    eligible_data <- prepared_data %>%
        dplyr::filter(.data$mfs_analysis_eligible)

    treatment_group_values <- if ("treatment_group" %in% names(eligible_data)) {
        as.character(eligible_data$treatment_group)
    } else {
        rep(NA_character_, nrow(eligible_data))
    }

    expected_mfs_5yr <- if ("expected_mfs_5yr" %in% names(eligible_data)) {
        as.numeric(eligible_data$expected_mfs_5yr)
    } else if ("biopsy1_gep_mfs" %in% names(eligible_data)) {
        as.numeric(eligible_data$biopsy1_gep_mfs)
    } else {
        rep(NA_real_, nrow(eligible_data))
    }

    predicted_mfs_risk_5yr <- if ("predicted_mfs_risk_5yr" %in% names(eligible_data)) {
        as.numeric(eligible_data$predicted_mfs_risk_5yr)
    } else {
        1 - expected_mfs_5yr
    }

    observed_events_5yr <- if ("mfs_event_5yr" %in% names(eligible_data)) {
        as.integer(!is.na(eligible_data$mfs_event_5yr) & eligible_data$mfs_event_5yr == 1)
    } else {
        rep(0L, nrow(eligible_data))
    }

    salvage_treatment <- if ("recurrence1_treatment_clean" %in% names(eligible_data)) {
        salvage_values <- as.character(eligible_data$recurrence1_treatment_clean)
        salvage_values[is.na(salvage_values) | salvage_values == ""] <- "None/Unknown"
        salvage_values
    } else {
        rep("None/Unknown", nrow(eligible_data))
    }

    both_initial_modalities <- if (all(c("initial_gk", "initial_plaque") %in% names(eligible_data))) {
        as.character(eligible_data$initial_gk) == "Y" & as.character(eligible_data$initial_plaque) == "Y"
    } else {
        rep(FALSE, nrow(eligible_data))
    }

    eligible_data %>%
        dplyr::mutate(
            cohort_dataset_name = dataset_name %||% NA_character_,
            cohort_label = cohort_label,
            gep_class_simple = as.character(.data$gep_class_simple),
            treatment_group = treatment_group_values,
            expected_mfs_5yr = expected_mfs_5yr,
            predicted_mfs_risk_5yr = predicted_mfs_risk_5yr,
            observed_events_5yr = observed_events_5yr,
            actual_mfs_5yr = 1 - .data$observed_events_5yr,
            five_year_followup_view = dplyr::case_when(
                .data$observed_events_5yr == 1 ~ "event_by_5yr",
                !is.na(.data$tt_mets_months) & .data$tt_mets_months >= 60 ~ "followup_ge_5yr",
                TRUE ~ "censored_pre_5yr"
            ),
            salvage_treatment = salvage_treatment,
            repeat_radiation_exposure = .data$salvage_treatment %in% c("GKSRS", "Plaque", "PBT"),
            both_initial_modalities = both_initial_modalities
        )
}

#' Summarize 5-year MFS by GEP class
#'
#' @param data Prepared Objective 4 MFS sensitivity dataset.
#' @param analysis_subset Character label describing the slice.
#'
#' @return Data frame with class-level 5-year MFS summary statistics.
summarize_objective4_mfs_by_class <- function(data, analysis_subset = "All eligible") {
    data <- harmonize_objective4_mfs_summary_input(data)
    if (is.null(data) || nrow(data) == 0) {
        return(data.frame(
            analysis_subset = character(),
            gep_class_simple = character(),
            n = integer(),
            observed_events_5yr = integer(),
            km_observed_events_5yr = numeric(),
            followup_ge_5yr = integer(),
            censored_pre_5yr = integer(),
            expected_survival_5yr = numeric(),
            expected_risk_5yr = numeric(),
            expected_events_5yr = numeric(),
            actual_mfs_5yr = numeric(),
            oe_ratio_5yr = numeric(),
            pbt_n = integer(),
            gksrs_n = integer(),
            nonstandard_treatment_n = integer(),
            pbt_prop = numeric(),
            gksrs_prop = numeric(),
            treatment_mix = character(),
            stringsAsFactors = FALSE
        ))
    }

    data %>%
        dplyr::group_by(.data$gep_class_simple) %>%
        dplyr::group_modify(~ {
            km_metrics <- estimate_mfs_km_at_horizon(
                data = .x,
                timepoint_months = 60
            )

            data.frame(
                analysis_subset = analysis_subset,
                n = nrow(.x),
                observed_events_5yr = sum(.x$observed_events_5yr, na.rm = TRUE),
                km_observed_events_5yr = km_metrics$observed_events,
                followup_ge_5yr = sum(.x$five_year_followup_view == "followup_ge_5yr", na.rm = TRUE),
                censored_pre_5yr = sum(.x$five_year_followup_view == "censored_pre_5yr", na.rm = TRUE),
                expected_survival_5yr = mean(.x$expected_mfs_5yr, na.rm = TRUE),
                expected_risk_5yr = mean(.x$predicted_mfs_risk_5yr, na.rm = TRUE),
                expected_events_5yr = sum(.x$predicted_mfs_risk_5yr, na.rm = TRUE),
                actual_mfs_5yr = km_metrics$survival,
                pbt_n = sum(.x$treatment_group == "PBT", na.rm = TRUE),
                gksrs_n = sum(.x$treatment_group == "GKSRS", na.rm = TRUE),
                nonstandard_treatment_n = sum(
                    !is.na(.x$treatment_group) &
                        !.x$treatment_group %in% c("PBT", "GKSRS"),
                    na.rm = TRUE
                ),
                treatment_mix = format_objective4_treatment_mix_label(.x$treatment_group),
                stringsAsFactors = FALSE
            )
        }) %>%
        dplyr::mutate(
            oe_ratio_5yr = dplyr::if_else(
                .data$expected_events_5yr > 0,
                .data$km_observed_events_5yr / .data$expected_events_5yr,
                NA_real_
            ),
            pbt_prop = dplyr::if_else(.data$n > 0, .data$pbt_n / .data$n, NA_real_),
            gksrs_prop = dplyr::if_else(.data$n > 0, .data$gksrs_n / .data$n, NA_real_)
        ) %>%
        dplyr::select(
            analysis_subset,
            gep_class_simple,
            n,
            observed_events_5yr,
            km_observed_events_5yr,
            followup_ge_5yr,
            censored_pre_5yr,
            expected_survival_5yr,
            expected_risk_5yr,
            expected_events_5yr,
            actual_mfs_5yr,
            oe_ratio_5yr,
            pbt_n,
            gksrs_n,
            nonstandard_treatment_n,
            pbt_prop,
            gksrs_prop,
            treatment_mix
        )
}

#' Summarize 5-year MFS by GEP class and treatment group
#'
#' @param data Prepared Objective 4 MFS sensitivity dataset.
#' @param analysis_subset Character label describing the slice.
#'
#' @return Data frame with class-by-treatment 5-year MFS summary statistics.
summarize_objective4_mfs_by_class_treatment <- function(data, analysis_subset = "All eligible") {
    data <- harmonize_objective4_mfs_summary_input(data)
    if (is.null(data) || nrow(data) == 0) {
        return(data.frame(
            analysis_subset = character(),
            gep_class_simple = character(),
            treatment_group = character(),
            n = integer(),
            observed_events_5yr = integer(),
            km_observed_events_5yr = numeric(),
            followup_ge_5yr = integer(),
            censored_pre_5yr = integer(),
            expected_survival_5yr = numeric(),
            expected_risk_5yr = numeric(),
            expected_events_5yr = numeric(),
            actual_mfs_5yr = numeric(),
            oe_ratio_5yr = numeric(),
            stringsAsFactors = FALSE
        ))
    }

    data %>%
        dplyr::group_by(.data$gep_class_simple, .data$treatment_group) %>%
        dplyr::group_modify(~ {
            km_metrics <- estimate_mfs_km_at_horizon(
                data = .x,
                timepoint_months = 60
            )

            data.frame(
                analysis_subset = analysis_subset,
                n = nrow(.x),
                observed_events_5yr = sum(.x$observed_events_5yr, na.rm = TRUE),
                km_observed_events_5yr = km_metrics$observed_events,
                followup_ge_5yr = sum(.x$five_year_followup_view == "followup_ge_5yr", na.rm = TRUE),
                censored_pre_5yr = sum(.x$five_year_followup_view == "censored_pre_5yr", na.rm = TRUE),
                expected_survival_5yr = mean(.x$expected_mfs_5yr, na.rm = TRUE),
                expected_risk_5yr = mean(.x$predicted_mfs_risk_5yr, na.rm = TRUE),
                expected_events_5yr = sum(.x$predicted_mfs_risk_5yr, na.rm = TRUE),
                actual_mfs_5yr = km_metrics$survival,
                stringsAsFactors = FALSE
            )
        }) %>%
        dplyr::mutate(
            oe_ratio_5yr = dplyr::if_else(
                .data$expected_events_5yr > 0,
                .data$km_observed_events_5yr / .data$expected_events_5yr,
                NA_real_
            )
        ) %>%
        dplyr::select(
            analysis_subset,
            gep_class_simple,
            treatment_group,
            n,
            observed_events_5yr,
            km_observed_events_5yr,
            followup_ge_5yr,
            censored_pre_5yr,
            expected_survival_5yr,
            expected_risk_5yr,
            expected_events_5yr,
            actual_mfs_5yr,
            oe_ratio_5yr
        )
}

#' Build 5-year MFS class annotations for reader-facing plots
#'
#' @param mfs_data Prepared Objective 4 MFS dataset or the simple-validation MFS
#'   subset.
#'
#' @return Data frame keyed by `gep_class_simple` with plot annotation text.
build_objective4_simple_mfs_plot_annotations <- function(mfs_data) {
    summary_df <- summarize_objective4_mfs_by_class(mfs_data, analysis_subset = "All eligible")
    if (nrow(summary_df) == 0) {
        return(data.frame(
            gep_class_simple = character(),
            plot_x_label = character(),
            class_event_label = character(),
            treatment_mix = character(),
            stringsAsFactors = FALSE
        ))
    }

    summary_df %>%
        dplyr::transmute(
            gep_class_simple = .data$gep_class_simple,
            class_event_label = sprintf("5-year mets: %d/%d", .data$observed_events_5yr, .data$n),
            treatment_mix = .data$treatment_mix,
            plot_x_label = sprintf(
                "%s\n5-year mets: %d/%d\nTx mix: %s",
                .data$gep_class_simple,
                .data$observed_events_5yr,
                .data$n,
                .data$treatment_mix
            )
        )
}

#' Build the 5-year MFS calibration caption block
#'
#' @param results MFS results container.
#' @param dataset_name Optional dataset identifier.
#'
#' @return Character scalar caption block, or `NULL` when 5-year annotations are
#'   unavailable.
build_objective4_mfs_calibration_caption <- function(results, dataset_name = NULL) {
    class_summary <- results$simple_class_summary_5yr %||% NULL
    if (is.null(class_summary) || !is.data.frame(class_summary) || nrow(class_summary) == 0) {
        return(NULL)
    }

    class_summary <- class_summary %>%
        dplyr::arrange(.data$gep_class_simple)

    class_lines <- sprintf(
        "%s: %d/%d 5-year metastasis events",
        class_summary$gep_class_simple,
        class_summary$observed_events_5yr,
        class_summary$n
    )

    paste(
        c(
            sprintf("5-year MFS cohort: %s", format_objective4_gep_cohort_label(dataset_name)),
            class_lines
        ),
        collapse = "\n"
    )
}

#' Collect Objective 4 MFS sensitivity results
#'
#' @param data Data frame passed into Objective 4.
#' @param dataset_name Optional dataset identifier.
#'
#' @return Named list containing the three requested sensitivity result groups
#'   plus guardrail notes.
collect_objective4_mfs_sensitivity_results <- function(data, dataset_name = NULL) {
    mfs_data <- prepare_objective4_mfs_sensitivity_data(data, dataset_name = dataset_name)
    cohort_label <- format_objective4_gep_cohort_label(dataset_name)
    followup_summary <- collect_objective4_endpoint_followup_summary(
        data = data,
        dataset_name = dataset_name,
        eligibility_filter = "mfs_analysis_eligible",
        event_prefix = "mfs",
        time_horizon_years = 5
    )

    operational_overall <- followup_summary$operational_overall
    operational_by_class <- followup_summary$operational_by_class
    operational_by_class_treatment <- followup_summary$operational_by_class_treatment
    horizon_overall <- followup_summary$horizon_overall %>%
        dplyr::rename(five_year_followup_view = horizon_followup_view)
    horizon_by_class <- followup_summary$horizon_by_class %>%
        dplyr::rename(five_year_followup_view = horizon_followup_view)
    horizon_by_class_treatment <- followup_summary$horizon_by_class_treatment %>%
        dplyr::rename(five_year_followup_view = horizon_followup_view)

    class_summary <- summarize_objective4_mfs_by_class(mfs_data, analysis_subset = "All eligible")
    class_treatment_summary <- summarize_objective4_mfs_by_class_treatment(mfs_data, analysis_subset = "All eligible")
    event_diagnostics <- build_objective4_mfs_event_diagnostics(mfs_data)

    pbt_only_data <- if (nrow(mfs_data) > 0) {
        dplyr::filter(mfs_data, .data$treatment_group == "PBT")
    } else {
        mfs_data
    }
    pbt_only_summary <- summarize_objective4_mfs_by_class(
        pbt_only_data,
        analysis_subset = "PBT-only descriptive slice"
    )

    repeat_exposure_summary <- if (nrow(mfs_data) > 0) {
        mfs_data %>%
            dplyr::count(
                .data$repeat_radiation_exposure,
                primary_treatment = .data$treatment_group,
                salvage_treatment = .data$salvage_treatment,
                name = "n"
            ) %>%
            dplyr::arrange(dplyr::desc(.data$repeat_radiation_exposure), .data$primary_treatment, dplyr::desc(.data$n))
    } else {
        data.frame(
            repeat_radiation_exposure = logical(),
            primary_treatment = character(),
            salvage_treatment = character(),
            n = integer(),
            stringsAsFactors = FALSE
        )
    }

    repeat_comparison <- dplyr::bind_rows(
        summarize_objective4_mfs_by_class(mfs_data, analysis_subset = "All eligible"),
        summarize_objective4_mfs_by_class(
            dplyr::filter(mfs_data, !.data$repeat_radiation_exposure),
            analysis_subset = "Exclude repeat/multiple radiation"
        ),
        summarize_objective4_mfs_by_class(
            dplyr::filter(mfs_data, .data$repeat_radiation_exposure),
            analysis_subset = "Repeat/multiple radiation only"
        )
    )

    data_quality_checks <- data.frame(
        cohort_label = cohort_label,
        eligible_n = nrow(mfs_data),
        repeat_radiation_exposure_n = sum(mfs_data$repeat_radiation_exposure, na.rm = TRUE),
        both_initial_modalities_n = sum(mfs_data$both_initial_modalities, na.rm = TRUE),
        stringsAsFactors = FALSE
    )

    guardrail_notes <- character()
    if (identical(dataset_name, "uveal_melanoma_gksrs_only_cohort")) {
        pbt_rows <- sum(mfs_data$treatment_group == "PBT", na.rm = TRUE)
        if (pbt_rows > 0) {
            guardrail_notes <- c(
                guardrail_notes,
                sprintf(
                    "Guardrail: the GKSRS-only cohort still contains %d PBT-labeled MFS-eligible row(s); sensitivity summaries report them explicitly rather than assuming treatment purity.",
                    pbt_rows
                )
            )
        }
    }
    if (sum(mfs_data$both_initial_modalities, na.rm = TRUE) > 0) {
        guardrail_notes <- c(
            guardrail_notes,
            sprintf(
                "Data-quality check: %d MFS-eligible row(s) show both initial_gk and initial_plaque flagged as Y.",
                sum(mfs_data$both_initial_modalities, na.rm = TRUE)
            )
        )
    }
    if (length(guardrail_notes) == 0) {
        guardrail_notes <- "No treatment-purity or mixed-initial-modality guardrail issues detected in the MFS-eligible Objective 4 rows."
    }

    list(
        cohort_label = cohort_label,
        mfs_followup_sensitivity = list(
            operational_overall = operational_overall,
            operational_by_class = operational_by_class,
            operational_by_class_treatment = operational_by_class_treatment,
            horizon_overall = horizon_overall,
            horizon_by_class = horizon_by_class,
            horizon_by_class_treatment = horizon_by_class_treatment
        ),
        mfs_treatment_mix_sensitivity = list(
            by_class = class_summary,
            by_class_treatment = class_treatment_summary,
            pbt_only = pbt_only_summary
        ),
        mfs_repeat_radiation_sensitivity = list(
            exposure_summary = repeat_exposure_summary,
            comparison_by_class = repeat_comparison,
            data_quality_checks = data_quality_checks
        ),
        mfs_event_diagnostics = event_diagnostics,
        guardrail_notes = data.frame(
            cohort_label = cohort_label,
            note = guardrail_notes,
            stringsAsFactors = FALSE
        )
    )
}

#' Write Objective 4 MFS sensitivity outputs
#'
#' @param sensitivity_results Result list from
#'   `collect_objective4_mfs_sensitivity_results()`.
#' @param output_dir Destination directory.
#' @param prefix Filename prefix.
#'
#' @return Named list containing the workbook and summary paths.
write_objective4_mfs_sensitivity_outputs <- function(sensitivity_results, output_dir, prefix = "") {
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    workbook_path <- file.path(output_dir, paste0(prefix, "mfs_sensitivity_summary.xlsx"))
    summary_path <- file.path(output_dir, paste0(prefix, "mfs_sensitivity_summary.md"))

    workbook_data <- list(
        Followup_Operational = sensitivity_results$mfs_followup_sensitivity$operational_overall,
        Followup_Operational_ByClass = sensitivity_results$mfs_followup_sensitivity$operational_by_class,
        Followup_Operational_ByTx = sensitivity_results$mfs_followup_sensitivity$operational_by_class_treatment,
        Followup_5yr = sensitivity_results$mfs_followup_sensitivity$horizon_overall,
        Followup_5yr_ByClass = sensitivity_results$mfs_followup_sensitivity$horizon_by_class,
        Followup_5yr_ByTx = sensitivity_results$mfs_followup_sensitivity$horizon_by_class_treatment,
        TxMix_ByClass = sensitivity_results$mfs_treatment_mix_sensitivity$by_class,
        TxMix_ByClassTx = sensitivity_results$mfs_treatment_mix_sensitivity$by_class_treatment,
        PBT_Only = sensitivity_results$mfs_treatment_mix_sensitivity$pbt_only,
        Repeat_Exposure = sensitivity_results$mfs_repeat_radiation_sensitivity$exposure_summary,
        Repeat_Comparison = sensitivity_results$mfs_repeat_radiation_sensitivity$comparison_by_class,
        Data_Quality = sensitivity_results$mfs_repeat_radiation_sensitivity$data_quality_checks,
        Guardrail_Notes = sensitivity_results$guardrail_notes
    )
    write_gep_workbook(workbook_data, workbook_path)

    class_summary <- sensitivity_results$mfs_treatment_mix_sensitivity$by_class
    repeat_quality <- sensitivity_results$mfs_repeat_radiation_sensitivity$data_quality_checks
    followup_horizon <- sensitivity_results$mfs_followup_sensitivity$horizon_overall
    followup_operational <- sensitivity_results$mfs_followup_sensitivity$operational_overall

    followup_horizon_table <- if (nrow(followup_horizon) > 0) {
        data.frame(
            View = followup_horizon$five_year_followup_view,
            Count = sprintf("%d (%.1f%%)", followup_horizon$n, 100 * followup_horizon$proportion),
            stringsAsFactors = FALSE
        )
    } else {
        data.frame()
    }

    followup_operational_table <- if (nrow(followup_operational) > 0) {
        data.frame(
            Status = followup_operational$operational_followup_status,
            Count = sprintf("%d (%.1f%%)", followup_operational$n, 100 * followup_operational$proportion),
            stringsAsFactors = FALSE
        )
    } else {
        data.frame()
    }

    class_summary_table <- if (nrow(class_summary) > 0) {
        data.frame(
            Class = class_summary$gep_class_simple,
            `Raw events` = sprintf("%d/%d", class_summary$observed_events_5yr, class_summary$n),
            `KM-observed MFS` = sprintf("%.1f%%", 100 * class_summary$actual_mfs_5yr),
            `Expected survival` = sprintf("%.1f%%", 100 * class_summary$expected_survival_5yr),
            `O/E` = sprintf("%.2f", class_summary$oe_ratio_5yr),
            `Tx mix` = class_summary$treatment_mix,
            stringsAsFactors = FALSE
        )
    } else {
        data.frame()
    }

    narrative_lines <- c(
        md_heading("Objective 4 MFS Sensitivity Summary", 1L),
        "",
        sprintf("Cohort: %s", sensitivity_results$cohort_label),
        sprintf("5-year censoring view evaluated at %d months.", 60),
        md_bullet("`followup_ge_5yr` means follow-up reached at least 5 years without metastasis before 5 years."),
        sprintf(
            "Operational lost-to-follow-up uses cutoff date %s and threshold %d days.",
            format(VITAL_STATUS_DATA_CUTOFF_DATE, "%Y-%m-%d"),
            LOST_TO_FOLLOWUP_CUTOFF_DAYS
        ),
        ""
    )

    if (nrow(followup_horizon) > 0) {
        narrative_lines <- c(
            narrative_lines,
            md_heading("5-Year Follow-Up View", 2L),
            md_table(followup_horizon_table),
            ""
        )
    }

    if (nrow(followup_operational) > 0) {
        narrative_lines <- c(
            narrative_lines,
            md_heading("Operational Follow-Up View", 2L),
            md_table(followup_operational_table),
            ""
        )
    }

    if (nrow(class_summary) > 0) {
        narrative_lines <- c(
            narrative_lines,
            md_heading("Class-Level 5-Year MFS Summary", 2L),
            md_table(class_summary_table),
            ""
        )
    }

    narrative_lines <- c(
        narrative_lines,
        md_heading("Repeat/Multiple Radiation Check", 2L),
        md_bullet(sprintf(
            "Repeat/multiple radiation exposure rows in MFS-eligible cohort: %d",
            repeat_quality$repeat_radiation_exposure_n[1] %||% 0
        )),
        md_bullet(sprintf(
            "Rows with both initial modalities flagged: %d",
            repeat_quality$both_initial_modalities_n[1] %||% 0
        )),
        "",
        md_heading("Guardrail Notes", 2L)
    )
    narrative_lines <- c(
        narrative_lines,
        md_bullet(sensitivity_results$guardrail_notes$note)
    )

    event_diagnostics <- sensitivity_results$mfs_event_diagnostics %||% data.frame()
    if (nrow(event_diagnostics) > 0) {
        narrative_lines <- c(narrative_lines, "", md_heading("Event-Row Diagnostics", 2L))
        for (gep_class in unique(event_diagnostics$gep_class_simple)) {
            class_rows <- event_diagnostics %>%
                dplyr::filter(.data$gep_class_simple == gep_class)
            row_ids <- as.character(class_rows$row_id)
            row_id_text <- if (length(row_ids) > 0) paste(row_ids, collapse = ", ") else "none"
            narrative_lines <- c(
                narrative_lines,
                md_bullet(sprintf("%s event row IDs: %s", gep_class, row_id_text))
            )
        }
    }

    writeLines(narrative_lines, summary_path)

    list(
        workbook = workbook_path,
        summary = summary_path
    )
}

#' Run Objective 4 MFS sensitivity summary generation
#'
#' @param data Data frame passed into Objective 4.
#' @param dataset_name Optional dataset identifier.
#' @param output_dir Destination directory for the sidecar artifacts.
#' @param prefix Filename prefix.
#'
#' @return Named list containing the collected sensitivity results and output
#'   paths.
run_objective4_mfs_sensitivity_summary <- function(data, dataset_name, output_dir, prefix = "") {
    sensitivity_results <- collect_objective4_mfs_sensitivity_results(
        data = data,
        dataset_name = dataset_name
    )
    output_paths <- write_objective4_mfs_sensitivity_outputs(
        sensitivity_results = sensitivity_results,
        output_dir = output_dir,
        prefix = prefix
    )

    c(
        sensitivity_results,
        list(output_paths = output_paths)
    )
}
