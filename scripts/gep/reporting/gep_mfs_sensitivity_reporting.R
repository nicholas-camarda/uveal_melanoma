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
        dplyr::summarise(
            analysis_subset = analysis_subset,
            n = dplyr::n(),
            observed_events_5yr = sum(.data$observed_events_5yr, na.rm = TRUE),
            followup_ge_5yr = sum(.data$five_year_followup_view == "followup_ge_5yr", na.rm = TRUE),
            censored_pre_5yr = sum(.data$five_year_followup_view == "censored_pre_5yr", na.rm = TRUE),
            expected_survival_5yr = mean(.data$expected_mfs_5yr, na.rm = TRUE),
            expected_risk_5yr = mean(.data$predicted_mfs_risk_5yr, na.rm = TRUE),
            expected_events_5yr = sum(.data$predicted_mfs_risk_5yr, na.rm = TRUE),
            actual_mfs_5yr = mean(.data$actual_mfs_5yr, na.rm = TRUE),
            pbt_n = sum(.data$treatment_group == "PBT", na.rm = TRUE),
            gksrs_n = sum(.data$treatment_group == "GKSRS", na.rm = TRUE),
            nonstandard_treatment_n = sum(
                !is.na(.data$treatment_group) &
                    !.data$treatment_group %in% c("PBT", "GKSRS"),
                na.rm = TRUE
            ),
            treatment_mix = format_objective4_treatment_mix_label(.data$treatment_group),
            .groups = "drop"
        ) %>%
        dplyr::mutate(
            oe_ratio_5yr = dplyr::if_else(
                .data$expected_events_5yr > 0,
                .data$observed_events_5yr / .data$expected_events_5yr,
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
        dplyr::summarise(
            analysis_subset = analysis_subset,
            n = dplyr::n(),
            observed_events_5yr = sum(.data$observed_events_5yr, na.rm = TRUE),
            followup_ge_5yr = sum(.data$five_year_followup_view == "followup_ge_5yr", na.rm = TRUE),
            censored_pre_5yr = sum(.data$five_year_followup_view == "censored_pre_5yr", na.rm = TRUE),
            expected_survival_5yr = mean(.data$expected_mfs_5yr, na.rm = TRUE),
            expected_risk_5yr = mean(.data$predicted_mfs_risk_5yr, na.rm = TRUE),
            expected_events_5yr = sum(.data$predicted_mfs_risk_5yr, na.rm = TRUE),
            actual_mfs_5yr = mean(.data$actual_mfs_5yr, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        dplyr::mutate(
            oe_ratio_5yr = dplyr::if_else(
                .data$expected_events_5yr > 0,
                .data$observed_events_5yr / .data$expected_events_5yr,
                NA_real_
            )
        ) %>%
        dplyr::select(
            analysis_subset,
            gep_class_simple,
            treatment_group,
            n,
            observed_events_5yr,
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
            stringsAsFactors = FALSE
        ))
    }

    summary_df %>%
        dplyr::transmute(
            gep_class_simple = .data$gep_class_simple,
            class_event_label = sprintf("5-year mets: %d/%d", .data$observed_events_5yr, .data$n),
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

    operational_overall <- if (nrow(mfs_data) > 0) {
        mfs_data %>%
            dplyr::count(.data$operational_followup_status, name = "n") %>%
            dplyr::mutate(
                cohort_label = cohort_label,
                proportion = .data$n / sum(.data$n)
            )
    } else {
        data.frame(
            operational_followup_status = character(),
            n = integer(),
            cohort_label = character(),
            proportion = numeric(),
            stringsAsFactors = FALSE
        )
    }

    operational_by_class <- if (nrow(mfs_data) > 0) {
        mfs_data %>%
            dplyr::count(.data$gep_class_simple, .data$operational_followup_status, name = "n") %>%
            dplyr::group_by(.data$gep_class_simple) %>%
            dplyr::mutate(proportion_within_class = .data$n / sum(.data$n)) %>%
            dplyr::ungroup()
    } else {
        data.frame(
            gep_class_simple = character(),
            operational_followup_status = character(),
            n = integer(),
            proportion_within_class = numeric(),
            stringsAsFactors = FALSE
        )
    }

    operational_by_class_treatment <- if (nrow(mfs_data) > 0) {
        mfs_data %>%
            dplyr::count(.data$gep_class_simple, .data$treatment_group, .data$operational_followup_status, name = "n") %>%
            dplyr::group_by(.data$gep_class_simple, .data$treatment_group) %>%
            dplyr::mutate(proportion_within_class_treatment = .data$n / sum(.data$n)) %>%
            dplyr::ungroup()
    } else {
        data.frame(
            gep_class_simple = character(),
            treatment_group = character(),
            operational_followup_status = character(),
            n = integer(),
            proportion_within_class_treatment = numeric(),
            stringsAsFactors = FALSE
        )
    }

    horizon_overall <- if (nrow(mfs_data) > 0) {
        mfs_data %>%
            dplyr::count(.data$five_year_followup_view, name = "n") %>%
            dplyr::mutate(
                cohort_label = cohort_label,
                proportion = .data$n / sum(.data$n)
            )
    } else {
        data.frame(
            five_year_followup_view = character(),
            n = integer(),
            cohort_label = character(),
            proportion = numeric(),
            stringsAsFactors = FALSE
        )
    }

    horizon_by_class <- if (nrow(mfs_data) > 0) {
        mfs_data %>%
            dplyr::count(.data$gep_class_simple, .data$five_year_followup_view, name = "n") %>%
            dplyr::group_by(.data$gep_class_simple) %>%
            dplyr::mutate(proportion_within_class = .data$n / sum(.data$n)) %>%
            dplyr::ungroup()
    } else {
        data.frame(
            gep_class_simple = character(),
            five_year_followup_view = character(),
            n = integer(),
            proportion_within_class = numeric(),
            stringsAsFactors = FALSE
        )
    }

    horizon_by_class_treatment <- if (nrow(mfs_data) > 0) {
        mfs_data %>%
            dplyr::count(.data$gep_class_simple, .data$treatment_group, .data$five_year_followup_view, name = "n") %>%
            dplyr::group_by(.data$gep_class_simple, .data$treatment_group) %>%
            dplyr::mutate(proportion_within_class_treatment = .data$n / sum(.data$n)) %>%
            dplyr::ungroup()
    } else {
        data.frame(
            gep_class_simple = character(),
            treatment_group = character(),
            five_year_followup_view = character(),
            n = integer(),
            proportion_within_class_treatment = numeric(),
            stringsAsFactors = FALSE
        )
    }

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
    summary_path <- file.path(output_dir, paste0(prefix, "mfs_sensitivity_summary.txt"))

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

    narrative_lines <- c(
        "OBJECTIVE 4 MFS SENSITIVITY SUMMARY",
        "===================================",
        "",
        sprintf("Cohort: %s", sensitivity_results$cohort_label),
        sprintf("5-year censoring view evaluated at %d months.", 60),
        sprintf(
            "Operational lost-to-follow-up uses cutoff date %s and threshold %d days.",
            format(VITAL_STATUS_DATA_CUTOFF_DATE, "%Y-%m-%d"),
            LOST_TO_FOLLOWUP_CUTOFF_DAYS
        ),
        ""
    )

    if (nrow(followup_horizon) > 0) {
        narrative_lines <- c(narrative_lines, "5-YEAR FOLLOW-UP VIEW:")
        for (i in seq_len(nrow(followup_horizon))) {
            narrative_lines <- c(
                narrative_lines,
                sprintf(
                    "  - %s: %d (%.1f%%)",
                    followup_horizon$five_year_followup_view[i],
                    followup_horizon$n[i],
                    100 * followup_horizon$proportion[i]
                )
            )
        }
        narrative_lines <- c(narrative_lines, "")
    }

    if (nrow(followup_operational) > 0) {
        narrative_lines <- c(narrative_lines, "OPERATIONAL FOLLOW-UP VIEW:")
        for (i in seq_len(nrow(followup_operational))) {
            narrative_lines <- c(
                narrative_lines,
                sprintf(
                    "  - %s: %d (%.1f%%)",
                    followup_operational$operational_followup_status[i],
                    followup_operational$n[i],
                    100 * followup_operational$proportion[i]
                )
            )
        }
        narrative_lines <- c(narrative_lines, "")
    }

    if (nrow(class_summary) > 0) {
        narrative_lines <- c(narrative_lines, "CLASS-LEVEL 5-YEAR MFS SUMMARY:")
        for (i in seq_len(nrow(class_summary))) {
            narrative_lines <- c(
                narrative_lines,
                sprintf(
                    "  - %s: events %d/%d, expected survival %.1f%%, actual MFS %.1f%%, O/E %.2f, tx mix %s",
                    class_summary$gep_class_simple[i],
                    class_summary$observed_events_5yr[i],
                    class_summary$n[i],
                    100 * class_summary$expected_survival_5yr[i],
                    100 * class_summary$actual_mfs_5yr[i],
                    class_summary$oe_ratio_5yr[i],
                    class_summary$treatment_mix[i]
                )
            )
        }
        narrative_lines <- c(narrative_lines, "")
    }

    narrative_lines <- c(
        narrative_lines,
        "REPEAT/MULTIPLE RADIATION CHECK:",
        sprintf(
            "  - Repeat/multiple radiation exposure rows in MFS-eligible cohort: %d",
            repeat_quality$repeat_radiation_exposure_n[1] %||% 0
        ),
        sprintf(
            "  - Rows with both initial modalities flagged: %d",
            repeat_quality$both_initial_modalities_n[1] %||% 0
        ),
        "",
        "GUARDRAIL NOTES:"
    )
    narrative_lines <- c(
        narrative_lines,
        paste0("  - ", sensitivity_results$guardrail_notes$note)
    )

    event_diagnostics <- sensitivity_results$mfs_event_diagnostics %||% data.frame()
    if (nrow(event_diagnostics) > 0) {
        narrative_lines <- c(narrative_lines, "", "EVENT-ROW DIAGNOSTICS:")
        for (gep_class in unique(event_diagnostics$gep_class_simple)) {
            class_rows <- event_diagnostics %>%
                dplyr::filter(.data$gep_class_simple == gep_class)
            row_ids <- as.character(class_rows$row_id)
            row_id_text <- if (length(row_ids) > 0) paste(row_ids, collapse = ", ") else "none"
            narrative_lines <- c(
                narrative_lines,
                sprintf("  - %s event row IDs: %s", gep_class, row_id_text)
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
