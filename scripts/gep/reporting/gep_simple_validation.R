# Simple GEP Validation (Project Goals)
# Computes expected vs actual 5-year MFS/MSS by GEP class and saves outputs

#' Create simple GEP validation plots
#'
#' Create side-by-side expected vs actual 5-year survival point plots for MFS
#' and MSS by GEP class, saving PNGs to disk.
#'
#' @param mfs_results Data frame of MFS class-level expected/actual rates
#' @param mss_results Data frame of MSS class-level expected/actual rates
#' @param mfs_output_dir Directory path to save the MFS image
#' @param mss_output_dir Directory path to save the MSS image
#' @param prefix Filename prefix for saved files
#' @return Invisibly returns NULL after writing files
create_simple_gep_plots <- function(mfs_results, mss_results, mfs_output_dir, mss_output_dir, prefix) {
    mfs_plot <- ggplot(mfs_results, aes(x = gep_class_simple)) +
        geom_point(aes(y = expected_rate, color = "Expected"), size = 3) +
        geom_point(aes(y = actual_rate, color = "Actual"), size = 3) +
        geom_segment(
            aes(
                x = gep_class_simple, xend = gep_class_simple,
                y = expected_rate, yend = actual_rate
            ),
            linetype = "dashed", alpha = 0.5
        ) +
        labs(
            title = "5-Year MFS: Expected vs Actual Rates",
            x = "GEP Class",
            y = "Survival Rate",
            color = "Rate Type"
        ) +
        theme_classic() +
        theme(
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white")
        ) +
        scale_color_manual(values = {
            pal <- get_qualitative_palette(2)
            names(pal) <- c("Expected", "Actual")
            pal
        })
    
    ggsave(file.path(mfs_output_dir, paste0(prefix, "simple_mfs_validation.png")),
        mfs_plot,
        width = SMALL_PLOT_WIDTH, height = SMALL_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white"
    )

    mss_plot <- ggplot(mss_results, aes(x = gep_class_simple)) +
        geom_point(aes(y = expected_rate, color = "Expected"), size = 3) +
        geom_point(aes(y = actual_rate, color = "Actual"), size = 3) +
        geom_segment(
            aes(
                x = gep_class_simple, xend = gep_class_simple,
                y = expected_rate, yend = actual_rate
            ),
            linetype = "dashed", alpha = 0.5
        ) +
        labs(
            title = "5-Year MSS: Expected vs Actual Rates",
            x = "GEP Class",
            y = "Survival Rate",
            color = "Rate Type"
        ) +
        theme_classic() +
        theme(
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white")
        ) +
        scale_color_manual(values = {
            pal <- get_qualitative_palette(2)
            names(pal) <- c("Expected", "Actual")
            pal
        })
    
    ggsave(file.path(mss_output_dir, paste0(prefix, "simple_mss_validation.png")),
        mss_plot,
        width = SMALL_PLOT_WIDTH, height = SMALL_PLOT_HEIGHT, dpi = PLOT_DPI, bg = "white"
    )
}

#' Create simple GEP report
#'
#' Write a text-based summary of 5-year expected vs actual survival by GEP
#' class for MFS and MSS, including an overall summary table.
#'
#' @param mfs_results Data frame of MFS class-level results
#' @param mss_results Data frame of MSS class-level results
#' @param overall_summary Data frame with overall expected/actual summaries
#' @param output_dir Directory to write the report
#' @param prefix Filename prefix for saved files
#' @return Invisibly returns NULL after writing files
create_simple_gep_report <- function(mfs_results, mss_results, overall_summary, output_dir, prefix) {
    report_content <- c(
        "SIMPLE GEP VALIDATION REPORT",
        "===========================",
        "",
        "Goal:",
        "Compare actual rates vs expected reported rates of 5-year MFS and MSS",
        "",
        "METASTASIS-FREE SURVIVAL (MFS) - 5 YEAR:",
        "----------------------------------------"
    )
    for (i in seq_len(nrow(mfs_results))) {
        row <- mfs_results[i, ]
        report_content <- c(
            report_content,
            sprintf("  %s (n=%d):", row$gep_class_simple, row$n),
            sprintf("    Expected: %.3f (%.1f%%)", row$expected_rate, row$expected_rate * 100),
            sprintf("    Actual:   %.3f (%.1f%%)", row$actual_rate, row$actual_rate * 100),
            sprintf("    Difference: %.3f (%.1f%%)", row$difference, row$percent_difference),
            ""
        )
    }
    report_content <- c(
        report_content,
        "MELANOMA-SPECIFIC SURVIVAL (MSS) - 5 YEAR:",
        "------------------------------------------"
    )
    for (i in seq_len(nrow(mss_results))) {
        row <- mss_results[i, ]
        report_content <- c(
            report_content,
            sprintf("  %s (n=%d):", row$gep_class_simple, row$n),
            sprintf("    Expected: %.3f (%.1f%%)", row$expected_rate, row$expected_rate * 100),
            sprintf("    Actual:   %.3f (%.1f%%)", row$actual_rate, row$actual_rate * 100),
            sprintf("    Difference: %.3f (%.1f%%)", row$difference, row$percent_difference),
            ""
        )
    }
    report_content <- c(
        report_content,
        "OVERALL SUMMARY:",
        "---------------",
        sprintf(
            "MFS - Overall: Expected %.1f%%, Actual %.1f%%, Difference %.1f%%",
            overall_summary$overall_expected[1] * 100,
            overall_summary$overall_actual[1] * 100,
            overall_summary$overall_percent_difference[1]
        ),
        sprintf(
            "MSS - Overall: Expected %.1f%%, Actual %.1f%%, Difference %.1f%%",
            overall_summary$overall_expected[2] * 100,
            overall_summary$overall_actual[2] * 100,
            overall_summary$overall_percent_difference[2]
        ),
        "",
        "INTERPRETATION:",
        "--------------",
        "Positive differences indicate GEP predictions were conservative (actual survival better than predicted)",
        "Negative differences indicate GEP predictions were optimistic (actual survival worse than predicted)",
        "Values close to 0 indicate good predictive accuracy"
    )
    writeLines(report_content, file.path(output_dir, paste0(prefix, "simple_gep_validation_report.txt")))
}

#' Simple GEP validation - Actual vs Expected rates
#'
#' Compute 5-year expected vs actual survival by GEP class for MFS and MSS,
#' save summary tables and plots, and return the key data frames.
#'
#' @param data Data frame with required GEP predictions and outcomes
#' @param output_dirs List of output directories (expects elements `obj4_mfs` and `obj4_mss`)
#' @param prefix Filename prefix for saved files
#' @return A list with `mfs_results`, `mss_results`, and `overall_summary`
simple_gep_validation <- function(data, output_dirs, prefix, dataset_name = NULL) {
    logger::log_info("Starting SIMPLE GEP validation (Project Goals)")

    # Resolve directories
    mfs_dir <- output_dirs$obj4_mfs
    mss_dir <- output_dirs$obj4_mss
    gep_base_dir <- dirname(mfs_dir)
    unified_dir <- file.path(gep_base_dir, "unified_summary")

    for (d in c(mfs_dir, mss_dir, unified_dir)) {
        if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }

    analysis_data <- data %>%
        filter(
            !is.na(biopsy1_gep_mfs),
            !is.na(biopsy1_gep_mss),
            biopsy1_gep_mfs >= 0 & biopsy1_gep_mfs <= 1,
            biopsy1_gep_mss >= 0 & biopsy1_gep_mss <= 1
        )
    display_analysis_data <- restore_gep_display_variables(analysis_data, dataset_name = dataset_name)
    logger::log_info(sprintf("Analysis dataset: %d patients with valid GEP predictions", nrow(analysis_data)))

    expected_mfs_col <- if ("expected_mfs_5yr" %in% names(analysis_data)) "expected_mfs_5yr" else "biopsy1_gep_mfs"
    expected_mss_col <- if ("expected_mss_5yr" %in% names(analysis_data)) "expected_mss_5yr" else "biopsy1_gep_mss"
    mfs_event_col <- if ("mfs_event_5yr" %in% names(analysis_data)) "mfs_event_5yr" else NULL
    mss_event_col <- if ("mss_event_5yr" %in% names(analysis_data)) "mss_event_5yr" else NULL
    mss_time_col <- dplyr::case_when(
        "tt_death_months" %in% names(analysis_data) ~ "tt_death_months",
        "tt_death_years" %in% names(analysis_data) ~ "tt_death_years",
        TRUE ~ NA_character_
    )

    mfs_data <- display_analysis_data %>%
        filter(
            if ("mfs_analysis_eligible" %in% names(.)) mfs_analysis_eligible else !is.na(tt_mets_months) & !is.na(mets_event),
            !is.na(.data[[expected_mfs_col]]),
            !is.na(tt_mets_months)
        ) %>%
        mutate(
            expected_mfs_5yr = .data[[expected_mfs_col]],
            actual_mfs_5yr = if (!is.null(mfs_event_col)) 1 - .data[[mfs_event_col]] else ifelse(tt_mets_months > 60 | (tt_mets_months <= 60 & mets_event == 0), 1, 0),
            time_to_5yr = pmin(tt_mets_months, 60)
        )

    mfs_results <- mfs_data %>%
        group_by(gep_class_simple) %>%
        summarise(
            n = n(),
            expected_rate = mean(expected_mfs_5yr, na.rm = TRUE),
            actual_rate = mean(actual_mfs_5yr, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        mutate(
            difference = actual_rate - expected_rate,
            percent_difference = (difference / expected_rate) * 100
        )

    mss_data <- display_analysis_data %>%
        filter(
            if ("mss_analysis_eligible" %in% names(.)) mss_analysis_eligible else !is.na(melanoma_death_event) & !is.na(.data[[mss_time_col]]),
            !is.na(.data[[expected_mss_col]]),
            !is.na(.data[[mss_time_col]])
        ) %>%
        mutate(
            expected_mss_5yr = .data[[expected_mss_col]],
            actual_mss_5yr = if (!is.null(mss_event_col)) {
                1 - .data[[mss_event_col]]
            } else if (mss_time_col == "tt_death_months") {
                ifelse(tt_death_months > 60 | (tt_death_months <= 60 & melanoma_death_event == 0), 1, 0)
            } else {
                ifelse(tt_death_years > 5 | (tt_death_years <= 5 & melanoma_death_event == 0), 1, 0)
            },
            time_to_5yr = if (mss_time_col == "tt_death_months") pmin(tt_death_months, 60) else pmin(tt_death_years * 12, 60)
        )

    mss_results <- mss_data %>%
        group_by(gep_class_simple) %>%
        summarise(
            n = n(),
            expected_rate = mean(expected_mss_5yr, na.rm = TRUE),
            actual_rate = mean(actual_mss_5yr, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        mutate(
            difference = actual_rate - expected_rate,
            percent_difference = (difference / expected_rate) * 100
        )

    logger::log_info("Creating simple validation summary")
    overall_summary <- data.frame(
        outcome = c("MFS", "MSS"),
        total_patients = c(nrow(mfs_data), nrow(mss_data)),
        overall_expected = c(
            mean(mfs_data$expected_mfs_5yr, na.rm = TRUE),
            mean(mss_data$expected_mss_5yr, na.rm = TRUE)
        ),
        overall_actual = c(
            mean(mfs_data$actual_mfs_5yr, na.rm = TRUE),
            mean(mss_data$actual_mss_5yr, na.rm = TRUE)
        ),
        overall_difference = c(
            mean(mfs_data$actual_mfs_5yr, na.rm = TRUE) - mean(mfs_data$expected_mfs_5yr, na.rm = TRUE),
            mean(mss_data$actual_mss_5yr, na.rm = TRUE) - mean(mss_data$expected_mss_5yr, na.rm = TRUE)
        ),
        stringsAsFactors = FALSE
    ) %>%
        mutate(
            overall_percent_difference = (overall_difference / overall_expected) * 100
        )

    write_gep_workbook(list(
        "MFS_By_Class" = mfs_results,
        "MSS_By_Class" = mss_results,
        "Overall_Summary" = overall_summary
    ), file.path(unified_dir, paste0(prefix, "simple_gep_validation.xlsx")))

    create_simple_gep_plots(mfs_results, mss_results, mfs_dir, mss_dir, prefix)
    create_simple_gep_report(mfs_results, mss_results, overall_summary, unified_dir, prefix)

    logger::log_info("Simple GEP validation completed")
    return(list(
        mfs_results = mfs_results,
        mss_results = mss_results,
        overall_summary = overall_summary
    ))
}
