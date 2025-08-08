# Simple GEP Validation (Project Goals)
# Computes expected vs actual 5-year MFS/MSS by GEP class and saves outputs

#' Create simple GEP validation plots
create_simple_gep_plots <- function(mfs_results, mss_results, output_dir, prefix) {
    mfs_plot <- ggplot(mfs_results, aes(x = gep_class_simple)) +
        geom_point(aes(y = expected_rate, color = "Expected"), size = 3) +
        geom_point(aes(y = actual_rate, color = "Actual"), size = 3) +
        geom_segment(aes(x = gep_class_simple, xend = gep_class_simple, 
                        y = expected_rate, yend = actual_rate), 
                    linetype = "dashed", alpha = 0.5) +
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
        scale_color_manual(values = c("Expected" = "blue", "Actual" = "red"))
    ggsave(file.path(output_dir, paste0(prefix, "simple_mfs_validation.png")), 
           mfs_plot, width = 8, height = 6, dpi = 300, bg = "white")

    mss_plot <- ggplot(mss_results, aes(x = gep_class_simple)) +
        geom_point(aes(y = expected_rate, color = "Expected"), size = 3) +
        geom_point(aes(y = actual_rate, color = "Actual"), size = 3) +
        geom_segment(aes(x = gep_class_simple, xend = gep_class_simple, 
                        y = expected_rate, yend = actual_rate), 
                    linetype = "dashed", alpha = 0.5) +
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
        scale_color_manual(values = c("Expected" = "blue", "Actual" = "red"))
    ggsave(file.path(output_dir, paste0(prefix, "simple_mss_validation.png")), 
           mss_plot, width = 8, height = 6, dpi = 300, bg = "white")
}

#' Create simple GEP report
create_simple_gep_report <- function(mfs_results, mss_results, overall_summary, output_dir, prefix) {
    report_content <- c(
        "SIMPLE GEP VALIDATION REPORT",
        "===========================",
        "",
        "This report directly addresses the project goals:",
        "Compare actual rates vs expected reported rates of 5-year MFS and MSS",
        "",
        "METASTASIS-FREE SURVIVAL (MFS) - 5 YEAR:",
        "----------------------------------------"
    )
    for (i in 1:nrow(mfs_results)) {
        row <- mfs_results[i, ]
        report_content <- c(report_content,
            sprintf("  %s (n=%d):", row$gep_class_simple, row$n),
            sprintf("    Expected: %.3f (%.1f%%)", row$expected_rate, row$expected_rate * 100),
            sprintf("    Actual:   %.3f (%.1f%%)", row$actual_rate, row$actual_rate * 100),
            sprintf("    Difference: %.3f (%.1f%%)", row$difference, row$percent_difference),
            ""
        )
    }
    report_content <- c(report_content,
        "MELANOMA-SPECIFIC SURVIVAL (MSS) - 5 YEAR:",
        "------------------------------------------"
    )
    for (i in 1:nrow(mss_results)) {
        row <- mss_results[i, ]
        report_content <- c(report_content,
            sprintf("  %s (n=%d):", row$gep_class_simple, row$n),
            sprintf("    Expected: %.3f (%.1f%%)", row$expected_rate, row$expected_rate * 100),
            sprintf("    Actual:   %.3f (%.1f%%)", row$actual_rate, row$actual_rate * 100),
            sprintf("    Difference: %.3f (%.1f%%)", row$difference, row$percent_difference),
            ""
        )
    }
    report_content <- c(report_content,
        "OVERALL SUMMARY:",
        "---------------",
        sprintf("MFS - Overall: Expected %.1f%%, Actual %.1f%%, Difference %.1f%%", 
                overall_summary$overall_expected[1] * 100, 
                overall_summary$overall_actual[1] * 100,
                overall_summary$overall_percent_difference[1]),
        sprintf("MSS - Overall: Expected %.1f%%, Actual %.1f%%, Difference %.1f%%", 
                overall_summary$overall_expected[2] * 100, 
                overall_summary$overall_actual[2] * 100,
                overall_summary$overall_percent_difference[2]),
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
simple_gep_validation <- function(data, output_dir, prefix) {
    log_enhanced("Starting SIMPLE GEP validation (Project Goals)", level = "INFO")
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    analysis_data <- data %>%
        filter(
            !is.na(biopsy1_gep_mfs),
            !is.na(biopsy1_gep_mss),
            biopsy1_gep_mfs >= 0 & biopsy1_gep_mfs <= 1,
            biopsy1_gep_mss >= 0 & biopsy1_gep_mss <= 1
        )
    log_enhanced(sprintf("Analysis dataset: %d patients with valid GEP predictions", nrow(analysis_data)), level = "INFO")

    mfs_data <- analysis_data %>%
        filter(!is.na(tt_mets_months), !is.na(mets_event)) %>%
        mutate(
            expected_mfs_5yr = biopsy1_gep_mfs,
            actual_mfs_5yr = ifelse(tt_mets_months > 60 | (tt_mets_months <= 60 & mets_event == 0), 1, 0),
            time_to_5yr = pmin(tt_mets_months, 60)
        )

    mfs_results <- mfs_data %>%
        group_by(gep_class_simple) %>%
        summarise(
            n = n(),
            expected_rate = mean(expected_mfs_5yr, na.rm = TRUE),
            actual_rate = mean(actual_mfs_5yr, na.rm = TRUE),
            .groups = 'drop'
        ) %>%
        mutate(
            difference = actual_rate - expected_rate,
            percent_difference = (difference / expected_rate) * 100
        )

    mss_data <- analysis_data %>%
        filter(!is.na(tt_death_months), !is.na(death_event)) %>%
        mutate(
            expected_mss_5yr = biopsy1_gep_mss,
            actual_mss_5yr = ifelse(tt_death_months > 60 | (tt_death_months <= 60 & death_event == 0), 1, 0),
            time_to_5yr = pmin(tt_death_months, 60)
        )

    mss_results <- mss_data %>%
        group_by(gep_class_simple) %>%
        summarise(
            n = n(),
            expected_rate = mean(expected_mss_5yr, na.rm = TRUE),
            actual_rate = mean(actual_mss_5yr, na.rm = TRUE),
            .groups = 'drop'
        ) %>%
        mutate(
            difference = actual_rate - expected_rate,
            percent_difference = (difference / expected_rate) * 100
        )

    log_enhanced("Creating simple validation summary", level = "INFO")
    overall_summary <- data.frame(
        outcome = c("MFS", "MSS"),
        total_patients = c(nrow(mfs_data), nrow(mss_data)),
        overall_expected = c(mean(mfs_data$expected_mfs_5yr, na.rm = TRUE), 
                           mean(mss_data$expected_mss_5yr, na.rm = TRUE)),
        overall_actual = c(mean(mfs_data$actual_mfs_5yr, na.rm = TRUE), 
                          mean(mss_data$actual_mss_5yr, na.rm = TRUE)),
        overall_difference = c(mean(mfs_data$actual_mfs_5yr, na.rm = TRUE) - mean(mfs_data$expected_mfs_5yr, na.rm = TRUE),
                              mean(mss_data$actual_mss_5yr, na.rm = TRUE) - mean(mss_data$expected_mss_5yr, na.rm = TRUE)),
        stringsAsFactors = FALSE
    ) %>%
        mutate(
            overall_percent_difference = (overall_difference / overall_expected) * 100
        )

    write_xlsx(list(
        "MFS_By_Class" = mfs_results,
        "MSS_By_Class" = mss_results,
        "Overall_Summary" = overall_summary
    ), file.path(output_dir, paste0(prefix, "simple_gep_validation.xlsx")))

    create_simple_gep_plots(mfs_results, mss_results, output_dir, prefix)
    create_simple_gep_report(mfs_results, mss_results, overall_summary, output_dir, prefix)

    log_enhanced("Simple GEP validation completed", level = "INFO")
    return(list(
        mfs_results = mfs_results,
        mss_results = mss_results,
        overall_summary = overall_summary
    ))
}
