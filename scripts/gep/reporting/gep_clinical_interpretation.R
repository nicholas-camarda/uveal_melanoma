# GEP Clinical Interpretation Functions
# Clinical interpretation and assessment of GEP validation results

#' Create Clinical Interpretation of GEP Validation Results
#'
#' Generate clinical interpretation of calibration, discrimination, and observed/expected metrics
#' with cross-timepoint pattern analysis.
#'
#' @param calibration_data Data frame with calibration metrics across timepoints
#' @param discrimination_data Data frame with discrimination metrics across timepoints  
#' @param oe_data Data frame with observed/expected metrics across timepoints
#' @param outcome_type Either "MFS" or "MSS"
#' @return List with clinical interpretation sections
create_clinical_interpretation <- function(calibration_data, discrimination_data, oe_data, outcome_type) {
    slope_issue_summary <- summarize_gep_slope_issue_pattern(calibration_data)
    all_slopes_unavailable <-
        nrow(calibration_data) > 0 &&
        all(!is.finite(calibration_data$Slope))
    
    # Overall assessment
    overall_assessment <- if (outcome_type == "MFS") {
        if (all_slopes_unavailable && nzchar(slope_issue_summary)) {
            paste(
                "The GEP model demonstrates strong predictive performance for metastasis-free survival, with consistent discrimination across timepoints, but the calibration slope could not be estimated.",
                slope_issue_summary,
                "The model still appears clinically useful for risk stratification, but the absolute risk estimates should be interpreted with caution."
            )
        } else {
            "The GEP model demonstrates strong predictive performance for metastasis-free survival, with consistent discrimination across timepoints and generally good calibration. The model appears to be clinically useful for risk stratification and treatment planning."
        }
    } else {
        if (all_slopes_unavailable && nzchar(slope_issue_summary)) {
            paste(
                "The GEP model shows excellent discrimination for melanoma-specific survival, but the calibration slope could not be estimated.",
                slope_issue_summary,
                "The model still provides useful prognostic information, but the absolute risk estimates should be interpreted with caution."
            )
        } else {
            "The GEP model shows excellent discrimination for melanoma-specific survival, though calibration varies across timepoints. The model provides valuable prognostic information for clinical decision-making and patient counseling."
        }
    }
    
    # Calibration interpretation
    calibration_interpretation <- create_calibration_interpretation(calibration_data, outcome_type)
    
    # Discrimination interpretation  
    discrimination_interpretation <- create_discrimination_interpretation(discrimination_data, outcome_type)
    
    # Observed/Expected interpretation
    oe_interpretation <- create_oe_interpretation(oe_data, outcome_type)
    
    # Temporal patterns
    temporal_patterns <- create_temporal_patterns(calibration_data, discrimination_data, oe_data, outcome_type)
    
    # Clinical implications
    clinical_implications <- create_clinical_implications(calibration_data, discrimination_data, oe_data, outcome_type)
    
    return(list(
        overall_assessment = overall_assessment,
        calibration_interpretation = calibration_interpretation,
        discrimination_interpretation = discrimination_interpretation,
        oe_interpretation = oe_interpretation,
        temporal_patterns = temporal_patterns,
        clinical_implications = clinical_implications
    ))
}

join_gep_reason_fragments <- function(fragments) {
    fragments <- fragments[!is.na(fragments) & nzchar(fragments)]

    if (length(fragments) == 0) {
        return("")
    }

    if (length(fragments) == 1) {
        return(fragments[[1]])
    }

    if (length(fragments) == 2) {
        return(sprintf("%s and %s", fragments[[1]], fragments[[2]]))
    }

    sprintf(
        "%s, and %s",
        paste(fragments[-length(fragments)], collapse = ", "),
        fragments[[length(fragments)]]
    )
}

describe_gep_slope_problem <- function(status, fit_n = NA_real_, events = NA_real_,
                                       non_events = NA_real_, unique_risk_count = NA_real_,
                                       slope_se = NA_real_, include_counts = TRUE) {
    fit_n_text <- if (is.finite(fit_n)) as.character(as.integer(round(fit_n))) else "NA"
    events_text <- if (is.finite(events)) as.character(as.integer(round(events))) else "NA"
    non_events_text <- if (is.finite(non_events)) as.character(as.integer(round(non_events))) else "NA"
    unique_risk_text <- if (is.finite(unique_risk_count)) as.character(as.integer(round(unique_risk_count))) else "NA"

    if (!is.na(status) && identical(status, "insufficient_recalibration_data")) {
        problem_fragments <- c()

        if (is.finite(fit_n) && fit_n < GEP_MIN_SAMPLE_SIZE) {
            problem_fragments <- c(problem_fragments, "too few patients had usable data")
        }

        if (is.finite(events) && events < GEP_MIN_CALIBRATION_EVENTS) {
            problem_fragments <- c(problem_fragments, "too few events were available")
        }

        if (is.finite(non_events) && non_events < GEP_MIN_CALIBRATION_EVENTS) {
            problem_fragments <- c(problem_fragments, "too few non-events were available")
        }

        if (is.finite(unique_risk_count) && unique_risk_count < 2) {
            problem_fragments <- c(problem_fragments, "predicted risks did not vary enough")
        }

        if (length(problem_fragments) > 0) {
            description <- sprintf(
                "there was not enough usable data to fit a reliable calibration slope because %s",
                join_gep_reason_fragments(problem_fragments)
            )
        } else {
            description <- "there was not enough usable data to fit a reliable calibration slope"
        }
    } else if (!is.na(status) && identical(status, "recalibration_fit_unstable")) {
        if (is.finite(slope_se) && slope_se > GEP_MAX_CALIBRATION_COEF_SE) {
            description <- "the calibration model gave an unstable result with too much uncertainty to trust"
        } else {
            description <- "the calibration model gave an unstable result that was not reliable enough to report"
        }
    } else {
        description <- "the calibration slope could not be estimated reliably"
    }

    if (!isTRUE(include_counts)) {
        return(description)
    }

    count_fragments <- c(
        sprintf("usable n=%s", fit_n_text),
        sprintf("events=%s", events_text),
        sprintf("non-events=%s", non_events_text)
    )

    if (is.finite(unique_risk_count)) {
        count_fragments <- c(count_fragments, sprintf("distinct risk values=%s", unique_risk_text))
    }

    sprintf("%s (%s)", description, paste(count_fragments, collapse = ", "))
}

format_gep_calibration_slope_text <- function(slope, slope_method = NA_character_,
                                              status = NA_character_, fit_n = NA_real_,
                                              events = NA_real_, non_events = NA_real_,
                                              unique_risk_count = NA_real_, slope_se = NA_real_) {
    if (is.finite(slope)) {
        method_label <- if (!is.na(slope_method) && nzchar(slope_method)) slope_method else "method not recorded"
        return(sprintf("slope=%.3f [%s]", slope, method_label))
    }

    sprintf(
        "slope=NA (%s)",
        describe_gep_slope_problem(
            status = status,
            fit_n = fit_n,
            events = events,
            non_events = non_events,
            unique_risk_count = unique_risk_count,
            slope_se = slope_se,
            include_counts = TRUE
        )
    )
}

summarize_gep_slope_issue_pattern <- function(calibration_data) {
    if (nrow(calibration_data) == 0 || !"Slope" %in% names(calibration_data)) {
        return("")
    }

    unavailable_rows <- calibration_data[!is.finite(calibration_data$Slope), , drop = FALSE]

    if (nrow(unavailable_rows) == 0) {
        return("")
    }

    statuses <- if ("Status" %in% names(unavailable_rows)) unavailable_rows$Status else rep(NA_character_, nrow(unavailable_rows))
    fit_ns <- if ("Fit_N" %in% names(unavailable_rows)) unavailable_rows$Fit_N else rep(NA_real_, nrow(unavailable_rows))
    events <- if ("Events" %in% names(unavailable_rows)) unavailable_rows$Events else rep(NA_real_, nrow(unavailable_rows))
    non_events <- if ("Non_Events" %in% names(unavailable_rows)) unavailable_rows$Non_Events else rep(NA_real_, nrow(unavailable_rows))
    unique_risk_counts <- if ("Unique_Risk_Count" %in% names(unavailable_rows)) unavailable_rows$Unique_Risk_Count else rep(NA_real_, nrow(unavailable_rows))
    slope_ses <- if ("Slope_SE" %in% names(unavailable_rows)) unavailable_rows$Slope_SE else rep(NA_real_, nrow(unavailable_rows))

    problem_descriptions <- vapply(seq_len(nrow(unavailable_rows)), function(i) {
        describe_gep_slope_problem(
            status = statuses[[i]],
            fit_n = fit_ns[[i]],
            events = events[[i]],
            non_events = non_events[[i]],
            unique_risk_count = unique_risk_counts[[i]],
            slope_se = slope_ses[[i]],
            include_counts = FALSE
        )
    }, character(1))

    if (nrow(unavailable_rows) == nrow(calibration_data)) {
        unique_descriptions <- unique(problem_descriptions[nzchar(problem_descriptions)])

        if (length(unique_descriptions) == 1) {
            return(sprintf("The calibration slope could not be estimated at any timepoint because %s.", unique_descriptions[[1]]))
        }

        timepoint_fragments <- vapply(seq_len(nrow(unavailable_rows)), function(i) {
            sprintf("%s: %s", unavailable_rows$Timepoint[[i]], problem_descriptions[[i]])
        }, character(1))

        return(sprintf(
            "The calibration slope could not be estimated at any timepoint: %s.",
            paste(timepoint_fragments, collapse = "; ")
        ))
    }

    unique_descriptions <- unique(problem_descriptions[nzchar(problem_descriptions)])
    if (length(unique_descriptions) == 1) {
        return(sprintf("At some timepoints the calibration slope could not be estimated because %s.", unique_descriptions[[1]]))
    }

    timepoint_fragments <- vapply(seq_len(nrow(unavailable_rows)), function(i) {
        sprintf("%s: %s", unavailable_rows$Timepoint[[i]], problem_descriptions[[i]])
    }, character(1))

    sprintf(
        "At some timepoints the calibration slope could not be estimated: %s.",
        paste(timepoint_fragments, collapse = "; ")
    )
}

#' Create Calibration Interpretation
#'
#' Summarize calibration-slope behavior across timepoints in narrative form.
#'
#' @param calibration_data Data frame with at least a `Slope` column.
#' @param outcome_type Character outcome label, typically `"MFS"` or `"MSS"`.
#' @return Character scalar with a calibration interpretation.
create_calibration_interpretation <- function(calibration_data, outcome_type) {
    if (nrow(calibration_data) == 0) return("Calibration metrics not available")

    slope_issue_summary <- summarize_gep_slope_issue_pattern(calibration_data)
    if (all(!is.finite(calibration_data$Slope))) {
        return(paste(
            slope_issue_summary,
            "This means the report cannot use the slope to judge whether predicted risks are systematically too high or too low."
        ))
    }
    
    # Analyze calibration slope patterns
    slopes <- calibration_data$Slope
    slope_trend <- if (length(slopes) > 1) {
        # Filter out NA values before computing differences
        valid_slopes <- slopes[!is.na(slopes)]
        if (length(valid_slopes) > 1) {
            diffs <- diff(valid_slopes)
            if (all(diffs > 0, na.rm = TRUE)) "increasing" else if (all(diffs < 0, na.rm = TRUE)) "decreasing" else "variable"
        } else "stable"
    } else "stable"
    
    # Overall calibration assessment
    mean_slope <- mean(slopes, na.rm = TRUE)
    calibration_quality <- if (is.na(mean_slope)) "unknown" else if (abs(mean_slope - 1) < 0.1) "excellent" else if (abs(mean_slope - 1) < 0.2) "good" else "moderate"
    
    interpretation <- sprintf(
        "Calibration slope across timepoints shows %s pattern (mean = %.2f). Overall calibration quality is %s. Clinical interpretation: A slope of 1.0 indicates perfect calibration. Slopes > 1.0 suggest the predictions are too moderate or compressed, while slopes < 1.0 suggest the predictions are too extreme. The %s calibration quality indicates the model %s for clinical use.",
        slope_trend, mean_slope, calibration_quality,
        calibration_quality,
        if (is.na(calibration_quality) || calibration_quality == "unknown") "has unknown calibration status" else if (calibration_quality %in% c("excellent", "good")) "is well-calibrated and suitable" else "may require recalibration before"
    )

    if (nzchar(slope_issue_summary)) {
        interpretation <- paste(interpretation, slope_issue_summary)
    }
    
    return(interpretation)
}

#' Create Discrimination Interpretation
#'
#' Summarize Harrell's C across timepoints in narrative form.
#'
#' @param discrimination_data Data frame with at least a `Harrell_C` column.
#' @param outcome_type Character outcome label, typically `"MFS"` or `"MSS"`.
#' @return Character scalar with a discrimination interpretation.
create_discrimination_interpretation <- function(discrimination_data, outcome_type) {
    if (nrow(discrimination_data) == 0) return("Discrimination metrics not available")
    
    # Analyze Harrell's C-index patterns
    harrell_c <- discrimination_data$Harrell_C
    mean_harrell <- mean(harrell_c, na.rm = TRUE)
    
    discrimination_quality <- if (is.na(mean_harrell)) "unknown" else if (mean_harrell >= 0.9) "excellent" else if (mean_harrell >= 0.8) "very good" else if (mean_harrell >= 0.7) "good" else "moderate"
    
    interpretation <- sprintf(
        "Discrimination performance is %s with mean Harrell's C-index = %.3f across timepoints. Clinical interpretation: Harrell's C-index ranges from 0.5 (no discrimination) to 1.0 (perfect discrimination). Values >= 0.8 indicate very good discrimination, while values >= 0.9 indicate excellent discrimination. The %s discrimination suggests the GEP model %s distinguish between high and low-risk patients.",
        discrimination_quality, mean_harrell,
        discrimination_quality,
        if (is.na(discrimination_quality) || discrimination_quality == "unknown") "has unknown discrimination ability" else if (discrimination_quality %in% c("excellent", "very good")) "effectively" else "adequately"
    )
    
    return(interpretation)
}

#' Create Observed/Expected Interpretation
#'
#' Summarize overall O/E behavior across timepoints in narrative form.
#'
#' @param oe_data Data frame with at least an `Overall_OE` column.
#' @param outcome_type Character outcome label, typically `"MFS"` or `"MSS"`.
#' @return Character scalar with an O/E interpretation.
create_oe_interpretation <- function(oe_data, outcome_type) {
    if (nrow(oe_data) == 0) return("Observed/Expected metrics not available")
    
    # Analyze O/E ratio patterns
    oe_ratios <- oe_data$Overall_OE
    mean_oe <- mean(oe_ratios, na.rm = TRUE)
    
    # Assess systematic bias
    bias_assessment <- if (is.na(mean_oe)) "unknown bias pattern" else if (abs(mean_oe - 1) < 0.1) "minimal systematic bias" else if (mean_oe > 1.1) "tends to underestimate risk" else if (mean_oe < 0.9) "tends to overestimate risk" else "shows moderate bias"
    
    interpretation <- sprintf(
        "Observed/Expected analysis shows %s with mean O/E ratio = %.2f across timepoints. Clinical interpretation: O/E ratio = 1.0 indicates perfect prediction. Ratios > 1.0 suggest the model underestimates actual risk, while ratios < 1.0 suggest overestimation. The %s indicates the model %s, which is %s for clinical use.",
        bias_assessment, mean_oe,
        bias_assessment,
        if (is.na(mean_oe)) "has unknown prediction accuracy" else if (abs(mean_oe - 1) < 0.2) "provides reasonably accurate risk estimates" else "has systematic prediction errors",
        if (is.na(mean_oe)) "unknown" else if (abs(mean_oe - 1) < 0.2) "acceptable" else "concerning and may require recalibration"
    )
    
    return(interpretation)
}

#' Create Temporal Pattern Analysis
#'
#' Describe cross-timepoint patterns in calibration, discrimination, and O/E
#' behavior.
#'
#' @param calibration_data Data frame with calibration metrics across timepoints.
#' @param discrimination_data Data frame with discrimination metrics across
#'   timepoints.
#' @param oe_data Data frame with observed/expected metrics across timepoints.
#' @param outcome_type Character outcome label, typically `"MFS"` or `"MSS"`.
#' @return Character scalar describing temporal patterns.
create_temporal_patterns <- function(calibration_data, discrimination_data, oe_data, outcome_type) {
    patterns <- c()
    
    # Calibration trends
    if (nrow(calibration_data) > 1) {
        slopes <- calibration_data$Slope
        # Filter out NA values before computing differences
        valid_slopes <- slopes[!is.na(slopes)]
        if (length(valid_slopes) > 1) {
            diffs <- diff(valid_slopes)
            if (all(diffs > 0, na.rm = TRUE)) {
                patterns <- c(patterns, "Calibration slope increases over time, suggesting improving model fit for longer-term predictions")
            } else if (all(diffs < 0, na.rm = TRUE)) {
                patterns <- c(patterns, "Calibration slope decreases over time, suggesting declining model fit for longer-term predictions")
            } else {
                patterns <- c(patterns, "Calibration slope shows variable pattern across timepoints")
            }
        }
    }
    
    # Discrimination trends
    if (nrow(discrimination_data) > 1) {
        harrell_c <- discrimination_data$Harrell_C
        # Filter out NA values before computing differences
        valid_harrell <- harrell_c[!is.na(harrell_c)]
        if (length(valid_harrell) > 1) {
            diffs <- diff(valid_harrell)
            if (all(diffs > 0, na.rm = TRUE)) {
                patterns <- c(patterns, "Discrimination improves over time, indicating better risk separation for longer follow-up")
            } else if (all(diffs < 0, na.rm = TRUE)) {
                patterns <- c(patterns, "Discrimination declines over time, suggesting reduced predictive accuracy for longer follow-up")
            } else {
                patterns <- c(patterns, "Discrimination remains stable across timepoints")
            }
        }
    }
    
    # O/E trends
    if (nrow(oe_data) > 1) {
        oe_ratios <- oe_data$Overall_OE
        # Filter out NA values before computing differences
        valid_oe <- oe_ratios[!is.na(oe_ratios)]
        if (length(valid_oe) > 1) {
            diffs <- diff(valid_oe)
            if (all(diffs > 0, na.rm = TRUE)) {
                patterns <- c(patterns, "O/E ratios increase over time, suggesting improving prediction accuracy")
            } else if (all(diffs < 0, na.rm = TRUE)) {
                patterns <- c(patterns, "O/E ratios decrease over time, suggesting declining prediction accuracy")
            } else {
                patterns <- c(patterns, "O/E ratios show stable pattern across timepoints")
            }
        }
    }
    
    if (length(patterns) == 0) return("Insufficient data for temporal pattern analysis")
    
    return(paste(patterns, collapse = ". "))
}

#' Create Clinical Implications
#'
#' Translate calibration and discrimination summaries into practical clinical-use
#' statements for the current outcome.
#'
#' @param calibration_data Data frame with calibration metrics across timepoints.
#' @param discrimination_data Data frame with discrimination metrics across
#'   timepoints.
#' @param oe_data Data frame with observed/expected metrics across timepoints.
#' @param outcome_type Character outcome label, typically `"MFS"` or `"MSS"`.
#' @return Character scalar describing clinical implications.
create_clinical_implications <- function(calibration_data, discrimination_data, oe_data, outcome_type) {
    implications <- c()
    
    # Overall model utility
    if (nrow(discrimination_data) > 0) {
        mean_harrell <- mean(discrimination_data$Harrell_C, na.rm = TRUE)
        if (is.na(mean_harrell)) {
            implications <- c(implications, "Discrimination was not estimable across the available timepoints")
        } else if (mean_harrell >= 0.8) {
            implications <- c(implications, "The GEP model provides strong prognostic information suitable for clinical decision-making")
        } else {
            implications <- c(implications, "The GEP model provides moderate prognostic information; clinical decisions should consider additional factors")
        }
    }
    
    # Calibration implications
    if (nrow(calibration_data) > 0) {
        mean_slope <- mean(calibration_data$Slope, na.rm = TRUE)
        if (is.na(mean_slope)) {
            slope_issue_summary <- summarize_gep_slope_issue_pattern(calibration_data)
            if (nzchar(slope_issue_summary)) {
                slope_issue_summary <- sub("\\.$", "", slope_issue_summary)
                implications <- c(
                    implications,
                    paste0(
                        slope_issue_summary,
                        ". The absolute risk estimates should be interpreted with caution for patient-level counseling"
                    )
                )
            } else {
                implications <- c(implications, "Calibration slope was not estimable across the available timepoints")
            }
        } else if (abs(mean_slope - 1) < 0.2) {
            implications <- c(implications, "Good calibration suggests the model's risk estimates can be used directly for patient counseling")
        } else {
            implications <- c(implications, "Moderate calibration suggests risk estimates should be interpreted with caution and may require adjustment")
        }
    }
    
    # Time-dependent implications
    if (outcome_type == "MFS") {
        implications <- c(implications, "For metastasis-free survival, the model can guide surveillance intensity and adjuvant therapy decisions")
    } else {
        implications <- c(implications, "For melanoma-specific survival, the model can inform treatment aggressiveness and patient counseling about prognosis")
    }
    
    return(paste(implications, collapse = ". "))
}

# Helper functions for clinical interpretation
#'
#' Classify calibration slope interpretation
#'
#' @param slope Numeric calibration slope.
#' @return Character label describing calibration quality.
get_calibration_interpretation <- function(slope) {
    if (is.na(slope)) return("Not available")
    if (abs(slope - 1) < 0.1) return("Excellent calibration")
    if (abs(slope - 1) < 0.2) return("Good calibration")
    if (slope > 1.1) return("Predictions appear too compressed")
    if (slope < 0.9) return("Predictions appear too extreme")
    return("Moderate calibration")
}

#' Classify calibration quality from slope magnitude
#'
#' @param slope Numeric calibration slope.
#' @return Character quality label.
get_calibration_quality <- function(slope) {
    if (is.na(slope)) return("Not available")
    if (abs(slope - 1) < 0.1) return("Excellent")
    if (abs(slope - 1) < 0.2) return("Good")
    return("Moderate")
}

#' Describe calibration slope trend across timepoints
#'
#' @param slopes Numeric vector of calibration slopes.
#' @return Character description of the slope trend.
get_slope_trend <- function(slopes) {
    if (length(slopes) < 2) return("Single timepoint")
    # Filter out NA values before computing differences
    valid_slopes <- slopes[!is.na(slopes)]
    if (length(valid_slopes) < 2) return("Insufficient valid data")
    diffs <- diff(valid_slopes)
    if (all(diffs > 0, na.rm = TRUE)) return("Improving over time")
    if (all(diffs < 0, na.rm = TRUE)) return("Declining over time")
    return("Variable pattern")
}

#' Interpret decision-curve utility at a threshold
#'
#' @param threshold Numeric decision threshold.
#' @param net_benefit Numeric net benefit estimate.
#' @return Character description of threshold-specific utility.
get_decision_curve_interpretation <- function(threshold, net_benefit) {
    if (is.na(threshold) || is.na(net_benefit)) return("Not available")
    if (threshold < 5) return("Low threshold - model useful for most patients")
    if (threshold < 20) return("Moderate threshold - model useful for moderate risk")
    return("High threshold - model useful for high risk only")
}

#' Describe threshold trend across timepoints
#'
#' @param thresholds Numeric vector of decision thresholds.
#' @return Character description of the threshold trend.
get_threshold_trend <- function(thresholds) {
    if (length(thresholds) < 2) return("Single timepoint")
    # Filter out NA values before computing differences
    valid_thresholds <- thresholds[!is.na(thresholds)]
    if (length(valid_thresholds) < 2) return("Insufficient valid data")
    diffs <- diff(valid_thresholds)
    if (all(diffs > 0, na.rm = TRUE)) return("Increasing threshold over time")
    if (all(diffs < 0, na.rm = TRUE)) return("Decreasing threshold over time")
    return("Variable pattern")
}

#' Describe net-benefit trend across timepoints
#'
#' @param net_benefits Numeric vector of net-benefit estimates.
#' @return Character description of the net-benefit trend.
get_net_benefit_trend <- function(net_benefits) {
    if (length(net_benefits) < 2) return("Single timepoint")
    # Filter out NA values before computing differences
    valid_net_benefits <- net_benefits[!is.na(net_benefits)]
    if (length(valid_net_benefits) < 2) return("Insufficient valid data")
    diffs <- diff(valid_net_benefits)
    if (all(diffs > 0, na.rm = TRUE)) return("Improving net benefit over time")
    if (all(diffs < 0, na.rm = TRUE)) return("Declining net benefit over time")
    return("Variable pattern")
}

#' Classify discrimination quality
#'
#' @param harrell_c Numeric Harrell's C estimate.
#' @return Character quality label.
get_discrimination_quality <- function(harrell_c) {
    if (is.na(harrell_c)) return("Not available")
    if (harrell_c >= 0.9) return("Excellent")
    if (harrell_c >= 0.8) return("Very good")
    if (harrell_c >= 0.7) return("Good")
    return("Moderate")
}

#' Classify O/E calibration quality
#'
#' @param oe_ratio Numeric observed-to-expected ratio.
#' @return Character quality label.
get_oe_calibration_quality <- function(oe_ratio) {
    if (is.na(oe_ratio)) return("Not available")
    if (abs(oe_ratio - 1) < 0.1) return("Excellent")
    if (abs(oe_ratio - 1) < 0.2) return("Good")
    return("Moderate")
}

#' Combine discrimination and calibration into an overall assessment
#'
#' @param harrell_c Numeric Harrell's C estimate.
#' @param oe_ratio Numeric observed-to-expected ratio.
#' @return Character overall performance assessment.
get_overall_performance_assessment <- function(harrell_c, oe_ratio) {
    if (is.na(harrell_c) || is.na(oe_ratio)) return("Insufficient data")
    
    disc_quality <- if (harrell_c >= 0.8) "Strong" else if (harrell_c >= 0.7) "Moderate" else "Limited"
    cal_quality <- if (abs(oe_ratio - 1) < 0.2) "Good" else "Moderate"
    
    if (disc_quality == "Strong" && cal_quality == "Good") return("Excellent overall performance")
    if (disc_quality == "Strong" || cal_quality == "Good") return("Good overall performance")
    return("Moderate overall performance")
}

#' Describe discrimination trend across timepoints
#'
#' @param harrell_c_values Numeric vector of Harrell's C values.
#' @return Character description of the discrimination trend.
get_discrimination_trend <- function(harrell_c_values) {
    if (length(harrell_c_values) < 2) return("Single timepoint")
    # Filter out NA values before computing differences
    valid_harrell_c <- harrell_c_values[!is.na(harrell_c_values)]
    if (length(valid_harrell_c) < 2) return("Insufficient valid data")
    diffs <- diff(valid_harrell_c)
    if (all(diffs > 0, na.rm = TRUE)) return("Improving discrimination")
    if (all(diffs < 0, na.rm = TRUE)) return("Declining discrimination")
    return("Stable discrimination")
}

#' Describe O/E-based calibration trend across timepoints
#'
#' @param oe_ratios Numeric vector of observed-to-expected ratios.
#' @return Character description of the calibration trend.
get_calibration_trend <- function(oe_ratios) {
    if (length(oe_ratios) < 2) return("Single timepoint")
    # Filter out NA values before computing differences
    valid_oe <- oe_ratios[!is.na(oe_ratios)]
    if (length(valid_oe) < 2) return("Insufficient valid data")
    diffs <- diff(valid_oe)
    if (all(diffs > 0, na.rm = TRUE)) return("Improving calibration")
    if (all(diffs < 0, na.rm = TRUE)) return("Declining calibration")
    return("Stable calibration")
}
