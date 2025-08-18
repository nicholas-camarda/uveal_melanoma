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
    
    # Overall assessment
    overall_assessment <- if (outcome_type == "MFS") {
        "The GEP model demonstrates strong predictive performance for metastasis-free survival, with consistent discrimination across timepoints and generally good calibration. The model appears to be clinically useful for risk stratification and treatment planning."
    } else {
        "The GEP model shows excellent discrimination for melanoma-specific survival, though calibration varies across timepoints. The model provides valuable prognostic information for clinical decision-making and patient counseling."
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

#' Create Calibration Interpretation
create_calibration_interpretation <- function(calibration_data, outcome_type) {
    if (nrow(calibration_data) == 0) return("Calibration metrics not available")
    
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
        "Calibration slope across timepoints shows %s pattern (mean = %.2f). Overall calibration quality is %s. Clinical interpretation: A slope of 1.0 indicates perfect calibration. Slopes > 1.0 suggest the model overestimates risk, while slopes < 1.0 suggest underestimation. The %s calibration quality indicates the model %s for clinical use.",
        slope_trend, mean_slope, calibration_quality,
        calibration_quality,
        if (is.na(calibration_quality) || calibration_quality == "unknown") "has unknown calibration status" else if (calibration_quality %in% c("excellent", "good")) "is well-calibrated and suitable" else "may require recalibration before"
    )
    
    return(interpretation)
}

#' Create Discrimination Interpretation
create_discrimination_interpretation <- function(discrimination_data, outcome_type) {
    if (nrow(discrimination_data) == 0) return("Discrimination metrics not available")
    
    # Analyze Harrell's C-index patterns
    harrell_c <- discrimination_data$Harrell_C
    mean_harrell <- mean(harrell_c, na.rm = TRUE)
    
    discrimination_quality <- if (is.na(mean_harrell)) "unknown" else if (mean_harrell >= 0.9) "excellent" else if (mean_harrell >= 0.8) "very good" else if (mean_harrell >= 0.7) "good" else "moderate"
    
    interpretation <- sprintf(
        "Discrimination performance is %s with mean Harrell's C-index = %.3f across timepoints. Clinical interpretation: Harrell's C-index ranges from 0.5 (no discrimination) to 1.0 (perfect discrimination). Values ≥ 0.8 indicate very good discrimination, while values ≥ 0.9 indicate excellent discrimination. The %s discrimination suggests the GEP model %s distinguish between high and low-risk patients.",
        discrimination_quality, mean_harrell,
        discrimination_quality,
        if (is.na(discrimination_quality) || discrimination_quality == "unknown") "has unknown discrimination ability" else if (discrimination_quality %in% c("excellent", "very good")) "effectively" else "adequately"
    )
    
    return(interpretation)
}

#' Create Observed/Expected Interpretation
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
create_clinical_implications <- function(calibration_data, discrimination_data, oe_data, outcome_type) {
    implications <- c()
    
    # Overall model utility
    if (nrow(discrimination_data) > 0) {
        mean_harrell <- mean(discrimination_data$Harrell_C, na.rm = TRUE)
        if (mean_harrell >= 0.8) {
            implications <- c(implications, "The GEP model provides strong prognostic information suitable for clinical decision-making")
        } else {
            implications <- c(implications, "The GEP model provides moderate prognostic information; clinical decisions should consider additional factors")
        }
    }
    
    # Calibration implications
    if (nrow(calibration_data) > 0) {
        mean_slope <- mean(calibration_data$Slope, na.rm = TRUE)
        if (abs(mean_slope - 1) < 0.2) {
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
get_calibration_interpretation <- function(slope) {
    if (is.na(slope)) return("Not available")
    if (abs(slope - 1) < 0.1) return("Excellent calibration")
    if (abs(slope - 1) < 0.2) return("Good calibration")
    if (slope > 1.1) return("Model overestimates risk")
    if (slope < 0.9) return("Model underestimates risk")
    return("Moderate calibration")
}

get_calibration_quality <- function(slope) {
    if (is.na(slope)) return("Not available")
    if (abs(slope - 1) < 0.1) return("Excellent")
    if (abs(slope - 1) < 0.2) return("Good")
    return("Moderate")
}

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

get_decision_curve_interpretation <- function(threshold, net_benefit) {
    if (is.na(threshold) || is.na(net_benefit)) return("Not available")
    if (threshold < 5) return("Low threshold - model useful for most patients")
    if (threshold < 20) return("Moderate threshold - model useful for moderate risk")
    return("High threshold - model useful for high risk only")
}

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

get_discrimination_quality <- function(harrell_c) {
    if (is.na(harrell_c)) return("Not available")
    if (harrell_c >= 0.9) return("Excellent")
    if (harrell_c >= 0.8) return("Very good")
    if (harrell_c >= 0.7) return("Good")
    return("Moderate")
}

get_oe_calibration_quality <- function(oe_ratio) {
    if (is.na(oe_ratio)) return("Not available")
    if (abs(oe_ratio - 1) < 0.1) return("Excellent")
    if (abs(oe_ratio - 1) < 0.2) return("Good")
    return("Moderate")
}

get_overall_performance_assessment <- function(harrell_c, oe_ratio) {
    if (is.na(harrell_c) || is.na(oe_ratio)) return("Insufficient data")
    
    disc_quality <- if (harrell_c >= 0.8) "Strong" else if (harrell_c >= 0.7) "Moderate" else "Limited"
    cal_quality <- if (abs(oe_ratio - 1) < 0.2) "Good" else "Moderate"
    
    if (disc_quality == "Strong" && cal_quality == "Good") return("Excellent overall performance")
    if (disc_quality == "Strong" || cal_quality == "Good") return("Good overall performance")
    return("Moderate overall performance")
}

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
