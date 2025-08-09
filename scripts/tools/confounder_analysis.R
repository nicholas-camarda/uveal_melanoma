# Confounder Analysis and Update Utility
# This script identifies variables that differ across treatment groups and updates the confounders list
# CRITICAL: Uses processed data with collapsed categories to match actual analysis pipeline

source("scripts/utils/all_helper_functions.R")

# Source configuration
# No need to source all_helper_functions.R - it will be sourced by the calling script

#' Interpret Cohen's d into qualitative label
#' @param d Numeric effect size
#' @return Character interpretation
interpret_cohens_d <- function(d) {
    if (abs(d) < 0.2) return("negligible effect")
    if (abs(d) < 0.5) return("small effect")
    if (abs(d) < 0.8) return("medium effect")
    return("large effect")
}

#' Interpret Cramer's V into qualitative label
#' @param v Numeric effect size
#' @return Character interpretation
interpret_cramers_v <- function(v) {
    if (v < 0.1) return("weak association")
    if (v < 0.3) return("moderate association")
    if (v < 0.5) return("strong association")
    return("very strong association")
}

#' Interpret odds ratio into qualitative label
#' @param or Numeric odds ratio
#' @return Character interpretation
interpret_odds_ratio <- function(or) {
    if (or < 1.5) return("small effect")
    if (or < 2.5) return("moderate effect")
    if (or < 4.0) return("large effect")
    return("very large effect")
}

#' Wrap effect size interpretation with parentheses for display
#' @param effect_size Numeric effect size
#' @param effect_size_type Character type (e.g., "Cohen's d")
#' @return Character string like "(small effect)"
get_effect_interpretation <- function(effect_size, effect_size_type) {
    if (is.na(effect_size) || length(effect_size) == 0) return("(effect size not available)")
    
    if (effect_size_type == "Cohen's d") {
        interpretation <- interpret_cohens_d(effect_size)
        return(sprintf("(%s)", interpretation))
    } else if (effect_size_type == "Cramer's V" || effect_size_type == "Cramer's V (alternative)") {
        interpretation <- interpret_cramers_v(effect_size)
        return(sprintf("(%s)", interpretation))
    } else if (effect_size_type == "Odds ratio" || effect_size_type == "Odds ratio (adjusted)") {
        interpretation <- interpret_odds_ratio(effect_size)
        return(sprintf("(%s)", interpretation))
    } else if (effect_size_type == "Not calculable") {
        return("(effect size not calculable due to data structure)")
    } else {
        return("(effect size interpretation not available)")
    }
}

cat("=== CONFOUNDER ANALYSIS AND UPDATE ===\n")
cat("USING PROCESSED DATA WITH COLLAPSED CATEGORIES\n\n")

# Load the processed cohort data (with collapsed categories)
data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

# Load the other_map to understand what categories were collapsed
other_map_file <- file.path(PROCESSED_DATA_DIR, "other_map.rds")
if (file.exists(other_map_file)) {
    other_map <- readRDS(other_map_file)
    cat("Loaded other_map with collapsed categories information\n")
    
    # Display collapsed categories
    if (length(other_map) > 0) {
        cat("\nCOLLAPSED CATEGORIES:\n")
        for (var_name in names(other_map)) {
            if (length(other_map[[var_name]]) > 0) {
                cat(sprintf("  %s: %s\n", var_name, paste(other_map[[var_name]], collapse = ", ")))
            }
        }
    }
} else {
    other_map <- list()
    cat("No other_map.rds found - using raw data categories\n")
}

cat(sprintf("\nLoaded processed cohort with %d patients\n\n", nrow(data)))

# Check current confounders
current_confounders <- confounders
cat("CURRENT CONFOUNDERS:\n")
for (i in seq_along(current_confounders)) {
    cat(sprintf("  %d. %s\n", i, current_confounders[i]))
}
cat("\n")

# Variables to test for differences (baseline characteristics)
test_variables <- BASELINE_VARIABLES_TO_SUMMARIZE[BASELINE_VARIABLES_TO_SUMMARIZE != "treatment_group"]

cat("TESTING", length(test_variables), "VARIABLES FOR TREATMENT GROUP DIFFERENCES:\n\n")

# Test each variable for differences between treatment groups
significant_vars <- list()
results_summary <- data.frame(
    variable = character(),
    p_value = numeric(),
    test_type = character(),
    effect_size = numeric(),
    effect_size_type = character(),
    effect_interpretation = character(),
    significant = logical(),
    stringsAsFactors = FALSE
)

for (var in test_variables) {
    if (!var %in% names(data)) {
        cat(sprintf("SKIP: %s (not in data)\n", var))
        next
    }
    
    # Skip if already in confounders
    if (var %in% current_confounders) {
        cat(sprintf("SKIP: %s (already in confounders)\n", var))
        next
    }
    
    # Check if variable has sufficient variation
    if (is.numeric(data[[var]])) {
        # Continuous variable - t-test and Cohen's d
        tryCatch({
            test_result <- t.test(data[[var]] ~ data$treatment_group)
            p_val <- test_result$p.value
            test_type <- "t-test"
            
            # Calculate Cohen's d
            group1_data <- data[[var]][data$treatment_group == levels(data$treatment_group)[1]]
            group2_data <- data[[var]][data$treatment_group == levels(data$treatment_group)[2]]
            
            # Check if we have sufficient data
            if (length(group1_data) < 2 || length(group2_data) < 2) {
                stop("Insufficient data for effect size calculation")
            }
            
            n1 <- length(group1_data)
            n2 <- length(group2_data)
            pooled_sd <- sqrt(((n1-1)*var(group1_data) + (n2-1)*var(group2_data)) / (n1 + n2 - 2))
            
            # Check for zero variance
            if (pooled_sd == 0) {
                cohens_d <- 0
            } else {
                cohens_d <- (mean(group1_data) - mean(group2_data)) / pooled_sd
            }
            
            effect_size <- cohens_d
            effect_size_type <- "Cohen's d"
        }, error = function(e) {
            p_val <- NA
            test_type <- "t-test (failed)"
            effect_size <- NA
            effect_size_type <- "Cohen's d"
        })
    } else {
        # Categorical variable - chi-square or fisher
        tryCatch({
            # Create contingency table
            cont_table <- table(data[[var]], data$treatment_group)
            
            # Check if chi-square is appropriate
            expected_counts <- chisq.test(cont_table)$expected
            if (any(expected_counts < 5)) {
                # Use Fisher's exact test for small expected counts
                test_result <- fisher.test(cont_table, simulate.p.value = TRUE)
                test_type <- "Fisher's exact"
                
                # Try to get odds ratio from Fisher's test
                if (!is.null(test_result$estimate) && !is.na(test_result$estimate)) {
                    effect_size <- test_result$estimate
                    effect_size_type <- "Odds ratio"
                } else {
                    # Manual odds ratio calculation for 2x2 tables
                    if (nrow(cont_table) == 2 && ncol(cont_table) == 2) {
                        # Add small constant to avoid zero cells
                        cont_table_adj <- cont_table + 0.5
                        odds_ratio <- (cont_table_adj[1,1] * cont_table_adj[2,2]) / 
                                     (cont_table_adj[1,2] * cont_table_adj[2,1])
                        effect_size <- odds_ratio
                        effect_size_type <- "Odds ratio (adjusted)"
                    } else {
                        # For larger tables, use Cramer's V as alternative
                        chi_sq <- chisq.test(cont_table)$statistic
                        n <- sum(cont_table)
                        min_dim <- min(nrow(cont_table), ncol(cont_table))
                        
                        if (n > 0 && min_dim > 1) {
                            cramers_v <- sqrt(chi_sq / (n * (min_dim - 1)))
                            effect_size <- cramers_v
                            effect_size_type <- "Cramer's V (alternative)"
                        } else {
                            effect_size <- NA
                            effect_size_type <- "Not calculable"
                        }
                    }
                }
            } else {
                test_result <- chisq.test(cont_table)
                test_type <- "Chi-square"
                
                # Calculate Cramer's V
                chi_sq <- test_result$statistic
                n <- sum(cont_table)
                min_dim <- min(nrow(cont_table), ncol(cont_table))
                
                # Check for valid calculation
                if (n > 0 && min_dim > 1) {
                    cramers_v <- sqrt(chi_sq / (n * (min_dim - 1)))
                } else {
                    cramers_v <- NA
                }
                
                effect_size <- cramers_v
                effect_size_type <- "Cramer's V"
            }
            p_val <- test_result$p.value
        }, error = function(e) {
            p_val <- NA
            test_type <- "categorical test (failed)"
            effect_size <- NA
            effect_size_type <- "Unknown"
        })
    }
    
    significant <- !is.na(p_val) && p_val < 0.05
    
    # Get human-readable interpretation
    interpretation <- get_effect_interpretation(effect_size, effect_size_type)
    
    # Store results
    results_summary <- rbind(results_summary, data.frame(
        variable = var,
        p_value = p_val,
        test_type = test_type,
        effect_size = effect_size,
        effect_size_type = effect_size_type,
        effect_interpretation = interpretation,
        significant = significant,
        stringsAsFactors = FALSE
    ))
    
    # Print results with effect size and interpretation
    if (significant) {
        # Format p-value with appropriate precision for very small values
        p_formatted <- ifelse(p_val < 0.0001, sprintf("p<%.1e", p_val), sprintf("p=%.4f", p_val))
        cat(sprintf("SIGNIFICANT: %s (%s, %s, %s=%.3f %s)\n", 
                    var, p_formatted, test_type, effect_size_type, effect_size, interpretation))
        significant_vars[[var]] <- list(p_value = p_val, test_type = test_type, 
                                       effect_size = effect_size, effect_size_type = effect_size_type,
                                       interpretation = interpretation)
    } else {
        # Format p-value with appropriate precision for very small values
        p_formatted <- ifelse(p_val < 0.0001, sprintf("p<%.1e", p_val), sprintf("p=%.4f", p_val))
        cat(sprintf("NS: %s (%s, %s, %s=%.3f %s)\n", 
                    var, p_formatted, test_type, effect_size_type, effect_size, interpretation))
    }
}

cat("\n=== SUMMARY ===\n")
cat(sprintf("Found %d variables with significant differences (p<0.05):\n", length(significant_vars)))

if (length(significant_vars) > 0) {
    cat("\nSIGNIFICANT VARIABLES:\n")
    for (i in seq_along(significant_vars)) {
        var_name <- names(significant_vars)[i]
        p_val <- significant_vars[[var_name]]$p_value
        test_type <- significant_vars[[var_name]]$test_type
        effect_size <- significant_vars[[var_name]]$effect_size
        effect_size_type <- significant_vars[[var_name]]$effect_size_type
        interpretation <- significant_vars[[var_name]]$interpretation
        
        # Format p-value with appropriate precision for very small values
        p_formatted <- ifelse(p_val < 0.0001, sprintf("p<%.1e", p_val), sprintf("p=%.4f", p_val))
        cat(sprintf("  %d. %s (%s, %s, %s=%.3f %s)\n", 
                    i, var_name, p_formatted, test_type, effect_size_type, effect_size, interpretation))
    }
    
    # Filter out variables that might be problematic
    problematic_vars <- c(
        "initial_mets",  # Too rare
        "initial_m_stage",  # Too rare
        "initial_n_stage",   # Too rare
        "initial_tumor_height"  # May cause overadjustment in height change analysis
    )
    
    recommended_vars <- names(significant_vars)[!names(significant_vars) %in% problematic_vars]
    
    cat("\nRECOMMENDED CONFOUNDERS TO ADD:\n")
    for (i in seq_along(recommended_vars)) {
        var_name <- recommended_vars[i]
        p_val <- significant_vars[[var_name]]$p_value
        effect_size <- significant_vars[[var_name]]$effect_size
        effect_size_type <- significant_vars[[var_name]]$effect_size_type
        interpretation <- significant_vars[[var_name]]$interpretation
        
        # Format p-value with appropriate precision for very small values
        p_formatted <- ifelse(p_val < 0.0001, sprintf("p<%.1e", p_val), sprintf("p=%.4f", p_val))
        cat(sprintf("  %d. %s (%s, %s=%.3f %s)\n", 
                    i, var_name, p_formatted, effect_size_type, effect_size, interpretation))
    }
    
    # Special handling for overall stage
    cat("\n=== STAGE ANALYSIS ===\n")
    if ("initial_overall_stage" %in% names(significant_vars)) {
        stage_table <- table(data$initial_overall_stage, data$treatment_group, useNA = "no")
        cat("Overall stage distribution:\n")
        print(stage_table)
        
        stage4_count <- sum(data$initial_overall_stage == "4", na.rm = TRUE)
        cat(sprintf("\nStage IV patients: %d (%.1f%% of cohort)\n", 
                    stage4_count, 100 * stage4_count / nrow(data)))
        
        if (stage4_count < 10) {
            cat("→ Stage IV has low numbers - consider binary stage variable or exclude\n")
        }
    }
    
    # Create updated confounders list
    updated_confounders <- c(current_confounders, recommended_vars)
    updated_confounders <- unique(updated_confounders)  # Remove duplicates
    
    cat("\n=== UPDATED CONFOUNDERS LIST ===\n")
    for (i in seq_along(updated_confounders)) {
        cat(sprintf("  %d. %s\n", i, updated_confounders[i]))
    }
    
    # Generate R code for updating config_constants.R
cat("\n=== R CODE TO UPDATE config_constants.R ===\n")
cat("# Replace the confounders line in config_constants.R with:\n")
    cat("confounders <- c(\n")
    for (i in seq_along(updated_confounders)) {
        comma <- if (i < length(updated_confounders)) "," else ""
        cat(sprintf('    "%s"%s\n', updated_confounders[i], comma))
    }
    cat(")\n\n")
    
    # Save detailed results to Excel file
    output_file <- file.path(PROCESSED_DATA_DIR, "tools_output", "confounder_analysis_results.xlsx")
    
    # Create workbook
    wb <- openxlsx::createWorkbook()
    
    # Add main results sheet
    sheet_name <- "Confounder Analysis Results"
    openxlsx::addWorksheet(wb, sheet_name)
    
    # Write data
    openxlsx::writeData(wb, sheet_name, results_summary, startRow = 2, startCol = 1)
    
    # Add header
    header_style <- openxlsx::createStyle(
        fontSize = 12,
        fontColour = "#FFFFFF",
        fgFill = "#366092",
        halign = "center",
        valign = "center",
        textDecoration = "bold"
    )
    
    openxlsx::writeData(wb, sheet_name, 
                       data.frame(Header = "CONFOUNDER ANALYSIS RESULTS"), 
                       startRow = 1, startCol = 1)
    openxlsx::mergeCells(wb, sheet_name, cols = 1:7, rows = 1)
    openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = 1)
    
    # Style column headers
    openxlsx::addStyle(wb, sheet_name, header_style, rows = 2, cols = 1:7)
    
    # Set column widths
    openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = 35)  # Variable
    openxlsx::setColWidths(wb, sheet_name, cols = 2, widths = 15)  # P-value (wider for scientific notation)
    openxlsx::setColWidths(wb, sheet_name, cols = 3, widths = 15)  # Test type
    openxlsx::setColWidths(wb, sheet_name, cols = 4, widths = 12)  # Effect size
    openxlsx::setColWidths(wb, sheet_name, cols = 5, widths = 15)  # Effect size type
    openxlsx::setColWidths(wb, sheet_name, cols = 6, widths = 25)  # Interpretation
    openxlsx::setColWidths(wb, sheet_name, cols = 7, widths = 12)  # Significant
    
    # Add alternating row colors for readability
    alt_style <- openxlsx::createStyle(fgFill = "#F2F2F2")
    for (i in seq(4, nrow(results_summary) + 2, by = 2)) {
        openxlsx::addStyle(wb, sheet_name, alt_style, rows = i, cols = 1:7)
    }
    
    # Add significant variables summary sheet
    if (length(significant_vars) > 0) {
        summary_sheet <- "Significant Variables"
        openxlsx::addWorksheet(wb, summary_sheet)
        
        # Create summary data
        summary_data <- data.frame(
            Variable = names(significant_vars),
            P_Value = sapply(significant_vars, function(x) x$p_value),
            Test_Type = sapply(significant_vars, function(x) x$test_type),
            Effect_Size = sapply(significant_vars, function(x) x$effect_size),
            Effect_Size_Type = sapply(significant_vars, function(x) x$effect_size_type),
            Interpretation = sapply(significant_vars, function(x) x$interpretation),
            stringsAsFactors = FALSE
        )
        
        openxlsx::writeData(wb, summary_sheet, 
                           data.frame(Header = "SIGNIFICANT VARIABLES SUMMARY"), 
                           startRow = 1, startCol = 1)
        openxlsx::mergeCells(wb, summary_sheet, cols = 1:6, rows = 1)
        openxlsx::addStyle(wb, summary_sheet, header_style, rows = 1, cols = 1)
        
        openxlsx::writeData(wb, summary_sheet, summary_data, startRow = 3, startCol = 1)
        openxlsx::addStyle(wb, summary_sheet, header_style, rows = 3, cols = 1:6)
        
        # Set column widths
        openxlsx::setColWidths(wb, summary_sheet, cols = 1, widths = 25)
        openxlsx::setColWidths(wb, summary_sheet, cols = 2, widths = 15)
        openxlsx::setColWidths(wb, summary_sheet, cols = 3, widths = 15)
        openxlsx::setColWidths(wb, summary_sheet, cols = 4, widths = 12)
        openxlsx::setColWidths(wb, summary_sheet, cols = 5, widths = 15)
        openxlsx::setColWidths(wb, summary_sheet, cols = 6, widths = 25)
    }
    
    # Add data preprocessing information sheet
    preprocessing_sheet <- "Data Preprocessing"
    openxlsx::addWorksheet(wb, preprocessing_sheet)
    
    # Create preprocessing summary
    preprocessing_info <- data.frame(
        Information = c(
            "Analysis Dataset",
            "Data Processing Applied",
            "Rare Category Threshold",
            "Variables with Collapsed Categories",
            "Total Variables Tested",
            "Significant Variables Found"
        ),
        Value = c(
            "Processed cohort with collapsed categories",
            "Rare categories collapsed into 'Other'",
            as.character(THRESHOLD_RARITY),
            as.character(length(other_map)),
            as.character(nrow(results_summary)),
            as.character(length(significant_vars))
        ),
        stringsAsFactors = FALSE
    )
    
    openxlsx::writeData(wb, preprocessing_sheet, 
                       data.frame(Header = "DATA PREPROCESSING INFORMATION"), 
                       startRow = 1, startCol = 1)
    openxlsx::mergeCells(wb, preprocessing_sheet, cols = 1:2, rows = 1)
    openxlsx::addStyle(wb, preprocessing_sheet, header_style, rows = 1, cols = 1)
    
    openxlsx::writeData(wb, preprocessing_sheet, preprocessing_info, startRow = 3, startCol = 1)
    openxlsx::addStyle(wb, preprocessing_sheet, header_style, rows = 3, cols = 1:2)
    
    # Add collapsed categories details if available
    if (length(other_map) > 0) {
        collapsed_data <- data.frame(
            Variable = character(),
            Collapsed_Categories = character(),
            stringsAsFactors = FALSE
        )
        
        for (var_name in names(other_map)) {
            if (length(other_map[[var_name]]) > 0) {
                collapsed_data <- rbind(collapsed_data, data.frame(
                    Variable = var_name,
                    Collapsed_Categories = paste(other_map[[var_name]], collapse = ", "),
                    stringsAsFactors = FALSE
                ))
            }
        }
        
        if (nrow(collapsed_data) > 0) {
            start_row <- nrow(preprocessing_info) + 5
            
            openxlsx::writeData(wb, preprocessing_sheet, 
                               data.frame(Header = "COLLAPSED CATEGORIES DETAILS"), 
                               startRow = start_row, startCol = 1)
            openxlsx::mergeCells(wb, preprocessing_sheet, cols = 1:2, rows = start_row)
            openxlsx::addStyle(wb, preprocessing_sheet, header_style, rows = start_row, cols = 1)
            
            openxlsx::writeData(wb, preprocessing_sheet, collapsed_data, 
                               startRow = start_row + 2, startCol = 1)
            openxlsx::addStyle(wb, preprocessing_sheet, header_style, 
                              rows = start_row + 2, cols = 1:2)
        }
    }
    
    # Set preprocessing column widths
    openxlsx::setColWidths(wb, preprocessing_sheet, cols = 1, widths = 30)
    openxlsx::setColWidths(wb, preprocessing_sheet, cols = 2, widths = 50)
    
    # Save workbook
    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
    
    cat(sprintf("Detailed results saved to: %s\n", output_file))
    cat("Excel file includes:\n")
    cat("- Complete analysis results with effect sizes\n")
    cat("- Human-readable effect size interpretations\n")
    cat("- Summary of significant variables\n")
    cat("- Data preprocessing information and collapsed categories\n")
    cat("- Professional formatting with headers and styling\n")
    
} else {
    cat("No additional variables found with significant differences.\n")
}

cat("\n=== EFFECT SIZE INTERPRETATION GUIDE ===\n")
cat("Cohen's d: negligible (<0.2), small (0.2-0.5), medium (0.5-0.8), large (>0.8)\n")
cat("Cramer's V: weak (<0.1), moderate (0.1-0.3), strong (0.3-0.5), very strong (>0.5)\n")
cat("Odds ratio: small (<1.5), moderate (1.5-2.5), large (2.5-4.0), very large (>4.0)\n\n")

cat("\n=== RECOMMENDATIONS ===\n")
cat("1. Review the significant variables above\n")
cat("2. Consider clinical relevance when adding confounders\n")
cat("3. Test models with new confounders to ensure convergence\n")
cat("4. Consider analysis-specific confounder lists for different outcomes\n\n")

cat("=== ANALYSIS COMPLETE ===\n") 