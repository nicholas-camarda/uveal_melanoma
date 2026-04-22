# Derived Variables Documentation System
# Author: Nicholas Camarda
# Description: Comprehensive documentation of all derived variables created during data processing
# This script generates documentation for all derived variables and exports to Excel

if (!exists("TOOLS_OUTPUT_DIR", inherits = TRUE)) {
    source(here::here("scripts", "load_all.R"))
}

# =============================================================================
# DERIVED VARIABLE DOCUMENTATION
# =============================================================================
# Comprehensive documentation of all derived variables created during data processing
# Each variable includes: description, calculation method, purpose, and data type

DERIVED_VARIABLE_DOCUMENTATION <- list(

    # ===== DEMOGRAPHIC AND BASIC VARIABLES =====
    age_at_diagnosis = list(
        description = "Patient age at time of diagnosis",
        calculation = "difftime(date_diagnosis, dob, units = 'days') / DAYS_IN_YEAR",
        purpose = "Primary demographic variable for analysis and adjustment",
        data_type = "numeric",
        units = "years"
    ),

    # ===== FOLLOW-UP TIME VARIABLES =====
    follow_up_days = list(
        description = "Total follow-up time from diagnosis to last known alive date",
        calculation = "difftime(last_known_alive_date, date_diagnosis, units = 'days')",
        purpose = "Raw follow-up time for censoring calculations",
        data_type = "numeric",
        units = "days"
    ),
    follow_up_years = list(
        description = "Total follow-up time in years",
        calculation = "follow_up_days / DAYS_IN_YEAR",
        purpose = "Follow-up time for survival analysis and reporting",
        data_type = "numeric",
        units = "years"
    ),
    follow_up_months = list(
        description = "Total follow-up time in months",
        calculation = "follow_up_days / DAYS_IN_MONTH",
        purpose = "Follow-up time for survival analysis (oncology standard)",
        data_type = "numeric",
        units = "months"
    ),

    # ===== TREATMENT VARIABLES =====
    treatment_date = list(
        description = "Date of initial treatment (GKSRS or PBT)",
        calculation = "case_when(treatment_group == 'GKSRS' ~ initial_gk_date, treatment_group == 'PBT' ~ initial_plaque_date)",
        purpose = "Reference date for all time-to-event calculations",
        data_type = "Date",
        units = "date"
    ),

    # ===== TIME-TO-EVENT VARIABLES (MONTHS) =====
    tt_recurrence_months = list(
        description = "Time from treatment to first recurrence (months)",
        calculation = "case_when(recurrence1 == 'Y' ~ interval(treatment_date, recurrence1_date), TRUE ~ interval(treatment_date, last_known_alive_date))",
        purpose = "Primary endpoint for recurrence-free survival analysis",
        data_type = "numeric",
        units = "months"
    ),
    tt_mets_months = list(
        description = "Time from treatment to metastatic progression (months)",
        calculation = "case_when(mets_progression == 'Y' ~ interval(treatment_date, mets_progression_date), TRUE ~ interval(treatment_date, last_known_alive_date))",
        purpose = "Primary endpoint for metastasis-free survival analysis",
        data_type = "numeric",
        units = "months"
    ),
    tt_death_months = list(
        description = "Time from treatment to death (months)",
        calculation = "case_when(!is.na(dod) ~ interval(treatment_date, dod), TRUE ~ interval(treatment_date, last_known_alive_date))",
        purpose = "Primary endpoint for overall survival analysis",
        data_type = "numeric",
        units = "months"
    ),
    tt_pfs_months = list(
        description = "Progression-free survival time (recurrence OR death, whichever comes first)",
        calculation = "pmin(tt_recurrence_months, tt_death_months, na.rm = FALSE)",
        purpose = "Composite endpoint for progression-free survival analysis",
        data_type = "numeric",
        units = "months"
    ),

    # ===== PFS-2 VARIABLES (SECOND PROGRESSION) =====
    tt_pfs2_months = list(
        description = "Time from first recurrence treatment to second recurrence (months)",
        calculation = "case_when(second recurrence observed before death ~ interval(recurrence1_treatment_date, recurrence2_date), recurrence1 == 'Y' & !is.na(recurrence1_treatment_date) ~ interval(recurrence1_treatment_date, censor_date), TRUE ~ NA)",
        purpose = "Secondary endpoint for progression after first recurrence treatment",
        data_type = "numeric",
        units = "months"
    ),

    # ===== LEGACY TIME VARIABLES (DAYS) =====
    tt_recurrence = list(
        description = "Time from treatment to first recurrence (days) - LEGACY",
        calculation = "case_when(recurrence1 == 'Y' ~ difftime(recurrence1_date, treatment_date, units = 'days'), TRUE ~ difftime(last_known_alive_date, treatment_date, units = 'days'))",
        purpose = "Legacy variable for backward compatibility",
        data_type = "numeric",
        units = "days"
    ),
    tt_mets = list(
        description = "Time from treatment to metastatic progression (days) - LEGACY",
        calculation = "case_when(mets_progression == 'Y' ~ difftime(mets_progression_date, treatment_date, units = 'days'), TRUE ~ difftime(last_known_alive_date, treatment_date, units = 'days'))",
        purpose = "Legacy variable for backward compatibility",
        data_type = "numeric",
        units = "days"
    ),
    tt_death = list(
        description = "Time from treatment to death (days) - LEGACY",
        calculation = "case_when(!is.na(dod) ~ difftime(dod, treatment_date, units = 'days'), TRUE ~ difftime(last_known_alive_date, treatment_date, units = 'days'))",
        purpose = "Legacy variable for backward compatibility",
        data_type = "numeric",
        units = "days"
    ),
    tt_pfs2 = list(
        description = "Time from first recurrence treatment to second recurrence (days) - LEGACY",
        calculation = "case_when(second recurrence observed before death ~ difftime(recurrence2_date, recurrence1_treatment_date, units = 'days'), recurrence1 == 'Y' & !is.na(recurrence1_treatment_date) ~ difftime(censor_date, recurrence1_treatment_date, units = 'days'), TRUE ~ NA)",
        purpose = "Legacy variable for backward compatibility",
        data_type = "numeric",
        units = "days"
    ),

    # ===== TIME VARIABLES (YEARS) =====
    tt_recurrence_years = list(
        description = "Time from treatment to first recurrence (years)",
        calculation = "case_when(recurrence1 == 'Y' ~ interval(treatment_date, recurrence1_date), TRUE ~ interval(treatment_date, last_known_alive_date))",
        purpose = "Time-to-event for reporting and reference",
        data_type = "numeric",
        units = "years"
    ),
    tt_mets_years = list(
        description = "Time from treatment to metastatic progression (years)",
        calculation = "case_when(mets_progression == 'Y' ~ interval(treatment_date, mets_progression_date), TRUE ~ interval(treatment_date, last_known_alive_date))",
        purpose = "Time-to-event for reporting and reference",
        data_type = "numeric",
        units = "years"
    ),
    tt_death_years = list(
        description = "Time from treatment to death (years)",
        calculation = "case_when(!is.na(dod) ~ interval(treatment_date, dod), TRUE ~ interval(treatment_date, last_known_alive_date))",
        purpose = "Time-to-event for reporting and reference",
        data_type = "numeric",
        units = "years"
    ),
    tt_pfs2_years = list(
        description = "Time from first recurrence treatment to second recurrence (years)",
        calculation = "case_when(second recurrence observed before death ~ interval(recurrence1_treatment_date, recurrence2_date), recurrence1 == 'Y' & !is.na(recurrence1_treatment_date) ~ interval(recurrence1_treatment_date, censor_date), TRUE ~ NA)",
        purpose = "Time-to-event for reporting and reference",
        data_type = "numeric",
        units = "years"
    ),

    # ===== TUMOR RESPONSE VARIABLES =====
    height_change = list(
        description = "Change in tumor height from initial to follow-up",
        calculation = "case_when(recurrence1 == 'Y' ~ initial_tumor_height - recurrence1_pretreatment_height, TRUE ~ initial_tumor_height - last_height)",
        purpose = "Tumor response endpoint for height analysis",
        data_type = "numeric",
        units = "mm"
    ),

    # ===== PRE-TREATMENT FLAGS =====
    mets_before_treatment = list(
        description = "Flag for patients with metastatic progression before treatment",
        calculation = "tt_mets_months < 0",
        purpose = "Exclusion criteria for post-treatment analyses",
        data_type = "logical",
        units = "boolean"
    ),
    recurrence_before_treatment = list(
        description = "Flag for patients with recurrence before treatment",
        calculation = "tt_recurrence_months < 0",
        purpose = "Exclusion criteria for post-treatment analyses",
        data_type = "logical",
        units = "boolean"
    ),
    death_before_treatment = list(
        description = "Flag for patients who died before treatment",
        calculation = "tt_death_months < 0",
        purpose = "Exclusion criteria for post-treatment analyses",
        data_type = "logical",
        units = "boolean"
    ),

    # ===== ANALYSIS-READY TIME VARIABLES =====
    tt_mets_months_analysis = list(
        description = "Time to metastasis for analysis (negative values set to 0)",
        calculation = "if_else(tt_mets_months < 0, 0, tt_mets_months)",
        purpose = "Analysis-ready variable for post-treatment survival models",
        data_type = "numeric",
        units = "months"
    ),
    tt_recurrence_months_analysis = list(
        description = "Time to recurrence for analysis (negative values set to 0)",
        calculation = "if_else(tt_recurrence_months < 0, 0, tt_recurrence_months)",
        purpose = "Analysis-ready variable for post-treatment survival models",
        data_type = "numeric",
        units = "months"
    ),
    tt_death_months_analysis = list(
        description = "Time to death for analysis (negative values set to 0)",
        calculation = "if_else(tt_death_months < 0, 0, tt_death_months)",
        purpose = "Analysis-ready variable for post-treatment survival models",
        data_type = "numeric",
        units = "months"
    ),
    tt_pfs_months_analysis = list(
        description = "Progression-free survival for analysis (negative values set to 0)",
        calculation = "pmin(tt_recurrence_months_analysis, tt_death_months_analysis, na.rm = FALSE)",
        purpose = "Analysis-ready variable for post-treatment survival models",
        data_type = "numeric",
        units = "months"
    ),

    # ===== EVENT INDICATORS =====
    recurrence_event = list(
        description = "Binary indicator for first recurrence (1 = occurred, 0 = censored)",
        calculation = "if_else(recurrence1 == 'Y', 1, 0, missing = 0)",
        purpose = "Event indicator for recurrence-free survival analysis",
        data_type = "numeric",
        units = "binary"
    ),
    mets_event = list(
        description = "Binary indicator for metastatic progression (1 = occurred, 0 = censored)",
        calculation = "if_else(mets_progression == 'Y', 1, 0, missing = 0)",
        purpose = "Event indicator for metastasis-free survival analysis",
        data_type = "numeric",
        units = "binary"
    ),
    death_event = list(
        description = "Binary indicator for death (1 = occurred, 0 = censored)",
        calculation = "if_else(!is.na(dod), 1, 0, missing = 0)",
        purpose = "Event indicator for overall survival analysis",
        data_type = "numeric",
        units = "binary"
    ),
    pfs_event = list(
        description = "Binary indicator for progression-free survival (1 = progression OR death, 0 = censored)",
        calculation = "if_else(recurrence_event == 1 | death_event == 1, 1, 0)",
        purpose = "Event indicator for progression-free survival analysis",
        data_type = "numeric",
        units = "binary"
    ),
    pfs2_event = list(
        description = "Binary indicator for PFS-2 (1 = second recurrence, 0 = censored, NA = no first recurrence)",
        calculation = "case_when(second recurrence observed before death ~ 1, recurrence1 == 'Y' & !is.na(recurrence1_treatment_date) ~ 0, TRUE ~ NA)",
        purpose = "Event indicator for PFS-2 analysis (only for patients with first recurrence)",
        data_type = "numeric",
        units = "binary"
    ),

    # ===== RECURRENCE TREATMENT VARIABLES =====
    recurrence1_treatment_clean = list(
        description = "Cleaned categorization of first recurrence treatment",
        calculation = "case_when(str_detect(tolower(recurrence1_treatment), 'gk') ~ 'GKSRS', str_detect(tolower(recurrence1_treatment), 'enuc') ~ 'Enucleation', str_detect(tolower(recurrence1_treatment), 'ttt') ~ 'TTT', TRUE ~ recurrence1_treatment)",
        purpose = "Categorized treatment for PFS-2 analysis. Note: Rare categories are collapsed into 'Other' by the standard rare category processing system.",
        data_type = "character",
        units = "categorical"
    ),

    # ===== BASELINE STATUS VARIABLES =====
    mets_free_at_baseline = list(
        description = "Flag for patients without metastatic disease at baseline",
        calculation = "!(mets_progression == 'Y' & mets_progression_date < treatment_date)",
        purpose = "Inclusion criteria for metastasis-free survival analysis",
        data_type = "logical",
        units = "boolean"
    ),

    # ===== GEP VALIDATION VARIABLES (OBJECTIVE 4) =====
    gep_class_simple = list(
        description = "Simplified GEP classification for analysis",
        calculation = "case_when(str_detect(biopsy1_gep_raw, 'Class_1') ~ 'Class 1', str_detect(biopsy1_gep_raw, 'Class_2') ~ 'Class 2', str_detect(biopsy1_gep_raw, 'No') ~ 'No', TRUE ~ NA)",
        purpose = "Primary GEP variable for survival analysis and validation",
        data_type = "character",
        units = "categorical"
    ),
    expected_mfs_5yr = list(
        description = "Expected 5-year metastasis-free survival from GEP",
        calculation = "biopsy1_gep_mfs",
        purpose = "GEP-predicted survival for validation analysis",
        data_type = "numeric",
        units = "probability (0-1)"
    ),
    expected_mfs_7yr = list(
        description = "Expected 7-year metastasis-free survival from GEP (extrapolated)",
        calculation = "biopsy1_gep_mfs^(7/5)",
        purpose = "Extended GEP prediction assuming exponential decay",
        data_type = "numeric",
        units = "probability (0-1)"
    ),
    expected_mfs_10yr = list(
        description = "Expected 10-year metastasis-free survival from GEP (extrapolated)",
        calculation = "biopsy1_gep_mfs^(10/5)",
        purpose = "Extended GEP prediction assuming exponential decay",
        data_type = "numeric",
        units = "probability (0-1)"
    ),
    expected_mss_5yr = list(
        description = "Expected 5-year melanoma-specific survival from GEP",
        calculation = "biopsy1_gep_mss",
        purpose = "GEP-predicted survival for validation analysis",
        data_type = "numeric",
        units = "probability (0-1)"
    ),
    expected_mss_7yr = list(
        description = "Expected 7-year melanoma-specific survival from GEP (extrapolated)",
        calculation = "biopsy1_gep_mss^(7/5)",
        purpose = "Extended GEP prediction assuming exponential decay",
        data_type = "numeric",
        units = "probability (0-1)"
    ),
    expected_mss_10yr = list(
        description = "Expected 10-year melanoma-specific survival from GEP (extrapolated)",
        calculation = "biopsy1_gep_mss^(10/5)",
        purpose = "Extended GEP prediction assuming exponential decay",
        data_type = "numeric",
        units = "probability (0-1)"
    ),
    prame_status = list(
        description = "PRAME status from GEP analysis",
        calculation = "case_when(str_detect(biopsy1_gep, 'PRAME_positive') ~ 'Positive', str_detect(biopsy1_gep, 'PRAME_negative') ~ 'Negative', str_detect(biopsy1_gep, 'PRAME_not_reported|PRAME_Unknown') ~ 'Unknown', TRUE ~ 'Not Available')",
        purpose = "Secondary GEP variable for subgroup analysis",
        data_type = "character",
        units = "categorical"
    ),
    gep12_prame_status = list(
        description = "PRAME status restricted to tumors with Class 1 or Class 2 DecisionDx results",
        calculation = "case_when(gep_class_simple %in% c('Class 1', 'Class 2') & prame_status %in% c('Positive', 'Negative') ~ prame_status, TRUE ~ NA)",
        purpose = "Supports subgrouping on PRAME expression while implicitly filtering to definitive GEP classes",
        data_type = "factor",
        units = "categorical"
    ),

    # ===== GEP VALIDATION SET =====
    gep_validation_set = list(
        description = "Objective 4 eligibility status for imported GEP validation",
        calculation = "Label rows with non-missing biopsy1_gep_mfs, non-missing biopsy1_gep_mss, and a definitive simplified GEP class as 'Eligible'; mark all other rows as 'No GEP Data'",
        purpose = "Identifies rows with analyzable imported GEP probabilities without creating a model-training partition",
        data_type = "character",
        units = "categorical"
    )

    # # ===== MODIFIED STAGE VARIABLE =====
    # initial_overall_stage_modified = list(
    #     description = "Modified overall stage excluding stages with insufficient numbers",
    #     calculation = "case_when(initial_overall_stage %in% STAGES_TO_EXCLUDE_FROM_MODIFIED ~ NA, TRUE ~ as.character(initial_overall_stage))",
    #     purpose = "Stage variable for analysis (excludes stages 3B, 3C, 4 due to small numbers)",
    #     data_type = "factor",
    #     units = "categorical"
    # )
)

# =============================================================================
# DERIVED VARIABLE DOCUMENTATION HELPER FUNCTIONS
# =============================================================================

#' Get Documentation for Derived Variables
#'
#' @param variable_name Name of the derived variable (optional)
#' @return List of variable documentation or specific variable info
#' @examples
#' get_derived_variable_docs() # All variables
#' get_derived_variable_docs("age_at_diagnosis") # Specific variable
get_derived_variable_docs <- function(variable_name = NULL) {
    if (is.null(variable_name)) {
        return(DERIVED_VARIABLE_DOCUMENTATION)
    } else {
        if (variable_name %in% names(DERIVED_VARIABLE_DOCUMENTATION)) {
            return(DERIVED_VARIABLE_DOCUMENTATION[[variable_name]])
        } else {
            stop(sprintf("Variable '%s' not found in DERIVED_VARIABLE_DOCUMENTATION", variable_name))
        }
    }
}

#' Print Summary of All Derived Variables
#'
#' @return NULL (prints to console)
#' @examples
#' print_derived_variables_summary()
print_derived_variables_summary <- function() {
    cat("=== DERIVED VARIABLES SUMMARY ===\n")
    cat("Total derived variables:", length(DERIVED_VARIABLE_DOCUMENTATION), "\n\n")

    for (var_name in names(DERIVED_VARIABLE_DOCUMENTATION)) {
        var_info <- DERIVED_VARIABLE_DOCUMENTATION[[var_name]]
        cat(sprintf("%s:\n", var_name))
        cat(sprintf("  Description: %s\n", var_info$description))
        cat(sprintf("  Purpose: %s\n", var_info$purpose))
        cat(sprintf("  Data Type: %s (%s)\n", var_info$data_type, var_info$units))
        cat("\n")
    }
}

#' Categorize Derived Variables
#'
#' @param variable_name Name of the variable
#' @return Category string
categorize_derived_variable <- function(variable_name) {
    if (grepl("^age_", variable_name)) {
        return("Demographic")
    } else if (grepl("^follow_up_", variable_name)) {
        return("Follow-up Time")
    } else if (grepl("^tt_", variable_name)) {
        return("Time-to-Event")
    } else if (grepl("_event$", variable_name)) {
        return("Event Indicators")
    } else if (grepl("^gep_", variable_name) || grepl("prame", variable_name) || grepl("expected_", variable_name)) {
        return("GEP Variables")
    } else if (grepl("^treatment_", variable_name)) {
        return("Treatment")
    } else if (grepl("^recurrence", variable_name)) {
        return("Recurrence")
    } else if (grepl("^mets", variable_name)) {
        return("Metastasis")
    } else if (grepl("^death", variable_name) || grepl("dod", variable_name)) {
        return("Mortality")
    } else if (grepl("^tumor_", variable_name) || grepl("height", variable_name) || grepl("diameter", variable_name)) {
        return("Tumor Characteristics")
    } else if (grepl("^before_", variable_name)) {
        return("Pre-treatment Flags")
    } else {
        return("Other")
    }
}

#' Export Derived Variables Documentation to Excel
#'
#' Creates a well-formatted Excel spreadsheet with all derived variable documentation
#' for easy human reading and reference.
#'
#' @param output_file Path to output Excel file (default: "derived_variables_documentation.xlsx")
#' @param include_timestamp Whether to include timestamp in filename (default: TRUE)
#' @return Path to created Excel file
#' @examples
#' export_derived_variables_to_excel() # Creates timestamped file
#' export_derived_variables_to_excel("my_docs.xlsx", include_timestamp = FALSE) # Custom filename
export_derived_variables_to_excel <- function(output_file = NULL, include_timestamp = TRUE) {
    # Check if openxlsx is available
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("openxlsx package is required. Install with: install.packages('openxlsx')")
    }

    # Create output filename
    if (is.null(output_file)) {
        if (include_timestamp) {
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            output_file <- sprintf("derived_variables_documentation_%s.xlsx", timestamp)
        } else {
            output_file <- "derived_variables_documentation.xlsx"
        }
    }

    # Convert documentation to data frame
    doc_list <- DERIVED_VARIABLE_DOCUMENTATION
    doc_df <- data.frame(
        Variable_Name = names(doc_list),
        Category = sapply(names(doc_list), categorize_derived_variable),
        Description = sapply(doc_list, function(x) x$description),
        Calculation = sapply(doc_list, function(x) x$calculation),
        Purpose = sapply(doc_list, function(x) x$purpose),
        Data_Type = sapply(doc_list, function(x) x$data_type),
        Units = sapply(doc_list, function(x) x$units),
        stringsAsFactors = FALSE
    )

    # Sort by category and then by variable name
    doc_df <- doc_df[order(doc_df$Category, doc_df$Variable_Name), ]

    # Create workbook
    wb <- openxlsx::createWorkbook()

    # Add worksheet
    sheet_name <- "Derived Variables"
    openxlsx::addWorksheet(wb, sheet_name)

    # Write data
    openxlsx::writeData(wb, sheet_name, doc_df, startRow = 2, startCol = 1)

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
        data.frame(Header = "DERIVED VARIABLES DOCUMENTATION"),
        startRow = 1, startCol = 1
    )
    openxlsx::mergeCells(wb, sheet_name, cols = 1:7, rows = 1)
    openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = 1)

    # Style column headers
    openxlsx::addStyle(wb, sheet_name, header_style, rows = 2, cols = 1:7)

    # Set column widths
    openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = 25) # Variable Name
    openxlsx::setColWidths(wb, sheet_name, cols = 2, widths = 15) # Category
    openxlsx::setColWidths(wb, sheet_name, cols = 3, widths = 40) # Description
    openxlsx::setColWidths(wb, sheet_name, cols = 4, widths = 60) # Calculation
    openxlsx::setColWidths(wb, sheet_name, cols = 5, widths = 40) # Purpose
    openxlsx::setColWidths(wb, sheet_name, cols = 6, widths = 12) # Data Type
    openxlsx::setColWidths(wb, sheet_name, cols = 7, widths = 10) # Units
    format_excel_worksheet_dimensions(wb, sheet_name, doc_df, start_row = 2)

    # Add alternating row colors for readability
    alt_style <- openxlsx::createStyle(fgFill = "#F2F2F2")
    for (i in seq(4, nrow(doc_df) + 2, by = 2)) {
        openxlsx::addStyle(wb, sheet_name, alt_style, rows = i, cols = 1:7)
    }

    # Add summary sheet
    summary_sheet <- "Summary"
    openxlsx::addWorksheet(wb, summary_sheet)

    # Create summary statistics
    category_counts <- table(doc_df$Category)
    summary_df <- data.frame(
        Category = names(category_counts),
        Count = as.numeric(category_counts),
        Percentage = round(as.numeric(category_counts) / sum(category_counts) * 100, 1)
    )

    openxlsx::writeData(wb, summary_sheet,
        data.frame(Header = "DERIVED VARIABLES SUMMARY"),
        startRow = 1, startCol = 1
    )
    openxlsx::mergeCells(wb, summary_sheet, cols = 1:3, rows = 1)
    openxlsx::addStyle(wb, summary_sheet, header_style, rows = 1, cols = 1)

    openxlsx::writeData(wb, summary_sheet, summary_df, startRow = 3, startCol = 1)
    openxlsx::addStyle(wb, summary_sheet, header_style, rows = 3, cols = 1:3)

    # Set summary column widths
    openxlsx::setColWidths(wb, summary_sheet, cols = 1, widths = 25)
    openxlsx::setColWidths(wb, summary_sheet, cols = 2, widths = 10)
    openxlsx::setColWidths(wb, summary_sheet, cols = 3, widths = 15)
    format_excel_worksheet_dimensions(wb, summary_sheet, summary_df, start_row = 3)

    # Save workbook
    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)

    cat(sprintf("Derived variables documentation exported to: %s\n", output_file))
    cat(sprintf("Total variables documented: %d\n", nrow(doc_df)))
    cat(sprintf("Categories: %s\n", paste(names(category_counts), collapse = ", ")))

    return(output_file)
}

#' Generate and Export Derived Variables Documentation
#'
#' Main function to generate comprehensive documentation and export to Excel
#' in the final_data/Analytic Dataset folder.
#'
#' @param dataset_name Analytic dataset to validate against.
#' @param output_dir Directory where documentation artifacts will be written.
#' @param include_timestamp Whether to include timestamp in filename (default: FALSE)
#' @return Path to created Excel file
#' @examples
#' generate_derived_variables_documentation() # Creates file in tools_output folder
generate_derived_variables_documentation <- function(
    dataset_name = "uveal_melanoma_full_cohort",
    output_dir = TOOLS_OUTPUT_DIR,
    include_timestamp = FALSE
) {
    output_dir <- ensure_tool_output_dir(output_dir)
    output_file <- tool_output_path(
        tool_name = "derived_variables_documentation",
        extension = "xlsx",
        output_dir = output_dir,
        include_timestamp = include_timestamp
    )

    validation_file <- tool_output_path(
        tool_name = "derived_variables_documentation_validation",
        extension = "csv",
        output_dir = output_dir,
        include_timestamp = include_timestamp
    )

    data <- load_tool_dataset(dataset_name)

    # Export to Excel
    result_file <- export_derived_variables_to_excel(output_file, include_timestamp = FALSE)

    validation_results <- validate_derived_variables_documentation(list(analytic_dataset = data))
    validation_summary <- data.frame(
        dataset_name = dataset_name,
        documented_variables = validation_results$documented_variables,
        actual_variables = validation_results$actual_variables,
        missing_in_data_count = length(validation_results$missing_in_data),
        undocumented_data_count = length(validation_results$undocumented_data),
        documentation_complete = validation_results$documentation_complete,
        missing_in_data = if (length(validation_results$missing_in_data) > 0) {
            paste(validation_results$missing_in_data, collapse = "; ")
        } else {
            ""
        },
        undocumented_data = if (length(validation_results$undocumented_data) > 0) {
            paste(validation_results$undocumented_data, collapse = "; ")
        } else {
            ""
        },
        stringsAsFactors = FALSE
    )
    write.csv(validation_summary, validation_file, row.names = FALSE)

    run_summary <- write_tool_run_summary(
        tool_name = "derived_variables_documentation",
        outputs = list(
            documentation = result_file,
            validation = validation_file
        ),
        dataset_name = dataset_name,
        notes = sprintf("documented_variables=%d", length(DERIVED_VARIABLE_DOCUMENTATION)),
        output_dir = output_dir
    )

    logger::log_info(sprintf("Derived variables documentation generated: %s", result_file))

    return(list(
        output_file = result_file,
        validation_file = validation_file,
        validation_results = validation_results,
        run_summary = run_summary
    ))
}

# =============================================================================
# VALIDATION FUNCTIONS
# =============================================================================

#' Validate Derived Variables Against Actual Data
#'
#' Compares documented derived variables with actual variables in processed data
#' to ensure documentation is complete and accurate.
#'
#' @param processed_data List of processed data frames (cohorts)
#' @return List of validation results
#' @examples
#' validate_derived_variables_documentation(processed_data)
validate_derived_variables_documentation <- function(processed_data) {
    documented_vars <- names(DERIVED_VARIABLE_DOCUMENTATION)
    actual_vars <- unique(unlist(lapply(processed_data, names)))

    # Check that every documented derived variable actually exists in the data
    missing_in_data <- setdiff(documented_vars, actual_vars)

    # List other variables present in data but not part of the derived-variable catalog
    undocumented_data <- setdiff(actual_vars, documented_vars)

    validation_results <- list(
        documented_variables = length(documented_vars),
        actual_variables = length(actual_vars),
        missing_in_data = missing_in_data,
        undocumented_data = undocumented_data,
        documentation_complete = length(missing_in_data) == 0
    )

    cat("=== DERIVED VARIABLES DOCUMENTATION VALIDATION ===\n")
    cat(sprintf("Documented variables: %d\n", validation_results$documented_variables))
    cat(sprintf("Actual variables: %d\n", validation_results$actual_variables))
    cat(sprintf("All documented variables present in data: %s\n", validation_results$documentation_complete))

    if (length(missing_in_data) > 0) {
        cat(sprintf("Missing in data: %s\n", paste(missing_in_data, collapse = ", ")))
    }

    if (length(undocumented_data) > 0) {
        cat("Note:", length(undocumented_data), "data columns are not in the derived-variable catalog (expected for raw source fields).\n")
    }

    return(validation_results)
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

if (sys.nframe() == 0L) {
    cat("=== DERIVED VARIABLES DOCUMENTATION GENERATION ===\n\n")

    # Generate and export documentation
    cat("Generating derived variables documentation...\n")
    result <- generate_derived_variables_documentation(include_timestamp = FALSE)
    cat(sprintf("Documentation exported to: %s\n", result$output_file))
    cat(sprintf("Validation summary written to: %s\n\n", result$validation_file))

    if (result$validation_results$documentation_complete) {
        cat("✓ Documentation validation passed - all documented variables exist in the analytic dataset\n")
    } else {
        cat("⚠ Documentation validation found documented variables missing in the dataset\n")
    }

    cat("\n=== DOCUMENTATION GENERATION COMPLETE ===\n")
}
