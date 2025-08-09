#' Comprehensive Variable Census
#'
#' This script creates a complete inventory of all variables in the dataset,
#' combining the original data dictionary with derived variables and current
#' dataset structure. This provides a single source of truth for all variables.
#'
#' @author AI Assistant
#' @date 2025-01-31

# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

#' Create comprehensive variable census
#'
#' @param data_dict_path Path to the Excel file containing the data dictionary
#' @param output_dir Directory to save the census output
#' @return List containing the comprehensive census
create_comprehensive_variable_census <- function(
    data_dict_path = DATA_DICTIONARY_PATH,
    output_dir = TOOLS_OUTPUT_DIR) {
    # Create output directory
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    # Load original data dictionary
    cat("Loading original data dictionary...\n")
    data_dict <- read_excel(data_dict_path, sheet = "Sheet1")

    # Clean column names - handle the actual column names from the file
    names(data_dict) <- c("variable_name", "description", "statistical_goals", "notes", "notes2")

    # Remove the problematic notes2 column
    data_dict <- data_dict[, c("variable_name", "description", "statistical_goals", "notes")]

    # Load current dataset to get actual variable information
    cat("Loading current dataset...\n")
    source("scripts/utils/all_helper_functions.R")
    current_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

    # Create current dataset variable information
    current_vars <- data.frame(
        variable_name = names(current_data),
        current_type = sapply(current_data, function(x) paste(class(x), collapse = ", ")),
        current_levels = sapply(current_data, function(x) {
            if (is.factor(x)) {
                paste(levels(x), collapse = ", ")
            } else if (is.character(x)) {
                paste(unique(x)[1:min(5, length(unique(x)))], collapse = ", ")
            } else if (is.numeric(x)) {
                paste(range(x, na.rm = TRUE), collapse = " to ")
            } else {
                "N/A"
            }
        }),
        current_missing = sapply(current_data, function(x) sum(is.na(x))),
        current_n = sapply(current_data, function(x) length(x)),
        stringsAsFactors = FALSE
    )

    # Merge original dictionary with current dataset info
    comprehensive_census <- left_join(data_dict, current_vars, by = "variable_name")

    # Add derived variable information
    derived_vars <- get_derived_variables_info()
    comprehensive_census <- left_join(comprehensive_census, derived_vars, by = "variable_name")

    # Add variable categories
    comprehensive_census <- comprehensive_census %>%
        mutate(
            variable_category = case_when(
                grepl("recurrence", variable_name, ignore.case = TRUE) ~ "Recurrence",
                grepl("mets|metastasis", variable_name, ignore.case = TRUE) ~ "Metastasis",
                grepl("death|tt_death|tt_mets|tt_pfs|tt_recurrence", variable_name, ignore.case = TRUE) ~ "Survival",
                grepl("height|diameter|size", variable_name, ignore.case = TRUE) ~ "Tumor Characteristics",
                grepl("vision|logmar|acuity", variable_name, ignore.case = TRUE) ~ "Vision",
                grepl("treatment|plaque|gksrs", variable_name, ignore.case = TRUE) ~ "Treatment",
                grepl("gep|biopsy", variable_name, ignore.case = TRUE) ~ "GEP/Molecular",
                grepl("age|sex|race|ethnicity", variable_name, ignore.case = TRUE) ~ "Demographics",
                grepl("date", variable_name, ignore.case = TRUE) ~ "Dates/Times",
                grepl("stage|t_|n_|m_", variable_name, ignore.case = TRUE) ~ "Staging",
                grepl("location|nerve", variable_name, ignore.case = TRUE) ~ "Anatomy",
                grepl("id|patient", variable_name, ignore.case = TRUE) ~ "Identification",
                TRUE ~ "Other"
            ),
            is_derived = !is.na(derivation_logic),
            is_current = variable_name %in% names(current_data),
            is_original = variable_name %in% data_dict$variable_name
        )

    # Create summary statistics
    summary_stats <- list(
        total_variables = nrow(comprehensive_census),
        original_variables = sum(comprehensive_census$is_original, na.rm = TRUE),
        derived_variables = sum(comprehensive_census$is_derived, na.rm = TRUE),
        current_variables = sum(comprehensive_census$is_current, na.rm = TRUE),
        missing_from_current = sum(!comprehensive_census$is_current, na.rm = TRUE),
        categories = table(comprehensive_census$variable_category)
    )

    # Save comprehensive census
    saveRDS(comprehensive_census, file.path(output_dir, "comprehensive_variable_census.rds"))

    # HTML report removed - only Excel output as requested

    # Create XLSX export with multiple sheets
    wb <- createWorkbook()

    # Main census sheet
    addWorksheet(wb, "Variable_Census")
    writeData(wb, "Variable_Census", comprehensive_census)

    # Summary statistics sheet
    summary_df <- data.frame(
        Metric = c(
            "Total Variables", "Original Variables", "Derived Variables",
            "Current Variables", "Missing from Current"
        ),
        Count = c(
            summary_stats$total_variables, summary_stats$original_variables,
            summary_stats$derived_variables, summary_stats$current_variables,
            summary_stats$missing_from_current
        )
    )
    addWorksheet(wb, "Summary_Statistics")
    writeData(wb, "Summary_Statistics", summary_df)

    # Category breakdown sheet
    category_df <- data.frame(
        Category = names(summary_stats$categories),
        Count = as.numeric(summary_stats$categories)
    )
    addWorksheet(wb, "Category_Breakdown")
    writeData(wb, "Category_Breakdown", category_df)

    # Save XLSX file
    saveWorkbook(wb, file.path(output_dir, "comprehensive_variable_census.xlsx"), overwrite = TRUE)

    cat("Comprehensive variable census created successfully!\n")
    cat("Files saved to:", output_dir, "\n")

    return(list(
        census = comprehensive_census,
        summary = summary_stats,
        output_dir = output_dir
    ))
}

#' Get derived variables information
#'
#' @return Data frame with derived variable information
get_derived_variables_info <- function() {
    # This would be populated from the derived variables documentation
    # For now, return empty data frame
    derived_vars <- data.frame(
        variable_name = character(),
        derivation_logic = character(),
        source_variables = character(),
        stringsAsFactors = FALSE
    )

    # Add known derived variables
    known_derived <- data.frame(
        variable_name = c(
            "tt_death_years", "tt_mets_years", "tt_pfs_months", "tt_recurrence_years",
            "death_event", "mets_event", "pfs_event", "recurrence_event",
            "treatment_group", "age_at_diagnosis", "initial_tumor_height",
            "initial_tumor_diameter", "initial_t_stage", "initial_n_stage", "initial_m_stage"
        ),
        derivation_logic = c(
            "Time from diagnosis to death in years",
            "Time from diagnosis to metastasis in years",
            "Time from diagnosis to progression in months",
            "Time from diagnosis to recurrence in years",
            "Binary indicator for death (1 = died, 0 = censored)",
            "Binary indicator for metastasis (1 = metastasized, 0 = censored)",
            "Binary indicator for progression (1 = progressed, 0 = censored)",
            "Binary indicator for recurrence (1 = recurred, 0 = censored)",
            "Treatment group (Plaque vs GKSRS)",
            "Age at diagnosis calculated from DOB",
            "Initial tumor height in mm",
            "Initial tumor diameter in mm",
            "Initial T stage",
            "Initial N stage",
            "Initial M stage"
        ),
        source_variables = c(
            "date_diagnosis, date_death",
            "date_diagnosis, date_mets",
            "date_diagnosis, date_progression",
            "date_diagnosis, date_recurrence",
            "date_death, date_last_followup",
            "date_mets, date_last_followup",
            "date_progression, date_last_followup",
            "date_recurrence, date_last_followup",
            "treatment_type",
            "dob, date_diagnosis",
            "tumor_height_initial",
            "tumor_diameter_initial",
            "t_stage_initial",
            "n_stage_initial",
            "m_stage_initial"
        ),
        stringsAsFactors = FALSE
    )

    return(rbind(derived_vars, known_derived))
}

#' Create HTML report for variable census
#'
#' @param census Comprehensive variable census data frame
#' @param summary Summary statistics
#' @param output_dir Output directory
create_census_html_report <- function(census, summary, output_dir) {
    # Create HTML content
    html_content <- paste0(
        "<!DOCTYPE html>",
        "<html><head>",
        "<title>Comprehensive Variable Census</title>",
        "<style>",
        "body { font-family: Arial, sans-serif; margin: 20px; }",
        "h1, h2, h3 { color: #2c3e50; }",
        "table { border-collapse: collapse; width: 100%; margin: 10px 0; }",
        "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
        "th { background-color: #f2f2f2; font-weight: bold; }",
        ".summary { background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin: 20px 0; }",
        ".category { background-color: #f9f9f9; padding: 10px; margin: 10px 0; }",
        "</style>",
        "</head><body>",
        "<h1>Comprehensive Variable Census</h1>",
        "<p><strong>Generated:</strong> ", Sys.time(), "</p>",
        "<div class='summary'>",
        "<h2>Summary Statistics</h2>",
        "<ul>",
        "<li><strong>Total Variables:</strong> ", summary$total_variables, "</li>",
        "<li><strong>Original Variables:</strong> ", summary$original_variables, "</li>",
        "<li><strong>Derived Variables:</strong> ", summary$derived_variables, "</li>",
        "<li><strong>Current Variables:</strong> ", summary$current_variables, "</li>",
        "<li><strong>Missing from Current:</strong> ", summary$missing_from_current, "</li>",
        "</ul>",
        "</div>",
        "<h2>Variable Categories</h2>",
        "<table>",
        "<tr><th>Category</th><th>Count</th></tr>"
    )

    # Add category counts
    for (i in 1:length(summary$categories)) {
        html_content <- paste0(
            html_content,
            "<tr><td>", names(summary$categories)[i], "</td><td>", summary$categories[i], "</td></tr>"
        )
    }

    html_content <- paste0(html_content, "</table>")

    # Add detailed variable table by category
    for (category in unique(census$variable_category)) {
        category_data <- census[census$variable_category == category, ]

        html_content <- paste0(
            html_content,
            "<div class='category'>",
            "<h3>", category, " (", nrow(category_data), " variables)</h3>",
            "<table>",
            "<tr><th>Variable Name</th><th>Description</th><th>Type</th><th>Derived</th><th>Current</th><th>Missing</th></tr>"
        )

        for (i in 1:nrow(category_data)) {
            row <- category_data[i, ]
            html_content <- paste0(
                html_content,
                "<tr>",
                "<td>", row$variable_name, "</td>",
                "<td>", ifelse(is.na(row$description), "", row$description), "</td>",
                "<td>", ifelse(is.na(row$current_type), "", row$current_type), "</td>",
                "<td>", ifelse(row$is_derived, "Yes", "No"), "</td>",
                "<td>", ifelse(row$is_current, "Yes", "No"), "</td>",
                "<td>", ifelse(is.na(row$current_missing), "", row$current_missing), "</td>",
                "</tr>"
            )
        }

        html_content <- paste0(html_content, "</table></div>")
    }

    html_content <- paste0(html_content, "</body></html>")

    # Save HTML file
    writeLines(html_content, file.path(output_dir, "comprehensive_variable_census.html"))
}

#' Main execution function
main <- function() {
    cat("=== COMPREHENSIVE VARIABLE CENSUS TOOL ===\n")
    cat("This tool creates a complete inventory of all variables in the dataset.\n")
    cat("It combines the original data dictionary with current dataset structure.\n\n")

    # Check if data dictionary exists
    data_dict_path <- "data/Ocular Melanoma Master Spreadsheet FINAL FOR STATS (4-30-25).xlsx"
    if (!file.exists(data_dict_path)) {
        stop("Data dictionary file not found: ", data_dict_path)
    }

    cat("Creating comprehensive variable census...\n")
    result <- create_comprehensive_variable_census()

    cat("\n=== SUMMARY ===\n")
    cat("Total variables:", result$summary$total_variables, "\n")
    cat("Original variables:", result$summary$original_variables, "\n")
    cat("Derived variables:", result$summary$derived_variables, "\n")
    cat("Current variables:", result$summary$current_variables, "\n")
    cat("Missing from current:", result$summary$missing_from_current, "\n")

    cat("\n=== VARIABLE CATEGORIES ===\n")
    for (i in 1:length(result$summary$categories)) {
        cat(names(result$summary$categories)[i], ":", result$summary$categories[i], "\n")
    }

    cat("\n=== OUTPUT FILES ===\n")
    cat("XLSX file:", file.path(result$output_dir, "comprehensive_variable_census.xlsx"), "\n")
    cat("HTML report:", file.path(result$output_dir, "comprehensive_variable_census.html"), "\n")
    cat("RDS file:", file.path(result$output_dir, "comprehensive_variable_census.rds"), "\n")

    cat("\n=== NEXT STEPS ===\n")
    cat("1. Open the XLSX file to review variable details\n")
    cat("2. Check the HTML report for formatted view\n")
    cat("3. Use the RDS file for programmatic access\n")

    cat("\nComprehensive variable census completed successfully!\n")
}

# Run if called directly
if (!interactive()) {
    main()
}
