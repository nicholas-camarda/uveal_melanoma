# Debug Factor Level Detection
# Let's examine the exact table structure to understand the issue

library(tidyverse)
library(gtsummary)
library(gt)

# Load test data
cat("Loading test data...\n")
full_data <- readRDS("final_data/Analytic Dataset/uveal_melanoma_full_cohort.rds")

test_data <- full_data %>%
    slice_head(n = 50) %>%
    select(treatment_group, age_at_diagnosis, sex, location, initial_tumor_height, biopsy1_gep) %>%
    filter(!is.na(sex), !is.na(location))

# Create baseline table
baseline_table <- test_data %>%
    tbl_summary(
        by = treatment_group,
        missing = "no"
    ) %>%
    add_overall() %>%
    add_p()

# Convert to tibble to examine structure
table_data <- baseline_table %>% as_tibble()

cat("=== TABLE STRUCTURE ANALYSIS ===\n")
cat("Number of rows:", nrow(table_data), "\n")
cat("Number of columns:", ncol(table_data), "\n")
cat("Column names:", paste(names(table_data), collapse = ", "), "\n\n")

# Show the actual data
cat("=== RAW TABLE DATA ===\n")
for (i in 1:nrow(table_data)) {
    cat(sprintf("Row %d: %s | %s | %s | %s | %s\n", 
                i, 
                table_data[[1]][i],
                table_data[[2]][i], 
                table_data[[3]][i],
                table_data[[4]][i],
                table_data[[5]][i]))
}

# Find data columns
col_names <- names(table_data)
total_cols <- ncol(table_data)
pvalue_col <- which(grepl("p.?value", col_names, ignore.case = TRUE))

if (length(pvalue_col) > 0) {
    data_cols <- setdiff(2:total_cols, pvalue_col)
} else {
    data_cols <- 2:(total_cols - 1)
}

cat("\n=== DETECTION ANALYSIS ===\n")
cat("Data columns:", paste(data_cols, collapse = ", "), "\n")

# Analyze each row
for (i in 1:nrow(table_data)) {
    data_row <- table_data[i, data_cols]
    all_na <- all(trimws(as.character(data_row)) == "NA", na.rm = TRUE)
    var_name <- table_data[[1]][i]
    
    cat(sprintf("Row %d (%s): data = [%s] -> all_na = %s\n", 
                i, 
                var_name,
                paste(as.character(data_row), collapse = ", "),
                all_na))
} 