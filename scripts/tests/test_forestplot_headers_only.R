# Simple test for corrected forest plot header formatting
# Based on forestploter documentation

cat("=== TESTING CORRECTED HEADER FORMATTING ===\n")

# Load required libraries
library(forestploter)
library(grid)

# Create test output directory
if (!dir.exists("test_output")) {
    dir.create("test_output", recursive = TRUE)
}

# Create simple test data with proper CI spacing column
test_data <- data.frame(
    Subgroup = c("Age at Diagnosis", "< 65 years", ">= 65 years"),
    GKSRS_n = c("", "45/95", "35/75"),
    Plaque_n = c("", "50/95", "40/75"),
    CI_space = c(paste(rep(" ", 20), collapse = " "),   # Blank for header row
                 paste(rep(" ", 20), collapse = " "),   # Blank for data row 1
                 paste(rep(" ", 20), collapse = " ")),  # Blank for data row 2
    Effect_CI = c("", "1.20 (0.90-1.60)", "0.80 (0.60-1.10)"),
    p_value = c("", "0.15", "0.25"),
    stringsAsFactors = FALSE
)

# The key fix: Set proper column names for forestploter headers
# This is what creates the headers according to forestploter documentation
colnames(test_data) <- c(
    "Subgroup", 
    "GKSRS n/N", 
    "Plaque n/N", 
    " ",  # Blank column for CI
    "HR (95% CI)", 
    "p value"
)

cat("✓ Test data created with proper column names\n")
cat("Column names:", paste(colnames(test_data), collapse = ", "), "\n")

# Create estimates - skip the header row
est_vals <- c(NA, 1.20, 0.80)
lower_vals <- c(NA, 0.90, 0.60) 
upper_vals <- c(NA, 1.60, 1.10)
is_sum <- c(TRUE, FALSE, FALSE)

cat("✓ Estimate values prepared\n")

# Create the forest plot with proper dimensions
tryCatch({
    p <- forest(test_data,
                est = est_vals,
                lower = lower_vals,
                upper = upper_vals,
                ci_column = 4,  # The blank column
                is_summary = is_sum,
                ref_line = 1,
                xlim = c(0.3, 2.5),
                arrow_lab = c("Favours GKSRS", "Favours Plaque"),
                title = "Test Forest Plot with Improved Formatting")
    
    cat("✓ Forest plot created successfully\n")
    
    # Save the plot with better dimensions
    png("test_output/test_header_formatting_fixed.png", width = 1000, height = 300, res = 150)
    plot(p)
    dev.off()
    
    cat("✓ Forest plot saved to test_output/test_header_formatting_fixed.png\n")
    
}, error = function(e) {
    cat("✗ Error creating forest plot:", conditionMessage(e), "\n")
})

cat("=== HEADER FORMATTING TEST COMPLETE ===\n") 