# Test following exact forestploter documentation pattern
# Based on https://cran.r-project.org/web/packages/forestploter/forestploter.pdf

cat("=== TESTING FORESTPLOTER FOLLOWING EXACT DOCUMENTATION ===\n")

library(forestploter)
library(grid)

# Create test output directory
if (!dir.exists("test_output")) {
    dir.create("test_output", recursive = TRUE)
}

# Following the exact documentation pattern
dt <- data.frame(
  Subgroup = c("Age at Diagnosis", "< 65 years", ">= 65 years"),
  GKSRS_n = c("GKSRS n/N", "45/95", "35/75"),
  Plaque_n = c("Plaque n/N", "50/95", "40/75"),
  stringsAsFactors = FALSE
)

# Add blank column for CI (this is crucial for forestploter)
dt$` ` <- paste(rep(" ", 20), collapse = " ")

# Add formatted CI column
dt$`HR (95% CI)` <- c("HR (95% CI)", "1.20 (0.90-1.60)", "0.80 (0.60-1.10)")

# Add p-value column
dt$`p value` <- c("p value", "0.15", "0.25")

cat("✓ Data structure created following documentation pattern\n")
print(dt)

# Create estimates (excluding header row)
est_vals <- c(NA, 1.20, 0.80)
lower_vals <- c(NA, 0.90, 0.60) 
upper_vals <- c(NA, 1.60, 1.10)
is_sum <- c(TRUE, FALSE, FALSE)  # First row is header

# Create forest plot following exact forestploter syntax
p <- forest(
  dt,  # Use the entire data frame
  est = est_vals,
  lower = lower_vals,
  upper = upper_vals,
  ci_column = 4,  # The blank column position
  ref_line = 1,
  arrow_lab = c("Favours GKSRS", "Favours Plaque"),
  xlim = c(0.3, 2.5),
  is_summary = is_sum,
  title = "Forest Plot Following Documentation Pattern"
)

# Save the plot
png("test_output/test_forestplot_documentation_pattern.png", width = 1000, height = 300, res = 150)
plot(p)
dev.off()

cat("✓ Forest plot saved following exact documentation pattern\n")
cat("=== DOCUMENTATION PATTERN TEST COMPLETE ===\n") 