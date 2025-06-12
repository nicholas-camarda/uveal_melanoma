# Test script to verify forestploter package works correctly
# Location: scripts/tests/test_forestploter_basic.R

cat("=== TESTING FORESTPLOTER PACKAGE ===\n")

# Test 1: Load required libraries (should be loaded in main.R)
cat("Test 1: Loading libraries...\n")
tryCatch({
    library(forestploter)
    library(grid)
    cat("✓ Libraries loaded successfully\n")
}, error = function(e) {
    cat("✗ Library loading failed:", conditionMessage(e), "\n")
})

# Test 2: Create basic forest plot data following forestploter documentation
cat("Test 2: Creating test data...\n")
tryCatch({
    # Create example data following forestploter documentation format
    dt <- data.frame(
        Study = c("Study A", "Study B", "Study C", "Overall"),
        estimate = c(1.5, 0.8, 1.2, 1.1),
        lower = c(1.1, 0.5, 0.9, 0.9), 
        upper = c(2.0, 1.1, 1.6, 1.4),
        stringsAsFactors = FALSE
    )
    
    # Add blank column for CI (this is crucial for forestploter)
    dt$` ` <- paste(rep(" ", 20), collapse = " ")
    
    # Add formatted CI column
    dt$`OR (95% CI)` <- sprintf("%.2f (%.2f-%.2f)", dt$estimate, dt$lower, dt$upper)
    
    cat("✓ Test data created successfully\n")
    print(head(dt))
}, error = function(e) {
    cat("✗ Data creation failed:", conditionMessage(e), "\n")
})

# Test 3: Create basic forest plot
cat("Test 3: Creating basic forest plot...\n")
tryCatch({
    # Create forest plot following exact forestploter syntax
    p <- forest(
        dt[, c("Study", " ", "OR (95% CI)")],  # Data frame with text and blank column
        est = dt$estimate,                      # Point estimates
        lower = dt$lower,                       # Lower CI bounds
        upper = dt$upper,                       # Upper CI bounds
        ci_column = 2,                          # Column number for CI (blank column)
        ref_line = 1,                           # Reference line at 1 (for OR)
        arrow_lab = c("Favours Control", "Favours Treatment"),
        xlim = c(0.3, 2.5),
        is_summary = c(FALSE, FALSE, FALSE, TRUE)  # Last row is summary
    )
    
    cat("✓ Forest plot created successfully\n")
    
    # Save the plot to test output directory
    test_output_dir <- "test_output"
    if (!dir.exists(test_output_dir)) {
        dir.create(test_output_dir, recursive = TRUE)
    }
    
    png(file.path(test_output_dir, "test_forestplot_basic.png"), width = 800, height = 600, res = 150)
    plot(p)
    dev.off()
    
    cat("✓ Forest plot saved to test_output/test_forestplot_basic.png\n")
    
}, error = function(e) {
    cat("✗ Forest plot creation failed:", conditionMessage(e), "\n")
})

# Test 4: Test with theme
cat("Test 4: Testing with custom theme...\n")
tryCatch({
    # Create theme
    tm <- forest_theme(
        base_size = 10,
        ci_pch = 15,
        ci_col = "#377eb8",
        ci_fill = "#377eb8",
        refline_gp = gpar(lwd = 1, lty = "dashed", col = "grey20")
    )
    
    # Create themed forest plot
    p_themed <- forest(
        dt[, c("Study", " ", "OR (95% CI)")],
        est = dt$estimate,
        lower = dt$lower,
        upper = dt$upper,
        ci_column = 2,
        ref_line = 1,
        theme = tm,
        title = "Test Forest Plot with Theme"
    )
    
    # Save themed plot
    png(file.path(test_output_dir, "test_forestplot_themed.png"), width = 800, height = 600, res = 150)
    plot(p_themed)
    dev.off()
    
    cat("✓ Themed forest plot created and saved to test_output/\n")
    
}, error = function(e) {
    cat("✗ Themed forest plot failed:", conditionMessage(e), "\n")
})

cat("=== FORESTPLOTER TESTING COMPLETE ===\n") 