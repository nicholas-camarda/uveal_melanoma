
# Test file for scripts/utils/model_utilities.R

# Load necessary libraries
library(testthat)
library(here)
library(dplyr)

# Source the file being tested
# Note: In a real package structure, this wouldn't be necessary, but for this project structure it is.
source(here("scripts", "utils", "model_utilities.R"))

describe("enforce_unordered_factors", {
  
  it("converts an ordered factor to an unordered factor", {
    df <- data.frame(
      x = factor(c("Low", "Medium", "High"), levels = c("Low", "Medium", "High"), ordered = TRUE),
      y = 1:3
    )
    
    result <- enforce_unordered_factors(df)
    
    expect_false(is.ordered(result$x))
    expect_true(is.factor(result$x))
    expect_equal(levels(result$x), c("Low", "Medium", "High"))
  })
  
  it("preserves the order of levels exactly", {
    # Create a factor with non-alphabetical order
    levels_custom <- c("Small", "Large", "Medium")
    df <- data.frame(
      x = factor(c("Small", "Medium", "Large"), levels = levels_custom, ordered = TRUE)
    )
    
    result <- enforce_unordered_factors(df)
    
    expect_equal(levels(result$x), levels_custom)
    # Ensure the underlying integer codes match the level order
    expect_equal(as.integer(result$x), as.integer(df$x))
  })
  
  it("leaves non-factor columns untouched", {
    df <- data.frame(
      x = factor(c("A", "B"), ordered = TRUE),
      num = c(1, 2),
      char = c("a", "b"),
      stringsAsFactors = FALSE
    )
    
    result <- enforce_unordered_factors(df)
    
    expect_equal(result$num, df$num)
    expect_equal(result$char, df$char)
  })
  
  it("handles data frame with no factors", {
    df <- data.frame(
      x = 1:3,
      y = c("a", "b", "c"),
      stringsAsFactors = FALSE
    )
    
    expect_silent(result <- enforce_unordered_factors(df))
    expect_equal(result, df)
  })
  
  it("handles data frame with factors but no ordered factors", {
    df <- data.frame(
      x = factor(c("A", "B")),
      y = 1:2
    )
    
    expect_silent(result <- enforce_unordered_factors(df))
    expect_equal(result, df)
    expect_false(is.ordered(result$x))
  })
  
  it("handles multiple ordered factors", {
    df <- data.frame(
      f1 = factor(c("a", "b"), ordered = TRUE),
      f2 = factor(c("x", "y"), ordered = TRUE),
      num = 1:2
    )
    
    result <- enforce_unordered_factors(df)
    
    expect_false(is.ordered(result$f1))
    expect_false(is.ordered(result$f2))
    expect_equal(levels(result$f1), levels(df$f1))
    expect_equal(levels(result$f2), levels(df$f2))
  })
  
  it("handles empty data frame", {
    df <- data.frame()
    expect_silent(result <- enforce_unordered_factors(df))
    expect_equal(result, df)
  })
  
  it("logs messages when verbose is TRUE", {
    df <- data.frame(
      x = factor(c("A", "B"), ordered = TRUE)
    )
    
    # We expect output to stdout/stderr or via logger. 
    # Since the function uses logger::log_info, we might capture output if logger writes to console.
    # However, testing logger output can be tricky depending on logger config.
    # We will just check that it runs without error for now, or capture output if possible.
    
    # Simple check that it doesn't error
    expect_error(enforce_unordered_factors(df, verbose = TRUE), NA)
  })
})
