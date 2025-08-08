# Set working directory to project root for consistent path handling
setwd(dirname(dirname(normalizePath("."))))

library(testthat)
library(dplyr)

source("scripts/utils/all_helper_functions.R")

test_that("Forced 'Other' mapping is applied centrally and recorded in other_map", {
  expect_true("initial_overall_stage_modified" %in% names(FORCED_OTHER_BY_VARIABLE))
  forced <- FORCED_OTHER_BY_VARIABLE[["initial_overall_stage_modified"]]
  expect_true(all(c("3B","3C","4") %in% forced))
  
  df <- tibble::tibble(
    initial_overall_stage_modified = factor(c("1", "2A", "2B", "3A", "3B", "3C", "4"),
                                   levels = c("1", "2A", "2B", "3A", "3B", "3C", "4"))
  )
  
  res <- handle_rare_categories(df, vars = c("initial_overall_stage_modified"), threshold = THRESHOLD_RARITY)
  df2 <- res$data
  omap <- res$other_map
  
  # Check that excluded stages mapped to "Other"
  expect_true("Other" %in% levels(df2$initial_overall_stage_modified))
  expect_equal(as.character(df2$initial_overall_stage_modified[5:7]), rep("Other", 3))
  
  # Check other_map recorded forced collapse
  expect_true("initial_overall_stage_modified" %in% names(omap))
  expect_true(all(c("3B","3C","4") %in% omap[["initial_overall_stage_modified"]]))
})
