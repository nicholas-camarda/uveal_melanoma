context("Source-level Other mapping for modified stage")

test_that("3B/3C/4 are collapsed to Other and tracked in other_map", {
  # Minimal dataframe with the modified stage present
  df <- data.frame(
    initial_overall_stage_modified = factor(
      c("1","2A","2B","3A","3B","3C","4"),
      levels = c("1","2A","2B","3A","3B","3C","4")
    )
  )

  # Apply centralized rare/forced collapsing
  collapsed <- handle_rare_categories(df, vars = c("initial_overall_stage_modified"), threshold = 5)

  data2 <- collapsed$data
  omap <- collapsed$other_map

  # Expect 'Other' level exists for modified stage
  expect_true("Other" %in% levels(data2$initial_overall_stage_modified))
  # Expect forced mapping tracked
  expect_true("initial_overall_stage_modified" %in% names(omap))
  expect_true(all(c("3B","3C","4") %in% omap$initial_overall_stage_modified))
})
