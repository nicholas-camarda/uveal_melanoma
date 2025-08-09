context("Subgroup HTML formatting and footnotes")

library(gt)

test_that("Subgroup HTML has correct headers and footnotes", {
  # Minimal subgroup result with an 'Other' level and interaction p
  se <- data.frame(
    subgroup_level = factor(c("A", "Other"), levels = c("A", "Other")),
    n_total = c(10, 5),
    n_plaque = c(6, 2),
    n_gksrs = c(4, 3),
    treatment_effect = c(-0.5, -1.0),
    ci_lower = c(-1.0, -2.0),
    ci_upper = c(0.0, 0.1),
    p_value = c(0.06, 0.07),
    stringsAsFactors = FALSE
  )

  subgroup_results <- list(location = list(
    interaction_p = 0.11,
    subgroup_effects = se
  ))

  # Other map for 'location'
  other_map <- list(location = c("Ciliary Body", "Conjunctival", "Irido-Ciliary", "Iris"))

  # Output path in test output folder
  out_dir <- file.path("tests", "testthat", "test_output")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, "tmp_subgroup_format.html")

  # Generate table
  res <- format_subgroup_analysis_results(
    subgroup_results = subgroup_results,
    outcome_name = "Tumor Height Change - TEST",
    effect_measure = "MD",
    output_path = sub(".html$", ".xlsx", out_path),
    other_map = list(location = other_map$location)
  )

  # Read generated HTML
  expect_true(file.exists(out_path))
  html <- paste(readLines(out_path, warn = FALSE), collapse = "\n")

  # Header: first column should be 'Subgroup'
  expect_true(grepl(">Subgroup<", html))

  # Interaction p-value column should be present by label (could be 'Int p' based on config)
  expect_true(grepl("Int p|Interaction p-value", html))

  # Footnote contains CI definition and Other mapping; ensure both present and separated by a newline or <br>
  expect_true(grepl("CI = confidence interval", html))
  expect_true(grepl("Other for", html))
})
