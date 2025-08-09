context("Forest diagnostics column set")

test_that("Diagnostics dataframe excludes interaction_p column", {
  # Minimal subgroup_results shape for diagnostics
  se <- data.frame(
    subgroup_level = factor(c("X","Y")),
    n_total = c(10, 12), n_plaque = c(6, 7), n_gksrs = c(4, 5),
    treatment_effect = c(0.8, 1.2), ci_lower = c(0.5, 0.9), ci_upper = c(1.1, 1.6), p_value = c(0.04, 0.07)
  )
  subgroup_results <- list(foo = list(interaction_p = 0.2, subgroup_effects = se))
  diag <- create_forest_plot_diagnostics(subgroup_results)
  expect_false("interaction_p" %in% names(diag))
})
