# GEP Core Metrics and Diagnostics

# Robustly locate project root from test directory
find_project_root <- function() {
  candidates <- c(
    ".",
    "..",
    "../..",
    "../../..",
    "../../../.."
  )
  for (root in candidates) {
    target <- file.path(root, "scripts/utils/all_helper_functions.R")
    if (file.exists(target)) return(normalizePath(root))
  }
  stop("Could not locate project root for sourcing helper scripts")
}

project_root <- find_project_root()

# Load all project libraries and scripts via a single entrypoint
suppressWarnings(suppressMessages({
  oldwd <- getwd(); setwd(project_root); on.exit(setwd(oldwd), add = TRUE)
  source("scripts/utils/all_helper_functions.R")
}))

# Create synthetic dataset with sufficient events
set.seed(123)
n_patients <- 150
syn_data <- data.frame(
  biopsy1_gep = sample(c("Class 1A", "Class 1B", "Class 2"), n_patients, replace = TRUE),
  biopsy1_gep_mfs = runif(n_patients, 0.6, 0.95),
  biopsy1_gep_mss = runif(n_patients, 0.7, 0.98),
  gep_class_simple = sample(c("Class 1A", "Class 1B", "Class 2"), n_patients, replace = TRUE),
  prame_status = sample(c("Positive", "Negative"), n_patients, replace = TRUE),
  tt_mets_months = rexp(n_patients, rate = 0.05) * 12,
  mets_event = rbinom(n_patients, 1, prob = 0.3),
  tt_death_months = rexp(n_patients, rate = 0.04) * 12,
  death_event = rbinom(n_patients, 1, prob = 0.25),
  stringsAsFactors = FALSE
)

# Add expected survival columns at 5/7/10 years based on biopsy probabilities
syn_data$expected_mfs_5yr <- pmax(pmin(syn_data$biopsy1_gep_mfs, 0.999), 0.001)
syn_data$expected_mfs_7yr <- pmax(pmin(syn_data$expected_mfs_5yr^(7/5), 0.999), 0.001)
syn_data$expected_mfs_10yr <- pmax(pmin(syn_data$expected_mfs_5yr^(10/5), 0.999), 0.001)

syn_data$expected_mss_5yr <- pmax(pmin(syn_data$biopsy1_gep_mss, 0.999), 0.001)
syn_data$expected_mss_7yr <- pmax(pmin(syn_data$expected_mss_5yr^(7/5), 0.999), 0.001)
syn_data$expected_mss_10yr <- pmax(pmin(syn_data$expected_mss_5yr^(10/5), 0.999), 0.001)

# Basic cause of death proxy for MSS competing risk prep
syn_data$cause_of_death <- ifelse(syn_data$death_event == 1,
  sample(c("Melanoma", "Other", "Cardiac"), sum(syn_data$death_event == 1), replace = TRUE),
  NA
)


test_that("Observed/Expected MFS returns sensible structure at 5 years", {
  res <- calculate_observed_expected_mfs(syn_data, timepoint = 5)
  expect_type(res, "list")
  expect_true(all(c("results_by_class", "overall_observed", "overall_expected", "overall_oe_ratio") %in% names(res)))
  expect_true(is.numeric(res$overall_expected) || is.na(res$overall_expected))
})


test_that("Calibration metrics for MFS are present with sufficient data", {
  cal <- perform_calibration_mfs(syn_data, timepoint = 5, bootstrap_iterations = 50)
  expect_true(all(c("nam_dagostino_p", "ici", "calibration_slope") %in% names(cal)))
})


test_that("Discrimination metrics for MFS include C-index and AUC when feasible", {
  disc <- perform_discrimination_mfs(syn_data, timepoint = 5)
  expect_true(all(c("harrell_c", "uno_c", "auc_timepoint") %in% names(disc)))
})


test_that("MSS standard and competing risk preparation returns expected fields", {
  prep <- prepare_mss_competing_risk_data(syn_data)
  expect_true(all(c("melanoma_death_event", "competing_death_event", "tt_death_years") %in% names(prep)))

  std <- perform_standard_mss_validation(prep, timepoint = 5, bootstrap_iterations = 10)
  expect_true(all(c("observed_expected", "calibration", "discrimination", "decision_curve") %in% names(std)))

  comp <- perform_competing_risk_mss_validation(prep, timepoint = 5)
  expect_true(all(c("cumulative_incidence", "cause_specific_hazards") %in% names(comp)))
})


test_that("GEP variable validation flags missing variables and passes with complete set", {
  # Missing one required variable
  bad <- syn_data
  bad$biopsy1_gep_mfs <- NULL
  v1 <- validate_gep_variables_with_report(bad)
  expect_false(v1$validation_passed)

  v2 <- validate_gep_variables_with_report(syn_data)
  expect_type(v2, "list")
  expect_true("detailed_results" %in% names(v2))
})

# Additional tests for MSS time-dependent discrimination and DCA

test_that("MSS time-dependent discrimination returns expected fields", {
  prep <- prepare_mss_competing_risk_data(syn_data)
  # Build timepoint-specific analysis data analogous to perform_standard_mss_validation internals
  analysis_data <- prep %>%
    dplyr::mutate(
      time_to_event = pmin(tt_death_years, 5),
      event_occurred = melanoma_death_event & (tt_death_years <= 5)
    )
  disc <- perform_discrimination_mss(analysis_data, timepoint = 5)
  expect_true(all(c("harrell_c", "uno_c", "auc_timepoint") %in% names(disc)))
})


test_that("MSS decision curve analysis returns curve data when feasible", {
  prep <- prepare_mss_competing_risk_data(syn_data)
  analysis_data <- prep %>%
    dplyr::mutate(
      time_to_event = pmin(tt_death_years, 5),
      event_occurred = melanoma_death_event & (tt_death_years <= 5)
    )
  dca <- perform_decision_curve_analysis_mss(analysis_data, timepoint = 5)
  expect_true(all(c("n", "dca_curve_data") %in% names(dca)))
})

# Deterministic datasets meeting sample-size thresholds

make_mfs_deterministic <- function(n_per_class = 20) {
  stopifnot(n_per_class >= 10)
  classes <- c("Class 1A", "Class 1B", "Class 2")
  exp_by_class <- c("Class 1A" = 0.95, "Class 1B" = 0.85, "Class 2" = 0.60)
  df <- do.call(rbind, lapply(classes, function(cls) {
    tibble::tibble(
      gep_class_simple = cls,
      expected_mfs_5yr = exp_by_class[[cls]],
      # Assign events within 5 years: 0 in 1A, 30% in 1B, 60% in 2
      mets_event = {
        if (cls == "Class 1A") rep(0L, n_per_class)
        else if (cls == "Class 1B") c(rep(1L, round(0.30 * n_per_class)), rep(0L, n_per_class - round(0.30 * n_per_class)))
        else c(rep(1L, round(0.60 * n_per_class)), rep(0L, n_per_class - round(0.60 * n_per_class)))
      },
      tt_mets_months = {
        # Events at 12-48 months, non-events censored after 60 months
        ev <- ifelse(mets_event == 1L, sample(c(12, 24, 36, 48), size = n_per_class, replace = TRUE), 999)
        as.numeric(ev)
      }
    )
  }))
  df
}

make_mss_deterministic <- function(n_per_class = 20) {
  stopifnot(n_per_class >= 10)
  classes <- c("Class 1A", "Class 1B", "Class 2")
  exp_by_class <- c("Class 1A" = 0.97, "Class 1B" = 0.90, "Class 2" = 0.70)
  df <- do.call(rbind, lapply(classes, function(cls) {
    ev_rate <- if (cls == "Class 1A") 0.00 else if (cls == "Class 1B") 0.20 else 0.50
    ev_vec <- c(rep(1L, round(ev_rate * n_per_class)), rep(0L, n_per_class - round(ev_rate * n_per_class)))
    tibble::tibble(
      gep_class_simple = cls,
      biopsy1_gep = "Valid",                 # required for filter
      biopsy1_gep_mss = exp_by_class[[cls]],  # predicted survival (5y)
      expected_mss_5yr = exp_by_class[[cls]], # explicit expected var used downstream
      tt_death_months = ifelse(ev_vec == 1L, sample(c(12, 24, 36, 48), size = n_per_class, replace = TRUE), 999),
      death_event = ev_vec,
      cause_of_death = ifelse(ev_vec == 1L, "melanoma", NA_character_)
    )
  }))
  df
}


test_that("MFS deterministic dataset yields expected O/E and positive DCA", {
  toy <- make_mfs_deterministic(n_per_class = 20)
  oe <- calculate_observed_expected_mfs(toy, timepoint = 5)
  # Expected events totals: 20*(1-0.95)=1, 20*(1-0.85)=3, 20*(1-0.60)=8 -> 12
  # Observed totals: 0 + 6 + 12 = 18
  expect_equal(oe$overall_expected, 12.00, tolerance = 1e-8)
  expect_equal(oe$overall_observed, 18)
  expect_equal(oe$overall_oe_ratio, 1.5, tolerance = 1e-3)

  disc <- perform_discrimination_mfs(toy, timepoint = 5)
  # If computed, Harrell's C should be reasonably high given clear risk ordering
  if (!is.na(disc$harrell_c)) expect_gt(disc$harrell_c, 0.7)

  dca <- perform_decision_curve_analysis_mfs(toy, timepoint = 5)
  # Expect at least one positive net benefit across thresholds
  expect_true(any(dca$dca_curve_data$net_benefit_model > 0, na.rm = TRUE))
})


test_that("MSS deterministic dataset yields expected class O/E and positive DCA", {
  raw <- make_mss_deterministic(n_per_class = 20)
  prep <- prepare_mss_competing_risk_data(raw)
  std <- perform_standard_mss_validation(prep, timepoint = 5, bootstrap_iterations = 0)

  # Check expected_rate equals 1 - expected_mss_5yr per class
  oe_df <- std$observed_expected
  row_by_class <- function(cls) oe_df[oe_df$gep_class_simple == cls, , drop = FALSE]
  expect_equal(as.numeric(row_by_class("Class 1A")$expected_rate), 1 - 0.97, tolerance = 1e-8)
  expect_equal(as.numeric(row_by_class("Class 1B")$expected_rate), 1 - 0.90, tolerance = 1e-8)
  expect_equal(as.numeric(row_by_class("Class 2")$expected_rate), 1 - 0.70, tolerance = 1e-8)

  # Discrimination should be good if computable
  analysis_data <- prep %>%
    dplyr::mutate(
      time_to_event = pmin(tt_death_years, 5),
      event_occurred = melanoma_death_event & (tt_death_years <= 5)
    )
  disc <- perform_discrimination_mss(analysis_data, timepoint = 5)
  if (!is.na(disc$harrell_c)) expect_gt(disc$harrell_c, 0.7)

  # DCA should show positive NB for some thresholds
  dca <- perform_decision_curve_analysis_mss(analysis_data, timepoint = 5)
  expect_true(any(dca$dca_curve_data$net_benefit_model > 0, na.rm = TRUE))
})
