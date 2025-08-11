# Integration test: Run Objective 4 end-to-end and verify outputs

# Helper to locate project root
find_project_root <- function() {
  candidates <- c(".", "..", "../..", "../../..", "../../../..")
  for (root in candidates) {
    if (file.exists(file.path(root, "scripts/utils/all_helper_functions.R"))) {
      return(normalizePath(root))
    }
  }
  stop("Project root not found")
}

project_root <- find_project_root()

suppressWarnings(suppressMessages({
  oldwd <- getwd()
  setwd(project_root)
  on.exit(setwd(oldwd), add = TRUE)
  source("scripts/utils/all_helper_functions.R")
}))

# Build synthetic processed dataset compliant with Objective 4
make_processed_dataset <- function(n_per_class = 40) {
  set.seed(42)
  classes <- c("Class 1A", "Class 1B", "Class 2")
  exp_mfs <- c("Class 1A" = 0.96, "Class 1B" = 0.88, "Class 2" = 0.65)
  exp_mss <- c("Class 1A" = 0.98, "Class 1B" = 0.92, "Class 2" = 0.75)
  prame_levels <- c("Negative", "Positive")
  val_sets <- c("Training", "Testing")

  df <- do.call(rbind, lapply(classes, function(cls) {
    n <- n_per_class
    tibble::tibble(
      id = seq_len(n),
      gep_class_simple = cls,
      biopsy1_gep = "Valid",
      gep_validation_set = sample(val_sets, size = n, replace = TRUE, prob = c(0.7, 0.3)),
      prame_status = sample(prame_levels, size = n, replace = TRUE),
      biopsy1_gep_mfs = exp_mfs[[cls]],
      biopsy1_gep_mss = exp_mss[[cls]],
      expected_mfs_5yr = exp_mfs[[cls]],
      expected_mfs_7yr = exp_mfs[[cls]]^(7 / 5),
      expected_mfs_10yr = exp_mfs[[cls]]^(10 / 5),
      expected_mss_5yr = exp_mss[[cls]],
      expected_mss_7yr = exp_mss[[cls]]^(7 / 5),
      expected_mss_10yr = exp_mss[[cls]]^(10 / 5),
      # MFS outcome
      mets_event = if (cls == "Class 1A") rbinom(n, 1, 0.05) else if (cls == "Class 1B") rbinom(n, 1, 0.20) else rbinom(n, 1, 0.40),
      tt_mets_months = ifelse(mets_event == 1, sample(c(12, 24, 36, 48), size = n, replace = TRUE), 999),
      # MSS outcome (melanoma death)
      death_event = if (cls == "Class 1A") rbinom(n, 1, 0.02) else if (cls == "Class 1B") rbinom(n, 1, 0.10) else rbinom(n, 1, 0.25),
      tt_death_months = ifelse(death_event == 1, sample(c(12, 24, 36, 48), size = n, replace = TRUE), 999),
      cause_of_death = ifelse(death_event == 1, "melanoma", NA_character_),
      # minimal confounders to satisfy any generic expectations
      age_at_diagnosis = sample(40:85, size = n, replace = TRUE),
      sex = sample(c("Female", "Male"), size = n, replace = TRUE),
      location = sample(c("Choroid", "Ciliary Body"), size = n, replace = TRUE)
    )
  }))
  df
}

# Ensure processed inputs exist
prepare_test_inputs <- function(dataset_name) {
  dir.create(PROCESSED_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
  data_path <- file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds"))
  other_map_path <- file.path(PROCESSED_DATA_DIR, "other_map.rds")

  # Clean any prior outputs for a fresh run
  cohort_dirs <- c("uveal_full", "uveal_restricted", "gksrs")
  for (cd in cohort_dirs) {
    unlink(file.path(OUTPUT_DIR, cd, "04_GEP_Validation"), recursive = TRUE, force = TRUE)
  }

  df <- make_processed_dataset(40)
  saveRDS(df, data_path)
  # Minimal other_map structure
  saveRDS(list(treatment_group_map = c("Plaque" = "Plaque", "GKSRS" = "GKSRS")), other_map_path)
}


test_that("Objective 4 pipeline runs and writes key artifacts", {
  dataset <- "uveal_melanoma_full_cohort"
  prepare_test_inputs(dataset)

  # Run only objective 4 on synthetic dataset
  invisible(with_log_context(cohort = dataset, objective = "objective_4_gep_analysis", subobjective = NULL, expr = {
    run_specific_objective(dataset, 4)
  }))

  # Verify key outputs exist
  base_dir <- file.path(OUTPUT_DIR, "uveal_full", "04_GEP_Validation")
  mfs_dir <- file.path(base_dir, "a_metastasis_free_survival")
  mss_dir <- file.path(base_dir, "b_melanoma_specific_survival")
  unified_dir <- file.path(base_dir, "unified_summary")

  expect_true(dir.exists(mfs_dir))
  expect_true(dir.exists(mss_dir))

  # MFS artifacts
  expect_true(file.exists(file.path(mfs_dir, paste0(prefix, "mfs_validation_summary.txt"))))
  expect_true(file.exists(file.path(mfs_dir, paste0(prefix, "mfs_validation_summary.xlsx"))))

  # MSS artifacts
  expect_true(file.exists(file.path(mss_dir, paste0(prefix, "mss_standard_validation_results.rds"))))
  expect_true(file.exists(file.path(mss_dir, paste0(prefix, "mss_validation_summary.xlsx"))))
  expect_true(file.exists(file.path(mss_dir, paste0(prefix, "mss_validation_summary.txt"))))

  # Unified summary dir (combined plots/report)
  expect_true(dir.exists(unified_dir))

  # Unified comprehensive report should list MSS timepoints
  unified_report <- file.path(unified_dir, paste0(prefix, "gep_comprehensive_report.txt"))
  expect_true(file.exists(unified_report))
  report_text <- paste(readLines(unified_report, warn = FALSE), collapse = "\n")
  expect_true(grepl("MSS Validation:", report_text, fixed = TRUE))
  expect_true(any(grepl("5yr", report_text, fixed = TRUE)))

  # Unified comparison table should exist and contain both outcomes and all timepoints
  comparison_path <- file.path(unified_dir, paste0(prefix, "gep_comparison_table.xlsx"))
  expect_true(file.exists(comparison_path))
  comp_df <- suppressMessages(readxl::read_xlsx(comparison_path))
  expect_true(all(c("MFS", "MSS") %in% unique(comp_df$outcome)))
  expect_true(all(c("5yr", "7yr", "10yr") %in% unique(comp_df$timepoint)))

  # MSS workbook structure and CIF with CI
  mss_xlsx <- file.path(mss_dir, paste0(prefix, "mss_validation_summary.xlsx"))
  expect_true(file.exists(mss_xlsx))
  mss_sheets <- readxl::excel_sheets(mss_xlsx)
  expect_true(all(c(
    "Observed_Expected_by_class",
    "Calibration",
    "Discrimination",
    "Counts",
    "CompetingRisk_CIF_with_CI"
  ) %in% mss_sheets))
  cif_df <- suppressMessages(readxl::read_xlsx(mss_xlsx, sheet = "CompetingRisk_CIF_with_CI"))
  expect_gt(nrow(cif_df), 0)
  expect_true(any(!is.na(cif_df$CIF)))
  expect_true(any(!is.na(cif_df$CI_Lower)) | any(!is.na(cif_df$CI_Upper)))

  # MFS workbook should include Overall rows per timepoint in Observed_Expected_by_class
  mfs_xlsx <- file.path(mfs_dir, paste0(prefix, "mfs_validation_summary.xlsx"))
  expect_true(file.exists(mfs_xlsx))
  mfs_oe <- suppressMessages(readxl::read_xlsx(mfs_xlsx, sheet = "Observed_Expected_by_class"))
  for (tp in c("5yr", "7yr", "10yr")) {
    has_overall <- any(mfs_oe$Timepoint == tp & mfs_oe$GEP_Class == "Overall")
    expect_true(has_overall)
  }
})
