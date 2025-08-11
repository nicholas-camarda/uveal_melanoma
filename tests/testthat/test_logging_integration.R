find_project_root <- function() {
  candidates <- c(".", "..", "../..", "../../..", "../../../..")
  for (root in candidates) {
    if (file.exists(file.path(root, "scripts/utils/all_helper_functions.R"))) return(normalizePath(root))
  }
  stop("Project root not found")
}

project_root <- find_project_root()

suppressWarnings(suppressMessages({
  oldwd <- getwd(); setwd(project_root); on.exit(setwd(oldwd), add = TRUE)
  source("scripts/utils/all_helper_functions.R")
}))

# Synthetic processed dataset (reuse from objective4 integration test)
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
      expected_mfs_7yr = exp_mfs[[cls]]^(7/5),
      expected_mfs_10yr = exp_mfs[[cls]]^(10/5),
      expected_mss_5yr = exp_mss[[cls]],
      expected_mss_7yr = exp_mss[[cls]]^(7/5),
      expected_mss_10yr = exp_mss[[cls]]^(10/5),
      mets_event = rbinom(n, 1, if (cls == "Class 1A") 0.05 else if (cls == "Class 1B") 0.20 else 0.40),
      tt_mets_months = ifelse(mets_event == 1, sample(c(12,24,36,48), size = n, replace = TRUE), 999),
      death_event = rbinom(n, 1, if (cls == "Class 1A") 0.02 else if (cls == "Class 1B") 0.10 else 0.25),
      tt_death_months = ifelse(death_event == 1, sample(c(12,24,36,48), size = n, replace = TRUE), 999),
      cause_of_death = ifelse(death_event == 1, "melanoma", NA_character_),
      age_at_diagnosis = sample(40:85, size = n, replace = TRUE),
      sex = sample(c("Female", "Male"), size = n, replace = TRUE),
      location = sample(c("Choroid", "Ciliary Body"), size = n, replace = TRUE)
    )
  }))
  df
}

prepare_test_inputs <- function(dataset_name) {
  dir.create(PROCESSED_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
  data_path <- file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds"))
  other_map_path <- file.path(PROCESSED_DATA_DIR, "other_map.rds")
  df <- make_processed_dataset(40)
  saveRDS(df, data_path)
  saveRDS(list(treatment_group_map = c("Plaque" = "Plaque", "GKSRS" = "GKSRS")), other_map_path)
}

parse_nonempty_json_lines <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  jsonlite::stream_in(textConnection(paste(lines, collapse = "\n")), verbose = FALSE)
}

test_that("Objective 4 logs produce valid JSON entries with context and idempotent setup", {
  dataset <- "uveal_melanoma_full_cohort"
  prepare_test_inputs(dataset)

  log_file <- tempfile(fileext = ".log")
  setup_logging(log_path = log_file, level = "INFO", progress = FALSE, context_in_file = TRUE)
  invisible(with_log_context(cohort = dataset, objective = "objective_4_gep_analysis", subobjective = NULL, expr = {
    run_specific_objective(dataset, 4)
  }))

  # Re-initialize to test idempotence
  setup_logging(log_path = log_file, level = "INFO", progress = FALSE, context_in_file = TRUE)
  logger::log_info("Idempotence check")

  expect_true(file.exists(log_file))
  df <- parse_nonempty_json_lines(log_file)
  # Basic structure checks
  expect_true(all(c("timestamp", "level_text", "level_num", "message", "cohort", "objective", "subobjective") %in% names(df)))
  expect_true(any(grepl("GEP MSS validation analysis completed successfully|GEP MFS validation analysis completed successfully", df$message)))
  expect_true(any(df$cohort == dataset))
  expect_true(any(df$objective == "objective_4_gep_analysis"))
})
