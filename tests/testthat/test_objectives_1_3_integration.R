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

# Synthetic processed dataset similar to Objective 4 test
make_processed_dataset_o13 <- function(n_per_class = 40) {
  set.seed(123)
  classes <- c("Class 1A", "Class 1B", "Class 2")
  exp_mfs <- c("Class 1A" = 0.96, "Class 1B" = 0.88, "Class 2" = 0.65)
  df <- do.call(rbind, lapply(classes, function(cls) {
    n <- n_per_class
    tibble::tibble(
      id = seq_len(n),
      gep_class_simple = cls,
      biopsy1_gep = "Valid",
      prame_status = sample(c("Negative", "Positive"), size = n, replace = TRUE),
      treatment_group = sample(c("Plaque", "GKSRS"), size = n, replace = TRUE),
      expected_mfs_5yr = exp_mfs[[cls]],
      # Binary outcomes (Obj1a/1b)
      recurrence1 = sample(c("Y","N"), size = n, replace = TRUE, prob = c(0.2,0.8)),
      tt_recurrence_months = ifelse(recurrence1 == "Y", sample(c(6,12,18,24,36), size = n, replace = TRUE), sample(c(999, 60, 72), size = n, replace = TRUE)),
      recurrence_event = ifelse(recurrence1 == "Y", 1L, 0L),
      mets_event = rbinom(n, 1, if (cls == "Class 1A") 0.05 else if (cls == "Class 1B") 0.20 else 0.40),
      tt_mets_months = ifelse(mets_event == 1, sample(c(12,24,36,48), size = n, replace = TRUE), 999),
      # Survival (OS/PFS)
      death_event = rbinom(n, 1, if (cls == "Class 1A") 0.02 else if (cls == "Class 1B") 0.10 else 0.25),
      tt_death_months = ifelse(death_event == 1, sample(c(12,24,36,48), size = n, replace = TRUE), 999),
      pfs_event = as.integer(mets_event == 1 | death_event == 1),
      tt_pfs_months = pmin(tt_mets_months, tt_death_months),
      # Vision analysis (Obj2a)
      initial_vision = rnorm(n, 0.4, 0.2),
      last_vision = pmax(0, initial_vision + rnorm(n, 0, 0.1)),
      recurrence1_pretreatment_vision = pmax(0, initial_vision + rnorm(n, 0, 0.1)),
      # Tumor height (Obj1e)
      initial_tumor_height = runif(n, 2, 10),
      height_change = rnorm(n, mean = ifelse(treatment_group == "GKSRS", -1.2, -0.8), sd = 0.6),
      # Safety/Toxicity (Obj2b)
      retinopathy = sample(c("Y","N"), size = n, replace = TRUE, prob = c(0.15, 0.85)),
      nvg = sample(c("Y","N"), size = n, replace = TRUE, prob = c(0.1, 0.9)),
      srd = sample(c("Y","N"), size = n, replace = TRUE, prob = c(0.1, 0.9)),
      srd_cause = ifelse(srd == "Y", sample(c("Radiation","Mass"), size = n, replace = TRUE, prob = c(0.7,0.3)), NA_character_),
      # PFS-2 minimal vars (Obj3)
      tt_pfs2_months = ifelse(recurrence1 == "Y", tt_recurrence_months + sample(c(3,6,9,12), size = n, replace = TRUE), NA_real_),
      pfs2_event = as.integer(recurrence1 == "Y" & runif(n) < 0.5),
      recurrence1_treatment_clean = factor(ifelse(recurrence1 == "Y", sample(c("Plaque","GKSRS"), size = n, replace = TRUE), NA_character_)),
      # Subgroup/diagnostic variables referenced
      initial_t_stage = sample(c("T1","T2","T3"), size = n, replace = TRUE),
      initial_tumor_diameter = runif(n, 5, 20),
      initial_overall_stage_modified = sample(c("I","II","III"), size = n, replace = TRUE),
      optic_nerve = sample(c("Involved","Not Involved"), size = n, replace = TRUE),
      # Covariates
      age_at_diagnosis = sample(40:85, size = n, replace = TRUE),
      sex = sample(c("Female", "Male"), size = n, replace = TRUE),
      location = sample(c("Choroid", "Ciliary Body"), size = n, replace = TRUE)
    )
  }))
  df
}

prepare_inputs_o13 <- function(dataset_name) {
  dir.create(PROCESSED_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
  data_path <- file.path(PROCESSED_DATA_DIR, paste0(dataset_name, ".rds"))
  other_map_path <- file.path(PROCESSED_DATA_DIR, "other_map.rds")
  df <- make_processed_dataset_o13(40)
  saveRDS(df, data_path)
  saveRDS(list(treatment_group_map = c("Plaque" = "Plaque", "GKSRS" = "GKSRS")), other_map_path)
}

parse_json_lines_quiet <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  jsonlite::stream_in(textConnection(paste(lines, collapse = "\n")), verbose = FALSE)
}

test_that("Objectives 1–3 run and produce readable JSON logs with context", {
  dataset <- "uveal_melanoma_full_cohort"
  prepare_inputs_o13(dataset)

  log_file <- tempfile(fileext = ".log")
  setup_logging(log_path = log_file, level = "INFO", progress = FALSE, context_in_file = TRUE)

  # Run 1..3
  for (obj in 1:3) {
    obj_name <- switch(as.character(obj),
      "1" = "objective_1_primary_outcomes",
      "2" = "objective_2_safety_toxicity",
      "3" = "objective_3_repeat_radiation"
    )
    invisible(with_log_context(cohort = dataset, objective = obj_name, subobjective = NULL, expr = {
      logger::log_info(sprintf("Quick-run Objective %d", obj))
      run_specific_objective(dataset, obj)
    }))
  }

  # Validate JSON structured logs contain expected fields and completion markers
  df <- parse_json_lines_quiet(log_file)
  expect_true(all(c("timestamp", "level_text", "level_num", "message", "cohort", "objective", "subobjective") %in% names(df)))
  # At least one completion marker per objective context
  for (obj_name in c("objective_1_primary_outcomes", "objective_2_safety_toxicity", "objective_3_repeat_radiation")) {
    has_obj <- which(df$objective == obj_name)
    expect_true(length(has_obj) > 0)
    any_done <- any(grepl(">>> COMPLETED STATISTICAL ANALYSIS", df$message[has_obj], fixed = TRUE))
    expect_true(any_done)
  }
})
